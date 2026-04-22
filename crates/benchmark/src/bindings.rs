//! Benchmarks for the `extui-bindings` router.
//!
//! Inputs span three axes so the numbers reflect real workloads rather
//! than a single hot tuple the branch predictor memorizes instantly:
//!
//! - **Build-time cost** uses the shipped `vim` / `nano` / `emacs` presets
//!   from `extui-editor` plus two synthetic shapes (1k flat bindings,
//!   1k two-key chord bindings) to separate flat-layer from chord-layer
//!   cost.
//! - **Lookup cost** replays a full ~1k-key pseudo-random session per
//!   iteration against the `vim` router, driving the
//!   lookup→payload→layer-transition state machine exactly as a host
//!   would. Sequences are long enough that branch history can't trivially
//!   memorize a short cycle, and each session targets a specific access
//!   pattern (motion burst, chord alternation, operator-pending
//!   expansion, Normal↔Insert mode flips with typed-text bodies, miss
//!   keys, and a braided mix).
//! - **Parsing / event conversion** rotates a handful of token shapes
//!   (bare char, modifier chord, angle-bracketed named, whitespace
//!   sequence) and a KeyEvent mix so the parser never sees the same
//!   string on consecutive iters.

use extui::event::{KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers};
use extui_bindings::{InputKey, LayerId, NamedKey, Payload, parse_key, parse_sequence};
use extui_editor::bindings::{
    EditorAction, EditorContext, EditorRouter, EditorRouterBuilder, Filter, MODE_INSERT,
    MODE_NORMAL, MODE_VISUAL, VimOptions, emacs, nano, vim,
};
use jsony_bench::{Bencher, Stat};
use oorandom::Rand32;
use std::cell::RefCell;

use crate::common::{BenchRecord, find_baseline, print_row};

/// Per-iteration session length for lookup benchmarks. Large enough that
/// the branch predictor can't collapse the whole body into a small cycle,
/// small enough that individual iterations stay under ~50 µs.
const LOOKUP_LEN: usize = 1024;
/// Braided mix gets a longer script since it multiplexes several access
/// patterns.
const LOOKUP_LEN_MIXED: usize = 2048;

pub const GROUP: &str = "bindings";

// ---------------------------------------------------------------------------
// Router construction
// ---------------------------------------------------------------------------

fn build_vim_router() -> EditorRouter {
    vim(VimOptions::default()).into_parts().0
}

fn build_nano_router() -> EditorRouter {
    nano().into_parts().0
}

fn build_emacs_router() -> EditorRouter {
    emacs().into_parts().0
}

/// N independent flat bindings at BASE; key payloads cycle through the
/// printable ASCII range with rotating modifiers so there are no dupes.
fn build_synthetic_flat(n: usize) -> EditorRouter {
    let mut b = EditorRouterBuilder::with_capacity(n);
    let filter = Filter::new(MODE_NORMAL);
    for i in 0..n {
        let ch = ((b' ' + (i % 90) as u8) as char).to_ascii_lowercase();
        let mods = match i % 4 {
            0 => KeyModifiers::empty(),
            1 => KeyModifiers::CONTROL,
            2 => KeyModifiers::ALT,
            _ => KeyModifiers::CONTROL | KeyModifiers::ALT,
        };
        let key = InputKey::char(ch, mods);
        b.bind(LayerId::BASE, filter, &[key], EditorAction::NoOp);
    }
    b.build()
}

/// N two-key chord bindings under a shared Space prefix. This exercises
/// the chord-layer interning + reuse path in the builder.
fn build_synthetic_chords(n: usize) -> EditorRouter {
    let mut b = EditorRouterBuilder::with_capacity(n * 2);
    let filter = Filter::new(MODE_NORMAL);
    let space = InputKey::char(' ', KeyModifiers::empty());
    for i in 0..n {
        let ch = (b'!' + (i % 90) as u8) as char;
        let mods = if i % 3 == 0 {
            KeyModifiers::CONTROL
        } else {
            KeyModifiers::empty()
        };
        let terminal = InputKey::char(ch, mods);
        b.bind(
            LayerId::BASE,
            filter,
            &[space, terminal],
            EditorAction::NoOp,
        );
    }
    b.build()
}

// ---------------------------------------------------------------------------
// Lookup scripts
// ---------------------------------------------------------------------------
//
// A script is a list of `(ctx, key)` pairs. The bench body replays the
// script through the router maintaining the caller's "current layer"
// state (advance on `Payload::Layer`, reset to `BASE` on `Payload::Action`
// or miss) — i.e. the same state machine real hosts run.

fn ctx_normal() -> EditorContext {
    EditorContext {
        mode: MODE_NORMAL as u8,
    }
}

fn ctx_insert() -> EditorContext {
    EditorContext {
        mode: MODE_INSERT as u8,
    }
}

fn ctx_visual() -> EditorContext {
    EditorContext {
        mode: MODE_VISUAL as u8,
    }
}

fn k(c: char) -> InputKey {
    InputKey::char(c, KeyModifiers::empty())
}

fn named(n: NamedKey) -> InputKey {
    InputKey::named(n, KeyModifiers::empty())
}

fn pick<'a, T>(rng: &mut Rand32, pool: &'a [T]) -> &'a T {
    &pool[(rng.rand_u32() as usize) % pool.len()]
}

/// Pseudo-random Normal-mode motion stream drawn from a realistic pool.
/// The pool is deliberately bigger than typical BTB history lengths so
/// repeated sessions can't collapse into a short predictable cycle.
fn gen_motion(len: usize) -> Vec<(EditorContext, InputKey)> {
    let ctx = ctx_normal();
    let pool = [
        k('j'),
        k('j'),
        k('k'),
        k('k'),
        k('h'),
        k('l'),
        k('l'),
        k('w'),
        k('b'),
        k('e'),
        k('0'),
        k('$'),
        k('^'),
        k('G'),
        k('{'),
        k('}'),
    ];
    let mut rng = Rand32::new(0xBE11_EF1F);
    (0..len).map(|_| (ctx, *pick(&mut rng, &pool))).collect()
}

/// Chord-trigger alternation: a prefix key that advances into a chord
/// layer followed by a terminator. Mixes `g`, `d`, `c`, `y`, `g U`, `g u`
/// starters and varied terminators so each pair exercises a different
/// `(layer, key)` slot.
fn gen_chord(len: usize) -> Vec<(EditorContext, InputKey)> {
    let ctx = ctx_normal();
    let mut rng = Rand32::new(0xABACABB0);
    let starters = [k('g'), k('g'), k('g'), k('d'), k('c'), k('y'), k('g')];
    let terms_g = [k('g'), k('j'), k('k'), k('U'), k('u')];
    let terms_op = [k('w'), k('b'), k('e'), k('$'), k('0'), k('i'), k('a')];
    let mut out = Vec::with_capacity(len + 4);
    while out.len() < len {
        let start = *pick(&mut rng, &starters);
        out.push((ctx, start));
        // Pick a terminator that's plausible for the starter.
        let term = if start == k('g') {
            *pick(&mut rng, &terms_g)
        } else {
            *pick(&mut rng, &terms_op)
        };
        out.push((ctx, term));
        // Occasional third key that models `g U U` / `d i w` / `c a W`.
        if rng.rand_u32() % 5 == 0 {
            out.push((
                ctx,
                *pick(&mut rng, &[k('w'), k('W'), k('p'), k('('), k('"')]),
            ));
        }
    }
    out.truncate(len);
    out
}

/// Operator-pending expansions: `dw`, `ciw`, `y$`, `gUU`, etc., drawn
/// so the mix of 2- and 3-key sequences spans the operator, case, and
/// text-object layers.
fn gen_operator(len: usize) -> Vec<(EditorContext, InputKey)> {
    let ctx = ctx_normal();
    let seqs: &[&[char]] = &[
        &['d', 'w'],
        &['d', 'd'],
        &['d', 'e'],
        &['d', 'i', 'w'],
        &['d', 'a', 'w'],
        &['d', '$'],
        &['c', 'w'],
        &['c', 'c'],
        &['c', 'i', 'w'],
        &['c', 'a', 'W'],
        &['y', 'y'],
        &['y', 'w'],
        &['y', '$'],
        &['y', 'i', '('],
        &['g', 'U', 'U'],
        &['g', 'u', 'u'],
    ];
    let mut rng = Rand32::new(0xFEED_F00D);
    let mut out = Vec::with_capacity(len + 4);
    while out.len() < len {
        let seq = *pick(&mut rng, seqs);
        for &c in seq {
            out.push((ctx, k(c)));
        }
    }
    out.truncate(len);
    out
}

/// Normal↔Insert↔Visual flips with realistic typed-text bodies so the
/// insert-mode branch actually sees character keys, not just mode
/// transitions.
fn gen_mode_switch(len: usize) -> Vec<(EditorContext, InputKey)> {
    let mut rng = Rand32::new(0xD00D_BEEF);
    let esc = named(NamedKey::Esc);
    let normal = ctx_normal();
    let insert = ctx_insert();
    let visual = ctx_visual();
    let words = [
        "hello", "world", "foo", "bar", "baz", "quux", "quick", "brown", "fox", "jumps", "lazy",
        "dog",
    ];
    let motions = [k('j'), k('k'), k('l'), k('h'), k('w'), k('b')];
    let enters = [k('i'), k('a'), k('I'), k('A'), k('o')];
    let mut out = Vec::with_capacity(len + 64);
    while out.len() < len {
        // Normal motion burst.
        let burst = (rng.rand_u32() as usize % 6) + 2;
        for _ in 0..burst {
            out.push((normal, *pick(&mut rng, &motions)));
        }
        // Enter insert.
        out.push((normal, *pick(&mut rng, &enters)));
        // Type a word plus a space.
        for c in pick(&mut rng, &words).chars() {
            out.push((insert, k(c)));
        }
        out.push((insert, k(' ')));
        out.push((insert, esc));
        // Occasional visual excursion.
        if rng.rand_u32() & 3 == 0 {
            out.push((normal, k('v')));
            for _ in 0..3 {
                out.push((visual, *pick(&mut rng, &motions)));
            }
            out.push((visual, esc));
        }
    }
    out.truncate(len);
    out
}

/// Mostly-miss Normal-mode stream with occasional hits, so the miss
/// path is the dominant cost but both branches are exercised.
fn gen_miss(len: usize) -> Vec<(EditorContext, InputKey)> {
    let ctx = ctx_normal();
    let mut rng = Rand32::new(0xFACE_B00C);
    let miss_pool = [
        k('z'),
        k('q'),
        k('@'),
        k('#'),
        k('&'),
        k('*'),
        k('?'),
        k('|'),
        k('!'),
        k(';'),
        k('%'),
        InputKey::named(NamedKey::PageUp, KeyModifiers::SUPER),
    ];
    let hit_pool = [k('j'), k('k'), k('w'), k('l'), k('h')];
    (0..len)
        .map(|_| {
            let key = if rng.rand_u32() % 7 == 0 {
                *pick(&mut rng, &hit_pool)
            } else {
                *pick(&mut rng, &miss_pool)
            };
            (ctx, key)
        })
        .collect()
}

/// Braided session that stitches variable-length chunks of the other
/// scripts in a pseudo-random order. Closer to what a router sees over
/// a minute of real editing than any single specialized pattern.
fn gen_mixed(len: usize) -> Vec<(EditorContext, InputKey)> {
    let mut rng = Rand32::new(0xC0FF_EE42);
    let segments = [
        gen_motion(len),
        gen_operator(len),
        gen_mode_switch(len),
        gen_chord(len / 2),
        gen_miss(len / 4),
    ];
    let mut cursors = [0usize; 5];
    let mut out = Vec::with_capacity(len + 64);
    while out.len() < len {
        let i = (rng.rand_u32() as usize) % segments.len();
        let seg = &segments[i];
        let take = ((rng.rand_u32() as usize) % 24 + 4).min(seg.len().saturating_sub(cursors[i]));
        if take == 0 {
            cursors[i] = 0;
            continue;
        }
        out.extend_from_slice(&seg[cursors[i]..cursors[i] + take]);
        cursors[i] += take;
    }
    out.truncate(len);
    out
}

// ---------------------------------------------------------------------------
// Parse inputs
// ---------------------------------------------------------------------------

const PARSE_BARE: &[&str] = &["a", "j", "k", "w", "0", "$", "G", "x", ";", "?"];
const PARSE_MODIFIED: &[&str] = &[
    "C-c", "C-x", "M-f", "C-M-x", "S-Tab", "C-Space", "M-b", "Ctrl-a", "Alt-6", "C-S-F12",
];
const PARSE_BRACKETED: &[&str] = &[
    "<Esc>", "<CR>", "<C-c>", "<C-M-x>", "<F5>", "<C-F12>", "<S-Tab>", "<PageUp>", "<lt>",
    "<Space>",
];
const PARSE_SEQUENCES: &[&str] = &[
    "g g",
    "C-x C-s",
    "d i w",
    "y a p",
    "<Space> f f",
    "g U U",
    "c a W",
    "<C-w> v",
    "<Esc> i Hello",
    "d f x",
];

fn sample_events() -> Vec<KeyEvent> {
    let mut out = Vec::new();
    for ch in [
        'a', 'b', 'c', 'j', 'k', '!', '1', '?', 'x', 'Z', ' ', '\t', '~', '$',
    ] {
        out.push(KeyEvent {
            code: KeyCode::Char(ch),
            modifiers: KeyModifiers::empty(),
            kind: KeyEventKind::Press,
            state: KeyEventState::empty(),
        });
    }
    for (code, mods) in [
        (KeyCode::Char('c'), KeyModifiers::CONTROL),
        (
            KeyCode::Char('x'),
            KeyModifiers::CONTROL | KeyModifiers::ALT,
        ),
        (KeyCode::Char('s'), KeyModifiers::SUPER),
        (KeyCode::Tab, KeyModifiers::SHIFT),
        (KeyCode::function(5), KeyModifiers::empty()),
        (KeyCode::function(12), KeyModifiers::CONTROL),
        (KeyCode::Esc, KeyModifiers::empty()),
        (KeyCode::Enter, KeyModifiers::empty()),
        (KeyCode::PageUp, KeyModifiers::empty()),
        (KeyCode::Home, KeyModifiers::empty()),
        (KeyCode::Backspace, KeyModifiers::empty()),
        (KeyCode::Delete, KeyModifiers::ALT),
    ] {
        out.push(KeyEvent {
            code,
            modifiers: mods,
            kind: KeyEventKind::Press,
            state: KeyEventState::empty(),
        });
    }
    out
}

// ---------------------------------------------------------------------------
// Bench harness
// ---------------------------------------------------------------------------

pub struct BindingsBench {
    pub name: &'static str,
    pub size: &'static str,
    pub run: fn(&mut Bencher) -> Stat,
    /// Work-unit count for the body (number of lookups for a session,
    /// `0` otherwise). Stored in the `bytes` column so `ns / bytes`
    /// reads as ns-per-lookup for session benches.
    pub work: u64,
}

fn cycle<F: FnMut(u64)>(bencher: &mut Bencher, mut body: F) -> Stat {
    let counter = RefCell::new(0u64);
    bencher.bench_with_generator(
        || {
            let mut c = counter.borrow_mut();
            *c = c.wrapping_add(1);
            *c
        },
        |i| body(i),
    )
}

/// Replays the full `script` through `router` per-iteration, driving the
/// layer-transition state machine a real host would run. Per-iteration
/// cost is the full session, not a single lookup, which (a) amortizes
/// the bencher's per-iter overhead and (b) makes the session long
/// enough that branch history can't trivially memorize a short cycle.
fn bench_lookup_session(bencher: &mut Bencher, script: Vec<(EditorContext, InputKey)>) -> Stat {
    let router = build_vim_router();
    bencher.bench_with_generator(
        || (),
        |()| {
            let mut layer = LayerId::BASE;
            let mut hits: u64 = 0;
            for &(ctx, key) in &script {
                match router.lookup(&ctx, layer, key) {
                    Some(entry) => match entry.payload() {
                        Payload::Action(_) => {
                            layer = LayerId::BASE;
                            hits = hits.wrapping_add(1);
                        }
                        Payload::Layer(l) => {
                            layer = l;
                        }
                    },
                    None => {
                        layer = LayerId::BASE;
                    }
                }
            }
            std::hint::black_box(hits);
        },
    )
}

fn bench_build_vim(bencher: &mut Bencher) -> Stat {
    cycle(bencher, |_| {
        let r = build_vim_router();
        std::hint::black_box(&r);
    })
}

fn bench_build_nano(bencher: &mut Bencher) -> Stat {
    cycle(bencher, |_| {
        let r = build_nano_router();
        std::hint::black_box(&r);
    })
}

fn bench_build_emacs(bencher: &mut Bencher) -> Stat {
    cycle(bencher, |_| {
        let r = build_emacs_router();
        std::hint::black_box(&r);
    })
}

fn bench_build_flat_1k(bencher: &mut Bencher) -> Stat {
    cycle(bencher, |_| {
        let r = build_synthetic_flat(1024);
        std::hint::black_box(&r);
    })
}

fn bench_build_chords_1k(bencher: &mut Bencher) -> Stat {
    cycle(bencher, |_| {
        let r = build_synthetic_chords(1024);
        std::hint::black_box(&r);
    })
}

fn bench_lookup_motion(bencher: &mut Bencher) -> Stat {
    bench_lookup_session(bencher, gen_motion(LOOKUP_LEN))
}

fn bench_lookup_chord(bencher: &mut Bencher) -> Stat {
    bench_lookup_session(bencher, gen_chord(LOOKUP_LEN))
}

fn bench_lookup_operator(bencher: &mut Bencher) -> Stat {
    bench_lookup_session(bencher, gen_operator(LOOKUP_LEN))
}

fn bench_lookup_mode_switch(bencher: &mut Bencher) -> Stat {
    bench_lookup_session(bencher, gen_mode_switch(LOOKUP_LEN))
}

fn bench_lookup_miss(bencher: &mut Bencher) -> Stat {
    bench_lookup_session(bencher, gen_miss(LOOKUP_LEN))
}

fn bench_lookup_mixed(bencher: &mut Bencher) -> Stat {
    bench_lookup_session(bencher, gen_mixed(LOOKUP_LEN_MIXED))
}

fn bench_parse_with(inputs: &'static [&'static str]) -> impl Fn(&mut Bencher) -> Stat {
    move |bencher: &mut Bencher| {
        cycle(bencher, |i| {
            let src = inputs[(i as usize) % inputs.len()];
            let r = parse_key(src);
            std::hint::black_box(&r);
        })
    }
}

fn bench_parse_bare(bencher: &mut Bencher) -> Stat {
    bench_parse_with(PARSE_BARE)(bencher)
}

fn bench_parse_modified(bencher: &mut Bencher) -> Stat {
    bench_parse_with(PARSE_MODIFIED)(bencher)
}

fn bench_parse_bracketed(bencher: &mut Bencher) -> Stat {
    bench_parse_with(PARSE_BRACKETED)(bencher)
}

fn bench_parse_sequence(bencher: &mut Bencher) -> Stat {
    cycle(bencher, |i| {
        let src = PARSE_SEQUENCES[(i as usize) % PARSE_SEQUENCES.len()];
        let r = parse_sequence(src);
        std::hint::black_box(&r);
    })
}

fn bench_from_event(bencher: &mut Bencher) -> Stat {
    let events = sample_events();
    cycle(bencher, |i| {
        let evt = &events[(i as usize) % events.len()];
        let k = InputKey::from_event(evt);
        std::hint::black_box(&k);
    })
}

pub const BENCHES: &[BindingsBench] = &[
    // Build-side: cost of assembling a router from scratch.
    BindingsBench {
        name: "build_nano",
        size: "nano",
        run: bench_build_nano,
        work: 0,
    },
    BindingsBench {
        name: "build_emacs",
        size: "emacs",
        run: bench_build_emacs,
        work: 0,
    },
    BindingsBench {
        name: "build_vim",
        size: "vim",
        run: bench_build_vim,
        work: 0,
    },
    BindingsBench {
        name: "build_flat_1k",
        size: "1024",
        run: bench_build_flat_1k,
        work: 0,
    },
    BindingsBench {
        name: "build_chords_1k",
        size: "1024",
        run: bench_build_chords_1k,
        work: 0,
    },
    // Lookup-side: full session replay against the vim router.
    BindingsBench {
        name: "lookup_motion",
        size: "1024k",
        run: bench_lookup_motion,
        work: LOOKUP_LEN as u64,
    },
    BindingsBench {
        name: "lookup_chord",
        size: "1024k",
        run: bench_lookup_chord,
        work: LOOKUP_LEN as u64,
    },
    BindingsBench {
        name: "lookup_operator",
        size: "1024k",
        run: bench_lookup_operator,
        work: LOOKUP_LEN as u64,
    },
    BindingsBench {
        name: "lookup_mode_switch",
        size: "1024k",
        run: bench_lookup_mode_switch,
        work: LOOKUP_LEN as u64,
    },
    BindingsBench {
        name: "lookup_miss",
        size: "1024k",
        run: bench_lookup_miss,
        work: LOOKUP_LEN as u64,
    },
    BindingsBench {
        name: "lookup_mixed",
        size: "2048k",
        run: bench_lookup_mixed,
        work: LOOKUP_LEN_MIXED as u64,
    },
    // Parse + event conversion.
    BindingsBench {
        name: "parse_bare",
        size: "token",
        run: bench_parse_bare,
        work: 0,
    },
    BindingsBench {
        name: "parse_modified",
        size: "token",
        run: bench_parse_modified,
        work: 0,
    },
    BindingsBench {
        name: "parse_bracketed",
        size: "token",
        run: bench_parse_bracketed,
        work: 0,
    },
    BindingsBench {
        name: "parse_sequence",
        size: "seq",
        run: bench_parse_sequence,
        work: 0,
    },
    BindingsBench {
        name: "from_event",
        size: "evt",
        run: bench_from_event,
        work: 0,
    },
];

pub fn list() -> Vec<(&'static str, &'static str)> {
    BENCHES.iter().map(|b| (GROUP, b.name)).collect()
}

pub fn run_matching(
    bencher: &mut Bencher,
    matches: &dyn Fn(&str, &str) -> bool,
    records: &mut Vec<BenchRecord>,
    baseline: Option<&[BenchRecord]>,
) {
    let mut any = false;
    for bench in BENCHES {
        if !matches(GROUP, bench.name) {
            continue;
        }
        let stat = (bench.run)(bencher);
        let record = BenchRecord {
            group: GROUP.to_string(),
            size: bench.size.to_string(),
            width: 0,
            height: 0,
            scenario: bench.name.to_string(),
            ns: f64::from(stat.nanos),
            cycles: f64::from(stat.cycles),
            inst: f64::from(stat.inst),
            branch: f64::from(stat.branch),
            bytes: bench.work,
        };
        let base = find_baseline(baseline, &record);
        print_row(&record, base);
        records.push(record);
        any = true;
    }
    if any {
        println!();
    }
}
