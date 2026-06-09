use extui::event::{KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers};
use extui::{Buffer, Rect};
use extui_editor::{Editor, Replacement, StyleRun, TextBuffer, TrackedChange};
use jsony_bench::{Bencher, Stat};
use std::cell::RefCell;

use crate::common::{BenchRecord, find_baseline, print_row};

pub const GROUP: &str = "editor";

pub const BIG_RS: &str = include_str!("../fixtures/big.rs.txt");
pub const WIDTH: u16 = 120;
pub const HEIGHT: u16 = 40;
pub const SIZE_NAME: &str = "120x40";

const SLICE_LINES: usize = 300;

fn slice_n_lines(s: &str, n: usize) -> &str {
    let mut count = 0;
    for (i, ch) in s.char_indices() {
        if ch == '\n' {
            count += 1;
            if count == n {
                return &s[..=i];
            }
        }
    }
    s
}

fn key_char(c: char) -> KeyEvent {
    let modifiers = if c.is_ascii_uppercase() {
        KeyModifiers::SHIFT
    } else {
        KeyModifiers::NONE
    };
    KeyEvent {
        code: KeyCode::Char(c),
        modifiers,
        kind: KeyEventKind::Press,
        state: KeyEventState::empty(),
    }
}

fn key_ctrl(c: char) -> KeyEvent {
    KeyEvent {
        code: KeyCode::Char(c),
        modifiers: KeyModifiers::CONTROL,
        kind: KeyEventKind::Press,
        state: KeyEventState::empty(),
    }
}

fn key_code(code: KeyCode) -> KeyEvent {
    KeyEvent {
        code,
        modifiers: KeyModifiers::NONE,
        kind: KeyEventKind::Press,
        state: KeyEventState::empty(),
    }
}

struct BufferSource<'a>(&'a TextBuffer);

impl<'a> tinyhl::Source for BufferSource<'a> {
    fn len(&self) -> u32 {
        self.0.len() as u32
    }
    fn page(&self, offset: u32) -> (u32, &[u8]) {
        self.0.page(offset)
    }
}

pub struct State {
    pub db: Buffer,
    pub ed: Editor,
    pub hl: Option<tinyhl::Highlighter>,
    pub runs: Vec<StyleRun>,
}

const RECT: Rect = Rect {
    x: 0,
    y: 0,
    w: WIDTH,
    h: HEIGHT,
};

/// Drain tracked changes into the highlighter, then rebuild the
/// per-frame run list. Mirrors the realistic host integration in the
/// `highlight` example.
fn refresh_runs(state: &mut State) {
    let Some(hl) = state.hl.as_mut() else {
        return;
    };
    let change = state.ed.take_tracked_change();
    match change {
        TrackedChange::None => {}
        TrackedChange::Reset => {
            let src = BufferSource(state.ed.text_buffer());
            hl.rebuild(&src);
        }
        TrackedChange::Merged(Replacement {
            offset,
            old_len,
            new_len,
        }) => {
            let src = BufferSource(state.ed.text_buffer());
            hl.apply_replacement(&src, tinyhl::Span::new(offset, old_len), new_len);
        }
    }
    state.runs.clear();
    if let Some(table) = hl.table() {
        let visible = state.ed.visible_byte_span(RECT);
        let span = tinyhl::Span::new(visible.offset, visible.len);
        let mut overlays = tinyhl::Overlays::new(hl, span);
        let keyword_style = extui::Style::DEFAULT.with_fg(extui::Color::rgb(0xbb, 0x9a, 0xf7));
        for tok in table.query(span) {
            let _ = tinyhl::unpack_kind(tok.kind);
            let _ = overlays.at(tok.span.offset);
            // Use a single style — the per-frame cost we care about
            // measuring is the walk + `set_string` overhead, not the
            // palette lookup (which lives in host code and is cheap).
            state.runs.push(StyleRun {
                offset: tok.span.offset,
                len: tok.span.len,
                style: keyword_style,
            });
        }
    }
}

fn paint_and_flush(state: &mut State) {
    refresh_runs(state);
    state
        .ed
        .render_with_styles(RECT, &mut state.db, &state.runs);
    state.db.render_internal();
    std::hint::black_box(&state.db.buf);
    state.db.buf.clear();
}

fn make_state(lang: Option<tinyhl::Language>, wrap: bool, text: &str) -> State {
    let db = Buffer::new(WIDTH, HEIGHT);
    let mut ed = Editor::new();
    ed.resize(WIDTH);
    ed.set_wrap(wrap);
    ed.set_lines(text);
    let hl = lang.map(|language| {
        ed.set_track_replacements(true);
        let mut hl = tinyhl::Highlighter::new(language);
        let src = BufferSource(ed.text_buffer());
        hl.rebuild(&src);
        // Drain the Reset that tracking-enable produced so the first
        // refresh_runs sees None.
        let _ = ed.take_tracked_change();
        hl
    });
    State {
        db,
        ed,
        hl,
        runs: Vec::new(),
    }
}

pub struct EditorBench {
    pub name: &'static str,
    pub build: fn() -> State,
    pub prepare: fn(&mut State, u64),
}

fn no_prepare(_state: &mut State, _frame: u64) {}

// ---- render-side ----

fn build_render_plain() -> State {
    make_state(None, false, slice_n_lines(BIG_RS, SLICE_LINES))
}
fn build_render_highlighted() -> State {
    make_state(
        Some(tinyhl::Language::Rust),
        false,
        slice_n_lines(BIG_RS, SLICE_LINES),
    )
}
fn build_render_wrapped() -> State {
    make_state(
        Some(tinyhl::Language::Rust),
        true,
        slice_n_lines(BIG_RS, SLICE_LINES),
    )
}
fn build_render_full_14k() -> State {
    make_state(Some(tinyhl::Language::Rust), false, BIG_RS)
}

// ---- edit-side ----

fn build_set_lines_big() -> State {
    make_state(Some(tinyhl::Language::Rust), false, "")
}

fn prepare_set_lines_big(state: &mut State, _frame: u64) {
    state.ed.set_lines(BIG_RS);
}

fn build_insert_char() -> State {
    let mut state = make_state(
        Some(tinyhl::Language::Rust),
        false,
        slice_n_lines(BIG_RS, SLICE_LINES),
    );
    state.ed.send_key(&key_char('i'));
    state
}

fn prepare_insert_char(state: &mut State, _frame: u64) {
    // Insert + backspace: net-zero buffer change so we can iterate
    // indefinitely.
    state.ed.send_key(&key_char('a'));
    state.ed.send_key(&key_code(KeyCode::Backspace));
}

fn build_delete_char() -> State {
    make_state(
        Some(tinyhl::Language::Rust),
        false,
        slice_n_lines(BIG_RS, SLICE_LINES),
    )
}

fn prepare_delete_char(state: &mut State, _frame: u64) {
    // `x` deletes a char; `u` undoes it. Steady state.
    state.ed.send_key(&key_char('x'));
    state.ed.send_key(&key_char('u'));
}

// ---- highlight-rebuild side ----
//
// Isolates the cost of rebuilding TokenTable + SemanticTable +
// DelimiterTable from scratch, without the per-frame render pass
// that other benchmarks include.

fn build_tables_rebuild() -> State {
    make_state(
        Some(tinyhl::Language::Rust),
        false,
        slice_n_lines(BIG_RS, SLICE_LINES),
    )
}

fn prepare_tables_rebuild(state: &mut State, _frame: u64) {
    let hl = state
        .hl
        .as_mut()
        .expect("tables_rebuild requires a highlighter");
    let src = BufferSource(state.ed.text_buffer());
    hl.rebuild(&src);
}

fn build_tables_rebuild_full_14k() -> State {
    make_state(Some(tinyhl::Language::Rust), false, BIG_RS)
}

fn prepare_tables_rebuild_full_14k(state: &mut State, frame: u64) {
    prepare_tables_rebuild(state, frame);
}

// ---- motion-side ----

fn build_word_forward() -> State {
    make_state(Some(tinyhl::Language::Rust), false, BIG_RS)
}

fn prepare_word_forward(state: &mut State, _frame: u64) {
    state.ed.send_key(&key_char('w'));
}

fn build_page_down() -> State {
    let mut state = make_state(Some(tinyhl::Language::Rust), false, BIG_RS);
    // Seed last_viewport_h so the first HalfPageDown scrolls a full
    // half-viewport rather than a single row.
    paint_and_flush(&mut state);
    state
}

fn prepare_page_down(state: &mut State, frame: u64) {
    // Alternate Ctrl-D / Ctrl-U so the cursor stays inside the buffer
    // across many iterations.
    let key = if frame & 1 == 0 { 'd' } else { 'u' };
    state.ed.send_key(&key_ctrl(key));
}

pub const BENCHES: &[EditorBench] = &[
    EditorBench {
        name: "render_plain",
        build: build_render_plain,
        prepare: no_prepare,
    },
    EditorBench {
        name: "render_highlighted",
        build: build_render_highlighted,
        prepare: no_prepare,
    },
    EditorBench {
        name: "render_wrapped",
        build: build_render_wrapped,
        prepare: no_prepare,
    },
    EditorBench {
        name: "render_full_14k",
        build: build_render_full_14k,
        prepare: no_prepare,
    },
    EditorBench {
        name: "set_lines_big",
        build: build_set_lines_big,
        prepare: prepare_set_lines_big,
    },
    EditorBench {
        name: "insert_char",
        build: build_insert_char,
        prepare: prepare_insert_char,
    },
    EditorBench {
        name: "delete_char",
        build: build_delete_char,
        prepare: prepare_delete_char,
    },
    EditorBench {
        name: "tables_rebuild",
        build: build_tables_rebuild,
        prepare: prepare_tables_rebuild,
    },
    EditorBench {
        name: "tables_rebuild_full_14k",
        build: build_tables_rebuild_full_14k,
        prepare: prepare_tables_rebuild_full_14k,
    },
    EditorBench {
        name: "word_forward",
        build: build_word_forward,
        prepare: prepare_word_forward,
    },
    EditorBench {
        name: "page_down",
        build: build_page_down,
        prepare: prepare_page_down,
    },
];

fn run_one(bencher: &mut Bencher, bench: &EditorBench) -> Stat {
    let state = RefCell::new((bench.build)());
    let mut frame: u64 = 0;
    bencher.bench_with_generator(
        || {
            frame = frame.wrapping_add(1);
            frame
        },
        |f| {
            let mut s = state.borrow_mut();
            (bench.prepare)(&mut s, f);
            paint_and_flush(&mut s);
        },
    )
}

fn measure_bytes(bench: &EditorBench) -> u64 {
    let mut state = (bench.build)();
    // Warm-up: discard first-paint bytes.
    (bench.prepare)(&mut state, 0);
    paint_and_flush(&mut state);
    state.db.buf.clear();
    // Measured frame.
    (bench.prepare)(&mut state, 1);
    refresh_runs(&mut state);
    state
        .ed
        .render_with_styles(RECT, &mut state.db, &state.runs);
    state.db.render_internal();
    state.db.buf.len() as u64
}

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
        let stat = run_one(bencher, bench);
        let bytes = measure_bytes(bench);
        let record = BenchRecord {
            group: GROUP.to_string(),
            size: SIZE_NAME.to_string(),
            width: WIDTH,
            height: HEIGHT,
            scenario: bench.name.to_string(),
            ns: f64::from(stat.nanos),
            cycles: f64::from(stat.cycles),
            inst: f64::from(stat.inst),
            branch: f64::from(stat.branch),
            bytes,
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
