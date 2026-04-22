//! Shared driver for the `rust_mutations` fuzz target.
//!
//! The harness turns an arbitrary byte stream into a sequence of mutations
//! against a Rust source buffer. After every mutation it asserts the
//! [`tinyhl::TokenTable::mutate`] result matches a fresh parse of the edited
//! source, plus a couple of subsidiary invariants.

use std::panic::{AssertUnwindSafe, catch_unwind};

use tinyhl::{Language, Source, Span, Token, TokenTable};

/// Seed source: ~10 lines of Rust dense with tokens that exercise every
/// lexer arm — keywords, lifetimes, hex/oct/bin numbers with suffixes,
/// escape-heavy strings, raw strings, char literals, and nested block
/// comments.
pub const SEED: &str = concat!(
    "fn f<'a>(x: &'a str) -> i32 {\n",
    "    let n: u32 = 0xFF_u32 + 0o17 + 0b101;\n",
    "    let s: &str = \"hi \\\"there\\\"\\n\";\n",
    "    let r = r#\"raw \\n text\"#;\n",
    "    let c: char = '\\u{2603}';\n",
    "    // line comment\n",
    "    /* nested /* block */ comment */\n",
    "    match x { \"a\" => 1, _ => 0 }\n",
    "}\n",
);

/// Bytes picked by mutation payloads. Covers the Rust-lexer-interesting
/// byte classes (quote kinds, escape, slash/star for comments, prefix
/// letters, digits, underscore, punctuation) while keeping the source
/// ASCII-only so offsets are always byte-aligned.
const INTERESTING: &[u8] = b" \n\"'\\/*#rbcx0_.;:<=,+(){}";

/// Maximum source length the driver permits before skipping further
/// inserts. Keeps each fuzz exec bounded and stays well below u32 limits.
const MAX_LEN: usize = 64 * 1024;

/// Maximum mutations applied per fuzz input.
const MAX_MUTATIONS: usize = 64;

/// Size of one command header in the input stream.
const CMD_HDR: usize = 5;

/// A single decoded mutation command.
#[derive(Clone, Debug)]
pub struct Command {
    pub op: u8,
    pub offset_raw: u16,
    pub del_nib: u8,
    pub pay_nib: u8,
    /// Raw payload bytes before they are mapped through [`INTERESTING`].
    pub pay_raw: Vec<u8>,
}

/// Consumes one [`Command`] from `data` starting at `*cur`, advancing `*cur`
/// past the bytes it consumes. Returns `None` when there aren't enough
/// bytes for a full command.
pub fn decode_command(data: &[u8], cur: &mut usize) -> Option<Command> {
    if *cur + CMD_HDR > data.len() {
        return None;
    }
    let op = data[*cur] % 3;
    let offset_raw = u16::from_le_bytes([data[*cur + 1], data[*cur + 2]]);
    let del_nib = data[*cur + 3] & 0x0F;
    let pay_nib = data[*cur + 4] & 0x0F;
    *cur += CMD_HDR;
    let pn = pay_nib as usize;
    if *cur + pn > data.len() {
        return None;
    }
    let pay_raw = data[*cur..*cur + pn].to_vec();
    *cur += pn;
    Some(Command { op, offset_raw, del_nib, pay_nib, pay_raw })
}

/// Resolves a [`Command`] against a current `source.len()`, returning the
/// `(offset, del_len, payload)` to apply — or `None` if the command would
/// exceed [`MAX_LEN`].
pub fn resolve_command(cmd: &Command, src_len: usize) -> Option<(usize, usize, Vec<u8>)> {
    let off = (cmd.offset_raw as usize) % (src_len + 1);
    let del_len = match cmd.op {
        0 => 0,
        _ => (cmd.del_nib as usize).min(src_len - off),
    };
    let payload: Vec<u8> = if cmd.op == 1 {
        Vec::new()
    } else {
        cmd.pay_raw
            .iter()
            .map(|&b| INTERESTING[b as usize % INTERESTING.len()])
            .collect()
    };
    let new_len = src_len - del_len + payload.len();
    if new_len > MAX_LEN {
        return None;
    }
    Some((off, del_len, payload))
}

/// Runs the mutation sequence encoded in `data` against [`SEED`], checking
/// the incremental invariant after every step.
///
/// Input grammar (per command):
/// ```text
///   byte 0      op         (0=insert, 1=delete, 2=replace; mod 3)
///   byte 1..3  offset_raw  (little-endian u16, reduced mod source.len()+1)
///   byte 3     del_len     (low nibble; deletion byte count)
///   byte 4     pay_len     (low nibble; payload byte count)
///   byte 5..   payload     (each byte mapped through INTERESTING[%])
/// ```
pub fn run(data: &[u8]) {
    let mut source: Vec<u8> = SEED.as_bytes().to_vec();
    let mut table = {
        let bytes: &[u8] = &source;
        let src: &dyn Source = &bytes;
        TokenTable::new(Language::Rust, src)
    };
    assert_eq!(table.source_len() as usize, source.len());

    let mut cur = 0usize;
    let mut applied = 0usize;
    while applied < MAX_MUTATIONS {
        let Some(cmd) = decode_command(data, &mut cur) else {
            break;
        };
        let Some((off, del_len, payload)) = resolve_command(&cmd, source.len()) else {
            continue;
        };

        source.splice(off..off + del_len, payload.iter().copied());

        let bytes: &[u8] = &source;
        let src: &dyn Source = &bytes;
        let invalidated =
            table.mutate(src, Span::new(off as u32, del_len as u32), payload.len() as u32);

        assert_eq!(
            table.source_len() as usize,
            source.len(),
            "source_len mismatch after op={} off={off} del={del_len} pay={}",
            cmd.op,
            payload.len(),
        );

        let fresh = TokenTable::new(Language::Rust, src);
        let mutated_tokens: Vec<Token> =
            table.query(Span::new(0, table.source_len())).collect();
        let fresh_tokens: Vec<Token> =
            fresh.query(Span::new(0, fresh.source_len())).collect();

        if mutated_tokens != fresh_tokens {
            let preview: String = String::from_utf8_lossy(&source).chars().take(200).collect();
            panic!(
                "incremental != fresh after op={} off={off} del={del_len} \
                 pay_len={} src_preview={preview:?}\n\
                 mutated={mutated_tokens:?}\nfresh={fresh_tokens:?}",
                cmd.op,
                payload.len()
            );
        }

        let edit_span = Span::new(off as u32, payload.len() as u32);
        for t in &mutated_tokens {
            if t.span.overlaps(&edit_span) {
                assert!(
                    t.span.overlaps(&invalidated),
                    "token {:?} overlaps edit {:?} but not invalidated {:?}",
                    t.span,
                    edit_span,
                    invalidated,
                );
            }
        }

        applied += 1;
    }
}

fn op_name(op: u8) -> &'static str {
    match op {
        0 => "insert",
        1 => "delete",
        _ => "replace",
    }
}

fn preview(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() + 4);
    for &b in bytes {
        match b {
            b'\n' => s.push_str("\\n"),
            b'\t' => s.push_str("\\t"),
            b'\r' => s.push_str("\\r"),
            b'"' => s.push_str("\\\""),
            b'\\' => s.push_str("\\\\"),
            0x20..=0x7e => s.push(b as char),
            _ => s.push_str(&format!("\\x{:02x}", b)),
        }
    }
    s
}

fn window_around(bytes: &[u8], off: usize, len: usize) -> String {
    let start = off.saturating_sub(20);
    let end = (off + len + 20).min(bytes.len());
    let left = preview(&bytes[start..off]);
    let mid = preview(&bytes[off..(off + len).min(bytes.len())]);
    let right = preview(&bytes[(off + len).min(bytes.len())..end]);
    format!("[{start}..{end}] {left}⟦{mid}⟧{right}")
}

fn token_diff(got: &[Token], want: &[Token]) -> String {
    let n = got.len().max(want.len());
    let mut out = String::new();
    out.push_str("    idx  got                                                      want\n");
    for i in 0..n {
        let g = got.get(i);
        let w = want.get(i);
        let marker = if g == w { "   " } else { "!!!" };
        out.push_str(&format!("    {marker} {:3}  {:?}  {:?}\n", i, g, w));
        if i > 40 && n - i > 2 {
            out.push_str("    ... (truncated)\n");
            break;
        }
    }
    out
}

/// Runs `run`-equivalent logic with a verbose, per-command trace on stdout.
/// Returns `true` if every invariant held for every applied mutation.
///
/// Used by the `debug_artifact` binary to reproduce libfuzzer crashes in a
/// human-readable form.
pub fn trace_rust_mutations(data: &[u8]) -> bool {
    println!("input     : {} bytes", data.len());
    println!("input hex : {}", data.iter().map(|b| format!("{:02x}", b)).collect::<Vec<_>>().join(" "));
    println!();
    println!("--- seed ({} bytes) ---", SEED.len());
    for (i, line) in SEED.lines().enumerate() {
        println!("{:3} | {line}", i + 1);
    }
    println!();

    let mut source: Vec<u8> = SEED.as_bytes().to_vec();
    let mut table = {
        let bytes: &[u8] = &source;
        let src: &dyn Source = &bytes;
        TokenTable::new(Language::Rust, src)
    };

    let mut cur = 0usize;
    let mut applied = 0usize;
    while applied < MAX_MUTATIONS {
        let before_cur = cur;
        let Some(cmd) = decode_command(data, &mut cur) else {
            println!("input exhausted after {} byte(s); stopping", before_cur);
            break;
        };
        let resolved = resolve_command(&cmd, source.len());
        println!("--- cmd #{applied} (bytes {}..{}) ---", before_cur, cur);
        println!(
            "  op={} ({}), offset_raw={}, del_nib={}, pay_nib={}, pay_raw={:?}",
            cmd.op,
            op_name(cmd.op),
            cmd.offset_raw,
            cmd.del_nib,
            cmd.pay_nib,
            cmd.pay_raw,
        );
        let Some((off, del_len, payload)) = resolved else {
            println!("  SKIP (would exceed MAX_LEN)");
            continue;
        };
        println!(
            "  resolved: off={}, del_len={}, payload={:?}",
            off,
            del_len,
            preview(&payload),
        );
        println!("  source.len before = {}", source.len());
        println!("  window before : {}", window_around(&source, off, del_len));

        let pre_source = source.clone();
        source.splice(off..off + del_len, payload.iter().copied());
        println!("  window after  : {}", window_around(&source, off, payload.len()));
        println!("  source.len after  = {}", source.len());

        let bytes: &[u8] = &source;
        let src: &dyn Source = &bytes;
        let mutate = catch_unwind(AssertUnwindSafe(|| {
            table.mutate(src, Span::new(off as u32, del_len as u32), payload.len() as u32)
        }));
        let invalidated = match mutate {
            Ok(s) => s,
            Err(_) => {
                println!("!!! PANIC inside TokenTable::mutate");
                println!("    pre-source  ({} bytes): {:?}", pre_source.len(), preview(&pre_source));
                println!("    post-source ({} bytes): {:?}", source.len(), preview(&source));
                println!("    op={}, off={off}, del_len={del_len}, pay_len={}", cmd.op, payload.len());
                return false;
            }
        };
        println!("  invalidated = {:?}", invalidated);

        if table.source_len() as usize != source.len() {
            println!(
                "!!! source_len mismatch: table={} expected={}",
                table.source_len(),
                source.len(),
            );
            return false;
        }

        let fresh = match catch_unwind(AssertUnwindSafe(|| TokenTable::new(Language::Rust, src))) {
            Ok(f) => f,
            Err(_) => {
                println!("!!! PANIC inside fresh TokenTable::new");
                println!("    source ({} bytes): {:?}", source.len(), preview(&source));
                return false;
            }
        };

        let mutated: Vec<Token> = table.query(Span::new(0, table.source_len())).collect();
        let expected: Vec<Token> = fresh.query(Span::new(0, fresh.source_len())).collect();

        if mutated != expected {
            println!("!!! incremental != fresh");
            println!(
                "    post-source ({} bytes): {:?}",
                source.len(),
                preview(&source),
            );
            println!("{}", token_diff(&mutated, &expected));
            return false;
        }

        let edit_span = Span::new(off as u32, payload.len() as u32);
        for t in &mutated {
            if t.span.overlaps(&edit_span) && !t.span.overlaps(&invalidated) {
                println!("!!! edit-overlapping token not in invalidated span");
                println!("    token        {:?}", t);
                println!("    edit span    {:?}", edit_span);
                println!("    invalidated  {:?}", invalidated);
                return false;
            }
        }

        println!(
            "  OK: {} tokens, {} chunk(s)",
            mutated.len(),
            table.chunk_count(),
        );
        applied += 1;
    }

    println!();
    println!("applied {} mutation(s); all invariants held", applied);
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Empty input does nothing but must not panic.
    #[test]
    fn empty_input_is_noop() {
        run(&[]);
    }

    /// A small curated corpus covering each op flavor.
    #[test]
    fn corpus_smoke() {
        // Each entry is one input; run checks invariants after every command.
        let cases: &[&[u8]] = &[
            // One insert at offset 0, 3 interesting bytes.
            &[0, 0, 0, 0, 3, 0, 1, 2],
            // One delete of 5 bytes near the middle.
            &[1, 32, 0, 5, 0],
            // One replace: delete 3, insert 4.
            &[2, 40, 0, 3, 4, 5, 6, 7, 8],
            // Several mixed ops.
            &[
                0, 10, 0, 0, 2, 3, 4, // insert 2 bytes at 10
                1, 20, 0, 7, 0, // delete 7 at 20
                2, 5, 0, 2, 3, 9, 10, 11, // replace at 5
                0, 0, 0, 0, 1, 2, // insert 1 byte at 0
            ],
            // Exercise offsets near EOF.
            &[
                0, 0xFF, 0xFF, 0, 4, 0, 1, 2, 3,
                1, 0xFE, 0xFF, 8, 0,
            ],
            // Long insert payload.
            &[0, 50, 0, 0, 15, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
        ];

        for (i, case) in cases.iter().enumerate() {
            run(case);
            // Rerun from scratch with same case — run() starts from SEED every
            // time, so this just confirms determinism.
            run(case);
            let _ = i;
        }
    }

    /// Pseudorandom sweep: a handful of deterministic seed bytes that
    /// exercises dozens of mutations.
    #[test]
    fn pseudorandom_sweep() {
        // Deterministic LCG-style byte stream. Not real randomness; just a
        // cheap way to fan out over the command space without depending on
        // the `rand` crate.
        let mut state: u32 = 0x9E37_79B9;
        let mut buf: Vec<u8> = Vec::with_capacity(4096);
        for _ in 0..4096 {
            state = state.wrapping_mul(1664525).wrapping_add(1013904223);
            buf.push((state >> 16) as u8);
        }
        run(&buf);
    }
}
