//! Python lexer.
//!
//! Python is lexed as a stateless byte machine: every token is produced at
//! [`LexState::INITIAL`] and carries [`STATE_BREAKPOINT`], because every
//! lexical construct — including triple-quoted strings, f-strings, and
//! comments — is scanned to completion as a single token. There is no
//! cross-token state to thread, so incremental relexing can resume at any
//! token boundary.
//!
//! Deliberate "good enough" choices:
//!
//! - All string flavours (plain, raw, byte, f-, triple-quoted) emit a single
//!   [`STRING`] token. Interpolations inside f-strings are not decomposed.
//! - Only the 35 hard keywords are recognised. The soft keywords `match`,
//!   `case`, and `type` stay [`IDENT`] so ordinary variables named after them
//!   are not mis-highlighted.
//! - Significant indentation, `INDENT`/`DEDENT`, and implicit line joining are
//!   parser concerns and do not affect the flat token stream.
//!
//! [`STRING`]: kinds::STRING
//! [`IDENT`]: kinds::IDENT
//! [`STATE_BREAKPOINT`]: flags::STATE_BREAKPOINT

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

kw::kw_table! {
    static PYTHON_KWS = [
        (b"False", kinds::KEYWORD),
        (b"None", kinds::KEYWORD),
        (b"True", kinds::KEYWORD),
        (b"and", kinds::KEYWORD),
        (b"as", kinds::KEYWORD),
        (b"assert", kinds::KEYWORD),
        (b"async", kinds::KEYWORD),
        (b"await", kinds::KEYWORD),
        (b"break", kinds::KEYWORD),
        (b"class", kinds::KEYWORD),
        (b"continue", kinds::KEYWORD),
        (b"def", kinds::KEYWORD),
        (b"del", kinds::KEYWORD),
        (b"elif", kinds::KEYWORD),
        (b"else", kinds::KEYWORD),
        (b"except", kinds::KEYWORD),
        (b"finally", kinds::KEYWORD),
        (b"for", kinds::KEYWORD),
        (b"from", kinds::KEYWORD),
        (b"global", kinds::KEYWORD),
        (b"if", kinds::KEYWORD),
        (b"import", kinds::KEYWORD),
        (b"in", kinds::KEYWORD),
        (b"is", kinds::KEYWORD),
        (b"lambda", kinds::KEYWORD),
        (b"nonlocal", kinds::KEYWORD),
        (b"not", kinds::KEYWORD),
        (b"or", kinds::KEYWORD),
        (b"pass", kinds::KEYWORD),
        (b"raise", kinds::KEYWORD),
        (b"return", kinds::KEYWORD),
        (b"try", kinds::KEYWORD),
        (b"while", kinds::KEYWORD),
        (b"with", kinds::KEYWORD),
        (b"yield", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 8; // "continue" / "nonlocal"

pub(crate) struct Python;

impl Lexer for Python {
    fn step_batch(
        view: &mut SourceView<'_>,
        cursor: u32,
        _state: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState) {
        let mut cursor = cursor;
        while !out.is_full() {
            let Some(b) = view.byte_at(cursor) else {
                out.push(LexStep::Eof);
                return (cursor, LexState::INITIAL);
            };

            let start = cursor;
            let (local_kind, end, is_error) = classify(view, cursor, b);
            debug_assert!(end > start, "lexer must consume at least one byte");

            let mut bit_flags = flags::STATE_BREAKPOINT;
            if is_error {
                bit_flags |= flags::IS_ERROR;
            }

            out.push(LexStep::Token {
                len: end - start,
                local_kind,
                state_out: LexState::INITIAL,
                flags: bit_flags,
            });
            cursor = end;
        }
        (cursor, LexState::INITIAL)
    }

    fn is_safe_state(_state: LexState) -> bool {
        true
    }
}

fn classify(view: &mut SourceView<'_>, cursor: u32, first: u8) -> (u16, u32, bool) {
    match first {
        b if byteclass::is_whitespace(b) => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            false,
        ),
        b'#' => (kinds::COMMENT, scan_hash_comment(view, cursor), false),
        b'\'' | b'"' => scan_string_at(view, cursor, 0),
        // Explicit line continuation (`\` at end of line) is trivia; a stray
        // backslash anywhere else is an error.
        b'\\' => match view.byte_at(cursor + 1) {
            Some(b'\n') | Some(b'\r') => (kinds::WHITESPACE, cursor + 1, false),
            _ => (kinds::ERROR, cursor + 1, true),
        },
        b'.' => match view.byte_at(cursor + 1) {
            Some(b) if byteclass::is_digit(b) => {
                let r = scan_py_number(view, cursor);
                (kinds::NUMBER, r.end, r.is_error)
            }
            Some(b'.') if view.byte_at(cursor + 2) == Some(b'.') => {
                (kinds::ELLIPSIS, cursor + 3, false)
            }
            _ => (kinds::DOT, cursor + 1, false),
        },
        b'0'..=b'9' => {
            let r = scan_py_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b'!' => match view.byte_at(cursor + 1) {
            Some(b'=') => (kinds::BANG_EQ, cursor + 2, false),
            _ => (kinds::ERROR, cursor + 1, true),
        },
        // String prefix letters take precedence over identifiers, but only
        // when an actual quote follows a valid prefix.
        b'r' | b'R' | b'b' | b'B' | b'f' | b'F' | b'u' | b'U' => {
            match string_prefix_len(view, cursor) {
                Some(n) => scan_string_at(view, cursor, n),
                None => classify_ident(view, cursor),
            }
        }
        b if byteclass::is_ident_start(b) => classify_ident(view, cursor),
        b'+' | b'-' | b'*' | b'/' | b'%' | b'@' | b'&' | b'|' | b'^' | b'~' | b'<' | b'>'
        | b'=' | b':' | b',' | b';' | b'(' | b')' | b'[' | b']' | b'{' | b'}' => {
            let (kind, end) = scan_operator(view, cursor, first);
            (kind, end, false)
        }
        b if b >= 0x80 => classify_unicode_ident(view, cursor),
        _ => (kinds::ERROR, cursor + 1, true),
    }
}

fn classify_ident(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan::scan_xid_ident(view, cursor, false);
    let len = (end - cursor) as usize;
    if len <= MAX_KW_LEN {
        let mut buf = [0u8; MAX_KW_LEN];
        if scan::copy_bytes(view, cursor, &mut buf[..len]) {
            if let Some(k) = kw_lookup(PYTHON_KWS, &buf[..len]) {
                return (k, end, false);
            }
        }
    }
    (kinds::IDENT, end, false)
}

fn classify_unicode_ident(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    // Non-ASCII start: try a Unicode XID identifier. Python keywords are all
    // ASCII, so a Unicode-starting identifier can never be a keyword.
    let end = scan::scan_xid_ident(view, cursor, false);
    if end > cursor {
        return (kinds::IDENT, end, false);
    }
    let len = scan::decode_char_at(view, cursor)
        .map(|(_, n)| n)
        .unwrap_or(1);
    (kinds::ERROR, cursor + len, true)
}

#[inline]
fn is_quote_byte(b: Option<u8>) -> bool {
    matches!(b, Some(b'\'') | Some(b'"'))
}

/// Returns the byte length (1 or 2) of a valid string prefix at `cursor` when
/// it is immediately followed by a quote, or [`None`] otherwise.
///
/// Recognised prefixes (case-insensitive): `r`, `b`, `f`, `u`, and the
/// two-letter combinations `rb`/`br`/`rf`/`fr`.
fn string_prefix_len(view: &mut SourceView<'_>, cursor: u32) -> Option<u32> {
    let b0 = view.byte_at(cursor)? | 0x20;
    if let Some(b1) = view.byte_at(cursor + 1) {
        let b1 = b1 | 0x20;
        let two_ok = matches!(
            (b0, b1),
            (b'r', b'b') | (b'b', b'r') | (b'r', b'f') | (b'f', b'r')
        );
        if two_ok && is_quote_byte(view.byte_at(cursor + 2)) {
            return Some(2);
        }
    }
    if matches!(b0, b'r' | b'b' | b'f' | b'u') && is_quote_byte(view.byte_at(cursor + 1)) {
        return Some(1);
    }
    None
}

fn scan_string_at(view: &mut SourceView<'_>, cursor: u32, prefix_len: u32) -> (u16, u32, bool) {
    let q = cursor + prefix_len;
    let quote = match view.byte_at(q) {
        Some(b) => b,
        None => return (kinds::STRING, q, true),
    };
    let triple = view.byte_at(q + 1) == Some(quote) && view.byte_at(q + 2) == Some(quote);
    let r = if triple {
        scan_triple_string(view, q, quote)
    } else {
        scan_single_string(view, q, quote)
    };
    (kinds::STRING, r.end, r.is_error)
}

/// Scans a single-line string body starting at the opening quote `cursor`.
///
/// A backslash escapes the next byte (so `\"` and a line-continuation
/// `\`-newline both keep the string open). An unescaped newline or
/// end-of-source terminates the string with an error.
fn scan_single_string(view: &mut SourceView<'_>, cursor: u32, quote: u8) -> ScanResult {
    let mut end = cursor + 1;
    loop {
        match view.byte_at(end) {
            None | Some(b'\n') => {
                return ScanResult {
                    end,
                    is_error: true,
                };
            }
            Some(b'\\') => {
                end += 1;
                if view.byte_at(end).is_some() {
                    end += 1;
                }
            }
            Some(b) if b == quote => {
                return ScanResult {
                    end: end + 1,
                    is_error: false,
                };
            }
            Some(_) => end += 1,
        }
    }
}

/// Scans a triple-quoted string body starting at the first opening quote
/// `cursor`. Newlines are part of the content; only an unescaped run of three
/// matching quotes closes it. End-of-source terminates it with an error.
fn scan_triple_string(view: &mut SourceView<'_>, cursor: u32, quote: u8) -> ScanResult {
    let mut end = cursor + 3;
    loop {
        match view.byte_at(end) {
            None => {
                return ScanResult {
                    end,
                    is_error: true,
                };
            }
            Some(b'\\') => {
                end += 1;
                if view.byte_at(end).is_some() {
                    end += 1;
                }
            }
            Some(b) if b == quote => {
                if view.byte_at(end + 1) == Some(quote) && view.byte_at(end + 2) == Some(quote) {
                    return ScanResult {
                        end: end + 3,
                        is_error: false,
                    };
                }
                end += 1;
            }
            Some(_) => end += 1,
        }
    }
}

fn scan_hash_comment(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut end = cursor + 1;
    loop {
        let (base, page) = view.window_at(end);
        if page.is_empty() {
            return end;
        }
        let mut i = (end - base) as usize;
        while i < page.len() && page[i] != b'\n' {
            i += 1;
        }
        end = base + i as u32;
        if i < page.len() {
            return end;
        }
    }
}

fn is_oct_digit(b: u8) -> bool {
    (b'0'..=b'7').contains(&b)
}

fn is_bin_digit(b: u8) -> bool {
    b == b'0' || b == b'1'
}

/// Consumes a run of digits accepted by `pred`, interspersed with `_`
/// grouping separators, starting at `cursor`.
fn eat_run(view: &mut SourceView<'_>, cursor: u32, pred: fn(u8) -> bool) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let start = (cursor - base) as usize;
        let mut i = start;
        while i < page.len() && (page[i] == b'_' || pred(page[i])) {
            i += 1;
        }
        cursor += (i - start) as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

/// Scans a Python numeric literal starting at `cursor`.
///
/// Recognises decimal, hexadecimal (`0x`), octal (`0o`), and binary (`0b`)
/// integers; decimal floats with fractional and exponent parts; and an
/// optional imaginary `j`/`J` suffix. Underscore separators are accepted
/// leniently. The caller must dispatch here only when the first byte is an
/// ASCII digit, or a `.` followed by a digit.
fn scan_py_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    if view.byte_at(cursor) == Some(b'0') {
        match view.byte_at(cursor + 1) {
            Some(b'x') | Some(b'X') => {
                return scan_radix(view, cursor + 2, byteclass::is_hex_digit);
            }
            Some(b'o') | Some(b'O') => return scan_radix(view, cursor + 2, is_oct_digit),
            Some(b'b') | Some(b'B') => return scan_radix(view, cursor + 2, is_bin_digit),
            _ => {}
        }
    }

    let mut end = eat_run(view, cursor, byteclass::is_digit);

    if view.byte_at(end) == Some(b'.') {
        end += 1;
        end = eat_run(view, end, byteclass::is_digit);
    }

    if matches!(view.byte_at(end), Some(b'e') | Some(b'E')) {
        let mut p = end + 1;
        if matches!(view.byte_at(p), Some(b'+') | Some(b'-')) {
            p += 1;
        }
        if matches!(view.byte_at(p), Some(b) if byteclass::is_digit(b)) {
            end = eat_run(view, p, byteclass::is_digit);
        }
    }

    if matches!(view.byte_at(end), Some(b'j') | Some(b'J')) {
        end += 1;
    }

    ScanResult {
        end,
        is_error: false,
    }
}

fn scan_radix(view: &mut SourceView<'_>, after_prefix: u32, pred: fn(u8) -> bool) -> ScanResult {
    let end = eat_run(view, after_prefix, pred);
    ScanResult {
        end,
        is_error: end == after_prefix,
    }
}

fn scan_operator(view: &mut SourceView<'_>, cursor: u32, b0: u8) -> (u16, u32) {
    let b1 = view.byte_at(cursor + 1);
    let b2 = view.byte_at(cursor + 2);

    let three = match (b0, b1, b2) {
        (b'*', Some(b'*'), Some(b'=')) => Some(kinds::STAR_STAR_EQ),
        (b'/', Some(b'/'), Some(b'=')) => Some(kinds::SLASH_SLASH_EQ),
        (b'<', Some(b'<'), Some(b'=')) => Some(kinds::SHL_EQ),
        (b'>', Some(b'>'), Some(b'=')) => Some(kinds::SHR_EQ),
        _ => None,
    };
    if let Some(k) = three {
        return (k, cursor + 3);
    }

    if let Some(b1) = b1 {
        let two = match (b0, b1) {
            (b'*', b'*') => Some(kinds::STAR_STAR),
            (b'/', b'/') => Some(kinds::SLASH_SLASH),
            (b'<', b'<') => Some(kinds::SHL),
            (b'>', b'>') => Some(kinds::SHR),
            (b'<', b'=') => Some(kinds::LT_EQ),
            (b'>', b'=') => Some(kinds::GT_EQ),
            (b'=', b'=') => Some(kinds::EQ_EQ),
            (b':', b'=') => Some(kinds::COLON_EQ),
            (b'-', b'>') => Some(kinds::THIN_ARROW),
            (b'+', b'=') => Some(kinds::PLUS_EQ),
            (b'-', b'=') => Some(kinds::MINUS_EQ),
            (b'*', b'=') => Some(kinds::STAR_EQ),
            (b'/', b'=') => Some(kinds::SLASH_EQ),
            (b'%', b'=') => Some(kinds::PERCENT_EQ),
            (b'@', b'=') => Some(kinds::AT_EQ),
            (b'&', b'=') => Some(kinds::AMP_EQ),
            (b'|', b'=') => Some(kinds::PIPE_EQ),
            (b'^', b'=') => Some(kinds::CARET_EQ),
            _ => None,
        };
        if let Some(k) = two {
            return (k, cursor + 2);
        }
    }

    let one = match b0 {
        b'+' => kinds::PLUS,
        b'-' => kinds::MINUS,
        b'*' => kinds::STAR,
        b'/' => kinds::SLASH,
        b'%' => kinds::PERCENT,
        b'@' => kinds::AT,
        b'&' => kinds::AMP,
        b'|' => kinds::PIPE,
        b'^' => kinds::CARET,
        b'~' => kinds::TILDE,
        b'<' => kinds::LT,
        b'>' => kinds::GT,
        b'=' => kinds::EQ,
        b':' => kinds::COLON,
        b',' => kinds::COMMA,
        b';' => kinds::SEMI,
        b'(' => kinds::OPEN_PAREN,
        b')' => kinds::CLOSE_PAREN,
        b'[' => kinds::OPEN_BRACKET,
        b']' => kinds::CLOSE_BRACKET,
        b'{' => kinds::OPEN_BRACE,
        b'}' => kinds::CLOSE_BRACE,
        _ => kinds::ERROR,
    };
    (one, cursor + 1)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Source;

    fn run(s: &str) -> Vec<(u16, u32, u32, u8)> {
        let src: &dyn Source = &s;
        let mut view = SourceView::new(src, 0);
        let mut out = StepBuf::new();
        let mut cursor = 0u32;
        let mut state = LexState::INITIAL;
        let mut tokens = Vec::new();
        loop {
            out.clear();
            let before = cursor;
            (cursor, state) = Python::step_batch(&mut view, cursor, state, &mut out);
            let mut pos = before;
            let mut saw_eof = false;
            for step in out.as_slice() {
                match *step {
                    LexStep::Token {
                        len,
                        local_kind,
                        flags,
                        ..
                    } => {
                        tokens.push((local_kind, pos, len, flags));
                        pos += len;
                    }
                    LexStep::Eof => saw_eof = true,
                    LexStep::Descend { .. } => unreachable!("Python has no embedding"),
                }
            }
            if saw_eof {
                return tokens;
            }
        }
    }

    fn kinds_of(s: &str) -> Vec<u16> {
        run(s).iter().map(|t| t.0).collect()
    }

    #[test]
    fn coverage_is_contiguous() {
        let input = "def f(x: int) -> int:\n    return x + 1  # ok\n";
        let tokens = run(input);
        let mut pos = 0u32;
        for (_k, off, len, _f) in &tokens {
            assert_eq!(*off, pos, "gap before {pos}");
            pos += len;
        }
        assert_eq!(pos as usize, input.len());
    }

    #[test]
    fn keyword_vs_ident() {
        assert_eq!(kinds_of("def"), vec![kinds::KEYWORD]);
        assert_eq!(kinds_of("define"), vec![kinds::IDENT]);
        assert_eq!(kinds_of("None"), vec![kinds::KEYWORD]);
        assert_eq!(kinds_of("lambda"), vec![kinds::KEYWORD]);
        // Soft keywords stay identifiers.
        assert_eq!(kinds_of("match"), vec![kinds::IDENT]);
        assert_eq!(kinds_of("case"), vec![kinds::IDENT]);
        assert_eq!(kinds_of("type"), vec![kinds::IDENT]);
    }

    #[test]
    fn prefix_letters_as_idents() {
        // Bare prefix letters and prefixed names are identifiers, not strings,
        // when no quote follows.
        for s in ["r", "b", "f", "u", "for", "from", "break", "bytes", "func"] {
            assert_eq!(run(s).len(), 1, "{s:?}");
            let k = kinds_of(s)[0];
            assert!(k == kinds::IDENT || k == kinds::KEYWORD, "{s:?} -> {k}");
        }
    }

    #[test]
    fn hash_comment() {
        assert_eq!(kinds_of("# a comment"), vec![kinds::COMMENT]);
        assert_eq!(
            kinds_of("x # c\ny"),
            vec![
                kinds::IDENT,
                kinds::WHITESPACE,
                kinds::COMMENT,
                kinds::WHITESPACE,
                kinds::IDENT,
            ]
        );
    }

    #[test]
    fn strings_basic_and_prefixed() {
        for s in [
            r#"'hi'"#,
            r#""hi""#,
            r#"r"raw\n""#,
            r#"b"bytes""#,
            r#"f"f {x}""#,
            r#"rb"rawbytes""#,
            r#"BR"x""#,
            r#"Rf'y'"#,
            r#"u"unicode""#,
        ] {
            let t = run(s);
            assert_eq!(t.len(), 1, "{s:?} -> {t:?}");
            assert_eq!(t[0].0, kinds::STRING, "{s:?}");
            assert_eq!(t[0].2 as usize, s.len(), "{s:?} length");
            assert_eq!(t[0].3 & flags::IS_ERROR, 0, "{s:?} should not error");
        }
    }

    #[test]
    fn triple_quoted_is_one_token() {
        let s = "\"\"\"line 1\nline 2\n'quoted'\nend\"\"\"";
        let t = run(s);
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].2 as usize, s.len());
        assert_eq!(t[0].3 & flags::IS_ERROR, 0);

        // Single-quote triple too.
        let t = run("'''a\nb'''");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
    }

    #[test]
    fn escaped_quotes_in_strings() {
        let t = run(r#""a\"b""#);
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].3 & flags::IS_ERROR, 0);

        // Triple with escaped closing quote.
        let t = run("\"\"\"a\\\"\"\"\"");
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn unterminated_strings_flag_error() {
        for s in [r#""abc"#, "'abc\nrest", "\"\"\"never closed"] {
            let t = run(s);
            assert_eq!(t[0].0, kinds::STRING, "{s:?}");
            assert_ne!(t[0].3 & flags::IS_ERROR, 0, "{s:?} should error");
        }
    }

    #[test]
    fn line_continuation_in_string() {
        // Backslash-newline keeps a single-quoted string open.
        let s = "\"a\\\nb\"";
        let t = run(s);
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].2 as usize, s.len());
        assert_eq!(t[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn numbers() {
        for s in [
            "0",
            "42",
            "1_000",
            "0xFF",
            "0xdead_beef",
            "0o755",
            "0b1010",
            "1.5",
            ".5",
            "1.",
            "1e10",
            "2.5e-3",
            "3j",
            "1.5J",
            "10_000.0",
            "0x_FF",
        ] {
            let t = run(s);
            assert_eq!(t.len(), 1, "{s:?} -> {t:?}");
            assert_eq!(t[0].0, kinds::NUMBER, "{s:?}");
            assert_eq!(t[0].2 as usize, s.len(), "{s:?} length");
            assert_eq!(t[0].3 & flags::IS_ERROR, 0, "{s:?}");
        }
    }

    #[test]
    fn bad_radix_is_error() {
        for s in ["0x", "0o", "0b"] {
            let t = run(s);
            assert_eq!(t[0].0, kinds::NUMBER, "{s:?}");
            assert_ne!(t[0].3 & flags::IS_ERROR, 0, "{s:?} should error");
        }
    }

    #[test]
    fn operators() {
        assert_eq!(kinds_of("//"), vec![kinds::SLASH_SLASH]);
        assert_eq!(kinds_of("//="), vec![kinds::SLASH_SLASH_EQ]);
        assert_eq!(kinds_of("**"), vec![kinds::STAR_STAR]);
        assert_eq!(kinds_of("**="), vec![kinds::STAR_STAR_EQ]);
        assert_eq!(kinds_of(":="), vec![kinds::COLON_EQ]);
        assert_eq!(kinds_of("->"), vec![kinds::THIN_ARROW]);
        assert_eq!(kinds_of("@="), vec![kinds::AT_EQ]);
        assert_eq!(kinds_of("@"), vec![kinds::AT]);
        assert_eq!(kinds_of("!="), vec![kinds::BANG_EQ]);
        assert_eq!(kinds_of("<<="), vec![kinds::SHL_EQ]);
        assert_eq!(kinds_of(">>"), vec![kinds::SHR]);
        assert_eq!(kinds_of("=="), vec![kinds::EQ_EQ]);
        assert_eq!(kinds_of("<="), vec![kinds::LT_EQ]);
    }

    #[test]
    fn lone_bang_and_backslash_are_errors() {
        let t = run("!");
        assert_eq!(t[0].0, kinds::ERROR);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);

        let t = run("\\x");
        assert_eq!(t[0].0, kinds::ERROR);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn ellipsis_and_dot() {
        assert_eq!(kinds_of("..."), vec![kinds::ELLIPSIS]);
        assert_eq!(
            kinds_of("a.b"),
            vec![kinds::IDENT, kinds::DOT, kinds::IDENT]
        );
    }

    #[test]
    fn decorator() {
        assert_eq!(kinds_of("@deco"), vec![kinds::AT, kinds::IDENT]);
    }

    #[test]
    fn unicode_identifier() {
        let t = run("café = 1");
        assert_eq!(t[0].0, kinds::IDENT);
        assert_eq!(t[0].2 as usize, "café".len());

        let t = run("Ω + 1");
        assert_eq!(t[0].0, kinds::IDENT);
        assert_eq!(t[0].2 as usize, "Ω".len());
    }

    #[test]
    fn every_token_has_state_breakpoint() {
        let input = "class A:\n    x = '''doc'''\n    def m(self): return 0xFF\n";
        for t in run(input) {
            assert_ne!(t.3 & flags::STATE_BREAKPOINT, 0, "missing STATE_BREAKPOINT");
        }
    }
}
