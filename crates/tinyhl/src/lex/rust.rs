//! Rust lexer.

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

kw::kw_table! {
    static RUST_KWS = [
        (b"Self", kinds::KEYWORD),
        (b"abstract", kinds::KEYWORD),
        (b"as", kinds::KEYWORD),
        (b"async", kinds::KEYWORD),
        (b"await", kinds::KEYWORD),
        (b"become", kinds::KEYWORD),
        (b"box", kinds::KEYWORD),
        (b"break", kinds::KEYWORD),
        (b"const", kinds::KEYWORD),
        (b"continue", kinds::KEYWORD),
        (b"crate", kinds::KEYWORD),
        (b"do", kinds::KEYWORD),
        (b"dyn", kinds::KEYWORD),
        (b"else", kinds::KEYWORD),
        (b"enum", kinds::KEYWORD),
        (b"extern", kinds::KEYWORD),
        (b"false", kinds::KEYWORD),
        (b"final", kinds::KEYWORD),
        (b"fn", kinds::KEYWORD),
        (b"for", kinds::KEYWORD),
        (b"if", kinds::KEYWORD),
        (b"impl", kinds::KEYWORD),
        (b"in", kinds::KEYWORD),
        (b"let", kinds::KEYWORD),
        (b"loop", kinds::KEYWORD),
        (b"macro", kinds::KEYWORD),
        (b"match", kinds::KEYWORD),
        (b"mod", kinds::KEYWORD),
        (b"move", kinds::KEYWORD),
        (b"mut", kinds::KEYWORD),
        (b"override", kinds::KEYWORD),
        (b"priv", kinds::KEYWORD),
        (b"pub", kinds::KEYWORD),
        (b"ref", kinds::KEYWORD),
        (b"return", kinds::KEYWORD),
        (b"self", kinds::KEYWORD),
        (b"static", kinds::KEYWORD),
        (b"struct", kinds::KEYWORD),
        (b"super", kinds::KEYWORD),
        (b"trait", kinds::KEYWORD),
        (b"true", kinds::KEYWORD),
        (b"try", kinds::KEYWORD),
        (b"type", kinds::KEYWORD),
        (b"typeof", kinds::KEYWORD),
        (b"union", kinds::KEYWORD),
        (b"unsafe", kinds::KEYWORD),
        (b"unsized", kinds::KEYWORD),
        (b"use", kinds::KEYWORD),
        (b"virtual", kinds::KEYWORD),
        (b"where", kinds::KEYWORD),
        (b"while", kinds::KEYWORD),
        (b"yield", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 8;

#[inline]
fn scan_rust_ident(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    scan::scan_xid_ident(view, cursor, false)
}

pub(crate) struct Rust;

impl Lexer for Rust {
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
        b' ' | b'\t' | b'\n' | b'\r' | 0x0B | 0x0C => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            false,
        ),
        b'/' => match view.byte_at(cursor + 1) {
            Some(b'/') => scan_line_comment(view, cursor),
            Some(b'*') => scan_block_comment(view, cursor),
            _ => (kinds::SLASH, cursor + 1, false),
        },
        b'"' => {
            let r = scan::scan_c_string(view, cursor);
            let end = eat_suffix(view, r.end);
            (kinds::STRING, end, r.is_error)
        }
        b'\'' => scan_char_or_lifetime(view, cursor),
        b'0'..=b'9' => {
            let r = scan_rust_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b'r' => scan_r_prefix(view, cursor),
        b'b' => scan_b_prefix(view, cursor),
        b'c' => scan_c_prefix(view, cursor),
        b if byteclass::is_ident_start(b) => scan_ident_or_keyword(view, cursor),
        b'{' => (kinds::OPEN_BRACE, cursor + 1, false),
        b'}' => (kinds::CLOSE_BRACE, cursor + 1, false),
        b'(' => (kinds::OPEN_PAREN, cursor + 1, false),
        b')' => (kinds::CLOSE_PAREN, cursor + 1, false),
        b'[' => (kinds::OPEN_BRACKET, cursor + 1, false),
        b']' => (kinds::CLOSE_BRACKET, cursor + 1, false),
        b',' => (kinds::COMMA, cursor + 1, false),
        b';' => (kinds::SEMI, cursor + 1, false),
        b':' => (kinds::COLON, cursor + 1, false),
        b'.' => (kinds::DOT, cursor + 1, false),
        b'?' => (kinds::QUESTION, cursor + 1, false),
        b'@' => (kinds::AT, cursor + 1, false),
        b'#' => (kinds::HASH, cursor + 1, false),
        b'~' => (kinds::TILDE, cursor + 1, false),
        b'$' => (kinds::DOLLAR, cursor + 1, false),
        b'=' => (kinds::EQ, cursor + 1, false),
        b'!' => (kinds::BANG, cursor + 1, false),
        b'<' => (kinds::LT, cursor + 1, false),
        b'>' => (kinds::GT, cursor + 1, false),
        b'-' => (kinds::MINUS, cursor + 1, false),
        b'&' => (kinds::AMP, cursor + 1, false),
        b'|' => (kinds::PIPE, cursor + 1, false),
        b'+' => (kinds::PLUS, cursor + 1, false),
        b'*' => (kinds::STAR, cursor + 1, false),
        b'^' => (kinds::CARET, cursor + 1, false),
        b'%' => (kinds::PERCENT, cursor + 1, false),
        b if b >= 0x80 => classify_unicode_ident(view, cursor),
        _ => (kinds::ERROR, cursor + 1, true),
    }
}

fn classify_unicode_ident(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    // Non-ASCII start: try a Unicode XID identifier. Rust keywords are all
    // ASCII, so a Unicode-starting identifier cannot be a keyword and the
    // shared keyword lookup is skipped.
    let end = scan_rust_ident(view, cursor);
    if end > cursor {
        return (kinds::IDENT, end, false);
    }
    // Not a valid XID_Start — advance by one full UTF-8 char so the lexer
    // makes forward progress without stranding continuation bytes.
    let len = scan::decoded_len_or_one(view, cursor);
    (kinds::ERROR, cursor + len, true)
}

fn scan_line_comment(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let mut end = cursor + 2;
    let is_doc = match view.byte_at(end) {
        Some(b'!') => true,
        Some(b'/') => view.byte_at(end + 1) != Some(b'/'),
        _ => false,
    };
    loop {
        let (base, page) = view.window_at(end);
        if page.is_empty() {
            break;
        }
        let mut i = (end - base) as usize;
        let start = i;
        while i < page.len() && page[i] != b'\n' {
            i += 1;
        }
        end = base + i as u32;
        if i < page.len() || i == start {
            break;
        }
    }
    let kind = if is_doc {
        kinds::DOC_COMMENT
    } else {
        kinds::COMMENT
    };
    (kind, end, false)
}

fn scan_block_comment(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = cursor + 2;
    let is_doc = match view.byte_at(end) {
        Some(b'!') => true,
        Some(b'*') => !matches!(view.byte_at(end + 1), Some(b'*') | Some(b'/')),
        _ => false,
    };
    let kind = if is_doc {
        kinds::DOC_COMMENT
    } else {
        kinds::COMMENT
    };

    let r = scan::scan_nested_block_comment(view, cursor);
    (kind, r.end, r.is_error)
}

fn scan_char_or_lifetime(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let c1 = view.byte_at(cursor + 1);
    let c2 = view.byte_at(cursor + 2);

    let can_be_lifetime = match (c1, c2) {
        (_, Some(b'\'')) => false,
        (Some(b), _) if byteclass::is_ident_start(b) || byteclass::is_digit(b) => true,
        _ => false,
    };

    if !can_be_lifetime {
        return scan_char_literal(view, cursor);
    }

    // Raw lifetime: 'r#ident.
    if c1 == Some(b'r') && c2 == Some(b'#') {
        let end = scan_rust_ident(view, cursor + 3);
        if end > cursor + 3 {
            return (kinds::LIFETIME, end, false);
        }
    }

    let mut end = cursor + 1;
    // Consume first character unconditionally (may be a digit).
    end += 1;
    while let Some(b) = view.byte_at(end) {
        if byteclass::is_ident_cont(b) {
            end += 1;
        } else {
            break;
        }
    }
    if view.byte_at(end) == Some(b'\'') {
        end += 1;
        end = eat_suffix(view, end);
        return (kinds::CHAR, end, false);
    }
    (kinds::LIFETIME, end, false)
}

fn scan_char_literal(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let mut end = cursor + 1;
    let first = view.byte_at(end);
    let second = view.byte_at(end + 1);
    if matches!(second, Some(b'\'')) && !matches!(first, Some(b'\\')) && first.is_some() {
        end += 2;
        end = eat_suffix(view, end);
        return (kinds::CHAR, end, false);
    }
    loop {
        match view.byte_at(end) {
            Some(b'\'') => {
                end += 1;
                end = eat_suffix(view, end);
                return (kinds::CHAR, end, false);
            }
            Some(b'\n') => return (kinds::CHAR, end, true),
            Some(b'\\') => {
                end += 1;
                if view.byte_at(end).is_some() {
                    end += 1;
                }
            }
            Some(_) => end += 1,
            None => return (kinds::CHAR, end, true),
        }
    }
}

fn scan_r_prefix(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    match (view.byte_at(cursor + 1), view.byte_at(cursor + 2)) {
        (Some(b'#'), _) => {
            let body = scan_rust_ident(view, cursor + 2);
            if body > cursor + 2 {
                return (kinds::IDENT, body, false);
            }
            let r = scan_raw_string(view, cursor + 1);
            let end = eat_suffix(view, r.end);
            (kinds::STRING, end, r.is_error)
        }
        (Some(b'"'), _) => {
            let r = scan_raw_string(view, cursor + 1);
            let end = eat_suffix(view, r.end);
            (kinds::STRING, end, r.is_error)
        }
        _ => scan_ident_or_keyword(view, cursor),
    }
}

fn scan_b_prefix(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    match (view.byte_at(cursor + 1), view.byte_at(cursor + 2)) {
        (Some(b'\''), _) => {
            let (_, end, err) = scan_char_literal(view, cursor + 1);
            (kinds::STRING, end, err)
        }
        (Some(b'"'), _) => {
            let r = scan::scan_c_string(view, cursor + 1);
            let end = eat_suffix(view, r.end);
            (kinds::STRING, end, r.is_error)
        }
        (Some(b'r'), Some(b'"')) | (Some(b'r'), Some(b'#')) => {
            let r = scan_raw_string(view, cursor + 2);
            let end = eat_suffix(view, r.end);
            (kinds::STRING, end, r.is_error)
        }
        _ => scan_ident_or_keyword(view, cursor),
    }
}

fn scan_c_prefix(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    match (view.byte_at(cursor + 1), view.byte_at(cursor + 2)) {
        (Some(b'"'), _) => {
            let r = scan::scan_c_string(view, cursor + 1);
            let end = eat_suffix(view, r.end);
            (kinds::STRING, end, r.is_error)
        }
        (Some(b'r'), Some(b'"')) | (Some(b'r'), Some(b'#')) => {
            let r = scan_raw_string(view, cursor + 2);
            let end = eat_suffix(view, r.end);
            (kinds::STRING, end, r.is_error)
        }
        _ => scan_ident_or_keyword(view, cursor),
    }
}

fn scan_raw_string(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor;
    let mut n_hashes: u32 = 0;
    while view.byte_at(end) == Some(b'#') {
        n_hashes += 1;
        end += 1;
    }
    if view.byte_at(end) != Some(b'"') {
        return ScanResult {
            end,
            is_error: true,
        };
    }
    end += 1;
    loop {
        match view.byte_at(end) {
            None => {
                return ScanResult {
                    end,
                    is_error: true,
                };
            }
            Some(b'"') => {
                end += 1;
                let mut closing: u32 = 0;
                while closing < n_hashes && view.byte_at(end) == Some(b'#') {
                    closing += 1;
                    end += 1;
                }
                if closing == n_hashes {
                    return ScanResult {
                        end,
                        is_error: false,
                    };
                }
            }
            Some(_) => end += 1,
        }
    }
}

fn scan_rust_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor;
    let first = view.byte_at(end).unwrap();
    end += 1;
    let mut has_error = false;

    let mut consumed_body = true;
    if first == b'0' {
        match view.byte_at(end) {
            Some(b'b') | Some(b'o') => {
                end += 1;
                let before = end;
                end = eat_decimal_digits(view, end);
                if end == before {
                    has_error = true;
                }
                consumed_body = false;
            }
            Some(b'x') => {
                end += 1;
                let before = end;
                end = eat_hex_digits(view, end);
                if end == before {
                    has_error = true;
                }
                consumed_body = false;
            }
            Some(b) if b == b'_' || byteclass::is_digit(b) => {
                end = eat_decimal_digits(view, end);
            }
            _ => {}
        }
    } else {
        end = eat_decimal_digits(view, end);
    }

    if consumed_body {
        match view.byte_at(end) {
            Some(b'.') => {
                let next = view.byte_at(end + 1);
                let eat_dot = !matches!(next, Some(b'.'))
                    && !matches!(next, Some(b) if byteclass::is_ident_start(b));
                if eat_dot {
                    end += 1;
                    if matches!(view.byte_at(end), Some(b'0'..=b'9')) {
                        end = eat_decimal_digits(view, end);
                        if matches!(view.byte_at(end), Some(b'e') | Some(b'E')) {
                            end += 1;
                            if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
                                end += 1;
                            }
                            let before = end;
                            end = eat_decimal_digits(view, end);
                            if end == before {
                                has_error = true;
                            }
                        }
                    }
                }
            }
            Some(b'e') | Some(b'E') => {
                end += 1;
                if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
                    end += 1;
                }
                let before = end;
                end = eat_decimal_digits(view, end);
                if end == before {
                    has_error = true;
                }
            }
            _ => {}
        }
    }

    end = eat_suffix(view, end);
    ScanResult {
        end,
        is_error: has_error,
    }
}

fn eat_decimal_digits(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut end = cursor;
    loop {
        let (base, page) = view.window_at(end);
        if page.is_empty() {
            return end;
        }
        let mut i = (end - base) as usize;
        while i < page.len() && (page[i] == b'_' || byteclass::is_digit(page[i])) {
            i += 1;
        }
        end = base + i as u32;
        if i < page.len() {
            return end;
        }
    }
}

fn eat_hex_digits(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut end = cursor;
    loop {
        let (base, page) = view.window_at(end);
        if page.is_empty() {
            return end;
        }
        let mut i = (end - base) as usize;
        while i < page.len() && (page[i] == b'_' || byteclass::is_hex_digit(page[i])) {
            i += 1;
        }
        end = base + i as u32;
        if i < page.len() {
            return end;
        }
    }
}

fn eat_suffix(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    scan_rust_ident(view, cursor)
}

fn scan_ident_or_keyword(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan_rust_ident(view, cursor);
    let len = (end - cursor) as usize;
    if len <= MAX_KW_LEN {
        let mut buf = [0u8; MAX_KW_LEN];
        if scan::copy_bytes(view, cursor, &mut buf[..len]) {
            if let Some(k) = kw_lookup(RUST_KWS, &buf[..len]) {
                return (k, end, false);
            }
        }
    }
    (kinds::IDENT, end, false)
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
            (cursor, state) = Rust::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("Rust has no embedding"),
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
        let input = r#"fn main() { let x: i32 = 42; println!("hi {x}"); }"#;
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
        assert_eq!(kinds_of("fn"), vec![kinds::KEYWORD]);
        assert_eq!(kinds_of("fna"), vec![kinds::IDENT]);
        assert_eq!(kinds_of("Self"), vec![kinds::KEYWORD]);
        assert_eq!(kinds_of("selfish"), vec![kinds::IDENT]);
    }

    #[test]
    fn line_and_doc_comments() {
        assert_eq!(kinds_of("// hi"), vec![kinds::COMMENT]);
        assert_eq!(kinds_of("//! inner"), vec![kinds::DOC_COMMENT]);
        assert_eq!(kinds_of("/// outer"), vec![kinds::DOC_COMMENT]);
        assert_eq!(kinds_of("//// four"), vec![kinds::COMMENT]);
    }

    #[test]
    fn block_comments_nested() {
        assert_eq!(kinds_of("/* a /* b */ c */"), vec![kinds::COMMENT]);
        assert_eq!(kinds_of("/** outer */"), vec![kinds::DOC_COMMENT]);
        assert_eq!(kinds_of("/*! inner */"), vec![kinds::DOC_COMMENT]);
        let t = run("/* unterminated");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::COMMENT);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn strings_and_prefixes() {
        assert_eq!(kinds_of(r#""hi""#), vec![kinds::STRING]);
        assert_eq!(kinds_of(r#"b"hi""#), vec![kinds::STRING]);
        assert_eq!(kinds_of(r#"c"hi""#), vec![kinds::STRING]);
        assert_eq!(kinds_of(r##"r#"raw"#"##), vec![kinds::STRING]);
        assert_eq!(kinds_of(r##"br#"raw"#"##), vec![kinds::STRING]);
        assert_eq!(kinds_of(r#""a\"b""#), vec![kinds::STRING]);
    }

    #[test]
    fn multiline_strings_are_one_token() {
        // Regular string: newlines are literal content, not a terminator.
        let t = run("\"line 1\nline 2\nline 3\"");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].3 & flags::IS_ERROR, 0);

        // Line continuation: `\` followed by newline — the `\n` must not
        // terminate the string and must not be misread as an escape pair.
        let s = "\"a\\\nb\"";
        let t = run(s);
        assert_eq!(t.len(), 1, "tokens for {s:?}: {t:?}");
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].2 as usize, s.len());

        // Escaped closing quote spanning two lines.
        let s = "\"first\\\n\"second\"";
        let t = run(s);
        // `"first\<newline>"` then `second` ident then `"` unterminated.
        assert!(t.iter().any(|(k, _, _, _)| *k == kinds::STRING));

        // Raw string with embedded newlines.
        let t = run("r\"line 1\nline 2\"");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].3 & flags::IS_ERROR, 0);

        // Hashed raw string with embedded newlines and a bare `"`.
        let t = run("r#\"one\n\"two\n\"#");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].3 & flags::IS_ERROR, 0);

        // Unterminated multi-line string — whole tail becomes a single
        // error-flagged STRING, followed by Eof only.
        let s = "\"open\nmiddle\nstill open";
        let t = run(s);
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);
        assert_eq!(t[0].2 as usize, s.len());
    }

    #[test]
    fn chars_and_lifetimes() {
        assert_eq!(kinds_of("'a'"), vec![kinds::CHAR]);
        assert_eq!(kinds_of("'\\n'"), vec![kinds::CHAR]);
        assert_eq!(kinds_of("'a"), vec![kinds::LIFETIME]);
        assert_eq!(kinds_of("'static"), vec![kinds::LIFETIME]);
        // Multi-char char literal (syntax error at parse time but lexes as CHAR).
        assert_eq!(kinds_of("'abc'"), vec![kinds::CHAR]);
    }

    #[test]
    fn raw_identifier() {
        let t = run("r#fn");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::IDENT);
    }

    #[test]
    fn numbers_basic() {
        assert_eq!(kinds_of("0"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("42"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("0x1F"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("0b1010"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("0o77"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("1.5"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("1_000_000u32"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("1e10"), vec![kinds::NUMBER]);
        assert_eq!(kinds_of("2.5E-3"), vec![kinds::NUMBER]);
    }

    #[test]
    fn number_dot_method_not_float() {
        // 1.foo() — the `1` should be a number, `.` DOT, `foo` ident.
        let ks = kinds_of("1.foo");
        assert_eq!(ks, vec![kinds::NUMBER, kinds::DOT, kinds::IDENT]);
    }

    #[test]
    fn range_not_float() {
        let ks = kinds_of("0..5");
        assert_eq!(ks.first(), Some(&kinds::NUMBER));
        assert!(ks.contains(&kinds::DOT));
    }

    #[test]
    fn error_on_non_ascii_byte() {
        let t = run("§");
        // `§` is Unicode category Po — not XID_Start, so it must error. The
        // whole 2-byte UTF-8 sequence is consumed as a single ERROR token.
        assert!(
            t.iter()
                .any(|(k, _, _, f)| *k == kinds::ERROR && *f & flags::IS_ERROR != 0)
        );
    }

    #[test]
    fn unicode_identifier() {
        let t = run("let αβγ = 1;");
        let idents: Vec<_> = t.iter().filter(|(k, _, _, _)| *k == kinds::IDENT).collect();
        assert_eq!(idents.len(), 1);
        assert_eq!(idents[0].2 as usize, "αβγ".len());
    }

    #[test]
    fn unicode_identifier_at_source_start() {
        // Starting with a non-ASCII XID_Start should not take the error path.
        let t = run("пример + 1");
        assert_eq!(t[0].0, kinds::IDENT);
        assert_eq!(t[0].2 as usize, "пример".len());
    }
}
