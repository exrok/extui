//! TOML lexer.

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

kw::kw_table! {
    static TOML_KWS = [
        (b"false", kinds::KEYWORD),
        (b"inf",   kinds::KEYWORD),
        (b"nan",   kinds::KEYWORD),
        (b"true",  kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 5;

pub(crate) struct Toml;

impl Lexer for Toml {
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
        b' ' | b'\t' | b'\n' | b'\r' => {
            let end = scan::scan_whitespace(view, cursor);
            (kinds::WHITESPACE, end, false)
        }
        b'#' => {
            let end = scan_toml_hash_comment(view, cursor);
            (kinds::COMMENT, end, false)
        }
        b'"' => {
            if view.byte_at(cursor + 1) == Some(b'"') && view.byte_at(cursor + 2) == Some(b'"') {
                let r = scan_toml_ml_basic_string(view, cursor);
                (kinds::STRING, r.end, r.is_error)
            } else {
                let r = scan_toml_basic_string(view, cursor);
                (kinds::STRING, r.end, r.is_error)
            }
        }
        b'\'' => {
            if view.byte_at(cursor + 1) == Some(b'\'') && view.byte_at(cursor + 2) == Some(b'\'') {
                let r = scan_toml_ml_literal_string(view, cursor);
                (kinds::STRING, r.end, r.is_error)
            } else {
                let r = scan_toml_literal_string(view, cursor);
                (kinds::STRING, r.end, r.is_error)
            }
        }
        b'=' => (kinds::EQ, cursor + 1, false),
        b'.' => (kinds::DOT, cursor + 1, false),
        b',' => (kinds::COMMA, cursor + 1, false),
        b'[' => (kinds::OPEN_BRACKET, cursor + 1, false),
        b']' => (kinds::CLOSE_BRACKET, cursor + 1, false),
        b'{' => (kinds::OPEN_BRACE, cursor + 1, false),
        b'}' => (kinds::CLOSE_BRACE, cursor + 1, false),
        b'0'..=b'9' => {
            let r = scan_toml_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b'+' | b'-' => match view.byte_at(cursor + 1) {
            Some(b) if byteclass::is_digit(b) => {
                let r = scan_toml_number(view, cursor);
                (kinds::NUMBER, r.end, r.is_error)
            }
            Some(b'i') if try_match_word(view, cursor + 1, b"inf") => {
                (kinds::KEYWORD, cursor + 4, false)
            }
            Some(b'n') if try_match_word(view, cursor + 1, b"nan") => {
                (kinds::KEYWORD, cursor + 4, false)
            }
            _ => (kinds::ERROR, cursor + 1, true),
        },
        b if byteclass::is_ident_start(b) => {
            let end = scan_toml_bare_key(view, cursor);
            let len = (end - cursor) as usize;
            if len <= MAX_KW_LEN {
                let mut buf = [0u8; MAX_KW_LEN];
                if scan::copy_bytes(view, cursor, &mut buf[..len]) {
                    if let Some(k) = kw_lookup(TOML_KWS, &buf[..len]) {
                        return (k, end, false);
                    }
                }
            }
            (kinds::IDENT, end, false)
        }
        _ => (kinds::ERROR, cursor + 1, true),
    }
}

fn try_match_word(view: &mut SourceView<'_>, start: u32, word: &[u8]) -> bool {
    for (i, &b) in word.iter().enumerate() {
        if view.byte_at(start + i as u32) != Some(b) {
            return false;
        }
    }
    match view.byte_at(start + word.len() as u32) {
        Some(b) if is_bare_key_cont(b) => false,
        _ => true,
    }
}

fn is_bare_key_cont(b: u8) -> bool {
    byteclass::is_ident_cont(b) || b == b'-'
}

fn is_number_cont(b: u8) -> bool {
    matches!(
        b,
        b'0'..=b'9' | b'_' | b'.' | b'+' | b'-' | b':' | b'T' | b't' | b'Z' | b'z' | b'e' | b'E'
    )
}

fn scan_toml_hash_comment(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor + 1;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let rel = (cursor - base) as usize;
        let mut i = rel;
        while i < page.len() && page[i] != b'\n' {
            i += 1;
        }
        cursor = base + i as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

fn scan_toml_bare_key(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let rel = (cursor - base) as usize;
        let mut i = rel;
        while i < page.len() && is_bare_key_cont(page[i]) {
            i += 1;
        }
        let consumed = i - rel;
        cursor += consumed as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

fn scan_toml_basic_string(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut cursor = cursor + 1;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return ScanResult {
                end: cursor,
                is_error: true,
            };
        }
        let rel = (cursor - base) as usize;
        let mut i = rel;
        let mut escape_off_page = false;
        while i < page.len() {
            let b = page[i];
            if b == b'"' {
                return ScanResult {
                    end: base + i as u32 + 1,
                    is_error: false,
                };
            }
            if b == b'\n' || b == b'\r' {
                return ScanResult {
                    end: base + i as u32,
                    is_error: true,
                };
            }
            if b == b'\\' {
                if i + 1 < page.len() {
                    i += 2;
                    continue;
                }
                i += 1;
                escape_off_page = true;
                break;
            }
            i += 1;
        }
        cursor = base + i as u32;
        if escape_off_page {
            match view.byte_at(cursor) {
                None => {
                    return ScanResult {
                        end: cursor,
                        is_error: true,
                    };
                }
                Some(b'\n') | Some(b'\r') => {
                    return ScanResult {
                        end: cursor,
                        is_error: true,
                    };
                }
                Some(_) => cursor += 1,
            }
            continue;
        }
        if i == rel {
            return ScanResult {
                end: cursor,
                is_error: true,
            };
        }
    }
}

fn scan_toml_literal_string(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut cursor = cursor + 1;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return ScanResult {
                end: cursor,
                is_error: true,
            };
        }
        let rel = (cursor - base) as usize;
        let mut i = rel;
        while i < page.len() {
            let b = page[i];
            if b == b'\'' {
                return ScanResult {
                    end: base + i as u32 + 1,
                    is_error: false,
                };
            }
            if b == b'\n' || b == b'\r' {
                return ScanResult {
                    end: base + i as u32,
                    is_error: true,
                };
            }
            i += 1;
        }
        cursor = base + i as u32;
        if i == rel {
            return ScanResult {
                end: cursor,
                is_error: true,
            };
        }
    }
}

fn scan_toml_ml_basic_string(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    scan_toml_ml_string(view, cursor + 3, b'"', true)
}

fn scan_toml_ml_literal_string(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    scan_toml_ml_string(view, cursor + 3, b'\'', false)
}

fn scan_toml_ml_string(
    view: &mut SourceView<'_>,
    start: u32,
    quote: u8,
    has_escapes: bool,
) -> ScanResult {
    let mut cursor = start;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return ScanResult {
                end: cursor,
                is_error: true,
            };
        }
        let rel = (cursor - base) as usize;
        let mut i = rel;
        let mut escape_off_page = false;
        let mut quote_run_off_page: u32 = 0;
        while i < page.len() {
            let b = page[i];
            if has_escapes && b == b'\\' {
                if i + 1 < page.len() {
                    i += 2;
                    continue;
                }
                i += 1;
                escape_off_page = true;
                break;
            }
            if b == quote {
                let run_start = i;
                while i < page.len() && page[i] == quote {
                    i += 1;
                }
                let run = (i - run_start) as u32;
                if i < page.len() {
                    if run >= 3 {
                        return ScanResult {
                            end: base + i as u32,
                            is_error: false,
                        };
                    }
                    continue;
                }
                quote_run_off_page = run;
                break;
            }
            i += 1;
        }
        cursor = base + i as u32;
        if escape_off_page {
            match view.byte_at(cursor) {
                None => {
                    return ScanResult {
                        end: cursor,
                        is_error: true,
                    };
                }
                Some(_) => cursor += 1,
            }
            continue;
        }
        if quote_run_off_page > 0 {
            let mut run = quote_run_off_page;
            while view.byte_at(cursor) == Some(quote) {
                cursor += 1;
                run += 1;
            }
            if run >= 3 {
                return ScanResult {
                    end: cursor,
                    is_error: false,
                };
            }
            continue;
        }
    }
}

fn scan_toml_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor;

    if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
        end += 1;
    }

    match view.byte_at(end) {
        Some(b) if byteclass::is_digit(b) => {}
        _ => {
            return ScanResult {
                end: end.max(cursor + 1),
                is_error: true,
            };
        }
    }

    if view.byte_at(end) == Some(b'0') {
        match view.byte_at(end + 1) {
            Some(b'x') | Some(b'X') => {
                let after_prefix = end + 2;
                let run_end = scan_radix_run(view, after_prefix, RADIX_HEX);
                if run_end == after_prefix {
                    return ScanResult {
                        end: after_prefix,
                        is_error: true,
                    };
                }
                return ScanResult {
                    end: run_end,
                    is_error: false,
                };
            }
            Some(b'o') | Some(b'O') => {
                let after_prefix = end + 2;
                let run_end = scan_radix_run(view, after_prefix, RADIX_OCT);
                if run_end == after_prefix {
                    return ScanResult {
                        end: after_prefix,
                        is_error: true,
                    };
                }
                return ScanResult {
                    end: run_end,
                    is_error: false,
                };
            }
            Some(b'b') | Some(b'B') => {
                let after_prefix = end + 2;
                let run_end = scan_radix_run(view, after_prefix, RADIX_BIN);
                if run_end == after_prefix {
                    return ScanResult {
                        end: after_prefix,
                        is_error: true,
                    };
                }
                return ScanResult {
                    end: run_end,
                    is_error: false,
                };
            }
            _ => {}
        }
    }

    loop {
        let (base, page) = view.window_at(end);
        if page.is_empty() {
            break;
        }
        let rel = (end - base) as usize;
        let mut i = rel;
        while i < page.len() && is_number_cont(page[i]) {
            i += 1;
        }
        let consumed = i - rel;
        end += consumed as u32;
        if i < page.len() {
            break;
        }
    }

    ScanResult {
        end,
        is_error: false,
    }
}

const RADIX_HEX: u8 = 16;
const RADIX_OCT: u8 = 8;
const RADIX_BIN: u8 = 2;

fn is_radix_digit(b: u8, radix: u8) -> bool {
    match radix {
        RADIX_HEX => byteclass::is_hex_digit(b),
        RADIX_OCT => matches!(b, b'0'..=b'7'),
        RADIX_BIN => matches!(b, b'0' | b'1'),
        _ => false,
    }
}

fn scan_radix_run(view: &mut SourceView<'_>, cursor: u32, radix: u8) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let rel = (cursor - base) as usize;
        let mut i = rel;
        while i < page.len() && (is_radix_digit(page[i], radix) || page[i] == b'_') {
            i += 1;
        }
        let consumed = i - rel;
        cursor += consumed as u32;
        if i < page.len() {
            return cursor;
        }
    }
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
            (cursor, state) = Toml::step_batch(&mut view, cursor, state, &mut out);
            let mut saw_eof = false;
            let mut pos = cursor
                - out
                    .as_slice()
                    .iter()
                    .map(|s| match s {
                        LexStep::Token { len, .. } => *len,
                        _ => 0,
                    })
                    .sum::<u32>();
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
                    LexStep::Descend { .. } => unreachable!("TOML has no embedding"),
                }
            }
            if saw_eof {
                return tokens;
            }
        }
    }

    fn kinds_of(tokens: &[(u16, u32, u32, u8)]) -> Vec<u16> {
        tokens.iter().map(|t| t.0).collect()
    }

    #[test]
    fn simple_kv() {
        let tokens = run(r#"key = "value""#);
        assert_eq!(
            kinds_of(&tokens),
            vec![
                kinds::IDENT,
                kinds::WHITESPACE,
                kinds::EQ,
                kinds::WHITESPACE,
                kinds::STRING,
            ]
        );
    }

    #[test]
    fn keys_with_dash_and_digit() {
        let tokens = run("foo-bar = 1\n2key = 2\n");
        let k = kinds_of(&tokens);
        assert!(k.contains(&kinds::IDENT));
        assert!(k.contains(&kinds::NUMBER));
    }

    #[test]
    fn numbers_basic() {
        for input in ["0", "42", "-17", "+99", "1_000_000"] {
            let t = run(input);
            assert_eq!(t.len(), 1, "input {input:?}");
            assert_eq!(t[0].0, kinds::NUMBER, "input {input:?}");
            assert_eq!(t[0].2 as usize, input.len(), "input {input:?}");
        }
    }

    #[test]
    fn numbers_radix() {
        for input in ["0xDEADBEEF", "0xff_ff", "0o755", "0b1010_1100"] {
            let t = run(input);
            assert_eq!(t.len(), 1, "input {input:?}");
            assert_eq!(t[0].0, kinds::NUMBER, "input {input:?}");
            assert_eq!(t[0].2 as usize, input.len(), "input {input:?}");
        }
    }

    #[test]
    fn numbers_float() {
        for input in ["3.14", "1e10", "1.5e-3", "-0.001", "+1.0"] {
            let t = run(input);
            assert_eq!(t.len(), 1, "input {input:?}");
            assert_eq!(t[0].0, kinds::NUMBER, "input {input:?}");
        }
    }

    #[test]
    fn dates_fold_into_number() {
        for input in [
            "2021-01-15",
            "07:32:00",
            "07:32:00.123",
            "2021-01-15T07:32:00Z",
            "1979-05-27T00:32:00-07:00",
        ] {
            let t = run(input);
            assert_eq!(t.len(), 1, "input {input:?}");
            assert_eq!(t[0].0, kinds::NUMBER, "input {input:?}");
            assert_eq!(t[0].2 as usize, input.len(), "input {input:?}");
        }
    }

    #[test]
    fn keywords() {
        let tokens = run("true false inf nan");
        assert_eq!(
            kinds_of(&tokens),
            vec![
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::KEYWORD,
            ]
        );
    }

    #[test]
    fn signed_inf_nan() {
        let tokens = run("+inf -nan +nan -inf");
        let k = kinds_of(&tokens);
        assert_eq!(
            k,
            vec![
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::KEYWORD,
            ]
        );
    }

    #[test]
    fn strings_basic() {
        for (input, expected_len) in [
            (r#""""#, 2),
            (r#""abc""#, 5),
            (r#""a\"b""#, 6),
            (r#""\n""#, 4),
            (r#""\uABCD""#, 8),
        ] {
            let t = run(input);
            assert_eq!(t.len(), 1, "input {input:?}");
            assert_eq!(t[0].0, kinds::STRING, "input {input:?}");
            assert_eq!(t[0].2, expected_len as u32, "input {input:?}");
            assert_eq!(t[0].3 & flags::IS_ERROR, 0, "input {input:?}");
        }
    }

    #[test]
    fn strings_literal() {
        let t = run("'raw\\nno'");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].2 as usize, "'raw\\nno'".len());
    }

    #[test]
    fn strings_ml_basic() {
        for input in [
            r#""""""""#,
            "\"\"\"abc\"\"\"",
            "\"\"\"a\nb\nc\"\"\"",
            "\"\"\"has \\\"quote\\\" inside\"\"\"",
        ] {
            let t = run(input);
            assert_eq!(t.len(), 1, "input {input:?}");
            assert_eq!(t[0].0, kinds::STRING, "input {input:?}");
            assert_eq!(t[0].2 as usize, input.len(), "input {input:?}");
            assert_eq!(t[0].3 & flags::IS_ERROR, 0, "input {input:?}");
        }
    }

    #[test]
    fn strings_ml_literal() {
        for input in ["'''abc'''", "'''a\nb'''", "'''with \"quotes\"'''"] {
            let t = run(input);
            assert_eq!(t.len(), 1, "input {input:?}");
            assert_eq!(t[0].0, kinds::STRING, "input {input:?}");
            assert_eq!(t[0].2 as usize, input.len(), "input {input:?}");
        }
    }

    #[test]
    fn ml_basic_trailing_quote_3_4_5() {
        let t = run("\"\"\"a\"\"\"\"");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].2 as usize, 8);

        let t = run("\"\"\"a\"\"\"\"\"");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].2 as usize, 9);

        let t = run("\"\"\"\"\"\"");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].2 as usize, 6);
    }

    #[test]
    fn ml_literal_trailing_quote_3_4_5() {
        let t = run("'''a''''");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].2 as usize, 8);

        let t = run("'''a'''''");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].2 as usize, 9);
    }

    #[test]
    fn ml_line_continuation() {
        let input = "\"\"\"a \\\nb\"\"\"";
        let t = run(input);
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].2 as usize, input.len());
    }

    #[test]
    fn comments() {
        let tokens = run("# hello\nkey = 1\n");
        let k = kinds_of(&tokens);
        assert!(k.contains(&kinds::COMMENT));
        assert!(k.contains(&kinds::IDENT));
        assert!(k.contains(&kinds::NUMBER));
    }

    #[test]
    fn arrays_and_inline_tables() {
        let tokens = run("a = [1, 2]\nb = {x = 1}");
        let k = kinds_of(&tokens);
        assert!(k.contains(&kinds::OPEN_BRACKET));
        assert!(k.contains(&kinds::CLOSE_BRACKET));
        assert!(k.contains(&kinds::OPEN_BRACE));
        assert!(k.contains(&kinds::CLOSE_BRACE));
        assert!(k.contains(&kinds::EQ));
        assert!(k.contains(&kinds::COMMA));
        assert!(k.contains(&kinds::NUMBER));
        assert!(k.contains(&kinds::IDENT));
    }

    #[test]
    fn tables_with_dotted_keys() {
        let tokens = run("[a.b.c]");
        assert_eq!(
            kinds_of(&tokens),
            vec![
                kinds::OPEN_BRACKET,
                kinds::IDENT,
                kinds::DOT,
                kinds::IDENT,
                kinds::DOT,
                kinds::IDENT,
                kinds::CLOSE_BRACKET,
            ]
        );
    }

    #[test]
    fn double_bracket_array_of_tables() {
        let tokens = run("[[x.y]]");
        assert_eq!(
            kinds_of(&tokens),
            vec![
                kinds::OPEN_BRACKET,
                kinds::OPEN_BRACKET,
                kinds::IDENT,
                kinds::DOT,
                kinds::IDENT,
                kinds::CLOSE_BRACKET,
                kinds::CLOSE_BRACKET,
            ]
        );
    }

    #[test]
    fn error_stray_byte() {
        let tokens = run("@");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::ERROR);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn error_unterminated_basic_string() {
        let tokens = run(r#""abc"#);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::STRING);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn error_unterminated_ml_basic() {
        let tokens = run(r#""""abc"#);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::STRING);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn error_bare_lf_in_basic_string() {
        let tokens = run("\"a\nb\"");
        assert_eq!(tokens[0].0, kinds::STRING);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn coverage_is_contiguous() {
        let input = "# sample\n[pkg]\nname = \"x\"\nver = 1.2.3\narr = [0xFF, +inf, true]\n";
        let tokens = run(input);
        let mut pos = 0u32;
        for (_kind, offset, len, _flags) in &tokens {
            assert_eq!(*offset, pos, "gap or overlap before offset {pos}");
            pos += len;
        }
        assert_eq!(pos as usize, input.len());
    }

    struct Paged<'a> {
        pages: &'a [&'a [u8]],
        total: u32,
    }

    impl<'a> Paged<'a> {
        fn new(pages: &'a [&'a [u8]]) -> Self {
            let mut total = 0u32;
            for p in pages {
                total += p.len() as u32;
            }
            Self { pages, total }
        }
    }

    impl Source for Paged<'_> {
        fn len(&self) -> u32 {
            self.total
        }

        fn page(&self, offset: u32) -> (u32, &[u8]) {
            let mut base = 0u32;
            for p in self.pages {
                let end = base + p.len() as u32;
                if offset < end {
                    return (base, p);
                }
                base = end;
            }
            (self.total, &[])
        }
    }

    #[test]
    fn ml_string_terminator_splits_across_pages() {
        let p = Paged::new(&[b"\"\"\"abc\"", b"\"\" trail"]);
        let src: &dyn Source = &p;
        let mut view = SourceView::new(src, 0);
        let r = scan_toml_ml_basic_string(&mut view, 0);
        assert!(!r.is_error);
        assert_eq!(r.end, 9);
    }

    #[test]
    fn ml_string_backslash_at_page_boundary() {
        let p = Paged::new(&[b"\"\"\"abc\\", b"\"\"\"\""]);
        let src: &dyn Source = &p;
        let mut view = SourceView::new(src, 0);
        let r = scan_toml_ml_basic_string(&mut view, 0);
        assert!(!r.is_error);
        assert_eq!(r.end, p.len());
    }
}
