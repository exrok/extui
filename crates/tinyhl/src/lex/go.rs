//! Go lexer.
//!
//! Go is lexed as a stateless byte machine: every token is produced at
//! [`LexState::INITIAL`] and carries [`STATE_BREAKPOINT`]. Comments, raw
//! strings, interpreted strings, rune literals, and numbers are each scanned
//! to completion as a single token, so incremental lexing can resume at any
//! token boundary.
//!
//! Deliberate "good enough" choices:
//!
//! - Semicolon insertion is parser state and is not represented; explicit
//!   semicolons are still emitted as [`SEMI`].
//! - Unicode identifiers use the shared XID scanner rather than Go's exact
//!   `unicode.IsLetter` / `unicode.IsDigit` tables.
//! - Numeric underscores are accepted leniently; missing radix/exponent digits
//!   are still flagged as errors.
//!
//! [`STATE_BREAKPOINT`]: flags::STATE_BREAKPOINT
//! [`SEMI`]: kinds::SEMI

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

kw::kw_table! {
    static GO_KWS = [
        (b"break", kinds::KEYWORD),
        (b"case", kinds::KEYWORD),
        (b"chan", kinds::KEYWORD),
        (b"const", kinds::KEYWORD),
        (b"continue", kinds::KEYWORD),
        (b"default", kinds::KEYWORD),
        (b"defer", kinds::KEYWORD),
        (b"else", kinds::KEYWORD),
        (b"fallthrough", kinds::KEYWORD),
        (b"for", kinds::KEYWORD),
        (b"func", kinds::KEYWORD),
        (b"go", kinds::KEYWORD),
        (b"goto", kinds::KEYWORD),
        (b"if", kinds::KEYWORD),
        (b"import", kinds::KEYWORD),
        (b"interface", kinds::KEYWORD),
        (b"map", kinds::KEYWORD),
        (b"package", kinds::KEYWORD),
        (b"range", kinds::KEYWORD),
        (b"return", kinds::KEYWORD),
        (b"select", kinds::KEYWORD),
        (b"struct", kinds::KEYWORD),
        (b"switch", kinds::KEYWORD),
        (b"type", kinds::KEYWORD),
        (b"var", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 11; // "fallthrough"

pub(crate) struct Go;

impl Lexer for Go {
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
        b if byteclass::is_whitespace(b) => {
            let end = scan::scan_whitespace(view, cursor);
            (kinds::WHITESPACE, end, false)
        }
        b'/' => match view.byte_at(cursor + 1) {
            Some(b'/') => {
                let end = scan::scan_line_comment(view, cursor);
                (kinds::COMMENT, end, false)
            }
            Some(b'*') => {
                let r = scan::scan_block_comment(view, cursor);
                (kinds::COMMENT, r.end, r.is_error)
            }
            _ => {
                let (kind, end) = scan_operator(view, cursor, first);
                (kind, end, false)
            }
        },
        b'"' => {
            let r = scan_go_quoted(view, cursor, b'"');
            (kinds::STRING, r.end, r.is_error)
        }
        b'\'' => {
            let r = scan_go_quoted(view, cursor, b'\'');
            (kinds::CHAR, r.end, r.is_error)
        }
        b'`' => {
            let r = scan_raw_string(view, cursor);
            (kinds::STRING, r.end, r.is_error)
        }
        b'.' => match view.byte_at(cursor + 1) {
            Some(b) if byteclass::is_digit(b) => {
                let r = scan_go_number(view, cursor);
                (kinds::NUMBER, r.end, r.is_error)
            }
            Some(b'.') if view.byte_at(cursor + 2) == Some(b'.') => {
                (kinds::ELLIPSIS, cursor + 3, false)
            }
            _ => (kinds::DOT, cursor + 1, false),
        },
        b'0'..=b'9' => {
            let r = scan_go_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b if byteclass::is_ident_start(b) => classify_ident(view, cursor),
        b'+' | b'-' | b'*' | b'%' | b'&' | b'|' | b'^' | b'<' | b'>' | b'=' | b'!' | b'~'
        | b':' | b',' | b';' | b'(' | b')' | b'{' | b'}' | b'[' | b']' => {
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
            if let Some(kind) = kw_lookup(GO_KWS, &buf[..len]) {
                return (kind, end, false);
            }
        }
    }
    (kinds::IDENT, end, false)
}

fn classify_unicode_ident(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan::scan_xid_ident(view, cursor, false);
    if end > cursor {
        return (kinds::IDENT, end, false);
    }
    let len = scan::decode_char_at(view, cursor)
        .map(|(_, n)| n)
        .unwrap_or(1);
    (kinds::ERROR, cursor + len, true)
}

fn scan_go_quoted(view: &mut SourceView<'_>, cursor: u32, quote: u8) -> ScanResult {
    let mut end = cursor + 1;
    loop {
        match view.byte_at(end) {
            None => return err(end),
            Some(b) if b == quote => return ok(end + 1),
            Some(b'\\') => {
                end += 1;
                match view.byte_at(end) {
                    None | Some(b'\n') | Some(b'\r') => return err(end),
                    Some(_) => end += 1,
                }
            }
            Some(b'\n') | Some(b'\r') => return err(end),
            Some(_) => end += 1,
        }
    }
}

fn scan_raw_string(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor + 1;
    loop {
        match view.byte_at(end) {
            None => return err(end),
            Some(b'`') => return ok(end + 1),
            Some(_) => end += 1,
        }
    }
}

fn scan_go_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    if view.byte_at(cursor) == Some(b'0') {
        match view.byte_at(cursor + 1) {
            Some(b'x') | Some(b'X') => return scan_hex_number(view, cursor + 2),
            Some(b'o') | Some(b'O') => return scan_radix_number(view, cursor + 2, is_oct_digit),
            Some(b'b') | Some(b'B') => return scan_radix_number(view, cursor + 2, is_bin_digit),
            _ => {}
        }
    }
    scan_decimal_number(view, cursor)
}

fn scan_hex_number(view: &mut SourceView<'_>, after_prefix: u32) -> ScanResult {
    let (int_end, had_int_digits) = eat_digit_run(view, after_prefix, byteclass::is_hex_digit);
    let mut end = int_end;
    let mut saw_dot = false;
    let mut had_frac_digits = false;

    if view.byte_at(end) == Some(b'.') {
        saw_dot = true;
        let (frac_end, had) = eat_digit_run(view, end + 1, byteclass::is_hex_digit);
        end = frac_end;
        had_frac_digits = had;
    }

    if !had_int_digits && !had_frac_digits {
        return err(end);
    }

    match view.byte_at(end) {
        Some(b'p') | Some(b'P') => {
            end += 1;
            if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
                end += 1;
            }
            let (exp_end, had_exp_digits) = eat_digit_run(view, end, byteclass::is_digit);
            if !had_exp_digits {
                return err(exp_end);
            }
            ok(eat_imag_suffix(view, exp_end))
        }
        _ if saw_dot => err(end),
        _ => ok(eat_imag_suffix(view, end)),
    }
}

fn scan_radix_number(
    view: &mut SourceView<'_>,
    after_prefix: u32,
    pred: fn(u8) -> bool,
) -> ScanResult {
    let (end, had_digits) = eat_digit_run(view, after_prefix, pred);
    if had_digits {
        ok(eat_imag_suffix(view, end))
    } else {
        err(end)
    }
}

fn scan_decimal_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor;
    let mut had_int_digits = false;
    if view.byte_at(cursor) != Some(b'.') {
        let (int_end, had) = eat_digit_run(view, cursor, byteclass::is_digit);
        end = int_end;
        had_int_digits = had;
    }

    let mut had_frac_digits = false;
    if view.byte_at(end) == Some(b'.') {
        let (frac_end, had) = eat_digit_run(view, end + 1, byteclass::is_digit);
        end = frac_end;
        had_frac_digits = had;
    }

    if !had_int_digits && !had_frac_digits {
        return err(end);
    }

    if matches!(view.byte_at(end), Some(b'e') | Some(b'E')) {
        end += 1;
        if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
            end += 1;
        }
        let (exp_end, had_exp_digits) = eat_digit_run(view, end, byteclass::is_digit);
        if !had_exp_digits {
            return err(exp_end);
        }
        end = exp_end;
    }

    ok(eat_imag_suffix(view, end))
}

fn eat_digit_run(view: &mut SourceView<'_>, cursor: u32, pred: fn(u8) -> bool) -> (u32, bool) {
    let mut cursor = cursor;
    let mut saw_digit = false;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return (cursor, saw_digit);
        }
        let start = (cursor - base) as usize;
        let mut i = start;
        while i < page.len() {
            if page[i] == b'_' {
                i += 1;
            } else if pred(page[i]) {
                saw_digit = true;
                i += 1;
            } else {
                break;
            }
        }
        cursor += (i - start) as u32;
        if i < page.len() {
            return (cursor, saw_digit);
        }
    }
}

fn eat_imag_suffix(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    if view.byte_at(cursor) == Some(b'i') {
        cursor + 1
    } else {
        cursor
    }
}

fn is_bin_digit(b: u8) -> bool {
    b == b'0' || b == b'1'
}

fn is_oct_digit(b: u8) -> bool {
    (b'0'..=b'7').contains(&b)
}

fn scan_operator(view: &mut SourceView<'_>, cursor: u32, b0: u8) -> (u16, u32) {
    let b1 = view.byte_at(cursor + 1);
    let b2 = view.byte_at(cursor + 2);

    let three = match (b0, b1, b2) {
        (b'&', Some(b'^'), Some(b'=')) => Some(kinds::AMP_CARET_EQ),
        (b'<', Some(b'<'), Some(b'=')) => Some(kinds::SHL_EQ),
        (b'>', Some(b'>'), Some(b'=')) => Some(kinds::SHR_EQ),
        (b'.', Some(b'.'), Some(b'.')) => Some(kinds::ELLIPSIS),
        _ => None,
    };
    if let Some(k) = three {
        return (k, cursor + 3);
    }

    if let Some(b1) = b1 {
        let two = match (b0, b1) {
            (b'=', b'=') => Some(kinds::EQ_EQ),
            (b'!', b'=') => Some(kinds::BANG_EQ),
            (b'<', b'=') => Some(kinds::LT_EQ),
            (b'>', b'=') => Some(kinds::GT_EQ),
            (b'&', b'&') => Some(kinds::AMP_AMP),
            (b'|', b'|') => Some(kinds::PIPE_PIPE),
            (b'<', b'<') => Some(kinds::SHL),
            (b'>', b'>') => Some(kinds::SHR),
            (b'&', b'^') => Some(kinds::AMP_CARET),
            (b'<', b'-') => Some(kinds::LEFT_ARROW),
            (b'+', b'+') => Some(kinds::PLUS_PLUS),
            (b'-', b'-') => Some(kinds::MINUS_MINUS),
            (b':', b'=') => Some(kinds::COLON_EQ),
            (b'+', b'=') => Some(kinds::PLUS_EQ),
            (b'-', b'=') => Some(kinds::MINUS_EQ),
            (b'*', b'=') => Some(kinds::STAR_EQ),
            (b'/', b'=') => Some(kinds::SLASH_EQ),
            (b'%', b'=') => Some(kinds::PERCENT_EQ),
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
        b'&' => kinds::AMP,
        b'|' => kinds::PIPE,
        b'^' => kinds::CARET,
        b'<' => kinds::LT,
        b'>' => kinds::GT,
        b'=' => kinds::EQ,
        b'!' => kinds::BANG,
        b'~' => kinds::TILDE,
        b':' => kinds::COLON,
        b',' => kinds::COMMA,
        b';' => kinds::SEMI,
        b'(' => kinds::OPEN_PAREN,
        b')' => kinds::CLOSE_PAREN,
        b'{' => kinds::OPEN_BRACE,
        b'}' => kinds::CLOSE_BRACE,
        b'[' => kinds::OPEN_BRACKET,
        b']' => kinds::CLOSE_BRACKET,
        b'.' => kinds::DOT,
        _ => kinds::ERROR,
    };
    (one, cursor + 1)
}

fn ok(end: u32) -> ScanResult {
    ScanResult {
        end,
        is_error: false,
    }
}

fn err(end: u32) -> ScanResult {
    ScanResult {
        end,
        is_error: true,
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
            let before = cursor;
            (cursor, state) = Go::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("Go has no embedding"),
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
    fn simple_function() {
        let tokens = run("package main\nfunc main() { return }\n");
        assert_eq!(
            kinds_of(&tokens),
            vec![
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::OPEN_PAREN,
                kinds::CLOSE_PAREN,
                kinds::WHITESPACE,
                kinds::OPEN_BRACE,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::CLOSE_BRACE,
                kinds::WHITESPACE,
            ]
        );
    }

    #[test]
    fn keywords_covering_the_table() {
        for kw in [
            "break",
            "case",
            "chan",
            "const",
            "continue",
            "default",
            "defer",
            "else",
            "fallthrough",
            "for",
            "func",
            "go",
            "goto",
            "if",
            "import",
            "interface",
            "map",
            "package",
            "range",
            "return",
            "select",
            "struct",
            "switch",
            "type",
            "var",
        ] {
            let tokens = run(kw);
            assert_eq!(tokens.len(), 1, "{kw:?}");
            assert_eq!(tokens[0].0, kinds::KEYWORD, "{kw:?} should be KEYWORD");
        }
    }

    #[test]
    fn predeclared_names_are_identifiers() {
        for ident in ["true", "false", "nil", "int", "string", "append", "make"] {
            let tokens = run(ident);
            assert_eq!(tokens.len(), 1, "{ident:?}");
            assert_eq!(tokens[0].0, kinds::IDENT, "{ident:?}");
        }
    }

    #[test]
    fn comments() {
        assert_eq!(
            kinds_of(&run("// hello\npackage")),
            vec![kinds::COMMENT, kinds::WHITESPACE, kinds::KEYWORD]
        );
        let block = run("/* block\ncomment */ var");
        assert_eq!(
            kinds_of(&block),
            vec![kinds::COMMENT, kinds::WHITESPACE, kinds::KEYWORD]
        );
        assert_eq!(block[0].2, "/* block\ncomment */".len() as u32);
    }

    #[test]
    fn unterminated_block_comment_flags_error() {
        let tokens = run("/* never ends");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::COMMENT);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn strings_and_runes() {
        for input in [r#""hi\n""#, "`raw\nstring`"] {
            let tokens = run(input);
            assert_eq!(tokens.len(), 1, "{input:?}");
            assert_eq!(tokens[0].0, kinds::STRING, "{input:?}");
            assert_eq!(tokens[0].2 as usize, input.len(), "{input:?}");
            assert_eq!(tokens[0].3 & flags::IS_ERROR, 0, "{input:?}");
        }
        let rune = run(r#"'x'"#);
        assert_eq!(rune.len(), 1);
        assert_eq!(rune[0].0, kinds::CHAR);
    }

    #[test]
    fn unterminated_strings_flag_error() {
        for input in [r#""oops"#, "\"line\nbreak\"", "`raw"] {
            let tokens = run(input);
            assert_eq!(tokens[0].0, kinds::STRING, "{input:?}");
            assert_ne!(tokens[0].3 & flags::IS_ERROR, 0, "{input:?}");
        }
    }

    #[test]
    fn numbers_mixed() {
        let tokens = run("0 42 1_000 0b1010 0o755 0xFF 1.5 1e10 .25 0x1.fp3 2i");
        let count = tokens.iter().filter(|t| t.0 == kinds::NUMBER).count();
        assert_eq!(count, 11);
        for t in tokens.iter().filter(|t| t.0 == kinds::NUMBER) {
            assert_eq!(t.3 & flags::IS_ERROR, 0, "{t:?}");
        }
    }

    #[test]
    fn bad_numbers_flag_error() {
        for input in ["0x", "0b", "0o", "1e", "0x1."] {
            let tokens = run(input);
            assert_eq!(tokens.len(), 1, "{input:?}");
            assert_eq!(tokens[0].0, kinds::NUMBER, "{input:?}");
            assert_ne!(tokens[0].3 & flags::IS_ERROR, 0, "{input:?}");
        }
    }

    #[test]
    fn operators_greedy() {
        let src = "a<<=b >>=c &^=d <-e :=f ...g &&h ||i ++j --k ==l !=m <=n >=o &^p";
        let tokens = run(src);
        for (kind, len) in [
            (kinds::SHL_EQ, 3),
            (kinds::SHR_EQ, 3),
            (kinds::AMP_CARET_EQ, 3),
            (kinds::ELLIPSIS, 3),
            (kinds::LEFT_ARROW, 2),
            (kinds::COLON_EQ, 2),
            (kinds::AMP_AMP, 2),
            (kinds::PIPE_PIPE, 2),
            (kinds::PLUS_PLUS, 2),
            (kinds::MINUS_MINUS, 2),
            (kinds::EQ_EQ, 2),
            (kinds::BANG_EQ, 2),
            (kinds::LT_EQ, 2),
            (kinds::GT_EQ, 2),
            (kinds::AMP_CARET, 2),
        ] {
            assert!(
                tokens.iter().any(|t| t.0 == kind && t.2 == len),
                "missing greedy operator kind {kind}"
            );
        }
    }

    #[test]
    fn unicode_ident() {
        let tokens = run("var \u{03c0} = 3");
        assert!(tokens.iter().any(|t| t.0 == kinds::IDENT && t.2 == 2));
    }

    #[test]
    fn error_on_unknown_byte() {
        let tokens = run("?");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::ERROR);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn coverage_is_contiguous() {
        let input = "package main\nfunc main() { ch := make(chan int); ch <- 1 }\n";
        let tokens = run(input);
        let mut pos = 0u32;
        for (_kind, offset, len, _flags) in &tokens {
            assert_eq!(*offset, pos, "gap or overlap before offset {pos}");
            pos += len;
        }
        assert_eq!(pos as usize, input.len());
    }

    #[test]
    fn every_token_has_state_breakpoint() {
        let input = "var s = `raw`; var n = 0x1.fp3i; /* c */ ch <- 'x'";
        let tokens = run(input);
        for t in &tokens {
            assert_ne!(t.3 & flags::STATE_BREAKPOINT, 0, "missing STATE_BREAKPOINT");
        }
    }
}
