//! WGSL lexer.
//!
//! State encoding: stateless. Every emitted token returns
//! [`LexState::INITIAL`] and carries [`STATE_BREAKPOINT`].
//!
//! The lexer intentionally stays lexical: it recognizes WGSL keywords,
//! attributes, operators, comments, identifiers, and numeric literals without
//! trying to classify builtins or context-dependent names.
//!
//! [`STATE_BREAKPOINT`]: flags::STATE_BREAKPOINT

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

kw::kw_table! {
    static WGSL_KWS = [
        (b"alias", kinds::KEYWORD),
        (b"as", kinds::KEYWORD),
        (b"break", kinds::KEYWORD),
        (b"case", kinds::KEYWORD),
        (b"const", kinds::KEYWORD),
        (b"const_assert", kinds::KEYWORD),
        (b"continue", kinds::KEYWORD),
        (b"continuing", kinds::KEYWORD),
        (b"default", kinds::KEYWORD),
        (b"diagnostic", kinds::KEYWORD),
        (b"discard", kinds::KEYWORD),
        (b"else", kinds::KEYWORD),
        (b"enable", kinds::KEYWORD),
        (b"false", kinds::KEYWORD),
        (b"fn", kinds::KEYWORD),
        (b"for", kinds::KEYWORD),
        (b"if", kinds::KEYWORD),
        (b"import", kinds::KEYWORD),
        (b"let", kinds::KEYWORD),
        (b"loop", kinds::KEYWORD),
        (b"override", kinds::KEYWORD),
        (b"package", kinds::KEYWORD),
        (b"requires", kinds::KEYWORD),
        (b"return", kinds::KEYWORD),
        (b"self", kinds::KEYWORD),
        (b"struct", kinds::KEYWORD),
        (b"super", kinds::KEYWORD),
        (b"switch", kinds::KEYWORD),
        (b"true", kinds::KEYWORD),
        (b"var", kinds::KEYWORD),
        (b"while", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 12; // "const_assert"

pub(crate) struct Wgsl;

impl Lexer for Wgsl {
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
                let r = scan::scan_nested_block_comment(view, cursor);
                (kinds::COMMENT, r.end, r.is_error)
            }
            _ => {
                let (kind, end) = scan_operator(view, cursor, first);
                (kind, end, false)
            }
        },
        b'.' => match view.byte_at(cursor + 1) {
            Some(b) if byteclass::is_digit(b) => {
                let r = scan_wgsl_number(view, cursor);
                (kinds::NUMBER, r.end, r.is_error)
            }
            _ => (kinds::DOT, cursor + 1, false),
        },
        b'0'..=b'9' => {
            let r = scan_wgsl_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b if byteclass::is_ident_start(b) => classify_ident(view, cursor),
        b'&' | b'-' | b'@' | b'!' | b'[' | b']' | b'{' | b'}' | b':' | b',' | b'=' | b'>'
        | b'<' | b'%' | b'+' | b'|' | b'(' | b')' | b';' | b'*' | b'~' | b'^' => {
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
            if let Some(kind) = kw_lookup(WGSL_KWS, &buf[..len]) {
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
    let len = scan::decoded_len_or_one(view, cursor);
    (kinds::ERROR, cursor + len, true)
}

fn scan_operator(view: &mut SourceView<'_>, cursor: u32, b0: u8) -> (u16, u32) {
    let b1 = view.byte_at(cursor + 1);
    let (kind, len) = match b0 {
        b'&' => match b1 {
            Some(b'&') => (kinds::AMP_AMP, 2),
            Some(b'=') => (kinds::AMP_EQ, 2),
            _ => (kinds::AMP, 1),
        },
        b'-' => match b1 {
            Some(b'>') => (kinds::THIN_ARROW, 2),
            Some(b'-') => (kinds::MINUS_MINUS, 2),
            Some(b'=') => (kinds::MINUS_EQ, 2),
            _ => (kinds::MINUS, 1),
        },
        b'@' => (kinds::AT, 1),
        b'!' => match b1 {
            Some(b'=') => (kinds::BANG_EQ, 2),
            _ => (kinds::BANG, 1),
        },
        b'[' => (kinds::OPEN_BRACKET, 1),
        b']' => (kinds::CLOSE_BRACKET, 1),
        b'{' => (kinds::OPEN_BRACE, 1),
        b'}' => (kinds::CLOSE_BRACE, 1),
        b':' => match b1 {
            Some(b':') => (kinds::COLON_COLON, 2),
            _ => (kinds::COLON, 1),
        },
        b',' => (kinds::COMMA, 1),
        b'=' => match b1 {
            Some(b'=') => (kinds::EQ_EQ, 2),
            _ => (kinds::EQ, 1),
        },
        b'>' => match b1 {
            Some(b'>') if view.byte_at(cursor + 2) == Some(b'=') => (kinds::SHR_EQ, 3),
            Some(b'>') => (kinds::SHR, 2),
            Some(b'=') => (kinds::GT_EQ, 2),
            _ => (kinds::GT, 1),
        },
        b'<' => match b1 {
            Some(b'<') if view.byte_at(cursor + 2) == Some(b'=') => (kinds::SHL_EQ, 3),
            Some(b'<') => (kinds::SHL, 2),
            Some(b'=') => (kinds::LT_EQ, 2),
            _ => (kinds::LT, 1),
        },
        b'%' => match b1 {
            Some(b'=') => (kinds::PERCENT_EQ, 2),
            _ => (kinds::PERCENT, 1),
        },
        b'+' => match b1 {
            Some(b'+') => (kinds::PLUS_PLUS, 2),
            Some(b'=') => (kinds::PLUS_EQ, 2),
            _ => (kinds::PLUS, 1),
        },
        b'|' => match b1 {
            Some(b'|') => (kinds::PIPE_PIPE, 2),
            Some(b'=') => (kinds::PIPE_EQ, 2),
            _ => (kinds::PIPE, 1),
        },
        b'(' => (kinds::OPEN_PAREN, 1),
        b')' => (kinds::CLOSE_PAREN, 1),
        b';' => (kinds::SEMI, 1),
        b'*' => match b1 {
            Some(b'=') => (kinds::STAR_EQ, 2),
            _ => (kinds::STAR, 1),
        },
        b'~' => (kinds::TILDE, 1),
        b'^' => match b1 {
            Some(b'=') => (kinds::CARET_EQ, 2),
            _ => (kinds::CARET, 1),
        },
        _ => (kinds::ERROR, 1),
    };
    (kind, cursor + len)
}

fn scan_wgsl_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    if view.byte_at(cursor) == Some(b'.') {
        return scan_decimal_number(view, cursor);
    }
    if view.byte_at(cursor) == Some(b'0') {
        match view.byte_at(cursor + 1) {
            Some(b'x') | Some(b'X') => return scan_hex_number(view, cursor),
            _ => {}
        }
    }
    scan_decimal_number(view, cursor)
}

fn scan_decimal_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end;
    let mut is_float = false;
    let mut is_error = false;

    if view.byte_at(cursor) == Some(b'.') {
        is_float = true;
        end = cursor + 1;
        let before = end;
        end = scan::scan_digits(view, end);
        if end == before {
            return err(end);
        }
    } else {
        end = scan::scan_digits(view, cursor);
        if view.byte_at(end) == Some(b'.') {
            is_float = true;
            end += 1;
            end = scan::scan_digits(view, end);
        }
    }

    if matches!(view.byte_at(end), Some(b'e') | Some(b'E')) {
        is_float = true;
        end += 1;
        if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
            end += 1;
        }
        let before = end;
        end = scan::scan_digits(view, end);
        if end == before {
            is_error = true;
        }
    }

    match view.byte_at(end) {
        Some(b'f') | Some(b'h') => end += 1,
        Some(b'i') | Some(b'u') if !is_float => end += 1,
        _ => {}
    }

    ScanResult { end, is_error }
}

fn scan_hex_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor + 2;
    let int_start = end;
    end = scan_hex_digits(view, end);
    let had_int = end > int_start;
    let mut is_float = false;
    let mut had_frac = false;

    if view.byte_at(end) == Some(b'.') {
        is_float = true;
        end += 1;
        let frac_start = end;
        end = scan_hex_digits(view, end);
        had_frac = end > frac_start;
    }

    if !had_int && !had_frac {
        return err(end);
    }

    if matches!(view.byte_at(end), Some(b'p') | Some(b'P')) {
        is_float = true;
        end += 1;
        if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
            end += 1;
        }
        let before = end;
        end = scan::scan_digits(view, end);
        if end == before {
            return err(end);
        }
    }

    match view.byte_at(end) {
        Some(b'f') | Some(b'h') if is_float => end += 1,
        Some(b'i') | Some(b'u') if !is_float => end += 1,
        _ => {}
    }

    ok(end)
}

fn scan_hex_digits(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let start = (cursor - base) as usize;
        let mut i = start;
        while i < page.len() && byteclass::is_hex_digit(page[i]) {
            i += 1;
        }
        cursor += (i - start) as u32;
        if i < page.len() {
            return cursor;
        }
    }
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
            (cursor, state) = Wgsl::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("WGSL has no embedding"),
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
        let tokens = run("@vertex fn main() -> vec4f { return vec4f(1.0); }");
        assert_eq!(
            kinds_of(&tokens),
            vec![
                kinds::AT,
                kinds::IDENT,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::OPEN_PAREN,
                kinds::CLOSE_PAREN,
                kinds::WHITESPACE,
                kinds::THIN_ARROW,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::WHITESPACE,
                kinds::OPEN_BRACE,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::OPEN_PAREN,
                kinds::NUMBER,
                kinds::CLOSE_PAREN,
                kinds::SEMI,
                kinds::WHITESPACE,
                kinds::CLOSE_BRACE,
            ]
        );
    }

    #[test]
    fn nested_block_comment_is_one_token() {
        let tokens = run("/* outer /* inner */ done */ fn f() {}");
        assert_eq!(tokens[0].0, kinds::COMMENT);
        assert_eq!(tokens[0].2, 28);
        assert_eq!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn unterminated_block_comment_is_error() {
        let tokens = run("/* outer /* inner */");
        assert_eq!(tokens[0].0, kinds::COMMENT);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn numeric_literals() {
        let tokens = run("0 1u 2i 3.0 .5 1e-2 1f 2h 0x1fu 0x1.fp+2f");
        for (kind, _, _, bits) in tokens
            .iter()
            .copied()
            .filter(|(kind, _, _, _)| *kind != kinds::WHITESPACE)
        {
            assert_eq!(kind, kinds::NUMBER);
            assert_eq!(bits & flags::IS_ERROR, 0);
        }
    }

    #[test]
    fn keywords_and_wesl_import_words() {
        let tokens =
            run("alias as break const_assert diagnostic enable import package self super while");
        for (kind, _, _, _) in tokens
            .iter()
            .copied()
            .filter(|(kind, _, _, _)| *kind != kinds::WHITESPACE)
        {
            assert_eq!(kind, kinds::KEYWORD);
        }
    }
}
