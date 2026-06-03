//! Perl lexer.
//!
//! State encoding: stateless. Every emitted token returns
//! [`LexState::INITIAL`] and carries [`STATE_BREAKPOINT`].
//!
//! This is a deliberately small lexer for common Perl source: comments,
//! sigil variables, C-like numbers, ordinary strings, quote-like operators,
//! keywords, and punctuation. Bare `/.../` is left as operators so division or
//! paths do not accidentally turn the rest of a line into a regex; explicit
//! `m//`, `qr//`, and simple `s///` forms are tokenized as [`REGEX`].
//!
//! [`REGEX`]: kinds::REGEX
//! [`STATE_BREAKPOINT`]: flags::STATE_BREAKPOINT

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

kw::kw_table! {
    static PERL_KWS = [
        (b"and", kinds::KEYWORD),
        (b"cmp", kinds::KEYWORD),
        (b"continue", kinds::KEYWORD),
        (b"default", kinds::KEYWORD),
        (b"defined", kinds::KEYWORD),
        (b"die", kinds::KEYWORD),
        (b"do", kinds::KEYWORD),
        (b"else", kinds::KEYWORD),
        (b"elsif", kinds::KEYWORD),
        (b"eq", kinds::KEYWORD),
        (b"for", kinds::KEYWORD),
        (b"foreach", kinds::KEYWORD),
        (b"ge", kinds::KEYWORD),
        (b"given", kinds::KEYWORD),
        (b"goto", kinds::KEYWORD),
        (b"gt", kinds::KEYWORD),
        (b"if", kinds::KEYWORD),
        (b"last", kinds::KEYWORD),
        (b"le", kinds::KEYWORD),
        (b"local", kinds::KEYWORD),
        (b"lt", kinds::KEYWORD),
        (b"my", kinds::KEYWORD),
        (b"ne", kinds::KEYWORD),
        (b"next", kinds::KEYWORD),
        (b"not", kinds::KEYWORD),
        (b"or", kinds::KEYWORD),
        (b"our", kinds::KEYWORD),
        (b"package", kinds::KEYWORD),
        (b"print", kinds::KEYWORD),
        (b"redo", kinds::KEYWORD),
        (b"require", kinds::KEYWORD),
        (b"return", kinds::KEYWORD),
        (b"say", kinds::KEYWORD),
        (b"state", kinds::KEYWORD),
        (b"sub", kinds::KEYWORD),
        (b"undef", kinds::KEYWORD),
        (b"unless", kinds::KEYWORD),
        (b"until", kinds::KEYWORD),
        (b"use", kinds::KEYWORD),
        (b"when", kinds::KEYWORD),
        (b"while", kinds::KEYWORD),
        (b"xor", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 8; // "continue"

pub(crate) struct Perl;

impl Lexer for Perl {
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
        b'#' => (kinds::COMMENT, scan::scan_hash_comment(view, cursor), false),
        b'"' | b'\'' => {
            let r = scan::scan_oneline_quoted(view, cursor, first);
            (kinds::STRING, r.end, r.is_error)
        }
        b'`' => {
            let r = scan::scan_oneline_quoted(view, cursor, first);
            (kinds::TEMPLATE_STRING, r.end, r.is_error)
        }
        b'$' | b'@' | b'%' => scan_sigil(view, cursor, first),
        b'.' => match view.byte_at(cursor + 1) {
            Some(b) if byteclass::is_digit(b) => {
                let r = scan::scan_c_number_with_underscores(view, cursor);
                (kinds::NUMBER, r.end, r.is_error)
            }
            _ => {
                let (kind, end) = scan_operator(view, cursor, first);
                (kind, end, false)
            }
        },
        b'0'..=b'9' => {
            let r = scan::scan_c_number_with_underscores(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b'q' | b'm' | b's' | b't' | b'y' => {
            if let Some((kind, end, is_error)) = try_quote_like(view, cursor, first) {
                (kind, end, is_error)
            } else {
                classify_ident(view, cursor)
            }
        }
        b if byteclass::is_ident_start(b) => classify_ident(view, cursor),
        b if b >= 0x80 => {
            let len = scan::decoded_len_or_one(view, cursor);
            (kinds::ERROR, cursor + len, true)
        }
        b'+' | b'-' | b'*' | b'/' | b'&' | b'|' | b'^' | b'~' | b'<' | b'>' | b'=' | b'!'
        | b':' | b',' | b';' | b'?' | b'(' | b')' | b'{' | b'}' | b'[' | b']' => {
            let (kind, end) = scan_operator(view, cursor, first);
            (kind, end, false)
        }
        _ => (kinds::ERROR, cursor + 1, true),
    }
}

fn classify_ident(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan::scan_ident_ascii(view, cursor);
    let len = (end - cursor) as usize;
    if len <= MAX_KW_LEN {
        let mut buf = [0u8; MAX_KW_LEN];
        if scan::copy_bytes(view, cursor, &mut buf[..len]) {
            if let Some(kind) = kw_lookup(PERL_KWS, &buf[..len]) {
                return (kind, end, false);
            }
        }
    }
    (kinds::IDENT, end, false)
}

fn scan_sigil(view: &mut SourceView<'_>, cursor: u32, sigil: u8) -> (u16, u32, bool) {
    let body = cursor + 1;
    match view.byte_at(body) {
        Some(b) if byteclass::is_ident_start(b) => {
            (kinds::IDENT, scan::scan_ident_ascii(view, body), false)
        }
        Some(b'0'..=b'9') => (kinds::IDENT, scan::scan_digits(view, body), false),
        Some(b) if is_special_var_byte(b) => (kinds::IDENT, body + 1, false),
        _ => {
            let kind = match sigil {
                b'$' => kinds::DOLLAR,
                b'@' => kinds::AT,
                _ => kinds::PERCENT,
            };
            (kind, body, false)
        }
    }
}

fn is_special_var_byte(b: u8) -> bool {
    matches!(
        b,
        b'_' | b'!'
            | b'?'
            | b'#'
            | b'$'
            | b'@'
            | b'%'
            | b'^'
            | b'&'
            | b'*'
            | b'+'
            | b'-'
            | b'/'
            | b'\\'
            | b'|'
            | b'~'
            | b'.'
            | b','
            | b';'
            | b':'
            | b'<'
            | b'>'
            | b'='
    )
}

fn try_quote_like(view: &mut SourceView<'_>, cursor: u32, first: u8) -> Option<(u16, u32, bool)> {
    match first {
        b'q' => {
            let b1 = view.byte_at(cursor + 1)?;
            let (prefix_len, kind) = match b1 {
                b'q' | b'w' => (2, kinds::STRING),
                b'x' => (2, kinds::TEMPLATE_STRING),
                b'r' => (2, kinds::REGEX),
                _ => (1, kinds::STRING),
            };
            let delim = cursor + prefix_len;
            if !is_delim_start(view.byte_at(delim)?) {
                return None;
            }
            let r = scan_delimited(view, delim);
            Some((kind, scan_flags(view, r.end), r.is_error))
        }
        b'm' => {
            let delim = cursor + 1;
            if !is_delim_start(view.byte_at(delim)?) {
                return None;
            }
            let r = scan_delimited(view, delim);
            Some((kinds::REGEX, scan_flags(view, r.end), r.is_error))
        }
        b's' => {
            let delim = cursor + 1;
            let d = view.byte_at(delim)?;
            if !is_delim_start(d) {
                return None;
            }
            let r = scan_substitution(view, delim, d);
            Some((kinds::REGEX, scan_flags(view, r.end), r.is_error))
        }
        b'y' => {
            let delim = cursor + 1;
            if !is_delim_start(view.byte_at(delim)?) {
                return None;
            }
            let r = scan_delimited(view, delim);
            Some((kinds::REGEX, scan_flags(view, r.end), r.is_error))
        }
        b't' if view.byte_at(cursor + 1) == Some(b'r') => {
            let delim = cursor + 2;
            if !is_delim_start(view.byte_at(delim)?) {
                return None;
            }
            let r = scan_delimited(view, delim);
            Some((kinds::REGEX, scan_flags(view, r.end), r.is_error))
        }
        _ => None,
    }
}

fn is_delim_start(b: u8) -> bool {
    !byteclass::is_whitespace(b) && !byteclass::is_ident_cont(b)
}

fn pair_close(open: u8) -> Option<u8> {
    match open {
        b'(' => Some(b')'),
        b'[' => Some(b']'),
        b'{' => Some(b'}'),
        b'<' => Some(b'>'),
        _ => None,
    }
}

fn scan_delimited(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let open = match view.byte_at(cursor) {
        Some(b) => b,
        None => return err(cursor),
    };
    if open == b'\'' || open == b'"' || open == b'`' {
        return scan::scan_oneline_quoted(view, cursor, open);
    }
    let close = pair_close(open).unwrap_or(open);
    let paired = close != open;
    let mut depth = 1u32;
    let mut end = cursor + 1;
    while depth > 0 {
        match view.byte_at(end) {
            None | Some(b'\n') | Some(b'\r') => return err(end),
            Some(b'\\') => {
                end += 1;
                if view.byte_at(end).is_some() {
                    end += 1;
                }
            }
            Some(b) if paired && b == open => {
                depth += 1;
                end += 1;
            }
            Some(b) if b == close => {
                depth -= 1;
                end += 1;
            }
            Some(_) => end += 1,
        }
    }
    ok(end)
}

fn scan_substitution(view: &mut SourceView<'_>, cursor: u32, delim: u8) -> ScanResult {
    let first = scan_delimited(view, cursor);
    if first.is_error {
        return first;
    }
    if pair_close(delim).is_some() {
        if view.byte_at(first.end) == Some(delim) {
            return scan_delimited(view, first.end);
        }
        return first;
    }
    scan_tail_to_delim(view, first.end, delim)
}

fn scan_tail_to_delim(view: &mut SourceView<'_>, cursor: u32, delim: u8) -> ScanResult {
    let mut end = cursor;
    loop {
        match view.byte_at(end) {
            None | Some(b'\n') | Some(b'\r') => return err(end),
            Some(b'\\') => {
                end += 1;
                if view.byte_at(end).is_some() {
                    end += 1;
                }
            }
            Some(b) if b == delim => return ok(end + 1),
            Some(_) => end += 1,
        }
    }
}

fn scan_flags(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut end = cursor;
    while let Some(b) = view.byte_at(end) {
        if byteclass::is_ident_cont(b) {
            end += 1;
        } else {
            break;
        }
    }
    end
}

fn scan_operator(view: &mut SourceView<'_>, cursor: u32, b0: u8) -> (u16, u32) {
    let b1 = view.byte_at(cursor + 1);
    let (kind, len) = match b0 {
        b'+' => match b1 {
            Some(b'+') => (kinds::PLUS_PLUS, 2),
            Some(b'=') => (kinds::PLUS_EQ, 2),
            _ => (kinds::PLUS, 1),
        },
        b'-' => match b1 {
            Some(b'>') => (kinds::THIN_ARROW, 2),
            Some(b'-') => (kinds::MINUS_MINUS, 2),
            Some(b'=') => (kinds::MINUS_EQ, 2),
            _ => (kinds::MINUS, 1),
        },
        b'*' => match b1 {
            Some(b'*') if view.byte_at(cursor + 2) == Some(b'=') => (kinds::STAR_STAR_EQ, 3),
            Some(b'*') => (kinds::STAR_STAR, 2),
            Some(b'=') => (kinds::STAR_EQ, 2),
            _ => (kinds::STAR, 1),
        },
        b'/' => match b1 {
            Some(b'/') if view.byte_at(cursor + 2) == Some(b'=') => (kinds::SLASH_SLASH_EQ, 3),
            Some(b'/') => (kinds::SLASH_SLASH, 2),
            Some(b'=') => (kinds::SLASH_EQ, 2),
            _ => (kinds::SLASH, 1),
        },
        b'&' => match b1 {
            Some(b'&') => (kinds::AMP_AMP, 2),
            Some(b'=') => (kinds::AMP_EQ, 2),
            _ => (kinds::AMP, 1),
        },
        b'|' => match b1 {
            Some(b'|') => (kinds::PIPE_PIPE, 2),
            Some(b'=') => (kinds::PIPE_EQ, 2),
            _ => (kinds::PIPE, 1),
        },
        b'^' => match b1 {
            Some(b'=') => (kinds::CARET_EQ, 2),
            _ => (kinds::CARET, 1),
        },
        b'<' => match b1 {
            Some(b'=') => (kinds::LT_EQ, 2),
            Some(b'<') => (kinds::SHL, 2),
            _ => (kinds::LT, 1),
        },
        b'>' => match b1 {
            Some(b'=') => (kinds::GT_EQ, 2),
            Some(b'>') => (kinds::SHR, 2),
            _ => (kinds::GT, 1),
        },
        b'=' => match b1 {
            Some(b'=') => (kinds::EQ_EQ, 2),
            Some(b'>') => (kinds::FAT_ARROW, 2),
            _ => (kinds::EQ, 1),
        },
        b'!' => match b1 {
            Some(b'=') => (kinds::BANG_EQ, 2),
            _ => (kinds::BANG, 1),
        },
        b':' => match b1 {
            Some(b':') => (kinds::COLON_COLON, 2),
            _ => (kinds::COLON, 1),
        },
        b'.' => match b1 {
            Some(b'.') if view.byte_at(cursor + 2) == Some(b'.') => (kinds::ELLIPSIS, 3),
            Some(b'.') => (kinds::DOT_DOT, 2),
            _ => (kinds::DOT, 1),
        },
        b',' => (kinds::COMMA, 1),
        b';' => (kinds::SEMI, 1),
        b'?' => (kinds::QUESTION, 1),
        b'~' => (kinds::TILDE, 1),
        b'(' => (kinds::OPEN_PAREN, 1),
        b')' => (kinds::CLOSE_PAREN, 1),
        b'{' => (kinds::OPEN_BRACE, 1),
        b'}' => (kinds::CLOSE_BRACE, 1),
        b'[' => (kinds::OPEN_BRACKET, 1),
        b']' => (kinds::CLOSE_BRACKET, 1),
        _ => (kinds::ERROR, 1),
    };
    (kind, cursor + len)
}

const fn ok(end: u32) -> ScanResult {
    ScanResult {
        end,
        is_error: false,
    }
}

const fn err(end: u32) -> ScanResult {
    ScanResult {
        end,
        is_error: true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::LexStep;
    use crate::{Source, SourceView};

    fn run(s: &str) -> Vec<(u16, u32, u32, u8)> {
        let src: &dyn Source = &s;
        let mut view = SourceView::new(src, 0);
        let mut out = StepBuf::new();
        let mut cursor = 0u32;
        let mut state = LexState::INITIAL;
        let mut tokens = Vec::new();
        loop {
            out.clear();
            let prev = cursor;
            (cursor, state) = Perl::step_batch(&mut view, cursor, state, &mut out);
            let mut pos = prev;
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
                    LexStep::Descend { .. } => unreachable!("Perl has no embedding"),
                }
            }
            if saw_eof {
                return tokens;
            }
        }
    }

    #[test]
    fn common_perl_tokens() {
        let tokens = run("use strict; my $x = qr/foo/i; if ($x) { say qq{hi $x}; } # ok\n");
        for kind in [
            kinds::KEYWORD,
            kinds::IDENT,
            kinds::REGEX,
            kinds::STRING,
            kinds::COMMENT,
            kinds::SEMI,
            kinds::OPEN_BRACE,
            kinds::CLOSE_BRACE,
        ] {
            assert!(tokens.iter().any(|t| t.0 == kind), "missing kind {kind}");
        }
    }

    #[test]
    fn slash_without_quote_operator_is_local() {
        let tokens = run("$x / $y\nnext;");
        assert!(tokens.iter().any(|t| t.0 == kinds::SLASH));
        assert!(tokens.iter().all(|t| t.3 & flags::IS_ERROR == 0));
    }
}
