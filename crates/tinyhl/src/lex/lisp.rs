//! Lisp/Scheme lexer.
//!
//! State encoding: stateless. Every emitted token returns
//! [`LexState::INITIAL`] and carries [`STATE_BREAKPOINT`]. The lexer covers
//! common Lisp-family syntax: whitespace, `;` line comments, nested `#| |#`
//! block comments, strings, symbols, numbers, booleans/chars, quote
//! punctuation, and delimiters.
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
    static LISP_KWS = [
        (b"and", kinds::KEYWORD),
        (b"begin", kinds::KEYWORD),
        (b"case", kinds::KEYWORD),
        (b"cond", kinds::KEYWORD),
        (b"define", kinds::KEYWORD),
        (b"defmacro", kinds::KEYWORD),
        (b"defparameter", kinds::KEYWORD),
        (b"defun", kinds::KEYWORD),
        (b"defvar", kinds::KEYWORD),
        (b"delay", kinds::KEYWORD),
        (b"do", kinds::KEYWORD),
        (b"else", kinds::KEYWORD),
        (b"if", kinds::KEYWORD),
        (b"lambda", kinds::KEYWORD),
        (b"let", kinds::KEYWORD),
        (b"let*", kinds::KEYWORD),
        (b"letrec", kinds::KEYWORD),
        (b"loop", kinds::KEYWORD),
        (b"progn", kinds::KEYWORD),
        (b"quasiquote", kinds::KEYWORD),
        (b"quote", kinds::KEYWORD),
        (b"set!", kinds::KEYWORD),
        (b"unquote", kinds::KEYWORD),
        (b"unquote-splicing", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 16; // "unquote-splicing"

pub(crate) struct Lisp;

impl Lexer for Lisp {
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
        b';' => (kinds::COMMENT, scan::scan_line_to_end(view, cursor), false),
        b'#' => classify_hash(view, cursor),
        b'"' => {
            let r = scan::scan_oneline_quoted(view, cursor, first);
            (kinds::STRING, r.end, r.is_error)
        }
        b'\'' => (kinds::QUOTE, cursor + 1, false),
        b'`' => (kinds::BACKTICK, cursor + 1, false),
        b',' => (kinds::COMMA, cursor + 1, false),
        b'(' => (kinds::OPEN_PAREN, cursor + 1, false),
        b')' => (kinds::CLOSE_PAREN, cursor + 1, false),
        b'[' => (kinds::OPEN_BRACKET, cursor + 1, false),
        b']' => (kinds::CLOSE_BRACKET, cursor + 1, false),
        b'{' => (kinds::OPEN_BRACE, cursor + 1, false),
        b'}' => (kinds::CLOSE_BRACE, cursor + 1, false),
        b'.' if is_delimiter(view.byte_at(cursor + 1)) => (kinds::DOT, cursor + 1, false),
        b'+' | b'-' if matches!(view.byte_at(cursor + 1), Some(b'0'..=b'9')) => {
            let end = scan_token_run(view, cursor);
            (kinds::NUMBER, end, false)
        }
        b'0'..=b'9' => {
            let end = scan_token_run(view, cursor);
            (kinds::NUMBER, end, false)
        }
        _ => classify_symbol(view, cursor),
    }
}

fn classify_hash(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    match view.byte_at(cursor + 1) {
        Some(b'|') => {
            let r = scan_block_comment(view, cursor);
            (kinds::COMMENT, r.end, r.is_error)
        }
        Some(b'\\') => {
            let end = scan_token_run(view, cursor);
            (kinds::CHAR, end, false)
        }
        Some(b'b' | b'B' | b'o' | b'O' | b'd' | b'D' | b'x' | b'X') => {
            let end = scan_token_run(view, cursor);
            (kinds::NUMBER, end, false)
        }
        Some(b't' | b'T' | b'f' | b'F') => {
            let end = scan_token_run(view, cursor);
            (kinds::KEYWORD, end, false)
        }
        _ => (kinds::HASH, cursor + 1, false),
    }
}

fn classify_symbol(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan_token_run(view, cursor);
    if end == cursor {
        let len = scan::decoded_len_or_one(view, cursor);
        return (kinds::ERROR, cursor + len, true);
    }
    let len = (end - cursor) as usize;
    if len <= MAX_KW_LEN {
        let mut buf = [0u8; MAX_KW_LEN];
        if scan::copy_bytes(view, cursor, &mut buf[..len]) {
            if let Some(kind) = kw_lookup(LISP_KWS, &buf[..len]) {
                return (kind, end, false);
            }
        }
    }
    (kinds::IDENT, end, false)
}

fn scan_token_run(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut end = cursor;
    loop {
        match view.byte_at(end) {
            Some(b) if !is_delimiter(Some(b)) => end += 1,
            _ => return end,
        }
    }
}

fn is_delimiter(b: Option<u8>) -> bool {
    match b {
        None => true,
        Some(b) => {
            byteclass::is_whitespace(b)
                || matches!(
                    b,
                    b'(' | b')' | b'[' | b']' | b'{' | b'}' | b'"' | b'\'' | b'`' | b',' | b';'
                )
        }
    }
}

fn scan_block_comment(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor + 2;
    let mut depth = 1u32;
    while depth > 0 {
        match view.byte_at(end) {
            Some(b'#') if view.byte_at(end + 1) == Some(b'|') => {
                depth += 1;
                end += 2;
            }
            Some(b'|') if view.byte_at(end + 1) == Some(b'#') => {
                depth -= 1;
                end += 2;
            }
            Some(_) => end += 1,
            None => {
                return ScanResult {
                    end,
                    is_error: true,
                };
            }
        }
    }
    ScanResult {
        end,
        is_error: false,
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
            (cursor, state) = Lisp::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("Lisp has no embedding"),
                }
            }
            if saw_eof {
                return tokens;
            }
        }
    }

    #[test]
    fn common_lisp_tokens() {
        let tokens = run("(define (square x) #| nested #| c |# |# (* x x)) ; ok\n'foo");
        for kind in [
            kinds::OPEN_PAREN,
            kinds::CLOSE_PAREN,
            kinds::KEYWORD,
            kinds::IDENT,
            kinds::COMMENT,
            kinds::QUOTE,
        ] {
            assert!(tokens.iter().any(|t| t.0 == kind), "missing kind {kind}");
        }
        assert!(tokens.iter().all(|t| t.3 & flags::IS_ERROR == 0));
    }

    #[test]
    fn booleans_numbers_and_chars() {
        let tokens = run("(list #t #false #\\space #x2a -3.5)");
        assert!(tokens.iter().any(|t| t.0 == kinds::KEYWORD));
        assert!(tokens.iter().any(|t| t.0 == kinds::CHAR));
        assert!(tokens.iter().any(|t| t.0 == kinds::NUMBER));
    }
}
