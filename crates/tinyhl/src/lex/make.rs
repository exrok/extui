//! Minimal Makefile lexer.
//!
//! State encoding: one `mid_line` bit. Make has significant line starts, but
//! this lexer keeps the model local: comments, variable references, quoted
//! strings, assignment/rule operators, and ordinary words are enough for useful
//! highlighting without carrying parser context across chunks.
//!
//! Deliberate "good enough" choices:
//!
//! - Recipe bodies are tokenized with the same byte rules as the rest of the
//!   file; embedded shell is not modelled.
//! - `$(...)` / `${...}` variable references are highlighted as [`IDENT`] only
//!   when their closer appears before the next newline.
//! - Quoted strings stop at the next quote or line break, so an unclosed quote
//!   cannot swallow later make rules.
//!
//! [`IDENT`]: kinds::IDENT

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

kw::kw_table! {
    static MAKE_KWS = [
        (b".DEFAULT", kinds::KEYWORD),
        (b".DELETE_ON_ERROR", kinds::KEYWORD),
        (b".PHONY", kinds::KEYWORD),
        (b".SECONDARY", kinds::KEYWORD),
        (b".SUFFIXES", kinds::KEYWORD),
        (b"define", kinds::KEYWORD),
        (b"else", kinds::KEYWORD),
        (b"endef", kinds::KEYWORD),
        (b"endif", kinds::KEYWORD),
        (b"export", kinds::KEYWORD),
        (b"if", kinds::KEYWORD),
        (b"ifdef", kinds::KEYWORD),
        (b"ifeq", kinds::KEYWORD),
        (b"ifndef", kinds::KEYWORD),
        (b"ifneq", kinds::KEYWORD),
        (b"include", kinds::KEYWORD),
        (b"override", kinds::KEYWORD),
        (b"private", kinds::KEYWORD),
        (b"undefine", kinds::KEYWORD),
        (b"unexport", kinds::KEYWORD),
        (b"vpath", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 16; // ".DELETE_ON_ERROR"

mod state {
    pub const MID_LINE: u16 = 1 << 0;
}

pub(crate) struct Make;

impl Lexer for Make {
    fn step_batch(
        view: &mut SourceView<'_>,
        cursor: u32,
        state: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState) {
        let mut cursor = cursor;
        let mut state = canonical_state(state);
        while !out.is_full() {
            let Some(b) = view.byte_at(cursor) else {
                out.push(LexStep::Eof);
                return (cursor, state);
            };

            let start = cursor;
            let (local_kind, end, is_error) = classify(view, cursor, b);
            debug_assert!(end > start, "lexer must consume at least one byte");
            let state_out = state_after(view, end);

            let mut bit_flags = flags::STATE_BREAKPOINT;
            if is_error {
                bit_flags |= flags::IS_ERROR;
            }

            out.push(LexStep::Token {
                len: end - start,
                local_kind,
                state_out,
                flags: bit_flags,
            });
            cursor = end;
            state = state_out;
        }
        (cursor, state)
    }

    fn is_safe_state(_state: LexState) -> bool {
        true
    }
}

#[inline]
fn canonical_state(state: LexState) -> LexState {
    LexState(state.bits() & state::MID_LINE)
}

#[inline]
fn state_after(view: &mut SourceView<'_>, end: u32) -> LexState {
    if end > 0 && view.byte_at(end - 1) == Some(b'\n') {
        LexState::INITIAL
    } else {
        LexState(state::MID_LINE)
    }
}

fn classify(view: &mut SourceView<'_>, cursor: u32, first: u8) -> (u16, u32, bool) {
    match first {
        b if byteclass::is_whitespace(b) => {
            let end = scan::scan_whitespace(view, cursor);
            (kinds::WHITESPACE, end, false)
        }
        b'#' => {
            let end = scan::scan_hash_comment(view, cursor);
            (kinds::COMMENT, end, false)
        }
        b'"' | b'\'' => {
            let r = scan_make_string(view, cursor, first);
            (kinds::STRING, r.end, r.is_error)
        }
        b'$' => match scan_variable_ref(view, cursor) {
            Some(end) => (kinds::IDENT, end, false),
            None => (kinds::DOLLAR, cursor + 1, false),
        },
        b':' => match view.byte_at(cursor + 1) {
            Some(b':') => match view.byte_at(cursor + 2) {
                Some(b'=') => {
                    if view.byte_at(cursor + 3) == Some(b'=') {
                        (kinds::COLON_EQ, cursor + 4, false)
                    } else {
                        (kinds::COLON_EQ, cursor + 3, false)
                    }
                }
                _ => (kinds::COLON_COLON, cursor + 2, false),
            },
            Some(b'=') => (kinds::COLON_EQ, cursor + 2, false),
            _ => (kinds::COLON, cursor + 1, false),
        },
        b'?' if view.byte_at(cursor + 1) == Some(b'=') => (kinds::QUESTION, cursor + 2, false),
        b'+' if view.byte_at(cursor + 1) == Some(b'=') => (kinds::PLUS_EQ, cursor + 2, false),
        b'!' if view.byte_at(cursor + 1) == Some(b'=') => (kinds::BANG_EQ, cursor + 2, false),
        b'=' => (kinds::EQ, cursor + 1, false),
        b'(' => (kinds::OPEN_PAREN, cursor + 1, false),
        b')' => (kinds::CLOSE_PAREN, cursor + 1, false),
        b'{' => (kinds::OPEN_BRACE, cursor + 1, false),
        b'}' => (kinds::CLOSE_BRACE, cursor + 1, false),
        b'[' => (kinds::OPEN_BRACKET, cursor + 1, false),
        b']' => (kinds::CLOSE_BRACKET, cursor + 1, false),
        b',' => (kinds::COMMA, cursor + 1, false),
        b';' => (kinds::SEMI, cursor + 1, false),
        b'@' => (kinds::AT, cursor + 1, false),
        b'0'..=b'9' => {
            let r = scan::scan_c_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        _ => classify_word(view, cursor),
    }
}

fn classify_word(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan_make_word(view, cursor);
    if end == cursor {
        if let Some((_, len)) = scan::decode_char_at(view, cursor) {
            return (kinds::ERROR, cursor + len, true);
        }
        return (kinds::ERROR, cursor + 1, true);
    }

    let len = (end - cursor) as usize;
    if len <= MAX_KW_LEN {
        let mut buf = [0u8; MAX_KW_LEN];
        if scan::copy_bytes(view, cursor, &mut buf[..len]) {
            if let Some(kind) = kw_lookup(MAKE_KWS, &buf[..len]) {
                return (kind, end, false);
            }
        }
    }

    let kind = if is_identish_word(view, cursor, end) {
        kinds::IDENT
    } else {
        kinds::TEXT
    };
    (kind, end, false)
}

fn scan_make_word(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let rel = (cursor - base) as usize;
        let mut i = rel;
        while i < page.len() && !is_word_break(page[i]) {
            i += 1;
        }
        cursor = base + i as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

fn is_word_break(b: u8) -> bool {
    matches!(
        b,
        b' ' | b'\t'
            | b'\n'
            | b'\r'
            | b'#'
            | b'"'
            | b'\''
            | b'$'
            | b':'
            | b'='
            | b'?'
            | b'+'
            | b'!'
            | b'('
            | b')'
            | b'{'
            | b'}'
            | b'['
            | b']'
            | b','
            | b';'
            | b'@'
    )
}

fn is_identish_word(view: &mut SourceView<'_>, start: u32, end: u32) -> bool {
    let Some(first) = view.byte_at(start) else {
        return false;
    };
    if first == b'.' {
        return true;
    }
    if !byteclass::is_ident_start(first) {
        return false;
    }
    let mut cursor = start + 1;
    while cursor < end {
        match view.byte_at(cursor) {
            Some(b) if byteclass::is_ident_cont(b) || b == b'-' || b == b'.' => cursor += 1,
            _ => return false,
        }
    }
    true
}

fn scan_variable_ref(view: &mut SourceView<'_>, cursor: u32) -> Option<u32> {
    match view.byte_at(cursor + 1) {
        Some(b'(') => scan_balanced_before_newline(view, cursor + 2, b'(', b')'),
        Some(b'{') => scan_balanced_before_newline(view, cursor + 2, b'{', b'}'),
        Some(b'\n') | Some(b'\r') | None => None,
        Some(_) => Some(cursor + 2),
    }
}

fn scan_balanced_before_newline(
    view: &mut SourceView<'_>,
    mut cursor: u32,
    open: u8,
    close: u8,
) -> Option<u32> {
    let mut depth = 1u8;
    while let Some(b) = view.byte_at(cursor) {
        match b {
            b'\n' | b'\r' => return None,
            b if b == open && depth < u8::MAX => depth += 1,
            b if b == close => {
                depth -= 1;
                if depth == 0 {
                    return Some(cursor + 1);
                }
            }
            _ => {}
        }
        cursor += 1;
    }
    None
}

fn scan_make_string(view: &mut SourceView<'_>, cursor: u32, quote: u8) -> ScanResult {
    let mut cursor = cursor + 1;
    loop {
        match view.byte_at(cursor) {
            Some(b) if b == quote => {
                return ScanResult {
                    end: cursor + 1,
                    is_error: false,
                };
            }
            Some(b'\\') => match view.byte_at(cursor + 1) {
                Some(b'\n') | Some(b'\r') | None => {
                    return ScanResult {
                        end: cursor,
                        is_error: true,
                    };
                }
                Some(_) => cursor += 2,
            },
            Some(b'\n') | Some(b'\r') | None => {
                return ScanResult {
                    end: cursor,
                    is_error: true,
                };
            }
            Some(_) => cursor += 1,
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
            (cursor, state) = Make::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("Make has no embedding"),
                }
            }
            if saw_eof {
                return tokens;
            }
        }
    }

    #[test]
    fn common_mapping_tokens() {
        let tokens = run(".PHONY: all\nCC := cc\nall: main.o\n\t$(CC) -o app main.o # build\n");
        let kinds: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert!(kinds.contains(&kinds::KEYWORD));
        assert!(kinds.contains(&kinds::IDENT));
        assert!(kinds.contains(&kinds::COLON));
        assert!(kinds.contains(&kinds::COLON_EQ));
        assert!(kinds.contains(&kinds::COMMENT));
        assert!(kinds.contains(&kinds::TEXT));
    }

    #[test]
    fn coverage_is_contiguous() {
        let input = "export CFLAGS += -O2\nmsg = \"hi\"\nifeq ($(OS),Linux)\nendif\n";
        let tokens = run(input);
        let mut pos = 0u32;
        for (_kind, offset, len, _flags) in &tokens {
            assert_eq!(*offset, pos, "gap or overlap before offset {pos}");
            pos += len;
        }
        assert_eq!(pos as usize, input.len());
    }

    #[test]
    fn unterminated_string_is_line_local_error() {
        let tokens = run("A = \"abc\nB = ok\n");
        assert!(
            tokens
                .iter()
                .any(|t| t.0 == kinds::STRING && (t.3 & flags::IS_ERROR) != 0)
        );
        assert!(tokens.iter().any(|t| t.0 == kinds::IDENT));
    }

    #[test]
    fn unmatched_variable_does_not_swallow_line() {
        let tokens = run("A = $(missing\nB = ok\n");
        assert!(tokens.iter().any(|t| t.0 == kinds::DOLLAR));
        assert!(tokens.iter().filter(|t| t.0 == kinds::IDENT).count() >= 2);
    }
}
