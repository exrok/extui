//! Generic smart configuration-file lexer.
//!
//! State encoding: `mode: u3`, with canonical values:
//! `0 = line start / leading whitespace`, `1 = mid-line`,
//! `2 = mid-line after whitespace`, `3 = inside [...] section header`,
//! `4 = inside {...} section header`.
//!
//! This intentionally follows Vim/Neovim's generic `conf` shape for unknown
//! config files: comments are conservative so `text#text` stays ordinary
//! text, and quoted strings are single-line. It also borrows the common `cfg`
//! conventions of section headers, `key = value` / `key: value` pairs,
//! booleans, numbers, and leading `//` comments.
//!
//! [`STATE_BREAKPOINT`]: flags::STATE_BREAKPOINT

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

pub(crate) struct Conf;

pub(super) const ST_START: u16 = 0;
pub(super) const ST_MID: u16 = 1;
pub(super) const ST_AFTER_SPACE: u16 = 2;
pub(super) const ST_SECTION_SQUARE: u16 = 3;
pub(super) const ST_SECTION_BRACE: u16 = 4;

impl Lexer for Conf {
    fn step_batch(
        view: &mut SourceView<'_>,
        cursor: u32,
        state: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState) {
        let mut cursor = cursor;
        let mut state_id = normalize_state(state);
        while !out.is_full() {
            let Some(b) = view.byte_at(cursor) else {
                out.push(LexStep::Eof);
                return (cursor, st(state_id));
            };

            let start = cursor;
            let (local_kind, end, is_error, next_state) = classify(view, cursor, b, state_id);
            debug_assert!(end > start, "lexer must consume at least one byte");
            state_id = next_state;

            let mut bit_flags = flags::STATE_BREAKPOINT;
            if is_error {
                bit_flags |= flags::IS_ERROR;
            }

            out.push(LexStep::Token {
                len: end - start,
                local_kind,
                state_out: st(state_id),
                flags: bit_flags,
            });
            cursor = end;
        }
        (cursor, st(state_id))
    }

    fn is_safe_state(state: LexState) -> bool {
        matches!(
            state.bits(),
            ST_START | ST_MID | ST_AFTER_SPACE | ST_SECTION_SQUARE | ST_SECTION_BRACE
        )
    }
}

fn classify(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
    state: u16,
) -> (u16, u32, bool, u16) {
    match first {
        b if byteclass::is_whitespace(b) => {
            let end = scan::scan_whitespace(view, cursor);
            return (
                kinds::WHITESPACE,
                end,
                false,
                state_after_whitespace(view, cursor, end, state),
            );
        }
        b'#' | b';' if is_comment_state(state) => {
            let end = scan::scan_line_to_end(view, cursor);
            return (kinds::COMMENT, end, false, ST_MID);
        }
        b'/' if view.byte_at(cursor + 1) == Some(b'/') && state == ST_START => {
            let end = scan::scan_line_to_end(view, cursor);
            return (kinds::COMMENT, end, false, ST_MID);
        }
        b'"' | b'\'' => {
            let r = scan::scan_oneline_quoted(view, cursor, first);
            return (kinds::STRING, r.end, r.is_error, ST_MID);
        }
        b'[' => {
            return (
                kinds::OPEN_BRACKET,
                cursor + 1,
                false,
                if state == ST_START {
                    ST_SECTION_SQUARE
                } else {
                    ST_MID
                },
            );
        }
        b']' => return (kinds::CLOSE_BRACKET, cursor + 1, false, ST_MID),
        b'{' => {
            return (
                kinds::OPEN_BRACE,
                cursor + 1,
                false,
                if state == ST_START {
                    ST_SECTION_BRACE
                } else {
                    ST_MID
                },
            );
        }
        b'}' => return (kinds::CLOSE_BRACE, cursor + 1, false, ST_MID),
        b'(' => return (kinds::OPEN_PAREN, cursor + 1, false, ST_MID),
        b')' => return (kinds::CLOSE_PAREN, cursor + 1, false, ST_MID),
        _ => {}
    }

    if let Some(end) = scan_section_name(view, cursor, state) {
        return (kinds::TAG_NAME, end, false, state);
    }

    if state == ST_START {
        if let Some(end) = scan_assignment_key(view, cursor, false, true) {
            return (kinds::ATTR_NAME, end, false, ST_MID);
        }
    }

    if starts_number(view, cursor, first) {
        let r = scan_number(view, cursor);
        return (kinds::NUMBER, r.end, r.is_error, ST_MID);
    }

    match first {
        b'=' => (kinds::EQ, cursor + 1, false, ST_MID),
        b':' => (kinds::COLON, cursor + 1, false, ST_MID),
        b',' => (kinds::COMMA, cursor + 1, false, ST_MID),
        b';' => (kinds::SEMI, cursor + 1, false, ST_MID),
        _ => {
            let end = scan_plain(view, cursor, true, state);
            if end == cursor {
                (kinds::ERROR, cursor + 1, true, ST_MID)
            } else {
                (plain_kind(view, cursor, end), end, false, ST_MID)
            }
        }
    }
}

#[inline]
pub(super) const fn st(state: u16) -> LexState {
    LexState(state)
}

#[inline]
pub(super) const fn normalize_state(state: LexState) -> u16 {
    match state.bits() {
        ST_START | ST_MID | ST_AFTER_SPACE | ST_SECTION_SQUARE | ST_SECTION_BRACE => state.bits(),
        _ => ST_START,
    }
}

#[inline]
pub(super) const fn is_comment_state(state: u16) -> bool {
    state == ST_START || state == ST_AFTER_SPACE
}

pub(super) fn is_space_comment_start(view: &mut SourceView<'_>, cursor: u32) -> bool {
    if cursor == 0 {
        return true;
    }
    matches!(view.byte_at(cursor - 1), Some(b' ' | b'\t' | b'\n' | b'\r'))
}

pub(super) fn state_after_whitespace(
    view: &mut SourceView<'_>,
    start: u32,
    end: u32,
    state: u16,
) -> u16 {
    let mut p = start;
    while p < end {
        if matches!(view.byte_at(p), Some(b'\n' | b'\r')) {
            return ST_START;
        }
        p += 1;
    }
    if state == ST_START || state == ST_SECTION_SQUARE || state == ST_SECTION_BRACE {
        state
    } else {
        ST_AFTER_SPACE
    }
}

pub(super) fn scan_assignment_key(
    view: &mut SourceView<'_>,
    cursor: u32,
    allow_bare_colon: bool,
    slash_comment: bool,
) -> Option<u32> {
    match view.byte_at(cursor)? {
        b'[' | b'{' | b'(' | b']' | b'}' | b')' | b'=' | b':' | b'#' | b';' | b'"' | b'\'' => {
            return None;
        }
        b'/' if slash_comment && view.byte_at(cursor + 1) == Some(b'/') => return None,
        _ => {}
    }

    let delim = find_assignment_delim(view, cursor, allow_bare_colon, slash_comment)?;
    let mut end = delim;
    while end > cursor {
        match view.byte_at(end - 1) {
            Some(b' ' | b'\t' | b'\r') => end -= 1,
            _ => break,
        }
    }
    if end > cursor { Some(end) } else { None }
}

pub(super) fn scan_section_name(view: &mut SourceView<'_>, cursor: u32, state: u16) -> Option<u32> {
    let close = match state {
        ST_SECTION_SQUARE => b']',
        ST_SECTION_BRACE => b'}',
        _ => return None,
    };
    if matches!(view.byte_at(cursor)?, b if b == close || b == b'\n' || b == b'\r') {
        return None;
    }

    let mut end = cursor;
    loop {
        match view.byte_at(end) {
            Some(b) if b == close || b == b'\n' || b == b'\r' || b == b' ' || b == b'\t' => break,
            Some(_) => end += 1,
            None => break,
        }
    }
    if end > cursor { Some(end) } else { None }
}

pub(super) fn starts_number(view: &mut SourceView<'_>, cursor: u32, first: u8) -> bool {
    match first {
        b'0'..=b'9' => true,
        b'+' | b'-' => match view.byte_at(cursor + 1) {
            Some(b'0'..=b'9') => true,
            Some(b'.') => matches!(view.byte_at(cursor + 2), Some(b'0'..=b'9')),
            _ => false,
        },
        b'.' => matches!(view.byte_at(cursor + 1), Some(b'0'..=b'9')),
        _ => false,
    }
}

pub(super) fn scan_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor;
    if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
        end += 1;
    }

    let mut digits = 0u32;
    end = scan_number_digits(view, end, &mut digits);

    if view.byte_at(end) == Some(b'.') {
        end += 1;
        end = scan_number_digits(view, end, &mut digits);
    }

    if digits == 0 {
        return ScanResult {
            end,
            is_error: true,
        };
    }

    match view.byte_at(end) {
        Some(b'e') | Some(b'E') => {
            let exp = end + 1;
            let exp_digits_start = if matches!(view.byte_at(exp), Some(b'+') | Some(b'-')) {
                exp + 1
            } else {
                exp
            };
            let mut exp_digits = 0u32;
            let exp_end = scan_number_digits(view, exp_digits_start, &mut exp_digits);
            if exp_digits == 0 {
                return ScanResult {
                    end: exp_end,
                    is_error: true,
                };
            }
            end = exp_end;
        }
        _ => {}
    }

    ScanResult {
        end,
        is_error: false,
    }
}

pub(super) fn scan_plain(
    view: &mut SourceView<'_>,
    cursor: u32,
    slash_comment: bool,
    state: u16,
) -> u32 {
    let mut end = cursor;
    loop {
        match view.byte_at(end) {
            None => return end,
            Some(b) if byteclass::is_whitespace(b) => return end,
            Some(b'"' | b'\'' | b'[' | b']' | b'{' | b'}' | b'(' | b')' | b'=' | b':' | b',') => {
                return end;
            }
            Some(b';') => return end,
            Some(b'#') if is_space_comment_start(view, end) => return end,
            Some(b'/')
                if slash_comment && view.byte_at(end + 1) == Some(b'/') && state == ST_START =>
            {
                return end;
            }
            Some(_) => end += 1,
        }
    }
}

pub(super) fn plain_kind(view: &mut SourceView<'_>, start: u32, end: u32) -> u16 {
    if is_bool_word(view, start, end) {
        kinds::KEYWORD
    } else if is_identish(view, start, end) {
        kinds::IDENT
    } else {
        kinds::TEXT
    }
}

fn find_assignment_delim(
    view: &mut SourceView<'_>,
    cursor: u32,
    allow_bare_colon: bool,
    slash_comment: bool,
) -> Option<u32> {
    let mut p = cursor;
    loop {
        match view.byte_at(p)? {
            b'\n' | b'\r' => return None,
            b'#' | b';' if is_space_comment_start(view, p) => return None,
            b'/' if slash_comment && p == cursor && view.byte_at(p + 1) == Some(b'/') => {
                return None;
            }
            b'"' | b'\'' => {
                let quote = view.byte_at(p)?;
                let r = scan::scan_oneline_quoted(view, p, quote);
                p = r.end;
            }
            b'=' => return Some(p),
            b':' if allow_bare_colon || is_colon_delim(view, p) => return Some(p),
            _ => p += 1,
        }
    }
}

fn is_colon_delim(view: &mut SourceView<'_>, cursor: u32) -> bool {
    matches!(
        view.byte_at(cursor + 1),
        None | Some(b' ' | b'\t' | b'\r' | b'\n')
    )
}

fn scan_number_digits(view: &mut SourceView<'_>, cursor: u32, digits: &mut u32) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let mut i = (cursor - base) as usize;
        while i < page.len() && byteclass::is_digit(page[i]) {
            i += 1;
            *digits += 1;
        }
        let consumed = i - (cursor - base) as usize;
        cursor += consumed as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

fn is_bool_word(view: &mut SourceView<'_>, start: u32, end: u32) -> bool {
    match end - start {
        2 => {
            word_eq_ignore_ascii_case(view, start, b"no")
                || word_eq_ignore_ascii_case(view, start, b"on")
        }
        3 => {
            word_eq_ignore_ascii_case(view, start, b"off")
                || word_eq_ignore_ascii_case(view, start, b"yes")
        }
        4 => word_eq_ignore_ascii_case(view, start, b"true"),
        5 => word_eq_ignore_ascii_case(view, start, b"false"),
        _ => false,
    }
}

fn word_eq_ignore_ascii_case(view: &mut SourceView<'_>, start: u32, word: &[u8]) -> bool {
    let mut i = 0usize;
    while i < word.len() {
        let Some(b) = view.byte_at(start + i as u32) else {
            return false;
        };
        if b.to_ascii_lowercase() != word[i] {
            return false;
        }
        i += 1;
    }
    true
}

fn is_identish(view: &mut SourceView<'_>, start: u32, end: u32) -> bool {
    let Some(first) = view.byte_at(start) else {
        return false;
    };
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
            (cursor, state) = Conf::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("Conf has no embedding"),
                }
            }
            if saw_eof {
                break;
            }
        }
        tokens
    }

    fn kinds_of(s: &str) -> Vec<u16> {
        run(s).into_iter().map(|t| t.0).collect()
    }

    #[test]
    fn highlights_keys_values_sections_and_comments() {
        let kinds = kinds_of(
            "# top\nserver.port = 8080\nfeature: ON\npath = /tmp/a#b # trailing\n// cfg\n[profile]\n{legacy}\nquote = \"a\\\"b\"\n",
        );
        for required in [
            kinds::COMMENT,
            kinds::ATTR_NAME,
            kinds::EQ,
            kinds::COLON,
            kinds::NUMBER,
            kinds::KEYWORD,
            kinds::TEXT,
            kinds::OPEN_BRACKET,
            kinds::OPEN_BRACE,
            kinds::TAG_NAME,
            kinds::STRING,
        ] {
            assert!(kinds.contains(&required), "missing kind {required}");
        }
    }

    #[test]
    fn avoids_midword_hash_comment() {
        let tokens = run("path=/tmp/a#b\n");
        assert!(!tokens.iter().any(|t| t.0 == kinds::COMMENT));
    }

    #[test]
    fn colon_key_requires_value_separator_shape() {
        assert_eq!(kinds_of("http://example.test\n")[0], kinds::IDENT);
        assert_eq!(kinds_of("name: value\n")[0], kinds::ATTR_NAME);
    }

    #[test]
    fn all_tokens_are_breakpoints() {
        for (_, _, _, token_flags) in run("a = 1\n; c\n[sec]\n") {
            assert_ne!(
                token_flags & flags::STATE_BREAKPOINT,
                0,
                "missing breakpoint"
            );
        }
    }
}
