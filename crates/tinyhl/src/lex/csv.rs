//! CSV lexer.
//!
//! This recognizes ordinary comma-separated data: comma delimiters, CR/LF/CRLF
//! record terminators, and double-quoted fields with doubled quote escapes.
//! It is lexical rather than validating; malformed input still produces a
//! contiguous token stream with only unterminated quoted fields marked as errors.

use crate::kind as kinds;
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

pub(crate) struct Csv;

impl Lexer for Csv {
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
        b',' => (kinds::COMMA, cursor + 1, false),
        b'\r' => {
            let end = if view.byte_at(cursor + 1) == Some(b'\n') {
                cursor + 2
            } else {
                cursor + 1
            };
            (kinds::WHITESPACE, end, false)
        }
        b'\n' => (kinds::WHITESPACE, cursor + 1, false),
        b'"' => {
            let (end, is_error) = scan_quoted_field(view, cursor);
            (kinds::STRING, end, is_error)
        }
        _ => {
            let end = scan_unquoted_field(view, cursor);
            let kind = if is_number_field(view, cursor, end) {
                kinds::NUMBER
            } else {
                kinds::TEXT
            };
            (kind, end, false)
        }
    }
}

fn scan_quoted_field(view: &mut SourceView<'_>, cursor: u32) -> (u32, bool) {
    let mut cursor = cursor + 1;
    while let Some(b) = view.byte_at(cursor) {
        if b == b'"' {
            let next = cursor + 1;
            if view.byte_at(next) == Some(b'"') {
                cursor += 2;
            } else {
                return (next, false);
            }
        } else {
            cursor += 1;
        }
    }
    (cursor, true)
}

fn scan_unquoted_field(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor;
    loop {
        let (_, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }

        let mut i = 0usize;
        while i < page.len() {
            match page[i] {
                b',' | b'\r' | b'\n' => return cursor + i as u32,
                _ => i += 1,
            }
        }
        cursor += page.len() as u32;
    }
}

fn is_number_field(view: &mut SourceView<'_>, mut start: u32, mut end: u32) -> bool {
    while start < end && is_hspace(view.byte_at(start)) {
        start += 1;
    }
    while end > start && is_hspace(view.byte_at(end - 1)) {
        end -= 1;
    }
    if start == end {
        return false;
    }

    let mut cursor = start;
    match view.byte_at(cursor) {
        Some(b'+') | Some(b'-') => cursor += 1,
        _ => {}
    }

    let mut digits = 0u32;
    cursor = scan_number_digits(view, cursor, end, &mut digits);

    if cursor < end && view.byte_at(cursor) == Some(b'.') {
        cursor += 1;
        cursor = scan_number_digits(view, cursor, end, &mut digits);
    }

    if digits == 0 {
        return false;
    }

    match view.byte_at(cursor) {
        Some(b'e') | Some(b'E') if cursor < end => {
            cursor += 1;
            match view.byte_at(cursor) {
                Some(b'+') | Some(b'-') => cursor += 1,
                _ => {}
            }
            let before_exp_digits = cursor;
            while cursor < end {
                match view.byte_at(cursor) {
                    Some(b'0'..=b'9') => cursor += 1,
                    _ => break,
                }
            }
            if cursor == before_exp_digits {
                return false;
            }
        }
        _ => {}
    }

    cursor == end
}

fn scan_number_digits(
    view: &mut SourceView<'_>,
    mut cursor: u32,
    end: u32,
    digits: &mut u32,
) -> u32 {
    while cursor < end {
        match view.byte_at(cursor) {
            Some(b'0'..=b'9') => {
                cursor += 1;
                *digits += 1;
            }
            _ => break,
        }
    }
    cursor
}

#[inline]
fn is_hspace(b: Option<u8>) -> bool {
    matches!(b, Some(b' ' | b'\t'))
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
            (cursor, state) = Csv::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("CSV has no embedding"),
                }
            }
            if saw_eof {
                return tokens;
            }
        }
    }

    #[test]
    fn simple_records() {
        let tokens = run("name,age\nAlice,42\n");
        let kinds: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert_eq!(
            kinds,
            vec![
                kinds::TEXT,
                kinds::COMMA,
                kinds::TEXT,
                kinds::WHITESPACE,
                kinds::TEXT,
                kinds::COMMA,
                kinds::NUMBER,
                kinds::WHITESPACE,
            ]
        );
    }

    #[test]
    fn quoted_fields_allow_delimiters_newlines_and_doubled_quotes() {
        let tokens = run("\"a,b\",\"x\"\"y\"\r\n2,-3.5");
        let kinds: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert_eq!(
            kinds,
            vec![
                kinds::STRING,
                kinds::COMMA,
                kinds::STRING,
                kinds::WHITESPACE,
                kinds::NUMBER,
                kinds::COMMA,
                kinds::NUMBER,
            ]
        );
    }

    #[test]
    fn quotes_after_field_content_are_text() {
        let tokens = run("  \"a\"  ,b");
        let kinds: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert_eq!(kinds, vec![kinds::TEXT, kinds::COMMA, kinds::TEXT]);
    }

    #[test]
    fn unterminated_quoted_field_is_error() {
        let tokens = run("\"a,b\nc");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::STRING);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }
}
