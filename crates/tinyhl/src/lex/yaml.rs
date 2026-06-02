//! Minimal YAML lexer.
//!
//! This is intentionally a stateless byte lexer. Full YAML scanning needs
//! indentation, simple-key, and block-scalar context; TinyHL keeps this subset
//! local so incremental re-lex can resume from any token boundary.

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

kw::kw_table! {
    static YAML_KWS = [
        (b".INF", kinds::KEYWORD),
        (b".Inf", kinds::KEYWORD),
        (b".NAN", kinds::KEYWORD),
        (b".NaN", kinds::KEYWORD),
        (b".inf", kinds::KEYWORD),
        (b".nan", kinds::KEYWORD),
        (b"FALSE", kinds::KEYWORD),
        (b"False", kinds::KEYWORD),
        (b"NULL",  kinds::KEYWORD),
        (b"Null",  kinds::KEYWORD),
        (b"TRUE",  kinds::KEYWORD),
        (b"True",  kinds::KEYWORD),
        (b"false", kinds::KEYWORD),
        (b"null",  kinds::KEYWORD),
        (b"true",  kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 5;

pub(crate) struct Yaml;

impl Lexer for Yaml {
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
        b' ' | b'\t' | b'\n' | b'\r' => {
            let end = scan::scan_whitespace(view, cursor);
            (kinds::WHITESPACE, end, false)
        }
        b'#' if is_comment_start(view, cursor) => {
            let end = scan::scan_hash_comment(view, cursor);
            (kinds::COMMENT, end, false)
        }
        b'"' => {
            let r = scan_yaml_double_string(view, cursor);
            (kinds::STRING, r.end, r.is_error)
        }
        b'\'' => {
            let r = scan_yaml_single_string(view, cursor);
            (kinds::STRING, r.end, r.is_error)
        }
        b'{' => (kinds::OPEN_BRACE, cursor + 1, false),
        b'}' => (kinds::CLOSE_BRACE, cursor + 1, false),
        b'[' => (kinds::OPEN_BRACKET, cursor + 1, false),
        b']' => (kinds::CLOSE_BRACKET, cursor + 1, false),
        b'(' => (kinds::OPEN_PAREN, cursor + 1, false),
        b')' => (kinds::CLOSE_PAREN, cursor + 1, false),
        b',' => (kinds::COMMA, cursor + 1, false),
        b':' => (kinds::COLON, cursor + 1, false),
        b'?' => (kinds::QUESTION, cursor + 1, false),
        b'|' => (kinds::PIPE, cursor + 1, false),
        b'>' => (kinds::GT, cursor + 1, false),
        b'&' => (kinds::AMP, cursor + 1, false),
        b'*' => (kinds::STAR, cursor + 1, false),
        b'!' => (kinds::BANG, cursor + 1, false),
        b'%' => (kinds::PERCENT, cursor + 1, false),
        b'=' => (kinds::EQ, cursor + 1, false),
        b'@' => (kinds::AT, cursor + 1, false),
        b'`' => (kinds::BACKTICK, cursor + 1, false),
        b'~' => (kinds::KEYWORD, cursor + 1, false),
        b'.' if view.byte_at(cursor + 1) == Some(b'.')
            && view.byte_at(cursor + 2) == Some(b'.') =>
        {
            (kinds::ELLIPSIS, cursor + 3, false)
        }
        b'.' if matches!(view.byte_at(cursor + 1), Some(b'0'..=b'9')) => {
            let r = scan_yaml_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b'+' | b'-' if starts_number_after_sign(view, cursor) => {
            let r = scan_yaml_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b'-' if view.byte_at(cursor + 1) == Some(b'-') => (kinds::MINUS_MINUS, cursor + 2, false),
        b'+' => (kinds::PLUS, cursor + 1, false),
        b'-' => (kinds::MINUS, cursor + 1, false),
        b'0'..=b'9' => {
            let r = scan_yaml_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        _ => classify_plain(view, cursor),
    }
}

fn starts_number_after_sign(view: &mut SourceView<'_>, cursor: u32) -> bool {
    match view.byte_at(cursor + 1) {
        Some(b'0'..=b'9') => true,
        Some(b'.') => matches!(view.byte_at(cursor + 2), Some(b'0'..=b'9')),
        _ => false,
    }
}

fn classify_plain(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan_plain_scalar(view, cursor);
    if end == cursor {
        return (kinds::ERROR, cursor + 1, true);
    }

    let len = (end - cursor) as usize;
    if len <= MAX_KW_LEN {
        let mut buf = [0u8; MAX_KW_LEN];
        if scan::copy_bytes(view, cursor, &mut buf[..len]) {
            if let Some(k) = kw_lookup(YAML_KWS, &buf[..len]) {
                return (k, end, false);
            }
        }
    }

    let kind = if is_identish_plain(view, cursor, end) {
        kinds::IDENT
    } else {
        kinds::TEXT
    };
    (kind, end, false)
}

fn is_identish_plain(view: &mut SourceView<'_>, start: u32, end: u32) -> bool {
    let Some(first) = view.byte_at(start) else {
        return false;
    };
    if !byteclass::is_ident_start(first) {
        return false;
    }
    let mut cursor = start + 1;
    while cursor < end {
        match view.byte_at(cursor) {
            Some(b) if byteclass::is_ident_cont(b) || b == b'-' => cursor += 1,
            _ => return false,
        }
    }
    true
}

fn scan_plain_scalar(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor;
    loop {
        let mut prev = if cursor == 0 {
            None
        } else {
            view.byte_at(cursor - 1)
        };
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let rel = (cursor - base) as usize;
        let mut i = rel;
        while i < page.len() {
            let pos = base + i as u32;
            let b = page[i];
            if is_plain_break(prev, b) {
                return pos;
            }
            prev = Some(b);
            i += 1;
        }
        cursor = base + i as u32;
        if i == rel {
            return cursor;
        }
    }
}

fn is_plain_break(prev: Option<u8>, b: u8) -> bool {
    match b {
        b' ' | b'\t' | b'\n' | b'\r' | b',' | b':' | b'{' | b'}' | b'[' | b']' | b'(' | b')'
        | b'"' | b'\'' | b'?' | b'|' | b'>' | b'&' | b'*' | b'!' | b'%' | b'=' | b'@' | b'`'
        | b'~' => true,
        b'#' => matches!(prev, None | Some(b' ' | b'\t' | b'\n' | b'\r')),
        _ => false,
    }
}

fn is_comment_start(view: &mut SourceView<'_>, cursor: u32) -> bool {
    if cursor == 0 {
        return true;
    }
    matches!(view.byte_at(cursor - 1), Some(b' ' | b'\t' | b'\n' | b'\r'))
}

fn scan_yaml_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor;
    if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
        end += 1;
    }

    if view.byte_at(end) == Some(b'0') && matches!(view.byte_at(end + 1), Some(b'x') | Some(b'X')) {
        end += 2;
        let before_digits = end;
        end = scan_digits_and_underscores(view, end, byteclass::is_hex_digit);
        return if end == before_digits {
            ScanResult {
                end,
                is_error: true,
            }
        } else {
            ScanResult {
                end,
                is_error: false,
            }
        };
    }

    let before_int = end;
    end = scan_digits_and_underscores(view, end, byteclass::is_digit);
    let mut saw_digit = end != before_int;

    if view.byte_at(end) == Some(b'.') {
        end += 1;
        let before_frac = end;
        end = scan_digits_and_underscores(view, end, byteclass::is_digit);
        saw_digit |= end != before_frac;
    }

    if !saw_digit {
        return ScanResult {
            end: end.max(cursor + 1),
            is_error: true,
        };
    }

    if matches!(view.byte_at(end), Some(b'e') | Some(b'E')) {
        end += 1;
        if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
            end += 1;
        }
        let before_exp = end;
        end = scan_digits_and_underscores(view, end, byteclass::is_digit);
        if end == before_exp {
            return ScanResult {
                end,
                is_error: true,
            };
        }
    }

    ScanResult {
        end,
        is_error: false,
    }
}

fn scan_digits_and_underscores(
    view: &mut SourceView<'_>,
    cursor: u32,
    digit: fn(u8) -> bool,
) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let rel = (cursor - base) as usize;
        let mut i = rel;
        while i < page.len() && (digit(page[i]) || page[i] == b'_') {
            i += 1;
        }
        cursor = base + i as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

fn scan_yaml_double_string(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
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
        let mut crossed_page_on_escape = false;
        while i < page.len() {
            match page[i] {
                b'"' => {
                    return ScanResult {
                        end: base + i as u32 + 1,
                        is_error: false,
                    };
                }
                b'\\' => {
                    i += 1;
                    if i < page.len() {
                        i += 1;
                    } else {
                        crossed_page_on_escape = true;
                        break;
                    }
                }
                _ => i += 1,
            }
        }
        cursor = base + i as u32;
        if crossed_page_on_escape {
            match view.byte_at(cursor) {
                Some(_) => cursor += 1,
                None => {
                    return ScanResult {
                        end: cursor,
                        is_error: true,
                    };
                }
            }
        }
    }
}

fn scan_yaml_single_string(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut cursor = cursor + 1;
    loop {
        match view.byte_at(cursor) {
            Some(b'\'') => {
                if view.byte_at(cursor + 1) == Some(b'\'') {
                    cursor += 2;
                } else {
                    return ScanResult {
                        end: cursor + 1,
                        is_error: false,
                    };
                }
            }
            Some(_) => cursor += 1,
            None => {
                return ScanResult {
                    end: cursor,
                    is_error: true,
                };
            }
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
            (cursor, state) = Yaml::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("YAML has no embedding"),
                }
            }
            if saw_eof {
                return tokens;
            }
        }
    }

    #[test]
    fn common_mapping_tokens() {
        let tokens = run("name: true\nitems: [1, null, {x: 'it''s'}]\n# ok");
        let kinds: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert!(kinds.contains(&kinds::IDENT));
        assert!(kinds.contains(&kinds::COLON));
        assert!(kinds.contains(&kinds::KEYWORD));
        assert!(kinds.contains(&kinds::OPEN_BRACKET));
        assert!(kinds.contains(&kinds::NUMBER));
        assert!(kinds.contains(&kinds::OPEN_BRACE));
        assert!(kinds.contains(&kinds::STRING));
        assert!(kinds.contains(&kinds::COMMENT));
    }

    #[test]
    fn coverage_is_contiguous() {
        let input = "---\na: -0.5\nb: \"x\\\"y\"\nc: /tmp/path#frag\n...\n";
        let tokens = run(input);
        let mut pos = 0u32;
        for (_kind, offset, len, _flags) in &tokens {
            assert_eq!(*offset, pos, "gap or overlap before offset {pos}");
            pos += len;
        }
        assert_eq!(pos as usize, input.len());
    }

    #[test]
    fn unterminated_string_is_local_error() {
        let tokens = run("\"abc");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::STRING);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn hash_inside_plain_scalar_is_not_comment() {
        let tokens = run("url: a#b # comment");
        let comments = tokens.iter().filter(|t| t.0 == kinds::COMMENT).count();
        assert_eq!(comments, 1);
        assert!(tokens.iter().any(|t| t.0 == kinds::TEXT));
    }
}
