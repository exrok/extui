//! XML lexer.
//!
//! This is intentionally lexical and non-validating. It recognizes XML's
//! event-shaped boundaries (text, tags, comments, CDATA, doctype, and
//! processing instructions) while keeping tag/attribute scanning reusable for
//! future HTML and TSX lexers.

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

const MODE_TEXT: u16 = 0;
const MODE_AFTER_LT: u16 = 1;
const MODE_TAG_NAME: u16 = 2;
const MODE_TAG: u16 = 3;
const MODE_PI_TARGET: u16 = 4;
const MODE_PI: u16 = 5;
const MODE_PI_CLOSE: u16 = 6;
const MODE_MASK: u16 = 0x000F;

const DOCTYPE: &[u8] = b"<!DOCTYPE";

#[inline]
const fn state(mode: u16) -> LexState {
    LexState(mode)
}

#[inline]
const fn mode(state: LexState) -> u16 {
    state.bits() & MODE_MASK
}

pub(crate) struct Xml;

impl Lexer for Xml {
    fn step_batch(
        view: &mut SourceView<'_>,
        cursor: u32,
        state_in: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState) {
        let mut cursor = cursor;
        let mut state = state_in;
        while !out.is_full() {
            let Some(b) = view.byte_at(cursor) else {
                out.push(LexStep::Eof);
                return (cursor, state);
            };

            let start = cursor;
            let (local_kind, end, is_error, state_out) = classify(view, cursor, b, state);
            debug_assert!(end > start, "lexer must consume at least one byte");

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

fn classify(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
    state_in: LexState,
) -> (u16, u32, bool, LexState) {
    match mode(state_in) {
        MODE_TEXT => classify_text(view, cursor, first),
        MODE_AFTER_LT => classify_after_lt(view, cursor, first),
        MODE_TAG_NAME => classify_tag_name(view, cursor, first),
        MODE_TAG => classify_tag(view, cursor, first),
        MODE_PI_TARGET => classify_pi_target(view, cursor, first),
        MODE_PI => classify_pi(view, cursor, first),
        MODE_PI_CLOSE => classify_pi_close(view, cursor, first),
        _ => (
            kinds::ERROR,
            error_end(view, cursor),
            true,
            LexState::INITIAL,
        ),
    }
}

fn classify_text(view: &mut SourceView<'_>, cursor: u32, first: u8) -> (u16, u32, bool, LexState) {
    if first == b'<' {
        if starts_with(view, cursor, b"<!--") {
            let r = scan_comment(view, cursor);
            return (kinds::COMMENT, r.end, r.is_error, LexState::INITIAL);
        }
        if starts_with(view, cursor, b"<![CDATA[") {
            let r = scan_cdata(view, cursor);
            return (kinds::CDATA, r.end, r.is_error, LexState::INITIAL);
        }
        if starts_doctype(view, cursor) {
            let r = scan_doctype(view, cursor);
            return (kinds::DOCTYPE, r.end, r.is_error, LexState::INITIAL);
        }
        return (kinds::LT, cursor + 1, false, state(MODE_AFTER_LT));
    }

    if first == b'&' {
        let r = scan_entity_ref(view, cursor);
        return (kinds::ENTITY_REF, r.end, r.is_error, LexState::INITIAL);
    }

    let end = scan_text_run(view, cursor);
    (kinds::TEXT, end, false, LexState::INITIAL)
}

fn classify_after_lt(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
) -> (u16, u32, bool, LexState) {
    match first {
        b if byteclass::is_whitespace(b) => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            true,
            state(MODE_AFTER_LT),
        ),
        b'/' => (kinds::SLASH, cursor + 1, false, state(MODE_TAG_NAME)),
        b'?' => (kinds::QUESTION, cursor + 1, false, state(MODE_PI_TARGET)),
        b'!' => (kinds::BANG, cursor + 1, true, state(MODE_TAG)),
        b'>' => (kinds::GT, cursor + 1, true, LexState::INITIAL),
        b if is_name_start(view, cursor, b, false) => {
            let end = scan_name(view, cursor, false);
            (kinds::TAG_NAME, end, false, state(MODE_TAG))
        }
        _ => (
            kinds::ERROR,
            error_end(view, cursor),
            true,
            state(MODE_TAG_NAME),
        ),
    }
}

fn classify_tag_name(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
) -> (u16, u32, bool, LexState) {
    match first {
        b if byteclass::is_whitespace(b) => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            true,
            state(MODE_TAG_NAME),
        ),
        b if is_name_start(view, cursor, b, false) => {
            let end = scan_name(view, cursor, false);
            (kinds::TAG_NAME, end, false, state(MODE_TAG))
        }
        b'>' => (kinds::GT, cursor + 1, true, LexState::INITIAL),
        b'/' => (kinds::SLASH, cursor + 1, true, state(MODE_TAG_NAME)),
        _ => (
            kinds::ERROR,
            error_end(view, cursor),
            true,
            state(MODE_TAG_NAME),
        ),
    }
}

fn classify_tag(view: &mut SourceView<'_>, cursor: u32, first: u8) -> (u16, u32, bool, LexState) {
    match first {
        b if byteclass::is_whitespace(b) => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            false,
            state(MODE_TAG),
        ),
        b'>' => (kinds::GT, cursor + 1, false, LexState::INITIAL),
        b'/' => (kinds::SLASH, cursor + 1, false, state(MODE_TAG)),
        b'=' => (kinds::EQ, cursor + 1, false, state(MODE_TAG)),
        b'\'' | b'"' => {
            let r = scan_quoted(view, cursor, first);
            (kinds::STRING, r.end, r.is_error, state(MODE_TAG))
        }
        b'&' => {
            let r = scan_entity_ref(view, cursor);
            (kinds::ENTITY_REF, r.end, r.is_error, state(MODE_TAG))
        }
        b if is_name_start(view, cursor, b, false) => {
            let end = scan_name(view, cursor, false);
            (kinds::ATTR_NAME, end, false, state(MODE_TAG))
        }
        b':' => (kinds::COLON, cursor + 1, false, state(MODE_TAG)),
        b'.' => (kinds::DOT, cursor + 1, false, state(MODE_TAG)),
        b'-' => (kinds::MINUS, cursor + 1, false, state(MODE_TAG)),
        b'?' => (kinds::QUESTION, cursor + 1, false, state(MODE_TAG)),
        b'<' => (kinds::LT, cursor + 1, true, state(MODE_AFTER_LT)),
        _ => (kinds::ERROR, error_end(view, cursor), true, state(MODE_TAG)),
    }
}

fn classify_pi_target(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
) -> (u16, u32, bool, LexState) {
    match first {
        b if byteclass::is_whitespace(b) => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            true,
            state(MODE_PI_TARGET),
        ),
        b if is_name_start(view, cursor, b, false) => {
            let end = scan_name(view, cursor, false);
            (kinds::TAG_NAME, end, false, state(MODE_PI))
        }
        b'?' if view.byte_at(cursor + 1) == Some(b'>') => {
            (kinds::QUESTION, cursor + 1, true, state(MODE_PI_CLOSE))
        }
        b'>' => (kinds::GT, cursor + 1, true, LexState::INITIAL),
        _ => (
            kinds::ERROR,
            error_end(view, cursor),
            true,
            state(MODE_PI_TARGET),
        ),
    }
}

fn classify_pi(view: &mut SourceView<'_>, cursor: u32, first: u8) -> (u16, u32, bool, LexState) {
    if first == b'?' && view.byte_at(cursor + 1) == Some(b'>') {
        return (kinds::QUESTION, cursor + 1, false, state(MODE_PI_CLOSE));
    }

    match first {
        b if byteclass::is_whitespace(b) => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            false,
            state(MODE_PI),
        ),
        b'=' => (kinds::EQ, cursor + 1, false, state(MODE_PI)),
        b'\'' | b'"' => {
            let r = scan_quoted(view, cursor, first);
            (kinds::STRING, r.end, r.is_error, state(MODE_PI))
        }
        b'&' => {
            let r = scan_entity_ref(view, cursor);
            (kinds::ENTITY_REF, r.end, r.is_error, state(MODE_PI))
        }
        b if is_name_start(view, cursor, b, false) => {
            let end = scan_name(view, cursor, false);
            (kinds::ATTR_NAME, end, false, state(MODE_PI))
        }
        b':' => (kinds::COLON, cursor + 1, false, state(MODE_PI)),
        b'.' => (kinds::DOT, cursor + 1, false, state(MODE_PI)),
        b'-' => (kinds::MINUS, cursor + 1, false, state(MODE_PI)),
        b'>' => (kinds::GT, cursor + 1, false, state(MODE_PI)),
        b'<' => (kinds::LT, cursor + 1, true, state(MODE_AFTER_LT)),
        _ => (kinds::ERROR, error_end(view, cursor), true, state(MODE_PI)),
    }
}

fn classify_pi_close(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
) -> (u16, u32, bool, LexState) {
    if first == b'>' {
        (kinds::GT, cursor + 1, false, LexState::INITIAL)
    } else {
        classify_pi(view, cursor, first)
    }
}

/// Returns true if `pattern` matches at `cursor`.
pub(crate) fn starts_with(view: &mut SourceView<'_>, cursor: u32, pattern: &[u8]) -> bool {
    for (i, &b) in pattern.iter().enumerate() {
        if view.byte_at(cursor + i as u32) != Some(b) {
            return false;
        }
    }
    true
}

fn starts_with_ascii_case(view: &mut SourceView<'_>, cursor: u32, pattern: &[u8]) -> bool {
    for (i, &b) in pattern.iter().enumerate() {
        let Some(got) = view.byte_at(cursor + i as u32) else {
            return false;
        };
        if got.to_ascii_uppercase() != b {
            return false;
        }
    }
    true
}

pub(crate) fn starts_doctype(view: &mut SourceView<'_>, cursor: u32) -> bool {
    if !starts_with_ascii_case(view, cursor, DOCTYPE) {
        return false;
    }
    match view.byte_at(cursor + DOCTYPE.len() as u32) {
        None | Some(b'>') => true,
        Some(b) => byteclass::is_whitespace(b),
    }
}

/// Scans a markup name. `allow_dollar` is kept for future TSX / JSX callers.
pub(crate) fn scan_name(view: &mut SourceView<'_>, cursor: u32, allow_dollar: bool) -> u32 {
    let Some(b0) = view.byte_at(cursor) else {
        return cursor;
    };
    let start_len = if b0 < 0x80 {
        if is_name_start_ascii(b0, allow_dollar) {
            1
        } else {
            return cursor;
        }
    } else {
        let Some((c, len)) = scan::decode_char_at(view, cursor) else {
            return cursor;
        };
        if !unicode_ident::is_xid_start(c) {
            return cursor;
        }
        len
    };

    let mut end = cursor + start_len;
    loop {
        let (base, page) = view.window_at(end);
        if page.is_empty() {
            return end;
        }
        let mut i = (end - base) as usize;
        while i < page.len() {
            let b = page[i];
            if b >= 0x80 {
                break;
            }
            if is_name_continue_ascii(b, allow_dollar) {
                i += 1;
            } else {
                return base + i as u32;
            }
        }
        end = base + i as u32;
        if i == page.len() {
            continue;
        }
        let Some((c, len)) = scan::decode_char_at(view, end) else {
            return end;
        };
        if !unicode_ident::is_xid_continue(c) {
            return end;
        }
        end += len;
    }
}

/// Scans a quoted XML attribute value, including both delimiters.
pub(crate) fn scan_quoted(view: &mut SourceView<'_>, cursor: u32, quote: u8) -> ScanResult {
    let mut end = cursor + 1;
    loop {
        match view.byte_at(end) {
            Some(b) if b == quote => {
                return ScanResult {
                    end: end + 1,
                    is_error: false,
                };
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
}

#[inline]
pub(crate) fn is_name_start(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
    allow_dollar: bool,
) -> bool {
    if first < 0x80 {
        is_name_start_ascii(first, allow_dollar)
    } else {
        scan::decode_char_at(view, cursor)
            .map(|(c, _)| unicode_ident::is_xid_start(c))
            .unwrap_or(false)
    }
}

#[inline]
fn is_name_start_ascii(b: u8, allow_dollar: bool) -> bool {
    byteclass::is_ident_start(b) || b == b':' || (allow_dollar && b == b'$')
}

#[inline]
fn is_name_continue_ascii(b: u8, allow_dollar: bool) -> bool {
    is_name_start_ascii(b, allow_dollar) || byteclass::is_digit(b) || matches!(b, b'-' | b'.')
}

pub(crate) fn scan_text_run(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut end = cursor;
    loop {
        let (base, page) = view.window_at(end);
        if page.is_empty() {
            return end;
        }
        let mut i = (end - base) as usize;
        while i < page.len() && page[i] != b'<' && page[i] != b'&' {
            i += 1;
        }
        end = base + i as u32;
        if i < page.len() {
            return end;
        }
    }
}

pub(crate) fn scan_entity_ref(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor + 1;
    match view.byte_at(end) {
        Some(b'#') => {
            end += 1;
            let radix = match view.byte_at(end) {
                Some(b'x') | Some(b'X') => {
                    end += 1;
                    16
                }
                _ => 10,
            };
            let digits_start = end;
            while let Some(b) = view.byte_at(end) {
                let is_digit = if radix == 16 {
                    byteclass::is_hex_digit(b)
                } else {
                    byteclass::is_digit(b)
                };
                if !is_digit {
                    break;
                }
                end += 1;
            }
            if end == digits_start {
                return ScanResult {
                    end,
                    is_error: true,
                };
            }
        }
        Some(b) if is_name_start(view, end, b, false) => {
            end = scan_name(view, end, false);
        }
        _ => {
            return ScanResult {
                end: cursor + 1,
                is_error: true,
            };
        }
    }

    if view.byte_at(end) == Some(b';') {
        ScanResult {
            end: end + 1,
            is_error: false,
        }
    } else {
        ScanResult {
            end,
            is_error: true,
        }
    }
}

pub(crate) fn scan_comment(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    scan_until_seq(view, cursor + 4, b"-->")
}

pub(crate) fn scan_cdata(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    scan_until_seq(view, cursor + 9, b"]]>")
}

fn scan_until_seq(view: &mut SourceView<'_>, mut end: u32, close: &[u8]) -> ScanResult {
    loop {
        if view.byte_at(end).is_none() {
            return ScanResult {
                end,
                is_error: true,
            };
        }
        if starts_with(view, end, close) {
            return ScanResult {
                end: end + close.len() as u32,
                is_error: false,
            };
        }
        end += 1;
    }
}

pub(crate) fn scan_doctype(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor + DOCTYPE.len() as u32;
    let mut bracket_depth = 0u16;
    loop {
        match view.byte_at(end) {
            None => {
                return ScanResult {
                    end,
                    is_error: true,
                };
            }
            Some(b'\'') | Some(b'"') => {
                let quote = view.byte_at(end).unwrap();
                let r = scan_quoted(view, end, quote);
                end = r.end;
                if r.is_error {
                    return r;
                }
            }
            Some(b'[') => {
                bracket_depth = bracket_depth.saturating_add(1);
                end += 1;
            }
            Some(b']') => {
                bracket_depth = bracket_depth.saturating_sub(1);
                end += 1;
            }
            Some(b'>') if bracket_depth == 0 => {
                return ScanResult {
                    end: end + 1,
                    is_error: false,
                };
            }
            Some(_) => end += 1,
        }
    }
}

fn error_end(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    cursor
        + scan::decode_char_at(view, cursor)
            .map(|(_, len)| len)
            .unwrap_or(1)
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
            (cursor, state) = Xml::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("XML has no embedding"),
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
    fn simple_element_with_attribute() {
        assert_eq!(
            kinds_of(r#"<note id="1">text</note>"#),
            vec![
                kinds::LT,
                kinds::TAG_NAME,
                kinds::WHITESPACE,
                kinds::ATTR_NAME,
                kinds::EQ,
                kinds::STRING,
                kinds::GT,
                kinds::TEXT,
                kinds::LT,
                kinds::SLASH,
                kinds::TAG_NAME,
                kinds::GT,
            ]
        );
    }

    #[test]
    fn processing_instruction_and_special_nodes() {
        assert_eq!(
            kinds_of(r#"<?xml version="1.0"?><!--c--><![CDATA[x]]><!DOCTYPE a>"#),
            vec![
                kinds::LT,
                kinds::QUESTION,
                kinds::TAG_NAME,
                kinds::WHITESPACE,
                kinds::ATTR_NAME,
                kinds::EQ,
                kinds::STRING,
                kinds::QUESTION,
                kinds::GT,
                kinds::COMMENT,
                kinds::CDATA,
                kinds::DOCTYPE,
            ]
        );
    }

    #[test]
    fn entity_references() {
        let tokens = run("a &amp; &#10; &bad");
        let kinds: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert_eq!(
            kinds,
            vec![
                kinds::TEXT,
                kinds::ENTITY_REF,
                kinds::TEXT,
                kinds::ENTITY_REF,
                kinds::TEXT,
                kinds::ENTITY_REF,
            ]
        );
        assert_eq!(tokens.last().unwrap().0, kinds::ENTITY_REF);
        assert_ne!(tokens.last().unwrap().3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn coverage_is_contiguous() {
        let input = r#"<?xml version="1.0"?><root a='b'>text &amp; <![CDATA[x]]><!--c--></root>"#;
        let tokens = run(input);
        let mut pos = 0u32;
        for (_kind, offset, len, _flags) in &tokens {
            assert_eq!(*offset, pos, "gap or overlap before offset {pos}");
            pos += len;
        }
        assert_eq!(pos as usize, input.len());
    }

    #[test]
    fn error_on_unterminated_comment() {
        let tokens = run("<!-- c");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::COMMENT);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn doctype_internal_subset_allows_gt_inside() {
        let tokens = run(r#"<!DOCTYPE note [<!ENTITY writer "Donald > Duck">]><note/>"#);
        assert_eq!(tokens[0].0, kinds::DOCTYPE);
        assert_eq!(
            tokens.iter().map(|t| t.0).collect::<Vec<_>>()[1..],
            [kinds::LT, kinds::TAG_NAME, kinds::SLASH, kinds::GT,]
        );
    }
}
