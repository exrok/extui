//! Markdown lexer, with embed dispatch for fenced code blocks.
//!
//! Recognized constructs are single-line. Cross-line constructs such as
//! setext headings, indented code blocks, and reference-style links are
//! not modelled.
//!
//! - ATX headings (`#`..`######`).
//! - `*` / `_` runs as [`EMPHASIS`]. Pairing into em and strong is the
//!   renderer's job.
//! - Same-line inline code spans `` `…` ``.
//! - Same-line links and images, `[text](url)` and `![alt](url)`.
//! - Blockquote line prefix `>` at column 0.
//! - List markers (`-` / `*` / `+` / digits-then-`.`/`)`) at column 0.
//! - Fenced code blocks with backtick or tilde fences of length at least
//!   three. Info strings `rust`/`rs`, `c`, `csv`, `json`, `xml`, `css`,
//!   `ts`/`typescript`, `tsx`/`jsx`, `html`/`htm`, and `python`/`py`
//!   (case-insensitive ASCII) dispatch to the corresponding language. An
//!   embedded `html` block in turn
//!   embeds CSS and TypeScript for its `<style>` and `<script>` content; a
//!   `tsx` block embeds JSX for elements in expression position. Other info
//!   strings and missing closers emit the body as an opaque [`CODE_BLOCK`]
//!   token.
//!
//! [`EMPHASIS`]: kinds::EMPHASIS
//! [`CODE_BLOCK`]: kinds::CODE_BLOCK

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{Language, LexState, SourceView};

mod state {
    pub const CLOSER_PENDING: u16 = 1 << 0;
    pub const MID_LINE: u16 = 1 << 1;
}

#[inline]
fn ends_at_newline(view: &mut SourceView<'_>, end: u32) -> bool {
    end > 0 && view.byte_at(end - 1) == Some(b'\n')
}

#[inline]
fn state_after(view: &mut SourceView<'_>, end: u32) -> LexState {
    if ends_at_newline(view, end) {
        LexState::INITIAL
    } else {
        LexState(state::MID_LINE)
    }
}

pub(crate) struct Markdown;

impl Lexer for Markdown {
    fn step_batch(
        view: &mut SourceView<'_>,
        cursor: u32,
        state: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState) {
        let mut cursor = cursor;
        let mut state = state;
        loop {
            if out.is_full() {
                return (cursor, state);
            }
            if (state.bits() & state::CLOSER_PENDING) != 0 {
                if view.byte_at(cursor).is_none() {
                    out.push(LexStep::Eof);
                    return (cursor, state);
                }
                let end = scan_to_eol_inclusive(view, cursor);
                let state_out = state_after(view, end);
                out.push(LexStep::Token {
                    len: end - cursor,
                    local_kind: kinds::CODE_FENCE,
                    state_out,
                    flags: flags::STATE_BREAKPOINT | flags::EMBED_CLOSE,
                });
                cursor = end;
                state = state_out;
                continue;
            }

            let Some(b) = view.byte_at(cursor) else {
                out.push(LexStep::Eof);
                return (cursor, state);
            };

            let bol = (state.bits() & state::MID_LINE) == 0;

            if bol {
                if b == b'`' || b == b'~' {
                    if out.remaining() < 3 {
                        return (cursor, state);
                    }
                    if let Some(advance) = try_fenced_code(view, cursor, b, out) {
                        cursor += advance.consumed;
                        state = advance.state;
                        if advance.descended {
                            return (cursor, state);
                        }
                        continue;
                    }
                }
                if b == b'#' {
                    if let Some(consumed) = try_atx_heading(view, cursor, out) {
                        cursor += consumed;
                        state = state_after(view, cursor);
                        continue;
                    }
                }
                if b == b'>' {
                    let end = scan_blockquote_prefix(view, cursor);
                    let state_out = state_after(view, end);
                    out.push(LexStep::Token {
                        len: end - cursor,
                        local_kind: kinds::BLOCKQUOTE,
                        state_out,
                        flags: flags::STATE_BREAKPOINT,
                    });
                    cursor = end;
                    state = state_out;
                    continue;
                }
                if let Some(end) = try_list_marker(view, cursor, b) {
                    let state_out = state_after(view, end);
                    out.push(LexStep::Token {
                        len: end - cursor,
                        local_kind: kinds::LIST_MARKER,
                        state_out,
                        flags: flags::STATE_BREAKPOINT,
                    });
                    cursor = end;
                    state = state_out;
                    continue;
                }
            }

            if b == b'[' || b == b'!' {
                if out.remaining() < 2 {
                    return (cursor, state);
                }
                if let Some(consumed) = try_link(view, cursor, b, out) {
                    cursor += consumed;
                    state = state_after(view, cursor);
                    continue;
                }
            }

            let (kind, end, is_error) = inline_classify(view, cursor, b);
            let mut bit_flags = flags::STATE_BREAKPOINT;
            if is_error {
                bit_flags |= flags::IS_ERROR;
            }
            let state_out = state_after(view, end);
            out.push(LexStep::Token {
                len: end - cursor,
                local_kind: kind,
                state_out,
                flags: bit_flags,
            });
            cursor = end;
            state = state_out;
        }
    }

    fn is_safe_state(_state: LexState) -> bool {
        true
    }
}

fn scan_to_eol_inclusive(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut p = cursor;
    loop {
        match view.byte_at(p) {
            None => return p,
            Some(b'\n') => return p + 1,
            Some(_) => p += 1,
        }
    }
}

fn scan_md_whitespace(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut p = cursor;
    loop {
        match view.byte_at(p) {
            Some(b' ') | Some(b'\t') | Some(b'\r') => p += 1,
            Some(b'\n') => return p + 1,
            _ => return p,
        }
    }
}

fn scan_blockquote_prefix(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut p = cursor + 1;
    if view.byte_at(p) == Some(b' ') {
        p += 1;
    }
    p
}

fn try_atx_heading(view: &mut SourceView<'_>, cursor: u32, out: &mut StepBuf) -> Option<u32> {
    let mut p = cursor;
    let mut count = 0u32;
    while count < 6 && view.byte_at(p) == Some(b'#') {
        count += 1;
        p += 1;
    }
    if count == 0 {
        return None;
    }
    match view.byte_at(p) {
        None | Some(b'\n') | Some(b' ') | Some(b'\t') => {}
        _ => return None,
    }
    if out.remaining() < 3 {
        return None;
    }

    let marker_end = p;
    out.push(LexStep::Token {
        len: marker_end - cursor,
        local_kind: kinds::HEADING_MARKER,
        state_out: LexState(state::MID_LINE),
        flags: 0,
    });

    let mut text_end = marker_end;
    while let Some(b) = view.byte_at(text_end) {
        if b == b'\n' {
            break;
        }
        text_end += 1;
    }
    if text_end > marker_end {
        out.push(LexStep::Token {
            len: text_end - marker_end,
            local_kind: kinds::HEADING_TEXT,
            state_out: LexState(state::MID_LINE),
            flags: 0,
        });
    }
    let mut consumed = text_end - cursor;
    if view.byte_at(text_end) == Some(b'\n') {
        out.push(LexStep::Token {
            len: 1,
            local_kind: kinds::WHITESPACE,
            state_out: LexState::INITIAL,
            flags: flags::STATE_BREAKPOINT,
        });
        consumed += 1;
    }
    Some(consumed)
}

fn try_list_marker(view: &mut SourceView<'_>, cursor: u32, first: u8) -> Option<u32> {
    match first {
        b'-' | b'+' | b'*' => {
            if view.byte_at(cursor + 1) == Some(b' ') {
                Some(cursor + 2)
            } else {
                None
            }
        }
        b'0'..=b'9' => {
            let mut p = cursor;
            let mut digits = 0u32;
            while digits < 9 {
                match view.byte_at(p) {
                    Some(b) if byteclass::is_digit(b) => {
                        digits += 1;
                        p += 1;
                    }
                    _ => break,
                }
            }
            if digits == 0 {
                return None;
            }
            match (view.byte_at(p), view.byte_at(p + 1)) {
                (Some(b'.'), Some(b' ')) | (Some(b')'), Some(b' ')) => Some(p + 2),
                _ => None,
            }
        }
        _ => None,
    }
}

fn try_link(view: &mut SourceView<'_>, cursor: u32, first: u8, out: &mut StepBuf) -> Option<u32> {
    let lbracket = if first == b'!' { cursor + 1 } else { cursor };
    if view.byte_at(lbracket) != Some(b'[') {
        return None;
    }
    let (text_end, url_end) = scan_link_brackets(view, lbracket)?;
    let text_len = text_end - cursor;
    let url_len = url_end - text_end;
    out.push(LexStep::Token {
        len: text_len,
        local_kind: kinds::LINK_TEXT,
        state_out: LexState(state::MID_LINE),
        flags: 0,
    });
    let url_state_out = state_after(view, url_end);
    out.push(LexStep::Token {
        len: url_len,
        local_kind: kinds::LINK_URL,
        state_out: url_state_out,
        flags: flags::STATE_BREAKPOINT,
    });
    Some(text_len + url_len)
}

fn scan_link_brackets(view: &mut SourceView<'_>, lbracket: u32) -> Option<(u32, u32)> {
    let mut p = lbracket + 1;
    loop {
        match view.byte_at(p) {
            None | Some(b'\n') => return None,
            Some(b']') => {
                p += 1;
                break;
            }
            Some(b'[') => return None,
            Some(_) => p += 1,
        }
    }
    let text_end = p;
    if view.byte_at(p) != Some(b'(') {
        return None;
    }
    p += 1;
    loop {
        match view.byte_at(p) {
            None | Some(b'\n') => return None,
            Some(b')') => {
                p += 1;
                return Some((text_end, p));
            }
            Some(b'(') => return None,
            Some(_) => p += 1,
        }
    }
}

struct FenceAdvance {
    consumed: u32,
    state: LexState,
    descended: bool,
}

fn try_fenced_code(
    view: &mut SourceView<'_>,
    cursor: u32,
    fc: u8,
    out: &mut StepBuf,
) -> Option<FenceAdvance> {
    let mut fence_end = cursor;
    let mut fence_count = 0u32;
    while view.byte_at(fence_end) == Some(fc) {
        fence_count += 1;
        fence_end += 1;
    }
    if fence_count < 3 {
        return None;
    }

    let line_end = scan_to_eol_inclusive(view, fence_end);
    let info_end = if line_end > fence_end && view.byte_at(line_end - 1) == Some(b'\n') {
        line_end - 1
    } else {
        line_end
    };
    let lang_opt = info_to_language(view, fence_end, info_end, fc);

    let body_start = line_end;
    let close_opt = scan_for_closer(view, body_start, fc, fence_count);

    // Within a fenced block we deliberately do NOT mark the opener or body
    // tokens as STATE_BREAKPOINT: a chunk split between opener and body, or
    // between body and closer, would put a token at a position where the
    // resumed lexer (driven only by state) would interpret the bytes as a
    // fresh opener instead of a closer / opaque body. Breakpoints on inner
    // (descended) tokens are fine — they belong to a different language.
    match close_opt {
        Some((close_start, closer_end)) => {
            let body_len = close_start - body_start;
            let opener_len = body_start - cursor;
            if let Some(lang) = lang_opt {
                if body_len > 0 {
                    out.push(LexStep::Token {
                        len: opener_len,
                        local_kind: kinds::CODE_FENCE,
                        state_out: state_after(view, body_start),
                        flags: flags::EMBED_OPEN,
                    });
                    out.push(LexStep::Descend {
                        language: lang,
                        inner_len: body_len,
                        inner_state_in: LexState::INITIAL,
                        outer_state_after: LexState(state::CLOSER_PENDING),
                    });
                    return Some(FenceAdvance {
                        consumed: opener_len + body_len,
                        state: LexState(state::CLOSER_PENDING),
                        descended: true,
                    });
                }
            }
            out.push(LexStep::Token {
                len: opener_len,
                local_kind: kinds::CODE_FENCE,
                state_out: state_after(view, body_start),
                flags: flags::EMBED_OPEN,
            });
            if body_len > 0 {
                out.push(LexStep::Token {
                    len: body_len,
                    local_kind: kinds::CODE_BLOCK,
                    state_out: state_after(view, close_start),
                    flags: 0,
                });
            }
            let closer_state_out = state_after(view, closer_end);
            out.push(LexStep::Token {
                len: closer_end - close_start,
                local_kind: kinds::CODE_FENCE,
                state_out: closer_state_out,
                flags: flags::STATE_BREAKPOINT | flags::EMBED_CLOSE,
            });
            Some(FenceAdvance {
                consumed: closer_end - cursor,
                state: closer_state_out,
                descended: false,
            })
        }
        None => {
            let body_len = view.len() - body_start;
            let opener_len = body_start - cursor;
            let opener_state_out = state_after(view, body_start);
            out.push(LexStep::Token {
                len: opener_len,
                local_kind: kinds::CODE_FENCE,
                state_out: opener_state_out,
                flags: flags::EMBED_OPEN,
            });
            if body_len == 0 {
                return Some(FenceAdvance {
                    consumed: opener_len,
                    state: opener_state_out,
                    descended: false,
                });
            }
            if let Some(lang) = lang_opt {
                out.push(LexStep::Descend {
                    language: lang,
                    inner_len: body_len,
                    inner_state_in: LexState::INITIAL,
                    outer_state_after: LexState::INITIAL,
                });
                return Some(FenceAdvance {
                    consumed: opener_len + body_len,
                    state: LexState::INITIAL,
                    descended: true,
                });
            }
            let body_end = body_start + body_len;
            let body_state_out = state_after(view, body_end);
            out.push(LexStep::Token {
                len: body_len,
                local_kind: kinds::CODE_BLOCK,
                state_out: body_state_out,
                flags: 0,
            });
            Some(FenceAdvance {
                consumed: opener_len + body_len,
                state: body_state_out,
                descended: false,
            })
        }
    }
}

fn scan_for_closer(
    view: &mut SourceView<'_>,
    body_start: u32,
    fc: u8,
    n: u32,
) -> Option<(u32, u32)> {
    let mut line_start = body_start;
    while view.byte_at(line_start).is_some() {
        let line_end = scan_to_eol_inclusive(view, line_start);
        if is_closer_line(view, line_start, line_end, fc, n) {
            return Some((line_start, line_end));
        }
        if line_end == line_start {
            return None;
        }
        line_start = line_end;
    }
    None
}

fn is_closer_line(
    view: &mut SourceView<'_>,
    line_start: u32,
    line_end: u32,
    fc: u8,
    n: u32,
) -> bool {
    let mut p = line_start;
    let mut spaces = 0u32;
    while spaces < 3 && view.byte_at(p) == Some(b' ') {
        spaces += 1;
        p += 1;
    }
    let mut count = 0u32;
    while view.byte_at(p) == Some(fc) {
        count += 1;
        p += 1;
    }
    if count < n {
        return false;
    }
    while p < line_end {
        match view.byte_at(p) {
            Some(b'\n') => return true,
            Some(b' ') | Some(b'\t') | Some(b'\r') => p += 1,
            _ => return false,
        }
    }
    true
}

fn info_to_language(
    view: &mut SourceView<'_>,
    info_start: u32,
    info_end: u32,
    fc: u8,
) -> Option<Language> {
    let mut start = info_start;
    while start < info_end {
        match view.byte_at(start) {
            Some(b' ') | Some(b'\t') | Some(b'\r') => start += 1,
            _ => break,
        }
    }
    let mut end = info_end;
    while end > start {
        match view.byte_at(end - 1) {
            Some(b' ') | Some(b'\t') | Some(b'\r') => end -= 1,
            _ => break,
        }
    }
    let mut tag_end = start;
    while tag_end < end {
        match view.byte_at(tag_end) {
            Some(b' ') | Some(b'\t') => break,
            _ => tag_end += 1,
        }
    }
    if tag_end == start {
        return None;
    }
    let len = (tag_end - start) as usize;
    if len > 16 {
        return None;
    }
    let mut buf = [0u8; 16];
    for i in 0..len {
        let b = view.byte_at(start + i as u32)?;
        if fc == b'`' && b == b'`' {
            return None;
        }
        buf[i] = b.to_ascii_lowercase();
    }
    match &buf[..len] {
        b"rust" | b"rs" => Some(Language::Rust),
        b"c" => Some(Language::C),
        b"csv" => Some(Language::Csv),
        b"json" => Some(Language::Json),
        b"xml" => Some(Language::Xml),
        b"css" => Some(Language::Css),
        b"ts" | b"typescript" => Some(Language::Ts),
        b"tsx" | b"jsx" => Some(Language::Tsx),
        b"html" | b"htm" => Some(Language::Html),
        b"python" | b"py" => Some(Language::Python),
        b"sql" => Some(Language::Sql),
        _ => None,
    }
}

fn inline_classify(view: &mut SourceView<'_>, cursor: u32, first: u8) -> (u16, u32, bool) {
    match first {
        b' ' | b'\t' | b'\r' | b'\n' => {
            let end = scan_md_whitespace(view, cursor);
            (kinds::WHITESPACE, end, false)
        }
        b'`' => {
            let mut count = 0u32;
            let mut p = cursor;
            while view.byte_at(p) == Some(b'`') {
                count += 1;
                p += 1;
            }
            if let Some(end) = scan_inline_code(view, p, count) {
                (kinds::CODE_INLINE, end, false)
            } else {
                (kinds::BACKTICK, p, false)
            }
        }
        b'*' | b'_' => {
            let mut p = cursor;
            while view.byte_at(p) == Some(first) {
                p += 1;
            }
            (kinds::EMPHASIS, p, false)
        }
        b'[' => (kinds::OPEN_BRACKET, cursor + 1, false),
        b']' => (kinds::CLOSE_BRACKET, cursor + 1, false),
        b'(' => (kinds::OPEN_PAREN, cursor + 1, false),
        b')' => (kinds::CLOSE_PAREN, cursor + 1, false),
        b'!' => (kinds::BANG, cursor + 1, false),
        b'#' => (kinds::HASH, cursor + 1, false),
        b'>' => (kinds::GT, cursor + 1, false),
        _ => {
            let end = scan_text_run(view, cursor);
            if end == cursor {
                (kinds::ERROR, cursor + 1, true)
            } else {
                (kinds::TEXT, end, false)
            }
        }
    }
}

fn scan_inline_code(view: &mut SourceView<'_>, after_open: u32, n: u32) -> Option<u32> {
    let mut p = after_open;
    loop {
        match view.byte_at(p) {
            None | Some(b'\n') => return None,
            Some(b'`') => {
                let mut close_count = 0u32;
                while view.byte_at(p) == Some(b'`') {
                    close_count += 1;
                    p += 1;
                }
                if close_count == n {
                    return Some(p);
                }
                if close_count > n {
                    return None;
                }
            }
            Some(_) => p += 1,
        }
    }
}

fn scan_text_run(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut p = cursor;
    loop {
        let Some(b) = view.byte_at(p) else { return p };
        match b {
            b' ' | b'\t' | b'\r' | b'\n' | b'`' | b'*' | b'_' | b'[' | b']' | b'(' | b')'
            | b'!' | b'#' | b'>' => return p,
            _ => p += 1,
        }
    }
}
