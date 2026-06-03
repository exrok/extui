//! JSX lexer, embedded by the [`Tsx`] lexer.
//!
//! Each `Jsx` region lexes exactly one element or fragment. Tag, attribute,
//! entity, comment, and name scanning are shared with [`crate::lex::xml`]
//! rather than duplicated. Two constructs descend into other lexers:
//!
//! - **Child elements** (`<span>…</span>` inside another element's content)
//!   descend into a fresh `Jsx` region. Each region owns one element, so
//!   arbitrary nesting is carried by the driver's embed stack instead of a
//!   depth counter that would not fit the 16-bit [`LexState`].
//! - **`{…}` expression containers** (attribute values and children) descend
//!   into [`Language::Tsx`]. The matching `}` is located with a
//!   brace-balanced scan that skips strings, templates, and comments using the
//!   TypeScript scanners, so the region boundary always lands on a token
//!   boundary the embedded lexer agrees with. The `{` and `}` themselves are
//!   owned by `Jsx`.
//!
//! A region self-terminates with [`LexStep::Eof`] once it has lexed its
//! matching close tag (or a self-closing `/>`), which returns control to the
//! parent lexer. Because `Jsx` is always embedded (`nest >= 1`), an
//! incremental re-lex never resumes from inside it — [`crate::TokenTable`]
//! backs up to the enclosing `nest == 0` chunk and re-enters naturally — so
//! every token may carry `STATE_BREAKPOINT`.
//!
//! # State encoding
//!
//! ```text
//! bits 0-3  mode   one of M_*; all other bits are canonically zero
//! ```
//!
//! [`Tsx`]: crate::lex::tsx::Tsx

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::scan;
use crate::lex::ts;
use crate::lex::xml;
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{Language, LexState, SourceView};

const M_NEED_LT: u16 = 0;
const M_AFTER_LT: u16 = 1;
const M_TAG: u16 = 2;
const M_TAG_SLASH: u16 = 3;
const M_CHILDREN: u16 = 4;
const M_END_SLASH: u16 = 5;
const M_END_NAME: u16 = 6;
const M_END_GT: u16 = 7;
const M_DONE: u16 = 8;
const MODE_MASK: u16 = 0x000F;

#[inline]
const fn st(mode: u16) -> LexState {
    LexState(mode)
}

#[inline]
const fn mode(state: LexState) -> u16 {
    state.bits() & MODE_MASK
}

pub(crate) struct Jsx;

impl Lexer for Jsx {
    fn step_batch(
        view: &mut SourceView<'_>,
        cursor: u32,
        state_in: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState) {
        let mut cursor = cursor;
        let mut state = state_in;
        while !out.is_full() {
            // The element finished on the previous step; hand control back to
            // the parent lexer.
            if mode(state) == M_DONE {
                out.push(LexStep::Eof);
                return (cursor, state);
            }

            let Some(b) = view.byte_at(cursor) else {
                out.push(LexStep::Eof);
                return (cursor, state);
            };

            // `{…}` expression container (attribute value or child). Descend
            // into TSX over the bytes between the braces. Needs room for the
            // `{` token plus the Descend step.
            if (mode(state) == M_TAG || mode(state) == M_CHILDREN) && b == b'{' {
                if out.remaining() < 2 {
                    return (cursor, state);
                }
                let region_end = scan_expr_region(view, cursor);
                out.push(LexStep::Token {
                    len: 1,
                    local_kind: kinds::OPEN_BRACE,
                    state_out: state,
                    flags: flags::STATE_BREAKPOINT,
                });
                let body_start = cursor + 1;
                if region_end > body_start {
                    out.push(LexStep::Descend {
                        language: Language::Tsx,
                        inner_len: region_end - body_start,
                        inner_state_in: LexState::INITIAL,
                        outer_state_after: state,
                    });
                    return (region_end, state);
                }
                // Empty `{}`: the `}` is classified next as CLOSE_BRACE.
                cursor = body_start;
                continue;
            }

            // In content, a `<` opens our own close tag, a comment, or a
            // nested child element.
            if mode(state) == M_CHILDREN && b == b'<' {
                match view.byte_at(cursor + 1) {
                    Some(b'/') => {
                        out.push(LexStep::Token {
                            len: 1,
                            local_kind: kinds::LT,
                            state_out: st(M_END_SLASH),
                            flags: flags::STATE_BREAKPOINT,
                        });
                        cursor += 1;
                        state = st(M_END_SLASH);
                        continue;
                    }
                    Some(b'!') if xml::starts_with(view, cursor, b"<!--") => {
                        let r = xml::scan_comment(view, cursor);
                        let mut f = flags::STATE_BREAKPOINT;
                        if r.is_error {
                            f |= flags::IS_ERROR;
                        }
                        out.push(LexStep::Token {
                            len: r.end - cursor,
                            local_kind: kinds::COMMENT,
                            state_out: st(M_CHILDREN),
                            flags: f,
                        });
                        cursor = r.end;
                        continue;
                    }
                    Some(c) if c == b'>' || xml::is_name_start(view, cursor + 1, c, false) => {
                        if out.remaining() < 1 {
                            return (cursor, state);
                        }
                        out.push(LexStep::Descend {
                            language: Language::InternalSingleJsxElement,
                            inner_len: view.len() - cursor,
                            inner_state_in: LexState::INITIAL,
                            outer_state_after: st(M_CHILDREN),
                        });
                        return (cursor, state);
                    }
                    _ => {
                        out.push(LexStep::Token {
                            len: 1,
                            local_kind: kinds::TEXT,
                            state_out: st(M_CHILDREN),
                            flags: flags::STATE_BREAKPOINT,
                        });
                        cursor += 1;
                        continue;
                    }
                }
            }

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
    state: LexState,
) -> (u16, u32, bool, LexState) {
    match mode(state) {
        M_NEED_LT => classify_need_lt(view, cursor, first),
        M_AFTER_LT => classify_after_lt(view, cursor, first),
        M_TAG => classify_tag(view, cursor, first),
        M_TAG_SLASH => classify_tag_slash(view, cursor, first),
        M_CHILDREN => classify_children(view, cursor, first),
        M_END_SLASH => classify_end_slash(view, cursor, first),
        M_END_NAME => classify_end_name(view, cursor, first),
        M_END_GT => classify_end_gt(view, cursor, first),
        _ => (kinds::ERROR, error_end(view, cursor), true, st(M_DONE)),
    }
}

fn classify_need_lt(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
) -> (u16, u32, bool, LexState) {
    if first == b'<' {
        (kinds::LT, cursor + 1, false, st(M_AFTER_LT))
    } else {
        // Unreachable in practice: a region is always entered at its `<`.
        (kinds::ERROR, error_end(view, cursor), true, st(M_DONE))
    }
}

fn classify_after_lt(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
) -> (u16, u32, bool, LexState) {
    match first {
        b'>' => (kinds::GT, cursor + 1, false, st(M_CHILDREN)),
        b if xml::is_name_start(view, cursor, b, false) => {
            let end = xml::scan_name(view, cursor, false);
            (kinds::TAG_NAME, end, false, st(M_TAG))
        }
        b if byteclass::is_whitespace(b) => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            false,
            st(M_AFTER_LT),
        ),
        _ => (kinds::ERROR, error_end(view, cursor), true, st(M_TAG)),
    }
}

fn classify_tag(view: &mut SourceView<'_>, cursor: u32, first: u8) -> (u16, u32, bool, LexState) {
    match first {
        b if byteclass::is_whitespace(b) => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            false,
            st(M_TAG),
        ),
        b'>' => (kinds::GT, cursor + 1, false, st(M_CHILDREN)),
        b'/' => (kinds::SLASH, cursor + 1, false, st(M_TAG_SLASH)),
        b'=' => (kinds::EQ, cursor + 1, false, st(M_TAG)),
        b'\'' | b'"' => {
            let r = xml::scan_quoted(view, cursor, first);
            (kinds::STRING, r.end, r.is_error, st(M_TAG))
        }
        // Close of a `{…}` attribute value, reached after the embedded TSX
        // region; the `{` and the descent were handled in `step_batch`.
        b'}' => (kinds::CLOSE_BRACE, cursor + 1, false, st(M_TAG)),
        b'&' => {
            let r = xml::scan_entity_ref(view, cursor);
            (kinds::ENTITY_REF, r.end, r.is_error, st(M_TAG))
        }
        b if xml::is_name_start(view, cursor, b, false) => {
            let end = xml::scan_name(view, cursor, false);
            (kinds::ATTR_NAME, end, false, st(M_TAG))
        }
        b':' => (kinds::COLON, cursor + 1, false, st(M_TAG)),
        b'.' => (kinds::DOT, cursor + 1, false, st(M_TAG)),
        b'-' => (kinds::MINUS, cursor + 1, false, st(M_TAG)),
        _ => (kinds::ERROR, error_end(view, cursor), true, st(M_TAG)),
    }
}

fn classify_tag_slash(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
) -> (u16, u32, bool, LexState) {
    if first == b'>' {
        (kinds::GT, cursor + 1, false, st(M_DONE))
    } else {
        // A `/` that was not a self-close; re-handle the byte as tag content.
        classify_tag(view, cursor, first)
    }
}

fn classify_children(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
) -> (u16, u32, bool, LexState) {
    match first {
        b'&' => {
            let r = xml::scan_entity_ref(view, cursor);
            (kinds::ENTITY_REF, r.end, r.is_error, st(M_CHILDREN))
        }
        // Close of a `{…}` child expression, reached after the embedded TSX
        // region; the `{` and the descent were handled in `step_batch`.
        b'}' => (kinds::CLOSE_BRACE, cursor + 1, false, st(M_CHILDREN)),
        _ => (
            kinds::TEXT,
            scan_jsx_text(view, cursor),
            false,
            st(M_CHILDREN),
        ),
    }
}

fn classify_end_slash(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
) -> (u16, u32, bool, LexState) {
    if first == b'/' {
        (kinds::SLASH, cursor + 1, false, st(M_END_NAME))
    } else {
        (kinds::ERROR, error_end(view, cursor), true, st(M_CHILDREN))
    }
}

fn classify_end_name(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
) -> (u16, u32, bool, LexState) {
    match first {
        b'>' => (kinds::GT, cursor + 1, false, st(M_DONE)),
        b if byteclass::is_whitespace(b) => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            false,
            st(M_END_NAME),
        ),
        b if xml::is_name_start(view, cursor, b, false) => {
            let end = xml::scan_name(view, cursor, false);
            (kinds::TAG_NAME, end, false, st(M_END_GT))
        }
        _ => (kinds::ERROR, error_end(view, cursor), true, st(M_END_GT)),
    }
}

fn classify_end_gt(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
) -> (u16, u32, bool, LexState) {
    match first {
        b'>' => (kinds::GT, cursor + 1, false, st(M_DONE)),
        b if byteclass::is_whitespace(b) => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            false,
            st(M_END_GT),
        ),
        _ => (kinds::ERROR, error_end(view, cursor), true, st(M_END_GT)),
    }
}

/// Scans a run of JSX text, stopping before `<`, `{`, `&`, or `}`. The caller
/// guarantees the first byte is none of those, so the run is non-empty.
fn scan_jsx_text(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut end = cursor;
    loop {
        let (base, page) = view.window_at(end);
        if page.is_empty() {
            return end;
        }
        let mut i = (end - base) as usize;
        while i < page.len() {
            let b = page[i];
            if matches!(b, b'<' | b'{' | b'&' | b'}') {
                return base + i as u32;
            }
            i += 1;
        }
        end = base + i as u32;
    }
}

/// Finds the `}` that closes the `{` at `brace_cursor`, balancing nested
/// braces and skipping strings, templates, and comments with the TypeScript
/// scanners. Returns the offset of that `}`, or the visible end if the braces
/// are unbalanced.
fn scan_expr_region(view: &mut SourceView<'_>, brace_cursor: u32) -> u32 {
    let mut p = brace_cursor + 1;
    let mut depth = 1u32;
    loop {
        let Some(b) = view.byte_at(p) else {
            return view.len();
        };
        match b {
            b'{' => {
                depth += 1;
                p += 1;
            }
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    return p;
                }
                p += 1;
            }
            b'\'' | b'"' => p = ts::scan_string(view, p, b).end,
            b'`' => p = ts::scan_template(view, p).end,
            b'/' => match view.byte_at(p + 1) {
                Some(b'/') => p = scan::scan_line_comment(view, p),
                Some(b'*') => p = scan::scan_block_comment(view, p).end,
                _ => p += 1,
            },
            _ => p += 1,
        }
    }
}

fn error_end(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    cursor + scan::decoded_len_or_one(view, cursor)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Source, Span, TokenTable};

    /// Drives a full TSX parse and returns the tokens whose `nest >= 1`
    /// (i.e. the JSX and nested expression tokens) as
    /// `(lang_tag, local_kind, offset, len, nest)`.
    fn jsx_toks(s: &str) -> Vec<(u8, u16, u32, u32, u8)> {
        let src: &dyn Source = &s;
        let table = TokenTable::new(Language::Tsx, src);
        table
            .query(Span::new(0, table.source_len()))
            .map(|t| {
                (
                    t.lang_tag(),
                    t.local_kind(),
                    t.span.offset,
                    t.span.len,
                    t.nest,
                )
            })
            .collect()
    }

    fn kinds_at_nest1(s: &str) -> Vec<u16> {
        jsx_toks(s)
            .into_iter()
            .filter(|t| t.0 == Language::InternalSingleJsxElement.tag() && t.4 == 1)
            .map(|t| t.1)
            .collect()
    }

    #[test]
    fn open_text_close() {
        let prefix = "x = ".len() as u32;
        let kinds = kinds_at_nest1("x = <p>hi</p>");
        assert_eq!(
            kinds,
            vec![
                kinds::LT,
                kinds::TAG_NAME,
                kinds::GT,
                kinds::TEXT,
                kinds::LT,
                kinds::SLASH,
                kinds::TAG_NAME,
                kinds::GT,
            ]
        );
        // The element starts right where TS handed off.
        let first = jsx_toks("x = <p>hi</p>")
            .into_iter()
            .find(|t| t.4 >= 1)
            .unwrap();
        assert_eq!(first.2, prefix);
    }

    #[test]
    fn attributes_string_and_expr() {
        let kinds = kinds_at_nest1("x = <a href=\"u\" on={h}>t</a>");
        assert!(kinds.contains(&kinds::ATTR_NAME));
        assert!(kinds.contains(&kinds::EQ));
        assert!(kinds.contains(&kinds::STRING));
        assert!(kinds.contains(&kinds::OPEN_BRACE));
        assert!(kinds.contains(&kinds::CLOSE_BRACE));
    }

    #[test]
    fn self_closing() {
        let kinds = kinds_at_nest1("x = <br/>");
        assert_eq!(
            kinds,
            vec![kinds::LT, kinds::TAG_NAME, kinds::SLASH, kinds::GT]
        );
    }

    #[test]
    fn fragment() {
        let kinds = kinds_at_nest1("x = <>a</>");
        assert_eq!(
            kinds,
            vec![
                kinds::LT,
                kinds::GT,
                kinds::TEXT,
                kinds::LT,
                kinds::SLASH,
                kinds::GT,
            ]
        );
    }

    #[test]
    fn entity_reference_in_children() {
        let kinds = kinds_at_nest1("x = <p>a &amp; b</p>");
        assert!(kinds.contains(&kinds::ENTITY_REF));
    }

    #[test]
    fn comment_in_children() {
        let kinds = kinds_at_nest1("x = <p><!-- c -->t</p>");
        assert!(kinds.contains(&kinds::COMMENT));
    }

    #[test]
    fn expr_with_braces_inside_string_is_balanced() {
        // The `}` inside the string must not end the `{…}` early; the whole
        // element must lex without leaving stray bytes.
        let s = "x = <p>{f('}')}</p>";
        let mut pos = 0u32;
        for (_l, _k, off, len, _n) in jsx_toks(s) {
            assert_eq!(off, pos);
            pos += len;
        }
        assert_eq!(pos as usize, s.len());
    }

    #[test]
    fn nested_expr_re_enters_tsx() {
        let toks = jsx_toks("x = <p>{<span>y</span>}</p>");
        // The inner `<span>` element is JSX again, two levels under the `<p>`.
        assert!(
            toks.iter()
                .any(|t| t.0 == Language::InternalSingleJsxElement.tag()
                    && t.1 == kinds::TAG_NAME
                    && t.4 == 3)
        );
    }
}
