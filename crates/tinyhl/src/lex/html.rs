//! HTML lexer, with embed dispatch for `<style>` and `<script>` content.
//!
//! HTML is markup like XML, so the tag/attribute/comment/entity scanning is
//! shared with [`crate::lex::xml`] rather than duplicated. The differences
//! that matter here are:
//!
//! - **Tag names are matched case-insensitively** when deciding whether a
//!   start tag opens a raw-text element.
//! - **`<style>` and `<script>` are raw-text elements.** Their content is not
//!   markup; it runs verbatim until the matching `</style>` / `</script>` end
//!   tag (HTML's rawtext rule — the first such end tag wins, even inside a
//!   string or comment). The body is dispatched to the [`Css`] and [`Ts`]
//!   lexers respectively via [`LexStep::Descend`], exactly like Markdown's
//!   fenced code blocks.
//! - `<!… >` (other than comments, CDATA, and `DOCTYPE`) and `<?…>` are
//!   consumed as bogus comments, and a `<` that does not begin a tag is data.
//!
//! [`Css`]: crate::lex::css::Css
//! [`Ts`]: crate::lex::ts::Ts
//!
//! # State encoding
//!
//! ```text
//! bits 0-3  mode      one of MODE_*
//! bits 4-5  raw_kind  RAW_NONE / RAW_SCRIPT / RAW_STYLE, set only inside a
//!                     start tag (MODE_TAG) whose name selects a raw-text
//!                     element; zero everywhere else
//! ```
//!
//! All unused bits are canonically zero so that chunk-boundary states compare
//! bitwise-equal across edits.

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::scan;
use crate::lex::xml;
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{Language, LexState, SourceView};

const MODE_TEXT: u16 = 0;
const MODE_AFTER_LT: u16 = 1;
const MODE_CLOSE_NAME: u16 = 2;
const MODE_TAG: u16 = 3;
const MODE_RAW_CLOSE: u16 = 4;
const MODE_MASK: u16 = 0x000F;

const RAW_NONE: u16 = 0;
const RAW_SCRIPT: u16 = 1;
const RAW_STYLE: u16 = 2;
const RAW_SHIFT: u16 = 4;
const RAW_MASK: u16 = 0x0030;

#[inline]
const fn st(mode: u16, raw: u16) -> LexState {
    LexState(mode | (raw << RAW_SHIFT))
}

#[inline]
const fn mode(state: LexState) -> u16 {
    state.bits() & MODE_MASK
}

#[inline]
const fn raw_kind(state: LexState) -> u16 {
    (state.bits() & RAW_MASK) >> RAW_SHIFT
}

pub(crate) struct Html;

impl Lexer for Html {
    fn step_batch(
        view: &mut SourceView<'_>,
        cursor: u32,
        state_in: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState) {
        let mut cursor = cursor;
        let mut state = state_in;
        while !out.is_full() {
            // Returning from a raw-text embed: emit the closing tag's `<` with
            // EMBED_CLOSE, then fall back into ordinary tag lexing so the
            // `</style>` / `</script>` end tag is tokenized normally.
            if mode(state) == MODE_RAW_CLOSE {
                if view.byte_at(cursor).is_none() {
                    out.push(LexStep::Eof);
                    return (cursor, state);
                }
                out.push(LexStep::Token {
                    len: 1,
                    local_kind: kinds::LT,
                    state_out: st(MODE_AFTER_LT, RAW_NONE),
                    flags: flags::STATE_BREAKPOINT | flags::EMBED_CLOSE,
                });
                cursor += 1;
                state = st(MODE_AFTER_LT, RAW_NONE);
                continue;
            }

            let Some(b) = view.byte_at(cursor) else {
                out.push(LexStep::Eof);
                return (cursor, state);
            };

            // The `>` that closes a `<style>` / `<script>` start tag opens an
            // embedded region. We need room for the opener token plus the
            // Descend step; if the batch is nearly full, hand control back so
            // the driver re-invokes us at this position with a fresh buffer.
            if mode(state) == MODE_TAG && raw_kind(state) != RAW_NONE && b == b'>' {
                if out.remaining() < 2 {
                    return (cursor, state);
                }
                let (lang, name): (Language, &[u8]) = if raw_kind(state) == RAW_SCRIPT {
                    (Language::Ts, b"script")
                } else {
                    (Language::Css, b"style")
                };
                let body_start = cursor + 1;
                let closer_start = find_rawtext_end(view, body_start, name);
                let inner_len = closer_start - body_start;

                if inner_len == 0 {
                    // Empty element body: nothing to embed. Emit the `>` as an
                    // ordinary tag close and let the end tag lex as markup.
                    out.push(LexStep::Token {
                        len: 1,
                        local_kind: kinds::GT,
                        state_out: st(MODE_TEXT, RAW_NONE),
                        flags: flags::STATE_BREAKPOINT,
                    });
                    cursor = body_start;
                    state = st(MODE_TEXT, RAW_NONE);
                    continue;
                }

                // The opener must NOT be a STATE_BREAKPOINT: a chunk split
                // between it and the embedded body would record an HTML
                // resume point at `body_start`, and the resumed lexer would
                // then read the body as markup instead of the inner language.
                out.push(LexStep::Token {
                    len: 1,
                    local_kind: kinds::GT,
                    state_out: st(MODE_RAW_CLOSE, RAW_NONE),
                    flags: flags::EMBED_OPEN,
                });
                out.push(LexStep::Descend {
                    language: lang,
                    inner_len,
                    inner_state_in: LexState::INITIAL,
                    outer_state_after: st(MODE_RAW_CLOSE, RAW_NONE),
                });
                return (closer_start, st(MODE_RAW_CLOSE, RAW_NONE));
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
    state_in: LexState,
) -> (u16, u32, bool, LexState) {
    match mode(state_in) {
        MODE_TEXT => classify_text(view, cursor, first),
        MODE_AFTER_LT => classify_after_lt(view, cursor, first),
        MODE_CLOSE_NAME => classify_close_name(view, cursor, first),
        MODE_TAG => classify_tag(view, cursor, first, raw_kind(state_in)),
        _ => (
            kinds::ERROR,
            error_end(view, cursor),
            true,
            st(MODE_TEXT, 0),
        ),
    }
}

fn classify_text(view: &mut SourceView<'_>, cursor: u32, first: u8) -> (u16, u32, bool, LexState) {
    if first == b'<' {
        if xml::starts_with(view, cursor, b"<!--") {
            let r = xml::scan_comment(view, cursor);
            return (kinds::COMMENT, r.end, r.is_error, st(MODE_TEXT, 0));
        }
        if xml::starts_with(view, cursor, b"<![CDATA[") {
            let r = xml::scan_cdata(view, cursor);
            return (kinds::CDATA, r.end, r.is_error, st(MODE_TEXT, 0));
        }
        if xml::starts_doctype(view, cursor) {
            let r = xml::scan_doctype(view, cursor);
            return (kinds::DOCTYPE, r.end, r.is_error, st(MODE_TEXT, 0));
        }
        match view.byte_at(cursor + 1) {
            // `<!…>` (not a comment / CDATA / DOCTYPE) and `<?…>` are bogus
            // comments in HTML: consumed verbatim up to the next `>`.
            Some(b'!') | Some(b'?') => {
                let end = scan_bogus_comment(view, cursor);
                (kinds::COMMENT, end, false, st(MODE_TEXT, 0))
            }
            Some(b'/') => (kinds::LT, cursor + 1, false, st(MODE_AFTER_LT, 0)),
            Some(b) if b.is_ascii_alphabetic() => {
                (kinds::LT, cursor + 1, false, st(MODE_AFTER_LT, 0))
            }
            // A `<` that begins neither a tag nor a declaration is data.
            _ => (kinds::TEXT, cursor + 1, false, st(MODE_TEXT, 0)),
        }
    } else if first == b'&' {
        let r = xml::scan_entity_ref(view, cursor);
        (kinds::ENTITY_REF, r.end, r.is_error, st(MODE_TEXT, 0))
    } else {
        let end = xml::scan_text_run(view, cursor);
        (kinds::TEXT, end, false, st(MODE_TEXT, 0))
    }
}

fn classify_after_lt(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
) -> (u16, u32, bool, LexState) {
    match first {
        b'/' => (kinds::SLASH, cursor + 1, false, st(MODE_CLOSE_NAME, 0)),
        b if xml::is_name_start(view, cursor, b, false) => {
            let end = xml::scan_name(view, cursor, false);
            let raw = rawtext_kind_of(view, cursor, end);
            (kinds::TAG_NAME, end, false, st(MODE_TAG, raw))
        }
        _ => (
            kinds::ERROR,
            error_end(view, cursor),
            true,
            st(MODE_TEXT, 0),
        ),
    }
}

fn classify_close_name(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
) -> (u16, u32, bool, LexState) {
    match first {
        b if byteclass::is_whitespace(b) => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            false,
            st(MODE_CLOSE_NAME, 0),
        ),
        b if xml::is_name_start(view, cursor, b, false) => {
            let end = xml::scan_name(view, cursor, false);
            (kinds::TAG_NAME, end, false, st(MODE_TAG, 0))
        }
        b'>' => (kinds::GT, cursor + 1, false, st(MODE_TEXT, 0)),
        _ => (
            kinds::ERROR,
            error_end(view, cursor),
            true,
            st(MODE_CLOSE_NAME, 0),
        ),
    }
}

fn classify_tag(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
    raw: u16,
) -> (u16, u32, bool, LexState) {
    match first {
        b if byteclass::is_whitespace(b) => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            false,
            st(MODE_TAG, raw),
        ),
        // Reached only when `raw == RAW_NONE`; a raw-text `>` is handled by
        // `step_batch` before `classify` runs.
        b'>' => (kinds::GT, cursor + 1, false, st(MODE_TEXT, 0)),
        // A `/` does not clear `raw`: `<script/>` is not self-closing in HTML
        // (script is not a void element), so its body still embeds.
        b'/' => (kinds::SLASH, cursor + 1, false, st(MODE_TAG, raw)),
        b'=' => (kinds::EQ, cursor + 1, false, st(MODE_TAG, raw)),
        b'\'' | b'"' => {
            let r = xml::scan_quoted(view, cursor, first);
            (kinds::STRING, r.end, r.is_error, st(MODE_TAG, raw))
        }
        b'&' => {
            let r = xml::scan_entity_ref(view, cursor);
            (kinds::ENTITY_REF, r.end, r.is_error, st(MODE_TAG, raw))
        }
        b if xml::is_name_start(view, cursor, b, false) => {
            let end = xml::scan_name(view, cursor, false);
            (kinds::ATTR_NAME, end, false, st(MODE_TAG, raw))
        }
        b':' => (kinds::COLON, cursor + 1, false, st(MODE_TAG, raw)),
        b'.' => (kinds::DOT, cursor + 1, false, st(MODE_TAG, raw)),
        b'-' => (kinds::MINUS, cursor + 1, false, st(MODE_TAG, raw)),
        _ => (
            kinds::ERROR,
            error_end(view, cursor),
            true,
            st(MODE_TAG, raw),
        ),
    }
}

/// Returns the raw-text element kind for the tag name in `[start, end)`,
/// matching `script` and `style` case-insensitively.
fn rawtext_kind_of(view: &mut SourceView<'_>, start: u32, end: u32) -> u16 {
    match end - start {
        6 if name_eq_ignore_case(view, start, b"script") => RAW_SCRIPT,
        5 if name_eq_ignore_case(view, start, b"style") => RAW_STYLE,
        _ => RAW_NONE,
    }
}

/// Returns `true` if the bytes starting at `start` equal `expected` (which
/// must be lowercase), comparing ASCII letters case-insensitively.
fn name_eq_ignore_case(view: &mut SourceView<'_>, start: u32, expected: &[u8]) -> bool {
    for (i, &want) in expected.iter().enumerate() {
        match view.byte_at(start + i as u32) {
            Some(got) if got.to_ascii_lowercase() == want => {}
            _ => return false,
        }
    }
    true
}

/// Finds the start of the raw-text end tag (`</name`) for a `<style>` /
/// `<script>` body beginning at `start`. Returns the offset of the `<`, or the
/// end of the visible source if no matching end tag is present.
fn find_rawtext_end(view: &mut SourceView<'_>, start: u32, name: &[u8]) -> u32 {
    let mut p = start;
    loop {
        let Some(lt) = scan_to_lt(view, p) else {
            return view.len();
        };
        if is_rawtext_close(view, lt, name) {
            return lt;
        }
        p = lt + 1;
    }
}

/// Returns the offset of the next `<` at or after `cursor`, or `None` at
/// end-of-source.
fn scan_to_lt(view: &mut SourceView<'_>, cursor: u32) -> Option<u32> {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return None;
        }
        let mut i = (cursor - base) as usize;
        while i < page.len() {
            if page[i] == b'<' {
                return Some(base + i as u32);
            }
            i += 1;
        }
        cursor = base + i as u32;
    }
}

/// Returns `true` if `at` (which points at a `<`) begins a `</name…>` end tag,
/// where `name` (lowercase) is followed by whitespace, `/`, `>`, or EOF.
fn is_rawtext_close(view: &mut SourceView<'_>, at: u32, name: &[u8]) -> bool {
    if view.byte_at(at + 1) != Some(b'/') {
        return false;
    }
    let mut p = at + 2;
    for &want in name {
        match view.byte_at(p) {
            Some(b) if b.to_ascii_lowercase() == want => p += 1,
            _ => return false,
        }
    }
    match view.byte_at(p) {
        None => true,
        Some(b) => byteclass::is_whitespace(b) || b == b'>' || b == b'/',
    }
}

/// Consumes a bogus comment starting at the `<`, up to and including the next
/// `>` (or end-of-source).
fn scan_bogus_comment(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut p = cursor + 1;
    loop {
        match view.byte_at(p) {
            None => return p,
            Some(b'>') => return p + 1,
            Some(_) => p += 1,
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

    /// Drives the HTML lexer directly, recording HTML-level tokens. Embedded
    /// regions are skipped (their bytes are accounted for but not lexed),
    /// which mirrors the real driver closely enough to test tag handling and
    /// embed framing.
    fn run(s: &str) -> Vec<(u16, u32, u32, u8)> {
        let src: &dyn Source = &s;
        let mut view = SourceView::new(src, 0);
        let mut out = StepBuf::new();
        let mut cursor = 0u32;
        let mut state = LexState::INITIAL;
        let mut tokens = Vec::new();
        loop {
            out.clear();
            Html::step_batch(&mut view, cursor, state, &mut out);
            let mut ended = false;
            for step in out.as_slice() {
                match *step {
                    LexStep::Token {
                        len,
                        local_kind,
                        flags,
                        state_out,
                    } => {
                        tokens.push((local_kind, cursor, len, flags));
                        cursor += len;
                        state = state_out;
                    }
                    LexStep::Descend {
                        inner_len,
                        outer_state_after,
                        ..
                    } => {
                        cursor += inner_len;
                        state = outer_state_after;
                        break;
                    }
                    LexStep::Eof => {
                        ended = true;
                        break;
                    }
                }
            }
            if ended {
                return tokens;
            }
        }
    }

    fn kinds_of(s: &str) -> Vec<u16> {
        run(s).iter().map(|t| t.0).collect()
    }

    /// Asserts that the full token stream (including embedded-language
    /// tokens) covers `s` with no gaps or overlaps. Driven through
    /// [`TokenTable`] so embed regions are lexed rather than skipped.
    fn coverage_is_contiguous(s: &str) {
        let src: &dyn Source = &s;
        let table = TokenTable::new(Language::Html, src);
        let mut pos = 0u32;
        for t in table.query(Span::new(0, table.source_len())) {
            assert_eq!(t.span.offset, pos, "gap or overlap before offset {pos}");
            pos += t.span.len;
        }
        assert_eq!(pos as usize, s.len(), "did not cover {s:?}");
    }

    #[test]
    fn simple_element_with_attribute() {
        assert_eq!(
            kinds_of(r#"<p id="x">hi</p>"#),
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
    fn comment_doctype_and_entities() {
        let tokens = run("<!DOCTYPE html><!--c-->a &amp; &#169;");
        let k: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert_eq!(k[0], kinds::DOCTYPE);
        assert_eq!(k[1], kinds::COMMENT);
        assert!(k.contains(&kinds::ENTITY_REF));
        coverage_is_contiguous("<!DOCTYPE html><!--c-->a &amp; &#169;");
    }

    #[test]
    fn bogus_comment_for_bang_and_question() {
        assert_eq!(kinds_of("<!bogus>"), vec![kinds::COMMENT]);
        assert_eq!(kinds_of("<?php echo 1; ?>"), vec![kinds::COMMENT]);
    }

    #[test]
    fn lt_not_starting_a_tag_is_data() {
        // `<` followed by a non-letter is plain text data.
        assert_eq!(
            kinds_of("a < b"),
            vec![kinds::TEXT, kinds::TEXT, kinds::TEXT]
        );
        coverage_is_contiguous("3 < 4 && 5 > 1");
    }

    #[test]
    fn tag_names_are_case_insensitive_for_rawtext() {
        // The opener `>` carries EMBED_OPEN, and the closer `<` carries
        // EMBED_CLOSE, regardless of tag-name case.
        for src in ["<STYLE>a{}</STYLE>", "<Script>x;</Script>"] {
            let tokens = run(src);
            assert!(
                tokens.iter().any(|t| t.3 & flags::EMBED_OPEN != 0),
                "missing EMBED_OPEN in {src:?}"
            );
            assert!(
                tokens.iter().any(|t| t.3 & flags::EMBED_CLOSE != 0),
                "missing EMBED_CLOSE in {src:?}"
            );
            coverage_is_contiguous(src);
        }
    }

    #[test]
    fn empty_rawtext_body_does_not_embed() {
        // `<style></style>` has no body: the opener `>` is an ordinary close,
        // and the end tag lexes as markup.
        let tokens = run("<style></style>");
        assert!(tokens.iter().all(|t| t.3 & flags::EMBED_OPEN == 0));
        assert_eq!(
            kinds_of("<style></style>"),
            vec![
                kinds::LT,
                kinds::TAG_NAME,
                kinds::GT,
                kinds::LT,
                kinds::SLASH,
                kinds::TAG_NAME,
                kinds::GT,
            ]
        );
    }

    #[test]
    fn opener_is_not_a_breakpoint() {
        // The `>` that opens an embed must not be a STATE_BREAKPOINT.
        let tokens = run("<style>a{}</style>");
        let opener = tokens
            .iter()
            .find(|t| t.3 & flags::EMBED_OPEN != 0)
            .expect("embed opener");
        assert_eq!(opener.3 & flags::STATE_BREAKPOINT, 0);
    }

    #[test]
    fn coverage_over_mixed_document() {
        coverage_is_contiguous(
            "<!DOCTYPE html>\n<html><head><style>b{color:red}</style></head>\
             <body class=\"x\"><!--c--><p>hi &amp; bye</p>\
             <script>let x=`v`;</script></body></html>",
        );
    }

    fn embedded_languages(src: &str) -> std::collections::HashSet<u8> {
        let s: &dyn Source = &src;
        let table = TokenTable::new(Language::Html, s);
        let mut langs = std::collections::HashSet::new();
        for t in table.query(Span::new(0, table.source_len())) {
            if t.nest >= 1 {
                langs.insert(t.lang_tag());
            }
        }
        langs
    }

    #[test]
    fn style_embeds_css_and_script_embeds_ts() {
        let langs = embedded_languages("<style>.a{color:#fff}</style><script>const x=1;</script>");
        assert!(
            langs.contains(&Language::Css.tag()),
            "style should embed CSS"
        );
        assert!(
            langs.contains(&Language::Ts.tag()),
            "script should embed TS"
        );
    }

    #[test]
    fn rawtext_closer_wins_inside_strings() {
        // HTML's rawtext rule: the first `</style>` ends the block even though
        // it sits inside what would be a CSS string. The embedded region is
        // therefore bounded before the `</style>`.
        let s = r#"<style>a{content:"</style>"}"#;
        let src: &dyn Source = &s;
        let table = TokenTable::new(Language::Html, src);
        let toks: Vec<_> = table.query(Span::new(0, table.source_len())).collect();
        // Find the EMBED_OPEN `>` and the following EMBED_CLOSE `<`.
        let open = toks
            .iter()
            .position(|t| t.flags & flags::EMBED_OPEN != 0)
            .unwrap();
        let close = toks
            .iter()
            .position(|t| t.flags & flags::EMBED_CLOSE != 0)
            .unwrap();
        let body: Vec<_> = toks[open + 1..close].iter().map(|t| t.nest).collect();
        assert!(!body.is_empty());
        assert!(
            body.iter().all(|&n| n >= 1),
            "embedded body should be nested"
        );
        // The closer begins right at the first `</style>`.
        assert_eq!(
            toks[close].span.offset as usize,
            s.find("</style>").unwrap()
        );
    }
}
