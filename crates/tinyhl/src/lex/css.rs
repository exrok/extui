//! CSS lexer.
//!
//! This follows the token shapes of the CSS Syntax Module Level 3 tokenizer
//! rather than the grammar: tokens are classified by their lexical form, not
//! by whether they appear in a selector, a declaration, or an at-rule prelude.
//! That keeps the lexer a pure, context-free state machine over bytes, which
//! is what makes the incremental algorithm sound.
//!
//! Because every construct is self-contained — comments, strings, and `url()`
//! bodies are each consumed as a single token — CSS never carries state across
//! a token boundary. The lexer is therefore stateless: every boundary is
//! [`LexState::INITIAL`] and a safe incremental resume point.
//!
//! Recognized tokens:
//!
//! - `/* … */` comments ([`COMMENT`]), `"…"` / `'…'` strings ([`STRING`]).
//! - Numbers, percentages, and dimensions, folded into one [`NUMBER`] token
//!   (e.g. `42`, `1.5e3`, `50%`, `10px`).
//! - `#`-prefixed hash tokens ([`HASH_TOKEN`], id selectors and hex colors)
//!   and `@`-prefixed at-keywords ([`AT_KEYWORD`]).
//! - Identifiers ([`IDENT`]) including custom properties (`--accent`), vendor
//!   prefixes (`-webkit-box`), Unicode names, and `\`-escapes.
//! - `url( … )` with an unquoted body, consumed whole as a [`STRING`] so data
//!   URIs and paths do not shatter into punctuation. Quoted `url("…")` lexes
//!   as an ordinary ident + parentheses + string.
//! - Single-byte delimiters and combinators mapped to the shared punctuation
//!   kinds (`{`, `}`, `:`, `;`, `>`, `+`, `~`, `&`, …).
//!
//! [`COMMENT`]: kinds::COMMENT
//! [`STRING`]: kinds::STRING
//! [`NUMBER`]: kinds::NUMBER
//! [`HASH_TOKEN`]: kinds::HASH_TOKEN
//! [`AT_KEYWORD`]: kinds::AT_KEYWORD
//! [`IDENT`]: kinds::IDENT

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

pub(crate) struct Css;

impl Lexer for Css {
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
        b' ' | b'\t' | b'\n' | b'\r' | 0x0B | 0x0C => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            false,
        ),
        b'/' if view.byte_at(cursor + 1) == Some(b'*') => {
            let r = scan::scan_block_comment(view, cursor);
            (kinds::COMMENT, r.end, r.is_error)
        }
        b'/' => (kinds::SLASH, cursor + 1, false),
        b'"' | b'\'' => {
            let r = scan_css_string(view, cursor);
            (kinds::STRING, r.end, r.is_error)
        }
        b'#' => {
            if starts_name(view, cursor + 1) {
                (kinds::HASH_TOKEN, scan_name_run(view, cursor + 1), false)
            } else {
                (kinds::HASH, cursor + 1, false)
            }
        }
        b'@' => {
            if ident_starts_at(view, cursor + 1) {
                (kinds::AT_KEYWORD, scan_name_run(view, cursor + 1), false)
            } else {
                (kinds::AT, cursor + 1, false)
            }
        }
        b'.' => {
            if is_digit(view.byte_at(cursor + 1)) {
                (kinds::NUMBER, scan_css_number(view, cursor), false)
            } else {
                (kinds::DOT, cursor + 1, false)
            }
        }
        b'+' => {
            if starts_number_after_sign(view, cursor + 1) {
                (kinds::NUMBER, scan_css_number(view, cursor), false)
            } else {
                (kinds::PLUS, cursor + 1, false)
            }
        }
        b'-' => {
            if starts_number_after_sign(view, cursor + 1) {
                (kinds::NUMBER, scan_css_number(view, cursor), false)
            } else if ident_starts_at(view, cursor) {
                classify_ident_or_url(view, cursor)
            } else {
                (kinds::MINUS, cursor + 1, false)
            }
        }
        b'0'..=b'9' => (kinds::NUMBER, scan_css_number(view, cursor), false),
        b'\\' => {
            if is_valid_escape(view, cursor) {
                classify_ident_or_url(view, cursor)
            } else {
                (kinds::ERROR, cursor + 1, true)
            }
        }
        b'{' => (kinds::OPEN_BRACE, cursor + 1, false),
        b'}' => (kinds::CLOSE_BRACE, cursor + 1, false),
        b'(' => (kinds::OPEN_PAREN, cursor + 1, false),
        b')' => (kinds::CLOSE_PAREN, cursor + 1, false),
        b'[' => (kinds::OPEN_BRACKET, cursor + 1, false),
        b']' => (kinds::CLOSE_BRACKET, cursor + 1, false),
        b':' => (kinds::COLON, cursor + 1, false),
        b';' => (kinds::SEMI, cursor + 1, false),
        b',' => (kinds::COMMA, cursor + 1, false),
        b'>' => (kinds::GT, cursor + 1, false),
        b'<' => (kinds::LT, cursor + 1, false),
        b'~' => (kinds::TILDE, cursor + 1, false),
        b'*' => (kinds::STAR, cursor + 1, false),
        b'=' => (kinds::EQ, cursor + 1, false),
        b'|' => (kinds::PIPE, cursor + 1, false),
        b'^' => (kinds::CARET, cursor + 1, false),
        b'$' => (kinds::DOLLAR, cursor + 1, false),
        b'!' => (kinds::BANG, cursor + 1, false),
        b'%' => (kinds::PERCENT, cursor + 1, false),
        b'&' => (kinds::AMP, cursor + 1, false),
        b if b < 0x80 && byteclass::is_ident_start(b) => classify_ident_or_url(view, cursor),
        b if b >= 0x80 => classify_ident_or_url(view, cursor),
        _ => (kinds::ERROR, error_end(view, cursor), true),
    }
}

/// Classifies an identifier that may instead be the start of an unquoted
/// `url( … )` token. The caller must have established that an identifier
/// starts at `cursor`.
fn classify_ident_or_url(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan_name_run(view, cursor);
    if view.byte_at(end) == Some(b'(') && name_eq_ignore_case(view, cursor, end, b"url") {
        if let Some(r) = scan_unquoted_url(view, end) {
            return (kinds::STRING, r.end, r.is_error);
        }
    }
    (kinds::IDENT, end, false)
}

/// Scans a CSS string starting at the opening quote at `cursor`.
///
/// Handles `\`-escapes and `\`-newline line continuations. An unescaped
/// newline or end-of-source terminates the string early with `is_error` set
/// (a *bad-string* in CSS terms); the offending newline is not consumed.
fn scan_css_string(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let Some(quote) = view.byte_at(cursor) else {
        return ScanResult {
            end: cursor,
            is_error: true,
        };
    };
    let mut end = cursor + 1;
    loop {
        match view.byte_at(end) {
            None => {
                return ScanResult {
                    end,
                    is_error: true,
                };
            }
            Some(b) if b == quote => {
                return ScanResult {
                    end: end + 1,
                    is_error: false,
                };
            }
            Some(b) if is_newline(b) => {
                return ScanResult {
                    end,
                    is_error: true,
                };
            }
            Some(b'\\') => match view.byte_at(end + 1) {
                None => {
                    return ScanResult {
                        end: end + 1,
                        is_error: true,
                    };
                }
                Some(b'\r') if view.byte_at(end + 2) == Some(b'\n') => end += 3,
                Some(nb) if is_newline(nb) => end += 2,
                Some(_) => {
                    let len = scan::decoded_len_or_one(view, end + 1);
                    end += 1 + len;
                }
            },
            Some(_) => end += 1,
        }
    }
}

/// Scans the unquoted body of a `url( … )` token. `ident_end` points just past
/// the `url` ident, at the `(`. The returned `end` is absolute, so the caller
/// pairs it with the original token start.
///
/// Returns `None` when the parenthesis introduces a quoted argument (`"`/`'`)
/// — in that case `url` is an ordinary function name and the caller emits it as
/// an identifier. Otherwise returns the end of the whole `url( … )` run,
/// flagging an unterminated body as an error.
fn scan_unquoted_url(view: &mut SourceView<'_>, ident_end: u32) -> Option<ScanResult> {
    let mut cursor = ident_end + 1;
    while matches!(view.byte_at(cursor), Some(b) if is_whitespace(b)) {
        cursor += 1;
    }
    match view.byte_at(cursor) {
        Some(b'"') | Some(b'\'') => return None,
        _ => {}
    }
    loop {
        match view.byte_at(cursor) {
            None => {
                return Some(ScanResult {
                    end: cursor,
                    is_error: true,
                });
            }
            Some(b')') => {
                return Some(ScanResult {
                    end: cursor + 1,
                    is_error: false,
                });
            }
            Some(b'\\') if matches!(view.byte_at(cursor + 1), Some(b) if !is_newline(b)) => {
                let len = scan::decoded_len_or_one(view, cursor + 1);
                cursor += 1 + len;
            }
            Some(_) => cursor += 1,
        }
    }
}

/// Scans a CSS number, percentage, or dimension starting at `cursor`.
///
/// The caller must have established that a number starts here. Any trailing
/// `%` or unit identifier is folded into the returned span so a dimension such
/// as `10px` is a single token.
fn scan_css_number(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut end = cursor;
    if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
        end += 1;
    }
    end = scan::scan_digits(view, end);

    if view.byte_at(end) == Some(b'.') && is_digit(view.byte_at(end + 1)) {
        end = scan::scan_digits(view, end + 1);
    }

    if matches!(view.byte_at(end), Some(b'e') | Some(b'E')) {
        let mut exp = end + 1;
        if matches!(view.byte_at(exp), Some(b'+') | Some(b'-')) {
            exp += 1;
        }
        if is_digit(view.byte_at(exp)) {
            end = scan::scan_digits(view, exp);
        }
    }

    if view.byte_at(end) == Some(b'%') {
        end + 1
    } else if ident_starts_at(view, end) {
        scan_name_run(view, end)
    } else {
        end
    }
}

/// Consumes a run of CSS name code points (`[A-Za-z0-9_-]`, any non-ASCII
/// byte, and `\`-escapes) starting at `cursor`. Returns `cursor` unchanged
/// when the first byte is not a name code point.
fn scan_name_run(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    let mut cursor = cursor;
    loop {
        let Some(b) = view.byte_at(cursor) else {
            return cursor;
        };
        if b < 0x80 {
            if is_name_byte(b) {
                cursor += 1;
            } else if b == b'\\' && matches!(view.byte_at(cursor + 1), Some(n) if !is_newline(n)) {
                let len = scan::decoded_len_or_one(view, cursor + 1);
                cursor += 1 + len;
            } else {
                return cursor;
            }
        } else {
            let len = scan::decoded_len_or_one(view, cursor);
            cursor += len;
        }
    }
}

/// Returns `true` if the bytes from `start` to `end` equal `expected`,
/// comparing ASCII letters case-insensitively. Used to spot the `url` ident.
fn name_eq_ignore_case(view: &mut SourceView<'_>, start: u32, end: u32, expected: &[u8]) -> bool {
    if (end - start) as usize != expected.len() {
        return false;
    }
    for (i, &want) in expected.iter().enumerate() {
        match view.byte_at(start + i as u32) {
            Some(got) if got.to_ascii_lowercase() == want => {}
            _ => return false,
        }
    }
    true
}

/// Returns `true` if an identifier (per the CSS "would start an identifier"
/// rule) begins at `cursor`.
fn ident_starts_at(view: &mut SourceView<'_>, cursor: u32) -> bool {
    match view.byte_at(cursor) {
        Some(b'-') => match view.byte_at(cursor + 1) {
            Some(b'-') => true,
            Some(b) if b >= 0x80 => true,
            Some(b) if byteclass::is_ident_start(b) => true,
            Some(b'\\') => is_valid_escape(view, cursor + 1),
            _ => false,
        },
        Some(b'\\') => is_valid_escape(view, cursor),
        Some(b) if b >= 0x80 => true,
        Some(b) => byteclass::is_ident_start(b),
        None => false,
    }
}

/// Returns `true` if the byte at `cursor` can be the first code point of a
/// CSS name (the hash-token rule, which — unlike an identifier — admits a
/// leading digit).
fn starts_name(view: &mut SourceView<'_>, cursor: u32) -> bool {
    match view.byte_at(cursor) {
        Some(b'\\') => is_valid_escape(view, cursor),
        Some(b) if b >= 0x80 => true,
        Some(b) => is_name_byte(b),
        None => false,
    }
}

/// Returns `true` if a number can start at `cursor`, where `cursor` already
/// sits just past a leading `+`/`-` sign.
fn starts_number_after_sign(view: &mut SourceView<'_>, cursor: u32) -> bool {
    if is_digit(view.byte_at(cursor)) {
        return true;
    }
    view.byte_at(cursor) == Some(b'.') && is_digit(view.byte_at(cursor + 1))
}

/// Returns `true` if `cursor` points at a `\` that begins a valid escape (a
/// backslash not immediately followed by a newline or end-of-source).
fn is_valid_escape(view: &mut SourceView<'_>, cursor: u32) -> bool {
    view.byte_at(cursor) == Some(b'\\')
        && matches!(view.byte_at(cursor + 1), Some(b) if !is_newline(b))
}

#[inline]
fn is_name_byte(b: u8) -> bool {
    byteclass::is_ident_cont(b) || b == b'-'
}

#[inline]
fn is_digit(b: Option<u8>) -> bool {
    matches!(b, Some(b) if byteclass::is_digit(b))
}

#[inline]
fn is_whitespace(b: u8) -> bool {
    byteclass::is_whitespace(b)
}

#[inline]
fn is_newline(b: u8) -> bool {
    matches!(b, b'\n' | b'\r' | 0x0C)
}

fn error_end(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    cursor + scan::decoded_len_or_one(view, cursor)
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
            (cursor, state) = Css::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("CSS has no embedding"),
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
    fn simple_rule() {
        assert_eq!(
            kinds_of("a { color: red; }"),
            vec![
                kinds::IDENT, // a
                kinds::WHITESPACE,
                kinds::OPEN_BRACE,
                kinds::WHITESPACE,
                kinds::IDENT, // color
                kinds::COLON,
                kinds::WHITESPACE,
                kinds::IDENT, // red
                kinds::SEMI,
                kinds::WHITESPACE,
                kinds::CLOSE_BRACE,
            ]
        );
    }

    #[test]
    fn selectors_class_id_pseudo() {
        assert_eq!(
            kinds_of(".btn#main:hover"),
            vec![
                kinds::DOT,
                kinds::IDENT,      // btn
                kinds::HASH_TOKEN, // #main
                kinds::COLON,
                kinds::IDENT, // hover
            ]
        );
    }

    #[test]
    fn hash_colors_and_bare_hash() {
        assert_eq!(run("#ff0000")[0].0, kinds::HASH_TOKEN);
        assert_eq!(run("#abc")[0].0, kinds::HASH_TOKEN);
        // A `#` with no following name code point is a bare delimiter.
        assert_eq!(run("# ")[0].0, kinds::HASH);
    }

    #[test]
    fn at_rules() {
        let t = run("@media (min-width: 700px) {}");
        assert_eq!(t[0].0, kinds::AT_KEYWORD);
        assert_eq!(t[0].2, 6); // "@media"
        let k = kinds_of("@import \"x.css\";");
        assert_eq!(k[0], kinds::AT_KEYWORD);
        assert!(k.contains(&kinds::STRING));
        // A bare `@` not followed by an identifier is a delimiter.
        assert_eq!(run("@ ")[0].0, kinds::AT);
    }

    #[test]
    fn numbers_dimensions_percent() {
        for (input, len) in [
            ("0", 1),
            ("42", 2),
            ("-1", 2),
            ("+3", 2),
            ("1.5", 3),
            (".5", 2),
            ("-.5", 3),
            ("1e3", 3),
            ("1.5e-3", 6),
            ("10px", 4),
            ("2.5rem", 6),
            ("50%", 3),
            ("1e3px", 5),
            ("1em", 3),
        ] {
            let t = run(input);
            assert_eq!(t.len(), 1, "input {input:?} -> {t:?}");
            assert_eq!(t[0].0, kinds::NUMBER, "input {input:?}");
            assert_eq!(t[0].2 as usize, len, "input {input:?}");
        }
    }

    #[test]
    fn dot_before_non_digit_is_delim() {
        assert_eq!(kinds_of(".foo"), vec![kinds::DOT, kinds::IDENT]);
        assert_eq!(kinds_of("1.foo")[0], kinds::NUMBER);
        assert_eq!(&kinds_of("1.foo")[1..], &[kinds::DOT, kinds::IDENT]);
    }

    #[test]
    fn custom_properties_and_vendor_prefixes() {
        assert_eq!(run("--main-color")[0].0, kinds::IDENT);
        assert_eq!(run("--main-color")[0].2, 12);
        assert_eq!(run("-webkit-box")[0].0, kinds::IDENT);
        // A lone `-` is a delimiter.
        assert_eq!(run("- ")[0].0, kinds::MINUS);
    }

    #[test]
    fn strings_basic_and_escapes() {
        for (input, len) in [
            (r#""abc""#, 5),
            (r#"'abc'"#, 5),
            (r#""a\"b""#, 6),
            (
                r#""line\
continued""#,
                17,
            ),
        ] {
            let t = run(input);
            assert_eq!(t[0].0, kinds::STRING, "input {input:?}");
            assert_eq!(t[0].2 as usize, len, "input {input:?}");
            assert_eq!(t[0].3 & flags::IS_ERROR, 0, "input {input:?}");
        }
    }

    #[test]
    fn string_unterminated_and_bad_newline() {
        let t = run("\"abc");
        assert_eq!(t[0].0, kinds::STRING);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);

        // A raw newline ends the string early and is not consumed.
        let t = run("\"a\nb");
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].2, 2);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);
        assert_eq!(t[1].0, kinds::WHITESPACE);
    }

    #[test]
    fn comments() {
        let t = run("/* hi */ a");
        assert_eq!(t[0].0, kinds::COMMENT);
        assert_eq!(t[0].2, 8);
        // Unterminated comment errors but still covers the rest of input.
        let t = run("/* open");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::COMMENT);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn url_unquoted_is_one_token() {
        let t = run("url(images/bg.png)");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].2 as usize, "url(images/bg.png)".len());

        // Data URIs with colons, slashes, and semicolons stay intact.
        let t = run("url(data:image/png;base64,iVBOR)");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
    }

    #[test]
    fn url_quoted_lexes_as_function() {
        assert_eq!(
            kinds_of("url(\"x.png\")"),
            vec![
                kinds::IDENT,
                kinds::OPEN_PAREN,
                kinds::STRING,
                kinds::CLOSE_PAREN
            ]
        );
    }

    #[test]
    fn url_unterminated_is_error() {
        let t = run("url(abc");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn functions_keep_parens_separate() {
        assert_eq!(
            kinds_of("rgb(1,2,3)"),
            vec![
                kinds::IDENT,
                kinds::OPEN_PAREN,
                kinds::NUMBER,
                kinds::COMMA,
                kinds::NUMBER,
                kinds::COMMA,
                kinds::NUMBER,
                kinds::CLOSE_PAREN,
            ]
        );
    }

    #[test]
    fn combinators_and_nesting_amp() {
        let k = kinds_of("a > b + c ~ d & e");
        assert!(k.contains(&kinds::GT));
        assert!(k.contains(&kinds::PLUS));
        assert!(k.contains(&kinds::TILDE));
        assert!(k.contains(&kinds::AMP));
    }

    #[test]
    fn attribute_selector() {
        assert_eq!(
            kinds_of("[data-x^=\"v\"]"),
            vec![
                kinds::OPEN_BRACKET,
                kinds::IDENT,
                kinds::CARET,
                kinds::EQ,
                kinds::STRING,
                kinds::CLOSE_BRACKET,
            ]
        );
    }

    #[test]
    fn important_is_bang_then_ident() {
        let k = kinds_of("!important");
        assert_eq!(k, vec![kinds::BANG, kinds::IDENT]);
    }

    #[test]
    fn unicode_identifier() {
        let t = run("\u{00e9}lan");
        assert_eq!(t[0].0, kinds::IDENT);
        assert_eq!(t[0].2 as usize, "\u{00e9}lan".len());
    }

    #[test]
    fn escaped_selector() {
        // Tailwind-style escaped class name `.md\:flex`.
        let k = kinds_of(".md\\:flex");
        assert_eq!(k, vec![kinds::DOT, kinds::IDENT]);
        assert_eq!(run(".md\\:flex")[1].2 as usize, "md\\:flex".len());
    }

    #[test]
    fn stray_backslash_is_error() {
        let t = run("\\\n");
        assert_eq!(t[0].0, kinds::ERROR);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn coverage_is_contiguous() {
        let input = "/* c */\n@media screen {\n  .box::before { content: \"\\2014\"; width: calc(100% - 10px); background: url(a.png) #fff; }\n}\n";
        let tokens = run(input);
        let mut pos = 0u32;
        for (_kind, offset, len, _flags) in &tokens {
            assert_eq!(*offset, pos, "gap or overlap before offset {pos}");
            pos += len;
        }
        assert_eq!(pos as usize, input.len());
    }

    struct Paged<'a> {
        pages: &'a [&'a [u8]],
        total: u32,
    }

    impl<'a> Paged<'a> {
        fn new(pages: &'a [&'a [u8]]) -> Self {
            let mut total = 0u32;
            for p in pages {
                total += p.len() as u32;
            }
            Self { pages, total }
        }
    }

    impl Source for Paged<'_> {
        fn len(&self) -> u32 {
            self.total
        }

        fn page(&self, offset: u32) -> (u32, &[u8]) {
            let mut base = 0u32;
            for p in self.pages {
                let end = base + p.len() as u32;
                if offset < end {
                    return (base, p);
                }
                base = end;
            }
            (self.total, &[])
        }
    }

    #[test]
    fn url_body_splits_across_pages() {
        let p = Paged::new(&[b"url(ima", b"ges/bg.png)x"]);
        let src: &dyn Source = &p;
        let mut view = SourceView::new(src, 0);
        let end = scan_name_run(&mut view, 0); // "url"
        let r = scan_unquoted_url(&mut view, end).unwrap();
        assert!(!r.is_error);
        assert_eq!(r.end as usize, "url(images/bg.png)".len());
    }
}
