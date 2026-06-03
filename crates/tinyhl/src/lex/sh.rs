//! POSIX shell lexer.
//!
//! Shell is lexed as a mostly stateless byte machine: ordinary tokens are
//! produced at [`LexState::INITIAL`] and carry [`STATE_BREAKPOINT`]. Quoted
//! strings, backtick command substitutions, comments, and here-doc bodies are
//! scanned to completion as single tokens so a `#`, quote, or shell keyword
//! inside them cannot change the apparent structure of the file.
//!
//! Deliberate "good enough" choices:
//!
//! - Reserved words are highlighted wherever they appear as bare identifiers;
//!   parser-position checks are not modelled.
//! - Parameter expansion is tokenized as `$`, delimiters, identifiers, and
//!   operators rather than as a special variable token.
//! - A here-doc redirection emits the `<<` / `<<-` operator first, then one
//!   [`STRING`] token from the delimiter word through the closing delimiter.
//!   The operator token is not a breakpoint, because resuming at the delimiter
//!   would lose the pending here-doc body.
//!
//! [`STRING`]: kinds::STRING
//! [`STATE_BREAKPOINT`]: flags::STATE_BREAKPOINT

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

kw::kw_table! {
    static SH_KWS = [
        (b"case", kinds::KEYWORD),
        (b"do", kinds::KEYWORD),
        (b"done", kinds::KEYWORD),
        (b"elif", kinds::KEYWORD),
        (b"else", kinds::KEYWORD),
        (b"esac", kinds::KEYWORD),
        (b"fi", kinds::KEYWORD),
        (b"for", kinds::KEYWORD),
        (b"if", kinds::KEYWORD),
        (b"in", kinds::KEYWORD),
        (b"then", kinds::KEYWORD),
        (b"until", kinds::KEYWORD),
        (b"while", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 5; // "until" / "while"
const MAX_HEREDOC_DELIM: usize = 64;

mod state {
    pub const MID_WORD: u16 = 1 << 0;
}

pub(crate) struct Sh;

impl Lexer for Sh {
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

            if b == b'<' && view.byte_at(cursor + 1) == Some(b'<') {
                if let Some(here) = try_heredoc(view, cursor) {
                    if out.remaining() < 2 {
                        return (cursor, state);
                    }
                    out.push(LexStep::Token {
                        len: here.op_len,
                        local_kind: kinds::SHL,
                        state_out: LexState(state::MID_WORD),
                        flags: 0,
                    });

                    let body_start = cursor + here.op_len;
                    let state_out = state_after_heredoc(view, here.end);
                    let mut bit_flags = flags::STATE_BREAKPOINT;
                    if here.is_error {
                        bit_flags |= flags::IS_ERROR;
                    }
                    out.push(LexStep::Token {
                        len: here.end - body_start,
                        local_kind: kinds::STRING,
                        state_out,
                        flags: bit_flags,
                    });
                    cursor = here.end;
                    state = state_out;
                    continue;
                }
            }

            let start = cursor;
            let (local_kind, end, is_error) = classify(view, cursor, b, state);
            debug_assert!(end > start, "lexer must consume at least one byte");
            let state_out = state_after_token(view, start, end, local_kind, state);

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
) -> (u16, u32, bool) {
    match first {
        b if byteclass::is_whitespace(b) => (
            kinds::WHITESPACE,
            scan::scan_whitespace(view, cursor),
            false,
        ),
        b'#' if comments_allowed(state) => {
            (kinds::COMMENT, scan::scan_hash_comment(view, cursor), false)
        }
        b'\'' => {
            let r = scan_single_quote(view, cursor);
            (kinds::STRING, r.end, r.is_error)
        }
        b'"' => {
            let r = scan_double_quote(view, cursor);
            (kinds::STRING, r.end, r.is_error)
        }
        b'`' => {
            let r = scan_backtick(view, cursor);
            (kinds::TEMPLATE_STRING, r.end, r.is_error)
        }
        b'\\' => scan_backslash(view, cursor),
        b'0'..=b'9' => (kinds::NUMBER, scan::scan_digits(view, cursor), false),
        b if byteclass::is_ident_start(b) => classify_ident(view, cursor),
        b if b >= 0x80 => classify_unicode_ident(view, cursor),
        b'+' | b'-' | b'*' | b'/' | b'%' | b'&' | b'|' | b'^' | b'~' | b'<' | b'>' | b'='
        | b'!' | b':' | b',' | b';' | b'.' | b'?' | b'@' | b'#' | b'$' | b'(' | b')' | b'{'
        | b'}' | b'[' | b']' => {
            let (kind, end) = scan_operator(view, cursor, first);
            (kind, end, false)
        }
        _ => (kinds::ERROR, cursor + 1, true),
    }
}

#[inline]
fn canonical_state(state: LexState) -> LexState {
    LexState(state.bits() & state::MID_WORD)
}

#[inline]
fn comments_allowed(state: LexState) -> bool {
    canonical_state(state).is_initial()
}

fn state_after_token(
    view: &mut SourceView<'_>,
    start: u32,
    end: u32,
    local_kind: u16,
    previous: LexState,
) -> LexState {
    match local_kind {
        kinds::WHITESPACE => {
            if view.byte_at(start) == Some(b'\\') {
                canonical_state(previous)
            } else {
                LexState::INITIAL
            }
        }
        kinds::SEMI
        | kinds::AMP
        | kinds::AMP_AMP
        | kinds::PIPE
        | kinds::PIPE_PIPE
        | kinds::OPEN_PAREN => LexState::INITIAL,
        kinds::COMMENT if ends_at_newline(view, end) => LexState::INITIAL,
        _ => LexState(state::MID_WORD),
    }
}

fn state_after_heredoc(view: &mut SourceView<'_>, end: u32) -> LexState {
    if ends_at_newline(view, end) {
        LexState::INITIAL
    } else {
        LexState(state::MID_WORD)
    }
}

fn classify_ident(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan::scan_ident_ascii(view, cursor);
    let len = (end - cursor) as usize;
    if len <= MAX_KW_LEN {
        let mut buf = [0u8; MAX_KW_LEN];
        if scan::copy_bytes(view, cursor, &mut buf[..len]) {
            if let Some(kind) = kw_lookup(SH_KWS, &buf[..len]) {
                return (kind, end, false);
            }
        }
    }
    (kinds::IDENT, end, false)
}

fn classify_unicode_ident(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    let end = scan::scan_xid_ident(view, cursor, false);
    if end > cursor {
        return (kinds::IDENT, end, false);
    }
    let len = scan::decoded_len_or_one(view, cursor);
    (kinds::ERROR, cursor + len, true)
}

fn ends_at_newline(view: &mut SourceView<'_>, end: u32) -> bool {
    end > 0 && view.byte_at(end - 1) == Some(b'\n')
}

fn scan_single_quote(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor + 1;
    loop {
        match view.byte_at(end) {
            None => return err(end),
            Some(b'\'') => return ok(end + 1),
            Some(_) => end += 1,
        }
    }
}

fn scan_double_quote(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor + 1;
    loop {
        match view.byte_at(end) {
            None => return err(end),
            Some(b'"') => return ok(end + 1),
            Some(b'\\') => {
                end += 1;
                match view.byte_at(end) {
                    None => return err(end),
                    Some(_) => end += 1,
                }
            }
            Some(_) => end += 1,
        }
    }
}

fn scan_backtick(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor + 1;
    loop {
        match view.byte_at(end) {
            None => return err(end),
            Some(b'`') => return ok(end + 1),
            Some(b'\\') => {
                end += 1;
                match view.byte_at(end) {
                    None => return err(end),
                    Some(_) => end += 1,
                }
            }
            Some(_) => end += 1,
        }
    }
}

fn scan_backslash(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool) {
    match view.byte_at(cursor + 1) {
        Some(b'\r') => {
            let mut end = cursor + 2;
            if view.byte_at(end) == Some(b'\n') {
                end += 1;
            }
            (kinds::WHITESPACE, end, false)
        }
        Some(b'\n') => (kinds::WHITESPACE, cursor + 2, false),
        Some(_) => (kinds::STRING, cursor + 2, false),
        None => (kinds::STRING, cursor + 1, true),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct HeredocScan {
    op_len: u32,
    end: u32,
    is_error: bool,
}

fn try_heredoc(view: &mut SourceView<'_>, cursor: u32) -> Option<HeredocScan> {
    let strip_tabs = view.byte_at(cursor + 2) == Some(b'-');
    let op_len = if strip_tabs { 3 } else { 2 };
    let mut p = cursor + op_len;
    while matches!(view.byte_at(p), Some(b' ' | b'\t')) {
        p += 1;
    }

    let mut delim = [0u8; MAX_HEREDOC_DELIM];
    let (after_word, delim_len) = parse_heredoc_delim(view, p, &mut delim)?;
    if delim_len == 0 {
        return None;
    }

    let line_end = scan_to_eol_inclusive(view, after_word);
    if line_end == view.len() && view.byte_at(line_end.saturating_sub(1)) != Some(b'\n') {
        return Some(HeredocScan {
            op_len,
            end: line_end,
            is_error: true,
        });
    }

    let mut line_start = line_end;
    while view.byte_at(line_start).is_some() {
        let next_line = scan_to_eol_inclusive(view, line_start);
        if heredoc_line_matches(view, line_start, next_line, strip_tabs, &delim[..delim_len]) {
            return Some(HeredocScan {
                op_len,
                end: next_line,
                is_error: false,
            });
        }
        if next_line == line_start {
            break;
        }
        line_start = next_line;
    }

    Some(HeredocScan {
        op_len,
        end: view.len(),
        is_error: true,
    })
}

fn parse_heredoc_delim(
    view: &mut SourceView<'_>,
    cursor: u32,
    out: &mut [u8; MAX_HEREDOC_DELIM],
) -> Option<(u32, usize)> {
    let mut p = cursor;
    let mut len = 0usize;
    let mut saw_any = false;
    loop {
        let Some(b) = view.byte_at(p) else {
            break;
        };
        if is_heredoc_word_end(b) {
            break;
        }
        saw_any = true;
        match b {
            b'\'' => {
                p += 1;
                loop {
                    match view.byte_at(p) {
                        Some(b'\'') => {
                            p += 1;
                            break;
                        }
                        Some(b'\n') | None => return None,
                        Some(x) => {
                            push_delim_byte(out, &mut len, x)?;
                            p += 1;
                        }
                    }
                }
            }
            b'"' => {
                p += 1;
                loop {
                    match view.byte_at(p) {
                        Some(b'"') => {
                            p += 1;
                            break;
                        }
                        Some(b'\n') | None => return None,
                        Some(b'\\') => {
                            p += 1;
                            let x = view.byte_at(p)?;
                            if x == b'\n' {
                                return None;
                            }
                            push_delim_byte(out, &mut len, x)?;
                            p += 1;
                        }
                        Some(x) => {
                            push_delim_byte(out, &mut len, x)?;
                            p += 1;
                        }
                    }
                }
            }
            b'\\' => {
                p += 1;
                let x = view.byte_at(p)?;
                if x == b'\n' {
                    return None;
                }
                push_delim_byte(out, &mut len, x)?;
                p += 1;
            }
            x => {
                push_delim_byte(out, &mut len, x)?;
                p += 1;
            }
        }
    }

    if saw_any { Some((p, len)) } else { None }
}

fn push_delim_byte(out: &mut [u8; MAX_HEREDOC_DELIM], len: &mut usize, b: u8) -> Option<()> {
    if *len == out.len() {
        return None;
    }
    out[*len] = b;
    *len += 1;
    Some(())
}

fn is_heredoc_word_end(b: u8) -> bool {
    matches!(
        b,
        b' ' | b'\t' | b'\r' | b'\n' | b';' | b'&' | b'|' | b'(' | b')' | b'<' | b'>'
    )
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

fn heredoc_line_matches(
    view: &mut SourceView<'_>,
    line_start: u32,
    line_end: u32,
    strip_tabs: bool,
    delim: &[u8],
) -> bool {
    let mut start = line_start;
    if strip_tabs {
        while view.byte_at(start) == Some(b'\t') {
            start += 1;
        }
    }

    let mut end = line_end;
    if end > start && view.byte_at(end - 1) == Some(b'\n') {
        end -= 1;
    }
    if end > start && view.byte_at(end - 1) == Some(b'\r') {
        end -= 1;
    }
    if (end - start) as usize != delim.len() {
        return false;
    }
    for (i, expected) in delim.iter().enumerate() {
        if view.byte_at(start + i as u32) != Some(*expected) {
            return false;
        }
    }
    true
}

fn scan_operator(view: &mut SourceView<'_>, cursor: u32, b0: u8) -> (u16, u32) {
    let b1 = view.byte_at(cursor + 1);
    let (kind, len) = match b0 {
        b'+' => (kinds::PLUS, 1),
        b'-' => (kinds::MINUS, 1),
        b'*' => (kinds::STAR, 1),
        b'/' => (kinds::SLASH, 1),
        b'%' => (kinds::PERCENT, 1),
        b'&' => match b1 {
            Some(b'&') => (kinds::AMP_AMP, 2),
            _ => (kinds::AMP, 1),
        },
        b'|' => match b1 {
            Some(b'|') => (kinds::PIPE_PIPE, 2),
            _ => (kinds::PIPE, 1),
        },
        b'^' => (kinds::CARET, 1),
        b'~' => (kinds::TILDE, 1),
        b'<' => match b1 {
            Some(b'<') => (kinds::SHL, 2),
            Some(b'=') => (kinds::LT_EQ, 2),
            Some(b'&' | b'>') => (kinds::LT, 2),
            _ => (kinds::LT, 1),
        },
        b'>' => match b1 {
            Some(b'>') => (kinds::SHR, 2),
            Some(b'=') => (kinds::GT_EQ, 2),
            Some(b'&' | b'|') => (kinds::GT, 2),
            _ => (kinds::GT, 1),
        },
        b'=' => match b1 {
            Some(b'=') => (kinds::EQ_EQ, 2),
            _ => (kinds::EQ, 1),
        },
        b'!' => match b1 {
            Some(b'=') => (kinds::BANG_EQ, 2),
            _ => (kinds::BANG, 1),
        },
        b':' => (kinds::COLON, 1),
        b',' => (kinds::COMMA, 1),
        b';' => match b1 {
            Some(b';') if view.byte_at(cursor + 2) == Some(b'&') => (kinds::SEMI, 3),
            Some(b';' | b'&') => (kinds::SEMI, 2),
            _ => (kinds::SEMI, 1),
        },
        b'.' => (kinds::DOT, 1),
        b'?' => (kinds::QUESTION, 1),
        b'@' => (kinds::AT, 1),
        b'#' => (kinds::HASH, 1),
        b'$' => (kinds::DOLLAR, 1),
        b'(' => (kinds::OPEN_PAREN, 1),
        b')' => (kinds::CLOSE_PAREN, 1),
        b'{' => (kinds::OPEN_BRACE, 1),
        b'}' => (kinds::CLOSE_BRACE, 1),
        b'[' => (kinds::OPEN_BRACKET, 1),
        b']' => (kinds::CLOSE_BRACKET, 1),
        _ => (kinds::ERROR, 1),
    };
    (kind, cursor + len)
}

fn ok(end: u32) -> ScanResult {
    ScanResult {
        end,
        is_error: false,
    }
}

fn err(end: u32) -> ScanResult {
    ScanResult {
        end,
        is_error: true,
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
            let before = cursor;
            (cursor, state) = Sh::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("Sh has no embedding"),
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
    fn coverage_is_contiguous() {
        let input = "if [ \"$x\" = yes ]; then echo ok; fi\n";
        let tokens = run(input);
        let mut pos = 0u32;
        for (_k, off, len, _f) in &tokens {
            assert_eq!(*off, pos, "gap before {pos}");
            pos += len;
        }
        assert_eq!(pos as usize, input.len());
    }

    #[test]
    fn keyword_vs_ident() {
        assert_eq!(kinds_of("if"), vec![kinds::KEYWORD]);
        assert_eq!(kinds_of("then"), vec![kinds::KEYWORD]);
        assert_eq!(kinds_of("done"), vec![kinds::KEYWORD]);
        assert_eq!(kinds_of("ifdef"), vec![kinds::IDENT]);
        assert_eq!(kinds_of("export"), vec![kinds::IDENT]);
    }

    #[test]
    fn comments_start_at_word_boundary() {
        assert_eq!(kinds_of("# c"), vec![kinds::COMMENT]);
        assert_eq!(
            kinds_of("echo foo#bar # c\nx"),
            vec![
                kinds::IDENT,
                kinds::WHITESPACE,
                kinds::IDENT,
                kinds::HASH,
                kinds::IDENT,
                kinds::WHITESPACE,
                kinds::COMMENT,
                kinds::WHITESPACE,
                kinds::IDENT,
            ]
        );
    }

    #[test]
    fn quoted_forms_are_single_tokens() {
        for s in ["'a\nb'", "\"a # b\"", "\"a\\\"b\"", "`echo # not-comment`"] {
            let t = run(s);
            assert_eq!(t.len(), 1, "{s:?} -> {t:?}");
            assert_eq!(
                t[0].0,
                if s.starts_with('`') {
                    kinds::TEMPLATE_STRING
                } else {
                    kinds::STRING
                },
                "{s:?}"
            );
            assert_eq!(t[0].2 as usize, s.len(), "{s:?} length");
            assert_eq!(t[0].3 & flags::IS_ERROR, 0, "{s:?}");
        }
    }

    #[test]
    fn unterminated_quotes_flag_error() {
        for s in ["'abc", "\"abc", "`abc"] {
            let t = run(s);
            assert_eq!(t.len(), 1, "{s:?}");
            assert_ne!(t[0].3 & flags::IS_ERROR, 0, "{s:?}");
        }
    }

    #[test]
    fn variables_and_parameter_expansion() {
        assert_eq!(
            kinds_of("$HOME ${name:-x} $? $$ $1"),
            vec![
                kinds::DOLLAR,
                kinds::IDENT,
                kinds::WHITESPACE,
                kinds::DOLLAR,
                kinds::OPEN_BRACE,
                kinds::IDENT,
                kinds::COLON,
                kinds::MINUS,
                kinds::IDENT,
                kinds::CLOSE_BRACE,
                kinds::WHITESPACE,
                kinds::DOLLAR,
                kinds::QUESTION,
                kinds::WHITESPACE,
                kinds::DOLLAR,
                kinds::DOLLAR,
                kinds::WHITESPACE,
                kinds::DOLLAR,
                kinds::NUMBER,
            ]
        );
    }

    #[test]
    fn heredoc_body_is_string() {
        let src = "cat <<EOF\nhello # not comment\nEOF\nnext\n";
        let tokens = run(src);
        assert_eq!(tokens[0].0, kinds::IDENT);
        assert_eq!(tokens[2].0, kinds::SHL);
        assert_eq!(tokens[2].2, 2);
        assert_eq!(tokens[2].3 & flags::STATE_BREAKPOINT, 0);
        assert_eq!(tokens[3].0, kinds::STRING);
        assert_eq!(tokens[3].3 & flags::IS_ERROR, 0);
        assert_eq!(tokens.last().unwrap().0, kinds::WHITESPACE);
    }

    #[test]
    fn quoted_and_tab_stripping_heredoc_delimiters() {
        let quoted = run("cat <<'EOF'\n$not expanded\nEOF\n");
        assert_eq!(quoted[2].0, kinds::SHL);
        assert_eq!(quoted[3].0, kinds::STRING);
        assert_eq!(quoted[3].3 & flags::IS_ERROR, 0);

        let stripped = run("cat <<-EOF\n\tbody\n\tEOF\n");
        assert_eq!(stripped[2].0, kinds::SHL);
        assert_eq!(stripped[2].2, 3);
        assert_eq!(stripped[3].0, kinds::STRING);
        assert_eq!(stripped[3].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn unterminated_heredoc_flags_error() {
        let tokens = run("cat <<EOF\nbody\n");
        assert_eq!(tokens[2].0, kinds::SHL);
        assert_eq!(tokens[3].0, kinds::STRING);
        assert_ne!(tokens[3].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn operators_greedy() {
        assert_eq!(kinds_of("&&"), vec![kinds::AMP_AMP]);
        assert_eq!(kinds_of("||"), vec![kinds::PIPE_PIPE]);
        assert_eq!(kinds_of(">>"), vec![kinds::SHR]);
        assert_eq!(kinds_of("<&"), vec![kinds::LT]);
        assert_eq!(kinds_of(">&"), vec![kinds::GT]);
        assert_eq!(kinds_of(";;"), vec![kinds::SEMI]);
        assert_eq!(kinds_of(";;&"), vec![kinds::SEMI]);
    }

    #[test]
    fn line_continuation_and_escapes() {
        assert_eq!(kinds_of("\\\n"), vec![kinds::WHITESPACE]);
        assert_eq!(kinds_of("\\ "), vec![kinds::STRING]);
        let t = run("\\");
        assert_eq!(t[0].0, kinds::STRING);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn every_safe_token_has_breakpoint() {
        let input = "if test \"$x\" = 1; then echo ok # comment\nfi\n";
        for t in run(input) {
            assert_ne!(
                t.3 & flags::STATE_BREAKPOINT,
                0,
                "missing breakpoint: {t:?}"
            );
        }
    }
}
