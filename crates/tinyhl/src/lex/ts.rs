//! TypeScript lexer.

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan::{self, ScanResult};
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

/// Set in [`LexState`] when the previous token ends a primary expression
/// (a value), so a following `/` is division and a `<` is a comparison rather
/// than the start of a regex or JSX element. Shared with the [`Tsx`] lexer.
///
/// [`Tsx`]: crate::lex::tsx::Tsx
pub(crate) const STATE_EXPR_END: u16 = 1 << 0;

kw::kw_table! {
    static TS_KWS = [
        (b"abstract", kinds::KEYWORD),
        (b"any", kinds::KEYWORD),
        (b"as", kinds::KEYWORD),
        (b"asserts", kinds::KEYWORD),
        (b"async", kinds::KEYWORD),
        (b"await", kinds::KEYWORD),
        (b"boolean", kinds::KEYWORD),
        (b"break", kinds::KEYWORD),
        (b"case", kinds::KEYWORD),
        (b"catch", kinds::KEYWORD),
        (b"class", kinds::KEYWORD),
        (b"const", kinds::KEYWORD),
        (b"continue", kinds::KEYWORD),
        (b"debugger", kinds::KEYWORD),
        (b"declare", kinds::KEYWORD),
        (b"default", kinds::KEYWORD),
        (b"delete", kinds::KEYWORD),
        (b"do", kinds::KEYWORD),
        (b"else", kinds::KEYWORD),
        (b"enum", kinds::KEYWORD),
        (b"export", kinds::KEYWORD),
        (b"extends", kinds::KEYWORD),
        (b"false", kinds::KEYWORD),
        (b"finally", kinds::KEYWORD),
        (b"for", kinds::KEYWORD),
        (b"from", kinds::KEYWORD),
        (b"function", kinds::KEYWORD),
        (b"get", kinds::KEYWORD),
        (b"if", kinds::KEYWORD),
        (b"implements", kinds::KEYWORD),
        (b"import", kinds::KEYWORD),
        (b"in", kinds::KEYWORD),
        (b"infer", kinds::KEYWORD),
        (b"instanceof", kinds::KEYWORD),
        (b"interface", kinds::KEYWORD),
        (b"is", kinds::KEYWORD),
        (b"keyof", kinds::KEYWORD),
        (b"let", kinds::KEYWORD),
        (b"module", kinds::KEYWORD),
        (b"namespace", kinds::KEYWORD),
        (b"never", kinds::KEYWORD),
        (b"new", kinds::KEYWORD),
        (b"null", kinds::KEYWORD),
        (b"number", kinds::KEYWORD),
        (b"of", kinds::KEYWORD),
        (b"package", kinds::KEYWORD),
        (b"private", kinds::KEYWORD),
        (b"protected", kinds::KEYWORD),
        (b"public", kinds::KEYWORD),
        (b"readonly", kinds::KEYWORD),
        (b"require", kinds::KEYWORD),
        (b"return", kinds::KEYWORD),
        (b"satisfies", kinds::KEYWORD),
        (b"set", kinds::KEYWORD),
        (b"static", kinds::KEYWORD),
        (b"string", kinds::KEYWORD),
        (b"super", kinds::KEYWORD),
        (b"switch", kinds::KEYWORD),
        (b"symbol", kinds::KEYWORD),
        (b"this", kinds::KEYWORD),
        (b"throw", kinds::KEYWORD),
        (b"true", kinds::KEYWORD),
        (b"try", kinds::KEYWORD),
        (b"type", kinds::KEYWORD),
        (b"typeof", kinds::KEYWORD),
        (b"undefined", kinds::KEYWORD),
        (b"unique", kinds::KEYWORD),
        (b"unknown", kinds::KEYWORD),
        (b"var", kinds::KEYWORD),
        (b"void", kinds::KEYWORD),
        (b"while", kinds::KEYWORD),
        (b"with", kinds::KEYWORD),
        (b"yield", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 10; // "instanceof", "implements"

fn is_value_keyword(bytes: &[u8]) -> bool {
    matches!(bytes, b"this" | b"true" | b"false" | b"null" | b"super")
}

/// TypeScript lexer.
pub(crate) struct Ts;

impl Lexer for Ts {
    fn step_batch(
        view: &mut SourceView<'_>,
        cursor: u32,
        state: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState) {
        let mut cursor = cursor;
        let mut state = state;
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

pub(crate) fn classify(
    view: &mut SourceView<'_>,
    cursor: u32,
    first: u8,
    state: LexState,
) -> (u16, u32, bool, LexState) {
    let expr_end = (state.bits() & STATE_EXPR_END) != 0;
    match first {
        b if byteclass::is_whitespace(b) => {
            let end = scan::scan_whitespace(view, cursor);
            (kinds::WHITESPACE, end, false, state)
        }
        b'/' => match view.byte_at(cursor + 1) {
            Some(b'/') => {
                let end = scan::scan_line_comment(view, cursor);
                (kinds::COMMENT, end, false, state)
            }
            Some(b'*') => {
                let r = scan::scan_block_comment(view, cursor);
                (kinds::COMMENT, r.end, r.is_error, state)
            }
            Some(b'=') if expr_end => (kinds::SLASH_EQ, cursor + 2, false, LexState::INITIAL),
            _ if expr_end => (kinds::SLASH, cursor + 1, false, LexState::INITIAL),
            _ => {
                let r = scan_regex(view, cursor);
                (kinds::REGEX, r.end, r.is_error, LexState(STATE_EXPR_END))
            }
        },
        b'"' | b'\'' => {
            let r = scan_string(view, cursor, first);
            (kinds::STRING, r.end, r.is_error, LexState(STATE_EXPR_END))
        }
        b'`' => {
            let r = scan_template(view, cursor);
            (
                kinds::TEMPLATE_STRING,
                r.end,
                r.is_error,
                LexState(STATE_EXPR_END),
            )
        }
        b'0'..=b'9' => {
            let r = scan_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error, LexState(STATE_EXPR_END))
        }
        b'.' => match view.byte_at(cursor + 1) {
            Some(b) if byteclass::is_digit(b) => {
                let r = scan_number(view, cursor);
                (kinds::NUMBER, r.end, r.is_error, LexState(STATE_EXPR_END))
            }
            Some(b'.') if view.byte_at(cursor + 2) == Some(b'.') => {
                (kinds::ELLIPSIS, cursor + 3, false, LexState::INITIAL)
            }
            _ => (kinds::DOT, cursor + 1, false, LexState::INITIAL),
        },
        b'#' => classify_hash(view, cursor),
        b if byteclass::is_ident_start(b) || b == b'$' => classify_ident(view, cursor),
        b'{' | b'}' | b'(' | b')' | b'[' | b']' | b',' | b';' | b':' | b'?' | b'@' | b'~'
        | b'=' | b'!' | b'<' | b'>' | b'+' | b'-' | b'*' | b'%' | b'&' | b'|' | b'^' => {
            let (kind, end) = scan_operator(view, cursor, first);
            (kind, end, false, state_out_for_punct(kind))
        }
        b if b >= 0x80 => classify_unicode(view, cursor),
        _ => (kinds::ERROR, cursor + 1, true, LexState::INITIAL),
    }
}

fn state_out_for_punct(kind: u16) -> LexState {
    if matches!(
        kind,
        kinds::CLOSE_PAREN
            | kinds::CLOSE_BRACKET
            | kinds::CLOSE_BRACE
            | kinds::PLUS_PLUS
            | kinds::MINUS_MINUS
    ) {
        LexState(STATE_EXPR_END)
    } else {
        LexState::INITIAL
    }
}

fn classify_ident(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool, LexState) {
    let end = scan::scan_xid_ident(view, cursor, true);
    let len = (end - cursor) as usize;
    if len <= MAX_KW_LEN {
        let mut buf = [0u8; MAX_KW_LEN];
        if scan::copy_bytes(view, cursor, &mut buf[..len]) {
            let bytes = &buf[..len];
            if let Some(kind) = kw_lookup(TS_KWS, bytes) {
                let state_out = if is_value_keyword(bytes) {
                    LexState(STATE_EXPR_END)
                } else {
                    LexState::INITIAL
                };
                return (kind, end, false, state_out);
            }
        }
    }
    (kinds::IDENT, end, false, LexState(STATE_EXPR_END))
}

fn classify_hash(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool, LexState) {
    // `#ident` — ECMAScript private class member. Scanned as a single IDENT
    // token including the leading `#`. Bare `#` followed by a non-ident byte
    // is an error.
    let body = scan::scan_xid_ident(view, cursor + 1, true);
    if body > cursor + 1 {
        (kinds::IDENT, body, false, LexState(STATE_EXPR_END))
    } else {
        (kinds::ERROR, cursor + 1, true, LexState::INITIAL)
    }
}

fn classify_unicode(view: &mut SourceView<'_>, cursor: u32) -> (u16, u32, bool, LexState) {
    // Non-ASCII byte: either a Unicode XID identifier or an uncategorizable
    // byte. Keyword tables are ASCII-only, so Unicode idents always resolve
    // to IDENT — no lookup needed.
    let end = scan::scan_xid_ident(view, cursor, true);
    if end > cursor {
        return (kinds::IDENT, end, false, LexState(STATE_EXPR_END));
    }
    // Not a valid XID_Start — consume the full UTF-8 char so the lexer makes
    // forward progress without leaving a stranded continuation byte.
    let len = scan::decode_char_at(view, cursor)
        .map(|(_, n)| n)
        .unwrap_or(1);
    (kinds::ERROR, cursor + len, true, LexState::INITIAL)
}

fn scan_operator(view: &mut SourceView<'_>, cursor: u32, b0: u8) -> (u16, u32) {
    let b1 = view.byte_at(cursor + 1);
    let b2 = view.byte_at(cursor + 2);
    let b3 = view.byte_at(cursor + 3);

    if let (b'>', Some(b'>'), Some(b'>'), Some(b'=')) = (b0, b1, b2, b3) {
        return (kinds::USHR_EQ, cursor + 4);
    }

    let three = match (b0, b1, b2) {
        (b'=', Some(b'='), Some(b'=')) => Some(kinds::EQ_EQ_EQ),
        (b'!', Some(b'='), Some(b'=')) => Some(kinds::BANG_EQ_EQ),
        (b'>', Some(b'>'), Some(b'>')) => Some(kinds::USHR),
        (b'<', Some(b'<'), Some(b'=')) => Some(kinds::SHL_EQ),
        (b'>', Some(b'>'), Some(b'=')) => Some(kinds::SHR_EQ),
        (b'*', Some(b'*'), Some(b'=')) => Some(kinds::STAR_STAR_EQ),
        (b'&', Some(b'&'), Some(b'=')) => Some(kinds::AMP_AMP_EQ),
        (b'|', Some(b'|'), Some(b'=')) => Some(kinds::PIPE_PIPE_EQ),
        (b'?', Some(b'?'), Some(b'=')) => Some(kinds::QUESTION_QUESTION_EQ),
        _ => None,
    };
    if let Some(k) = three {
        return (k, cursor + 3);
    }

    if let Some(b1) = b1 {
        let two = match (b0, b1) {
            (b'=', b'=') => Some(kinds::EQ_EQ),
            (b'!', b'=') => Some(kinds::BANG_EQ),
            (b'<', b'=') => Some(kinds::LT_EQ),
            (b'>', b'=') => Some(kinds::GT_EQ),
            (b'<', b'<') => Some(kinds::SHL),
            (b'>', b'>') => Some(kinds::SHR),
            (b'&', b'&') => Some(kinds::AMP_AMP),
            (b'|', b'|') => Some(kinds::PIPE_PIPE),
            (b'?', b'?') => Some(kinds::QUESTION_QUESTION),
            (b'?', b'.') => Some(kinds::OPTIONAL_CHAIN),
            (b'=', b'>') => Some(kinds::FAT_ARROW),
            (b'+', b'+') => Some(kinds::PLUS_PLUS),
            (b'-', b'-') => Some(kinds::MINUS_MINUS),
            (b'*', b'*') => Some(kinds::STAR_STAR),
            (b'+', b'=') => Some(kinds::PLUS_EQ),
            (b'-', b'=') => Some(kinds::MINUS_EQ),
            (b'*', b'=') => Some(kinds::STAR_EQ),
            (b'%', b'=') => Some(kinds::PERCENT_EQ),
            (b'&', b'=') => Some(kinds::AMP_EQ),
            (b'|', b'=') => Some(kinds::PIPE_EQ),
            (b'^', b'=') => Some(kinds::CARET_EQ),
            _ => None,
        };
        if let Some(k) = two {
            return (k, cursor + 2);
        }
    }

    let one = match b0 {
        b'{' => kinds::OPEN_BRACE,
        b'}' => kinds::CLOSE_BRACE,
        b'(' => kinds::OPEN_PAREN,
        b')' => kinds::CLOSE_PAREN,
        b'[' => kinds::OPEN_BRACKET,
        b']' => kinds::CLOSE_BRACKET,
        b',' => kinds::COMMA,
        b';' => kinds::SEMI,
        b':' => kinds::COLON,
        b'?' => kinds::QUESTION,
        b'@' => kinds::AT,
        b'~' => kinds::TILDE,
        b'=' => kinds::EQ,
        b'!' => kinds::BANG,
        b'<' => kinds::LT,
        b'>' => kinds::GT,
        b'+' => kinds::PLUS,
        b'-' => kinds::MINUS,
        b'*' => kinds::STAR,
        b'%' => kinds::PERCENT,
        b'&' => kinds::AMP,
        b'|' => kinds::PIPE,
        b'^' => kinds::CARET,
        _ => kinds::ERROR,
    };
    (one, cursor + 1)
}

/// Scans a single- or double-quoted string literal. Honors `\`-escape for the
/// next byte (with `\r\n` treated as a single escaped line terminator) and
/// ends on the matching quote. A bare newline or end-of-source before the
/// closing quote terminates the scan with an error.
pub(crate) fn scan_string(view: &mut SourceView<'_>, cursor: u32, quote: u8) -> ScanResult {
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
            Some(b'\\') => {
                end += 1;
                match view.byte_at(end) {
                    None => {
                        return ScanResult {
                            end,
                            is_error: true,
                        };
                    }
                    Some(b'\r') => {
                        end += 1;
                        if view.byte_at(end) == Some(b'\n') {
                            end += 1;
                        }
                    }
                    Some(_) => end += 1,
                }
            }
            Some(b'\n') | Some(b'\r') => {
                return ScanResult {
                    end,
                    is_error: true,
                };
            }
            Some(_) => end += 1,
        }
    }
}

/// Scans a regular expression literal `/<body>/<flags>`, consuming the
/// opening and closing slashes and any trailing ASCII identifier flag run.
/// Inside a `[…]` character class, `/` is a literal; outside, `/` ends the
/// body. Backslash escapes the next byte. A bare newline or end-of-source
/// before the closing slash terminates the scan with an error.
fn scan_regex(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let mut end = cursor + 1;
    let mut in_class = false;
    loop {
        match view.byte_at(end) {
            None => {
                return ScanResult {
                    end,
                    is_error: true,
                };
            }
            Some(b'\n') | Some(b'\r') => {
                return ScanResult {
                    end,
                    is_error: true,
                };
            }
            Some(b'\\') => {
                end += 1;
                match view.byte_at(end) {
                    None => {
                        return ScanResult {
                            end,
                            is_error: true,
                        };
                    }
                    Some(b'\n') | Some(b'\r') => {
                        return ScanResult {
                            end,
                            is_error: true,
                        };
                    }
                    Some(_) => end += 1,
                }
            }
            Some(b'[') => {
                in_class = true;
                end += 1;
            }
            Some(b']') if in_class => {
                in_class = false;
                end += 1;
            }
            Some(b'/') if !in_class => {
                end += 1;
                while let Some(b) = view.byte_at(end) {
                    if byteclass::is_ident_cont(b) {
                        end += 1;
                    } else {
                        break;
                    }
                }
                return ScanResult {
                    end,
                    is_error: false,
                };
            }
            Some(_) => end += 1,
        }
    }
}

/// Scans a template literal starting at the opening backtick.
///
/// Handles `\` escapes and `${…}` substitutions by matching braces. Inside a
/// substitution the scanner recognises nested strings, templates, and
/// comments so that their contents (which may contain `{`/`}` or backticks)
/// do not prematurely terminate the outer template.
pub(crate) fn scan_template(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    // `depths` tracks brace depth for each nested `${…}` expression currently
    // open. Empty ⇒ we are in the literal's body. Max nesting of 15 is
    // sufficient for any realistic template and keeps the scanner non-
    // allocating; deeper nesting folds into the template as an error.
    const MAX_DEPTH: usize = 15;
    let mut depths = [0u32; MAX_DEPTH];
    let mut sp: usize = 0;
    let mut end = cursor + 1;
    loop {
        if sp == 0 {
            match view.byte_at(end) {
                None => {
                    return ScanResult {
                        end,
                        is_error: true,
                    };
                }
                Some(b'`') => {
                    return ScanResult {
                        end: end + 1,
                        is_error: false,
                    };
                }
                Some(b'\\') => {
                    end += 1;
                    if view.byte_at(end).is_some() {
                        end += 1;
                    }
                }
                Some(b'$') if view.byte_at(end + 1) == Some(b'{') => {
                    end += 2;
                    if sp >= MAX_DEPTH {
                        return ScanResult {
                            end,
                            is_error: true,
                        };
                    }
                    depths[sp] = 1;
                    sp += 1;
                }
                Some(_) => end += 1,
            }
        } else {
            match view.byte_at(end) {
                None => {
                    return ScanResult {
                        end,
                        is_error: true,
                    };
                }
                Some(b'{') => {
                    depths[sp - 1] += 1;
                    end += 1;
                }
                Some(b'}') => {
                    depths[sp - 1] -= 1;
                    end += 1;
                    if depths[sp - 1] == 0 {
                        sp -= 1;
                    }
                }
                Some(b'\'') => {
                    let r = scan_string(view, end, b'\'');
                    end = r.end;
                }
                Some(b'"') => {
                    let r = scan_string(view, end, b'"');
                    end = r.end;
                }
                Some(b'`') => {
                    let r = scan_template(view, end);
                    end = r.end;
                    if r.is_error {
                        return ScanResult {
                            end,
                            is_error: true,
                        };
                    }
                }
                Some(b'/') => match view.byte_at(end + 1) {
                    Some(b'/') => {
                        end = scan::scan_line_comment(view, end);
                    }
                    Some(b'*') => {
                        let r = scan::scan_block_comment(view, end);
                        end = r.end;
                        if r.is_error {
                            return ScanResult {
                                end,
                                is_error: true,
                            };
                        }
                    }
                    _ => end += 1,
                },
                Some(_) => end += 1,
            }
        }
    }
}

/// Scans a numeric literal, including the `0x` / `0b` / `0o` radix prefixes,
/// decimal and exponent forms, numeric separators (`_`), and an optional
/// trailing BigInt `n` suffix. The caller must dispatch here only when the
/// first byte is an ASCII digit or a `.` followed by an ASCII digit.
fn scan_number(view: &mut SourceView<'_>, cursor: u32) -> ScanResult {
    let first = view.byte_at(cursor).unwrap();

    if first == b'0' {
        match view.byte_at(cursor + 1) {
            Some(b'x') | Some(b'X') => {
                let after = cursor + 2;
                let end = scan_digit_run(view, after, is_hex_or_sep);
                if end == after {
                    return ScanResult {
                        end,
                        is_error: true,
                    };
                }
                return ScanResult {
                    end: eat_bigint(view, end),
                    is_error: false,
                };
            }
            Some(b'b') | Some(b'B') => {
                let after = cursor + 2;
                let end = scan_digit_run(view, after, is_binary_or_sep);
                if end == after {
                    return ScanResult {
                        end,
                        is_error: true,
                    };
                }
                return ScanResult {
                    end: eat_bigint(view, end),
                    is_error: false,
                };
            }
            Some(b'o') | Some(b'O') => {
                let after = cursor + 2;
                let end = scan_digit_run(view, after, is_octal_or_sep);
                if end == after {
                    return ScanResult {
                        end,
                        is_error: true,
                    };
                }
                return ScanResult {
                    end: eat_bigint(view, end),
                    is_error: false,
                };
            }
            _ => {}
        }
    }

    let mut end = scan_digit_run(view, cursor, is_decimal_or_sep);
    let mut is_float = false;
    let mut saw_error = false;

    if view.byte_at(end) == Some(b'.') {
        let next = view.byte_at(end + 1);
        if end == cursor || matches!(next, Some(b) if byteclass::is_digit(b)) || next.is_none() {
            end += 1;
            is_float = true;
            end = scan_digit_run(view, end, is_decimal_or_sep);
        }
    }

    if matches!(view.byte_at(end), Some(b'e') | Some(b'E')) {
        end += 1;
        is_float = true;
        if matches!(view.byte_at(end), Some(b'+') | Some(b'-')) {
            end += 1;
        }
        let before = end;
        end = scan_digit_run(view, end, is_decimal_or_sep);
        if end == before {
            saw_error = true;
        }
    }

    if !is_float && view.byte_at(end) == Some(b'n') {
        end += 1;
    }

    ScanResult {
        end,
        is_error: saw_error,
    }
}

fn eat_bigint(view: &mut SourceView<'_>, cursor: u32) -> u32 {
    if view.byte_at(cursor) == Some(b'n') {
        cursor + 1
    } else {
        cursor
    }
}

fn is_decimal_or_sep(b: u8) -> bool {
    byteclass::is_digit(b) || b == b'_'
}

fn is_hex_or_sep(b: u8) -> bool {
    byteclass::is_hex_digit(b) || b == b'_'
}

fn is_binary_or_sep(b: u8) -> bool {
    b == b'0' || b == b'1' || b == b'_'
}

fn is_octal_or_sep(b: u8) -> bool {
    matches!(b, b'0'..=b'7') || b == b'_'
}

fn scan_digit_run(view: &mut SourceView<'_>, cursor: u32, accept: fn(u8) -> bool) -> u32 {
    let mut cursor = cursor;
    loop {
        let (base, page) = view.window_at(cursor);
        if page.is_empty() {
            return cursor;
        }
        let start = (cursor - base) as usize;
        let mut i = start;
        while i < page.len() && accept(page[i]) {
            i += 1;
        }
        cursor += (i - start) as u32;
        if i < page.len() {
            return cursor;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Source;

    fn run(s: &str) -> Vec<(u16, u32, u32, u8, u16)> {
        let src: &dyn Source = &s;
        let mut view = SourceView::new(src, 0);
        let mut out = StepBuf::new();
        let mut cursor = 0u32;
        let mut state = LexState::INITIAL;
        let mut tokens = Vec::new();
        loop {
            out.clear();
            let before = cursor;
            (cursor, state) = Ts::step_batch(&mut view, cursor, state, &mut out);
            let mut pos = before;
            let mut saw_eof = false;
            for step in out.as_slice() {
                match *step {
                    LexStep::Token {
                        len,
                        local_kind,
                        flags,
                        state_out,
                    } => {
                        tokens.push((local_kind, pos, len, flags, state_out.bits()));
                        pos += len;
                    }
                    LexStep::Eof => saw_eof = true,
                    LexStep::Descend { .. } => unreachable!("TS has no embedding"),
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
        let input = "const x: number = 42; // ok\n";
        let tokens = run(input);
        let mut pos = 0u32;
        for (_k, off, len, _f, _s) in &tokens {
            assert_eq!(*off, pos, "gap before {pos}");
            pos += len;
        }
        assert_eq!(pos as usize, input.len());
    }

    #[test]
    fn keyword_vs_ident() {
        assert_eq!(kinds_of("const"), vec![kinds::KEYWORD]);
        assert_eq!(kinds_of("consts"), vec![kinds::IDENT]);
        assert_eq!(kinds_of("type"), vec![kinds::KEYWORD]);
        assert_eq!(kinds_of("Type"), vec![kinds::IDENT]);
        assert_eq!(kinds_of("undefined"), vec![kinds::KEYWORD]);
    }

    #[test]
    fn dollar_and_private_idents() {
        assert_eq!(kinds_of("$foo"), vec![kinds::IDENT]);
        assert_eq!(kinds_of("_bar"), vec![kinds::IDENT]);
        assert_eq!(kinds_of("a$b"), vec![kinds::IDENT]);
        assert_eq!(kinds_of("#priv"), vec![kinds::IDENT]);
    }

    #[test]
    fn line_and_block_comments() {
        assert_eq!(kinds_of("// hello"), vec![kinds::COMMENT]);
        assert_eq!(kinds_of("/* hi */"), vec![kinds::COMMENT]);
        let t = run("/* unterminated");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::COMMENT);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn strings_basic() {
        for input in [r#""hi""#, r#"'hi'"#, r#""a\"b""#, r#"'a\'b'"#, r#""\n""#] {
            let t = run(input);
            assert_eq!(t.len(), 1, "{input:?}");
            assert_eq!(t[0].0, kinds::STRING, "{input:?}");
            assert_eq!(t[0].3 & flags::IS_ERROR, 0, "{input:?}");
        }
    }

    #[test]
    fn string_line_continuation() {
        let s = "\"a\\\nb\"";
        let t = run(s);
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::STRING);
        assert_eq!(t[0].2 as usize, s.len());
    }

    #[test]
    fn unterminated_string_errors() {
        let t = run("\"oops\n");
        assert_eq!(t[0].0, kinds::STRING);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn templates_basic() {
        for input in [
            "``",
            "`hello`",
            "`a\\`b`",
            "`line 1\nline 2`",
            "`${x}`",
            "`pre ${a + b} post`",
        ] {
            let t = run(input);
            assert_eq!(t.len(), 1, "{input:?}");
            assert_eq!(t[0].0, kinds::TEMPLATE_STRING, "{input:?}");
            assert_eq!(t[0].2 as usize, input.len(), "{input:?}");
            assert_eq!(t[0].3 & flags::IS_ERROR, 0, "{input:?}");
        }
    }

    #[test]
    fn template_with_braces_in_string_inside_expr() {
        // The `}` inside the single-quoted string must not close the `${…}`.
        let input = "`${'}'}`";
        let t = run(input);
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::TEMPLATE_STRING);
        assert_eq!(t[0].2 as usize, input.len());
    }

    #[test]
    fn nested_template_literal() {
        let input = "`${`inner ${42}`}`";
        let t = run(input);
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::TEMPLATE_STRING);
        assert_eq!(t[0].2 as usize, input.len());
    }

    #[test]
    fn numbers_basic() {
        for input in [
            "0",
            "42",
            "3.14",
            "1e10",
            "1.5e-3",
            "0xFF",
            "0b1010",
            "0o755",
            "1_000_000",
            "0xDEAD_BEEF",
            "42n",
            "0xFFn",
        ] {
            let t = run(input);
            assert_eq!(t.len(), 1, "{input:?}");
            assert_eq!(t[0].0, kinds::NUMBER, "{input:?}");
            assert_eq!(t[0].2 as usize, input.len(), "{input:?}");
            assert_eq!(t[0].3 & flags::IS_ERROR, 0, "{input:?}");
        }
    }

    #[test]
    fn leading_dot_float() {
        let t = run(".5");
        assert_eq!(t.len(), 1);
        assert_eq!(t[0].0, kinds::NUMBER);
    }

    #[test]
    fn bad_hex_flags_error() {
        let t = run("0x");
        assert_eq!(t[0].0, kinds::NUMBER);
        assert_ne!(t[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn regex_after_assignment() {
        let tokens = run("const r = /ab+c/gi;");
        let kinds_seen: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert!(kinds_seen.contains(&kinds::REGEX));
    }

    #[test]
    fn division_after_identifier() {
        let tokens = run("a / b");
        let kinds_seen: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert!(kinds_seen.contains(&kinds::SLASH));
        assert!(!kinds_seen.contains(&kinds::REGEX));
    }

    #[test]
    fn regex_after_return() {
        let tokens = run("return /x/;");
        let kinds_seen: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert!(kinds_seen.contains(&kinds::REGEX));
    }

    #[test]
    fn division_after_number() {
        let tokens = run("1 / 2");
        let kinds_seen: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert!(kinds_seen.contains(&kinds::SLASH));
    }

    #[test]
    fn division_after_close_paren() {
        let tokens = run("(x) / 2");
        let kinds_seen: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert!(kinds_seen.contains(&kinds::SLASH));
    }

    #[test]
    fn regex_with_char_class_containing_slash() {
        let tokens = run("= /[/]/");
        let kinds_seen: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert!(kinds_seen.contains(&kinds::REGEX));
    }

    #[test]
    fn unterminated_regex_is_error() {
        let t = run("= /abc\n");
        assert!(
            t.iter()
                .any(|t| t.0 == kinds::REGEX && t.3 & flags::IS_ERROR != 0)
        );
    }

    #[test]
    fn slash_equal_division() {
        let t = run("a /= 2");
        let kinds_seen: Vec<u16> = t.iter().map(|t| t.0).collect();
        assert!(kinds_seen.contains(&kinds::SLASH_EQ));
    }

    #[test]
    fn operators_greedy() {
        let src = "a >>>= b === c !== d ?? e ?. f => g";
        let tokens = run(src);
        let kinds_seen: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert!(kinds_seen.contains(&kinds::USHR_EQ));
        assert!(kinds_seen.contains(&kinds::EQ_EQ_EQ));
        assert!(kinds_seen.contains(&kinds::BANG_EQ_EQ));
        assert!(kinds_seen.contains(&kinds::QUESTION_QUESTION));
        assert!(kinds_seen.contains(&kinds::OPTIONAL_CHAIN));
        assert!(kinds_seen.contains(&kinds::FAT_ARROW));
    }

    #[test]
    fn spread_vs_dot() {
        let tokens = run("...x . y");
        let kinds_seen: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert!(kinds_seen.contains(&kinds::ELLIPSIS));
        assert!(kinds_seen.contains(&kinds::DOT));
    }

    #[test]
    fn error_on_unknown_byte() {
        let t = run("`ok` \u{00A3}");
        assert!(
            t.iter()
                .any(|t| t.0 == kinds::ERROR && t.3 & flags::IS_ERROR != 0)
        );
    }

    #[test]
    fn unicode_identifier() {
        // αβγ is all Unicode Ll (XID_Start/Continue) so should tokenize as one IDENT.
        let t = run("const αβγ = 1;");
        let seen: Vec<u16> = t.iter().map(|t| t.0).collect();
        assert!(seen.contains(&kinds::IDENT));
        let ident = t.iter().find(|t| t.0 == kinds::IDENT).unwrap();
        assert_eq!(ident.2 as usize, "αβγ".len());
    }

    #[test]
    fn unicode_identifier_at_start() {
        // Starting the input with a Unicode ident still classifies correctly.
        let t = run("日本語 + 1");
        assert_eq!(t[0].0, kinds::IDENT);
        assert_eq!(t[0].2 as usize, "日本語".len());
    }

    #[test]
    fn every_token_has_state_breakpoint() {
        let input = "const f = (x: number) => `v=${x}` + /ab/g; // done\n";
        let tokens = run(input);
        for t in &tokens {
            assert_ne!(t.3 & flags::STATE_BREAKPOINT, 0, "missing STATE_BREAKPOINT");
        }
    }
}
