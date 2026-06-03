//! INI lexer.
//!
//! State encoding: `mode: u3`, sharing the generic config lexer states:
//! line start, mid-line, after-space, and inside `[...]` section header.
//!
//! This covers the common INI shape used by Vim/Neovim's `dosini` syntax:
//! section headers, labels before `=`, numeric values, strings, and `#` / `;`
//! comments. It also accepts `:` as an assignment delimiter because that form
//! is common in INI-like files.
//!
//! [`STATE_BREAKPOINT`]: flags::STATE_BREAKPOINT

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::conf;
use crate::lex::scan;
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

pub(crate) struct Ini;

impl Lexer for Ini {
    fn step_batch(
        view: &mut SourceView<'_>,
        cursor: u32,
        state: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState) {
        let mut cursor = cursor;
        let mut state_id = conf::normalize_state(state);
        while !out.is_full() {
            let Some(b) = view.byte_at(cursor) else {
                out.push(LexStep::Eof);
                return (cursor, conf::st(state_id));
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
                state_out: conf::st(state_id),
                flags: bit_flags,
            });
            cursor = end;
        }
        (cursor, conf::st(state_id))
    }

    fn is_safe_state(state: LexState) -> bool {
        matches!(
            state.bits(),
            conf::ST_START | conf::ST_MID | conf::ST_AFTER_SPACE | conf::ST_SECTION_SQUARE
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
                conf::state_after_whitespace(view, cursor, end, state),
            );
        }
        b'#' | b';' if conf::is_comment_state(state) => {
            let end = scan::scan_line_to_end(view, cursor);
            return (kinds::COMMENT, end, false, conf::ST_MID);
        }
        b'"' | b'\'' => {
            let r = scan::scan_oneline_quoted(view, cursor, first);
            return (kinds::STRING, r.end, r.is_error, conf::ST_MID);
        }
        b'[' => {
            return (
                kinds::OPEN_BRACKET,
                cursor + 1,
                false,
                if state == conf::ST_START {
                    conf::ST_SECTION_SQUARE
                } else {
                    conf::ST_MID
                },
            );
        }
        b']' => return (kinds::CLOSE_BRACKET, cursor + 1, false, conf::ST_MID),
        b'(' => return (kinds::OPEN_PAREN, cursor + 1, false, conf::ST_MID),
        b')' => return (kinds::CLOSE_PAREN, cursor + 1, false, conf::ST_MID),
        _ => {}
    }

    if let Some(end) = conf::scan_section_name(view, cursor, state) {
        return (kinds::TAG_NAME, end, false, state);
    }

    if state == conf::ST_START {
        if let Some(end) = conf::scan_assignment_key(view, cursor, true, false) {
            return (kinds::ATTR_NAME, end, false, conf::ST_MID);
        }
    }

    if conf::starts_number(view, cursor, first) {
        let r = conf::scan_number(view, cursor);
        return (kinds::NUMBER, r.end, r.is_error, conf::ST_MID);
    }

    match first {
        b'=' => (kinds::EQ, cursor + 1, false, conf::ST_MID),
        b':' => (kinds::COLON, cursor + 1, false, conf::ST_MID),
        b',' => (kinds::COMMA, cursor + 1, false, conf::ST_MID),
        b';' => (kinds::SEMI, cursor + 1, false, conf::ST_MID),
        _ => {
            let end = conf::scan_plain(view, cursor, false, state);
            if end == cursor {
                (kinds::ERROR, cursor + 1, true, conf::ST_MID)
            } else {
                (
                    conf::plain_kind(view, cursor, end),
                    end,
                    false,
                    conf::ST_MID,
                )
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
            let before = cursor;
            (cursor, state) = Ini::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("Ini has no embedding"),
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
    fn highlights_sections_keys_and_values() {
        let kinds = kinds_of(
            "; top\n[database.replica]\nhost = \"db.local\"\nport=5432\nenabled: yes\nratio = -1.5e+2\n",
        );
        for required in [
            kinds::COMMENT,
            kinds::OPEN_BRACKET,
            kinds::TAG_NAME,
            kinds::CLOSE_BRACKET,
            kinds::ATTR_NAME,
            kinds::EQ,
            kinds::COLON,
            kinds::STRING,
            kinds::NUMBER,
            kinds::KEYWORD,
        ] {
            assert!(kinds.contains(&required), "missing kind {required}");
        }
    }

    #[test]
    fn keeps_hash_inside_values_plain() {
        let tokens = run("secret=abc#123\n");
        assert!(!tokens.iter().any(|t| t.0 == kinds::COMMENT));
    }

    #[test]
    fn all_tokens_are_breakpoints() {
        for (_, _, _, token_flags) in run("[sec]\na = true\n# c\n") {
            assert_ne!(
                token_flags & flags::STATE_BREAKPOINT,
                0,
                "missing breakpoint"
            );
        }
    }
}
