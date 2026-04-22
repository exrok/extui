//! JSON lexer.

use crate::kind as kinds;
use crate::lex::byteclass;
use crate::lex::kw::{self, kw_lookup};
use crate::lex::scan;
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{LexState, SourceView};

kw::kw_table! {
    static JSON_KWS = [
        (b"false", kinds::KEYWORD),
        (b"null", kinds::KEYWORD),
        (b"true", kinds::KEYWORD),
    ];
}

const MAX_KW_LEN: usize = 5;

pub(crate) struct Json;

impl Lexer for Json {
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
        b'{' => (kinds::OPEN_BRACE, cursor + 1, false),
        b'}' => (kinds::CLOSE_BRACE, cursor + 1, false),
        b'[' => (kinds::OPEN_BRACKET, cursor + 1, false),
        b']' => (kinds::CLOSE_BRACKET, cursor + 1, false),
        b':' => (kinds::COLON, cursor + 1, false),
        b',' => (kinds::COMMA, cursor + 1, false),
        b'"' => {
            let r = scan::scan_json_string(view, cursor);
            (kinds::STRING, r.end, r.is_error)
        }
        b'-' | b'0'..=b'9' => {
            let r = scan::scan_json_number(view, cursor);
            (kinds::NUMBER, r.end, r.is_error)
        }
        b if byteclass::is_ident_start(b) => {
            let end = scan::scan_ident_ascii(view, cursor);
            let len = (end - cursor) as usize;
            if len <= MAX_KW_LEN {
                let mut buf = [0u8; MAX_KW_LEN];
                if scan::copy_bytes(view, cursor, &mut buf[..len]) {
                    if let Some(k) = kw_lookup(JSON_KWS, &buf[..len]) {
                        return (k, end, false);
                    }
                }
            }
            (kinds::ERROR, end, true)
        }
        _ => (kinds::ERROR, cursor + 1, true),
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
            (cursor, state) = Json::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Eof => {
                        saw_eof = true;
                    }
                    LexStep::Descend { .. } => unreachable!("JSON has no embedding"),
                }
            }
            if saw_eof {
                return tokens;
            }
        }
    }

    #[test]
    fn simple_object() {
        let tokens = run(r#"{"a":1}"#);
        let kinds: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert_eq!(
            kinds,
            vec![
                kinds::OPEN_BRACE,  // {
                kinds::STRING,      // "a"
                kinds::COLON,       // :
                kinds::NUMBER,      // 1
                kinds::CLOSE_BRACE, // }
            ]
        );
    }

    #[test]
    fn keywords_and_whitespace() {
        let tokens = run("  true false null  ");
        let kinds: Vec<u16> = tokens.iter().map(|t| t.0).collect();
        assert_eq!(
            kinds,
            vec![
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
                kinds::KEYWORD,
                kinds::WHITESPACE,
            ]
        );
    }

    #[test]
    fn coverage_is_contiguous() {
        let input = r#"  {"x": [1, 2.5, true, null], "y": "hello"}  "#;
        let tokens = run(input);
        let mut pos = 0u32;
        for (_kind, offset, len, _flags) in &tokens {
            assert_eq!(*offset, pos, "gap or overlap before offset {pos}");
            pos += len;
        }
        assert_eq!(pos as usize, input.len());
    }

    #[test]
    fn error_on_unknown_byte() {
        let tokens = run("@");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::ERROR);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn error_on_unterminated_string() {
        let tokens = run(r#""abc"#);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::STRING);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }
}
