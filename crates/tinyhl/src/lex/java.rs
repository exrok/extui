//! Java lexer.
//!
//! Java shares the C-family lexer implementation in [`crate::lex::c`]. The
//! dialect switch adds Java keywords, XID identifiers, underscore numeric
//! separators, annotations, text blocks, and Java shift operators.

use crate::lex::c::{self, Dialect};
use crate::lex::{Lexer, StepBuf};
use crate::{LexState, SourceView};

pub(crate) struct Java;

impl Lexer for Java {
    fn step_batch(
        view: &mut SourceView<'_>,
        cursor: u32,
        state: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState) {
        c::step_batch_for(Dialect::Java, view, cursor, state, out)
    }

    fn is_safe_state(_state: LexState) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::kind as kinds;
    use crate::lex::LexStep;
    use crate::token::flags;
    use crate::{Source, SourceView};

    fn run(s: &str) -> Vec<(u16, u32, u32, u8)> {
        let src: &dyn Source = &s;
        let mut view = SourceView::new(src, 0);
        let mut out = StepBuf::new();
        let mut cursor = 0u32;
        let mut state = LexState::INITIAL;
        let mut tokens = Vec::new();
        loop {
            out.clear();
            let prev = cursor;
            (cursor, state) = Java::step_batch(&mut view, cursor, state, &mut out);
            let mut pos = prev;
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
                    LexStep::Descend { .. } => unreachable!("Java has no embedding"),
                }
            }
            if saw_eof {
                return tokens;
            }
        }
    }

    #[test]
    fn class_with_annotation_and_text_block() {
        let tokens = run("@Override class Demo { String s = \"\"\"hi\n\"\"\"; int n = 1_000; }");
        for kind in [
            kinds::AT,
            kinds::KEYWORD,
            kinds::IDENT,
            kinds::STRING,
            kinds::NUMBER,
            kinds::OPEN_BRACE,
            kinds::CLOSE_BRACE,
        ] {
            assert!(tokens.iter().any(|t| t.0 == kind), "missing kind {kind}");
        }
        assert!(tokens.iter().all(|t| t.3 & flags::IS_ERROR == 0));
    }

    #[test]
    fn unterminated_normal_string_stops_at_newline() {
        let tokens = run("String s = \"oops\nint n = 1;");
        let string = tokens.iter().find(|t| t.0 == kinds::STRING).unwrap();
        assert_eq!(string.2, 5);
        assert_ne!(string.3 & flags::IS_ERROR, 0);
    }
}
