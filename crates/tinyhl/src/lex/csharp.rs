//! C# lexer.
//!
//! C# shares the C-family lexer implementation in [`crate::lex::c`]. The
//! dialect switch adds C# keywords, XID identifiers, verbatim/interpolated
//! strings, raw string literals, underscore numeric separators, and C#-only
//! operators.

use crate::lex::c::{self, Dialect};
use crate::lex::{Lexer, StepBuf};
use crate::{LexState, SourceView};

pub(crate) struct Csharp;

impl Lexer for Csharp {
    fn step_batch(
        view: &mut SourceView<'_>,
        cursor: u32,
        state: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState) {
        c::step_batch_for(Dialect::Csharp, view, cursor, state, out)
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
            (cursor, state) = Csharp::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("C# has no embedding"),
                }
            }
            if saw_eof {
                return tokens;
            }
        }
    }

    #[test]
    fn record_with_strings_and_nullish_ops() {
        let tokens = run(
            "public record Demo(string Name) { string s = $@\"hi \"\"{Name}\"\"\"; var n = a?.B ?? 1_000; }",
        );
        for kind in [
            kinds::KEYWORD,
            kinds::IDENT,
            kinds::STRING,
            kinds::NUMBER,
            kinds::OPEN_BRACE,
            kinds::CLOSE_BRACE,
            kinds::OPTIONAL_CHAIN,
            kinds::QUESTION_QUESTION,
        ] {
            assert!(tokens.iter().any(|t| t.0 == kind), "missing kind {kind}");
        }
        assert!(tokens.iter().all(|t| t.3 & flags::IS_ERROR == 0));
    }

    #[test]
    fn at_identifier_is_not_keyword() {
        let tokens = run("@class class C {}");
        assert_eq!(tokens[0].0, kinds::IDENT);
        assert!(tokens.iter().any(|t| t.0 == kinds::KEYWORD));
    }
}
