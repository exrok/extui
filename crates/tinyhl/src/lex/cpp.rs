//! C++ lexer.
//!
//! C++ shares the C-family lexer implementation in [`crate::lex::c`]. The
//! dialect switch adds C++ keywords, raw strings, digit separators, UTF-8
//! character literals, user-defined literal suffixes, and C++-only operators.

use crate::lex::c::{self, Dialect};
use crate::lex::{Lexer, StepBuf};
use crate::{LexState, SourceView};

pub(crate) struct Cpp;

impl Lexer for Cpp {
    fn step_batch(
        view: &mut SourceView<'_>,
        cursor: u32,
        state: LexState,
        out: &mut StepBuf,
    ) -> (u32, LexState) {
        c::step_batch_for(Dialect::Cpp, view, cursor, state, out)
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
            (cursor, state) = Cpp::step_batch(&mut view, cursor, state, &mut out);
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
                    LexStep::Descend { .. } => unreachable!("C++ has no embedding"),
                }
            }
            if saw_eof {
                return tokens;
            }
        }
    }

    fn kinds_of(tokens: &[(u16, u32, u32, u8)]) -> Vec<u16> {
        tokens.iter().map(|t| t.0).collect()
    }

    #[test]
    fn simple_class() {
        let tokens = run("class Widget final { public: auto size() const -> int; };");
        assert_eq!(
            kinds_of(&tokens),
            vec![
                kinds::KEYWORD,     // class
                kinds::WHITESPACE,  //
                kinds::IDENT,       // Widget
                kinds::WHITESPACE,  //
                kinds::KEYWORD,     // final
                kinds::WHITESPACE,  //
                kinds::OPEN_BRACE,  // {
                kinds::WHITESPACE,  //
                kinds::KEYWORD,     // public
                kinds::COLON,       // :
                kinds::WHITESPACE,  //
                kinds::KEYWORD,     // auto
                kinds::WHITESPACE,  //
                kinds::IDENT,       // size
                kinds::OPEN_PAREN,  // (
                kinds::CLOSE_PAREN, // )
                kinds::WHITESPACE,  //
                kinds::KEYWORD,     // const
                kinds::WHITESPACE,  //
                kinds::THIN_ARROW,  // ->
                kinds::WHITESPACE,  //
                kinds::KEYWORD,     // int
                kinds::SEMI,        // ;
                kinds::WHITESPACE,  //
                kinds::CLOSE_BRACE, // }
                kinds::SEMI,        // ;
            ]
        );
    }

    #[test]
    fn selected_cpp_keywords() {
        for kw in [
            "alignas",
            "char8_t",
            "co_await",
            "concept",
            "consteval",
            "module",
            "namespace",
            "noexcept",
            "nullptr",
            "reinterpret_cast",
            "requires",
            "thread_local",
            "typename",
            "xor_eq",
        ] {
            let tokens = run(kw);
            assert_eq!(tokens.len(), 1, "{kw:?}");
            assert_eq!(tokens[0].0, kinds::KEYWORD, "{kw:?} should be KEYWORD");
        }
    }

    #[test]
    fn raw_strings_and_literal_suffixes() {
        for input in [
            r#"R"(quote " and /* comment */)""#,
            r#"u8R"tag(line
)tag"_sv"#,
            r#""plain"_name"#,
            r#"u8'x'"#,
        ] {
            let tokens = run(input);
            assert_eq!(tokens.len(), 1, "{input:?}");
            assert!(
                matches!(tokens[0].0, kinds::STRING | kinds::CHAR),
                "{input:?}"
            );
            assert_eq!(tokens[0].2, input.len() as u32, "{input:?}");
            assert_eq!(tokens[0].3 & flags::IS_ERROR, 0, "{input:?}");
        }
    }

    #[test]
    fn unterminated_raw_string_flags_error() {
        let tokens = run(r#"R"tag(no closer)"#);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, kinds::STRING);
        assert_ne!(tokens[0].3 & flags::IS_ERROR, 0);
    }

    #[test]
    fn digit_separators_stay_inside_numbers() {
        let tokens = run("1'000 0xFF'AA 0b1010'0101 1.5e+1'2");
        let numbers: Vec<_> = tokens.iter().filter(|t| t.0 == kinds::NUMBER).collect();
        assert_eq!(numbers.len(), 4);
        assert!(
            tokens.iter().all(|t| t.0 != kinds::CHAR),
            "digit separators must not become char literals: {tokens:?}"
        );
    }

    #[test]
    fn cpp_operators_are_greedy() {
        let tokens = run("std::vector<int> p->*m .* q <=> r ... xs");
        for kind in [
            kinds::COLON_COLON,
            kinds::ARROW_STAR,
            kinds::DOT_STAR,
            kinds::SPACESHIP,
            kinds::ELLIPSIS,
        ] {
            assert!(tokens.iter().any(|t| t.0 == kind), "missing kind {kind}");
        }
    }
}
