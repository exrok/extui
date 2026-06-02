//! TSX lexer: TypeScript with embedded JSX.
//!
//! `Tsx` is a strict superset of the [`Ts`] lexer. Every byte is classified by
//! [`ts::classify`] except for one case: when a `<` appears in
//! expression-start position — the resumable state does not carry
//! [`ts::STATE_EXPR_END`] — and the next byte can begin a JSX element or
//! fragment, the lexer emits a [`LexStep::Descend`] into [`Language::InternalSingleJsxElement`].
//!
//! The embedded region is *self-terminating*: the JSX lexer signals
//! [`LexStep::Eof`] once it closes its outermost element, so the descend length
//! is simply the remaining visible source. Because the JSX trigger is derived
//! purely from the resumable state and the upcoming bytes, a chunk boundary may
//! fall anywhere — resuming `Tsx` re-detects JSX exactly as the forward pass
//! did, so no special breakpoint handling is needed at the boundary.
//!
//! [`Ts`]: crate::lex::ts::Ts

use crate::lex::ts;
use crate::lex::{LexStep, Lexer, StepBuf};
use crate::token::flags;
use crate::{Language, LexState, SourceView};

/// Returns `true` if a `<` immediately followed by `b` (in expression-start
/// position) opens a JSX element (`<div`), a fragment (`<>`), or a
/// component/identifier name (`<_x`, `<$x`), rather than a comparison.
#[inline]
fn opens_jsx(b: u8) -> bool {
    b.is_ascii_alphabetic() || b == b'_' || b == b'$' || b == b'>'
}

pub(crate) struct Tsx;

impl Lexer for Tsx {
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

            let expr_end = (state.bits() & ts::STATE_EXPR_END) != 0;
            if b == b'<' && !expr_end && matches!(view.byte_at(cursor + 1), Some(c) if opens_jsx(c))
            {
                // JSX in expression position. The JSX lexer owns the entire
                // element, including the opening `<`, and self-terminates at
                // its matching close. A complete element is a primary
                // expression, so control returns to TS in expression-end state.
                out.push(LexStep::Descend {
                    language: Language::InternalSingleJsxElement,
                    inner_len: view.len() - cursor,
                    inner_state_in: LexState::INITIAL,
                    outer_state_after: LexState(ts::STATE_EXPR_END),
                });
                return (cursor, state);
            }

            let start = cursor;
            let (local_kind, end, is_error, state_out) = ts::classify(view, cursor, b, state);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::kind as kinds;
    use crate::{Source, Span, TokenTable};

    /// Lexes `s` through the full driver so embedded JSX regions are expanded,
    /// returning `(lang_tag, local_kind, offset, len, nest)` per token.
    fn toks(s: &str) -> Vec<(u8, u16, u32, u32, u8)> {
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

    fn assert_contiguous(s: &str) {
        let mut pos = 0u32;
        for (_lang, _kind, off, len, _nest) in toks(s) {
            assert_eq!(off, pos, "gap or overlap before {pos} in {s:?}");
            pos += len;
        }
        assert_eq!(pos as usize, s.len(), "did not cover {s:?}");
    }

    #[test]
    fn plain_ts_has_no_jsx() {
        // Without JSX, every token is plain TS (lang tag Tsx, nest 0).
        for (lang, _kind, _off, _len, nest) in toks("const a = b < c && d > e;") {
            assert_eq!(lang, Language::Tsx.tag());
            assert_eq!(nest, 0);
        }
        assert_contiguous("const a = b < c && d > e;");
    }

    #[test]
    fn lt_after_value_is_comparison_not_jsx() {
        // `a < b` — `<` follows a value, so it stays a comparison operator.
        let saw_jsx = toks("a < b").iter().any(|t| t.4 >= 1);
        assert!(!saw_jsx, "`<` after a value must not start JSX");
    }

    #[test]
    fn simple_element_embeds_jsx() {
        let tokens = toks("const x = <div className=\"a\">hi</div>;");
        let jsx_langs: std::collections::HashSet<u8> =
            tokens.iter().filter(|t| t.4 >= 1).map(|t| t.0).collect();
        assert!(
            jsx_langs.contains(&Language::InternalSingleJsxElement.tag()),
            "element should embed JSX"
        );
        // The tag name is a JSX TAG_NAME at nest 1.
        assert!(
            tokens
                .iter()
                .any(|t| t.0 == Language::InternalSingleJsxElement.tag()
                    && t.1 == kinds::TAG_NAME
                    && t.4 == 1)
        );
        assert_contiguous("const x = <div className=\"a\">hi</div>;");
    }

    #[test]
    fn expression_container_re_enters_tsx() {
        // The `{count + 1}` child is TSX again, nested one deeper than the JSX.
        let tokens = toks("const e = <p>{count + 1}</p>;");
        assert!(
            tokens
                .iter()
                .any(|t| t.0 == Language::Tsx.tag() && t.1 == kinds::IDENT && t.4 == 2),
            "expression child should re-enter TSX at nest 2"
        );
        assert_contiguous("const e = <p>{count + 1}</p>;");
    }

    #[test]
    fn nested_elements_and_fragment() {
        assert_contiguous("const t = <><a><b>x</b></a></>;");
        assert_contiguous("const t = <ul><li>1</li><li>2</li></ul>;");
    }

    #[test]
    fn self_closing_and_attributes() {
        assert_contiguous("const i = <img src=\"a.png\" alt={label} />;");
        assert_contiguous("return <Foo.Bar a b={c} {...rest} />;");
    }

    #[test]
    fn jsx_in_arrow_and_ternary() {
        assert_contiguous("const f = () => <div/>;");
        assert_contiguous("const g = cond ? <a/> : <b/>;");
        assert_contiguous("items.map(i => <li key={i}>{i}</li>);");
    }

    #[test]
    fn generics_after_value_stay_ts() {
        // `useState<number>()` — `<` after the `useState` value is a type
        // argument list, not JSX.
        let saw_jsx = toks("useState<number>(0)").iter().any(|t| t.4 >= 1);
        assert!(!saw_jsx, "type arguments after a value must not start JSX");
        assert_contiguous("useState<number>(0)");
    }

    #[test]
    fn unterminated_element_still_covers() {
        assert_contiguous("const x = <div>oops");
        assert_contiguous("const x = <div className={a");
    }
}
