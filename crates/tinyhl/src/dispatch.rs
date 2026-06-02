//! Per-language lexer dispatch.

use crate::lex::{
    Lexer, StepBuf, c::C, json::Json, markdown::Markdown, rust::Rust, toml::Toml, ts::Ts, xml::Xml,
};
use crate::{Language, LexState, SourceView};

pub(crate) fn step_batch(
    language: Language,
    view: &mut SourceView<'_>,
    cursor: u32,
    state: LexState,
    out: &mut StepBuf,
) -> (u32, LexState) {
    match language {
        Language::Json => Json::step_batch(view, cursor, state, out),
        Language::Rust => Rust::step_batch(view, cursor, state, out),
        Language::C => C::step_batch(view, cursor, state, out),
        Language::Markdown => Markdown::step_batch(view, cursor, state, out),
        Language::Toml => Toml::step_batch(view, cursor, state, out),
        Language::Ts => Ts::step_batch(view, cursor, state, out),
        Language::Xml => Xml::step_batch(view, cursor, state, out),
        Language::Csv => {
            unimplemented!("lexer not yet implemented for {language:?}")
        }
    }
}
