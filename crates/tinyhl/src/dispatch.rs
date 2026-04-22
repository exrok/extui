//! Per-language lexer dispatch.

use crate::lex::{
    Lexer, StepBuf, c::C, json::Json, markdown::Markdown, rust::Rust, toml::Toml, ts::Ts,
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
        Language::Csv | Language::Xml => {
            unimplemented!("lexer not yet implemented for {language:?}")
        }
    }
}

#[allow(dead_code)]
pub(crate) fn is_safe_state(language: Language, state: LexState) -> bool {
    match language {
        Language::Json => Json::is_safe_state(state),
        Language::Rust => Rust::is_safe_state(state),
        Language::C => C::is_safe_state(state),
        Language::Markdown => Markdown::is_safe_state(state),
        Language::Toml => Toml::is_safe_state(state),
        Language::Ts => Ts::is_safe_state(state),
        Language::Csv | Language::Xml => {
            unimplemented!("lexer not yet implemented for {language:?}")
        }
    }
}
