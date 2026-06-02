//! Per-language lexer dispatch.

use crate::lex::{
    Lexer, StepBuf, c::C, css::Css, csv::Csv, go::Go, html::Html, json::Json, jsx::Jsx,
    markdown::Markdown, python::Python, rust::Rust, sh::Sh, sql::Sql, toml::Toml, ts::Ts, tsx::Tsx,
    xml::Xml,
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
        Language::Csv => Csv::step_batch(view, cursor, state, out),
        Language::Css => Css::step_batch(view, cursor, state, out),
        Language::Html => Html::step_batch(view, cursor, state, out),
        Language::Tsx => Tsx::step_batch(view, cursor, state, out),
        Language::InternalSingleJsxElement => Jsx::step_batch(view, cursor, state, out),
        Language::Python => Python::step_batch(view, cursor, state, out),
        Language::Sql => Sql::step_batch(view, cursor, state, out),
        Language::Go => Go::step_batch(view, cursor, state, out),
        Language::Sh => Sh::step_batch(view, cursor, state, out),
    }
}
