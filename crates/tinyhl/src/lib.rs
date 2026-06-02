//! Incremental syntax highlighting for a small, fixed set of languages.
//!
//! TinyHL produces a flat stream of fixed-size [`Token`]s from any type that
//! implements [`Source`], then maintains that stream across edits without
//! re-lexing unchanged regions.
//!
//! # Getting started
//!
//! The simplest path is [`Highlighter`], which bundles a [`TokenTable`] with
//! the [`SemanticTable`] and [`DelimiterTable`] overlays and forwards edits
//! to all three at once:
//!
//! ```
//! use tinyhl::{Highlighter, Language, Source, Span};
//!
//! let mut hl = Highlighter::new(Language::Json);
//! let src: &str = r#"{"name": "tinyhl"}"#;
//! hl.rebuild(&src as &dyn Source);
//! assert!(hl.table().unwrap().token_count() > 0);
//! ```
//!
//! Use [`TokenTable`] directly when only the lexical stream is needed.
//!
//! # Concepts
//!
//! - [`Token`] — 16 bytes: a [`Span`] plus a packed `language:4 | local:12`
//!   kind and per-language lexer state.
//! - [`Source`] — paged byte access. A single contiguous buffer is never
//!   required.
//! - [`TokenTable`] — chunked storage with [`TokenTable::query`] for reads
//!   and [`TokenTable::mutate`] for incremental updates.
//! - [`SemanticTable`] / [`DelimiterTable`] — semantic roles and
//!   rainbow-delimiter depth layered on top of the lexical stream.

pub mod delimiter;
pub(crate) mod dispatch;
mod highlighter;
pub mod kind;
pub mod lex;
pub mod semantic;
pub mod source;
pub mod state;
pub mod table;
pub mod token;

pub use delimiter::{DelimiterKind, DelimiterTable, RainbowDelimiter};
pub use highlighter::{Highlighter, Overlays};
pub use semantic::{SemanticKind, SemanticTable, SemanticToken};
pub use source::Source;
pub(crate) use source::SourceView;
pub use state::LexState;
pub use table::{TokenMutation, TokenTable};
pub use token::{Span, Token, flags, unpack_kind};

/// Languages TinyHL can tokenize.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Language {
    Json = 0,
    Rust = 1,
    C = 2,
    Ts = 3,
    Toml = 4,
    Csv = 5,
    Xml = 6,
    Markdown = 7,
    Css = 8,
    Html = 9,
    /// TypeScript with embedded JSX. Use this for both `.tsx` and `.jsx`
    /// source — it is a strict superset of [`Language::Ts`] that descends into
    /// embedded JSX markup for elements in expression position.
    Tsx = 10,
    /// **Internal embed target — not a root language.** Lexes exactly one JSX
    /// element or fragment and then stops, so passing it to
    /// [`TokenTable::new`] would lex only the first element of a file. It
    /// exists solely as the descend target used by [`Language::Tsx`]; to
    /// highlight a `.tsx` or `.jsx` file use [`Language::Tsx`] instead.
    #[doc(hidden)]
    InternalSingleJsxElement = 11,
    Python = 12,
    Sql = 13,
}

impl Language {
    /// Returns the discriminant value for this variant.
    #[inline]
    pub const fn tag(self) -> u8 {
        self as u8
    }

    /// Returns the variant matching `tag` (the value returned by
    /// [`Language::tag`]).
    ///
    /// # Panics
    ///
    /// Panics if `tag` is not a known language tag.
    pub const fn from_tag(tag: u8) -> Self {
        match tag {
            0 => Self::Json,
            1 => Self::Rust,
            2 => Self::C,
            3 => Self::Ts,
            4 => Self::Toml,
            5 => Self::Csv,
            6 => Self::Xml,
            7 => Self::Markdown,
            8 => Self::Css,
            9 => Self::Html,
            10 => Self::Tsx,
            11 => Self::InternalSingleJsxElement,
            12 => Self::Python,
            13 => Self::Sql,
            _ => panic!("unknown language tag"),
        }
    }
}
