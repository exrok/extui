//! Resolved render spans: lexical tokens paired with their overlays.
//!
//! [`RenderSpans`] walks the lexical tokens of a [`Highlighter`], looks up the
//! semantic and delimiter overlay at each, and yields one [`RenderSpan`] per
//! colored region. A span carries the full detail a theme colors from: the
//! lexical [`local_kind`], the render-ready [`SemanticKind`], the rainbow
//! [`delimiter`] depth, and the [`lang_tag`]. Mapping those to a style is the
//! consumer's job, so a theme keeps every distinction the engine drew.
//!
//! The iterator applies the two structural decisions a consumer should not
//! repeat per token. A lifetime yields two spans, splitting its leading `'`
//! from its name. The [`SemanticKind`] is the render-ready
//! [`SemanticToken::display_kind`], folding the type-casing convention, the
//! `Self` type styling, and the macro `!` in so a consumer maps it straight to
//! a color.
//!
//! [`local_kind`]: RenderSpan::local_kind
//! [`delimiter`]: RenderSpan::delimiter
//! [`lang_tag`]: RenderSpan::lang_tag
//! [`SemanticToken::display_kind`]: crate::SemanticToken::display_kind

use crate::highlighter::{Highlighter, Overlays};
use crate::kind;
use crate::semantic::SemanticKind;
use crate::table::QueryIter;
use crate::token::Span;

/// A colored source region with the lexical and semantic detail to style it.
///
/// A theme maps the fields to a style with no token-to-token context. The
/// fields are independent overlays on the same byte range, so a consumer reads
/// whichever ones its palette distinguishes and ignores the rest.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RenderSpan {
    /// Position and length in the source.
    pub span: Span,
    /// Language tag of the underlying lexical token, matching
    /// [`Language::tag`]. Disambiguates kinds a language reuses, such as
    /// Markdown structure.
    ///
    /// [`Language::tag`]: crate::Language::tag
    pub lang_tag: u8,
    /// Lexical kind, a constant from [`kind`] such as [`kind::STRING`] or
    /// [`kind::KEYWORD`].
    pub local_kind: u16,
    /// Render-ready semantic role of an identifier, or [`None`] for a token
    /// the analyzer left untagged.
    ///
    /// This is [`SemanticToken::display_kind`], so the type-casing convention,
    /// `Self`, and the macro `!` are already folded in.
    ///
    /// [`SemanticToken::display_kind`]: crate::SemanticToken::display_kind
    pub semantic: Option<SemanticKind>,
    /// Rainbow nesting depth when this span is a bracket, else [`None`].
    pub delimiter: Option<u16>,
}

/// Iterator over the [`RenderSpan`]s of a [`Highlighter`] within a query span.
///
/// Construct with [`RenderSpans::new`] or [`Highlighter::render`] and feed the
/// whole document span for a full repaint. Spans arrive in source order and
/// cover every lexical token. A lifetime yields two.
pub struct RenderSpans<'t> {
    tokens: Option<QueryIter<'t>>,
    overlays: Overlays<'t>,
    pending: Option<RenderSpan>,
}

impl<'t> RenderSpans<'t> {
    /// Returns the render spans of `highlighter` whose tokens lie in `span`.
    pub fn new(highlighter: &'t Highlighter, span: Span) -> Self {
        Self {
            tokens: highlighter.table().map(|table| table.query(span)),
            overlays: Overlays::new(highlighter, span),
            pending: None,
        }
    }
}

impl Iterator for RenderSpans<'_> {
    type Item = RenderSpan;

    fn next(&mut self) -> Option<RenderSpan> {
        if let Some(pending) = self.pending.take() {
            return Some(pending);
        }
        let token = self.tokens.as_mut()?.next()?;
        let local = token.local_kind();
        let lang_tag = token.lang_tag();
        let (semantic, delimiter) = self.overlays.at(token.span.offset);

        // A lifetime yields two spans so a theme can color the leading `'`
        // apart from the name. The tick carries no semantic role. The name
        // keeps the lifetime role. Consumers never split a token by byte.
        if local == kind::LIFETIME && token.span.len > 1 {
            self.pending = Some(RenderSpan {
                span: Span::new(token.span.offset + 1, token.span.len - 1),
                lang_tag,
                local_kind: kind::LIFETIME,
                semantic,
                delimiter: None,
            });
            return Some(RenderSpan {
                span: Span::new(token.span.offset, 1),
                lang_tag,
                local_kind: kind::LIFETIME,
                semantic: None,
                delimiter: None,
            });
        }

        Some(RenderSpan {
            span: token.span,
            lang_tag,
            local_kind: local,
            semantic,
            delimiter,
        })
    }
}
