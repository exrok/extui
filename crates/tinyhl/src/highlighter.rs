//! Bundled lexical, semantic, and delimiter state for a host editor.
//!
//! [`Highlighter`] owns a [`TokenTable`] together with the [`SemanticTable`]
//! and [`DelimiterTable`] overlays, forwarding every edit to each of them.
//! Hosts call [`Highlighter::rebuild`] on a language switch or wholesale
//! reset, [`Highlighter::apply_replacement`] after each buffer edit, and
//! [`Overlays`] when painting a span.
//!
//! The bundle is deliberately host-agnostic. It produces lexical tokens,
//! semantic kinds, and delimiter depths. Mapping those to colors or text
//! styles is the caller's job.

use crate::Language;
use crate::delimiter::{DelimiterQueryIter, DelimiterTable};
use crate::render::RenderSpans;
use crate::semantic::{SemanticKind, SemanticQueryIter, SemanticTable};
use crate::source::Source;
use crate::table::TokenTable;
use crate::token::Span;

/// Incremental highlighter combining a [`TokenTable`], [`SemanticTable`],
/// and [`DelimiterTable`].
///
/// All three tables start uninitialized. The first
/// [`Highlighter::rebuild`] primes them, and every subsequent
/// [`Highlighter::apply_replacement`] keeps them aligned with the edited
/// source. A replacement called before the first rebuild falls back to a
/// rebuild.
pub struct Highlighter {
    language: Language,
    table: Option<TokenTable>,
    semantic: Option<SemanticTable>,
    delimiters: Option<DelimiterTable>,
}

impl Highlighter {
    /// Returns an empty highlighter targeting `language`.
    ///
    /// No lexing happens until the first call to [`Highlighter::rebuild`].
    pub fn new(language: Language) -> Self {
        Self {
            language,
            table: None,
            semantic: None,
            delimiters: None,
        }
    }

    /// Returns the root language the highlighter targets.
    pub fn language(&self) -> Language {
        self.language
    }

    /// Lexes `src` and populates all three tables from scratch.
    ///
    /// Call this on a language change, on any wholesale text reset, or
    /// once before the first edit to prime the incremental path.
    pub fn rebuild(&mut self, src: &dyn Source) {
        let table = TokenTable::new(self.language, src);
        let semantic = SemanticTable::new(&table, src);
        let delimiters = DelimiterTable::new(&table);
        self.table = Some(table);
        self.semantic = Some(semantic);
        self.delimiters = Some(delimiters);
    }

    /// Applies one buffer replacement to every backing table.
    ///
    /// - `src` — the post-edit source.
    /// - `original` — the replaced range in pre-edit coordinates.
    /// - `new_len` — the length of the replacement in post-edit
    ///   coordinates.
    ///
    /// If the highlighter has never been primed, this falls back to
    /// [`Highlighter::rebuild`].
    pub fn apply_replacement(&mut self, src: &dyn Source, original: Span, new_len: u32) {
        let Some(table) = self.table.as_mut() else {
            self.rebuild(src);
            return;
        };
        let mutation = table.mutate_detailed(src, original, new_len);
        if let Some(sem) = self.semantic.as_mut() {
            sem.mutate(table, src, &mutation);
        } else {
            self.semantic = Some(SemanticTable::new(table, src));
        }
        if let Some(delim) = self.delimiters.as_mut() {
            delim.mutate(table, &mutation);
        } else {
            self.delimiters = Some(DelimiterTable::new(table));
        }
    }

    /// Returns the lexical token table, or [`None`] before the first
    /// [`Highlighter::rebuild`].
    pub fn table(&self) -> Option<&TokenTable> {
        self.table.as_ref()
    }

    /// Returns the semantic-token overlay.
    pub fn semantic(&self) -> Option<&SemanticTable> {
        self.semantic.as_ref()
    }

    /// Returns the delimiter overlay.
    pub fn delimiters(&self) -> Option<&DelimiterTable> {
        self.delimiters.as_ref()
    }

    /// Returns the [`RenderSpan`]s whose tokens lie in `span`, in source
    /// order.
    ///
    /// This is the highest-level render entry point: each [`RenderSpan`]
    /// carries the lexical kind, render-ready semantic kind, and delimiter
    /// depth a theme maps to a style, with the macro `!`, `Self`, and
    /// lifetime-split policy already applied.
    ///
    /// [`RenderSpan`]: crate::RenderSpan
    pub fn render(&self, span: Span) -> RenderSpans<'_> {
        RenderSpans::new(self, span)
    }
}

/// Iterator pair yielding the semantic kind and delimiter depth at a
/// lexical-token offset.
///
/// A typical render loop builds one [`Overlays`] per visible span, then
/// calls [`Overlays::at`] with each lexical token's start offset. Calls
/// must be made with non-decreasing offsets because the iterators only
/// advance forward.
pub struct Overlays<'t> {
    sem: Option<core::iter::Peekable<SemanticQueryIter<'t>>>,
    delim: Option<core::iter::Peekable<DelimiterQueryIter<'t>>>,
}

impl<'t> Overlays<'t> {
    /// Returns overlays restricted to tokens inside `span`.
    pub fn new(highlighter: &'t Highlighter, span: Span) -> Self {
        let sem = match highlighter.semantic() {
            Some(table) => Some(table.query(span).peekable()),
            None => None,
        };
        let delim = match highlighter.delimiters() {
            Some(table) => Some(table.query(span).peekable()),
            None => None,
        };
        Self { sem, delim }
    }

    /// Returns the semantic kind and delimiter depth whose entries start
    /// at `offset`, or `None` for either side that has no match.
    ///
    /// The semantic kind is render-ready: it is [`SemanticToken::display_kind`],
    /// which folds the type-casing convention in so a consumer maps it to a
    /// color with no further logic. Query [`SemanticTable`] directly for the
    /// raw structural kind.
    ///
    /// Each call advances past entries starting before `offset`, so
    /// `offset` must be non-decreasing across successive calls.
    ///
    /// [`SemanticToken::display_kind`]: crate::SemanticToken::display_kind
    /// [`SemanticTable`]: crate::SemanticTable
    pub fn at(&mut self, offset: u32) -> (Option<SemanticKind>, Option<u16>) {
        let sem_kind = match self.sem.as_mut() {
            Some(iter) => {
                while let Some(entry) = iter.peek() {
                    if entry.span.offset < offset {
                        iter.next();
                    } else {
                        break;
                    }
                }
                match iter.peek() {
                    Some(entry) if entry.span.offset == offset => Some(entry.display_kind()),
                    _ => None,
                }
            }
            None => None,
        };
        let delim_depth = match self.delim.as_mut() {
            Some(iter) => {
                while let Some(entry) = iter.peek() {
                    if entry.span.offset < offset {
                        iter.next();
                    } else {
                        break;
                    }
                }
                match iter.peek() {
                    Some(entry) if entry.span.offset == offset => Some(entry.depth),
                    _ => None,
                }
            }
            None => None,
        };
        (sem_kind, delim_depth)
    }
}
