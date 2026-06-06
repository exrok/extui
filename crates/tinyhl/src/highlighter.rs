//! Bundled lexical, semantic, and delimiter state for a host editor.
//!
//! [`Highlighter`] owns enough state to render lexical tokens together with
//! semantic and delimiter overlays. Most languages keep a [`TokenTable`],
//! [`SemanticTable`], and [`DelimiterTable`]; Rust uses a render-cache fast
//! path because Rust semantic highlighting dominates current editor costs.
//! Hosts call [`Highlighter::rebuild`] on a language switch or wholesale
//! reset, [`Highlighter::apply_replacement`] after each buffer edit, and
//! [`Overlays`] when painting a span.
//!
//! The bundle is deliberately host-agnostic. It produces lexical tokens,
//! semantic kinds, and delimiter depths. Mapping those to colors or text
//! styles is the caller's job.

use crate::Language;
use crate::delimiter::{DelimiterQueryIter, DelimiterTable};
use crate::kind;
use crate::render::{RenderSpan, RenderSpans};
use crate::semantic::{self, SemanticKind, SemanticQueryIter, SemanticTable};
use crate::source::Source;
use crate::table::TokenTable;
use crate::token::Span;

/// Incremental highlighter combining lexical, semantic, and delimiter data.
///
/// All three tables start uninitialized. The first
/// [`Highlighter::rebuild`] primes them, and every subsequent
/// [`Highlighter::apply_replacement`] keeps them aligned with the edited
/// source. A replacement called before the first rebuild falls back to a
/// rebuild.
pub struct Highlighter {
    language: Language,
    source_len: Option<u32>,
    table: Option<TokenTable>,
    semantic: Option<SemanticTable>,
    delimiters: Option<DelimiterTable>,
    render_cache: Option<Vec<RenderSpan>>,
}

impl Highlighter {
    /// Returns an empty highlighter targeting `language`.
    ///
    /// No lexing happens until the first call to [`Highlighter::rebuild`].
    pub fn new(language: Language) -> Self {
        Self {
            language,
            source_len: None,
            table: None,
            semantic: None,
            delimiters: None,
            render_cache: None,
        }
    }

    /// Returns the root language the highlighter targets.
    pub fn language(&self) -> Language {
        self.language
    }

    /// Lexes `src` and populates render state from scratch.
    ///
    /// Call this on a language change, on any wholesale text reset, or
    /// once before the first edit to prime the incremental path.
    pub fn rebuild(&mut self, src: &dyn Source) {
        if self.language == Language::Rust {
            let (source_len, render_cache) = semantic::render_rust_fast(src);
            self.source_len = Some(source_len);
            self.table = None;
            self.semantic = None;
            self.delimiters = None;
            self.render_cache = Some(render_cache);
            return;
        }

        let table = TokenTable::new(self.language, src);
        let semantic = SemanticTable::new(&table, src);
        let delimiters = DelimiterTable::new(&table);
        self.source_len = Some(table.source_len());
        self.table = Some(table);
        self.semantic = Some(semantic);
        self.delimiters = Some(delimiters);
        self.render_cache = None;
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
        if self.language == Language::Rust && self.render_cache.is_some() {
            self.rebuild(src);
            return;
        }
        let Some(table) = self.table.as_mut() else {
            self.rebuild(src);
            return;
        };
        self.render_cache = None;
        let mutation = table.mutate_detailed(src, original, new_len);
        self.source_len = Some(table.source_len());
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
    ///
    /// Rust rebuilds use a render-cache fast path for [`Highlighter::render`]
    /// and do not retain a lexical table. Use [`TokenTable::new`] directly
    /// when raw Rust lexical tokens are needed.
    pub fn table(&self) -> Option<&TokenTable> {
        self.table.as_ref()
    }

    /// Returns the byte length of the most recent source, or [`None`] before
    /// the first rebuild.
    #[inline]
    pub fn source_len(&self) -> Option<u32> {
        self.source_len
    }

    /// Returns the semantic-token overlay.
    ///
    /// Rust rebuilds cache render-ready semantic data directly, so this
    /// accessor returns [`None`] for the Rust fast path. Use
    /// [`SemanticTable::new`] directly when raw Rust semantic tokens are
    /// needed.
    pub fn semantic(&self) -> Option<&SemanticTable> {
        self.semantic.as_ref()
    }

    /// Returns the delimiter overlay.
    ///
    /// Rust rebuilds cache render-ready delimiter depths directly, so this
    /// accessor returns [`None`] for the Rust fast path. Use
    /// [`DelimiterTable::new`] directly when raw Rust delimiter tokens are
    /// needed.
    pub fn delimiters(&self) -> Option<&DelimiterTable> {
        self.delimiters.as_ref()
    }

    pub(crate) fn render_cache(&self) -> Option<&[RenderSpan]> {
        self.render_cache.as_deref()
    }

    /// Returns cached render spans inside `span` when the highlighter has a
    /// render cache for the current language.
    ///
    /// This is a fast path for renderers that can consume a borrowed slice
    /// directly. It currently applies to Rust rebuilds.
    pub fn cached_render(&self, span: Span) -> Option<&[RenderSpan]> {
        let cache = self.render_cache()?;
        let start = cache.partition_point(|entry| entry.span.end() <= span.offset);
        let end = cache[start..].partition_point(|entry| entry.span.offset < span.end());
        Some(&cache[start..start + end])
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
    cached: Option<core::iter::Peekable<core::slice::Iter<'t, RenderSpan>>>,
}

impl<'t> Overlays<'t> {
    pub(crate) fn empty() -> Self {
        Self {
            sem: None,
            delim: None,
            cached: None,
        }
    }

    /// Returns overlays restricted to tokens inside `span`.
    pub fn new(highlighter: &'t Highlighter, span: Span) -> Self {
        if let Some(cache) = highlighter.render_cache() {
            let start = cache.partition_point(|entry| entry.span.end() <= span.offset);
            let end = cache[start..].partition_point(|entry| entry.span.offset < span.end());
            return Self {
                sem: None,
                delim: None,
                cached: Some(cache[start..start + end].iter().peekable()),
            };
        }

        let sem = match highlighter.semantic() {
            Some(table) => Some(table.query(span).peekable()),
            None => None,
        };
        let delim = match highlighter.delimiters() {
            Some(table) => Some(table.query(span).peekable()),
            None => None,
        };
        Self {
            sem,
            delim,
            cached: None,
        }
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
        if let Some(iter) = self.cached.as_mut() {
            while let Some(entry) = iter.peek() {
                if entry.span.offset < offset {
                    iter.next();
                } else {
                    break;
                }
            }
            let current = iter.peek().copied().copied();
            return match current {
                Some(entry) if entry.span.offset == offset => {
                    let semantic = if entry.local_kind == kind::LIFETIME && entry.semantic.is_none()
                    {
                        let mut lookahead = iter.clone();
                        lookahead.next();
                        match lookahead.peek() {
                            Some(next)
                                if next.local_kind == kind::LIFETIME
                                    && next.span.offset == offset + 1 =>
                            {
                                next.semantic
                            }
                            _ => entry.semantic,
                        }
                    } else {
                        entry.semantic
                    };
                    (semantic, entry.delimiter)
                }
                _ => (None, None),
            };
        }

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
