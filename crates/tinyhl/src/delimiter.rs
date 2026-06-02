//! Rainbow-delimiter overlay derived from a [`TokenTable`].
//!
//! [`DelimiterTable`] pairs every opening and closing bracket token with
//! a nesting depth, which a theme can map to a color cycle.

use crate::kind;
use crate::table::TokenMutation;
use crate::token::{kind_lang_tag, kind_local};
use crate::{Language, Span, Token, TokenTable};

const CHUNK_TOKEN_TARGET: usize = 128;

/// Family a bracket token belongs to.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum DelimiterKind {
    /// `(` or `)`.
    Paren = 0,
    /// `{` or `}`, plus the C digraphs `<%` and `%>`.
    Brace = 1,
    /// `[` or `]`, plus the C digraphs `<:` and `:>`.
    Bracket = 2,
}

/// Bracket token paired with its nesting depth.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RainbowDelimiter {
    /// Position and length in the source.
    pub span: Span,
    /// Which bracket family this is.
    pub kind: DelimiterKind,
    /// Zero-based nesting depth of the outer-most enclosing bracket pair.
    /// The outermost pair has depth 0, its contents depth 1, and so on.
    pub depth: u16,
    /// `true` if this token opens a pair, `false` if it closes one.
    pub is_open: bool,
    /// Language tag of the producing token (see [`Token::lang_tag`]).
    pub lang_tag: u8,
    /// Embedded-language nesting depth of the producing token.
    pub nest: u8,
}

#[derive(Clone, Copy, Debug)]
struct RelDelimiter {
    rel_offset: u32,
    len: u32,
    kind: DelimiterKind,
    depth: u16,
    is_open: bool,
    lang_tag: u8,
    nest: u8,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct DelimiterState {
    stack: Vec<DelimiterKind>,
}

impl DelimiterState {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }
}

#[derive(Clone, Debug)]
struct DelimiterChunk {
    base_offset: u32,
    end_offset: u32,
    lang_tag: u8,
    nest: u8,
    state_in: DelimiterState,
    lex_token_count: usize,
    tokens: Vec<RelDelimiter>,
}

impl DelimiterChunk {
    fn new(base_offset: u32, lang_tag: u8, nest: u8, state_in: DelimiterState) -> Self {
        Self {
            base_offset,
            end_offset: base_offset,
            lang_tag,
            nest,
            state_in,
            lex_token_count: 0,
            tokens: Vec::new(),
        }
    }
}

/// Rainbow-delimiter overlay kept in sync with a [`TokenTable`].
pub struct DelimiterTable {
    source_len: u32,
    root_lang: Language,
    chunks: Vec<DelimiterChunk>,
}

impl DelimiterTable {
    /// Builds a delimiter table from the current lexical tokens.
    pub fn new(tokens: &TokenTable) -> Self {
        let mut table = Self {
            source_len: tokens.source_len(),
            root_lang: tokens.root_language(),
            chunks: Vec::new(),
        };
        table.rebuild_from(tokens, 0, None);
        table
    }

    /// Re-analyzes the delimiter stream after a lexical edit and returns
    /// the post-edit span covering every re-emitted delimiter.
    pub fn mutate(&mut self, tokens: &TokenTable, edit: &TokenMutation) -> Span {
        let delta = edit.new_len as i64 - edit.original.len as i64;
        let edit_end = edit.invalidated.end();
        let start_idx = self
            .chunks
            .partition_point(|c| c.base_offset < edit.invalidated.offset)
            .saturating_sub(1);
        let start_offset = self
            .chunks
            .get(start_idx)
            .map(|c| c.base_offset)
            .unwrap_or(0);
        let mut old_stop_idx = self.chunks.len();
        let mut builder = Builder::new(tokens, start_offset);
        let mut new_chunks = Vec::new();

        if let Some(chunk) = self.chunks.get(start_idx) {
            builder.state = chunk.state_in.clone();
            builder.current_lang = Language::from_tag(chunk.lang_tag);
            builder.current_nest = chunk.nest;
        }

        while let Some(next_start) = builder.next_chunk_start() {
            let next_old_offset = (next_start as i64 - delta) as u32;
            if next_start >= edit_end {
                if let Some(idx) = self.find_chunk(next_old_offset) {
                    let old = &self.chunks[idx];
                    if old.base_offset == next_old_offset
                        && old.lang_tag == builder.current_lang.tag()
                        && old.nest == builder.current_nest
                        && old.state_in == builder.state
                    {
                        old_stop_idx = idx;
                        break;
                    }
                }
            }
            if let Some(chunk) = builder.take_chunk() {
                new_chunks.push(chunk);
            }
        }
        if let Some(chunk) = builder.finish_chunk() {
            new_chunks.push(chunk);
        }

        let invalidated_start = start_offset;
        let invalidated_end = if old_stop_idx < self.chunks.len() {
            (self.chunks[old_stop_idx].base_offset as i64 + delta) as u32
        } else {
            tokens.source_len()
        };

        let num_new = new_chunks.len();
        self.chunks.splice(start_idx..old_stop_idx, new_chunks);
        let shift_from = start_idx + num_new;
        if delta != 0 {
            for chunk in &mut self.chunks[shift_from..] {
                chunk.base_offset = (chunk.base_offset as i64 + delta) as u32;
                chunk.end_offset = (chunk.end_offset as i64 + delta) as u32;
            }
        }
        self.source_len = tokens.source_len();

        Span::new(
            invalidated_start,
            invalidated_end.saturating_sub(invalidated_start),
        )
    }

    /// Returns every delimiter whose span starts inside `span`, in source
    /// order.
    pub fn query(&self, span: Span) -> DelimiterQueryIter<'_> {
        let chunk_idx = self.chunks.partition_point(|c| c.end_offset <= span.offset);
        DelimiterQueryIter {
            chunks: &self.chunks,
            chunk_idx,
            tok_idx: 0,
            query_end: span.end(),
        }
    }

    /// Returns the byte length of the source.
    #[inline]
    pub fn source_len(&self) -> u32 {
        self.source_len
    }

    /// Returns the outermost [`Language`] the underlying token table was
    /// built for.
    #[inline]
    pub fn root_language(&self) -> Language {
        self.root_lang
    }

    /// Returns the number of stored delimiter entries.
    pub fn token_count(&self) -> usize {
        self.chunks.iter().map(|c| c.tokens.len()).sum()
    }

    #[doc(hidden)]
    pub fn chunk_count(&self) -> usize {
        self.chunks.len()
    }

    fn rebuild_from(
        &mut self,
        tokens: &TokenTable,
        start_offset: u32,
        state: Option<(DelimiterState, Language, u8)>,
    ) {
        let mut builder = Builder::new(tokens, start_offset);
        if let Some((state, lang, nest)) = state {
            builder.state = state;
            builder.current_lang = lang;
            builder.current_nest = nest;
        }
        while builder.next_chunk_start().is_some() {
            if let Some(chunk) = builder.take_chunk() {
                self.chunks.push(chunk);
            }
        }
        if let Some(chunk) = builder.finish_chunk() {
            self.chunks.push(chunk);
        }
    }

    fn find_chunk(&self, offset: u32) -> Option<usize> {
        self.chunks
            .binary_search_by_key(&offset, |c| c.base_offset)
            .ok()
    }
}

/// Iterator yielding [`RainbowDelimiter`]s from [`DelimiterTable::query`].
pub struct DelimiterQueryIter<'t> {
    chunks: &'t [DelimiterChunk],
    chunk_idx: usize,
    tok_idx: usize,
    query_end: u32,
}

impl Iterator for DelimiterQueryIter<'_> {
    type Item = RainbowDelimiter;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let chunk = self.chunks.get(self.chunk_idx)?;
            if self.tok_idx >= chunk.tokens.len() {
                self.chunk_idx += 1;
                self.tok_idx = 0;
                continue;
            }
            let rel = chunk.tokens[self.tok_idx];
            self.tok_idx += 1;
            let abs_offset = chunk.base_offset + rel.rel_offset;
            if abs_offset >= self.query_end {
                return None;
            }
            return Some(RainbowDelimiter {
                span: Span::new(abs_offset, rel.len),
                kind: rel.kind,
                depth: rel.depth,
                is_open: rel.is_open,
                lang_tag: rel.lang_tag,
                nest: rel.nest,
            });
        }
    }
}

struct Builder<'t> {
    cursor: crate::table::TokenCursor<'t>,
    current_lang: Language,
    current_nest: u8,
    state: DelimiterState,
    chunk: Option<DelimiterChunk>,
}

impl<'t> Builder<'t> {
    fn new(tokens: &'t TokenTable, start_offset: u32) -> Self {
        let cursor = tokens.cursor_at(start_offset);
        let first = cursor.peek();
        Self {
            current_lang: first
                .map(|t| Language::from_tag(kind_lang_tag(t.kind)))
                .unwrap_or(tokens.root_language()),
            current_nest: first.map(|t| t.nest).unwrap_or(0),
            state: DelimiterState::new(),
            chunk: None,
            cursor,
        }
    }

    fn next_chunk_start(&mut self) -> Option<u32> {
        loop {
            let token = self.cursor.peek()?;
            let token_lang = Language::from_tag(kind_lang_tag(token.kind));
            if self.chunk.is_none() {
                if token_lang != self.current_lang || token.nest != self.current_nest {
                    self.current_lang = token_lang;
                    self.current_nest = token.nest;
                    self.state = DelimiterState::new();
                }
                self.chunk = Some(DelimiterChunk::new(
                    token.span.offset,
                    token_lang.tag(),
                    token.nest,
                    self.state.clone(),
                ));
            }

            let split_for_lang = token_lang != self.current_lang || token.nest != self.current_nest;
            let split_for_size = self
                .chunk
                .as_ref()
                .map(|chunk| chunk.lex_token_count >= CHUNK_TOKEN_TARGET)
                .unwrap_or(false);

            if split_for_lang || split_for_size {
                return Some(token.span.offset);
            }

            let token = self.cursor.next().unwrap();
            self.current_lang = token_lang;
            self.current_nest = token.nest;
            self.process_token(token);
        }
    }

    fn take_chunk(&mut self) -> Option<DelimiterChunk> {
        let next = self.cursor.peek();
        let mut chunk = self.chunk.take()?;
        chunk.end_offset = next.map(|t| t.span.offset).unwrap_or(chunk.end_offset);
        Some(chunk)
    }

    fn finish_chunk(&mut self) -> Option<DelimiterChunk> {
        let mut chunk = self.chunk.take()?;
        chunk.end_offset = self.cursor.offset().max(chunk.base_offset);
        Some(chunk)
    }

    fn process_token(&mut self, token: Token) {
        let Some(chunk) = &mut self.chunk else {
            return;
        };
        chunk.lex_token_count += 1;
        chunk.end_offset = token.span.end();

        let Some((kind, is_open)) = classify_delimiter(token) else {
            return;
        };

        let depth = if is_open {
            self.state.stack.len() as u16
        } else {
            match self.state.stack.last().copied() {
                Some(top) if top == kind => self.state.stack.len().saturating_sub(1) as u16,
                Some(_) => self.state.stack.len() as u16,
                None => 0,
            }
        };

        chunk.tokens.push(RelDelimiter {
            rel_offset: token.span.offset - chunk.base_offset,
            len: token.span.len,
            kind,
            depth,
            is_open,
            lang_tag: kind_lang_tag(token.kind),
            nest: token.nest,
        });

        if is_open {
            self.state.stack.push(kind);
        } else if matches!(self.state.stack.last().copied(), Some(top) if top == kind) {
            self.state.stack.pop();
        }
    }
}

fn classify_delimiter(token: Token) -> Option<(DelimiterKind, bool)> {
    match kind_local(token.kind) {
        kind::OPEN_BRACE | kind::LT_PERCENT => Some((DelimiterKind::Brace, true)),
        kind::CLOSE_BRACE | kind::PERCENT_GT => Some((DelimiterKind::Brace, false)),
        kind::OPEN_PAREN => Some((DelimiterKind::Paren, true)),
        kind::CLOSE_PAREN => Some((DelimiterKind::Paren, false)),
        kind::OPEN_BRACKET | kind::LT_COLON => Some((DelimiterKind::Bracket, true)),
        kind::CLOSE_BRACKET | kind::COLON_GT => Some((DelimiterKind::Bracket, false)),
        _ => None,
    }
}
