//! Incremental token table.
//!
//! [`TokenTable`] holds the lexed output of a [`Source`]. Queries are range
//! lookups. Edits are applied with [`TokenTable::mutate`], which re-lexes
//! only the affected region and reuses existing tokens on either side.

use crate::dispatch;
use crate::lex::{LexStep, StepBuf};
use crate::token::{flags, pack_kind};
use crate::{Language, LexState, Source, SourceView, Span, Token};

const CHUNK_TOKEN_TARGET: usize = 128;

#[derive(Clone, Copy, Debug)]
struct RelToken {
    rel_offset: u32,
    len: u32,
    kind: u16,
    state_out: u16,
    flags: u8,
    nest: u8,
    _reserved: u16,
}

#[derive(Clone, Debug)]
struct Chunk {
    base_offset: u32,
    state_in: u16,
    language: u8,
    nest: u8,
    tokens: Vec<RelToken>,
}

impl Chunk {
    fn start(base_offset: u32, state_in: u16, language: u8, nest: u8) -> Self {
        Self {
            base_offset,
            state_in,
            language,
            nest,
            tokens: Vec::new(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct EmbedFrame {
    parent_lang: Language,
    resume_state: LexState,
    region_end: u32,
    parent_nest: u8,
    parent_visible_len: u32,
}

/// Lexed view of a [`Source`], queryable by [`Span`] and updatable with
/// [`TokenTable::mutate`].
///
/// # Examples
///
/// ```
/// use tinyhl::{Language, Source, Span, TokenTable};
///
/// let src: &str = r#"{"x":1}"#;
/// let table = TokenTable::new(Language::Json, &src as &dyn Source);
/// let all: Vec<_> = table.query(Span::new(0, table.source_len())).collect();
/// assert_eq!(all.len(), 5);
/// ```
pub struct TokenTable {
    source_len: u32,
    root_lang: Language,
    chunks: Vec<Chunk>,
}

/// Summary of an edit applied with [`TokenTable::mutate_detailed`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TokenMutation {
    /// Edited range in pre-edit coordinates.
    pub original: Span,
    /// Length of the replacement in post-edit coordinates.
    pub new_len: u32,
    /// Span in post-edit coordinates covering every token whose content or
    /// position changed.
    pub invalidated: Span,
}

#[derive(Clone)]
pub(crate) struct TokenCursor<'t> {
    table: &'t TokenTable,
    chunk_idx: usize,
    tok_idx: usize,
}

impl TokenTable {
    /// Lexes `src` starting in `language` and returns the populated table.
    ///
    /// Root `language` applies at the outermost level. Embedded regions
    /// such as fenced code blocks in Markdown are lexed in their own
    /// language and tagged with an increased [`Token::nest`] depth.
    pub fn new(language: Language, src: &dyn Source) -> Self {
        let mut table = Self {
            source_len: src.len(),
            root_lang: language,
            chunks: Vec::new(),
        };

        let mut view = SourceView::new(src, 0);
        let mut buf = StepBuf::new();
        let mut stack: Vec<EmbedFrame> = Vec::new();
        let mut cur_lang = language;
        let mut state = LexState::INITIAL;
        let mut nest: u8 = 0;
        let mut cursor: u32 = 0;

        let mut chunk = Chunk::start(cursor, state.bits(), cur_lang.tag(), nest);

        loop {
            while let Some(top) = stack.last() {
                if top.region_end != cursor {
                    break;
                }
                let frame = stack.pop().unwrap();
                cur_lang = frame.parent_lang;
                state = frame.resume_state;
                nest = frame.parent_nest;
                view.set_visible_len(frame.parent_visible_len);
            }

            buf.clear();
            let (_ret_cursor, _ret_state) =
                dispatch::step_batch(cur_lang, &mut view, cursor, state, &mut buf);

            let mut ended = false;
            let mut switched = false;

            for step in buf.as_slice() {
                match *step {
                    LexStep::Token {
                        len,
                        local_kind,
                        state_out,
                        flags: f,
                    } => {
                        let rel = cursor - chunk.base_offset;
                        chunk.tokens.push(RelToken {
                            rel_offset: rel,
                            len,
                            kind: pack_kind(cur_lang.tag(), local_kind),
                            state_out: state_out.bits(),
                            flags: f,
                            nest,
                            _reserved: 0,
                        });
                        cursor += len;
                        state = state_out;

                        let at_breakpoint = (f & flags::STATE_BREAKPOINT) != 0;
                        if chunk.tokens.len() >= CHUNK_TOKEN_TARGET && at_breakpoint {
                            table.chunks.push(core::mem::replace(
                                &mut chunk,
                                Chunk::start(cursor, state.bits(), cur_lang.tag(), nest),
                            ));
                        }
                    }
                    LexStep::Descend {
                        language: inner,
                        inner_len,
                        inner_state_in,
                        outer_state_after,
                    } => {
                        let region_end = cursor + inner_len;
                        let parent_visible_len = view.set_visible_len(region_end);
                        stack.push(EmbedFrame {
                            parent_lang: cur_lang,
                            resume_state: outer_state_after,
                            region_end,
                            parent_nest: nest,
                            parent_visible_len,
                        });
                        cur_lang = inner;
                        state = inner_state_in;
                        nest = nest.saturating_add(1);
                        switched = true;
                        break;
                    }
                    LexStep::Eof => {
                        if let Some(frame) = stack.pop() {
                            cur_lang = frame.parent_lang;
                            state = frame.resume_state;
                            nest = frame.parent_nest;
                            view.set_visible_len(frame.parent_visible_len);
                            switched = true;
                        } else {
                            ended = true;
                        }
                        break;
                    }
                }
            }

            if ended {
                break;
            }
            if switched {
                continue;
            }
        }

        if !chunk.tokens.is_empty() {
            table.chunks.push(chunk);
        }

        table
    }

    /// Applies an edit and returns the invalidated post-edit region.
    ///
    /// - `original` — pre-edit range that was replaced.
    /// - `new_len` — byte length of the replacement.
    /// - `src` — post-edit source.
    ///
    /// The returned [`Span`] is in post-edit coordinates and covers every
    /// token whose content or position changed. It always encloses
    /// `[original.offset, original.offset + new_len)`.
    pub fn mutate(&mut self, src: &dyn Source, original: Span, new_len: u32) -> Span {
        self.mutate_detailed(src, original, new_len).invalidated
    }

    /// Applies an edit and returns a [`TokenMutation`] describing the
    /// replacement and the invalidated range.
    pub fn mutate_detailed(
        &mut self,
        src: &dyn Source,
        original: Span,
        new_len: u32,
    ) -> TokenMutation {
        let new_source_len = src.len();
        let delta = new_len as i64 - original.len as i64;
        let edit_end_new = original.offset + new_len;

        // Use strict `<` so that an edit sitting exactly on a chunk boundary
        // resumes from the preceding chunk. If we picked the chunk whose
        // `base_offset == original.offset`, the previous chunk's last token
        // would be treated as final — but an edit at the boundary can extend
        // it (e.g. inserting whitespace adjacent to a trailing whitespace
        // token). Backing up one chunk lets that token merge correctly.
        let mut start_chunk_idx = if self.chunks.is_empty() {
            0
        } else {
            self.chunks
                .partition_point(|c| c.base_offset < original.offset)
                .saturating_sub(1)
        };
        // The mutate driver starts with an empty embed-frame stack. If the
        // resume chunk sits inside an embedded region (`nest > 0`), the
        // re-lex would proceed in the inner language with no frame to pop,
        // so it would never return to the outer language and convergence
        // could not fire. Back up to a `nest == 0` chunk so the re-lex
        // re-enters the embed naturally and rebuilds the frame stack.
        while start_chunk_idx > 0 && self.chunks[start_chunk_idx].nest > 0 {
            start_chunk_idx -= 1;
        }

        let (start_cursor, start_state, start_lang, start_nest) =
            match self.chunks.get(start_chunk_idx) {
                Some(c) => (
                    c.base_offset,
                    LexState::from(c.state_in),
                    Language::from_tag(c.language),
                    c.nest,
                ),
                None => (0, LexState::INITIAL, self.root_lang, 0),
            };

        let mut view = SourceView::new(src, start_cursor);
        let mut buf = StepBuf::new();
        let mut stack: Vec<EmbedFrame> = Vec::new();
        let mut cur_lang = start_lang;
        let mut state = start_state;
        let mut nest = start_nest;
        let mut cursor = start_cursor;
        let mut new_chunks: Vec<Chunk> = Vec::new();
        let mut chunk = Chunk::start(cursor, state.bits(), cur_lang.tag(), nest);

        let mut converged_at: Option<usize> = None;

        'outer: loop {
            while let Some(top) = stack.last() {
                if top.region_end != cursor {
                    break;
                }
                let frame = stack.pop().unwrap();
                cur_lang = frame.parent_lang;
                state = frame.resume_state;
                nest = frame.parent_nest;
                view.set_visible_len(frame.parent_visible_len);
            }

            if stack.is_empty() && cursor >= edit_end_new {
                if let Some(idx) = find_converge_chunk(
                    &self.chunks,
                    cursor,
                    delta,
                    state.bits(),
                    cur_lang.tag(),
                    nest,
                ) {
                    converged_at = Some(idx);
                    break 'outer;
                }
            }

            buf.clear();
            let _ = dispatch::step_batch(cur_lang, &mut view, cursor, state, &mut buf);

            for step in buf.as_slice() {
                match *step {
                    LexStep::Token {
                        len,
                        local_kind,
                        state_out,
                        flags: f,
                    } => {
                        let rel = cursor - chunk.base_offset;
                        chunk.tokens.push(RelToken {
                            rel_offset: rel,
                            len,
                            kind: pack_kind(cur_lang.tag(), local_kind),
                            state_out: state_out.bits(),
                            flags: f,
                            nest,
                            _reserved: 0,
                        });
                        cursor += len;
                        state = state_out;

                        let at_breakpoint = (f & flags::STATE_BREAKPOINT) != 0;
                        if chunk.tokens.len() >= CHUNK_TOKEN_TARGET && at_breakpoint {
                            new_chunks.push(core::mem::replace(
                                &mut chunk,
                                Chunk::start(cursor, state.bits(), cur_lang.tag(), nest),
                            ));
                        }

                        if stack.is_empty() && cursor >= edit_end_new {
                            if let Some(idx) = find_converge_chunk(
                                &self.chunks,
                                cursor,
                                delta,
                                state.bits(),
                                cur_lang.tag(),
                                nest,
                            ) {
                                converged_at = Some(idx);
                                break;
                            }
                        }
                    }
                    LexStep::Descend {
                        language: inner,
                        inner_len,
                        inner_state_in,
                        outer_state_after,
                    } => {
                        let region_end = cursor + inner_len;
                        let parent_visible_len = view.set_visible_len(region_end);
                        stack.push(EmbedFrame {
                            parent_lang: cur_lang,
                            resume_state: outer_state_after,
                            region_end,
                            parent_nest: nest,
                            parent_visible_len,
                        });
                        cur_lang = inner;
                        state = inner_state_in;
                        nest = nest.saturating_add(1);
                        break;
                    }
                    LexStep::Eof => {
                        if let Some(frame) = stack.pop() {
                            cur_lang = frame.parent_lang;
                            state = frame.resume_state;
                            nest = frame.parent_nest;
                            view.set_visible_len(frame.parent_visible_len);
                        } else {
                            break 'outer;
                        }
                        break;
                    }
                }
            }

            if converged_at.is_some() {
                break;
            }
        }

        if !chunk.tokens.is_empty() {
            new_chunks.push(chunk);
        }

        let stop_idx = converged_at.unwrap_or(self.chunks.len());
        let invalidated_start = start_cursor;
        let invalidated_end_new = if stop_idx < self.chunks.len() {
            (self.chunks[stop_idx].base_offset as i64 + delta) as u32
        } else {
            new_source_len
        };

        let num_new = new_chunks.len();
        self.chunks.splice(start_chunk_idx..stop_idx, new_chunks);

        let shift_from = start_chunk_idx + num_new;
        if delta != 0 {
            for c in &mut self.chunks[shift_from..] {
                c.base_offset = (c.base_offset as i64 + delta) as u32;
            }
        }

        self.source_len = new_source_len;

        TokenMutation {
            original,
            new_len,
            invalidated: Span::new(
                invalidated_start,
                invalidated_end_new.saturating_sub(invalidated_start),
            ),
        }
    }

    /// Returns the byte length of the source.
    #[inline]
    pub fn source_len(&self) -> u32 {
        self.source_len
    }

    /// Returns the outermost [`Language`] this table was built for.
    #[inline]
    pub fn root_language(&self) -> Language {
        self.root_lang
    }

    /// Returns the total token count.
    pub fn token_count(&self) -> usize {
        self.chunks.iter().map(|c| c.tokens.len()).sum()
    }

    #[doc(hidden)]
    pub fn chunk_count(&self) -> usize {
        self.chunks.len()
    }

    /// Returns an iterator over every [`Token`] overlapping `span`, in
    /// source order.
    pub fn query(&self, span: Span) -> QueryIter<'_> {
        let (chunk_idx, tok_idx) = self.locate(span.offset);
        QueryIter {
            chunks: &self.chunks,
            chunk_idx,
            tok_idx,
            query_end: span.end(),
        }
    }

    pub(crate) fn cursor_at(&self, offset: u32) -> TokenCursor<'_> {
        let (chunk_idx, tok_idx) = self.locate(offset);
        TokenCursor {
            table: self,
            chunk_idx,
            tok_idx,
        }
    }

    fn locate(&self, offset: u32) -> (usize, usize) {
        if self.chunks.is_empty() {
            return (0, 0);
        }
        let idx = self
            .chunks
            .partition_point(|c| c.base_offset <= offset)
            .saturating_sub(1);
        let chunk = &self.chunks[idx];
        let rel = offset.saturating_sub(chunk.base_offset);
        let tok_idx = chunk
            .tokens
            .partition_point(|t| t.rel_offset + t.len <= rel);
        (idx, tok_idx)
    }
}

impl<'t> TokenCursor<'t> {
    pub(crate) fn offset(&self) -> u32 {
        match self.peek() {
            Some(token) => token.span.offset,
            None => self.table.source_len,
        }
    }

    pub(crate) fn peek(&self) -> Option<Token> {
        let mut chunk_idx = self.chunk_idx;
        let mut tok_idx = self.tok_idx;
        loop {
            let chunk = self.table.chunks.get(chunk_idx)?;
            if tok_idx >= chunk.tokens.len() {
                chunk_idx += 1;
                tok_idx = 0;
                continue;
            }
            let rt = chunk.tokens[tok_idx];
            return Some(Token {
                span: Span::new(chunk.base_offset + rt.rel_offset, rt.len),
                kind: rt.kind,
                state_out: rt.state_out,
                flags: rt.flags,
                nest: rt.nest,
                _reserved: 0,
            });
        }
    }

    pub(crate) fn next(&mut self) -> Option<Token> {
        let token = self.peek()?;
        self.advance();
        Some(token)
    }

    fn advance(&mut self) {
        loop {
            let Some(chunk) = self.table.chunks.get(self.chunk_idx) else {
                return;
            };
            if self.tok_idx + 1 < chunk.tokens.len() {
                self.tok_idx += 1;
                return;
            }
            self.chunk_idx += 1;
            self.tok_idx = 0;
            if self.table.chunks.get(self.chunk_idx).is_some() {
                return;
            }
        }
    }
}

fn find_converge_chunk(
    chunks: &[Chunk],
    cursor_new: u32,
    delta: i64,
    state_bits: u16,
    lang_tag: u8,
    nest: u8,
) -> Option<usize> {
    let p_old = (cursor_new as i64 - delta) as u32;
    let idx = chunks
        .binary_search_by_key(&p_old, |c| c.base_offset)
        .ok()?;
    let c = &chunks[idx];
    if c.state_in == state_bits && c.language == lang_tag && c.nest == nest {
        Some(idx)
    } else {
        None
    }
}

/// Iterator yielding [`Token`]s from [`TokenTable::query`].
pub struct QueryIter<'t> {
    chunks: &'t [Chunk],
    chunk_idx: usize,
    tok_idx: usize,
    query_end: u32,
}

impl Iterator for QueryIter<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        loop {
            let chunk = self.chunks.get(self.chunk_idx)?;
            if self.tok_idx >= chunk.tokens.len() {
                self.chunk_idx += 1;
                self.tok_idx = 0;
                continue;
            }
            let rt = chunk.tokens[self.tok_idx];
            self.tok_idx += 1;
            let abs_offset = chunk.base_offset + rt.rel_offset;
            if abs_offset >= self.query_end {
                return None;
            }
            return Some(Token {
                span: Span::new(abs_offset, rt.len),
                kind: rt.kind,
                state_out: rt.state_out,
                flags: rt.flags,
                nest: rt.nest,
                _reserved: 0,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn toks(language: Language, s: &str) -> Vec<Token> {
        let src: &dyn Source = &s;
        let table = TokenTable::new(language, src);
        table.query(Span::new(0, table.source_len())).collect()
    }

    #[test]
    fn empty_source() {
        let s = "";
        let src: &dyn Source = &s;
        let table = TokenTable::new(Language::Json, src);
        assert_eq!(table.source_len(), 0);
        assert_eq!(table.token_count(), 0);
    }

    #[test]
    fn json_coverage_is_contiguous() {
        let input = r#"  {"x": [1, 2.5, true, null], "y": "hello"}  "#;
        let tokens = toks(Language::Json, input);
        let mut pos = 0u32;
        for t in &tokens {
            assert_eq!(t.span.offset, pos);
            pos += t.span.len;
        }
        assert_eq!(pos as usize, input.len());
    }

    #[test]
    fn query_subrange() {
        let input = r#"[1,2,3,4,5]"#;
        let src: &dyn Source = &input;
        let table = TokenTable::new(Language::Json, src);
        let tokens: Vec<_> = table.query(Span::new(3, 5)).collect();
        assert!(!tokens.is_empty());
        for t in &tokens {
            assert!(t.span.overlaps(&Span::new(3, 5)));
        }
    }

    fn flat_tokens(t: &TokenTable) -> Vec<Token> {
        t.query(Span::new(0, t.source_len())).collect()
    }

    fn assert_matches_fresh(old_source: &str, original: Span, replacement: &str) {
        let mut new_source = String::new();
        new_source.push_str(&old_source[..original.offset as usize]);
        new_source.push_str(replacement);
        new_source.push_str(&old_source[original.offset as usize + original.len as usize..]);

        let src_old: &dyn Source = &old_source;
        let mut table = TokenTable::new(Language::Json, src_old);

        let new_ref: &str = &new_source;
        let src_new: &dyn Source = &new_ref;
        let invalidated = table.mutate(src_new, original, replacement.len() as u32);

        assert_eq!(
            table.source_len(),
            new_source.len() as u32,
            "source_len not updated for edit {original:?} -> {replacement:?}"
        );

        let fresh = TokenTable::new(Language::Json, src_new);
        let mutated_tokens = flat_tokens(&table);
        let fresh_tokens = flat_tokens(&fresh);

        assert_eq!(
            mutated_tokens, fresh_tokens,
            "mutate diverged from fresh parse\nold: {old_source:?}\nnew: {new_source:?}\nedit: {original:?} -> {replacement:?}"
        );

        for t in &mutated_tokens {
            let in_edit = t
                .span
                .overlaps(&Span::new(original.offset, replacement.len() as u32));
            let in_invalidated = t.span.overlaps(&invalidated);
            if in_edit {
                assert!(
                    in_invalidated,
                    "edit-overlapping token {:?} not in invalidated span {:?}",
                    t.span, invalidated
                );
            }
        }
    }

    #[test]
    fn mutate_simple_insert() {
        assert_matches_fresh(r#"[1, 2, 3]"#, Span::new(3, 0), "9, ");
    }

    #[test]
    fn mutate_simple_delete() {
        assert_matches_fresh(r#"[1, 2, 3]"#, Span::new(3, 3), "");
    }

    #[test]
    fn mutate_replace_number() {
        assert_matches_fresh(r#"[1, 2, 3]"#, Span::new(1, 1), "42");
    }

    #[test]
    fn mutate_edit_inside_string() {
        assert_matches_fresh(r#"{"a": "hello"}"#, Span::new(8, 3), "owd");
    }

    #[test]
    fn mutate_delete_all() {
        assert_matches_fresh(r#"[1, 2, 3]"#, Span::new(0, 9), "");
    }

    #[test]
    fn mutate_insert_into_empty() {
        assert_matches_fresh("", Span::new(0, 0), r#"{"a": 1}"#);
    }

    #[test]
    fn mutate_large_with_convergence() {
        let piece = r#"[1, 2, 3, 4, 5, 6, 7, 8, 9, 10], "#;
        let mut input = String::from("[");
        for _ in 0..40 {
            input.push_str(piece);
        }
        input.push(']');

        assert_matches_fresh(&input, Span::new(5, 1), "9");
    }

    #[test]
    fn mutate_at_end() {
        let s = r#"[1, 2, 3]"#;
        assert_matches_fresh(s, Span::new(s.len() as u32, 0), ", 4");
    }

    #[test]
    fn mutate_sweep_single_char_inserts() {
        let source = r#"[1, 2.5, "hi", true, null, {"a": 9}]"#;
        for offset in 0..=source.len() {
            for ch in ["x", "9", "\"", " ", "{"] {
                assert_matches_fresh(source, Span::new(offset as u32, 0), ch);
            }
        }
    }

    #[test]
    fn mutate_sweep_single_char_deletes() {
        let source = r#"[1, 2.5, "hi", true, null, {"a": 9}]"#;
        for offset in 0..source.len() {
            assert_matches_fresh(source, Span::new(offset as u32, 1), "");
        }
    }

    #[test]
    fn mutate_sweep_single_char_replaces() {
        let source = r#"[1, 2.5, "hi", true]"#;
        for offset in 0..source.len() {
            for ch in ["x", "9", ":", "}"] {
                assert_matches_fresh(source, Span::new(offset as u32, 1), ch);
            }
        }
    }

    #[test]
    fn mutate_twice() {
        let old = r#"[1, 2, 3]"#;
        let mid = r#"[1, 9, 3]"#;
        let new = r#"[1, 9, 99]"#;

        let s_old: &dyn Source = &old;
        let mut t = TokenTable::new(Language::Json, s_old);
        let mid_ref: &str = &mid;
        let s_mid: &dyn Source = &mid_ref;
        t.mutate(s_mid, Span::new(4, 1), 1);
        let new_ref: &str = &new;
        let s_new: &dyn Source = &new_ref;
        t.mutate(s_new, Span::new(7, 1), 2);

        let fresh = TokenTable::new(Language::Json, s_new);
        assert_eq!(flat_tokens(&t), flat_tokens(&fresh));
    }

    #[test]
    fn multiple_chunks_reconstruct_positions() {
        let piece = r#"[1, 2, 3, 4, 5, 6, 7, 8, 9, 10], "#;
        let mut input = String::from("[");
        for _ in 0..40 {
            input.push_str(piece);
        }
        input.push(']');
        let src: &dyn Source = &input.as_str();
        let table = TokenTable::new(Language::Json, src);
        assert!(table.chunk_count() > 1, "expected multiple chunks");

        let tokens: Vec<Token> = table.query(Span::new(0, table.source_len())).collect();
        let mut pos = 0u32;
        for t in &tokens {
            assert_eq!(t.span.offset, pos, "gap before {pos}");
            pos += t.span.len;
        }
        assert_eq!(pos, table.source_len());
    }
}
