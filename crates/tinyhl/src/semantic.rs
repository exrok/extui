//! Semantic-highlighting overlay derived from a [`TokenTable`].
//!
//! [`SemanticTable`] walks the lexical token stream and tags identifiers
//! with a [`SemanticKind`] such as *type definition*, *function call*, or
//! *parameter*. The table rebuilds incrementally in response to edits,
//! just like [`TokenTable`].
//!
//! Classification is a "good enough" local analysis. It does not parse,
//! resolve names, or consult types. Rust, TypeScript, and C have tailored
//! analyzers. Other languages emit no semantic tokens.

use crate::kind;
use crate::source::Source;
use crate::table::{TokenCursor, TokenMutation};
use crate::token::{kind_lang_tag, kind_local};
use crate::{Language, Span, Token, TokenTable};

const CHUNK_TOKEN_TARGET: usize = 128;

/// Category assigned to an identifier by the semantic analyzer.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SemanticKind {
    /// Name introduced by a type declaration (`struct`, `class`,
    /// `interface`, `enum`, `typedef`, etc.).
    TypeDefinition = 0,
    /// Reference to a type in a type-position context.
    TypeName = 1,
    /// Name introduced by a free-function declaration.
    FunctionDefinition = 2,
    /// Call of a free function.
    FunctionCall = 3,
    /// Name introduced by a method declaration.
    MethodDefinition = 4,
    /// Call of a method through member access.
    MethodCall = 5,
    /// Parameter name in a function or method declaration.
    Parameter = 6,
    /// Identifier passed as an argument to a call.
    Argument = 7,
    /// Name introduced by a local or item-level variable declaration.
    VariableDefinition = 8,
    /// Reference to a variable.
    Variable = 9,
    /// Name introduced by a struct or class field declaration.
    FieldDefinition = 10,
    /// Reference to a field through member access.
    Field = 11,
    /// Segment of a path that is neither a final name nor a type.
    PathComponent = 12,
}

/// Semantic classification for a specific source range.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SemanticToken {
    /// Position and length in the source.
    pub span: Span,
    /// Assigned category.
    pub kind: SemanticKind,
    /// Language tag of the underlying lexical token (see
    /// [`Token::lang_tag`]).
    pub lang_tag: u8,
    /// Embedded-language nesting depth of the underlying lexical token.
    pub nest: u8,
}

#[derive(Clone, Copy, Debug)]
struct RelSemanticToken {
    rel_offset: u32,
    len: u32,
    kind: SemanticKind,
    lang_tag: u8,
    nest: u8,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum StateSnapshot {
    None,
    Rust(RustState),
    Ts(TsState),
    C(CState),
    Py(PyState),
}

impl StateSnapshot {
    fn for_lang(lang: Language) -> Self {
        match lang {
            Language::Rust => Self::Rust(RustState::default()),
            Language::Ts | Language::Tsx => Self::Ts(TsState::default()),
            Language::C => Self::C(CState::default()),
            Language::Python => Self::Py(PyState::default()),
            _ => Self::None,
        }
    }

    /// Clear any indices that refer into the current chunk's
    /// `tokens` vector. Called right before splitting a chunk so
    /// that the clone snapshotted into the new chunk's `state_in`
    /// — and the live `self.state` used to lex the next chunk —
    /// cannot index back into the sealed chunk.
    fn reset_chunk_local(&mut self) {
        match self {
            Self::Rust(state) => {
                state.pending_call = None;
                state.pending_fn_token = None;
            }
            Self::Ts(state) => {
                state.recent_var_def = None;
            }
            Self::Py(state) => {
                state.pending_fn_token = None;
            }
            Self::C(_) | Self::None => {}
        }
    }
}

#[derive(Clone, Debug)]
struct SemanticChunk {
    base_offset: u32,
    end_offset: u32,
    lang_tag: u8,
    nest: u8,
    state_in: StateSnapshot,
    lex_token_count: usize,
    tokens: Vec<RelSemanticToken>,
}

impl SemanticChunk {
    fn new(base_offset: u32, lang_tag: u8, nest: u8, state_in: StateSnapshot) -> Self {
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

/// Semantic overlay kept in sync with a [`TokenTable`].
pub struct SemanticTable {
    source_len: u32,
    root_lang: Language,
    chunks: Vec<SemanticChunk>,
}

impl SemanticTable {
    /// Builds a semantic table from the lexical tokens and source.
    pub fn new(tokens: &TokenTable, src: &dyn Source) -> Self {
        let mut table = Self {
            source_len: tokens.source_len(),
            root_lang: tokens.root_language(),
            chunks: Vec::new(),
        };
        table.rebuild_from(tokens, src, 0, None);
        table
    }

    /// Re-analyzes the semantic stream after a lexical edit and returns
    /// the post-edit span covering every re-emitted semantic token.
    pub fn mutate(&mut self, tokens: &TokenTable, src: &dyn Source, edit: &TokenMutation) -> Span {
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
        let mut new_chunks = Vec::new();

        let mut builder = Builder::new(tokens, src, start_offset);
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

    /// Returns every semantic token whose span starts inside `span`, in
    /// source order.
    pub fn query(&self, span: Span) -> SemanticQueryIter<'_> {
        let chunk_idx = self.chunks.partition_point(|c| c.end_offset <= span.offset);
        SemanticQueryIter {
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

    /// Returns the number of stored semantic tokens.
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
        src: &dyn Source,
        start_offset: u32,
        state: Option<(StateSnapshot, Language, u8)>,
    ) {
        let mut builder = Builder::new(tokens, src, start_offset);
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

/// Iterator yielding [`SemanticToken`]s from [`SemanticTable::query`].
pub struct SemanticQueryIter<'t> {
    chunks: &'t [SemanticChunk],
    chunk_idx: usize,
    tok_idx: usize,
    query_end: u32,
}

impl Iterator for SemanticQueryIter<'_> {
    type Item = SemanticToken;

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
            return Some(SemanticToken {
                span: Span::new(abs_offset, rel.len),
                kind: rel.kind,
                lang_tag: rel.lang_tag,
                nest: rel.nest,
            });
        }
    }
}

struct Builder<'t> {
    cursor: TokenCursor<'t>,
    src: &'t dyn Source,
    current_lang: Language,
    current_nest: u8,
    state: StateSnapshot,
    chunk: Option<SemanticChunk>,
}

impl<'t> Builder<'t> {
    fn new(tokens: &'t TokenTable, src: &'t dyn Source, start_offset: u32) -> Self {
        let cursor = tokens.cursor_at(start_offset);
        let first = cursor.peek();
        let current_lang = first
            .map(|t| Language::from_tag(kind_lang_tag(t.kind)))
            .unwrap_or(tokens.root_language());
        let current_nest = first.map(|t| t.nest).unwrap_or(0);
        let state = StateSnapshot::for_lang(current_lang);
        Self {
            cursor,
            src,
            current_lang,
            current_nest,
            state,
            chunk: None,
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
                    self.state = StateSnapshot::for_lang(token_lang);
                }
                self.chunk = Some(SemanticChunk::new(
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
                self.state.reset_chunk_local();
                return Some(token.span.offset);
            }

            let token = self.cursor.next().unwrap();
            self.current_lang = token_lang;
            self.current_nest = token.nest;
            self.process_token(token);
        }
    }

    fn take_chunk(&mut self) -> Option<SemanticChunk> {
        let next = self.cursor.peek();
        let chunk = self.chunk.take()?;
        let mut chunk = chunk;
        chunk.end_offset = next.map(|t| t.span.offset).unwrap_or(chunk.end_offset);
        Some(chunk)
    }

    fn finish_chunk(&mut self) -> Option<SemanticChunk> {
        let mut chunk = self.chunk.take()?;
        chunk.end_offset = self.cursor.offset().max(chunk.base_offset);
        Some(chunk)
    }

    fn process_token(&mut self, token: Token) {
        if let Some(chunk) = &mut self.chunk {
            chunk.lex_token_count += 1;
            chunk.end_offset = token.span.end();
        }

        let (next_sig, next2_sig) = self.peek_next_sig_pair();

        match &mut self.state {
            StateSnapshot::None => {}
            StateSnapshot::Rust(state) => analyze_rust(
                self.src,
                token,
                next_sig,
                next2_sig,
                state,
                self.chunk.as_mut().unwrap(),
            ),
            StateSnapshot::Ts(state) => analyze_ts(
                self.src,
                token,
                next_sig,
                state,
                self.chunk.as_mut().unwrap(),
            ),
            StateSnapshot::C(state) => analyze_c(
                self.src,
                token,
                next_sig,
                state,
                self.chunk.as_mut().unwrap(),
            ),
            StateSnapshot::Py(state) => analyze_python(
                self.src,
                token,
                next_sig,
                state,
                self.chunk.as_mut().unwrap(),
            ),
        }
    }

    fn peek_next_sig_pair(&self) -> (Option<Token>, Option<Token>) {
        let mut cursor = self.cursor.clone();
        let mut out = [None, None];
        let mut seen = 0usize;
        while let Some(token) = cursor.next() {
            if !is_trivia(token) {
                out[seen] = Some(token);
                seen += 1;
                if seen == 2 {
                    break;
                }
            }
        }
        (out[0], out[1])
    }
}

fn push_semantic(chunk: &mut SemanticChunk, token: Token, kind: SemanticKind) -> usize {
    chunk.tokens.push(RelSemanticToken {
        rel_offset: token.span.offset - chunk.base_offset,
        len: token.span.len,
        kind,
        lang_tag: kind_lang_tag(token.kind),
        nest: token.nest,
    });
    chunk.tokens.len() - 1
}

fn token_eq(src: &dyn Source, token: Token, expected: &[u8]) -> bool {
    if token.span.len as usize != expected.len() {
        return false;
    }
    let mut copied = 0usize;
    let mut offset = token.span.offset;
    while copied < expected.len() {
        let (base, page) = src.page(offset);
        if page.is_empty() {
            return false;
        }
        let rel = (offset - base) as usize;
        let take = (expected.len() - copied).min(page.len().saturating_sub(rel));
        if take == 0 {
            return false;
        }
        if page[rel..rel + take] != expected[copied..copied + take] {
            return false;
        }
        copied += take;
        offset += take as u32;
    }
    true
}

fn is_ident(token: Token, language: Language) -> bool {
    match language {
        Language::Rust => kind_local(token.kind) == kind::IDENT,
        Language::Ts | Language::Tsx => kind_local(token.kind) == kind::IDENT,
        Language::C => kind_local(token.kind) == kind::IDENT,
        Language::Python => kind_local(token.kind) == kind::IDENT,
        _ => false,
    }
}

fn is_trivia(token: Token) -> bool {
    match Language::from_tag(kind_lang_tag(token.kind)) {
        Language::Rust => matches!(
            kind_local(token.kind),
            kind::WHITESPACE | kind::COMMENT | kind::DOC_COMMENT
        ),
        Language::Ts | Language::Tsx => {
            matches!(kind_local(token.kind), kind::WHITESPACE | kind::COMMENT)
        }
        Language::C => matches!(kind_local(token.kind), kind::WHITESPACE | kind::COMMENT),
        Language::Python => matches!(kind_local(token.kind), kind::WHITESPACE | kind::COMMENT),
        _ => false,
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct RustCall {
    depth: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct RustDepthMark {
    paren_depth: usize,
    brace_depth: usize,
    bracket_depth: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RustBraceKind {
    Block,
    RecordFields,
    RecordExpr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct RustTypeContext {
    base: RustDepthMark,
    angle_depth: usize,
    allow_plus_bounds: bool,
    from_turbofish: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct RustPendingCall {
    semantic_idx: usize,
    method: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
struct RustState {
    prev_sig: Option<Token>,
    prev_prev_sig: Option<Token>,
    let_pending: bool,
    let_annotation_base: Option<RustDepthMark>,
    item_annotation_base: Option<RustDepthMark>,
    for_pending: bool,
    fn_pending: bool,
    fn_header_pending: bool,
    type_def_pending: bool,
    type_alias_name_pending: bool,
    type_alias_rhs_pending: bool,
    record_fields_pending: bool,
    const_pending: bool,
    static_pending: bool,
    after_dot: bool,
    paren_depth: usize,
    brace_depth: usize,
    bracket_depth: usize,
    brace_stack: Vec<RustBraceKind>,
    call_stack: Vec<RustCall>,
    fn_param_depth: Option<usize>,
    fn_param_base: Option<RustDepthMark>,
    pending_fn_token: Option<usize>,
    type_context: Option<RustTypeContext>,
    pending_call: Option<RustPendingCall>,
    can_start_value_path: bool,
    record_expr_candidate: bool,
    record_expr_value_depth: Option<usize>,
}

fn rust_depth_mark(state: &RustState) -> RustDepthMark {
    RustDepthMark {
        paren_depth: state.paren_depth,
        brace_depth: state.brace_depth,
        bracket_depth: state.bracket_depth,
    }
}

fn rust_is_at_depth(state: &RustState, mark: RustDepthMark) -> bool {
    rust_depth_mark(state) == mark
}

fn rust_current_brace_kind(state: &RustState) -> Option<RustBraceKind> {
    state.brace_stack.last().copied()
}

fn rust_type_context_is_root(state: &RustState) -> bool {
    state
        .type_context
        .map(|ctx| ctx.angle_depth == 0 && rust_is_at_depth(state, ctx.base))
        .unwrap_or(false)
}

fn rust_start_type_context(state: &mut RustState, allow_plus_bounds: bool, from_turbofish: bool) {
    if let Some(ctx) = &mut state.type_context {
        ctx.allow_plus_bounds |= allow_plus_bounds;
        return;
    }
    state.type_context = Some(RustTypeContext {
        base: rust_depth_mark(state),
        angle_depth: usize::from(from_turbofish),
        allow_plus_bounds,
        from_turbofish,
    });
}

fn rust_name_token(src: &dyn Source, token: Token) -> bool {
    is_ident(token, Language::Rust)
        || (kind_local(token.kind) == kind::KEYWORD
            && matches_keyword_any(src, token, &[b"self", b"Self", b"super", b"crate"]))
}

fn rust_type_keyword(src: &dyn Source, token: Token) -> bool {
    kind_local(token.kind) == kind::KEYWORD
        && matches_keyword_any(
            src,
            token,
            &[
                b"Self", b"self", b"super", b"crate", b"dyn", b"impl", b"fn", b"mut", b"const",
                b"unsafe", b"extern",
            ],
        )
}

fn rust_in_param_root(state: &RustState) -> bool {
    state
        .fn_param_base
        .map(|base| {
            state.fn_param_depth == Some(state.paren_depth) && rust_is_at_depth(state, base)
        })
        .unwrap_or(false)
}

fn rust_should_start_annotation_type(state: &RustState) -> bool {
    state
        .let_annotation_base
        .map(|base| rust_is_at_depth(state, base))
        .unwrap_or(false)
        || state
            .item_annotation_base
            .map(|base| rust_is_at_depth(state, base))
            .unwrap_or(false)
        || rust_in_param_root(state)
}

fn rust_finish_type_context(
    src: &dyn Source,
    token: Token,
    next_sig: Option<Token>,
    next2_sig: Option<Token>,
    state: &mut RustState,
) {
    let Some(ctx) = state.type_context else {
        return;
    };

    if ctx.from_turbofish && ctx.angle_depth == 0 {
        state.type_context = None;
        return;
    }
    let local = kind_local(token.kind);
    let closed_past_base = match local {
        kind::CLOSE_PAREN => {
            ctx.base.paren_depth == state.paren_depth + 1
                && ctx.base.brace_depth == state.brace_depth
                && ctx.base.bracket_depth == state.bracket_depth
        }
        kind::CLOSE_BRACKET => {
            ctx.base.bracket_depth == state.bracket_depth + 1
                && ctx.base.paren_depth == state.paren_depth
                && ctx.base.brace_depth == state.brace_depth
        }
        kind::CLOSE_BRACE => {
            ctx.base.brace_depth == state.brace_depth + 1
                && ctx.base.paren_depth == state.paren_depth
                && ctx.base.bracket_depth == state.bracket_depth
        }
        _ => false,
    };
    if closed_past_base {
        state.type_context = None;
        return;
    }
    if !rust_type_context_is_root(state) {
        return;
    }

    let keep = match local {
        kind::IDENT | kind::LIFETIME => true,
        kind::KEYWORD => rust_type_keyword(src, token),
        kind::LT | kind::GT | kind::OPEN_PAREN | kind::OPEN_BRACKET => true,
        kind::COLON | kind::AMP | kind::STAR | kind::BANG | kind::QUESTION => true,
        kind::MINUS => matches!(next_sig.map(|t| kind_local(t.kind)), Some(kind::GT)),
        kind::PLUS => ctx.allow_plus_bounds,
        kind::EQ => ctx.angle_depth > 0,
        kind::CLOSE_PAREN => {
            matches!(next_sig.map(|t| kind_local(t.kind)), Some(kind::MINUS))
                && matches!(next2_sig.map(|t| kind_local(t.kind)), Some(kind::GT))
        }
        kind::CLOSE_BRACKET => {
            matches!(next_sig.map(|t| kind_local(t.kind)), Some(kind::COLON))
                && matches!(next2_sig.map(|t| kind_local(t.kind)), Some(kind::COLON))
        }
        _ => false,
    };
    if !keep {
        state.type_context = None;
    }
}

fn analyze_rust(
    src: &dyn Source,
    token: Token,
    next_sig: Option<Token>,
    next2_sig: Option<Token>,
    state: &mut RustState,
    chunk: &mut SemanticChunk,
) {
    let local = kind_local(token.kind);
    if is_trivia(token) {
        return;
    }
    let type_root_before = rust_type_context_is_root(state);

    match local {
        kind::OPEN_PAREN => {
            state.paren_depth += 1;
            if state.fn_header_pending {
                state.fn_param_depth = Some(state.paren_depth);
                state.fn_param_base = Some(rust_depth_mark(state));
                state.fn_header_pending = false;
            } else if matches!(state.prev_sig.map(|t| kind_local(t.kind)), Some(kind::GT)) {
                if let Some(pending) = state.pending_call.take() {
                    chunk.tokens[pending.semantic_idx].kind = if pending.method {
                        SemanticKind::MethodCall
                    } else {
                        SemanticKind::FunctionCall
                    };
                    state.call_stack.push(RustCall {
                        depth: state.paren_depth,
                    });
                }
            } else if let Some(prev) = state.prev_sig {
                if rust_name_token(src, prev) {
                    state.call_stack.push(RustCall {
                        depth: state.paren_depth,
                    });
                }
            }
            state.after_dot = false;
            state.can_start_value_path = true;
            state.record_expr_candidate = false;
        }
        kind::CLOSE_PAREN => {
            if state.paren_depth > 0 {
                if state.fn_param_depth == Some(state.paren_depth) {
                    state.fn_param_depth = None;
                    state.fn_param_base = None;
                    state.pending_fn_token = None;
                }
                while state
                    .call_stack
                    .last()
                    .map(|c| c.depth >= state.paren_depth)
                    .unwrap_or(false)
                {
                    state.call_stack.pop();
                }
                state.paren_depth -= 1;
            }
            state.after_dot = false;
            state.can_start_value_path = false;
            state.record_expr_candidate = false;
        }
        kind::OPEN_BRACKET => {
            state.bracket_depth += 1;
            state.after_dot = false;
            state.can_start_value_path = true;
            state.record_expr_candidate = false;
        }
        kind::CLOSE_BRACKET => {
            if state.bracket_depth > 0 {
                state.bracket_depth -= 1;
            }
            state.after_dot = false;
            state.can_start_value_path = false;
            state.record_expr_candidate = false;
        }
        kind::OPEN_BRACE => {
            if type_root_before {
                state.type_context = None;
            }
            state.brace_depth += 1;
            let brace_kind = if state.record_fields_pending {
                state.record_fields_pending = false;
                RustBraceKind::RecordFields
            } else if state.record_expr_candidate {
                RustBraceKind::RecordExpr
            } else {
                RustBraceKind::Block
            };
            state.brace_stack.push(brace_kind);
            state.after_dot = false;
            state.can_start_value_path = true;
            state.record_expr_candidate = false;
        }
        kind::CLOSE_BRACE => {
            if state.brace_depth > 0 {
                state.brace_depth -= 1;
            }
            state.brace_stack.pop();
            if state
                .record_expr_value_depth
                .map(|depth| depth > state.brace_depth)
                .unwrap_or(false)
            {
                state.record_expr_value_depth = None;
            }
            state.after_dot = false;
            state.can_start_value_path = false;
            state.record_expr_candidate = false;
        }
        kind::DOT => {
            state.after_dot = true;
            state.pending_call = None;
            state.can_start_value_path = false;
            state.record_expr_candidate = false;
        }
        kind::COLON => {
            if matches!(next_sig.map(|t| kind_local(t.kind)), Some(kind::COLON)) {
                if !matches!(next2_sig.map(|t| kind_local(t.kind)), Some(kind::LT)) {
                    state.pending_call = None;
                }
                state.after_dot = false;
            } else if rust_current_brace_kind(state) == Some(RustBraceKind::RecordFields) {
                rust_start_type_context(state, false, false);
                state.after_dot = false;
                state.can_start_value_path = false;
                state.record_expr_candidate = false;
            } else if rust_current_brace_kind(state) == Some(RustBraceKind::RecordExpr) {
                state.after_dot = false;
                state.can_start_value_path = true;
                state.record_expr_candidate = false;
                state.record_expr_value_depth = Some(state.brace_depth);
            } else if rust_should_start_annotation_type(state) {
                rust_start_type_context(state, false, false);
                state.after_dot = false;
                state.can_start_value_path = false;
                state.record_expr_candidate = false;
            } else {
                state.after_dot = false;
                state.can_start_value_path = false;
                state.record_expr_candidate = false;
            }
        }
        kind::LT => {
            if let Some(ctx) = &mut state.type_context {
                ctx.angle_depth += 1;
            } else if matches!(
                state.prev_sig.map(|t| kind_local(t.kind)),
                Some(kind::COLON)
            ) && matches!(
                state.prev_prev_sig.map(|t| kind_local(t.kind)),
                Some(kind::COLON)
            ) {
                rust_start_type_context(state, false, true);
            }
            state.after_dot = false;
            state.can_start_value_path = false;
        }
        kind::MINUS => {
            state.after_dot = false;
            state.can_start_value_path = false;
            state.record_expr_candidate = false;
        }
        kind::GT
            if matches!(
                state.prev_sig.map(|t| kind_local(t.kind)),
                Some(kind::MINUS)
            ) =>
        {
            rust_start_type_context(state, false, false);
            state.after_dot = false;
            state.can_start_value_path = false;
            state.record_expr_candidate = false;
        }
        kind::GT => {
            if let Some(ctx) = &mut state.type_context {
                if ctx.angle_depth > 0 {
                    ctx.angle_depth -= 1;
                }
            }
            state.after_dot = false;
            state.can_start_value_path = false;
        }
        kind::COMMA => {
            state.after_dot = false;
            state.can_start_value_path = true;
            state.record_expr_candidate = false;
            state.record_expr_value_depth = None;
            if !state
                .type_context
                .map(|ctx| ctx.from_turbofish)
                .unwrap_or(false)
            {
                state.pending_call = None;
            }
        }
        kind::SEMI => {
            state.let_annotation_base = None;
            state.item_annotation_base = None;
            state.type_alias_rhs_pending = false;
            state.record_fields_pending = false;
            state.const_pending = false;
            state.static_pending = false;
            state.after_dot = false;
            state.can_start_value_path = true;
            state.record_expr_candidate = false;
            state.pending_call = None;
        }
        kind::EQ => {
            if state.type_alias_rhs_pending {
                rust_start_type_context(state, false, false);
                state.type_alias_rhs_pending = false;
            }
            state.let_annotation_base = None;
            state.item_annotation_base = None;
            state.after_dot = false;
            state.can_start_value_path = true;
            state.record_expr_candidate = false;
            state.pending_call = None;
        }
        _ if rust_name_token(src, token) => {
            let in_turbofish = state
                .type_context
                .map(|ctx| ctx.from_turbofish)
                .unwrap_or(false);
            let in_record_expr_value = state.record_expr_value_depth == Some(state.brace_depth);
            let direct_call =
                matches!(next_sig.map(|t| kind_local(t.kind)), Some(kind::OPEN_PAREN));
            let path_sep = matches!(next_sig.map(|t| kind_local(t.kind)), Some(kind::COLON))
                && matches!(next2_sig.map(|t| kind_local(t.kind)), Some(kind::COLON));
            let record_field_def = rust_current_brace_kind(state)
                == Some(RustBraceKind::RecordFields)
                && matches!(next_sig.map(|t| kind_local(t.kind)), Some(kind::COLON));
            let record_field_use = !in_record_expr_value
                && rust_current_brace_kind(state) == Some(RustBraceKind::RecordExpr)
                && matches!(
                    next_sig.map(|t| kind_local(t.kind)),
                    Some(kind::COLON | kind::COMMA | kind::CLOSE_BRACE)
                );
            let record_ctor = state.can_start_value_path
                && !state.after_dot
                && matches!(next_sig.map(|t| kind_local(t.kind)), Some(kind::OPEN_BRACE));

            let kind = if state.fn_pending {
                let idx = push_semantic(chunk, token, SemanticKind::FunctionDefinition);
                state.pending_fn_token = Some(idx);
                state.fn_pending = false;
                state.fn_header_pending = true;
                None
            } else if state.type_def_pending {
                state.type_def_pending = false;
                if state.type_alias_name_pending {
                    state.type_alias_rhs_pending = true;
                    state.type_alias_name_pending = false;
                }
                Some(SemanticKind::TypeDefinition)
            } else if state.const_pending || state.static_pending {
                state.const_pending = false;
                state.static_pending = false;
                state.item_annotation_base = Some(rust_depth_mark(state));
                Some(SemanticKind::VariableDefinition)
            } else if record_field_def {
                Some(SemanticKind::FieldDefinition)
            } else if state.let_pending || state.for_pending {
                if state.let_pending {
                    state.let_annotation_base = Some(rust_depth_mark(state));
                }
                state.let_pending = false;
                state.for_pending = false;
                Some(SemanticKind::VariableDefinition)
            } else if state.type_context.is_some() {
                if path_sep {
                    Some(SemanticKind::PathComponent)
                } else {
                    Some(SemanticKind::TypeName)
                }
            } else if rust_in_param_root(state) {
                Some(SemanticKind::Parameter)
            } else if state.after_dot {
                if direct_call {
                    Some(SemanticKind::MethodCall)
                } else {
                    Some(SemanticKind::Field)
                }
            } else if record_field_use {
                Some(SemanticKind::Field)
            } else if path_sep {
                Some(SemanticKind::PathComponent)
            } else if direct_call {
                Some(SemanticKind::FunctionCall)
            } else if record_ctor {
                Some(SemanticKind::TypeName)
            } else if state
                .call_stack
                .last()
                .map(|c| c.depth == state.paren_depth)
                .unwrap_or(false)
            {
                Some(SemanticKind::Argument)
            } else {
                Some(SemanticKind::Variable)
            };
            if let Some(kind) = kind {
                let idx = push_semantic(chunk, token, kind);
                if path_sep {
                    state.pending_call = Some(RustPendingCall {
                        semantic_idx: idx,
                        method: state.after_dot,
                    });
                } else if !in_turbofish {
                    state.pending_call = None;
                }
            } else if !in_turbofish {
                state.pending_call = None;
            }
            state.record_expr_candidate =
                state.can_start_value_path && !state.after_dot && (path_sep || record_ctor);
            state.after_dot = false;
            if matches!(
                chunk.tokens.last().map(|t| t.kind),
                Some(SemanticKind::Parameter)
            ) && token_eq(src, token, b"self")
            {
                if let Some(idx) = state.pending_fn_token {
                    chunk.tokens[idx].kind = SemanticKind::MethodDefinition;
                }
            }
            state.can_start_value_path = false;
            if in_record_expr_value {
                state.record_expr_value_depth = None;
            }
        }
        kind::KEYWORD => {
            state.can_start_value_path = false;
            if token_eq(src, token, b"let") {
                state.let_pending = true;
                state.let_annotation_base = None;
            } else if token_eq(src, token, b"for") {
                if rust_type_context_is_root(state) {
                    rust_start_type_context(state, false, false);
                } else {
                    state.for_pending = true;
                }
            } else if token_eq(src, token, b"fn") {
                if state.type_context.is_none() {
                    state.fn_pending = true;
                }
            } else if matches_keyword_any(
                src,
                token,
                &[b"struct", b"enum", b"trait", b"type", b"union"],
            ) {
                if state.type_context.is_none() {
                    state.type_def_pending = true;
                    state.type_alias_name_pending = token_eq(src, token, b"type");
                    state.record_fields_pending =
                        matches_keyword_any(src, token, &[b"struct", b"union"]);
                }
            } else if token_eq(src, token, b"const") {
                if state.type_context.is_none() {
                    state.const_pending = true;
                }
            } else if token_eq(src, token, b"static") {
                if state.type_context.is_none() {
                    state.static_pending = true;
                }
            } else if matches_keyword_any(src, token, &[b"as", b"impl"]) {
                rust_start_type_context(state, false, false);
            } else if token_eq(src, token, b"dyn") {
                rust_start_type_context(state, true, false);
            } else if matches_keyword_any(src, token, &[b"return", b"break"]) {
                state.can_start_value_path = true;
            } else if token_eq(src, token, b"in") {
                state.for_pending = false;
                state.can_start_value_path = true;
            }
            state.after_dot = false;
            state.record_expr_candidate = false;
            if state.record_expr_value_depth == Some(state.brace_depth) {
                state.record_expr_value_depth = None;
            }
        }
        _ => {
            state.after_dot = false;
            state.can_start_value_path = false;
            state.record_expr_candidate = false;
            if !state
                .type_context
                .map(|ctx| ctx.from_turbofish)
                .unwrap_or(false)
            {
                state.pending_call = None;
            }
            if state.record_expr_value_depth == Some(state.brace_depth) {
                state.record_expr_value_depth = None;
            }
        }
    }

    rust_finish_type_context(src, token, next_sig, next2_sig, state);
    state.prev_prev_sig = state.prev_sig;
    state.prev_sig = Some(token);
}

fn matches_keyword_any(src: &dyn Source, token: Token, set: &[&[u8]]) -> bool {
    set.iter().any(|kw| token_eq(src, token, kw))
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct TsCall {
    depth: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
struct TsState {
    prev_sig: Option<Token>,
    var_pending: bool,
    fn_pending: bool,
    param_header_pending: bool,
    class_pending: bool,
    interface_pending: bool,
    class_body_pending: bool,
    interface_body_pending: bool,
    type_pending: bool,
    enum_pending: bool,
    pending_type: bool,
    after_dot: bool,
    paren_depth: usize,
    call_stack: Vec<TsCall>,
    param_depth: Option<usize>,
    class_body_depth: Option<usize>,
    interface_body_depth: Option<usize>,
    brace_depth: usize,
    recent_var_def: Option<usize>,
}

fn analyze_ts(
    src: &dyn Source,
    token: Token,
    next_sig: Option<Token>,
    state: &mut TsState,
    chunk: &mut SemanticChunk,
) {
    let local = kind_local(token.kind);
    if is_trivia(token) {
        return;
    }

    match local {
        kind::OPEN_BRACE => {
            state.brace_depth += 1;
            if state.class_body_pending {
                state.class_body_depth = Some(state.brace_depth);
                state.class_body_pending = false;
            }
            if state.interface_body_pending {
                state.interface_body_depth = Some(state.brace_depth);
                state.interface_body_pending = false;
            }
            state.prev_sig = Some(token);
            return;
        }
        kind::CLOSE_BRACE => {
            if state.class_body_depth == Some(state.brace_depth) {
                state.class_body_depth = None;
            }
            if state.interface_body_depth == Some(state.brace_depth) {
                state.interface_body_depth = None;
            }
            if state.brace_depth > 0 {
                state.brace_depth -= 1;
            }
        }
        kind::OPEN_PAREN => {
            state.paren_depth += 1;
            if state.param_header_pending {
                state.param_depth = Some(state.paren_depth);
                state.param_header_pending = false;
            } else if let Some(prev) = state.prev_sig {
                if is_ident(prev, Language::Ts) {
                    state.call_stack.push(TsCall {
                        depth: state.paren_depth,
                    });
                }
            }
            state.prev_sig = Some(token);
            return;
        }
        kind::CLOSE_PAREN => {
            if state.param_depth == Some(state.paren_depth) {
                state.param_depth = None;
            }
            while state
                .call_stack
                .last()
                .map(|c| c.depth >= state.paren_depth)
                .unwrap_or(false)
            {
                state.call_stack.pop();
            }
            if state.paren_depth > 0 {
                state.paren_depth -= 1;
            }
        }
        _ => {}
    }

    match local {
        kind::KEYWORD => {
            state.var_pending = matches_keyword_any(src, token, &[b"const", b"let", b"var"]);
            state.fn_pending = token_eq(src, token, b"function");
            state.class_pending = token_eq(src, token, b"class");
            state.interface_pending = token_eq(src, token, b"interface");
            if state.class_pending {
                state.class_body_pending = true;
            }
            if state.interface_pending {
                state.interface_body_pending = true;
            }
            state.type_pending = token_eq(src, token, b"type");
            state.enum_pending = token_eq(src, token, b"enum");
            state.pending_type =
                matches_keyword_any(src, token, &[b"as", b"extends", b"implements", b"new"]);
            state.after_dot = false;
        }
        kind::DOT | kind::OPTIONAL_CHAIN => {
            state.after_dot = true;
        }
        kind::COLON => {
            state.pending_type = true;
            state.after_dot = false;
        }
        kind::FAT_ARROW => {
            if let Some(idx) = state.recent_var_def.take() {
                chunk.tokens[idx].kind = SemanticKind::FunctionDefinition;
            }
            state.pending_type = false;
            state.after_dot = false;
        }
        kind::SEMI | kind::EQ | kind::COMMA => {
            state.after_dot = false;
        }
        _ if is_ident(token, Language::Ts) => {
            let emitted = if state.class_pending
                || state.interface_pending
                || state.type_pending
                || state.enum_pending
            {
                let emitted = Some(push_semantic(chunk, token, SemanticKind::TypeDefinition));
                state.class_pending = false;
                state.interface_pending = false;
                state.type_pending = false;
                state.enum_pending = false;
                emitted
            } else if state.fn_pending {
                let emitted = Some(push_semantic(
                    chunk,
                    token,
                    SemanticKind::FunctionDefinition,
                ));
                state.fn_pending = false;
                state.param_header_pending = true;
                emitted
            } else if state.var_pending {
                let idx = push_semantic(chunk, token, SemanticKind::VariableDefinition);
                state.recent_var_def = Some(idx);
                state.var_pending = false;
                Some(idx)
            } else if state.param_depth == Some(state.paren_depth) {
                Some(push_semantic(chunk, token, SemanticKind::Parameter))
            } else if state.after_dot {
                Some(push_semantic(chunk, token, SemanticKind::Field))
            } else if state.pending_type {
                Some(push_semantic(chunk, token, SemanticKind::TypeName))
            } else if state.class_body_depth == Some(state.brace_depth)
                || state.interface_body_depth == Some(state.brace_depth)
            {
                let kind = if matches!(next_sig.map(|t| kind_local(t.kind)), Some(kind::OPEN_PAREN))
                {
                    SemanticKind::MethodDefinition
                } else {
                    SemanticKind::FieldDefinition
                };
                let emitted = Some(push_semantic(chunk, token, kind));
                if kind == SemanticKind::MethodDefinition {
                    state.param_header_pending = true;
                }
                emitted
            } else if state
                .call_stack
                .last()
                .map(|c| c.depth == state.paren_depth)
                .unwrap_or(false)
            {
                Some(push_semantic(chunk, token, SemanticKind::Argument))
            } else {
                Some(push_semantic(chunk, token, SemanticKind::Variable))
            };

            if let Some(idx) = emitted {
                if matches!(next_sig.map(|t| kind_local(t.kind)), Some(kind::OPEN_PAREN)) {
                    if state.after_dot {
                        chunk.tokens[idx].kind = SemanticKind::MethodCall;
                    } else if !matches!(
                        chunk.tokens[idx].kind,
                        SemanticKind::FunctionDefinition
                            | SemanticKind::MethodDefinition
                            | SemanticKind::FieldDefinition
                            | SemanticKind::TypeDefinition
                            | SemanticKind::TypeName
                    ) {
                        chunk.tokens[idx].kind = SemanticKind::FunctionCall;
                    }
                }
            }
            state.pending_type = false;
            state.after_dot = false;
        }
        _ => {
            state.pending_type = false;
            state.after_dot = false;
        }
    }

    state.prev_sig = Some(token);
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
struct CState {
    prev_sig: Option<Token>,
    param_depth: Option<usize>,
    paren_depth: usize,
    brace_depth: usize,
    struct_tag_pending: bool,
    enum_tag_pending: bool,
    struct_body_pending: bool,
    struct_body_depth: Option<usize>,
    decl_mode: bool,
    after_member: bool,
}

fn analyze_c(
    src: &dyn Source,
    token: Token,
    next_sig: Option<Token>,
    state: &mut CState,
    chunk: &mut SemanticChunk,
) {
    let local = kind_local(token.kind);
    if is_trivia(token) {
        return;
    }

    match local {
        kind::OPEN_BRACE if state.struct_body_pending => {
            state.brace_depth += 1;
            state.struct_body_depth = Some(state.brace_depth);
            state.struct_body_pending = false;
        }
        kind::OPEN_BRACE => state.brace_depth += 1,
        kind::CLOSE_BRACE => {
            if state.struct_body_depth == Some(state.brace_depth) {
                state.struct_body_depth = None;
            }
            if state.brace_depth > 0 {
                state.brace_depth -= 1;
            }
        }
        kind::OPEN_PAREN => {
            state.paren_depth += 1;
            if state.decl_mode && state.param_depth.is_none() {
                state.param_depth = Some(state.paren_depth);
            }
        }
        kind::CLOSE_PAREN => {
            if state.param_depth == Some(state.paren_depth) {
                state.param_depth = None;
            }
            if state.paren_depth > 0 {
                state.paren_depth -= 1;
            }
        }
        _ => {}
    }

    match local {
        kind::KEYWORD => {
            state.struct_tag_pending =
                token_eq(src, token, b"struct") || token_eq(src, token, b"union");
            state.enum_tag_pending = token_eq(src, token, b"enum");
            state.struct_body_pending = state.struct_tag_pending;
            state.decl_mode = matches_keyword_any(
                src,
                token,
                &[
                    b"char",
                    b"short",
                    b"int",
                    b"long",
                    b"float",
                    b"double",
                    b"void",
                    b"unsigned",
                    b"signed",
                    b"const",
                    b"volatile",
                    b"static",
                    b"extern",
                    b"typedef",
                    b"struct",
                    b"union",
                    b"enum",
                ],
            );
            state.after_member = false;
        }
        kind::DOT | kind::THIN_ARROW => {
            state.after_member = true;
        }
        kind::SEMI => {
            state.decl_mode = false;
            state.after_member = false;
        }
        _ if is_ident(token, Language::C) => {
            if state.struct_tag_pending || state.enum_tag_pending {
                push_semantic(chunk, token, SemanticKind::TypeDefinition);
                state.struct_tag_pending = false;
                state.enum_tag_pending = false;
            } else if state.after_member {
                push_semantic(chunk, token, SemanticKind::Field);
                state.after_member = false;
            } else if state.param_depth == Some(state.paren_depth) {
                push_semantic(chunk, token, SemanticKind::Parameter);
            } else if state.struct_body_depth == Some(state.brace_depth) {
                push_semantic(chunk, token, SemanticKind::FieldDefinition);
            } else if state.decl_mode {
                if matches!(next_sig.map(|t| kind_local(t.kind)), Some(kind::OPEN_PAREN)) {
                    push_semantic(chunk, token, SemanticKind::FunctionDefinition);
                } else {
                    push_semantic(chunk, token, SemanticKind::VariableDefinition);
                }
            } else {
                if matches!(next_sig.map(|t| kind_local(t.kind)), Some(kind::OPEN_PAREN)) {
                    push_semantic(chunk, token, SemanticKind::FunctionCall);
                } else {
                    push_semantic(chunk, token, SemanticKind::Variable);
                }
            }
        }
        _ => {
            state.after_member = false;
        }
    }

    state.prev_sig = Some(token);
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct PyCall {
    depth: usize,
}

/// Local analysis state for Python.
///
/// Python has no block delimiters, so methods are told apart from free
/// functions by their first parameter (`self`/`cls`) rather than by a class
/// body. Logical-line starts are tracked through newline-bearing whitespace
/// (gated on zero bracket depth so implicit line joins do not count) to spot
/// statement-level assignment targets.
///
/// Every field is offset-free — depths, counts, and booleans only — so the
/// snapshot taken at a chunk boundary compares equal whenever two builds reach
/// the same logical point, keeping `mutate` convergent. The one index field,
/// `pending_fn_token`, points into the current chunk and is cleared by
/// [`StateSnapshot::reset_chunk_local`] before any split.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
struct PyState {
    started: bool,
    at_line_start: bool,
    after_dot: bool,
    def_pending: bool,
    class_pending: bool,
    param_header_pending: bool,
    method_check_pending: bool,
    pending_fn_token: Option<usize>,
    class_base_pending: bool,
    class_base_paren: Option<usize>,
    lambda_params_pending: bool,
    for_target_pending: bool,
    as_pending: bool,
    decorator_pending: bool,
    annotate_pending: bool,
    pending_type: bool,
    prev_was_ident: bool,
    paren_depth: usize,
    bracket_depth: usize,
    brace_depth: usize,
    param_depth: Option<usize>,
    call_stack: Vec<PyCall>,
}

fn py_depth_all_zero(state: &PyState) -> bool {
    state.paren_depth == 0 && state.bracket_depth == 0 && state.brace_depth == 0
}

fn py_is_assign_op(local: u16) -> bool {
    matches!(
        local,
        kind::EQ
            | kind::PLUS_EQ
            | kind::MINUS_EQ
            | kind::STAR_EQ
            | kind::SLASH_EQ
            | kind::PERCENT_EQ
            | kind::STAR_STAR_EQ
            | kind::SLASH_SLASH_EQ
            | kind::SHL_EQ
            | kind::SHR_EQ
            | kind::AMP_EQ
            | kind::PIPE_EQ
            | kind::CARET_EQ
            | kind::AT_EQ
    )
}

/// Returns `true` if any byte of `token`'s span is a newline.
fn token_has_newline(src: &dyn Source, token: Token) -> bool {
    let mut offset = token.span.offset;
    let end = token.span.end();
    while offset < end {
        let (base, page) = src.page(offset);
        if page.is_empty() {
            return false;
        }
        let rel = (offset - base) as usize;
        let take = ((end - offset) as usize).min(page.len().saturating_sub(rel));
        if take == 0 {
            return false;
        }
        if page[rel..rel + take].contains(&b'\n') {
            return true;
        }
        offset += take as u32;
    }
    false
}

fn analyze_python(
    src: &dyn Source,
    token: Token,
    next_sig: Option<Token>,
    state: &mut PyState,
    chunk: &mut SemanticChunk,
) {
    let local = kind_local(token.kind);

    // Trivia is not significant, but a newline at bracket depth zero opens a
    // new logical line — record it for the next real token without disturbing
    // any other state.
    if local == kind::WHITESPACE {
        if py_depth_all_zero(state) && token_has_newline(src, token) {
            state.at_line_start = true;
        }
        return;
    }
    if local == kind::COMMENT {
        return;
    }

    let line_head = state.at_line_start || !state.started;
    state.started = true;
    state.at_line_start = false;

    let next_local = next_sig.map(|t| kind_local(t.kind));
    let is_call = next_local == Some(kind::OPEN_PAREN);
    let next_is_assign = next_local.map(py_is_assign_op).unwrap_or(false);

    match local {
        kind::OPEN_PAREN => {
            state.paren_depth += 1;
            if state.param_header_pending {
                state.param_depth = Some(state.paren_depth);
                state.method_check_pending = true;
                state.param_header_pending = false;
            } else if state.class_base_pending {
                state.class_base_paren = Some(state.paren_depth);
                state.pending_type = true;
                state.class_base_pending = false;
            } else if state.prev_was_ident {
                state.call_stack.push(PyCall {
                    depth: state.paren_depth,
                });
            }
            state.after_dot = false;
        }
        kind::CLOSE_PAREN => {
            if state.param_depth == Some(state.paren_depth) {
                state.param_depth = None;
                state.method_check_pending = false;
                state.pending_fn_token = None;
            }
            if state.class_base_paren == Some(state.paren_depth) {
                state.class_base_paren = None;
                state.pending_type = false;
            }
            while state
                .call_stack
                .last()
                .map(|c| c.depth >= state.paren_depth)
                .unwrap_or(false)
            {
                state.call_stack.pop();
            }
            if state.paren_depth > 0 {
                state.paren_depth -= 1;
            }
            state.after_dot = false;
            state.pending_type = false;
        }
        kind::OPEN_BRACKET => {
            state.bracket_depth += 1;
            state.after_dot = false;
        }
        kind::CLOSE_BRACKET => {
            if state.bracket_depth > 0 {
                state.bracket_depth -= 1;
            }
            state.after_dot = false;
        }
        kind::OPEN_BRACE => {
            state.brace_depth += 1;
            state.after_dot = false;
            state.pending_type = false;
        }
        kind::CLOSE_BRACE => {
            if state.brace_depth > 0 {
                state.brace_depth -= 1;
            }
            state.after_dot = false;
        }
        kind::DOT => {
            state.after_dot = true;
        }
        kind::THIN_ARROW => {
            state.pending_type = true;
            state.after_dot = false;
        }
        kind::COLON => {
            let annotate = state.annotate_pending;
            state.annotate_pending = false;
            state.for_target_pending = false;
            state.lambda_params_pending = false;
            state.after_dot = false;
            // An annotation colon opens a type context; a block, dict, slice,
            // or lambda colon ends one.
            state.pending_type = annotate;
        }
        kind::COMMA => {
            state.after_dot = false;
            // Keep the type context across commas only inside a subscript
            // (`Dict[str, int]`) or a class base list (`class C(A, B)`).
            state.pending_type = state.pending_type
                && (state.bracket_depth > 0 || state.class_base_paren == Some(state.paren_depth));
        }
        kind::AT if line_head => {
            state.decorator_pending = true;
            state.after_dot = false;
        }
        _ if py_is_assign_op(local) => {
            state.after_dot = false;
            state.pending_type = false;
        }
        kind::KEYWORD => {
            state.after_dot = false;
            state.pending_type = false;
            state.decorator_pending = false;
            if token_eq(src, token, b"def") {
                state.def_pending = true;
            } else if token_eq(src, token, b"class") {
                state.class_pending = true;
            } else if token_eq(src, token, b"lambda") {
                state.lambda_params_pending = true;
            } else if token_eq(src, token, b"for") {
                state.for_target_pending = true;
            } else if token_eq(src, token, b"in") {
                state.for_target_pending = false;
            } else if token_eq(src, token, b"as") {
                state.as_pending = true;
            }
        }
        _ if is_ident(token, Language::Python) => {
            let kind = if next_local == Some(kind::COLON_EQ) {
                // Walrus binding (`n := ...`), valid anywhere.
                Some(SemanticKind::VariableDefinition)
            } else if state.def_pending {
                state.def_pending = false;
                if next_local == Some(kind::OPEN_PAREN) {
                    state.param_header_pending = true;
                }
                let idx = push_semantic(chunk, token, SemanticKind::FunctionDefinition);
                state.pending_fn_token = Some(idx);
                None
            } else if state.class_pending {
                state.class_pending = false;
                if next_local == Some(kind::OPEN_PAREN) {
                    state.class_base_pending = true;
                }
                Some(SemanticKind::TypeDefinition)
            } else if state.as_pending {
                state.as_pending = false;
                Some(SemanticKind::VariableDefinition)
            } else if state.decorator_pending {
                state.decorator_pending = false;
                if next_local == Some(kind::DOT) {
                    Some(SemanticKind::Variable)
                } else {
                    Some(SemanticKind::FunctionCall)
                }
            } else if state.lambda_params_pending {
                Some(SemanticKind::Parameter)
            } else if state.for_target_pending {
                Some(SemanticKind::VariableDefinition)
            } else if state.after_dot {
                if is_call {
                    Some(SemanticKind::MethodCall)
                } else if next_is_assign {
                    Some(SemanticKind::FieldDefinition)
                } else {
                    Some(SemanticKind::Field)
                }
            } else if state.param_depth == Some(state.paren_depth) {
                if state.method_check_pending {
                    state.method_check_pending = false;
                    if token_eq(src, token, b"self") || token_eq(src, token, b"cls") {
                        if let Some(idx) = state.pending_fn_token {
                            chunk.tokens[idx].kind = SemanticKind::MethodDefinition;
                        }
                    }
                }
                if next_local == Some(kind::COLON) {
                    state.annotate_pending = true;
                }
                Some(SemanticKind::Parameter)
            } else if state.pending_type {
                Some(SemanticKind::TypeName)
            } else if line_head && py_depth_all_zero(state) {
                if is_call {
                    Some(SemanticKind::FunctionCall)
                } else if next_is_assign {
                    Some(SemanticKind::VariableDefinition)
                } else if next_local == Some(kind::COLON) {
                    state.annotate_pending = true;
                    Some(SemanticKind::VariableDefinition)
                } else {
                    Some(SemanticKind::Variable)
                }
            } else if is_call {
                Some(SemanticKind::FunctionCall)
            } else if state
                .call_stack
                .last()
                .map(|c| c.depth == state.paren_depth)
                .unwrap_or(false)
            {
                Some(SemanticKind::Argument)
            } else {
                Some(SemanticKind::Variable)
            };

            if let Some(kind) = kind {
                push_semantic(chunk, token, kind);
            }
            state.after_dot = false;
        }
        _ => {
            state.after_dot = false;
            state.pending_type = false;
        }
    }

    state.prev_was_ident = is_ident(token, Language::Python);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn apply(old: &str, span: Span, new: &str) -> String {
        let mut s = String::new();
        s.push_str(&old[..span.offset as usize]);
        s.push_str(new);
        s.push_str(&old[span.offset as usize + span.len as usize..]);
        s
    }

    #[test]
    fn mutate_shifts_converged_tail_chunks() {
        let mut source = String::from("[");
        for i in 0..300 {
            if i > 0 {
                source.push(',');
            }
            source.push_str(&format!("\"value_{i}\""));
        }
        source.push(']');

        let old_ref = source.as_str();
        let old_src: &dyn Source = &old_ref;
        let mut tokens = TokenTable::new(Language::Json, old_src);
        let mut semantic = SemanticTable::new(&tokens, old_src);
        assert!(semantic.chunk_count() > 2);

        let off = source.find("value_140").unwrap() + "value".len();
        let edit = Span::new(off as u32, 0);
        let new = apply(&source, edit, "x");
        let new_ref = new.as_str();
        let new_src: &dyn Source = &new_ref;
        let token_edit = tokens.mutate_detailed(new_src, edit, 1);
        semantic.mutate(&tokens, new_src, &token_edit);

        let fresh = SemanticTable::new(&tokens, new_src);
        let mutated_chunks: Vec<_> = semantic
            .chunks
            .iter()
            .map(|chunk| (chunk.base_offset, chunk.end_offset))
            .collect();
        let fresh_chunks: Vec<_> = fresh
            .chunks
            .iter()
            .map(|chunk| (chunk.base_offset, chunk.end_offset))
            .collect();
        assert_eq!(mutated_chunks, fresh_chunks);
    }
}
