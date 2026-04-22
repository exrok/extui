use tinyhl::{Language, SemanticTable, Source, Span, TokenMutation, TokenTable};

fn apply(old: &str, span: Span, new: &str) -> String {
    let mut s = String::new();
    s.push_str(&old[..span.offset as usize]);
    s.push_str(new);
    s.push_str(&old[span.offset as usize + span.len as usize..]);
    s
}

fn semantic_dump(language: Language, src: &str) -> Vec<(Span, tinyhl::SemanticKind, u8, u8)> {
    let source: &dyn Source = &src;
    let tokens = TokenTable::new(language, source);
    let semantic = SemanticTable::new(&tokens, source);
    semantic
        .query(Span::new(0, semantic.source_len()))
        .map(|token| (token.span, token.kind, token.lang_tag, token.nest))
        .collect()
}

fn assert_incremental_semantics(language: Language, old: &str, edit: Span, replacement: &str) {
    let new = apply(old, edit, replacement);

    let old_src: &dyn Source = &old;
    let mut tokens = TokenTable::new(language, old_src);
    let mut semantic = SemanticTable::new(&tokens, old_src);

    let new_src: &dyn Source = &new.as_str();
    let token_edit: TokenMutation = tokens.mutate_detailed(new_src, edit, replacement.len() as u32);
    semantic.mutate(&tokens, new_src, &token_edit);

    let fresh = semantic_dump(language, &new);
    let mutated: Vec<_> = semantic
        .query(Span::new(0, semantic.source_len()))
        .map(|token| (token.span, token.kind, token.lang_tag, token.nest))
        .collect();

    assert_eq!(
        mutated, fresh,
        "semantic mutate != fresh ({language:?})\nold: {old:?}\nnew: {new:?}\nedit: {edit:?} -> {replacement:?}"
    );
}

#[test]
fn rust_semantic_incremental_matches_fresh() {
    let source = "struct Foo; impl Foo { fn bar(&self, arg: Bar) -> Baz { self.field(arg); let local = arg; local } }";
    for (off, len, rep) in [
        (0usize, 0usize, "pub "),
        (19, 3, "Widget"),
        (43, 0, "extra: Qux, "),
        (63, 5, "invoke"),
        (source.len() - 2, 0, "\n"),
    ] {
        assert_incremental_semantics(language(), source, Span::new(off as u32, len as u32), rep);
    }
}

#[test]
fn ts_semantic_incremental_matches_fresh() {
    let source = "class Counter { value: number; inc(step: number): number { return this.value + step; } } const build = (name: Name) => factory.make(name);";
    for (off, len, rep) in [
        (0usize, 0usize, "export "),
        (6, 7, "Meter"),
        (32, 6, "amount"),
        (98, 4, "create"),
        (source.len(), 0, " new Counter().inc(1);"),
    ] {
        assert_incremental_semantics(Language::Ts, source, Span::new(off as u32, len as u32), rep);
    }
}

#[test]
fn c_semantic_incremental_matches_fresh() {
    let source = "struct point { int x; int y; }; static int distance(struct point *a, struct point *b) { return a->x + b->y; }";
    for (off, len, rep) in [
        (0usize, 0usize, "typedef "),
        (7, 5, "coord"),
        (43, 8, "metric"),
        (69, 1, "lhs"),
        (
            source.len(),
            0,
            " int main(void) { struct point p; return distance(&p, &p); }",
        ),
    ] {
        assert_incremental_semantics(Language::C, source, Span::new(off as u32, len as u32), rep);
    }
}

#[test]
fn markdown_embed_semantic_incremental_matches_fresh() {
    let source = "```rust\nstruct Foo;\nfn make(arg: Foo) -> Foo { arg }\n```\n";
    for (off, len, rep) in [
        (0usize, 0usize, "\n"),
        (17, 3, "Bar"),
        (22, 4, "build"),
        (source.len() - 4, 0, "// tail\n"),
    ] {
        assert_incremental_semantics(
            Language::Markdown,
            source,
            Span::new(off as u32, len as u32),
            rep,
        );
    }
}

fn language() -> Language {
    Language::Rust
}
