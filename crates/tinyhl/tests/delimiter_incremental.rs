use tinyhl::{DelimiterKind, DelimiterTable, Language, Source, Span, TokenMutation, TokenTable};

fn apply(old: &str, span: Span, new: &str) -> String {
    let mut s = String::new();
    s.push_str(&old[..span.offset as usize]);
    s.push_str(new);
    s.push_str(&old[span.offset as usize + span.len as usize..]);
    s
}

fn dump(language: Language, src: &str) -> Vec<(Span, DelimiterKind, u16, bool, u8, u8)> {
    let source: &dyn Source = &src;
    let tokens = TokenTable::new(language, source);
    let table = DelimiterTable::new(&tokens);
    table
        .query(Span::new(0, table.source_len()))
        .map(|token| {
            (
                token.span,
                token.kind,
                token.depth,
                token.is_open,
                token.lang_tag,
                token.nest,
            )
        })
        .collect()
}

fn assert_incremental(language: Language, old: &str, edit: Span, replacement: &str) {
    let new = apply(old, edit, replacement);
    let old_src: &dyn Source = &old;
    let mut tokens = TokenTable::new(language, old_src);
    let mut delimiters = DelimiterTable::new(&tokens);

    let new_src: &dyn Source = &new.as_str();
    let token_edit: TokenMutation = tokens.mutate_detailed(new_src, edit, replacement.len() as u32);
    delimiters.mutate(&tokens, &token_edit);

    let fresh = dump(language, &new);
    let mutated: Vec<_> = delimiters
        .query(Span::new(0, delimiters.source_len()))
        .map(|token| {
            (
                token.span,
                token.kind,
                token.depth,
                token.is_open,
                token.lang_tag,
                token.nest,
            )
        })
        .collect();
    assert_eq!(
        mutated, fresh,
        "delimiter mutate != fresh ({language:?})\nold: {old:?}\nnew: {new:?}\nedit: {edit:?} -> {replacement:?}"
    );
}

#[test]
fn rust_delimiter_incremental_matches_fresh() {
    let source = "fn main() { let xs = [foo(bar[0]), baz]; if xs[0] == 1 { call(xs[1]); } }";
    for (off, len, rep) in [
        (0usize, 0usize, "pub "),
        (15, 1, "{"),
        (24, 3, "(qux)"),
        (source.len(), 0, "\n"),
    ] {
        assert_incremental(
            Language::Rust,
            source,
            Span::new(off as u32, len as u32),
            rep,
        );
    }
}

#[test]
fn markdown_embed_delimiter_incremental_matches_fresh() {
    let source = "```rust\nfn main() { let xs = [1, 2]; }\n```\n";
    for (off, len, rep) in [
        (0usize, 0usize, "\n"),
        (16, 0, "if true "),
        (27, 1, "("),
        (source.len() - 4, 0, "// trailing\n"),
    ] {
        assert_incremental(
            Language::Markdown,
            source,
            Span::new(off as u32, len as u32),
            rep,
        );
    }
}

#[test]
fn large_chunked_delimiter_incremental_matches_fresh() {
    let mut source = String::from("fn main() {");
    for _ in 0..220 {
        source.push_str(" let _x = [foo(bar[0]), baz];");
    }
    source.push_str(" }");
    let step = (source.len() / 32).max(1);
    for off in (0..source.len()).step_by(step) {
        assert_incremental(Language::Rust, &source, Span::new(off as u32, 0), "(");
    }
}
