use tinyhl::{DelimiterKind, DelimiterTable, Language, Source, Span, TokenTable};

fn delimiters(language: Language, src: &str) -> Vec<(String, DelimiterKind, u16, bool, u8)> {
    let source: &dyn Source = &src;
    let tokens = TokenTable::new(language, source);
    let table = DelimiterTable::new(&tokens);
    table
        .query(Span::new(0, table.source_len()))
        .map(|token| {
            (
                src[token.span.offset as usize..token.span.end() as usize].to_string(),
                token.kind,
                token.depth,
                token.is_open,
                token.nest,
            )
        })
        .collect()
}

#[test]
fn rust_rainbow_depths_track_nested_delimiters() {
    let src = "fn main() { let x = [foo(bar[0]), baz]; }";
    let got = delimiters(Language::Rust, src);
    let expected = vec![
        ("(".to_string(), DelimiterKind::Paren, 0, true, 0),
        (")".to_string(), DelimiterKind::Paren, 0, false, 0),
        ("{".to_string(), DelimiterKind::Brace, 0, true, 0),
        ("[".to_string(), DelimiterKind::Bracket, 1, true, 0),
        ("(".to_string(), DelimiterKind::Paren, 2, true, 0),
        ("[".to_string(), DelimiterKind::Bracket, 3, true, 0),
        ("]".to_string(), DelimiterKind::Bracket, 3, false, 0),
        (")".to_string(), DelimiterKind::Paren, 2, false, 0),
        ("]".to_string(), DelimiterKind::Bracket, 1, false, 0),
        ("}".to_string(), DelimiterKind::Brace, 0, false, 0),
    ];
    assert_eq!(got, expected);
}

#[test]
fn c_digraph_delimiters_map_to_braces_and_brackets() {
    let src = "int main() <% int arr<:2:> = <% 1, 2 %>; return arr<:0:>; %>";
    let got = delimiters(Language::C, src);
    assert!(
        got.iter()
            .any(|(t, kind, _, open, _)| t == "<%" && *kind == DelimiterKind::Brace && *open)
    );
    assert!(
        got.iter()
            .any(|(t, kind, _, open, _)| t == "%>" && *kind == DelimiterKind::Brace && !*open)
    );
    assert!(
        got.iter()
            .any(|(t, kind, _, open, _)| t == "<:" && *kind == DelimiterKind::Bracket && *open)
    );
    assert!(
        got.iter()
            .any(|(t, kind, _, open, _)| t == ":>" && *kind == DelimiterKind::Bracket && !*open)
    );
}

#[test]
fn markdown_embeds_keep_embed_nest_in_delimiters() {
    let src = "```rust\nfn main() { println!(\"hi\"); }\n```\n";
    let got = delimiters(Language::Markdown, src);
    assert!(
        got.iter().any(|(t, _, _, _, nest)| t == "{" && *nest == 1),
        "expected embedded rust delimiter with embed nest in {got:?}"
    );
}

#[test]
fn mismatched_closer_keeps_current_depth() {
    let src = "{])";
    let got = delimiters(Language::Rust, src);
    assert_eq!(
        got,
        vec![
            ("{".to_string(), DelimiterKind::Brace, 0, true, 0),
            ("]".to_string(), DelimiterKind::Bracket, 1, false, 0),
            (")".to_string(), DelimiterKind::Paren, 1, false, 0),
        ]
    );
}
