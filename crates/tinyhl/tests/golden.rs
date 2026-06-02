use tinyhl::{Language, Source, Span, TokenTable};

fn load(name: &str) -> String {
    let path = format!("{}/fixtures/json/{name}", env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(path).unwrap_or_else(|e| panic!("fixture {name}: {e}"))
}

fn load_rust(name: &str) -> String {
    let path = format!("{}/fixtures/rust/{name}", env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(path).unwrap_or_else(|e| panic!("fixture {name}: {e}"))
}

fn load_c(name: &str) -> String {
    let path = format!("{}/fixtures/c/{name}", env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(path).unwrap_or_else(|e| panic!("fixture {name}: {e}"))
}

fn load_markdown(name: &str) -> String {
    let path = format!("{}/fixtures/markdown/{name}", env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(path).unwrap_or_else(|e| panic!("fixture {name}: {e}"))
}

fn load_ts(name: &str) -> String {
    let path = format!("{}/fixtures/ts/{name}", env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(path).unwrap_or_else(|e| panic!("fixture {name}: {e}"))
}

fn load_xml(name: &str) -> String {
    let path = format!("{}/fixtures/xml/{name}", env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(path).unwrap_or_else(|e| panic!("fixture {name}: {e}"))
}

fn load_csv(name: &str) -> String {
    let path = format!("{}/fixtures/csv/{name}", env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(path).unwrap_or_else(|e| panic!("fixture {name}: {e}"))
}

fn load_css(name: &str) -> String {
    let path = format!("{}/fixtures/css/{name}", env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(path).unwrap_or_else(|e| panic!("fixture {name}: {e}"))
}

fn load_html(name: &str) -> String {
    let path = format!("{}/fixtures/html/{name}", env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(path).unwrap_or_else(|e| panic!("fixture {name}: {e}"))
}

fn tokens_of(language: Language, s: &str) -> Vec<tinyhl::Token> {
    let src: &dyn Source = &s;
    let table = TokenTable::new(language, src);
    table.query(Span::new(0, table.source_len())).collect()
}

#[test]
fn simple_json_coverage_and_tags() {
    let input = load("simple.json.in");
    let tokens = tokens_of(Language::Json, &input);

    let mut pos = 0u32;
    for t in &tokens {
        assert_eq!(t.span.offset, pos, "gap before {pos}");
        pos += t.span.len;
        assert_eq!(t.lang_tag(), Language::Json.tag());
    }
    assert_eq!(pos as usize, input.len());
}

#[test]
fn nested_json_local_kinds_seen() {
    let input = load("nested.json.in");
    let tokens = tokens_of(Language::Json, &input);

    use tinyhl::kind as kinds;
    let mut kinds_seen = std::collections::HashSet::new();
    for t in &tokens {
        kinds_seen.insert(t.local_kind());
    }
    for required in [
        kinds::WHITESPACE,
        kinds::OPEN_BRACE,
        kinds::CLOSE_BRACE,
        kinds::OPEN_BRACKET,
        kinds::CLOSE_BRACKET,
        kinds::COLON,
        kinds::COMMA,
        kinds::STRING,
        kinds::NUMBER,
        kinds::KEYWORD,
    ] {
        assert!(
            kinds_seen.contains(&required),
            "fixture should exercise kind {required}"
        );
    }
}

#[test]
fn query_subrange_returns_overlapping_tokens_only() {
    let input = load("simple.json.in");
    let src: &dyn Source = &input.as_str();
    let table = TokenTable::new(Language::Json, src);

    let q = Span::new(10, 20);
    for t in table.query(q) {
        assert!(
            t.span.overlaps(&q),
            "token {:?} outside query {q:?}",
            t.span
        );
    }
}

#[test]
fn simple_rust_coverage_and_tags() {
    let input = load_rust("simple.rs.in");
    let tokens = tokens_of(Language::Rust, &input);

    let mut pos = 0u32;
    for t in &tokens {
        assert_eq!(t.span.offset, pos, "gap before {pos}");
        pos += t.span.len;
        assert_eq!(t.lang_tag(), Language::Rust.tag());
    }
    assert_eq!(pos as usize, input.len());
}

#[test]
fn rust_fixtures_exercise_expected_kinds() {
    use tinyhl::kind as kinds;

    let mut seen = std::collections::HashSet::new();
    for name in ["simple.rs.in", "edge.rs.in"] {
        let input = load_rust(name);
        for t in tokens_of(Language::Rust, &input) {
            seen.insert(t.local_kind());
        }
    }
    for required in [
        kinds::WHITESPACE,
        kinds::OPEN_BRACE,
        kinds::CLOSE_BRACE,
        kinds::OPEN_PAREN,
        kinds::CLOSE_PAREN,
        kinds::SEMI,
        kinds::COLON,
        kinds::STRING,
        kinds::NUMBER,
        kinds::KEYWORD,
        kinds::IDENT,
        kinds::COMMENT,
        kinds::DOC_COMMENT,
        kinds::LIFETIME,
        kinds::CHAR,
    ] {
        assert!(
            seen.contains(&required),
            "rust fixtures should exercise kind {required}"
        );
    }
}

#[test]
fn simple_c_coverage_and_tags() {
    let input = load_c("simple.c.in");
    let tokens = tokens_of(Language::C, &input);

    let mut pos = 0u32;
    for t in &tokens {
        assert_eq!(t.span.offset, pos, "gap before {pos}");
        pos += t.span.len;
        assert_eq!(t.lang_tag(), Language::C.tag());
    }
    assert_eq!(pos as usize, input.len());
}

#[test]
fn c_fixtures_exercise_expected_kinds() {
    use tinyhl::kind as kinds;

    let mut seen = std::collections::HashSet::new();
    for name in ["simple.c.in", "edge.c.in", "kr.c.in"] {
        let input = load_c(name);
        for t in tokens_of(Language::C, &input) {
            seen.insert(t.local_kind());
        }
    }
    for required in [
        kinds::WHITESPACE,
        kinds::OPEN_BRACE,
        kinds::CLOSE_BRACE,
        kinds::OPEN_PAREN,
        kinds::CLOSE_PAREN,
        kinds::SEMI,
        kinds::STRING,
        kinds::NUMBER,
        kinds::KEYWORD,
        kinds::IDENT,
        kinds::COMMENT,
        kinds::CHAR,
    ] {
        assert!(
            seen.contains(&required),
            "c fixtures should exercise kind {required}"
        );
    }
}

#[test]
fn simple_ts_coverage_and_tags() {
    let input = load_ts("simple.ts.in");
    let tokens = tokens_of(Language::Ts, &input);

    let mut pos = 0u32;
    for t in &tokens {
        assert_eq!(t.span.offset, pos, "gap before {pos}");
        pos += t.span.len;
        assert_eq!(t.lang_tag(), Language::Ts.tag());
    }
    assert_eq!(pos as usize, input.len());
}

#[test]
fn ts_fixture_exercises_expected_kinds() {
    use tinyhl::kind as kinds;

    let mut seen = std::collections::HashSet::new();
    let input = load_ts("simple.ts.in");
    for t in tokens_of(Language::Ts, &input) {
        seen.insert(t.local_kind());
    }
    for required in [
        kinds::WHITESPACE,
        kinds::OPEN_BRACE,
        kinds::CLOSE_BRACE,
        kinds::OPEN_PAREN,
        kinds::CLOSE_PAREN,
        kinds::OPEN_BRACKET,
        kinds::CLOSE_BRACKET,
        kinds::SEMI,
        kinds::COLON,
        kinds::COMMA,
        kinds::DOT,
        kinds::STRING,
        kinds::TEMPLATE_STRING,
        kinds::NUMBER,
        kinds::KEYWORD,
        kinds::IDENT,
        kinds::COMMENT,
        kinds::REGEX,
        kinds::FAT_ARROW,
        kinds::QUESTION_QUESTION,
    ] {
        assert!(
            seen.contains(&required),
            "ts fixture should exercise kind {required}"
        );
    }
}

#[test]
fn simple_markdown_coverage_and_tags() {
    let input = load_markdown("simple.md.in");
    let tokens = tokens_of(Language::Markdown, &input);

    let mut pos = 0u32;
    let md_tag = Language::Markdown.tag();
    for t in &tokens {
        assert_eq!(t.span.offset, pos, "gap before {pos}");
        pos += t.span.len;
        if t.nest == 0 {
            assert_eq!(
                t.lang_tag(),
                md_tag,
                "outer-level token at {} should be markdown",
                t.span.offset,
            );
        } else {
            assert_ne!(
                t.lang_tag(),
                md_tag,
                "embedded token at {} should not be markdown",
                t.span.offset,
            );
        }
    }
    assert_eq!(pos as usize, input.len());
}

#[test]
fn markdown_fixtures_exercise_expected_kinds() {
    use tinyhl::kind as kinds;

    let mut seen = std::collections::HashSet::new();
    let input = load_markdown("simple.md.in");
    for t in tokens_of(Language::Markdown, &input) {
        if t.lang_tag() == Language::Markdown.tag() {
            seen.insert(t.local_kind());
        }
    }
    for required in [
        kinds::WHITESPACE,
        kinds::OPEN_PAREN,
        kinds::CLOSE_PAREN,
        kinds::GT,
        kinds::TEXT,
        kinds::HEADING_MARKER,
        kinds::HEADING_TEXT,
        kinds::EMPHASIS,
        kinds::CODE_INLINE,
        kinds::CODE_FENCE,
        kinds::CODE_BLOCK,
        kinds::LINK_TEXT,
        kinds::LINK_URL,
        kinds::BLOCKQUOTE,
        kinds::LIST_MARKER,
    ] {
        assert!(
            seen.contains(&required),
            "markdown fixture should exercise kind {required}"
        );
    }
}

#[test]
fn markdown_embed_dispatches_to_inner_languages() {
    let input = load_markdown("simple.md.in");
    let tokens = tokens_of(Language::Markdown, &input);

    let mut nested_langs = std::collections::HashSet::new();
    for t in &tokens {
        if t.nest >= 1 {
            nested_langs.insert(t.lang_tag());
        }
    }
    assert!(
        nested_langs.contains(&Language::Rust.tag()),
        "rust fence should embed"
    );
    assert!(
        nested_langs.contains(&Language::C.tag()),
        "c fence should embed"
    );
    assert!(
        nested_langs.contains(&Language::Csv.tag()),
        "csv fence should embed"
    );
    assert!(
        nested_langs.contains(&Language::Json.tag()),
        "json fence should embed"
    );
    assert!(
        nested_langs.contains(&Language::Xml.tag()),
        "xml fence should embed"
    );
    assert!(
        nested_langs.contains(&Language::Css.tag()),
        "css fence should embed"
    );
}

#[test]
fn simple_xml_coverage_and_tags() {
    let input = load_xml("simple.xml.in");
    let tokens = tokens_of(Language::Xml, &input);

    let mut pos = 0u32;
    for t in &tokens {
        assert_eq!(t.span.offset, pos, "gap before {pos}");
        pos += t.span.len;
        assert_eq!(t.lang_tag(), Language::Xml.tag());
    }
    assert_eq!(pos as usize, input.len());
}

#[test]
fn xml_fixture_exercises_expected_kinds() {
    use tinyhl::kind as kinds;

    let mut seen = std::collections::HashSet::new();
    let input = load_xml("simple.xml.in");
    for t in tokens_of(Language::Xml, &input) {
        seen.insert(t.local_kind());
    }
    for required in [
        kinds::TEXT,
        kinds::WHITESPACE,
        kinds::LT,
        kinds::GT,
        kinds::SLASH,
        kinds::QUESTION,
        kinds::EQ,
        kinds::TAG_NAME,
        kinds::ATTR_NAME,
        kinds::STRING,
        kinds::COMMENT,
        kinds::ENTITY_REF,
        kinds::CDATA,
        kinds::DOCTYPE,
    ] {
        assert!(
            seen.contains(&required),
            "xml fixture should exercise kind {required}"
        );
    }
}

#[test]
fn simple_csv_coverage_and_tags() {
    let input = load_csv("simple.csv.in");
    let tokens = tokens_of(Language::Csv, &input);

    let mut pos = 0u32;
    for t in &tokens {
        assert_eq!(t.span.offset, pos, "gap before {pos}");
        pos += t.span.len;
        assert_eq!(t.lang_tag(), Language::Csv.tag());
    }
    assert_eq!(pos as usize, input.len());
}

#[test]
fn csv_fixture_exercises_expected_kinds() {
    use tinyhl::kind as kinds;

    let mut seen = std::collections::HashSet::new();
    let input = load_csv("simple.csv.in");
    for t in tokens_of(Language::Csv, &input) {
        seen.insert(t.local_kind());
    }
    for required in [
        kinds::TEXT,
        kinds::WHITESPACE,
        kinds::COMMA,
        kinds::STRING,
        kinds::NUMBER,
    ] {
        assert!(
            seen.contains(&required),
            "csv fixture should exercise kind {required}"
        );
    }
}

#[test]
fn simple_css_coverage_and_tags() {
    let input = load_css("simple.css.in");
    let tokens = tokens_of(Language::Css, &input);

    let mut pos = 0u32;
    for t in &tokens {
        assert_eq!(t.span.offset, pos, "gap before {pos}");
        pos += t.span.len;
        assert_eq!(t.lang_tag(), Language::Css.tag());
    }
    assert_eq!(pos as usize, input.len());
}

#[test]
fn css_fixture_exercises_expected_kinds() {
    use tinyhl::kind as kinds;

    let mut seen = std::collections::HashSet::new();
    let input = load_css("simple.css.in");
    for t in tokens_of(Language::Css, &input) {
        seen.insert(t.local_kind());
    }
    for required in [
        kinds::WHITESPACE,
        kinds::COMMENT,
        kinds::STRING,
        kinds::NUMBER,
        kinds::IDENT,
        kinds::AT_KEYWORD,
        kinds::HASH_TOKEN,
        kinds::OPEN_BRACE,
        kinds::CLOSE_BRACE,
        kinds::OPEN_PAREN,
        kinds::CLOSE_PAREN,
        kinds::OPEN_BRACKET,
        kinds::CLOSE_BRACKET,
        kinds::COLON,
        kinds::SEMI,
        kinds::COMMA,
        kinds::DOT,
        kinds::GT,
        kinds::BANG,
        kinds::CARET,
        kinds::EQ,
    ] {
        assert!(
            seen.contains(&required),
            "css fixture should exercise kind {required}"
        );
    }
}

#[test]
fn simple_html_coverage_and_tags() {
    let input = load_html("simple.html.in");
    let tokens = tokens_of(Language::Html, &input);

    let mut pos = 0u32;
    let html_tag = Language::Html.tag();
    for t in &tokens {
        assert_eq!(t.span.offset, pos, "gap before {pos}");
        pos += t.span.len;
        if t.nest == 0 {
            assert_eq!(
                t.lang_tag(),
                html_tag,
                "outer-level token at {} should be html",
                t.span.offset,
            );
        } else {
            assert_ne!(
                t.lang_tag(),
                html_tag,
                "embedded token at {} should not be html",
                t.span.offset,
            );
        }
    }
    assert_eq!(pos as usize, input.len());
}

#[test]
fn html_fixture_exercises_expected_kinds() {
    use tinyhl::kind as kinds;

    let mut seen = std::collections::HashSet::new();
    let input = load_html("simple.html.in");
    for t in tokens_of(Language::Html, &input) {
        if t.lang_tag() == Language::Html.tag() {
            seen.insert(t.local_kind());
        }
    }
    for required in [
        kinds::TEXT,
        kinds::WHITESPACE,
        kinds::LT,
        kinds::GT,
        kinds::SLASH,
        kinds::EQ,
        kinds::TAG_NAME,
        kinds::ATTR_NAME,
        kinds::STRING,
        kinds::COMMENT,
        kinds::ENTITY_REF,
        kinds::DOCTYPE,
    ] {
        assert!(
            seen.contains(&required),
            "html fixture should exercise kind {required}"
        );
    }
}

#[test]
fn markdown_ts_fence_embeds_typescript() {
    let source =
        "intro\n\n```ts\nconst x: number = 42;\n```\n\n```typescript\nlet y = `hi`;\n```\n";
    let tokens = tokens_of(Language::Markdown, source);
    let embedded: std::collections::HashSet<u8> = tokens
        .iter()
        .filter(|t| t.nest >= 1)
        .map(|t| t.lang_tag())
        .collect();
    assert!(
        embedded.contains(&Language::Ts.tag()),
        "both `ts` and `typescript` fences should embed TypeScript"
    );
}

#[test]
fn markdown_html_embed_nests_css_and_ts() {
    // Full embed stack: Markdown (nest 0) → HTML (nest 1) → CSS / TS (nest 2).
    let source = "# Title\n\n```html\n<style>.a{color:red}</style>\n<p>hi &amp; bye</p>\n<script>const x = 1;</script>\n```\n\ndone\n";
    let tokens = tokens_of(Language::Markdown, source);

    // Coverage stays contiguous through two levels of embedding.
    let mut pos = 0u32;
    for t in &tokens {
        assert_eq!(t.span.offset, pos, "gap before {pos}");
        pos += t.span.len;
    }
    assert_eq!(pos as usize, source.len());

    let mut seen = std::collections::HashSet::new();
    for t in &tokens {
        seen.insert((t.nest, t.lang_tag()));
    }
    assert!(
        seen.contains(&(0u8, Language::Markdown.tag())),
        "outer level should be Markdown"
    );
    assert!(
        seen.contains(&(1u8, Language::Html.tag())),
        "first embed level should be HTML"
    );
    assert!(
        seen.contains(&(2u8, Language::Css.tag())),
        "<style> should embed CSS at nest 2"
    );
    assert!(
        seen.contains(&(2u8, Language::Ts.tag())),
        "<script> should embed TypeScript at nest 2"
    );

    // Every token's language tag must agree with its nesting depth: only the
    // outermost level is Markdown, and the deepest tokens are never HTML.
    for t in &tokens {
        match t.nest {
            0 => assert_eq!(t.lang_tag(), Language::Markdown.tag()),
            1 => assert_eq!(t.lang_tag(), Language::Html.tag()),
            2 => assert!(
                t.lang_tag() == Language::Css.tag() || t.lang_tag() == Language::Ts.tag(),
                "nest-2 token at {} should be CSS or TS",
                t.span.offset
            ),
            other => panic!("unexpected nest depth {other}"),
        }
    }
}

#[test]
fn html_embed_dispatches_to_inner_languages() {
    let input = load_html("simple.html.in");
    let tokens = tokens_of(Language::Html, &input);

    let mut nested_langs = std::collections::HashSet::new();
    for t in &tokens {
        if t.nest >= 1 {
            nested_langs.insert(t.lang_tag());
        }
    }
    assert!(
        nested_langs.contains(&Language::Css.tag()),
        "style block should embed CSS"
    );
    assert!(
        nested_langs.contains(&Language::Ts.tag()),
        "script block should embed TypeScript"
    );
}
