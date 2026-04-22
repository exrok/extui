use tinyhl::{Language, SemanticKind, SemanticTable, Source, Span, TokenTable};

fn semantic_pairs(language: Language, src: &str) -> Vec<(String, SemanticKind)> {
    let source: &dyn Source = &src;
    let tokens = TokenTable::new(language, source);
    let semantic = SemanticTable::new(&tokens, source);
    semantic
        .query(Span::new(0, semantic.source_len()))
        .map(|token| {
            (
                src[token.span.offset as usize..token.span.end() as usize].to_string(),
                token.kind,
            )
        })
        .collect()
}

fn assert_has(pairs: &[(String, SemanticKind)], text: &str, kind: SemanticKind) {
    assert!(
        pairs.iter().any(|(t, k)| t == text && *k == kind),
        "missing ({text:?}, {kind:?}) in {pairs:?}"
    );
}

fn assert_lacks(pairs: &[(String, SemanticKind)], text: &str, kind: SemanticKind) {
    assert!(
        !pairs.iter().any(|(t, k)| t == text && *k == kind),
        "unexpected ({text:?}, {kind:?}) in {pairs:?}"
    );
}

#[test]
fn rust_semantic_categories() {
    let src = r#"
struct Foo;

impl Foo {
    fn bar(&self, arg: Bar) -> Baz {
        self.field(arg);
        Foo::make(arg);
        let local = arg;
        local
    }
}
"#;
    let pairs = semantic_pairs(Language::Rust, src);
    assert_has(&pairs, "Foo", SemanticKind::TypeDefinition);
    assert_has(&pairs, "Foo", SemanticKind::TypeName);
    assert_has(&pairs, "bar", SemanticKind::MethodDefinition);
    assert_has(&pairs, "arg", SemanticKind::Parameter);
    assert_has(&pairs, "Bar", SemanticKind::TypeName);
    assert_has(&pairs, "Baz", SemanticKind::TypeName);
    assert_has(&pairs, "field", SemanticKind::MethodCall);
    assert_has(&pairs, "Foo", SemanticKind::PathComponent);
    assert_has(&pairs, "make", SemanticKind::FunctionCall);
    assert_has(&pairs, "local", SemanticKind::VariableDefinition);
    assert_has(&pairs, "local", SemanticKind::Variable);
    assert_has(&pairs, "arg", SemanticKind::Argument);
}

#[test]
fn rust_record_fields_and_turbofish_follow_context() {
    let src = r#"
struct Config {
    field_name: FieldTy,
}

impl Config {
    fn build(field_value: FieldTy) -> Self {
        Self { field_name: field_value }
    }

    fn convert<T>(value: T) -> T {
        value
    }
}

fn use_it(arg_value: FieldTy) {
    Config::convert::<FieldTy>(arg_value);
}
"#;
    let pairs = semantic_pairs(Language::Rust, src);
    assert_has(&pairs, "Config", SemanticKind::TypeDefinition);
    assert_has(&pairs, "field_name", SemanticKind::FieldDefinition);
    assert_has(&pairs, "field_name", SemanticKind::Field);
    assert_has(&pairs, "FieldTy", SemanticKind::TypeName);
    assert_has(&pairs, "convert", SemanticKind::FunctionCall);
    assert_has(&pairs, "Config", SemanticKind::PathComponent);
    assert_has(&pairs, "arg_value", SemanticKind::Argument);
    assert_has(&pairs, "field_value", SemanticKind::Variable);
    assert_lacks(&pairs, "field_value", SemanticKind::TypeName);
}

#[test]
fn ts_semantic_categories() {
    let src = r#"
class Counter {
    value: number;
    inc(step: number): number {
        return this.value + step;
    }
}

const build = (name: Name) => factory.make(name);
new Counter().inc(1);
"#;
    let pairs = semantic_pairs(Language::Ts, src);
    assert_has(&pairs, "Counter", SemanticKind::TypeDefinition);
    assert_has(&pairs, "value", SemanticKind::FieldDefinition);
    assert_has(&pairs, "inc", SemanticKind::MethodDefinition);
    assert_has(&pairs, "step", SemanticKind::Parameter);
    assert_has(&pairs, "Name", SemanticKind::TypeName);
    assert_has(&pairs, "build", SemanticKind::FunctionDefinition);
    assert_has(&pairs, "make", SemanticKind::MethodCall);
    assert_has(&pairs, "name", SemanticKind::Argument);
    assert_has(&pairs, "Counter", SemanticKind::TypeName);
    assert_has(&pairs, "inc", SemanticKind::MethodCall);
}

#[test]
fn c_semantic_categories() {
    let src = r#"
struct point {
    int x;
    int y;
};

static int distance(struct point *a, struct point *b) {
    return a->x + b->y;
}

int main(void) {
    struct point p;
    return distance(&p, &p);
}
"#;
    let pairs = semantic_pairs(Language::C, src);
    assert_has(&pairs, "point", SemanticKind::TypeDefinition);
    assert_has(&pairs, "x", SemanticKind::FieldDefinition);
    assert_has(&pairs, "y", SemanticKind::FieldDefinition);
    assert_has(&pairs, "distance", SemanticKind::FunctionDefinition);
    assert_has(&pairs, "a", SemanticKind::Parameter);
    assert_has(&pairs, "b", SemanticKind::Parameter);
    assert_has(&pairs, "x", SemanticKind::Field);
    assert_has(&pairs, "y", SemanticKind::Field);
    assert_has(&pairs, "main", SemanticKind::FunctionDefinition);
    assert_has(&pairs, "p", SemanticKind::VariableDefinition);
    assert_has(&pairs, "distance", SemanticKind::FunctionCall);
}

#[test]
fn markdown_embeds_get_semantics() {
    let src = r#"
```rust
struct Foo;
fn make(arg: Foo) -> Foo { arg }
```
"#;
    let source: &dyn Source = &src;
    let tokens = TokenTable::new(Language::Markdown, source);
    let semantic = SemanticTable::new(&tokens, source);
    let pairs: Vec<_> = semantic
        .query(Span::new(0, semantic.source_len()))
        .map(|token| {
            (
                src[token.span.offset as usize..token.span.end() as usize].to_string(),
                token.kind,
                token.lang_tag,
            )
        })
        .collect();
    assert!(
        pairs.iter().any(|(text, kind, lang)| {
            text == "Foo" && *kind == SemanticKind::TypeDefinition && *lang == Language::Rust.tag()
        }),
        "expected embedded rust semantic token in {pairs:?}"
    );
}
