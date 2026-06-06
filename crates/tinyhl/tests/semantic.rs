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

fn semantic_display_pairs(language: Language, src: &str) -> Vec<(String, SemanticKind)> {
    let source: &dyn Source = &src;
    let tokens = TokenTable::new(language, source);
    let semantic = SemanticTable::new(&tokens, source);
    semantic
        .query(Span::new(0, semantic.source_len()))
        .map(|token| {
            (
                src[token.span.offset as usize..token.span.end() as usize].to_string(),
                token.display_kind(),
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
fn rust_macro_attr_and_path_turbofish_roles() {
    let src = r#"
macro_rules! gen { () => {} }

#[derive(Debug, Clone)]
#[rustfmt::skip]
struct Holder;

fn run(payload: Bytes) {
    let a = jsony::from_binary::<crate::rpc::Req>(payload);
    let b = thing.cast::<Wrap<Inner::Kind>>();
    let c = parse::<[i64; 2]>(payload);
    let p = &x as *const _ as *const libc::c_void;
    let __r: ::std::vec::Vec<u8> = make();
}
"#;
    let pairs = semantic_pairs(Language::Rust, src);
    // `macro_rules! name` — the defined name is a function definition.
    assert_has(&pairs, "gen", SemanticKind::FunctionDefinition);
    // Attribute bodies are neutral: `derive`/`rustfmt`/`skip` are not calls or
    // path segments. (`Debug`/`Clone` stay as-is — the case rule tans them.)
    assert_has(&pairs, "derive", SemanticKind::Variable);
    assert_lacks(&pairs, "derive", SemanticKind::FunctionCall);
    assert_has(&pairs, "rustfmt", SemanticKind::Variable);
    assert_lacks(&pairs, "rustfmt", SemanticKind::PathComponent);
    assert_has(&pairs, "skip", SemanticKind::Variable);
    // A turbofish call whose type args carry inner `::` paths still promotes
    // the call name, and the inner segments stay path components / types.
    assert_has(&pairs, "from_binary", SemanticKind::FunctionCall);
    assert_lacks(&pairs, "from_binary", SemanticKind::PathComponent);
    assert_has(&pairs, "rpc", SemanticKind::PathComponent);
    assert_has(&pairs, "Req", SemanticKind::TypeName);
    // Method turbofish with a nested path argument.
    assert_has(&pairs, "cast", SemanticKind::MethodCall);
    assert_has(&pairs, "Inner", SemanticKind::PathComponent);
    // A `;` inside an array-type turbofish arg must not break the promotion.
    assert_has(&pairs, "parse", SemanticKind::FunctionCall);
    // `*const T::path`: a pointer path, not a `const` binding name.
    assert_has(&pairs, "libc", SemanticKind::PathComponent);
    assert_lacks(&pairs, "libc", SemanticKind::VariableDefinition);
    // `let __r: ::std::...`: the leading-`::` absolute path in the annotation
    // must not make the binding read as a path segment.
    assert_has(&pairs, "__r", SemanticKind::VariableDefinition);
    assert_lacks(&pairs, "__r", SemanticKind::PathComponent);
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
fn rust_record_expr_prefixed_value_is_not_a_field() {
    let src = r#"
fn main() {
    let source = NumberedLineSource { lines: &source_lines, len: source_len };
}
"#;
    let pairs = semantic_pairs(Language::Rust, src);
    // Field keys.
    assert_has(&pairs, "lines", SemanticKind::Field);
    assert_has(&pairs, "len", SemanticKind::Field);
    // `&source_lines` is a value: the leading `&` must not end the value
    // region and let `source_lines` read as a fresh field key.
    assert_has(&pairs, "source_lines", SemanticKind::Variable);
    assert_lacks(&pairs, "source_lines", SemanticKind::Field);
    assert_has(&pairs, "source_len", SemanticKind::Variable);
    assert_lacks(&pairs, "source_len", SemanticKind::Field);
}

#[test]
fn rust_let_pattern_distinguishes_constructor_from_binding() {
    let src = r#"
fn main() {
    let Some(binding) = Some(22) else {
        return;
    };
}
"#;
    let pairs = semantic_pairs(Language::Rust, src);
    // `Some` in pattern position is an enum-variant constructor, not a binding.
    assert_has(&pairs, "Some", SemanticKind::TypeName);
    assert_lacks(&pairs, "Some", SemanticKind::VariableDefinition);
    // `binding` is the real binding.
    assert_has(&pairs, "binding", SemanticKind::VariableDefinition);
    assert_lacks(&pairs, "binding", SemanticKind::TypeName);
    // RHS `Some(22)` is a constructor call (kept distinct from the pattern).
    assert_has(&pairs, "Some", SemanticKind::FunctionCall);
}

#[test]
fn rust_nested_struct_slice_path_pattern() {
    let src = r#"
fn main() {
    let crate::path::Type { name: [binding_1, binding_2] } = make();
}
"#;
    let pairs = semantic_pairs(Language::Rust, src);
    assert_has(&pairs, "crate", SemanticKind::PathComponent);
    assert_has(&pairs, "path", SemanticKind::PathComponent);
    assert_has(&pairs, "Type", SemanticKind::TypeName);
    // `name` is a field, not a binding.
    assert_has(&pairs, "name", SemanticKind::Field);
    assert_lacks(&pairs, "name", SemanticKind::VariableDefinition);
    // The slice sub-patterns are the bindings.
    assert_has(&pairs, "binding_1", SemanticKind::VariableDefinition);
    assert_has(&pairs, "binding_2", SemanticKind::VariableDefinition);
}

#[test]
fn rust_assorted_pattern_positions() {
    let src = r#"
fn main() {
    let (a, b) = pair();
    let None = opt() else { return; };
    for (k, v) in items() {}
    if let Ok(value) = result() {}
    while let Some(item) = next() {}
}
"#;
    let pairs = semantic_pairs(Language::Rust, src);
    assert_has(&pairs, "a", SemanticKind::VariableDefinition);
    assert_has(&pairs, "b", SemanticKind::VariableDefinition);
    // Unit variant in pattern position is a constructor, not a binding.
    assert_has(&pairs, "None", SemanticKind::TypeName);
    assert_lacks(&pairs, "None", SemanticKind::VariableDefinition);
    assert_has(&pairs, "k", SemanticKind::VariableDefinition);
    assert_has(&pairs, "v", SemanticKind::VariableDefinition);
    assert_has(&pairs, "Ok", SemanticKind::TypeName);
    assert_has(&pairs, "value", SemanticKind::VariableDefinition);
    assert_has(&pairs, "Some", SemanticKind::TypeName);
    assert_has(&pairs, "item", SemanticKind::VariableDefinition);
    // Constructors in patterns are never bindings.
    assert_lacks(&pairs, "Ok", SemanticKind::VariableDefinition);
    assert_lacks(&pairs, "Some", SemanticKind::VariableDefinition);
}

#[test]
fn rust_let_type_annotation_after_pattern() {
    let src = r#"
fn main() {
    let value: Vec<u8> = make();
}
"#;
    let pairs = semantic_pairs(Language::Rust, src);
    assert_has(&pairs, "value", SemanticKind::VariableDefinition);
    // The annotation after the pattern is still a type position.
    assert_has(&pairs, "Vec", SemanticKind::TypeName);
    assert_lacks(&pairs, "Vec", SemanticKind::VariableDefinition);
}

#[test]
fn rust_generic_params_are_type_positions() {
    // `GenericParams` after a definition name open a type-position region: the
    // parameters and their bounds are types, not value expressions.
    let src = r#"
fn convert<Item: Iterator, Out>(x: Item) -> Out where Out: Clone { todo!() }
struct Pair<Lhs: Clone, Rhs>(Lhs, Rhs);
trait Sink<Msg>: Clone { type Output; }
type Alias<Elem> = Container<Elem>;
"#;
    let pairs = semantic_pairs(Language::Rust, src);
    // Function generics + their bounds.
    assert_has(&pairs, "Item", SemanticKind::TypeName);
    assert_has(&pairs, "Iterator", SemanticKind::TypeName);
    assert_has(&pairs, "Out", SemanticKind::TypeName);
    assert_lacks(&pairs, "Item", SemanticKind::Variable);
    assert_lacks(&pairs, "Iterator", SemanticKind::Variable);
    // Struct / trait generics.
    assert_has(&pairs, "Lhs", SemanticKind::TypeName);
    assert_has(&pairs, "Rhs", SemanticKind::TypeName);
    assert_has(&pairs, "Msg", SemanticKind::TypeName);
    // Type-alias generics and RHS.
    assert_has(&pairs, "Elem", SemanticKind::TypeName);
    assert_has(&pairs, "Container", SemanticKind::TypeName);
    assert_lacks(&pairs, "Container", SemanticKind::Variable);
}

#[test]
fn rust_impl_for_and_qualified_path_roles() {
    let src = r#"
impl Display for Widget {
    fn fmt(&self) -> Result { Ok(()) }
}
fn run(x: Input) {
    let v = <Wrap as Convert>::from(x);
    let n = core::mem::size_of::<Cell>() as usize * 2;
    let m = match <Map as Lookup>::get(x) { _ => 0 };
}
"#;
    let pairs = semantic_pairs(Language::Rust, src);
    // The implementing type after `for` is a type, not a value.
    assert_has(&pairs, "Widget", SemanticKind::TypeName);
    assert_lacks(&pairs, "Widget", SemanticKind::Variable);
    // A leading qualified-path type `<Wrap as Convert>::from(..)`: the angle
    // body is types, and the trailing segment is the call.
    assert_has(&pairs, "Wrap", SemanticKind::TypeName);
    assert_has(&pairs, "Convert", SemanticKind::TypeName);
    assert_has(&pairs, "from", SemanticKind::FunctionCall);
    assert_lacks(&pairs, "from", SemanticKind::TypeName);
    // A cast target does not bleed into the following multiplication: the
    // turbofish call still promotes.
    assert_has(&pairs, "size_of", SemanticKind::FunctionCall);
    assert_has(&pairs, "Cell", SemanticKind::TypeName);
    // A qualified path in `match` scrutinee position works too.
    assert_has(&pairs, "Map", SemanticKind::TypeName);
    assert_has(&pairs, "get", SemanticKind::FunctionCall);
    assert_lacks(&pairs, "get", SemanticKind::TypeName);
}

#[test]
fn rust_field_access_vs_field_key() {
    let src = r#"
struct P { field: u8 }
fn main() {
    let p = P { field: 1 };
    let z = p.field;
}
"#;
    let pairs = semantic_pairs(Language::Rust, src);
    // The same name fills three distinct roles, each its own kind.
    assert_has(&pairs, "field", SemanticKind::FieldDefinition);
    assert_has(&pairs, "field", SemanticKind::Field); // struct-literal key
    assert_has(&pairs, "field", SemanticKind::FieldAccess); // member access
}

#[test]
fn rust_macro_metavariables() {
    let src = r#"
macro_rules! m {
    ($val:expr, $f:ident, $Ty:ty) => {
        $f($val);
        $Ty::new()
    };
}
"#;
    let pairs = semantic_pairs(Language::Rust, src);
    // Every `$name` is a metavariable, never a call/argument/path.
    assert_has(&pairs, "val", SemanticKind::MetaVariable);
    assert_has(&pairs, "f", SemanticKind::MetaVariable);
    assert_has(&pairs, "Ty", SemanticKind::MetaVariable);
    assert_lacks(&pairs, "f", SemanticKind::FunctionCall);
    assert_lacks(&pairs, "val", SemanticKind::Argument);
    assert_lacks(&pairs, "Ty", SemanticKind::PathComponent);

    // Casing folds in via `display_kind`: uppercase metavariable renders as a
    // type, lowercase stays neutral.
    let display = semantic_display_pairs(Language::Rust, src);
    assert_has(&display, "val", SemanticKind::MetaVariable);
    assert_has(&display, "Ty", SemanticKind::TypeName);
}

#[test]
fn rust_type_styled_display_kind() {
    let src = r#"
fn main() {
    let x = Foo;
    let y = Some(1);
    let z = u32;
    let w = local;
}
"#;
    let display = semantic_display_pairs(Language::Rust, src);
    // UpperCamel and primitive names render as a type by spelling...
    assert_has(&display, "Foo", SemanticKind::TypeName);
    assert_has(&display, "Some", SemanticKind::TypeName);
    assert_has(&display, "u32", SemanticKind::TypeName);
    // ...while a lowercase local stays neutral.
    assert_has(&display, "local", SemanticKind::Variable);
    // The structural kind is preserved: a constructor is still a call.
    let structural = semantic_pairs(Language::Rust, src);
    assert_has(&structural, "Some", SemanticKind::FunctionCall);
    assert_lacks(&structural, "Foo", SemanticKind::TypeName);
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
    assert_has(&pairs, "x", SemanticKind::FieldAccess);
    assert_has(&pairs, "y", SemanticKind::FieldAccess);
    assert_has(&pairs, "main", SemanticKind::FunctionDefinition);
    assert_has(&pairs, "p", SemanticKind::VariableDefinition);
    assert_has(&pairs, "distance", SemanticKind::FunctionCall);
}

#[test]
fn cpp_reuses_c_semantic_categories() {
    let src = r#"
namespace demo {
struct point {
    int x;
    int y;
};

static int distance(struct point *a, struct point *b) {
    return a->x + b->y;
}
}
"#;
    let pairs = semantic_pairs(Language::Cpp, src);
    assert_has(&pairs, "point", SemanticKind::TypeDefinition);
    assert_has(&pairs, "x", SemanticKind::FieldDefinition);
    assert_has(&pairs, "distance", SemanticKind::FunctionDefinition);
    assert_has(&pairs, "a", SemanticKind::Parameter);
    assert_has(&pairs, "x", SemanticKind::FieldAccess);
}

#[test]
fn python_semantic_categories() {
    let src = r#"
import os as system

class Point(Base):
    def __init__(self, x, y=0):
        self.x = x
        self.y = y

    def norm(self) -> float:
        return self.x


@staticmethod
def make(scale: float) -> Point:
    p = Point(1, 2)
    total = p.norm()
    for item in values:
        total += item
    return total


result = make(2.0)
print(result, system)
"#;
    let pairs = semantic_pairs(Language::Python, src);
    // Definitions.
    assert_has(&pairs, "Point", SemanticKind::TypeDefinition);
    assert_has(&pairs, "__init__", SemanticKind::MethodDefinition);
    assert_has(&pairs, "norm", SemanticKind::MethodDefinition);
    assert_has(&pairs, "make", SemanticKind::FunctionDefinition);
    // Parameters and their annotations.
    assert_has(&pairs, "self", SemanticKind::Parameter);
    assert_has(&pairs, "x", SemanticKind::Parameter);
    assert_has(&pairs, "scale", SemanticKind::Parameter);
    assert_has(&pairs, "float", SemanticKind::TypeName);
    assert_has(&pairs, "Point", SemanticKind::TypeName);
    assert_has(&pairs, "Base", SemanticKind::TypeName);
    // Fields through member access.
    assert_has(&pairs, "x", SemanticKind::FieldDefinition);
    assert_has(&pairs, "x", SemanticKind::FieldAccess);
    // Calls.
    assert_has(&pairs, "Point", SemanticKind::FunctionCall);
    assert_has(&pairs, "make", SemanticKind::FunctionCall);
    assert_has(&pairs, "norm", SemanticKind::MethodCall);
    assert_has(&pairs, "print", SemanticKind::FunctionCall);
    // Variables, arguments, loop targets, aliases.
    assert_has(&pairs, "p", SemanticKind::VariableDefinition);
    assert_has(&pairs, "result", SemanticKind::VariableDefinition);
    assert_has(&pairs, "item", SemanticKind::VariableDefinition);
    assert_has(&pairs, "system", SemanticKind::VariableDefinition);
    assert_has(&pairs, "result", SemanticKind::Argument);
    // A plain method receiver stays a variable, not a field.
    assert_has(&pairs, "p", SemanticKind::Variable);
    // The decorator name is highlighted as a call, not a bare variable.
    assert_has(&pairs, "staticmethod", SemanticKind::FunctionCall);
    assert_lacks(&pairs, "x", SemanticKind::TypeName);
}

#[test]
fn python_walrus_is_a_binding() {
    let pairs = semantic_pairs(
        Language::Python,
        "if (n := compute(data)) > 0:\n    use(n)\n",
    );
    assert_has(&pairs, "n", SemanticKind::VariableDefinition);
    assert_has(&pairs, "compute", SemanticKind::FunctionCall);
    assert_has(&pairs, "use", SemanticKind::FunctionCall);
}

#[test]
fn python_semantic_regressions() {
    let pairs = semantic_pairs(
        Language::Python,
        r#"
import os, pkg.mod
from lib import name, other as alias

def f(x: User, y=factory(default), *, z: Other = make()) -> Result:
    return x

a, (b, c) = row
d = e = make()
foo
(bar)
"#,
    );

    for name in ["os", "pkg", "name", "alias", "a", "b", "c", "d", "e"] {
        assert_has(&pairs, name, SemanticKind::VariableDefinition);
    }
    for name in ["x", "y", "z"] {
        assert_has(&pairs, name, SemanticKind::Parameter);
    }
    for name in ["User", "Other", "Result"] {
        assert_has(&pairs, name, SemanticKind::TypeName);
    }

    assert_has(&pairs, "factory", SemanticKind::FunctionCall);
    assert_has(&pairs, "make", SemanticKind::FunctionCall);
    assert_has(&pairs, "row", SemanticKind::Variable);
    assert_has(&pairs, "foo", SemanticKind::Variable);
    assert_lacks(&pairs, "foo", SemanticKind::FunctionCall);
    assert_lacks(&pairs, "factory", SemanticKind::Parameter);
    assert_lacks(&pairs, "default", SemanticKind::Parameter);
}

#[test]
fn markdown_python_fence_gets_semantics() {
    let src = "```python\nclass Widget:\n    def run(self):\n        return self.size\n```\n";
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
            text == "Widget"
                && *kind == SemanticKind::TypeDefinition
                && *lang == Language::Python.tag()
        }),
        "expected embedded python semantic token in {pairs:?}"
    );
    assert!(
        pairs.iter().any(|(text, kind, lang)| {
            text == "run"
                && *kind == SemanticKind::MethodDefinition
                && *lang == Language::Python.tag()
        }),
        "expected embedded python method in {pairs:?}"
    );
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
