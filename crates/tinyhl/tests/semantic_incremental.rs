use tinyhl::{Language, SemanticTable, Source, Span, TokenMutation, TokenTable};

fn apply(old: &str, span: Span, new: &str) -> String {
    let mut s = String::new();
    s.push_str(&old[..span.offset as usize]);
    s.push_str(new);
    s.push_str(&old[span.offset as usize + span.len as usize..]);
    s
}

fn semantic_dump(language: Language, src: &str) -> Vec<(Span, tinyhl::SemanticKind, bool, u8, u8)> {
    let source: &dyn Source = &src;
    let tokens = TokenTable::new(language, source);
    let semantic = SemanticTable::new(&tokens, source);
    semantic
        .query(Span::new(0, semantic.source_len()))
        .map(|token| {
            (
                token.span,
                token.kind,
                token.type_styled,
                token.lang_tag,
                token.nest,
            )
        })
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
        .map(|token| {
            (
                token.span,
                token.kind,
                token.type_styled,
                token.lang_tag,
                token.nest,
            )
        })
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
fn rust_pattern_semantic_insert_delete_sweep() {
    // Pattern regions add state (pattern_base, Pattern braces) that must stay
    // canonical across chunk boundaries, so a relexed suffix converges to a
    // fresh parse for an edit at every offset. Covers let/let-else, tuple,
    // slice, struct, scoped-path patterns, if-let, while-let, and for.
    let source = "fn main() {\n    let Some(binding) = Some(22) else { return; };\n    let crate::path::Type { name: [a, b] } = make();\n    let (x, y): (u8, u8) = pair();\n    for (k, v) in items() {}\n    if let Ok(value) = result() {}\n    while let None = opt() {}\n}\n";
    for off in 0..=source.len() {
        if !source.is_char_boundary(off) {
            continue;
        }
        for ins in ["x", "1", " ", ":", "(", "{", ".", "=", "Some", "let", "\n"] {
            assert_incremental_semantics(Language::Rust, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        if source.is_char_boundary(off) && source.is_char_boundary(off + 1) {
            assert_incremental_semantics(Language::Rust, source, Span::new(off as u32, 1), "");
        }
    }
}

#[test]
fn rust_generics_and_qualified_path_semantic_insert_delete_sweep() {
    // Covers state added for type-position `<...>` regions: generic parameter
    // lists on definitions (angle_type_pending + angle-scoped type context),
    // type-alias RHS contexts (started_alias_rhs), `impl Trait for Type`, and
    // leading qualified-path types `<T as Trait>::method`, including a
    // `match`-scrutinee qualified path. All introduce canonical state that must
    // round-trip across chunk boundaries, so a relexed suffix has to converge
    // to a fresh parse for an edit at every offset.
    let source = "type Alias<T> = Vec<T>;\nstruct Pair<A: Clone, const N: usize>(A);\ntrait Handler<Req>: Clone {\n    type Out;\n    fn handle<T: Send>(&self, r: Req) -> T;\n}\nimpl Display for Widget {\n    fn fmt(&self) -> Result { Ok(()) }\n}\nfn run<T>(x: T) {\n    let v = <Wrap as Trait>::from(x);\n    let n = size_of::<Item>() as usize * 2;\n    let m = match <Map as Trait>::get(x) { _ => 0 };\n}\n";
    for off in 0..=source.len() {
        if !source.is_char_boundary(off) {
            continue;
        }
        for ins in [
            "x", "1", " ", ":", "<", ">", "(", "{", "as", "::", "for", "T", "\n",
        ] {
            assert_incremental_semantics(Language::Rust, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        if source.is_char_boundary(off) && source.is_char_boundary(off + 1) {
            assert_incremental_semantics(Language::Rust, source, Span::new(off as u32, 1), "");
        }
    }
}

#[test]
fn rust_macro_attr_turbofish_semantic_insert_delete_sweep() {
    // Covers state added for: `macro_rules!` definition names
    // (macro_def_pending), metavariables (`$name` -> MetaVariable), member
    // access (`obj.field` -> FieldAccess), attribute bodies `#[...]`
    // (attr_bracket_depth), turbofish path-calls whose type args carry inner
    // `::` paths (`name::<a::B>(...)` pending-call preservation), `fn(T)`
    // pointer types, and `*const T::path` pointer paths. Every one introduces or
    // mutates canonical state that must round-trip across chunk boundaries, so a
    // relexed suffix has to converge to a fresh parse for an edit at every
    // offset.
    let source = "macro_rules! mac { ($x:expr, $f:ident) => { $f($x.field) } }\n#[derive(Debug, Clone)]\n#[rustfmt::skip]\nstruct S;\nfn run() {\n    let v = parse::<crate::rpc::Req>(payload);\n    let w = items.collect::<Vec<U::Out>>();\n    let n = parse::<[i64; 2]>(s);\n    let p = &x as *const _ as *const libc::c_void;\n    let q: ::core::cmp::Ordering = z;\n    let f: fn(i32) -> u8 = g;\n}\n";
    for off in 0..=source.len() {
        if !source.is_char_boundary(off) {
            continue;
        }
        for ins in [
            "x",
            "1",
            " ",
            ":",
            "(",
            "{",
            "<",
            ">",
            "!",
            "#",
            "[",
            ".",
            "$",
            "::",
            "fn",
            "macro_rules",
            "\n",
        ] {
            assert_incremental_semantics(Language::Rust, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        if source.is_char_boundary(off) && source.is_char_boundary(off + 1) {
            assert_incremental_semantics(Language::Rust, source, Span::new(off as u32, 1), "");
        }
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
fn tsx_semantic_incremental_matches_fresh() {
    // The TS analyzer runs over the TSX top level and re-enters across JSX
    // expression containers; edits that make or break JSX must keep the
    // semantic overlay identical to a fresh build.
    let source =
        "const make = (name: Name) => <div title={name}>{render(name)}</div>; const z = make(x);";
    let name = source.find("Name").unwrap();
    let render = source.find("render").unwrap();
    let cases: &[(usize, usize, &str)] = &[
        (0, 0, "export "),
        (6, 4, "build"),
        (name, 4, "Label"),
        (render, 6, "format"),
        (source.len(), 0, "\nconst w = <p/>;"),
    ];
    for &(off, len, rep) in cases {
        assert_incremental_semantics(
            Language::Tsx,
            source,
            Span::new(off as u32, len as u32),
            rep,
        );
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
fn cpp_semantic_incremental_matches_fresh() {
    let source = "namespace demo { struct point { int x; int y; }; static int distance(struct point *a, struct point *b) { return a->x + b->y; } }";
    for (off, len, rep) in [
        (0usize, 0usize, "inline "),
        (24, 5, "coord"),
        (59, 8, "metric"),
        (85, 1, "lhs"),
        (
            source.len(),
            0,
            " int main() { struct point p; return distance(&p, &p); }",
        ),
    ] {
        assert_incremental_semantics(
            Language::Cpp,
            source,
            Span::new(off as u32, len as u32),
            rep,
        );
    }
}

#[test]
fn python_semantic_incremental_matches_fresh() {
    let source = "class Point:\n    def __init__(self, x):\n        self.x = x\n\n    def norm(self) -> float:\n        return self.x\n\ndef make(scale):\n    p = Point(1)\n    return p.norm()\n\nresult = make(2)\nprint(result)\n";
    let init = source.find("__init__").unwrap();
    let scale = source.find("scale").unwrap();
    let cases: &[(usize, usize, &str)] = &[
        (0, 0, "import os\n"),
        (6, 5, "Vector"),
        (init, 8, "setup"),
        (scale, 5, "factor"),
        (source.len(), 0, "x: int = 0\n"),
    ];
    for &(off, len, rep) in cases {
        assert_incremental_semantics(
            Language::Python,
            source,
            Span::new(off as u32, len as u32),
            rep,
        );
    }
}

#[test]
fn python_semantic_insert_delete_sweep() {
    // The state machine must be deterministic on its token stream: a relexed
    // suffix has to converge to the same semantic stream as a fresh parse for
    // an edit at every offset. Covers def/class headers, member access,
    // annotations, calls, comprehensions, and walrus.
    let source = "class P:\n    def m(self, x: int):\n        self.x = x\n        return self.x\n\ndef f(a):\n    p = P()\n    ys = [p.m(i) for i in a if i]\n    return p.m(a)\n\nr = f(1)\nprint(r, café := 2)\n";
    for off in 0..=source.len() {
        if !source.is_char_boundary(off) {
            continue;
        }
        for ins in ["x", "1", " ", ":", "(", ".", "=", "@", "\n", "self"] {
            assert_incremental_semantics(Language::Python, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        // Stay on UTF-8 boundaries so the spliced source is valid.
        if source.is_char_boundary(off) && source.is_char_boundary(off + 1) {
            assert_incremental_semantics(Language::Python, source, Span::new(off as u32, 1), "");
        }
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

#[test]
fn html_embed_semantic_incremental_matches_fresh() {
    let source = "<style>.a { color: red }</style>\n<script>\nfunction make(arg: Foo): Foo { return build(arg); }\n</script>\n";
    let make = source.find("make").unwrap();
    let foo = source.find("Foo").unwrap();
    let build = source.find("build").unwrap();
    let cases: &[(usize, usize, &str)] = &[
        (0, 0, "<!-- top -->\n"),
        (make, 4, "create"),
        (foo, 3, "Widget"),
        (build, 5, "factory"),
        (source.len(), 0, "<p>tail</p>\n"),
    ];
    for &(off, len, rep) in cases {
        assert_incremental_semantics(
            Language::Html,
            source,
            Span::new(off as u32, len as u32),
            rep,
        );
    }
}

#[test]
fn large_semantic_insert_shifts_converged_tail() {
    let mut source = String::from("fn head(arg: Type) -> Type { arg }\n");
    for i in 0..260 {
        source.push_str(&format!(
            "fn item_{i}(param_{i}: Type) -> Type {{ helper_{i}(param_{i}) }}\n"
        ));
    }
    let off = source.find("fn item_140").unwrap();
    assert_incremental_semantics(Language::Rust, &source, Span::new(off as u32, 0), " ");
}

fn language() -> Language {
    Language::Rust
}
