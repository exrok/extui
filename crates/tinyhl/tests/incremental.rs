use tinyhl::{Language, Source, Span, Token, TokenTable};

fn tokens(language: Language, src: &str) -> Vec<Token> {
    let s: &dyn Source = &src;
    let t = TokenTable::new(language, s);
    t.query(Span::new(0, t.source_len())).collect()
}

fn apply(old: &str, span: Span, new: &str) -> String {
    let mut s = String::new();
    s.push_str(&old[..span.offset as usize]);
    s.push_str(new);
    s.push_str(&old[span.offset as usize + span.len as usize..]);
    s
}

fn assert_mutate_matches_lang(language: Language, old: &str, span: Span, replacement: &str) {
    let new = apply(old, span, replacement);
    let src_old: &dyn Source = &old;
    let mut table = TokenTable::new(language, src_old);
    let new_ref: &str = &new;
    let src_new: &dyn Source = &new_ref;
    let invalidated = table.mutate(src_new, span, replacement.len() as u32);

    let mutated: Vec<Token> = table.query(Span::new(0, table.source_len())).collect();
    let fresh = tokens(language, &new);

    assert_eq!(
        mutated, fresh,
        "mutate != fresh parse ({language:?})\nold:  {old:?}\nnew:  {new:?}\nedit: {span:?} -> {replacement:?}"
    );

    let edit_span = Span::new(span.offset, replacement.len() as u32);
    for t in &mutated {
        if t.span.overlaps(&edit_span) {
            assert!(
                t.span.overlaps(&invalidated),
                "token {:?} overlaps edit {:?} but not invalidated {:?}",
                t.span,
                edit_span,
                invalidated
            );
        }
    }
}

fn assert_mutate_matches(old: &str, span: Span, replacement: &str) {
    assert_mutate_matches_lang(Language::Json, old, span, replacement);
}

#[test]
fn insert_at_every_offset() {
    let source = r#"[1, 2.5, "hi", true, null, {"a": 9}]"#;
    for off in 0..=source.len() {
        for ins in ["x", "9", "\"", " ", "{", ","] {
            assert_mutate_matches(source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn delete_single_byte_at_every_offset() {
    let source = r#"[1, 2.5, "hi", true, null, {"a": 9}]"#;
    for off in 0..source.len() {
        assert_mutate_matches(source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn replace_ranges_in_small_json() {
    let source = r#"{"a": 1, "b": "two", "c": [true, null]}"#;
    let cases: &[(usize, usize, &str)] = &[
        (0, 0, "[1] "),
        (6, 1, "42"),
        (13, 5, r#""changed""#),
        (25, 10, "{}"),
        (source.len(), 0, "\n"),
    ];
    for &(off, len, rep) in cases {
        assert_mutate_matches(source, Span::new(off as u32, len as u32), rep);
    }
}

#[test]
fn rust_insert_sweep() {
    let source = "fn f(x: i32) -> i32 { let s = \"hi\"; x + 1 }";
    for off in 0..=source.len() {
        for ins in ["x", "9", "\"", " ", "{", "/", "*", "'", "r"] {
            assert_mutate_matches_lang(Language::Rust, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn rust_delete_sweep() {
    let source = "fn f(x: i32) -> i32 { let s = \"hi\"; x + 1 }";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Rust, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn rust_replace_sweep() {
    let source = "let x = 0x1F_u32; let y = 'a'; let z = r#\"raw\"#;";
    for off in 0..source.len() {
        for rep in ["x", "/", "*", "\"", "'", "#"] {
            assert_mutate_matches_lang(Language::Rust, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn rust_multiline_string_sweep() {
    // Exercise every offset in a source with a multi-line string so that
    // inserts inside, before, and after the string all converge.
    let source = "let s = \"line 1\nline 2\nline 3\";\nlet t = 42;\n";
    for off in 0..=source.len() {
        for ins in ["x", "\n", "\\", "\"", " ", "/*"] {
            assert_mutate_matches_lang(Language::Rust, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Rust, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn rust_multiline_edits() {
    let source = "/* comment\nstill */ fn f() { /* nested /* */ */ }";
    let cases: &[(usize, usize, &str)] = &[
        (0, 0, "// line\n"),
        (2, 1, "/"),
        (11, 1, ""),
        (source.len(), 0, "\nfn g() {}"),
    ];
    for &(off, len, rep) in cases {
        assert_mutate_matches_lang(
            Language::Rust,
            source,
            Span::new(off as u32, len as u32),
            rep,
        );
    }
}

#[test]
fn rust_boundary_insert_extends_previous_token() {
    // Regression for a fuzz crash: two inserts at what, after the first
    // edit, becomes a chunk boundary. The previous chunk's trailing token
    // must be re-lexed so the adjacent whitespace merges into one token.
    let seed = "fn main() { let x = 1; }";
    let src_old: &dyn Source = &seed;
    let mut table = TokenTable::new(Language::Rust, src_old);

    let step1 = apply(seed, Span::new(0, 0), " ");
    let step1_ref: &str = &step1;
    let src1: &dyn Source = &step1_ref;
    table.mutate(src1, Span::new(0, 0), 1);
    assert_eq!(
        tokens(Language::Rust, &step1),
        table
            .query(Span::new(0, table.source_len()))
            .collect::<Vec<_>>(),
    );

    let step2 = apply(&step1, Span::new(1, 0), " ");
    let step2_ref: &str = &step2;
    let src2: &dyn Source = &step2_ref;
    let invalidated = table.mutate(src2, Span::new(1, 0), 1);
    let mutated: Vec<Token> = table.query(Span::new(0, table.source_len())).collect();
    let fresh = tokens(Language::Rust, &step2);
    assert_eq!(
        mutated, fresh,
        "boundary insert must re-merge leading whitespace"
    );
    // The first token's span must be fully covered by the invalidated span.
    assert!(
        mutated[0].span.overlaps(&invalidated),
        "first token {:?} should overlap invalidated {:?}",
        mutated[0].span,
        invalidated
    );
}

#[test]
fn rust_chain_of_edits_stays_in_sync() {
    let mut current = String::from("fn main() {}");
    let src: &dyn Source = &current.as_str();
    let mut table = TokenTable::new(Language::Rust, src);

    let edits: &[(usize, usize, &str)] = &[
        (10, 0, " let x = 1; "),
        (12, 1, "mut y"),
        (0, 2, "pub fn"),
        (current.len(), 0, "\n// trailing\n"),
    ];

    for &(off, len, rep) in edits {
        let off = off.min(current.len());
        let len = len.min(current.len() - off);
        let edited = apply(&current, Span::new(off as u32, len as u32), rep);
        let edited_ref: &str = &edited;
        let s: &dyn Source = &edited_ref;
        table.mutate(s, Span::new(off as u32, len as u32), rep.len() as u32);

        let fresh = TokenTable::new(Language::Rust, s);
        let mutated: Vec<Token> = table.query(Span::new(0, table.source_len())).collect();
        let expected: Vec<Token> = fresh.query(Span::new(0, fresh.source_len())).collect();
        assert_eq!(mutated, expected, "diverged after edit on {edited:?}");

        current = edited;
    }
}

#[test]
fn rust_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/rust/simple.rs.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    // Exercise a handful of offsets across a multi-chunk file.
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\"", "/*"] {
            assert_mutate_matches_lang(Language::Rust, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn c_insert_sweep() {
    let source = "int main() { char *s = \"hi\"; return 0; }";
    for off in 0..=source.len() {
        for ins in ["x", "9", "\"", " ", "{", "/", "*", "'", "#", "\\", "0x"] {
            assert_mutate_matches_lang(Language::C, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn c_delete_sweep() {
    let source = "int main() { char *s = \"hi\"; return 0; }";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::C, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn c_replace_sweep() {
    let source = "int n = 0xFFu; float f = 1.5e-3f; char c = '\\n';";
    for off in 0..source.len() {
        for rep in ["x", "/", "*", "\"", "'", "#", "0", "."] {
            assert_mutate_matches_lang(Language::C, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn c_multiline_comment_edits() {
    let source = "int a; /* comment\nspanning\nlines */ int b;\n";
    for off in 0..=source.len() {
        for ins in ["x", "\n", "*", "/", "\"", " "] {
            assert_mutate_matches_lang(Language::C, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::C, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn c_multiline_string_sweep() {
    let source = "const char *s = \"line 1\nline 2\nline 3\";\nint x = 42;\n";
    for off in 0..=source.len() {
        for ins in ["x", "\n", "\\", "\"", " ", "/*"] {
            assert_mutate_matches_lang(Language::C, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn c_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/c/simple.c.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\"", "/*", "//"] {
            assert_mutate_matches_lang(Language::C, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn c_kr_fixture_sweep() {
    let path = format!("{}/fixtures/c/kr.c.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 96).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\"", "/*", "//", "0x"] {
            assert_mutate_matches_lang(Language::C, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn cpp_insert_sweep() {
    let source = "namespace n { struct P { int x; }; auto s = R\"tag(a)tag\"; auto n = 1'000; }";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", "\"", " ", "{", "/", "*", "'", "#", "\\", "0x", "R", ":", "<",
        ] {
            assert_mutate_matches_lang(Language::Cpp, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn cpp_delete_sweep() {
    let source = "namespace n { struct P { int x; }; auto s = R\"tag(a)tag\"; auto n = 1'000; }";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Cpp, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn cpp_replace_sweep() {
    let source = "auto n = 0xFF'AAu; auto cmp = a <=> b; auto m = obj.*ptr + p->*q;";
    for off in 0..source.len() {
        for rep in ["x", "/", "*", "\"", "'", "#", "0", ".", ":", "<", "R"] {
            assert_mutate_matches_lang(Language::Cpp, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn cpp_raw_string_sweep() {
    let source = "auto s = R\"tag(line 1\nline \"/* two */\"\n)tag\";\nauto t = 42;\n";
    for off in 0..=source.len() {
        for ins in ["x", "\n", "\\", "\"", " ", "/*", ")", "tag"] {
            assert_mutate_matches_lang(Language::Cpp, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Cpp, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn cpp_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/cpp/simple.cpp.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 96).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\"", "/*", "//", "0x", "R\"", "::", "<=>"] {
            assert_mutate_matches_lang(Language::Cpp, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn go_insert_sweep() {
    let source =
        "package main\nfunc main() { ch := make(chan int); go func(){ ch <- 1 }(); _ = <-ch }\n";
    for off in 0..=source.len() {
        for ins in ["x", "9", "\"", "`", "'", " ", "{", "/", "*", "<", "&", "0x"] {
            assert_mutate_matches_lang(Language::Go, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn go_delete_sweep() {
    let source =
        "package main\nfunc main() { ch := make(chan int); go func(){ ch <- 1 }(); _ = <-ch }\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Go, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn go_replace_sweep() {
    let source = "n := 0xFF; f := 1.5e-3i; r := 'x'; s := `raw`; n &^= 1; xs := []int{1,2,3}";
    for off in 0..source.len() {
        for rep in ["x", "/", "*", "\"", "'", "`", "&", "<", ".", "0", "e"] {
            assert_mutate_matches_lang(Language::Go, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn go_raw_string_sweep() {
    let source = "package main\nvar s = `line 1\nline 2\nline 3`\nvar t = 42\n";
    for off in 0..=source.len() {
        for ins in ["x", "\n", "\\", "`", "\"", " ", "/*"] {
            assert_mutate_matches_lang(Language::Go, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Go, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn go_unicode_ident_sweep() {
    let source = "var caf\u{00e9} = 1\nfunc \u{03c0}() int { return caf\u{00e9} }\n";
    for off in 0..=source.len() {
        if !source.is_char_boundary(off) {
            continue;
        }
        for ins in ["x", "\u{03b1}", "\"", "`", " "] {
            assert_mutate_matches_lang(Language::Go, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn go_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/go/simple.go.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 96).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\"", "`", "'", "/*", "//", "0x", "<-", "&^"] {
            assert_mutate_matches_lang(Language::Go, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn sh_insert_sweep() {
    let source = "if [ \"$x\" = yes ]; then echo \"$x\" # ok\nfi\n";
    for off in 0..=source.len() {
        for ins in ["x", "9", "\"", "'", "`", " ", "#", "$", "{", "<", "&", "\\"] {
            assert_mutate_matches_lang(Language::Sh, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn sh_delete_sweep() {
    let source = "if [ \"$x\" = yes ]; then echo \"$x\" # ok\nfi\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Sh, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn sh_replace_sweep() {
    let source = "name=${USER:-guest}; printf '%s\\n' \"$name\" >>out.log || echo fail";
    for off in 0..source.len() {
        for rep in [
            "x", "0", "'", "\"", "`", "#", "$", "{", "}", "<", ">", "&", "|",
        ] {
            assert_mutate_matches_lang(Language::Sh, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn sh_quote_sweep() {
    let source = "name='a b'\nmsg=\"line $name\"\ncmd=`printf %s \"$msg\"`\n";
    for off in 0..=source.len() {
        for ins in ["x", "\n", "\\", "'", "\"", "`", "$", "#", " "] {
            assert_mutate_matches_lang(Language::Sh, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Sh, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn sh_heredoc_sweep() {
    let source = "cat <<EOF\nhello $USER\n# not comment\nEOF\necho done\n";
    for off in 0..=source.len() {
        for ins in ["x", "\n", "\\", "'", "\"", "#", "$", "<", "-"] {
            assert_mutate_matches_lang(Language::Sh, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Sh, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn sh_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/sh/simple.sh.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 96).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "'", "\"", "`", "#", "$", "<<", "&&", "||"] {
            assert_mutate_matches_lang(Language::Sh, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn ts_insert_sweep() {
    let source = "const x: number = 42; let s = `hi ${x}`; // ok\n";
    for off in 0..=source.len() {
        for ins in ["x", "9", "\"", "'", "`", " ", "{", "/", "*", "$", "#"] {
            assert_mutate_matches_lang(Language::Ts, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn ts_delete_sweep() {
    let source = "const x: number = 42; let s = `hi ${x}`; // ok\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Ts, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn ts_regex_replace_sweep() {
    let source = "const r = /ab+c/gi; const q = a / b; return /x/;";
    for off in 0..source.len() {
        for rep in ["x", "/", "\\", "[", "]", "\n", " "] {
            assert_mutate_matches_lang(Language::Ts, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn ts_template_sweep() {
    let source = "const t = `pre ${`in ${1+2}`} post`;\nconst u = 1;\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "$", "{", "}", "\n", "\\"] {
            assert_mutate_matches_lang(Language::Ts, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Ts, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn ts_unicode_ident_sweep() {
    // Edits around Unicode identifiers exercise UTF-8 decoding inside the
    // shared XID scanner.
    let source = "const αβγ = 1; let 日本語 = `hi`;\n";
    for off in 0..=source.len() {
        // Only byte offsets on UTF-8 boundaries are valid edit points.
        if !source.is_char_boundary(off) {
            continue;
        }
        for ins in ["x", "α", "漢", " ", "`"] {
            assert_mutate_matches_lang(Language::Ts, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn rust_unicode_ident_sweep() {
    let source = "fn пример() -> u32 { let αβ = 1; αβ + 2 }\n";
    for off in 0..=source.len() {
        if !source.is_char_boundary(off) {
            continue;
        }
        for ins in ["x", "α", "\"", " "] {
            assert_mutate_matches_lang(Language::Rust, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn ts_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/ts/simple.ts.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\"", "'", "`", "/*", "//", "${", "0x"] {
            assert_mutate_matches_lang(Language::Ts, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn tsx_insert_sweep() {
    let source = "const e = <div id=\"a\" on={h}>hi {x + 1} <b/></div>;\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "<", ">", "/", "=", "\"", "'", "{", "}", "`", " ", "\n", "&", "$",
        ] {
            assert_mutate_matches_lang(Language::Tsx, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn tsx_delete_sweep() {
    let source = "const e = <div id=\"a\" on={h}>hi {x + 1} <b/></div>;\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Tsx, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn tsx_replace_sweep() {
    let source = "return cond ? <a href='u'>{x}</a> : <>frag</>;";
    for off in 0..source.len() {
        for rep in ["x", "<", ">", "/", "{", "}", "\"", "'", "&", " ", "="] {
            assert_mutate_matches_lang(Language::Tsx, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn tsx_jsx_vs_operator_sweep() {
    // Edits flip `<` between comparison/shift operators and JSX openers, and
    // make or break the JSX trigger (the byte after `<`). Each must keep
    // `mutate` identical to a fresh parse.
    let source = "const a = b < c; const d = <e/>; let f = g << 2;\n";
    for off in 0..=source.len() {
        for ins in ["x", "<", ">", "/", " ", "{", "}", "=", "\n"] {
            assert_mutate_matches_lang(Language::Tsx, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Tsx, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn tsx_nested_expr_and_template_sweep() {
    // Deep recursion: TSX → JSX → `{…}` → TSX → template literal that itself
    // embeds an expression. State must survive edits at every boundary.
    let source = "const v = <p>{`a ${q}` + h(<i>{n}</i>)}</p>;\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "$", "{", "}", "<", ">", "/", " ", "\n"] {
            assert_mutate_matches_lang(Language::Tsx, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Tsx, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn tsx_unicode_ident_sweep() {
    let source = "const δ = <Δ ω={x}>{φ}</Δ>;\n";
    for off in 0..=source.len() {
        if !source.is_char_boundary(off) {
            continue;
        }
        for ins in ["x", "α", "<", "{", "}", " "] {
            assert_mutate_matches_lang(Language::Tsx, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn tsx_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/tsx/simple.tsx.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 96).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", "<", ">", "/", "\"", "'", "{", "}", "`", " ", "&"] {
            assert_mutate_matches_lang(Language::Tsx, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn markdown_tsx_nested_embed_sweep() {
    // Markdown → TSX → JSX → `{…}` → TSX. Every edit at every offset must keep
    // `mutate` identical to a fresh parse across all four nesting levels.
    let source = "# T\n\n```tsx\nconst e = <p>{x + 1}</p>;\n```\n\nend\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "<", ">", "/", "{", "}", " ", "\n"] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn markdown_insert_sweep() {
    let source = "# Hi\n\n*em* and `code` and [a](b).\n\n- one\n- two\n> quote\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "#", "*", "_", "`", "[", "]", "(", ")", "\n", " ", ">", "-",
        ] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn markdown_delete_sweep() {
    let source = "# Hi\n\n*em* and `code` and [a](b).\n\n- one\n- two\n> quote\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn markdown_replace_sweep() {
    let source = "# Hi\n\n**bold** _em_ `c` [t](u)\n";
    for off in 0..source.len() {
        for rep in ["x", "#", "*", "_", "`", "[", "]", "(", ")", "\n"] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn markdown_with_rust_embed_sweep() {
    let source = "# Hi\n\n```rust\nfn f() { 1 }\n```\n\nafter\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "\n", " ", "{", "/", "r"] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn markdown_with_c_and_json_embeds_sweep() {
    let source = "intro\n\n```c\nint x;\n```\n\nmid\n\n```json\n{\"a\":1}\n```\n\nend\n";
    let step = (source.len() / 48).max(1);
    for off in (0..=source.len()).step_by(step) {
        for ins in ["x", "`", "\n", "{", "\""] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn markdown_fixture_sweep_over_chunks() {
    let path = format!(
        "{}/fixtures/markdown/simple.md.in",
        env!("CARGO_MANIFEST_DIR")
    );
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", "`", "\n", " ", "#", "*"] {
            assert_mutate_matches_lang(Language::Markdown, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn xml_insert_sweep() {
    let source =
        r#"<?xml version="1.0"?><root a="b">text &amp; <![CDATA[x]]><!--c--><empty/></root>"#;
    for off in 0..=source.len() {
        for ins in [
            "x",
            "<",
            ">",
            "/",
            "?",
            "!",
            "\"",
            "'",
            " ",
            "&",
            ";",
            "<!--",
            "<![CDATA[",
        ] {
            assert_mutate_matches_lang(Language::Xml, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn xml_delete_sweep() {
    let source =
        r#"<?xml version="1.0"?><root a="b">text &amp; <![CDATA[x]]><!--c--><empty/></root>"#;
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Xml, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn xml_replace_sweep() {
    let source = r#"<root xmlns:h="urn:h"><h:item name='one'>value</h:item><empty/></root>"#;
    for off in 0..source.len() {
        for rep in ["x", "<", ">", "/", "=", "\"", "'", "&", " "] {
            assert_mutate_matches_lang(Language::Xml, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn xml_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/xml/simple.xml.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", "<", ">", "\"", "'", "&", "<!--", "<![CDATA["] {
            assert_mutate_matches_lang(Language::Xml, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn csv_insert_sweep() {
    let source = "name,age\nAlice,42\n\"a,b\",-1\n";
    for off in 0..=source.len() {
        for ins in ["x", ",", "\"", "\n", "\r\n", "5"] {
            assert_mutate_matches_lang(Language::Csv, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn csv_delete_sweep() {
    let source = "name,age\nAlice,42\n\"a,b\",-1\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Csv, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn csv_multiline_quoted_sweep() {
    let source = "id,note\r\n1,\"line 1\nline 2, with comma\nline 3\"\r\n2,\"x\"\"y\"\n";
    for off in 0..=source.len() {
        for ins in ["x", ",", "\"", "\n", "\r\n", "9"] {
            assert_mutate_matches_lang(Language::Csv, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Csv, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn csv_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/csv/simple.csv.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    for off in 0..=source.len() {
        for rep in ["x", ",", "\"", "\n"] {
            assert_mutate_matches_lang(Language::Csv, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn css_insert_sweep() {
    let source = ".btn#x:hover { color: #fff; width: calc(50% - 10px) !important; }";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", "\"", "'", " ", "{", "}", "(", ")", "[", "]", "/", "*", "#", "@", ".", ":",
            ";", "%", "\\", "-",
        ] {
            assert_mutate_matches_lang(Language::Css, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn css_delete_sweep() {
    let source = ".btn#x:hover { color: #fff; width: calc(50% - 10px) !important; }";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Css, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn css_replace_sweep() {
    let source = "@media (w:1px){a{background:url(a.png) #f00;font:1e3px/2 'x'}}";
    for off in 0..source.len() {
        for rep in [
            "x", "/", "*", "\"", "'", "#", "@", "(", ")", "0", ".", "%", "e",
        ] {
            assert_mutate_matches_lang(Language::Css, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn css_multiline_comment_and_url_sweep() {
    let source = "/* a\nmulti\nline */ .x {\n  background: url(path/to/img.png);\n}\n";
    for off in 0..=source.len() {
        for ins in ["x", "\n", "*", "/", "\"", " ", ")", "("] {
            assert_mutate_matches_lang(Language::Css, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Css, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn css_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/css/simple.css.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\"", "/*", "#", "@", "url(", ")", "%"] {
            assert_mutate_matches_lang(Language::Css, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn html_insert_sweep() {
    let source = "<div id=\"a\"><style>b{color:red}</style><p>hi &amp; bye</p>\
                  <script>let x=`v`;</script></div>";
    for off in 0..=source.len() {
        for ins in [
            "x", "<", ">", "/", "=", "\"", " ", "&", "{", "}", "`", ";", "#",
        ] {
            assert_mutate_matches_lang(Language::Html, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn html_delete_sweep() {
    let source = "<div id=\"a\"><style>b{color:red}</style><p>hi &amp; bye</p>\
                  <script>let x=`v`;</script></div>";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Html, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn html_replace_sweep() {
    let source = "<!DOCTYPE html><a href='x'>t</a><!--c-->&copy;<br/>";
    for off in 0..source.len() {
        for rep in ["x", "<", ">", "/", "=", "\"", "'", "&", " ", "!", "?"] {
            assert_mutate_matches_lang(Language::Html, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn html_style_embed_sweep() {
    // Every edit in and around a `<style>` block must keep the embedded CSS
    // tokens in sync, including edits that make or break the `</style>` closer.
    let source =
        "<style>\n  .x::before { content: \"\\2014\"; width: calc(1% - 2px); }\n</style>after";
    for off in 0..=source.len() {
        for ins in ["x", "<", ">", "/", "{", "}", "\"", " ", "\n", "%", "#"] {
            assert_mutate_matches_lang(Language::Html, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Html, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn html_script_embed_sweep() {
    // The `<script>` body is TypeScript: template literals, regex, and
    // comments all carry lexer state that must survive edits at the embed
    // boundary.
    let source = "<script>\n  const r = /ab+c/g;\n  let s = `hi ${x}`; // ok\n</script>tail";
    for off in 0..=source.len() {
        for ins in ["x", "<", ">", "/", "`", "$", "{", "}", "\n", " ", "*"] {
            assert_mutate_matches_lang(Language::Html, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Html, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn html_fixture_sweep_over_chunks() {
    let path = format!(
        "{}/fixtures/html/simple.html.in",
        env!("CARGO_MANIFEST_DIR")
    );
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", "<", ">", "/", "\"", "&", "{", "}", "`", "<!--"] {
            assert_mutate_matches_lang(Language::Html, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn markdown_html_nested_embed_sweep() {
    // Two levels of embedding: Markdown → HTML → CSS / TS. Every edit, at
    // every offset, must keep `mutate` identical to a fresh parse — including
    // edits that make or break the Markdown fence, the HTML tags, and the
    // inner `<style>` / `<script>` rawtext boundaries.
    let source = "# T\n\n```html\n<style>b{color:red}</style>\n<p>hi</p>\n\
                  <script>let x=`v`;</script>\n```\n\nend\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "<", ">", "/", "{", "}", " ", "\n", "&", "$"] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn python_insert_sweep() {
    let source = "def f(x: int) -> int:\n    return x // 2 ** 1  # ok\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", "\"", "'", " ", "{", "(", "/", "*", "#", "\\", ":", "0x", "f\"",
        ] {
            assert_mutate_matches_lang(Language::Python, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn python_delete_sweep() {
    let source = "def f(x: int) -> int:\n    return x // 2 ** 1  # ok\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Python, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn python_replace_sweep() {
    let source = "n = 0xFF_u; xs = [1, 2.5e-3, 3j]; s = r'a\\b'; t = (m := 10)";
    for off in 0..source.len() {
        for rep in ["x", "/", "*", "\"", "'", "#", "0", ".", ":", "=", "\\"] {
            assert_mutate_matches_lang(Language::Python, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn python_triple_string_sweep() {
    // A docstring with embedded quotes and newlines: every edit, inside,
    // before, and after the string must converge.
    let source = "x = 1\ns = \"\"\"line 1\n'line' 2\nline 3\"\"\"\ny = 42\n";
    for off in 0..=source.len() {
        for ins in ["x", "\n", "\\", "\"", "'", " ", "#"] {
            assert_mutate_matches_lang(Language::Python, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Python, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn python_string_prefix_sweep() {
    // Edits that make or break string prefixes (r/b/f/u and combos) flip the
    // bytes between identifiers and string literals.
    let source = "a = rb'x'; b = f\"{v}\"; c = u'y'; d = forks; e = breaker\n";
    for off in 0..=source.len() {
        for ins in ["r", "b", "f", "u", "\"", "'", "x", " "] {
            assert_mutate_matches_lang(Language::Python, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Python, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn python_number_sweep() {
    let source = "a=0xFF;b=0o7;c=0b1;d=1_0.5e-3;e=.5;f=3j;g=1.";
    for off in 0..source.len() {
        for rep in ["x", "0", ".", "e", "j", "_", "o", "b", "+", "-"] {
            assert_mutate_matches_lang(Language::Python, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn python_unicode_ident_sweep() {
    let source = "café = 1\nΩ = café + 1\nπ = '日本'\n";
    for off in 0..=source.len() {
        if !source.is_char_boundary(off) {
            continue;
        }
        for ins in ["x", "α", "漢", " ", "'", "="] {
            assert_mutate_matches_lang(Language::Python, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn python_fixture_sweep_over_chunks() {
    let path = format!(
        "{}/fixtures/python/simple.py.in",
        env!("CARGO_MANIFEST_DIR")
    );
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 96).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\"", "'", "#", "\"\"\"", "0x", ":", "//"] {
            assert_mutate_matches_lang(Language::Python, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn markdown_python_embed_sweep() {
    let source = "# T\n\n```python\ndef f(x):\n    return x // 2  # c\n```\n\nend\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "\n", " ", "#", "\"", ":", "/"] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn sql_insert_sweep() {
    let source = "SELECT id, name FROM users WHERE active = TRUE AND score >= 10;\n";
    for off in 0..=source.len() {
        for ins in ["x", "9", "'", "\"", " ", "-", "/", "*", "$", "[", "]", "."] {
            assert_mutate_matches_lang(Language::Sql, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn sql_delete_sweep() {
    let source = "SELECT id, name FROM users WHERE active = TRUE AND score >= 10;\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Sql, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn sql_replace_sweep() {
    let source = "/* c */ SELECT 'it''s', $$body$$, \"col\" FROM t WHERE x<>1 AND y::int:=.5e+2";
    for off in 0..source.len() {
        for rep in ["x", "0", "'", "\"", "$", "-", "/", "*", ":", "=", ".", "["] {
            assert_mutate_matches_lang(Language::Sql, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn sql_string_sweep() {
    let source = "SELECT 'line 1\nline 2', E'a\\'b', q'[x''y]', $tag$a $ b$tag$;\n";
    for off in 0..=source.len() {
        for ins in ["x", "\n", "\\", "'", "$", "]", " ", "-"] {
            assert_mutate_matches_lang(Language::Sql, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Sql, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn sql_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/sql/simple.sql.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 96).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "'", "\"", "--", "/*", "$$", "0x", "::"] {
            assert_mutate_matches_lang(Language::Sql, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn markdown_sql_embed_sweep() {
    let source = "# T\n\n```sql\nSELECT id FROM users WHERE active = TRUE;\n```\n\nend\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "\n", " ", "'", "-", "/", "$"] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn markdown_go_embed_sweep() {
    let source = "# T\n\n```go\npackage main\nfunc main() { println(\"hi\") }\n```\n\nend\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "\n", " ", "\"", "'", "/", "<", "&"] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn yaml_insert_sweep() {
    let source = "---\nname: tinyhl\nitems:\n  - true\n  - \"quoted\"\nflow: [1, null, {a: b}]\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", "\"", "'", " ", "-", "#", ":", "[", "]", "{", "}", "!", "&", "*", "/", "\n",
        ] {
            assert_mutate_matches_lang(Language::Yaml, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn yaml_delete_sweep() {
    let source = "---\nname: tinyhl\nitems:\n  - true\n  - \"quoted\"\nflow: [1, null, {a: b}]\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Yaml, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn yaml_replace_sweep() {
    let source =
        "a: 1\nb: 'it''s'\nc: \"x\\\"y\"\nd: /tmp/path#frag # comment\nrefs: [&base, *base]\n";
    for off in 0..source.len() {
        for rep in [
            "x", "0", "'", "\"", "#", "-", ":", ".", "!", "&", "*", "[", "]",
        ] {
            assert_mutate_matches_lang(Language::Yaml, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn yaml_fixture_sweep_over_chunks() {
    let path = format!(
        "{}/fixtures/yaml/simple.yaml.in",
        env!("CARGO_MANIFEST_DIR")
    );
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "'", "\"", "#", "---", "...", "[", "{", "&", "!"] {
            assert_mutate_matches_lang(Language::Yaml, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn markdown_yaml_embed_sweep() {
    let source = "# T\n\n```yaml\nname: tinyhl\nitems:\n  - true\n```\n\nend\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "\n", " ", "\"", "'", "#", "-", ":", "["] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn lua_insert_sweep() {
    let source = "global<const> *\nlocal t = {name = \"hi\"}; if t.name ~= nil then print(t.name .. [[x]]) end\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", "\"", "'", "[", "]", "-", "=", "<", ">", ".", "\n", "\\",
        ] {
            assert_mutate_matches_lang(Language::Lua, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn lua_delete_sweep() {
    let source = "global<const> *\nlocal t = {name = \"hi\"}; if t.name ~= nil then print(t.name .. [[x]]) end\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Lua, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn lua_replace_sweep() {
    let source = "n=0x1.fp3; m=.5e-2; s='a\\z\n b'; bits=(n << 1) | (~n >> 2); q=n//2";
    for off in 0..source.len() {
        for rep in [
            "x", "0", ".", "e", "p", "'", "\"", "[", "]", "-", "=", "<", ">", "/",
        ] {
            assert_mutate_matches_lang(Language::Lua, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn lua_long_bracket_sweep() {
    let source =
        "--[=[comment\n--[[nested-looking]]\n]=]\nlocal s = [==[line 1\n]===]\nline 3]==]\n";
    for off in 0..=source.len() {
        for ins in ["x", "\n", "[", "]", "=", "-", "--", "[=["] {
            assert_mutate_matches_lang(Language::Lua, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Lua, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn lua_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/lua/simple.lua.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 96).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "'", "\"", "--", "[=[", "]", "0x", "::", ".."] {
            assert_mutate_matches_lang(Language::Lua, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn markdown_lua_embed_sweep() {
    let source = "# T\n\n```lua\nglobal<const> *\nlocal s = [=[hi]=]\n```\n\nend\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "\n", " ", "'", "\"", "-", "[", "]", "="] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn make_insert_sweep() {
    let source = ".PHONY: all\nCC := cc\nall: main.o\n\t$(CC) -o $@ main.o # build\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", "$", "(", ")", "\"", "'", "#", ":", "=", "+", "?", "\n",
        ] {
            assert_mutate_matches_lang(Language::Make, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn make_delete_sweep() {
    let source = ".PHONY: all\nCC := cc\nall: main.o\n\t$(CC) -o $@ main.o # build\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Make, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn make_replace_sweep() {
    let source = "ifeq ($(OS),Linux)\nNAME ?= tinyhl\nCFLAGS += -O2\nmsg = \"hi\"\nendif\n";
    for off in 0..source.len() {
        for rep in [
            "x", "0", "$(", "${", ")", "}", "'", "\"", "#", ":", "::", ":=",
        ] {
            assert_mutate_matches_lang(Language::Make, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn make_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/make/simple.mk.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "$@", "$(", "\"", "#", ":=", "\n"] {
            assert_mutate_matches_lang(Language::Make, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn markdown_make_embed_sweep() {
    let source = "# T\n\n```make\n.PHONY: all\nall:\n\t@echo ok\n```\n\nend\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "\n", " ", "$", "#", ":", "="] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn cmake_insert_sweep() {
    let source = "project(tinyhl)\nset(NAME \"tinyhl\")\nif(ON AND NOT OFF)\nendif()\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", "$", "{", "}", "\"", "#", "(", ")", "[", "]", "=", "\n",
        ] {
            assert_mutate_matches_lang(Language::Cmake, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn cmake_delete_sweep() {
    let source = "project(tinyhl)\nset(NAME \"tinyhl\")\nif(ON AND NOT OFF)\nendif()\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Cmake, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn cmake_replace_sweep() {
    let source = "set(TEXT [=[hello\nworld]=])\nmessage(${TEXT})\n#[=[comment]=]\n";
    for off in 0..source.len() {
        for rep in ["x", "0", "$", "${", "}", "\"", "#", "[", "]", "=", "(", ")"] {
            assert_mutate_matches_lang(Language::Cmake, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn cmake_fixture_sweep_over_chunks() {
    let path = format!(
        "{}/fixtures/cmake/simple.cmake.in",
        env!("CARGO_MANIFEST_DIR")
    );
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "${VAR}", "$<A:B>", "\"", "#", "[=[", "]", "\n"] {
            assert_mutate_matches_lang(Language::Cmake, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn markdown_cmake_embed_sweep() {
    let source = "# T\n\n```cmake\nproject(tinyhl)\nset(TEXT [=[ok]=])\n```\n\nend\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "\n", " ", "$", "#", "[", "]", "(", ")"] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn protobuf_insert_sweep() {
    let source =
        "syntax = \"proto3\";\nmessage M { optional string name = 1 [deprecated = false]; }\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", "\"", "'", " ", "{", "}", "(", ")", "[", "]", "/", "*", ".", "=", "<", ">",
        ] {
            assert_mutate_matches_lang(Language::Protobuf, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn protobuf_delete_sweep() {
    let source =
        "syntax = \"proto3\";\nmessage M { optional string name = 1 [deprecated = false]; }\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Protobuf, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn protobuf_replace_sweep() {
    let source =
        "message M { map<string, bytes> labels = 1; double ratio = 2 [default = -1.5e+2]; }";
    for off in 0..source.len() {
        for rep in [
            "x", "0", "\"", "'", "/", "*", ".", "=", "<", ">", "+", "-", "[",
        ] {
            assert_mutate_matches_lang(Language::Protobuf, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn protobuf_string_comment_sweep() {
    let source = "message M {\n  // comment\n  string s = 1 [default = \"line\\nvalue\"];\n  /* block */ bytes b = 2;\n}\n";
    for off in 0..=source.len() {
        for ins in ["x", "\n", "\\", "\"", "'", "/", "*", " "] {
            assert_mutate_matches_lang(Language::Protobuf, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Protobuf, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn protobuf_fixture_sweep_over_chunks() {
    let path = format!(
        "{}/fixtures/protobuf/simple.proto.in",
        env!("CARGO_MANIFEST_DIR")
    );
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\"", "'", "/*", "//", "0x", "[", "<", "returns"] {
            assert_mutate_matches_lang(Language::Protobuf, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn markdown_protobuf_embed_sweep() {
    let source =
        "# T\n\n```proto\nsyntax = \"proto3\";\nmessage M { string name = 1; }\n```\n\nend\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "\n", " ", "\"", "'", "/", "{", "}", "["] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn ini_insert_sweep() {
    let source =
        "; top\n[core]\nname = tinyhl\nport=5432\nenabled: yes\npath = a#b\nratio=-1.5e+2\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", " ", "\n", "#", ";", "[", "]", "=", ":", "\"", "'", ".",
        ] {
            assert_mutate_matches_lang(Language::Ini, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn ini_delete_sweep() {
    let source =
        "; top\n[core]\nname = tinyhl\nport=5432\nenabled: yes\npath = a#b\nratio=-1.5e+2\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Ini, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn ini_replace_sweep() {
    let source = "[database.replica]\nhost = \"db.local\"\nsecret = abc#123\nfeature: OFF\n";
    for off in 0..source.len() {
        for rep in ["x", "0", " ", "\n", "#", ";", "=", ":", "\"", "'", "[", "]"] {
            assert_mutate_matches_lang(Language::Ini, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn ini_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/ini/simple.ini.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\n", "#", ";", "[section]", "=", ":", "\""] {
            assert_mutate_matches_lang(Language::Ini, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn conf_insert_sweep() {
    let source = "# top\nserver.port = 8080\nfeature: ON\npath = /tmp/a#b # note\n// cfg\n[profile]\n{legacy}\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", " ", "\n", "#", ";", "/", "//", "[", "]", "{", "}", "=", ":", "\"", "'",
        ] {
            assert_mutate_matches_lang(Language::Conf, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn conf_delete_sweep() {
    let source = "# top\nserver.port = 8080\nfeature: ON\npath = /tmp/a#b # note\n// cfg\n[profile]\n{legacy}\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Conf, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn conf_replace_sweep() {
    let source = "url = http://example.test/a#b\nname: value\nlist = alpha,beta;gamma\n";
    for off in 0..source.len() {
        for rep in [
            "x", "0", " ", "\n", "#", ";", "/", "=", ":", "\"", "'", "{", "}",
        ] {
            assert_mutate_matches_lang(Language::Conf, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn conf_fixture_sweep_over_chunks() {
    let path = format!(
        "{}/fixtures/conf/simple.conf.in",
        env!("CARGO_MANIFEST_DIR")
    );
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in [
            "x",
            " ",
            "\n",
            "#",
            ";",
            "//",
            "[section]",
            "{section}",
            "=",
            ": ",
        ] {
            assert_mutate_matches_lang(Language::Conf, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn markdown_ini_conf_embed_sweep() {
    let source = "# T\n\n```ini\n[core]\nname = tinyhl\n```\n\n```conf\nfeature: ON\n```\n\nend\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "\n", " ", "#", ";", "[", "]", "=", ":"] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn wgsl_insert_sweep() {
    let source = "@compute @workgroup_size(1) fn main(@builtin(global_invocation_id) id: vec3u) { var x = 0x1.fp+2f; if (x >= 1.0) { x += 1.0; } }\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", "\"", " ", "{", "}", "(", ")", "[", "]", "/", "*", "@", "<", ">", "=", "-",
            ".",
        ] {
            assert_mutate_matches_lang(Language::Wgsl, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn wgsl_delete_sweep() {
    let source = "@compute @workgroup_size(1) fn main(@builtin(global_invocation_id) id: vec3u) { var x = 0x1.fp+2f; if (x >= 1.0) { x += 1.0; } }\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Wgsl, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn wgsl_replace_sweep() {
    let source = "/* nested /* c */ done */ var<storage, read_write> data: array<vec4f, 4u>; value = (value << 1u) & 0xffu;";
    for off in 0..source.len() {
        for rep in [
            "x", "0", "\"", "/", "*", "@", "<", ">", "=", ".", "+", "-", "&",
        ] {
            assert_mutate_matches_lang(Language::Wgsl, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn wgsl_fixture_sweep_over_chunks() {
    let path = format!(
        "{}/fixtures/wgsl/simple.wgsl.in",
        env!("CARGO_MANIFEST_DIR")
    );
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\"", "/*", "//", "0x", "@", "->", "<<"] {
            assert_mutate_matches_lang(Language::Wgsl, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn markdown_wgsl_embed_sweep() {
    let source = "# T\n\n```wgsl\n@compute fn main() { var x = 1u; }\n```\n\nend\n";
    for off in 0..=source.len() {
        for ins in ["x", "`", "\n", " ", "\"", "/", "@", "{", "}", "<"] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn perl_insert_sweep() {
    let source = "use strict; my $x = qr/foo/i; if ($x) { say qq{hi $x}; }\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", "\"", "'", "`", " ", "#", "$", "@", "%", "/", "{", "q",
        ] {
            assert_mutate_matches_lang(Language::Perl, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn perl_delete_sweep() {
    let source = "use strict; my $x = qr/foo/i; if ($x) { say qq{hi $x}; }\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Perl, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn perl_replace_sweep() {
    let source = "$msg =~ s/foo/bar/g; my @xs = (1_000, 2); print q{done}; # ok\n";
    for off in 0..source.len() {
        for rep in [
            "x", "0", "\"", "'", "`", "#", "$", "@", "%", "/", "{", "}", ".",
        ] {
            assert_mutate_matches_lang(Language::Perl, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn perl_fixture_sweep_over_chunks() {
    let path = format!("{}/fixtures/perl/simple.pl.in", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\"", "'", "#", "$", "qr/", "s/", "qq{"] {
            assert_mutate_matches_lang(Language::Perl, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn csharp_insert_sweep() {
    let source =
        "public record R(string Name) { string S => $@\"hi {Name}\"; var n = a?.B ?? 0; }\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", "\"", "'", " ", "{", "}", "(", ")", "/", "*", "@", "$", "?", ".",
        ] {
            assert_mutate_matches_lang(Language::Csharp, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn csharp_delete_sweep() {
    let source =
        "public record R(string Name) { string S => $@\"hi {Name}\"; var n = a?.B ?? 0; }\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Csharp, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn csharp_replace_sweep() {
    let source = "var path = @\"c:\\tmp\\x\"; var raw = \"\"\"line\nvalue\"\"\"; var n = 1_000;";
    for off in 0..source.len() {
        for rep in ["x", "0", "\"", "'", "@", "$", "/", "*", "?", ".", "=", "_"] {
            assert_mutate_matches_lang(Language::Csharp, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn csharp_fixture_sweep_over_chunks() {
    let path = format!(
        "{}/fixtures/csharp/simple.cs.in",
        env!("CARGO_MANIFEST_DIR")
    );
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\"", "/*", "//", "@\"", "$\"", "?.", "??"] {
            assert_mutate_matches_lang(Language::Csharp, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn java_insert_sweep() {
    let source =
        "@Deprecated public class C { String s = \"\"\"hi\n\"\"\"; int n = 1_000 >>> 1; }\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", "\"", "'", " ", "{", "}", "(", ")", "/", "*", "@", "<", ">", "?",
        ] {
            assert_mutate_matches_lang(Language::Java, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn java_delete_sweep() {
    let source =
        "@Deprecated public class C { String s = \"\"\"hi\n\"\"\"; int n = 1_000 >>> 1; }\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Java, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn java_replace_sweep() {
    let source = "package demo; import java.util.*; class C<T> { char c='x'; String s=\"v\"; }";
    for off in 0..source.len() {
        for rep in [
            "x", "0", "\"", "'", "/", "*", "@", "<", ">", "?", ":", ".", "_",
        ] {
            assert_mutate_matches_lang(Language::Java, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn java_fixture_sweep_over_chunks() {
    let path = format!(
        "{}/fixtures/java/simple.java.in",
        env!("CARGO_MANIFEST_DIR")
    );
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\"", "/*", "//", "@", ">>>", "0x", "<T>"] {
            assert_mutate_matches_lang(Language::Java, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn lisp_insert_sweep() {
    let source = "(define (square x) #| c |# (* x x)) ; ok\n'(1 #t #\\space \"s\")\n";
    for off in 0..=source.len() {
        for ins in [
            "x", "9", "\"", " ", "\n", ";", "#", "|", "'", "`", ",", "(", ")",
        ] {
            assert_mutate_matches_lang(Language::Lisp, source, Span::new(off as u32, 0), ins);
        }
    }
}

#[test]
fn lisp_delete_sweep() {
    let source = "(define (square x) #| c |# (* x x)) ; ok\n'(1 #t #\\space \"s\")\n";
    for off in 0..source.len() {
        assert_mutate_matches_lang(Language::Lisp, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn lisp_replace_sweep() {
    let source = "(let* ((xs '(1 2 3))) (if #false (display \"no\") (quote ok)))";
    for off in 0..source.len() {
        for rep in ["x", "0", "\"", ";", "#", "|", "'", "`", ",", "(", ")", "-"] {
            assert_mutate_matches_lang(Language::Lisp, source, Span::new(off as u32, 1), rep);
        }
    }
}

#[test]
fn lisp_fixture_sweep_over_chunks() {
    let path = format!(
        "{}/fixtures/lisp/simple.lisp.in",
        env!("CARGO_MANIFEST_DIR")
    );
    let source = std::fs::read_to_string(&path).unwrap();
    let step = (source.len() / 64).max(1);
    for off in (0..source.len()).step_by(step) {
        for rep in ["x", " ", "\"", ";", "#|", "|#", "'", "(", ")"] {
            assert_mutate_matches_lang(Language::Lisp, &source, Span::new(off as u32, 0), rep);
        }
    }
}

#[test]
fn markdown_new_language_embed_sweep() {
    let source = "# T\n\n```perl\nmy $x = qr/a/;\n```\n\n```cs\nrecord R(int X);\n```\n\n```java\nclass C {}\n```\n\n```scheme\n(define x 1)\n```\n";
    let step = (source.len() / 64).max(1);
    for off in (0..=source.len()).step_by(step) {
        for ins in ["x", "`", "\n", " ", "\"", "'", "#", "{", "}", "(", ")"] {
            assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 0), ins);
        }
    }
    for off in (0..source.len()).step_by(step) {
        assert_mutate_matches_lang(Language::Markdown, source, Span::new(off as u32, 1), "");
    }
}

#[test]
fn chain_of_edits_stays_in_sync() {
    let mut current = String::from("[]");
    let src: &dyn Source = &current.as_str();
    let mut table = TokenTable::new(Language::Json, src);

    let edits: &[(usize, usize, &str)] = &[
        (1, 0, "1, 2, 3"),
        (1, 1, "42"),
        (4, 1, "\"x\""),
        (0, 1, "{"),
        (current.len(), 0, ","),
    ];

    for &(off, len, rep) in edits {
        let off = off.min(current.len());
        let len = len.min(current.len() - off);
        let edited = apply(&current, Span::new(off as u32, len as u32), rep);
        let edited_ref: &str = &edited;
        let s: &dyn Source = &edited_ref;
        table.mutate(s, Span::new(off as u32, len as u32), rep.len() as u32);

        let fresh = TokenTable::new(Language::Json, s);
        let mutated: Vec<Token> = table.query(Span::new(0, table.source_len())).collect();
        let expected: Vec<Token> = fresh.query(Span::new(0, fresh.source_len())).collect();
        assert_eq!(mutated, expected, "diverged after edit on {edited:?}");

        current = edited;
    }
}
