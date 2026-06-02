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
