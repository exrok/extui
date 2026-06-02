use extui_editor::{Editor, Replacement, Span, TextBuffer, TrackedChange};
use tinyhl::{Highlighter, Language, Source};

struct BufferSource<'a>(&'a TextBuffer);

impl Source for BufferSource<'_> {
    fn len(&self) -> u32 {
        self.0.len() as u32
    }

    fn page(&self, offset: u32) -> (u32, &[u8]) {
        self.0.page(offset)
    }
}

fn apply_change(hl: &mut Highlighter, ed: &Editor, change: TrackedChange) {
    let src = BufferSource(ed.text_buffer());
    match change {
        TrackedChange::None => {}
        TrackedChange::Reset => hl.rebuild(&src),
        TrackedChange::Merged(Replacement {
            offset,
            old_len,
            new_len,
        }) => hl.apply_replacement(&src, tinyhl::Span::new(offset, old_len), new_len),
    }
}

fn lexical_dump(hl: &Highlighter) -> Vec<tinyhl::Token> {
    let table = hl.table().expect("highlighter should be initialized");
    table
        .query(tinyhl::Span::new(0, table.source_len()))
        .collect()
}

fn semantic_dump(hl: &Highlighter) -> Vec<(tinyhl::Span, tinyhl::SemanticKind, u8, u8)> {
    let Some(semantic) = hl.semantic() else {
        return Vec::new();
    };
    semantic
        .query(tinyhl::Span::new(0, semantic.source_len()))
        .map(|tok| (tok.span, tok.kind, tok.lang_tag, tok.nest))
        .collect()
}

fn delimiter_dump(
    hl: &Highlighter,
) -> Vec<(tinyhl::Span, tinyhl::DelimiterKind, u16, bool, u8, u8)> {
    let Some(delimiters) = hl.delimiters() else {
        return Vec::new();
    };
    delimiters
        .query(tinyhl::Span::new(0, delimiters.source_len()))
        .map(|tok| {
            (
                tok.span,
                tok.kind,
                tok.depth,
                tok.is_open,
                tok.lang_tag,
                tok.nest,
            )
        })
        .collect()
}

fn assert_matches_fresh(language: Language, hl: &Highlighter, text: &str) {
    let src: &dyn Source = &text;
    let mut fresh = Highlighter::new(language);
    fresh.rebuild(src);
    assert_eq!(lexical_dump(hl), lexical_dump(&fresh), "lexical stream");
    assert_eq!(semantic_dump(hl), semantic_dump(&fresh), "semantic stream");
    assert_eq!(
        delimiter_dump(hl),
        delimiter_dump(&fresh),
        "delimiter stream"
    );
}

#[test]
fn batched_editor_insertions_keep_tinyhl_in_sync() {
    let language = Language::Rust;
    let mut ed = Editor::new();
    ed.set_lines("fn main() {\n    let value = foo(1);\n}\n");
    ed.set_track_replacements(true);

    let mut hl = Highlighter::new(language);
    let change = ed.take_tracked_change();
    apply_change(&mut hl, &ed, change);
    assert_matches_fresh(language, &hl, &ed.text());

    for (offset, text) in [
        (0, "pub "),
        (18, "mut "),
        (34, " + bar(2)"),
        (ed.text_len(), "\nfn bar(x: i32) -> i32 { x }\n"),
    ] {
        ed.replace_range(Span::new(offset, 0), text);
    }
    let change = ed.take_tracked_change();
    apply_change(&mut hl, &ed, change);
    assert_matches_fresh(language, &hl, &ed.text());
}

#[test]
fn editor_identifier_insert_keeps_delimiter_overlay_in_sync() {
    let language = Language::Rust;
    let mut source = String::from("fn main() {\n");
    for i in 0..260 {
        source.push_str(&format!("    let value_{i} = [foo(bar[{i}]), baz];\n"));
    }
    source.push_str("}\n");

    let mut ed = Editor::new();
    ed.set_lines(&source);
    ed.set_track_replacements(true);

    let mut hl = Highlighter::new(language);
    let change = ed.take_tracked_change();
    apply_change(&mut hl, &ed, change);

    let off = ed.text().find("value_140").unwrap() + "value".len();
    ed.replace_range(Span::new(off as u32, 0), "x");
    let change = ed.take_tracked_change();
    apply_change(&mut hl, &ed, change);

    assert_matches_fresh(language, &hl, &ed.text());
}
