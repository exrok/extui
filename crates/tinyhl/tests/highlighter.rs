use tinyhl::{
    DelimiterTable, Highlighter, Language, Overlays, RenderSpan, SemanticTable, Source, Span,
    TokenTable, kind,
};

fn reference_render(language: Language, src: &str) -> Vec<RenderSpan> {
    let source: &dyn Source = &src;
    let tokens = TokenTable::new(language, source);
    let semantic = SemanticTable::new(&tokens, source);
    let delimiters = DelimiterTable::new(&tokens);
    let all = Span::new(0, tokens.source_len());
    let mut sem = semantic.query(all).peekable();
    let mut delim = delimiters.query(all).peekable();
    let mut out = Vec::new();

    for token in tokens.query(all) {
        let offset = token.span.offset;
        while matches!(sem.peek(), Some(entry) if entry.span.offset < offset) {
            sem.next();
        }
        while matches!(delim.peek(), Some(entry) if entry.span.offset < offset) {
            delim.next();
        }
        let semantic = match sem.peek() {
            Some(entry) if entry.span.offset == offset => Some(entry.display_kind()),
            _ => None,
        };
        let delimiter = match delim.peek() {
            Some(entry) if entry.span.offset == offset => Some(entry.depth),
            _ => None,
        };
        let local = token.local_kind();
        let lang_tag = token.lang_tag();

        if local == kind::LIFETIME && token.span.len > 1 {
            out.push(RenderSpan {
                span: Span::new(token.span.offset, 1),
                lang_tag,
                local_kind: kind::LIFETIME,
                semantic: None,
                delimiter: None,
            });
            out.push(RenderSpan {
                span: Span::new(token.span.offset + 1, token.span.len - 1),
                lang_tag,
                local_kind: kind::LIFETIME,
                semantic,
                delimiter: None,
            });
        } else {
            out.push(RenderSpan {
                span: token.span,
                lang_tag,
                local_kind: local,
                semantic,
                delimiter,
            });
        }
    }

    out
}

#[test]
fn rust_fast_render_matches_reference_tables() {
    let src = r#"
macro_rules! gen { ($name:ident, $ty:ty) => { fn $name<'a>(arg: &'a $ty) { arg.call::<Vec<u8>>(); } } }

#[derive(Debug, Clone)]
struct Holder<T> { field: T }

impl<T> Holder<T> {
    fn make(&self, payload: Bytes) -> Result<Self, Error> {
        let Holder { field } = self;
        let parsed = parse::<crate::rpc::Req>(payload);
        self.field(parsed);
        Ok(Self { field })
    }
}
"#;
    let source: &dyn Source = &src;
    let mut highlighter = Highlighter::new(Language::Rust);
    highlighter.rebuild(source);
    let got: Vec<_> = highlighter
        .render(Span::new(0, highlighter.source_len().unwrap()))
        .collect();
    let expected: Vec<_> = reference_render(Language::Rust, src)
        .into_iter()
        .filter(|span| span.local_kind != kind::WHITESPACE)
        .collect();

    assert_eq!(got, expected);
}

#[test]
fn rust_fast_overlays_match_reference_tables() {
    let src = r#"
fn run<'a>(input: &'a Holder) {
    let path = crate::parse::<Vec<u8>>(input.bytes());
    input.field(path);
}
"#;
    let source: &dyn Source = &src;
    let tokens = TokenTable::new(Language::Rust, source);
    let semantic = SemanticTable::new(&tokens, source);
    let delimiters = DelimiterTable::new(&tokens);
    let all = Span::new(0, tokens.source_len());

    let mut highlighter = Highlighter::new(Language::Rust);
    highlighter.rebuild(source);
    let mut cached = Overlays::new(&highlighter, all);
    let mut sem = semantic.query(all).peekable();
    let mut delim = delimiters.query(all).peekable();

    for token in tokens.query(all) {
        let offset = token.span.offset;
        while matches!(sem.peek(), Some(entry) if entry.span.offset < offset) {
            sem.next();
        }
        while matches!(delim.peek(), Some(entry) if entry.span.offset < offset) {
            delim.next();
        }
        let expected_semantic = match sem.peek() {
            Some(entry) if entry.span.offset == offset => Some(entry.display_kind()),
            _ => None,
        };
        let expected_delimiter = match delim.peek() {
            Some(entry) if entry.span.offset == offset => Some(entry.depth),
            _ => None,
        };

        assert_eq!(
            cached.at(offset),
            (expected_semantic, expected_delimiter),
            "token at {offset}: {token:?}",
        );
    }
}
