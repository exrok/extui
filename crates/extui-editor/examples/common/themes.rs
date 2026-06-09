//! Example-only syntax palette + external highlighter wiring.
//!
//! `extui-editor` intentionally knows nothing about syntax colors —
//! it renders plain text with an optional `&[StyleRun]` overlay. Hosts
//! that want syntax highlighting drive a [`tinyhl::Highlighter`]
//! themselves and pass its output through
//! [`extui_editor::Editor::render_with_styles`].
//!
//! This module is included via `#[path = "common/themes.rs"]` by every
//! example that wants highlighting, so changes here propagate to all
//! demos. It is not a crate API.

#![allow(dead_code)]

use extui::{Color, Rgb, Style, vt::Modifier};
use extui_editor::{Editor, Replacement, StyleRun, TextBuffer, TrackedChange};

/// Newtype wrapper that adapts [`TextBuffer`] to [`tinyhl::Source`].
/// `extui-editor` itself no longer depends on tinyhl, so hosts that
/// want to run tinyhl on the editor's text own this adapter.
struct BufferSource<'a>(&'a TextBuffer);

impl<'a> tinyhl::Source for BufferSource<'a> {
    fn len(&self) -> u32 {
        self.0.len() as u32
    }

    fn page(&self, offset: u32) -> (u32, &[u8]) {
        self.0.page(offset)
    }
}

pub const RAINBOW_DEPTHS: usize = 6;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SyntaxTheme {
    pub text: Style,
    pub keyword: Style,
    pub string: Style,
    pub number: Style,
    pub comment: Style,
    pub doc_comment: Style,
    pub lifetime: Style,
    pub error: Style,
    pub heading: Style,
    pub emphasis: Style,
    pub code: Style,
    pub link_text: Style,
    pub link_url: Style,
    pub punctuation: Style,
    pub type_name: Style,
    pub function: Style,
    pub method: Style,
    pub parameter: Style,
    pub argument: Style,
    pub variable_def: Style,
    pub field: Style,
    pub path: Style,
    pub rainbow: [Style; RAINBOW_DEPTHS],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BuiltInPalette {
    Default,
    TokyoNight,
    GruvboxDark,
    SolarizedDark,
    Nord,
}

impl BuiltInPalette {
    pub const ALL: [BuiltInPalette; 5] = [
        BuiltInPalette::Default,
        BuiltInPalette::TokyoNight,
        BuiltInPalette::GruvboxDark,
        BuiltInPalette::SolarizedDark,
        BuiltInPalette::Nord,
    ];

    pub const fn name(self) -> &'static str {
        match self {
            BuiltInPalette::Default => "Default",
            BuiltInPalette::TokyoNight => "Tokyo Night",
            BuiltInPalette::GruvboxDark => "Gruvbox Dark",
            BuiltInPalette::SolarizedDark => "Solarized Dark",
            BuiltInPalette::Nord => "Nord",
        }
    }

    pub fn next(self) -> Self {
        let idx = Self::ALL
            .iter()
            .position(|candidate| *candidate == self)
            .unwrap_or(0);
        Self::ALL[(idx + 1) % Self::ALL.len()]
    }

    pub fn text(self) -> Style {
        match self {
            BuiltInPalette::Default => Style::DEFAULT,
            BuiltInPalette::TokyoNight => Style::DEFAULT
                .with_fg(rgb(0xc0, 0xca, 0xf5))
                .with_bg(rgb(0x1a, 0x1b, 0x26)),
            BuiltInPalette::GruvboxDark => Style::DEFAULT
                .with_fg(rgb(0xeb, 0xdb, 0xb2))
                .with_bg(rgb(0x28, 0x28, 0x28)),
            BuiltInPalette::SolarizedDark => Style::DEFAULT
                .with_fg(rgb(0x93, 0xa1, 0xa1))
                .with_bg(rgb(0x00, 0x2b, 0x36)),
            BuiltInPalette::Nord => Style::DEFAULT
                .with_fg(rgb(0xd8, 0xde, 0xe9))
                .with_bg(rgb(0x2e, 0x34, 0x40)),
        }
    }

    pub fn selection_bg(self) -> Color {
        match self {
            BuiltInPalette::Default => rgb(0x44, 0x44, 0x44),
            BuiltInPalette::TokyoNight => rgb(0x2a, 0x2f, 0x4a),
            BuiltInPalette::GruvboxDark => rgb(0x45, 0x49, 0x3e),
            BuiltInPalette::SolarizedDark => rgb(0x07, 0x36, 0x42),
            BuiltInPalette::Nord => rgb(0x43, 0x4c, 0x5e),
        }
    }

    pub fn syntax(self) -> SyntaxTheme {
        match self {
            BuiltInPalette::Default => default_syntax(),
            BuiltInPalette::TokyoNight => tokyo_night_syntax(),
            BuiltInPalette::GruvboxDark => gruvbox_syntax(),
            BuiltInPalette::SolarizedDark => solarized_syntax(),
            BuiltInPalette::Nord => nord_syntax(),
        }
    }

    pub fn apply_to(self, ed: &mut Editor) {
        use extui_editor::{EditorTheme, SelectionTheme};
        let selection_bg = self.selection_bg();
        ed.set_theme(EditorTheme {
            name: self.name(),
            text: self.text(),
            selection: SelectionTheme {
                charwise: Style::DEFAULT.with_bg(selection_bg),
                linewise: Style::DEFAULT.with_bg(selection_bg),
                blockwise: Style::DEFAULT.with_bg(selection_bg),
            },
        });
    }
}

/// External syntax highlighter that keeps tinyhl's three tables in
/// sync with an [`Editor`] via [`Editor::take_tracked_change`], then
/// drives [`Editor::render_with_styles`] with a reusable run buffer.
pub struct SyntaxHighlighter {
    hl: tinyhl::Highlighter,
    runs: Vec<StyleRun>,
}

impl SyntaxHighlighter {
    pub fn new(language: tinyhl::Language) -> Self {
        Self {
            hl: tinyhl::Highlighter::new(language),
            runs: Vec::new(),
        }
    }

    pub fn bind(&mut self, ed: &mut Editor) {
        ed.set_track_replacements(true);
        let src = BufferSource(ed.text_buffer());
        self.hl.rebuild(&src);
    }

    pub fn sync(&mut self, ed: &mut Editor) {
        let change = ed.take_tracked_change();
        self.apply_change(ed, change);
    }

    /// Apply a previously-drained [`TrackedChange`] to the
    /// highlighter. Use this variant when your host shares the
    /// drained change with other overlays (search, spellcheck)
    /// so that `take_tracked_change` is only called once per frame.
    pub fn apply_change(&mut self, ed: &mut Editor, change: TrackedChange) {
        match change {
            TrackedChange::None => {}
            TrackedChange::Reset => {
                let src = BufferSource(ed.text_buffer());
                self.hl.rebuild(&src);
            }
            TrackedChange::Merged(Replacement {
                offset,
                old_len,
                new_len,
            }) => {
                let src = BufferSource(ed.text_buffer());
                self.hl
                    .apply_replacement(&src, tinyhl::Span::new(offset, old_len), new_len);
            }
        }
    }

    pub fn render(
        &mut self,
        ed: &mut Editor,
        rect: extui::Rect,
        buf: &mut extui::Buffer,
        syntax: &SyntaxTheme,
        text_style: Style,
    ) {
        self.runs.clear();
        if let Some(table) = self.hl.table() {
            // Clip token query to the visible byte range; materializing
            // runs for the whole buffer every frame regresses large-file
            // workloads.
            let visible = ed.visible_byte_span(rect);
            let span = tinyhl::Span::new(visible.offset, visible.len);
            let mut overlays = tinyhl::Overlays::new(&self.hl, span);
            for tok in table.query(span) {
                let (lang, local) = tinyhl::unpack_kind(tok.kind);
                let (sem, depth) = overlays.at(tok.span.offset);
                self.runs.push(StyleRun {
                    offset: tok.span.offset,
                    len: tok.span.len,
                    style: style_for(syntax, text_style, lang, local, sem, depth),
                });
            }
        }
        ed.render_with_styles(rect, buf, &self.runs);
    }
}

pub fn style_for(
    syntax: &SyntaxTheme,
    text_style: Style,
    lang_tag: u8,
    local_kind: u16,
    semantic: Option<tinyhl::SemanticKind>,
    delimiter_depth: Option<u16>,
) -> Style {
    let lexical = lexical_token_style(syntax, lang_tag, local_kind);
    let mut style = merge_style(text_style, lexical);
    if let Some(kind) = semantic {
        if let Some(sem) = semantic_style(syntax, kind) {
            style = merge_style(style, sem);
        }
    }
    if let Some(depth) = delimiter_depth {
        let rainbow = syntax.rainbow[(depth as usize) % syntax.rainbow.len()];
        style = merge_style(style, rainbow);
    }
    style
}

fn lexical_token_style(syntax: &SyntaxTheme, lang_tag: u8, local_kind: u16) -> Style {
    use tinyhl::kind;
    match lang_tag {
        7 => match local_kind {
            kind::HEADING_MARKER | kind::HEADING_TEXT => syntax.heading,
            kind::EMPHASIS => syntax.emphasis,
            kind::CODE_INLINE | kind::CODE_FENCE | kind::CODE_BLOCK => syntax.code,
            kind::LINK_TEXT => syntax.link_text,
            kind::LINK_URL => syntax.link_url,
            kind::BLOCKQUOTE | kind::LIST_MARKER => syntax.punctuation,
            kind::ERROR => syntax.error,
            _ => generic_style(syntax, local_kind),
        },
        _ => generic_style(syntax, local_kind),
    }
}

fn generic_style(syntax: &SyntaxTheme, local_kind: u16) -> Style {
    use tinyhl::kind;
    match local_kind {
        kind::KEYWORD => syntax.keyword,
        kind::STRING | kind::TEMPLATE_STRING | kind::CHAR | kind::REGEX => syntax.string,
        kind::NUMBER => syntax.number,
        kind::COMMENT => syntax.comment,
        kind::DOC_COMMENT => syntax.doc_comment,
        kind::LIFETIME => syntax.lifetime,
        kind::ERROR => syntax.error,
        _ => syntax.text,
    }
}

fn semantic_style(syntax: &SyntaxTheme, kind: tinyhl::SemanticKind) -> Option<Style> {
    use tinyhl::SemanticKind::*;
    Some(match kind {
        TypeDefinition | TypeName => syntax.type_name,
        FunctionDefinition | FunctionCall | MacroCall => syntax.function,
        MethodDefinition | MethodCall => syntax.method,
        Parameter => syntax.parameter,
        Argument => syntax.argument,
        VariableDefinition => syntax.variable_def,
        FieldDefinition | Field => syntax.field,
        PathComponent => syntax.path,
        Lifetime => syntax.lifetime,
        Variable | FieldAccess | MetaVariable => return None,
    })
}

fn merge_style(base: Style, overlay: Style) -> Style {
    let mut out = if overlay.is_palette() { overlay } else { base };
    if overlay.is_palette() {
        if let Some(fg) = base.fg() {
            out = out.with_fg(fg);
        }
        if let Some(bg) = base.bg() {
            out = out.with_bg(bg);
        }
    }
    if let Some(fg) = overlay.fg() {
        out = out.with_fg(fg);
    }
    if let Some(bg) = overlay.bg() {
        out = out.with_bg(bg);
    }
    out.with_modifier(base.modifiers())
        .with_modifier(overlay.modifiers())
}

const fn rgb(r: u8, g: u8, b: u8) -> Color {
    Color::Rgb(Rgb(r, g, b))
}

fn fg(c: Color) -> Style {
    Style::DEFAULT.with_fg(c)
}

fn fg_bold(c: Color) -> Style {
    Style::DEFAULT.with_fg(c).with_modifier(Modifier::BOLD)
}

fn default_syntax() -> SyntaxTheme {
    SyntaxTheme {
        text: Style::DEFAULT,
        keyword: fg_bold(rgb(0xff, 0x5f, 0xff)),
        string: fg(rgb(0x5f, 0xff, 0x87)),
        number: fg(rgb(0xff, 0xd7, 0x5f)),
        comment: Style::DEFAULT
            .with_fg(rgb(0x80, 0x80, 0x80))
            .with_modifier(Modifier::ITALIC),
        doc_comment: Style::DEFAULT
            .with_fg(rgb(0x5f, 0xd7, 0xaf))
            .with_modifier(Modifier::ITALIC),
        lifetime: fg(rgb(0xff, 0xd7, 0x87)),
        error: Style::DEFAULT
            .with_fg(rgb(0xff, 0x5f, 0x5f))
            .with_modifier(Modifier::UNDERLINED),
        heading: fg_bold(rgb(0x5f, 0x87, 0xff)),
        emphasis: Style::DEFAULT.with_modifier(Modifier::ITALIC),
        code: fg(rgb(0xaf, 0xaf, 0xaf)),
        link_text: fg(rgb(0x5f, 0xd7, 0xd7)),
        link_url: Style::DEFAULT
            .with_fg(rgb(0x5f, 0x87, 0xff))
            .with_modifier(Modifier::UNDERLINED),
        punctuation: fg(rgb(0x8a, 0x8a, 0x8a)),
        type_name: fg(rgb(0xff, 0xd7, 0x5f)),
        function: fg_bold(rgb(0x87, 0xaf, 0xff)),
        method: fg(rgb(0x87, 0xaf, 0xff)),
        parameter: fg(rgb(0xd7, 0xaf, 0x87)),
        argument: fg(rgb(0xff, 0xaf, 0x5f)),
        variable_def: fg(rgb(0xff, 0x87, 0x87)),
        field: fg(rgb(0x87, 0xd7, 0xd7)),
        path: fg(rgb(0xff, 0xaf, 0x5f)),
        rainbow: [
            fg_bold(rgb(0xff, 0x5f, 0xff)),
            fg_bold(rgb(0x5f, 0x87, 0xff)),
            fg_bold(rgb(0x5f, 0xff, 0x87)),
            fg_bold(rgb(0xff, 0xaf, 0x5f)),
            fg_bold(rgb(0x5f, 0xd7, 0xd7)),
            fg_bold(rgb(0xff, 0x5f, 0x5f)),
        ],
    }
}

fn tokyo_night_syntax() -> SyntaxTheme {
    SyntaxTheme {
        text: Style::DEFAULT
            .with_fg(rgb(0xc0, 0xca, 0xf5))
            .with_bg(rgb(0x1a, 0x1b, 0x26)),
        keyword: fg_bold(rgb(0xbb, 0x9a, 0xf7)),
        string: fg(rgb(0x9e, 0xce, 0x6a)),
        number: fg(rgb(0xff, 0x9e, 0x64)),
        comment: Style::DEFAULT
            .with_fg(rgb(0x56, 0x5f, 0x89))
            .with_modifier(Modifier::ITALIC),
        doc_comment: Style::DEFAULT
            .with_fg(rgb(0x73, 0xd0, 0xff))
            .with_modifier(Modifier::ITALIC),
        lifetime: fg(rgb(0xe0, 0xaf, 0x68)),
        error: Style::DEFAULT
            .with_fg(rgb(0xf7, 0x76, 0x8e))
            .with_modifier(Modifier::UNDERLINED),
        heading: fg_bold(rgb(0x7a, 0xa2, 0xf7)),
        emphasis: Style::DEFAULT.with_modifier(Modifier::ITALIC),
        code: fg(rgb(0x9a, 0xa5, 0xce)),
        link_text: fg(rgb(0x73, 0xd0, 0xff)),
        link_url: Style::DEFAULT
            .with_fg(rgb(0x7a, 0xa2, 0xf7))
            .with_modifier(Modifier::UNDERLINED),
        punctuation: fg(rgb(0x63, 0x6d, 0xa6)),
        type_name: fg(rgb(0x2a, 0xc3, 0xde)),
        function: fg_bold(rgb(0x7a, 0xa2, 0xf7)),
        method: fg(rgb(0x7d, 0xcf, 0xff)),
        parameter: fg(rgb(0xe0, 0xaf, 0x68)),
        argument: fg(rgb(0xff, 0x9e, 0x64)),
        variable_def: fg(rgb(0xf7, 0x76, 0x8e)),
        field: fg(rgb(0x73, 0xd0, 0xff)),
        path: fg(rgb(0xff, 0x9e, 0x64)),
        rainbow: [
            fg_bold(rgb(0xbb, 0x9a, 0xf7)),
            fg_bold(rgb(0x7a, 0xa2, 0xf7)),
            fg_bold(rgb(0x9e, 0xce, 0x6a)),
            fg_bold(rgb(0xff, 0x9e, 0x64)),
            fg_bold(rgb(0x7d, 0xcf, 0xff)),
            fg_bold(rgb(0xf7, 0x76, 0x8e)),
        ],
    }
}

fn gruvbox_syntax() -> SyntaxTheme {
    SyntaxTheme {
        text: Style::DEFAULT
            .with_fg(rgb(0xeb, 0xdb, 0xb2))
            .with_bg(rgb(0x28, 0x28, 0x28)),
        keyword: fg_bold(rgb(0xfb, 0x49, 0x34)),
        string: fg(rgb(0xb8, 0xbb, 0x26)),
        number: fg(rgb(0xfe, 0x80, 0x19)),
        comment: Style::DEFAULT
            .with_fg(rgb(0x92, 0x83, 0x74))
            .with_modifier(Modifier::ITALIC),
        doc_comment: Style::DEFAULT
            .with_fg(rgb(0x8e, 0xc0, 0x7c))
            .with_modifier(Modifier::ITALIC),
        lifetime: fg(rgb(0xfa, 0xbd, 0x2f)),
        error: Style::DEFAULT
            .with_fg(rgb(0xfb, 0x49, 0x34))
            .with_modifier(Modifier::UNDERLINED),
        heading: fg_bold(rgb(0x83, 0xa5, 0x98)),
        emphasis: Style::DEFAULT.with_modifier(Modifier::ITALIC),
        code: fg(rgb(0xd5, 0xc4, 0xa1)),
        link_text: fg(rgb(0x8e, 0xc0, 0x7c)),
        link_url: Style::DEFAULT
            .with_fg(rgb(0x83, 0xa5, 0x98))
            .with_modifier(Modifier::UNDERLINED),
        punctuation: fg(rgb(0xa8, 0x99, 0x84)),
        type_name: fg(rgb(0xfa, 0xbd, 0x2f)),
        function: fg_bold(rgb(0xb8, 0xbb, 0x26)),
        method: fg(rgb(0x8e, 0xc0, 0x7c)),
        parameter: fg(rgb(0xd7, 0x99, 0x21)),
        argument: fg(rgb(0xfe, 0x80, 0x19)),
        variable_def: fg(rgb(0xfb, 0x49, 0x34)),
        field: fg(rgb(0x83, 0xa5, 0x98)),
        path: fg(rgb(0xfe, 0x80, 0x19)),
        rainbow: [
            fg_bold(rgb(0xd3, 0x86, 0x9b)),
            fg_bold(rgb(0x83, 0xa5, 0x98)),
            fg_bold(rgb(0xb8, 0xbb, 0x26)),
            fg_bold(rgb(0xfe, 0x80, 0x19)),
            fg_bold(rgb(0x8e, 0xc0, 0x7c)),
            fg_bold(rgb(0xfb, 0x49, 0x34)),
        ],
    }
}

fn solarized_syntax() -> SyntaxTheme {
    SyntaxTheme {
        text: Style::DEFAULT
            .with_fg(rgb(0x93, 0xa1, 0xa1))
            .with_bg(rgb(0x00, 0x2b, 0x36)),
        keyword: fg_bold(rgb(0x85, 0x99, 0x00)),
        string: fg(rgb(0x2a, 0xa1, 0x98)),
        number: fg(rgb(0xd3, 0x36, 0x82)),
        comment: Style::DEFAULT
            .with_fg(rgb(0x58, 0x6e, 0x75))
            .with_modifier(Modifier::ITALIC),
        doc_comment: Style::DEFAULT
            .with_fg(rgb(0x26, 0x8b, 0xd2))
            .with_modifier(Modifier::ITALIC),
        lifetime: fg(rgb(0xcb, 0x4b, 0x16)),
        error: Style::DEFAULT
            .with_fg(rgb(0xdc, 0x32, 0x2f))
            .with_modifier(Modifier::UNDERLINED),
        heading: fg_bold(rgb(0x26, 0x8b, 0xd2)),
        emphasis: Style::DEFAULT.with_modifier(Modifier::ITALIC),
        code: fg(rgb(0x83, 0x94, 0x96)),
        link_text: fg(rgb(0x2a, 0xa1, 0x98)),
        link_url: Style::DEFAULT
            .with_fg(rgb(0x26, 0x8b, 0xd2))
            .with_modifier(Modifier::UNDERLINED),
        punctuation: fg(rgb(0x65, 0x7b, 0x83)),
        type_name: fg(rgb(0xb5, 0x89, 0x00)),
        function: fg_bold(rgb(0x26, 0x8b, 0xd2)),
        method: fg(rgb(0x2a, 0xa1, 0x98)),
        parameter: fg(rgb(0xcb, 0x4b, 0x16)),
        argument: fg(rgb(0xb5, 0x89, 0x00)),
        variable_def: fg(rgb(0xdc, 0x32, 0x2f)),
        field: fg(rgb(0x2a, 0xa1, 0x98)),
        path: fg(rgb(0xcb, 0x4b, 0x16)),
        rainbow: [
            fg_bold(rgb(0x6c, 0x71, 0xc4)),
            fg_bold(rgb(0x26, 0x8b, 0xd2)),
            fg_bold(rgb(0x85, 0x99, 0x00)),
            fg_bold(rgb(0xcb, 0x4b, 0x16)),
            fg_bold(rgb(0x2a, 0xa1, 0x98)),
            fg_bold(rgb(0xdc, 0x32, 0x2f)),
        ],
    }
}

fn nord_syntax() -> SyntaxTheme {
    SyntaxTheme {
        text: Style::DEFAULT
            .with_fg(rgb(0xd8, 0xde, 0xe9))
            .with_bg(rgb(0x2e, 0x34, 0x40)),
        keyword: fg_bold(rgb(0x81, 0xa1, 0xc1)),
        string: fg(rgb(0xa3, 0xbe, 0x8c)),
        number: fg(rgb(0xb4, 0x8e, 0xad)),
        comment: Style::DEFAULT
            .with_fg(rgb(0x61, 0x6e, 0x88))
            .with_modifier(Modifier::ITALIC),
        doc_comment: Style::DEFAULT
            .with_fg(rgb(0x8f, 0xbc, 0xbb))
            .with_modifier(Modifier::ITALIC),
        lifetime: fg(rgb(0xeb, 0xcb, 0x8b)),
        error: Style::DEFAULT
            .with_fg(rgb(0xbf, 0x61, 0x6a))
            .with_modifier(Modifier::UNDERLINED),
        heading: fg_bold(rgb(0x88, 0xc0, 0xd0)),
        emphasis: Style::DEFAULT.with_modifier(Modifier::ITALIC),
        code: fg(rgb(0xe5, 0xe9, 0xf0)),
        link_text: fg(rgb(0x8f, 0xbc, 0xbb)),
        link_url: Style::DEFAULT
            .with_fg(rgb(0x81, 0xa1, 0xc1))
            .with_modifier(Modifier::UNDERLINED),
        punctuation: fg(rgb(0x4c, 0x56, 0x6a)),
        type_name: fg(rgb(0x8f, 0xbc, 0xbb)),
        function: fg_bold(rgb(0x88, 0xc0, 0xd0)),
        method: fg(rgb(0x88, 0xc0, 0xd0)),
        parameter: fg(rgb(0xeb, 0xcb, 0x8b)),
        argument: fg(rgb(0xd0, 0x87, 0x70)),
        variable_def: fg(rgb(0xbf, 0x61, 0x6a)),
        field: fg(rgb(0x8f, 0xbc, 0xbb)),
        path: fg(rgb(0xd0, 0x87, 0x70)),
        rainbow: [
            fg_bold(rgb(0xb4, 0x8e, 0xad)),
            fg_bold(rgb(0x81, 0xa1, 0xc1)),
            fg_bold(rgb(0xa3, 0xbe, 0x8c)),
            fg_bold(rgb(0xd0, 0x87, 0x70)),
            fg_bold(rgb(0x88, 0xc0, 0xd0)),
            fg_bold(rgb(0xbf, 0x61, 0x6a)),
        ],
    }
}
