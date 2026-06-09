use extui::Style;

/// Styles applied to the three shapes of visual-mode selection.
///
/// Each field is layered on top of the existing cell style via
/// [`Buffer::set_style`] during rendering, so the foreground
/// from any syntax-coloring [`StyleRun`] is preserved.
///
/// [`Buffer::set_style`]: extui::Buffer::set_style
/// [`StyleRun`]: crate::StyleRun
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SelectionTheme {
    /// Applied inside [`Mode::Visual`] selections.
    ///
    /// [`Mode::Visual`]: crate::Mode::Visual
    pub charwise: Style,
    /// Applied across every cell of rows inside [`Mode::VisualLine`]
    /// selections.
    ///
    /// [`Mode::VisualLine`]: crate::Mode::VisualLine
    pub linewise: Style,
    /// Applied to the rectangular region of [`Mode::VisualBlock`]
    /// selections.
    ///
    /// [`Mode::VisualBlock`]: crate::Mode::VisualBlock
    pub blockwise: Style,
}

impl Default for SelectionTheme {
    fn default() -> Self {
        Self {
            charwise: Style::DEFAULT,
            linewise: Style::DEFAULT,
            blockwise: Style::DEFAULT,
        }
    }
}

/// Palette the editor paints itself with.
///
/// The editor owns two concerns: plain-text cells (drawn with
/// [`Self::text`]) and selection overlays (drawn with
/// [`Self::selection`]). Syntax coloring is external: hosts pass a
/// sorted slice of [`StyleRun`] to [`Editor::render_with_styles`].
///
/// [`StyleRun`]: crate::StyleRun
/// [`Editor::render_with_styles`]: crate::Editor::render_with_styles
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EditorTheme {
    /// Human-readable name used by theme pickers.
    pub name: &'static str,
    /// Style applied to every text cell before any style overlays.
    pub text: Style,
    /// Style overlays applied to selection regions.
    pub selection: SelectionTheme,
}

impl Default for EditorTheme {
    fn default() -> Self {
        Self {
            name: "Default",
            text: Style::DEFAULT,
            selection: SelectionTheme::default(),
        }
    }
}
