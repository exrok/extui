use extui::vt::CursorShape;

/// Editing mode the [`Editor`] is currently in.
///
/// Determines how keys are dispatched through the binding router and
/// selects the rendered cursor shape via [`Self::cursor_shape`].
///
/// [`Editor`]: crate::Editor
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Mode {
    Normal,
    Insert,
    Visual,
    VisualLine,
    VisualBlock,
}

impl Mode {
    /// Returns `true` for [`Visual`], [`VisualLine`], and [`VisualBlock`].
    ///
    /// [`Visual`]: Self::Visual
    /// [`VisualLine`]: Self::VisualLine
    /// [`VisualBlock`]: Self::VisualBlock
    pub fn is_visual(self) -> bool {
        matches!(self, Mode::Visual | Mode::VisualLine | Mode::VisualBlock)
    }

    /// Returns the cursor shape conventionally associated with this
    /// mode: a steady block for Normal and Visual variants, a steady
    /// bar for Insert.
    pub fn cursor_shape(self) -> CursorShape {
        match self {
            Mode::Normal | Mode::Visual | Mode::VisualLine | Mode::VisualBlock => {
                CursorShape::SteadyBlock
            }
            Mode::Insert => CursorShape::SteadyBar,
        }
    }
}
