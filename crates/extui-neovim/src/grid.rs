//! Internal grid state mirroring Neovim's `ext_linegrid` protocol.
//!
//! The reader thread decodes each `redraw` notification and forwards
//! it to [`GridState`]. The grid owns an [`extui::Buffer`] directly,
//! so [`GridState::render`] paints into an [`extui::DoubleBuffer`] by
//! copying cells in place rather than re-segmenting text on every
//! frame. Highlight attributes are resolved once at definition time
//! and stored as ready-to-use [`extui::Style`] values.

use extui::vt::Modifier;
use extui::{AnsiColor, Buffer, Color, DoubleBuffer, Rect, Rgb, Style};

use crate::msgpack::{Error as MsgpackError, Kind, Reader};

/// Error raised while applying a redraw event.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    /// The event payload was malformed.
    Msgpack(MsgpackError),
    /// An event referenced a grid index, row, or column outside the grid.
    OutOfBounds,
}

impl From<MsgpackError> for Error {
    fn from(e: MsgpackError) -> Self {
        Error::Msgpack(e)
    }
}

/// Cursor shape for a given Neovim editor mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CursorShape {
    /// Solid block (`block`).
    Block,
    /// Vertical bar (`vertical`).
    Bar,
    /// Underline (`horizontal`).
    Underline,
}

impl Default for CursorShape {
    fn default() -> Self {
        CursorShape::Block
    }
}

#[derive(Clone, Copy)]
struct ModeInfo {
    shape: CursorShape,
}

impl Default for ModeInfo {
    fn default() -> Self {
        ModeInfo {
            shape: CursorShape::Block,
        }
    }
}

/// Resolved rgb and cterm flavours of a single highlight attribute.
///
/// One entry is stored per `hl_attr_define` id so the render path can
/// pick the active [`Style`] in O(1) based on the current
/// `termguicolors` setting.
#[derive(Clone, Copy, PartialEq, Eq)]
struct HlEntry {
    rgb: Style,
    cterm: Style,
}

impl HlEntry {
    const DEFAULT: HlEntry = HlEntry {
        rgb: Style::DEFAULT,
        cterm: Style::DEFAULT,
    };
}

/// Mirrors the `ext_linegrid` grid maintained by an embedded Neovim.
pub struct GridState {
    grid: Buffer,
    cursor_row: u16,
    cursor_col: u16,
    default_rgb_style: Style,
    default_cterm_style: Style,
    hl_attrs: Vec<HlEntry>,
    termguicolors: bool,
    modes: Vec<ModeInfo>,
    current_mode: usize,
    busy: bool,
    dirty: bool,
    alive: bool,
}

impl GridState {
    /// Creates an empty grid of `width` columns by `height` rows.
    pub fn new(width: u16, height: u16) -> Self {
        GridState {
            grid: Buffer::new(width, height),
            cursor_row: 0,
            cursor_col: 0,
            default_rgb_style: Style::DEFAULT,
            default_cterm_style: Style::DEFAULT,
            hl_attrs: vec![HlEntry::DEFAULT],
            termguicolors: true,
            modes: Vec::new(),
            current_mode: 0,
            busy: false,
            dirty: false,
            alive: true,
        }
    }

    /// Returns the grid width in cells.
    pub fn width(&self) -> u16 {
        self.grid.width()
    }

    /// Returns the grid height in cells.
    pub fn height(&self) -> u16 {
        self.grid.height()
    }

    /// Returns the cursor row, zero-based within the grid.
    pub fn cursor_row(&self) -> u16 {
        self.cursor_row
    }

    /// Returns the cursor column, zero-based within the grid.
    pub fn cursor_col(&self) -> u16 {
        self.cursor_col
    }

    /// Returns the shape currently configured for the active editor mode.
    pub fn cursor_shape(&self) -> CursorShape {
        self.modes
            .get(self.current_mode)
            .map(|m| m.shape)
            .unwrap_or_default()
    }

    /// Returns `true` if the cursor should be hidden because Neovim is busy.
    pub fn is_busy(&self) -> bool {
        self.busy
    }

    /// Returns whether the embedded Neovim process is still alive.
    pub fn alive(&self) -> bool {
        self.alive
    }

    /// Marks the process as no longer alive.
    pub fn mark_dead(&mut self) {
        self.alive = false;
        self.dirty = true;
    }

    /// Consumes the dirty flag and returns its previous value.
    pub fn take_dirty(&mut self) -> bool {
        let d = self.dirty;
        self.dirty = false;
        d
    }

    /// Returns the current dirty flag without clearing it.
    pub fn is_dirty(&self) -> bool {
        self.dirty
    }

    /// Sets the dirty flag.
    pub fn mark_dirty(&mut self) {
        self.dirty = true;
    }

    fn current_default_style(&self) -> Style {
        if self.termguicolors {
            self.default_rgb_style
        } else {
            self.default_cterm_style
        }
    }

    /// Picks the active [`Style`] for a highlight id, merging in the
    /// current default fg/bg when the highlight leaves them unset.
    ///
    /// This runs once per run of same-styled cells inside
    /// `apply_grid_line`, never from the render path.
    fn style_for(&self, hl_id: u64) -> Style {
        let id = hl_id as usize;
        if id == 0 {
            return self.current_default_style();
        }
        let entry = self.hl_attrs.get(id).copied().unwrap_or(HlEntry::DEFAULT);
        let mut style = if self.termguicolors {
            entry.rgb
        } else {
            entry.cterm
        };
        let default_style = self.current_default_style();
        if style.fg().is_none() {
            if let Some(c) = default_style.fg() {
                style = style.with_fg(c);
            }
        }
        if style.bg().is_none() {
            if let Some(c) = default_style.bg() {
                style = style.with_bg(c);
            }
        }
        style
    }

    fn put_hl_attr(&mut self, id: u64, entry: HlEntry) {
        let id = id as usize;
        if id >= self.hl_attrs.len() {
            self.hl_attrs.resize(id + 1, HlEntry::DEFAULT);
        }
        self.hl_attrs[id] = entry;
    }

    /// Applies a batch of `redraw` events decoded from `r`.
    ///
    /// The reader must be positioned at the outer array of events,
    /// where each element takes the shape `[name, params_1, ...]`.
    ///
    /// # Errors
    ///
    /// Returns [`Error::Msgpack`] if the payload is truncated or
    /// malformed, or [`Error::OutOfBounds`] if an event references a
    /// coordinate outside the grid.
    pub fn apply_redraw(&mut self, r: &mut Reader<'_>) -> Result<(), Error> {
        let event_count = r.read_array_len()?;
        for _ in 0..event_count {
            let group_len = r.read_array_len()?;
            if group_len == 0 {
                continue;
            }
            let name = r.read_str()?;
            for _ in 1..group_len {
                let params_len = r.read_array_len()?;
                self.apply_event(name, params_len, r)?;
            }
        }
        self.maybe_compact_side();
        Ok(())
    }

    /// Calls [`extui::Buffer::compact_side`] when the grid's handle
    /// storage has grown past a size proportional to the grid area.
    ///
    /// Neovim rewrites the grid on every frame. Sessions that touch
    /// long graphemes such as flag emoji, ZWJ sequences, or combining
    /// marks accumulate unreferenced handle bytes over time. Running
    /// this check after each redraw keeps the storage bounded without
    /// triggering work in sessions made up of ordinary ASCII text.
    fn maybe_compact_side(&mut self) {
        let area = self.grid.width() as usize * self.grid.height() as usize;
        let threshold = (area * 32).max(16 * 1024);
        if self.grid.side_len() > threshold {
            self.grid.compact_side();
        }
    }

    fn apply_event(
        &mut self,
        name: &str,
        params_len: usize,
        r: &mut Reader<'_>,
    ) -> Result<(), Error> {
        match name {
            "grid_resize" => self.apply_grid_resize(params_len, r),
            "grid_clear" => self.apply_grid_clear(params_len, r),
            "grid_line" => self.apply_grid_line(params_len, r),
            "grid_scroll" => self.apply_grid_scroll(params_len, r),
            "grid_cursor_goto" => self.apply_grid_cursor_goto(params_len, r),
            "default_colors_set" => self.apply_default_colors_set(params_len, r),
            "hl_attr_define" => self.apply_hl_attr_define(params_len, r),
            "option_set" => self.apply_option_set(params_len, r),
            "mode_info_set" => self.apply_mode_info_set(params_len, r),
            "mode_change" => self.apply_mode_change(params_len, r),
            "busy_start" => {
                skip_remaining(r, params_len)?;
                self.busy = true;
                Ok(())
            }
            "busy_stop" => {
                skip_remaining(r, params_len)?;
                self.busy = false;
                Ok(())
            }
            "flush" => {
                skip_remaining(r, params_len)?;
                self.dirty = true;
                Ok(())
            }
            _ => {
                skip_remaining(r, params_len)?;
                Ok(())
            }
        }
    }

    fn apply_grid_resize(&mut self, params_len: usize, r: &mut Reader<'_>) -> Result<(), Error> {
        let _grid = r.read_u64()?;
        let width = r.read_u64()? as u16;
        let height = r.read_u64()? as u16;
        for _ in 3..params_len {
            r.skip()?;
        }
        self.resize(width, height);
        Ok(())
    }

    fn resize(&mut self, width: u16, height: u16) {
        self.grid = Buffer::new(width, height);
        if self.cursor_col >= width {
            self.cursor_col = width.saturating_sub(1);
        }
        if self.cursor_row >= height {
            self.cursor_row = height.saturating_sub(1);
        }
    }

    fn apply_grid_clear(&mut self, params_len: usize, r: &mut Reader<'_>) -> Result<(), Error> {
        skip_remaining(r, params_len)?;
        // Neovim follows `grid_clear` with fresh `grid_line` events that
        // repaint the visible area, so we just drop back to an empty
        // buffer here.
        self.grid = Buffer::new(self.grid.width(), self.grid.height());
        Ok(())
    }

    fn apply_grid_line(&mut self, params_len: usize, r: &mut Reader<'_>) -> Result<(), Error> {
        let _grid = r.read_u64()?;
        let row = r.read_u64()? as u16;
        let col_start = r.read_u64()? as u16;
        let cell_count = r.read_array_len()?;
        let mut col = col_start;
        let mut current_hl_id: u64 = 0;
        let mut current_style = self.style_for(current_hl_id);
        for _ in 0..cell_count {
            let tuple_len = r.read_array_len()?;
            if tuple_len == 0 {
                return Err(Error::Msgpack(MsgpackError::TypeMismatch));
            }
            let text = r.read_str()?;
            let mut repeat: u64 = 1;
            if tuple_len >= 2 {
                let new_hl_id = r.read_u64()?;
                if new_hl_id != current_hl_id {
                    current_hl_id = new_hl_id;
                    current_style = self.style_for(current_hl_id);
                }
            }
            if tuple_len >= 3 {
                repeat = r.read_u64()?;
            }
            for _ in 3..tuple_len {
                r.skip()?;
            }
            // An empty text marks a wide-char continuation cell; leave
            // the slot as the EMPTY cell the lead grapheme already
            // installed via `set_stringn`'s width-aware fill.
            if text.is_empty() {
                for _ in 0..repeat {
                    col = col.saturating_add(1);
                }
                continue;
            }
            for _ in 0..repeat {
                if col >= self.grid.width() || row >= self.grid.height() {
                    break;
                }
                // `max_width = usize::MAX` lets wide graphemes (e.g.
                // `日`, width 2) actually land; the wire protocol sends
                // an explicit empty continuation cell that we
                // subsequently skip over, so advancing `col` by one
                // here is correct regardless of grapheme width.
                self.grid
                    .set_stringn(col, row, text, usize::MAX, current_style);
                col = col.saturating_add(1);
            }
        }
        for _ in 4..params_len {
            r.skip()?;
        }
        Ok(())
    }

    fn apply_grid_scroll(&mut self, params_len: usize, r: &mut Reader<'_>) -> Result<(), Error> {
        let _grid = r.read_u64()?;
        let top = r.read_u64()? as u16;
        let bot = r.read_u64()? as u16;
        let left = r.read_u64()? as u16;
        let right = r.read_u64()? as u16;
        let rows = r.read_i64()?;
        for _ in 6..params_len {
            r.skip()?;
        }
        self.grid.scroll_region(top, bot, left, right, rows as i32);
        Ok(())
    }

    fn apply_grid_cursor_goto(
        &mut self,
        params_len: usize,
        r: &mut Reader<'_>,
    ) -> Result<(), Error> {
        let _grid = r.read_u64()?;
        let row = r.read_u64()? as u16;
        let col = r.read_u64()? as u16;
        for _ in 3..params_len {
            r.skip()?;
        }
        self.cursor_row = row;
        self.cursor_col = col;
        Ok(())
    }

    fn apply_default_colors_set(
        &mut self,
        params_len: usize,
        r: &mut Reader<'_>,
    ) -> Result<(), Error> {
        let rgb_fg = r.read_i64()?;
        let rgb_bg = r.read_i64()?;
        let _rgb_sp = r.read_i64()?;
        let cterm_fg = r.read_i64()?;
        let cterm_bg = r.read_i64()?;
        for _ in 5..params_len {
            r.skip()?;
        }
        let mut rgb_style = Style::DEFAULT;
        if let Some(c) = rgb_color_from_wire(rgb_fg) {
            rgb_style = rgb_style.with_fg(c);
        }
        if let Some(c) = rgb_color_from_wire(rgb_bg) {
            rgb_style = rgb_style.with_bg(c);
        }

        let mut cterm_style = Style::DEFAULT;
        if let Some(c) = cterm_default_color(cterm_fg) {
            cterm_style = cterm_style.with_fg(c);
        }
        if let Some(c) = cterm_default_color(cterm_bg) {
            cterm_style = cterm_style.with_bg(c);
        }
        self.default_rgb_style = rgb_style;
        self.default_cterm_style = cterm_style;
        if self.hl_attrs.is_empty() {
            self.hl_attrs.push(HlEntry {
                rgb: rgb_style,
                cterm: cterm_style,
            });
        } else {
            self.hl_attrs[0] = HlEntry {
                rgb: rgb_style,
                cterm: cterm_style,
            };
        }
        self.dirty = true;
        Ok(())
    }

    fn apply_hl_attr_define(&mut self, params_len: usize, r: &mut Reader<'_>) -> Result<(), Error> {
        let id = r.read_u64()?;
        let rgb = read_rgb_attr_map(r)?;
        let cterm = read_attr_map(r)?;
        for _ in 3..params_len {
            r.skip()?;
        }
        // Linegrid always sends both rgb and cterm attrs. Resolve both
        // into a [`Style`] here, once; the render path just picks one
        // based on the current `termguicolors` setting.
        let mut rgb_style = rgb.modifiers;
        if let Some(c) = rgb.fg {
            rgb_style = rgb_style.with_fg(c);
        }
        if let Some(c) = rgb.bg {
            rgb_style = rgb_style.with_bg(c);
        }

        let mut cterm_style = cterm.modifiers;
        if let Some(c) = cterm.fg {
            cterm_style = cterm_style.with_fg(c);
        }
        if let Some(c) = cterm.bg {
            cterm_style = cterm_style.with_bg(c);
        }
        self.put_hl_attr(
            id,
            HlEntry {
                rgb: rgb_style,
                cterm: cterm_style,
            },
        );
        self.dirty = true;
        Ok(())
    }

    fn apply_option_set(&mut self, params_len: usize, r: &mut Reader<'_>) -> Result<(), Error> {
        if params_len == 0 {
            return Ok(());
        }
        let name = r.read_str()?;
        if name == "termguicolors" && params_len >= 2 {
            self.termguicolors = r.read_bool()?;
            for _ in 2..params_len {
                r.skip()?;
            }
            self.dirty = true;
            return Ok(());
        }
        for _ in 1..params_len {
            r.skip()?;
        }
        Ok(())
    }

    fn apply_mode_info_set(&mut self, params_len: usize, r: &mut Reader<'_>) -> Result<(), Error> {
        let _enabled = r.read_bool()?;
        let mode_count = r.read_array_len()?;
        let mut modes = Vec::with_capacity(mode_count);
        for _ in 0..mode_count {
            modes.push(read_mode_info(r)?);
        }
        for _ in 2..params_len {
            r.skip()?;
        }
        self.modes = modes;
        if self.current_mode >= self.modes.len() {
            self.current_mode = 0;
        }
        Ok(())
    }

    fn apply_mode_change(&mut self, params_len: usize, r: &mut Reader<'_>) -> Result<(), Error> {
        let _name = r.read_str()?;
        let idx = r.read_u64()? as usize;
        for _ in 2..params_len {
            r.skip()?;
        }
        self.current_mode = idx;
        Ok(())
    }

    /// Paints the grid into `rect` on `buf`.
    ///
    /// The grid is clipped to fit `rect` if its dimensions differ. The
    /// area of `buf` outside the grid's footprint is left untouched.
    pub fn render(&self, rect: Rect, buf: &mut DoubleBuffer) {
        let grid_w = self.grid.width();
        let grid_h = self.grid.height();
        let height = grid_h.min(rect.h);
        let width = grid_w.min(rect.w);
        let cells = self.grid.cells();
        for row in 0..height {
            let base = (row as usize) * (grid_w as usize);
            let row_slice = &cells[base..base + width as usize];
            for (col, &cell) in row_slice.iter().enumerate() {
                let target_x = rect.x + col as u16;
                let target_y = rect.y + row;
                if !cell.is_handle() {
                    buf.set_cell(target_x, target_y, cell);
                    continue;
                }
                if let Some(bytes) = self.grid.handle_text(cell) {
                    // Handle cells only exist for graphemes that passed
                    // UTF-8 validation via `set_stringn`, so this is
                    // guaranteed valid.
                    let text =
                        unsafe { std::str::from_utf8_unchecked(bytes) };
                    buf.set_stringn(target_x, target_y, text, 1, cell.style());
                }
            }
        }
    }
}

fn skip_remaining(r: &mut Reader<'_>, params_len: usize) -> Result<(), Error> {
    for _ in 0..params_len {
        r.skip()?;
    }
    Ok(())
}

struct AttrMap {
    fg: Option<Color>,
    bg: Option<Color>,
    modifiers: Style,
}

fn read_attr_map(r: &mut Reader<'_>) -> Result<AttrMap, Error> {
    let n = r.read_map_len()?;
    let mut out = AttrMap {
        fg: None,
        bg: None,
        modifiers: Style::DEFAULT,
    };
    for _ in 0..n {
        let key = r.read_str()?;
        match key {
            "foreground" => {
                out.fg = read_cterm_color(r)?;
            }
            "background" => {
                out.bg = read_cterm_color(r)?;
            }
            "bold" => {
                if r.read_bool()? {
                    out.modifiers = out.modifiers | Style::from(Modifier::BOLD);
                }
            }
            "italic" => {
                if r.read_bool()? {
                    out.modifiers = out.modifiers | Style::from(Modifier::ITALIC);
                }
            }
            "underline" | "underdouble" | "undercurl" | "underdotted" | "underdashed" => {
                if r.read_bool()? {
                    out.modifiers = out.modifiers | Style::from(Modifier::UNDERLINED);
                }
            }
            "reverse" | "standout" => {
                if r.read_bool()? {
                    out.modifiers = out.modifiers | Style::from(Modifier::REVERSED);
                }
            }
            "strikethrough" => {
                if r.read_bool()? {
                    out.modifiers = out.modifiers | Style::from(Modifier::CROSSED_OUT);
                }
            }
            _ => {
                r.skip()?;
            }
        }
    }
    Ok(out)
}

/// Reads an `hl_attr_define` rgb-attr dict, preserving 24-bit colors.
///
/// Modifier bits are collected in the same way as [`read_attr_map`]
/// and the caller picks whichever source it prefers based on the
/// current `termguicolors` setting. The host terminal's truecolor
/// support is handled by [`DoubleBuffer`] during emission.
fn read_rgb_attr_map(r: &mut Reader<'_>) -> Result<AttrMap, Error> {
    let n = r.read_map_len()?;
    let mut out = AttrMap {
        fg: None,
        bg: None,
        modifiers: Style::DEFAULT,
    };
    for _ in 0..n {
        let key = r.read_str()?;
        match key {
            "foreground" => out.fg = read_rgb_color(r)?,
            "background" => out.bg = read_rgb_color(r)?,
            "bold" => {
                if r.read_bool()? {
                    out.modifiers = out.modifiers | Style::from(Modifier::BOLD);
                }
            }
            "italic" => {
                if r.read_bool()? {
                    out.modifiers = out.modifiers | Style::from(Modifier::ITALIC);
                }
            }
            "underline" | "underdouble" | "undercurl" | "underdotted" | "underdashed" => {
                if r.read_bool()? {
                    out.modifiers = out.modifiers | Style::from(Modifier::UNDERLINED);
                }
            }
            "reverse" | "standout" => {
                if r.read_bool()? {
                    out.modifiers = out.modifiers | Style::from(Modifier::REVERSED);
                }
            }
            "strikethrough" => {
                if r.read_bool()? {
                    out.modifiers = out.modifiers | Style::from(Modifier::CROSSED_OUT);
                }
            }
            _ => {
                r.skip()?;
            }
        }
    }
    Ok(out)
}

fn read_rgb_color(r: &mut Reader<'_>) -> Result<Option<Color>, Error> {
    match r.peek_kind()? {
        Kind::Nil => {
            r.read_nil()?;
            Ok(None)
        }
        Kind::Int => {
            let v = r.read_i64()?;
            Ok(rgb_color_from_wire(v))
        }
        _ => {
            r.skip()?;
            Ok(None)
        }
    }
}

/// Unpacks a 24-bit RGB value as received on the Neovim RPC wire.
///
/// Values outside `0..=0xffffff` mean "no color". ANSI fallback is
/// handled by [`DoubleBuffer`] on terminals that lack truecolor.
fn rgb_color_from_wire(v: i64) -> Option<Color> {
    if !(0..=0xff_ff_ff).contains(&v) {
        return None;
    }
    let v = v as u32;
    let r = (v >> 16) as u8;
    let g = (v >> 8) as u8;
    let b = v as u8;
    Some(Color::Rgb(Rgb(r, g, b)))
}

/// Reads a cterm color value from an `hl_attr_define` cterm dict.
///
/// In the cterm dict on the wire, present `foreground`/`background` keys
/// are zero-based 256-color indices. An absent key means "fall back to
/// the default highlight", which is handled by the caller.
fn read_cterm_color(r: &mut Reader<'_>) -> Result<Option<Color>, Error> {
    match r.peek_kind()? {
        Kind::Nil => {
            r.read_nil()?;
            Ok(None)
        }
        Kind::Int => {
            let v = r.read_i64()?;
            if (0..=255).contains(&v) {
                Ok(Some(Color::Ansi(AnsiColor(v as u8))))
            } else {
                Ok(None)
            }
        }
        _ => {
            r.skip()?;
            Ok(None)
        }
    }
}

/// Maps a `default_colors_set` cterm field to an optional color.
///
/// Neovim sends these as 1-based values where `0` means "use the
/// terminal default". Any positive value `v` maps to `AnsiColor(v - 1)`.
fn cterm_default_color(v: i64) -> Option<Color> {
    if (1..=256).contains(&v) {
        Some(Color::Ansi(AnsiColor((v - 1) as u8)))
    } else {
        None
    }
}

fn read_mode_info(r: &mut Reader<'_>) -> Result<ModeInfo, Error> {
    let n = r.read_map_len()?;
    let mut info = ModeInfo::default();
    for _ in 0..n {
        let key = r.read_str()?;
        match key {
            "cursor_shape" => {
                let shape = r.read_str()?;
                info.shape = match shape {
                    "block" => CursorShape::Block,
                    "horizontal" => CursorShape::Underline,
                    "vertical" => CursorShape::Bar,
                    _ => CursorShape::Block,
                };
            }
            _ => r.skip()?,
        }
    }
    Ok(info)
}

#[cfg(test)]
mod tests {
    use extui::Color;

    use super::*;
    use crate::msgpack::Writer;

    fn build_redraw(events: &[&dyn Fn(&mut Writer)]) -> Vec<u8> {
        let mut w = Writer::new();
        w.write_array_header(events.len());
        for ev in events {
            ev(&mut w);
        }
        w.into_bytes()
    }

    fn cell_text(g: &GridState, col: u16, row: u16) -> String {
        let w = g.grid.width() as usize;
        let idx = row as usize * w + col as usize;
        let cell = g.grid.cells()[idx];
        if let Some(s) = cell.text_inline() {
            s.to_string()
        } else if let Some(bytes) = g.grid.handle_text(cell) {
            std::str::from_utf8(bytes).unwrap().to_string()
        } else {
            String::new()
        }
    }

    #[test]
    fn grid_line_rle_and_inherit() {
        let bytes = build_redraw(&[&|w| {
            w.write_array_header(2);
            w.write_str("grid_line");
            w.write_array_header(5);
            w.write_u64(1); // grid
            w.write_u64(0); // row
            w.write_u64(0); // col_start
            w.write_array_header(3);
            {
                w.write_array_header(3);
                w.write_str("a");
                w.write_u64(0);
                w.write_u64(3);
            }
            {
                w.write_array_header(2);
                w.write_str("b");
                w.write_u64(0);
            }
            {
                w.write_array_header(1);
                w.write_str("c");
            }
            w.write_bool(false); // wrap
        }]);
        let mut g = GridState::new(5, 1);
        let mut r = Reader::new(&bytes);
        g.apply_redraw(&mut r).unwrap();
        let cells: Vec<String> = (0..5).map(|c| cell_text(&g, c, 0)).collect();
        assert_eq!(cells, vec!["a", "a", "a", "b", "c"]);
    }

    #[test]
    fn grid_scroll_up() {
        let mut g = GridState::new(4, 3);
        for row in 0..3u16 {
            let s = match row {
                0 => "A",
                1 => "B",
                _ => "C",
            };
            for col in 0..4u16 {
                g.grid.set_stringn(col, row, s, 1, Style::DEFAULT);
            }
        }
        g.grid.scroll_region(0, 3, 0, 4, 1);
        let row0: String = (0..4).map(|c| cell_text(&g, c, 0)).collect();
        let row1: String = (0..4).map(|c| cell_text(&g, c, 1)).collect();
        assert_eq!(row0, "BBBB");
        assert_eq!(row1, "CCCC");
    }

    #[test]
    fn grid_scroll_down() {
        let mut g = GridState::new(4, 3);
        for row in 0..3u16 {
            let s = match row {
                0 => "A",
                1 => "B",
                _ => "C",
            };
            for col in 0..4u16 {
                g.grid.set_stringn(col, row, s, 1, Style::DEFAULT);
            }
        }
        g.grid.scroll_region(0, 3, 0, 4, -1);
        let row1: String = (0..4).map(|c| cell_text(&g, c, 1)).collect();
        let row2: String = (0..4).map(|c| cell_text(&g, c, 2)).collect();
        assert_eq!(row1, "AAAA");
        assert_eq!(row2, "BBBB");
    }

    #[test]
    fn default_colors_and_hl_attr_define() {
        let bytes = build_redraw(&[
            &|w| {
                w.write_array_header(2);
                w.write_str("default_colors_set");
                w.write_array_header(5);
                w.write_i64(-1);
                w.write_i64(-1);
                w.write_i64(-1);
                w.write_u64(16);
                w.write_u64(1);
            },
            &|w| {
                w.write_array_header(2);
                w.write_str("hl_attr_define");
                w.write_array_header(4);
                w.write_u64(1);
                w.write_map_header(0);
                w.write_map_header(3);
                w.write_str("foreground");
                w.write_u64(196);
                w.write_str("bold");
                w.write_bool(true);
                w.write_str("reverse");
                w.write_bool(true);
                w.write_array_header(0);
            },
        ]);

        let mut g = GridState::new(1, 1);
        let mut r = Reader::new(&bytes);
        g.apply_redraw(&mut r).unwrap();
        g.termguicolors = false;
        assert_eq!(g.current_default_style().fg(), Some(Color::ansi(15)));
        assert_eq!(g.current_default_style().bg(), Some(Color::ansi(0)));
        let hl = g.style_for(1);
        assert_eq!(hl.fg(), Some(Color::ansi(196)));
        assert_eq!(hl.bg(), Some(Color::ansi(0)));
        assert!(hl.modifiers().has(Modifier::BOLD));
        assert!(hl.modifiers().has(Modifier::REVERSED));
    }

    #[test]
    fn hl_attr_define_prefers_cterm_over_rgb() {
        // With 'termguicolors' off, prefer the cterm attr whenever it has
        // explicit content.
        let bytes = build_redraw(&[&|w| {
            w.write_array_header(2);
            w.write_str("hl_attr_define");
            w.write_array_header(4);
            w.write_u64(7);
            w.write_map_header(2);
            w.write_str("foreground");
            w.write_u64(0x5f_87_af);
            w.write_str("background");
            w.write_u64(0x1c_1c_1c);
            w.write_map_header(2);
            w.write_str("foreground");
            w.write_u64(67);
            w.write_str("background");
            w.write_u64(234);
            w.write_array_header(0);
        }]);
        let mut g = GridState::new(1, 1);
        let mut r = Reader::new(&bytes);
        g.apply_redraw(&mut r).unwrap();
        g.termguicolors = false;
        let hl = g.style_for(7);
        assert_eq!(hl.fg(), Some(Color::ansi(67)));
        assert_eq!(hl.bg(), Some(Color::ansi(234)));
    }

    #[test]
    fn hl_attr_define_falls_back_to_cterm_when_rgb_empty() {
        let bytes = build_redraw(&[&|w| {
            w.write_array_header(2);
            w.write_str("hl_attr_define");
            w.write_array_header(4);
            w.write_u64(3);
            w.write_map_header(0);
            w.write_map_header(2);
            w.write_str("foreground");
            w.write_u64(9);
            w.write_str("bold");
            w.write_bool(true);
            w.write_array_header(0);
        }]);
        let mut g = GridState::new(1, 1);
        let mut r = Reader::new(&bytes);
        g.apply_redraw(&mut r).unwrap();
        g.termguicolors = false;
        let hl = g.style_for(3);
        assert_eq!(hl.fg(), Some(Color::ansi(9)));
        assert!(hl.modifiers().has(Modifier::BOLD));
    }

    #[test]
    fn hl_attr_define_falls_back_to_rgb_when_cterm_is_empty() {
        let bytes = build_redraw(&[&|w| {
            w.write_array_header(2);
            w.write_str("hl_attr_define");
            w.write_array_header(4);
            w.write_u64(5);
            w.write_map_header(2);
            w.write_str("foreground");
            w.write_u64(0xee_ee_ee);
            w.write_str("italic");
            w.write_bool(true);
            w.write_map_header(0);
            w.write_array_header(0);
        }]);
        let mut g = GridState::new(1, 1);
        let mut r = Reader::new(&bytes);
        g.apply_redraw(&mut r).unwrap();
        g.termguicolors = true;
        let hl = g.style_for(5);
        assert_eq!(hl.fg(), Some(Color::rgb(0xee, 0xee, 0xee)));
        assert_eq!(hl.bg(), None);
        assert!(hl.modifiers().has(Modifier::ITALIC));
    }

    #[test]
    fn hl_attr_define_prefers_rgb_when_termguicolors_enabled() {
        let bytes = build_redraw(&[&|w| {
            w.write_array_header(2);
            w.write_str("hl_attr_define");
            w.write_array_header(4);
            w.write_u64(9);
            w.write_map_header(2);
            w.write_str("foreground");
            w.write_u64(0x5f_87_af);
            w.write_str("background");
            w.write_u64(0x1c_1c_1c);
            w.write_map_header(2);
            w.write_str("foreground");
            w.write_u64(14);
            w.write_str("background");
            w.write_u64(0);
            w.write_array_header(0);
        }]);
        let mut g = GridState::new(1, 1);
        let mut r = Reader::new(&bytes);
        g.apply_redraw(&mut r).unwrap();
        g.termguicolors = true;
        let hl = g.style_for(9);
        assert_eq!(hl.fg(), Some(Color::rgb(0x5f, 0x87, 0xaf)));
        assert_eq!(hl.bg(), Some(Color::rgb(0x1c, 0x1c, 0x1c)));
    }

    #[test]
    fn hl_attr_define_resolves_reverse_from_cterm_side() {
        // Under 'termguicolors' off, the default `StatusLine` often ships as
        // `cterm={reverse=true}` with no explicit cterm fg/bg. The embed
        // should surface the reverse modifier and inherit the colours from
        // the current default style via `style_for`.
        let bytes = build_redraw(&[&|w| {
            w.write_array_header(2);
            w.write_str("hl_attr_define");
            w.write_array_header(4);
            w.write_u64(4);
            w.write_map_header(0);
            w.write_map_header(1);
            w.write_str("reverse");
            w.write_bool(true);
            w.write_array_header(0);
        }]);
        let mut g = GridState::new(1, 1);
        let mut r = Reader::new(&bytes);
        g.apply_redraw(&mut r).unwrap();
        g.termguicolors = false;
        let hl = g.style_for(4);
        assert!(hl.modifiers().has(Modifier::REVERSED));
    }

    #[test]
    fn default_colors_set_leaves_terminal_defaults_unset() {
        let bytes = build_redraw(&[&|w| {
            w.write_array_header(2);
            w.write_str("default_colors_set");
            w.write_array_header(5);
            w.write_i64(0x12_34_56);
            w.write_i64(0x23_23_23);
            w.write_i64(-1);
            w.write_i64(0);
            w.write_i64(0);
        }]);

        let mut g = GridState::new(1, 1);
        let mut r = Reader::new(&bytes);
        g.apply_redraw(&mut r).unwrap();
        g.termguicolors = false;
        assert_eq!(g.current_default_style(), Style::DEFAULT);
    }

    #[test]
    fn mode_info_and_cursor_goto() {
        let bytes = build_redraw(&[
            &|w| {
                w.write_array_header(2);
                w.write_str("mode_info_set");
                w.write_array_header(2);
                w.write_bool(true);
                w.write_array_header(2);
                {
                    w.write_map_header(1);
                    w.write_str("cursor_shape");
                    w.write_str("block");
                }
                {
                    w.write_map_header(1);
                    w.write_str("cursor_shape");
                    w.write_str("vertical");
                }
            },
            &|w| {
                w.write_array_header(2);
                w.write_str("mode_change");
                w.write_array_header(2);
                w.write_str("insert");
                w.write_u64(1);
            },
            &|w| {
                w.write_array_header(2);
                w.write_str("grid_cursor_goto");
                w.write_array_header(3);
                w.write_u64(1);
                w.write_u64(3);
                w.write_u64(7);
            },
            &|w| {
                w.write_array_header(2);
                w.write_str("flush");
                w.write_array_header(0);
            },
        ]);
        let mut g = GridState::new(20, 10);
        let mut r = Reader::new(&bytes);
        g.apply_redraw(&mut r).unwrap();
        assert_eq!(g.cursor_row, 3);
        assert_eq!(g.cursor_col, 7);
        assert_eq!(g.cursor_shape(), CursorShape::Bar);
        assert!(g.take_dirty());
        assert!(!g.take_dirty());
    }

    #[test]
    fn side_arena_stays_bounded_under_handle_churn() {
        // Flag emojis always land as handle cells (8 bytes > 7 inline
        // cap). Repeatedly overwriting the same slot with a fresh flag
        // should not let the side arena grow without bound: once we
        // cross the threshold, `apply_redraw` rebuilds it.
        let flag = "🇨🇦";

        let mut g = GridState::new(10, 1);
        for _ in 0..4000 {
            let bytes = build_redraw(&[&|w| {
                w.write_array_header(2);
                w.write_str("grid_line");
                w.write_array_header(5);
                w.write_u64(1);
                w.write_u64(0);
                w.write_u64(0);
                w.write_array_header(1);
                w.write_array_header(2);
                w.write_str(flag);
                w.write_u64(0);
                w.write_bool(false);
            }]);
            let mut r = Reader::new(&bytes);
            g.apply_redraw(&mut r).unwrap();
        }

        // Post-churn arena should be far below the uncompacted total
        // (4000 × 8 = 32 KB). Threshold for a 10×1 grid is 16 KiB, so
        // we should see at most one threshold's worth of bytes live.
        assert!(
            g.grid.side_len() <= 16 * 1024,
            "side arena leaked: {} bytes",
            g.grid.side_len(),
        );

        // Whatever survived must still resolve the flag correctly.
        let cell = g.grid.cells()[0];
        assert!(cell.is_handle());
        assert_eq!(g.grid.handle_text(cell).unwrap(), flag.as_bytes());
    }

    #[test]
    fn grid_line_wide_char_continuation() {
        let bytes = build_redraw(&[&|w| {
            w.write_array_header(2);
            w.write_str("grid_line");
            w.write_array_header(5);
            w.write_u64(1);
            w.write_u64(0);
            w.write_u64(0);
            w.write_array_header(3);
            {
                w.write_array_header(2);
                w.write_str("日");
                w.write_u64(0);
            }
            {
                w.write_array_header(2);
                w.write_str("");
                w.write_u64(0);
            }
            {
                w.write_array_header(2);
                w.write_str("x");
                w.write_u64(0);
            }
            w.write_bool(false);
        }]);
        let mut g = GridState::new(3, 1);
        let mut r = Reader::new(&bytes);
        g.apply_redraw(&mut r).unwrap();
        assert_eq!(cell_text(&g, 0, 0), "日");
        // Continuation cell left empty by set_stringn width fill.
        assert!(g.grid.cells()[1].is_empty());
        assert_eq!(cell_text(&g, 2, 0), "x");
    }
}
