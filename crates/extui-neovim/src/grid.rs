//! Internal grid state mirroring Neovim's `ext_linegrid` protocol.
//!
//! The reader thread decodes each `redraw` notification and calls the
//! corresponding `apply_*` method on [`GridState`]. Rendering walks the
//! cell buffer and emits styled runs into an [`extui::DoubleBuffer`].

use extui::vt::Modifier;
use extui::{Color, DoubleBuffer, Rect, Style};

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

/// A single grid cell: up to four UTF-8 bytes plus a style.
///
/// `len == 0` marks a continuation cell placed after a double-width
/// grapheme. Renderers must skip continuations; `extui` consumes the second
/// column automatically via the width of the lead grapheme.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct GridCell {
    bytes: [u8; 4],
    len: u8,
    style: Style,
}

impl GridCell {
    const fn blank() -> Self {
        GridCell {
            bytes: [b' ', 0, 0, 0],
            len: 1,
            style: Style::DEFAULT,
        }
    }

    fn with_style(style: Style) -> Self {
        GridCell {
            bytes: [b' ', 0, 0, 0],
            len: 1,
            style,
        }
    }

    fn from_str(s: &str, style: Style) -> Self {
        let mut bytes = [0u8; 4];
        let len = s.len().min(4);
        if s.len() <= 4 {
            bytes[..len].copy_from_slice(s.as_bytes());
            GridCell {
                bytes,
                len: len as u8,
                style,
            }
        } else {
            bytes[0] = b'?';
            GridCell {
                bytes,
                len: 1,
                style,
            }
        }
    }

    fn text(&self) -> &str {
        let len = self.len as usize;
        unsafe { std::str::from_utf8_unchecked(&self.bytes[..len]) }
    }
}

/// Mirrors the `ext_linegrid` grid maintained by an embedded Neovim.
pub struct GridState {
    width: u16,
    height: u16,
    cells: Vec<GridCell>,
    cursor_row: u16,
    cursor_col: u16,
    default_style: Style,
    hl_attrs: Vec<Style>,
    modes: Vec<ModeInfo>,
    current_mode: usize,
    busy: bool,
    dirty: bool,
    alive: bool,
}

impl GridState {
    /// Creates a new grid of the given dimensions filled with blank cells.
    pub fn new(width: u16, height: u16) -> Self {
        let mut hl_attrs = Vec::new();
        hl_attrs.push(Style::DEFAULT);
        GridState {
            width,
            height,
            cells: vec![GridCell::blank(); width as usize * height as usize],
            cursor_row: 0,
            cursor_col: 0,
            default_style: Style::DEFAULT,
            hl_attrs,
            modes: Vec::new(),
            current_mode: 0,
            busy: false,
            dirty: false,
            alive: true,
        }
    }

    /// Returns the grid width in cells.
    pub fn width(&self) -> u16 {
        self.width
    }

    /// Returns the grid height in cells.
    pub fn height(&self) -> u16 {
        self.height
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

    fn index(&self, row: u16, col: u16) -> Option<usize> {
        if row >= self.height || col >= self.width {
            return None;
        }
        Some(row as usize * self.width as usize + col as usize)
    }

    fn style_for(&self, hl_id: u64) -> Style {
        let id = hl_id as usize;
        if id == 0 {
            return self.default_style;
        }
        let raw = self
            .hl_attrs
            .get(id)
            .copied()
            .unwrap_or(Style::DEFAULT);
        let mut style = raw;
        if style.fg().is_none() {
            if let Some(c) = self.default_style.fg() {
                style = style.with_fg(c);
            }
        }
        if style.bg().is_none() {
            if let Some(c) = self.default_style.bg() {
                style = style.with_bg(c);
            }
        }
        style
    }

    fn put_hl_attr(&mut self, id: u64, style: Style) {
        let id = id as usize;
        if id >= self.hl_attrs.len() {
            self.hl_attrs.resize(id + 1, self.default_style);
        }
        self.hl_attrs[id] = style;
    }

    /// Dispatches a top-level `redraw` event batch from a [`Reader`].
    ///
    /// Expects the reader to be positioned at the outer array of events
    /// (each element is `[name, params_1, params_2, ...]`).
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
        Ok(())
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
        self.width = width;
        self.height = height;
        let blank = GridCell::with_style(self.default_style);
        self.cells = vec![blank; width as usize * height as usize];
        if self.cursor_col >= width {
            self.cursor_col = width.saturating_sub(1);
        }
        if self.cursor_row >= height {
            self.cursor_row = height.saturating_sub(1);
        }
    }

    fn apply_grid_clear(&mut self, params_len: usize, r: &mut Reader<'_>) -> Result<(), Error> {
        skip_remaining(r, params_len)?;
        let blank = GridCell::with_style(self.default_style);
        self.cells.fill(blank);
        Ok(())
    }

    fn apply_grid_line(&mut self, params_len: usize, r: &mut Reader<'_>) -> Result<(), Error> {
        let _grid = r.read_u64()?;
        let row = r.read_u64()? as u16;
        let col_start = r.read_u64()? as u16;
        let cell_count = r.read_array_len()?;
        let mut col = col_start;
        let mut current_hl_id: u64 = 0;
        for _ in 0..cell_count {
            let tuple_len = r.read_array_len()?;
            if tuple_len == 0 {
                return Err(Error::Msgpack(MsgpackError::TypeMismatch));
            }
            let text = r.read_str()?;
            let mut repeat: u64 = 1;
            if tuple_len >= 2 {
                current_hl_id = r.read_u64()?;
            }
            if tuple_len >= 3 {
                repeat = r.read_u64()?;
            }
            for _ in 3..tuple_len {
                r.skip()?;
            }
            let style = self.style_for(current_hl_id);
            let cell = if text.is_empty() {
                GridCell {
                    bytes: [0; 4],
                    len: 0,
                    style,
                }
            } else {
                GridCell::from_str(text, style)
            };
            for _ in 0..repeat {
                let Some(idx) = self.index(row, col) else {
                    break;
                };
                self.cells[idx] = cell;
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
        self.scroll_region(top, bot, left, right, rows);
        Ok(())
    }

    fn scroll_region(&mut self, top: u16, bot: u16, left: u16, right: u16, rows: i64) {
        if bot <= top || right <= left || rows == 0 {
            return;
        }
        let top = top.min(self.height);
        let bot = bot.min(self.height);
        let left = left.min(self.width);
        let right = right.min(self.width);
        let width = self.width as usize;
        let run = (right - left) as usize;
        if rows > 0 {
            let s = rows as u16;
            if s >= bot - top {
                return;
            }
            for y in top..bot - s {
                let src = (y + s) as usize * width + left as usize;
                let dst = y as usize * width + left as usize;
                self.cells.copy_within(src..src + run, dst);
            }
        } else {
            let s = (-rows) as u16;
            if s >= bot - top {
                return;
            }
            let mut y = bot - 1;
            while y >= top + s {
                let src = (y - s) as usize * width + left as usize;
                let dst = y as usize * width + left as usize;
                self.cells.copy_within(src..src + run, dst);
                if y == 0 {
                    break;
                }
                y -= 1;
            }
        }
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
        // The embed forces `termguicolors=false` at attach time, so the
        // authoritative values are `cterm_fg`/`cterm_bg`. Fall back to
        // the rgb fields (quantised to the 256-colour cube) only if
        // cterm wasn't set — that covers the transient period between
        // `ui_attach` and our subsequent `set_option` taking effect,
        // and any future code paths that opt out of the toggle.
        let mut style = Style::DEFAULT;
        let fg = cterm_default_color(cterm_fg).or_else(|| quantize_rgb_optional(rgb_fg));
        let bg = cterm_default_color(cterm_bg).or_else(|| quantize_rgb_optional(rgb_bg));
        if let Some(c) = fg {
            style = style.with_fg(c);
        }
        if let Some(c) = bg {
            style = style.with_bg(c);
        }
        self.default_style = style;
        if self.hl_attrs.is_empty() {
            self.hl_attrs.push(style);
        } else {
            self.hl_attrs[0] = style;
        }
        Ok(())
    }

    fn apply_hl_attr_define(
        &mut self,
        params_len: usize,
        r: &mut Reader<'_>,
    ) -> Result<(), Error> {
        let id = r.read_u64()?;
        let rgb = read_rgb_attr_map(r)?;
        let cterm = read_attr_map(r)?;
        for _ in 3..params_len {
            r.skip()?;
        }
        // cterm is authoritative because the embed pins
        // `termguicolors=false` at attach time — that makes nvim
        // resolve the active colorscheme against the 256-colour
        // palette itself, so the cterm dict reflects the user's
        // actual theme (not stale built-in defaults). Only consult
        // the rgb side when cterm is empty — e.g. the transient
        // window before our set_option takes effect, or a legacy
        // highlight that only set gui attributes.
        //
        // Absent colours stay as None so `style_for` can inherit them
        // from `default_style` if it arrives later.
        let source = if cterm.fg.is_some() || cterm.bg.is_some() || cterm.modifiers != Style::DEFAULT {
            cterm
        } else {
            rgb
        };
        let mut style = source.modifiers;
        if let Some(c) = source.fg {
            style = style.with_fg(c);
        }
        if let Some(c) = source.bg {
            style = style.with_bg(c);
        }
        self.put_hl_attr(id, style);
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

    /// Renders the grid into `rect` of `buf`, clipping to `rect`'s dimensions.
    ///
    /// A scratch string is used to coalesce contiguous cells sharing the
    /// same [`Style`] into a single [`DoubleBuffer::set_stringn`] call.
    pub fn render(&self, rect: Rect, buf: &mut DoubleBuffer) {
        let mut run = String::new();
        let height = self.height.min(rect.h);
        let width = self.width.min(rect.w);
        for row in 0..height {
            let base = row as usize * self.width as usize;
            let mut col: u16 = 0;
            while col < width {
                let lead_idx = base + col as usize;
                let lead = self.cells[lead_idx];
                if lead.len == 0 {
                    col += 1;
                    continue;
                }
                let style = lead.style;
                let run_start = col;
                run.clear();
                run.push_str(lead.text());
                col += 1;
                while col < width {
                    let cell = self.cells[base + col as usize];
                    if cell.len == 0 {
                        col += 1;
                        continue;
                    }
                    if cell.style != style {
                        break;
                    }
                    run.push_str(cell.text());
                    col += 1;
                }
                let run_cols = (col - run_start) as usize;
                buf.set_stringn(
                    rect.x + run_start,
                    rect.y + row,
                    &run,
                    run_cols,
                    style,
                );
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

/// Reads an `hl_attr_define` rgb-attr dict, quantizing any 24-bit colors
/// to the 6×6×6 256-color cube. Modifier bits are collected identically
/// to [`read_attr_map`]; the caller picks whichever source it prefers.
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
            Ok(quantize_rgb_optional(v))
        }
        _ => {
            r.skip()?;
            Ok(None)
        }
    }
}

fn quantize_rgb_optional(v: i64) -> Option<Color> {
    if (0..=0xff_ff_ff).contains(&v) {
        Some(quantize_rgb(v as u32))
    } else {
        None
    }
}

/// Maps a 24-bit RGB triple to a 256-color palette index.
///
/// Uses xterm's exact cube/grayscale tables (not a naive `v*6/256`) so
/// greys like `#767676` round to the grayscale ramp entry the user
/// actually sees in a 256-color xterm, and colours snap to their
/// intended cube slot. Mirrors how terminals already interpret the
/// indices emitted by the cube/ramp path in `vim_rgb2cterm` and
/// matches the tables in `hl_cterm2rgb_color` so a round-trip through
/// quantization stays stable.
fn quantize_rgb(rgb: u32) -> Color {
    const CUBE_LEVELS: [i32; 6] = [0x00, 0x5f, 0x87, 0xaf, 0xd7, 0xff];
    const GREY_LEVELS: [i32; 24] = [
        0x08, 0x12, 0x1c, 0x26, 0x30, 0x3a, 0x44, 0x4e,
        0x58, 0x62, 0x6c, 0x76, 0x80, 0x8a, 0x94, 0x9e,
        0xa8, 0xb2, 0xbc, 0xc6, 0xd0, 0xda, 0xe4, 0xee,
    ];

    fn closest(value: i32, table: &[i32]) -> usize {
        let mut best = 0;
        let mut best_d = i32::MAX;
        for (i, &v) in table.iter().enumerate() {
            let d = (value - v).abs();
            if d < best_d {
                best_d = d;
                best = i;
            }
        }
        best
    }

    let r = ((rgb >> 16) & 0xff) as i32;
    let g = ((rgb >> 8) & 0xff) as i32;
    let b = (rgb & 0xff) as i32;

    let ci_r = closest(r, &CUBE_LEVELS);
    let ci_g = closest(g, &CUBE_LEVELS);
    let ci_b = closest(b, &CUBE_LEVELS);
    let cube_idx = 16 + ci_r * 36 + ci_g * 6 + ci_b;
    let cube_r = CUBE_LEVELS[ci_r];
    let cube_g = CUBE_LEVELS[ci_g];
    let cube_b = CUBE_LEVELS[ci_b];
    let cube_dist = (r - cube_r).pow(2) + (g - cube_g).pow(2) + (b - cube_b).pow(2);

    // The grayscale ramp is only a candidate when the color is close to
    // neutral; picking the nearest ramp cell for saturated colors would
    // wash them out.
    let avg = (r + g + b) / 3;
    let gi = closest(avg, &GREY_LEVELS);
    let grey_level = GREY_LEVELS[gi];
    let grey_dist = (r - grey_level).pow(2) + (g - grey_level).pow(2) + (b - grey_level).pow(2);

    if grey_dist < cube_dist {
        Color((232 + gi) as u8)
    } else {
        Color(cube_idx as u8)
    }
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
                Ok(Some(Color(v as u8)))
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
/// terminal default". Any positive value `v` maps to `Color(v - 1)`.
fn cterm_default_color(v: i64) -> Option<Color> {
    if (1..=256).contains(&v) {
        Some(Color((v - 1) as u8))
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
        let cells: Vec<&str> = (0..5).map(|c| g.cells[c].text()).collect();
        assert_eq!(cells, vec!["a", "a", "a", "b", "c"]);
    }

    #[test]
    fn grid_scroll_up() {
        let mut g = GridState::new(4, 3);
        for row in 0..3 {
            for col in 0..4 {
                g.cells[row * 4 + col] = GridCell::from_str(
                    match row {
                        0 => "A",
                        1 => "B",
                        _ => "C",
                    },
                    Style::DEFAULT,
                );
            }
        }
        g.scroll_region(0, 3, 0, 4, 1);
        let row0: String = (0..4).map(|c| g.cells[c].text()).collect();
        let row1: String = (0..4).map(|c| g.cells[4 + c].text()).collect();
        assert_eq!(row0, "BBBB");
        assert_eq!(row1, "CCCC");
    }

    #[test]
    fn grid_scroll_down() {
        let mut g = GridState::new(4, 3);
        for row in 0..3 {
            for col in 0..4 {
                g.cells[row * 4 + col] = GridCell::from_str(
                    match row {
                        0 => "A",
                        1 => "B",
                        _ => "C",
                    },
                    Style::DEFAULT,
                );
            }
        }
        g.scroll_region(0, 3, 0, 4, -1);
        let row1: String = (0..4).map(|c| g.cells[4 + c].text()).collect();
        let row2: String = (0..4).map(|c| g.cells[8 + c].text()).collect();
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
        assert_eq!(g.default_style.fg(), Some(Color(15)));
        assert_eq!(g.default_style.bg(), Some(Color(0)));
        let hl = g.style_for(1);
        assert_eq!(hl.fg(), Some(Color(196)));
        assert_eq!(hl.bg(), Some(Color(0)));
        assert!(hl.modifiers().has(Modifier::BOLD));
        assert!(hl.modifiers().has(Modifier::REVERSED));
    }

    #[test]
    fn hl_attr_define_prefers_cterm_over_rgb() {
        // With `termguicolors=false` pinned at attach time, nvim
        // quantises the active colorscheme to cterm indices itself, so
        // the cterm dict is the source of truth. The parallel rgb
        // dict must be ignored when cterm has content.
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
        let hl = g.style_for(7);
        assert_eq!(hl.fg(), Some(Color(67)));
        assert_eq!(hl.bg(), Some(Color(234)));
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
        let hl = g.style_for(3);
        assert_eq!(hl.fg(), Some(Color(9)));
        assert!(hl.modifiers().has(Modifier::BOLD));
    }

    #[test]
    fn hl_attr_define_resolves_reverse_from_cterm_side() {
        // Under `termguicolors=false` the default `StatusLine` ships
        // as `cterm={reverse=true}` with no explicit cterm fg/bg. The
        // embed should surface the reverse modifier and inherit the
        // colours from `default_style` via `style_for`.
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
        let hl = g.style_for(4);
        assert!(hl.modifiers().has(Modifier::REVERSED));
    }

    #[test]
    fn quantize_rgb_snaps_to_cube_and_ramp() {
        // Pure black / white sit in the cube corners.
        assert_eq!(quantize_rgb(0x00_00_00), Color(16));
        assert_eq!(quantize_rgb(0xff_ff_ff), Color(231));
        // Known xterm cube anchor: (0x5f, 0x87, 0xaf) → cube (1,2,3).
        assert_eq!(quantize_rgb(0x5f_87_af), Color(16 + 36 + 12 + 3));
        // Neutral greys prefer the 24-step ramp over the cube.
        assert_eq!(quantize_rgb(0x1c_1c_1c), Color(234));
        assert_eq!(quantize_rgb(0xc6_c6_c6), Color(251));
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
        assert_eq!(g.cells[0].text(), "日");
        assert_eq!(g.cells[1].len, 0);
        assert_eq!(g.cells[2].text(), "x");
    }
}
