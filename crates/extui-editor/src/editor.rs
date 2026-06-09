use std::borrow::Cow;
use std::collections::HashMap;

use extui::{Buffer, Rect, event::KeyEvent};
use extui_bindings::{InputKey, LayerId, Payload};
use unicode_segmentation::UnicodeSegmentation;

use crate::{
    bindings::{
        self, CaseTransform, EditorAction as Action, EditorBindings, EditorContext, EditorRouter,
        Motion, PairKind, TextObject, action_needs_capture, layer_to_operator_state, mode_bit,
    },
    buffer::{
        self, Edit, Replacement, Span, TextBuffer, TrackedChange, padded_chunks, split_chunks,
    },
    cursor::{self, Cursor, MotionKind},
    history::History,
    mode::Mode,
    render::{self, StyleRun},
    theme::EditorTheme,
    visual::{Selection, VisualKind},
};

/// The yank register (single unnamed register in v1).
#[derive(Clone, Debug, Default)]
pub(crate) struct Yank {
    pub lines: Vec<String>,
    pub kind: YankKind,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) enum YankKind {
    #[default]
    Charwise,
    Linewise,
    Blockwise,
}

/// Which operator a pending / visual-mode range is being consumed by.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Operator {
    Delete,
    Change,
    Yank,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PendingBlockChange {
    row_start: usize,
    row_end: usize,
    col: usize,
}

/// Byte-offset keyed map of named marks (`m{letter}`). Keys are
/// lowercase ASCII letters; `HashMap` is small enough that the
/// per-edit projection pass is cheaper than any alternative layout.
type Marks = HashMap<char, u32>;

/// Tab-handling options for the editor.
///
/// Mirrors the four vim options that govern tab behavior.
/// [`Editor::set_tab_settings`] installs a new configuration;
/// [`Editor::tab_settings`] reads the current one.
///
/// The defaults ([`Self::default`]) insert four spaces for a `Tab`
/// keypress and render hard tabs already in the buffer at a width of
/// 4.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TabSettings {
    /// Expand typed `Tab` keys into spaces instead of inserting `\t`.
    pub expandtab: bool,
    /// Display width used when rendering hard tabs already in the
    /// buffer.
    pub tabstop: u16,
    /// Width inserted by a `Tab` keypress when [`Self::expandtab`] is
    /// `true`. `0` falls back to [`Self::tabstop`].
    pub softtabstop: u16,
    /// Indent width for indentation-aware commands.
    pub shiftwidth: u16,
}

impl Default for TabSettings {
    fn default() -> Self {
        Self {
            expandtab: true,
            tabstop: 4,
            softtabstop: 4,
            shiftwidth: 4,
        }
    }
}

impl TabSettings {
    pub(crate) fn normalized(self) -> Self {
        Self {
            expandtab: self.expandtab,
            tabstop: self.tabstop.max(1),
            softtabstop: self.softtabstop,
            shiftwidth: self.shiftwidth.max(1),
        }
    }

    pub(crate) fn effective_softtabstop(self) -> u16 {
        let settings = self.normalized();
        if settings.softtabstop == 0 {
            settings.tabstop
        } else {
            settings.softtabstop
        }
    }

    pub(crate) fn tab_input_text(self) -> Cow<'static, str> {
        let settings = self.normalized();
        if !settings.expandtab {
            return Cow::Borrowed("\t");
        }
        Cow::Owned(" ".repeat(settings.effective_softtabstop() as usize))
    }

    pub(crate) fn expandtab_width(self, display_col: u16) -> u16 {
        let stop = self.effective_softtabstop().max(1);
        let rem = display_col % stop;
        if rem == 0 { stop } else { stop - rem }
    }
}

/// Embeddable text editor widget.
///
/// Owns the buffer, cursor, mode state, undo history, and a
/// configurable key-binding router. The host owns the terminal and
/// event loop; each frame:
///
/// 1. Forward key events via [`Self::send_key`].
/// 2. Query [`Self::desired_height`] to size the widget inline.
/// 3. Paint via [`Self::render`] (or [`Self::render_with_styles`] to
///    layer syntax highlighting).
///
/// [`Self::new`] installs the built-in Vim preset. Use
/// [`Self::with_bindings`] for a custom [`EditorBindings`] graph.
///
/// # Examples
///
/// ```no_run
/// use extui::{Buffer, Rect};
/// use extui_editor::Editor;
///
/// let mut editor = Editor::new();
/// editor.set_height_bounds(2, 10);
/// editor.resize(80);
/// editor.set_lines("fn main() {\n    println!(\"hi\");\n}");
///
/// let mut buf = Buffer::new(80, 24);
/// editor.render(Rect { x: 0, y: 0, w: 80, h: editor.desired_height() }, &mut buf);
/// ```
///
/// [`EditorBindings`]: crate::bindings::EditorBindings
pub struct Editor {
    pub(crate) buf: TextBuffer,
    pub(crate) cursor: Cursor,
    pub(crate) mode: Mode,
    primary_mode: Mode,
    pub(crate) selection: Option<Selection>,
    pub(crate) yank: Yank,
    pub(crate) history: History,
    pub(crate) width: u16,
    pub(crate) wrap: bool,
    pub(crate) scroll_offset: u16,
    pub(crate) horizontal_scroll: u16,
    /// Height (in cell rows) of the most recent viewport passed to
    /// [`Self::render`]. Used by half-page motions (`<C-d>`/`<C-u>`)
    /// that need a notion of "a page" but don't themselves know the
    /// widget's drawn size. Zero before the first render.
    pub(crate) last_viewport_h: u16,
    pub(crate) min_height: u16,
    pub(crate) max_height: u16,
    pub(crate) single_line: bool,
    pub(crate) dirty: bool,
    pub(crate) text_version: u64,
    /// Remembered display column for vertical motions (vim's `curswant`).
    /// `u16::MAX` means "always go to EOL" (after a `$`).
    pub(crate) desired_display_col: u16,
    pub(crate) theme: EditorTheme,
    pub(crate) tab_settings: TabSettings,
    /// Accumulated per-edit replacements, in the coordinates of the
    /// buffer immediately before each one was applied. Populated only
    /// while [`Self::track_replacements`] is true.
    replacements: Vec<Replacement>,
    track_replacements: bool,
    /// Next drain must signal [`TrackedChange::Reset`] — either because
    /// tracking was just enabled, or because `set_lines`/`clear` wiped
    /// the buffer while tracking was on.
    replacements_reset_pending: bool,
    marks: Marks,
    pending_block_change: Option<PendingBlockChange>,
    router: EditorRouter,
    ctx: EditorContext,
    current_layer: LayerId,
    pending_capture: Option<Action>,
    pending_count: Option<u32>,
}

impl Default for Editor {
    fn default() -> Self {
        Self::new()
    }
}

impl Editor {
    /// Creates an editor with the built-in Vim preset and default
    /// options.
    ///
    /// Equivalent to `Editor::with_bindings(bindings::vim(VimOptions::default()))`.
    pub fn new() -> Self {
        Self::with_bindings(bindings::vim(bindings::VimOptions::default()))
    }

    /// Creates an editor backed by a custom binding graph.
    ///
    /// Use this to install the [`nano`] or [`emacs`] presets, or a
    /// hand-built [`EditorBindings`]. The primary mode carried by
    /// `bindings` becomes the editor's resting mode (the one Insert
    /// returns to on `Esc` for Vim-style bindings, or the only mode
    /// for modeless presets).
    ///
    /// [`nano`]: crate::bindings::nano
    /// [`emacs`]: crate::bindings::emacs
    /// [`EditorBindings`]: crate::bindings::EditorBindings
    pub fn with_bindings(bindings: EditorBindings) -> Self {
        let (router, primary_mode) = bindings.into_parts();
        let ctx = EditorContext {
            mode: mode_bit(primary_mode),
        };
        Self {
            buf: TextBuffer::new(),
            cursor: Cursor::ORIGIN,
            mode: primary_mode,
            primary_mode,
            selection: None,
            yank: Yank::default(),
            history: History::new(),
            width: 0,
            wrap: false,
            scroll_offset: 0,
            horizontal_scroll: 0,
            last_viewport_h: 0,
            min_height: 1,
            max_height: u16::MAX,
            single_line: false,
            dirty: true,
            text_version: 0,
            desired_display_col: 0,
            theme: EditorTheme::default(),
            tab_settings: TabSettings::default(),
            replacements: Vec::new(),
            track_replacements: false,
            replacements_reset_pending: false,
            marks: Marks::new(),
            pending_block_change: None,
            router,
            ctx,
            current_layer: LayerId::BASE,
            pending_capture: None,
            pending_count: None,
        }
    }

    fn set_mode_ctx(&mut self, mode: Mode) {
        self.mode = mode;
        self.ctx.mode = mode_bit(mode);
        self.current_layer = LayerId::BASE;
        self.pending_capture = None;
    }

    fn reset_to_primary_mode(&mut self) {
        self.set_mode_ctx(self.primary_mode);
    }

    /// Installs `theme`. No-op when the theme is unchanged.
    pub fn set_theme(&mut self, theme: EditorTheme) {
        if self.theme == theme {
            return;
        }
        self.theme = theme;
        self.dirty = true;
    }

    /// Returns the active theme.
    pub fn theme(&self) -> &EditorTheme {
        &self.theme
    }

    /// Returns the current tab-handling configuration.
    pub fn tab_settings(&self) -> TabSettings {
        self.tab_settings
    }

    /// Installs `settings`. Values are normalized (zero tab stops clamp
    /// to 1); a no-op when the normalized form is unchanged.
    pub fn set_tab_settings(&mut self, settings: TabSettings) {
        let settings = settings.normalized();
        if self.tab_settings == settings {
            return;
        }
        self.tab_settings = settings;
        self.update_desired_display_col();
        self.dirty = true;
    }

    /// Sets the inclusive range for [`Self::desired_height`].
    ///
    /// `min` is clamped to at least 1; `max` is clamped to at least
    /// `min`. Content that exceeds `max` is reached by internal
    /// scrolling.
    pub fn set_height_bounds(&mut self, min: u16, max: u16) {
        let min = min.max(1);
        let max = max.max(min);
        self.min_height = min;
        self.max_height = max;
        self.dirty = true;
    }

    /// Locks the buffer to a single line.
    ///
    /// While enabled, existing CR/LF are stripped, `Enter` / `o` / `O`
    /// stop creating new rows, [`Self::desired_height`] returns 1, and
    /// vertical scrolling is disabled. Suited for inline input fields.
    pub fn set_single_line(&mut self, enabled: bool) {
        if self.single_line == enabled {
            return;
        }
        self.single_line = enabled;
        if enabled {
            self.enforce_single_line_mode();
        }
        self.scroll_offset = 0;
        self.horizontal_scroll = 0;
        self.dirty = true;
    }

    /// Returns `true` when soft-wrapping is enabled.
    pub fn wrap(&self) -> bool {
        self.wrap
    }

    /// Toggles soft-wrapping of long lines.
    ///
    /// When enabled, long lines flow onto additional visual rows and
    /// vertical scrolling tracks those rows. When disabled (the
    /// default), long lines stay on one row and the viewport shifts
    /// horizontally to keep the cursor visible.
    ///
    /// Forced off while [`Self::set_single_line`] is enabled.
    pub fn set_wrap(&mut self, enabled: bool) {
        if self.wrap == enabled {
            return;
        }
        self.wrap = enabled;
        if enabled {
            self.horizontal_scroll = 0;
        }
        self.dirty = true;
    }

    /// Notifies the editor of its available width in cells.
    ///
    /// Affects wrapping, horizontal scrolling, and the value returned
    /// by [`Self::desired_height`] in wrap mode.
    pub fn resize(&mut self, width: u16) {
        self.width = width;
        self.dirty = true;
    }

    /// Returns the height in rows the widget would like to occupy.
    ///
    /// The result sits within the range configured by
    /// [`Self::set_height_bounds`]. Always `1` when
    /// [`Self::set_single_line`] is enabled.
    pub fn desired_height(&self) -> u16 {
        if self.single_line {
            return 1;
        }
        let lines = if self.effective_wrap() && self.width > 0 {
            let mut total = 0u32;
            for row in 0..self.buf.line_count().max(1) {
                total += render::wrapped_line_rows(
                    self.buf.line(row).as_ref(),
                    self.width,
                    self.tab_settings.tabstop,
                ) as u32;
            }
            total.max(1)
        } else {
            self.buf.line_count().max(1) as u32
        };
        lines.clamp(self.min_height as u32, self.max_height as u32) as u16
    }

    /// Returns `true` when any state that would affect rendering has
    /// changed since the last call. Consume-on-read: the dirty flag is
    /// cleared on every call.
    pub fn poll_updates(&mut self) -> bool {
        std::mem::replace(&mut self.dirty, false)
    }

    /// Returns the current editing [`Mode`].
    pub fn mode(&self) -> Mode {
        self.mode
    }

    /// Returns the cursor as `(row, byte_col)`.
    ///
    /// `byte_col` is a 0-based byte offset on a grapheme-cluster
    /// boundary. Matches what `nvim_win_get_cursor` reports.
    pub fn cursor(&self) -> (usize, usize) {
        (self.cursor.row, self.cursor.col)
    }

    /// Returns the cursor as `(row, display_col)`, where `display_col`
    /// is the terminal-cell column after tab expansion and grapheme
    /// widths.
    ///
    /// Use this when comparing cursor position against terminal grid
    /// coordinates; [`Self::cursor`] is the right choice for edits and
    /// buffer-space comparisons.
    pub fn cursor_display(&self) -> (usize, u16) {
        let line = self.buf.line(self.cursor.row);
        (
            self.cursor.row,
            render::cursor_display_col(
                line.as_ref(),
                self.cursor.col,
                self.mode,
                self.tab_settings.tabstop,
            ),
        )
    }

    /// Returns the buffer as two contiguous byte slices.
    ///
    /// Either half may be empty; concatenated they yield the full
    /// buffer. Avoids the allocation that [`Self::text`] performs.
    pub fn text_slices(&self) -> (&str, &str) {
        self.buf.as_slices()
    }

    /// Returns the total byte length of the buffer.
    pub fn text_len(&self) -> u32 {
        self.buf.len() as u32
    }

    /// Returns the entire buffer as an owned [`String`].
    pub fn text(&self) -> String {
        self.buf.text()
    }

    /// Walks chunks covering `core` padded by `query_len - 1` bytes
    /// on each side, guaranteeing that every substring of length up to
    /// `query_len` touching `core` appears contiguously in exactly
    /// one chunk.
    ///
    /// The visitor receives `(chunk, base, max_start)`:
    ///
    /// - `base` is the chunk's byte offset in the full buffer.
    /// - `max_start` is an exclusive upper bound on the match-start
    ///   positions this chunk is responsible for. Emitting only
    ///   matches with `start < max_start` prevents duplicates at the
    ///   internal gap bridge.
    ///
    /// Returns the effective window after padding and UTF-8 boundary
    /// snapping. The bridge across the internal gap allocates a
    /// [`String`] at most `2 * (query_len - 1) + 6` bytes long, and
    /// only when the padded window straddles the gap.
    ///
    /// Suited for fixed-length substring search. For token-shaped
    /// workloads (spellcheck, newline-bounded regex) use
    /// [`Self::for_each_split_chunk`].
    pub fn for_each_padded_chunk<F>(&self, core: Span, query_len: u32, visit: F) -> Span
    where
        F: FnMut(&str, u32, u32),
    {
        padded_chunks(self.text_slices(), core, query_len, visit)
    }

    /// Walks chunks covering `core` expanded outward until each
    /// endpoint sits on a byte satisfying `is_boundary` or at a buffer
    /// edge.
    ///
    /// Every emitted chunk is self-contained under `is_boundary`:
    /// consumers that tokenize on the same predicate can process
    /// chunks independently without cross-chunk duplicates. The chunk
    /// straddling the internal gap (when one exists) is allocated as
    /// a single [`String`]; every other chunk is borrowed.
    ///
    /// Returns the effective window after expansion. Consumers
    /// tracking overlay state key their drop-zone or splice point on
    /// this range.
    pub fn for_each_split_chunk<F, P>(&self, core: Span, is_boundary: P, visit: F) -> Span
    where
        F: FnMut(&str, u32),
        P: Fn(char) -> bool,
    {
        split_chunks(self.text_slices(), core, is_boundary, visit)
    }

    /// Shorthand for [`Self::for_each_split_chunk`] with a newline
    /// predicate.
    ///
    /// Use for overlays that scan line-by-line, such as regex search
    /// where patterns never cross a newline.
    pub fn for_each_line_chunk<F>(&self, core: Span, visit: F) -> Span
    where
        F: FnMut(&str, u32),
    {
        self.for_each_split_chunk(core, |c| c == '\n', visit)
    }

    /// Returns a monotonic counter that increments on every buffer
    /// mutation.
    ///
    /// Cursor movement, mode transitions, scrolling, and resizing do
    /// not bump the counter. Use it to key caches of expensive
    /// buffer-derived computations (spellcheck, search match lists)
    /// and recompute only when the value changes.
    pub fn text_version(&self) -> u64 {
        self.text_version
    }

    /// Enables or disables per-edit replacement tracking.
    ///
    /// While enabled, every applied edit is recorded as a
    /// [`Replacement`] in pre-edit coordinates. Drain the accumulated
    /// delta each frame with [`Self::take_tracked_change`].
    ///
    /// Idempotent: repeated calls with the same value do nothing. A
    /// false-to-true transition primes tracking so the next drain
    /// reports [`TrackedChange::Reset`], signaling overlays to build
    /// their state from scratch.
    pub fn set_track_replacements(&mut self, enabled: bool) {
        if self.track_replacements == enabled {
            return;
        }
        self.track_replacements = enabled;
        self.replacements.clear();
        self.replacements_reset_pending = enabled;
    }

    /// Returns `true` when replacement tracking is enabled.
    pub fn is_tracking_replacements(&self) -> bool {
        self.track_replacements
    }

    /// Drains the accumulated tracked changes since the last call.
    ///
    /// Tracking stays on across drains. See [`TrackedChange`] for the
    /// three possible outcomes.
    pub fn take_tracked_change(&mut self) -> TrackedChange {
        if self.replacements_reset_pending {
            self.replacements_reset_pending = false;
            self.replacements.clear();
            return TrackedChange::Reset;
        }
        let mut it = self.replacements.drain(..);
        let Some(first) = it.next() else {
            return TrackedChange::None;
        };
        let merged = it.fold(first, Replacement::merge);
        TrackedChange::Merged(merged)
    }

    /// Returns the cursor position as a byte offset into the buffer.
    ///
    /// Pairs with [`Self::set_cursor_offset`] for features that track
    /// buffer positions in flat-offset form, such as search matches.
    pub fn cursor_offset(&self) -> u32 {
        self.buf.rowcol_to_offset(self.cursor.row, self.cursor.col)
    }

    /// Moves the cursor to `offset` bytes into the buffer.
    ///
    /// The offset is clamped to the buffer length and aligned to the
    /// nearest valid grapheme start for the current mode. Resets the
    /// vertical-motion `curswant` so a subsequent `j` or `k` lands on
    /// this column. The viewport is re-centered on the cursor by the
    /// next [`Self::render`] call.
    pub fn set_cursor_offset(&mut self, offset: u32) {
        let (row, col) = self.buf.offset_to_rowcol(offset);
        self.cursor = Cursor { row, col };
        self.fixup_cursor();
        self.update_desired_display_col();
        if self.mode.is_visual() {
            self.update_selection_head();
        }
        self.dirty = true;
    }

    /// Replaces the bytes in `span` with `text`, leaving the cursor at the
    /// end of the inserted text.
    ///
    /// `span` is interpreted in current buffer coordinates and clamped to the
    /// buffer length. The edit records an undo checkpoint so it reverts as a
    /// single unit, and (with tracking enabled) surfaces through
    /// [`Self::take_tracked_change`] like any other edit. In single-line mode
    /// line breaks in `text` are stripped.
    ///
    /// Intended for host-driven edits outside the key-binding flow, such as
    /// spell correction or snippet expansion.
    pub fn replace_range(&mut self, span: Span, text: &str) {
        let len = self.buf.len() as u32;
        let offset = span.offset.min(len);
        let old_len = span.len.min(len - offset);
        let new = self.normalize_text_for_mode(text).into_owned();
        let new_end = offset + new.len() as u32;
        self.checkpoint();
        self.commit(Edit::replace(offset, old_len, new));
        let (row, col) = self.buf.offset_to_rowcol(new_end);
        self.cursor = Cursor { row, col };
        self.fixup_cursor();
        self.update_desired_display_col();
        self.dirty = true;
    }

    /// Maps a buffer byte range to screen rectangles inside `rect`.
    ///
    /// The produced rectangles match the layout used by
    /// [`Self::render`] (scroll offset, tab expansion, grapheme
    /// widths). One rectangle is emitted per visible line segment the
    /// range touches, each of height 1. Segments scrolled off screen
    /// yield nothing.
    ///
    /// Use for post-render overlays such as search highlights or
    /// spellcheck underlines, applied via [`Buffer::set_style`]
    /// so the existing text and syntax colors are preserved.
    ///
    /// [`Buffer::set_style`]: extui::Buffer::set_style
    pub fn visible_range_rects(
        &self,
        rect: Rect,
        start: u32,
        end: u32,
    ) -> impl Iterator<Item = Rect> + '_ {
        render::visible_range_rects(
            rect,
            &self.buf,
            self.scroll_offset,
            self.horizontal_scroll,
            self.effective_wrap(),
            start,
            end,
            self.tab_settings.tabstop,
        )
    }

    /// Returns the cursor's screen position inside `rect`, or [`None`]
    /// when the cursor is outside the visible viewport.
    ///
    /// The returned coordinates are absolute screen cells, ready to
    /// hand to [`Buffer::set_cursor`].
    ///
    /// [`Buffer::set_cursor`]: extui::Buffer::set_cursor
    pub fn cursor_position(&self, rect: Rect) -> Option<(u16, u16)> {
        render::cursor_position(
            rect,
            &self.buf,
            self.tab_settings.tabstop,
            self.cursor,
            self.mode,
            self.scroll_offset,
            self.horizontal_scroll,
            self.effective_wrap(),
            None,
        )
    }

    /// Replaces the entire buffer with `s`.
    ///
    /// Resets the cursor to the origin, clears the selection and
    /// undo history, and bumps [`Self::text_version`]. In single-line
    /// mode, CR/LF are stripped from `s`.
    pub fn set_lines(&mut self, s: &str) {
        let text = self.normalize_text_for_mode(s);
        self.buf.set_text(text.as_ref());
        self.reset_buffer_state();
    }

    /// Clears the buffer to a single empty line.
    ///
    /// Same reset semantics as [`Self::set_lines`] with an empty
    /// string.
    pub fn clear(&mut self) {
        self.buf.clear();
        self.reset_buffer_state();
    }

    /// Shared tail of `set_lines` / `clear` — everything that resets
    /// after a wholesale buffer replacement.
    fn reset_buffer_state(&mut self) {
        self.cursor = Cursor::ORIGIN;
        self.reset_to_primary_mode();
        self.selection = None;
        self.pending_block_change = None;
        self.scroll_offset = 0;
        self.horizontal_scroll = 0;
        self.desired_display_col = 0;
        self.history.reset();
        self.marks.clear();
        self.flag_replacements_reset();
        self.dirty = true;
        self.text_version = self.text_version.wrapping_add(1);
    }

    fn flag_replacements_reset(&mut self) {
        if self.track_replacements {
            self.replacements.clear();
            self.replacements_reset_pending = true;
        }
    }

    /// Dispatches `key` through the binding router.
    ///
    /// Returns `true` when the key was consumed (either by firing a
    /// binding, inserting a character in Insert mode, or absorbing a
    /// count prefix). Returns `false` when the key has no meaning for
    /// the current mode and layer, letting the host fall back to its
    /// own handling.
    pub fn send_key(&mut self, key: &KeyEvent) -> bool {
        let Some(input) = InputKey::from_event(key) else {
            return false;
        };

        // A capture-needing action stashed on the previous key consumes the
        // next keystroke as its `char` argument, regardless of layer.
        if let Some(pending) = self.pending_capture.take() {
            let ch = input.as_char();
            self.dispatch(pending, ch);
            return true;
        }

        if self.try_absorb_count(input) {
            self.dirty = true;
            return true;
        }

        let Some(entry) = self.router.lookup(&self.ctx, self.current_layer, input) else {
            // Unhandled key while operator-pending cancels the operator.
            if layer_to_operator_state(self.current_layer).is_some() {
                self.current_layer = LayerId::BASE;
                self.pending_count = None;
                self.dirty = true;
                return true;
            }
            self.current_layer = LayerId::BASE;
            if self.mode == Mode::Insert {
                if let Some(c) = input.as_char() {
                    self.exec_insert_char(c);
                    return true;
                }
            }
            self.pending_count = None;
            return false;
        };

        match entry.payload() {
            Payload::Action(id) => {
                let action = self.router.action(id);
                self.current_layer = LayerId::BASE;
                if action_needs_capture(action) {
                    self.pending_capture = Some(action);
                    self.dirty = true;
                } else {
                    self.dispatch(action, None);
                }
            }
            Payload::Layer(target) => {
                self.current_layer = target;
                self.dirty = true;
            }
        }
        true
    }

    /// Consumes the key as a count-prefix digit when legal. Counts accumulate
    /// at [`LayerId::BASE`] and also at operator-pending layers, so both
    /// `3dw` and `d3w` work.
    fn try_absorb_count(&mut self, input: InputKey) -> bool {
        let at_base = self.current_layer == LayerId::BASE;
        let at_operator = layer_to_operator_state(self.current_layer).is_some();
        if !(at_base || at_operator) {
            return false;
        }
        if self.mode != Mode::Normal {
            return false;
        }
        let Some(d) = input.as_bare_digit() else {
            return false;
        };
        if d == 0 && self.pending_count.is_none() {
            // Bare `0` is the LineStart motion, not a count.
            return false;
        }
        let current = self.pending_count.unwrap_or(0);
        let next = current.saturating_mul(10).saturating_add(d as u32);
        self.pending_count = Some(next);
        true
    }

    fn take_count(&mut self) -> u32 {
        self.pending_count.take().unwrap_or(1).max(1)
    }

    fn dispatch(&mut self, action: Action, captured: Option<char>) {
        let count = self.take_count();
        self.execute(action, count, captured);
    }

    /// Paints the editor into `rect` on `buf` with no syntax
    /// styling.
    ///
    /// Shorthand for [`Self::render_with_styles`] with an empty run
    /// slice.
    pub fn render(&mut self, rect: Rect, buf: &mut Buffer) {
        self.render_with_styles(rect, buf, &[]);
    }

    /// Paints the editor into `rect` on `buf`, applying the style
    /// runs in `runs` to their covered byte ranges.
    ///
    /// `runs` must be sorted by `offset` and non-overlapping. The
    /// renderer walks them in lock-step with the wrap and tab layout
    /// and uses a binary search to skip runs outside the visible
    /// viewport, so passing a full-file run list is cheap on large
    /// buffers.
    pub fn render_with_styles(&mut self, rect: Rect, buf: &mut Buffer, runs: &[StyleRun]) {
        let wrap = self.effective_wrap();
        self.last_viewport_h = rect.h;
        self.ensure_cursor_visible(rect);
        render::render(
            buf,
            rect,
            &self.buf,
            &self.theme,
            self.tab_settings.tabstop,
            self.cursor,
            self.mode,
            self.selection.as_ref(),
            self.scroll_offset,
            self.horizontal_scroll,
            wrap,
            runs,
        );
        self.dirty = false;
    }

    /// Borrows the underlying [`TextBuffer`].
    ///
    /// Intended for integrations that feed an external highlighter
    /// (`tinyhl`, tree-sitter) via [`TextBuffer::page`] and
    /// [`TextBuffer::len`].
    pub fn text_buffer(&self) -> &TextBuffer {
        &self.buf
    }

    /// Returns a conservative byte range covering the buffer currently
    /// visible in `rect`.
    ///
    /// Hosts building per-frame [`StyleRun`] slices should clip their
    /// queries to this span so work scales with viewport height
    /// rather than buffer size. The span is oversized in wrapped mode
    /// to absorb wrap blow-up without a full layout pass.
    pub fn visible_byte_span(&self, rect: Rect) -> Span {
        let line_count = self.buf.line_count();
        let top_row = (self.scroll_offset as usize).min(line_count);
        // Worst case: every visible visual row maps to one buffer row.
        // Double to absorb partial-line scroll + wrap expansion.
        let extra = (rect.h as usize).saturating_mul(2).max(1);
        let bottom_row = top_row.saturating_add(extra).min(line_count);
        let top_byte = self.buf.line_start(top_row);
        let bottom_byte = if bottom_row >= line_count {
            self.buf.len() as u32
        } else {
            self.buf.line_start(bottom_row)
        };
        Span::new(top_byte, bottom_byte.saturating_sub(top_byte))
    }

    fn ensure_cursor_visible(&mut self, rect: Rect) {
        if rect.is_empty() {
            return;
        }
        if self.effective_wrap() {
            self.horizontal_scroll = 0;
            let width = rect.w.max(1);
            let cursor_col = self.cursor_display().1 as u32;
            let mut visual_row = cursor_col / width as u32;
            let mut total_rows = 0u32;
            for row in 0..self.buf.line_count() {
                let rows = render::wrapped_line_rows(
                    self.buf.line(row).as_ref(),
                    width,
                    self.tab_settings.tabstop,
                ) as u32;
                if row < self.cursor.row {
                    visual_row += rows;
                }
                total_rows += rows;
            }
            let h = rect.h as u32;
            if visual_row < self.scroll_offset as u32 {
                self.scroll_offset = visual_row as u16;
            } else if visual_row >= self.scroll_offset as u32 + h {
                self.scroll_offset = (visual_row + 1 - h) as u16;
            }
            // Never leave blank rows below the content while rows are hidden
            // above. When the viewport has grown (or content shrunk) enough to
            // show more, pull the scroll back down to the last needed row.
            let max_scroll = total_rows.saturating_sub(h);
            if self.scroll_offset as u32 > max_scroll {
                self.scroll_offset = max_scroll as u16;
            }
            return;
        }
        if self.single_line {
            self.scroll_offset = 0;
        } else {
            let h = rect.h as usize;
            let row = self.cursor.row;
            let scroll = self.scroll_offset as usize;
            if row < scroll {
                self.scroll_offset = row as u16;
            } else if row >= scroll + h {
                self.scroll_offset = (row + 1 - h) as u16;
            }
            let max_scroll = self.buf.line_count().saturating_sub(h);
            if self.scroll_offset as usize > max_scroll {
                self.scroll_offset = max_scroll as u16;
            }
        }
        let cursor_col = self.cursor_display().1 as u32;
        let left = self.horizontal_scroll as u32;
        let width = rect.w as u32;
        if cursor_col < left {
            self.horizontal_scroll = cursor_col as u16;
        } else if cursor_col >= left + width {
            self.horizontal_scroll = cursor_col.saturating_add(1).saturating_sub(width) as u16;
        }
    }

    fn effective_wrap(&self) -> bool {
        self.wrap && !self.single_line
    }

    /// Returns half of the most-recent viewport height in cell rows,
    /// clamped to at least 1 so half-page motions always advance when a
    /// motion has been issued before the first render.
    fn half_page_rows(&self) -> u32 {
        (u32::from(self.last_viewport_h) / 2).max(1)
    }

    fn execute(&mut self, action: Action, count: u32, captured: Option<char>) {
        let n = count.max(1) as usize;
        match action {
            Action::EnterInsert => {
                self.checkpoint();
                self.enter_insert_at_cursor();
            }
            Action::EnterInsertAfter => {
                self.checkpoint();
                self.enter_insert_after();
            }
            Action::EnterInsertFirstNonblank => {
                self.checkpoint();
                self.enter_insert_at_first_nonblank();
            }
            Action::EnterInsertEol => {
                self.checkpoint();
                self.enter_insert_at_eol();
            }
            Action::OpenBelow => {
                self.checkpoint();
                self.open_line_below_and_insert();
            }
            Action::OpenAbove => {
                self.checkpoint();
                self.open_line_above_and_insert();
            }

            Action::EnterVisualChar => self.toggle_or_enter_visual(VisualKind::Char),
            Action::EnterVisualLine => self.toggle_or_enter_visual(VisualKind::Line),
            Action::EnterVisualBlock => self.toggle_or_enter_visual(VisualKind::Block),
            Action::ExitVisual => self.exit_visual(),

            Action::Motion(m) => self.exec_motion(m, count, captured),

            Action::DeleteChar => self.exec_x(n),
            Action::Substitute => self.exec_s(n),
            Action::SubstituteLine => self.exec_big_s(n),
            Action::JoinLines => self.exec_join_lines(n),
            Action::PasteAfter => self.exec_paste_after(n),
            Action::PasteBefore => self.exec_paste_before(n),
            Action::Undo => self.exec_undo(),
            Action::Redo => self.exec_redo(),
            Action::Replace => {
                if let Some(c) = captured {
                    self.exec_replace(c, n);
                }
            }

            Action::DeleteMotion(m) => {
                self.exec_operator_motion(Operator::Delete, m, count, captured)
            }
            Action::ChangeMotion(m) => {
                self.exec_operator_motion(Operator::Change, m, count, captured)
            }
            Action::YankMotion(m) => self.exec_operator_motion(Operator::Yank, m, count, captured),
            Action::DeleteTextObject(t) => {
                self.exec_operator_text_object(Operator::Delete, t, count)
            }
            Action::ChangeTextObject(t) => {
                self.exec_operator_text_object(Operator::Change, t, count)
            }
            Action::YankTextObject(t) => self.exec_operator_text_object(Operator::Yank, t, count),
            Action::DeleteLine => self.exec_operator_line(Operator::Delete, n),
            Action::ChangeLine => self.exec_operator_line(Operator::Change, n),
            Action::YankLine => self.exec_operator_line(Operator::Yank, n),

            Action::ToggleCaseChar => self.exec_toggle_case_char(n),
            Action::CaseMotion(t, m) => self.exec_case_motion(t, m, count, captured),
            Action::CaseTextObject(t, obj) => self.exec_case_text_object(t, obj, count),
            Action::CaseLine(t) => self.exec_case_line(t, n),
            Action::CaseSelection(t) => self.exec_case_selection(t),

            Action::IncrementNumber => self.exec_increment(n, 1),
            Action::DecrementNumber => self.exec_increment(n, -1),

            Action::SetMark => {
                if let Some(c) = captured {
                    self.exec_set_mark(c);
                }
            }

            Action::ExitInsert => self.exec_exit_insert(),
            Action::InsertChar => {
                if let Some(c) = captured {
                    self.exec_insert_char(c);
                }
            }
            Action::InsertNewline => self.exec_insert_newline(),
            Action::InsertTab => self.exec_insert_tab(),
            Action::BackspaceDelete => self.exec_backspace(),

            Action::DeleteSelection => self.apply_visual_operator(Operator::Delete),
            Action::ChangeSelection => self.apply_visual_operator(Operator::Change),
            Action::YankSelection => self.apply_visual_operator(Operator::Yank),
            Action::SurroundSelection => {
                if let Some(c) = captured {
                    self.exec_surround_selection(c);
                }
            }
            Action::SelectTextObject(obj) => self.exec_select_text_object(obj, count),

            Action::NoOp => {}
        }
        if self.single_line {
            self.scroll_offset = 0;
            self.fixup_cursor();
        }
    }

    fn exec_motion(&mut self, motion: Motion, count: u32, captured: Option<char>) {
        let n = count.max(1);
        // Visual modes use Insert-mode clamping so the selection head can
        // reach the line end.
        let motion_mode = if self.mode.is_visual() {
            Mode::Insert
        } else {
            self.mode
        };
        let Some((target, _kind)) =
            self.resolve_motion(motion, self.cursor, n, motion_mode, captured)
        else {
            self.dirty = true;
            return;
        };
        self.cursor = target;
        match motion {
            Motion::LineEnd => self.desired_display_col = u16::MAX,
            Motion::Up
            | Motion::Down
            | Motion::DisplayUp
            | Motion::DisplayDown
            | Motion::HalfPageDown
            | Motion::HalfPageUp => {}
            _ => self.update_desired_display_col(),
        }
        if self.mode.is_visual() {
            self.update_selection_head();
        }
        self.dirty = true;
    }

    fn exec_operator_motion(
        &mut self,
        op: Operator,
        motion: Motion,
        count: u32,
        captured: Option<char>,
    ) {
        let start = self.cursor;
        // Vim's "cw" special case: on a non-blank grapheme, behaves as "ce".
        let effective_motion = if op == Operator::Change && motion == Motion::WordForward {
            let line = self.buf.line(start.row);
            if !line.is_empty()
                && cursor::class_at(line.as_ref(), start.col) != cursor::Class::Whitespace
            {
                Motion::WordEnd
            } else {
                Motion::WordForward
            }
        } else {
            motion
        };

        let n = count.max(1);
        let Some((target, kind)) =
            self.resolve_motion(effective_motion, start, n, self.mode, captured)
        else {
            self.dirty = true;
            return;
        };

        // Vim's `w` with operator: if the motion crosses a line boundary,
        // clamp to the end of the start line so `dw` at EOL does not
        // swallow the newline.
        let target = if matches!(effective_motion, Motion::WordForward)
            && target.row != start.row
            && start.row < self.buf.line_count()
        {
            let line = self.buf.line(start.row);
            Cursor {
                row: start.row,
                col: line.len(),
            }
        } else {
            target
        };

        self.apply_operator_motion(op, start, target, kind);
        self.dirty = true;
    }

    fn exec_operator_line(&mut self, op: Operator, n: usize) {
        let max_row = self.buf.max_row();
        let start_row = self.cursor.row.min(max_row);
        let end_row = start_row.saturating_add(n.saturating_sub(1)).min(max_row);
        self.apply_operator_linewise(op, start_row, end_row);
        self.dirty = true;
    }

    fn exec_operator_text_object(&mut self, op: Operator, object: TextObject, count: u32) {
        if let Some((start, end)) = resolve_text_object(&self.buf, self.cursor, object, count) {
            self.apply_operator_charwise(op, start, end);
            self.dirty = true;
        }
    }

    fn resolve_motion(
        &self,
        motion: Motion,
        start: Cursor,
        n: u32,
        mode: Mode,
        captured: Option<char>,
    ) -> Option<(Cursor, MotionKind)> {
        let cursor = match motion {
            Motion::Left => cursor::motion_h(&self.buf, start, n),
            Motion::Right => cursor::motion_l(&self.buf, start, n, mode),
            Motion::WordForward => cursor::motion_w(&self.buf, start, n, mode),
            Motion::WordBack => cursor::motion_b(&self.buf, start, n),
            Motion::WordEnd => cursor::motion_e(&self.buf, start, n),
            Motion::LineStart => cursor::motion_0(&self.buf, start),
            Motion::LineEnd => cursor::motion_dollar(&self.buf, start, n, mode),
            Motion::LineFirstNonblank => cursor::motion_caret(&self.buf, start, mode),
            Motion::Down => self.motion_vertical(start, n, mode, true),
            Motion::Up => self.motion_vertical(start, n, mode, false),
            Motion::DisplayDown => self.motion_display_line(start, n, mode, true),
            Motion::DisplayUp => self.motion_display_line(start, n, mode, false),
            Motion::HalfPageDown => {
                self.motion_vertical(start, n.saturating_mul(self.half_page_rows()), mode, true)
            }
            Motion::HalfPageUp => {
                self.motion_vertical(start, n.saturating_mul(self.half_page_rows()), mode, false)
            }
            // `{motion} alone` (count<=1) is the bare motion; N{motion}
            // takes a 1-based line number.
            Motion::FirstLine => cursor::motion_gg(&self.buf, if n <= 1 { None } else { Some(n) }),
            Motion::LastLine => cursor::motion_G(&self.buf, if n <= 1 { None } else { Some(n) }),
            Motion::ParagraphForward => cursor::motion_paragraph_forward(&self.buf, start, n),
            Motion::ParagraphBack => cursor::motion_paragraph_back(&self.buf, start, n),
            Motion::FindChar { forward, till } => {
                cursor::motion_find_char(&self.buf, start, captured?, n, forward, till)?
            }
            Motion::Mark { linewise } => {
                let (row, col) = self.mark_position(captured?)?;
                if linewise {
                    // `'m` jumps to the first non-blank on the target line.
                    cursor::motion_caret(&self.buf, Cursor { row, col: 0 }, mode)
                } else {
                    Cursor { row, col }
                }
            }
        };
        Some((cursor, motion_kind(motion)))
    }

    /// `j`/`k`: move `n` rows down or up preserving `curswant`.
    fn motion_vertical(&self, start: Cursor, n: u32, mode: Mode, down: bool) -> Cursor {
        let step = if down {
            cursor::motion_j
        } else {
            cursor::motion_k
        };
        step(
            &self.buf,
            start,
            n,
            mode,
            self.desired_display_col,
            self.tab_settings.tabstop,
        )
    }

    fn mark_position(&self, ch: char) -> Option<(usize, usize)> {
        let offset = *self.marks.get(&ch)?;
        Some(self.buf.offset_to_rowcol(offset.min(self.buf.len() as u32)))
    }

    fn motion_display_line(&self, start: Cursor, n: u32, mode: Mode, down: bool) -> Cursor {
        let width = self.width.max(1);
        if !self.effective_wrap() || width == 0 {
            return self.motion_vertical(start, n, mode, down);
        }

        let desired_display_col = if self.desired_display_col == u16::MAX {
            self.cursor_display().1
        } else {
            self.desired_display_col
        };
        let target_visual_col = desired_display_col % width;
        let mut cursor = start;

        for _ in 0..n.max(1) {
            let line = self.buf.line(cursor.row);
            let line = line.as_ref();
            let display_col =
                render::cursor_display_col(line, cursor.col, mode, self.tab_settings.tabstop);
            let segment = display_col / width;
            let line_rows = render::wrapped_line_rows(line, width, self.tab_settings.tabstop);

            let (target_row, target_segment) = if down {
                if segment + 1 < line_rows {
                    (cursor.row, segment + 1)
                } else if cursor.row + 1 < self.buf.line_count() {
                    (cursor.row + 1, 0)
                } else {
                    break;
                }
            } else if segment > 0 {
                (cursor.row, segment - 1)
            } else if cursor.row > 0 {
                let prev_row = cursor.row - 1;
                let prev_line = self.buf.line(prev_row);
                let prev_rows =
                    render::wrapped_line_rows(prev_line.as_ref(), width, self.tab_settings.tabstop);
                (prev_row, prev_rows.saturating_sub(1))
            } else {
                break;
            };

            let target_line = self.buf.line(target_row);
            let target_abs_col = target_segment
                .saturating_mul(width)
                .saturating_add(target_visual_col);
            let target_col = cursor::col_from_display(
                target_line.as_ref(),
                target_abs_col,
                mode,
                self.tab_settings.tabstop,
            );
            cursor = Cursor {
                row: target_row,
                col: target_col,
            };
        }

        cursor
    }

    /// Puts the editor into Insert mode at the current cursor position.
    ///
    /// Use when the host wants "new buffer, start typing" semantics
    /// without synthesizing a keypress.
    pub fn enter_insert_mode(&mut self) {
        self.enter_insert_at_cursor();
    }

    fn enter_insert_at_cursor(&mut self) {
        self.set_mode_ctx(Mode::Insert);
        self.dirty = true;
    }

    fn enter_insert_after(&mut self) {
        // `a`: move cursor right one grapheme (clamped to line.len()) then Insert.
        let line = self.buf.line(self.cursor.row);
        let line = line.as_ref();
        if !line.is_empty() {
            self.cursor.col = crate::buffer::next_grapheme_start(line, self.cursor.col);
        }
        self.enter_insert_at_cursor();
    }

    fn enter_insert_at_first_nonblank(&mut self) {
        let line = self.buf.line(self.cursor.row);
        self.cursor.col = leading_whitespace_end(line.as_ref());
        self.enter_insert_at_cursor();
    }

    fn enter_insert_at_eol(&mut self) {
        let line = self.buf.line(self.cursor.row);
        let line = line.as_ref();
        self.cursor.col = line.len();
        self.enter_insert_at_cursor();
    }

    fn open_line_below_and_insert(&mut self) {
        self.open_line_and_insert(true);
    }

    fn open_line_above_and_insert(&mut self) {
        self.open_line_and_insert(false);
    }

    fn open_line_and_insert(&mut self, below: bool) {
        if self.single_line {
            return;
        }
        let row = self.cursor.row;
        let (offset, new_row) = if below {
            let end = self.buf.line_start(row) + self.buf.line(row).len() as u32;
            (end, row + 1)
        } else {
            (self.buf.line_start(row), row)
        };
        self.commit(Edit::insert(offset, "\n".to_string()));
        self.cursor = Cursor {
            row: new_row,
            col: 0,
        };
        self.enter_insert_at_cursor();
    }

    fn exec_exit_insert(&mut self) {
        self.finish_pending_block_change();
        self.reset_to_primary_mode();

        if self.primary_mode == Mode::Normal {
            // Vim moves the cursor back by one grapheme on Esc (unless at col
            // 0). Then clamp to the last grapheme start of the line.
            let line = self.buf.line(self.cursor.row);
            let line = line.as_ref();
            if self.cursor.col > 0 {
                self.cursor.col = buffer::prev_grapheme_start(line, self.cursor.col);
            }
            let line = self.buf.line(self.cursor.row);
            let line = line.as_ref();
            if !line.is_empty() {
                let max = buffer::last_grapheme_start(line).unwrap_or(0);
                if self.cursor.col > max {
                    self.cursor.col = max;
                }
            }
        }
        self.update_desired_display_col();
        self.dirty = true;
    }

    fn finish_pending_block_change(&mut self) {
        let Some(pending) = self.pending_block_change.take() else {
            return;
        };
        if self.cursor.row != pending.row_start {
            return;
        }
        let line = self.buf.line(pending.row_start);
        let line = line.as_ref();
        let start_col = pending.col.min(line.len());
        let end_col = self.cursor.col.min(line.len());
        if end_col <= start_col {
            return;
        }
        let inserted = line[start_col..end_col].to_string();
        for row in ((pending.row_start + 1)..=pending.row_end).rev() {
            let line = self.buf.line(row);
            let col = pending.col.min(line.len());
            let offset = self.buf.rowcol_to_offset(row, col);
            self.commit(Edit::insert(offset, inserted.clone()));
        }
    }

    fn toggle_or_enter_visual(&mut self, target: VisualKind) {
        if !self.mode.is_visual() {
            self.enter_visual_fresh(target);
            return;
        }
        if VisualKind::from_mode(self.mode) == target {
            self.exit_visual();
            return;
        }
        if let Some(sel) = self.selection.as_mut() {
            sel.kind = target;
        }
        self.set_mode_ctx(target.mode());
        self.dirty = true;
    }

    fn enter_visual_fresh(&mut self, kind: VisualKind) {
        self.selection = Some(Selection::new(self.cursor, kind));
        self.set_mode_ctx(kind.mode());
        self.dirty = true;
    }

    fn exit_visual(&mut self) {
        self.selection = None;
        self.reset_to_primary_mode();
        self.dirty = true;
    }

    fn update_selection_head(&mut self) {
        if let Some(sel) = self.selection.as_mut() {
            sel.head = self.cursor;
        }
    }

    fn apply_operator_motion(
        &mut self,
        op: Operator,
        start: Cursor,
        end: Cursor,
        kind: MotionKind,
    ) {
        let (s, e) = self.normalize_motion_range(start, end, kind);
        if matches!(kind, MotionKind::Linewise) {
            self.apply_operator_linewise(op, s.0, e.0);
        } else {
            self.apply_operator_charwise(op, s, e);
        }
    }

    /// Returns the `(start, end)` byte endpoints produced by a motion
    /// of `kind`: exclusive kinds pass through, `CharInclusive` expands
    /// `end` by one grapheme so the motion's landing grapheme is
    /// included in the range.
    fn normalize_motion_range(
        &self,
        start: Cursor,
        end: Cursor,
        kind: MotionKind,
    ) -> ((usize, usize), (usize, usize)) {
        let ((sr, sc), (er, ec)) = order_range(start, end);
        let end = if matches!(kind, MotionKind::CharInclusive) {
            (er, buffer::grapheme_end(self.buf.line(er).as_ref(), ec))
        } else {
            (er, ec)
        };
        ((sr, sc), end)
    }

    fn apply_operator_charwise(
        &mut self,
        op: Operator,
        start: (usize, usize),
        end: (usize, usize),
    ) {
        let text = extract_charwise(&self.buf, start, end);
        self.yank = Yank {
            lines: split_to_lines(&text),
            kind: YankKind::Charwise,
        };
        self.cursor = Cursor {
            row: start.0,
            col: start.1,
        };
        match op {
            Operator::Yank => {}
            Operator::Delete => {
                self.checkpoint();
                self.commit_delete_range(start, end);
                self.fixup_cursor();
            }
            Operator::Change => {
                self.checkpoint();
                self.commit_delete_range(start, end);
                self.enter_insert_at_cursor();
            }
        }
        self.update_desired_display_col();
    }

    /// Deletes `buf.text()[start_offset..end_offset]` where the two
    /// endpoints are given in `(row, col)` form. No-op on empty range.
    fn commit_delete_range(&mut self, start: (usize, usize), end: (usize, usize)) {
        let s_off = self.buf.rowcol_to_offset(start.0, start.1);
        let e_off = self.buf.rowcol_to_offset(end.0, end.1);
        if e_off > s_off {
            self.commit(Edit::delete(s_off, e_off - s_off));
        }
    }

    fn apply_operator_linewise(&mut self, op: Operator, row_start: usize, row_end: usize) {
        let max_row = self.buf.max_row();
        let (rs, re) = if row_start <= row_end {
            (row_start.min(max_row), row_end.min(max_row))
        } else {
            (row_end.min(max_row), row_start.min(max_row))
        };
        // rs <= re and both <= max_row, so slicing is always valid.
        let mut lines: Vec<String> = Vec::with_capacity(re - rs + 1);
        for r in rs..=re {
            lines.push(self.buf.line(r).to_string());
        }
        self.yank = Yank {
            lines,
            kind: YankKind::Linewise,
        };
        self.cursor = Cursor { row: rs, col: 0 };
        match op {
            Operator::Yank => {}
            Operator::Delete => {
                self.checkpoint();
                self.commit_delete_lines(rs, re);
                self.cursor.row = rs.min(self.buf.line_count() - 1);
                self.fixup_cursor();
            }
            Operator::Change => {
                self.checkpoint();
                // Delete the content of rows `rs..=re` (including their
                // separating newlines) but leave row `rs` present as an
                // empty line — that's where insert-mode resumes.
                let start_off = self.buf.line_start(rs);
                let end_off = if re + 1 < self.buf.line_count() {
                    self.buf.line_start(re + 1) - 1
                } else {
                    self.buf.len() as u32
                };
                if end_off > start_off {
                    self.commit(Edit::delete(start_off, end_off - start_off));
                }
                self.enter_insert_at_cursor();
            }
        }
        self.update_desired_display_col();
    }

    /// Delete entire lines `rs..=re`. Ensures the buffer retains at
    /// least one (possibly empty) line.
    fn commit_delete_lines(&mut self, rs: usize, re: usize) {
        let max_row = self.buf.max_row();
        let end = re.min(max_row);
        let start = rs.min(end);
        let start_off = self.buf.line_start(start);
        let (apply_start, apply_len) = if end + 1 < self.buf.line_count() {
            // Consume the `\n` after line `end` so the split falls on a
            // true line boundary.
            let end_off = self.buf.line_start(end + 1);
            (start_off, end_off - start_off)
        } else if start > 0 {
            // Deleting through EOF from a non-first row: also consume
            // the preceding `\n` so no trailing empty row survives.
            (start_off - 1, self.buf.len() as u32 - (start_off - 1))
        } else {
            // Deleting the whole buffer.
            (0, self.buf.len() as u32)
        };
        if apply_len > 0 {
            self.commit(Edit::delete(apply_start, apply_len));
        }
    }

    fn exec_insert_char(&mut self, c: char) {
        if self.single_line && matches!(c, '\n' | '\r') {
            return;
        }
        self.commit_insert_at_cursor(c.to_string());
        self.update_desired_display_col();
    }

    fn exec_insert_tab(&mut self) {
        let text = if self.tab_settings.expandtab {
            let line = self.buf.line(self.cursor.row);
            let display_col = render::byte_col_to_display_col(
                line.as_ref(),
                self.cursor.col,
                self.tab_settings.tabstop,
            );
            let width = self.tab_settings.expandtab_width(display_col);
            Cow::Owned(" ".repeat(width as usize))
        } else {
            self.tab_settings.tab_input_text()
        };
        self.commit_insert_at_cursor(text.into_owned());
        self.update_desired_display_col();
    }

    fn exec_insert_newline(&mut self) {
        if self.single_line {
            return;
        }
        self.commit_insert_at_cursor("\n".to_string());
        self.desired_display_col = 0;
    }

    /// Inserts `text` at the cursor and advances the cursor past the
    /// insertion. Does NOT refresh `desired_display_col`.
    fn commit_insert_at_cursor(&mut self, text: String) {
        let offset = self.buf.rowcol_to_offset(self.cursor.row, self.cursor.col);
        let len = text.len() as u32;
        self.commit(Edit::insert(offset, text));
        let (r, col) = self.buf.offset_to_rowcol(offset + len);
        self.cursor = Cursor { row: r, col };
    }

    fn exec_backspace(&mut self) {
        let (row, col) = (self.cursor.row, self.cursor.col);
        if col == 0 {
            if row == 0 {
                return;
            }
            let prev_row = row - 1;
            let prev_end = self.buf.line(prev_row).len();
            let nl_offset = self.buf.line_start(row) - 1;
            self.commit(Edit::delete(nl_offset, 1));
            self.cursor = Cursor {
                row: prev_row,
                col: prev_end,
            };
        } else {
            let line = self.buf.line(row);
            let line = line.as_ref();
            let start_col = buffer::prev_grapheme_start(line, col);
            let offset = self.buf.line_start(row) + start_col as u32;
            let len = (col - start_col) as u32;
            self.commit(Edit::delete(offset, len));
            self.cursor = Cursor {
                row,
                col: start_col,
            };
        }
        self.update_desired_display_col();
    }

    fn exec_x(&mut self, n: usize) {
        let max_row = self.buf.max_row();
        let row = self.cursor.row.min(max_row);
        let line = self.buf.line(row);
        let line = line.as_ref();
        if line.is_empty() {
            return;
        }
        let start_col = buffer::align_to_grapheme_start(line, self.cursor.col.min(line.len()));
        let mut end_col = start_col;
        for _ in 0..n {
            if end_col >= line.len() {
                break;
            }
            end_col = buffer::next_grapheme_start(line, end_col);
        }
        self.apply_operator_charwise(Operator::Delete, (row, start_col), (row, end_col));
    }

    fn exec_replace(&mut self, ch: char, n: usize) {
        let max_row = self.buf.max_row();
        let r = self.cursor.row.min(max_row);
        let line = self.buf.line(r);
        let line = line.as_ref();
        if line.is_empty() {
            return;
        }
        let start = buffer::align_to_grapheme_start(line, self.cursor.col.min(line.len()));
        self.checkpoint();
        let mut col = start;
        let mut remaining = n;
        while remaining > 0 {
            let line = self.buf.line(r);
            let line = line.as_ref();
            if col >= line.len() {
                self.history.abort();
                return;
            }
            col = buffer::next_grapheme_start(line, col);
            remaining -= 1;
        }
        let replacement: String = std::iter::repeat(ch).take(n).collect();
        let offset = self.buf.line_start(r) + start as u32;
        let old_len = (col - start) as u32;
        self.commit(Edit::replace(offset, old_len, replacement));
        let last_start = if n > 0 {
            start + (n - 1) * ch.len_utf8()
        } else {
            start
        };
        self.cursor = Cursor {
            row: r,
            col: last_start,
        };
    }

    fn exec_s(&mut self, n: usize) {
        self.exec_x(n);
        self.enter_insert_at_cursor();
    }

    fn exec_big_s(&mut self, _n: usize) {
        let max_row = self.buf.max_row();
        let r = self.cursor.row.min(max_row);
        self.apply_operator_linewise(Operator::Change, r, r);
    }

    fn exec_join_lines(&mut self, count: usize) {
        let max_row = self.buf.max_row();
        let row = self.cursor.row.min(max_row);
        if row >= max_row {
            return;
        }

        // Vim's `[count]J` joins `count` lines total; bare `J` joins the
        // current line with the next one.
        let joins = if count <= 1 { 1 } else { count - 1 };
        self.checkpoint();
        for _ in 0..joins {
            if row + 1 >= self.buf.line_count() {
                break;
            }

            let line = self.buf.line(row);
            let line = line.as_ref();
            let next = self.buf.line(row + 1);
            let next = next.as_ref();
            let line_len = line.len();
            let next_content_col = leading_whitespace_end(next);
            let replacement = join_separator(line, next, next_content_col);
            let offset = self.buf.line_start(row) + line_len as u32;
            let old_len = 1 + next_content_col as u32;
            self.commit(Edit::replace(offset, old_len, replacement.to_string()));
            self.cursor = Cursor { row, col: line_len };
            self.fixup_cursor();
        }
        self.update_desired_display_col();
    }

    fn exec_paste_after(&mut self, count: usize) {
        self.exec_paste(count, true);
    }

    fn exec_paste_before(&mut self, count: usize) {
        self.exec_paste(count, false);
    }

    fn exec_paste(&mut self, count: usize, after: bool) {
        if self.yank.lines.is_empty() {
            return;
        }
        self.checkpoint();
        match self.yank.kind {
            YankKind::Charwise => {
                let insert_col = if after {
                    let line = self.buf.line(self.cursor.row);
                    let line = line.as_ref();
                    if line.is_empty() {
                        0
                    } else {
                        buffer::next_grapheme_start(line, self.cursor.col)
                    }
                } else {
                    self.cursor.col
                };
                let text = self.paste_text();
                let end_off =
                    self.commit_insert_repeated(self.cursor.row, insert_col, &text, count);
                self.cursor = self.cursor_one_grapheme_before(end_off);
            }
            YankKind::Linewise if self.single_line => {
                let insert_col = if after {
                    self.buf.line(self.cursor.row).len()
                } else {
                    0
                };
                let text = self.paste_text();
                self.commit_insert_repeated(self.cursor.row, insert_col, &text, count);
                self.cursor = Cursor {
                    row: self.cursor.row,
                    col: insert_col,
                };
            }
            YankKind::Linewise => {
                let insert_at = self.cursor.row + after as usize;
                let mut to_insert: Vec<String> = Vec::new();
                for _ in 0..count {
                    to_insert.extend(self.yank.lines.iter().cloned());
                }
                self.commit_insert_lines(insert_at, &to_insert);
                self.cursor = Cursor {
                    row: insert_at,
                    col: first_nonblank(self.buf.line(insert_at).as_ref()),
                };
            }
            YankKind::Blockwise => {
                let start_col = self.cursor.col;
                let start_row = self.cursor.row;
                let lines = self.yank.lines.clone();
                for (i, text) in lines.iter().enumerate() {
                    let r = start_row + i;
                    self.commit_ensure_row_exists(r);
                    let col = start_col.min(self.buf.line(r).len());
                    let offset = self.buf.rowcol_to_offset(r, col);
                    self.commit(Edit::insert(offset, text.clone()));
                }
                self.cursor = Cursor {
                    row: start_row,
                    col: start_col,
                };
            }
        }
        self.update_desired_display_col();
    }

    fn yank_text_joined(&self) -> String {
        self.yank.lines.join("\n")
    }

    fn paste_text(&self) -> String {
        let text = self.yank_text_joined();
        self.normalize_text_for_mode(&text).into_owned()
    }

    /// THE mutation entry point. Applies `edit` to the buffer,
    /// records the resulting inverse into the in-progress undo group
    /// (paired with the pre-edit cursor), and marks the editor dirty.
    /// Every mutation — user-driven or undo/redo-driven — must go
    /// through `commit` so that extension hooks (syntax highlighting,
    /// history) see every change in a uniform way.
    fn commit(&mut self, edit: Edit) -> Edit {
        let cursor_before = self.cursor;
        let inverse = self.apply_with_hooks(&edit);
        self.history.record(inverse.clone(), cursor_before);
        inverse
    }

    /// Low-level mutation: applies `edit`, notifies the syntax
    /// highlighter, flips `dirty`. Does NOT touch history.
    ///
    /// Used by `commit` (which layers history on top) and by
    /// `exec_undo` / `exec_redo` (which own their own history
    /// bookkeeping and would otherwise double-record).
    fn apply_with_hooks(&mut self, edit: &Edit) -> Edit {
        let normalized_edit;
        let edit = if self.single_line {
            normalized_edit = Edit {
                offset: edit.offset,
                old_len: edit.old_len,
                new: strip_single_line_breaks(&edit.new).into_owned(),
            };
            &normalized_edit
        } else {
            edit
        };
        let replacement = Replacement {
            offset: edit.offset,
            old_len: edit.old_len,
            new_len: edit.new.len() as u32,
        };
        let edit_is_noop = edit.old_len == 0 && edit.new.is_empty();
        if self.track_replacements && !edit_is_noop {
            self.replacements.push(replacement);
        }
        if !edit_is_noop {
            self.shift_marks(replacement);
        }
        let inverse = self.buf.apply(edit);
        self.dirty = true;
        self.text_version = self.text_version.wrapping_add(1);
        inverse
    }

    /// Inserts `text` `count` times at `(row, col)` and returns the
    /// byte offset one past the final insertion.
    fn commit_insert_repeated(&mut self, row: usize, col: usize, text: &str, count: usize) -> u32 {
        let text = self.normalize_text_for_mode(text).into_owned();
        let mut offset = self.buf.rowcol_to_offset(row, col);
        if text.is_empty() {
            return offset;
        }
        for _ in 0..count {
            self.commit(Edit::insert(offset, text.to_string()));
            offset += text.len() as u32;
        }
        offset
    }

    /// Splices `lines` in as full rows at `row` — the equivalent of
    /// `Vec::insert_many(row, lines)` on the old line-per-entry
    /// storage.
    ///
    /// When `row < line_count`, inserts `"L0\nL1\n...Ln\n"` at that
    /// row's start. When `row >= line_count`, appends `"\nL0\n...Ln"`
    /// at end-of-buffer so the existing trailing row isn't merged
    /// with `L0`.
    fn commit_insert_lines(&mut self, row: usize, lines: &[String]) {
        if lines.is_empty() {
            return;
        }
        let at_end = row >= self.buf.line_count();
        let offset = if at_end {
            self.buf.len() as u32
        } else {
            self.buf.line_start(row)
        };
        if self.single_line {
            self.commit(Edit::insert(offset, lines.concat()));
            return;
        }
        // When inserting at end-of-buffer we prepend the separator so the
        // existing trailing row isn't merged with `lines[0]`; for inline
        // inserts the separator follows each line so the splice lands on
        // a fresh row.
        let mut content = String::new();
        for line in lines {
            if at_end {
                content.push('\n');
                content.push_str(line);
            } else {
                content.push_str(line);
                content.push('\n');
            }
        }
        self.commit(Edit::insert(offset, content));
    }

    /// Extends the buffer with empty rows until `row` is a valid index.
    fn commit_ensure_row_exists(&mut self, row: usize) {
        if self.single_line {
            return;
        }
        while self.buf.line_count() <= row {
            let offset = self.buf.len() as u32;
            self.commit(Edit::insert(offset, "\n".to_string()));
        }
    }

    fn normalize_text_for_mode<'a>(&self, text: &'a str) -> Cow<'a, str> {
        if self.single_line {
            strip_single_line_breaks(text)
        } else {
            Cow::Borrowed(text)
        }
    }

    fn enforce_single_line_mode(&mut self) {
        let old_text = self.buf.text();
        let offset = self.buf.rowcol_to_offset(self.cursor.row, self.cursor.col) as usize;
        let normalized = strip_single_line_breaks(&old_text);
        let cursor_offset = single_line_prefix_len(&old_text, offset) as u32;
        if normalized.as_ref() != old_text {
            self.buf.set_text(normalized.as_ref());
        }
        self.history.reset();
        self.yank = single_line_yank(&self.yank);
        self.selection = None;
        let target_mode = if self.mode.is_visual() {
            self.primary_mode
        } else {
            self.mode
        };
        self.set_mode_ctx(target_mode);
        let (row, col) = self.buf.offset_to_rowcol(cursor_offset);
        self.cursor = Cursor { row, col };
        self.fixup_cursor();
        self.update_desired_display_col();
        self.scroll_offset = 0;
    }

    /// Deletes bytes `[c_lo..c_hi)` from each row in `[r0..=r1]`.
    /// Applies row-by-row in reverse so edits on earlier rows don't
    /// shift the offsets needed for later rows.
    fn commit_delete_block(&mut self, r0: usize, r1: usize, c_lo: usize, c_hi: usize) {
        for r in (r0..=r1).rev() {
            let line = self.buf.line(r);
            let line = line.as_ref();
            let (lo, hi) = block_span(line, c_lo, c_hi);
            if lo < hi {
                let offset = self.buf.line_start(r) + lo as u32;
                self.commit(Edit::delete(offset, (hi - lo) as u32));
            }
        }
    }

    /// Helper for paste: given `end_offset` (one past the last
    /// inserted byte), returns the cursor position on the grapheme
    /// immediately preceding that offset. Mirrors vim's post-paste
    /// cursor placement on non-empty pastes.
    fn cursor_one_grapheme_before(&self, end_offset: u32) -> Cursor {
        let (r, c) = self.buf.offset_to_rowcol(end_offset);
        let final_col = if c == 0 {
            0
        } else {
            let line = self.buf.line(r);
            let line = line.as_ref();
            buffer::prev_grapheme_start(line, c)
        };
        Cursor {
            row: r,
            col: final_col,
        }
    }

    fn checkpoint(&mut self) {
        self.history.checkpoint();
    }

    /// Ensure cursor is within a valid position for Normal mode after
    /// an edit: clamp to last grapheme start; clamp row; handle empty buffer.
    fn fixup_cursor(&mut self) {
        // `TextBuffer` maintains at least one line, so `line_count >= 1`.
        let max_row = self.buf.line_count() - 1;
        if self.cursor.row > max_row {
            self.cursor.row = max_row;
        }
        let line = self.buf.line(self.cursor.row);
        let line = line.as_ref();
        if line.is_empty() {
            self.cursor.col = 0;
        } else if matches!(
            self.mode,
            Mode::Normal | Mode::Visual | Mode::VisualLine | Mode::VisualBlock
        ) {
            let max = buffer::last_grapheme_start(line).unwrap_or(0);
            if self.cursor.col > max {
                self.cursor.col = max;
            } else {
                self.cursor.col = buffer::align_to_grapheme_start(line, self.cursor.col);
            }
        } else if self.cursor.col > line.len() {
            self.cursor.col = line.len();
        }
    }

    fn update_desired_display_col(&mut self) {
        let line = self.buf.line(self.cursor.row);
        let line = line.as_ref();
        // `curswant` in vim tracks the display col of the cursor's
        // visible position, not the grapheme's start col — the two
        // differ for hard tabs in Normal/Visual mode.
        self.desired_display_col =
            render::cursor_display_col(line, self.cursor.col, self.mode, self.tab_settings.tabstop);
    }

    fn apply_visual_operator(&mut self, op: Operator) {
        let Some(sel) = self.selection.clone() else {
            return;
        };
        match sel.kind {
            VisualKind::Char => {
                let (start, end) = render::char_range(&self.buf, &sel);
                let end_byte = buffer::grapheme_end(self.buf.line(end.row).as_ref(), end.col);
                self.apply_operator_charwise(op, (start.row, start.col), (end.row, end_byte));
            }
            VisualKind::Line => {
                let (r0, r1) = sel.rows_ordered();
                self.apply_operator_linewise(op, r0, r1);
            }
            VisualKind::Block => {
                self.apply_block_operator(op, &sel);
            }
        }
        self.selection = None;
        if op != Operator::Change {
            // Change already transitioned into Insert via apply_operator_*,
            // which `set_mode_ctx`'d the current layer back to BASE.
            self.reset_to_primary_mode();
        }
        self.dirty = true;
    }

    fn apply_block_operator(&mut self, op: Operator, sel: &Selection) {
        let max_row = self.buf.max_row();
        let (r0, r1) = sel.rows_ordered();
        let r0 = r0.min(max_row);
        let r1 = r1.min(max_row);
        let (c_lo, c_hi) = sel.cols_ordered();

        let mut block_lines: Vec<String> = Vec::with_capacity(r1 - r0 + 1);
        for r in r0..=r1 {
            let line = self.buf.line(r);
            let line = line.as_ref();
            let (lo, hi) = block_span(line, c_lo, c_hi);
            block_lines.push(line[lo..hi].to_string());
        }

        self.yank = Yank {
            lines: block_lines,
            kind: YankKind::Blockwise,
        };
        self.cursor = Cursor { row: r0, col: c_lo };
        if matches!(op, Operator::Delete | Operator::Change) {
            self.checkpoint();
            self.commit_delete_block(r0, r1, c_lo, c_hi);
            self.fixup_cursor();
        }
        if matches!(op, Operator::Change) {
            self.pending_block_change = Some(PendingBlockChange {
                row_start: r0,
                row_end: r1,
                col: c_lo,
            });
            self.cursor = Cursor { row: r0, col: c_lo };
            self.enter_insert_at_cursor();
        }
        self.update_desired_display_col();
    }

    /// Grow (or establish) the visual selection to cover `object`.
    ///
    /// Matches nvim's `vip`, `viw`, `va"`, etc.: the head lands on the
    /// last grapheme of the object; the anchor moves to its first
    /// byte. If the object is line-shaped (`paragraph`), the mode
    /// switches to VisualLine so subsequent operators act on whole
    /// rows.
    fn exec_select_text_object(&mut self, object: TextObject, count: u32) {
        if !self.mode.is_visual() {
            return;
        }
        let Some((start, end)) = resolve_text_object(&self.buf, self.cursor, object, count) else {
            return;
        };

        // `end` is exclusive; step back one grapheme so the head sits on
        // the object's last selected grapheme.
        let head = {
            let line = self.buf.line(end.0);
            let line = line.as_ref();
            if end.1 == 0 && end.0 > 0 {
                // Object ended at the start of a row — back up to the
                // previous row's last grapheme.
                let prev = self.buf.line(end.0 - 1);
                let prev = prev.as_ref();
                Cursor {
                    row: end.0 - 1,
                    col: buffer::last_grapheme_start(prev).unwrap_or(0),
                }
            } else if end.1 == 0 {
                Cursor { row: 0, col: 0 }
            } else {
                let aligned = buffer::align_to_grapheme_start(line, end.1);
                Cursor {
                    row: end.0,
                    col: buffer::prev_grapheme_start(line, end.1.min(aligned + 1)),
                }
            }
        };
        let anchor = Cursor {
            row: start.0,
            col: start.1,
        };

        // Paragraphs are line-shaped; switch to VisualLine so subsequent
        // operators act on whole rows.
        if matches!(object, TextObject::Paragraph { .. }) && self.mode != Mode::VisualLine {
            self.set_mode_ctx(Mode::VisualLine);
        }

        self.selection = Some(Selection {
            anchor,
            head,
            kind: VisualKind::from_mode(self.mode),
        });
        self.cursor = head;
        self.update_desired_display_col();
        self.current_layer = LayerId::BASE;
        self.pending_capture = None;
        self.dirty = true;
    }

    fn exec_surround_selection(&mut self, delimiter: char) {
        let pair = surround_pair(delimiter);
        let Some(sel) = self.selection else {
            return;
        };

        match sel.kind {
            VisualKind::Char => self.surround_visual_charwise(&sel, pair),
            VisualKind::Line => self.surround_visual_linewise(&sel, pair),
            VisualKind::Block => self.surround_visual_blockwise(&sel, pair),
        }

        self.selection = None;
        self.reset_to_primary_mode();
        self.dirty = true;
        self.fixup_cursor();
        self.update_desired_display_col();
    }

    fn surround_visual_charwise(&mut self, sel: &Selection, pair: SurroundPair) {
        let (start, end) = render::char_range(&self.buf, sel);
        let end_byte = buffer::grapheme_end(self.buf.line(end.row).as_ref(), end.col);
        let start_off = self.buf.rowcol_to_offset(start.row, start.col);
        let end_off = self.buf.rowcol_to_offset(end.row, end_byte);
        self.checkpoint();
        self.commit(Edit::insert(end_off, pair.close.to_string()));
        self.commit(Edit::insert(start_off, pair.open.to_string()));
        self.cursor = Cursor {
            row: start.row,
            col: start.col,
        };
    }

    fn surround_visual_linewise(&mut self, sel: &Selection, pair: SurroundPair) {
        let max_row = self.buf.max_row();
        let (r0, r1) = sel.rows_ordered();
        let r0 = r0.min(max_row);
        let r1 = r1.min(max_row);
        let start_off = self.buf.line_start(r0);
        let end_off = if r1 + 1 < self.buf.line_count() {
            self.buf.line_start(r1 + 1) - 1
        } else {
            self.buf.len() as u32
        };
        self.checkpoint();
        self.commit(Edit::insert(end_off, pair.close.to_string()));
        self.commit(Edit::insert(start_off, pair.open.to_string()));
        self.cursor = Cursor { row: r0, col: 0 };
    }

    fn surround_visual_blockwise(&mut self, sel: &Selection, pair: SurroundPair) {
        let max_row = self.buf.max_row();
        let (r0, r1) = sel.rows_ordered();
        let r0 = r0.min(max_row);
        let r1 = r1.min(max_row);
        let (c_lo, c_hi) = sel.cols_ordered();

        self.checkpoint();
        for r in (r0..=r1).rev() {
            let line = self.buf.line(r);
            let line = line.as_ref();
            let (lo, hi) = block_span(line, c_lo, c_hi);
            if lo == hi {
                continue;
            }
            let close_off = self.buf.rowcol_to_offset(r, hi);
            let open_off = self.buf.rowcol_to_offset(r, lo);
            self.commit(Edit::insert(close_off, pair.close.to_string()));
            self.commit(Edit::insert(open_off, pair.open.to_string()));
        }
        self.cursor = Cursor { row: r0, col: c_lo };
    }

    fn exec_set_mark(&mut self, ch: char) {
        if !is_valid_mark(ch) {
            return;
        }
        let offset = self.buf.rowcol_to_offset(self.cursor.row, self.cursor.col);
        self.marks.insert(ch, offset);
    }

    fn shift_marks(&mut self, rep: Replacement) {
        self.marks.retain(|_, offset| {
            match rep.project(Span::new(*offset, 0)) {
                Some(shifted) => {
                    *offset = shifted.offset;
                    true
                }
                // The mark's byte fell inside a replaced range. Clamp
                // to the replacement's start so the mark still resolves
                // to a valid position rather than dropping silently.
                None => {
                    *offset = rep.offset;
                    true
                }
            }
        });
    }

    fn exec_toggle_case_char(&mut self, n: usize) {
        if n == 0 {
            return;
        }
        let r = self.cursor.row.min(self.buf.max_row());
        let line = self.buf.line(r);
        let line = line.as_ref();
        if line.is_empty() {
            return;
        }
        let start = buffer::align_to_grapheme_start(line, self.cursor.col.min(line.len()));
        let mut end = start;
        for _ in 0..n {
            if end >= line.len() {
                break;
            }
            end = buffer::next_grapheme_start(line, end);
        }
        if end <= start {
            return;
        }
        self.apply_case_charwise(CaseTransform::Toggle, (r, start), (r, end));
        // Vim parks the cursor just past the transformed range, unlike
        // the generic case operator which lands on `start`.
        let new_line_len = self.buf.line(r).len();
        self.cursor = Cursor {
            row: r,
            col: end.min(new_line_len),
        };
        self.fixup_cursor();
        self.update_desired_display_col();
    }

    fn exec_case_motion(
        &mut self,
        transform: CaseTransform,
        motion: Motion,
        count: u32,
        captured: Option<char>,
    ) {
        let start = self.cursor;
        let n = count.max(1);
        let Some((target, kind)) = self.resolve_motion(motion, start, n, self.mode, captured)
        else {
            return;
        };
        self.apply_case_motion(transform, start, target, kind);
    }

    fn exec_case_text_object(&mut self, transform: CaseTransform, object: TextObject, count: u32) {
        let Some((start, end)) = resolve_text_object(&self.buf, self.cursor, object, count) else {
            return;
        };
        self.apply_case_charwise(transform, start, end);
    }

    fn exec_case_line(&mut self, transform: CaseTransform, n: usize) {
        let max_row = self.buf.max_row();
        let start_row = self.cursor.row.min(max_row);
        let end_row = start_row.saturating_add(n.saturating_sub(1)).min(max_row);
        self.apply_case_linewise(transform, start_row, end_row);
    }

    fn exec_case_selection(&mut self, transform: CaseTransform) {
        let Some(sel) = self.selection.clone() else {
            return;
        };
        match sel.kind {
            VisualKind::Char => {
                let (start, end) = render::char_range(&self.buf, &sel);
                let end_byte = buffer::grapheme_end(self.buf.line(end.row).as_ref(), end.col);
                self.apply_case_charwise(transform, (start.row, start.col), (end.row, end_byte));
            }
            VisualKind::Line => {
                let (r0, r1) = sel.rows_ordered();
                self.apply_case_linewise(transform, r0, r1);
            }
            VisualKind::Block => {
                self.apply_case_block(transform, &sel);
            }
        }
        self.selection = None;
        self.reset_to_primary_mode();
        self.dirty = true;
    }

    fn apply_case_motion(
        &mut self,
        transform: CaseTransform,
        start: Cursor,
        end: Cursor,
        kind: MotionKind,
    ) {
        let (s, e) = self.normalize_motion_range(start, end, kind);
        if matches!(kind, MotionKind::Linewise) {
            self.apply_case_linewise(transform, s.0, e.0);
        } else {
            self.apply_case_charwise(transform, s, e);
        }
    }

    fn apply_case_charwise(
        &mut self,
        transform: CaseTransform,
        start: (usize, usize),
        end: (usize, usize),
    ) {
        let s_off = self.buf.rowcol_to_offset(start.0, start.1);
        let e_off = self.buf.rowcol_to_offset(end.0, end.1);
        if e_off <= s_off {
            return;
        }
        let text = extract_charwise(&self.buf, start, end);
        let transformed = case_transform_text(transform, &text);
        if transformed == text {
            self.cursor = Cursor {
                row: start.0,
                col: start.1,
            };
            self.fixup_cursor();
            self.update_desired_display_col();
            self.dirty = true;
            return;
        }
        self.checkpoint();
        self.commit(Edit::replace(s_off, e_off - s_off, transformed));
        self.cursor = Cursor {
            row: start.0,
            col: start.1,
        };
        self.fixup_cursor();
        self.update_desired_display_col();
        self.dirty = true;
    }

    fn apply_case_linewise(&mut self, transform: CaseTransform, rs: usize, re: usize) {
        let max_row = self.buf.max_row();
        let (rs, re) = (rs.min(max_row), re.min(max_row));
        let (rs, re) = if rs <= re { (rs, re) } else { (re, rs) };
        self.checkpoint();
        for r in rs..=re {
            let line = self.buf.line(r);
            let line = line.as_ref();
            if line.is_empty() {
                continue;
            }
            let transformed = case_transform_text(transform, line);
            if transformed == line {
                continue;
            }
            let offset = self.buf.line_start(r);
            self.commit(Edit::replace(offset, line.len() as u32, transformed));
        }
        self.cursor = Cursor { row: rs, col: 0 };
        self.fixup_cursor();
        self.update_desired_display_col();
        self.dirty = true;
    }

    fn apply_case_block(&mut self, transform: CaseTransform, sel: &Selection) {
        let max_row = self.buf.max_row();
        let (r0, r1) = sel.rows_ordered();
        let r0 = r0.min(max_row);
        let r1 = r1.min(max_row);
        let (c_lo, c_hi) = sel.cols_ordered();

        self.checkpoint();
        for r in (r0..=r1).rev() {
            let line = self.buf.line(r);
            let line = line.as_ref();
            let (lo, hi) = block_span(line, c_lo, c_hi);
            if lo >= hi {
                continue;
            }
            let slice = &line[lo..hi];
            let transformed = case_transform_text(transform, slice);
            if transformed == slice {
                continue;
            }
            let offset = self.buf.line_start(r) + lo as u32;
            self.commit(Edit::replace(offset, (hi - lo) as u32, transformed));
        }
        self.cursor = Cursor { row: r0, col: c_lo };
        self.fixup_cursor();
        self.update_desired_display_col();
    }

    fn exec_increment(&mut self, n: usize, sign: i64) {
        let r = self.cursor.row.min(self.buf.max_row());
        let line = self.buf.line(r);
        let line = line.as_ref();
        let Some((start, end, value)) = find_number_from(line, self.cursor.col) else {
            return;
        };
        let delta = (n as i64).saturating_mul(sign);
        let new_value = value.saturating_add(delta);
        let replacement = new_value.to_string();
        self.checkpoint();
        let offset = self.buf.line_start(r) + start as u32;
        self.commit(Edit::replace(
            offset,
            (end - start) as u32,
            replacement.clone(),
        ));
        // Vim parks the cursor on the last digit of the new number.
        let last_digit_offset = offset as usize + replacement.len().saturating_sub(1);
        let line_start = self.buf.line_start(r) as usize;
        self.cursor = Cursor {
            row: r,
            col: last_digit_offset - line_start,
        };
        self.fixup_cursor();
        self.update_desired_display_col();
    }

    fn exec_undo(&mut self) {
        self.replay_history(false);
    }

    fn exec_redo(&mut self) {
        self.replay_history(true);
    }

    /// Replay one history group. `redo == false` pops from undo and
    /// applies steps in reverse; `true` pops from redo and applies
    /// them forward. Either way, the cursor lands on the group's
    /// first-recorded `cursor_before` (where the original action
    /// started) and the inverse group is pushed onto the opposite
    /// stack in forward order.
    fn replay_history(&mut self, redo: bool) {
        let group = if redo {
            self.history.pop_redo_group()
        } else {
            self.history.pop_undo_group()
        };
        let Some(group) = group else {
            return;
        };
        let restore_cursor = group
            .steps
            .first()
            .map(|s| s.cursor_before)
            .unwrap_or(self.cursor);
        let n = group.steps.len();
        let mut new_steps: Vec<crate::history::UndoStep> = Vec::with_capacity(n);
        for i in 0..n {
            let idx = if redo { i } else { n - 1 - i };
            let step = &group.steps[idx];
            let inverse = self.apply_with_hooks(&step.inverse);
            new_steps.push(crate::history::UndoStep {
                inverse,
                cursor_before: step.cursor_before,
            });
        }
        if !redo {
            new_steps.reverse();
        }
        let new_group = crate::history::UndoGroup { steps: new_steps };
        if redo {
            self.history.push_undo_group(new_group);
        } else {
            self.history.push_redo_group(new_group);
        }
        self.cursor = restore_cursor;
        self.fixup_cursor();
        self.update_desired_display_col();
    }
}

/// The [`MotionKind`] each [`Motion`] produces when fed into an
/// operator. Derived purely from the motion variant — it does not
/// depend on the cursor or buffer.
fn motion_kind(motion: Motion) -> MotionKind {
    match motion {
        Motion::WordEnd | Motion::LineEnd | Motion::FindChar { .. } => MotionKind::CharInclusive,
        Motion::Up
        | Motion::Down
        | Motion::HalfPageUp
        | Motion::HalfPageDown
        | Motion::FirstLine
        | Motion::LastLine => MotionKind::Linewise,
        Motion::Mark { linewise: true } => MotionKind::Linewise,
        _ => MotionKind::CharExclusive,
    }
}

fn order_range(a: Cursor, b: Cursor) -> ((usize, usize), (usize, usize)) {
    let (pa, pb) = ((a.row, a.col), (b.row, b.col));
    if pa <= pb { (pa, pb) } else { (pb, pa) }
}

fn first_nonblank(line: &str) -> usize {
    let mut col = 0;
    while col < line.len() {
        if cursor::class_at(line, col) != cursor::Class::Whitespace {
            return col;
        }
        col = buffer::next_grapheme_start(line, col);
    }
    0
}

fn leading_whitespace_end(line: &str) -> usize {
    let mut col = 0;
    while col < line.len() && cursor::class_at(line, col) == cursor::Class::Whitespace {
        col = buffer::next_grapheme_start(line, col);
    }
    col
}

fn ends_with_whitespace(line: &str) -> bool {
    if line.is_empty() {
        return false;
    }
    let col = buffer::prev_grapheme_start(line, line.len());
    cursor::class_at(line, col) == cursor::Class::Whitespace
}

fn join_separator(current: &str, next: &str, next_content_col: usize) -> &'static str {
    let next_content = &next[next_content_col..];
    if next_content.is_empty() || current.is_empty() || ends_with_whitespace(current) {
        ""
    } else if next_content.starts_with(')') {
        ""
    } else {
        " "
    }
}

fn extract_charwise(buf: &TextBuffer, start: (usize, usize), end: (usize, usize)) -> String {
    let max_row = buf.max_row();
    let sr = start.0.min(max_row);
    let er = end.0.min(max_row);
    let clamp = |line: &str, col: usize| buffer::align_to_grapheme_start(line, col.min(line.len()));
    if sr == er {
        let line = buf.line(sr);
        let line = line.as_ref();
        let sc = clamp(line, start.1);
        let ec = clamp(line, end.1).max(sc);
        return line[sc..ec].to_string();
    }
    let mut out = String::new();
    let first_line = buf.line(sr);
    let first_line = first_line.as_ref();
    let sc = clamp(first_line, start.1);
    out.push_str(&first_line[sc..]);
    out.push('\n');
    for r in (sr + 1)..er {
        let line = buf.line(r);
        out.push_str(line.as_ref());
        out.push('\n');
    }
    let last_line = buf.line(er);
    let last_line = last_line.as_ref();
    let ec = clamp(last_line, end.1);
    out.push_str(&last_line[..ec]);
    out
}

/// Returns `(lo, hi)` clamped byte indices for a block-selection span on
/// `line` between display-column anchors `c_lo` and `c_hi` (inclusive),
/// both given as byte offsets. `lo == hi` means the line falls outside
/// the selection on this row.
fn block_span(line: &str, c_lo: usize, c_hi: usize) -> (usize, usize) {
    let len = line.len();
    if c_lo >= len {
        return (len, len);
    }
    let lo = buffer::align_to_grapheme_start(line, c_lo);
    let hi = if c_hi >= len {
        len
    } else {
        let aligned = buffer::align_to_grapheme_start(line, c_hi);
        buffer::next_grapheme_start(line, aligned).min(len)
    };
    (lo, hi)
}

fn split_to_lines(text: &str) -> Vec<String> {
    text.split('\n').map(|s| s.to_string()).collect()
}

fn strip_single_line_breaks(text: &str) -> Cow<'_, str> {
    if !text.contains(['\n', '\r']) {
        return Cow::Borrowed(text);
    }
    let mut out = String::with_capacity(text.len());
    for ch in text.chars() {
        if !matches!(ch, '\n' | '\r') {
            out.push(ch);
        }
    }
    Cow::Owned(out)
}

fn single_line_prefix_len(text: &str, offset: usize) -> usize {
    text[..offset.min(text.len())]
        .chars()
        .filter(|ch| !matches!(ch, '\n' | '\r'))
        .map(char::len_utf8)
        .sum()
}

fn single_line_yank(yank: &Yank) -> Yank {
    if yank.lines.is_empty() {
        return Yank::default();
    }
    Yank {
        lines: vec![strip_single_line_breaks(&yank.lines.join("\n")).into_owned()],
        kind: match yank.kind {
            YankKind::Blockwise => YankKind::Charwise,
            kind => kind,
        },
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct SurroundPair {
    open: char,
    close: char,
}

fn surround_pair(delimiter: char) -> SurroundPair {
    match delimiter {
        '(' | ')' | 'b' => SurroundPair {
            open: '(',
            close: ')',
        },
        '[' | ']' => SurroundPair {
            open: '[',
            close: ']',
        },
        '{' | '}' | 'B' => SurroundPair {
            open: '{',
            close: '}',
        },
        '<' | '>' => SurroundPair {
            open: '<',
            close: '>',
        },
        _ => SurroundPair {
            open: delimiter,
            close: delimiter,
        },
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TextRunClass {
    Whitespace,
    Keyword,
    Other,
    BigWord,
}

impl TextRunClass {
    fn is_whitespace(self) -> bool {
        matches!(self, TextRunClass::Whitespace)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct TextRun {
    class: TextRunClass,
    start: usize,
    end: usize,
}

impl TextRun {
    fn is_whitespace(self) -> bool {
        self.class.is_whitespace()
    }
}

fn resolve_text_object(
    buf: &TextBuffer,
    cursor: Cursor,
    object: TextObject,
    count: u32,
) -> Option<((usize, usize), (usize, usize))> {
    let count = count.max(1) as usize;
    let text = buf.text();
    let offset = buf.rowcol_to_offset(cursor.row, cursor.col) as usize;
    let (start, end) = match object {
        TextObject::Word { around, big } => {
            Some(word_text_object_offsets(&text, offset, count, big, around))
        }
        TextObject::Quote { around, delimiter } => {
            quote_text_object_offsets(buf, cursor, delimiter, around)
        }
        TextObject::Pair { around, kind } => {
            pair_text_object_offsets(&text, offset, kind, around, count)
        }
        TextObject::Paragraph { around } => {
            paragraph_text_object_offsets(buf, cursor.row, around, count)
        }
    }?;
    Some((
        buf.offset_to_rowcol(start as u32),
        buf.offset_to_rowcol(end as u32),
    ))
}

/// Build an `ip` / `ap` text object around the line `row`. Paragraphs
/// are runs of consecutive non-blank *or* blank lines; `around`
/// extends the range with the trailing blank (or non-blank) block.
fn paragraph_text_object_offsets(
    buf: &TextBuffer,
    row: usize,
    around: bool,
    count: usize,
) -> Option<(usize, usize)> {
    let line_count = buf.line_count();
    if line_count == 0 {
        return None;
    }
    let max_row = line_count - 1;
    let row = row.min(max_row);
    let starts_blank = line_is_blank(buf, row);

    let mut start = row;
    while start > 0 && line_is_blank(buf, start - 1) == starts_blank {
        start -= 1;
    }
    let mut end = row;
    while end < max_row && line_is_blank(buf, end + 1) == starts_blank {
        end += 1;
    }

    // Extend to `count` paragraphs (same-class + trailing opposite-class runs).
    let mut remaining = count.saturating_sub(1);
    while remaining > 0 && end < max_row {
        let next_class = line_is_blank(buf, end + 1);
        let mut r = end + 1;
        while r < max_row && line_is_blank(buf, r + 1) == next_class {
            r += 1;
        }
        end = r;
        remaining -= 1;
    }

    if around {
        // Add the following opposite-class run. If there is none, fall
        // back to the preceding opposite-class run.
        if end < max_row && line_is_blank(buf, end + 1) != starts_blank {
            end += 1;
            while end < max_row && line_is_blank(buf, end + 1) != starts_blank {
                end += 1;
            }
        } else if start > 0 && line_is_blank(buf, start - 1) != starts_blank {
            start -= 1;
            while start > 0 && line_is_blank(buf, start - 1) != starts_blank {
                start -= 1;
            }
        }
    }

    let start_off = buf.line_start(start) as usize;
    // Include each selected row's trailing newline so `dap`/`dip`
    // behave linewise (consuming whole rows including terminators).
    let end_off = if end + 1 < line_count {
        buf.line_start(end + 1) as usize
    } else {
        buf.len()
    };
    Some((start_off, end_off))
}

fn line_is_blank(buf: &TextBuffer, row: usize) -> bool {
    buf.line(row).as_ref().trim().is_empty()
}

fn is_valid_mark(ch: char) -> bool {
    ch.is_ascii_lowercase()
}

fn case_transform_text(transform: CaseTransform, text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for ch in text.chars() {
        match transform {
            CaseTransform::Upper => out.extend(ch.to_uppercase()),
            CaseTransform::Lower => out.extend(ch.to_lowercase()),
            CaseTransform::Toggle => {
                if ch.is_uppercase() {
                    out.extend(ch.to_lowercase());
                } else if ch.is_lowercase() {
                    out.extend(ch.to_uppercase());
                } else {
                    out.push(ch);
                }
            }
        }
    }
    out
}

/// Vim's `<C-a>` / `<C-x>` scans for a non-negative decimal run either
/// at the cursor or rightward on the same line. Negative sign handling
/// matches nvim: if the digit run is immediately preceded by `-`, the
/// sign is included.
fn find_number_from(line: &str, col: usize) -> Option<(usize, usize, i64)> {
    if line.is_empty() {
        return None;
    }
    let bytes = line.as_bytes();
    let mut i = col.min(line.len());
    while i < bytes.len() && !bytes[i].is_ascii_digit() {
        i += 1;
    }
    if i >= bytes.len() {
        return None;
    }
    let mut end = i;
    while end < bytes.len() && bytes[end].is_ascii_digit() {
        end += 1;
    }
    let mut start = i;
    if start > 0 && bytes[start - 1] == b'-' {
        start -= 1;
    }
    let value: i64 = line[start..end].parse().ok()?;
    Some((start, end, value))
}

fn word_text_object_offsets(
    text: &str,
    offset: usize,
    count: usize,
    big: bool,
    around: bool,
) -> (usize, usize) {
    let offset = offset.min(text.len());
    if text.is_empty() || offset == text.len() {
        return (offset, offset);
    }

    let runs = word_runs(text, big);
    let Some(idx) = runs
        .iter()
        .position(|run| run.start <= offset && offset < run.end)
    else {
        return (offset, offset);
    };

    if around {
        around_word_offsets(&runs, idx, count)
    } else {
        inner_word_offsets(&runs, idx, count)
    }
}

fn word_runs(text: &str, big: bool) -> Vec<TextRun> {
    let mut runs: Vec<TextRun> = Vec::new();
    for (start, grapheme) in text.grapheme_indices(true) {
        let class = word_run_class(grapheme, big);
        let end = start + grapheme.len();
        if let Some(last) = runs.last_mut() {
            if last.class == class {
                last.end = end;
                continue;
            }
        }
        runs.push(TextRun { class, start, end });
    }
    runs
}

fn word_run_class(grapheme: &str, big: bool) -> TextRunClass {
    match cursor::class_of(grapheme) {
        cursor::Class::Whitespace => TextRunClass::Whitespace,
        cursor::Class::Keyword if big => TextRunClass::BigWord,
        cursor::Class::Other if big => TextRunClass::BigWord,
        cursor::Class::Keyword => TextRunClass::Keyword,
        cursor::Class::Other => TextRunClass::Other,
    }
}

fn inner_word_offsets(runs: &[TextRun], idx: usize, count: usize) -> (usize, usize) {
    let start = runs[idx].start;
    let mut end_idx = idx;
    for _ in 1..count.max(1) {
        let Some(next_idx) = next_run_idx(runs, end_idx) else {
            break;
        };
        end_idx = next_idx;
    }
    (start, runs[end_idx].end)
}

fn around_word_offsets(runs: &[TextRun], idx: usize, count: usize) -> (usize, usize) {
    let count = count.max(1);
    if runs[idx].is_whitespace() {
        if let Some(first_word_idx) = next_word_run_idx(runs, idx) {
            let last_word_idx = advance_word_runs(runs, first_word_idx, count - 1);
            return (runs[idx].start, runs[last_word_idx].end);
        }
        if let Some(last_word_idx) = prev_word_run_idx(runs, idx) {
            let first_word_idx = retreat_word_runs(runs, last_word_idx, count - 1);
            return (runs[first_word_idx].start, runs[idx].end);
        }
        return (runs[idx].start, runs[idx].end);
    }

    let first_word_idx = idx;
    let last_word_idx = advance_word_runs(runs, first_word_idx, count - 1);
    let mut start = runs[first_word_idx].start;
    let end = with_trailing_whitespace(runs, last_word_idx);
    if end == runs[last_word_idx].end {
        if let Some(ws_before_idx) = immediate_whitespace_before(runs, first_word_idx) {
            start = runs[ws_before_idx].start;
        }
    }
    (start, end)
}

fn next_run_idx(runs: &[TextRun], idx: usize) -> Option<usize> {
    idx.checked_add(1).filter(|i| *i < runs.len())
}

fn next_word_run_idx(runs: &[TextRun], idx: usize) -> Option<usize> {
    ((idx + 1)..runs.len()).find(|i| !runs[*i].is_whitespace())
}

fn prev_word_run_idx(runs: &[TextRun], idx: usize) -> Option<usize> {
    (0..idx).rev().find(|i| !runs[*i].is_whitespace())
}

fn advance_word_runs(runs: &[TextRun], idx: usize, additional_words: usize) -> usize {
    let mut word_idx = idx;
    for _ in 0..additional_words {
        let Some(next_idx) = next_word_run_idx(runs, word_idx) else {
            break;
        };
        word_idx = next_idx;
    }
    word_idx
}

fn retreat_word_runs(runs: &[TextRun], idx: usize, additional_words: usize) -> usize {
    let mut word_idx = idx;
    for _ in 0..additional_words {
        let Some(prev_idx) = prev_word_run_idx(runs, word_idx) else {
            break;
        };
        word_idx = prev_idx;
    }
    word_idx
}

fn immediate_whitespace_before(runs: &[TextRun], idx: usize) -> Option<usize> {
    idx.checked_sub(1).filter(|i| runs[*i].is_whitespace())
}

fn with_trailing_whitespace(runs: &[TextRun], idx: usize) -> usize {
    if let Some(ws_idx) = next_run_idx(runs, idx).filter(|i| runs[*i].is_whitespace()) {
        runs[ws_idx].end
    } else {
        runs[idx].end
    }
}

fn quote_text_object_offsets(
    buf: &TextBuffer,
    cursor: Cursor,
    delimiter: char,
    around: bool,
) -> Option<(usize, usize)> {
    let line = buf.line(cursor.row);
    let line = line.as_ref();
    let line_start = buf.line_start(cursor.row) as usize;
    let col = cursor.col.min(line.len());
    let (start, end) = quote_pair_in_line(line, col, delimiter)?;
    if around {
        let mut start = start;
        let mut end = end;
        let trailing = extend_whitespace_forward(line, end);
        if trailing > end {
            end = trailing;
        } else {
            start = extend_whitespace_backward(line, start);
        }
        Some((line_start + start, line_start + end))
    } else {
        let width = delimiter.len_utf8();
        Some((line_start + start + width, line_start + end - width))
    }
}

fn quote_pair_in_line(line: &str, col: usize, delimiter: char) -> Option<(usize, usize)> {
    let mut open: Option<usize> = None;
    let mut pairs = Vec::new();
    for (idx, ch) in line.char_indices() {
        if ch != delimiter || is_escaped_delimiter(line, idx) {
            continue;
        }
        if let Some(open_idx) = open.take() {
            pairs.push((open_idx, idx + ch.len_utf8()));
        } else {
            open = Some(idx);
        }
    }

    let col = col.min(line.len());
    pairs.into_iter().find(|(start, end)| {
        let close_start = end.saturating_sub(delimiter.len_utf8());
        *start <= col && col <= close_start
    })
}

fn is_escaped_delimiter(line: &str, idx: usize) -> bool {
    let bytes = line.as_bytes();
    let mut slash_count = 0;
    let mut i = idx;
    while i > 0 {
        i -= 1;
        if bytes[i] != b'\\' {
            break;
        }
        slash_count += 1;
    }
    slash_count % 2 == 1
}

fn extend_whitespace_forward(line: &str, mut col: usize) -> usize {
    while col < line.len() && cursor::class_at(line, col) == cursor::Class::Whitespace {
        col = buffer::next_grapheme_start(line, col);
    }
    col
}

fn extend_whitespace_backward(line: &str, mut col: usize) -> usize {
    while col > 0 {
        let prev = buffer::prev_grapheme_start(line, col);
        if cursor::class_at(line, prev) != cursor::Class::Whitespace {
            break;
        }
        col = prev;
    }
    col
}

fn pair_text_object_offsets(
    text: &str,
    offset: usize,
    kind: PairKind,
    around: bool,
    count: usize,
) -> Option<(usize, usize)> {
    let (open, close) = pair_delimiters(kind);
    let mut stack = Vec::new();
    let mut containing = Vec::new();

    for (idx, ch) in text.char_indices() {
        if ch == open {
            stack.push(idx);
        } else if ch == close {
            if let Some(start) = stack.pop() {
                if start <= offset && offset <= idx {
                    containing.push((start, idx + ch.len_utf8()));
                }
            }
        }
    }

    let (start, end) = *containing.get(count.checked_sub(1)?)?;
    if around {
        Some((start, end))
    } else {
        Some((start + open.len_utf8(), end - close.len_utf8()))
    }
}

fn pair_delimiters(kind: PairKind) -> (char, char) {
    match kind {
        PairKind::Paren => ('(', ')'),
        PairKind::Bracket => ('[', ']'),
        PairKind::Brace => ('{', '}'),
        PairKind::Angle => ('<', '>'),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use extui::{
        Cell, Buffer, Grid, Rect,
        event::{KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers},
    };

    fn mk(code: KeyCode, mods: KeyModifiers) -> KeyEvent {
        KeyEvent {
            code,
            modifiers: mods,
            kind: KeyEventKind::Press,
            state: KeyEventState::empty(),
        }
    }

    fn key_char(c: char) -> KeyEvent {
        let mods = if c.is_ascii_uppercase() {
            KeyModifiers::SHIFT
        } else {
            KeyModifiers::NONE
        };
        mk(KeyCode::Char(c), mods)
    }

    fn key_ctrl(c: char) -> KeyEvent {
        mk(KeyCode::Char(c), KeyModifiers::CONTROL)
    }

    fn key_alt(c: char) -> KeyEvent {
        mk(KeyCode::Char(c), KeyModifiers::ALT)
    }

    fn key_esc() -> KeyEvent {
        mk(KeyCode::Esc, KeyModifiers::NONE)
    }

    fn key_tab() -> KeyEvent {
        mk(KeyCode::Tab, KeyModifiers::NONE)
    }

    fn feed(ed: &mut Editor, keys: &[KeyEvent]) {
        for k in keys {
            ed.send_key(k);
        }
    }

    fn chars(s: &str) -> Vec<KeyEvent> {
        s.chars().map(key_char).collect()
    }

    fn cell_text(buf: &Grid, cell: Cell) -> String {
        cell.text_inline()
            .or_else(|| {
                buf.handle_text(cell)
                    .and_then(|bytes| std::str::from_utf8(bytes).ok())
            })
            .unwrap_or("")
            .to_string()
    }

    fn render_rows(ed: &mut Editor, rect: Rect) -> Vec<String> {
        let mut db = Buffer::new(rect.w.max(1), rect.h.max(1));
        ed.render(rect, &mut db);
        let buf = db.current();
        (0..rect.h as usize)
            .map(|y| {
                (0..rect.w as usize)
                    .map(|x| cell_text(buf, buf.cells()[y * rect.w as usize + x]))
                    .collect::<String>()
            })
            .collect()
    }

    #[test]
    fn insert_and_escape_types_text() {
        let mut ed = Editor::new();
        feed(&mut ed, &[key_char('i')]);
        feed(&mut ed, &chars("hello"));
        feed(&mut ed, &[key_esc()]);
        assert_eq!(ed.text(), "hello");
        assert_eq!(ed.cursor(), (0, 4));
        assert_eq!(ed.mode(), Mode::Normal);
    }

    #[test]
    fn replace_range_swaps_word_and_moves_cursor() {
        let mut ed = Editor::new();
        ed.set_lines("teh cat");
        ed.replace_range(Span::new(0, 3), "the");
        assert_eq!(ed.text(), "the cat");
        // Cursor lands one past the inserted text.
        assert_eq!(ed.cursor_offset(), 3);
    }

    #[test]
    fn replace_range_grows_and_reverts_via_undo() {
        let mut ed = Editor::new();
        ed.set_lines("teh");
        // Insert mode lets the cursor rest one past the inserted text, as it
        // does during host-driven spell correction.
        feed(&mut ed, &[key_char('i')]);
        ed.replace_range(Span::new(0, 3), "thumb");
        assert_eq!(ed.text(), "thumb");
        assert_eq!(ed.cursor_offset(), 5);
        feed(&mut ed, &[key_esc()]);
        feed(&mut ed, &[key_char('u')]);
        assert_eq!(ed.text(), "teh");
    }

    #[test]
    fn replace_range_clamps_out_of_bounds_span() {
        let mut ed = Editor::new();
        ed.set_lines("hi");
        ed.replace_range(Span::new(1, 99), "ello there");
        assert_eq!(ed.text(), "hello there");
    }

    #[test]
    fn tab_in_insert_mode_defaults_to_four_spaces() {
        let mut ed = Editor::new();
        feed(
            &mut ed,
            &[key_char('i'), key_tab(), key_char('x'), key_esc()],
        );
        assert_eq!(ed.text(), "    x");
        assert_eq!(ed.cursor(), (0, 4));
    }

    #[test]
    fn tab_settings_can_restore_hard_tabs() {
        let mut ed = Editor::new();
        ed.set_tab_settings(TabSettings {
            expandtab: false,
            tabstop: 8,
            softtabstop: 4,
            shiftwidth: 4,
        });
        feed(
            &mut ed,
            &[key_char('i'), key_tab(), key_char('x'), key_esc()],
        );
        assert_eq!(ed.text(), "\tx");
        assert_eq!(ed.cursor(), (0, 1));
        assert_eq!(ed.cursor_display(), (0, 8));
    }

    #[test]
    fn dd_deletes_current_line() {
        let mut ed = Editor::new();
        ed.set_lines("one\ntwo\nthree");
        feed(&mut ed, &[key_char('j'), key_char('d'), key_char('d')]);
        assert_eq!(ed.text(), "one\nthree");
        assert_eq!(ed.cursor(), (1, 0));
    }

    #[test]
    fn dw_deletes_word() {
        let mut ed = Editor::new();
        ed.set_lines("foo bar baz");
        feed(&mut ed, &[key_char('d'), key_char('w')]);
        assert_eq!(ed.text(), "bar baz");
        assert_eq!(ed.cursor(), (0, 0));
    }

    #[test]
    fn yy_then_p_pastes_line_below() {
        let mut ed = Editor::new();
        ed.set_lines("foo\nbar");
        feed(&mut ed, &[key_char('y'), key_char('y'), key_char('p')]);
        assert_eq!(ed.text(), "foo\nfoo\nbar");
        assert_eq!(ed.cursor(), (1, 0));
    }

    #[test]
    fn x_deletes_char_under_cursor() {
        let mut ed = Editor::new();
        ed.set_lines("abc");
        feed(&mut ed, &[key_char('l'), key_char('x')]);
        assert_eq!(ed.text(), "ac");
        assert_eq!(ed.cursor(), (0, 1));
    }

    #[test]
    fn r_replaces_character() {
        let mut ed = Editor::new();
        ed.set_lines("abc");
        feed(&mut ed, &[key_char('l'), key_char('r'), key_char('X')]);
        assert_eq!(ed.text(), "aXc");
        assert_eq!(ed.cursor(), (0, 1));
    }

    #[test]
    fn count_prefix_applies() {
        let mut ed = Editor::new();
        ed.set_lines("abcdef");
        feed(&mut ed, &[key_char('3'), key_char('l')]);
        assert_eq!(ed.cursor(), (0, 3));
    }

    #[test]
    fn set_theme_marks_editor_dirty() {
        let mut ed = Editor::new();
        assert!(ed.poll_updates());
        assert!(!ed.poll_updates());
        let alt = EditorTheme {
            name: "Alt",
            ..EditorTheme::default()
        };
        ed.set_theme(alt);
        assert!(ed.poll_updates());
        assert_eq!(ed.theme().name, "Alt");
    }

    #[test]
    fn gg_goes_to_first_line() {
        let mut ed = Editor::new();
        ed.set_lines("one\ntwo\nthree");
        feed(&mut ed, &[key_char('G'), key_char('g'), key_char('g')]);
        assert_eq!(ed.cursor(), (0, 0));
    }

    #[test]
    fn a_moves_cursor_right_then_insert() {
        let mut ed = Editor::new();
        ed.set_lines("abc");
        feed(&mut ed, &[key_char('a'), key_char('X'), key_esc()]);
        assert_eq!(ed.text(), "aXbc");
    }

    #[test]
    fn open_line_below_creates_blank_line() {
        let mut ed = Editor::new();
        ed.set_lines("foo\nbar");
        feed(&mut ed, &[key_char('o'), key_char('X'), key_esc()]);
        assert_eq!(ed.text(), "foo\nX\nbar");
    }

    #[test]
    fn cw_acts_like_ce() {
        let mut ed = Editor::new();
        ed.set_lines("foo bar");
        feed(
            &mut ed,
            &[key_char('c'), key_char('w'), key_char('X'), key_esc()],
        );
        assert_eq!(ed.text(), "X bar");
    }

    #[test]
    fn visual_char_delete() {
        let mut ed = Editor::new();
        ed.set_lines("hello world");
        feed(
            &mut ed,
            &[
                key_char('v'),
                key_char('l'),
                key_char('l'),
                key_char('l'),
                key_char('d'),
            ],
        );
        assert_eq!(ed.text(), "o world");
        assert_eq!(ed.mode(), Mode::Normal);
    }

    #[test]
    fn visual_line_yank_and_paste() {
        let mut ed = Editor::new();
        ed.set_lines("foo\nbar\nbaz");
        feed(
            &mut ed,
            &[key_char('V'), key_char('y'), key_char('j'), key_char('p')],
        );
        assert_eq!(ed.text(), "foo\nbar\nfoo\nbaz");
    }

    #[test]
    fn visual_block_delete_rectangle() {
        let mut ed = Editor::new();
        ed.set_lines("hello\nhi\nhelper");
        feed(
            &mut ed,
            &[
                key_ctrl('v'),
                key_char('j'),
                key_char('j'),
                key_char('l'),
                key_char('d'),
            ],
        );
        assert_eq!(ed.text(), "llo\n\nlper");
    }

    #[test]
    fn visual_block_change_replays_insert_on_each_line() {
        let mut ed = Editor::new();
        ed.set_lines("abcd\nefgh\nijkl");
        feed(
            &mut ed,
            &[
                key_ctrl('v'),
                key_char('j'),
                key_char('j'),
                key_char('l'),
                key_char('c'),
            ],
        );
        feed(&mut ed, &chars("XY"));
        feed(&mut ed, &[key_esc()]);
        assert_eq!(ed.text(), "XYcd\nXYgh\nXYkl");
        assert_eq!(ed.mode(), Mode::Normal);
    }

    #[test]
    fn visual_block_change_replays_insert_on_short_lines() {
        let mut ed = Editor::new();
        ed.set_lines("hello\nhi\nhelper");
        feed(
            &mut ed,
            &[
                key_ctrl('v'),
                key_char('j'),
                key_char('j'),
                key_char('l'),
                key_char('c'),
            ],
        );
        feed(&mut ed, &chars("Z"));
        feed(&mut ed, &[key_esc()]);
        assert_eq!(ed.text(), "Zllo\nZ\nZlper");
        assert_eq!(ed.mode(), Mode::Normal);
    }

    #[test]
    fn visual_escape_returns_to_normal() {
        let mut ed = Editor::new();
        ed.set_lines("abc");
        feed(&mut ed, &[key_char('v'), key_char('l'), key_esc()]);
        assert_eq!(ed.mode(), Mode::Normal);
    }

    #[test]
    fn undo_reverts_insert_session() {
        let mut ed = Editor::new();
        ed.set_lines("foo");
        feed(&mut ed, &[key_char('A')]);
        feed(&mut ed, &chars("bar"));
        feed(&mut ed, &[key_esc()]);
        assert_eq!(ed.text(), "foobar");
        feed(&mut ed, &[key_char('u')]);
        assert_eq!(ed.text(), "foo");
    }

    #[test]
    fn undo_reverts_dd() {
        let mut ed = Editor::new();
        ed.set_lines("one\ntwo\nthree");
        feed(&mut ed, &[key_char('d'), key_char('d'), key_char('u')]);
        assert_eq!(ed.text(), "one\ntwo\nthree");
    }

    #[test]
    fn redo_restores_undone_change() {
        let mut ed = Editor::new();
        ed.set_lines("a");
        feed(&mut ed, &[key_char('A')]);
        feed(&mut ed, &chars("bc"));
        feed(&mut ed, &[key_esc()]);
        assert_eq!(ed.text(), "abc");
        feed(&mut ed, &[key_char('u')]);
        assert_eq!(ed.text(), "a");
        feed(&mut ed, &[key_ctrl('r')]);
        assert_eq!(ed.text(), "abc");
    }

    #[test]
    fn desired_height_clamps_to_bounds() {
        let mut ed = Editor::new();
        ed.set_height_bounds(3, 10);
        assert_eq!(ed.desired_height(), 3);
        ed.set_lines("a\nb\nc\nd\ne");
        assert_eq!(ed.desired_height(), 5);
        ed.set_lines(&("x\n".repeat(20)));
        assert_eq!(ed.desired_height(), 10);
    }

    #[test]
    fn single_line_mode_flattens_text_and_height() {
        let mut ed = Editor::new();
        ed.set_height_bounds(2, 10);
        ed.set_single_line(true);
        ed.set_lines("foo\r\nbar\nbaz");
        assert_eq!(ed.text(), "foobarbaz");
        assert_eq!(ed.desired_height(), 1);
    }

    #[test]
    fn single_line_mode_blocks_newline_commands() {
        let mut ed = Editor::new();
        ed.set_single_line(true);
        ed.set_lines("foo");
        feed(
            &mut ed,
            &[
                key_char('A'),
                mk(KeyCode::Enter, KeyModifiers::NONE),
                key_char('X'),
                key_esc(),
            ],
        );
        assert_eq!(ed.text(), "fooX");
        assert_eq!(ed.cursor(), (0, 3));

        feed(&mut ed, &[key_char('o')]);
        assert_eq!(ed.text(), "fooX");
        assert_eq!(ed.mode(), Mode::Normal);
    }

    #[test]
    fn enabling_single_line_flattens_existing_buffer_and_linewise_yank() {
        let mut ed = Editor::new();
        ed.set_lines("foo\nbar");
        feed(&mut ed, &[key_char('V'), key_char('j'), key_char('y')]);

        ed.set_single_line(true);
        assert_eq!(ed.text(), "foobar");

        feed(&mut ed, &[key_char('p')]);
        assert_eq!(ed.text(), "foobarfoobar");
        assert_eq!(ed.cursor(), (0, 6));
    }

    #[test]
    fn scroll_follows_cursor_past_viewport() {
        let mut ed = Editor::new();
        ed.set_height_bounds(1, 3);
        ed.set_lines("l0\nl1\nl2\nl3\nl4\nl5");
        let mut db = Buffer::new(10, 10);
        let rect = Rect {
            x: 0,
            y: 0,
            w: 10,
            h: 3,
        };
        ed.render(rect, &mut db);
        assert_eq!(ed.scroll_offset, 0);
        feed(&mut ed, &[key_char('4'), key_char('G')]);
        ed.render(rect, &mut db);
        assert!(
            ed.scroll_offset > 0,
            "expected scroll, got {}",
            ed.scroll_offset
        );
        assert_eq!(ed.scroll_offset, 1);
    }

    #[test]
    fn grown_viewport_clears_stale_scroll_wrap() {
        let mut ed = Editor::new();
        ed.set_wrap(true);
        ed.resize(4);
        ed.set_height_bounds(1, 8);
        // 12 chars over width 4 = 3 visual rows.
        ed.set_lines("abcdefghijkl");
        feed(&mut ed, &[key_char('G'), key_char('$')]);

        // A 2-row viewport forces a scroll to keep the cursor visible.
        let small = Rect {
            x: 0,
            y: 0,
            w: 4,
            h: 2,
        };
        render_rows(&mut ed, small);
        assert!(ed.scroll_offset > 0, "small viewport should scroll");

        // The host grows the editor to its desired height; all rows now fit, so
        // the scroll must reset to keep the top row visible.
        let grown = Rect {
            x: 0,
            y: 0,
            w: 4,
            h: ed.desired_height(),
        };
        let rows = render_rows(&mut ed, grown);
        assert_eq!(ed.scroll_offset, 0, "grown viewport should reset scroll");
        assert_eq!(
            rows,
            vec!["abcd".to_string(), "efgh".to_string(), "ijkl".to_string()]
        );
    }

    #[test]
    fn grown_viewport_clears_stale_scroll_nowrap() {
        let mut ed = Editor::new();
        ed.set_height_bounds(1, 8);
        ed.set_lines("l0\nl1\nl2\nl3\nl4");
        feed(&mut ed, &[key_char('G')]);

        let small = Rect {
            x: 0,
            y: 0,
            w: 10,
            h: 2,
        };
        render_rows(&mut ed, small);
        assert!(ed.scroll_offset > 0, "small viewport should scroll");

        let grown = Rect {
            x: 0,
            y: 0,
            w: 10,
            h: ed.desired_height(),
        };
        let rows = render_rows(&mut ed, grown);
        assert_eq!(ed.scroll_offset, 0, "grown viewport should reset scroll");
        assert_eq!(&rows[0], "l0        ");
    }

    #[test]
    fn single_line_mode_never_scrolls() {
        let mut ed = Editor::new();
        ed.set_single_line(true);
        ed.set_lines("hello\nworld");

        let mut db = Buffer::new(10, 10);
        let rect = Rect {
            x: 0,
            y: 0,
            w: 10,
            h: 3,
        };
        ed.render(rect, &mut db);

        assert_eq!(ed.text(), "helloworld");
        assert_eq!(ed.scroll_offset, 0);
    }

    #[test]
    fn nowrap_horizontally_scrolls_to_keep_cursor_visible() {
        let mut ed = Editor::new();
        ed.set_lines("0123456789abcdef");
        feed(&mut ed, &[key_char('$')]);
        let rect = Rect {
            x: 0,
            y: 0,
            w: 5,
            h: 1,
        };
        let rows = render_rows(&mut ed, rect);
        assert_eq!(ed.horizontal_scroll, 11);
        assert_eq!(rows, vec!["bcdef".to_string()]);
    }

    #[test]
    fn wrap_expands_rendered_rows_and_height() {
        let mut ed = Editor::new();
        ed.resize(4);
        ed.set_wrap(true);
        ed.set_lines("abcdef");
        assert_eq!(ed.desired_height(), 2);
        let rect = Rect {
            x: 0,
            y: 0,
            w: 4,
            h: 2,
        };
        let rows = render_rows(&mut ed, rect);
        assert_eq!(rows, vec!["abcd".to_string(), "ef  ".to_string()]);
    }

    #[test]
    fn wrap_scrolls_by_visual_row_to_keep_cursor_visible() {
        let mut ed = Editor::new();
        ed.set_wrap(true);
        ed.set_lines("abcdefghijkl");
        feed(&mut ed, &[key_char('$')]);
        let rect = Rect {
            x: 0,
            y: 0,
            w: 4,
            h: 2,
        };
        let rows = render_rows(&mut ed, rect);
        assert_eq!(ed.scroll_offset, 1);
        assert_eq!(rows, vec!["efgh".to_string(), "ijkl".to_string()]);
    }

    #[test]
    fn gj_moves_by_wrapped_screen_line() {
        let mut ed = Editor::new();
        ed.resize(4);
        ed.set_wrap(true);
        ed.set_lines("abcdefghij");
        feed(
            &mut ed,
            &[key_char('l'), key_char('l'), key_char('g'), key_char('j')],
        );
        assert_eq!(ed.cursor_display(), (0, 6));
    }

    #[test]
    fn gk_moves_up_by_wrapped_screen_line() {
        let mut ed = Editor::new();
        ed.resize(4);
        ed.set_wrap(true);
        ed.set_lines("abcdefghij");
        feed(
            &mut ed,
            &[key_char('6'), key_char('l'), key_char('g'), key_char('k')],
        );
        assert_eq!(ed.cursor_display(), (0, 2));
    }

    #[test]
    fn gj_gk_preserve_screen_column_across_wrapped_lines() {
        let mut ed = Editor::new();
        ed.resize(4);
        ed.set_wrap(true);
        ed.set_lines("abcdefghij\nxy");
        feed(
            &mut ed,
            &[
                key_char('l'),
                key_char('l'),
                key_char('g'),
                key_char('j'),
                key_char('g'),
                key_char('j'),
                key_char('g'),
                key_char('j'),
            ],
        );
        assert_eq!(ed.cursor(), (1, 1));
        feed(&mut ed, &[key_char('g'), key_char('k')]);
        assert_eq!(ed.cursor_display(), (0, 9));
    }

    #[test]
    fn new_edit_after_undo_clears_redo() {
        let mut ed = Editor::new();
        ed.set_lines("a");
        feed(&mut ed, &[key_char('A')]);
        feed(&mut ed, &chars("b"));
        feed(&mut ed, &[key_esc()]);
        feed(&mut ed, &[key_char('u')]);
        assert_eq!(ed.text(), "a");
        feed(&mut ed, &[key_char('A')]);
        feed(&mut ed, &chars("X"));
        feed(&mut ed, &[key_esc()]);
        assert_eq!(ed.text(), "aX");
        feed(&mut ed, &[key_ctrl('r')]);
        assert_eq!(ed.text(), "aX");
    }

    #[test]
    fn visual_upper_s_surrounds_char_selection() {
        let mut ed = Editor::new();
        ed.set_lines("hello");
        feed(&mut ed, &[key_char('v'), key_char('l'), key_char('l')]);
        feed(&mut ed, &[key_char('S'), key_char('b')]);
        assert_eq!(ed.text(), "(hel)lo");
        assert_eq!(ed.cursor(), (0, 0));
        assert_eq!(ed.mode(), Mode::Normal);
    }

    #[test]
    fn visual_upper_s_surrounds_char_selection_with_arbitrary_char() {
        let mut ed = Editor::new();
        ed.set_lines("hello");
        feed(&mut ed, &[key_char('v'), key_char('l'), key_char('l')]);
        feed(&mut ed, &[key_char('S'), key_char('*')]);
        assert_eq!(ed.text(), "*hel*lo");
        assert_eq!(ed.cursor(), (0, 0));
        assert_eq!(ed.mode(), Mode::Normal);
    }

    #[test]
    fn visual_upper_s_surrounds_block_selection_per_line() {
        let mut ed = Editor::new();
        ed.set_lines("abcd\nefgh\nijkl");
        feed(
            &mut ed,
            &[
                key_ctrl('v'),
                key_char('j'),
                key_char('l'),
                key_char('S'),
                key_char('b'),
            ],
        );
        assert_eq!(ed.text(), "(ab)cd\n(ef)gh\nijkl");
        assert_eq!(ed.cursor(), (0, 0));
        assert_eq!(ed.mode(), Mode::Normal);
    }

    #[test]
    fn visual_lower_s_defaults_to_change_selection() {
        let mut ed = Editor::new();
        ed.set_lines("hello");
        feed(&mut ed, &[key_char('v'), key_char('l'), key_char('s')]);
        feed(&mut ed, &[key_char('X'), key_esc()]);
        assert_eq!(ed.text(), "Xllo");
        assert_eq!(ed.mode(), Mode::Normal);
    }

    #[test]
    fn visual_lower_s_can_be_configured_for_surround() {
        let mut ed = Editor::with_bindings(bindings::vim(bindings::VimOptions {
            use_lower_s_for_visual_surround: true,
        }));
        ed.set_lines("hello");
        feed(&mut ed, &[key_char('v'), key_char('l'), key_char('l')]);
        feed(&mut ed, &[key_char('s'), key_char('b')]);
        assert_eq!(ed.text(), "(hel)lo");
        assert_eq!(ed.cursor(), (0, 0));
        assert_eq!(ed.mode(), Mode::Normal);
    }

    #[test]
    fn visual_lower_s_can_use_arbitrary_char_when_configured_for_surround() {
        let mut ed = Editor::with_bindings(bindings::vim(bindings::VimOptions {
            use_lower_s_for_visual_surround: true,
        }));
        ed.set_lines("hello");
        feed(&mut ed, &[key_char('v'), key_char('l'), key_char('l')]);
        feed(&mut ed, &[key_char('s'), key_char('x')]);
        assert_eq!(ed.text(), "xhelxlo");
        assert_eq!(ed.cursor(), (0, 0));
        assert_eq!(ed.mode(), Mode::Normal);
    }

    #[test]
    fn nano_starts_and_resets_in_insert_mode() {
        let mut ed = Editor::with_bindings(bindings::nano());
        assert_eq!(ed.mode(), Mode::Insert);

        feed(&mut ed, &chars("hi"));
        assert_eq!(ed.text(), "hi");

        feed(
            &mut ed,
            &[
                key_alt('a'),
                mk(KeyCode::Right, KeyModifiers::NONE),
                key_esc(),
            ],
        );
        assert_eq!(ed.mode(), Mode::Insert);

        ed.set_lines("abc");
        assert_eq!(ed.mode(), Mode::Insert);
    }

    #[test]
    fn nano_line_cut_copy_and_paste_use_insert_mode() {
        let mut ed = Editor::with_bindings(bindings::nano());
        ed.set_lines("one\ntwo");

        feed(&mut ed, &[key_ctrl('k')]);
        assert_eq!(ed.text(), "two");
        assert_eq!(ed.mode(), Mode::Insert);

        ed.set_lines("one\ntwo");
        feed(&mut ed, &[key_alt('6'), key_ctrl('u')]);
        assert_eq!(ed.text(), "one\none\ntwo");
        assert_eq!(ed.mode(), Mode::Insert);
    }

    #[test]
    fn emacs_region_cut_returns_to_insert_mode() {
        let mut ed = Editor::with_bindings(bindings::emacs());
        assert_eq!(ed.mode(), Mode::Insert);
        ed.set_lines("hello");

        feed(
            &mut ed,
            &[
                mk(KeyCode::Char(' '), KeyModifiers::CONTROL),
                mk(KeyCode::Right, KeyModifiers::NONE),
                mk(KeyCode::Right, KeyModifiers::NONE),
                key_ctrl('w'),
            ],
        );
        assert_eq!(ed.text(), "lo");
        assert_eq!(ed.mode(), Mode::Insert);
    }

    #[test]
    fn emacs_undo_and_redo_chords_work() {
        let mut ed = Editor::with_bindings(bindings::emacs());
        feed(&mut ed, &chars("ab"));
        assert_eq!(ed.text(), "ab");

        feed(&mut ed, &[key_ctrl('x'), key_char('u')]);
        assert_eq!(ed.text(), "");
        assert_eq!(ed.mode(), Mode::Insert);

        feed(&mut ed, &[key_ctrl('x'), key_char('r')]);
        assert_eq!(ed.text(), "ab");
        assert_eq!(ed.mode(), Mode::Insert);
    }

    #[test]
    fn undo_reverts_visual_surround() {
        let mut ed = Editor::new();
        ed.set_lines("hello");
        feed(&mut ed, &[key_char('v'), key_char('l'), key_char('l')]);
        feed(&mut ed, &[key_char('S'), key_char('b')]);
        assert_eq!(ed.text(), "(hel)lo");
        feed(&mut ed, &[key_char('u')]);
        assert_eq!(ed.text(), "hello");
        assert_eq!(ed.cursor(), (0, 2));
    }

    #[test]
    fn big_d_deletes_to_eol() {
        let mut ed = Editor::new();
        ed.set_lines("hello world\nnext");
        feed(&mut ed, &[key_char('l'), key_char('l'), key_char('D')]);
        assert_eq!(ed.text(), "he\nnext");
        assert_eq!(ed.cursor(), (0, 1));
    }

    #[test]
    fn big_c_changes_to_eol() {
        let mut ed = Editor::new();
        ed.set_lines("hello world");
        feed(&mut ed, &[key_char('l'), key_char('l'), key_char('C')]);
        feed(&mut ed, &[key_char('X'), key_esc()]);
        assert_eq!(ed.text(), "heX");
    }

    #[test]
    fn big_y_yanks_to_eol() {
        // Matches nvim's default: Y == y$, charwise yank.
        let mut ed = Editor::new();
        ed.set_lines("foo\nbar");
        feed(&mut ed, &[key_char('Y'), key_char('p')]);
        assert_eq!(ed.text(), "ffoooo\nbar");
    }

    #[test]
    fn caret_jumps_to_first_nonblank() {
        let mut ed = Editor::new();
        ed.set_lines("    hello");
        feed(&mut ed, &[key_char('$'), key_char('^')]);
        assert_eq!(ed.cursor(), (0, 4));
    }

    #[test]
    fn caret_on_blank_line_stays_at_zero() {
        let mut ed = Editor::new();
        ed.set_lines("");
        feed(&mut ed, &[key_char('^')]);
        assert_eq!(ed.cursor(), (0, 0));
    }

    #[test]
    fn d_caret_deletes_to_first_nonblank() {
        let mut ed = Editor::new();
        ed.set_lines("    hello");
        feed(&mut ed, &[key_char('$'), key_char('d'), key_char('^')]);
        assert_eq!(ed.text(), "    o");
    }

    #[test]
    fn paragraph_forward_lands_on_blank() {
        let mut ed = Editor::new();
        ed.set_lines("alpha\nbeta\n\ngamma");
        feed(&mut ed, &[key_char('}')]);
        assert_eq!(ed.cursor(), (2, 0));
    }

    #[test]
    fn paragraph_backward_returns_to_blank() {
        let mut ed = Editor::new();
        ed.set_lines("alpha\n\nbeta\ngamma");
        feed(&mut ed, &[key_char('G'), key_char('{')]);
        assert_eq!(ed.cursor(), (1, 0));
    }

    #[test]
    fn dip_deletes_inner_paragraph() {
        let mut ed = Editor::new();
        ed.set_lines("alpha\nbeta\n\ngamma");
        feed(&mut ed, &[key_char('d'), key_char('i'), key_char('p')]);
        assert_eq!(ed.text(), "\ngamma");
    }

    #[test]
    fn dap_deletes_paragraph_and_trailing_blanks() {
        let mut ed = Editor::new();
        ed.set_lines("alpha\nbeta\n\ngamma");
        feed(&mut ed, &[key_char('d'), key_char('a'), key_char('p')]);
        assert_eq!(ed.text(), "gamma");
    }

    #[test]
    fn find_char_forward() {
        let mut ed = Editor::new();
        ed.set_lines("hello world");
        feed(&mut ed, &[key_char('f'), key_char('o')]);
        assert_eq!(ed.cursor(), (0, 4));
    }

    #[test]
    fn find_char_backward() {
        let mut ed = Editor::new();
        ed.set_lines("hello world");
        feed(&mut ed, &[key_char('$'), key_char('F'), key_char('o')]);
        assert_eq!(ed.cursor(), (0, 7));
    }

    #[test]
    fn till_char_forward_stops_before() {
        let mut ed = Editor::new();
        ed.set_lines("hello world");
        feed(&mut ed, &[key_char('t'), key_char('o')]);
        assert_eq!(ed.cursor(), (0, 3));
    }

    #[test]
    fn df_includes_target() {
        let mut ed = Editor::new();
        ed.set_lines("hello world");
        feed(&mut ed, &[key_char('d'), key_char('f'), key_char('l')]);
        assert_eq!(ed.text(), "lo world");
    }

    #[test]
    fn dt_excludes_target() {
        let mut ed = Editor::new();
        ed.set_lines("hello world");
        feed(&mut ed, &[key_char('d'), key_char('t'), key_char('l')]);
        assert_eq!(ed.text(), "llo world");
    }

    #[test]
    fn find_char_missing_is_noop() {
        let mut ed = Editor::new();
        ed.set_lines("hello");
        feed(&mut ed, &[key_char('f'), key_char('Z')]);
        assert_eq!(ed.cursor(), (0, 0));
    }

    #[test]
    fn set_and_jump_mark() {
        let mut ed = Editor::new();
        ed.set_lines("line one\nline two\nline three");
        feed(
            &mut ed,
            &[
                key_char('j'),
                key_char('l'),
                key_char('l'),
                key_char('m'),
                key_char('a'),
            ],
        );
        feed(&mut ed, &[key_char('G')]);
        feed(&mut ed, &[key_char('`'), key_char('a')]);
        assert_eq!(ed.cursor(), (1, 2));
    }

    #[test]
    fn jump_to_mark_line_goes_to_first_nonblank() {
        let mut ed = Editor::new();
        ed.set_lines("  hello\nworld");
        feed(
            &mut ed,
            &[
                key_char('j'),
                key_char('m'),
                key_char('a'),
                key_char('g'),
                key_char('g'),
                key_char('\''),
                key_char('a'),
            ],
        );
        // mark was set at (1, 0); '{mark} goes to that line's first non-blank
        assert_eq!(ed.cursor().0, 1);
    }

    #[test]
    fn delete_to_mark() {
        let mut ed = Editor::new();
        ed.set_lines("abcdef");
        feed(
            &mut ed,
            &[
                key_char('l'),
                key_char('l'),
                key_char('m'),
                key_char('a'),
                key_char('0'),
                key_char('d'),
                key_char('`'),
                key_char('a'),
            ],
        );
        assert_eq!(ed.text(), "cdef");
    }

    #[test]
    fn tilde_toggles_case_and_advances() {
        let mut ed = Editor::new();
        ed.set_lines("abcdef");
        feed(&mut ed, &[key_char('~')]);
        assert_eq!(ed.text(), "Abcdef");
        assert_eq!(ed.cursor(), (0, 1));
    }

    #[test]
    fn tilde_count_transforms_multiple_chars() {
        let mut ed = Editor::new();
        ed.set_lines("abcdef");
        feed(&mut ed, &[key_char('3'), key_char('~')]);
        assert_eq!(ed.text(), "ABCdef");
    }

    #[test]
    fn gu_motion_lowercases() {
        let mut ed = Editor::new();
        ed.set_lines("HELLO WORLD");
        feed(&mut ed, &[key_char('g'), key_char('u'), key_char('w')]);
        assert_eq!(ed.text(), "hello WORLD");
    }

    #[test]
    fn big_gu_motion_uppercases() {
        let mut ed = Editor::new();
        ed.set_lines("hello world");
        feed(&mut ed, &[key_char('g'), key_char('U'), key_char('$')]);
        assert_eq!(ed.text(), "HELLO WORLD");
    }

    #[test]
    fn g_tilde_motion_toggles() {
        let mut ed = Editor::new();
        ed.set_lines("Hello World");
        feed(&mut ed, &[key_char('g'), key_char('~'), key_char('$')]);
        assert_eq!(ed.text(), "hELLO wORLD");
    }

    #[test]
    fn guu_lowercases_whole_line() {
        let mut ed = Editor::new();
        ed.set_lines("HELLO WORLD\nNEXT");
        feed(&mut ed, &[key_char('g'), key_char('u'), key_char('u')]);
        assert_eq!(ed.text(), "hello world\nNEXT");
    }

    #[test]
    fn big_guu_uppercases_whole_line() {
        let mut ed = Editor::new();
        ed.set_lines("hello\nworld");
        feed(&mut ed, &[key_char('g'), key_char('U'), key_char('U')]);
        assert_eq!(ed.text(), "HELLO\nworld");
    }

    #[test]
    fn visual_upper_uppercases_selection() {
        let mut ed = Editor::new();
        ed.set_lines("hello world");
        feed(
            &mut ed,
            &[
                key_char('v'),
                key_char('l'),
                key_char('l'),
                key_char('l'),
                key_char('l'),
                key_char('U'),
            ],
        );
        assert_eq!(ed.text(), "HELLO world");
        assert_eq!(ed.mode(), Mode::Normal);
    }

    #[test]
    fn visual_lower_lowercases_selection() {
        let mut ed = Editor::new();
        ed.set_lines("HELLO WORLD");
        feed(
            &mut ed,
            &[
                key_char('v'),
                key_char('l'),
                key_char('l'),
                key_char('l'),
                key_char('l'),
                key_char('u'),
            ],
        );
        assert_eq!(ed.text(), "hello WORLD");
    }

    #[test]
    fn visual_tilde_toggles_selection() {
        let mut ed = Editor::new();
        ed.set_lines("Hello");
        feed(
            &mut ed,
            &[key_char('v'), key_char('l'), key_char('l'), key_char('~')],
        );
        assert_eq!(ed.text(), "hELlo");
    }

    #[test]
    fn increment_number() {
        let mut ed = Editor::new();
        ed.set_lines("count = 41");
        feed(&mut ed, &[key_ctrl('a')]);
        assert_eq!(ed.text(), "count = 42");
    }

    #[test]
    fn increment_with_count() {
        let mut ed = Editor::new();
        ed.set_lines("x = 10");
        feed(&mut ed, &[key_char('5'), key_ctrl('a')]);
        assert_eq!(ed.text(), "x = 15");
    }

    #[test]
    fn decrement_number() {
        let mut ed = Editor::new();
        ed.set_lines("x = 10");
        feed(&mut ed, &[key_ctrl('x')]);
        assert_eq!(ed.text(), "x = 9");
    }

    #[test]
    fn increment_finds_next_number_on_line() {
        let mut ed = Editor::new();
        ed.set_lines("x = 5");
        // Cursor at col 0 ('x'); <C-a> should scan rightward and land on 6.
        feed(&mut ed, &[key_ctrl('a')]);
        assert_eq!(ed.text(), "x = 6");
    }

    #[test]
    fn increment_handles_negative() {
        let mut ed = Editor::new();
        ed.set_lines("n = -3");
        feed(&mut ed, &[key_char('$'), key_ctrl('a')]);
        assert_eq!(ed.text(), "n = -2");
    }

    #[test]
    fn viw_selects_inner_word_then_delete() {
        let mut ed = Editor::new();
        ed.set_lines("foo bar baz");
        feed(
            &mut ed,
            &[
                key_char('l'),
                key_char('l'),
                key_char('v'),
                key_char('i'),
                key_char('w'),
                key_char('d'),
            ],
        );
        assert_eq!(ed.text(), " bar baz");
    }

    #[test]
    fn vaw_selects_around_word_then_delete() {
        let mut ed = Editor::new();
        ed.set_lines("foo bar baz");
        feed(
            &mut ed,
            &[
                key_char('l'),
                key_char('v'),
                key_char('a'),
                key_char('w'),
                key_char('d'),
            ],
        );
        assert_eq!(ed.text(), "bar baz");
    }

    #[test]
    fn vip_selects_paragraph_linewise_then_delete() {
        let mut ed = Editor::new();
        ed.set_lines("alpha\nbeta\n\ngamma");
        feed(
            &mut ed,
            &[key_char('v'), key_char('i'), key_char('p'), key_char('d')],
        );
        assert_eq!(ed.text(), "\ngamma");
    }

    #[test]
    fn vap_selects_paragraph_with_trailing_blanks() {
        let mut ed = Editor::new();
        ed.set_lines("alpha\nbeta\n\ngamma");
        feed(
            &mut ed,
            &[key_char('v'), key_char('a'), key_char('p'), key_char('d')],
        );
        assert_eq!(ed.text(), "gamma");
    }

    #[test]
    fn vi_double_quote_selects_inside() {
        let mut ed = Editor::new();
        ed.set_lines("a \"hello\" b");
        feed(
            &mut ed,
            &[
                key_char('l'),
                key_char('l'),
                key_char('l'),
                key_char('v'),
                key_char('i'),
                key_char('"'),
                key_char('d'),
            ],
        );
        assert_eq!(ed.text(), "a \"\" b");
    }

    #[test]
    fn vi_paren_selects_inner_pair() {
        let mut ed = Editor::new();
        ed.set_lines("(hello world)");
        feed(
            &mut ed,
            &[
                key_char('l'),
                key_char('l'),
                key_char('v'),
                key_char('i'),
                key_char('('),
                key_char('d'),
            ],
        );
        assert_eq!(ed.text(), "()");
    }
}
