use extui::{Buffer, Rect, Style};
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

use crate::{
    buffer::{self, TextBuffer},
    cursor::Cursor,
    mode::Mode,
    theme::EditorTheme,
    visual::{Selection, VisualKind},
};

/// Contiguous byte range of buffer text paired with a [`Style`].
///
/// Hosts produce these per frame from a syntax highlighter or any
/// other style source and pass a sorted, non-overlapping slice to
/// [`Editor::render_with_styles`]. Runs may span newlines; the
/// renderer clips each run to the line it is painting.
///
/// [`Editor::render_with_styles`]: crate::Editor::render_with_styles
#[derive(Clone, Copy, Debug)]
pub struct StyleRun {
    /// Byte offset of the run's start in the full buffer.
    pub offset: u32,
    /// Length in bytes.
    pub len: u32,
    /// Style applied to every cell the run covers.
    pub style: Style,
}

impl StyleRun {
    #[inline]
    fn end(self) -> u32 {
        self.offset + self.len
    }
}

/// Forward cursor into a `&[StyleRun]` slice. Hot-path helper: the
/// renderer advances this in sync with its wrap-layout walk, never
/// replaying the slice from the start.
pub(crate) struct RunCursor<'a> {
    runs: &'a [StyleRun],
    idx: usize,
}

impl<'a> RunCursor<'a> {
    pub(crate) fn new(runs: &'a [StyleRun]) -> Self {
        Self { runs, idx: 0 }
    }

    /// Jump the cursor to the first run that ends after `offset`.
    /// Uses a binary search so callers can cheaply land at the top
    /// of the viewport when `runs` covers the whole file.
    pub(crate) fn seek_to(&mut self, offset: u32) {
        self.idx = self.runs.partition_point(|r| r.end() <= offset);
    }

    /// Advance past every run that ends at or before `offset`.
    /// Cheap when the slice is already near the target.
    #[inline]
    fn advance_past(&mut self, offset: u32) {
        while let Some(run) = self.runs.get(self.idx) {
            if run.end() <= offset {
                self.idx += 1;
            } else {
                break;
            }
        }
    }

    #[inline]
    fn peek(&self) -> Option<StyleRun> {
        self.runs.get(self.idx).copied()
    }

    #[inline]
    fn advance(&mut self) {
        self.idx += 1;
    }
}

/// Display-column width of a single grapheme cluster, when not preceded
/// by any column-dependent expansion. Returns 0 for graphemes that
/// contain any control character (including `\t`, which is handled by
/// the caller via the configured tab stop).
fn grapheme_width(g: &str) -> u16 {
    if g.contains(char::is_control) {
        0
    } else {
        UnicodeWidthStr::width(g) as u16
    }
}

/// Advance a running display column by `\t`'s expansion: jump to the
/// next multiple of `tabstop`.
fn next_tab_stop(col: u16, tabstop: u16) -> u16 {
    let tabstop = tabstop.max(1);
    (col / tabstop).saturating_add(1).saturating_mul(tabstop)
}

/// Map a byte column within a line to a display column. Mirrors how
/// [`render_line`] lays cells down, so cursor positioning and text
/// rendering stay in lockstep:
///
/// - `\t` expands to the next `tabstop` boundary.
/// - Other control-char graphemes contribute zero width (matching
///   `Buffer::set_stringn`, which drops them entirely).
/// - Everything else contributes its `unicode-width`.
///
/// Returns the column *at the start of* the grapheme at `byte_col`
/// (i.e. "just before" the grapheme) — the correct endpoint for
/// selection highlights. Cursor rendering uses
/// [`cursor_display_col`] instead, which applies vim's tab-cursor
/// convention on top of this.
pub fn byte_col_to_display_col(line: &str, byte_col: usize, tabstop: u16) -> u16 {
    let limit = byte_col.min(line.len());
    let mut col: u16 = 0;
    for (i, g) in line.grapheme_indices(true) {
        if i >= limit {
            break;
        }
        if g == "\t" {
            col = next_tab_stop(col, tabstop);
        } else {
            col = col.saturating_add(grapheme_width(g));
        }
    }
    col
}

/// Display column at which the cursor block (or bar) should be drawn.
///
/// In Insert mode the bar sits *between* graphemes, so this is just
/// [`byte_col_to_display_col`]. In Normal / Visual modes the block
/// sits *on* a grapheme, and vim's convention differs by grapheme
/// kind:
///
/// - On a hard tab, the cursor covers the *last* cell of the tab's
///   expansion (one cell before the next tab stop). This is the
///   legacy vi/vim behavior that nvim preserves.
/// - On any other grapheme (including wide CJK cells), the cursor
///   covers the *first* cell — same as [`byte_col_to_display_col`].
pub fn cursor_display_col(line: &str, byte_col: usize, mode: Mode, tabstop: u16) -> u16 {
    let start = byte_col_to_display_col(line, byte_col, tabstop);
    if matches!(mode, Mode::Insert) {
        return start;
    }
    // Cursor "on a grapheme". For tabs, jump to the last cell of the
    // tab's expansion. (`byte_col` may equal `line.len()` on an empty
    // line — `graphemes().next()` then returns None, so `start` stands.)
    let tail = line.get(byte_col..).unwrap_or("");
    match tail.graphemes(true).next() {
        Some("\t") => next_tab_stop(start, tabstop).saturating_sub(1),
        _ => start,
    }
}

/// Render the full editor widget into `rect` on `buf`.
pub fn render(
    buf: &mut Buffer,
    rect: Rect,
    text: &TextBuffer,
    theme: &EditorTheme,
    tabstop: u16,
    cursor: Cursor,
    mode: Mode,
    selection: Option<&Selection>,
    scroll_offset: u16,
    horizontal_scroll: u16,
    wrap: bool,
    runs: &[StyleRun],
    inline_completion: Option<(&str, Style)>,
) {
    if rect.is_empty() {
        return;
    }
    for y in 0..rect.h {
        for x in 0..rect.w {
            buf.set_string(rect.x + x, rect.y + y, " ", theme.text);
        }
    }
    let layout = wrap.then(|| WrapLayout::new(text, rect.w.max(1), tabstop));
    let mut cursor_runs = RunCursor::new(runs);
    if !runs.is_empty() {
        let top_offset = text.line_start(scroll_offset as usize);
        cursor_runs.seek_to(top_offset);
    }
    if wrap {
        render_wrapped(
            buf,
            rect,
            text,
            theme,
            tabstop,
            scroll_offset,
            &mut cursor_runs,
            layout.as_ref().unwrap(),
        );
    } else {
        render_nowrap(
            buf,
            rect,
            text,
            theme,
            tabstop,
            scroll_offset,
            horizontal_scroll,
            &mut cursor_runs,
        );
    }

    if let Some(sel) = selection {
        paint_selection(
            buf,
            rect,
            text,
            theme,
            sel,
            scroll_offset,
            horizontal_scroll,
            tabstop,
            wrap,
            layout.as_ref(),
        );
    }

    if let Some((suffix, style)) = inline_completion {
        paint_inline_completion(
            buf,
            rect,
            text,
            theme,
            tabstop,
            cursor,
            mode,
            scroll_offset,
            horizontal_scroll,
            wrap,
            layout.as_ref(),
            suffix,
            style,
        );
    }

    place_cursor(
        buf,
        rect,
        text,
        tabstop,
        cursor,
        mode,
        scroll_offset,
        horizontal_scroll,
        wrap,
        layout.as_ref(),
    );
}

/// Draw `line` starting at `rect.x` on screen row `y`, expanding hard
/// tabs to the next configured tab stop. Returns the first x that was not
/// written to (so the caller can pad the trailing region with spaces).
///
/// When `runs` yields entries overlapping the line, splits it by run
/// boundaries and emits each sub-segment with the run's style. Gaps
/// between runs get [`EditorTheme::text`].
fn render_nowrap(
    buf: &mut Buffer,
    rect: Rect,
    text: &TextBuffer,
    theme: &EditorTheme,
    tabstop: u16,
    scroll_offset: u16,
    horizontal_scroll: u16,
    runs: &mut RunCursor<'_>,
) {
    for y in 0..rect.h {
        let row = scroll_offset as usize + y as usize;
        let line = text.line(row);
        let line_base = text.line_start(row);
        render_line_window(
            buf,
            rect,
            rect.y + y,
            line.as_ref(),
            line_base,
            tabstop,
            runs,
            theme,
            horizontal_scroll,
            horizontal_scroll.saturating_add(rect.w),
        );
    }
}

fn render_wrapped(
    buf: &mut Buffer,
    rect: Rect,
    text: &TextBuffer,
    theme: &EditorTheme,
    tabstop: u16,
    scroll_offset: u16,
    runs: &mut RunCursor<'_>,
    layout: &WrapLayout,
) {
    let width = layout.width();
    let top = scroll_offset as u32;
    let bottom = top + rect.h as u32;
    for row in 0..text.line_count().max(1) {
        let visual_row = layout.rows_before(row);
        let line_rows = layout.line_rows(row);
        let line_end = visual_row + line_rows;
        if line_end <= top {
            continue;
        }
        if visual_row >= bottom {
            break;
        }
        let line = text.line(row);
        let line = line.as_ref();
        let line_base = text.line_start(row);
        let first_seg = top.saturating_sub(visual_row);
        let last_seg = (bottom.saturating_sub(visual_row)).min(line_rows);
        let base_y = rect.y + (visual_row + first_seg - top) as u16;
        render_wrapped_line(
            buf, rect, line, line_base, tabstop, runs, theme, width, first_seg, last_seg, base_y,
        );
    }
}

/// Render one logical line in a single forward pass across graphemes.
/// Advances the shared [`RunCursor`] through style runs overlapping the
/// line, threading `display_col` across all styled regions, and emits
/// into the visible segments `[first_seg, last_seg)` directly at their
/// screen rows.
fn render_wrapped_line(
    buf: &mut Buffer,
    rect: Rect,
    line: &str,
    line_base: u32,
    tabstop: u16,
    runs: &mut RunCursor<'_>,
    theme: &EditorTheme,
    width: u16,
    first_seg: u32,
    last_seg: u32,
    base_y: u16,
) {
    if first_seg >= last_seg || width == 0 {
        return;
    }
    let width_u32 = width as u32;
    let stop_at = last_seg * width_u32;
    let mut display_col: u32 = 0;

    let line_end_abs = line_base + line.len() as u32;
    runs.advance_past(line_base);

    // Current style window covers bytes `[0, style_end)`. When `byte_idx
    // >= style_end`, advance the window by either applying the pending
    // run or fetching the next from `runs`.
    let mut pending: Option<(usize, usize, Style)> = None;
    let mut style_end: usize = 0;
    let mut current_style = theme.text;

    let line_bytes = line.as_bytes();
    let mut byte_idx = 0usize;
    while byte_idx < line.len() {
        while style_end <= byte_idx {
            if pending.is_none() {
                while let Some(run) = runs.peek() {
                    if run.offset >= line_end_abs {
                        break;
                    }
                    let (tok_s, tok_e) =
                        clip_run_to_line(run.offset, run.end(), line_base, line.len());
                    if tok_e <= byte_idx {
                        if run.end() <= line_end_abs {
                            runs.advance();
                            continue;
                        }
                        break;
                    }
                    if run.end() <= line_end_abs {
                        runs.advance();
                    }
                    pending = Some((tok_s, tok_e, run.style));
                    break;
                }
            }
            match pending {
                Some((tok_s, tok_e, style)) if tok_s <= byte_idx => {
                    current_style = style;
                    style_end = tok_e;
                    pending = None;
                }
                Some((tok_s, _, _)) => {
                    current_style = theme.text;
                    style_end = tok_s;
                }
                None => {
                    current_style = theme.text;
                    style_end = line.len().max(byte_idx + 1);
                }
            }
        }

        if display_col >= stop_at {
            break;
        }

        // ASCII-printable fast path: find the longest run of bytes in
        // [0x20, 0x7E] that stays within the current style window. Each
        // byte is a width-1 standalone grapheme, so byte-offsets and
        // display-column offsets coincide. Slice per-segment and emit
        // directly; `set_string`'s ASCII path writes each chunk without
        // grapheme segmentation. Pull the last byte back into the
        // general path when the run is followed by a non-ASCII byte
        // (potential combining mark).
        let b = line_bytes[byte_idx];
        if matches!(b, 0x20..=0x7E) {
            let limit = style_end.min(line.len());
            let mut end = byte_idx + 1;
            while end < limit && matches!(line_bytes[end], 0x20..=0x7E) {
                end += 1;
            }
            if end < line.len() && line_bytes[end] >= 0x80 && end > byte_idx + 1 {
                end -= 1;
            }
            if end > byte_idx {
                emit_wrapped_ascii_run(
                    buf,
                    rect,
                    base_y,
                    first_seg,
                    last_seg,
                    width_u32,
                    stop_at,
                    &line[byte_idx..end],
                    display_col,
                    current_style,
                );
                display_col += (end - byte_idx) as u32;
                byte_idx = end;
                continue;
            }
        }

        let g = match line[byte_idx..].graphemes(true).next() {
            Some(g) => g,
            None => break,
        };
        let g_len = g.len();

        let seg = display_col / width_u32;

        if g == "\t" {
            let next = next_tab_stop(display_col as u16, tabstop) as u32;
            let mut col = display_col;
            while col < next {
                let s = col / width_u32;
                let seg_end = ((s + 1) * width_u32).min(next);
                if s >= first_seg && s < last_seg {
                    let y = base_y + (s - first_seg) as u16;
                    for c in col..seg_end {
                        let x_in_seg = (c - s * width_u32) as u16;
                        buf.set_string(rect.x + x_in_seg, y, " ", current_style);
                    }
                }
                col = seg_end;
            }
            display_col = next;
            byte_idx += g_len;
            continue;
        }

        let w = grapheme_width(g) as u32;
        if w == 0 {
            byte_idx += g_len;
            continue;
        }
        let next = display_col + w;
        let seg_end = (seg + 1) * width_u32;
        let straddles = next > seg_end;
        if !straddles && seg >= first_seg && seg < last_seg {
            let y = base_y + (seg - first_seg) as u16;
            let x_in_seg = (display_col - seg * width_u32) as u16;
            buf.set_string(rect.x + x_in_seg, y, g, current_style);
        }
        display_col = next;
        byte_idx += g_len;
    }
}

/// Emit an ASCII-only, same-style, tab-free run of `chunk` bytes across
/// visible wrap segments `[first_seg, last_seg)`. `chunk.len()` must
/// equal the number of display columns the run occupies.
fn emit_wrapped_ascii_run(
    buf: &mut Buffer,
    rect: Rect,
    base_y: u16,
    first_seg: u32,
    last_seg: u32,
    width_u32: u32,
    stop_at: u32,
    chunk: &str,
    display_col: u32,
    style: Style,
) {
    let chunk_start = display_col;
    let chunk_end = (chunk_start + chunk.len() as u32).min(stop_at);
    let lo = chunk_start.max(first_seg * width_u32);
    let hi = chunk_end.min(last_seg * width_u32);
    if lo >= hi {
        return;
    }
    let mut col = lo;
    while col < hi {
        let s = col / width_u32;
        let seg_col_end = ((s + 1) * width_u32).min(hi);
        let y = base_y + (s - first_seg) as u16;
        let x_in_seg = (col - s * width_u32) as u16;
        let byte_lo = (col - chunk_start) as usize;
        let byte_hi = (seg_col_end - chunk_start) as usize;
        buf.set_string(rect.x + x_in_seg, y, &chunk[byte_lo..byte_hi], style);
        col = seg_col_end;
    }
}

fn render_line_window(
    buf: &mut Buffer,
    rect: Rect,
    y: u16,
    line: &str,
    line_base: u32,
    tabstop: u16,
    runs: &mut RunCursor<'_>,
    theme: &EditorTheme,
    view_start: u16,
    view_end: u16,
) {
    let mut display_col = 0u16;
    let line_end_abs = line_base + line.len() as u32;
    runs.advance_past(line_base);

    let mut cursor_in_line: usize = 0;
    while let Some(run) = runs.peek() {
        if run.offset >= line_end_abs {
            break;
        }
        let (tok_s, tok_e) = clip_run_to_line(run.offset, run.end(), line_base, line.len());
        if tok_e <= cursor_in_line {
            if run.end() <= line_end_abs {
                runs.advance();
                continue;
            }
            break;
        }
        let s = tok_s.max(cursor_in_line);
        if s > cursor_in_line {
            emit_run_window(
                buf,
                rect,
                y,
                &line[cursor_in_line..s],
                tabstop,
                theme.text,
                view_start,
                view_end,
                &mut display_col,
            );
        }
        emit_run_window(
            buf,
            rect,
            y,
            &line[s..tok_e],
            tabstop,
            run.style,
            view_start,
            view_end,
            &mut display_col,
        );
        cursor_in_line = tok_e;
        if run.end() <= line_end_abs {
            runs.advance();
        } else {
            // Run extends past this line; leave cursor on it for the next line.
            break;
        }
    }
    if cursor_in_line < line.len() {
        emit_run_window(
            buf,
            rect,
            y,
            &line[cursor_in_line..],
            tabstop,
            theme.text,
            view_start,
            view_end,
            &mut display_col,
        );
    }
}

/// Emit `text` at screen position `(x, y)` within `rect`, expanding
/// `\t` to the next configured tab stop. Returns the first x that was not
/// written to. Tab padding inherits `style` so a styled run reads as
/// a single visual band.
fn emit_run_window(
    buf: &mut Buffer,
    rect: Rect,
    y: u16,
    text: &str,
    tabstop: u16,
    style: Style,
    view_start: u16,
    view_end: u16,
    display_col: &mut u16,
) {
    if view_start >= view_end {
        return;
    }
    let rect_right = view_start.saturating_add(rect.w);

    // Split on `\t` and hand each tab-free chunk to the ASCII or grapheme
    // path depending on its content. `set_string` has its own ASCII fast
    // path (`Buffer::set_stringn`), so pushing a whole ASCII chunk in a
    // single call lets the segmentation + unicode-width work drop out
    // entirely on source-code-like input.
    let mut rest = text;
    loop {
        let (chunk, tail) = match rest.as_bytes().iter().position(|&b| b == b'\t') {
            Some(i) => (&rest[..i], Some(&rest[i + 1..])),
            None => (rest, None),
        };
        if !chunk.is_empty() {
            if chunk.is_ascii() {
                emit_ascii_chunk(
                    buf,
                    rect,
                    y,
                    chunk,
                    style,
                    view_start,
                    view_end,
                    rect_right,
                    display_col,
                );
            } else {
                emit_grapheme_chunk(
                    buf,
                    rect,
                    y,
                    chunk,
                    style,
                    view_start,
                    view_end,
                    display_col,
                );
            }
            if *display_col >= view_end {
                return;
            }
        }
        let Some(next_tail) = tail else {
            return;
        };
        let next = next_tab_stop(*display_col, tabstop);
        let vis_start = (*display_col).max(view_start);
        let vis_end = next.min(view_end);
        for col in vis_start..vis_end {
            buf.set_string(rect.x + (col - view_start), y, " ", style);
        }
        *display_col = next;
        if *display_col >= view_end {
            return;
        }
        rest = next_tail;
    }
}

#[inline]
fn emit_ascii_chunk(
    buf: &mut Buffer,
    rect: Rect,
    y: u16,
    chunk: &str,
    style: Style,
    view_start: u16,
    view_end: u16,
    rect_right: u16,
    display_col: &mut u16,
) {
    // Every byte in `chunk` is ASCII (`is_ascii()` holds), so each is a
    // width-1 grapheme and byte index == display-column offset within
    // the chunk. Control bytes (`\n`, etc.) are filtered by
    // `Buffer::set_stringn`'s ASCII path.
    let run_start_col = *display_col;
    let run_end_col = run_start_col.saturating_add(chunk.len() as u16);
    let vis_lo = run_start_col.max(view_start);
    let vis_hi = run_end_col.min(view_end).min(rect_right);
    if vis_lo < vis_hi {
        let byte_lo = (vis_lo - run_start_col) as usize;
        let byte_hi = (vis_hi - run_start_col) as usize;
        let slice = &chunk[byte_lo..byte_hi];
        buf.set_string(rect.x + (vis_lo - view_start), y, slice, style);
    }
    *display_col = run_end_col;
}

fn emit_grapheme_chunk(
    buf: &mut Buffer,
    rect: Rect,
    y: u16,
    chunk: &str,
    style: Style,
    view_start: u16,
    view_end: u16,
    display_col: &mut u16,
) {
    for g in chunk.graphemes(true) {
        let width = grapheme_width(g);
        let next = (*display_col).saturating_add(width);
        if width > 0
            && *display_col >= view_start
            && next <= view_end
            && *display_col < rect.w.saturating_add(view_start)
        {
            buf.set_string(rect.x + (*display_col - view_start), y, g, style);
        }
        *display_col = next;
        if *display_col >= view_end {
            break;
        }
    }
}

/// Clip an absolute byte range to a single line's byte range.
/// Returns `(start, end)` in byte offsets within the line's `&str`.
#[inline]
fn clip_run_to_line(
    offset: u32,
    end_offset: u32,
    line_base: u32,
    line_len: usize,
) -> (usize, usize) {
    let line_end = line_base + line_len as u32;
    let start = offset.max(line_base).min(line_end);
    let end = end_offset.max(line_base).min(line_end);
    ((start - line_base) as usize, (end - line_base) as usize)
}

fn paint_selection(
    buf: &mut Buffer,
    rect: Rect,
    text: &TextBuffer,
    theme: &EditorTheme,
    sel: &Selection,
    scroll_offset: u16,
    horizontal_scroll: u16,
    tabstop: u16,
    wrap: bool,
    layout: Option<&WrapLayout>,
) {
    match sel.kind {
        VisualKind::Line => {
            let highlight = theme.selection.linewise;
            let (r0, r1) = sel.rows_ordered();
            for r in r0..=r1 {
                if let (true, Some(layout)) = (wrap, layout) {
                    let line_rows = layout.line_rows(r);
                    let line_start = layout.rows_before(r);
                    for seg in 0..line_rows {
                        let row = line_start + seg;
                        if row < scroll_offset as u32 || row >= scroll_offset as u32 + rect.h as u32
                        {
                            continue;
                        }
                        buf.set_style(
                            Rect {
                                x: rect.x,
                                y: rect.y + (row - scroll_offset as u32) as u16,
                                w: rect.w,
                                h: 1,
                            },
                            highlight,
                        );
                    }
                } else if let Some(y) = screen_row(r, scroll_offset, rect) {
                    buf.set_style(
                        Rect {
                            x: rect.x,
                            y,
                            w: rect.w,
                            h: 1,
                        },
                        highlight,
                    );
                }
            }
        }
        VisualKind::Block => {
            let highlight = theme.selection.blockwise;
            let (r0, r1) = sel.rows_ordered();
            let (c_lo, c_hi) = sel.cols_ordered();
            for r in r0..=r1 {
                let line = text.line(r);
                let line = line.as_ref();
                let x0 = byte_col_to_display_col(line, c_lo, tabstop);
                let end_byte = buffer::grapheme_end(line, c_hi);
                let x1 = byte_col_to_display_col(line, end_byte, tabstop);
                for span in display_span_rects(
                    rect,
                    scroll_offset,
                    horizontal_scroll,
                    wrap,
                    r,
                    x0,
                    x1,
                    layout,
                ) {
                    buf.set_style(span, highlight);
                }
            }
        }
        VisualKind::Char => {
            let highlight = theme.selection.charwise;
            let (start, end) = char_range(text, sel);
            for r in start.row..=end.row {
                let line = text.line(r);
                let line = line.as_ref();
                let seg_start = if r == start.row { start.col } else { 0 };
                let seg_end = if r == end.row {
                    buffer::grapheme_end(line, end.col)
                } else {
                    line.len()
                };
                if seg_end <= seg_start {
                    continue;
                }
                let x0 = byte_col_to_display_col(line, seg_start, tabstop);
                let x1 = byte_col_to_display_col(line, seg_end, tabstop);
                for span in display_span_rects(
                    rect,
                    scroll_offset,
                    horizontal_scroll,
                    wrap,
                    r,
                    x0,
                    x1,
                    layout,
                ) {
                    buf.set_style(span, highlight);
                }
            }
        }
    }
}

/// Normalize a charwise selection into (start, end_inclusive) cursor
/// positions in buffer order. `end.col` is the grapheme-start byte of
/// the last included cluster.
pub fn char_range(text: &TextBuffer, sel: &Selection) -> (Cursor, Cursor) {
    let a = sel.anchor;
    let b = sel.head;
    let (s, e) = if (a.row, a.col) <= (b.row, b.col) {
        (a, b)
    } else {
        (b, a)
    };
    let line = text.line(e.row);
    let aligned = buffer::align_to_grapheme_start(line.as_ref(), e.col);
    (
        s,
        Cursor {
            row: e.row,
            col: aligned,
        },
    )
}

/// Map a buffer byte range to screen rectangles inside `rect`, matching
/// the layout produced by [`render`] (scroll offset, tab expansion,
/// grapheme widths).
///
/// Returns an iterator over one rectangle per visible line segment the
/// range touches. Segments on rows above or below the viewport, or
/// starting past `rect.w`, yield no items. Each emitted rect has height
/// `1` and is clipped horizontally to `rect`.
///
/// Intended for post-[`render`] overlays (search highlights, spellcheck
/// underlines) applied via [`Buffer::set_style`], which layers a
/// style on top of existing cells without overwriting text or syntax
/// colors.
pub(crate) fn visible_range_rects(
    rect: Rect,
    text: &TextBuffer,
    scroll_offset: u16,
    horizontal_scroll: u16,
    wrap: bool,
    start: u32,
    end: u32,
    tabstop: u16,
) -> impl Iterator<Item = Rect> + '_ {
    let mut rects = Vec::new();
    let len = text.len() as u32;
    let start = start.min(len);
    let end = end.min(len);
    if !rect.is_empty() && end > start {
        let layout = wrap.then(|| WrapLayout::new(text, rect.w.max(1), tabstop));
        let (start_row, _) = text.offset_to_rowcol(start);
        let (end_row, _) = text.offset_to_rowcol(end);
        for row in start_row..=end_row {
            let line_base = text.line_start(row);
            let line = text.line(row);
            let line = line.as_ref();
            let seg_start = if row == start_row {
                (start - line_base) as usize
            } else {
                0
            };
            let seg_end = if row == end_row {
                (end - line_base) as usize
            } else {
                line.len()
            };
            if seg_end <= seg_start {
                continue;
            }
            let x0 = byte_col_to_display_col(line, seg_start, tabstop);
            let x1 = byte_col_to_display_col(line, seg_end, tabstop);
            rects.extend(display_span_rects(
                rect,
                scroll_offset,
                horizontal_scroll,
                wrap,
                row,
                x0,
                x1,
                layout.as_ref(),
            ));
        }
    }
    rects.into_iter()
}

fn paint_inline_completion(
    buf: &mut Buffer,
    rect: Rect,
    text: &TextBuffer,
    theme: &EditorTheme,
    tabstop: u16,
    cursor: Cursor,
    mode: Mode,
    scroll_offset: u16,
    horizontal_scroll: u16,
    wrap: bool,
    layout: Option<&WrapLayout>,
    suffix: &str,
    style: Style,
) {
    if rect.is_empty() || suffix.is_empty() {
        return;
    }
    let line = text.line(cursor.row);
    let start_col = cursor_display_col(line.as_ref(), cursor.col, mode, tabstop) as u32;
    let style = theme.text.patch(style);
    if wrap {
        let Some(layout) = layout else {
            return;
        };
        paint_inline_completion_wrapped(
            buf,
            rect,
            layout,
            cursor.row,
            scroll_offset,
            start_col,
            suffix,
            style,
            tabstop,
        );
    } else {
        let Some(y) = screen_row(cursor.row, scroll_offset, rect) else {
            return;
        };
        paint_inline_completion_nowrap(
            buf,
            rect,
            y,
            horizontal_scroll as u32,
            start_col,
            suffix,
            style,
            tabstop,
        );
    }
}

fn paint_inline_completion_wrapped(
    buf: &mut Buffer,
    rect: Rect,
    layout: &WrapLayout,
    cursor_row: usize,
    scroll_offset: u16,
    start_col: u32,
    suffix: &str,
    style: Style,
    tabstop: u16,
) {
    let width = rect.w.max(1) as u32;
    let line_visual_base = layout.rows_before(cursor_row);
    let mut col = start_col;
    for g in suffix.graphemes(true) {
        if g == "\n" {
            break;
        }
        if g == "\t" {
            let next = next_tab_stop(col.min(u16::MAX as u32) as u16, tabstop) as u32;
            paint_inline_spaces_wrapped(
                buf,
                rect,
                line_visual_base,
                scroll_offset,
                col,
                next.max(col),
                style,
                width,
            );
            col = next.max(col);
            continue;
        }
        let w = grapheme_width(g) as u32;
        if w == 0 {
            continue;
        }
        let next = col.saturating_add(w);
        if next <= ((col / width) + 1) * width {
            let visual_row = line_visual_base + col / width;
            if visual_row >= scroll_offset as u32
                && visual_row < scroll_offset as u32 + rect.h as u32
            {
                buf.set_string(
                    rect.x + (col % width) as u16,
                    rect.y + (visual_row - scroll_offset as u32) as u16,
                    g,
                    style,
                );
            }
        }
        col = next;
    }
}

fn paint_inline_spaces_wrapped(
    buf: &mut Buffer,
    rect: Rect,
    line_visual_base: u32,
    scroll_offset: u16,
    mut start: u32,
    end: u32,
    style: Style,
    width: u32,
) {
    while start < end {
        let seg = start / width;
        let seg_end = end.min((seg + 1) * width);
        let visual_row = line_visual_base + seg;
        if visual_row >= scroll_offset as u32 && visual_row < scroll_offset as u32 + rect.h as u32 {
            let y = rect.y + (visual_row - scroll_offset as u32) as u16;
            for col in start..seg_end {
                buf.set_string(rect.x + (col % width) as u16, y, " ", style);
            }
        }
        start = seg_end;
    }
}

fn paint_inline_completion_nowrap(
    buf: &mut Buffer,
    rect: Rect,
    y: u16,
    horizontal_scroll: u32,
    start_col: u32,
    suffix: &str,
    style: Style,
    tabstop: u16,
) {
    let right = horizontal_scroll + rect.w as u32;
    let mut col = start_col;
    for g in suffix.graphemes(true) {
        if g == "\n" {
            break;
        }
        if g == "\t" {
            let next = next_tab_stop(col.min(u16::MAX as u32) as u16, tabstop) as u32;
            for c in col..next.max(col) {
                if c >= horizontal_scroll && c < right {
                    buf.set_string(rect.x + (c - horizontal_scroll) as u16, y, " ", style);
                }
            }
            col = next.max(col);
            continue;
        }
        let w = grapheme_width(g) as u32;
        if w == 0 {
            continue;
        }
        let next = col.saturating_add(w);
        if col >= horizontal_scroll && next <= right {
            buf.set_string(rect.x + (col - horizontal_scroll) as u16, y, g, style);
        }
        col = next;
    }
}

fn screen_row(row: usize, scroll_offset: u16, rect: Rect) -> Option<u16> {
    let scroll = scroll_offset as usize;
    if row < scroll {
        return None;
    }
    let rel = row - scroll;
    if rel >= rect.h as usize {
        return None;
    }
    Some(rect.y + rel as u16)
}

fn place_cursor(
    buf: &mut Buffer,
    rect: Rect,
    text: &TextBuffer,
    tabstop: u16,
    cursor: Cursor,
    mode: Mode,
    scroll_offset: u16,
    horizontal_scroll: u16,
    wrap: bool,
    layout: Option<&WrapLayout>,
) {
    let Some((x, y)) = cursor_position(
        rect,
        text,
        tabstop,
        cursor,
        mode,
        scroll_offset,
        horizontal_scroll,
        wrap,
        layout,
    ) else {
        buf.hide_cursor();
        return;
    };
    buf.set_cursor(x, y, mode.cursor_shape());
}

pub(crate) fn cursor_position(
    rect: Rect,
    text: &TextBuffer,
    tabstop: u16,
    cursor: Cursor,
    mode: Mode,
    scroll_offset: u16,
    horizontal_scroll: u16,
    wrap: bool,
    layout: Option<&WrapLayout>,
) -> Option<(u16, u16)> {
    let line = text.line(cursor.row);
    let x_off = cursor_display_col(line.as_ref(), cursor.col, mode, tabstop) as u32;
    if wrap {
        let width = rect.w.max(1) as u32;
        let owned;
        let layout = match layout {
            Some(l) => l,
            None => {
                owned = WrapLayout::new(text, rect.w.max(1), tabstop);
                &owned
            }
        };
        let row = layout.rows_before(cursor.row) + x_off / width;
        if row < scroll_offset as u32 || row >= scroll_offset as u32 + rect.h as u32 {
            return None;
        }
        Some((
            rect.x + (x_off % width) as u16,
            rect.y + (row - scroll_offset as u32) as u16,
        ))
    } else {
        let Some(y) = screen_row(cursor.row, scroll_offset, rect) else {
            return None;
        };
        if x_off < horizontal_scroll as u32 || x_off >= horizontal_scroll as u32 + rect.w as u32 {
            return None;
        }
        Some((rect.x + (x_off - horizontal_scroll as u32) as u16, y))
    }
}

pub(crate) fn wrapped_line_rows(line: &str, width: u16, tabstop: u16) -> u16 {
    if width == 0 {
        return 0;
    }
    let cells = byte_col_to_display_col(line, line.len(), tabstop).max(1) as u32;
    ((cells - 1) / width as u32 + 1) as u16
}

/// Cached wrapped-row prefix sums for a render/layout pass. Built once
/// per frame in wrap mode so that `wrapped_line_rows` runs O(1) per row
/// lookup instead of being summed from 0 on every call.
pub(crate) struct WrapLayout {
    prefix: Vec<u32>,
    width: u16,
}

impl WrapLayout {
    pub(crate) fn new(text: &TextBuffer, width: u16, tabstop: u16) -> Self {
        let width = width.max(1);
        let line_count = text.line_count().max(1);
        let mut prefix = Vec::with_capacity(line_count + 1);
        prefix.push(0u32);
        let mut total = 0u32;
        for row in 0..line_count {
            total = total
                .saturating_add(wrapped_line_rows(text.line(row).as_ref(), width, tabstop) as u32);
            prefix.push(total);
        }
        Self { prefix, width }
    }

    pub(crate) fn rows_before(&self, row: usize) -> u32 {
        self.prefix[row.min(self.prefix.len() - 1)]
    }

    pub(crate) fn line_rows(&self, row: usize) -> u32 {
        let next = (row + 1).min(self.prefix.len() - 1);
        self.prefix[next] - self.prefix[row.min(self.prefix.len() - 1)]
    }

    pub(crate) fn width(&self) -> u16 {
        self.width
    }
}

fn display_span_rects(
    rect: Rect,
    scroll_offset: u16,
    horizontal_scroll: u16,
    wrap: bool,
    row: usize,
    x0: u16,
    x1: u16,
    layout: Option<&WrapLayout>,
) -> Vec<Rect> {
    if x1 <= x0 || rect.is_empty() {
        return Vec::new();
    }
    if wrap {
        let Some(layout) = layout else {
            return Vec::new();
        };
        let width = layout.width() as u32;
        let line_start = layout.rows_before(row);
        let mut out = Vec::new();
        let mut start = x0 as u32;
        let end = x1 as u32;
        while start < end {
            let seg_row = start / width;
            let seg_end = end.min((seg_row + 1) * width);
            let screen_row = line_start + seg_row;
            if screen_row >= scroll_offset as u32
                && screen_row < scroll_offset as u32 + rect.h as u32
            {
                out.push(Rect {
                    x: rect.x + (start % width) as u16,
                    y: rect.y + (screen_row - scroll_offset as u32) as u16,
                    w: (seg_end - start) as u16,
                    h: 1,
                });
            }
            start = seg_end;
        }
        out
    } else {
        let Some(y) = screen_row(row, scroll_offset, rect) else {
            return Vec::new();
        };
        let left = horizontal_scroll as u32;
        let right = left + rect.w as u32;
        let start = (x0 as u32).max(left);
        let end = (x1 as u32).min(right);
        if end <= start {
            return Vec::new();
        }
        vec![Rect {
            x: rect.x + (start - left) as u16,
            y,
            w: (end - start) as u16,
            h: 1,
        }]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::EditorTheme;
    use extui::{Buffer, Color, Style, vt::Modifier};

    fn rect(x: u16, y: u16, w: u16, h: u16) -> Rect {
        Rect { x, y, w, h }
    }

    fn keyword_fg() -> Color {
        Color::rgb(0xbb, 0x9a, 0xf7)
    }
    fn number_fg() -> Color {
        Color::rgb(0xff, 0x9e, 0x64)
    }
    fn selection_bg() -> Color {
        Color::rgb(0x2a, 0x2f, 0x4a)
    }

    fn themed() -> EditorTheme {
        EditorTheme {
            name: "Test",
            text: Style::DEFAULT
                .with_fg(Color::rgb(0xc0, 0xca, 0xf5))
                .with_bg(Color::rgb(0x1a, 0x1b, 0x26)),
            selection: crate::SelectionTheme {
                charwise: Style::DEFAULT.with_bg(selection_bg()),
                linewise: Style::DEFAULT.with_bg(selection_bg()),
                blockwise: Style::DEFAULT.with_bg(selection_bg()),
            },
        }
    }

    #[test]
    fn single_line_range_produces_one_rect() {
        let buf = TextBuffer::from_str("hello world");
        let got: Vec<_> =
            visible_range_rects(rect(2, 0, 80, 4), &buf, 0, 0, false, 6, 11, 4).collect();
        assert_eq!(got, vec![rect(2 + 6, 0, 5, 1)]);
    }

    #[test]
    fn multi_line_range_produces_per_line_rects() {
        let buf = TextBuffer::from_str("abc\ndef\nghi");
        // Range covers "c\ndef\ng" — partial tail on row 0, full row 1,
        // partial head on row 2.
        let got: Vec<_> =
            visible_range_rects(rect(0, 0, 10, 4), &buf, 0, 0, false, 2, 9, 4).collect();
        assert_eq!(
            got,
            vec![rect(2, 0, 1, 1), rect(0, 1, 3, 1), rect(0, 2, 1, 1)],
        );
    }

    #[test]
    fn scrolled_rows_are_skipped() {
        let buf = TextBuffer::from_str("a\nb\nc\nd");
        // Viewport shows rows 2..4. Range spans rows 0..=3.
        let got: Vec<_> =
            visible_range_rects(rect(0, 0, 10, 2), &buf, 2, 0, false, 0, 7, 4).collect();
        assert_eq!(got, vec![rect(0, 0, 1, 1), rect(0, 1, 1, 1)]);
    }

    #[test]
    fn range_with_tabs_uses_display_columns() {
        let buf = TextBuffer::from_str("\tab");
        // Highlight "ab" (byte 1..3): display cols 8..10.
        let got: Vec<_> =
            visible_range_rects(rect(0, 0, 20, 1), &buf, 0, 0, false, 1, 3, 4).collect();
        assert_eq!(got, vec![rect(4, 0, 2, 1)]);
    }

    #[test]
    fn range_clipped_to_rect_width() {
        let buf = TextBuffer::from_str("hello world");
        // Viewport is 8 wide; range 6..11 starts at col 6, clipped to w=2.
        let got: Vec<_> =
            visible_range_rects(rect(0, 0, 8, 1), &buf, 0, 0, false, 6, 11, 4).collect();
        assert_eq!(got, vec![rect(6, 0, 2, 1)]);
    }

    #[test]
    fn empty_or_inverted_range_yields_nothing() {
        let buf = TextBuffer::from_str("abc");
        let r = rect(0, 0, 10, 1);
        assert_eq!(
            visible_range_rects(r, &buf, 0, 0, false, 2, 2, 4).count(),
            0
        );
        assert_eq!(
            visible_range_rects(r, &buf, 0, 0, false, 5, 2, 4).count(),
            0
        );
    }

    #[test]
    fn charwise_selection_uses_theme_style_not_inverse() {
        let text = TextBuffer::from_str("hello");
        let theme = themed();
        let sel = Selection {
            anchor: Cursor { row: 0, col: 0 },
            head: Cursor { row: 0, col: 1 },
            kind: VisualKind::Char,
        };
        let mut db = Buffer::new(8, 1);
        render(
            &mut db,
            rect(0, 0, 8, 1),
            &text,
            &theme,
            4,
            Cursor { row: 0, col: 0 },
            Mode::Visual,
            Some(&sel),
            0,
            0,
            false,
            &[],
            None,
        );
        let cells = db.current().cells().to_vec();
        let style = cells[0].style();
        assert_eq!(style.bg(), theme.selection.charwise.bg());
        assert!(!style.modifiers().has(Modifier::REVERSED));
    }

    #[test]
    fn syntax_style_survives_selection_overlay() {
        // A host supplies a StyleRun covering "let" (bytes 0..3). The
        // renderer paints the keyword fg during the text phase; the
        // selection overlay then layers its bg on top without
        // clobbering the fg.
        let text = TextBuffer::from_str("let x = 1;");
        let theme = themed();
        let runs = vec![StyleRun {
            offset: 0,
            len: 3,
            style: Style::DEFAULT.with_fg(keyword_fg()),
        }];
        let sel = Selection {
            anchor: Cursor { row: 0, col: 0 },
            head: Cursor { row: 0, col: 2 },
            kind: VisualKind::Char,
        };
        let mut db = Buffer::new(16, 1);
        render(
            &mut db,
            rect(0, 0, 16, 1),
            &text,
            &theme,
            4,
            Cursor { row: 0, col: 0 },
            Mode::Visual,
            Some(&sel),
            0,
            0,
            false,
            &runs,
            None,
        );
        let cells = db.current().cells().to_vec();
        let style = cells[0].style();
        assert_eq!(style.fg(), Some(keyword_fg()));
        assert_eq!(style.bg(), Some(selection_bg()));
        assert!(!style.modifiers().has(Modifier::REVERSED));
    }

    #[test]
    fn linewise_selection_uses_theme_background() {
        let text = TextBuffer::from_str("aa\nbb");
        let mut theme = themed();
        theme.selection.linewise = Style::DEFAULT.with_bg(Color::rgb(1, 2, 3));
        let sel = Selection {
            anchor: Cursor { row: 0, col: 0 },
            head: Cursor { row: 1, col: 0 },
            kind: VisualKind::Line,
        };
        let mut db = Buffer::new(4, 2);
        render(
            &mut db,
            rect(0, 0, 4, 2),
            &text,
            &theme,
            4,
            Cursor { row: 0, col: 0 },
            Mode::VisualLine,
            Some(&sel),
            0,
            0,
            false,
            &[],
            None,
        );
        let cells = db.current().cells().to_vec();
        assert_eq!(cells[0].style().bg(), Some(Color::rgb(1, 2, 3)));
        assert_eq!(cells[4].style().bg(), Some(Color::rgb(1, 2, 3)));
    }

    fn row_text(db: &mut Buffer, width: u16, y: u16) -> String {
        let cells = db.current().cells();
        (0..width)
            .map(|x| {
                cells[(y as usize) * (width as usize) + x as usize]
                    .text_inline()
                    .unwrap_or("")
                    .to_string()
            })
            .collect()
    }

    fn render_wrapped_to(db: &mut Buffer, r: Rect, text: &TextBuffer, tabstop: u16) {
        render(
            db,
            r,
            text,
            &themed(),
            tabstop,
            Cursor { row: 0, col: 0 },
            Mode::Normal,
            None,
            0,
            0,
            true,
            &[],
            None,
        );
    }

    #[test]
    fn wrap_plain_line_splits_at_width() {
        let text = TextBuffer::from_str("abcdefgh");
        let mut db = Buffer::new(4, 2);
        render_wrapped_to(&mut db, rect(0, 0, 4, 2), &text, 4);
        assert_eq!(row_text(&mut db, 4, 0), "abcd");
        assert_eq!(row_text(&mut db, 4, 1), "efgh");
    }

    #[test]
    fn wrap_tab_crosses_segment_boundary() {
        // "ab\tc" at tabstop=4, width=3:
        //   display cols 0,1 = a,b; tab at col 2 expands to col 4, filling
        //   cols 2 and 3. Col 2 is row 0 last cell; col 3 is row 1 first
        //   cell. c lands at col 4 = row 1 col 1.
        let text = TextBuffer::from_str("ab\tc");
        let mut db = Buffer::new(3, 2);
        render_wrapped_to(&mut db, rect(0, 0, 3, 2), &text, 4);
        assert_eq!(row_text(&mut db, 3, 0), "ab ");
        assert_eq!(row_text(&mut db, 3, 1), " c ");
    }

    #[test]
    fn wrap_wide_grapheme_at_boundary_dropped() {
        // "a世" in width=2: `a` at col 0, `世` is width 2 and would span
        // cols 1..=2, straddling the segment boundary — dropped. Row 0
        // ends up "a " and row 1 is empty.
        let text = TextBuffer::from_str("a世");
        let mut db = Buffer::new(2, 2);
        render_wrapped_to(&mut db, rect(0, 0, 2, 2), &text, 4);
        assert_eq!(row_text(&mut db, 2, 0), "a ");
        assert_eq!(row_text(&mut db, 2, 1), "  ");
    }

    #[test]
    fn wrap_cursor_at_segment_boundary() {
        let text = TextBuffer::from_str("abcdefgh");
        let layout = WrapLayout::new(&text, 4, 4);
        let got = cursor_position(
            rect(0, 0, 4, 2),
            &text,
            4,
            Cursor { row: 0, col: 4 },
            Mode::Insert,
            0,
            0,
            true,
            Some(&layout),
        );
        assert_eq!(got, Some((0, 1)));
    }

    #[test]
    fn wrap_linewise_selection_covers_all_segments() {
        let line: String = "a".repeat(9);
        let text = TextBuffer::from_str(&line);
        let mut theme = themed();
        theme.selection.linewise = Style::DEFAULT.with_bg(Color::rgb(1, 2, 3));
        let sel = Selection {
            anchor: Cursor { row: 0, col: 0 },
            head: Cursor { row: 0, col: 0 },
            kind: VisualKind::Line,
        };
        let mut db = Buffer::new(4, 3);
        render(
            &mut db,
            rect(0, 0, 4, 3),
            &text,
            &theme,
            4,
            Cursor { row: 0, col: 0 },
            Mode::VisualLine,
            Some(&sel),
            0,
            0,
            true,
            &[],
            None,
        );
        let cells = db.current().cells();
        for y in 0..3 {
            for x in 0..4 {
                assert_eq!(
                    cells[y * 4 + x].style().bg(),
                    Some(Color::rgb(1, 2, 3)),
                    "cell ({x},{y}) missing linewise bg",
                );
            }
        }
    }

    #[test]
    fn wrap_charwise_selection_across_wrap() {
        // "abcdefg" wrapped at width 4: row 0 "abcd", row 1 "efg ".
        // Charwise selection bytes 2..6 = "cdef", which spans the wrap.
        let text = TextBuffer::from_str("abcdefg");
        let got: Vec<_> =
            visible_range_rects(rect(0, 0, 4, 2), &text, 0, 0, true, 2, 6, 4).collect();
        assert_eq!(got, vec![rect(2, 0, 2, 1), rect(0, 1, 2, 1)]);
    }

    #[test]
    fn wrap_syntax_highlighting_preserved_across_segments() {
        // A long identifier-then-keyword line wrapped into 2 visual
        // rows. Verify that styled runs survive on the second segment
        // — the wrap walker must clip runs spanning wrap boundaries
        // (though in this specific input the runs don't span).
        let text = TextBuffer::from_str("let aaaaaaa = 1;");
        let theme = themed();
        let runs = vec![
            StyleRun {
                offset: 0,
                len: 3,
                style: Style::DEFAULT.with_fg(keyword_fg()),
            },
            StyleRun {
                offset: 14,
                len: 1,
                style: Style::DEFAULT.with_fg(number_fg()),
            },
        ];
        let mut db = Buffer::new(8, 2);
        render(
            &mut db,
            rect(0, 0, 8, 2),
            &text,
            &theme,
            4,
            Cursor { row: 0, col: 0 },
            Mode::Normal,
            None,
            0,
            0,
            true,
            &runs,
            None,
        );
        // `let` keyword is on row 0 cols 0..=2.
        let cells = db.current().cells();
        assert_eq!(cells[0].style().fg(), Some(keyword_fg()));
        // Row 1 starts at display col 8 — `1` at byte 14 lives at col
        // 14, i.e. row 1 col 6.
        let one_cell = cells[1 * 8 + 6];
        assert_eq!(one_cell.text_inline(), Some("1"));
        assert_eq!(one_cell.style().fg(), Some(number_fg()));
    }

    #[test]
    fn wrap_layout_prefix_matches_manual_fold() {
        let text = TextBuffer::from_str("abc\n\t\nfoobarbaz\nxy");
        let layout = WrapLayout::new(&text, 4, 4);
        let mut expected = 0u32;
        assert_eq!(layout.rows_before(0), 0);
        for row in 0..text.line_count() {
            let line = text.line(row);
            let rows = wrapped_line_rows(line.as_ref(), 4, 4) as u32;
            assert_eq!(layout.line_rows(row), rows);
            expected += rows;
            assert_eq!(layout.rows_before(row + 1), expected);
        }
    }
}
