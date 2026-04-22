use std::{borrow::Cow, ops::Range};

use unicode_segmentation::UnicodeSegmentation;

/// Contiguous byte range in a text buffer, stored as `(offset, len)`.
///
/// Shared currency across the editor's tracking and chunking APIs
/// ([`Replacement::project`], [`Editor::for_each_padded_chunk`],
/// [`Editor::visible_byte_span`]). Overlay state that tracks buffer
/// ranges (search matches, spellcheck regions) composes with these
/// APIs directly when stored as `Vec<Span>`.
///
/// [`Editor`]: crate::Editor
/// [`Editor::for_each_padded_chunk`]: crate::Editor::for_each_padded_chunk
/// [`Editor::visible_byte_span`]: crate::Editor::visible_byte_span
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Span {
    /// Byte offset of the first byte.
    pub offset: u32,
    /// Length in bytes.
    pub len: u32,
}

impl Span {
    /// Creates a span of `len` bytes starting at `offset`.
    pub const fn new(offset: u32, len: u32) -> Self {
        Self { offset, len }
    }

    /// Creates a zero-length span at `offset`.
    pub const fn empty_at(offset: u32) -> Self {
        Self { offset, len: 0 }
    }

    /// Returns the byte offset one past the last byte.
    pub const fn end(&self) -> u32 {
        self.offset + self.len
    }

    /// Returns `true` when the span covers zero bytes.
    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Returns `true` when `offset` lies in `[self.offset, self.end())`.
    pub const fn contains(&self, offset: u32) -> bool {
        offset >= self.offset && offset < self.end()
    }

    /// Returns `true` when the two spans share at least one byte.
    pub const fn overlaps(&self, other: &Self) -> bool {
        self.offset < other.end() && other.offset < self.end()
    }
}

/// A byte-range replacement to apply to a [`TextBuffer`].
///
/// `offset` and `old_len` refer to the buffer *before* the edit: the
/// bytes currently at `offset..offset + old_len` are replaced with
/// the contents of `new`.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Edit {
    /// Start byte of the replaced range.
    pub offset: u32,
    /// Length in bytes of the replaced range.
    pub old_len: u32,
    /// Text to splice in place of the replaced range.
    pub new: String,
}

impl Edit {
    /// Creates an insertion of `new` at `offset`.
    pub fn insert(offset: u32, new: String) -> Self {
        Self {
            offset,
            old_len: 0,
            new,
        }
    }

    /// Creates a deletion of `old_len` bytes starting at `offset`.
    pub fn delete(offset: u32, old_len: u32) -> Self {
        Self {
            offset,
            old_len,
            new: String::new(),
        }
    }

    /// Creates a replacement of `old_len` bytes at `offset` with `new`.
    pub fn replace(offset: u32, old_len: u32, new: String) -> Self {
        Self {
            offset,
            old_len,
            new,
        }
    }
}

/// Shape of a byte-range replacement: what region changed, and by
/// how much it grew or shrank.
///
/// The three edit shapes are encoded by varying the lengths:
///
/// - insert: `old_len == 0`
/// - delete: `new_len == 0`
/// - replacement: both non-zero
///
/// `offset` and `old_len` are expressed in the coordinate space of
/// the buffer *immediately before* the replacement was applied.
/// Merged replacements (see [`Self::merge`]) keep the coordinate
/// space of the buffer before the *first* replacement in the chain.
///
/// Paired with [`TrackedChange`] so overlays can rescan only the
/// affected region after an edit rather than the whole buffer.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Replacement {
    /// Start byte of the replaced range in the pre-edit buffer.
    pub offset: u32,
    /// Length in bytes of the replaced range in the pre-edit buffer.
    pub old_len: u32,
    /// Length in bytes of the text inserted in its place.
    pub new_len: u32,
}

impl Replacement {
    /// Creates a replacement covering `old_len` bytes at `offset` that
    /// leaves `new_len` bytes in their place.
    pub const fn new(offset: u32, old_len: u32, new_len: u32) -> Self {
        Self {
            offset,
            old_len,
            new_len,
        }
    }

    /// Returns `new_len - old_len` as a signed delta: positive for net
    /// insertions, negative for net deletions.
    pub const fn delta(&self) -> i64 {
        self.new_len as i64 - self.old_len as i64
    }

    /// Projects `span` from pre-replacement coordinates into
    /// post-replacement coordinates.
    ///
    /// - Spans entirely before the replaced range pass through
    ///   unchanged.
    /// - Spans entirely after the replaced range are shifted by
    ///   [`Self::delta`].
    /// - Spans overlapping the replaced range return [`None`]; the
    ///   caller typically drops them and rescans the new region.
    ///
    /// # Examples
    ///
    /// Shift a list of match positions past a recorded replacement:
    ///
    /// ```
    /// use extui_editor::{Replacement, Span};
    ///
    /// let rep = Replacement::new(10, 3, 5);
    /// let mut matches = vec![Span::new(2, 4), Span::new(11, 2), Span::new(20, 3)];
    /// matches.retain_mut(|m| match rep.project(*m) {
    ///     Some(shifted) => { *m = shifted; true }
    ///     None => false,
    /// });
    /// assert_eq!(matches, vec![Span::new(2, 4), Span::new(22, 3)]);
    /// ```
    pub fn project(&self, span: Span) -> Option<Span> {
        if span.end() <= self.offset {
            Some(span)
        } else if span.offset >= self.offset + self.old_len {
            let shifted = (span.offset as i64 + self.delta()) as u32;
            Some(Span::new(shifted, span.len))
        } else {
            None
        }
    }

    /// Folds `next` into `self` and returns a single replacement with
    /// the same net effect as applying both in sequence.
    ///
    /// `next.offset` is interpreted in the coordinate space of the
    /// buffer *after* `self` has been applied. The returned
    /// replacement expresses the combined change in the coordinate
    /// space before `self`.
    ///
    /// When the two replacements touch disjoint regions, the merged
    /// range covers their bounding hull: bytes between them are
    /// reported as modified even when their content hasn't changed.
    /// Keeping a single contiguous range lets consumers rescan the
    /// affected area in one pass.
    pub fn merge(self, next: Replacement) -> Replacement {
        // `self` is in original coords; `next` is in post-`self` coords.
        // Every byte outside self's replaced range in the post-`self`
        // buffer maps back to original coords by subtracting `delta`
        // (past the range) or is identity (before it); bytes inside the
        // replacement block have no single original offset, so we clamp
        // to the range's edges.
        let cur_mod_start = self.offset;
        let cur_mod_end = self.offset + self.new_len;
        let orig_mod_end = self.offset + self.old_len;
        let delta = self.delta();

        let r_start = next.offset;
        let r_end = next.offset + next.old_len;

        let new_orig_left = if r_start < cur_mod_start {
            r_start
        } else {
            cur_mod_start
        };
        let new_orig_right = if r_end <= cur_mod_end {
            orig_mod_end
        } else {
            (r_end as i64 - delta) as u32
        };

        let hull_left = cur_mod_start.min(r_start);
        let hull_right = cur_mod_end.max(r_end);
        let new_len = (hull_right - hull_left) - (r_end - r_start) + next.new_len;

        Replacement {
            offset: new_orig_left,
            old_len: new_orig_right - new_orig_left,
            new_len,
        }
    }
}

/// Result of draining the editor's tracked changes.
///
/// Returned by [`Editor::take_tracked_change`]. The `Reset` variant
/// covers both the first drain after tracking was enabled and a
/// wholesale buffer replacement ([`Editor::set_lines`] or
/// [`Editor::clear`]) while tracking was on, so consumers observe a
/// single channel and never need to inspect [`Editor::text_version`]
/// to distinguish the two.
///
/// [`Editor::take_tracked_change`]: crate::Editor::take_tracked_change
/// [`Editor::set_lines`]: crate::Editor::set_lines
/// [`Editor::clear`]: crate::Editor::clear
/// [`Editor::text_version`]: crate::Editor::text_version
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TrackedChange {
    /// No edits have been recorded since the last drain.
    None,
    /// Tracking was primed or the buffer was wholly replaced.
    ///
    /// Overlay state keyed against the prior buffer is stale;
    /// rebuild from [`Editor::text_slices`].
    ///
    /// [`Editor::text_slices`]: crate::Editor::text_slices
    Reset,
    /// Net change since the last drain, merged into one
    /// [`Replacement`] in the coordinates of the buffer as it was
    /// immediately after the previous drain.
    Merged(Replacement),
}

const DEFAULT_GAP: usize = 128;

#[derive(Clone, Debug)]
struct GapBuffer {
    bytes: Vec<u8>,
    gap_start: usize,
    gap_end: usize,
}

impl Default for GapBuffer {
    fn default() -> Self {
        Self::with_text("")
    }
}

impl GapBuffer {
    fn with_text(s: &str) -> Self {
        let mut bytes = vec![0; s.len() + DEFAULT_GAP];
        bytes[..s.len()].copy_from_slice(s.as_bytes());
        Self {
            bytes,
            gap_start: s.len(),
            gap_end: s.len() + DEFAULT_GAP,
        }
    }

    fn len(&self) -> usize {
        self.bytes.len() - self.gap_len()
    }

    fn gap_len(&self) -> usize {
        self.gap_end - self.gap_start
    }

    pub(crate) fn as_slices(&self) -> (&str, &str) {
        (
            Self::bytes_to_str(&self.bytes[0..self.gap_start]),
            Self::bytes_to_str(&self.bytes[self.gap_end..]),
        )
    }

    fn page(&self, offset: usize) -> (usize, &[u8]) {
        let len = self.len();
        if offset >= len {
            return (len, &[]);
        }
        if offset < self.gap_start {
            (0, &self.bytes[..self.gap_start])
        } else {
            (self.gap_start, &self.bytes[self.gap_end..])
        }
    }

    fn slice(&self, range: Range<usize>) -> Cow<'_, str> {
        assert!(range.start <= range.end, "invalid slice range");
        assert!(range.end <= self.len(), "slice past end of gap buffer");

        if range.is_empty() {
            return Cow::Borrowed("");
        }

        if range.end <= self.gap_start {
            return Cow::Borrowed(Self::bytes_to_str(&self.bytes[range.start..range.end]));
        }

        let gap_len = self.gap_len();
        if range.start >= self.gap_start {
            let start = range.start + gap_len;
            let end = range.end + gap_len;
            return Cow::Borrowed(Self::bytes_to_str(&self.bytes[start..end]));
        }

        let mut out = String::with_capacity(range.end - range.start);
        out.push_str(Self::bytes_to_str(&self.bytes[range.start..self.gap_start]));
        let tail_len = range.end - self.gap_start;
        out.push_str(Self::bytes_to_str(
            &self.bytes[self.gap_end..self.gap_end + tail_len],
        ));
        Cow::Owned(out)
    }

    fn replace_range(&mut self, start: usize, end: usize, replacement: &str) {
        self.move_gap(start);
        self.gap_end += end - start;
        self.ensure_gap(replacement.len());

        let insert_end = self.gap_start + replacement.len();
        self.bytes[self.gap_start..insert_end].copy_from_slice(replacement.as_bytes());
        self.gap_start = insert_end;
    }

    fn move_gap(&mut self, offset: usize) {
        assert!(offset <= self.len(), "gap move past end of buffer");

        if offset < self.gap_start {
            let shift = self.gap_start - offset;
            self.bytes
                .copy_within(offset..self.gap_start, self.gap_end - shift);
            self.gap_start = offset;
            self.gap_end -= shift;
        } else if offset > self.gap_start {
            let shift = offset - self.gap_start;
            self.bytes
                .copy_within(self.gap_end..self.gap_end + shift, self.gap_start);
            self.gap_start += shift;
            self.gap_end += shift;
        }
    }

    fn ensure_gap(&mut self, min_size: usize) {
        if self.gap_len() >= min_size {
            return;
        }

        let live_len = self.len();
        let grow_by = min_size.max(live_len / 2).max(DEFAULT_GAP);
        let before = self.gap_start;
        let after = self.bytes.len() - self.gap_end;
        let mut bytes = vec![0; live_len + grow_by];
        bytes[..before].copy_from_slice(&self.bytes[..before]);
        let new_gap_end = before + grow_by;
        bytes[new_gap_end..new_gap_end + after].copy_from_slice(&self.bytes[self.gap_end..]);
        self.bytes = bytes;
        self.gap_end = new_gap_end;
    }

    fn byte_at(&self, offset: usize) -> Option<u8> {
        if offset >= self.len() {
            None
        } else {
            Some(self.bytes[self.logical_to_physical(offset)])
        }
    }

    fn logical_to_physical(&self, offset: usize) -> usize {
        if offset < self.gap_start {
            offset
        } else {
            offset + self.gap_len()
        }
    }

    fn is_char_boundary(&self, offset: usize) -> bool {
        if offset == 0 || offset == self.len() {
            return true;
        }
        matches!(self.byte_at(offset), Some(b) if !is_utf8_continuation(b))
    }

    fn bytes_to_str(bytes: &[u8]) -> &str {
        std::str::from_utf8(bytes).expect("gap buffer must contain valid UTF-8")
    }
}

fn is_utf8_continuation(b: u8) -> bool {
    (b & 0b1100_0000) == 0b1000_0000
}

/// The underlying gap-buffered text store with a line index.
///
/// Exposed by [`Editor::text_buffer`] for integrations that drive an
/// external highlighter (`tinyhl`, tree-sitter, …) and need direct
/// paged access to the buffer bytes via [`Self::page`].
///
/// Lines follow nvim's line model: a trailing `\n` introduces a
/// trailing empty line, and the buffer always exposes at least one
/// (possibly empty) line.
///
/// [`Editor::text_buffer`]: crate::Editor::text_buffer
#[derive(Clone, Debug)]
pub struct TextBuffer {
    text: GapBuffer,
    line_starts: Vec<u32>,
}

impl Default for TextBuffer {
    fn default() -> Self {
        Self {
            text: GapBuffer::default(),
            line_starts: vec![0],
        }
    }
}

impl TextBuffer {
    /// Creates an empty buffer with a single empty line.
    pub fn new() -> Self {
        Self::default()
    }

    #[cfg(test)]
    pub fn from_str(s: &str) -> Self {
        let mut b = Self::default();
        b.set_text(s);
        b
    }

    /// Returns the total byte length of the buffer.
    pub fn len(&self) -> usize {
        self.text.len()
    }

    /// Returns a contiguous byte page starting at or after `offset`,
    /// paired with the page's base offset in the buffer.
    ///
    /// The buffer holds its text in at most two contiguous pages, so
    /// a forward-walking reader encounters at most one page boundary.
    /// An empty returned slice signals end-of-buffer.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// # use extui_editor::TextBuffer;
    /// # let buf: TextBuffer = unimplemented!();
    /// let mut offset = 0;
    /// loop {
    ///     let (base, page) = buf.page(offset);
    ///     if page.is_empty() { break; }
    ///     // ... consume page[..] starting at `base` ...
    ///     offset = base + page.len() as u32;
    /// }
    /// ```
    pub fn page(&self, offset: u32) -> (u32, &[u8]) {
        let (base, page) = self.text.page(offset as usize);
        (base as u32, page)
    }

    /// Returns the number of lines, always at least 1.
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }

    /// Returns the index of the last row.
    pub fn max_row(&self) -> usize {
        self.line_count().saturating_sub(1)
    }

    /// Returns the text of `row`, excluding the trailing newline.
    ///
    /// Rows outside `0..line_count()` return the empty string. The
    /// returned [`Cow`] borrows when the line lies on one side of the
    /// internal gap and allocates when it straddles the gap.
    pub fn line(&self, row: usize) -> Cow<'_, str> {
        let Some(range) = self.line_range(row) else {
            return Cow::Borrowed("");
        };
        self.text.slice(range)
    }

    /// Returns the byte length of `row`, excluding the trailing newline.
    pub fn line_len(&self, row: usize) -> usize {
        self.line_range(row).map_or(0, |range| range.len())
    }

    fn line_range(&self, row: usize) -> Option<Range<usize>> {
        let start = *self.line_starts.get(row)? as usize;
        let end = self
            .line_starts
            .get(row + 1)
            .map(|&next| next as usize - 1)
            .unwrap_or(self.len());
        Some(start..end)
    }

    /// Returns the byte offset of `row`'s first byte, or
    /// [`Self::len`] when `row` is past the end.
    pub fn line_start(&self, row: usize) -> u32 {
        self.line_starts
            .get(row)
            .copied()
            .unwrap_or(self.len() as u32)
    }

    /// Maps a `(row, col)` position to a flat byte offset, clamping
    /// `col` to the row's byte length.
    pub fn rowcol_to_offset(&self, row: usize, col: usize) -> u32 {
        let start = self.line_start(row);
        let line_len = self.line_len(row) as u32;
        start + (col as u32).min(line_len)
    }

    /// Inverse of [`Self::rowcol_to_offset`].
    pub fn offset_to_rowcol(&self, offset: u32) -> (usize, usize) {
        let offset = offset.min(self.len() as u32);
        let row = self.row_of_offset(offset as usize);
        let col = offset as usize - self.line_starts[row] as usize;
        (row, col)
    }

    /// Returns the two contiguous halves of the buffer's text, in
    /// logical order. Either may be empty; concatenated they yield
    /// the full buffer without allocation.
    pub fn as_slices(&self) -> (&str, &str) {
        self.text.as_slices()
    }

    /// Returns the entire buffer as an owned [`String`]. Lines are
    /// separated by `\n`.
    pub fn text(&self) -> String {
        self.text.slice(0..self.len()).into_owned()
    }

    /// Replaces the entire contents with `s`.
    ///
    /// A trailing `\n` in `s` introduces a trailing empty line,
    /// matching nvim's line model.
    pub fn set_text(&mut self, s: &str) {
        self.text = GapBuffer::with_text(s);
        self.rebuild_line_starts();
    }

    /// Resets the buffer to a single empty line.
    pub fn clear(&mut self) {
        *self = Self::default();
    }

    /// Applies `edit` to the buffer and returns its inverse, suitable
    /// for pushing onto an undo stack.
    ///
    /// # Panics
    ///
    /// Panics if `edit.offset` or `edit.offset + edit.old_len` fall
    /// past the end of the buffer or on a byte that is not a UTF-8
    /// character boundary.
    pub fn apply(&mut self, edit: &Edit) -> Edit {
        let start = edit.offset as usize;
        let old_len = edit.old_len as usize;
        let len_before = self.len();
        let end = start + old_len;

        assert!(end <= len_before, "edit past end of buffer");
        assert!(
            self.text.is_char_boundary(start),
            "edit offset must be on a UTF-8 boundary"
        );
        assert!(
            self.text.is_char_boundary(end),
            "edit end must be on a UTF-8 boundary"
        );

        let original = self.text.slice(start..end).into_owned();

        let start_row = self.row_of_offset(start);
        let end_row = self.row_of_offset(end);
        let replace_end = if end_row + 1 < self.line_starts.len() {
            end_row + 2
        } else {
            self.line_starts.len()
        };
        let region_start = self.line_starts[start_row] as usize;
        let region_end_old = if end_row + 1 < self.line_starts.len() {
            self.line_starts[end_row + 1] as usize
        } else {
            len_before
        };

        self.text.replace_range(start, end, &edit.new);

        let delta = edit.new.len() as isize - old_len as isize;
        let region_end_new = if region_end_old == len_before {
            self.len()
        } else {
            (region_end_old as isize + delta) as usize
        };
        self.refresh_line_index_after_edit(
            start_row,
            replace_end,
            region_start,
            region_end_new,
            delta,
        );

        Edit {
            offset: edit.offset,
            old_len: edit.new.len() as u32,
            new: original,
        }
    }

    fn row_of_offset(&self, offset: usize) -> usize {
        let offset = offset.min(self.len()) as u32;
        match self.line_starts.binary_search(&offset) {
            Ok(i) => i,
            Err(i) => i - 1,
        }
    }

    fn rebuild_line_starts(&mut self) {
        self.line_starts = self.collect_line_starts(0, self.len());
    }

    fn collect_line_starts(&self, start: usize, end: usize) -> Vec<u32> {
        let mut starts = vec![start as u32];
        self.extend_line_starts(start, end, &mut starts);
        starts
    }

    fn extend_line_starts(&self, start: usize, end: usize, starts: &mut Vec<u32>) {
        let mut offset = start;
        while offset < end {
            let (base, page) = self.text.page(offset);
            if page.is_empty() {
                break;
            }
            let page_start = offset - base;
            let page_end = (end - base).min(page.len());
            for (i, b) in page[page_start..page_end].iter().enumerate() {
                if *b == b'\n' {
                    starts.push((base + page_start + i + 1) as u32);
                }
            }
            offset = base + page_end;
        }
    }

    fn refresh_line_index_after_edit(
        &mut self,
        start_row: usize,
        replace_end: usize,
        region_start: usize,
        region_end_new: usize,
        delta: isize,
    ) {
        let new_starts = self.collect_line_starts(region_start, region_end_new);
        let inserted = new_starts.len();
        self.line_starts.splice(start_row..replace_end, new_starts);
        if delta != 0 {
            for start in &mut self.line_starts[start_row + inserted..] {
                *start = (*start as isize + delta) as u32;
            }
        }
    }
}

/// Rounds `col` down to the nearest grapheme-cluster start in `line`.
pub fn align_to_grapheme_start(line: &str, col: usize) -> usize {
    if col >= line.len() {
        return line.len();
    }
    let mut last = 0;
    for (i, _) in line.grapheme_indices(true) {
        if i > col {
            break;
        }
        last = i;
    }
    last
}

/// Returns the byte index of the last grapheme cluster start in `line`,
/// or `None` if the line is empty.
pub fn last_grapheme_start(line: &str) -> Option<usize> {
    line.grapheme_indices(true).last().map(|(i, _)| i)
}

/// Returns the byte index of the next grapheme after `col`, or `line.len()`
/// if none. Panics if `col` is past the end.
pub fn next_grapheme_start(line: &str, col: usize) -> usize {
    let mut it = line.grapheme_indices(true).skip_while(|(i, _)| *i <= col);
    it.next().map(|(i, _)| i).unwrap_or(line.len())
}

/// Byte index one past the grapheme cluster containing `col`.
///
/// Clamps to `line.len()` so it's safe to call with an unaligned `col`
/// that may sit past EOL. Equivalent to aligning to the grapheme start
/// then stepping to the next grapheme.
pub fn grapheme_end(line: &str, col: usize) -> usize {
    next_grapheme_start(line, align_to_grapheme_start(line, col))
}

/// Returns the byte index of the previous grapheme before `col`, or `0`
/// if none.
pub fn prev_grapheme_start(line: &str, col: usize) -> usize {
    let mut prev = 0;
    for (i, _) in line.grapheme_indices(true) {
        if i >= col {
            break;
        }
        prev = i;
    }
    prev
}

/// Returns the grapheme cluster at `col` (possibly empty if `col` is
/// past the end).
pub fn grapheme_at<'a>(line: &'a str, col: usize) -> &'a str {
    let start = align_to_grapheme_start(line, col);
    let end = next_grapheme_start(line, start);
    &line[start..end]
}

// The two functions below are the underlying building blocks for
// `Editor::for_each_padded_chunk` / `for_each_split_chunk`. They take
// the gap buffer's two partitions as `(a, b)` so they're testable
// without constructing an `Editor`.

/// Clamp `window` into `[0, total_len)` and snap each endpoint to a
/// UTF-8 char boundary — outward (floor on the low end, ceil on the
/// high end) so no valid match in the requested window is lost.
fn snap_window(slices: (&str, &str), window: Span) -> (u32, u32) {
    let (a, b) = slices;
    let al = a.len() as u32;
    let total = al + b.len() as u32;
    let lo = window.offset.min(total);
    let hi = window.end().min(total).max(lo);
    // Snap low down, high up, per slice. The gap itself (`al`) is
    // always a char boundary.
    let lo_snapped = if lo <= al {
        a.floor_char_boundary(lo as usize) as u32
    } else {
        al + b.floor_char_boundary((lo - al) as usize) as u32
    };
    let hi_snapped = if hi <= al {
        a.ceil_char_boundary(hi as usize) as u32
    } else {
        al + b.ceil_char_boundary((hi - al) as usize) as u32
    };
    (lo_snapped, hi_snapped)
}

/// Emit chunks covering `core` padded by `query_len - 1` bytes on
/// each side, so every substring of length up to `query_len` that
/// touches `core` appears contiguously in exactly one emitted chunk.
///
/// The visitor receives `(chunk, base, max_start)`, where `max_start`
/// is an exclusive global upper bound on the match-start positions
/// the chunk is responsible for; callers emit only matches with
/// `start < max_start` to avoid duplicates at the gap bridge.
///
/// Returns the effective post-padding, post-snap window — consumers
/// can use this to seed a drop-zone for their own state management
/// (though for fixed-length pattern search, dropping via
/// [`Replacement::project`] and keying the splice point on
/// `r.offset` is usually enough).
///
/// Allocates at most a `2*(query_len - 1) + 6` byte bridge `String`,
/// and only when the padded window straddles the gap.
pub(crate) fn padded_chunks<F>(
    slices: (&str, &str),
    core: Span,
    query_len: u32,
    mut visit: F,
) -> Span
where
    F: FnMut(&str, u32, u32),
{
    let (a, b) = slices;
    let al = a.len() as u32;
    let total = al + b.len() as u32;
    // Pad `core` by `query_len - 1` on each side, clamp to `[0, total]`,
    // then snap to UTF-8 char boundaries (outward).
    let pad = query_len.saturating_sub(1);
    let padded = Span::new(
        core.offset.saturating_sub(pad),
        core.len.saturating_add(2 * pad),
    );
    let (lo, hi) = snap_window(slices, padded);
    // Clamp to total after pad (snap_window also clamps, but the
    // `+ 2*pad` above might have pushed `end` past `total`).
    let hi = hi.min(total);
    let effective = Span::new(lo, hi.saturating_sub(lo));
    if lo >= hi {
        return effective;
    }
    if hi <= al {
        visit(&a[lo as usize..hi as usize], lo, hi);
        return effective;
    }
    if lo >= al {
        visit(&b[(lo - al) as usize..(hi - al) as usize], lo, hi);
        return effective;
    }

    // Straddle. `query_len <= 1` can't produce straddling matches.
    if query_len <= 1 {
        visit(&a[lo as usize..a.len()], lo, al);
        visit(&b[..(hi - al) as usize], al, hi);
        return effective;
    }

    // Bridge [br_s, br_e) in full-buffer coords. `br_s` floors into
    // `a`; `br_e_in_b` ceils into `b`. Both snaps are outward so the
    // bridge is at least wide enough to hold any straddling match.
    let br_s_raw = al.saturating_sub(pad).max(lo);
    let br_e_raw = (al + pad).min(hi);
    let br_s = a.floor_char_boundary(br_s_raw as usize) as u32;
    let br_e_in_b = b.ceil_char_boundary((br_e_raw - al) as usize);
    let br_e = al + br_e_in_b as u32;

    // Left borrow: `a[lo..al]`. Reports matches in `[lo, br_s)`.
    visit(&a[lo as usize..a.len()], lo, br_s);
    // Bridge: `a[br_s..al] + b[0..br_e_in_b]`. Reports matches in
    // `[br_s, al)` (the straddlers plus any a-only matches whose
    // start is ≥ br_s — the left borrow's filter handed them over).
    let mut bridge = String::with_capacity((br_e - br_s) as usize);
    bridge.push_str(&a[br_s as usize..a.len()]);
    bridge.push_str(&b[..br_e_in_b]);
    visit(&bridge, br_s, al);
    // Right borrow: `b[0..hi-al]`. Reports matches starting at `al`+.
    visit(&b[..(hi - al) as usize], al, hi);
    effective
}

/// Walk left from `idx` through the gap-buffered text until a byte
/// satisfying `is_boundary` is found, returning one past that byte.
/// Returns `0` if no boundary is found before the buffer start.
fn expand_left<P: Fn(char) -> bool>(slices: (&str, &str), idx: u32, is_boundary: &P) -> u32 {
    let (a, b) = slices;
    let al = a.len() as u32;
    if idx <= al {
        a[..idx as usize]
            .rfind(is_boundary)
            .map(|i| i as u32 + 1)
            .unwrap_or(0)
    } else {
        let off = (idx - al) as usize;
        if let Some(i) = b[..off].rfind(is_boundary) {
            return al + i as u32 + 1;
        }
        a.rfind(is_boundary).map(|i| i as u32 + 1).unwrap_or(0)
    }
}

/// Walk right from `idx` through the gap-buffered text until a byte
/// satisfying `is_boundary` is found, returning its byte index.
/// Returns `total` if no boundary is found before the buffer end.
fn expand_right<P: Fn(char) -> bool>(
    slices: (&str, &str),
    idx: u32,
    total: u32,
    is_boundary: &P,
) -> u32 {
    let (a, b) = slices;
    let al = a.len() as u32;
    if idx >= al {
        let off = (idx - al) as usize;
        b[off..]
            .find(is_boundary)
            .map(|i| idx + i as u32)
            .unwrap_or(total)
    } else {
        if let Some(i) = a[idx as usize..].find(is_boundary) {
            return idx + i as u32;
        }
        b.find(is_boundary).map(|i| al + i as u32).unwrap_or(total)
    }
}

/// Emit chunks covering `core` expanded outward until each endpoint
/// sits on a byte satisfying `is_boundary` (or at the buffer edge).
/// Inside the expanded window, the chunk straddling the gap (if any)
/// is allocated as a single `String` containing exactly the segment
/// between the last pre-gap and first post-gap boundary bytes; all
/// other chunks are borrowed.
///
/// Returns the effective post-expansion window so callers can key
/// their "drop zone" / splice point on it without recomputing.
///
/// Each emitted chunk is self-contained under `is_boundary` —
/// consumers that tokenize on the same predicate (spell-check on
/// whitespace, regex on newlines, ...) can process chunks
/// independently without duplicates.
pub(crate) fn split_chunks<F, P>(
    slices: (&str, &str),
    core: Span,
    is_boundary: P,
    mut visit: F,
) -> Span
where
    F: FnMut(&str, u32),
    P: Fn(char) -> bool,
{
    let (a, b) = slices;
    let al = a.len() as u32;
    let total = al + b.len() as u32;
    // Snap the caller's core inward to valid UTF-8 boundaries first,
    // then expand outward until we hit `is_boundary` bytes (or edges).
    let (core_lo, core_hi) = snap_window(slices, core);
    let lo = expand_left(slices, core_lo, &is_boundary);
    let hi = expand_right(slices, core_hi, total, &is_boundary);
    let effective = Span::new(lo, hi.saturating_sub(lo));
    if lo >= hi {
        return effective;
    }
    if hi <= al {
        visit(&a[lo as usize..hi as usize], lo);
        return effective;
    }
    if lo >= al {
        visit(&b[(lo - al) as usize..(hi - al) as usize], lo);
        return effective;
    }

    // Straddle. Find the last boundary in `a[lo..]` and the first in
    // `b[..hi - al]` to isolate the straddling segment.
    let left_end_in_a = a[lo as usize..]
        .rfind(&is_boundary)
        .map(|i| lo + i as u32 + 1)
        .unwrap_or(lo);
    let hi_in_b = (hi - al) as usize;
    let right_start_in_b = b[..hi_in_b]
        .find(&is_boundary)
        .map(|i| i as u32)
        .unwrap_or(hi - al);

    if left_end_in_a > lo {
        visit(&a[lo as usize..left_end_in_a as usize], lo);
    }
    let bridge_len = (al - left_end_in_a) as usize + right_start_in_b as usize;
    if bridge_len > 0 {
        let mut bridge = String::with_capacity(bridge_len);
        bridge.push_str(&a[left_end_in_a as usize..a.len()]);
        bridge.push_str(&b[..right_start_in_b as usize]);
        visit(&bridge, left_end_in_a);
    }
    if (hi - al) > right_start_in_b {
        visit(
            &b[right_start_in_b as usize..hi_in_b],
            al + right_start_in_b,
        );
    }
    effective
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn apply_insert_single_char() {
        let mut b = TextBuffer::from_str("helo");
        b.apply(&Edit::insert(2, "l".to_string()));
        assert_eq!(b.text(), "hello");
    }

    #[test]
    fn apply_insert_newline_splits_line() {
        let mut b = TextBuffer::from_str("hello world");
        b.apply(&Edit::insert(5, "\n".to_string()));
        assert_eq!(b.line_count(), 2);
        assert_eq!(b.line(0), "hello");
        assert_eq!(b.line(1), " world");
    }

    #[test]
    fn apply_delete_at_end_of_line_joins_lines() {
        let mut b = TextBuffer::from_str("abc\ndef");
        b.apply(&Edit::delete(3, 1));
        assert_eq!(b.line_count(), 1);
        assert_eq!(b.line(0), "abcdef");
    }

    #[test]
    fn apply_multiline_insert() {
        let mut b = TextBuffer::from_str("ac");
        b.apply(&Edit::insert(1, "b\nB".to_string()));
        assert_eq!(b.line_count(), 2);
        assert_eq!(b.line(0), "ab");
        assert_eq!(b.line(1), "Bc");
    }

    #[test]
    fn apply_delete_across_lines() {
        let mut b = TextBuffer::from_str("foo\nbar\nbaz");
        // Delete "oo\nbar\nba" — offsets 1..10.
        b.apply(&Edit::delete(1, 9));
        assert_eq!(b.line_count(), 1);
        assert_eq!(b.line(0), "fz");
    }

    #[test]
    fn apply_returns_inverse() {
        let mut b = TextBuffer::from_str("hello world");
        let fwd = Edit::replace(6, 5, "rust".to_string());
        let inv = b.apply(&fwd);
        assert_eq!(b.text(), "hello rust");
        assert_eq!(inv, Edit::replace(6, 4, "world".to_string()));
        b.apply(&inv);
        assert_eq!(b.text(), "hello world");
    }

    #[test]
    fn offset_roundtrip() {
        let b = TextBuffer::from_str("foo\nbar\nbaz");
        assert_eq!(b.rowcol_to_offset(0, 0), 0);
        assert_eq!(b.rowcol_to_offset(1, 0), 4);
        assert_eq!(b.rowcol_to_offset(2, 2), 10);
        assert_eq!(b.offset_to_rowcol(0), (0, 0));
        assert_eq!(b.offset_to_rowcol(4), (1, 0));
        assert_eq!(b.offset_to_rowcol(10), (2, 2));
    }

    #[test]
    fn trailing_newline_makes_empty_line() {
        let b = TextBuffer::from_str("foo\n");
        assert_eq!(b.line_count(), 2);
        assert_eq!(b.line(0), "foo");
        assert_eq!(b.line(1), "");
    }

    #[test]
    fn split_line_reads_through_gap() {
        let mut b = TextBuffer::from_str("abcd");
        b.apply(&Edit::insert(2, "X".to_string()));
        assert_eq!(b.line(0), "abXcd");
    }

    #[test]
    fn line_index_stays_correct_after_merging_at_line_start() {
        let mut b = TextBuffer::from_str("foo\nbar\nbaz");
        b.apply(&Edit::delete(3, 1));
        b.apply(&Edit::delete(6, 1));
        assert_eq!(b.line_count(), 1);
        assert_eq!(b.line_start(0), 0);
        assert_eq!(b.line(0), "foobarbaz");
    }

    #[test]
    fn source_pages_switch_at_gap() {
        let mut b = TextBuffer::from_str("hello world");
        b.apply(&Edit::insert(5, ",".to_string()));
        let (base0, page0) = b.page(0);
        assert_eq!(base0, 0);
        assert_eq!(std::str::from_utf8(page0).unwrap(), "hello,");
        let (base1, page1) = b.page(6);
        assert_eq!(base1, 6);
        assert_eq!(std::str::from_utf8(page1).unwrap(), " world");
    }

    // Convenience: record an edit as a `Replacement` in the *current*
    // pre-edit coords, then apply it, so merge tests can mirror how the
    // editor records replacements in the real integration.
    fn record(buf: &mut TextBuffer, edit: Edit) -> Replacement {
        let rep = Replacement {
            offset: edit.offset,
            old_len: edit.old_len,
            new_len: edit.new.len() as u32,
        };
        buf.apply(&edit);
        rep
    }

    // Check that the bytes outside the merged replaced range match
    // between the original and final buffers — i.e. that the merged
    // replacement describes a valid diff between them.
    fn materialize(original: &str, merged: Replacement, final_text: &str) {
        let src_end = (merged.offset + merged.old_len) as usize;
        let final_end = (merged.offset + merged.new_len) as usize;
        assert!(src_end <= original.len());
        assert!(final_end <= final_text.len());
        assert_eq!(
            &original[..merged.offset as usize],
            &final_text[..merged.offset as usize],
            "prefix differs",
        );
        assert_eq!(
            &original[src_end..],
            &final_text[final_end..],
            "suffix differs",
        );
    }

    #[test]
    fn merge_two_inserts_same_position() {
        let mut buf = TextBuffer::from_str("abc");
        let r1 = record(&mut buf, Edit::insert(1, "X".to_string()));
        let r2 = record(&mut buf, Edit::insert(1, "Y".to_string()));
        let merged = r1.merge(r2);
        assert_eq!(merged, Replacement::new(1, 0, 2));
        materialize("abc", merged, &buf.text());
        assert_eq!(buf.text(), "aYXbc");
    }

    #[test]
    fn merge_second_insert_after_first() {
        let mut buf = TextBuffer::from_str("abc");
        let r1 = record(&mut buf, Edit::insert(1, "X".to_string()));
        // In post-r1 coords, offset 3 is between 'b' and 'c'.
        let r2 = record(&mut buf, Edit::insert(3, "Y".to_string()));
        let merged = r1.merge(r2);
        assert_eq!(merged, Replacement::new(1, 1, 3));
        materialize("abc", merged, &buf.text());
        assert_eq!(buf.text(), "aXbYc");
    }

    #[test]
    fn merge_second_insert_before_first() {
        let mut buf = TextBuffer::from_str("abcdef");
        let r1 = record(&mut buf, Edit::insert(4, "X".to_string()));
        let r2 = record(&mut buf, Edit::insert(1, "Y".to_string()));
        let merged = r1.merge(r2);
        assert_eq!(merged, Replacement::new(1, 3, 5));
        materialize("abcdef", merged, &buf.text());
        assert_eq!(buf.text(), "aYbcdXef");
    }

    #[test]
    fn merge_delete_inside_prior_insert() {
        let mut buf = TextBuffer::from_str("abc");
        let r1 = record(&mut buf, Edit::insert(1, "XY".to_string()));
        // Delete 'Y' in post-r1 coords (offset 2..3).
        let r2 = record(&mut buf, Edit::delete(2, 1));
        let merged = r1.merge(r2);
        assert_eq!(merged, Replacement::new(1, 0, 1));
        materialize("abc", merged, &buf.text());
        assert_eq!(buf.text(), "aXbc");
    }

    #[test]
    fn merge_replacement_fully_inside_prior_replacement() {
        let mut buf = TextBuffer::from_str("abc");
        let r1 = record(&mut buf, Edit::replace(1, 1, "XYZ".to_string()));
        let r2 = record(&mut buf, Edit::replace(2, 1, "Q".to_string()));
        let merged = r1.merge(r2);
        assert_eq!(merged, Replacement::new(1, 1, 3));
        materialize("abc", merged, &buf.text());
        assert_eq!(buf.text(), "aXQZc");
    }

    #[test]
    fn merge_delete_spanning_prior_insert_and_beyond() {
        let mut buf = TextBuffer::from_str("abcdef");
        let r1 = record(&mut buf, Edit::insert(2, "XY".to_string()));
        // current: "abXYcdef" (len 8). Delete "Ycd" i.e. bytes 3..6.
        let r2 = record(&mut buf, Edit::delete(3, 3));
        let merged = r1.merge(r2);
        // Original range replaced is [2..4) = "cd" (extended out to the
        // delete's right edge in original coords), with 1 byte "X" kept.
        assert_eq!(merged, Replacement::new(2, 2, 1));
        materialize("abcdef", merged, &buf.text());
        assert_eq!(buf.text(), "abXef");
    }

    #[test]
    fn merge_chain_three_edits_folded_pairwise() {
        let mut buf = TextBuffer::from_str("hello world");
        let r1 = record(&mut buf, Edit::replace(6, 5, "rust".to_string()));
        let r2 = record(&mut buf, Edit::insert(0, "// ".to_string()));
        let r3 = record(&mut buf, Edit::delete(3, 6)); // delete "hello "
        let merged = r1.merge(r2).merge(r3);
        materialize("hello world", merged, &buf.text());
        assert_eq!(buf.text(), "// rust");
    }

    #[test]
    fn merge_insert_at_end_of_prior_replacement_boundary() {
        // Appending right at the edge of a replacement must stay
        // clamped to the replacement's original end — we do *not* want
        // to extend the span into untouched territory past it.
        let mut buf = TextBuffer::from_str("abcdef");
        let r1 = record(&mut buf, Edit::replace(1, 2, "XY".to_string()));
        // current: "aXYdef". Insert 'Z' at pos 3 (right after "XY").
        let r2 = record(&mut buf, Edit::insert(3, "Z".to_string()));
        let merged = r1.merge(r2);
        assert_eq!(merged, Replacement::new(1, 2, 3));
        materialize("abcdef", merged, &buf.text());
        assert_eq!(buf.text(), "aXYZdef");
    }

    #[test]
    fn span_basics() {
        let s = Span::new(3, 4);
        assert_eq!(s.offset, 3);
        assert_eq!(s.end(), 7);
        assert!(!s.is_empty());
        assert!(Span::empty_at(5).is_empty());
        assert!(s.contains(3));
        assert!(s.contains(6));
        assert!(!s.contains(7));
        assert!(s.overlaps(&Span::new(6, 2)));
        assert!(!s.overlaps(&Span::new(7, 1)));
        assert!(!s.overlaps(&Span::new(0, 3)));
    }

    #[test]
    fn project_before_edit_unchanged() {
        let r = Replacement::new(10, 3, 5);
        // Span ends exactly at the edit start — still unchanged.
        assert_eq!(r.project(Span::new(2, 8)), Some(Span::new(2, 8)));
        assert_eq!(r.project(Span::new(0, 4)), Some(Span::new(0, 4)));
    }

    #[test]
    fn project_after_edit_shifts() {
        let r = Replacement::new(10, 3, 5); // delta = +2
        // Span starts exactly at edit end — shifts.
        assert_eq!(r.project(Span::new(13, 4)), Some(Span::new(15, 4)));
        // Pure delete: delta negative.
        let r = Replacement::new(5, 3, 0); // delta = -3
        assert_eq!(r.project(Span::new(8, 2)), Some(Span::new(5, 2)));
    }

    #[test]
    fn project_overlapping_returns_none() {
        let r = Replacement::new(10, 3, 5);
        assert_eq!(r.project(Span::new(9, 2)), None); // straddles left
        assert_eq!(r.project(Span::new(12, 2)), None); // straddles right
        assert_eq!(r.project(Span::new(10, 3)), None); // exact edit
        assert_eq!(r.project(Span::new(11, 1)), None); // inside edit
    }

    #[test]
    fn project_zero_length_span_boundaries() {
        let r = Replacement::new(10, 3, 5);
        // Empty span right at edit start: sits on the boundary — before.
        assert_eq!(r.project(Span::new(10, 0)), Some(Span::new(10, 0)));
        // Empty span right at edit end: past.
        assert_eq!(r.project(Span::new(13, 0)), Some(Span::new(15, 0)));
    }

    #[test]
    fn replacement_delta_sign() {
        assert_eq!(Replacement::new(0, 2, 5).delta(), 3);
        assert_eq!(Replacement::new(0, 5, 2).delta(), -3);
        assert_eq!(Replacement::new(0, 4, 4).delta(), 0);
    }

    fn collect_padded(slices: (&str, &str), core: Span, q: u32) -> (Span, Vec<(String, u32, u32)>) {
        let mut out = Vec::new();
        let effective = padded_chunks(slices, core, q, |c, base, max_start| {
            out.push((c.to_string(), base, max_start));
        });
        (effective, out)
    }

    fn collect_split<P: Fn(char) -> bool>(
        slices: (&str, &str),
        core: Span,
        pred: P,
    ) -> (Span, Vec<(String, u32)>) {
        let mut out = Vec::new();
        let effective = split_chunks(slices, core, pred, |c, base| {
            out.push((c.to_string(), base));
        });
        (effective, out)
    }

    #[test]
    fn padded_chunks_single_side_a() {
        let (eff, chunks) = collect_padded(("hello world", ""), Span::new(0, 11), 3);
        assert_eq!(eff, Span::new(0, 11));
        assert_eq!(chunks.len(), 1);
        assert_eq!(chunks[0], ("hello world".to_string(), 0, 11));
    }

    #[test]
    fn padded_chunks_single_side_b() {
        let (eff, chunks) = collect_padded(("", "hello world"), Span::new(0, 11), 3);
        assert_eq!(eff, Span::new(0, 11));
        assert_eq!(chunks.len(), 1);
        assert_eq!(chunks[0], ("hello world".to_string(), 0, 11));
    }

    #[test]
    fn padded_chunks_straddle_partitions_matches() {
        // a = "abcde" (len 5), b = "fghij" (len 5); total 10.
        // Core covers full buffer. q=3 → pad=2, but padding saturates
        // against buffer bounds so effective = [0, 10).
        let (eff, chunks) = collect_padded(("abcde", "fghij"), Span::new(0, 10), 3);
        assert_eq!(eff, Span::new(0, 10));
        assert_eq!(chunks.len(), 3);
        assert_eq!(chunks[0], ("abcde".to_string(), 0, 3));
        // Bridge: a[3..5] + b[0..2] = "de" + "fg".
        assert_eq!(chunks[1], ("defg".to_string(), 3, 5));
        assert_eq!(chunks[2], ("fghij".to_string(), 5, 10));
    }

    #[test]
    fn padded_chunks_query_len_one_no_bridge() {
        let (eff, chunks) = collect_padded(("abc", "def"), Span::new(0, 6), 1);
        assert_eq!(eff, Span::new(0, 6));
        assert_eq!(chunks.len(), 2);
        assert_eq!(chunks[0], ("abc".to_string(), 0, 3));
        assert_eq!(chunks[1], ("def".to_string(), 3, 6));
    }

    #[test]
    fn padded_chunks_pads_narrow_core_internally() {
        // Core is just the edit region [2, 4). Padding by q-1=2 widens
        // it to [0, 6) — the full buffer in this example — and the
        // straddle bridge falls out correctly.
        let (eff, chunks) = collect_padded(("abc", "def"), Span::new(2, 2), 3);
        assert_eq!(eff, Span::new(0, 6));
        assert_eq!(chunks.len(), 3);
        // Left borrow: `a[0..3]` = "abc"; reports matches in [0, br_s=1).
        assert_eq!(chunks[0], ("abc".to_string(), 0, 1));
        // Bridge [1, 5): `a[1..3] + b[0..2]` = "bc" + "de" = "bcde";
        // reports matches in [br_s=1, al=3).
        assert_eq!(chunks[1], ("bcde".to_string(), 1, 3));
        // Right borrow: `b[0..3]` = "def"; reports matches in [al=3, 6).
        assert_eq!(chunks[2], ("def".to_string(), 3, 6));
    }

    #[test]
    fn padded_chunks_snaps_non_boundary_endpoints() {
        // UTF-8: "héllo" = h(1) é(2) l(1) l(1) o(1) = 6 bytes.
        // Core offset 2 lands mid-é. With q=1 (no padding) the snap
        // behavior is directly observable: lo floors from 2 to 1
        // (start of é), hi ceils from 5 to 5 (already on boundary).
        let (eff, chunks) = collect_padded(("héllo", ""), Span::new(2, 3), 1);
        assert_eq!(eff, Span::new(1, 4));
        assert_eq!(chunks.len(), 1);
        assert_eq!(chunks[0].0.as_str(), "éll");
        assert_eq!(chunks[0].1, 1);
    }

    #[test]
    fn split_chunks_single_side() {
        let (eff, chunks) =
            collect_split(("hello world", ""), Span::new(0, 11), |c| c.is_whitespace());
        assert_eq!(eff, Span::new(0, 11));
        assert_eq!(chunks.len(), 1);
        assert_eq!(chunks[0], ("hello world".to_string(), 0));
    }

    #[test]
    fn split_chunks_straddle_on_whitespace() {
        // a = "hello wo" (8), b = "rld nice" (8), total 16. Core is
        // the full buffer — no expansion possible.
        let (eff, chunks) = collect_split(
            ("hello wo", "rld nice"),
            Span::new(0, 16),
            char::is_whitespace,
        );
        assert_eq!(eff, Span::new(0, 16));
        assert_eq!(chunks.len(), 3);
        assert_eq!(chunks[0], ("hello ".to_string(), 0));
        assert_eq!(chunks[1], ("world".to_string(), 6));
        assert_eq!(chunks[2], (" nice".to_string(), 11));
    }

    #[test]
    fn split_chunks_expands_narrow_core_to_whitespace() {
        // Core targets just the straddling letter 'r' at offset 8
        // (first byte of `b`). Expansion finds the ws at a[5] going
        // left and the ws at b[3] going right (global offset 11),
        // giving effective [6, 11). The straddling word "world"
        // becomes the single emitted chunk.
        let (eff, chunks) = collect_split(
            ("hello wo", "rld nice"),
            Span::new(8, 1),
            char::is_whitespace,
        );
        assert_eq!(eff, Span::new(6, 5));
        assert_eq!(chunks.len(), 1);
        assert_eq!(chunks[0], ("world".to_string(), 6));
    }

    #[test]
    fn split_chunks_no_boundary_expands_to_buffer_edges() {
        // No whitespace anywhere: expansion falls through to buffer
        // edges. Bridge covers the full buffer.
        let (eff, chunks) = collect_split(("abcd", "efgh"), Span::new(2, 4), |c| c.is_whitespace());
        assert_eq!(eff, Span::new(0, 8));
        assert_eq!(chunks.len(), 1);
        assert_eq!(chunks[0], ("abcdefgh".to_string(), 0));
    }

    #[test]
    fn split_chunks_straddle_ws_only_on_one_side() {
        // Whitespace only in `a`: expansion right falls through `b`
        // to the buffer edge.
        let (eff, chunks) =
            collect_split(("foo bar", "baz"), Span::new(0, 10), char::is_whitespace);
        assert_eq!(eff, Span::new(0, 10));
        assert_eq!(chunks.len(), 2);
        assert_eq!(chunks[0], ("foo ".to_string(), 0));
        // Straddler "barbaz" goes entirely into the bridge.
        assert_eq!(chunks[1], ("barbaz".to_string(), 4));
    }

    #[test]
    fn line_split_via_split_chunks() {
        let (eff, chunks) =
            collect_split(("line1\nlin", "e2\nline3"), Span::new(0, 17), |c| c == '\n');
        assert_eq!(eff, Span::new(0, 17));
        assert_eq!(chunks.len(), 3);
        assert_eq!(chunks[0], ("line1\n".to_string(), 0));
        assert_eq!(chunks[1], ("line2".to_string(), 6));
        assert_eq!(chunks[2], ("\nline3".to_string(), 11));
    }
}
