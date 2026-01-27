use super::*;
use unicode_width::UnicodeWidthStr;

/// Vertical alignment within a region.
#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub enum VAlign {
    /// Aligns content to the top.
    #[default]
    Top,
    /// Centers content vertically.
    Center,
    /// Aligns content to the bottom.
    Bottom,
}

/// Horizontal alignment within a region.
#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub enum HAlign {
    /// Aligns content to the left.
    #[default]
    Left,
    /// Centers content horizontally.
    Center,
    /// Aligns content to the right.
    Right,
}

/// Properties controlling how content is rendered within a region.
#[derive(Default, Clone, Copy)]
pub struct RenderProperties {
    /// Text style to apply.
    pub style: Style,
    /// Vertical alignment.
    pub valign: VAlign,
    /// Horizontal alignment.
    pub halign: HAlign,
    /// Left offset (consumed from left edge).
    pub offset: u16,
    /// Right offset (consumed from right edge).
    pub right_offset: u16,
    /// Whether to show ellipsis ('…') when text is truncated.
    pub ellipsis: bool,
}

/// Marker type to enable ellipsis truncation.
///
/// When applied, text that overflows will be truncated with '…'.
#[derive(Clone, Copy)]
pub struct Ellipsis(pub bool);

/// A property that can be applied to [`RenderProperties`].
///
/// Implemented for [`Style`], [`VAlign`], [`HAlign`], and [`RenderProperties`].
pub trait RenderProperty {
    /// Applies this property to the given render properties.
    fn apply(self, properties: &mut RenderProperties);
}

impl RenderProperty for Style {
    fn apply(self, properties: &mut RenderProperties) {
        properties.style = self;
    }
}
impl RenderProperty for VAlign {
    fn apply(self, properties: &mut RenderProperties) {
        properties.valign = self;
    }
}
impl RenderProperty for HAlign {
    fn apply(self, properties: &mut RenderProperties) {
        properties.halign = self;
    }
}
impl RenderProperty for RenderProperties {
    fn apply(self, properties: &mut RenderProperties) {
        *properties = self;
    }
}

impl RenderProperty for Ellipsis {
    fn apply(self, properties: &mut RenderProperties) {
        properties.ellipsis = self.0;
    }
}

impl DisplayRect {
    /// Adds a render property to this display rectangle.
    pub fn with(self, property: impl RenderProperty) -> DisplayRect {
        let mut properties = self.properties;
        property.apply(&mut properties);
        DisplayRect {
            rect: self.rect,
            properties,
        }
    }

    /// Fills the rectangle with the current style.
    pub fn fill<'a>(self, buf: &mut DoubleBuffer) -> DisplayRect {
        if self.rect.is_empty() {
            return self;
        }
        buf.current.set_style(self.rect, self.properties.style);
        self
    }

    /// Advances the horizontal offset by the given amount.
    ///
    /// For left/center alignment, this increases the left offset.
    /// For right alignment, this increases the right offset.
    pub fn skip(mut self, amount: u16) -> DisplayRect {
        match self.properties.halign {
            HAlign::Left | HAlign::Center => {
                self.properties.offset = self.properties.offset.saturating_add(amount);
            }
            HAlign::Right => {
                self.properties.right_offset = self.properties.right_offset.saturating_add(amount);
            }
        }
        self
    }
    /// Renders formatted content using [`Display`](std::fmt::Display).
    pub fn fmt<'a>(self, buf: &mut DoubleBuffer, content: impl std::fmt::Display) -> DisplayRect {
        if self.rect.is_empty() {
            return self;
        }
        let start_buf = buf.buf.len();
        write!(buf.buf, "{content}").unwrap();
        let text = unsafe { std::str::from_utf8_unchecked(&buf.buf[start_buf..]) };
        let rect = self.text_inner(&mut buf.current, text);
        unsafe {
            buf.buf.set_len(start_buf);
        }
        rect
    }
    /// Renders the given text string.
    pub fn text(self, buf: &mut DoubleBuffer, text: &str) -> DisplayRect {
        self.text_inner(&mut buf.current, text)
    }
    pub(crate) fn text_inner(mut self, buf: &mut Buffer, text: &str) -> DisplayRect {
        if self.rect.is_empty() {
            return self;
        }
        let left = self.properties.offset;
        let right = self.properties.right_offset;
        let available_width = (self.rect.w).saturating_sub(left).saturating_sub(right) as usize;

        let text_width = UnicodeWidthStr::width(text);
        let needs_ellipsis = self.properties.ellipsis && text_width > available_width;
        let truncate_width = if needs_ellipsis {
            available_width.saturating_sub(1)
        } else {
            available_width
        };

        match self.properties.halign {
            HAlign::Left => {
                let start_x = self.rect.x.saturating_add(left);
                let (nx, _ny) = buf.set_stringn(
                    start_x,
                    self.rect.y,
                    text,
                    truncate_width,
                    self.properties.style,
                );
                let mut end_x = nx;
                if needs_ellipsis && available_width > 0 {
                    buf.set_string(nx, self.rect.y, "…", self.properties.style);
                    end_x = nx.saturating_add(1);
                }
                self.properties.offset = end_x.saturating_sub(self.rect.x);
                self
            }
            HAlign::Center => {
                let start_x = if text_width >= available_width {
                    self.rect.x.saturating_add(left)
                } else {
                    self.rect
                        .x
                        .saturating_add(left)
                        .saturating_add((available_width - text_width) as u16 / 2)
                };
                let (nx, _ny) = buf.set_stringn(
                    start_x,
                    self.rect.y,
                    text,
                    truncate_width,
                    self.properties.style,
                );
                if needs_ellipsis && available_width > 0 {
                    buf.set_string(nx, self.rect.y, "…", self.properties.style);
                }
                // Center consumes the entire remaining width
                self.properties.offset = self.rect.w;
                self
            }
            HAlign::Right => {
                // Place text at right edge minus right_offset
                let right_edge = self.rect.w.saturating_sub(right);
                let display_width = if needs_ellipsis {
                    available_width
                } else {
                    text_width.min(available_width)
                };
                let start_x = self
                    .rect
                    .x
                    .saturating_add(right_edge)
                    .saturating_sub(display_width as u16);
                let (nx, _ny) = buf.set_stringn(
                    start_x,
                    self.rect.y,
                    text,
                    truncate_width,
                    self.properties.style,
                );
                if needs_ellipsis && available_width > 0 {
                    buf.set_string(nx, self.rect.y, "…", self.properties.style);
                }
                // Update right_offset: distance from right edge to where text started
                self.properties.right_offset = self
                    .rect
                    .w
                    .saturating_sub(start_x.saturating_sub(self.rect.x));
                self
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper to render a DoubleBuffer and parse the VT output
    fn render_and_parse(db: &mut DoubleBuffer) -> vt100::Parser {
        let mut parser = vt100::Parser::new(db.height(), db.width(), 0);
        db.render_internal();
        parser.process(&db.buf);
        db.buf.clear();
        parser
    }

    // Helper for aquiring the contents directly from blank terminal size,
    // a function.
    fn render(w: u16, h: u16, mut func: impl FnMut(Rect, &mut DoubleBuffer)) -> String {
        let mut db = DoubleBuffer::new(w, h);
        let rect = db.rect();
        func(rect, &mut db);
        let parser = render_and_parse(&mut db);
        parser.screen().contents()
    }

    #[test]
    fn halign_right_offset() {
        let mut buffer = Buffer::new(10, 1);
        let rect = Rect {
            x: 0,
            y: 0,
            w: 10,
            h: 1,
        };

        // Test with text that fits: "abc" (width 3) in a 10-wide rect
        let result = rect.with(HAlign::Right).text_inner(&mut buffer, "abc");
        // Right-aligned text should position "abc" at x=7,8,9
        assert_eq!(buffer.cells[7].text(), "a");
        assert_eq!(buffer.cells[8].text(), "b");
        assert_eq!(buffer.cells[9].text(), "c");
        // Right offset should be 3 (consumed from right)
        assert_eq!(result.properties.right_offset, 3);

        // Test chaining: add more text before the existing text
        let mut buffer2 = Buffer::new(10, 1);
        let rect2 = Rect {
            x: 0,
            y: 0,
            w: 10,
            h: 1,
        };
        let result2 = rect2.with(HAlign::Right).text_inner(&mut buffer2, "cd");
        // "cd" at x=8,9, right_offset=2
        assert_eq!(result2.properties.right_offset, 2);
        // Now add "ab" before it - should go at x=6,7
        let result3 = result2.text_inner(&mut buffer2, "ab");
        assert_eq!(buffer2.cells[6].text(), "a");
        assert_eq!(buffer2.cells[7].text(), "b");
        assert_eq!(buffer2.cells[8].text(), "c");
        assert_eq!(buffer2.cells[9].text(), "d");
        // Right offset should now be 4 (consumed 4 from right)
        assert_eq!(result3.properties.right_offset, 4);
    }

    #[test]
    fn halign_center_offset() {
        let mut buffer = Buffer::new(10, 1);
        let rect = Rect {
            x: 0,
            y: 0,
            w: 10,
            h: 1,
        };

        // Test with text that fits: "abcd" (width 4) in a 10-wide rect
        // Should be centered at x=3,4,5,6 (3 spaces on each side)
        let result = rect.with(HAlign::Center).text_inner(&mut buffer, "abcd");
        assert_eq!(buffer.cells[3].text(), "a");
        assert_eq!(buffer.cells[4].text(), "b");
        assert_eq!(buffer.cells[5].text(), "c");
        assert_eq!(buffer.cells[6].text(), "d");
        // Center should consume the entire rectangle
        assert_eq!(result.properties.offset, 10);
    }

    #[test]
    fn vt100_left_align_with_skip() {
        let contents = render(20, 1, |rect, buf| {
            rect.display().text(buf, "hello").skip(2).text(buf, "world");
        });
        assert_eq!(contents.trim_end(), "hello  world");
    }

    #[test]
    fn vt100_right_align_with_skip() {
        let contents = render(20, 1, |rect, buf| {
            rect.with(HAlign::Right)
                .text(buf, "world")
                .skip(2)
                .text(buf, "hello");
        });
        // "world" at 15-19, gap at 13-14, "hello" at 8-12
        assert_eq!(&contents[15..20], "world");
        assert_eq!(&contents[13..15], "  ");
        assert_eq!(&contents[8..13], "hello");
    }

    #[test]
    fn vt100_left_then_right_align() {
        let contents = render(20, 1, |rect, buf| {
            rect.display()
                .text(buf, "LEFT")
                .with(HAlign::Right)
                .text(buf, "RIGHT");
        });
        // "LEFT" at 0-3, "RIGHT" at 15-19, should not overlap
        assert_eq!(&contents[0..4], "LEFT");
        assert_eq!(&contents[15..20], "RIGHT");
    }

    #[test]
    fn vt100_right_then_left_align() {
        let contents = render(20, 1, |rect, buf| {
            rect.with(HAlign::Right)
                .text(buf, "RIGHT")
                .with(HAlign::Left)
                .text(buf, "LEFT");
        });
        // "LEFT" at 0-3, "RIGHT" at 15-19, should not overlap
        assert_eq!(&contents[0..4], "LEFT");
        assert_eq!(&contents[15..20], "RIGHT");
    }

    #[test]
    fn vt100_left_and_right_fill_available() {
        let contents = render(10, 1, |rect, buf| {
            rect.display()
                .text(buf, "abc")
                .with(HAlign::Right)
                .text(buf, "xyz");
        });
        // "abc" at 0-2, "xyz" at 7-9
        assert_eq!(&contents[0..3], "abc");
        assert_eq!(&contents[7..10], "xyz");
    }

    #[test]
    fn vt100_skip_reduces_available_width() {
        let contents = render(10, 1, |rect, buf| {
            rect.display().skip(3).with(HAlign::Right).text(buf, "test");
        });
        // "test" should be at right edge: positions 6-9
        assert_eq!(&contents[6..10], "test");
        // Positions 0-2 should be spaces (skipped)
        assert_eq!(&contents[0..3], "   ");
    }

    #[test]
    fn vt100_overflow_truncates() {
        let contents = render(5, 1, |rect, buf| {
            rect.display().text(buf, "hello world");
        });
        // Should be truncated to "hello"
        assert_eq!(contents.trim_end(), "hello");
    }

    #[test]
    fn vt100_ellipsis_left_align() {
        let contents = render(8, 1, |rect, buf| {
            rect.with(Ellipsis(true)).text(buf, "hello world");
        });
        // Should be "hello w…" (7 chars + ellipsis)
        assert_eq!(contents.trim_end(), "hello w…");
    }

    #[test]
    fn vt100_ellipsis_no_truncation_needed() {
        let contents = render(20, 1, |rect, buf| {
            rect.with(Ellipsis(true)).text(buf, "hello");
        });
        assert_eq!(contents.trim_end(), "hello");
    }

    #[test]
    fn vt100_ellipsis_right_align() {
        let contents = render(8, 1, |rect, buf| {
            rect.with(HAlign::Right)
                .with(Ellipsis(true))
                .text(buf, "hello world");
        });
        // Should be "hello w…" (truncated with ellipsis at end, right-aligned)
        assert_eq!(contents.trim_end(), "hello w…");
    }

    #[test]
    fn vt100_ellipsis_center_align() {
        let contents = render(8, 1, |rect, buf| {
            rect.with(HAlign::Center)
                .with(Ellipsis(true))
                .text(buf, "hello world");
        });
        // Should be "hello w…" (truncated with ellipsis at end)
        assert_eq!(contents.trim_end(), "hello w…");
    }

    #[test]
    fn vt100_ellipsis_exact_fit() {
        let contents = render(5, 1, |rect, buf| {
            rect.with(Ellipsis(true)).text(buf, "hello");
        });
        assert_eq!(contents.trim_end(), "hello");
    }

    #[test]
    fn vt100_ellipsis_one_over() {
        let contents = render(5, 1, |rect, buf| {
            rect.with(Ellipsis(true)).text(buf, "hello!");
        });
        // Should be "hell…"
        assert_eq!(contents.trim_end(), "hell…");
    }
}
