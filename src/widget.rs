//! Optional widget primitives.
//!
//! Contains pre-built widgets for common UI patterns. These serve as both
//! ready-to-use components and examples of how to build widgets using extui
//! primitives.

use crate::{Cell, DoubleBuffer, Rect, vt::Modifier};

/// A vertical scrollbar widget.
///
/// Renders a scrollbar thumb within a track to indicate scroll position.
pub struct ScrollBar {
    /// Total number of scrollable items.
    pub total: u32,
    /// Current scroll offset from the top.
    pub offset: u32,
}

impl ScrollBar {
    /// Renders a thin scrollbar using box-drawing characters.
    ///
    /// Uses half-block characters for sub-cell positioning.
    pub fn render(mut self, area: Rect, buf: &mut DoubleBuffer) {
        const TOP_END: Cell = Cell::new_const("┴", crate::Style::DEFAULT);
        const BOT_END: Cell = Cell::new_const("┬", crate::Style::DEFAULT);
        const THUMB_UPPER: Cell = Cell::new_const("╿", crate::Style::DEFAULT);
        const THUMB_LOWER: Cell = Cell::new_const("╽", crate::Style::DEFAULT);
        const THUMB_FULL: Cell = Cell::new_const("┃", crate::Style::DEFAULT);

        // The number of items/lines visible in the viewport.
        // We subtract 2 for the top and bottom borders of the area.
        let viewport_height = (area.h - 2) as u32;

        if viewport_height >= self.total {
            return;
        }

        let x = area.x;

        buf.set_cell(x, area.y + 1, TOP_END);
        buf.set_cell(x, area.y + area.h - 2, BOT_END);

        // Smoother Scrolling
        let track_height = (area.h - 4) as u32;
        let virtual_track_height = track_height * 2;

        // Proportional to the percentage visible
        let thumb_height = std::cmp::max((virtual_track_height * viewport_height) / self.total, 1);

        // Clamp the scroll offset to a valid range.
        self.offset = self.offset.min(self.total.saturating_sub(1));

        let mut thumb_offset = if self.offset == 0 {
            0
        } else {
            // Max 1 ensures a visible gap when not completely at the top
            std::cmp::max((virtual_track_height * self.offset) / self.total, 1)
        };

        let is_at_bottom = self.total - self.offset <= viewport_height;
        if is_at_bottom {
            thumb_offset = virtual_track_height - thumb_height;
        }

        let thumb_start = thumb_offset as i32;
        let thumb_end = (thumb_offset + thumb_height) as i32;
        let mut virtual_b = 0;

        for y in area.y + 2..area.y + area.h - 2 {
            let virtual_y_a = virtual_b;
            virtual_b += 2; // Each physical cell covers 2 virtual units.

            if virtual_b <= thumb_start || virtual_y_a >= thumb_end {
                continue;
            }

            let mut thumb_char = THUMB_FULL;

            if virtual_y_a + 1 == thumb_start {
                thumb_char = THUMB_LOWER;
            }

            if virtual_b == thumb_end + 1 {
                thumb_char = THUMB_UPPER;
            }

            buf.set_cell(x, y, thumb_char);
        }
    }
    /// Renders a thick scrollbar using block characters.
    ///
    /// Uses eighth-block characters for smooth sub-cell positioning.
    pub fn render_thick(mut self, area: Rect, buf: &mut DoubleBuffer) {
        // Use block elements for a thicker, more modern look.
        const TOP_END: Cell = Cell::new_const("┴", crate::Style::DEFAULT);
        const BOT_END: Cell = Cell::new_const("┬", crate::Style::DEFAULT);
        // Characters for 0/8 to 8/8 filled cells.
        // U+2581 to U+2587 are lower 1/8 to 7/8 blocks.
        const THUMB_CELLS: [Cell; 9] = [
            Cell::new_const(" ", crate::Style::DEFAULT), // 0/8
            Cell::new_const("▁", crate::Style::DEFAULT), // 1/8
            Cell::new_const("▂", crate::Style::DEFAULT), // 2/8
            Cell::new_const("▃", crate::Style::DEFAULT), // 3/8
            Cell::new_const("▄", crate::Style::DEFAULT), // 4/8
            Cell::new_const("▅", crate::Style::DEFAULT), // 5/8
            Cell::new_const("▆", crate::Style::DEFAULT), // 6/8
            Cell::new_const("▇", crate::Style::DEFAULT), // 7/8
            Cell::new_const("█", crate::Style::DEFAULT), // 8/8
        ];

        let viewport_height = (area.h - 2) as u32;
        if viewport_height >= self.total {
            return; // No scrollbar needed
        }

        let x = area.x;

        // Draw the top and bottom caps of the scrollbar track.
        buf.set_cell(x, area.y + 1, TOP_END);
        buf.set_cell(x, area.y + area.h - 2, BOT_END);

        let track_height = (area.h - 4) as u32;
        let virtual_track_height = track_height * 8; // 8 virtual units per cell

        // Thumb height is proportional to the visible content percentage. Min height of 4 virtual units.
        let thumb_height = std::cmp::max((virtual_track_height * viewport_height) / self.total, 4);

        // Clamp scroll offset to a valid range.
        self.offset = self.offset.min(self.total.saturating_sub(viewport_height));

        // Calculate the thumb's virtual offset.
        let mut thumb_offset = if self.offset == 0 {
            0
        } else {
            std::cmp::max((virtual_track_height * self.offset) / self.total, 1)
        };

        // Snap thumb to the bottom when at the end of the content.
        let is_at_bottom = self.offset >= self.total.saturating_sub(viewport_height);
        if is_at_bottom {
            thumb_offset = virtual_track_height - thumb_height;
        }

        let thumb_start = thumb_offset as i32;
        let thumb_end = (thumb_offset + thumb_height) as i32;

        let mut virtual_b = 0;
        for y in area.y + 2..area.y + area.h - 2 {
            let virtual_y_a = virtual_b;
            virtual_b += 8;

            // First, draw the track line in every cell of the track.

            // Now, calculate thumb overlap to potentially draw over the track line.
            let overlap_start = virtual_y_a.max(thumb_start);
            let overlap_end = virtual_b.min(thumb_end);

            let coverage = (overlap_end - overlap_start).max(0) as usize;

            if coverage > 0 {
                // This cell is part of the thumb, so we overwrite the track piece.
                let is_start_cap = thumb_start >= virtual_y_a && thumb_start < virtual_b;
                let is_end_cap = thumb_end > virtual_y_a && thumb_end <= virtual_b;

                let thumb_cell = if is_start_cap && !is_end_cap {
                    // This is the TOP-MOST cell of the thumb.
                    // Create a top-aligned block using the REVERSED style trick.
                    let uncovered_at_bottom = (thumb_start - virtual_y_a) as usize;
                    THUMB_CELLS[uncovered_at_bottom]
                } else if is_end_cap && !is_start_cap {
                    // This is the BOTTOM-MOST cell of the thumb.
                    // Uses standard bottom-aligned block characters.
                    let covered_from_bottom = (thumb_end - virtual_y_a) as usize;
                    THUMB_CELLS[covered_from_bottom]
                } else {
                    // This cell is either fully covered (coverage=8), or the entire
                    // thumb fits within this single cell.
                    THUMB_CELLS[coverage].with_style_merged(Modifier::REVERSED.into())
                };

                buf.set_cell(x, y, thumb_cell);
            }
        }
    }
}
