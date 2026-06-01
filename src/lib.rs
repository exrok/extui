//! An opinionated, minimal terminal UI crate.
//!
//! # Rect based rendering and layout
//!
//! [Rect] is the core abstraction in extui, use builder style methods to
//! paint to the screen using an [DoubleBuffer] for rendering to screen
//! efficiently.
//!
//! ```no_run
//! use extui::{Rect, DoubleBuffer, Style};
//!
//! fn render_list(items: &[&str], mut area: Rect, buf: &mut DoubleBuffer) {
//!     for item in items {
//!         if area.h == 0 {
//!             break;
//!         }
//!         area.take_top(1).with(Style::DEFAULT).text(buf, item);
//!     }
//! }
//! ```
//!
//! # Deliberate Limitations
//!
//! extui intentionally omits certain features in exchange for simpler
//! interfaces and better performance:
//!
//! - **Unix only.** No Windows support.
//!
//! Foreground and background colors can be either a 256-color palette index
//! ([`AnsiColor`]) or a 24-bit RGB value ([`Rgb`]), selected per cell. Use
//! [`Color`] to hold either variant.
//!
//! # Getting Started
//!
//! ```no_run
//! use extui::{Terminal, TerminalFlags, DoubleBuffer, Style, AnsiColor};
//! use extui::event::{self, Event, KeyCode, Events};
//!
//! // Open terminal in raw mode with alternate screen
//! let mut term = Terminal::open(
//!     TerminalFlags::RAW_MODE | TerminalFlags::ALT_SCREEN | TerminalFlags::HIDE_CURSOR
//! )?;
//!
//! let (w, h) = term.size()?;
//! let mut buf = DoubleBuffer::new(w, h);
//! let mut events = Events::default();
//!
//! loop {
//!     // Render
//!     buf.rect().with(AnsiColor::Blue1.as_bg()).fill(&mut buf);
//!     buf.rect().with(Style::DEFAULT).text(&mut buf, "Press 'q' to quit");
//!     buf.render(&mut term);
//!
//!     // Poll for events
//!     if event::poll(&std::io::stdin(), None)?.is_readable() {
//!         events.read_from(&std::io::stdin())?;
//!         while let Some(ev) = events.next(term.is_raw()) {
//!             if let Event::Key(key) = ev {
//!                 if key.code == KeyCode::Char('q') {
//!                     return Ok(());
//!                 }
//!             }
//!         }
//!     }
//! }
//! # Ok::<(), std::io::Error>(())
//! ```

mod base64;
mod display_rect;
mod features;
use std::{
    io::{IsTerminal, Write},
    mem::ManuallyDrop,
    os::fd::{AsFd, FromRawFd, RawFd},
};

use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

use crate::{
    display_rect::RenderProperty,
    event::KeyboardEnhancementFlags,
    vt::{
        BufferWrite, Modifier, MoveCursor, MoveCursorRight, MoveCursorUp, ScrollBufferDown,
        ScrollBufferUp, ScrollRegion, SetCursorStyle,
    },
};

pub use crate::vt::CursorShape;

pub mod event;
mod sys;
pub mod vt;
pub mod widget;
pub use display_rect::{Ellipsis, HAlign, RenderProperties, VAlign};
pub use features::{DEFAULT_TERMINAL_FEATURE_QUERY_TIMEOUT, TerminalFeatures};

/// Writes multiple VT escape sequences to a byte buffer.
///
/// Takes a mutable reference to a `Vec<u8>` and any number of expressions
/// that implement [`BufferWrite`]. Each expression is written sequentially
/// to the buffer.
///
/// # Examples
///
/// ```
/// use extui::{splat, vt::{MoveCursor, CLEAR_STYLE}};
///
/// let mut buf = Vec::new();
/// splat!(&mut buf, MoveCursor(0, 0), CLEAR_STYLE);
/// ```
///
/// [`BufferWrite`]: crate::vt::BufferWrite
#[macro_export]
macro_rules! splat {
    ($out: expr, $($expr:expr),* $(,)?) => {{
        use $crate::vt::BufferWrite;
        let out: &mut Vec<u8> = $out;
        $(
            $expr.write_to_buffer(out);
        )*
    }};
}

/// A single terminal cell holding one styled grapheme cluster.
///
/// Cells are the fundamental unit of rendering in extui. Each cell pairs
/// a [`Style`] with the grapheme that occupies one column of the
/// terminal. Short graphemes (up to seven UTF-8 bytes) are stored
/// directly inside the cell, while longer clusters such as flag emoji
/// and ZWJ sequences are held in the owning [`Buffer`].
///
/// [`Cell`] is deliberately not `PartialEq`/`Eq`. Comparing a cell that
/// references text stored in its owning buffer against one from a
/// different buffer requires access to both buffers, which a [`Cell`]
/// alone cannot provide.
#[repr(C, align(16))]
#[derive(Clone, Copy)]
pub struct Cell {
    pub(crate) style: u64,
    pub(crate) text: [u8; 8],
}

impl std::fmt::Debug for Cell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct("Cell");
        s.field("style", &self.style());
        if self.is_handle() {
            s.field("handle_offset", &self.handle_offset())
                .field("len", &self.text[7]);
        } else {
            // SAFETY: inline path. `text_inline` returns valid UTF-8.
            s.field("text", &self.text_inline().unwrap_or(""));
        }
        s.finish()
    }
}

/// Horizontal text alignment within a region.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Alignment {
    /// Aligns text to the left edge.
    Left,
    /// Centers text horizontally.
    Center,
    /// Aligns text to the right edge.
    Right,
}

/// A relative position within a 3x3 grid.
///
/// Used to specify which parts of a box border or region to render.
#[repr(u8)]
#[derive(Copy, Clone)]
pub enum Rel {
    /// Top-left corner.
    TopLeft,
    /// Top center edge.
    TopCenter,
    /// Top-right corner.
    TopRight,
    /// Center-left edge.
    CenterLeft,
    /// Center of the region.
    CenterCenter,
    /// Center-right edge.
    CenterRight,
    /// Bottom-left corner.
    BottomLeft,
    /// Bottom center edge.
    BottomCenter,
    /// Bottom-right corner.
    BottomRight,
}

/// A set of relative positions for partial box rendering.
///
/// Supports bitwise OR to combine positions and check membership.
///
/// # Examples
///
/// ```
/// use extui::{RelSet, Rel};
///
/// let edges = RelSet::TOP | RelSet::BOTTOM;
/// assert!(edges.contains(Rel::TopLeft));
/// assert!(!edges.contains(Rel::CenterLeft));
/// ```
#[derive(Copy, Clone)]
pub struct RelSet(u16);

impl std::ops::BitOr for RelSet {
    type Output = RelSet;

    fn bitor(self, rhs: Self) -> Self::Output {
        RelSet(self.0 | rhs.0)
    }
}
impl std::ops::BitAnd for RelSet {
    type Output = RelSet;

    fn bitand(self, rhs: Self) -> Self::Output {
        RelSet(self.0 & rhs.0)
    }
}

impl RelSet {
    /// All positions along the top edge.
    pub const TOP: RelSet = RelSet::new(&[Rel::TopLeft, Rel::TopCenter, Rel::TopRight]);
    /// All positions along the bottom edge.
    pub const BOTTOM: RelSet = RelSet::new(&[Rel::BottomLeft, Rel::BottomCenter, Rel::BottomRight]);
    /// All positions along the left edge.
    pub const LEFT: RelSet = RelSet::new(&[Rel::TopLeft, Rel::CenterLeft, Rel::BottomLeft]);
    /// All positions along the right edge.
    pub const RIGHT: RelSet = RelSet::new(&[Rel::TopRight, Rel::CenterRight, Rel::BottomRight]);
    /// All border positions (excludes center).
    pub const BOX: RelSet = RelSet(0b111_101_111);

    /// Creates a new set from a slice of positions.
    pub const fn new(mut rels: &[Rel]) -> RelSet {
        let mut bits = 0u16;
        while let [rel, rest @ ..] = rels {
            bits |= 1 << (*rel as u8);
            rels = rest;
        }
        RelSet(bits)
    }

    /// Returns `true` if the set contains the given position.
    pub const fn contains(self, rel: Rel) -> bool {
        (self.0 & (1 << (rel as u8))) != 0
    }

    /// Returns `true` if the set is empty.
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
}

/// Characters used to draw box borders.
///
/// Provides predefined styles like [`ASCII`](Self::ASCII) and [`LIGHT`](Self::LIGHT).
pub struct BoxStyle {
    /// Top-left corner character.
    pub top_left: Cell,
    /// Top-right corner character.
    pub top_right: Cell,
    /// Bottom-right corner character.
    pub bottom_right: Cell,
    /// Bottom-left corner character.
    pub bottom_left: Cell,
    /// Horizontal line character.
    pub horizontal: Cell,
    /// Vertical line character.
    pub vertical: Cell,
}
impl BoxStyle {
    /// ASCII box drawing characters (`+`, `-`, `|`).
    pub const ASCII: BoxStyle = unsafe {
        BoxStyle {
            top_right: Cell::new_ascii(b'+', Style::DEFAULT),
            top_left: Cell::new_ascii(b'+', Style::DEFAULT),
            bottom_right: Cell::new_ascii(b'+', Style::DEFAULT),
            bottom_left: Cell::new_ascii(b'+', Style::DEFAULT),
            horizontal: Cell::new_ascii(b'-', Style::DEFAULT),
            vertical: Cell::new_ascii(b'|', Style::DEFAULT),
        }
    };
    /// Unicode light box drawing characters (`┌`, `─`, `│`, etc.).
    pub const LIGHT: BoxStyle = BoxStyle {
        top_right: Cell::new_const("┐", Style::DEFAULT),
        top_left: Cell::new_const("┌", Style::DEFAULT),
        bottom_right: Cell::new_const("┘", Style::DEFAULT),
        bottom_left: Cell::new_const("└", Style::DEFAULT),
        horizontal: Cell::new_const("─", Style::DEFAULT),
        vertical: Cell::new_const("│", Style::DEFAULT),
    };
}

impl BoxStyle {
    /// Renders a complete box border within the given rectangle.
    ///
    /// Returns the inner rectangle after subtracting the border.
    pub fn render(&self, rect: Rect, buf: &mut DoubleBuffer) -> Rect {
        self.render_partial(rect, buf, RelSet::BOX)
    }

    /// Renders only the specified parts of a box border.
    ///
    /// Returns the inner rectangle after subtracting the rendered edges.
    pub fn render_partial(&self, mut rect: Rect, buf: &mut DoubleBuffer, set: RelSet) -> Rect {
        let Rect { x, y, w, h } = rect;
        if w == 0 || h == 0 {
            return rect;
        }

        if let Some(row) = buf.current.row_remaining_mut(x, y) {
            if w >= 1 && set.contains(Rel::TopLeft) {
                row[0] = self.top_left;
            }
            if set.contains(Rel::TopCenter) {
                for cell in row
                    .iter_mut()
                    .take(w as usize)
                    .skip(1)
                    .take(w.saturating_sub(2) as usize)
                {
                    *cell = self.horizontal;
                }
            }
            if w >= 2 && set.contains(Rel::TopRight) {
                row[w as usize - 1] = self.top_right;
            }
        }
        if let Some(row) = buf.current.row_remaining_mut(x, y + h - 1) {
            if w >= 1 && set.contains(Rel::BottomLeft) {
                row[0] = self.bottom_left;
            }
            if set.contains(Rel::BottomCenter) {
                for cell in row
                    .iter_mut()
                    .take(w as usize)
                    .skip(1)
                    .take(w.saturating_sub(2) as usize)
                {
                    *cell = self.horizontal;
                }
            }
            if w >= 2 && set.contains(Rel::BottomRight) {
                row[w as usize - 1] = self.bottom_right;
            }
        }

        for y in y + ((!(set & RelSet::TOP).is_empty()) as u16)
            ..y + h - ((!(set & RelSet::BOTTOM).is_empty()) as u16)
        {
            if set.contains(Rel::CenterLeft)
                && let Some(row) = buf.current.row_remaining_mut(x, y)
            {
                row[0] = self.vertical;
            }
            if set.contains(Rel::CenterRight)
                && let Some(row) = buf.current.row_remaining_mut(x + w - 1, y)
            {
                row[0] = self.vertical;
            }
        }
        // Subjects sides that have any to provide the inner vec
        if !(set & RelSet::TOP).is_empty() {
            rect.y += 1;
            rect.h = rect.h.saturating_sub(1);
        }
        if !(set & RelSet::BOTTOM).is_empty() {
            rect.h = rect.h.saturating_sub(1);
        }
        if !(set & RelSet::LEFT).is_empty() {
            rect.x += 1;
            rect.w = rect.w.saturating_sub(1);
        }
        if !(set & RelSet::RIGHT).is_empty() {
            rect.w = rect.w.saturating_sub(1);
        }
        rect
    }
}

/// Tracks a style transition for efficient escape sequence generation.
///
/// Computes the minimal set of escape codes needed to change from the
/// current style to the target style.
pub struct StyleDelta {
    pub(crate) current: u64,
    pub(crate) target: Style,
}

impl StyleDelta {
    /// Sets the previous style for delta computation.
    pub fn with_previous(self, style: Style) -> StyleDelta {
        StyleDelta {
            current: style.0,
            target: self.target,
        }
    }
}

/// Text styling attributes including foreground color, background color, and modifiers.
///
/// Combines colors and text modifiers (bold, italic, etc.) into a single value.
/// Use [`with_fg`](Self::with_fg), [`with_bg`](Self::with_bg), and
/// [`with_modifier`](Self::with_modifier) to build styles.
///
/// # Examples
///
/// ```
/// use extui::{Style, AnsiColor, Color, vt::Modifier};
///
/// let palette = Style::DEFAULT
///     .with_fg(AnsiColor::Red1)
///     .with_bg(AnsiColor::Black)
///     .with_modifier(Modifier::BOLD);
///
/// // 24-bit RGB is also supported per cell:
/// let rgb = Style::DEFAULT.with_fg(Color::rgb(255, 128, 0));
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub struct Style(pub(crate) u64);

impl std::fmt::Debug for Style {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_palette() {
            f.debug_struct("Style")
                .field("palette_index", &self.palette_index())
                .finish()
        } else {
            f.debug_struct("Style")
                .field("fg", &self.fg())
                .field("bg", &self.bg())
                .field("modifiers", &self.modifiers())
                .finish()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rgb(pub u8, pub u8, pub u8);

impl Rgb {
    /// Maps this 24-bit RGB triple to the nearest 256-color palette index.
    ///
    /// Uses xterm's exact cube/grayscale tables (not a naive `v*6/256`) so
    /// greys like `#767676` round to the grayscale ramp entry the user
    /// actually sees in a 256-color xterm, and colors snap to their
    /// intended cube slot. A round-trip through quantization stays stable
    /// against the tables in `hl_cterm2rgb_color`.
    pub const fn to_ansi(self) -> AnsiColor {
        const CUBE_LEVELS: [i32; 6] = [0x00, 0x5f, 0x87, 0xaf, 0xd7, 0xff];
        const GREY_LEVELS: [i32; 24] = [
            0x08, 0x12, 0x1c, 0x26, 0x30, 0x3a, 0x44, 0x4e, 0x58, 0x62, 0x6c, 0x76, 0x80, 0x8a,
            0x94, 0x9e, 0xa8, 0xb2, 0xbc, 0xc6, 0xd0, 0xda, 0xe4, 0xee,
        ];

        const fn closest(value: i32, table: &[i32]) -> usize {
            let mut best = 0;
            let mut best_d = i32::MAX;
            let mut i = 0;
            while i < table.len() {
                let d = (value - table[i]).abs();
                if d < best_d {
                    best_d = d;
                    best = i;
                }
                i += 1;
            }
            best
        }

        let Rgb(r, g, b) = self;
        let r = r as i32;
        let g = g as i32;
        let b = b as i32;

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
            AnsiColor((232 + gi) as u8)
        } else {
            AnsiColor(cube_idx as u8)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Color {
    Ansi(AnsiColor),
    Rgb(Rgb),
}

impl Color {
    pub fn rgb(r: u8, g: u8, b: u8) -> Self {
        Color::Rgb(Rgb(r, g, b))
    }
    pub fn ansi(color: u8) -> Self {
        Color::Ansi(AnsiColor(color))
    }

    /// Returns this color as an [`AnsiColor`], quantizing RGB if needed.
    pub const fn to_ansi(self) -> AnsiColor {
        match self {
            Color::Ansi(c) => c,
            Color::Rgb(rgb) => rgb.to_ansi(),
        }
    }
}

impl From<AnsiColor> for Color {
    fn from(value: AnsiColor) -> Self {
        Color::Ansi(value)
    }
}

impl From<Rgb> for Color {
    fn from(value: Rgb) -> Self {
        Color::Rgb(value)
    }
}
/// A 256-color palette index.
///
/// Provides named constants for common colors and a grayscale ramp.
/// The inner `u8` represents the ANSI 256-color palette index.
///
/// # Examples
///
/// ```
/// use extui::AnsiColor;
///
/// let red = AnsiColor::Red1;
/// let gray = AnsiColor::Grey[15];
/// let custom = AnsiColor(42);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AnsiColor(pub u8);

#[allow(non_upper_case_globals)]
impl AnsiColor {
    pub const NavyBlue: AnsiColor = AnsiColor(17);
    pub const DarkBlue: AnsiColor = AnsiColor(18);
    pub const Blue3: AnsiColor = AnsiColor(19);
    pub const Blue1: AnsiColor = AnsiColor(21);
    pub const DarkGreen: AnsiColor = AnsiColor(22);
    pub const DeepSkyBlue4: AnsiColor = AnsiColor(23);
    pub const DodgerBlue3: AnsiColor = AnsiColor(26);
    pub const DodgerBlue2: AnsiColor = AnsiColor(27);
    pub const Green4: AnsiColor = AnsiColor(28);
    pub const SpringGreen4: AnsiColor = AnsiColor(29);
    pub const Turquoise4: AnsiColor = AnsiColor(30);
    pub const DeepSkyBlue3: AnsiColor = AnsiColor(31);
    pub const DodgerBlue1: AnsiColor = AnsiColor(33);
    pub const Green3: AnsiColor = AnsiColor(34);
    pub const DarkCyan: AnsiColor = AnsiColor(36);
    pub const DeepSkyBlue2: AnsiColor = AnsiColor(38);
    pub const DeepSkyBlue1: AnsiColor = AnsiColor(39);
    pub const SpringGreen3: AnsiColor = AnsiColor(41);
    pub const SpringGreen: AnsiColor = AnsiColor(42);
    pub const Cyan3: AnsiColor = AnsiColor(43);
    pub const DarkTurquoise: AnsiColor = AnsiColor(44);
    pub const Turquoise2: AnsiColor = AnsiColor(45);
    pub const Green1: AnsiColor = AnsiColor(46);
    pub const SpringGreen2: AnsiColor = AnsiColor(47);
    pub const SpringGreen1: AnsiColor = AnsiColor(48);
    pub const MediumSpringGreen: AnsiColor = AnsiColor(49);
    pub const Cyan2: AnsiColor = AnsiColor(50);
    pub const Cyan1: AnsiColor = AnsiColor(51);
    pub const DarkRed: AnsiColor = AnsiColor(52);
    pub const DeepPink4: AnsiColor = AnsiColor(53);
    pub const Purple3: AnsiColor = AnsiColor(56);
    pub const BlueViolet: AnsiColor = AnsiColor(57);
    pub const Orange4: AnsiColor = AnsiColor(58);
    pub const MediumPurple4: AnsiColor = AnsiColor(60);
    pub const SlateBlue3: AnsiColor = AnsiColor(61);
    pub const RoyalBlue1: AnsiColor = AnsiColor(63);
    pub const Chartreuse4: AnsiColor = AnsiColor(64);
    pub const PaleTurquoise4: AnsiColor = AnsiColor(66);
    pub const SteelBlue: AnsiColor = AnsiColor(67);
    pub const SteelBlue3: AnsiColor = AnsiColor(68);
    pub const CornflowerBlue: AnsiColor = AnsiColor(69);
    pub const Chartreuse3: AnsiColor = AnsiColor(70);
    pub const DarkSeaGreen4: AnsiColor = AnsiColor(71);
    pub const CadetBlue: AnsiColor = AnsiColor(72);
    pub const SkyBlue3: AnsiColor = AnsiColor(74);
    pub const SteelBlue1: AnsiColor = AnsiColor(75);
    pub const PaleGreen3: AnsiColor = AnsiColor(77);
    pub const SeaGreen3: AnsiColor = AnsiColor(78);
    pub const Aquamarine3: AnsiColor = AnsiColor(79);
    pub const MediumTurquoise: AnsiColor = AnsiColor(80);
    pub const Chartreuse2: AnsiColor = AnsiColor(82);
    pub const Aquamarine1: AnsiColor = AnsiColor(86);
    pub const DarkSlateGray2: AnsiColor = AnsiColor(87);
    pub const DarkViolet: AnsiColor = AnsiColor(92);
    pub const Purple: AnsiColor = AnsiColor(93);
    pub const LightPink4: AnsiColor = AnsiColor(95);
    pub const Plum4: AnsiColor = AnsiColor(96);
    pub const SlateBlue1: AnsiColor = AnsiColor(99);
    pub const Yellow4: AnsiColor = AnsiColor(100);
    pub const Wheat4: AnsiColor = AnsiColor(101);
    pub const LightSlateGrey: AnsiColor = AnsiColor(103);
    pub const MediumPurple: AnsiColor = AnsiColor(104);
    pub const LightSlateBlue: AnsiColor = AnsiColor(105);
    pub const DarkOliveGreen3: AnsiColor = AnsiColor(107);
    pub const DarkSeaGreen: AnsiColor = AnsiColor(108);
    pub const Grey: [AnsiColor; 31] = [
        AnsiColor(16),
        AnsiColor(232),
        AnsiColor(233),
        AnsiColor(234),
        AnsiColor(235),
        AnsiColor(236),
        AnsiColor(237),
        AnsiColor(238),
        AnsiColor(239),
        AnsiColor(240),
        AnsiColor(59),
        AnsiColor(241),
        AnsiColor(242),
        AnsiColor(243),
        AnsiColor(244),
        AnsiColor(102),
        AnsiColor(245),
        AnsiColor(246),
        AnsiColor(247),
        AnsiColor(139),
        AnsiColor(248),
        AnsiColor(145),
        AnsiColor(249),
        AnsiColor(250),
        AnsiColor(251),
        AnsiColor(252),
        AnsiColor(188),
        AnsiColor(253),
        AnsiColor(254),
        AnsiColor(255),
        AnsiColor(231),
    ];
    pub const SkyBlue2: AnsiColor = AnsiColor(111);
    pub const DarkOliveGreen: AnsiColor = AnsiColor(113);
    pub const DarkSeaGreen3: AnsiColor = AnsiColor(115);
    pub const DarkSlateGray3: AnsiColor = AnsiColor(116);
    pub const SkyBlue1: AnsiColor = AnsiColor(117);
    pub const Chartreuse1: AnsiColor = AnsiColor(118);
    pub const LightGreen: AnsiColor = AnsiColor(119);
    pub const PaleGreen1: AnsiColor = AnsiColor(121);
    pub const DarkSlateGray1: AnsiColor = AnsiColor(123);
    pub const Red3: AnsiColor = AnsiColor(124);
    pub const MediumVioletRed: AnsiColor = AnsiColor(126);
    pub const Magenta3: AnsiColor = AnsiColor(127);
    pub const DarkOrange3: AnsiColor = AnsiColor(130);
    pub const IndianRed: AnsiColor = AnsiColor(131);
    pub const HotPink3: AnsiColor = AnsiColor(132);
    pub const MediumOrchid3: AnsiColor = AnsiColor(133);
    pub const MediumOrchid: AnsiColor = AnsiColor(134);
    pub const DarkGoldenrod: AnsiColor = AnsiColor(136);
    pub const LightSalmon3: AnsiColor = AnsiColor(137);
    pub const RosyBrown: AnsiColor = AnsiColor(138);
    pub const Violet: AnsiColor = AnsiColor(140);
    pub const MediumPurple1: AnsiColor = AnsiColor(141);
    pub const Gold3: AnsiColor = AnsiColor(142);
    pub const DarkKhaki: AnsiColor = AnsiColor(143);
    pub const NavajoWhite3: AnsiColor = AnsiColor(144);
    pub const LightSteelBlue3: AnsiColor = AnsiColor(146);
    pub const LightSteelBlue: AnsiColor = AnsiColor(147);
    pub const Yellow3: AnsiColor = AnsiColor(148);
    pub const LightCyan3: AnsiColor = AnsiColor(152);
    pub const LightSkyBlue1: AnsiColor = AnsiColor(153);
    pub const GreenYellow: AnsiColor = AnsiColor(154);
    pub const DarkOliveGreen2: AnsiColor = AnsiColor(155);
    pub const DarkSeaGreen1: AnsiColor = AnsiColor(158);
    pub const PaleTurquoise1: AnsiColor = AnsiColor(159);
    pub const Magenta2: AnsiColor = AnsiColor(165);
    pub const HotPink2: AnsiColor = AnsiColor(169);
    pub const Orchid: AnsiColor = AnsiColor(170);
    pub const MediumOrchid1: AnsiColor = AnsiColor(171);
    pub const Orange3: AnsiColor = AnsiColor(172);
    pub const LightPink3: AnsiColor = AnsiColor(174);
    pub const Pink3: AnsiColor = AnsiColor(175);
    pub const Plum3: AnsiColor = AnsiColor(176);
    pub const LightGoldenrod3: AnsiColor = AnsiColor(179);
    pub const Tan: AnsiColor = AnsiColor(180);
    pub const MistyRose3: AnsiColor = AnsiColor(181);
    pub const Thistle3: AnsiColor = AnsiColor(182);
    pub const Plum2: AnsiColor = AnsiColor(183);
    pub const Khaki3: AnsiColor = AnsiColor(185);
    pub const LightYellow3: AnsiColor = AnsiColor(187);
    pub const LightSteelBlue1: AnsiColor = AnsiColor(189);
    pub const Yellow2: AnsiColor = AnsiColor(190);
    pub const DarkOliveGreen1: AnsiColor = AnsiColor(191);
    pub const LightSeaGreen: AnsiColor = AnsiColor(193);
    pub const Honeydew: AnsiColor = AnsiColor(194);
    pub const LightCyan1: AnsiColor = AnsiColor(195);
    pub const Red1: AnsiColor = AnsiColor(196);
    pub const DeepPink2: AnsiColor = AnsiColor(197);
    pub const DeepPink1: AnsiColor = AnsiColor(198);
    pub const Magenta1: AnsiColor = AnsiColor(201);
    pub const OrangeRed1: AnsiColor = AnsiColor(202);
    pub const NeonRed: AnsiColor = AnsiColor(203);
    pub const HotPink: AnsiColor = AnsiColor(205);
    pub const DarkOrange: AnsiColor = AnsiColor(208);
    pub const Salmon: AnsiColor = AnsiColor(209);
    pub const LightCoral: AnsiColor = AnsiColor(210);
    pub const PaleVioletRed1: AnsiColor = AnsiColor(211);
    pub const Orchid2: AnsiColor = AnsiColor(212);
    pub const Orchid1: AnsiColor = AnsiColor(213);
    pub const Orange1: AnsiColor = AnsiColor(214);
    pub const SandyBrown: AnsiColor = AnsiColor(215);
    pub const LightSalmon1: AnsiColor = AnsiColor(216);
    pub const LightPink1: AnsiColor = AnsiColor(217);
    pub const Pink1: AnsiColor = AnsiColor(218);
    pub const Plum1: AnsiColor = AnsiColor(219);
    pub const Gold1: AnsiColor = AnsiColor(220);
    pub const LightGoldenrod2: AnsiColor = AnsiColor(221);
    pub const NavajoWhite: AnsiColor = AnsiColor(223);
    pub const MistyRose: AnsiColor = AnsiColor(224);
    pub const Thistle: AnsiColor = AnsiColor(225);
    pub const Yellow1: AnsiColor = AnsiColor(226);
    pub const LightGoldenrod1: AnsiColor = AnsiColor(227);
    pub const Khaki1: AnsiColor = AnsiColor(228);
    pub const Wheat1: AnsiColor = AnsiColor(229);
    pub const Cornsilk1: AnsiColor = AnsiColor(230);

    pub const White: AnsiColor = AnsiColor(231);
    pub const Black: AnsiColor = AnsiColor(16);
}
impl AnsiColor {
    /// Creates a style with this color as the foreground.
    pub fn as_fg(self) -> Style {
        Style::DEFAULT.with_fg(self)
    }

    /// Creates a style with this color as the background.
    pub fn as_bg(self) -> Style {
        Style::DEFAULT.with_bg(self)
    }

    /// Creates a style with this color as background and the given foreground.
    pub fn with_fg(self, fg: AnsiColor) -> Style {
        Style::DEFAULT.with_fg(fg).with_bg(self)
    }

    /// Creates a style with this color as foreground and the given background.
    pub fn with_bg(self, bg: AnsiColor) -> Style {
        Style::DEFAULT.with_bg(bg).with_fg(self)
    }
}

impl std::ops::BitOrAssign<Modifier> for Style {
    fn bitor_assign(&mut self, rhs: Modifier) {
        self.0 |= rhs.0 as u64;
    }
}

impl std::ops::BitOr<Modifier> for Style {
    type Output = Style;

    fn bitor(self, rhs: Modifier) -> Self::Output {
        Style(self.0 | rhs.0 as u64)
    }
}

impl std::ops::BitOrAssign for Style {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl std::ops::BitOr for Style {
    type Output = Style;

    fn bitor(self, rhs: Self) -> Self::Output {
        Style(self.0 | rhs.0)
    }
}

impl Style {
    /// The default style with no colors or modifiers.
    pub const DEFAULT: Style = Style(0);
    pub(crate) const HAS_FG: u64 = 1 << 9;
    pub(crate) const FG_IS_RGB: u64 = 1 << 10;
    pub(crate) const HAS_BG: u64 = 1 << 11;
    pub(crate) const BG_IS_RGB: u64 = 1 << 12;
    pub(crate) const IS_PALETTE: u64 = 1 << 13;
    pub(crate) const FG_MASK: u64 = 0x00ff_ffff << 16;
    pub(crate) const BG_MASK: u64 = 0x00ff_ffff << 40;
    pub(crate) const FG_ALL_MASK: u64 = Self::HAS_FG | Self::FG_IS_RGB | Self::FG_MASK;
    pub(crate) const BG_ALL_MASK: u64 = Self::HAS_BG | Self::BG_IS_RGB | Self::BG_MASK;

    /// Creates a palette style referencing an entry in the [`DoubleBuffer`]'s palette table.
    ///
    /// Palette entries contain raw VT escape bytes that are emitted verbatim during rendering,
    /// replacing the modifier attributes. Foreground and background colors can still be set
    /// independently via [`with_fg`](Self::with_fg) and [`with_bg`](Self::with_bg).
    ///
    /// Register entries with [`DoubleBuffer::set_palette`] before rendering.
    /// Do not combine palette styles with modifier methods.
    pub const fn palette(index: u8) -> Style {
        // In palette-style mode modifier bits are unused; overlay the index at bits 0..8.
        Style((index as u64) | Self::IS_PALETTE)
    }

    /// Returns `true` if this is a palette style.
    pub const fn is_palette(&self) -> bool {
        self.0 & Self::IS_PALETTE != 0
    }

    /// Returns the palette index for a palette style.
    pub const fn palette_index(&self) -> u8 {
        (self.0 & 0xff) as u8
    }

    /// Creates a [`StyleDelta`] for transitioning to this style.
    pub const fn delta(self) -> StyleDelta {
        StyleDelta {
            current: u64::MAX,
            target: self,
        }
    }

    /// Returns a new style with the given foreground color.
    ///
    /// Accepts any type convertible to [`Color`], typically
    /// [`AnsiColor`] or [`Rgb`].
    pub fn with_fg(self, color: impl Into<Color>) -> Style {
        match color.into() {
            Color::Ansi(c) => self.with_fg_ansi(c),
            Color::Rgb(Rgb(r, g, b)) => self.with_fg_rgb(r, g, b),
        }
    }

    /// Returns a new style with the given foreground color in the 256-color palette.
    ///
    /// `const`-friendly variant of [`with_fg`](Self::with_fg).
    pub const fn with_fg_ansi(self, color: AnsiColor) -> Style {
        let cleared = self.0 & !Self::FG_ALL_MASK;
        Style(cleared | Self::HAS_FG | ((color.0 as u64) << 16))
    }

    /// Returns a new style with the given 24-bit RGB foreground color.
    ///
    /// `const`-friendly variant of [`with_fg`](Self::with_fg).
    pub const fn with_fg_rgb(self, r: u8, g: u8, b: u8) -> Style {
        let cleared = self.0 & !Self::FG_ALL_MASK;
        let packed = (r as u64) | ((g as u64) << 8) | ((b as u64) << 16);
        Style(cleared | Self::HAS_FG | Self::FG_IS_RGB | (packed << 16))
    }

    /// Returns a new style without a foreground color.
    pub const fn without_fg(self) -> Style {
        Style(self.0 & !Self::FG_ALL_MASK)
    }

    /// Returns a new style without a background color.
    pub const fn without_bg(self) -> Style {
        Style(self.0 & !Self::BG_ALL_MASK)
    }

    /// Returns a copy of this style with any 24-bit RGB fg/bg colors
    /// replaced by their nearest 256-color palette equivalent.
    ///
    /// Ansi colors and palette styles are returned unchanged.
    pub const fn quantize_rgb(self) -> Style {
        let mut out = self.0;
        if self.0 & Self::FG_IS_RGB != 0 {
            let raw = ((self.0 >> 16) & 0x00ff_ffff) as u32;
            let rgb = Rgb(raw as u8, (raw >> 8) as u8, (raw >> 16) as u8);
            let idx = rgb.to_ansi().0 as u64;
            out = (out & !Self::FG_ALL_MASK) | Self::HAS_FG | (idx << 16);
        }
        if self.0 & Self::BG_IS_RGB != 0 {
            let raw = ((self.0 >> 40) & 0x00ff_ffff) as u32;
            let rgb = Rgb(raw as u8, (raw >> 8) as u8, (raw >> 16) as u8);
            let idx = rgb.to_ansi().0 as u64;
            out = (out & !Self::BG_ALL_MASK) | Self::HAS_BG | (idx << 40);
        }
        Style(out)
    }

    /// Returns the text modifiers applied to this style.
    pub const fn modifiers(self) -> Modifier {
        Modifier(self.0 as u16 & Modifier::ALL.0)
    }

    /// Returns a new style with the given modifiers added.
    pub const fn with_modifier(self, mods: Modifier) -> Style {
        Style(self.0 | mods.0 as u64)
    }

    /// Returns a new style with the given modifiers removed.
    pub const fn without_modifier(self, mods: Modifier) -> Style {
        Style(self.0 & !(mods.0 as u64))
    }

    /// Returns the foreground color if set.
    pub const fn fg(self) -> Option<Color> {
        if self.0 & Self::HAS_FG == 0 {
            return None;
        }
        let raw = ((self.0 >> 16) & 0x00ff_ffff) as u32;
        Some(if self.0 & Self::FG_IS_RGB != 0 {
            Color::Rgb(Rgb(raw as u8, (raw >> 8) as u8, (raw >> 16) as u8))
        } else {
            Color::Ansi(AnsiColor(raw as u8))
        })
    }

    /// Returns a new style with the given background color.
    ///
    /// Accepts any type convertible to [`Color`], typically
    /// [`AnsiColor`] or [`Rgb`].
    pub fn with_bg(self, color: impl Into<Color>) -> Style {
        match color.into() {
            Color::Ansi(c) => self.with_bg_ansi(c),
            Color::Rgb(Rgb(r, g, b)) => self.with_bg_rgb(r, g, b),
        }
    }

    /// Returns a new style with the given background color in the 256-color palette.
    ///
    /// `const`-friendly variant of [`with_bg`](Self::with_bg).
    pub const fn with_bg_ansi(self, color: AnsiColor) -> Style {
        let cleared = self.0 & !Self::BG_ALL_MASK;
        Style(cleared | Self::HAS_BG | ((color.0 as u64) << 40))
    }

    /// Returns a new style with the given 24-bit RGB background color.
    ///
    /// `const`-friendly variant of [`with_bg`](Self::with_bg).
    pub const fn with_bg_rgb(self, r: u8, g: u8, b: u8) -> Style {
        let cleared = self.0 & !Self::BG_ALL_MASK;
        let packed = (r as u64) | ((g as u64) << 8) | ((b as u64) << 16);
        Style(cleared | Self::HAS_BG | Self::BG_IS_RGB | (packed << 40))
    }

    /// Returns the background color if set.
    pub const fn bg(self) -> Option<Color> {
        if self.0 & Self::HAS_BG == 0 {
            return None;
        }
        let raw = ((self.0 >> 40) & 0x00ff_ffff) as u32;
        Some(if self.0 & Self::BG_IS_RGB != 0 {
            Color::Rgb(Rgb(raw as u8, (raw >> 8) as u8, (raw >> 16) as u8))
        } else {
            Color::Ansi(AnsiColor(raw as u8))
        })
    }
}

// impl std::ops::BitOr for Style {
//     type Output = Style;

//     fn bitor(self, rhs: Self) -> Self::Output {
//         Style(self.0 | rhs.0)
//     }
// }

impl Cell {
    const EMPTY: Cell = Cell {
        style: 0,
        text: [0; 8],
    };

    /// Returns `true` if this cell holds no grapheme.
    ///
    /// An empty cell renders as a transparent gap. Freshly created
    /// buffers are filled with empty cells.
    pub fn is_empty(self) -> bool {
        self.text == [0; 8]
    }

    /// Returns `true` if this cell refers to a grapheme held by its
    /// owning [`Buffer`] rather than inline in the cell itself.
    ///
    /// Handle cells are produced by [`Buffer::set_stringn`] for
    /// clusters longer than seven UTF-8 bytes. Resolve their text with
    /// [`Buffer::handle_text`], and pass a handle cell only to the
    /// buffer that produced it.
    #[inline]
    pub fn is_handle(self) -> bool {
        self.text[7] > 7
    }

    /// Returns the raw grapheme offset recorded on a handle cell.
    ///
    /// Most callers should use [`Buffer::handle_text`], which resolves
    /// a handle cell against its owning buffer and hands back the
    /// grapheme bytes directly. This accessor exists for the handful
    /// of places that need to interpret the raw layout, and it is
    /// trivially misusable.
    ///
    /// # Invariants
    ///
    /// The returned value is only meaningful when [`Cell::is_handle`]
    /// is `true`, and only against the [`Buffer`] that produced the
    /// cell. Feeding an offset from one buffer into another, or
    /// reading one from an inline cell, yields garbage.
    #[doc(hidden)]
    #[inline]
    pub fn handle_offset(self) -> u32 {
        u32::from_le_bytes([self.text[0], self.text[1], self.text[2], self.text[3]])
    }

    /// Returns the raw grapheme length recorded on a handle cell.
    ///
    /// Paired with [`Cell::handle_offset`] for low-level handle
    /// inspection. Prefer [`Buffer::handle_text`] in normal code.
    ///
    /// # Invariants
    ///
    /// The value is only meaningful for handle cells. Inline cells
    /// store their UTF-8 byte count here as well, but that shape is an
    /// implementation detail and callers should not rely on it.
    #[doc(hidden)]
    #[inline]
    pub fn handle_len(self) -> u8 {
        self.text[7]
    }

    /// Returns the grapheme stored inline in this cell, if any.
    ///
    /// Returns `None` for handle cells and for empty cells. Use
    /// [`Buffer::handle_text`] when the cell may be a handle.
    #[inline]
    pub fn text_inline(&self) -> Option<&str> {
        if self.is_handle() {
            return None;
        }
        let len = self.text[7] as usize;
        // SAFETY: inline path always holds valid UTF-8 in `text[..len]`.
        Some(unsafe { std::str::from_utf8_unchecked(self.text.get_unchecked(..len)) })
    }

    /// Returns the raw grapheme bytes of this cell, resolving handles
    /// through the given [`SideBuffer`]. Returns an empty slice for empty
    /// cells or for handles that fail bounds checking.
    #[inline]
    fn text<'a>(&'a self, side: &'a SideBuffer) -> &'a [u8] {
        if self.is_handle() {
            side.get(self.handle_offset(), self.text[7]).unwrap_or(&[])
        } else {
            let len = self.text[7] as usize;
            // `len` is at most 7, always in bounds of `text: [u8; 8]`.
            &self.text[..len]
        }
    }
    fn new(text: &str, style: Style) -> Cell {
        let bytes = text.as_bytes();
        let len = bytes.len();
        if len > 7 {
            panic!("extui::Cell::new grapheme exceeds 7-byte cap — use set_stringn");
        }
        let mut buf = [0u8; 8];
        buf[..len].copy_from_slice(bytes);
        buf[7] = len as u8;
        Cell {
            style: style.0,
            text: buf,
        }
    }
    const fn new_const(text: &str, style: Style) -> Cell {
        let bytes = text.as_bytes();
        if bytes.len() > 7 {
            panic!("extui::Cell grapheme exceeds 7-byte cap");
        }
        let mut buf = [0u8; 8];
        let mut i = 0;
        while i < bytes.len() {
            buf[i] = bytes[i];
            i += 1;
        }
        buf[7] = bytes.len() as u8;
        Cell {
            style: style.0,
            text: buf,
        }
    }

    /// Builds a cell that references a grapheme stored at `offset` in the
    /// owning [`Buffer`]'s side buffer. `len` is the byte length of the
    /// grapheme (must be > 7 and ≤ 255).
    #[inline]
    fn new_handle(offset: u32, len: u8, style: Style) -> Cell {
        debug_assert!(len > 7, "handle cells must carry a len > 7");
        let o = offset.to_le_bytes();
        Cell {
            style: style.0,
            text: [o[0], o[1], o[2], o[3], 0, 0, 0, len],
        }
    }

    /// Returns a copy of this cell with `style` merged on top of the
    /// existing style.
    ///
    /// Modifier bits and color fields from `style` take precedence,
    /// while fields left unset on `style` keep their existing values.
    ///
    /// # Invariants
    ///
    /// The returned cell shares any grapheme storage with its source
    /// cell. If the source is a handle cell, the result is only valid
    /// inside the [`Buffer`] that produced it.
    pub fn with_style_merged(&self, style: Style) -> Cell {
        Cell {
            style: self.style | style.0,
            text: self.text,
        }
    }
    /// Returns the style component of this cell.
    #[inline]
    pub fn style(&self) -> Style {
        Style(self.style)
    }
    const unsafe fn new_ascii(ch: u8, style: Style) -> Cell {
        Cell {
            style: style.0,
            text: [ch, 0, 0, 0, 0, 0, 0, 1],
        }
    }

    /// Returns true if this cell is one of the blank constants
    /// ([`Cell::EMPTY`] or [`Cell::BLANK`]). Handle cells are never blank.
    #[inline]
    fn is_blank_or_empty(&self) -> bool {
        // Both constants have style == 0 and are non-handles.
        self.style == 0 && (self.text == [0; 8] || self.text == [b' ', 0, 0, 0, 0, 0, 0, 1])
    }
}

/// Flat append-only byte arena holding UTF-8 graphemes that are too long
/// to fit inline in a [`Cell`] (strictly more than 7 bytes).
///
/// Each [`Buffer`] owns one of these. Handle cells carry a u32 byte offset
/// into [`SideBuffer::data`] plus the grapheme's length in the cell itself,
/// so the arena stores no framing — just concatenated UTF-8.
#[derive(Default)]
pub(crate) struct SideBuffer {
    data: Vec<u8>,
}

impl SideBuffer {
    /// Appends `s` to the arena and returns the byte offset of the start
    /// of the entry. The caller is responsible for storing `s.len()` on
    /// the cell itself.
    #[inline]
    fn intern(&mut self, s: &str) -> u32 {
        let offset = self.data.len() as u32;
        self.data.extend_from_slice(s.as_bytes());
        offset
    }

    /// Returns the raw grapheme bytes stored at `offset` with the given
    /// byte length, or `None` if the slice is out of bounds.
    ///
    /// The `(offset, len)` pair comes from a [`Cell`]'s `text` field, which
    /// is callee-controlled and may be stale (e.g. carried across a swap
    /// or copied between buffers via [`Cell::with_style_merged`]). Bounds
    /// are therefore checked on every lookup.
    ///
    /// Returns `&[u8]` rather than `&str`: the renderer only ever writes
    /// bytes to VT output, and equality comparisons only need byte
    /// equality, so validating UTF-8 on every read would be wasted work.
    #[inline]
    fn get(&self, offset: u32, len: u8) -> Option<&[u8]> {
        let o = offset as usize;
        let end = o.checked_add(len as usize)?;
        self.data.get(o..end)
    }

    #[inline]
    fn clear(&mut self) {
        self.data.clear(); // keeps capacity
    }
}

/// A rectangular grid of terminal cells.
///
/// Stores styled text content for rendering to a terminal. Use
/// [`set_string`](Self::set_string) and [`set_style`](Self::set_style)
/// to modify the buffer contents.
pub struct Buffer {
    pub(crate) cells: Box<[Cell]>,
    pub(crate) side: SideBuffer,
    pub(crate) width: u16,
    pub(crate) height: u16,
    /// When `true`, styles written via [`set_stringn`](Self::set_stringn)
    /// and [`set_style`](Self::set_style) are quantized to the 256-color
    /// palette before being stored. Owned and toggled by [`DoubleBuffer`].
    pub(crate) quantize_rgb: bool,
}

/// A rectangular region defined by position and size.
///
/// Provides methods for splitting, containment checks, and layout calculations.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Rect {
    /// X coordinate of the top-left corner.
    pub x: u16,
    /// Y coordinate of the top-left corner.
    pub y: u16,
    /// Width of the rectangle.
    pub w: u16,
    /// Height of the rectangle.
    pub h: u16,
}

impl Rect {
    /// A zero-sized rectangle at the origin.
    pub const EMPTY: Rect = Rect {
        x: 0,
        y: 0,
        w: 0,
        h: 0,
    };
}

/// Defines how to split a dimension into two parts.
///
/// Implemented for `i32` (absolute pixel count) and `f64` (ratio).
/// Negative values take from the end instead of the beginning.
pub trait SplitRule: std::ops::Neg<Output = Self> {
    /// Splits the given value into two parts according to this rule.
    fn split_once(self, value: u16) -> (u16, u16);
}

/// A rectangle with associated rendering properties.
///
/// Created from a [`Rect`] via [`display`](Rect::display) or [`with`](Rect::with).
/// Provides a fluent API for rendering styled text.
pub struct DisplayRect {
    rect: Rect,
    properties: RenderProperties,
}

impl Rect {
    /// Returns `true` if the point is within this rectangle.
    pub fn contains(&self, x: u16, y: u16) -> bool {
        let x_delta = (x as u32).wrapping_sub(self.x as u32);
        let y_delta = (y as u32).wrapping_sub(self.y as u32);
        (x_delta < (self.w as u32)) & (y_delta < (self.h as u32))
    }
    /// Returns `true` if the rectangle has zero area.
    pub fn is_empty(self) -> bool {
        self.w == 0 || self.h == 0
    }

    /// Creates a [`DisplayRect`] for rendering within this rectangle.
    #[must_use]
    pub fn display(self) -> DisplayRect {
        DisplayRect {
            rect: self,
            properties: RenderProperties::default(),
        }
    }
    /// Creates a [`DisplayRect`] with the given property applied.
    #[must_use]
    pub fn with(self, property: impl RenderProperty) -> DisplayRect {
        let properties = {
            let mut props = RenderProperties::default();
            property.apply(&mut props);
            props
        };
        DisplayRect {
            rect: self,
            properties,
        }
    }
    /// Returns the left edge X coordinate.
    pub fn left(self) -> u16 {
        self.x
    }

    /// Returns the right edge X coordinate.
    pub fn right(self) -> u16 {
        self.x.saturating_add(self.w)
    }

    /// Returns the top edge Y coordinate.
    pub fn top(self) -> u16 {
        self.y
    }

    /// Returns the bottom edge Y coordinate.
    pub fn bottom(self) -> u16 {
        self.y.saturating_add(self.h)
    }

    /// Splits the rectangle vertically according to the given rule.
    ///
    /// Returns (top, bottom) rectangles.
    pub fn v_split(&self, rule: impl SplitRule) -> (Self, Self) {
        let (h1, h2) = rule.split_once(self.h);
        (
            Rect { h: h1, ..*self },
            Rect {
                h: h2,
                y: self.y + h1,
                ..*self
            },
        )
    }

    /// Splits the rectangle horizontally according to the given rule.
    ///
    /// Returns (left, right) rectangles.
    pub fn h_split(&self, rule: impl SplitRule) -> (Self, Self) {
        let (w1, w2) = rule.split_once(self.w);
        (
            Rect { w: w1, ..*self },
            Rect {
                w: w2,
                x: self.x + w1,
                ..*self
            },
        )
    }
    /// Removes and returns a portion from the right edge.
    pub fn take_right(&mut self, rule: impl SplitRule) -> Self {
        let (rest, new) = self.h_split(rule.neg());
        *self = rest;
        new
    }

    /// Removes and returns a portion from the bottom edge.
    pub fn take_bottom(&mut self, rule: impl SplitRule) -> Self {
        let (rest, new) = self.v_split(rule.neg());
        *self = rest;
        new
    }

    /// Removes and returns a portion from the left edge.
    pub fn take_left(&mut self, rule: impl SplitRule) -> Self {
        let (rest, new) = self.h_split(rule);
        *self = new;
        rest
    }

    /// Removes and returns a portion from the top edge.
    pub fn take_top(&mut self, rule: impl SplitRule) -> Self {
        let (rest, new) = self.v_split(rule);
        *self = new;
        rest
    }
}

impl SplitRule for i32 {
    fn split_once(self, value: u16) -> (u16, u16) {
        let value = value as u32;
        let c = self.unsigned_abs();
        let (mut a, mut b) = if value >= c {
            (c as u16, (value - c) as u16)
        } else {
            (value as u16, 0)
        };
        if self < 0 {
            std::mem::swap(&mut a, &mut b);
        }
        (a, b)
    }
}

impl SplitRule for f64 {
    fn split_once(self, value: u16) -> (u16, u16) {
        assert!((-1.0..=1.0).contains(&self), "Invalid Split Ratio");
        let mut a = ((value as f64) * self.abs()) as u16;
        let mut b = value - a;
        if self < 0.0 {
            std::mem::swap(&mut a, &mut b);
        }
        (a, b)
    }
}

fn write_palette_or_diff(
    current: Style,
    new: Style,
    palette: &[Vec<u8>],
    buf: &mut Vec<u8>,
    ignore_fg: bool,
) -> Style {
    let mut new = new;
    if ignore_fg {
        new.0 = (new.0 & !Style::FG_ALL_MASK) | (current.0 & Style::FG_ALL_MASK);
    }
    if new.is_palette() {
        if current == new {
            return new;
        }
        // Same palette index: only fg/bg changed, no need to re-emit palette bytes
        if current.is_palette() && current.palette_index() == new.palette_index() {
            let removed = (new.0 | current.0) ^ new.0;
            if removed & (Style::HAS_BG | Style::HAS_FG) == 0 {
                let mut target = new;
                if current.fg() == target.fg() {
                    target = target.without_fg();
                }
                if current.bg() == target.bg() {
                    target = target.without_bg();
                }
                let color_only = Style(target.0 & (Style::FG_ALL_MASK | Style::BG_ALL_MASK));
                if color_only != Style::DEFAULT {
                    vt::style(buf, color_only, false);
                }
                return new;
            }
        }
        // Different palette index or colors removed: full clear + palette + colors
        buf.extend_from_slice(b"\x1b[0m");
        if let Some(entry) = palette.get(new.palette_index() as usize) {
            buf.extend_from_slice(entry);
        }
        let color_only = Style(new.0 & (Style::FG_ALL_MASK | Style::BG_ALL_MASK));
        if color_only != Style::DEFAULT {
            vt::style(buf, color_only, false);
        }
        return new;
    }
    write_style_diff(current, new, buf)
}

pub(crate) fn write_style_diff(current: Style, new: Style, buf: &mut Vec<u8>) -> Style {
    if current == new {
        return current;
    }
    if current.is_palette() {
        vt::style(buf, new, true);
        return new;
    }
    let removed = (new.0 | current.0) ^ new.0;
    let clearing = removed & (Style::HAS_BG | Style::HAS_FG | Modifier::ALL.0 as u64) != 0;
    if clearing {
        vt::style(buf, new, true);
        return new;
    }
    let mut target = new;
    if current.fg() == target.fg() {
        target = target.without_fg();
    }
    if current.bg() == target.bg() {
        target = target.without_bg();
    }
    // todo only have the added modifiers
    vt::style(buf, target, false);
    // if removed & (Style::HAS_BG | Style::HAS_FG | Modifier::ALL.0 as u32) != 0 {
    //     vt::CLEAR_STYLE.write_to_buffer(buf);
    //     current = Style::DEFAULT;
    //     added = new.0;
    // }

    // vt::write_all_modifiers(buf, Modifier(added as u16 & Modifier::ALL.0));

    // if let Some(fg) = new.fg() {
    //     if current.fg() != Some(fg) {
    //         vt::fg_ansi(buf, fg);
    //     }
    // }
    // if let Some(bg) = new.bg() {
    //     if current.bg() != Some(bg) {
    //         vt::bg_ansi(buf, bg);
    //     }
    // }
    new
}

/// Compares two cells for rendering equivalence across distinct buffers.
///
/// Fast path: when `a` is inline, degenerates to a direct style + `[u8; 8]`
/// compare — the same codegen the old `derive(Eq)` produced. Only when `a`
/// is a handle do we consult the two side buffers.
#[inline(always)]
fn cells_eq(a: Cell, b: Cell, a_side: &SideBuffer, b_side: &SideBuffer) -> bool {
    if a.is_handle() {
        cells_eq_slow(a, b, a_side, b_side)
    } else {
        // `a` is inline (text[7] ≤ 7). If `b` is a handle (text[7] > 7), the
        // raw byte compare below catches it on the length byte alone, so the
        // fast path is correct whether or not `b` is a handle.
        a.style == b.style && a.text == b.text
    }
}

#[cold]
fn cells_eq_slow(a: Cell, b: Cell, a_side: &SideBuffer, b_side: &SideBuffer) -> bool {
    if !b.is_handle() {
        return false;
    }
    if a.style != b.style {
        return false;
    }
    let la = a.text[7];
    let lb = b.text[7];
    if la != lb {
        return false;
    }
    // A `None` return from `get` means a corrupted or stale handle. Treat
    // that as unequal so the diff forces a redraw rather than silently
    // conflating two bogus cells.
    match (
        a_side.get(a.handle_offset(), la),
        b_side.get(b.handle_offset(), lb),
    ) {
        (Some(a_bytes), Some(b_bytes)) => a_bytes == b_bytes,
        _ => false,
    }
}

pub fn clear_cells(cells: &mut [Cell]) {
    // cells.fill(Cell::EMPTY), but optimizes better
    let ptr = unsafe {
        std::slice::from_raw_parts_mut(cells.as_mut_ptr() as *mut _ as *mut u64, cells.len() * 2)
    };
    for cell in ptr {
        *cell = 0;
    }
}

impl Buffer {
    /// Scrolls the buffer upward by `amount` lines.
    ///
    /// Lines shifted off the top are discarded. New lines exposed at
    /// the bottom are filled with empty cells.
    pub fn scroll_up(&mut self, amount: u16) {
        if amount == 0 || self.height == 0 {
            return;
        }
        let amount = amount.min(self.height);
        let total = self.cells.len();
        let shifted = (self.width as usize) * (amount as usize);
        self.cells.copy_within(shifted.., 0);
        self.cells[total - shifted..].fill(Cell::EMPTY);
    }
    /// Scrolls the buffer downward by `amount` lines.
    ///
    /// Lines shifted off the bottom are discarded. New lines exposed
    /// at the top are filled with empty cells.
    pub fn scroll_down(&mut self, amount: u16) {
        if amount == 0 || self.height == 0 {
            return;
        }
        let amount = amount.min(self.height);
        let total = self.cells.len();
        let shifted = (self.width as usize) * (amount as usize);
        self.cells.copy_within(0..total - shifted, shifted);
        self.cells[0..shifted].fill(Cell::EMPTY);
    }

    /// Scrolls a rectangular sub-region by `rows` lines.
    ///
    /// The region covers columns `left..right` and rows `top..bot`.
    /// A positive `rows` value shifts content upward and a negative
    /// value shifts content downward. Cells exposed on the trailing
    /// edge are left untouched, so callers should overwrite them with
    /// fresh content if the vacated area needs to be cleared.
    ///
    /// Out-of-range coordinates are clamped to the buffer dimensions.
    /// If the region is empty or `rows` is zero the call is a no-op.
    pub fn scroll_region(&mut self, top: u16, bot: u16, left: u16, right: u16, rows: i32) {
        if bot <= top || right <= left || rows == 0 {
            return;
        }
        let top = top.min(self.height) as usize;
        let bot = bot.min(self.height) as usize;
        let left = left.min(self.width) as usize;
        let right = right.min(self.width) as usize;
        let width = self.width as usize;
        let run = right - left;
        let region_rows = bot - top;
        if rows > 0 {
            let s = rows as usize;
            if s >= region_rows {
                return;
            }
            for y in top..bot - s {
                let src = (y + s) * width + left;
                let dst = y * width + left;
                self.cells.copy_within(src..src + run, dst);
            }
        } else {
            let s = (-rows) as usize;
            if s >= region_rows {
                return;
            }
            for y in ((top + s)..bot).rev() {
                let src = (y - s) * width + left;
                let dst = y * width + left;
                self.cells.copy_within(src..src + run, dst);
            }
        }
    }
    /// Returns the buffer width in cells.
    pub fn width(&self) -> u16 {
        self.width
    }

    /// Returns the buffer height in cells.
    pub fn height(&self) -> u16 {
        self.height
    }

    /// Creates a new buffer of `width` columns by `height` rows,
    /// filled with empty cells.
    pub fn new(width: u16, height: u16) -> Buffer {
        let cells = vec![Cell::EMPTY; width as usize * height as usize].into_boxed_slice();
        Buffer {
            cells,
            side: SideBuffer::default(),
            width,
            height,
            quantize_rgb: false,
        }
    }
    /// Returns a mutable reference to the cell at `(x, y)`, or `None`
    /// if the position is outside the buffer.
    ///
    /// This is a raw escape hatch that bypasses grapheme segmentation,
    /// display-width accounting, and RGB quantization. Prefer
    /// [`Buffer::set_stringn`] and [`Buffer::set_style`] for ordinary
    /// drawing.
    ///
    /// # Invariants
    ///
    /// Writing a handle cell obtained from a different buffer leaves
    /// the new slot pointing at the wrong arena and produces garbage
    /// on resolution. Writing a wide grapheme into a single cell
    /// without also clearing the cell to its right desynchronizes the
    /// display-width accounting that the render path relies on.
    #[doc(hidden)]
    pub fn get_mut(&mut self, x: u16, y: u16) -> Option<&mut Cell> {
        let index = (y as u32) * (self.width as u32) + (x as u32);
        self.cells.get_mut(index as usize)
    }
    /// Returns a read-only view of every cell in the buffer in row-major
    /// order.
    ///
    /// The slice length is always `width * height`. Index a cell at
    /// `(x, y)` as `cells[y * width + x]`.
    ///
    /// # Invariants
    ///
    /// A handle cell borrows its grapheme from the buffer it came from.
    /// Resolve handle cells with [`Buffer::handle_text`] before copying
    /// them into any other buffer.
    #[inline]
    pub fn cells(&self) -> &[Cell] {
        &self.cells
    }

    /// Returns the grapheme bytes of a handle cell produced by this
    /// buffer.
    ///
    /// For inline cells use [`Cell::text_inline`] instead. The
    /// returned slice is valid UTF-8 and points into storage owned by
    /// this buffer.
    ///
    /// # Invariants
    ///
    /// `cell` must have been produced by this buffer. Passing a handle
    /// cell from another buffer returns `None` if its offset happens to
    /// fall outside this buffer's storage, and silently returns
    /// unrelated bytes if it does not.
    #[inline]
    pub fn handle_text(&self, cell: Cell) -> Option<&[u8]> {
        if !cell.is_handle() {
            return None;
        }
        self.side.get(cell.handle_offset(), cell.handle_len())
    }

    /// Returns the number of bytes currently retained to back handle
    /// cells in this buffer.
    ///
    /// Handle storage grows every time a cluster longer than seven
    /// UTF-8 bytes is written, and overwritten handle cells leave
    /// their former bytes behind until the storage is compacted. Use
    /// this reading to decide when to call [`Buffer::compact_side`] on
    /// long-lived buffers.
    #[inline]
    pub fn side_len(&self) -> usize {
        self.side.data.len()
    }

    /// Reclaims handle storage that is no longer referenced by any
    /// live handle cell.
    ///
    /// Long-lived buffers accumulate unreferenced handle bytes as
    /// cells are overwritten. Calling this method rebuilds the storage
    /// so that only the bytes reachable from the current cell grid
    /// remain. Handle cells are updated in place and continue to
    /// resolve through [`Buffer::handle_text`] afterwards. Cells whose
    /// recorded position is already out of range are reset to empty.
    pub fn compact_side(&mut self) {
        if self.side.data.is_empty() {
            return;
        }
        let mut new_side = SideBuffer::default();
        let old_side = &self.side;
        for cell in self.cells.iter_mut() {
            if !cell.is_handle() {
                continue;
            }
            let len = cell.handle_len();
            let offset = cell.handle_offset();
            let Some(bytes) = old_side.get(offset, len) else {
                *cell = Cell::EMPTY;
                continue;
            };
            let new_offset = new_side.data.len() as u32;
            new_side.data.extend_from_slice(bytes);
            let o = new_offset.to_le_bytes();
            cell.text[0] = o[0];
            cell.text[1] = o[1];
            cell.text[2] = o[2];
            cell.text[3] = o[3];
        }
        self.side = new_side;
    }

    /// Returns a mutable slice covering the cells from `(x, y)` to the
    /// end of row `y`, or `None` if the position is outside the
    /// buffer.
    ///
    /// Intended for internal row-at-a-time overwrites. The same risks
    /// that apply to [`Buffer::get_mut`] apply here, amplified by
    /// acting on a whole run of cells at once.
    ///
    /// # Invariants
    ///
    /// Every cell written into the returned slice must come from this
    /// same buffer or be an inline cell. Wide graphemes must be
    /// followed by an empty cell so the render path preserves their
    /// display width.
    #[doc(hidden)]
    pub fn row_remaining_mut(&mut self, x: u16, y: u16) -> Option<&mut [Cell]> {
        let base = (y as u32) * (self.width as u32);
        let start = base + (x as u32);
        // todo can opt this
        let end = base + self.width as u32;
        self.cells.get_mut(start as usize..end as usize)
    }

    fn render_diff(
        &mut self,
        buf: &mut Vec<u8>,
        old: &Buffer,
        x_offset: u16,
        y_offset: u16,
        blanking: bool,
        inline: bool,
        palette: &[Vec<u8>],
    ) {
        if self.cells.len() != old.cells.len() {
            // Fall back to full render; use bounded=false to preserve old behavior
            // for callers that haven't opted into sub-region rendering. Callers
            // that use x_offset/bounded always ensure old and new have matching
            // sizes (they're swapped in-place inside DoubleBuffer), so this path
            // is only reached on resize, which resets diffable to false anyway.
            self.render(buf, x_offset, y_offset, false, inline, palette);
            return;
        }
        if self.width == 0 || self.height == 0 {
            return;
        }
        let new_side = &self.side;
        let old_side = &old.side;
        vt::CLEAR_STYLE.write_to_buffer(buf);
        let mut current_style = Style::DEFAULT;
        let mut old_cells = old.cells.iter();
        let mut cur_y: u16 = 0;
        macro_rules! position_to {
            ($col:expr, $y:expr) => {{
                let col_v: u16 = $col;
                let y_v: u16 = $y;
                if inline {
                    if y_v > cur_y {
                        // Reset SGR before any \r\n that may scroll the screen.
                        // BCE-enabled terminals fill the newly scrolled-in line
                        // with the active background color, leaking the previous
                        // row's bg into the next row.
                        if current_style != Style::DEFAULT {
                            vt::CLEAR_STYLE.write_to_buffer(buf);
                            current_style = Style::DEFAULT;
                        }
                        for _ in 0..(y_v - cur_y) {
                            buf.extend_from_slice(b"\r\n");
                        }
                        cur_y = y_v;
                    }
                    buf.push(b'\r');
                    if col_v > 0 {
                        MoveCursorRight(col_v).write_to_buffer(buf);
                    }
                } else {
                    MoveCursor(col_v, y_v + y_offset).write_to_buffer(buf);
                }
            }};
        }
        for (y, row) in self.cells.chunks_exact(self.width as usize).enumerate() {
            let y = y as u16;
            let mut moved = false;
            let mut matching_count = 0;
            let mut new_cells = row.iter();
            let mut erased_cell: Option<Cell> = None;
            'next_cell: while let Some(new) = new_cells.next() {
                let mut new = *new;
                let Some(old) = erased_cell.or_else(|| old_cells.next().copied()) else {
                    return;
                };
                if cells_eq(new, old, new_side, old_side) {
                    matching_count += 1;
                    continue;
                }
                if new.is_empty() {
                    let mut blank_overwrite = 1;
                    let mut blank_kind = new;
                    'continue_new: {
                        loop {
                            let Some(&new_k) = new_cells.next() else {
                                // end of row
                                break;
                            };
                            let Some(old_k) = erased_cell.or_else(|| old_cells.next().copied())
                            else {
                                // end of file
                                break;
                            };
                            if cells_eq(new_k, old_k, new_side, old_side) {
                                if !moved {
                                    moved = true;
                                    position_to!(x_offset + matching_count, y);
                                    matching_count = 0;
                                }
                                if matching_count > 0 {
                                    MoveCursorRight(matching_count).write_to_buffer(buf);
                                    // matching_count = 0;
                                }
                                current_style = write_palette_or_diff(
                                    current_style,
                                    blank_kind.style(),
                                    palette,
                                    buf,
                                    true,
                                );
                                if !blanking || blank_overwrite < 50 {
                                    buf.extend(std::iter::repeat_n(b' ', blank_overwrite as usize));
                                    matching_count = 1;
                                } else {
                                    buf.extend_from_slice(b"\x1b[K");
                                    let rem_old = new_cells.len();
                                    if rem_old > 0 && erased_cell.is_none() {
                                        old_cells.nth(rem_old - 1);
                                    }
                                    erased_cell = Some(blank_kind);
                                    matching_count = blank_overwrite;
                                    if cells_eq(new_k, blank_kind, new_side, new_side) {
                                        matching_count += 1;
                                        continue 'next_cell;
                                    } else {
                                        new = new_k;
                                        break 'continue_new;
                                    }
                                }
                                continue 'next_cell;
                            }
                            if cells_eq(new_k, blank_kind, new_side, new_side) {
                                blank_overwrite += 1;
                                continue;
                            }

                            if !moved {
                                moved = true;
                                position_to!(x_offset + matching_count, y);
                                matching_count = 0;
                            }
                            if matching_count > 0 {
                                MoveCursorRight(matching_count).write_to_buffer(buf);
                                matching_count = 0;
                            }

                            current_style = write_palette_or_diff(
                                current_style,
                                blank_kind.style(),
                                palette,
                                buf,
                                true,
                            );
                            if !blanking || blank_overwrite < 50 {
                                buf.extend(std::iter::repeat_n(b' ', blank_overwrite as usize));
                            } else {
                                buf.extend_from_slice(b"\x1b[K");
                                let rem_old = new_cells.len();
                                if rem_old > 0 && erased_cell.is_none() {
                                    old_cells.nth(rem_old - 1);
                                }
                                erased_cell = Some(blank_kind);
                                matching_count = blank_overwrite;
                            }

                            if new_k.is_empty() {
                                blank_kind = new_k;
                                blank_overwrite = 1;
                                continue;
                            } else {
                                new = new_k;
                                break 'continue_new;
                            }
                        }

                        if !moved {
                            // moved = true;
                            position_to!(x_offset + matching_count, y);
                            matching_count = 0;
                        }
                        if matching_count > 0 {
                            MoveCursorRight(matching_count).write_to_buffer(buf);
                            // matching_count = 0;
                        }
                        current_style = write_palette_or_diff(
                            current_style,
                            blank_kind.style(),
                            palette,
                            buf,
                            true,
                        );

                        if !blanking || blank_overwrite < 8 {
                            for _ in 0..blank_overwrite {
                                buf.push(b' ');
                            }
                        } else {
                            buf.extend_from_slice(b"\x1b[K");
                        }

                        break 'next_cell;
                    }
                }

                if !moved {
                    moved = true;
                    position_to!(x_offset + matching_count, y);
                    matching_count = 0;
                }
                if matching_count > 0 {
                    MoveCursorRight(matching_count).write_to_buffer(buf);
                    matching_count = 0;
                }
                let text = new.text(new_side);
                current_style = write_palette_or_diff(
                    current_style,
                    new.style(),
                    palette,
                    buf,
                    text.is_empty(),
                );
                if text.is_empty() {
                    buf.push(b' ');
                } else {
                    buf.extend_from_slice(text);
                }
            }
        }
        if inline {
            vt::CLEAR_STYLE.write_to_buffer(buf);
            let trailing = self.height.saturating_sub(cur_y);
            for _ in 0..trailing {
                buf.extend_from_slice(b"\r\n");
            }
        }
    }
    fn render(
        &mut self,
        buf: &mut Vec<u8>,
        x_offset: u16,
        y_offset: u16,
        bounded: bool,
        inline: bool,
        palette: &[Vec<u8>],
    ) {
        if self.width == 0 || self.height == 0 {
            return;
        }
        if !bounded && !inline {
            if x_offset == 0 && y_offset == 0 {
                vt::MOVE_CURSOR_TO_ORIGIN.write_to_buffer(buf);
            } else {
                MoveCursor(x_offset, y_offset).write_to_buffer(buf);
            }
        }
        vt::CLEAR_STYLE.write_to_buffer(buf);
        if !bounded || inline {
            buf.extend_from_slice(vt::CLEAR_BELOW);
        }

        let side = &self.side;
        let mut current_style = Style::DEFAULT;
        let mut cur_y: u16 = 0;
        for (y, row) in self.cells.chunks_exact(self.width as usize).enumerate() {
            let y = y as u16;
            let mut moved = false;
            let mut blank_extension = 0;
            for &cell in row.iter() {
                if cell.is_blank_or_empty() {
                    blank_extension += 1;
                    continue;
                }
                if !moved {
                    moved = true;
                    if inline {
                        if y > cur_y {
                            // Reset SGR before any \r\n that may scroll the screen.
                            // BCE-enabled terminals fill the newly scrolled-in
                            // line with the active background color, leaking the
                            // previous row's bg into the next row.
                            if current_style != Style::DEFAULT {
                                vt::CLEAR_STYLE.write_to_buffer(buf);
                                current_style = Style::DEFAULT;
                            }
                            for _ in 0..(y - cur_y) {
                                buf.extend_from_slice(b"\r\n");
                            }
                            cur_y = y;
                        }
                        buf.push(b'\r');
                        let col = x_offset + blank_extension;
                        if col > 0 {
                            MoveCursorRight(col).write_to_buffer(buf);
                        }
                    } else {
                        MoveCursor(x_offset + blank_extension, y + y_offset).write_to_buffer(buf);
                    }
                    blank_extension = 0;
                }
                if blank_extension > 0 {
                    if blank_extension < 5 && current_style == Style::DEFAULT {
                        for _ in 0..blank_extension {
                            buf.push(b' ');
                        }
                    } else {
                        MoveCursorRight(blank_extension).write_to_buffer(buf);
                    }
                    blank_extension = 0;
                }
                current_style =
                    write_palette_or_diff(current_style, cell.style(), palette, buf, false);
                let text = cell.text(side);
                if text.is_empty() {
                    buf.push(b' ');
                } else {
                    buf.extend_from_slice(text);
                }
            }
        }
        if inline {
            vt::CLEAR_STYLE.write_to_buffer(buf);
            let trailing = self.height.saturating_sub(cur_y);
            for _ in 0..trailing {
                buf.extend_from_slice(b"\r\n");
            }
        } else if !bounded {
            vt::MOVE_CURSOR_TO_ORIGIN.write_to_buffer(buf);
        }
    }
    /// Merges `style` into every cell inside `area`.
    ///
    /// Foreground and background colors on `style` overwrite whatever
    /// the cell currently carries, and any color left unset on `style`
    /// is preserved. Modifier bits on `style` are applied on top of
    /// the existing modifiers. Cells outside the buffer are ignored.
    pub fn set_style(&mut self, area: Rect, style: Style) {
        let Rect { x, y, w, h } = area;
        let style = if self.quantize_rgb {
            style.quantize_rgb()
        } else {
            style
        };
        let mut keep_mask: u64 = 0;
        let mut new_mask: u64 = style.0;

        if style.fg().is_none() {
            keep_mask |= Style::FG_ALL_MASK;
        }
        if style.bg().is_none() {
            keep_mask |= Style::BG_ALL_MASK;
        }
        new_mask &= !keep_mask;

        for y in y..y + h {
            if let Some(row) = self.row_remaining_mut(x, y) {
                for cell in row.iter_mut().take(w as usize) {
                    cell.style = (cell.style & keep_mask) | new_mask;
                }
            }
        }
    }
    /// Writes `string` at `(x, y)` using `style`.
    ///
    /// Writing stops at the right edge of the buffer or when `string`
    /// is exhausted. Returns the position one cell past the last cell
    /// written.
    pub fn set_string(&mut self, x: u16, y: u16, string: &str, style: Style) -> (u16, u16) {
        self.set_stringn(x, y, string, usize::MAX, style)
    }

    /// Writes `string` at `(x, y)` using `style`, stopping after
    /// `max_width` columns of display width.
    ///
    /// Text is segmented into grapheme clusters and each cluster
    /// occupies its natural display width. Wide clusters such as CJK
    /// characters take two columns, and clusters that cannot fit in
    /// the remaining width are dropped. Returns the position one cell
    /// past the last cell written.
    pub fn set_stringn(
        &mut self,
        x: u16,
        y: u16,
        string: &str,
        max_width: usize,
        style: Style,
    ) -> (u16, u16) {
        let style = if self.quantize_rgb {
            style.quantize_rgb()
        } else {
            style
        };
        let mut remaining_width = (self.width.saturating_sub(x) as usize).min(max_width) as u16;
        let initial_remaining_width = remaining_width;

        // Split borrow on `self`: grab `cells` and `side` independently so
        // long graphemes can be interned while we're writing to cells.
        let row_width = self.width as u32;
        let base = (y as u32) * row_width;
        let start = (base + x as u32) as usize;
        let end = (base + row_width) as usize;
        let Some(target) = self.cells.get_mut(start..end) else {
            return (x, y);
        };
        let mut target_cells = target.iter_mut();
        if string.is_ascii() {
            for &byte in string.as_bytes() {
                if byte.is_ascii_control() {
                    continue;
                }
                let Some(new_rem) = remaining_width.checked_sub(1) else {
                    break;
                };
                remaining_width = new_rem;
                let Some(slot) = target_cells.next() else {
                    return (x + initial_remaining_width, y);
                };
                // SAFETY: `string.is_ascii()` guarantees every retained byte is
                // valid one-byte UTF-8, and control bytes are skipped above.
                *slot = unsafe { Cell::new_ascii(byte, style) };
            }
            return (x + (initial_remaining_width - remaining_width), y);
        }

        let side = &mut self.side;
        for symbol in UnicodeSegmentation::graphemes(string, true) {
            if symbol.contains(char::is_control) {
                continue;
            }
            let width = symbol.width() as u16;
            if width == 0 {
                continue;
            }
            let Some(new_rem) = remaining_width.checked_sub(width) else {
                break;
            };
            remaining_width = new_rem;

            let cell = if symbol.len() <= 7 {
                Cell::new(symbol, style)
            } else {
                // The handle cell carries a u8 length, so graphemes wider
                // than 255 bytes are truncated at the largest UTF-8 char
                // boundary that still fits. No realistic grapheme hits this.
                let truncated = if symbol.len() <= u8::MAX as usize {
                    symbol
                } else {
                    &symbol[..symbol.floor_char_boundary(u8::MAX as usize)]
                };
                if truncated.len() <= 7 {
                    Cell::new(truncated, style)
                } else {
                    let offset = side.intern(truncated);
                    Cell::new_handle(offset, truncated.len() as u8, style)
                }
            };

            if let Some(slot) = target_cells.next() {
                *slot = cell;
            } else {
                return (x + initial_remaining_width, y);
            }
            // Pad wider-than-one graphemes with empty cells so they occupy
            // their full display width.
            for _ in 1..width {
                if let Some(slot) = target_cells.next() {
                    *slot = Cell::EMPTY;
                } else {
                    return (x + initial_remaining_width, y);
                }
            }
        }
        (x + (initial_remaining_width - remaining_width), y)
    }
}

bitflags::bitflags! {
    /// Configuration flags for terminal mode and capabilities.
    ///
    /// Controls raw mode, alternate screen, mouse capture, cursor visibility,
    /// extended keyboard input, and bracketed paste mode.
    #[derive(Debug, PartialOrd, PartialEq, Eq, Clone, Copy, Hash)]
    pub struct TerminalFlags: u8 {
        /// Enables raw mode (disables line buffering and echo).
        const RAW_MODE = 0b0000_0001;
        /// Switches to the alternate screen buffer.
        const ALT_SCREEN = 0b0000_0010;
        /// Enables mouse event capture.
        const MOUSE_CAPTURE = 0b0000_0100;
        /// Hides the terminal cursor.
        const HIDE_CURSOR = 0b0000_1000;
        /// Enables extended keyboard input (Kitty protocol).
        const EXTENDED_KEYBOARD_INPUTS = 0b0001_0000;
        /// Enables bracketed paste mode.
        ///
        /// Pasted text is wrapped in `ESC[200~` and `ESC[201~`. The
        /// corresponding [`Event::Paste`](crate::event::Event::Paste) is only
        /// available when the `bracketed-paste` crate feature is enabled.
        const BRACKETED_PASTE = 0b0010_0000;
    }
}

/// A handle to the terminal with RAII-based mode management.
///
/// Automatically restores terminal settings when dropped. Implements
/// [`Write`] for direct output.
///
/// [`Write`]: std::io::Write
///
/// # Examples
///
/// ```no_run
/// use extui::{Terminal, TerminalFlags};
///
/// let mut term = Terminal::open(
///     TerminalFlags::RAW_MODE | TerminalFlags::ALT_SCREEN
/// )?;
/// # Ok::<(), std::io::Error>(())
/// ```
pub struct Terminal {
    fd: std::mem::ManuallyDrop<std::fs::File>,
    termios: sys::Termios,
    flags: TerminalFlags,
}

fn write_enable_terminal_flags(
    file: &mut std::fs::File,
    flags: TerminalFlags,
) -> std::io::Result<()> {
    // todo use stack buffer
    let mut buffer = Vec::new();

    if flags.contains(TerminalFlags::MOUSE_CAPTURE) {
        buffer.extend_from_slice(vt::ENABLE_NON_MOTION_MOUSE_EVENTS);
    }
    if flags.contains(TerminalFlags::HIDE_CURSOR) {
        buffer.extend_from_slice(vt::HIDE_CURSOR);
    }

    if flags.contains(TerminalFlags::ALT_SCREEN) {
        buffer.extend_from_slice(vt::ENABLE_ALT_SCREEN);
    }
    if flags.contains(TerminalFlags::BRACKETED_PASTE) {
        buffer.extend_from_slice(vt::ENABLE_BRACKETED_PASTE);
    }
    if flags.contains(TerminalFlags::EXTENDED_KEYBOARD_INPUTS) {
        (KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES
            | KeyboardEnhancementFlags::REPORT_ALTERNATE_KEYS)
            .write_to_buffer(&mut buffer);
    }

    file.write_all(&buffer)?;
    Ok(())
}

fn write_disable_terminal_flags(
    file: &mut std::fs::File,
    flags: TerminalFlags,
) -> std::io::Result<()> {
    // todo use stack buffer
    let mut buffer = Vec::new();
    if flags.contains(TerminalFlags::MOUSE_CAPTURE) {
        buffer.extend_from_slice(vt::DISABLE_NON_MOTION_MOUSE_EVENTS);
    }
    if flags.contains(TerminalFlags::HIDE_CURSOR) {
        buffer.extend_from_slice(vt::SHOW_CURSOR);
    }
    if flags.contains(TerminalFlags::ALT_SCREEN) {
        buffer.extend_from_slice(vt::DISABLE_ALT_SCREEN);
    }
    if flags.contains(TerminalFlags::BRACKETED_PASTE) {
        buffer.extend_from_slice(vt::DISABLE_BRACKETED_PASTE);
    }
    if flags.contains(TerminalFlags::EXTENDED_KEYBOARD_INPUTS) {
        buffer.extend_from_slice(vt::POP_KEYBOARD_ENABLEMENT);
    }
    file.write_all(&buffer)?;
    Ok(())
}
impl std::io::Write for Terminal {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.fd.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl Terminal {
    /// Returns `true` if the terminal is in raw mode.
    pub fn is_raw(&self) -> bool {
        self.flags.contains(TerminalFlags::RAW_MODE)
    }

    /// Temporarily restores original terminal settings while executing a function.
    ///
    /// Useful for spawning subprocesses or showing prompts.
    pub fn with_bypass<T>(&mut self, mut func: impl FnMut() -> T) -> std::io::Result<T> {
        write_disable_terminal_flags(&mut self.fd, self.flags)?;
        sys::attr::set_terminal_attr(self.fd.as_fd(), &self.termios)?;
        //Note: if func panics then Terminal will not be restored.
        let t = func();
        if self.flags.contains(TerminalFlags::RAW_MODE) {
            let mut raw_term = self.termios;
            sys::attr::raw_terminal_attr(&mut raw_term);
            sys::attr::set_terminal_attr(self.fd.as_fd(), &raw_term)?;
        }
        write_enable_terminal_flags(&mut self.fd, self.flags)?;
        Ok(t)
    }
    /// Creates a terminal handle from a raw file descriptor.
    ///
    /// # Errors
    ///
    /// Returns an error if the file descriptor is not a terminal.
    pub fn new(fd: RawFd, flags: TerminalFlags) -> std::io::Result<Terminal> {
        let mut stdout = ManuallyDrop::new(unsafe { std::fs::File::from_raw_fd(fd) });
        if !stdout.is_terminal() {
            return Err(std::io::Error::other("Stdout is not a terminal"));
        }
        let termios = sys::attr::get_terminal_attr(stdout.as_fd())?;
        if flags.contains(TerminalFlags::RAW_MODE) {
            let mut raw_term = termios;
            sys::attr::raw_terminal_attr(&mut raw_term);
            sys::attr::set_terminal_attr(stdout.as_fd(), &raw_term)?;
        }
        write_enable_terminal_flags(&mut stdout, flags)?;
        Ok(Terminal {
            fd: stdout,
            flags,
            termios,
        })
    }
    /// Opens the terminal using stdout.
    ///
    /// # Errors
    ///
    /// Returns an error if stdout is not a terminal.
    pub fn open(flags: TerminalFlags) -> std::io::Result<Terminal> {
        let mut stdout = ManuallyDrop::new(unsafe { std::fs::File::from_raw_fd(1) });
        if !stdout.is_terminal() {
            return Err(std::io::Error::other("Stdout is not a terminal"));
        }
        let termios = sys::attr::get_terminal_attr(stdout.as_fd())?;
        if flags.contains(TerminalFlags::RAW_MODE) {
            let mut raw_term = termios;
            sys::attr::raw_terminal_attr(&mut raw_term);
            sys::attr::set_terminal_attr(stdout.as_fd(), &raw_term)?;
        }
        write_enable_terminal_flags(&mut stdout, flags)?;
        Ok(Terminal {
            fd: stdout,
            flags,
            termios,
        })
    }
    /// Returns the terminal size as (columns, rows).
    pub fn size(&self) -> std::io::Result<(u16, u16)> {
        sys::size::terminal_size_fd(&self.fd.as_fd())
    }
}

impl Drop for Terminal {
    fn drop(&mut self) {
        let _ = write_disable_terminal_flags(&mut self.fd, self.flags);
        let _ = sys::attr::set_terminal_attr(self.fd.as_fd(), &self.termios);
    }
}

/// Returns `true` if the environment advertises 24-bit RGB (truecolor)
/// support via the `COLORTERM` variable.
///
/// Checks for the conventional values `truecolor` and `24bit`, which are
/// set by terminals that handle the `\x1b[38;2;R;G;Bm` SGR sequence. This
/// is the same heuristic used by most terminal libraries and is reliable
/// in practice across modern terminal emulators, multiplexers, and SSH
/// sessions that forward the variable.
///
/// This check performs no terminal I/O. Pair it with
/// [`DoubleBuffer::set_rgb_supported`] during setup:
///
/// ```no_run
/// use extui::{DoubleBuffer, rgb_supported_from_env};
///
/// let mut buf = DoubleBuffer::new(80, 24);
/// buf.set_rgb_supported(rgb_supported_from_env());
/// ```
pub fn rgb_supported_from_env() -> bool {
    let Ok(value) = std::env::var("COLORTERM") else {
        return false;
    };
    value.eq_ignore_ascii_case("truecolor") || value.eq_ignore_ascii_case("24bit")
}

/// Double-buffered terminal output with differential rendering.
///
/// Maintains two buffers to compute minimal updates between frames,
/// reducing flicker and bandwidth.
///
/// # Examples
///
/// ```no_run
/// use extui::{DoubleBuffer, Terminal, TerminalFlags};
///
/// let mut term = Terminal::open(TerminalFlags::RAW_MODE)?;
/// let (w, h) = term.size()?;
/// let mut buf = DoubleBuffer::new(w, h);
///
/// // Draw to buf, then render
/// buf.render(&mut term);
/// # Ok::<(), std::io::Error>(())
/// ```
pub struct DoubleBuffer {
    current: Buffer,
    previous: Buffer,
    /// The output byte buffer containing VT escape sequences.
    pub buf: Vec<u8>,
    diffable: bool,
    blanking: bool,
    rgb_supported: bool,
    scroll: i16,
    scroll_region: Option<QueuedScrollRegion>,
    #[doc(hidden)]
    pub y_offset: u16,
    /// Column offset applied to every `MoveCursor` emitted during render.
    /// Use together with [`bounded`](Self::bounded) to render a sub-region
    /// that does not start at column zero of the terminal.
    #[doc(hidden)]
    pub x_offset: u16,
    /// When `true`, this buffer is treated as a bounded sub-region of the
    /// terminal rather than owning everything from `(0, y_offset)` downward.
    /// The initial full render skips `CLEAR_BELOW` so existing content under
    /// the buffer is preserved, and no final cursor-to-origin escape is
    /// emitted. Use this for overlay widgets like command palettes.
    #[doc(hidden)]
    pub bounded: bool,
    epoch: u64,
    palette: Vec<Vec<u8>>,
    cursor: CursorOutput,
}

#[derive(Clone, Copy)]
struct QueuedScrollRegion {
    top: u16,
    bottom: u16,
    amount: i16,
}

#[derive(Clone, Copy)]
struct CursorRequest {
    x: u16,
    y: u16,
    shape: CursorShape,
}

#[derive(Clone, Copy, Default)]
enum CursorMode {
    /// The cursor API has not been touched — `render_internal` emits no
    /// cursor bytes, preserving behaviour for callers that never opted in.
    #[default]
    Untouched,
    Visible(CursorRequest),
    Hidden,
}

#[derive(Default)]
struct CursorOutput {
    mode: CursorMode,
    last_emitted_visible: Option<bool>,
    last_emitted_shape: Option<CursorShape>,
}

impl CursorOutput {
    fn invalidate(&mut self) {
        self.last_emitted_visible = None;
        self.last_emitted_shape = None;
    }
}

impl DoubleBuffer {
    /// Returns the full buffer area as a [`Rect`].
    pub fn rect(&self) -> Rect {
        Rect {
            x: 0,
            y: 0,
            w: self.current.width,
            h: self.current.height,
        }
    }
    /// Returns the buffer epoch, incremented on each resize or reset.
    pub fn epoch(&self) -> u64 {
        self.epoch
    }

    /// Returns the buffer width in cells.
    pub fn width(&self) -> u16 {
        self.current.width
    }

    /// Returns the buffer height in cells.
    pub fn height(&self) -> u16 {
        self.current.height
    }
    /// Writes `cell` verbatim at `(x, y)`, skipping grapheme
    /// segmentation and width accounting.
    ///
    /// This is the fast path for copying cells between buffers when
    /// the caller already knows the grapheme and style.
    ///
    /// # Invariants
    ///
    /// Only inline cells may be safely copied across buffers. A handle
    /// cell produced by one buffer must not be written into another
    /// via this method. To copy a handle cell, resolve it with
    /// [`Buffer::handle_text`] on its source buffer and write the
    /// resulting text through [`DoubleBuffer::set_stringn`].
    pub fn set_cell(&mut self, x: u16, y: u16, cell: Cell) {
        if let Some(target) = self.current.get_mut(x, y) {
            *target = cell;
        }
    }
    /// Applies a style to all cells within the given area.
    pub fn set_style(&mut self, area: Rect, style: Style) {
        self.current.set_style(area, style)
    }

    /// Writes `string` at `(x, y)` using `style`.
    ///
    /// See [`Buffer::set_string`] for the detailed semantics.
    pub fn set_string(&mut self, x: u16, y: u16, string: &str, style: Style) -> (u16, u16) {
        self.current.set_string(x, y, string, style)
    }

    /// Writes `string` at `(x, y)` using `style`, stopping after
    /// `max_width` columns of display width.
    ///
    /// See [`Buffer::set_stringn`] for the detailed semantics.
    pub fn set_stringn(
        &mut self,
        x: u16,
        y: u16,
        string: &str,
        max_width: usize,
        style: Style,
    ) -> (u16, u16) {
        self.current.set_stringn(x, y, string, max_width, style)
    }

    /// Creates a new double buffer of `width` columns by `height`
    /// rows.
    pub fn new(width: u16, height: u16) -> DoubleBuffer {
        DoubleBuffer {
            current: Buffer::new(width, height),
            previous: Buffer::new(width, height),
            diffable: false,
            blanking: true,
            rgb_supported: true,
            buf: Vec::with_capacity(16 * 1024),
            scroll: 0,
            scroll_region: None,
            epoch: 0,
            y_offset: 0,
            x_offset: 0,
            bounded: false,
            palette: Vec::new(),
            cursor: CursorOutput::default(),
        }
    }

    /// Requests that a visible terminal cursor be drawn at `(x, y)` with
    /// `shape` the next time the buffer is rendered.
    ///
    /// The request is sticky: once set, the cursor stays at this position
    /// and shape across renders until [`set_cursor`](Self::set_cursor) or
    /// [`hide_cursor`](Self::hide_cursor) is called again. The position is
    /// always re-emitted on every render because the cell diff moves the
    /// real terminal cursor around as it paints; shape changes are diffed
    /// so DECSCUSR is only emitted when the shape differs from the last
    /// rendered frame.
    ///
    /// Calling this for the first time takes over cursor management from
    /// [`TerminalFlags::HIDE_CURSOR`]: the initial hide-cursor escape
    /// emitted at terminal open time is overridden by the show-cursor
    /// escape written on the first render after `set_cursor`.
    pub fn set_cursor(&mut self, x: u16, y: u16, shape: CursorShape) {
        self.cursor.mode = CursorMode::Visible(CursorRequest { x, y, shape });
    }

    /// Requests that the terminal cursor be hidden from the next render
    /// onward. Sticky; remains hidden until
    /// [`set_cursor`](Self::set_cursor) is called again.
    ///
    /// Has no effect if the cursor API has never been used and the
    /// terminal is already hidden via [`TerminalFlags::HIDE_CURSOR`] —
    /// the hide escape is only emitted when the last rendered visibility
    /// state disagrees with the request.
    pub fn hide_cursor(&mut self) {
        self.cursor.mode = CursorMode::Hidden;
    }

    /// Returns `true` if the output terminal is assumed to accept 24-bit
    /// RGB color escapes. When `false`, any [`Color::Rgb`] on
    /// a [`Style`] passed into [`set_string`](Self::set_string),
    /// [`set_stringn`](Self::set_stringn), or [`set_style`](Self::set_style)
    /// is quantized to the nearest 256-color palette index before the
    /// cell is stored.
    pub fn rgb_supported(&self) -> bool {
        self.rgb_supported
    }

    /// Sets whether the output terminal supports 24-bit RGB color.
    ///
    /// When disabled, any [`Color::Rgb`] written into the
    /// buffer is quantized to the nearest 256-color palette entry on
    /// the spot. Callers can build RGB-styled buffers without having
    /// to special-case terminals that lack truecolor.
    ///
    /// Call this once during setup, before any cells are written.
    /// Toggling mid-frame affects only subsequent writes, so cells
    /// already in the buffer keep whatever colors they were stored
    /// with. Call [`reset`](Self::reset) after toggling to start from
    /// a clean slate.
    pub fn set_rgb_supported(&mut self, supported: bool) {
        self.rgb_supported = supported;
        self.current.quantize_rgb = !supported;
        self.previous.quantize_rgb = !supported;
    }

    /// Clears both buffers and increments the epoch.
    pub fn reset(&mut self) {
        clear_cells(&mut self.current.cells);
        self.current.side.clear();
        clear_cells(&mut self.previous.cells);
        // self.previous.cells.fill(Cell::EMPTY);
        self.previous.side.clear();
        self.diffable = false;
        self.scroll = 0;
        self.scroll_region = None;
        self.epoch = self.epoch.wrapping_add(1);
        self.cursor.invalidate();
    }
    /// Resizes the buffer if dimensions have changed.
    pub fn resize(&mut self, width: u16, height: u16) {
        if self.current.width != width || self.current.height != height {
            let quantize = !self.rgb_supported;
            self.current = Buffer::new(width, height);
            self.previous = Buffer::new(width, height);
            self.current.quantize_rgb = quantize;
            self.previous.quantize_rgb = quantize;
            self.diffable = false;
            self.scroll = 0;
            self.scroll_region = None;
            self.epoch = self.epoch.wrapping_add(1);
            self.cursor.invalidate();
        }
    }
    /// Returns the size of the last rendered output in bytes.
    pub fn last_write_size(&self) -> usize {
        self.buf.len()
    }

    /// Returns the rendered output buffer.
    pub fn write_buffer(&self) -> &[u8] {
        &self.buf
    }

    /// Queues a scroll operation for the next render.
    ///
    /// Positive values scroll up, negative values scroll down.
    pub fn scroll(&mut self, amount: i16) {
        self.scroll += amount;
        self.scroll_region = None;
    }

    /// Queues a vertical scroll operation scoped to rows `top..bottom`.
    ///
    /// Positive values scroll up, negative values scroll down.
    ///
    /// This uses the terminal scroll-region escape on the next render, so
    /// only the requested rows are shifted while content outside the region
    /// stays visually stable.
    pub fn scroll_region(&mut self, top: u16, bottom: u16, amount: i16) {
        if amount == 0 {
            return;
        }
        let top = top.min(self.current.height);
        let bottom = bottom.min(self.current.height);
        if bottom <= top {
            return;
        }
        if top == 0 && bottom == self.current.height {
            self.scroll(amount);
            return;
        }
        match self.scroll_region.as_mut() {
            Some(region) if region.top == top && region.bottom == bottom => {
                region.amount += amount;
                if region.amount == 0 {
                    self.scroll_region = None;
                }
            }
            _ => {
                self.scroll = 0;
                self.scroll_region = Some(QueuedScrollRegion {
                    top,
                    bottom,
                    amount,
                });
            }
        }
    }

    /// Renders the current buffer to the internal byte buffer.
    ///
    /// Use [`write_buffer`](Self::write_buffer) to access the output.
    pub fn render_internal(&mut self) {
        if self.diffable {
            self.apply_scroll_optimization();
            self.current.render_diff(
                &mut self.buf,
                &self.previous,
                self.x_offset,
                self.y_offset,
                self.blanking && !self.bounded,
                false,
                &self.palette,
            );
        } else {
            self.scroll = 0;
            self.current.render(
                &mut self.buf,
                self.x_offset,
                self.y_offset,
                self.bounded,
                false,
                &self.palette,
            );
            self.diffable = true;
        }
        std::mem::swap(&mut self.current, &mut self.previous);
        // self.current.cells.fill(Cell::EMPTY);
        clear_cells(&mut self.current.cells);
        self.current.side.clear();
        self.emit_cursor();
    }

    fn apply_scroll_optimization(&mut self) {
        if let Some(region) = self.scroll_region.take() {
            self.scroll = 0;
            self.apply_scroll_region(region);
            return;
        }

        if self.y_offset == 0 {
            if self.scroll < 0 {
                vt::CLEAR_STYLE.write_to_buffer(&mut self.buf);
                ScrollBufferDown(-self.scroll as u16).write_to_buffer(&mut self.buf);
                self.previous.scroll_down(-self.scroll as u16);
            } else if self.scroll > 0 {
                vt::CLEAR_STYLE.write_to_buffer(&mut self.buf);
                ScrollBufferUp(self.scroll as u16).write_to_buffer(&mut self.buf);
                self.previous.scroll_up(self.scroll as u16);
            }
        }
        self.scroll = 0;
    }

    fn apply_scroll_region(&mut self, region: QueuedScrollRegion) {
        let height = region.bottom - region.top;
        let shift = region.amount.unsigned_abs();
        if height == 0 || shift == 0 || shift >= height {
            return;
        }

        let absolute_top = self.y_offset.saturating_add(region.top);
        let absolute_bottom = self.y_offset.saturating_add(region.bottom);
        vt::CLEAR_STYLE.write_to_buffer(&mut self.buf);
        ScrollRegion(absolute_top + 1, absolute_bottom).write_to_buffer(&mut self.buf);
        if region.amount < 0 {
            ScrollBufferDown(shift).write_to_buffer(&mut self.buf);
        } else {
            ScrollBufferUp(shift).write_to_buffer(&mut self.buf);
        }
        ScrollRegion::RESET.write_to_buffer(&mut self.buf);
        blank_scrolled_rows(&mut self.previous, region.top, region.bottom, region.amount);
    }

    fn emit_cursor(&mut self) {
        match self.cursor.mode {
            CursorMode::Untouched => {}
            CursorMode::Visible(req) => {
                if self.cursor.last_emitted_shape != Some(req.shape) {
                    SetCursorStyle(req.shape).write_to_buffer(&mut self.buf);
                    self.cursor.last_emitted_shape = Some(req.shape);
                }
                MoveCursor(req.x, req.y).write_to_buffer(&mut self.buf);
                if self.cursor.last_emitted_visible != Some(true) {
                    self.buf.extend_from_slice(vt::SHOW_CURSOR);
                    self.cursor.last_emitted_visible = Some(true);
                }
            }
            CursorMode::Hidden => {
                if self.cursor.last_emitted_visible != Some(false) {
                    self.buf.extend_from_slice(vt::HIDE_CURSOR);
                    self.cursor.last_emitted_visible = Some(false);
                }
            }
        }
    }
    /// Renders and writes the output to the terminal.
    pub fn render(&mut self, term: &mut Terminal) -> usize {
        self.render_internal();
        if self.buf == b"\x1b[0m" {
            self.buf.clear();
            return 0;
        }
        term.write_all(&self.buf).unwrap();
        let size = self.buf.len();
        self.buf.clear();
        size
    }

    /// Renders the current buffer inline using only relative cursor moves
    /// and writes it to the terminal. The buffer's height controls how many
    /// rows are painted; resize the buffer between frames to grow or shrink
    /// the region.
    ///
    /// `prev_height` must equal the value returned by the previous call,
    /// or `0` for the first call. The cursor is rewound that many rows,
    /// the current frame is painted, and the cursor is left at column 0
    /// of the row immediately below the painted region.
    ///
    /// Cursor invariant: at call time the cursor must sit at column 0 of
    /// the row immediately below the previous inline region (or at its
    /// initial position for the first call). The relative-move encoding
    /// survives terminal scrolling, so painted rows that scroll off the
    /// top end up in normal terminal scrollback.
    ///
    /// After the final inline frame, write any further output directly to
    /// the terminal — it lands below the painted region (or in scrollback
    /// if the region itself scrolled).
    ///
    /// Mixing this with [`render`](Self::render) on the same buffer
    /// corrupts diff state. Call [`reset`](Self::reset) before switching
    /// modes.
    ///
    /// # Errors
    ///
    /// Propagates any I/O error from writing to the terminal.
    pub fn render_inline(&mut self, term: &mut Terminal, prev_height: u16) -> std::io::Result<u16> {
        if prev_height > 0 {
            MoveCursorUp(prev_height).write_to_buffer(&mut self.buf);
        }
        self.buf.push(b'\r');

        self.scroll = 0;
        self.scroll_region = None;
        if self.diffable {
            self.current.render_diff(
                &mut self.buf,
                &self.previous,
                0,
                0,
                false,
                true,
                &self.palette,
            );
        } else {
            self.current
                .render(&mut self.buf, 0, 0, false, true, &self.palette);
            self.diffable = true;
        }

        let painted = self.current.height;

        std::mem::swap(&mut self.current, &mut self.previous);
        clear_cells(&mut self.current.cells);
        self.current.side.clear();
        self.cursor.invalidate();

        term.write_all(&self.buf)?;
        self.buf.clear();
        Ok(painted)
    }

    /// Sets a palette entry containing raw VT escape bytes at the given index.
    ///
    /// Gaps below `index` are filled with empty entries. Use [`Style::palette`]
    /// to create a style referencing this index.
    pub fn set_palette(&mut self, index: u8, entry: impl Into<Vec<u8>>) {
        let index = index as usize;
        if index >= self.palette.len() {
            self.palette.resize(index + 1, Vec::new());
        }
        self.palette[index] = entry.into();
    }

    /// Returns the palette entries.
    pub fn palette(&self) -> &[Vec<u8>] {
        &self.palette
    }

    /// Returns a mutable reference to the current buffer.
    pub fn current(&mut self) -> &mut Buffer {
        &mut self.current
    }
}

fn blank_scrolled_rows(buf: &mut Buffer, top: u16, bottom: u16, amount: i16) {
    let width = buf.width as usize;
    buf.scroll_region(top, bottom, 0, buf.width, amount as i32);

    let shift = amount.unsigned_abs().min(bottom - top);
    if shift == 0 {
        return;
    }

    let (blank_top, blank_bottom) = if amount > 0 {
        (bottom - shift, bottom)
    } else {
        (top, top + shift)
    };

    for y in blank_top as usize..blank_bottom as usize {
        let start = y * width;
        let end = start + width;
        buf.cells[start..end].fill(Cell::EMPTY);
    }
}

/// Style of border characters for blocks.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BorderType {
    /// ASCII characters (`+`, `-`, `|`).
    Ascii,
    /// Thin Unicode box-drawing characters.
    Thin,
}

impl std::default::Default for Block<'_> {
    fn default() -> Self {
        Self {
            title: Default::default(),
            title_alignment: Alignment::Center,
            borders: Default::default(),
            border_style: Default::default(),
            border_type: BorderType::Thin,
            style: Default::default(),
        }
    }
}

impl Block<'_> {
    /// Left border flag.
    pub const LEFT: u8 = 0b0001;
    /// Right border flag.
    pub const RIGHT: u8 = 0b0010;
    /// Top border flag.
    pub const TOP: u8 = 0b0100;
    /// Bottom border flag.
    pub const BOTTOM: u8 = 0b1000;
    /// All borders flag.
    pub const ALL: u8 = Self::LEFT | Self::RIGHT | Self::TOP | Self::BOTTOM;
}

/// A bordered container widget with optional title.
///
/// Renders a box border around a region with configurable borders, title,
/// and styles.
///
/// # Examples
///
/// ```
/// use extui::{Block, BorderType, Alignment};
///
/// let block = Block {
///     title: Some("My Title"),
///     borders: Block::ALL,
///     ..Default::default()
/// };
/// ```
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Block<'a> {
    /// Optional title displayed on the top border.
    pub title: Option<&'a str>,
    /// Alignment of the title within the top border.
    pub title_alignment: Alignment,
    /// Bitmask of visible borders (use [`LEFT`](Self::LEFT), [`RIGHT`](Self::RIGHT), etc.).
    pub borders: u8,
    /// Style applied to border characters.
    pub border_style: Style,
    /// Type of border characters to use.
    pub border_type: BorderType,
    /// Style applied to the block background.
    pub style: Style,
}

impl Block<'_> {
    /// Renders the block within the given rectangle.
    pub fn render(&self, rect: Rect, buf: &mut Buffer) {
        let Rect { x, y, w, h } = rect;
        if w == 0 || h == 0 {
            return;
        }
        let box_style = match self.border_type {
            BorderType::Ascii => &BoxStyle::ASCII,
            BorderType::Thin => &BoxStyle::LIGHT,
        };
        if self.borders & Self::TOP != 0
            && let Some(row) = buf.row_remaining_mut(x, y)
        {
            if w >= 1 {
                row[0] = box_style.top_left;
            }
            for cell in row
                .iter_mut()
                .take(w as usize)
                .skip(1)
                .take(w.saturating_sub(2) as usize)
            {
                *cell = box_style.horizontal;
            }
            if w >= 2 {
                row[w as usize - 1] = box_style.top_right;
            }
        }
        if self.borders & Self::BOTTOM != 0
            && let Some(row) = buf.row_remaining_mut(x, y + h - 1)
        {
            if w >= 1 {
                row[0] = box_style.bottom_left;
            }
            for cell in row
                .iter_mut()
                .take(w as usize)
                .skip(1)
                .take(w.saturating_sub(2) as usize)
            {
                *cell = box_style.horizontal;
            }
            if w >= 2 {
                row[w as usize - 1] = box_style.bottom_right;
            }
        }
        for y in y + (self.borders & Self::TOP != 0) as u16
            ..y + h - (self.borders & Self::BOTTOM != 0) as u16
        {
            if self.borders & Self::LEFT != 0
                && let Some(row) = buf.row_remaining_mut(x, y)
            {
                row[0] = box_style.vertical;
            }
            if self.borders & Self::RIGHT != 0
                && let Some(row) = buf.row_remaining_mut(x + w - 1, y)
            {
                row[0] = box_style.vertical;
            }
        }

        buf.set_style(rect, self.style);
        if let Some(title) = self.title {
            let title_len = title.width() as u16;
            if title_len + 2 < w {
                let title_x = match self.title_alignment {
                    Alignment::Left => x + 1,
                    Alignment::Center => x + (w - title_len) / 2,
                    Alignment::Right => x + w - title_len - 1,
                };
                buf.set_stringn(title_x, y, title, (w - 2) as usize, self.border_style);
                if title_x > x + 1 {
                    buf.set_stringn(title_x - 1, y, " ", 1, self.border_style);
                }
                if title_x + title_len < x + w - 1 {
                    buf.set_stringn(title_x + title_len, y, " ", 1, self.border_style);
                }
            }
        }
    }
    pub fn inner(&self, rect: Rect) -> Rect {
        let mut x = rect.x;
        let mut y = rect.y;
        let mut w = rect.w;
        let mut h = rect.h;
        if self.borders & Self::LEFT != 0 {
            x = x.saturating_add(1);
            w = w.saturating_sub(1);
        }
        if self.borders & Self::RIGHT != 0 {
            w = w.saturating_sub(1);
        }
        if self.borders & Self::TOP != 0 {
            y = y.saturating_add(1);
            h = h.saturating_sub(1);
        }
        if self.borders & Self::BOTTOM != 0 {
            h = h.saturating_sub(1);
        }
        Rect { x, y, w, h }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn style_encoding() {
        for fg in 0..=255u8 {
            let fg = AnsiColor(fg);
            let colored = Style::DEFAULT.with_fg(fg);
            assert_eq!(colored.fg().unwrap(), Color::Ansi(fg));
            for bg in 0..=255u8 {
                let bg = AnsiColor(bg);
                let colored = colored.with_bg(bg);
                assert_eq!(colored.bg().unwrap(), Color::Ansi(bg));
                assert_eq!(colored.fg().unwrap(), Color::Ansi(fg));
            }
        }
    }

    #[test]
    fn cell_size_is_16_bytes() {
        assert_eq!(std::mem::size_of::<Cell>(), 16);
        assert_eq!(std::mem::align_of::<Cell>(), 16);
    }

    #[test]
    fn cursor_untouched_emits_no_cursor_bytes() {
        let mut db = DoubleBuffer::new(4, 1);
        db.set_string(0, 0, "hi", Style::DEFAULT);
        db.render_internal();
        let out = std::str::from_utf8(&db.buf).unwrap();
        assert!(!out.contains("\x1b[?25"), "unexpected show/hide: {out:?}");
        assert!(!out.contains(" q"), "unexpected DECSCUSR: {out:?}");
    }

    #[test]
    fn cursor_set_emits_shape_move_show_then_diffs() {
        let mut db = DoubleBuffer::new(8, 2);
        db.set_string(0, 0, "hi", Style::DEFAULT);
        db.set_cursor(3, 1, CursorShape::SteadyBar);
        db.render_internal();
        let first = std::str::from_utf8(&db.buf).unwrap().to_owned();
        assert!(
            first.contains("\x1b[6 q"),
            "expected DECSCUSR bar: {first:?}"
        );
        assert!(
            first.contains("\x1b[2;4H"),
            "expected MoveCursor: {first:?}"
        );
        assert!(first.contains("\x1b[?25h"), "expected show: {first:?}");

        db.buf.clear();
        db.set_string(0, 0, "hi", Style::DEFAULT);
        db.set_cursor(3, 1, CursorShape::SteadyBar);
        db.render_internal();
        let second = std::str::from_utf8(&db.buf).unwrap().to_owned();
        assert!(
            second.contains("\x1b[2;4H"),
            "expected MoveCursor on frame 2: {second:?}"
        );
        assert!(
            !second.contains(" q"),
            "DECSCUSR should not re-emit: {second:?}"
        );
        assert!(
            !second.contains("\x1b[?25h"),
            "show should not re-emit: {second:?}"
        );
    }

    #[test]
    fn cursor_shape_change_re_emits_decscusr() {
        let mut db = DoubleBuffer::new(4, 1);
        db.set_cursor(0, 0, CursorShape::SteadyBlock);
        db.render_internal();
        db.buf.clear();

        db.set_cursor(0, 0, CursorShape::SteadyBar);
        db.render_internal();
        let out = std::str::from_utf8(&db.buf).unwrap();
        assert!(out.contains("\x1b[6 q"), "expected new DECSCUSR: {out:?}");
    }

    #[test]
    fn cursor_hide_after_show_emits_hide_once() {
        let mut db = DoubleBuffer::new(4, 1);
        db.set_cursor(0, 0, CursorShape::SteadyBlock);
        db.render_internal();
        db.buf.clear();

        db.hide_cursor();
        db.render_internal();
        let first = std::str::from_utf8(&db.buf).unwrap().to_owned();
        assert!(first.contains("\x1b[?25l"), "expected hide: {first:?}");
        db.buf.clear();

        db.hide_cursor();
        db.render_internal();
        let second = std::str::from_utf8(&db.buf).unwrap().to_owned();
        assert!(
            !second.contains("\x1b[?25l"),
            "hide should not re-emit: {second:?}"
        );
    }

    #[test]
    fn cursor_reset_re_emits_next_frame() {
        let mut db = DoubleBuffer::new(4, 1);
        db.set_cursor(0, 0, CursorShape::SteadyBlock);
        db.render_internal();
        db.buf.clear();

        db.reset();
        db.set_cursor(0, 0, CursorShape::SteadyBlock);
        db.render_internal();
        let out = std::str::from_utf8(&db.buf).unwrap();
        assert!(
            out.contains("\x1b[2 q"),
            "expected DECSCUSR after reset: {out:?}"
        );
        assert!(
            out.contains("\x1b[?25h"),
            "expected show after reset: {out:?}"
        );
    }

    #[test]
    fn inline_render_emits_no_absolute_cursor_positioning() {
        let mut db = DoubleBuffer::new(8, 3);
        db.set_string(0, 0, "row0", Style::DEFAULT);
        db.set_string(2, 1, "row1", Style::DEFAULT);
        db.set_string(0, 2, "row2", Style::DEFAULT);

        let mut buf = Vec::new();
        db.current.render(&mut buf, 0, 0, false, true, &[]);
        let s = std::str::from_utf8(&buf).unwrap();
        assert!(
            !s.contains("\x1b[H"),
            "inline render must not emit MOVE_CURSOR_TO_ORIGIN: {s:?}"
        );
        assert!(
            !s.contains(";1H") && !s.contains(";2H") && !s.contains(";3H"),
            "inline render must not emit absolute MoveCursor: {s:?}"
        );
        assert!(s.contains("row0"));
        assert!(s.contains("row1"));
        assert!(s.contains("row2"));
        assert!(s.contains("\r\n"), "expected row separator: {s:?}");
    }

    #[test]
    fn inline_render_advances_cursor_past_blank_trailing_rows() {
        let mut db = DoubleBuffer::new(4, 4);
        db.set_string(0, 0, "hi", Style::DEFAULT);

        let mut buf = Vec::new();
        db.current.render(&mut buf, 0, 0, false, true, &[]);
        let nl_count = buf.iter().filter(|&&b| b == b'\n').count();
        assert_eq!(
            nl_count, 4,
            "inline render must end cursor below the region: {buf:?}"
        );
    }

    #[test]
    fn inline_render_diff_skips_unchanged_rows() {
        let mut db = DoubleBuffer::new(8, 3);
        db.set_string(0, 0, "row0", Style::DEFAULT);
        db.set_string(0, 1, "row1", Style::DEFAULT);
        db.set_string(0, 2, "row2", Style::DEFAULT);
        let mut buf = Vec::new();
        db.current.render(&mut buf, 0, 0, false, true, &[]);
        std::mem::swap(&mut db.current, &mut db.previous);
        clear_cells(&mut db.current.cells);
        db.current.side.clear();

        db.set_string(0, 0, "row0", Style::DEFAULT);
        db.set_string(0, 1, "row1", Style::DEFAULT);
        db.set_string(0, 2, "row2", Style::DEFAULT);
        let mut diff_buf = Vec::new();
        db.current
            .render_diff(&mut diff_buf, &db.previous, 0, 0, false, true, &[]);
        let s = std::str::from_utf8(&diff_buf).unwrap();
        assert!(
            !s.contains("\x1b[H") && !s.contains(";1H") && !s.contains(";2H"),
            "inline diff must not emit absolute MoveCursor: {s:?}"
        );
        assert!(
            !s.contains("row0") && !s.contains("row1") && !s.contains("row2"),
            "inline diff must skip unchanged content: {s:?}"
        );
    }

    #[test]
    fn inline_render_diff_emits_only_changed_cell() {
        let mut db = DoubleBuffer::new(8, 3);
        db.set_string(0, 0, "row0", Style::DEFAULT);
        db.set_string(0, 1, "row1", Style::DEFAULT);
        db.set_string(0, 2, "row2", Style::DEFAULT);
        let mut buf = Vec::new();
        db.current.render(&mut buf, 0, 0, false, true, &[]);
        std::mem::swap(&mut db.current, &mut db.previous);
        clear_cells(&mut db.current.cells);
        db.current.side.clear();

        db.set_string(0, 0, "row0", Style::DEFAULT);
        db.set_string(0, 1, "ROWX", Style::DEFAULT);
        db.set_string(0, 2, "row2", Style::DEFAULT);
        let mut diff_buf = Vec::new();
        db.current
            .render_diff(&mut diff_buf, &db.previous, 0, 0, false, true, &[]);
        let s = std::str::from_utf8(&diff_buf).unwrap();
        assert!(s.contains("ROWX"), "expected new content: {s:?}");
        assert!(
            !s.contains("row0"),
            "row 0 unchanged, must not appear: {s:?}"
        );
        assert!(
            !s.contains("row2"),
            "row 2 unchanged, must not appear: {s:?}"
        );
        assert!(
            !s.contains("\x1b[H") && !s.contains(";1H") && !s.contains(";2H") && !s.contains(";3H"),
            "inline diff must not emit absolute MoveCursor: {s:?}"
        );
    }

    #[test]
    fn rgb_to_ansi_snaps_to_cube_and_ramp() {
        // Exact corners of the 6x6x6 color cube.
        assert_eq!(Rgb(0x00, 0x00, 0x00).to_ansi(), AnsiColor(16));
        assert_eq!(Rgb(0xff, 0xff, 0xff).to_ansi(), AnsiColor(231));
        // Mid cube slot.
        assert_eq!(Rgb(0x5f, 0x87, 0xaf).to_ansi(), AnsiColor(16 + 36 + 12 + 3));
        // Grayscale ramp snaps to the 24-step ramp entries.
        assert_eq!(Rgb(0x1c, 0x1c, 0x1c).to_ansi(), AnsiColor(234));
        assert_eq!(Rgb(0xc6, 0xc6, 0xc6).to_ansi(), AnsiColor(251));
    }

    #[test]
    fn style_quantize_rgb_preserves_non_rgb() {
        let s = Style::DEFAULT
            .with_fg(AnsiColor::Red1)
            .with_bg(AnsiColor::Blue1)
            .with_modifier(Modifier::BOLD);
        assert_eq!(s.quantize_rgb(), s);

        let palette = Style::palette(3).with_fg(AnsiColor::Red1);
        assert_eq!(palette.quantize_rgb(), palette);
    }

    #[test]
    fn style_quantize_rgb_downgrades_rgb_colors() {
        let s = Style::DEFAULT
            .with_fg(Color::rgb(0x5f, 0x87, 0xaf))
            .with_bg(Color::rgb(0xff, 0xff, 0xff))
            .with_modifier(Modifier::BOLD);
        let q = s.quantize_rgb();
        assert_eq!(q.fg(), Some(Color::Ansi(AnsiColor(16 + 36 + 12 + 3))));
        assert_eq!(q.bg(), Some(Color::Ansi(AnsiColor(231))));
        assert!(q.modifiers().has(Modifier::BOLD));
    }

    #[test]
    fn double_buffer_without_rgb_support_emits_palette_escape() {
        // With RGB disabled, an RGB cell should quantize to 256-color SGR.
        let mut db = DoubleBuffer::new(4, 1);
        db.set_rgb_supported(false);
        let style = Style::DEFAULT.with_fg(Color::rgb(0x5f, 0x87, 0xaf));
        db.set_string(0, 0, "x", style);
        db.render_internal();
        let bytes = std::str::from_utf8(&db.buf).unwrap();
        let expected_idx = (16 + 36 + 12 + 3).to_string();
        assert!(
            bytes.contains(&format!("38;5;{expected_idx}")),
            "expected 256-color SGR, got: {bytes:?}",
        );
        assert!(
            !bytes.contains("38;2;"),
            "must not emit truecolor SGR when rgb is disabled: {bytes:?}",
        );
    }

    #[test]
    fn double_buffer_with_rgb_support_emits_truecolor_escape() {
        let mut db = DoubleBuffer::new(4, 1);
        assert!(db.rgb_supported());
        let style = Style::DEFAULT.with_fg(Color::rgb(0x5f, 0x87, 0xaf));
        db.set_string(0, 0, "x", style);
        db.render_internal();
        let bytes = std::str::from_utf8(&db.buf).unwrap();
        assert!(
            bytes.contains("38;2;95;135;175"),
            "expected truecolor SGR, got: {bytes:?}",
        );
    }

    #[test]
    fn rgb_quantized_via_buffer_set_style_bypass() {
        // `DisplayRect::fill` writes through `buf.current.set_style(...)`,
        // bypassing the `DoubleBuffer::set_style` wrapper. The flag lives
        // on `Buffer` itself, so this path must still quantize.
        let mut db = DoubleBuffer::new(4, 1);
        db.set_rgb_supported(false);
        let style = Style::DEFAULT.with_bg(Color::rgb(0x5f, 0x87, 0xaf));
        db.set_string(0, 0, "xxxx", Style::DEFAULT);
        let rect = db.rect();
        db.current().set_style(rect, style);
        db.render_internal();
        let bytes = std::str::from_utf8(&db.buf).unwrap();
        let expected_idx = (16 + 36 + 12 + 3).to_string();
        assert!(
            bytes.contains(&format!("48;5;{expected_idx}")),
            "expected quantized bg, got: {bytes:?}",
        );
        assert!(
            !bytes.contains("48;2;"),
            "must not emit truecolor bg: {bytes:?}",
        );
    }

    #[test]
    fn style_rgb_roundtrip() {
        let s = Style::DEFAULT.with_fg(Color::rgb(12, 34, 56));
        assert_eq!(s.fg(), Some(Color::Rgb(Rgb(12, 34, 56))));
        assert_eq!(s.bg(), None);

        let s = s.with_bg(Color::rgb(200, 100, 50));
        assert_eq!(s.fg(), Some(Color::Rgb(Rgb(12, 34, 56))));
        assert_eq!(s.bg(), Some(Color::Rgb(Rgb(200, 100, 50))));

        // Mixing RGB fg with palette bg
        let s = Style::DEFAULT
            .with_fg(Color::rgb(1, 2, 3))
            .with_bg(AnsiColor::Red1);
        assert_eq!(s.fg(), Some(Color::Rgb(Rgb(1, 2, 3))));
        assert_eq!(s.bg(), Some(Color::Ansi(AnsiColor::Red1)));
    }

    #[test]
    fn cell_seven_byte_grapheme_roundtrip() {
        let seven = "éabcde"; // 2 + 5 = 7 bytes.
        assert_eq!(seven.len(), 7);
        let cell = Cell::new(seven, Style::DEFAULT);
        assert!(!cell.is_handle());
        assert_eq!(cell.text_inline(), Some(seven));

        let four = "🎉"; // 4 bytes.
        assert_eq!(four.len(), 4);
        let cell = Cell::new(four, Style::DEFAULT);
        assert!(!cell.is_handle());
        assert_eq!(cell.text_inline(), Some(four));
    }

    #[test]
    fn long_grapheme_via_side_buffer() {
        // 🇨🇦 (Canadian flag) is a 2-codepoint ZWJ-like sequence = 8 bytes.
        let flag = "🇨🇦";
        assert_eq!(flag.len(), 8);

        let mut buffer = Buffer::new(4, 1);
        buffer.set_string(0, 0, flag, Style::DEFAULT);

        // The flag should land in cell 0 as a handle cell.
        let cell = buffer.cells[0];
        assert!(cell.is_handle(), "flag emoji should become a handle cell");
        assert_eq!(cell.text_inline(), None);
        assert_eq!(cell.text(&buffer.side), flag.as_bytes());

        // A family ZWJ sequence: 18 bytes.
        let family = "👨‍👩‍👧";
        let mut buffer = Buffer::new(4, 1);
        buffer.set_string(0, 0, family, Style::DEFAULT);
        let cell = buffer.cells[0];
        assert!(cell.is_handle());
        assert_eq!(cell.text(&buffer.side), family.as_bytes());
    }

    #[test]
    fn compact_side_drops_orphaned_bytes() {
        let flag = "🇨🇦"; // 8 bytes → handle cell.
        let family = "👨‍👩‍👧"; // 18 bytes → handle cell.
        let mut buffer = Buffer::new(4, 1);

        // Write flag, then overwrite the same cell with family. The
        // flag's 8 bytes are now dead weight in the side arena.
        buffer.set_string(0, 0, flag, Style::DEFAULT);
        buffer.set_string(0, 0, family, Style::DEFAULT);
        // Write another throwaway into cell 1 and then overwrite it
        // with a plain inline char so *nothing* references it.
        buffer.set_string(1, 0, flag, Style::DEFAULT);
        buffer.set_string(1, 0, "x", Style::DEFAULT);

        let before = buffer.side_len();
        assert_eq!(before, flag.len() * 2 + family.len());

        buffer.compact_side();

        // Only the family bytes survive.
        let after = buffer.side_len();
        assert_eq!(after, family.len());

        // The surviving handle cell still resolves correctly.
        let cell = buffer.cells[0];
        assert!(cell.is_handle());
        assert_eq!(buffer.handle_text(cell).unwrap(), family.as_bytes());

        // Cell 1 is untouched (still the inline 'x').
        assert_eq!(buffer.cells[1].text_inline(), Some("x"));
    }

    #[test]
    fn compact_side_is_noop_on_empty_arena() {
        let mut buffer = Buffer::new(4, 1);
        buffer.set_string(0, 0, "abcd", Style::DEFAULT);
        assert_eq!(buffer.side_len(), 0);
        buffer.compact_side();
        assert_eq!(buffer.side_len(), 0);
        assert_eq!(buffer.cells[0].text_inline(), Some("a"));
    }

    #[test]
    fn ascii_set_stringn_fast_path_respects_width() {
        let mut buffer = Buffer::new(5, 1);
        let end = buffer.set_stringn(1, 0, "hello", 3, Style::DEFAULT);

        assert_eq!(end, (4, 0));
        assert_eq!(buffer.side_len(), 0);
        assert_eq!(buffer.cells[0].text_inline(), Some(""));
        assert_eq!(buffer.cells[1].text_inline(), Some("h"));
        assert_eq!(buffer.cells[2].text_inline(), Some("e"));
        assert_eq!(buffer.cells[3].text_inline(), Some("l"));
        assert_eq!(buffer.cells[4].text_inline(), Some(""));
    }

    #[test]
    fn ascii_set_stringn_skips_control_bytes() {
        let mut buffer = Buffer::new(5, 1);
        buffer.set_string(0, 0, ".....", Style::DEFAULT);

        let end = buffer.set_stringn(1, 0, "A\n\tB\u{7f}C", 2, Style::DEFAULT);

        assert_eq!(end, (3, 0));
        assert_eq!(buffer.side_len(), 0);
        assert_eq!(buffer.cells[0].text_inline(), Some("."));
        assert_eq!(buffer.cells[1].text_inline(), Some("A"));
        assert_eq!(buffer.cells[2].text_inline(), Some("B"));
        assert_eq!(buffer.cells[3].text_inline(), Some("."));
        assert_eq!(buffer.cells[4].text_inline(), Some("."));
    }

    #[test]
    fn zalgo_grapheme_roundtrip() {
        // A modest zalgo cluster: base 'a' + 44 combining marks = 45
        // codepoints, 89 bytes, visual width 1. Stored as raw bytes so
        // the source file stays readable.
        #[rustfmt::skip]
        const ZALGO_BYTES: &[u8] = &[
            0x61,
            0xCC, 0xB5, 0xCD, 0x80, 0xCD, 0x98, 0xCD, 0x8A, 0xCD, 0x86, 0xCD, 0x81,
            0xCC, 0x9B, 0xCC, 0x9A, 0xCC, 0x82, 0xCC, 0x83, 0xCC, 0x84, 0xCD, 0x80,
            0xCC, 0x90, 0xCC, 0x8F, 0xCD, 0x97, 0xCC, 0xBE, 0xCC, 0x84, 0xCC, 0x92,
            0xCD, 0x9D, 0xCC, 0x94, 0xCC, 0x87, 0xCC, 0xA4, 0xCC, 0xBB, 0xCC, 0xB2,
            0xCC, 0xBC, 0xCC, 0xBC, 0xCC, 0xA3, 0xCC, 0x9F, 0xCC, 0x98, 0xCC, 0x9D,
            0xCC, 0x9C, 0xCD, 0x87, 0xCD, 0x87, 0xCC, 0x97, 0xCC, 0x9E, 0xCC, 0xB2,
            0xCC, 0x9C, 0xCC, 0xAF, 0xCC, 0xA4, 0xCC, 0xBA, 0xCC, 0xAA, 0xCC, 0x9D,
            0xCD, 0x8D, 0xCD, 0x8D,
        ];
        let zalgo = std::str::from_utf8(ZALGO_BYTES).unwrap();
        assert_eq!(zalgo.len(), 89);
        assert!(zalgo.len() > 7 && zalgo.len() <= u8::MAX as usize);

        let mut buffer = Buffer::new(4, 1);
        buffer.set_string(0, 0, zalgo, Style::DEFAULT);
        let cell = buffer.cells[0];
        assert!(cell.is_handle());
        assert_eq!(cell.text(&buffer.side), ZALGO_BYTES);
    }

    #[test]
    fn extreme_zalgo_truncates_at_char_boundary() {
        // Synthetic zalgo exceeding the 255-byte handle cap so that
        // `set_stringn` is forced down the truncation path. Base letter
        // 'a' (1 byte) plus 200 U+0301 COMBINING ACUTE ACCENT (2 bytes
        // each) = 401 bytes total in a single grapheme cluster.
        let mut extreme = String::from("a");
        for _ in 0..200 {
            extreme.push('\u{0301}');
        }
        assert!(extreme.len() > u8::MAX as usize);
        assert_eq!(
            UnicodeSegmentation::graphemes(extreme.as_str(), true).count(),
            1,
            "combining marks must collapse into one grapheme cluster",
        );

        let mut buffer = Buffer::new(4, 1);
        buffer.set_string(0, 0, &extreme, Style::DEFAULT);
        let cell = buffer.cells[0];
        assert!(cell.is_handle(), "truncated zalgo should still be a handle");

        let stored = cell.text(&buffer.side);
        assert!(
            stored.len() <= u8::MAX as usize,
            "stored length {} exceeds u8 cap",
            stored.len(),
        );
        // Must still be valid UTF-8 — truncation is at a codepoint boundary.
        let decoded = std::str::from_utf8(stored).expect("truncation landed off a UTF-8 boundary");
        // Must be a non-empty prefix of the input.
        assert!(!decoded.is_empty());
        assert!(extreme.starts_with(decoded));
    }

    #[test]
    fn long_grapheme_diff_is_idempotent() {
        // Writing the same long grapheme twice should diff to empty output
        // (after the initial paint), exercising the slow-path equality.
        let mut db = DoubleBuffer::new(4, 1);
        db.set_string(0, 0, "🇨🇦hi", Style::DEFAULT);
        db.render_internal();
        db.buf.clear();
        // Frame 2: identical content; expect only trailing cursor-return and
        // clear-style, no text output.
        db.set_string(0, 0, "🇨🇦hi", Style::DEFAULT);
        db.render_internal();
        // No grapheme bytes should appear in the diff output.
        let out = &db.buf;
        assert!(
            !out.windows(4).any(|w| w == [0xF0, 0x9F, 0x87, 0xA8]),
            "diff re-emitted the flag grapheme: {:?}",
            out
        );
    }

    pub struct BufferDiffCheck {
        db_a: DoubleBuffer,
        term_1: vt100::Parser,
        db_b: DoubleBuffer,
        term_2: vt100::Parser,
        step: u32,
    }

    impl BufferDiffCheck {
        pub fn new(width: u16, height: u16) -> BufferDiffCheck {
            BufferDiffCheck {
                db_a: DoubleBuffer::new(width, height),
                term_1: vt100::Parser::new(height, width, 0),
                db_b: DoubleBuffer::new(width, height),

                term_2: vt100::Parser::new(height, width, 0),
                step: 0,
            }
        }
        pub fn step(&mut self, fnx: impl Fn(Rect, &mut DoubleBuffer)) {
            self.step += 1;
            let rect = Rect {
                x: 0,
                y: 0,
                w: self.db_a.width(),
                h: self.db_a.height(),
            };
            fnx(rect, &mut self.db_a);
            self.db_a.render_internal();
            self.term_1.process(&self.db_a.buf);

            fnx(rect, &mut self.db_b);
            self.db_b.render_internal();
            self.term_2.process(&self.db_b.buf);

            let _ = std::fs::write(format!("/tmp/term_{}.bin", self.step), &self.db_b.buf);

            let a = self.term_2.screen().contents();
            let b = self.term_1.screen().contents();
            for (a, b) in a.lines().zip(b.lines()) {
                assert_eq!(a, b);
            }
        }
    }

    #[test]
    fn blanking_optimization() {
        let mut checker = BufferDiffCheck::new(200, 4);
        checker.db_b.blanking = false;
        checker.step(|mut rect, out| {
            for i in 0..4 {
                let (mut a, mut b) = rect.take_top(1).h_split(0.5);
                a.take_left(6)
                    .with(AnsiColor::LightGoldenrod2.with_fg(AnsiColor::Black))
                    .text(out, " Done ");

                a.with(if i == 0 {
                    AnsiColor::Blue1.with_fg(AnsiColor::Black)
                } else {
                    AnsiColor(248).as_fg()
                })
                .fill(out)
                .skip(1)
                .text(out, "die R:0 S:0");

                b.take_left(6)
                    .with(AnsiColor::Aquamarine1.with_bg(AnsiColor::Grey[4]))
                    .text(out, " Fail ");
                b.with(AnsiColor(248).as_fg())
                    .fill(out)
                    .skip(1)
                    .text(out, "cargo run -- die")
                    .skip(1)
                    .with(HAlign::Right)
                    .text(out, "100s ");
            }
        });
        checker.step(|mut rect, out| {
            for i in 0..4 {
                let (mut a, mut b) = rect.take_top(1).h_split(0.5);
                a.take_left(6)
                    .with(AnsiColor::LightGoldenrod2.with_fg(AnsiColor::Black))
                    .text(out, " Done ");

                a.with(if i == 1 {
                    AnsiColor::Blue1.with_fg(AnsiColor::Black)
                } else {
                    AnsiColor(248).as_fg()
                })
                .fill(out)
                .skip(1)
                .text(out, "die R:0 S:0");

                b.take_left(6)
                    .with(AnsiColor::BlueViolet.with_bg(AnsiColor::Grey[4]))
                    .text(out, " Done ");
                b.with(AnsiColor(248).as_fg())
                    .fill(out)
                    .skip(1)
                    .text(out, "cargo info")
                    .skip(1)
                    .with(HAlign::Right)
                    .text(out, "101s ");
            }
        });
    }

    #[test]
    fn buffer_set_style() {
        let mut buffer = Buffer::new(8, 1);
        buffer.set_string(1, 0, "hello", Style::DEFAULT);
        buffer.set_style(
            Rect {
                x: 0,
                y: 0,
                w: 5,
                h: 1,
            },
            AnsiColor(1).as_bg(),
        );
        buffer.set_style(
            Rect {
                x: 2,
                y: 0,
                w: 6,
                h: 1,
            },
            AnsiColor(2).as_fg(),
        );
        buffer.set_style(
            Rect {
                x: 3,
                y: 0,
                w: 1,
                h: 1,
            },
            Style::DEFAULT.with_modifier(Modifier::BOLD),
        );
        let expected = [
            /* 0 */ Cell::new("", AnsiColor(1).as_bg()),
            /* 1 */ Cell::new("h", AnsiColor(1).as_bg()),
            /* 2 */ Cell::new("e", AnsiColor(1).as_bg() | AnsiColor(2).as_fg()),
            /* 3 */
            Cell::new(
                "l",
                AnsiColor(1).as_bg() | AnsiColor(2).as_fg() | Modifier::BOLD,
            ),
            /* 4 */ Cell::new("l", AnsiColor(1).as_bg() | AnsiColor(2).as_fg()),
            /* 5 */ Cell::new("o", AnsiColor(2).as_fg()),
            /* 6 */ Cell::new("", AnsiColor(2).as_fg()),
            /* 7 */ Cell::new("", AnsiColor(2).as_fg()),
        ];
        for (i, (got, expected)) in buffer.cells.iter().zip(expected.iter()).enumerate() {
            assert_eq!(got.style, expected.style, "style mismatch at cell {}", i);
            assert_eq!(got.text, expected.text, "text mismatch at cell {}", i);
        }
    }

    #[test]
    fn palette_style_roundtrip() {
        for i in [0u8, 1, 42, 127, 255] {
            let s = Style::palette(i);
            assert!(s.is_palette());
            assert_eq!(s.palette_index(), i);
            assert_ne!(s, Style::DEFAULT);
        }
        assert!(!Style::DEFAULT.is_palette());
        assert!(!Style::DEFAULT.with_fg(AnsiColor::Red1).is_palette());
    }

    #[test]
    fn palette_preserves_fg_bg() {
        let s = Style::palette(5)
            .with_fg(AnsiColor::Red1)
            .with_bg(AnsiColor::Blue1);
        assert!(s.is_palette());
        assert_eq!(s.palette_index(), 5);
        assert_eq!(s.fg(), Some(Color::Ansi(AnsiColor::Red1)));
        assert_eq!(s.bg(), Some(Color::Ansi(AnsiColor::Blue1)));
    }

    #[test]
    fn palette_set_style_merges_colors() {
        let mut buffer = Buffer::new(4, 1);
        buffer.set_string(0, 0, "abcd", AnsiColor(1).as_fg() | AnsiColor(2).as_bg());
        // set_style with palette (no fg/bg) should replace modifiers but keep existing colors
        buffer.set_style(
            Rect {
                x: 1,
                y: 0,
                w: 2,
                h: 1,
            },
            Style::palette(7),
        );
        let s1 = buffer.cells[1].style();
        assert!(s1.is_palette());
        assert_eq!(s1.palette_index(), 7);
        // Existing fg/bg should be preserved through the merge
        assert_eq!(s1.fg(), Some(Color::Ansi(AnsiColor(1))));
        assert_eq!(s1.bg(), Some(Color::Ansi(AnsiColor(2))));
        // Cells outside the region should be unchanged
        assert!(!buffer.cells[0].style().is_palette());
        assert!(!buffer.cells[3].style().is_palette());
    }

    #[test]
    fn palette_render() {
        // Palette entry for curly underline
        let mut db = DoubleBuffer::new(20, 2);
        db.set_palette(0, b"\x1b[4:3m".to_vec());
        db.set_palette(1, b"\x1b[58;2;255;0;0m".to_vec());

        let underline = Style::palette(0).with_fg(AnsiColor::Red1);
        let colored_ul = Style::palette(1).with_fg(AnsiColor::Blue1);

        // Frame 1: full render path
        db.set_string(0, 0, "error", underline);
        db.set_string(6, 0, "warn", colored_ul);
        db.set_string(0, 1, "normal", AnsiColor::Red1.as_fg());
        db.render_internal();

        let output = String::from_utf8_lossy(&db.buf);
        assert!(output.contains("\x1b[4:3m"), "palette 0 missing");
        assert!(output.contains("error"), "text missing");
        assert!(output.contains("\x1b[58;2;255;0;0m"), "palette 1 missing");
        assert!(output.contains("warn"), "text missing");
        assert!(output.contains("\x1b[0m"), "style reset missing");
        db.buf.clear();

        // Frame 2: diff path - same palette, different fg
        let underline_blue = Style::palette(0).with_fg(AnsiColor::Blue1);
        db.set_string(0, 0, "error", underline_blue);
        db.set_string(6, 0, "warn", colored_ul);
        db.set_string(0, 1, "normal", AnsiColor::Red1.as_fg());
        db.render_internal();

        let output = String::from_utf8_lossy(&db.buf);
        // Same palette index - should NOT re-emit palette bytes, just the fg change
        assert!(output.contains("error"), "changed text missing in diff");
        assert!(
            !output.contains("warn"),
            "unchanged region should be skipped"
        );
        db.buf.clear();

        // Frame 3: transition from palette to normal
        db.set_string(0, 0, "plain", AnsiColor::Blue1.as_fg());
        db.set_string(6, 0, "warn", colored_ul);
        db.set_string(0, 1, "normal", AnsiColor::Red1.as_fg());
        db.render_internal();

        let output = String::from_utf8_lossy(&db.buf);
        assert!(output.contains("plain"), "normal text missing");
        db.buf.clear();
    }

    #[test]
    fn double_buffer_scroll_region_preserves_rows_outside_region() {
        let mut fast = DoubleBuffer::new(6, 4);
        let mut slow = DoubleBuffer::new(6, 4);
        let mut term_fast = vt100::Parser::new(4, 6, 0);
        let mut term_slow = vt100::Parser::new(4, 6, 0);

        let render_frame = |buf: &mut DoubleBuffer, rows: [&str; 4]| {
            for (y, row) in rows.into_iter().enumerate() {
                buf.set_string(0, y as u16, row, Style::DEFAULT);
            }
        };

        render_frame(&mut fast, ["AAAAAA", "BBBBBB", "status", "input "]);
        render_frame(&mut slow, ["AAAAAA", "BBBBBB", "status", "input "]);
        fast.render_internal();
        slow.render_internal();
        term_fast.process(&fast.buf);
        term_slow.process(&slow.buf);
        fast.buf.clear();
        slow.buf.clear();

        render_frame(&mut fast, ["BBBBBB", "CCCCCC", "status", "input "]);
        render_frame(&mut slow, ["BBBBBB", "CCCCCC", "status", "input "]);
        fast.scroll_region(0, 2, 1);
        fast.render_internal();
        slow.render_internal();

        let fast_output = String::from_utf8_lossy(&fast.buf);
        assert!(
            fast_output.contains("\x1b[1;2r"),
            "expected scoped scroll region escape, got: {fast_output:?}"
        );

        term_fast.process(&fast.buf);
        term_slow.process(&slow.buf);
        assert_eq!(
            term_fast.screen().contents(),
            term_slow.screen().contents(),
            "scoped scroll render diverged from plain diff render"
        );
    }

    /// A bounded DoubleBuffer with `x_offset > 0` must emit `MoveCursor`
    /// sequences targeting `(x_offset + col, y_offset + row)` on the
    /// terminal, not `(col, y_offset + row)`.
    #[test]
    fn bounded_buffer_uses_x_offset() {
        let mut db = DoubleBuffer::new(6, 3);
        db.x_offset = 20;
        db.y_offset = 5;
        db.bounded = true;
        db.set_string(0, 0, "hello", Style::DEFAULT);
        db.render_internal();

        let out = std::str::from_utf8(&db.buf).unwrap();
        assert!(
            out.contains("\x1b[6;21H"),
            "expected MoveCursor to (col=21,row=6) for x_offset=20, y_offset=5, got: {:?}",
            out
        );
    }

    /// A bounded DoubleBuffer must not emit `CLEAR_BELOW` during its first
    /// render — that would destroy cells under the widget managed by other
    /// code (task trees, status bars, etc.).
    #[test]
    fn bounded_buffer_skips_clear_below() {
        let mut db = DoubleBuffer::new(4, 2);
        db.x_offset = 10;
        db.y_offset = 3;
        db.bounded = true;
        db.set_string(0, 0, "AB", Style::DEFAULT);
        db.render_internal();

        let out = std::str::from_utf8(&db.buf).unwrap();
        assert!(
            !out.contains("\x1b[J"),
            "bounded render must not emit CLEAR_BELOW: {:?}",
            out
        );
        assert!(
            !out.contains("\x1b[0J"),
            "bounded render must not emit CLEAR_BELOW (0J): {:?}",
            out
        );
    }

    /// Bounded render_diff must not use the `\x1b[K` (EL/erase-to-end-of-line)
    /// optimization for runs of blank cells — that clears cells to the right
    /// of the buffer's rightmost column, destroying widgets rendered beside it.
    #[test]
    fn bounded_render_diff_does_not_emit_clear_line_optimization() {
        let mut db = DoubleBuffer::new(60, 4);
        db.x_offset = 10;
        db.y_offset = 5;
        db.bounded = true;
        let fg = Style::DEFAULT;
        let hl = AnsiColor(153).with_fg(AnsiColor::Black);

        db.set_string(0, 0, "initial", fg);
        db.render_internal();
        db.buf.clear();

        db.set_style(
            Rect {
                x: 0,
                y: 1,
                w: 60,
                h: 1,
            },
            hl,
        );
        db.render_internal();

        let out = std::str::from_utf8(&db.buf).unwrap();
        assert!(
            !out.contains("\x1b[K") && !out.contains("\x1b[0K"),
            "bounded render_diff must never emit CLEAR_LINE_TO_RIGHT (EL), got: {:?}",
            out
        );
    }

    /// A bounded DoubleBuffer must not destroy cells outside its own column
    /// range even when rendering a full-width styled row.
    #[test]
    fn bounded_buffer_preserves_cells_beside_it() {
        let mut term = vt100::Parser::new(8, 30, 0);

        let mut setup = Vec::new();
        MoveCursor(0, 2).write_to_buffer(&mut setup);
        setup.extend_from_slice(b"LEFT_A");
        MoveCursor(20, 2).write_to_buffer(&mut setup);
        setup.extend_from_slice(b"RIGHT_A");
        MoveCursor(0, 3).write_to_buffer(&mut setup);
        setup.extend_from_slice(b"LEFT_B");
        MoveCursor(20, 3).write_to_buffer(&mut setup);
        setup.extend_from_slice(b"RIGHT_B");
        term.process(&setup);

        let mut db = DoubleBuffer::new(10, 2);
        db.x_offset = 8;
        db.y_offset = 2;
        db.bounded = true;
        let hl = AnsiColor(153).with_fg(AnsiColor::Black);
        db.set_style(
            Rect {
                x: 0,
                y: 0,
                w: 10,
                h: 2,
            },
            hl,
        );
        db.set_string(1, 0, "palette", Style::DEFAULT);
        db.render_internal();
        term.process(&db.buf);
        db.buf.clear();

        let row2 = term.screen().rows(0, 30).nth(2).unwrap();
        let row3 = term.screen().rows(0, 30).nth(3).unwrap();
        assert!(row2.contains("LEFT_A"), "row 2 lost LEFT_A, got {:?}", row2);
        assert!(
            row2.contains("RIGHT_A"),
            "row 2 lost RIGHT_A, got {:?}",
            row2
        );
        assert!(row3.contains("LEFT_B"), "row 3 lost LEFT_B, got {:?}", row3);
        assert!(
            row3.contains("RIGHT_B"),
            "row 3 lost RIGHT_B, got {:?}",
            row3
        );
    }

    /// A bounded DoubleBuffer with `y_offset > 0` must not destroy cells
    /// BELOW its rendering area (e.g. task tree pane).
    #[test]
    fn bounded_buffer_preserves_cells_below() {
        let mut term = vt100::Parser::new(12, 20, 0);

        let mut setup = Vec::new();
        MoveCursor(0, 8).write_to_buffer(&mut setup);
        setup.extend_from_slice(b"STATUS_BAR");
        MoveCursor(0, 9).write_to_buffer(&mut setup);
        setup.extend_from_slice(b"TASK_TREE_A");
        MoveCursor(0, 10).write_to_buffer(&mut setup);
        setup.extend_from_slice(b"TASK_TREE_B");
        term.process(&setup);

        let mut db = DoubleBuffer::new(12, 3);
        db.x_offset = 4;
        db.y_offset = 3;
        db.bounded = true;
        db.set_string(1, 0, "palette", Style::DEFAULT);
        db.set_string(1, 1, "item 1", Style::DEFAULT);
        db.set_string(1, 2, "item 2", Style::DEFAULT);
        db.render_internal();
        term.process(&db.buf);

        let row8 = term.screen().rows(0, 20).nth(8).unwrap();
        let row9 = term.screen().rows(0, 20).nth(9).unwrap();
        let row10 = term.screen().rows(0, 20).nth(10).unwrap();
        assert!(
            row8.contains("STATUS_BAR"),
            "row 8 destroyed, got {:?}",
            row8
        );
        assert!(
            row9.contains("TASK_TREE_A"),
            "row 9 destroyed, got {:?}",
            row9
        );
        assert!(
            row10.contains("TASK_TREE_B"),
            "row 10 destroyed, got {:?}",
            row10
        );
    }

    /// A non-bounded DoubleBuffer at x_offset=0 must still behave exactly
    /// like it did before the x_offset/bounded fields were introduced.
    #[test]
    fn non_bounded_buffer_behavior_unchanged() {
        let mut db = DoubleBuffer::new(4, 2);
        db.set_string(0, 0, "test", Style::DEFAULT);
        db.render_internal();
        let out = std::str::from_utf8(&db.buf).unwrap();
        assert!(
            out.contains("\x1b[J") || out.contains("\x1b[0J"),
            "non-bounded must emit CLEAR_BELOW: {:?}",
            out
        );
    }
}
