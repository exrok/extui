//! An opinionated, minimal terminal UI crate.
//!
//! # Rect based rendering and layout
//!
//! [Rect] is the core abstraction in extui, use builder style methods to
//! paint to the screen using an [DoubleBuffer] for rendering to screen
//! efficiently.
//!
//! ```no_run
//! use extui::{Rect, DoubleBuffer, Style, Color};
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
//! extui intentionally omits certain features for simpler interfaces and better
//! performance:
//!
//! - **8-bit color only** - No 24-bit (true color) support. The 256-color palette
//!   covers most use cases with significantly simpler encoding.
//! - **4-byte grapheme limit** - Characters exceeding 4 bytes are truncated. This
//!   enables fixed-size [`Cell`] storage. Most text fits within this limit.
//! - **Unix only** - No Windows support. This allows direct use of POSIX APIs
//!   without abstraction overhead.
//!
//! # Getting Started
//!
//! ```no_run
//! use extui::{Terminal, TerminalFlags, DoubleBuffer, Style, Color};
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
//!     buf.rect().with(Color::Blue1.as_bg()).fill(&mut buf);
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

use std::{
    io::{IsTerminal, Write},
    mem::ManuallyDrop,
    os::fd::{AsFd, FromRawFd, RawFd},
};

use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

use crate::{
    event::KeyboardEnhancementFlags,
    vt::{BufferWrite, Modifier, MoveCursor, MoveCursorRight, ScrollBufferDown, ScrollBufferUp},
};

pub mod event;
mod sys;
pub mod vt;
pub mod widget;

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

/// A single terminal cell containing styled text.
///
/// Stores a grapheme cluster (up to 4 bytes) along with its associated
/// [`Style`]. Cells are the fundamental unit for terminal rendering.
///
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Cell(u64);

impl std::fmt::Debug for Cell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cell")
            .field("style", &self.style())
            .field("text", &self.text())
            .finish()
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
                && let Some(row) = buf.current.row_remaining_mut(x, y) {
                    row[0] = self.vertical;
                }
            if set.contains(Rel::CenterRight)
                && let Some(row) = buf.current.row_remaining_mut(x + w - 1, y) {
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
    current: u32,
    target: Style,
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
/// use extui::{Style, Color, vt::Modifier};
///
/// let style = Style::DEFAULT
///     .with_fg(Color::Red1)
///     .with_bg(Color::Black)
///     .with_modifier(Modifier::BOLD);
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub struct Style(u32);

impl std::fmt::Debug for Style {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Style")
            .field("fg", &self.fg())
            .field("bg", &self.bg())
            .field("modifiers", &self.modifiers())
            .finish()
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
/// use extui::Color;
///
/// let red = Color::Red1;
/// let gray = Color::Grey[15];
/// let custom = Color(42);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Color(pub u8);

#[allow(non_upper_case_globals)]
impl Color {
    pub const NavyBlue: Color = Color(17);
    pub const DarkBlue: Color = Color(18);
    pub const Blue3: Color = Color(19);
    pub const Blue1: Color = Color(21);
    pub const DarkGreen: Color = Color(22);
    pub const DeepSkyBlue4: Color = Color(23);
    pub const DodgerBlue3: Color = Color(26);
    pub const DodgerBlue2: Color = Color(27);
    pub const Green4: Color = Color(28);
    pub const SpringGreen4: Color = Color(29);
    pub const Turquoise4: Color = Color(30);
    pub const DeepSkyBlue3: Color = Color(31);
    pub const DodgerBlue1: Color = Color(33);
    pub const Green3: Color = Color(34);
    pub const DarkCyan: Color = Color(36);
    pub const DeepSkyBlue2: Color = Color(38);
    pub const DeepSkyBlue1: Color = Color(39);
    pub const SpringGreen3: Color = Color(41);
    pub const SpringGreen: Color = Color(42);
    pub const Cyan3: Color = Color(43);
    pub const DarkTurquoise: Color = Color(44);
    pub const Turquoise2: Color = Color(45);
    pub const Green1: Color = Color(46);
    pub const SpringGreen2: Color = Color(47);
    pub const SpringGreen1: Color = Color(48);
    pub const MediumSpringGreen: Color = Color(49);
    pub const Cyan2: Color = Color(50);
    pub const Cyan1: Color = Color(51);
    pub const DarkRed: Color = Color(52);
    pub const DeepPink4: Color = Color(53);
    pub const Purple3: Color = Color(56);
    pub const BlueViolet: Color = Color(57);
    pub const Orange4: Color = Color(58);
    pub const MediumPurple4: Color = Color(60);
    pub const SlateBlue3: Color = Color(61);
    pub const RoyalBlue1: Color = Color(63);
    pub const Chartreuse4: Color = Color(64);
    pub const PaleTurquoise4: Color = Color(66);
    pub const SteelBlue: Color = Color(67);
    pub const SteelBlue3: Color = Color(68);
    pub const CornflowerBlue: Color = Color(69);
    pub const Chartreuse3: Color = Color(70);
    pub const DarkSeaGreen4: Color = Color(71);
    pub const CadetBlue: Color = Color(72);
    pub const SkyBlue3: Color = Color(74);
    pub const SteelBlue1: Color = Color(75);
    pub const PaleGreen3: Color = Color(77);
    pub const SeaGreen3: Color = Color(78);
    pub const Aquamarine3: Color = Color(79);
    pub const MediumTurquoise: Color = Color(80);
    pub const Chartreuse2: Color = Color(82);
    pub const Aquamarine1: Color = Color(86);
    pub const DarkSlateGray2: Color = Color(87);
    pub const DarkViolet: Color = Color(92);
    pub const Purple: Color = Color(93);
    pub const LightPink4: Color = Color(95);
    pub const Plum4: Color = Color(96);
    pub const SlateBlue1: Color = Color(99);
    pub const Yellow4: Color = Color(100);
    pub const Wheat4: Color = Color(101);
    pub const LightSlateGrey: Color = Color(103);
    pub const MediumPurple: Color = Color(104);
    pub const LightSlateBlue: Color = Color(105);
    pub const DarkOliveGreen3: Color = Color(107);
    pub const DarkSeaGreen: Color = Color(108);
    pub const Grey: [Color; 31] = [
        Color(16),
        Color(232),
        Color(233),
        Color(234),
        Color(235),
        Color(236),
        Color(237),
        Color(238),
        Color(239),
        Color(240),
        Color(59),
        Color(241),
        Color(242),
        Color(243),
        Color(244),
        Color(102),
        Color(245),
        Color(246),
        Color(247),
        Color(139),
        Color(248),
        Color(145),
        Color(249),
        Color(250),
        Color(251),
        Color(252),
        Color(188),
        Color(253),
        Color(254),
        Color(255),
        Color(231),
    ];
    pub const SkyBlue2: Color = Color(111);
    pub const DarkOliveGreen: Color = Color(113);
    pub const DarkSeaGreen3: Color = Color(115);
    pub const DarkSlateGray3: Color = Color(116);
    pub const SkyBlue1: Color = Color(117);
    pub const Chartreuse1: Color = Color(118);
    pub const LightGreen: Color = Color(119);
    pub const PaleGreen1: Color = Color(121);
    pub const DarkSlateGray1: Color = Color(123);
    pub const Red3: Color = Color(124);
    pub const MediumVioletRed: Color = Color(126);
    pub const Magenta3: Color = Color(127);
    pub const DarkOrange3: Color = Color(130);
    pub const IndianRed: Color = Color(131);
    pub const HotPink3: Color = Color(132);
    pub const MediumOrchid3: Color = Color(133);
    pub const MediumOrchid: Color = Color(134);
    pub const DarkGoldenrod: Color = Color(136);
    pub const LightSalmon3: Color = Color(137);
    pub const RosyBrown: Color = Color(138);
    pub const Violet: Color = Color(140);
    pub const MediumPurple1: Color = Color(141);
    pub const Gold3: Color = Color(142);
    pub const DarkKhaki: Color = Color(143);
    pub const NavajoWhite3: Color = Color(144);
    pub const LightSteelBlue3: Color = Color(146);
    pub const LightSteelBlue: Color = Color(147);
    pub const Yellow3: Color = Color(148);
    pub const LightCyan3: Color = Color(152);
    pub const LightSkyBlue1: Color = Color(153);
    pub const GreenYellow: Color = Color(154);
    pub const DarkOliveGreen2: Color = Color(155);
    pub const DarkSeaGreen1: Color = Color(158);
    pub const PaleTurquoise1: Color = Color(159);
    pub const Magenta2: Color = Color(165);
    pub const HotPink2: Color = Color(169);
    pub const Orchid: Color = Color(170);
    pub const MediumOrchid1: Color = Color(171);
    pub const Orange3: Color = Color(172);
    pub const LightPink3: Color = Color(174);
    pub const Pink3: Color = Color(175);
    pub const Plum3: Color = Color(176);
    pub const LightGoldenrod3: Color = Color(179);
    pub const Tan: Color = Color(180);
    pub const MistyRose3: Color = Color(181);
    pub const Thistle3: Color = Color(182);
    pub const Plum2: Color = Color(183);
    pub const Khaki3: Color = Color(185);
    pub const LightYellow3: Color = Color(187);
    pub const LightSteelBlue1: Color = Color(189);
    pub const Yellow2: Color = Color(190);
    pub const DarkOliveGreen1: Color = Color(191);
    pub const LightSeaGreen: Color = Color(193);
    pub const Honeydew: Color = Color(194);
    pub const LightCyan1: Color = Color(195);
    pub const Red1: Color = Color(196);
    pub const DeepPink2: Color = Color(197);
    pub const DeepPink1: Color = Color(198);
    pub const Magenta1: Color = Color(201);
    pub const OrangeRed1: Color = Color(202);
    pub const NeonRed: Color = Color(203);
    pub const HotPink: Color = Color(205);
    pub const DarkOrange: Color = Color(208);
    pub const Salmon: Color = Color(209);
    pub const LightCoral: Color = Color(210);
    pub const PaleVioletRed1: Color = Color(211);
    pub const Orchid2: Color = Color(212);
    pub const Orchid1: Color = Color(213);
    pub const Orange1: Color = Color(214);
    pub const SandyBrown: Color = Color(215);
    pub const LightSalmon1: Color = Color(216);
    pub const LightPink1: Color = Color(217);
    pub const Pink1: Color = Color(218);
    pub const Plum1: Color = Color(219);
    pub const Gold1: Color = Color(220);
    pub const LightGoldenrod2: Color = Color(221);
    pub const NavajoWhite: Color = Color(223);
    pub const MistyRose: Color = Color(224);
    pub const Thistle: Color = Color(225);
    pub const Yellow1: Color = Color(226);
    pub const LightGoldenrod1: Color = Color(227);
    pub const Khaki1: Color = Color(228);
    pub const Wheat1: Color = Color(229);
    pub const Cornsilk1: Color = Color(230);

    pub const White: Color = Color(231);
    pub const Black: Color = Color(16);
}
impl Color {
    /// Creates a style with this color as the foreground.
    pub fn as_fg(self) -> Style {
        Style::DEFAULT.with_fg(self)
    }

    /// Creates a style with this color as the background.
    pub fn as_bg(self) -> Style {
        Style::DEFAULT.with_bg(self)
    }

    /// Creates a style with this color as background and the given foreground.
    pub fn with_fg(self, fg: Color) -> Style {
        Style::DEFAULT.with_fg(fg).with_bg(self)
    }

    /// Creates a style with this color as foreground and the given background.
    pub fn with_bg(self, bg: Color) -> Style {
        Style::DEFAULT.with_bg(bg).with_fg(self)
    }
}

impl std::ops::BitOrAssign<Modifier> for Style {
    fn bitor_assign(&mut self, rhs: Modifier) {
        self.0 |= rhs.0 as u32;
    }
}

impl std::ops::BitOr<Modifier> for Style {
    type Output = Style;

    fn bitor(self, rhs: Modifier) -> Self::Output {
        Style(self.0 | rhs.0 as u32)
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
    pub(crate) const HAS_BG: u32 = 0b10_000;
    pub(crate) const FG_MASK: u32 = 0xff00_0000;
    pub(crate) const BG_MASK: u32 = 0x00ff_0000;
    pub(crate) const MASK: u32 = 0xffff_fff8;
    pub(crate) const HAS_FG: u32 = 0b01_000;

    /// Creates a [`StyleDelta`] for transitioning to this style.
    pub const fn delta(self) -> StyleDelta {
        StyleDelta {
            current: u32::MAX,
            target: self,
        }
    }
    /// Returns a new style with the given foreground color.
    pub const fn with_fg(self, color: Color) -> Style {
        Style((self.0 & 0x00ff_ffff) | ((color.0 as u32) << 24) | 0b1_000)
    }

    /// Returns a new style without a foreground color.
    pub const fn without_fg(self) -> Style {
        Style(self.0 & !(Style::FG_MASK | Style::HAS_FG))
    }

    /// Returns a new style without a background color.
    pub const fn without_bg(self) -> Style {
        Style(self.0 & !(Style::BG_MASK | Style::HAS_BG))
    }

    /// Returns the text modifiers applied to this style.
    pub const fn modifiers(self) -> Modifier {
        Modifier(self.0 as u16 & Modifier::ALL.0)
    }

    /// Returns a new style with the given modifiers added.
    pub const fn with_modifier(self, mods: Modifier) -> Style {
        Style(self.0 | mods.0 as u32)
    }

    /// Returns a new style with the given modifiers removed.
    pub const fn without_modifier(self, mods: Modifier) -> Style {
        Style(self.0 & !(mods.0 as u32))
    }

    /// Returns the foreground color if set.
    pub const fn fg(self) -> Option<Color> {
        if self.0 & 0b1_000 != 0 {
            Some(Color(((self.0 >> 24) & 0xff) as u8))
        } else {
            None
        }
    }

    /// Returns a new style with the given background color.
    pub const fn with_bg(self, color: Color) -> Style {
        Style((self.0 & 0xff00_ffff) | ((color.0 as u32) << 16) | 0b10_000)
    }

    /// Returns the background color if set.
    pub const fn bg(self) -> Option<Color> {
        if self.0 & 0b10_000 != 0 {
            Some(Color(((self.0 >> 16) & 0xff) as u8))
        } else {
            None
        }
    }
}

// impl std::ops::BitOr for Style {
//     type Output = Style;

//     fn bitor(self, rhs: Self) -> Self::Output {
//         Style(self.0 | rhs.0)
//     }
// }

impl Cell {
    const EMPTY: Cell = Cell(0);
    const BLANK: Cell = unsafe { Cell::new_ascii(b' ', Style::DEFAULT) };

    /// Returns `true` if the cell contains no visible content.
    pub fn is_empty(self) -> bool {
        self.0 <= 0xffff_ffff
    }
    #[inline]
    fn text(&self) -> &str {
        let len = (self.0 & 0b111) as usize;
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                ((&self.0 as *const u64) as *const u8).add(4),
                len,
            ))
        }
    }
    fn new(text: &str, style: Style) -> Cell {
        if text.len() > 4 {
            panic!();
        }
        let mut initial = [0u8; 8];
        for (i, ch) in text.as_bytes().iter().enumerate() {
            initial[i + 4] = *ch;
        }
        Cell(u64::from_ne_bytes(initial) | (style.0 as u64) | text.len() as u64)
    }
    const fn new_const(text: &str, style: Style) -> Cell {
        if text.len() > 4 {
            panic!();
        }
        let mut initial = [0u8; 8];
        let bytes = text.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            initial[i + 4] = bytes[i];
            i += 1;
        }
        Cell(u64::from_ne_bytes(initial) | (style.0 as u64) | text.len() as u64)
    }

    /// Returns a new cell with the given style merged into the existing style.
    pub fn with_style_merged(&self, style: Style) -> Cell {
        Cell(self.0 | style.0 as u64)
    }
    fn style(&self) -> Style {
        Style((self.0 & 0xfffffff8) as u32)
    }
    const unsafe fn new_ascii(ch: u8, style: Style) -> Cell {
        Cell(((ch as u64) << (4 * 8)) | (style.0 as u64) | 1)
    }
}

/// A rectangular grid of terminal cells.
///
/// Stores styled text content for rendering to a terminal. Use
/// [`set_string`](Self::set_string) and [`set_style`](Self::set_style)
/// to modify the buffer contents.
pub struct Buffer {
    pub(crate) cells: Box<[Cell]>,
    pub(crate) width: u16,
    pub(crate) height: u16,
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
    /// Character offset for continued rendering.
    pub offset: u32,
}

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

/// A rectangle with associated rendering properties.
///
/// Created from a [`Rect`] via [`display`](Rect::display) or [`with`](Rect::with).
/// Provides a fluent API for rendering styled text.
pub struct DisplayRect {
    rect: Rect,
    properties: RenderProperties,
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
    pub fn skip(mut self, amount: u32) -> DisplayRect {
        self.properties.offset = self.properties.offset.wrapping_add(amount);
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
    fn text_inner(mut self, buf: &mut Buffer, text: &str) -> DisplayRect {
        if self.rect.is_empty() {
            return self;
        }
        match self.properties.halign {
            HAlign::Left => {
                let (nx, _ny) = buf.set_stringn(
                    self.rect.x + self.properties.offset as u16,
                    self.rect.y,
                    text,
                    self.rect.w as usize,
                    self.properties.style,
                );
                self.properties.offset = (nx - self.rect.x) as u32;
                self
            }
            HAlign::Center => todo!(),
            HAlign::Right => {
                let text_width = UnicodeWidthStr::width(text);
                let start_x = if text_width as u16 >= self.rect.w {
                    self.rect.x
                } else {
                    self.rect.x + self.rect.w - text_width as u16
                };
                let (nx, _ny) = buf.set_stringn(
                    start_x,
                    self.rect.y,
                    text,
                    self.rect.w as usize,
                    self.properties.style,
                );
                self.properties.offset = (nx - self.rect.x) as u32;
                self
            }
        }
    }
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

pub(crate) fn write_style_diff_ignore_fg(
    current: Style,
    mut new: Style,
    buf: &mut Vec<u8>,
) -> Style {
    new.0 = (new.0 & !(Style::FG_MASK | Style::HAS_FG))
        | (current.0 & (Style::FG_MASK | Style::HAS_FG));
    write_style_diff(current, new, buf)
}
pub(crate) fn write_style_diff(current: Style, new: Style, buf: &mut Vec<u8>) -> Style {
    if current == new {
        return current;
    }
    let removed = (new.0 | current.0) ^ new.0;
    let clearing = removed & (Style::HAS_BG | Style::HAS_FG | Modifier::ALL.0 as u32) != 0;
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

impl Buffer {
    /// Scrolls content up by the given number of lines.
    ///
    /// Lines shifted off the top are discarded; new lines at the bottom
    /// are filled with empty cells.
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
    /// Scrolls content down by the given number of lines.
    ///
    /// Lines shifted off the bottom are discarded; new lines at the top
    /// are filled with empty cells.
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
    /// Returns the buffer width in cells.
    pub fn width(&self) -> u16 {
        self.width
    }

    /// Returns the buffer height in cells.
    pub fn height(&self) -> u16 {
        self.height
    }

    /// Creates a new buffer with the given dimensions.
    pub fn new(width: u16, height: u16) -> Buffer {
        let cells = vec![Cell::EMPTY; width as usize * height as usize].into_boxed_slice();
        Buffer {
            cells,
            width,
            height,
        }
    }
    /// Returns a mutable reference to the cell at the given position.
    pub fn get_mut(&mut self, x: u16, y: u16) -> Option<&mut Cell> {
        let index = (y as u32) * (self.width as u32) + (x as u32);
        self.cells.get_mut(index as usize)
    }
    /// Returns a mutable slice of cells from position (x, y) to the end of the row.
    pub fn row_remaining_mut(&mut self, x: u16, y: u16) -> Option<&mut [Cell]> {
        let base = (y as u32) * (self.width as u32);
        let start = base + (x as u32);
        // todo can opt this
        let end = base + self.width as u32;
        self.cells.get_mut(start as usize..end as usize)
    }

    fn render_diff(&mut self, buf: &mut Vec<u8>, old: &Buffer, y_offset: u16, blanking: bool) {
        if self.cells.len() != old.cells.len() {
            self.render(buf, y_offset);
            return;
        }
        vt::CLEAR_STYLE.write_to_buffer(buf);
        let mut current_style = Style::DEFAULT;
        let mut old_cells = old.cells.iter();
        for (y, row) in self.cells.chunks_exact(self.width as usize).enumerate() {
            let y = y as u16 + y_offset;
            let mut moved = false;
            let mut matching_count = 0;
            let mut new_cells = row.iter();
            let mut erased_cell: Option<Cell> = None;
            'next_cell: while let Some(new) = new_cells.next() {
                let mut new = *new;
                let Some(old) = erased_cell.or_else(|| old_cells.next().copied()) else {
                    return;
                };
                if new == old {
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
                            if old_k == new_k {
                                if !moved {
                                    moved = true;
                                    MoveCursor(matching_count, y).write_to_buffer(buf);
                                    matching_count = 0;
                                }
                                if matching_count > 0 {
                                    MoveCursorRight(matching_count).write_to_buffer(buf);
                                    // matching_count = 0;
                                }
                                current_style = write_style_diff_ignore_fg(
                                    current_style,
                                    blank_kind.style(),
                                    buf,
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
                                    if new_k == blank_kind {
                                        matching_count += 1;
                                        continue 'next_cell;
                                    } else {
                                        new = new_k;
                                        break 'continue_new;
                                    }
                                }
                                continue 'next_cell;
                            }
                            if new_k == blank_kind {
                                blank_overwrite += 1;
                                continue;
                            }

                            if !moved {
                                moved = true;
                                MoveCursor(matching_count, y).write_to_buffer(buf);
                                matching_count = 0;
                            }
                            if matching_count > 0 {
                                MoveCursorRight(matching_count).write_to_buffer(buf);
                                matching_count = 0;
                            }

                            current_style =
                                write_style_diff_ignore_fg(current_style, blank_kind.style(), buf);
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
                            MoveCursor(matching_count, y).write_to_buffer(buf);
                            matching_count = 0;
                        }
                        if matching_count > 0 {
                            MoveCursorRight(matching_count).write_to_buffer(buf);
                            // matching_count = 0;
                        }
                        current_style =
                            write_style_diff_ignore_fg(current_style, blank_kind.style(), buf);

                        if blank_overwrite < 8 {
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
                    MoveCursor(matching_count, y).write_to_buffer(buf);
                    matching_count = 0;
                }
                if matching_count > 0 {
                    MoveCursorRight(matching_count).write_to_buffer(buf);
                    matching_count = 0;
                }
                let text = new.text();
                if text.is_empty() {
                    current_style = write_style_diff_ignore_fg(current_style, new.style(), buf);
                } else {
                    current_style = write_style_diff(current_style, new.style(), buf);
                }
                if text.is_empty() {
                    buf.push(b' ');
                } else {
                    buf.extend_from_slice(text.as_bytes());
                }
            }
        }
    }
    fn render(&mut self, buf: &mut Vec<u8>, y_offset: u16) {
        if y_offset == 0 {
            vt::MOVE_CURSOR_TO_ORIGIN.write_to_buffer(buf);
        } else {
            MoveCursor(0, y_offset).write_to_buffer(buf);
        }
        vt::CLEAR_STYLE.write_to_buffer(buf);
        buf.extend_from_slice(vt::CLEAR_BELOW);

        let mut current_style = Style::DEFAULT;
        for (y, row) in self.cells.chunks_exact(self.width as usize).enumerate() {
            let y = y as u16 + y_offset;
            let mut moved = false;
            let mut blank_extension = 0;
            for &cell in row.iter() {
                if cell == Cell::BLANK || cell == Cell::EMPTY {
                    blank_extension += 1;
                    continue;
                }
                if !moved {
                    moved = true;
                    MoveCursor(blank_extension, y).write_to_buffer(buf);
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
                current_style = write_style_diff(current_style, cell.style(), buf);
                let text = cell.text();
                if text.is_empty() {
                    buf.push(b' ');
                } else {
                    buf.extend_from_slice(text.as_bytes());
                }
            }
        }
        vt::MOVE_CURSOR_TO_ORIGIN.write_to_buffer(buf);
    }
    pub fn set_style(&mut self, area: Rect, style: Style) {
        let Rect { x, y, w, h } = area;
        let mut keep_mask = !(Style::MASK as u64);
        let mut new_mask = style.0 as u64;

        if style.fg().is_none() {
            keep_mask |= (Style::HAS_FG as u64) | Style::FG_MASK as u64;
        }
        if style.bg().is_none() {
            keep_mask |= (Style::HAS_BG as u64) | Style::BG_MASK as u64;
        }
        new_mask &= !keep_mask;

        for y in y..y + h {
            if let Some(row) = self.row_remaining_mut(x, y) {
                for cell in row.iter_mut().take(w as usize) {
                    cell.0 = (cell.0 & keep_mask) | new_mask;
                }
            }
        }
    }
    /// Writes a styled string at the given position.
    ///
    /// Returns the cursor position after writing.
    pub fn set_string(&mut self, x: u16, y: u16, string: &str, style: Style) -> (u16, u16) {
        self.set_stringn(x, y, string, usize::MAX, style)
    }

    /// Writes a styled string with a maximum width.
    ///
    /// Returns the cursor position after writing.
    pub fn set_stringn(
        &mut self,
        x: u16,
        y: u16,
        string: &str,
        max_width: usize,
        style: Style,
    ) -> (u16, u16) {
        let mut remaining_width = (self.width.saturating_sub(x) as usize).min(max_width) as u16;
        let initial_remaining_width = remaining_width;
        let Some(target) = self.row_remaining_mut(x, y) else {
            return (x, y);
        };
        let graphemes = UnicodeSegmentation::graphemes(string, true)
            .filter(|symbol| !symbol.contains(char::is_control))
            .map(|symbol| (symbol, symbol.width() as u16))
            .filter(|(_symbol, width)| *width > 0)
            .map_while(|(symbol, width)| {
                remaining_width = remaining_width.checked_sub(width)?;
                Some((symbol, width))
            });
        let mut target_cells = target.iter_mut();
        for (symbol, width) in graphemes {
            if let Some(cell) = target_cells.next() {
                *cell = Cell::new(symbol, style);
            } else {
                return (x + initial_remaining_width, y);
            }
            // Todo: When a cell takes more space, what do we do with the pad spaces
            // do we these, what happens in some over rights them?
            for _ in 1..width {
                if let Some(cell) = target_cells.next() {
                    *cell = Cell::EMPTY;
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
    /// and extended keyboard input.
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
            return Err(std::io::Error::other(
                "Stdout is not a terminal",
            ));
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
            return Err(std::io::Error::other(
                "Stdout is not a terminal",
            ));
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
    scroll: i16,
    #[doc(hidden)]
    pub y_offset: u16,
    epoch: u64,
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
    pub(crate) fn set_cell(&mut self, x: u16, y: u16, cell: Cell) {
        if let Some(target) = self.current.get_mut(x, y) {
            *target = cell;
        }
    }
    /// Applies a style to all cells within the given area.
    pub fn set_style(&mut self, area: Rect, style: Style) {
        self.current.set_style(area, style)
    }

    /// Writes a styled string at the given position.
    ///
    /// Returns the cursor position after writing.
    pub fn set_string(&mut self, x: u16, y: u16, string: &str, style: Style) -> (u16, u16) {
        self.current.set_string(x, y, string, style)
    }

    /// Writes a styled string with a maximum width.
    ///
    /// Returns the cursor position after writing.
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
    /// Creates a new double buffer with the given dimensions.
    pub fn new(width: u16, height: u16) -> DoubleBuffer {
        DoubleBuffer {
            current: Buffer::new(width, height),
            previous: Buffer::new(width, height),
            diffable: false,
            blanking: true,
            buf: Vec::with_capacity(16 * 1024),
            scroll: 0,
            epoch: 0,
            y_offset: 0,
        }
    }

    /// Clears both buffers and increments the epoch.
    pub fn reset(&mut self) {
        self.current.cells.fill(Cell::EMPTY);
        self.previous.cells.fill(Cell::EMPTY);
        self.diffable = false;
        self.epoch = self.epoch.wrapping_add(1);
    }
    /// Resizes the buffer if dimensions have changed.
    pub fn resize(&mut self, width: u16, height: u16) {
        if self.current.width != width || self.current.height != height {
            self.current = Buffer::new(width, height);
            self.previous = Buffer::new(width, height);
            self.diffable = false;
            self.epoch = self.epoch.wrapping_add(1);
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
    }
    /// Renders the current buffer to the internal byte buffer.
    ///
    /// Use [`write_buffer`](Self::write_buffer) to access the output.
    pub fn render_internal(&mut self) {
        if self.diffable {
            if self.y_offset == 0 {
                if self.scroll < 0 {
                    vt::CLEAR_STYLE.write_to_buffer(&mut self.buf);
                    ScrollBufferDown(-self.scroll as u16).write_to_buffer(&mut self.buf);
                    // mutage prev buffer to match
                    self.previous.scroll_down(-self.scroll as u16);
                } else if self.scroll > 0 {
                    vt::CLEAR_STYLE.write_to_buffer(&mut self.buf);
                    ScrollBufferUp(self.scroll as u16).write_to_buffer(&mut self.buf);
                    self.previous.scroll_up(self.scroll as u16);
                }
            }
            self.scroll = 0;
            self.current
                .render_diff(&mut self.buf, &self.previous, self.y_offset, self.blanking);
        } else {
            self.scroll = 0;
            self.current.render(&mut self.buf, self.y_offset);
            self.diffable = true;
        }
        std::mem::swap(&mut self.current, &mut self.previous);
        self.current.cells.fill(Cell::EMPTY);
    }
    /// Renders and writes the output to the terminal.
    pub fn render(&mut self, term: &mut Terminal) {
        self.render_internal();
        term.write_all(&self.buf).unwrap();
        self.buf.clear();
    }
    /// Returns a mutable reference to the current buffer.
    pub fn current(&mut self) -> &mut Buffer {
        &mut self.current
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
            && let Some(row) = buf.row_remaining_mut(x, y) {
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
            && let Some(row) = buf.row_remaining_mut(x, y + h - 1) {
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
                && let Some(row) = buf.row_remaining_mut(x, y) {
                    row[0] = box_style.vertical;
                }
            if self.borders & Self::RIGHT != 0
                && let Some(row) = buf.row_remaining_mut(x + w - 1, y) {
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
                    buf.set_stringn(x + 1, y, " ", 1, self.border_style);
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
            let fg = Color(fg);
            let colored = Style::DEFAULT.with_fg(fg);
            assert_eq!(colored.fg().unwrap(), fg);
            for bg in 0..=255u8 {
                let bg = Color(bg);
                let colored = colored.with_bg(bg);
                assert_eq!(colored.bg().unwrap(), bg);
                assert_eq!(colored.fg().unwrap(), fg);
            }
        }
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
                    .with(Color::LightGoldenrod2.with_fg(Color::Black))
                    .text(out, " Done ");

                a.with(if i == 0 {
                    Color::Blue1.with_fg(Color::Black)
                } else {
                    Color(248).as_fg()
                })
                .fill(out)
                .skip(1)
                .text(out, "die R:0 S:0");

                b.take_left(6)
                    .with(Color::Aquamarine1.with_bg(Color::Grey[4]))
                    .text(out, " Fail ");
                b.with(Color(248).as_fg())
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
                    .with(Color::LightGoldenrod2.with_fg(Color::Black))
                    .text(out, " Done ");

                a.with(if i == 1 {
                    Color::Blue1.with_fg(Color::Black)
                } else {
                    Color(248).as_fg()
                })
                .fill(out)
                .skip(1)
                .text(out, "die R:0 S:0");

                b.take_left(6)
                    .with(Color::BlueViolet.with_bg(Color::Grey[4]))
                    .text(out, " Done ");
                b.with(Color(248).as_fg())
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
            Color(1).as_bg(),
        );
        buffer.set_style(
            Rect {
                x: 2,
                y: 0,
                w: 6,
                h: 1,
            },
            Color(2).as_fg(),
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
            /* 0 */ Cell::new("", Color(1).as_bg()),
            /* 1 */ Cell::new("h", Color(1).as_bg()),
            /* 2 */ Cell::new("e", Color(1).as_bg() | Color(2).as_fg()),
            /* 3 */ Cell::new("l", Color(1).as_bg() | Color(2).as_fg() | Modifier::BOLD),
            /* 4 */ Cell::new("l", Color(1).as_bg() | Color(2).as_fg()),
            /* 5 */ Cell::new("o", Color(2).as_fg()),
            /* 6 */ Cell::new("", Color(2).as_fg()),
            /* 7 */ Cell::new("", Color(2).as_fg()),
        ];
        for (i, (got, expected)) in buffer.cells.iter().zip(expected.iter()).enumerate() {
            assert_eq!(got, expected, "Mismatch at cell {}", i);
        }
    }
}
