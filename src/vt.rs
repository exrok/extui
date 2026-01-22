//! VT escape sequence generation.
//!
//! Provides types and constants for generating ANSI/VT escape sequences used to
//! control terminal behavior: cursor movement, text styling, screen clearing,
//! and mode switching.
//!
//! # BufferWrite Trait
//!
//! Types implementing [`BufferWrite`] can be written to a byte buffer and used
//! with the [`splat!`](crate::splat) macro:
//!
//! ```
//! use extui::{splat, vt::{MoveCursor, CLEAR_STYLE, HIDE_CURSOR}};
//!
//! let mut buf = Vec::new();
//! splat!(&mut buf, HIDE_CURSOR, MoveCursor(0, 0), CLEAR_STYLE);
//! ```
//!
//! # Cursor Movement
//!
//! - [`MoveCursor`] - Move to absolute position
//! - [`MoveCursorUp`] / [`MoveCursorDown`] - Move vertically by N rows
//! - [`MoveCursorLeft`] / [`MoveCursorRight`] - Move horizontally by N columns
//! - [`MoveCursorNextLine`] / [`MoveCursorPrevLine`] - Move to start of line N rows away
//! - [`MoveCursorToColumn`] - Move to specific column on current line
//! - [`MOVE_CURSOR_TO_ORIGIN`] - Move to top-left corner
//!
//! # Cursor State
//!
//! - [`SAVE_CURSOR`] / [`RESTORE_CURSOR`] - Save/restore cursor position (DEC)
//! - [`SAVE_CURSOR_ANSI`] / [`RESTORE_CURSOR_ANSI`] - Save/restore cursor position (ANSI)
//! - [`HIDE_CURSOR`] / [`SHOW_CURSOR`] - Cursor visibility
//!
//! # Screen Control
//!
//! - [`ENABLE_ALT_SCREEN`] / [`DISABLE_ALT_SCREEN`] - Alternate screen buffer
//! - [`CLEAR_SCREEN`] - Clear entire screen
//! - [`CLEAR_SCREEN_AND_SCROLLBACK`] - Clear screen and scrollback buffer
//! - [`CLEAR_BELOW`] / [`CLEAR_ABOVE`] - Clear from cursor to screen edge
//! - [`ENABLE_LINE_WRAP`] / [`DISABLE_LINE_WRAP`] - Line wrapping at screen edge
//!
//! # Line Operations
//!
//! - [`CLEAR_LINE`] - Clear entire current line
//! - [`CLEAR_LINE_TO_RIGHT`] / [`CLEAR_LINE_TO_LEFT`] - Clear from cursor to line edge
//! - [`InsertLines`] / [`DeleteLines`] - Insert or delete lines
//!
//! # Character Operations
//!
//! - [`InsertChars`] / [`DeleteChars`] - Insert or delete characters
//! - [`EraseChars`] - Erase characters without shifting
//!
//! # Scrolling
//!
//! - [`ScrollBufferUp`] / [`ScrollBufferDown`] - Scroll terminal content
//! - [`ScrollRegion`] - Define scroll region bounds
//!
//! # Styling
//!
//! - [`Modifier`] - Text attributes (bold, italic, underline, etc.)
//! - [`style`] - Generate SGR escape sequences
//! - [`CLEAR_STYLE`] - Reset all attributes
//!
//! # Miscellaneous
//!
//! - [`BELL`] - Ring the terminal bell

use crate::{Style, StyleDelta, event::KeyboardEnhancementFlags};

// # Safety Rationale for Unsafe Blocks
//
// All unsafe blocks in this module follow a common pattern for performance-critical
// VT escape sequence generation:
// 1. Reserve sufficient capacity before any writes
// 2. Write bytes directly via raw pointer arithmetic
// 3. Set the Vec length to include exactly the bytes written
//
// Correctness is verified by miri tests (cargo +nightly miri test --lib tui).

/// Writes a CSI sequence with a single numeric parameter: `ESC[n<suffix>`.
///
/// Used for cursor movement, scrolling, line/character operations.
fn write_csi_n(buffer: &mut Vec<u8>, n: u16, suffix: u8) {
    buffer.reserve(2 + 5 + 1);
    let len = buffer.len();
    let optr = buffer.as_mut_ptr();
    // SAFETY: Reserved 8 bytes (ESC[ + n + suffix). Max u16 is 5 digits.
    unsafe {
        let mut ptr = optr.add(len);
        *(ptr as *mut [u8; 2]) = [0x1b, b'['];
        ptr = ptr.add(2);
        let offset = itoap::write_to_ptr(ptr, n);
        ptr = ptr.add(offset);
        *ptr = suffix;
        let new_len = ptr.offset_from(optr) as usize + 1;
        buffer.set_len(new_len);
    }
}

/// Writes VT escape sequences to a byte buffer.
///
/// Implement this trait to enable use with the [`splat!`](crate::splat) macro.
pub trait BufferWrite {
    /// Appends the VT sequence bytes to the buffer.
    fn write_to_buffer(&self, buffer: &mut Vec<u8>);
}

impl BufferWrite for [u8] {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        buffer.extend_from_slice(self);
    }
}

impl BufferWrite for str {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        buffer.extend_from_slice(self.as_bytes());
    }
}

impl<T: itoap::Integer + Copy> BufferWrite for T {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        itoap::write_to_vec(buffer, *self);
    }
}

#[rustfmt::skip]
static LOOKUP: [u32;256] = [
    16777264,16777265,16777266,16777267,16777268,16777269,16777270,16777271,16777272,16777273,
    33566769,33567025,33567281,33567537,33567793,33568049,33568305,33568561,33568817,33569073,
    33566770,33567026,33567282,33567538,33567794,33568050,33568306,33568562,33568818,33569074,
    33566771,33567027,33567283,33567539,33567795,33568051,33568307,33568563,33568819,33569075,
    33566772,33567028,33567284,33567540,33567796,33568052,33568308,33568564,33568820,33569076,
    33566773,33567029,33567285,33567541,33567797,33568053,33568309,33568565,33568821,33569077,
    33566774,33567030,33567286,33567542,33567798,33568054,33568310,33568566,33568822,33569078,
    33566775,33567031,33567287,33567543,33567799,33568055,33568311,33568567,33568823,33569079,
    33566776,33567032,33567288,33567544,33567800,33568056,33568312,33568568,33568824,33569080,
    33566777,33567033,33567289,33567545,33567801,33568057,33568313,33568569,33568825,33569081,
    53489713,53555249,53620785,53686321,53751857,53817393,53882929,53948465,54014001,54079537,
    53489969,53555505,53621041,53686577,53752113,53817649,53883185,53948721,54014257,54079793,
    53490225,53555761,53621297,53686833,53752369,53817905,53883441,53948977,54014513,54080049,
    53490481,53556017,53621553,53687089,53752625,53818161,53883697,53949233,54014769,54080305,
    53490737,53556273,53621809,53687345,53752881,53818417,53883953,53949489,54015025,54080561,
    53490993,53556529,53622065,53687601,53753137,53818673,53884209,53949745,54015281,54080817,
    53491249,53556785,53622321,53687857,53753393,53818929,53884465,53950001,54015537,54081073,
    53491505,53557041,53622577,53688113,53753649,53819185,53884721,53950257,54015793,54081329,
    53491761,53557297,53622833,53688369,53753905,53819441,53884977,53950513,54016049,54081585,
    53492017,53557553,53623089,53688625,53754161,53819697,53885233,53950769,54016305,54081841,
    53489714,53555250,53620786,53686322,53751858,53817394,53882930,53948466,54014002,54079538,
    53489970,53555506,53621042,53686578,53752114,53817650,53883186,53948722,54014258,54079794,
    53490226,53555762,53621298,53686834,53752370,53817906,53883442,53948978,54014514,54080050,
    53490482,53556018,53621554,53687090,53752626,53818162,53883698,53949234,54014770,54080306,
    53490738,53556274,53621810,53687346,53752882,53818418,53883954,53949490,54015026,54080562,
    53490994,53556530,53622066,53687602,53753138,53818674
];

/// Writes a style escape sequence to the buffer.
///
/// If `clear` is true, prepends `0;` to reset attributes first.
pub fn style(out: &mut Vec<u8>, style: Style, clear: bool) {
    if style == Style::DEFAULT {
        if clear {
            out.extend_from_slice(b"\x1b[0m");
        }
        return;
    }
    out.reserve(64);
    let len = out.len();
    let ogptr = out.as_mut_ptr();
    let ptr = out.as_mut_ptr();

    // SAFETY: Reserved 64 bytes. Max output is ~47 bytes.
    unsafe {
        let mut ptr = ptr.add(len);
        *(ptr as *mut [u8; 2]) = [0x1b, b'['];
        ptr = ptr.add(2);
        if clear {
            *(ptr as *mut [u8; 2]) = [b'0', b';'];
            ptr = ptr.add(2);
        }
        let mods = style.modifiers();
        let mut first = true;
        if !mods.is_empty() {
            ptr = write_all_modifiers_inner(ptr, mods);
            first = false;
        }
        if let Some(color) = style.fg() {
            if !first {
                *(ptr as *mut u8) = b';';
                ptr = ptr.add(1);
            } else {
                first = false;
            }
            *(ptr as *mut [u8; 8]) = *b"38;5;000";
            ptr = ptr.add(5);
            let mask = LOOKUP[color.0 as usize];
            let numlen = (mask >> 24) as usize;
            *(ptr as *mut [u8; 4]) = mask.to_ne_bytes();
            ptr = ptr.add(numlen);
        }
        if let Some(color) = style.bg() {
            if !first {
                *(ptr as *mut u8) = b';';
                ptr = ptr.add(1);
            }
            *(ptr as *mut [u8; 8]) = *b"48;5;000";
            ptr = ptr.add(5);
            let mask = LOOKUP[color.0 as usize];
            let numlen = (mask >> 24) as usize;
            *(ptr as *mut [u8; 4]) = mask.to_ne_bytes();
            ptr = ptr.add(numlen);
        }
        *ptr = b'm';
        let new_len = ptr.offset_from(ogptr) as usize + 1;
        out.set_len(new_len);
    }
}

/// Move the cursor to a specific position.
///
/// Coordinates are 0-indexed (column, row). The VT sequence uses 1-indexed
/// positions, so 1 is added internally. Uses `ESC[row;colH`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MoveCursor(pub u16, pub u16);

impl BufferWrite for MoveCursor {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        let (x, y) = (self.0, self.1);
        buffer.reserve(2 + 5 + 1 + 5 + 1);
        let len = buffer.len();
        let optr = buffer.as_mut_ptr();
        // SAFETY: Reserved 14 bytes (ESC[ + row + ; + col + H).
        unsafe {
            let mut ptr = optr.add(len);
            *(ptr as *mut [u8; 2]) = [0x1b, b'['];
            ptr = ptr.add(2);
            let offset = itoap::write_to_ptr(ptr, y.saturating_add(1));
            ptr = ptr.add(offset);
            *ptr = b';';
            ptr = ptr.add(1);
            let offset = itoap::write_to_ptr(ptr, x.saturating_add(1));
            ptr = ptr.add(offset);
            *ptr = b'H';
            let new_len = ptr.offset_from(optr) as usize + 1;
            buffer.set_len(new_len);
        }
    }
}

/// Scrolls the terminal buffer down by `n` lines.
///
/// Inserts `n` blank lines at the top of the scroll region, moving existing
/// content down. Uses the VT sequence `ESC[nT`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ScrollBufferDown(pub u16);

impl BufferWrite for ScrollBufferDown {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        write_csi_n(buffer, self.0, b'T');
    }
}

/// Scrolls the terminal buffer up by `n` lines.
///
/// Removes `n` lines from the top of the scroll region and adds `n` blank
/// lines at the bottom. Uses the VT sequence `ESC[nS`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ScrollBufferUp(pub u16);

impl BufferWrite for ScrollBufferUp {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        write_csi_n(buffer, self.0, b'S');
    }
}

/// Moves the cursor right by `n` columns.
///
/// Uses the VT sequence `ESC[nC`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MoveCursorRight(pub u16);

impl BufferWrite for MoveCursorRight {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        write_csi_n(buffer, self.0, b'C');
    }
}

/// Moves the cursor left by `n` columns.
///
/// Uses the VT sequence `ESC[nD`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MoveCursorLeft(pub u16);

impl BufferWrite for MoveCursorLeft {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        write_csi_n(buffer, self.0, b'D');
    }
}

/// Moves the cursor up by `n` rows.
///
/// Uses the VT sequence `ESC[nA`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MoveCursorUp(pub u16);

impl BufferWrite for MoveCursorUp {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        write_csi_n(buffer, self.0, b'A');
    }
}

/// Moves the cursor down by `n` rows.
///
/// Uses the VT sequence `ESC[nB`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MoveCursorDown(pub u16);

impl BufferWrite for MoveCursorDown {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        write_csi_n(buffer, self.0, b'B');
    }
}

/// Moves the cursor to the beginning of a line `n` rows down.
///
/// Uses the VT sequence `ESC[nE`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MoveCursorNextLine(pub u16);

impl BufferWrite for MoveCursorNextLine {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        write_csi_n(buffer, self.0, b'E');
    }
}

/// Moves the cursor to the beginning of a line `n` rows up.
///
/// Uses the VT sequence `ESC[nF`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MoveCursorPrevLine(pub u16);

impl BufferWrite for MoveCursorPrevLine {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        write_csi_n(buffer, self.0, b'F');
    }
}

/// Moves the cursor to a specific column on the current line.
///
/// The column is 0-indexed. Uses the VT sequence `ESC[nG`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MoveCursorToColumn(pub u16);

impl BufferWrite for MoveCursorToColumn {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        write_csi_n(buffer, self.0.saturating_add(1), b'G');
    }
}

/// Moves the cursor to the top-left corner. VT sequence `ESC[H`.
pub const MOVE_CURSOR_TO_ORIGIN: &[u8] = b"\x1b[H";

/// Resets all text attributes to default. VT sequence `ESC[0m`.
pub const CLEAR_STYLE: &[u8] = b"\x1b[0m";

/// Clears the entire screen. VT sequence `ESC[2J`.
pub const CLEAR_SCREEN: &[u8] = b"\x1b[2J";
/// Clears the entire screen and scrollback buffer. VT sequence `ESC[3J`.
pub const CLEAR_SCREEN_AND_SCROLLBACK: &[u8] = b"\x1b[3J";
/// Clears from cursor to end of screen. VT sequence `ESC[J`.
pub const CLEAR_BELOW: &[u8] = b"\x1b[J";
/// Clears from cursor to beginning of screen. VT sequence `ESC[1J`.
pub const CLEAR_ABOVE: &[u8] = b"\x1b[1J";

/// Clears the entire current line. VT sequence `ESC[2K`.
pub const CLEAR_LINE: &[u8] = b"\x1b[2K";
/// Clears from cursor to end of line. VT sequence `ESC[K`.
pub const CLEAR_LINE_TO_RIGHT: &[u8] = b"\x1b[K";
/// Clears from cursor to beginning of line. VT sequence `ESC[1K`.
pub const CLEAR_LINE_TO_LEFT: &[u8] = b"\x1b[1K";

/// Saves the current cursor position (DEC private). VT sequence `ESC7`.
pub const SAVE_CURSOR: &[u8] = b"\x1b7";
/// Restores the previously saved cursor position (DEC private). VT sequence `ESC8`.
pub const RESTORE_CURSOR: &[u8] = b"\x1b8";

/// Saves the current cursor position (ANSI). VT sequence `ESC[s`.
pub const SAVE_CURSOR_ANSI: &[u8] = b"\x1b[s";
/// Restores the previously saved cursor position (ANSI). VT sequence `ESC[u`.
pub const RESTORE_CURSOR_ANSI: &[u8] = b"\x1b[u";

/// Enables line wrapping at screen edge. VT sequence `ESC[?7h`.
pub const ENABLE_LINE_WRAP: &[u8] = b"\x1b[?7h";
/// Disables line wrapping at screen edge. VT sequence `ESC[?7l`.
pub const DISABLE_LINE_WRAP: &[u8] = b"\x1b[?7l";

/// Rings the terminal bell. VT sequence `BEL` (0x07).
pub const BELL: &[u8] = b"\x07";

/// Inserts `n` blank lines at the cursor position.
///
/// Lines below are shifted down. Uses the VT sequence `ESC[nL`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InsertLines(pub u16);

impl BufferWrite for InsertLines {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        write_csi_n(buffer, self.0, b'L');
    }
}

/// Deletes `n` lines starting at the cursor position.
///
/// Lines below are shifted up. Uses the VT sequence `ESC[nM`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DeleteLines(pub u16);

impl BufferWrite for DeleteLines {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        write_csi_n(buffer, self.0, b'M');
    }
}

/// Inserts `n` blank characters at the cursor position.
///
/// Characters to the right are shifted right. Uses the VT sequence `ESC[n@`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InsertChars(pub u16);

impl BufferWrite for InsertChars {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        write_csi_n(buffer, self.0, b'@');
    }
}

/// Deletes `n` characters at the cursor position.
///
/// Characters to the right are shifted left. Uses the VT sequence `ESC[nP`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DeleteChars(pub u16);

impl BufferWrite for DeleteChars {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        write_csi_n(buffer, self.0, b'P');
    }
}

/// Erases `n` characters at the cursor position, replacing them with blanks.
///
/// Does not shift characters. Uses the VT sequence `ESC[nX`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EraseChars(pub u16);

impl BufferWrite for EraseChars {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        write_csi_n(buffer, self.0, b'X');
    }
}

/// Text styling modifiers such as bold, italic, and underline.
///
/// Combine modifiers using bitwise OR. Use [`Style::with_modifier`](crate::Style::with_modifier)
/// to apply modifiers to a style.
#[derive(Clone, Copy)]
pub struct Modifier(pub(crate) u16);

impl std::fmt::Debug for Modifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let values: &[(&str, Modifier)] = &[
            ("BOLD", Modifier::BOLD),
            ("DIM", Modifier::DIM),
            ("ITALIC", Modifier::ITALIC),
            ("UNDERLINED", Modifier::UNDERLINED),
            ("SLOW_BLINK", Modifier::SLOW_BLINK),
            ("RAPID_BLINK", Modifier::RAPID_BLINK),
            ("REVERSED", Modifier::REVERSED),
            ("HIDDEN", Modifier::HIDDEN),
            ("CROSSED_OUT", Modifier::CROSSED_OUT),
        ];
        let mut first = true;
        f.debug_set()
            .entries(values.iter().filter_map(|(name, val)| {
                if self.has(*val) {
                    if first {
                        first = false;
                    }
                    Some(*name)
                } else {
                    None
                }
            }))
            .finish()
    }
}
impl Modifier {
    /// Bold text.
    pub const BOLD: Modifier = Modifier(0b0000_0000_0001_00_000);
    /// Dim/faint text.
    pub const DIM: Modifier = Modifier(0b0000_0000_0010_00_000);
    /// Italic text.
    pub const ITALIC: Modifier = Modifier(0b0000_0000_0100_00_000);
    /// Underlined text.
    pub const UNDERLINED: Modifier = Modifier(0b0000_0000_1000_00_000);
    /// Slow blinking text.
    pub const SLOW_BLINK: Modifier = Modifier(0b0000_0001_0000_00_000);
    /// Rapid blinking text.
    pub const RAPID_BLINK: Modifier = Modifier(0b0000_0010_0000_00_000);
    /// Reversed foreground and background colors.
    pub const REVERSED: Modifier = Modifier(0b0000_0100_0000_00_000);
    /// Hidden/invisible text.
    pub const HIDDEN: Modifier = Modifier(0b0000_1000_0000_00_000);
    /// Crossed-out/strikethrough text.
    pub const CROSSED_OUT: Modifier = Modifier(0b0001_0000_0000_00_000);
    /// All modifiers combined.
    pub const ALL: Modifier = Modifier(0b0001_1111_1111_00_000);

    /// Returns `true` if no modifiers are set.
    pub fn is_empty(self) -> bool {
        self.0 == 0
    }
}

impl From<Modifier> for Style {
    fn from(value: Modifier) -> Self {
        Style(value.0 as u32)
    }
}
impl Modifier {
    /// Returns `true` if this modifier set contains the given modifier.
    pub fn has(self, other: Modifier) -> bool {
        (self.0 & other.0) != 0
    }
}

/// Writes modifier codes (1-9) with semicolon separators, returns pointer before trailing semicolon.
///
/// # Safety
/// Caller must ensure `ptr` has at least 18 bytes of writable space (9 modifiers * 2 bytes).
unsafe fn write_all_modifiers_inner(mut ptr: *mut u8, modifiers: Modifier) -> *mut u8 {
    unsafe {
        if modifiers.has(Modifier::BOLD) {
            *(ptr as *mut [u8; 2]) = [b'1', b';'];
            ptr = ptr.add(2);
        }
        if modifiers.has(Modifier::DIM) {
            *(ptr as *mut [u8; 2]) = [b'2', b';'];
            ptr = ptr.add(2);
        }
        if modifiers.has(Modifier::ITALIC) {
            *(ptr as *mut [u8; 2]) = [b'3', b';'];
            ptr = ptr.add(2);
        }
        if modifiers.has(Modifier::UNDERLINED) {
            *(ptr as *mut [u8; 2]) = [b'4', b';'];
            ptr = ptr.add(2);
        }
        if modifiers.has(Modifier::SLOW_BLINK) {
            *(ptr as *mut [u8; 2]) = [b'5', b';'];
            ptr = ptr.add(2);
        }
        if modifiers.has(Modifier::RAPID_BLINK) {
            *(ptr as *mut [u8; 2]) = [b'6', b';'];
            ptr = ptr.add(2);
        }
        if modifiers.has(Modifier::REVERSED) {
            *(ptr as *mut [u8; 2]) = [b'7', b';'];
            ptr = ptr.add(2);
        }
        if modifiers.has(Modifier::HIDDEN) {
            *(ptr as *mut [u8; 2]) = [b'8', b';'];
            ptr = ptr.add(2);
        }
        if modifiers.has(Modifier::CROSSED_OUT) {
            *(ptr as *mut [u8; 2]) = [b'9', b';'];
            ptr = ptr.add(2);
        }
        ptr.sub(1)
    }
}

/// Enables mouse event reporting (button press/release and drag).
pub const ENABLE_NON_MOTION_MOUSE_EVENTS: &[u8] = b"\x1b[?1000h\x1B[?1002h\x1b[?1015h\x1b[?1006h";
/// Disables mouse event reporting.
pub const DISABLE_NON_MOTION_MOUSE_EVENTS: &[u8] = b"\x1b[?1006l\x1b[?1015l\x1B[?1002l\x1b[?1000l";

/// Hides the terminal cursor.
pub const HIDE_CURSOR: &[u8] = b"\x1b[?25l";
/// Shows the terminal cursor.
pub const SHOW_CURSOR: &[u8] = b"\x1b[?25h";

/// Switches to the alternate screen buffer.
pub const ENABLE_ALT_SCREEN: &[u8] = b"\x1b[?1049h";
/// Returns to the primary screen buffer.
pub const DISABLE_ALT_SCREEN: &[u8] = b"\x1b[?1049l";
/// Pops the keyboard enhancement mode stack.
pub const POP_KEYBOARD_ENABLEMENT: &[u8] = b"\x1b[<1u";

impl BufferWrite for KeyboardEnhancementFlags {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        buffer.extend_from_slice(b"\x1b[>4;");
        itoap::write_to_vec(buffer, self.bits());
        buffer.extend_from_slice(b"m");
    }
}

impl BufferWrite for Style {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        style(buffer, *self, false);
    }
}

/// Defines a vertical scroll region within the terminal.
///
/// Scroll operations only affect lines within this region.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ScrollRegion(pub u16, pub u16);

impl ScrollRegion {
    /// Resets the scroll region to the full terminal height.
    pub const RESET: ScrollRegion = ScrollRegion(0, 0);
}
impl BufferWrite for ScrollRegion {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        if *self == ScrollRegion::RESET {
            buffer.extend_from_slice(b"\x1b[r");
            return;
        }
        buffer.extend_from_slice(b"\x1b[");
        itoap::write_to_vec(buffer, self.0);
        if self.1 != 0 {
            buffer.push(b';');
            itoap::write_to_vec(buffer, self.1);
        }
        buffer.push(b'r');
    }
}

impl BufferWrite for StyleDelta {
    fn write_to_buffer(&self, buffer: &mut Vec<u8>) {
        if self.current == u32::MAX {
            style(buffer, self.target, true);
            return;
        }
        if self.current == self.target.0 {
            return;
        }
        let removed = (self.target.0 | self.current) ^ self.target.0;
        let clearing = removed & (Style::HAS_BG | Style::HAS_FG | Modifier::ALL.0 as u32) != 0;
        if clearing {
            style(buffer, self.target, true);
            return;
        }
        let mut target = self.target;
        if Style(self.current).fg() == target.fg() {
            target = target.without_fg();
        }
        if Style(self.current).bg() == target.bg() {
            target = target.without_bg();
        }
        style(buffer, target, false);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_buffer_write_tests(cases: &[(&dyn BufferWrite, &[u8], &str)]) {
        for (writer, expected, desc) in cases {
            // Test from empty buffer (zero allocation)
            let mut buf = Vec::new();
            writer.write_to_buffer(&mut buf);
            assert_eq!(&buf, expected, "{desc} (empty buffer)");

            // Test with pre-existing contents
            let prefix = b"PREFIX";
            let mut buf = prefix.to_vec();
            writer.write_to_buffer(&mut buf);
            let mut expected_with_prefix = prefix.to_vec();
            expected_with_prefix.extend_from_slice(expected);
            assert_eq!(&buf, &expected_with_prefix, "{desc} (prefilled buffer)");
        }
    }

    #[test]
    fn buffer_write_sequences() {
        run_buffer_write_tests(&[
            // ScrollBufferUp
            (&ScrollBufferUp(5), b"\x1b[5S", "ScrollBufferUp(5)"),
            (&ScrollBufferUp(100), b"\x1b[100S", "ScrollBufferUp(100)"),
            (
                &ScrollBufferUp(65535),
                b"\x1b[65535S",
                "ScrollBufferUp(65535)",
            ),
            // ScrollBufferDown
            (&ScrollBufferDown(3), b"\x1b[3T", "ScrollBufferDown(3)"),
            (&ScrollBufferDown(1), b"\x1b[1T", "ScrollBufferDown(1)"),
            // MoveCursor
            (&MoveCursor(0, 0), b"\x1b[1;1H", "MoveCursor(0, 0)"),
            (&MoveCursor(10, 20), b"\x1b[21;11H", "MoveCursor(10, 20)"),
            (&MoveCursor(99, 49), b"\x1b[50;100H", "MoveCursor(99, 49)"),
            // MoveCursorRight
            (&MoveCursorRight(1), b"\x1b[1C", "MoveCursorRight(1)"),
            (&MoveCursorRight(50), b"\x1b[50C", "MoveCursorRight(50)"),
            // MoveCursorLeft
            (&MoveCursorLeft(1), b"\x1b[1D", "MoveCursorLeft(1)"),
            (&MoveCursorLeft(25), b"\x1b[25D", "MoveCursorLeft(25)"),
            // MoveCursorUp
            (&MoveCursorUp(1), b"\x1b[1A", "MoveCursorUp(1)"),
            (&MoveCursorUp(10), b"\x1b[10A", "MoveCursorUp(10)"),
            // MoveCursorDown
            (&MoveCursorDown(1), b"\x1b[1B", "MoveCursorDown(1)"),
            (&MoveCursorDown(10), b"\x1b[10B", "MoveCursorDown(10)"),
            // MoveCursorNextLine
            (&MoveCursorNextLine(1), b"\x1b[1E", "MoveCursorNextLine(1)"),
            (&MoveCursorNextLine(5), b"\x1b[5E", "MoveCursorNextLine(5)"),
            // MoveCursorPrevLine
            (&MoveCursorPrevLine(1), b"\x1b[1F", "MoveCursorPrevLine(1)"),
            (&MoveCursorPrevLine(5), b"\x1b[5F", "MoveCursorPrevLine(5)"),
            // MoveCursorToColumn
            (
                &MoveCursorToColumn(0),
                b"\x1b[1G",
                "MoveCursorToColumn(0)",
            ),
            (
                &MoveCursorToColumn(79),
                b"\x1b[80G",
                "MoveCursorToColumn(79)",
            ),
            // InsertLines
            (&InsertLines(1), b"\x1b[1L", "InsertLines(1)"),
            (&InsertLines(10), b"\x1b[10L", "InsertLines(10)"),
            // DeleteLines
            (&DeleteLines(1), b"\x1b[1M", "DeleteLines(1)"),
            (&DeleteLines(10), b"\x1b[10M", "DeleteLines(10)"),
            // InsertChars
            (&InsertChars(1), b"\x1b[1@", "InsertChars(1)"),
            (&InsertChars(5), b"\x1b[5@", "InsertChars(5)"),
            // DeleteChars
            (&DeleteChars(1), b"\x1b[1P", "DeleteChars(1)"),
            (&DeleteChars(5), b"\x1b[5P", "DeleteChars(5)"),
            // EraseChars
            (&EraseChars(1), b"\x1b[1X", "EraseChars(1)"),
            (&EraseChars(10), b"\x1b[10X", "EraseChars(10)"),
            // ScrollRegion
            (&ScrollRegion(1, 24), b"\x1b[1;24r", "ScrollRegion(1, 24)"),
            (&ScrollRegion::RESET, b"\x1b[r", "ScrollRegion::RESET"),
            // Style with fg
            (
                &Style::DEFAULT.with_fg(crate::Color(196)),
                b"\x1b[38;5;196m".as_slice(),
                "Style with fg color 196",
            ),
            // Style with modifiers
            (
                &Style::DEFAULT.with_modifier(Modifier::BOLD),
                b"\x1b[1m".as_slice(),
                "Style with BOLD",
            ),
            (
                &Style::DEFAULT
                    .with_modifier(Modifier::BOLD)
                    .with_modifier(Modifier::ITALIC),
                b"\x1b[1;3m".as_slice(),
                "Style with BOLD and ITALIC",
            ),
            // Style default (no output)
            (
                &Style::DEFAULT,
                b"".as_slice(),
                "Style::DEFAULT (no output)",
            ),
        ]);
    }
}
