//! Normalized key representation used by the router.
//!
//! [`InputKey`] packs a key code and its modifier bits into a single
//! `u64` so that lookup comparisons are a single integer compare and so
//! that shift-folding is applied consistently (pressing `Shift+a` hashes
//! and compares equal to pressing `A`). [`NamedKey`] enumerates the
//! non-character keys the router recognises.

use extui::event::{KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use std::fmt;

const MODS_SHIFT: u32 = 32;
const TAG_SHIFT: u32 = 40;
const PAYLOAD_MASK: u64 = 0x0000_0000_FFFF_FFFF;
const MODS_MASK: u64 = 0xFF << MODS_SHIFT;
const TAG_MASK: u64 = 0xFF << TAG_SHIFT;

const TAG_CHAR: u64 = 0 << TAG_SHIFT;
const TAG_NAMED: u64 = 1 << TAG_SHIFT;

/// A non-character key recognised by the router.
///
/// The discriminant is used directly as the payload of [`InputKey`] for
/// `Named` keys, so variant ordering is part of the in-memory layout:
/// reordering or removing a variant changes the bit pattern produced by
/// [`InputKey::named`].
///
/// # Examples
///
/// ```
/// use extui_bindings::{InputKey, NamedKey};
/// use extui::event::KeyModifiers;
///
/// let esc = InputKey::named(NamedKey::Esc, KeyModifiers::empty());
/// assert_eq!(esc.as_named(), Some(NamedKey::Esc));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
#[repr(u32)]
pub enum NamedKey {
    Backspace = 0,
    Enter,
    Left,
    Right,
    Up,
    Down,
    Home,
    End,
    PageUp,
    PageDown,
    Tab,
    BackTab,
    Delete,
    Insert,
    Null,
    Esc,
    CapsLock,
    ScrollLock,
    NumLock,
    PrintScreen,
    Pause,
    Menu,
    KeypadBegin,
    Play,
    PauseMedia,
    PlayPause,
    Reverse,
    Stop,
    FastForward,
    Rewind,
    TrackNext,
    TrackPrevious,
    Record,
    LowerVolume,
    RaiseVolume,
    MuteVolume,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    F13,
    F14,
    F15,
    F16,
    F17,
    F18,
    F19,
    F20,
    F21,
    F22,
    F23,
    F24,
    F25,
    F26,
    F27,
    F28,
    F29,
    F30,
    F31,
    F32,
    F33,
    F34,
    F35,
}

const NAMED_LAST: u32 = NamedKey::F35 as u32;

impl NamedKey {
    /// Returns the canonical CamelCase name of this key.
    ///
    /// The returned string matches the body of the vim-style
    /// angle-bracket token for the key (e.g. `"Esc"` corresponds to
    /// `<Esc>`) and is the spelling [`fmt::Display`] emits for
    /// [`InputKey`].
    ///
    /// # Examples
    ///
    /// ```
    /// use extui_bindings::NamedKey;
    ///
    /// assert_eq!(NamedKey::PageUp.canonical_name(), "PageUp");
    /// assert_eq!(NamedKey::F12.canonical_name(), "F12");
    /// ```
    pub const fn canonical_name(self) -> &'static str {
        match self {
            NamedKey::Backspace => "Backspace",
            NamedKey::Enter => "Enter",
            NamedKey::Left => "Left",
            NamedKey::Right => "Right",
            NamedKey::Up => "Up",
            NamedKey::Down => "Down",
            NamedKey::Home => "Home",
            NamedKey::End => "End",
            NamedKey::PageUp => "PageUp",
            NamedKey::PageDown => "PageDown",
            NamedKey::Tab => "Tab",
            NamedKey::BackTab => "BackTab",
            NamedKey::Delete => "Delete",
            NamedKey::Insert => "Insert",
            NamedKey::Null => "Null",
            NamedKey::Esc => "Esc",
            NamedKey::CapsLock => "CapsLock",
            NamedKey::ScrollLock => "ScrollLock",
            NamedKey::NumLock => "NumLock",
            NamedKey::PrintScreen => "PrintScreen",
            NamedKey::Pause => "Pause",
            NamedKey::Menu => "Menu",
            NamedKey::KeypadBegin => "KeypadBegin",
            NamedKey::Play => "Play",
            NamedKey::PauseMedia => "PauseMedia",
            NamedKey::PlayPause => "PlayPause",
            NamedKey::Reverse => "Reverse",
            NamedKey::Stop => "Stop",
            NamedKey::FastForward => "FastForward",
            NamedKey::Rewind => "Rewind",
            NamedKey::TrackNext => "TrackNext",
            NamedKey::TrackPrevious => "TrackPrevious",
            NamedKey::Record => "Record",
            NamedKey::LowerVolume => "LowerVolume",
            NamedKey::RaiseVolume => "RaiseVolume",
            NamedKey::MuteVolume => "MuteVolume",
            NamedKey::F1 => "F1",
            NamedKey::F2 => "F2",
            NamedKey::F3 => "F3",
            NamedKey::F4 => "F4",
            NamedKey::F5 => "F5",
            NamedKey::F6 => "F6",
            NamedKey::F7 => "F7",
            NamedKey::F8 => "F8",
            NamedKey::F9 => "F9",
            NamedKey::F10 => "F10",
            NamedKey::F11 => "F11",
            NamedKey::F12 => "F12",
            NamedKey::F13 => "F13",
            NamedKey::F14 => "F14",
            NamedKey::F15 => "F15",
            NamedKey::F16 => "F16",
            NamedKey::F17 => "F17",
            NamedKey::F18 => "F18",
            NamedKey::F19 => "F19",
            NamedKey::F20 => "F20",
            NamedKey::F21 => "F21",
            NamedKey::F22 => "F22",
            NamedKey::F23 => "F23",
            NamedKey::F24 => "F24",
            NamedKey::F25 => "F25",
            NamedKey::F26 => "F26",
            NamedKey::F27 => "F27",
            NamedKey::F28 => "F28",
            NamedKey::F29 => "F29",
            NamedKey::F30 => "F30",
            NamedKey::F31 => "F31",
            NamedKey::F32 => "F32",
            NamedKey::F33 => "F33",
            NamedKey::F34 => "F34",
            NamedKey::F35 => "F35",
        }
    }

    fn from_keycode(code: KeyCode) -> Option<NamedKey> {
        let key = match code {
            KeyCode::Backspace => NamedKey::Backspace,
            KeyCode::Enter => NamedKey::Enter,
            KeyCode::Left => NamedKey::Left,
            KeyCode::Right => NamedKey::Right,
            KeyCode::Up => NamedKey::Up,
            KeyCode::Down => NamedKey::Down,
            KeyCode::Home => NamedKey::Home,
            KeyCode::End => NamedKey::End,
            KeyCode::PageUp => NamedKey::PageUp,
            KeyCode::PageDown => NamedKey::PageDown,
            KeyCode::Tab => NamedKey::Tab,
            KeyCode::BackTab => NamedKey::BackTab,
            KeyCode::Delete => NamedKey::Delete,
            KeyCode::Insert => NamedKey::Insert,
            KeyCode::Null => NamedKey::Null,
            KeyCode::Esc => NamedKey::Esc,
            KeyCode::CapsLock => NamedKey::CapsLock,
            KeyCode::ScrollLock => NamedKey::ScrollLock,
            KeyCode::NumLock => NamedKey::NumLock,
            KeyCode::PrintScreen => NamedKey::PrintScreen,
            KeyCode::Pause => NamedKey::Pause,
            KeyCode::Menu => NamedKey::Menu,
            KeyCode::KeypadBegin => NamedKey::KeypadBegin,
            KeyCode::Play => NamedKey::Play,
            KeyCode::PauseMedia => NamedKey::PauseMedia,
            KeyCode::PlayPause => NamedKey::PlayPause,
            KeyCode::Reverse => NamedKey::Reverse,
            KeyCode::Stop => NamedKey::Stop,
            KeyCode::FastForward => NamedKey::FastForward,
            KeyCode::Rewind => NamedKey::Rewind,
            KeyCode::TrackNext => NamedKey::TrackNext,
            KeyCode::TrackPrevious => NamedKey::TrackPrevious,
            KeyCode::Record => NamedKey::Record,
            KeyCode::LowerVolume => NamedKey::LowerVolume,
            KeyCode::RaiseVolume => NamedKey::RaiseVolume,
            KeyCode::MuteVolume => NamedKey::MuteVolume,
            KeyCode::F1 => NamedKey::F1,
            KeyCode::F2 => NamedKey::F2,
            KeyCode::F3 => NamedKey::F3,
            KeyCode::F4 => NamedKey::F4,
            KeyCode::F5 => NamedKey::F5,
            KeyCode::F6 => NamedKey::F6,
            KeyCode::F7 => NamedKey::F7,
            KeyCode::F8 => NamedKey::F8,
            KeyCode::F9 => NamedKey::F9,
            KeyCode::F10 => NamedKey::F10,
            KeyCode::F11 => NamedKey::F11,
            KeyCode::F12 => NamedKey::F12,
            KeyCode::F13 => NamedKey::F13,
            KeyCode::F14 => NamedKey::F14,
            KeyCode::F15 => NamedKey::F15,
            KeyCode::F16 => NamedKey::F16,
            KeyCode::F17 => NamedKey::F17,
            KeyCode::F18 => NamedKey::F18,
            KeyCode::F19 => NamedKey::F19,
            KeyCode::F20 => NamedKey::F20,
            KeyCode::F21 => NamedKey::F21,
            KeyCode::F22 => NamedKey::F22,
            KeyCode::F23 => NamedKey::F23,
            KeyCode::F24 => NamedKey::F24,
            KeyCode::F25 => NamedKey::F25,
            KeyCode::F26 => NamedKey::F26,
            KeyCode::F27 => NamedKey::F27,
            KeyCode::F28 => NamedKey::F28,
            KeyCode::F29 => NamedKey::F29,
            KeyCode::F30 => NamedKey::F30,
            KeyCode::F31 => NamedKey::F31,
            KeyCode::F32 => NamedKey::F32,
            KeyCode::F33 => NamedKey::F33,
            KeyCode::F34 => NamedKey::F34,
            KeyCode::F35 => NamedKey::F35,
            _ => return None,
        };
        Some(key)
    }

    fn from_raw(raw: u32) -> Option<NamedKey> {
        if raw > NAMED_LAST {
            return None;
        }
        Some(unsafe { std::mem::transmute::<u32, NamedKey>(raw) })
    }

    /// Returns the function-key variant for `n`, where `1 <= n <= 35`
    /// maps to [`NamedKey::F1`] through [`NamedKey::F35`].
    ///
    /// Returns [`None`] for any `n` outside that range.
    ///
    /// # Examples
    ///
    /// ```
    /// use extui_bindings::NamedKey;
    ///
    /// assert_eq!(NamedKey::from_f_number(5), Some(NamedKey::F5));
    /// assert_eq!(NamedKey::from_f_number(0), None);
    /// assert_eq!(NamedKey::from_f_number(36), None);
    /// ```
    pub fn from_f_number(n: u32) -> Option<NamedKey> {
        if n < 1 || n > 35 {
            return None;
        }
        NamedKey::from_raw(NamedKey::F1 as u32 + n - 1)
    }
}

/// A key press encoded as a single `u64` for fast lookup.
///
/// The packed layout is:
///
/// | bits      | meaning                                                           |
/// |-----------|-------------------------------------------------------------------|
/// | `0..32`   | payload: Unicode codepoint for `Char`, variant index for `Named`  |
/// | `32..40`  | [`KeyModifiers`] bits (with `SHIFT` folded into the character)    |
/// | `40..48`  | tag: `0 = Char`, `1 = Named`                                      |
/// | `48..64`  | reserved                                                          |
///
/// Shift is normalized away from `Char` keys whose payload is an ASCII
/// letter: `Shift+a` and `A` are stored identically, and the `SHIFT`
/// bit is cleared. This matches the normalization performed by the
/// [`PartialEq`] and [`Hash`] impls of [`extui::event::KeyEvent`], so
/// `InputKey` values derived from events compare and hash consistently
/// with the source events.
///
/// # Examples
///
/// ```
/// use extui_bindings::InputKey;
/// use extui::event::KeyModifiers;
///
/// let ctrl_c = InputKey::char('c', KeyModifiers::CONTROL);
/// assert_eq!(ctrl_c.as_char(), Some('c'));
/// assert_eq!(ctrl_c.modifiers(), KeyModifiers::CONTROL);
///
/// // Shift folding: these three constructions produce the same key.
/// assert_eq!(
///     InputKey::char('a', KeyModifiers::SHIFT),
///     InputKey::char('A', KeyModifiers::empty()),
/// );
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct InputKey(pub u64);

impl InputKey {
    /// Returns an `InputKey` for a character press with the given modifiers.
    ///
    /// Shift is folded into the character case for ASCII letters: if
    /// `c` is lowercase and `mods` contains `SHIFT`, the character is
    /// uppercased and `SHIFT` is cleared; if `c` is already uppercase,
    /// `SHIFT` is cleared from `mods`. Non-letter characters are left
    /// alone.
    ///
    /// # Examples
    ///
    /// ```
    /// use extui_bindings::InputKey;
    /// use extui::event::KeyModifiers;
    ///
    /// let lower = InputKey::char('a', KeyModifiers::empty());
    /// let shifted = InputKey::char('a', KeyModifiers::SHIFT);
    /// let upper = InputKey::char('A', KeyModifiers::empty());
    /// assert_eq!(shifted, upper);
    /// assert_ne!(lower, upper);
    /// ```
    pub const fn char(c: char, mods: KeyModifiers) -> InputKey {
        let (c, mods) = if c.is_ascii_lowercase() && mods.contains(KeyModifiers::SHIFT) {
            (c.to_ascii_uppercase(), mods.difference(KeyModifiers::SHIFT))
        } else if c.is_ascii_uppercase() {
            (c, mods.difference(KeyModifiers::SHIFT))
        } else {
            (c, mods)
        };
        InputKey(TAG_CHAR | ((mods.bits() as u64) << MODS_SHIFT) | (c as u64))
    }

    /// Returns an `InputKey` for a [`NamedKey`] press with the given
    /// modifiers.
    ///
    /// Unlike [`InputKey::char`], no case folding is applied: the stored
    /// modifiers are exactly `mods`.
    ///
    /// # Examples
    ///
    /// ```
    /// use extui_bindings::{InputKey, NamedKey};
    /// use extui::event::KeyModifiers;
    ///
    /// let tab = InputKey::named(NamedKey::Tab, KeyModifiers::SHIFT);
    /// assert_eq!(tab.as_named(), Some(NamedKey::Tab));
    /// assert_eq!(tab.modifiers(), KeyModifiers::SHIFT);
    /// ```
    pub const fn named(name: NamedKey, mods: KeyModifiers) -> InputKey {
        InputKey(TAG_NAMED | ((mods.bits() as u64) << MODS_SHIFT) | (name as u64))
    }

    /// Returns the [`KeyModifiers`] stored in this key.
    ///
    /// The returned value never contains `SHIFT` for `Char` keys whose
    /// payload is an ASCII letter — the shift is encoded by the letter
    /// case instead. See [`InputKey::char`] for details.
    pub const fn modifiers(self) -> KeyModifiers {
        KeyModifiers::from_bits_truncate(((self.0 & MODS_MASK) >> MODS_SHIFT) as u8)
    }

    /// Returns the character payload if this key was built from a
    /// character, or [`None`] if it was built from a [`NamedKey`].
    ///
    /// # Examples
    ///
    /// ```
    /// use extui_bindings::{InputKey, NamedKey};
    /// use extui::event::KeyModifiers;
    ///
    /// let x = InputKey::char('x', KeyModifiers::empty());
    /// let esc = InputKey::named(NamedKey::Esc, KeyModifiers::empty());
    /// assert_eq!(x.as_char(), Some('x'));
    /// assert_eq!(esc.as_char(), None);
    /// ```
    #[inline]
    pub const fn as_char(self) -> Option<char> {
        if (self.0 & TAG_MASK) != TAG_CHAR {
            return None;
        }
        char::from_u32((self.0 & PAYLOAD_MASK) as u32)
    }

    /// Returns the [`NamedKey`] payload if this key was built from a
    /// named key, or [`None`] if it was built from a character.
    ///
    /// # Examples
    ///
    /// ```
    /// use extui_bindings::{InputKey, NamedKey};
    /// use extui::event::KeyModifiers;
    ///
    /// let esc = InputKey::named(NamedKey::Esc, KeyModifiers::empty());
    /// let x = InputKey::char('x', KeyModifiers::empty());
    /// assert_eq!(esc.as_named(), Some(NamedKey::Esc));
    /// assert_eq!(x.as_named(), None);
    /// ```
    pub fn as_named(self) -> Option<NamedKey> {
        if (self.0 & TAG_MASK) != TAG_NAMED {
            return None;
        }
        NamedKey::from_raw((self.0 & PAYLOAD_MASK) as u32)
    }

    /// Returns the digit `0..=9` if this key is a bare ASCII digit with
    /// no modifiers, or [`None`] otherwise.
    ///
    /// Any modifier (including `CONTROL` or `ALT`) or a non-digit
    /// character yields [`None`].
    ///
    /// # Examples
    ///
    /// ```
    /// use extui_bindings::InputKey;
    /// use extui::event::KeyModifiers;
    ///
    /// assert_eq!(InputKey::char('3', KeyModifiers::empty()).as_bare_digit(), Some(3));
    /// assert_eq!(InputKey::char('3', KeyModifiers::CONTROL).as_bare_digit(), None);
    /// assert_eq!(InputKey::char('a', KeyModifiers::empty()).as_bare_digit(), None);
    /// ```
    #[inline]
    pub const fn as_bare_digit(self) -> Option<u8> {
        if (self.0 & (TAG_MASK | MODS_MASK)) != TAG_CHAR {
            return None;
        }
        let payload = (self.0 & PAYLOAD_MASK) as u32;
        if payload >= b'0' as u32 && payload <= b'9' as u32 {
            Some(payload as u8 - b'0')
        } else {
            None
        }
    }

    /// Returns `true` if this key is a bare ASCII digit in `1..=9`.
    ///
    /// `0` is deliberately excluded: vim-style count layers treat a
    /// leading `0` as a motion (go to first column) rather than as the
    /// first digit of a count, so callers discriminating "is this the
    /// start of a count?" must reject it.
    ///
    /// # Examples
    ///
    /// ```
    /// use extui_bindings::InputKey;
    /// use extui::event::KeyModifiers;
    ///
    /// assert!(InputKey::char('1', KeyModifiers::empty()).is_count_starter());
    /// assert!(!InputKey::char('0', KeyModifiers::empty()).is_count_starter());
    /// assert!(!InputKey::char('a', KeyModifiers::empty()).is_count_starter());
    /// ```
    pub const fn is_count_starter(self) -> bool {
        match self.as_bare_digit() {
            Some(d) if d >= 1 => true,
            _ => false,
        }
    }

    /// Returns an `InputKey` derived from `evt`, or [`None`] if the
    /// event cannot drive a binding.
    ///
    /// An event is rejected when it is:
    ///
    /// - a release event ([`KeyEventKind::Release`]),
    /// - a modifier-only press (e.g. a bare `Shift`), or
    /// - carries a [`KeyCode`] outside the representable set.
    ///
    /// # Examples
    ///
    /// ```
    /// use extui_bindings::InputKey;
    /// use extui::event::{KeyCode, KeyEvent, KeyModifiers};
    ///
    /// let evt = KeyEvent::new(KeyCode::Char('k'), KeyModifiers::empty());
    /// let key = InputKey::from_event(&evt).unwrap();
    /// assert_eq!(key.as_char(), Some('k'));
    /// ```
    #[inline]
    pub fn from_event(evt: &KeyEvent) -> Option<InputKey> {
        if matches!(evt.kind, KeyEventKind::Release) {
            return None;
        }
        match evt.code {
            KeyCode::Char(c) => Some(InputKey::char(c, evt.modifiers)),
            code => {
                let Some(name) = NamedKey::from_keycode(code) else {
                    return None;
                };
                Some(InputKey::named(name, evt.modifiers))
            }
        }
    }
}

impl fmt::Display for InputKey {
    /// Formats the key using the same tokens accepted by [`parse_key`].
    ///
    /// Bare character keys format as the character itself (`a`, `A`)
    /// and named keys as their canonical name (`Esc`, `F12`). Modifiers
    /// are emitted as `C-`, `M-`, `D-`, `H-`, `T-`, `S-` prefixes in
    /// that order, and `' '` is spelled `Space`.
    ///
    /// [`parse_key`]: crate::parse::parse_key
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mods = self.modifiers();
        let has_ctrl = mods.contains(KeyModifiers::CONTROL);
        let has_alt = mods.contains(KeyModifiers::ALT);
        let has_super = mods.contains(KeyModifiers::SUPER);
        let has_hyper = mods.contains(KeyModifiers::HYPER);
        let has_meta = mods.contains(KeyModifiers::META);
        let has_shift = mods.contains(KeyModifiers::SHIFT);

        let prefix_len = has_ctrl as usize
            + has_alt as usize
            + has_super as usize
            + has_hyper as usize
            + has_meta as usize
            + has_shift as usize;

        if let Some(c) = self.as_char() {
            if c == ' ' {
                if prefix_len == 0 {
                    return f.write_str("Space");
                }
                write_mods(f, mods)?;
                return f.write_str("Space");
            }
            if prefix_len == 0 {
                return f.write_fmt(format_args!("{c}"));
            }
            write_mods(f, mods)?;
            return f.write_fmt(format_args!("{c}"));
        }

        if let Some(name) = self.as_named() {
            write_mods(f, mods)?;
            return f.write_str(name.canonical_name());
        }

        f.write_str("Unknown")
    }
}

fn write_mods(f: &mut fmt::Formatter<'_>, mods: KeyModifiers) -> fmt::Result {
    if mods.contains(KeyModifiers::CONTROL) {
        f.write_str("C-")?;
    }
    if mods.contains(KeyModifiers::ALT) {
        f.write_str("M-")?;
    }
    if mods.contains(KeyModifiers::SUPER) {
        f.write_str("D-")?;
    }
    if mods.contains(KeyModifiers::HYPER) {
        f.write_str("H-")?;
    }
    if mods.contains(KeyModifiers::META) {
        f.write_str("T-")?;
    }
    if mods.contains(KeyModifiers::SHIFT) {
        f.write_str("S-")?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shift_fold_upper_equals_shifted_lower() {
        let a = InputKey::char('A', KeyModifiers::empty());
        let b = InputKey::char('a', KeyModifiers::SHIFT);
        assert_eq!(a, b);
        assert!(!a.modifiers().contains(KeyModifiers::SHIFT));
    }

    #[test]
    fn ctrl_c_roundtrip() {
        let k = InputKey::char('c', KeyModifiers::CONTROL);
        assert_eq!(k.as_char(), Some('c'));
        assert_eq!(k.modifiers(), KeyModifiers::CONTROL);
    }

    #[test]
    fn named_key_roundtrip() {
        let k = InputKey::named(NamedKey::F10, KeyModifiers::ALT);
        assert_eq!(k.as_named(), Some(NamedKey::F10));
        assert_eq!(k.modifiers(), KeyModifiers::ALT);
        assert!(k.as_char().is_none());
    }

    #[test]
    fn bare_digit_detection() {
        assert_eq!(
            InputKey::char('3', KeyModifiers::empty()).as_bare_digit(),
            Some(3)
        );
        assert_eq!(
            InputKey::char('0', KeyModifiers::empty()).as_bare_digit(),
            Some(0)
        );
        assert_eq!(
            InputKey::char('3', KeyModifiers::CONTROL).as_bare_digit(),
            None
        );
        assert_eq!(
            InputKey::char('a', KeyModifiers::empty()).as_bare_digit(),
            None
        );
    }

    #[test]
    fn count_starter_excludes_zero() {
        assert!(InputKey::char('1', KeyModifiers::empty()).is_count_starter());
        assert!(!InputKey::char('0', KeyModifiers::empty()).is_count_starter());
    }

    #[test]
    fn from_event_drops_release() {
        let mut evt = KeyEvent::new(KeyCode::Char('k'), KeyModifiers::empty());
        evt.kind = KeyEventKind::Release;
        assert!(InputKey::from_event(&evt).is_none());
    }

    #[test]
    fn from_event_named() {
        let evt = KeyEvent::new(KeyCode::Esc, KeyModifiers::empty());
        let k = InputKey::from_event(&evt).unwrap();
        assert_eq!(k.as_named(), Some(NamedKey::Esc));
    }

    #[test]
    fn display_formats() {
        assert_eq!(InputKey::char('a', KeyModifiers::empty()).to_string(), "a");
        assert_eq!(
            InputKey::char('c', KeyModifiers::CONTROL).to_string(),
            "C-c"
        );
        assert_eq!(InputKey::char('A', KeyModifiers::empty()).to_string(), "A");
        assert_eq!(
            InputKey::named(NamedKey::Esc, KeyModifiers::empty()).to_string(),
            "Esc"
        );
        assert_eq!(
            InputKey::char(' ', KeyModifiers::CONTROL).to_string(),
            "C-Space"
        );
    }
}
