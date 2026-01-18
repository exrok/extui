use std::os::fd::AsRawFd;

use crate::event::{
    Event, InternalEvent, KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers,
    KeyboardEnhancementFlags, MouseButton, MouseEvent, MouseEventKind, polling::resize_count,
};

/// Buffered terminal event reader and parser.
///
/// Reads raw input from a file descriptor and parses it into [`Event`]s.
pub struct Events {
    buffer: Vec<u8>,
    read: usize,
    resize_count: u64,
}

impl Default for Events {
    fn default() -> Self {
        Self {
            buffer: Default::default(),
            read: Default::default(),
            resize_count: resize_count(),
        }
    }
}
impl Events {
    /// Reads available input from the file descriptor into the internal buffer.
    ///
    /// # Errors
    ///
    /// Returns an error if the read operation fails.
    pub fn read_from(&mut self, fd: &impl AsRawFd) -> std::io::Result<()> {
        fn read_from_inner(events: &mut Events, fd: i32) -> std::io::Result<()> {
            if events.read >= events.buffer.len() {
                events.buffer.clear();
                events.read = 0;
            }
            let mut target = events.buffer.spare_capacity_mut();
            if target.len() < 16 {
                events.buffer.reserve(1024);
                target = events.buffer.spare_capacity_mut();
            }
            unsafe {
                // use libc read to avoid needed to initialize buffer
                let ret = libc::read(fd, target.as_mut_ptr() as *mut _, target.len());
                if ret < 0 {
                    return Err(std::io::Error::last_os_error());
                }
                let len = events.buffer.len();
                events.buffer.set_len(len + ret as usize);
                Ok(())
            }
        }
        read_from_inner(self, fd.as_raw_fd())
    }
    /// Parses and returns the next event from the buffer.
    ///
    /// Returns `None` if no complete event is available.
    pub fn next(&mut self, in_raw_mode: bool) -> Option<Event> {
        loop {
            let remaining = &self.buffer[self.read..];
            match parse_event(remaining, in_raw_mode) {
                ParseResult::Event { consumed, event } => {
                    self.read += consumed as usize;
                    if let InternalEvent::Event(event) = event { return Some(event) }
                }
                ParseResult::Incomplete => {
                    let new_resize_count = resize_count();
                    if new_resize_count != self.resize_count {
                        self.resize_count = new_resize_count;
                        return Some(Event::Resized);
                    }
                    return None;
                }
                ParseResult::Error { consumed, .. } => {
                    self.read += consumed as usize;
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    /// The input buffer contains an invalid UTF-8 sequence.
    InvalidUtf8,
    /// A generic error for when a sequence could not be parsed.
    CouldNotParse,
    /// An error occurred while parsing a number from a sequence.
    InvalidNumber,
    /// An error occurred while parsing mouse input bytes.
    InvalidMouseInput,
}

/// Represents the result of a parsing operation.
///
/// `Consumed` refers to the number of bytes consumed from the input buffer.
/// A value of zero for `consumed` in the `Error` variant means the entire
/// buffer should be considered consumed and discarded.
#[derive(Debug, PartialEq, Eq)]
pub enum ParseResult {
    /// A complete event was parsed successfully.
    Event { consumed: u32, event: InternalEvent },
    /// The input buffer is incomplete and more data is needed to parse an event.
    Incomplete,
    /// An error occurred during parsing.
    Error { consumed: u32, error: ParseError },
}

const fn pressed(code: KeyCode) -> InternalEvent {
    InternalEvent::Event(Event::Key(KeyEvent {
        code,
        modifiers: KeyModifiers::empty(),
        kind: KeyEventKind::Press,
        state: KeyEventState::empty(),
    }))
}

pub fn parse_event(buffer: &[u8], is_raw: bool) -> ParseResult {
    let Some(&first) = buffer.first() else {
        return ParseResult::Incomplete;
    };

    match first {
        b'\x1B' => {
            let Some(&second) = buffer.get(1) else {
                return ParseResult::Event {
                    consumed: 1,
                    event: pressed(KeyCode::Esc),
                };
            };
            match second {
                b'O' => {
                    let Some(&third) = buffer.get(2) else {
                        return ParseResult::Incomplete;
                    };
                    let key_code = match third {
                        b'D' => KeyCode::Left,
                        b'C' => KeyCode::Right,
                        b'A' => KeyCode::Up,
                        b'B' => KeyCode::Down,
                        b'H' => KeyCode::Home,
                        b'F' => KeyCode::End,
                        val @ b'P'..=b'S' => KeyCode::function(1 + val - b'P'),
                        _ => {
                            return ParseResult::Error {
                                consumed: 3,
                                error: ParseError::CouldNotParse,
                            };
                        }
                    };
                    ParseResult::Event {
                        consumed: 3,
                        event: pressed(key_code),
                    }
                }
                b'[' => parse_csi(buffer, is_raw),
                b'\x1B' => ParseResult::Event {
                    consumed: 2,
                    event: pressed(KeyCode::Esc),
                },
                _ => {
                    // Alt + key
                    match parse_event(&buffer[1..], is_raw) {
                        ParseResult::Event { consumed, event } => {
                            let new_event =
                                if let InternalEvent::Event(Event::Key(key_event)) = event {
                                    let mut alt_key_event = key_event;
                                    alt_key_event.modifiers |= KeyModifiers::ALT;
                                    InternalEvent::Event(Event::Key(alt_key_event))
                                } else {
                                    event
                                };
                            ParseResult::Event {
                                consumed: consumed + 1,
                                event: new_event,
                            }
                        }
                        ParseResult::Error { consumed, error } => {
                            // Propagate error, adjusting consumed count for the ESC byte
                            ParseResult::Error {
                                consumed: consumed + 1,
                                error,
                            }
                        }
                        ParseResult::Incomplete => ParseResult::Incomplete,
                    }
                }
            }
        }
        b'\r' => ParseResult::Event {
            consumed: 1,
            event: pressed(KeyCode::Enter),
        },
        b'\n' if !is_raw => ParseResult::Event {
            consumed: 1,
            event: pressed(KeyCode::Enter),
        },
        b'\t' => ParseResult::Event {
            consumed: 1,
            event: pressed(KeyCode::Tab),
        },
        b'\x7F' => ParseResult::Event {
            consumed: 1,
            event: pressed(KeyCode::Backspace),
        },
        c @ b'\x01'..=b'\x1A' => ParseResult::Event {
            consumed: 1,
            event: InternalEvent::Event(Event::Key(KeyEvent::new(
                KeyCode::Char((c - 0x1 + b'a') as char),
                KeyModifiers::CONTROL,
            ))),
        },
        c @ b'\x1C'..=b'\x1F' => ParseResult::Event {
            consumed: 1,
            event: InternalEvent::Event(Event::Key(KeyEvent::new(
                KeyCode::Char((c - 0x1C + b'4') as char),
                KeyModifiers::CONTROL,
            ))),
        },
        b'\0' => ParseResult::Event {
            consumed: 1,
            event: InternalEvent::Event(Event::Key(KeyEvent::new(
                KeyCode::Char(' '),
                KeyModifiers::CONTROL,
            ))),
        },
        _ => match parse_utf8_char(buffer) {
            Ok(Some((ch, consumed))) => ParseResult::Event {
                consumed: consumed as u32,
                event: InternalEvent::Event(Event::Key(char_code_to_event(KeyCode::Char(ch)))),
            },
            Ok(None) => ParseResult::Incomplete,
            Err(error) => ParseResult::Error { consumed: 1, error },
        },
    }
}

// converts KeyCode to KeyEvent (adds shift modifier in case of uppercase characters)
fn char_code_to_event(code: KeyCode) -> KeyEvent {
    let modifiers = match code {
        KeyCode::Char(c) if c.is_uppercase() => KeyModifiers::SHIFT,
        _ => KeyModifiers::empty(),
    };
    KeyEvent::new(code, modifiers)
}

pub(crate) fn parse_csi(buffer: &[u8], is_raw: bool) -> ParseResult {
    assert!(buffer.starts_with(b"\x1B[")); // ESC [

    if buffer.len() < 3 {
        return ParseResult::Incomplete;
    }

    // Handle special fixed-length cases that don't follow the standard CSI parameter format.
    match buffer[2] {
        b'M' => return parse_csi_normal_mouse(buffer),
        #[cfg(feature = "bracketed-paste")]
        b'2' if buffer.starts_with(b"\x1B[200~") => {
            return parse_csi_bracketed_paste(buffer);
        }
        _ => {}
    }

    // For variable-length CSI sequences, find the terminating byte.
    let end_index = match buffer
        .iter()
        .skip(2)
        .position(|&b| (0x40..=0x7E).contains(&b))
    {
        Some(i) => 2 + i,
        None => return ParseResult::Incomplete,
    };
    let event_buffer = &buffer[..=end_index];
    let consumed = event_buffer.len() as u32;

    let event = match event_buffer[2] {
        b'[' => {
            if event_buffer.len() < 4 {
                return ParseResult::Error {
                    consumed,
                    error: ParseError::CouldNotParse,
                };
            }
            let key_code = match event_buffer[3] {
                val @ b'A'..=b'E' => KeyCode::function(1 + val - b'A'),
                _ => {
                    return ParseResult::Error {
                        consumed,
                        error: ParseError::CouldNotParse,
                    };
                }
            };
            pressed(key_code)
        }
        b'D' => pressed(KeyCode::Left),
        b'C' => pressed(KeyCode::Right),
        b'A' => pressed(KeyCode::Up),
        b'B' => pressed(KeyCode::Down),
        b'H' => pressed(KeyCode::Home),
        b'F' => pressed(KeyCode::End),
        b'Z' => InternalEvent::Event(Event::Key(KeyEvent::new_with_kind(
            KeyCode::BackTab,
            KeyModifiers::SHIFT,
            KeyEventKind::Press,
        ))),
        b'<' => return parse_csi_sgr_mouse(event_buffer),
        b'I' => InternalEvent::Event(Event::FocusGained),
        b'O' => InternalEvent::Event(Event::FocusLost),
        b'P' => pressed(KeyCode::F1),
        b'Q' => pressed(KeyCode::F2),
        b'S' => pressed(KeyCode::F4),
        // For other sequences, dispatch based on the final byte
        _ => {
            let last_byte = event_buffer[event_buffer.len() - 1];
            return match last_byte {
                b'M' => parse_csi_rxvt_mouse(event_buffer),
                b'm' => parse_csi_sgr_mouse(event_buffer), // SGR mouse release
                b'~' => parse_csi_special_key_code(event_buffer),
                b'u' => {
                    if event_buffer.starts_with(b"\x1B[?") {
                        parse_csi_keyboard_enhancement_flags(event_buffer)
                    } else {
                        parse_csi_u_encoded_key_code(event_buffer, is_raw)
                    }
                }
                b'c' if event_buffer.starts_with(b"\x1B[?") => {
                    parse_csi_primary_device_attributes(event_buffer)
                }
                b'R' if event_buffer.contains(&b';') => parse_csi_cursor_position(event_buffer),
                _ => parse_csi_modifier_key_code(event_buffer),
            };
        }
    };
    ParseResult::Event { consumed, event }
}

pub(crate) fn next_parsed<T>(iter: &mut dyn Iterator<Item = &str>) -> Result<T, ParseError>
where
    T: std::str::FromStr,
{
    iter.next()
        .ok_or(ParseError::CouldNotParse)?
        .parse::<T>()
        .map_err(|_| ParseError::InvalidNumber)
}

fn modifier_and_kind_parsed(iter: &mut dyn Iterator<Item = &str>) -> Result<(u8, u8), ParseError> {
    let mut sub_split = iter.next().ok_or(ParseError::CouldNotParse)?.split(':');

    let modifier_mask = next_parsed::<u8>(&mut sub_split)?;

    if let Ok(kind_code) = next_parsed::<u8>(&mut sub_split) {
        Ok((modifier_mask, kind_code))
    } else {
        Ok((modifier_mask, 1))
    }
}

pub(crate) fn parse_csi_cursor_position(event_buffer: &[u8]) -> ParseResult {
    assert!(event_buffer.starts_with(b"\x1B["));
    assert!(event_buffer.ends_with(b"R"));

    let consumed = event_buffer.len() as u32;

    let result: Result<InternalEvent, ParseError> = (|| {
        let s = std::str::from_utf8(&event_buffer[2..event_buffer.len() - 1])
            .map_err(|_| ParseError::InvalidUtf8)?;
        let mut split = s.split(';');
        let y = next_parsed::<u16>(&mut split)? - 1;
        let x = next_parsed::<u16>(&mut split)? - 1;
        Ok(InternalEvent::CursorPosition(x, y))
    })();

    match result {
        Ok(event) => ParseResult::Event { consumed, event },
        Err(error) => ParseResult::Error { consumed, error },
    }
}

fn parse_csi_keyboard_enhancement_flags(event_buffer: &[u8]) -> ParseResult {
    assert!(event_buffer.starts_with(b"\x1B[?"));
    assert!(event_buffer.ends_with(b"u"));

    let consumed = event_buffer.len() as u32;

    if event_buffer.len() < 5 {
        return ParseResult::Error {
            consumed,
            error: ParseError::CouldNotParse,
        };
    }

    let bits = event_buffer[3];
    let flags = KeyboardEnhancementFlags::from_bits_truncate(bits);

    ParseResult::Event {
        consumed,
        event: InternalEvent::KeyboardEnhancementFlags(flags),
    }
}

fn parse_csi_primary_device_attributes(event_buffer: &[u8]) -> ParseResult {
    assert!(event_buffer.starts_with(b"\x1B[?"));
    assert!(event_buffer.ends_with(b"c"));
    ParseResult::Event {
        consumed: event_buffer.len() as u32,
        event: InternalEvent::PrimaryDeviceAttributes,
    }
}

fn parse_modifiers(mask: u8) -> KeyModifiers {
    let modifier_mask = mask.saturating_sub(1);
    let mut modifiers = KeyModifiers::empty();
    if modifier_mask & 1 != 0 {
        modifiers |= KeyModifiers::SHIFT;
    }
    if modifier_mask & 2 != 0 {
        modifiers |= KeyModifiers::ALT;
    }
    if modifier_mask & 4 != 0 {
        modifiers |= KeyModifiers::CONTROL;
    }
    if modifier_mask & 8 != 0 {
        modifiers |= KeyModifiers::SUPER;
    }
    if modifier_mask & 16 != 0 {
        modifiers |= KeyModifiers::HYPER;
    }
    if modifier_mask & 32 != 0 {
        modifiers |= KeyModifiers::META;
    }
    modifiers
}

fn parse_modifiers_to_state(mask: u8) -> KeyEventState {
    let modifier_mask = mask.saturating_sub(1);
    let mut state = KeyEventState::empty();
    if modifier_mask & 64 != 0 {
        state |= KeyEventState::CAPS_LOCK;
    }
    if modifier_mask & 128 != 0 {
        state |= KeyEventState::NUM_LOCK;
    }
    state
}

fn parse_key_event_kind(kind: u8) -> KeyEventKind {
    match kind {
        1 => KeyEventKind::Press,
        2 => KeyEventKind::Repeat,
        3 => KeyEventKind::Release,
        _ => KeyEventKind::Press,
    }
}

pub(crate) fn parse_csi_modifier_key_code(event_buffer: &[u8]) -> ParseResult {
    assert!(event_buffer.starts_with(b"\x1B["));

    let consumed = event_buffer.len() as u32;

    let result: Result<InternalEvent, ParseError> = (|| {
        let s = std::str::from_utf8(&event_buffer[2..event_buffer.len() - 1])
            .map_err(|_| ParseError::InvalidUtf8)?;
        let mut split = s.split(';');
        split.next();

        let (modifiers, kind) =
            if let Ok((modifier_mask, kind_code)) = modifier_and_kind_parsed(&mut split) {
                (
                    parse_modifiers(modifier_mask),
                    parse_key_event_kind(kind_code),
                )
            } else if event_buffer.len() > 3 {
                let digit = (event_buffer[event_buffer.len() - 2] as char)
                    .to_digit(10)
                    .ok_or(ParseError::InvalidNumber)? as u8;
                (parse_modifiers(digit), KeyEventKind::Press)
            } else {
                (KeyModifiers::NONE, KeyEventKind::Press)
            };
        let key = event_buffer[event_buffer.len() - 1];

        let keycode = match key {
            b'A' => KeyCode::Up,
            b'B' => KeyCode::Down,
            b'C' => KeyCode::Right,
            b'D' => KeyCode::Left,
            b'F' => KeyCode::End,
            b'H' => KeyCode::Home,
            b'P' => KeyCode::F1,
            b'Q' => KeyCode::F2,
            b'R' => KeyCode::F3,
            b'S' => KeyCode::F4,
            _ => return Err(ParseError::CouldNotParse),
        };
        Ok(InternalEvent::Event(Event::Key(KeyEvent::new_with_kind(
            keycode, modifiers, kind,
        ))))
    })();

    match result {
        Ok(event) => ParseResult::Event { consumed, event },
        Err(error) => ParseResult::Error { consumed, error },
    }
}
fn translate_functional_keypad(codepoint: u32) -> Option<KeyCode> {
    use KeyCode::*;
    const LUT: [KeyCode; 29] = [
        Char('0'),
        Char('1'),
        Char('2'),
        Char('3'),
        Char('4'),
        Char('5'),
        Char('6'),
        Char('7'),
        Char('8'),
        Char('9'),
        Char('.'),
        Char('/'),
        Char('*'),
        Char('-'),
        Char('+'),
        Enter,
        Char('='),
        Char(','),
        Left,
        Right,
        Up,
        Down,
        PageUp,
        PageDown,
        Home,
        End,
        Insert,
        Delete,
        KeypadBegin,
    ];
    LUT.get(codepoint.wrapping_sub(57399) as usize).map(|value| *value)
}

fn translate_functional_other(codepoint: u32) -> Option<KeyCode> {
    use KeyCode::*;
    const META: [KeyCode; 6] = [CapsLock, ScrollLock, NumLock, PrintScreen, Pause, Menu];
    if (57358..57364).contains(&codepoint) {
        return Some(META[(codepoint - 57358) as usize]);
    }
    if (57376..57399).contains(&codepoint) {
        return Some(KeyCode::function((codepoint - 57376 + 13) as u8));
    }
    const MEDIA_MODES: [KeyCode; 27] = [
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
        LeftShift,
        LeftControl,
        LeftAlt,
        LeftSuper,
        LeftHyper,
        LeftMeta,
        RightShift,
        RightControl,
        RightAlt,
        RightSuper,
        RightHyper,
        RightMeta,
        IsoLevel3Shift,
        IsoLevel5Shift,
    ];
    if (57428..57455).contains(&codepoint) {
        return Some(MEDIA_MODES[(codepoint - 57428) as usize]);
    }
    None
}
fn translate_functional_key_code(codepoint: u32) -> Option<(KeyCode, KeyEventState)> {
    if let Some(keypad_key) = translate_functional_keypad(codepoint) {
        return Some((keypad_key, KeyEventState::KEYPAD));
    }
    if let Some(other_key) = translate_functional_other(codepoint) {
        return Some((other_key, KeyEventState::empty()));
    }
    None
}

pub(crate) fn parse_csi_u_encoded_key_code(event_buffer: &[u8], is_raw: bool) -> ParseResult {
    assert!(event_buffer.starts_with(b"\x1B["));
    assert!(event_buffer.ends_with(b"u"));

    let consumed = event_buffer.len() as u32;

    let result: Result<InternalEvent, ParseError> = (|| {
        let s = std::str::from_utf8(&event_buffer[2..event_buffer.len() - 1])
            .map_err(|_| ParseError::InvalidUtf8)?;
        let mut split = s.split(';');
        let mut codepoints = split.next().ok_or(ParseError::CouldNotParse)?.split(':');
        let codepoint = codepoints
            .next()
            .ok_or(ParseError::CouldNotParse)?
            .parse::<u32>()
            .map_err(|_| ParseError::InvalidNumber)?;

        let (mut modifiers, kind, state_from_modifiers) =
            if let Ok((modifier_mask, kind_code)) = modifier_and_kind_parsed(&mut split) {
                (
                    parse_modifiers(modifier_mask),
                    parse_key_event_kind(kind_code),
                    parse_modifiers_to_state(modifier_mask),
                )
            } else {
                (KeyModifiers::NONE, KeyEventKind::Press, KeyEventState::NONE)
            };

        let (mut keycode, state_from_keycode) =
            if let Some((special_key_code, state)) = translate_functional_key_code(codepoint) {
                (special_key_code, state)
            } else if let Some(c) = char::from_u32(codepoint) {
                (
                    match c {
                        '\x1B' => KeyCode::Esc,
                        '\r' => KeyCode::Enter,
                        '\n' if !is_raw => KeyCode::Enter,
                        '\t' => {
                            if modifiers.contains(KeyModifiers::SHIFT) {
                                KeyCode::BackTab
                            } else {
                                KeyCode::Tab
                            }
                        }
                        '\x7F' => KeyCode::Backspace,
                        _ => KeyCode::Char(c),
                    },
                    KeyEventState::empty(),
                )
            } else {
                return Err(ParseError::CouldNotParse);
            };

        // if let KeyCode::Modifier(modifier_keycode) = keycode {
        match keycode {
            KeyCode::LeftAlt | KeyCode::RightAlt => modifiers.set(KeyModifiers::ALT, true),
            KeyCode::LeftControl | KeyCode::RightControl => {
                modifiers.set(KeyModifiers::CONTROL, true)
            }
            KeyCode::LeftShift | KeyCode::RightShift => modifiers.set(KeyModifiers::SHIFT, true),
            KeyCode::LeftSuper | KeyCode::RightSuper => modifiers.set(KeyModifiers::SUPER, true),
            KeyCode::LeftHyper | KeyCode::RightHyper => modifiers.set(KeyModifiers::HYPER, true),
            KeyCode::LeftMeta | KeyCode::RightMeta => modifiers.set(KeyModifiers::META, true),
            _ => {}
        }
        // }
        if modifiers.contains(KeyModifiers::SHIFT)
            && let Some(shifted_c) = codepoints
                .next()
                .and_then(|cp| cp.parse::<u32>().ok())
                .and_then(char::from_u32)
            {
                keycode = KeyCode::Char(shifted_c);
                modifiers.set(KeyModifiers::SHIFT, false);
            }
        Ok(InternalEvent::Event(Event::Key(
            KeyEvent::new_with_kind_and_state(
                keycode,
                modifiers,
                kind,
                state_from_keycode | state_from_modifiers,
            ),
        )))
    })();

    match result {
        Ok(event) => ParseResult::Event { consumed, event },
        Err(error) => ParseResult::Error { consumed, error },
    }
}

pub(crate) fn parse_csi_special_key_code(event_buffer: &[u8]) -> ParseResult {
    assert!(event_buffer.starts_with(b"\x1B["));
    assert!(event_buffer.ends_with(b"~"));

    let consumed = event_buffer.len() as u32;

    let result: Result<InternalEvent, ParseError> = (|| {
        let s = std::str::from_utf8(&event_buffer[2..event_buffer.len() - 1])
            .map_err(|_| ParseError::InvalidUtf8)?;
        let mut split = s.split(';');
        let first = next_parsed::<u8>(&mut split)?;

        let (modifiers, kind, state) =
            if let Ok((modifier_mask, kind_code)) = modifier_and_kind_parsed(&mut split) {
                (
                    parse_modifiers(modifier_mask),
                    parse_key_event_kind(kind_code),
                    parse_modifiers_to_state(modifier_mask),
                )
            } else {
                (KeyModifiers::NONE, KeyEventKind::Press, KeyEventState::NONE)
            };
        let keycode = match first {
            1 | 7 => KeyCode::Home,
            2 => KeyCode::Insert,
            3 => KeyCode::Delete,
            4 | 8 => KeyCode::End,
            5 => KeyCode::PageUp,
            6 => KeyCode::PageDown,
            v @ 11..=15 => KeyCode::function(v - 10),
            v @ 17..=21 => KeyCode::function(v - 11),
            v @ 23..=26 => KeyCode::function(v - 12),
            v @ 28..=29 => KeyCode::function(v - 15),
            v @ 31..=34 => KeyCode::function(v - 17),
            _ => return Err(ParseError::CouldNotParse),
        };
        Ok(InternalEvent::Event(Event::Key(
            KeyEvent::new_with_kind_and_state(keycode, modifiers, kind, state),
        )))
    })();

    match result {
        Ok(event) => ParseResult::Event { consumed, event },
        Err(error) => ParseResult::Error { consumed, error },
    }
}

pub(crate) fn parse_csi_rxvt_mouse(event_buffer: &[u8]) -> ParseResult {
    assert!(event_buffer.starts_with(b"\x1B["));
    assert!(event_buffer.ends_with(b"M"));

    let consumed = event_buffer.len() as u32;

    let result: Result<InternalEvent, ParseError> = (|| {
        let s = std::str::from_utf8(&event_buffer[2..event_buffer.len() - 1])
            .map_err(|_| ParseError::InvalidUtf8)?;
        let mut split = s.split(';');
        let cb = next_parsed::<u8>(&mut split)?
            .checked_sub(32)
            .ok_or(ParseError::InvalidMouseInput)?;
        let (kind, modifiers) = parse_cb(cb)?;
        let cx = next_parsed::<u16>(&mut split)? - 1;
        let cy = next_parsed::<u16>(&mut split)? - 1;
        Ok(InternalEvent::Event(Event::Mouse(MouseEvent {
            kind,
            column: cx,
            row: cy,
            modifiers,
        })))
    })();

    match result {
        Ok(event) => ParseResult::Event { consumed, event },
        Err(error) => ParseResult::Error { consumed, error },
    }
}

pub(crate) fn parse_csi_normal_mouse(buffer: &[u8]) -> ParseResult {
    assert!(buffer.starts_with(b"\x1B[M"));

    if buffer.len() < 6 {
        return ParseResult::Incomplete;
    }

    let result: Result<InternalEvent, ParseError> = (|| {
        let cb = buffer[3]
            .checked_sub(32)
            .ok_or(ParseError::InvalidMouseInput)?;
        let (kind, modifiers) = parse_cb(cb)?;
        let cx = u16::from(buffer[4].saturating_sub(32)) - 1;
        let cy = u16::from(buffer[5].saturating_sub(32)) - 1;
        Ok(InternalEvent::Event(Event::Mouse(MouseEvent {
            kind,
            column: cx,
            row: cy,
            modifiers,
        })))
    })();

    match result {
        Ok(event) => ParseResult::Event { consumed: 6, event },
        Err(error) => ParseResult::Error { consumed: 6, error },
    }
}

pub(crate) fn parse_csi_sgr_mouse(event_buffer: &[u8]) -> ParseResult {
    assert!(event_buffer.starts_with(b"\x1B[<"));
    assert!(event_buffer.ends_with(b"m") || event_buffer.ends_with(b"M"));

    let consumed = event_buffer.len() as u32;

    let result: Result<InternalEvent, ParseError> = (|| {
        let s = std::str::from_utf8(&event_buffer[3..event_buffer.len() - 1])
            .map_err(|_| ParseError::InvalidUtf8)?;
        let mut split = s.split(';');
        let cb = next_parsed::<u8>(&mut split)?;
        let (kind, modifiers) = parse_cb(cb)?;
        let cx = next_parsed::<u16>(&mut split)? - 1;
        let cy = next_parsed::<u16>(&mut split)? - 1;
        let kind = if event_buffer.last() == Some(&b'm') {
            match kind {
                MouseEventKind::Down(button) => MouseEventKind::Up(button),
                other => other,
            }
        } else {
            kind
        };
        Ok(InternalEvent::Event(Event::Mouse(MouseEvent {
            kind,
            column: cx,
            row: cy,
            modifiers,
        })))
    })();

    match result {
        Ok(event) => ParseResult::Event { consumed, event },
        Err(error) => ParseResult::Error { consumed, error },
    }
}

fn parse_cb(cb: u8) -> Result<(MouseEventKind, KeyModifiers), ParseError> {
    let button_number = (cb & 0b0000_0011) | ((cb & 0b1100_0000) >> 4);
    let dragging = (cb & 0b0010_0000) == 0b0010_0000;

    let kind = match (button_number, dragging) {
        (0, false) => MouseEventKind::Down(MouseButton::Left),
        (1, false) => MouseEventKind::Down(MouseButton::Middle),
        (2, false) => MouseEventKind::Down(MouseButton::Right),
        (0, true) => MouseEventKind::Drag(MouseButton::Left),
        (1, true) => MouseEventKind::Drag(MouseButton::Middle),
        (2, true) => MouseEventKind::Drag(MouseButton::Right),
        (3, false) => MouseEventKind::Up(MouseButton::Left),
        (3, true) | (4, true) | (5, true) => MouseEventKind::Moved,
        (4, false) => MouseEventKind::ScrollUp,
        (5, false) => MouseEventKind::ScrollDown,
        (6, false) => MouseEventKind::ScrollLeft,
        (7, false) => MouseEventKind::ScrollRight,
        _ => return Err(ParseError::InvalidMouseInput),
    };

    let mut modifiers = KeyModifiers::empty();
    if cb & 0b0000_0100 == 0b0000_0100 {
        modifiers |= KeyModifiers::SHIFT;
    }
    if cb & 0b0000_1000 == 0b0000_1000 {
        modifiers |= KeyModifiers::ALT;
    }
    if cb & 0b0001_0000 == 0b0001_0000 {
        modifiers |= KeyModifiers::CONTROL;
    }

    Ok((kind, modifiers))
}

#[cfg(feature = "bracketed-paste")]
pub(crate) fn parse_csi_bracketed_paste(buffer: &[u8]) -> ParseResult {
    assert!(buffer.starts_with(b"\x1B[200~"));

    const END_PASTE: &[u8] = b"\x1B[201~";

    if let Some(end_pos) = buffer[6..]
        .windows(END_PASTE.len())
        .position(|w| w == END_PASTE)
    {
        let content_end = 6 + end_pos;
        let consumed = (content_end + END_PASTE.len()) as u32;
        let paste = String::from_utf8_lossy(&buffer[6..content_end]).to_string();
        ParseResult::Event {
            consumed,
            event: InternalEvent::Event(Event::Paste(paste)),
        }
    } else {
        ParseResult::Incomplete
    }
}

/// A helper function to parse a UTF-8 character from the beginning of a buffer.
///
/// Returns `Ok(Some((char, consumed_bytes)))` on success.
/// Returns `Ok(None)` if the buffer is incomplete.
/// Returns `Err(ParseError)` if the buffer contains an invalid UTF-8 sequence.
fn parse_utf8_char(buffer: &[u8]) -> Result<Option<(char, usize)>, ParseError> {
    if buffer.is_empty() {
        return Ok(None);
    }

    // Determine the required number of bytes for a character from the first byte.
    let required_bytes = match buffer[0] {
        // 1-byte sequence (ASCII)
        0x00..=0x7F => 1,
        // 2-byte sequence start
        0xC2..=0xDF => 2,
        // 3-byte sequence start
        0xE0..=0xEF => 3,
        // 4-byte sequence start
        0xF0..=0xF4 => 4,
        // Anything else is an invalid start byte according to modern UTF-8 standards
        _ => return Err(ParseError::InvalidUtf8),
    };

    if buffer.len() < required_bytes {
        // Not enough bytes for a full character.
        // Check if the bytes we have so far are valid continuation bytes (10xxxxxx).
        for byte in &buffer[1..] {
            if (*byte & 0xC0) != 0x80 {
                return Err(ParseError::InvalidUtf8);
            }
        }
        // It's a valid-looking prefix, so we're just incomplete.
        return Ok(None);
    }

    // We have enough bytes, so let's try to parse the character from that slice.
    let char_slice = &buffer[..required_bytes];
    match std::str::from_utf8(char_slice) {
        Ok(s) => {
            // It should parse to exactly one character.
            let ch = s.chars().next().ok_or(ParseError::InvalidUtf8)?;
            Ok(Some((ch, required_bytes)))
        }
        Err(_) => {
            // We had enough bytes, but it was still invalid (e.g., overlong encoding).
            Err(ParseError::InvalidUtf8)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::event::{KeyEventState, KeyModifiers, MouseButton, MouseEvent};

    #[test]
    fn test_esc_key() {
        assert_eq!(
            parse_event(b"\x1B", false),
            ParseResult::Event {
                consumed: 1,
                event: pressed(KeyCode::Esc)
            },
        );
    }

    #[test]
    fn test_alt_key() {
        assert_eq!(
            parse_event(b"\x1Bc", false),
            ParseResult::Event {
                consumed: 2,
                event: InternalEvent::Event(Event::Key(KeyEvent::new(
                    KeyCode::Char('c'),
                    KeyModifiers::ALT
                )))
            },
        );
    }

    #[test]
    fn test_alt_shift() {
        assert_eq!(
            parse_event(b"\x1BH", false),
            ParseResult::Event {
                consumed: 2,
                event: InternalEvent::Event(Event::Key(KeyEvent::new(
                    KeyCode::Char('H'),
                    KeyModifiers::ALT | KeyModifiers::SHIFT
                )))
            },
        );
    }

    #[test]
    fn test_alt_ctrl() {
        assert_eq!(
            parse_event(b"\x1B\x14", false),
            ParseResult::Event {
                consumed: 2,
                event: InternalEvent::Event(Event::Key(KeyEvent::new(
                    KeyCode::Char('t'),
                    KeyModifiers::ALT | KeyModifiers::CONTROL
                )))
            },
        );
    }

    #[test]
    fn test_parse_event_subsequent_calls() {
        assert_eq!(
            parse_event(b"\x1B[20;10R", false),
            ParseResult::Event {
                consumed: 8,
                event: InternalEvent::CursorPosition(9, 19)
            }
        );
        assert_eq!(
            parse_event(b"\x1B[D", false),
            ParseResult::Event {
                consumed: 3,
                event: pressed(KeyCode::Left)
            }
        );
        assert_eq!(
            parse_event(b"\x1B[2D", false),
            ParseResult::Event {
                consumed: 4,
                event: InternalEvent::Event(Event::Key(KeyEvent::new(
                    KeyCode::Left,
                    KeyModifiers::SHIFT
                )))
            }
        );
        assert_eq!(
            parse_event(b"\x1B[3~", false),
            ParseResult::Event {
                consumed: 4,
                event: pressed(KeyCode::Delete)
            },
        );
        #[cfg(feature = "bracketed-paste")]
        assert_eq!(
            parse_event(b"\x1B[200~on and on and on\x1B[201~", false),
            ParseResult::Event {
                consumed: 28,
                event: InternalEvent::Event(Event::Paste("on and on and on".to_string()))
            },
        );
        assert_eq!(
            parse_event(b"\x1B[32;30;40;M", false),
            ParseResult::Event {
                consumed: 12,
                event: InternalEvent::Event(Event::Mouse(MouseEvent {
                    kind: MouseEventKind::Down(MouseButton::Left),
                    column: 29,
                    row: 39,
                    modifiers: KeyModifiers::empty(),
                }))
            }
        );
        assert_eq!(
            parse_event(b"\x1B[M0\x60\x70", false),
            ParseResult::Event {
                consumed: 6,
                event: InternalEvent::Event(Event::Mouse(MouseEvent {
                    kind: MouseEventKind::Down(MouseButton::Left),
                    column: 63,
                    row: 79,
                    modifiers: KeyModifiers::CONTROL,
                }))
            }
        );
        assert_eq!(
            parse_event(b"\x1B[<0;20;10;M", false),
            ParseResult::Event {
                consumed: 12,
                event: InternalEvent::Event(Event::Mouse(MouseEvent {
                    kind: MouseEventKind::Down(MouseButton::Left),
                    column: 19,
                    row: 9,
                    modifiers: KeyModifiers::empty(),
                }))
            }
        );
        assert_eq!(
            parse_event("Ž".as_bytes(), false),
            ParseResult::Event {
                consumed: 2,
                event: InternalEvent::Event(Event::Key(KeyEvent::new(
                    KeyCode::Char('Ž'),
                    KeyModifiers::SHIFT
                )))
            },
        );
    }

    #[test]
    fn test_parse_event() {
        assert_eq!(
            parse_event(b"\t", false),
            ParseResult::Event {
                consumed: 1,
                event: pressed(KeyCode::Tab)
            },
        );
    }

    #[test]
    fn test_parse_csi_cursor_position() {
        assert_eq!(
            parse_csi_cursor_position(b"\x1B[20;10R"),
            ParseResult::Event {
                consumed: 8,
                event: InternalEvent::CursorPosition(9, 19)
            }
        );
    }

    #[test]
    fn test_parse_csi() {
        assert_eq!(
            parse_csi(b"\x1B[D", false),
            ParseResult::Event {
                consumed: 3,
                event: pressed(KeyCode::Left)
            },
        );
    }

    #[test]
    fn test_parse_csi_modifier_key_code() {
        assert_eq!(
            parse_csi_modifier_key_code(b"\x1B[2D"),
            ParseResult::Event {
                consumed: 4,
                event: InternalEvent::Event(Event::Key(KeyEvent::new(
                    KeyCode::Left,
                    KeyModifiers::SHIFT
                )))
            },
        );
    }

    #[test]
    fn test_parse_csi_special_key_code() {
        assert_eq!(
            parse_csi_special_key_code(b"\x1B[3~"),
            ParseResult::Event {
                consumed: 4,
                event: pressed(KeyCode::Delete)
            },
        );
    }

    #[test]
    fn test_parse_csi_special_key_code_multiple_values() {
        assert_eq!(
            parse_csi_special_key_code(b"\x1B[3;2~"),
            ParseResult::Event {
                consumed: 6,
                event: InternalEvent::Event(Event::Key(KeyEvent::new(
                    KeyCode::Delete,
                    KeyModifiers::SHIFT
                )))
            },
        );
    }

    #[cfg(feature = "bracketed-paste")]
    #[test]
    fn test_parse_csi_bracketed_paste() {
        assert_eq!(
            parse_event(b"\x1B[200~o", false),
            ParseResult::Incomplete,
            "A partial bracketed paste isn't parsed"
        );
        assert_eq!(
            parse_event(b"\x1B[200~o\x1B[2D", false),
            ParseResult::Incomplete,
            "A partial bracketed paste containing another escape code isn't parsed"
        );
        let input = b"\x1B[200~o\x1B[2D\x1B[201~";
        assert_eq!(
            parse_event(input, false),
            ParseResult::Event {
                consumed: input.len() as u32,
                event: InternalEvent::Event(Event::Paste("o\x1B[2D".to_string()))
            }
        );
    }

    #[test]
    fn test_parse_csi_focus() {
        assert_eq!(
            parse_csi(b"\x1B[O", false),
            ParseResult::Event {
                consumed: 3,
                event: InternalEvent::Event(Event::FocusLost)
            }
        );
    }

    #[test]
    fn test_parse_csi_rxvt_mouse() {
        let input = b"\x1B[32;30;40;M";
        assert_eq!(
            parse_csi_rxvt_mouse(input),
            ParseResult::Event {
                consumed: input.len() as u32,
                event: InternalEvent::Event(Event::Mouse(MouseEvent {
                    kind: MouseEventKind::Down(MouseButton::Left),
                    column: 29,
                    row: 39,
                    modifiers: KeyModifiers::empty(),
                }))
            }
        );
    }

    #[test]
    fn test_parse_csi_normal_mouse() {
        let input = b"\x1B[M0\x60\x70";
        assert_eq!(
            parse_csi_normal_mouse(input),
            ParseResult::Event {
                consumed: 6,
                event: InternalEvent::Event(Event::Mouse(MouseEvent {
                    kind: MouseEventKind::Down(MouseButton::Left),
                    column: 63,
                    row: 79,
                    modifiers: KeyModifiers::CONTROL,
                }))
            }
        );
    }

    #[test]
    fn test_parse_csi_sgr_mouse() {
        let input1 = b"\x1B[<0;20;10;M";
        assert_eq!(
            parse_csi_sgr_mouse(input1),
            ParseResult::Event {
                consumed: input1.len() as u32,
                event: InternalEvent::Event(Event::Mouse(MouseEvent {
                    kind: MouseEventKind::Down(MouseButton::Left),
                    column: 19,
                    row: 9,
                    modifiers: KeyModifiers::empty(),
                }))
            }
        );
        let input2 = b"\x1B[<0;20;10M";
        assert_eq!(
            parse_csi_sgr_mouse(input2),
            ParseResult::Event {
                consumed: input2.len() as u32,
                event: InternalEvent::Event(Event::Mouse(MouseEvent {
                    kind: MouseEventKind::Down(MouseButton::Left),
                    column: 19,
                    row: 9,
                    modifiers: KeyModifiers::empty(),
                }))
            }
        );
        let input3 = b"\x1B[<0;20;10;m";
        assert_eq!(
            parse_csi_sgr_mouse(input3),
            ParseResult::Event {
                consumed: input3.len() as u32,
                event: InternalEvent::Event(Event::Mouse(MouseEvent {
                    kind: MouseEventKind::Up(MouseButton::Left),
                    column: 19,
                    row: 9,
                    modifiers: KeyModifiers::empty(),
                }))
            }
        );
        let input4 = b"\x1B[<0;20;10m";
        assert_eq!(
            parse_csi_sgr_mouse(input4),
            ParseResult::Event {
                consumed: input4.len() as u32,
                event: InternalEvent::Event(Event::Mouse(MouseEvent {
                    kind: MouseEventKind::Up(MouseButton::Left),
                    column: 19,
                    row: 9,
                    modifiers: KeyModifiers::empty(),
                }))
            }
        );
    }

    #[test]
    fn test_utf8() {
        // 'Valid ASCII' => "a",
        assert_eq!(parse_utf8_char(b"a"), Ok(Some(('a', 1))));
        // 'Valid 2 Octet Sequence' => "\xc3\xb1",
        assert_eq!(parse_utf8_char(&[0xC3, 0xB1]), Ok(Some(('ñ', 2))));
        // 'Incomplete 2 Octet Sequence'
        assert_eq!(parse_utf8_char(&[0xC3]), Ok(None));
        // 'Invalid 2 Octet Sequence' => "\xc3\x28",
        assert_eq!(parse_utf8_char(&[0xC3, 0x28]), Err(ParseError::InvalidUtf8));
        // 'Invalid Sequence Identifier' => "\xa0\xa1",
        assert_eq!(parse_utf8_char(&[0xA0, 0xA1]), Err(ParseError::InvalidUtf8));
        // 'Valid 3 Octet Sequence' => "\xe2\x82\xa1",
        assert_eq!(
            parse_utf8_char(&[0xE2, 0x82, 0xA1]),
            Ok(Some(('\u{20A1}', 3)))
        );
        // 'Invalid 3 Octet Sequence (in 2nd Octet)' => "\xe2\x28\xa1",
        assert_eq!(
            parse_utf8_char(&[0xE2, 0x28, 0xA1]),
            Err(ParseError::InvalidUtf8)
        );
        // 'Invalid 3 Octet Sequence (in 3rd Octet)' => "\xe2\x82\x28",
        assert_eq!(
            parse_utf8_char(&[0xE2, 0x82, 0x28]),
            Err(ParseError::InvalidUtf8)
        );
        // 'Valid 4 Octet Sequence' => "\xf0\x90\x8c\xbc",
        assert_eq!(
            parse_utf8_char(&[0xF0, 0x90, 0x8C, 0xBC]),
            Ok(Some(('𐌼', 4)))
        );
        // 'Invalid 4 Octet Sequence (in 2nd Octet)' => "\xf0\x28\x8c\xbc",
        assert_eq!(
            parse_utf8_char(&[0xF0, 0x28, 0x8C, 0xBC]),
            Err(ParseError::InvalidUtf8)
        );
        // 'Invalid 4 Octet Sequence (in 3rd Octet)' => "\xf0\x90\x28\xbc",
        assert_eq!(
            parse_utf8_char(&[0xF0, 0x90, 0x28, 0xBC]),
            Err(ParseError::InvalidUtf8)
        );
        // 'Invalid 4 Octet Sequence (in 4th Octet)' => "\xf0\x28\x8c\x28",
        assert_eq!(
            parse_utf8_char(&[0xF0, 0x90, 0x8C, 0x28]),
            Err(ParseError::InvalidUtf8)
        );
    }

    #[test]
    fn test_parse_char_event_lowercase() {
        assert_eq!(
            parse_event(b"c", false),
            ParseResult::Event {
                consumed: 1,
                event: InternalEvent::Event(Event::Key(KeyEvent::new(
                    KeyCode::Char('c'),
                    KeyModifiers::empty()
                )))
            },
        );
    }

    #[test]
    fn test_parse_char_event_uppercase() {
        assert_eq!(
            parse_event(b"C", false),
            ParseResult::Event {
                consumed: 1,
                event: InternalEvent::Event(Event::Key(KeyEvent::new(
                    KeyCode::Char('C'),
                    KeyModifiers::SHIFT
                )))
            },
        );
    }

    // --- `CSI u` sequence tests ---

    fn u_event(consumed: u32, event: Event) -> ParseResult {
        ParseResult::Event {
            consumed,
            event: InternalEvent::Event(event),
        }
    }

    #[test]
    fn test_parse_basic_csi_u_encoded_key_code() {
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[97u", false),
            u_event(5, Event::Key(KeyCode::Char('a').into()))
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[97;2u", false),
            u_event(
                7,
                Event::Key(KeyEvent::new(KeyCode::Char('A'), KeyModifiers::SHIFT))
            )
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[97;7u", false),
            u_event(
                7,
                Event::Key(KeyEvent::new(
                    KeyCode::Char('a'),
                    KeyModifiers::ALT | KeyModifiers::CONTROL
                ))
            )
        );
    }

    #[test]
    fn test_parse_basic_csi_u_encoded_key_code_special_keys() {
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[13u", false),
            u_event(5, Event::Key(KeyCode::Enter.into()))
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[27u", false),
            u_event(5, Event::Key(KeyCode::Esc.into()))
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[57358u", false),
            u_event(8, Event::Key(KeyCode::CapsLock.into()))
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[57376u", false),
            u_event(8, Event::Key(KeyCode::F13.into()))
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[57428u", false),
            u_event(8, Event::Key(KeyCode::Play.into()))
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[57441u", false),
            u_event(
                8,
                Event::Key(KeyEvent::new(KeyCode::LeftShift, KeyModifiers::SHIFT,))
            )
        );
    }

    #[test]
    fn test_parse_csi_u_encoded_keypad_code() {
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[57399u", false),
            u_event(
                8,
                Event::Key(KeyEvent::new_with_kind_and_state(
                    KeyCode::Char('0'),
                    KeyModifiers::empty(),
                    KeyEventKind::Press,
                    KeyEventState::KEYPAD,
                ))
            )
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[57419u", false),
            u_event(
                8,
                Event::Key(KeyEvent::new_with_kind_and_state(
                    KeyCode::Up,
                    KeyModifiers::empty(),
                    KeyEventKind::Press,
                    KeyEventState::KEYPAD,
                ))
            )
        );
    }

    #[test]
    fn test_parse_csi_u_encoded_key_code_with_types() {
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[97;1u", false),
            u_event(
                7,
                Event::Key(KeyEvent::new_with_kind(
                    KeyCode::Char('a'),
                    KeyModifiers::empty(),
                    KeyEventKind::Press,
                ))
            )
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[97;1:1u", false),
            u_event(
                9,
                Event::Key(KeyEvent::new_with_kind(
                    KeyCode::Char('a'),
                    KeyModifiers::empty(),
                    KeyEventKind::Press,
                ))
            )
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[97;5:1u", false),
            u_event(
                9,
                Event::Key(KeyEvent::new_with_kind(
                    KeyCode::Char('a'),
                    KeyModifiers::CONTROL,
                    KeyEventKind::Press,
                ))
            )
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[97;1:2u", false),
            u_event(
                9,
                Event::Key(KeyEvent::new_with_kind(
                    KeyCode::Char('a'),
                    KeyModifiers::empty(),
                    KeyEventKind::Repeat,
                ))
            )
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[97;1:3u", false),
            u_event(
                9,
                Event::Key(KeyEvent::new_with_kind(
                    KeyCode::Char('a'),
                    KeyModifiers::empty(),
                    KeyEventKind::Release,
                ))
            )
        );
    }

    #[test]
    fn test_parse_csi_u_encoded_key_code_has_modifier_on_modifier_press() {
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[57449u", false),
            u_event(
                8,
                Event::Key(KeyEvent::new_with_kind(
                    KeyCode::RightAlt,
                    KeyModifiers::ALT,
                    KeyEventKind::Press,
                ))
            )
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[57449;3:3u", false),
            u_event(
                12,
                Event::Key(KeyEvent::new_with_kind(
                    KeyCode::RightAlt,
                    KeyModifiers::ALT,
                    KeyEventKind::Release,
                ))
            )
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[57450u", false),
            u_event(
                8,
                Event::Key(KeyEvent::new(KeyCode::RightSuper, KeyModifiers::SUPER,))
            )
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[57451u", false),
            u_event(
                8,
                Event::Key(KeyEvent::new(KeyCode::RightHyper, KeyModifiers::HYPER,))
            )
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[57452u", false),
            u_event(
                8,
                Event::Key(KeyEvent::new(KeyCode::RightMeta, KeyModifiers::META,))
            )
        );
    }

    #[test]
    fn test_parse_csi_u_encoded_key_code_with_extra_modifiers() {
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[97;9u", false),
            u_event(
                7,
                Event::Key(KeyEvent::new(KeyCode::Char('a'), KeyModifiers::SUPER))
            )
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[97;17u", false),
            u_event(
                8,
                Event::Key(KeyEvent::new(KeyCode::Char('a'), KeyModifiers::HYPER))
            )
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[97;33u", false),
            u_event(
                8,
                Event::Key(KeyEvent::new(KeyCode::Char('a'), KeyModifiers::META))
            )
        );
    }

    #[test]
    fn test_parse_csi_u_encoded_key_code_with_extra_state() {
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[97;65u", false),
            u_event(
                8,
                Event::Key(KeyEvent::new_with_kind_and_state(
                    KeyCode::Char('a'),
                    KeyModifiers::empty(),
                    KeyEventKind::Press,
                    KeyEventState::CAPS_LOCK,
                ))
            )
        );
        assert_eq!(
            parse_csi_u_encoded_key_code(b"\x1B[49;129u", false),
            u_event(
                9,
                Event::Key(KeyEvent::new_with_kind_and_state(
                    KeyCode::Char('1'),
                    KeyModifiers::empty(),
                    KeyEventKind::Press,
                    KeyEventState::NUM_LOCK,
                ))
            )
        );
    }

    #[test]
    fn test_parse_csi_u_with_shifted_keycode() {
        assert_eq!(
            parse_event(b"\x1B[57:40;4u", false),
            u_event(
                10,
                Event::Key(KeyEvent::new(KeyCode::Char('('), KeyModifiers::ALT))
            )
        );
        assert_eq!(
            parse_event(b"\x1B[45:95;4u", false),
            u_event(
                10,
                Event::Key(KeyEvent::new(KeyCode::Char('_'), KeyModifiers::ALT))
            )
        );
    }

    #[test]
    fn test_parse_csi_special_key_code_with_types() {
        assert_eq!(
            parse_event(b"\x1B[;1:3B", false),
            u_event(
                7,
                Event::Key(KeyEvent::new_with_kind(
                    KeyCode::Down,
                    KeyModifiers::empty(),
                    KeyEventKind::Release,
                ))
            )
        );
        assert_eq!(
            parse_event(b"\x1B[1;1:3B", false),
            u_event(
                8,
                Event::Key(KeyEvent::new_with_kind(
                    KeyCode::Down,
                    KeyModifiers::empty(),
                    KeyEventKind::Release,
                ))
            )
        );
    }

    #[test]
    fn test_parse_csi_numbered_escape_code_with_types() {
        assert_eq!(
            parse_event(b"\x1B[5;1:3~", false),
            u_event(
                8,
                Event::Key(KeyEvent::new_with_kind(
                    KeyCode::PageUp,
                    KeyModifiers::empty(),
                    KeyEventKind::Release,
                ))
            )
        );
        assert_eq!(
            parse_event(b"\x1B[6;5:3~", false),
            u_event(
                8,
                Event::Key(KeyEvent::new_with_kind(
                    KeyCode::PageDown,
                    KeyModifiers::CONTROL,
                    KeyEventKind::Release,
                ))
            )
        );
    }
}
