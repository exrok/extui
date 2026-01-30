//! Terminal event polling and parsing.
//!
//! Provides low-level access to terminal input events including keyboard, mouse,
//! focus, and resize events.
//!
//! # Polling
//!
//! Use [`poll`] to wait for input with an optional timeout:
//!
//! ```no_run
//! use extui::event::{poll, Polled};
//! use std::time::Duration;
//!
//! let result = poll(&std::io::stdin(), Some(Duration::from_millis(100)))?;
//! if result == Polled::ReadReady {
//!     // Input is available
//! }
//! # Ok::<(), std::io::Error>(())
//! ```
//!
//! # Parsing
//!
//! The [`Events`] struct buffers and parses input into [`Event`] values:
//!
//! ```no_run
//! use extui::event::{Events, Event, KeyCode};
//!
//! let mut events = Events::default();
//! events.read_from(&std::io::stdin())?;
//!
//! while let Some(event) = events.next(true) {
//!     match event {
//!         Event::Key(key) => println!("Key: {:?}", key.code),
//!         Event::Mouse(mouse) => println!("Mouse: {:?}", mouse.kind),
//!         Event::Resized => println!("Terminal resized"),
//!         _ => {}
//!     }
//! }
//! # Ok::<(), std::io::Error>(())
//! ```
//!
//! # Signal Handling
//!
//! The [`polling`] submodule provides global waker initialization for handling
//! `SIGWINCH` (resize) and termination signals.

use std::{
    fmt::{self, Display, Write},
    os::fd::AsFd,
    time::Duration,
};

use bitflags::bitflags;
use std::hash::{Hash, Hasher};

pub(crate) mod parse;
pub mod polling;

pub use parse::Events;

/// Result of a polling operation.
#[derive(Debug, PartialEq, Eq)]
pub enum Polled {
    /// Data is ready to read from the file descriptor.
    ReadReady,
    /// The waker was triggered.
    Woken,
    /// The timeout expired with no events.
    TimedOut,
}

impl Polled {
    /// Returns `true` if data is ready to read.
    pub fn is_readable(&self) -> bool {
        matches!(self, Polled::ReadReady)
    }
}

/// Polls a file descriptor for read readiness using the global waker.
///
/// # Errors
///
/// Returns an error if the poll system call fails.
pub fn poll(fd: &impl AsFd, timeout: Option<Duration>) -> std::io::Result<Polled> {
    poll_with_custom_waker(fd, polling::global_waker(), timeout)
}

/// Polls a file descriptor for read readiness with a custom waker.
///
/// # Errors
///
/// Returns an error if the poll system call fails.
pub fn poll_with_custom_waker(
    fd: &impl AsFd,
    waker: Option<&polling::Waker>,
    timeout: Option<Duration>,
) -> std::io::Result<Polled> {
    polling::wait_on_fd_inner(Some(fd.as_fd()), waker, timeout)
}

/// An internal event.
///
/// Encapsulates publicly available `Event` with additional internal
/// events that shouldn't be publicly available to the crate users.
#[derive(Debug, PartialOrd, PartialEq, Hash, Clone, Eq)]
pub(crate) enum InternalEvent {
    /// An event.
    Event(Event),
    /// A cursor position (`col`, `row`).
    #[cfg(unix)]
    CursorPosition(u16, u16),
    /// The progressive keyboard enhancement flags enabled by the terminal.
    #[cfg(unix)]
    KeyboardEnhancementFlags(KeyboardEnhancementFlags),
    /// Attributes and architectural class of the terminal.
    #[cfg(unix)]
    PrimaryDeviceAttributes,
}

bitflags! {
    /// Represents special flags that tell compatible terminals to add extra information to keyboard events.
    ///
    /// See <https://sw.kovidgoyal.net/kitty/keyboard-protocol/#progressive-enhancement> for more information.
    ///
    /// Alternate keys and Unicode codepoints are not yet supported by crossterm.
    #[derive(Debug, PartialOrd, PartialEq, Eq, Clone, Copy, Hash)]
    pub struct KeyboardEnhancementFlags: u8 {
        /// Represent Escape and modified keys using CSI-u sequences, so they can be unambiguously
        /// read.
        const DISAMBIGUATE_ESCAPE_CODES = 0b0000_0001;
        /// Add extra events with [`KeyEvent.kind`] set to [`KeyEventKind::Repeat`] or
        /// [`KeyEventKind::Release`] when keys are autorepeated or released.
        const REPORT_EVENT_TYPES = 0b0000_0010;
        /// Send [alternate keycodes](https://sw.kovidgoyal.net/kitty/keyboard-protocol/#key-codes)
        /// in addition to the base keycode. The alternate keycode overrides the base keycode in
        /// resulting `KeyEvent`s.
        const REPORT_ALTERNATE_KEYS = 0b0000_0100;
        /// Represent all keyboard events as CSI-u sequences. This is required to get repeat/release
        /// events for plain-text keys.
        const REPORT_ALL_KEYS_AS_ESCAPE_CODES = 0b0000_1000;
        // Send the Unicode codepoint as well as the keycode.
        //
        // *Note*: this is not yet supported by crossterm.
        // const REPORT_ASSOCIATED_TEXT = 0b0001_0000;
    }
}

/// Represents an event.
#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Hash)]
pub enum Event {
    /// The terminal gained focus
    FocusGained,
    /// The terminal lost focus
    FocusLost,
    /// A single key event with additional pressed modifiers.
    Key(KeyEvent),
    /// A single mouse event with additional pressed modifiers.
    Mouse(MouseEvent),
    /// A string that was pasted into the terminal. Only emitted if bracketed paste has been
    /// enabled.
    #[cfg(feature = "bracketed-paste")]
    Paste(String),
    /// A resize event with new dimensions after resize (columns, rows).
    /// **Note** that resize events can occur in batches.
    Resized,
}

impl Event {
    /// Returns `true` if the event is a key press event.
    ///
    /// This is useful for waiting for any key press event, regardless of the key that was pressed.
    ///
    /// Returns `false` for key release and repeat events (as well as for non-key events).
    ///
    /// # Examples
    ///
    /// The following code runs a loop that processes events until a key press event is encountered:
    ///
    #[inline]
    pub fn is_key_press(&self) -> bool {
        matches!(
            self,
            Event::Key(KeyEvent {
                kind: KeyEventKind::Press,
                ..
            })
        )
    }

    /// Returns `true` if the event is a key release event.
    #[inline]
    pub fn is_key_release(&self) -> bool {
        matches!(
            self,
            Event::Key(KeyEvent {
                kind: KeyEventKind::Release,
                ..
            })
        )
    }

    /// Returns `true` if the event is a key repeat event.
    #[inline]
    pub fn is_key_repeat(&self) -> bool {
        matches!(
            self,
            Event::Key(KeyEvent {
                kind: KeyEventKind::Repeat,
                ..
            })
        )
    }

    /// Returns the key event if the event is a key event, otherwise `None`.
    ///
    /// This is a convenience method that makes apps that only care about key events easier to write.
    ///
    /// # Examples
    ///
    /// The following code runs a loop that only processes key events:
    ///
    #[inline]
    pub fn as_key_event(&self) -> Option<KeyEvent> {
        match self {
            Event::Key(event) => Some(*event),
            _ => None,
        }
    }

    /// Returns an Option containing the KeyEvent if the event is a key press event.
    ///
    /// This is a convenience method that makes apps that only care about key press events, and not
    /// key release or repeat events (or non-key events), easier to write.
    ///
    /// Returns `None` for key release and repeat events (as well as for non-key events).
    ///
    /// # Examples
    ///
    /// The following code runs a loop that only processes key press events:
    #[inline]
    pub fn as_key_press_event(&self) -> Option<KeyEvent> {
        match self {
            Event::Key(event) if self.is_key_press() => Some(*event),
            _ => None,
        }
    }

    /// Returns an Option containing the `KeyEvent` if the event is a key release event.
    #[inline]
    pub fn as_key_release_event(&self) -> Option<KeyEvent> {
        match self {
            Event::Key(event) if self.is_key_release() => Some(*event),
            _ => None,
        }
    }

    /// Returns an Option containing the `KeyEvent` if the event is a key repeat event.
    #[inline]
    pub fn as_key_repeat_event(&self) -> Option<KeyEvent> {
        match self {
            Event::Key(event) if self.is_key_repeat() => Some(*event),
            _ => None,
        }
    }

    /// Returns the mouse event if the event is a mouse event, otherwise `None`.
    ///
    /// This is a convenience method that makes code which only cares about mouse events easier to
    /// write.
    #[inline]
    pub fn as_mouse_event(&self) -> Option<MouseEvent> {
        match self {
            Event::Mouse(event) => Some(*event),
            _ => None,
        }
    }

    /// Returns the pasted string if the event is a paste event, otherwise `None`.
    ///
    /// This is a convenience method that makes code which only cares about paste events easier to write.
    ///
    /// # Examples
    ///
    #[cfg(feature = "bracketed-paste")]
    #[inline]
    pub fn as_paste_event(&self) -> Option<&str> {
        match self {
            Event::Paste(paste) => Some(paste),
            _ => None,
        }
    }
}

/// Represents a mouse event.
///
/// # Platform-specific Notes
///
/// ## Mouse Buttons
///
/// Some platforms/terminals do not report mouse button for the
/// `MouseEventKind::Up` and `MouseEventKind::Drag` events. `MouseButton::Left`
/// is returned if we don't know which button was used.
///
/// ## Key Modifiers
///
/// Some platforms/terminals does not report all key modifiers
/// combinations for all mouse event types. For example - macOS reports
/// `Ctrl` + left mouse button click as a right mouse button click.
#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Hash)]
pub struct MouseEvent {
    /// The kind of mouse event that was caused.
    pub kind: MouseEventKind,
    /// The column that the event occurred on.
    pub column: u16,
    /// The row that the event occurred on.
    pub row: u16,
    /// The key modifiers active when the event occurred.
    pub modifiers: KeyModifiers,
}

/// A mouse event kind.
///
/// # Platform-specific Notes
///
/// ## Mouse Buttons
///
/// Some platforms/terminals do not report mouse button for the
/// `MouseEventKind::Up` and `MouseEventKind::Drag` events. `MouseButton::Left`
/// is returned if we don't know which button was used.
#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Hash)]
pub enum MouseEventKind {
    /// Pressed mouse button. Contains the button that was pressed.
    Down(MouseButton),
    /// Released mouse button. Contains the button that was released.
    Up(MouseButton),
    /// Moved the mouse cursor while pressing the contained mouse button.
    Drag(MouseButton),
    /// Moved the mouse cursor while not pressing a mouse button.
    Moved,
    /// Scrolled mouse wheel downwards (towards the user).
    ScrollDown,
    /// Scrolled mouse wheel upwards (away from the user).
    ScrollUp,
    /// Scrolled mouse wheel left (mostly on a laptop touchpad).
    ScrollLeft,
    /// Scrolled mouse wheel right (mostly on a laptop touchpad).
    ScrollRight,
}

/// Represents a mouse button.
#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Hash)]
pub enum MouseButton {
    /// Left mouse button.
    Left,
    /// Right mouse button.
    Right,
    /// Middle mouse button.
    Middle,
}

bitflags! {
    /// Represents key modifiers (shift, control, alt, etc.).
    ///
    /// **Note:** `SUPER`, `HYPER`, and `META` can only be read if
    /// [`KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES`] has been enabled.
    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Hash)]
    pub struct KeyModifiers: u8 {
        const SHIFT = 0b0000_0001;
        const CONTROL = 0b0000_0010;
        const ALT = 0b0000_0100;
        const SUPER = 0b0000_1000;
        const HYPER = 0b0001_0000;
        const META = 0b0010_0000;
        const NONE = 0b0000_0000;
    }
}

impl Display for KeyModifiers {
    /// Formats the key modifiers using the given formatter.
    ///
    /// The key modifiers are joined by a `+` character.
    ///
    /// # Platform-specific Notes
    ///
    /// On macOS, the control, alt, and super keys is displayed as "Control", "Option", and
    /// "Command" respectively. See
    /// <https://support.apple.com/guide/applestyleguide/welcome/1.0/web>.
    ///
    /// On Windows, the super key is displayed as "Windows" and the control key is displayed as
    /// "Ctrl". See
    /// <https://learn.microsoft.com/en-us/style-guide/a-z-word-list-term-collections/term-collections/keys-keyboard-shortcuts>.
    ///
    /// On other platforms, the super key is referred to as "Super" and the control key is
    /// displayed as "Ctrl".
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for modifier in self.iter() {
            if !first {
                f.write_str("+")?;
            }

            first = false;
            match modifier {
                KeyModifiers::SHIFT => f.write_str("Shift")?,
                #[cfg(unix)]
                KeyModifiers::CONTROL => f.write_str("Control")?,
                #[cfg(windows)]
                KeyModifiers::CONTROL => f.write_str("Ctrl")?,
                #[cfg(target_os = "macos")]
                KeyModifiers::ALT => f.write_str("Option")?,
                #[cfg(not(target_os = "macos"))]
                KeyModifiers::ALT => f.write_str("Alt")?,
                #[cfg(target_os = "macos")]
                KeyModifiers::SUPER => f.write_str("Command")?,
                #[cfg(target_os = "windows")]
                KeyModifiers::SUPER => f.write_str("Windows")?,
                #[cfg(not(any(target_os = "macos", target_os = "windows")))]
                KeyModifiers::SUPER => f.write_str("Super")?,
                KeyModifiers::HYPER => f.write_str("Hyper")?,
                KeyModifiers::META => f.write_str("Meta")?,
                _ => unreachable!(),
            }
        }
        Ok(())
    }
}

/// Represents a keyboard event kind.
#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Hash)]
pub enum KeyEventKind {
    /// A key was pressed.
    Press,
    /// A key is being held and auto-repeating.
    Repeat,
    /// A key was released.
    Release,
}

bitflags! {
    /// Represents extra state about the key event.
    ///
    /// **Note:** This state can only be read if
    /// [`KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES`] has been enabled.
    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Hash)]
    pub struct KeyEventState: u8 {
        /// The key event origins from the keypad.
        const KEYPAD = 0b0000_0001;
        /// Caps Lock was enabled for this key event.
        ///
        /// **Note:** this is set for the initial press of Caps Lock itself.
        const CAPS_LOCK = 0b0000_0010;
        /// Num Lock was enabled for this key event.
        ///
        /// **Note:** this is set for the initial press of Num Lock itself.
        const NUM_LOCK = 0b0000_0100;
        const NONE = 0b0000_0000;
    }
}

/// Represents a key event.
#[derive(Debug, PartialOrd, Ord, Clone, Copy)]
pub struct KeyEvent {
    /// The key itself.
    pub code: KeyCode,
    /// Additional key modifiers.
    pub modifiers: KeyModifiers,
    /// Kind of event.
    ///
    /// Only set if:
    /// - Unix: [`KeyboardEnhancementFlags::REPORT_EVENT_TYPES`] has been enabled.
    /// - Windows: always
    pub kind: KeyEventKind,
    /// Keyboard state.
    ///
    /// Only set if [`KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES`] has been enabled.
    pub state: KeyEventState,
}

impl KeyEvent {
    /// Creates a new key press event with the given code and modifiers.
    pub const fn new(code: KeyCode, modifiers: KeyModifiers) -> KeyEvent {
        KeyEvent {
            code,
            modifiers,
            kind: KeyEventKind::Press,
            state: KeyEventState::empty(),
        }
    }

    /// Creates a new key event with the given code, modifiers, and kind.
    pub const fn new_with_kind(
        code: KeyCode,
        modifiers: KeyModifiers,
        kind: KeyEventKind,
    ) -> KeyEvent {
        KeyEvent {
            code,
            modifiers,
            kind,
            state: KeyEventState::empty(),
        }
    }

    /// Creates a new key event with all fields specified.
    pub const fn new_with_kind_and_state(
        code: KeyCode,
        modifiers: KeyModifiers,
        kind: KeyEventKind,
        state: KeyEventState,
    ) -> KeyEvent {
        KeyEvent {
            code,
            modifiers,
            kind,
            state,
        }
    }

    // modifies the KeyEvent,
    // so that KeyModifiers::SHIFT is present iff
    // an uppercase char is present.
    fn normalize_case(mut self) -> KeyEvent {
        let c = match self.code {
            KeyCode::Char(c) => c,
            _ => return self,
        };

        if c.is_ascii_uppercase() {
            self.modifiers.insert(KeyModifiers::SHIFT);
        } else if self.modifiers.contains(KeyModifiers::SHIFT) {
            self.code = KeyCode::Char(c.to_ascii_uppercase())
        }
        self
    }

    /// Returns whether the key event is a press event.
    pub fn is_press(&self) -> bool {
        matches!(self.kind, KeyEventKind::Press)
    }

    /// Returns whether the key event is a release event.
    pub fn is_release(&self) -> bool {
        matches!(self.kind, KeyEventKind::Release)
    }

    /// Returns whether the key event is a repeat event.
    pub fn is_repeat(&self) -> bool {
        matches!(self.kind, KeyEventKind::Repeat)
    }
}

impl From<KeyCode> for KeyEvent {
    fn from(code: KeyCode) -> Self {
        KeyEvent {
            code,
            modifiers: KeyModifiers::empty(),
            kind: KeyEventKind::Press,
            state: KeyEventState::empty(),
        }
    }
}

impl PartialEq for KeyEvent {
    fn eq(&self, other: &KeyEvent) -> bool {
        let KeyEvent {
            code: lhs_code,
            modifiers: lhs_modifiers,
            kind: lhs_kind,
            state: lhs_state,
        } = self.normalize_case();
        let KeyEvent {
            code: rhs_code,
            modifiers: rhs_modifiers,
            kind: rhs_kind,
            state: rhs_state,
        } = other.normalize_case();
        (lhs_code == rhs_code)
            && (lhs_modifiers == rhs_modifiers)
            && (lhs_kind == rhs_kind)
            && (lhs_state == rhs_state)
    }
}

impl Eq for KeyEvent {}

impl Hash for KeyEvent {
    fn hash<H: Hasher>(&self, hash_state: &mut H) {
        let KeyEvent {
            code,
            modifiers,
            kind,
            state,
        } = self.normalize_case();
        code.hash(hash_state);
        modifiers.hash(hash_state);
        kind.hash(hash_state);
        state.hash(hash_state);
    }
}

impl KeyCode {
    /// Generates a KeyCode::F{num} when num is in the range of 1..=35,
    /// defaulting to F35 for out of range values
    pub fn function(num: u8) -> KeyCode {
        match num {
            1 => KeyCode::F1,
            2 => KeyCode::F2,
            3 => KeyCode::F3,
            4 => KeyCode::F4,
            5 => KeyCode::F5,
            6 => KeyCode::F6,
            7 => KeyCode::F7,
            8 => KeyCode::F8,
            9 => KeyCode::F9,
            10 => KeyCode::F10,
            11 => KeyCode::F11,
            12 => KeyCode::F12,
            13 => KeyCode::F13,
            14 => KeyCode::F14,
            15 => KeyCode::F15,
            16 => KeyCode::F16,
            17 => KeyCode::F17,
            18 => KeyCode::F18,
            19 => KeyCode::F19,
            20 => KeyCode::F20,
            21 => KeyCode::F21,
            22 => KeyCode::F22,
            23 => KeyCode::F23,
            24 => KeyCode::F24,
            25 => KeyCode::F25,
            26 => KeyCode::F26,
            27 => KeyCode::F27,
            28 => KeyCode::F28,
            29 => KeyCode::F29,
            30 => KeyCode::F30,
            31 => KeyCode::F31,
            32 => KeyCode::F32,
            33 => KeyCode::F33,
            34 => KeyCode::F34,
            _ => KeyCode::F35,
        }
    }
}
/// Represents a key.
#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Hash)]
pub enum KeyCode {
    /// Backspace key (Delete on macOS, Backspace on other platforms).
    Backspace,
    /// Enter key.
    Enter,
    /// Left arrow key.
    Left,
    /// Right arrow key.
    Right,
    /// Up arrow key.
    Up,
    /// Down arrow key.
    Down,
    /// Home key.
    Home,
    /// End key.
    End,
    /// Page up key.
    PageUp,
    /// Page down key.
    PageDown,
    /// Tab key.
    Tab,
    /// Shift + Tab key.
    BackTab,
    /// Delete key. (Fn+Delete on macOS, Delete on other platforms)
    Delete,
    /// Insert key.
    Insert,
    /// F key.
    ///
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
    /// A character.
    ///
    /// `KeyCode::Char('c')` represents `c` character, etc.
    Char(char),
    /// Null.
    Null,
    /// Escape key.
    Esc,
    /// Caps Lock key.
    ///
    /// **Note:** this key can only be read if
    /// [`KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES`] has been enabled.
    CapsLock,
    /// Scroll Lock key.
    ///
    /// **Note:** this key can only be read if
    /// [`KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES`] has been enabled.
    ScrollLock,
    /// Num Lock key.
    ///
    /// **Note:** this key can only be read if
    /// [`KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES`] has been enabled.
    NumLock,
    /// Print Screen key.
    ///
    /// **Note:** this key can only be read if
    /// [`KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES`] has been enabled.
    PrintScreen,
    /// Pause key.
    ///
    /// **Note:** this key can only be read if
    /// [`KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES`] has been enabled.
    Pause,
    /// Menu key.
    ///
    /// **Note:** this key can only be read if
    /// [`KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES`] has been enabled.
    Menu,
    /// The "Begin" key (often mapped to the 5 key when Num Lock is turned on).
    ///
    /// **Note:** this key can only be read if
    /// [`KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES`] has been enabled.
    KeypadBegin,
    Play,
    /// Pause media key.
    PauseMedia,
    /// Play/Pause media key.
    PlayPause,
    /// Reverse media key.
    Reverse,
    /// Stop media key.
    Stop,
    /// Fast-forward media key.
    FastForward,
    /// Rewind media key.
    Rewind,
    /// Next-track media key.
    TrackNext,
    /// Previous-track media key.
    TrackPrevious,
    /// Record media key.
    Record,
    /// Lower-volume media key.
    LowerVolume,
    /// Raise-volume media key.
    RaiseVolume,
    /// Mute media key.
    MuteVolume,
    /// Left Shift key.
    LeftShift,
    /// Left Control key. (Control on macOS, Ctrl on other platforms)
    LeftControl,
    /// Left Alt key. (Option on macOS, Alt on other platforms)
    LeftAlt,
    /// Left Super key. (Command on macOS, Windows on Windows, Super on other platforms)
    LeftSuper,
    /// Left Hyper key.
    LeftHyper,
    /// Left Meta key.
    LeftMeta,
    /// Right Shift key.
    RightShift,
    /// Right Control key. (Control on macOS, Ctrl on other platforms)
    RightControl,
    /// Right Alt key. (Option on macOS, Alt on other platforms)
    RightAlt,
    /// Right Super key. (Command on macOS, Windows on Windows, Super on other platforms)
    RightSuper,
    /// Right Hyper key.
    RightHyper,
    /// Right Meta key.
    RightMeta,
    /// Iso Level3 Shift key.
    IsoLevel3Shift,
    /// Iso Level5 Shift key.
    IsoLevel5Shift,
}

impl KeyCode {
    /// Returns `true` if the key code is the given function key.
    ///
    /// # Examples
    ///
    /// ```
    /// # use extui::event::KeyCode;
    /// assert!(KeyCode::F1.is_function_key(1));
    /// assert!(!KeyCode::F1.is_function_key(2));
    /// ```
    pub fn is_function_key(&self, n: u8) -> bool {
        self == &KeyCode::function(n)
    }

    /// Returns `true` if the key code is the given character.
    ///
    /// # Examples
    ///
    /// ```
    /// # use extui::event::KeyCode;
    /// assert!(KeyCode::Char('a').is_char('a'));
    /// assert!(!KeyCode::Char('a').is_char('b'));
    /// assert!(!KeyCode::F1.is_char('a'));
    /// ```
    pub fn is_char(&self, c: char) -> bool {
        matches!(self, KeyCode::Char(m) if *m == c)
    }

    /// Returns the character if the key code is a character key.
    ///
    /// Returns `None` if the key code is not a character key.
    ///
    /// # Examples
    ///
    /// ```
    /// # use extui::event::KeyCode;
    /// assert_eq!(KeyCode::Char('a').as_char(), Some('a'));
    /// assert_eq!(KeyCode::F1.as_char(), None);
    /// ```
    pub fn as_char(&self) -> Option<char> {
        match self {
            KeyCode::Char(c) => Some(*c),
            _ => None,
        }
    }

    // /// Returns `true` if the key code is the given modifier key.
    // ///
    // /// **Note:** this method requires both
    // /// [`KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES`] and
    // /// [`KeyboardEnhancementFlags::REPORT_ALL_KEYS_AS_ESCAPE_CODES`] to be enabled with
    // /// [`PushKeyboardEnhancementFlags`].
    // ///
    // /// # Examples
    // ///
    // /// ```
    // /// # use extui::event::{KeyCode, ModifierKeyCode};
    // /// assert!(KeyCode::Modifier(ModifierKeyCode::LeftShift).is_modifier(ModifierKeyCode::LeftShift));
    // /// assert!(!KeyCode::Modifier(ModifierKeyCode::LeftShift).is_modifier(ModifierKeyCode::RightShift));
    // /// ```
    // pub fn is_modifier(&self, modifier: ModifierKeyCode) -> bool {
    //     matches!(self, KeyCode::Modifier(m) if *m == modifier)
    // }
}

impl Display for KeyCode {
    /// Formats the `KeyCode` using the given formatter.
    ///
    /// # Platform-specific Notes
    ///
    /// On macOS, the Backspace key is displayed as "Delete", the Delete key is displayed as "Fwd
    /// Del", and the Enter key is displayed as "Return". See
    /// <https://support.apple.com/guide/applestyleguide/welcome/1.0/web>.
    ///
    /// On other platforms, the Backspace key is displayed as "Backspace", the Delete key is
    /// displayed as "Del", and the Enter key is displayed as "Enter".
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            // On macOS, the Backspace key is called "Delete" and the Delete key is called "Fwd Del".
            #[cfg(target_os = "macos")]
            KeyCode::Backspace => "Delete",
            #[cfg(target_os = "macos")]
            KeyCode::Delete => "Fwd Del",

            #[cfg(not(target_os = "macos"))]
            KeyCode::Backspace => "Backspace",
            #[cfg(not(target_os = "macos"))]
            KeyCode::Delete => "Del",

            #[cfg(target_os = "macos")]
            KeyCode::Enter => "Return",
            #[cfg(not(target_os = "macos"))]
            KeyCode::Enter => "Enter",
            KeyCode::Left => "Left",
            KeyCode::Right => "Right",
            KeyCode::Up => "Up",
            KeyCode::Down => "Down",
            KeyCode::Home => "Home",
            KeyCode::End => "End",
            KeyCode::PageUp => "Page Up",
            KeyCode::PageDown => "Page Down",
            KeyCode::Tab => "Tab",
            KeyCode::BackTab => "Back Tab",
            KeyCode::Insert => "Insert",
            KeyCode::F1 => "F1",
            KeyCode::F2 => "F2",
            KeyCode::F3 => "F3",
            KeyCode::F4 => "F4",
            KeyCode::F5 => "F5",
            KeyCode::F6 => "F6",
            KeyCode::F7 => "F7",
            KeyCode::F8 => "F8",
            KeyCode::F9 => "F9",
            KeyCode::F10 => "F10",
            KeyCode::F11 => "F11",
            KeyCode::F12 => "F12",
            KeyCode::F13 => "F13",
            KeyCode::F14 => "F14",
            KeyCode::F15 => "F15",
            KeyCode::F16 => "F16",
            KeyCode::F17 => "F17",
            KeyCode::F18 => "F18",
            KeyCode::F19 => "F19",
            KeyCode::F20 => "F20",
            KeyCode::F21 => "F21",
            KeyCode::F22 => "F22",
            KeyCode::F23 => "F23",
            KeyCode::F24 => "F24",
            KeyCode::F25 => "F25",
            KeyCode::F26 => "F26",
            KeyCode::F27 => "F27",
            KeyCode::F28 => "F28",
            KeyCode::F29 => "F29",
            KeyCode::F30 => "F30",
            KeyCode::F31 => "F31",
            KeyCode::F32 => "F32",
            KeyCode::F33 => "F33",
            KeyCode::F34 => "F34",
            KeyCode::F35 => "F35",
            KeyCode::Char(c) => match c {
                // special case for non-visible characters
                ' ' => "Space",
                c => return f.write_char(*c),
            },
            KeyCode::Null => "Null",
            KeyCode::Esc => "Esc",
            KeyCode::CapsLock => "Caps Lock",
            KeyCode::ScrollLock => "Scroll Lock",
            KeyCode::NumLock => "Num Lock",
            KeyCode::PrintScreen => "Print Screen",
            KeyCode::Pause => "Pause",
            KeyCode::Menu => "Menu",
            KeyCode::KeypadBegin => "Begin",
            KeyCode::Play => "Play",
            KeyCode::PauseMedia => "Pause Media",
            KeyCode::PlayPause => "Play/Pause",
            KeyCode::Reverse => "Reverse",
            KeyCode::Stop => "Stop",
            KeyCode::FastForward => "Fast Forward",
            KeyCode::Rewind => "Rewind",
            KeyCode::TrackNext => "Next Track",
            KeyCode::TrackPrevious => "Previous Track",
            KeyCode::Record => "Record",
            KeyCode::LowerVolume => "Lower Volume",
            KeyCode::RaiseVolume => "Raise Volume",
            KeyCode::MuteVolume => "Mute Volume",
            KeyCode::LeftShift => "Left Shift",
            KeyCode::LeftHyper => "Left Hyper",
            KeyCode::LeftMeta => "Left Meta",
            KeyCode::RightShift => "Right Shift",
            KeyCode::RightHyper => "Right Hyper",
            KeyCode::RightMeta => "Right Meta",
            KeyCode::IsoLevel3Shift => "Iso Level 3 Shift",
            KeyCode::IsoLevel5Shift => "Iso Level 5 Shift",

            #[cfg(target_os = "macos")]
            KeyCode::LeftControl => "Left Control",
            #[cfg(not(target_os = "macos"))]
            KeyCode::LeftControl => "Left Ctrl",

            #[cfg(target_os = "macos")]
            KeyCode::LeftAlt => "Left Option",
            #[cfg(not(target_os = "macos"))]
            KeyCode::LeftAlt => "Left Alt",

            #[cfg(target_os = "macos")]
            KeyCode::LeftSuper => "Left Command",
            #[cfg(target_os = "windows")]
            KeyCode::LeftSuper => "Left Windows",
            #[cfg(not(any(target_os = "macos", target_os = "windows")))]
            KeyCode::LeftSuper => "Left Super",

            #[cfg(target_os = "macos")]
            KeyCode::RightControl => "Right Control",
            #[cfg(not(target_os = "macos"))]
            KeyCode::RightControl => "Right Ctrl",

            #[cfg(target_os = "macos")]
            KeyCode::RightAlt => "Right Option",
            #[cfg(not(target_os = "macos"))]
            KeyCode::RightAlt => "Right Alt",

            #[cfg(target_os = "macos")]
            KeyCode::RightSuper => "Right Command",
            #[cfg(target_os = "windows")]
            KeyCode::RightSuper => "Right Windows",
            #[cfg(not(any(target_os = "macos", target_os = "windows")))]
            KeyCode::RightSuper => "Right Super",
        };
        f.write_str(text)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    use super::*;
    use KeyCode::*;

    #[test]
    fn test_equality() {
        let lowercase_d_with_shift = KeyEvent::new(KeyCode::Char('d'), KeyModifiers::SHIFT);
        let uppercase_d_with_shift = KeyEvent::new(KeyCode::Char('D'), KeyModifiers::SHIFT);
        let uppercase_d = KeyEvent::new(KeyCode::Char('D'), KeyModifiers::NONE);
        assert_eq!(lowercase_d_with_shift, uppercase_d_with_shift);
        assert_eq!(uppercase_d, uppercase_d_with_shift);
    }

    #[test]
    fn test_hash() {
        let lowercase_d_with_shift_hash = {
            let mut hasher = DefaultHasher::new();
            KeyEvent::new(KeyCode::Char('d'), KeyModifiers::SHIFT).hash(&mut hasher);
            hasher.finish()
        };
        let uppercase_d_with_shift_hash = {
            let mut hasher = DefaultHasher::new();
            KeyEvent::new(KeyCode::Char('D'), KeyModifiers::SHIFT).hash(&mut hasher);
            hasher.finish()
        };
        let uppercase_d_hash = {
            let mut hasher = DefaultHasher::new();
            KeyEvent::new(KeyCode::Char('D'), KeyModifiers::NONE).hash(&mut hasher);
            hasher.finish()
        };
        assert_eq!(lowercase_d_with_shift_hash, uppercase_d_with_shift_hash);
        assert_eq!(uppercase_d_hash, uppercase_d_with_shift_hash);
    }

    #[test]
    fn keycode_display() {
        #[cfg(target_os = "macos")]
        {
            assert_eq!(format!("{Backspace}"), "Delete");
            assert_eq!(format!("{Delete}"), "Fwd Del");
            assert_eq!(format!("{Enter}"), "Return");
        }
        #[cfg(not(target_os = "macos"))]
        {
            assert_eq!(format!("{}", Backspace), "Backspace");
            assert_eq!(format!("{}", Delete), "Del");
            assert_eq!(format!("{}", Enter), "Enter");
        }
        assert_eq!(format!("{Left}"), "Left");
        assert_eq!(format!("{Right}"), "Right");
        assert_eq!(format!("{Up}"), "Up");
        assert_eq!(format!("{Down}"), "Down");
        assert_eq!(format!("{Home}"), "Home");
        assert_eq!(format!("{End}"), "End");
        assert_eq!(format!("{PageUp}"), "Page Up");
        assert_eq!(format!("{PageDown}"), "Page Down");
        assert_eq!(format!("{Tab}"), "Tab");
        assert_eq!(format!("{BackTab}"), "Back Tab");
        assert_eq!(format!("{Insert}"), "Insert");
        assert_eq!(format!("{}", F1), "F1");
        assert_eq!(format!("{}", Char('a')), "a");
        assert_eq!(format!("{Null}"), "Null");
        assert_eq!(format!("{Esc}"), "Esc");
        assert_eq!(format!("{CapsLock}"), "Caps Lock");
        assert_eq!(format!("{ScrollLock}"), "Scroll Lock");
        assert_eq!(format!("{NumLock}"), "Num Lock");
        assert_eq!(format!("{PrintScreen}"), "Print Screen");
        assert_eq!(format!("{}", KeyCode::Pause), "Pause");
        assert_eq!(format!("{Menu}"), "Menu");
        assert_eq!(format!("{KeypadBegin}"), "Begin");
    }

    #[test]
    fn media_keycode_display() {
        assert_eq!(format!("{}", Play), "Play");
        assert_eq!(format!("{}", PauseMedia), "Pause Media");
        assert_eq!(format!("{}", PlayPause), "Play/Pause");
        assert_eq!(format!("{}", Reverse), "Reverse");
        assert_eq!(format!("{}", Stop), "Stop");
        assert_eq!(format!("{}", FastForward), "Fast Forward");
        assert_eq!(format!("{}", Rewind), "Rewind");
        assert_eq!(format!("{}", TrackNext), "Next Track");
        assert_eq!(format!("{}", TrackPrevious), "Previous Track");
        assert_eq!(format!("{}", Record), "Record");
        assert_eq!(format!("{}", LowerVolume), "Lower Volume");
        assert_eq!(format!("{}", RaiseVolume), "Raise Volume");
        assert_eq!(format!("{}", MuteVolume), "Mute Volume");
    }

    #[test]
    fn modifier_keycode_display() {
        assert_eq!(format!("{}", LeftShift), "Left Shift");
        assert_eq!(format!("{}", LeftHyper), "Left Hyper");
        assert_eq!(format!("{}", LeftMeta), "Left Meta");
        assert_eq!(format!("{}", RightShift), "Right Shift");
        assert_eq!(format!("{}", RightHyper), "Right Hyper");
        assert_eq!(format!("{}", RightMeta), "Right Meta");
        assert_eq!(format!("{}", IsoLevel3Shift), "Iso Level 3 Shift");
        assert_eq!(format!("{}", IsoLevel5Shift), "Iso Level 5 Shift");
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn modifier_keycode_display_macos() {
        assert_eq!(format!("{}", Modifier(LeftControl)), "Left Control");
        assert_eq!(format!("{}", Modifier(LeftAlt)), "Left Option");
        assert_eq!(format!("{}", Modifier(LeftSuper)), "Left Command");
        assert_eq!(format!("{}", Modifier(RightControl)), "Right Control");
        assert_eq!(format!("{}", Modifier(RightAlt)), "Right Option");
        assert_eq!(format!("{}", Modifier(RightSuper)), "Right Command");
    }

    #[cfg(target_os = "windows")]
    #[test]
    fn modifier_keycode_display_windows() {
        assert_eq!(format!("{}", Modifier(LeftControl)), "Left Ctrl");
        assert_eq!(format!("{}", Modifier(LeftAlt)), "Left Alt");
        assert_eq!(format!("{}", Modifier(LeftSuper)), "Left Windows");
        assert_eq!(format!("{}", Modifier(RightControl)), "Right Ctrl");
        assert_eq!(format!("{}", Modifier(RightAlt)), "Right Alt");
        assert_eq!(format!("{}", Modifier(RightSuper)), "Right Windows");
    }

    #[cfg(not(any(target_os = "macos", target_os = "windows")))]
    #[test]
    fn modifier_keycode_display_other() {
        assert_eq!(format!("{}", LeftControl), "Left Ctrl");
        assert_eq!(format!("{}", LeftAlt), "Left Alt");
        assert_eq!(format!("{}", LeftSuper), "Left Super");
        assert_eq!(format!("{}", RightControl), "Right Ctrl");
        assert_eq!(format!("{}", RightAlt), "Right Alt");
        assert_eq!(format!("{}", RightSuper), "Right Super");
    }

    #[test]
    fn key_modifiers_display() {
        let modifiers = KeyModifiers::SHIFT | KeyModifiers::CONTROL | KeyModifiers::ALT;

        #[cfg(not(any(target_os = "macos", target_os = "windows")))]
        assert_eq!(modifiers.to_string(), "Shift+Control+Alt");
    }

    const ESC_PRESSED: KeyEvent =
        KeyEvent::new_with_kind(KeyCode::Esc, KeyModifiers::empty(), KeyEventKind::Press);
    const ESC_RELEASED: KeyEvent =
        KeyEvent::new_with_kind(KeyCode::Esc, KeyModifiers::empty(), KeyEventKind::Release);
    const ESC_REPEAT: KeyEvent =
        KeyEvent::new_with_kind(KeyCode::Esc, KeyModifiers::empty(), KeyEventKind::Repeat);
    const MOUSE_CLICK: MouseEvent = MouseEvent {
        kind: MouseEventKind::Down(MouseButton::Left),
        column: 1,
        row: 1,
        modifiers: KeyModifiers::empty(),
    };

    #[test]
    fn event_as() {
        let event = Event::FocusGained;
        assert_eq!(event.as_key_event(), None);

        let event = Event::Key(ESC_PRESSED);
        assert_eq!(event.as_key_event(), Some(ESC_PRESSED));
        assert_eq!(event.as_key_press_event(), Some(ESC_PRESSED));
        assert_eq!(event.as_key_release_event(), None);
        assert_eq!(event.as_key_repeat_event(), None);

        let event = Event::Key(ESC_RELEASED);
        assert_eq!(event.as_key_event(), Some(ESC_RELEASED));
        assert_eq!(event.as_key_release_event(), Some(ESC_RELEASED));
        assert_eq!(event.as_key_press_event(), None);
        assert_eq!(event.as_key_repeat_event(), None);

        let event = Event::Key(ESC_REPEAT);
        assert_eq!(event.as_key_event(), Some(ESC_REPEAT));
        assert_eq!(event.as_key_repeat_event(), Some(ESC_REPEAT));
        assert_eq!(event.as_key_press_event(), None);
        assert_eq!(event.as_key_release_event(), None);

        let event = Event::Resized;
        assert_eq!(event.as_key_event(), None);

        let event = Event::Mouse(MOUSE_CLICK);
        assert_eq!(event.as_mouse_event(), Some(MOUSE_CLICK));
        assert_eq!(event.as_key_event(), None);

        #[cfg(feature = "bracketed-paste")]
        {
            let event = Event::Paste("".to_string());
            assert_eq!(event.as_paste_event(), Some(""));
            assert_eq!(event.as_key_event(), None);
        }
    }
}
