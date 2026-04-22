//! Textual parsers for [`InputKey`] tokens and key sequences.
//!
//! Two interchangeable dialects are accepted: Emacs-style (`C-c`,
//! `M-x`, `F1`) and vim-style angle-bracketed (`<C-c>`, `<M-x>`,
//! `<CR>`). Whitespace-separated sequences are parsed by
//! [`parse_sequence`], which is how chord bindings are typically
//! written in config files.

use crate::key::{InputKey, NamedKey};
use extui::event::KeyModifiers;
use std::fmt;

/// The error returned by [`parse_key`] and [`parse_sequence`] when a
/// token cannot be resolved to an [`InputKey`].
///
/// The original text is preserved and can be retrieved with
/// [`ParseError::input`]; a human-readable description is available
/// through the [`fmt::Display`] impl.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    input: String,
    kind: ParseErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ParseErrorKind {
    Empty,
    TrailingDash,
    UnknownKey(String),
    MismatchedBrackets,
}

impl ParseError {
    /// Returns the original, unmodified input that failed to parse.
    pub fn input(&self) -> &str {
        &self.input
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ParseErrorKind::Empty => write!(f, "empty key binding `{}`", self.input),
            ParseErrorKind::TrailingDash => {
                write!(f, "trailing modifier prefix in `{}`", self.input)
            }
            ParseErrorKind::UnknownKey(k) => {
                write!(f, "unknown key `{k}` in `{}`", self.input)
            }
            ParseErrorKind::MismatchedBrackets => {
                write!(f, "mismatched angle brackets in `{}`", self.input)
            }
        }
    }
}

impl std::error::Error for ParseError {}

/// Parses a single key token into an [`InputKey`].
///
/// Tokens take one of two interchangeable forms:
///
/// - Emacs-style: `c`, `C-c`, `C-M-x`, `Space`, `Enter`, `F1`.
/// - Vim-style (angle-bracketed): `<C-c>`, `<M-x>`, `<Space>`,
///   `<CR>`, `<F1>`.
///
/// Modifier prefixes are case-sensitive and separated from the payload
/// by `-`:
///
/// | prefix                   | modifier bit              |
/// |--------------------------|---------------------------|
/// | `C`, `Ctrl`              | [`KeyModifiers::CONTROL`] |
/// | `S`, `Shift`             | [`KeyModifiers::SHIFT`]   |
/// | `A`, `Alt`, `M`, `Meta`  | [`KeyModifiers::ALT`]     |
/// | `D`, `Super`, `Cmd`      | [`KeyModifiers::SUPER`]   |
/// | `H`, `Hyper`             | [`KeyModifiers::HYPER`]   |
/// | `T`                      | [`KeyModifiers::META`]    |
///
/// `M-` maps to `ALT` because terminals almost never surface a distinct
/// Meta modifier. Named-key aliases themselves are matched
/// case-insensitively (`Esc`, `esc`, and `ESCAPE` all parse to
/// [`NamedKey::Esc`]).
///
/// # Errors
///
/// Returns [`ParseError`] when the input is empty, has mismatched angle
/// brackets, ends with a dangling modifier prefix (e.g. `C-`), or the
/// payload does not match a known alias or single character.
///
/// # Examples
///
/// ```
/// use extui::event::KeyModifiers;
/// use extui_bindings::{parse_key, InputKey, NamedKey};
///
/// assert_eq!(parse_key("C-c").unwrap(), InputKey::char('c', KeyModifiers::CONTROL));
/// assert_eq!(parse_key("<C-c>").unwrap(), InputKey::char('c', KeyModifiers::CONTROL));
/// assert_eq!(parse_key("<CR>").unwrap(), InputKey::named(NamedKey::Enter, KeyModifiers::empty()));
/// assert!(parse_key("C-").is_err());
/// ```
pub fn parse_key(src: &str) -> Result<InputKey, ParseError> {
    let input = src.trim();
    if input.is_empty() {
        return Err(ParseError {
            input: src.to_owned(),
            kind: ParseErrorKind::Empty,
        });
    }

    let body = if let Some(rest) = input.strip_prefix('<') {
        let Some(body) = rest.strip_suffix('>') else {
            return Err(ParseError {
                input: src.to_owned(),
                kind: ParseErrorKind::MismatchedBrackets,
            });
        };
        body
    } else if input.ends_with('>') {
        return Err(ParseError {
            input: src.to_owned(),
            kind: ParseErrorKind::MismatchedBrackets,
        });
    } else {
        input
    };

    parse_token(body, src)
}

/// Parses a whitespace-separated sequence of key tokens.
///
/// Each token is forwarded to [`parse_key`]. An empty or
/// whitespace-only input returns an empty [`Vec`].
///
/// # Errors
///
/// Returns the [`ParseError`] produced by the first token that fails to
/// parse.
///
/// # Examples
///
/// ```
/// use extui::event::KeyModifiers;
/// use extui_bindings::{parse_sequence, InputKey};
///
/// let keys = parse_sequence("g g").unwrap();
/// assert_eq!(keys, vec![
///     InputKey::char('g', KeyModifiers::empty()),
///     InputKey::char('g', KeyModifiers::empty()),
/// ]);
/// assert!(parse_sequence("").unwrap().is_empty());
/// ```
pub fn parse_sequence(src: &str) -> Result<Vec<InputKey>, ParseError> {
    let mut out = Vec::new();
    for token in src.split_ascii_whitespace() {
        out.push(parse_key(token)?);
    }
    Ok(out)
}

fn parse_token(body: &str, original: &str) -> Result<InputKey, ParseError> {
    if body.is_empty() {
        return Err(ParseError {
            input: original.to_owned(),
            kind: ParseErrorKind::Empty,
        });
    }

    let mut mods = KeyModifiers::empty();
    let mut rem = body;

    loop {
        let Some((prefix, rest)) = rem.split_once('-') else {
            break;
        };

        let Some(modifier) = modifier_for_prefix(prefix) else {
            break;
        };

        if rest.is_empty() {
            return Err(ParseError {
                input: original.to_owned(),
                kind: ParseErrorKind::TrailingDash,
            });
        }

        mods |= modifier;
        rem = rest;
    }

    let Some(key) = parse_payload(rem, mods) else {
        return Err(ParseError {
            input: original.to_owned(),
            kind: ParseErrorKind::UnknownKey(rem.to_owned()),
        });
    };

    Ok(key)
}

fn modifier_for_prefix(prefix: &str) -> Option<KeyModifiers> {
    let modifier = match prefix {
        "C" | "Ctrl" => KeyModifiers::CONTROL,
        "S" | "Shift" => KeyModifiers::SHIFT,
        "A" | "Alt" | "M" | "Meta" => KeyModifiers::ALT,
        "D" | "Super" | "Cmd" => KeyModifiers::SUPER,
        "H" | "Hyper" => KeyModifiers::HYPER,
        "T" => KeyModifiers::META,
        _ => return None,
    };
    Some(modifier)
}

fn parse_payload(body: &str, mods: KeyModifiers) -> Option<InputKey> {
    let normalized = body.to_ascii_lowercase();

    if let Some(c) = char_alias(&normalized) {
        return Some(InputKey::char(c, mods));
    }
    if let Some(name) = fn_key_alias(&normalized) {
        return Some(InputKey::named(name, mods));
    }
    if let Some(name) = named_alias(&normalized) {
        return Some(InputKey::named(name, mods));
    }

    let mut chars = body.chars();
    let Some(c) = chars.next() else {
        return None;
    };
    if chars.next().is_some() {
        return None;
    }
    Some(InputKey::char(c, mods))
}

fn char_alias(normalized: &str) -> Option<char> {
    let c = match normalized {
        "space" => ' ',
        "lt" => '<',
        "gt" => '>',
        "bslash" => '\\',
        "bar" => '|',
        _ => return None,
    };
    Some(c)
}

fn named_alias(normalized: &str) -> Option<NamedKey> {
    let key = match normalized {
        "backspace" | "bksp" | "bs" => NamedKey::Backspace,
        "enter" | "return" | "cr" => NamedKey::Enter,
        "left" => NamedKey::Left,
        "right" => NamedKey::Right,
        "up" => NamedKey::Up,
        "down" => NamedKey::Down,
        "home" => NamedKey::Home,
        "end" => NamedKey::End,
        "pageup" | "pgup" | "page_up" => NamedKey::PageUp,
        "pagedown" | "pgdn" | "page_down" => NamedKey::PageDown,
        "tab" => NamedKey::Tab,
        "backtab" | "bktab" | "back_tab" => NamedKey::BackTab,
        "delete" | "del" => NamedKey::Delete,
        "insert" | "ins" => NamedKey::Insert,
        "null" | "nul" => NamedKey::Null,
        "escape" | "esc" => NamedKey::Esc,
        "caps" | "capslock" | "caps_lock" => NamedKey::CapsLock,
        "scrolllock" | "scrlk" | "scroll_lock" => NamedKey::ScrollLock,
        "numlock" | "num_lock" => NamedKey::NumLock,
        "printscreen" | "prtsc" | "print_screen" => NamedKey::PrintScreen,
        "pause" => NamedKey::Pause,
        "menu" => NamedKey::Menu,
        "keypadbegin" | "keypad_begin" => NamedKey::KeypadBegin,
        "play" => NamedKey::Play,
        "pausemedia" => NamedKey::PauseMedia,
        "playpause" => NamedKey::PlayPause,
        "reverse" => NamedKey::Reverse,
        "stop" => NamedKey::Stop,
        "fastforward" => NamedKey::FastForward,
        "rewind" => NamedKey::Rewind,
        "tracknext" => NamedKey::TrackNext,
        "trackprevious" => NamedKey::TrackPrevious,
        "record" => NamedKey::Record,
        "lowervolume" | "voldown" => NamedKey::LowerVolume,
        "raisevolume" | "volup" => NamedKey::RaiseVolume,
        "mutevolume" | "mute" => NamedKey::MuteVolume,
        _ => return None,
    };
    Some(key)
}

fn fn_key_alias(normalized: &str) -> Option<NamedKey> {
    let rest = normalized.strip_prefix('f')?;
    if rest.is_empty() {
        return None;
    }
    let n: u32 = rest.parse().ok()?;
    NamedKey::from_f_number(n)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bare_char() {
        assert_eq!(
            parse_key("a").unwrap(),
            InputKey::char('a', KeyModifiers::empty())
        );
    }

    #[test]
    fn ctrl_c_emacs_and_vim() {
        let want = InputKey::char('c', KeyModifiers::CONTROL);
        assert_eq!(parse_key("C-c").unwrap(), want);
        assert_eq!(parse_key("<C-c>").unwrap(), want);
        assert_eq!(parse_key("Ctrl-c").unwrap(), want);
    }

    #[test]
    fn named_key_aliases() {
        let esc = InputKey::named(NamedKey::Esc, KeyModifiers::empty());
        assert_eq!(parse_key("Esc").unwrap(), esc);
        assert_eq!(parse_key("ESCAPE").unwrap(), esc);
        assert_eq!(parse_key("<esc>").unwrap(), esc);

        let enter = InputKey::named(NamedKey::Enter, KeyModifiers::empty());
        assert_eq!(parse_key("<CR>").unwrap(), enter);
        assert_eq!(parse_key("Return").unwrap(), enter);
    }

    #[test]
    fn space_alias() {
        let space = InputKey::char(' ', KeyModifiers::empty());
        assert_eq!(parse_key("Space").unwrap(), space);
        assert_eq!(parse_key("<Space>").unwrap(), space);
        assert_eq!(
            parse_key("C-Space").unwrap(),
            InputKey::char(' ', KeyModifiers::CONTROL)
        );
    }

    #[test]
    fn multiple_modifiers() {
        let key = parse_key("C-M-x").unwrap();
        assert_eq!(key.as_char(), Some('x'));
        assert!(key.modifiers().contains(KeyModifiers::CONTROL));
        assert!(key.modifiers().contains(KeyModifiers::ALT));
    }

    #[test]
    fn fn_keys() {
        assert_eq!(
            parse_key("F5").unwrap(),
            InputKey::named(NamedKey::F5, KeyModifiers::empty())
        );
        assert_eq!(
            parse_key("<C-F12>").unwrap(),
            InputKey::named(NamedKey::F12, KeyModifiers::CONTROL)
        );
    }

    #[test]
    fn dash_char() {
        assert_eq!(
            parse_key("-").unwrap(),
            InputKey::char('-', KeyModifiers::empty())
        );
        let key = parse_key("C--").unwrap();
        assert_eq!(key.as_char(), Some('-'));
        assert_eq!(key.modifiers(), KeyModifiers::CONTROL);
    }

    #[test]
    fn lt_gt_aliases() {
        assert_eq!(
            parse_key("<lt>").unwrap(),
            InputKey::char('<', KeyModifiers::empty())
        );
        assert_eq!(
            parse_key("<gt>").unwrap(),
            InputKey::char('>', KeyModifiers::empty())
        );
    }

    #[test]
    fn error_cases() {
        assert!(parse_key("").is_err());
        assert!(parse_key("C-").is_err());
        assert!(parse_key("<C-c").is_err());
        assert!(parse_key("Nonsense").is_err());
    }

    #[test]
    fn sequence() {
        let keys = parse_sequence("g g").unwrap();
        assert_eq!(keys.len(), 2);
        assert_eq!(keys[0], InputKey::char('g', KeyModifiers::empty()));
        assert_eq!(keys[1], InputKey::char('g', KeyModifiers::empty()));

        let keys = parse_sequence("<C-x> <C-s>").unwrap();
        assert_eq!(keys.len(), 2);
        assert_eq!(keys[0], InputKey::char('x', KeyModifiers::CONTROL));
        assert_eq!(keys[1], InputKey::char('s', KeyModifiers::CONTROL));
    }

    #[test]
    fn empty_sequence() {
        assert!(parse_sequence("").unwrap().is_empty());
    }
}
