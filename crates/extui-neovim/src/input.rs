//! Translation from extui input events to Neovim key-notation strings.
//!
//! Translation is written into a caller-supplied scratch [`String`]
//! so no allocation is needed per keystroke. The resulting string is
//! suitable for the `nvim_input` RPC call.

use extui::event::{KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};

/// Appends the Neovim key notation for `key` to `out`.
///
/// Returns `false` for keys that have no Neovim translation such as
/// modifier-only, media, or lock keys. The caller should drop events
/// that return `false`.
pub fn write_key(out: &mut String, key: &KeyEvent) -> bool {
    let mods = key.modifiers;
    let has_ctrl = mods.contains(KeyModifiers::CONTROL);
    let has_alt = mods.contains(KeyModifiers::ALT) || mods.contains(KeyModifiers::META);
    let has_shift = mods.contains(KeyModifiers::SHIFT);

    match key.code {
        KeyCode::Char(c) => write_char(out, c, has_ctrl, has_alt, has_shift),
        KeyCode::Enter => write_named(out, "CR", has_ctrl, has_alt, false),
        KeyCode::Esc => write_named(out, "Esc", has_ctrl, has_alt, false),
        KeyCode::Backspace => write_named(out, "BS", has_ctrl, has_alt, false),
        KeyCode::Tab => write_named(out, "Tab", has_ctrl, has_alt, has_shift),
        KeyCode::BackTab => write_named(out, "Tab", has_ctrl, has_alt, true),
        KeyCode::Delete => write_named(out, "Del", has_ctrl, has_alt, has_shift),
        KeyCode::Insert => write_named(out, "Insert", has_ctrl, has_alt, has_shift),
        KeyCode::Home => write_named(out, "Home", has_ctrl, has_alt, has_shift),
        KeyCode::End => write_named(out, "End", has_ctrl, has_alt, has_shift),
        KeyCode::PageUp => write_named(out, "PageUp", has_ctrl, has_alt, has_shift),
        KeyCode::PageDown => write_named(out, "PageDown", has_ctrl, has_alt, has_shift),
        KeyCode::Up => write_named(out, "Up", has_ctrl, has_alt, has_shift),
        KeyCode::Down => write_named(out, "Down", has_ctrl, has_alt, has_shift),
        KeyCode::Left => write_named(out, "Left", has_ctrl, has_alt, has_shift),
        KeyCode::Right => write_named(out, "Right", has_ctrl, has_alt, has_shift),
        KeyCode::Null => false,
        code if matches!(code, KeyCode::F1) => write_fn(out, 1, has_ctrl, has_alt, has_shift),
        code => {
            let n = function_key_number(code);
            if n > 0 {
                return write_fn(out, n, has_ctrl, has_alt, has_shift);
            }
            false
        }
    }
}

fn function_key_number(code: KeyCode) -> u8 {
    match code {
        KeyCode::F1 => 1,
        KeyCode::F2 => 2,
        KeyCode::F3 => 3,
        KeyCode::F4 => 4,
        KeyCode::F5 => 5,
        KeyCode::F6 => 6,
        KeyCode::F7 => 7,
        KeyCode::F8 => 8,
        KeyCode::F9 => 9,
        KeyCode::F10 => 10,
        KeyCode::F11 => 11,
        KeyCode::F12 => 12,
        KeyCode::F13 => 13,
        KeyCode::F14 => 14,
        KeyCode::F15 => 15,
        KeyCode::F16 => 16,
        KeyCode::F17 => 17,
        KeyCode::F18 => 18,
        KeyCode::F19 => 19,
        KeyCode::F20 => 20,
        _ => 0,
    }
}

fn write_modifier_prefix(out: &mut String, ctrl: bool, alt: bool, shift: bool) {
    if ctrl {
        out.push_str("C-");
    }
    if alt {
        out.push_str("M-");
    }
    if shift {
        out.push_str("S-");
    }
}

fn write_named(out: &mut String, name: &str, ctrl: bool, alt: bool, shift: bool) -> bool {
    out.push('<');
    write_modifier_prefix(out, ctrl, alt, shift);
    out.push_str(name);
    out.push('>');
    true
}

fn write_fn(out: &mut String, n: u8, ctrl: bool, alt: bool, shift: bool) -> bool {
    out.push('<');
    write_modifier_prefix(out, ctrl, alt, shift);
    out.push('F');
    // Function keys are F1..=F20 per `function_key_number`, so we only
    // ever need one or two ASCII digits — bypass `core::fmt` entirely.
    if n >= 10 {
        out.push((b'0' + n / 10) as char);
    }
    out.push((b'0' + n % 10) as char);
    out.push('>');
    true
}

fn write_char(out: &mut String, c: char, ctrl: bool, alt: bool, shift: bool) -> bool {
    if c == '<' {
        if ctrl || alt {
            out.push('<');
            write_modifier_prefix(out, ctrl, alt, false);
            out.push_str("lt>");
        } else {
            out.push_str("<lt>");
        }
        return true;
    }
    if c == ' ' && (ctrl || alt) {
        out.push('<');
        write_modifier_prefix(out, ctrl, alt, false);
        out.push_str("Space>");
        return true;
    }
    if ctrl || alt {
        out.push('<');
        write_modifier_prefix(out, ctrl, alt, false);
        if shift && c.is_ascii_lowercase() {
            out.push((c as u8 - b'a' + b'A') as char);
        } else {
            out.push(c);
        }
        out.push('>');
        return true;
    }
    out.push(c);
    true
}

/// A translated mouse event ready to hand to `nvim_input_mouse`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NvimMouse {
    /// Button name: `"left"`, `"right"`, `"middle"`, `"wheel"`.
    pub button: &'static str,
    /// Action name: `"press"`, `"release"`, `"drag"`, `"up"`, `"down"`, `"left"`, `"right"`.
    pub action: &'static str,
    /// Modifier string such as `"C-"` or `"S-M-"`.
    pub modifier: String,
    /// Zero-based grid row (relative to the embedding rect).
    pub row: u16,
    /// Zero-based grid column (relative to the embedding rect).
    pub col: u16,
}

/// Translates a [`MouseEvent`] against an embedding rect at `(rect_x, rect_y)`
/// of size `(rect_w, rect_h)`, returning `None` if the event falls outside.
///
/// `MouseEventKind::Moved` always returns `None` since Neovim does not
/// distinguish hover events in `nvim_input_mouse`.
pub fn translate_mouse(
    mouse: &MouseEvent,
    rect_x: u16,
    rect_y: u16,
    rect_w: u16,
    rect_h: u16,
) -> Option<NvimMouse> {
    if mouse.column < rect_x || mouse.row < rect_y {
        return None;
    }
    let col = mouse.column - rect_x;
    let row = mouse.row - rect_y;
    if col >= rect_w || row >= rect_h {
        return None;
    }
    let (button, action) = match mouse.kind {
        MouseEventKind::Down(b) => (mouse_button_name(b), "press"),
        MouseEventKind::Up(b) => (mouse_button_name(b), "release"),
        MouseEventKind::Drag(b) => (mouse_button_name(b), "drag"),
        MouseEventKind::ScrollUp => ("wheel", "up"),
        MouseEventKind::ScrollDown => ("wheel", "down"),
        MouseEventKind::ScrollLeft => ("wheel", "left"),
        MouseEventKind::ScrollRight => ("wheel", "right"),
        MouseEventKind::Moved => return None,
    };
    let mut modifier = String::new();
    if mouse.modifiers.contains(KeyModifiers::CONTROL) {
        modifier.push_str("C-");
    }
    if mouse.modifiers.contains(KeyModifiers::SHIFT) {
        modifier.push_str("S-");
    }
    if mouse.modifiers.contains(KeyModifiers::ALT) || mouse.modifiers.contains(KeyModifiers::META) {
        modifier.push_str("M-");
    }
    Some(NvimMouse {
        button,
        action,
        modifier,
        row,
        col,
    })
}

fn mouse_button_name(b: MouseButton) -> &'static str {
    match b {
        MouseButton::Left => "left",
        MouseButton::Middle => "middle",
        MouseButton::Right => "right",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use extui::event::{KeyEventKind, KeyEventState};

    fn key(code: KeyCode, mods: KeyModifiers) -> KeyEvent {
        KeyEvent {
            code,
            modifiers: mods,
            kind: KeyEventKind::Press,
            state: KeyEventState::empty(),
        }
    }

    fn render(k: KeyEvent) -> String {
        let mut s = String::new();
        assert!(write_key(&mut s, &k));
        s
    }

    #[test]
    fn plain_char() {
        assert_eq!(render(key(KeyCode::Char('a'), KeyModifiers::NONE)), "a");
    }

    #[test]
    fn ctrl_c() {
        assert_eq!(
            render(key(KeyCode::Char('c'), KeyModifiers::CONTROL)),
            "<C-c>"
        );
    }

    #[test]
    fn escape_lt() {
        assert_eq!(render(key(KeyCode::Char('<'), KeyModifiers::NONE)), "<lt>");
        assert_eq!(
            render(key(KeyCode::Char('<'), KeyModifiers::CONTROL)),
            "<C-lt>"
        );
    }

    #[test]
    fn alt_space() {
        assert_eq!(
            render(key(KeyCode::Char(' '), KeyModifiers::ALT)),
            "<M-Space>"
        );
    }

    #[test]
    fn named_keys() {
        assert_eq!(render(key(KeyCode::Enter, KeyModifiers::NONE)), "<CR>");
        assert_eq!(render(key(KeyCode::Esc, KeyModifiers::NONE)), "<Esc>");
        assert_eq!(render(key(KeyCode::Backspace, KeyModifiers::NONE)), "<BS>");
        assert_eq!(render(key(KeyCode::Tab, KeyModifiers::NONE)), "<Tab>");
        assert_eq!(render(key(KeyCode::BackTab, KeyModifiers::NONE)), "<S-Tab>");
        assert_eq!(render(key(KeyCode::Delete, KeyModifiers::NONE)), "<Del>");
    }

    #[test]
    fn function_keys() {
        assert_eq!(render(key(KeyCode::F1, KeyModifiers::NONE)), "<F1>");
        assert_eq!(render(key(KeyCode::F12, KeyModifiers::NONE)), "<F12>");
        assert_eq!(
            render(key(KeyCode::F5, KeyModifiers::CONTROL | KeyModifiers::ALT)),
            "<C-M-F5>"
        );
    }

    #[test]
    fn mouse_inside_rect() {
        let m = MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: 12,
            row: 6,
            modifiers: KeyModifiers::NONE,
        };
        let t = translate_mouse(&m, 10, 5, 10, 10).unwrap();
        assert_eq!(t.button, "left");
        assert_eq!(t.action, "press");
        assert_eq!(t.col, 2);
        assert_eq!(t.row, 1);
    }

    #[test]
    fn mouse_outside_rect() {
        let m = MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: 4,
            row: 4,
            modifiers: KeyModifiers::NONE,
        };
        assert!(translate_mouse(&m, 10, 5, 10, 10).is_none());
    }

    #[test]
    fn mouse_scroll() {
        let m = MouseEvent {
            kind: MouseEventKind::ScrollUp,
            column: 10,
            row: 5,
            modifiers: KeyModifiers::NONE,
        };
        let t = translate_mouse(&m, 10, 5, 10, 10).unwrap();
        assert_eq!(t.button, "wheel");
        assert_eq!(t.action, "up");
    }
}
