use extui::{
    TerminalFlags,
    event::{Event, KeyCode, KeyEvent, KeyModifiers},
};
use std::{io::Write, time::Duration};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mode = TerminalFlags::RAW_MODE
        | TerminalFlags::MOUSE_CAPTURE
        | TerminalFlags::EXTENDED_KEYBOARD_INPUTS;

    let mut terminal = extui::Terminal::open(mode).expect("Valid TTY");
    let mut events = extui::event::Events::default();
    let stdin = std::io::stdin();
    write!(terminal, "Press Ctrl+C to exit. Raw bytes are printed per read.\n\r")?;
    loop {
        match extui::event::poll(&stdin, Some(Duration::from_secs(1)))? {
            extui::event::Polled::ReadReady => {
                events.read_from(&stdin)?;
                // The previous iteration drained the buffer to `Incomplete`, so the
                // unread bytes here are exactly the batch just read from the terminal.
                let raw = events.unread_bytes();
                if !raw.is_empty() {
                    write!(terminal, "Raw: {}\n\r", escape_bytes(raw))?;
                }
            }
            extui::event::Polled::Woken => {
                // Occurs on resize
                write!(terminal, "Woken by waker\n\r")?;
            }
            extui::event::Polled::TimedOut => {
                write!(terminal, "Timed out waiting for events\n\r")?;
            }
        }
        while let Some(event) = events.next(terminal.is_raw()) {
            write!(terminal, "Event: {:?}\n\r", event)?;
            if matches!(
                event,
                Event::Key(KeyEvent {
                    code: KeyCode::Char('c'),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                })
            ) {
                return Ok(());
            }
        }
    }
}

/// Renders raw input bytes as a printable line: a C-style escaped string
/// followed by the raw hex, so escape sequences are unambiguous.
fn escape_bytes(bytes: &[u8]) -> String {
    let mut escaped = String::new();
    for &byte in bytes {
        match byte {
            0x1B => escaped.push_str("\\e"),
            b'\n' => escaped.push_str("\\n"),
            b'\r' => escaped.push_str("\\r"),
            b'\t' => escaped.push_str("\\t"),
            b'\\' => escaped.push_str("\\\\"),
            0x20..=0x7E => escaped.push(byte as char),
            _ => escaped.push_str(&format!("\\x{byte:02x}")),
        }
    }
    let mut hex = String::new();
    for &byte in bytes {
        if !hex.is_empty() {
            hex.push(' ');
        }
        hex.push_str(&format!("{byte:02x}"));
    }
    format!("\"{escaped}\"  [{hex}]")
}
