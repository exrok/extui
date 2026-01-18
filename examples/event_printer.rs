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
    loop {
        match extui::event::poll(&stdin, Some(Duration::from_secs(1)))? {
            extui::event::Polled::ReadReady => {
                events.read_from(&stdin)?;
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
