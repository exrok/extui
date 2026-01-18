# extui: Exrok's Minimal Unix TUI Crate

extui provides powerful primitives and low-level access for terminal interfaces tighly
integrated together in a single crate.

[![Crates.io](https://img.shields.io/crates/v/extui?style=flat-square)](https://crates.io/crates/extui)
[![Docs.rs](https://img.shields.io/docsrs/extui?style=flat-square)](https://docs.rs/extui/latest/extui/)
[![License](https://img.shields.io/badge/license-MIT-blue?style=flat-square)](LICENSE)

extui was largly made for my own needs and is used some of my other projects such as:

- [devsm](https://github.com/exrok/devsm): A development focused service manager
- [extask](https://github.com/exrok/extask): My todo list manager

## Example

```rust
use extui::event::polling::GlobalWakerConfig;
use extui::event::{self, Event, Events, KeyCode, KeyEvent};
use extui::{Color, DoubleBuffer, Style, Terminal, TerminalFlags};

fn main() -> std::io::Result<()> {
    extui::event::polling::initialize_global_waker(GlobalWakerConfig {
        resize: true,
        ..Default::default()
    })?;

    let mut term = Terminal::open(
        TerminalFlags::RAW_MODE | TerminalFlags::ALT_SCREEN | TerminalFlags::HIDE_CURSOR,
    )?;

    let (w, h) = term.size()?;
    let mut buf = DoubleBuffer::new(w, h);
    let mut events = Events::default();
    let stdin = std::io::stdin();
    let mut last_key = None::<KeyEvent>;

    loop {
        // Render
        buf.rect().with(Color::Blue1.as_bg()).fill(&mut buf);
        let mut rect = buf.rect();
        rect.take_top(1)
            .with(Style::DEFAULT)
            .text(&mut buf, "Press 'q' to quit");

        if let Some(key) = last_key {
            rect.take_top(1)
                .with(Color::Black.with_fg(Color::White))
                .text(&mut buf, "Last Key Pressed: ")
                .fmt(&mut buf, format_args!("{key:?}"));
        }

        buf.render(&mut term);

        // Poll for events
        if event::poll(&stdin, None)?.is_readable() {
            events.read_from(&stdin)?;
        }
        while let Some(ev) = events.next(term.is_raw()) {
            match ev {
                Event::FocusGained => {}
                Event::FocusLost => {}
                Event::Key(key) => {
                    if key.code == KeyCode::Char('q') {
                        return Ok(());
                    }
                    last_key = Some(key);
                }
                Event::Mouse(_mouse_event) => {}
                Event::Resized => {
                    let (new_w, new_h) = term.size()?;
                    buf.resize(new_w, new_h);
                }
            }
        }
    }
}

```

## Deliberate Limitations

extui intentionally omits certain features. This results in simpler interfaces and better performance:

| Limitation                | Rationale                                                                                |
| ------------------------- | ---------------------------------------------------------------------------------------- |
| **8-bit color only**      | No 24-bit true color. The 256-color palette covers most use cases with simpler encoding. |
| **4-byte grapheme limit** | Characters exceeding 4 bytes are truncated. Enables fixed-size `Cell` storage.           |
| **Unix only**             | No Windows support. Allows direct POSIX APIs without abstraction overhead.               |

## Inspirations

extui draws inspiration from excellent crates in the Rust terminal ecosystem while pursuing different tradeoffs.

### ratatui

The lower-level buffer API designs are influenced by [ratatui](https://github.com/ratatui/ratatui), with the following
differences:

- New diffing algorithm: Generates VT rendering bytes directly in a greedy single-pass fashion, rather than computing diffs separately.
- Smaller diffs: Smarter about style transitions and leverages advanced VT escape sequences when possible, such as clearing whole lines to fill spaces.
- Compact Cell storage: The core `Cell` abstraction is 8 bytes by default, packed into a single `u64` for trivial diffing and cache-friendly iteration.

### crossterm

Terminal input events and parsing are inspired by [crossterm](https://github.com/crossterm-rs/crossterm). extui improves on this foundation:

- Compact KeyEvent: `KeyEvent` is only 8 bytes, fitting inside a single CPU register.
- Linear parsing: The event parser was optimized from O(n²) to O(n).
