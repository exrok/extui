//! Interactive embed test: fullscreen nvim inside extui.
//!
//! Run with `cargo run -p extui-neovim --example embed [file]`. Requires
//! `nvim` on the `$PATH`. Press `Ctrl+Q` in the host terminal to exit.

use std::io::Write;
use std::process::Command;

use extui::event::polling::GlobalWakerConfig;
use extui::event::{self, Event, Events, KeyCode, KeyModifiers};
use extui::vt::MoveCursor;
use extui::{DoubleBuffer, Rect, Terminal, TerminalFlags};
use extui_neovim::NeovimEmbed;

fn main() -> std::io::Result<()> {
    extui::event::polling::initialize_global_waker(GlobalWakerConfig {
        resize: true,
        termination: true,
    })?;

    let mut term = Terminal::open(TerminalFlags::RAW_MODE | TerminalFlags::ALT_SCREEN)?;

    let (w, h) = term.size()?;
    let mut buf = DoubleBuffer::new(w, h);
    let mut events = Events::default();
    let stdin = std::io::stdin();

    let mut cmd = Command::new("nvim");
    cmd.arg("--embed");
    for arg in std::env::args().skip(1) {
        cmd.arg(arg);
    }
    let mut nvim = NeovimEmbed::spawn_with(cmd, w, h)?;

    loop {
        if !nvim.is_alive() {
            break;
        }

        let rect = Rect {
            x: 0,
            y: 0,
            w: buf.width(),
            h: buf.height(),
        };
        nvim.render(rect, &mut buf);
        buf.render_internal();
        term.write_all(&buf.buf)?;
        buf.buf.clear();

        if let Some((cx, cy)) = nvim.cursor_position(rect) {
            let mut cur = Vec::with_capacity(16);
            use extui::vt::BufferWrite;
            MoveCursor(cx, cy).write_to_buffer(&mut cur);
            cur.extend_from_slice(b"\x1b[?25h");
            term.write_all(&cur)?;
        } else {
            term.write_all(b"\x1b[?25l")?;
        }

        if event::poll(&stdin, None)?.is_readable() {
            events.read_from(&stdin)?;
        }
        while let Some(ev) = events.next(term.is_raw()) {
            match ev {
                Event::Key(key) => {
                    if key.code == KeyCode::Char('q')
                        && key.modifiers.contains(KeyModifiers::CONTROL)
                    {
                        return Ok(());
                    }
                    let _ = nvim.send_key(&key);
                }
                Event::Mouse(m) => {
                    let rect = Rect {
                        x: 0,
                        y: 0,
                        w: buf.width(),
                        h: buf.height(),
                    };
                    let _ = nvim.send_mouse(&m, rect);
                }
                Event::Resized => {
                    let (new_w, new_h) = term.size()?;
                    buf.resize(new_w, new_h);
                    let _ = nvim.resize(new_w, new_h);
                }
                _ => {}
            }
        }

        nvim.poll_updates();
    }

    Ok(())
}
