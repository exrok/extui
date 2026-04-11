//! Interactive embed test: fullscreen nvim inside extui.
//!
//! Run with `cargo run -p extui-neovim --example embed [file]`. Requires
//! `nvim` on the `$PATH`. Press `Ctrl+Q` in the host terminal to exit.

use std::process::Command;

use extui::event::polling::GlobalWakerConfig;
use extui::event::{self, Event, Events, KeyCode, KeyModifiers};
use extui::{DoubleBuffer, Rect, Terminal, TerminalFlags};
use extui_neovim::NeovimEmbed;

fn main() -> std::io::Result<()> {
    extui::event::polling::initialize_global_waker(GlobalWakerConfig {
        resize: true,
        termination: true,
    })?;

    let mut term = Terminal::open(
        TerminalFlags::RAW_MODE | TerminalFlags::ALT_SCREEN | TerminalFlags::HIDE_CURSOR,
    )?;

    let (w, h) = term.size()?;
    let mut buf = DoubleBuffer::new(w, h);
    buf.set_rgb_supported(true);
    let mut events = Events::default();
    let stdin = std::io::stdin();

    let mut cmd = Command::new("nvim");
    cmd.arg("--embed");
    for arg in std::env::args().skip(1) {
        cmd.arg(arg);
    }
    let mut nvim = NeovimEmbed::spawn_with(cmd, w, h)?;
    if extui::rgb_supported_from_env() {
        nvim.set_termguicolors(true)?;
    }
    nvim.enable_decorations(0);

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
        buf.render(&mut term);

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
