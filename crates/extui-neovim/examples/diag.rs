//! Offline diagnostic: dump every cell of the statusline row with its
//! resolved foreground / background / modifier state.

use std::thread::sleep;
use std::time::Duration;

use extui::event::polling::{GlobalWakerConfig, initialize_global_waker};
use extui::{Buffer, Rect};
use extui_neovim::NeovimEmbed;

fn main() -> std::io::Result<()> {
    initialize_global_waker(GlobalWakerConfig::default())?;

    let nvim = NeovimEmbed::spawn(60, 10)?;
    sleep(Duration::from_millis(1000));

    let mut buf = Buffer::new(60, 10);
    let rect = Rect {
        x: 0,
        y: 0,
        w: 60,
        h: 10,
    };
    nvim.render(rect, &mut buf);

    let current = buf.current();
    for y in [0u16, 8, 9] {
        println!("== row y={y} ==");
        for x in 0..60u16 {
            if let Some(cell) = current.get_mut(x, y) {
                let dbg = format!("{cell:?}");
                println!("  x={x:2} {dbg}");
            }
        }
    }
    Ok(())
}
