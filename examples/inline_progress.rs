//! Demonstrates [`DoubleBuffer::render_inline`] by painting a progress
//! region below the shell prompt that grows, holds, and shrinks across
//! frames. The shell history above the cursor stays visible throughout.
//! After the run, the final frame remains in scrollback.
//!
//! Run with `cargo run --example inline_progress` and watch the prompt
//! line stay fixed while the band of N rows updates in place.

use std::io::Write;
use std::thread::sleep;
use std::time::Duration;

use extui::{AnsiColor, DoubleBuffer, Style, Terminal, TerminalFlags};

fn main() -> std::io::Result<()> {
    let mut term = Terminal::open(TerminalFlags::RAW_MODE | TerminalFlags::HIDE_CURSOR)?;
    let (width, _) = term.size()?;
    let mut buf = DoubleBuffer::new(width, 1);

    let frames: &[(u16, &[(&str, AnsiColor)])] = &[
        (1, &[("step 1: bootstrapping…", AnsiColor::Cyan1)]),
        (
            2,
            &[
                ("step 1: ok", AnsiColor::SpringGreen),
                ("step 2: fetching…", AnsiColor::Cyan1),
            ],
        ),
        (
            3,
            &[
                ("step 1: ok", AnsiColor::SpringGreen),
                ("step 2: ok", AnsiColor::SpringGreen),
                ("step 3: building…", AnsiColor::Cyan1),
            ],
        ),
        (
            5,
            &[
                ("step 1: ok", AnsiColor::SpringGreen),
                ("step 2: ok", AnsiColor::SpringGreen),
                ("step 3: ok", AnsiColor::SpringGreen),
                ("step 4: testing…", AnsiColor::Cyan1),
                ("    log line: starting suite", AnsiColor::Grey[14]),
            ],
        ),
        (
            3,
            &[
                ("step 4: ok", AnsiColor::SpringGreen),
                ("step 5: deploying…", AnsiColor::Cyan1),
                ("    log line: connecting…", AnsiColor::Grey[14]),
            ],
        ),
        (1, &[("done.", AnsiColor::SpringGreen)]),
    ];

    let mut prev_height: u16 = 0;
    for (height, rows) in frames {
        buf.resize(width, *height);
        for (i, (text, color)) in rows.iter().enumerate() {
            buf.set_string(0, i as u16, text, Style::DEFAULT.with_fg(*color));
        }
        prev_height = buf.render_inline(&mut term, prev_height)?;
        sleep(Duration::from_millis(700));
    }

    drop(term);

    let mut stdout = std::io::stdout();
    writeln!(stdout, "summary printed normally below the live view")?;
    Ok(())
}
