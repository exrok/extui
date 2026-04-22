//! Theme-cycling demo for `extui_editor::Editor`.
//!
//! Run with `cargo run -p extui-editor --example themes`. Press
//! `Ctrl+T` to cycle built-in palettes and `Ctrl+Q` to exit.

use std::io;
use std::time::Duration;

use extui::event::polling::{GlobalWakerConfig, initialize_global_waker};
use extui::event::{self, Event, Events, KeyCode, KeyModifiers};
use extui::{Color, DoubleBuffer, HAlign, Rect, Style, Terminal, TerminalFlags};

use extui_editor::{Editor, Mode, bindings};

#[path = "common/themes.rs"]
mod palette;
use palette::{BuiltInPalette, SyntaxHighlighter};

const MIN_H: u16 = 4;
const MAX_H: u16 = 16;

const SAMPLE: &str = "\
# Theme demo

Press `Ctrl-T` to cycle through the built-in themes.
Selections use the active theme instead of reverse video.

```rust
fn greet(name: &str) -> String {
    let count = 3;
    format!(\"hello, {name}! x{count}\")
}
```

- Tokyo Night
- Gruvbox Dark
- Solarized Dark
- Nord
";

#[derive(Clone, Copy)]
struct DemoChrome {
    background: Style,
    editor_bg: Style,
    header: Style,
    footer: Style,
}

fn main() -> io::Result<()> {
    initialize_global_waker(GlobalWakerConfig {
        resize: true,
        termination: true,
    })?;

    let mut term = Terminal::open(
        TerminalFlags::RAW_MODE | TerminalFlags::ALT_SCREEN | TerminalFlags::HIDE_CURSOR,
    )?;

    let (w, h) = term.size()?;
    let mut double = DoubleBuffer::new(w, h);
    double.set_rgb_supported(true);

    let stdin = std::io::stdin();
    let mut events = Events::default();

    let mut editor = Editor::with_bindings(bindings::vim(bindings::VimOptions {
        use_lower_s_for_visual_surround: true,
    }));
    editor.set_height_bounds(MIN_H, MAX_H);
    editor.resize(w);
    editor.set_wrap(true);
    editor.set_lines(SAMPLE);

    let mut active_theme = BuiltInPalette::TokyoNight;
    active_theme.apply_to(&mut editor);

    let mut hl = SyntaxHighlighter::new(tinyhl::Language::Markdown);
    hl.bind(&mut editor);

    loop {
        hl.sync(&mut editor);

        let mut screen = Rect {
            x: 0,
            y: 0,
            w: double.width(),
            h: double.height(),
        };
        let chrome = chrome_for(active_theme);
        screen.with(chrome.background).fill(&mut double);

        let header = screen.take_top(1);
        header.with(chrome.header).fill(&mut double);
        header.with(chrome.header).with(HAlign::Left).text(
            &mut double,
            &format!(
                " -- extui-editor themes --   [{}]   [{}]",
                active_theme.name(),
                mode_label(editor.mode())
            ),
        );
        header
            .with(chrome.header)
            .with(HAlign::Right)
            .text(&mut double, "Ctrl+T: theme  Ctrl+Q: quit ");

        let widget_h = editor.desired_height().min(screen.h.saturating_sub(2));
        let widget = screen.take_top(widget_h as i32);
        widget.with(chrome.editor_bg).fill(&mut double);
        let syntax = active_theme.syntax();
        let text_style = editor.theme().text;
        hl.render(&mut editor, widget, &mut double, &syntax, text_style);

        let footer = screen.take_bottom(1);
        footer.with(chrome.footer).fill(&mut double);
        footer.with(chrome.footer).with(HAlign::Left).text(
            &mut double,
            " i=insert  v/V/Ctrl-V=visual  selection follows theme  u=undo  Ctrl-R=redo ",
        );

        double.render(&mut term);

        if event::poll(&stdin, Some(Duration::from_millis(100)))?.is_readable() {
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
                    if key.code == KeyCode::Char('t')
                        && key.modifiers.contains(KeyModifiers::CONTROL)
                    {
                        active_theme = active_theme.next();
                        active_theme.apply_to(&mut editor);
                        continue;
                    }
                    editor.send_key(&key);
                }
                Event::Resized => {
                    let (w, h) = term.size()?;
                    double.resize(w, h);
                    editor.resize(w);
                }
                _ => {}
            }
        }
    }
}

fn chrome_for(theme: BuiltInPalette) -> DemoChrome {
    match theme {
        BuiltInPalette::Default => DemoChrome {
            background: Style::DEFAULT.with_bg(Color::rgb(18, 18, 18)),
            editor_bg: Style::DEFAULT.with_bg(Color::rgb(24, 24, 24)),
            header: Style::DEFAULT
                .with_bg(Color::rgb(45, 45, 45))
                .with_fg(Color::rgb(240, 240, 240)),
            footer: Style::DEFAULT
                .with_bg(Color::rgb(36, 36, 36))
                .with_fg(Color::rgb(220, 220, 220)),
        },
        BuiltInPalette::TokyoNight => DemoChrome {
            background: Style::DEFAULT.with_bg(Color::rgb(20, 21, 31)),
            editor_bg: Style::DEFAULT.with_bg(Color::rgb(26, 27, 38)),
            header: Style::DEFAULT
                .with_bg(Color::rgb(41, 46, 66))
                .with_fg(Color::rgb(192, 202, 245)),
            footer: Style::DEFAULT
                .with_bg(Color::rgb(31, 35, 53))
                .with_fg(Color::rgb(169, 177, 214)),
        },
        BuiltInPalette::GruvboxDark => DemoChrome {
            background: Style::DEFAULT.with_bg(Color::rgb(34, 33, 31)),
            editor_bg: Style::DEFAULT.with_bg(Color::rgb(40, 40, 40)),
            header: Style::DEFAULT
                .with_bg(Color::rgb(80, 73, 69))
                .with_fg(Color::rgb(251, 241, 199)),
            footer: Style::DEFAULT
                .with_bg(Color::rgb(60, 56, 54))
                .with_fg(Color::rgb(235, 219, 178)),
        },
        BuiltInPalette::SolarizedDark => DemoChrome {
            background: Style::DEFAULT.with_bg(Color::rgb(0, 36, 46)),
            editor_bg: Style::DEFAULT.with_bg(Color::rgb(0, 43, 54)),
            header: Style::DEFAULT
                .with_bg(Color::rgb(7, 54, 66))
                .with_fg(Color::rgb(147, 161, 161)),
            footer: Style::DEFAULT
                .with_bg(Color::rgb(0, 57, 70))
                .with_fg(Color::rgb(131, 148, 150)),
        },
        BuiltInPalette::Nord => DemoChrome {
            background: Style::DEFAULT.with_bg(Color::rgb(36, 41, 51)),
            editor_bg: Style::DEFAULT.with_bg(Color::rgb(46, 52, 64)),
            header: Style::DEFAULT
                .with_bg(Color::rgb(67, 76, 94))
                .with_fg(Color::rgb(216, 222, 233)),
            footer: Style::DEFAULT
                .with_bg(Color::rgb(59, 66, 82))
                .with_fg(Color::rgb(229, 233, 240)),
        },
    }
}

fn mode_label(mode: Mode) -> &'static str {
    match mode {
        Mode::Normal => "NORMAL",
        Mode::Insert => "INSERT",
        Mode::Visual => "VISUAL",
        Mode::VisualLine => "VISUAL LINE",
        Mode::VisualBlock => "VISUAL BLOCK",
    }
}
