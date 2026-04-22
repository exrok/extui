//! External syntax highlighting demo for `extui_editor::Editor`.
//!
//! Run with `cargo run -p extui-editor --example highlight`. Press
//! `Ctrl+T` to cycle through the built-in palettes and `Ctrl+Q` to exit.
//!
//! `extui-editor` itself has no syntax highlighting. This example
//! drives a [`tinyhl::Highlighter`] and pushes its output into the
//! editor via [`extui_editor::Editor::render_with_styles`]. The
//! `common/themes.rs` helper module holds the palette definitions and
//! a small [`SyntaxHighlighter`] wrapper that syncs edits into tinyhl
//! and materializes a reusable `Vec<StyleRun>` per frame.

use std::io;
use std::time::Duration;

use extui::event::polling::{GlobalWakerConfig, initialize_global_waker};
use extui::event::{self, Event, Events, KeyCode, KeyModifiers};
use extui::{Color, DoubleBuffer, HAlign, Rect, Style, Terminal, TerminalFlags};

use extui_editor::{Editor, Mode, bindings};

#[path = "common/themes.rs"]
mod themes;
use themes::{BuiltInPalette, SyntaxHighlighter};

const MIN_H: u16 = 2;
const MAX_H: u16 = 100;

const SAMPLE: &str = r##"// Lexical + semantic + rainbow delimiter demo.
//
// Press Ctrl+T to cycle palettes. Type to edit; the TokenTable,
// SemanticTable, and DelimiterTable all stay in sync incrementally
// via their respective `mutate` entry points.

use std::collections::HashMap;

/// Observations grouped by bucket. Comments use the `doc_comment`
/// syntax color; regular `// ...` uses `comment`.
pub struct Observations<'a> {
    name: &'a str,
    buckets: HashMap<String, Vec<u64>>,
}

impl<'a> Observations<'a> {
    pub fn new(name: &'a str) -> Self {
        Self {
            name,
            buckets: HashMap::new(),
        }
    }

    pub fn record(&mut self, key: impl Into<String>, value: u64) {
        self.buckets.entry(key.into()).or_default().push(value);
    }

    pub fn summarize(&self) -> Vec<(String, f64)> {
        self.buckets
            .iter()
            .map(|(k, xs)| {
                let total: u64 = xs.iter().copied().sum();
                (k.clone(), total as f64 / xs.len().max(1) as f64)
            })
            .collect()
    }
}

fn main() {
    let mut obs = Observations::new("demo");
    for (key, value) in [("alpha", 10), ("beta", 20), ("alpha", 30)] {
        obs.record(key, value);
    }
    for (bucket, avg) in obs.summarize() {
        println!("{bucket}: {avg:.2}");
    }
}
"##;

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

    let mut events = Events::default();
    let stdin = std::io::stdin();

    let mut ed = Editor::with_bindings(bindings::vim(bindings::VimOptions {
        use_lower_s_for_visual_surround: true,
    }));
    ed.set_height_bounds(MIN_H, MAX_H);
    ed.resize(w);
    ed.set_lines(SAMPLE);

    let mut active_palette = BuiltInPalette::TokyoNight;
    active_palette.apply_to(&mut ed);

    let mut hl = SyntaxHighlighter::new(tinyhl::Language::Rust);
    hl.bind(&mut ed);

    loop {
        hl.sync(&mut ed);

        let mut screen = Rect {
            x: 0,
            y: 0,
            w: double.width(),
            h: double.height(),
        };
        let chrome = chrome_for(active_palette);
        screen.with(chrome.background).fill(&mut double);

        let header = screen.take_top(1);
        header.with(chrome.header).fill(&mut double);
        header.with(chrome.header).with(HAlign::Left).text(
            &mut double,
            &format!(
                " -- extui-editor highlight --   [{}]   [{}]",
                active_palette.name(),
                mode_label(ed.mode()),
            ),
        );
        header
            .with(chrome.header)
            .with(HAlign::Right)
            .text(&mut double, "Ctrl+T: theme  Ctrl+Q: quit ");

        let widget_h = ed.desired_height().min(screen.h.saturating_sub(2));
        let widget_rect = screen.take_top(widget_h as i32);
        widget_rect.with(chrome.editor_bg).fill(&mut double);
        let syntax = active_palette.syntax();
        let text_style = ed.theme().text;
        hl.render(&mut ed, widget_rect, &mut double, &syntax, text_style);

        let footer = screen.take_bottom(1);
        footer.with(chrome.footer).fill(&mut double);
        footer.with(chrome.footer).with(HAlign::Left).text(
            &mut double,
            " i=insert  v/V/Ctrl-V=visual  dd/yy/cc/d{motion}  u=undo  Ctrl-R=redo ",
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
                        active_palette = active_palette.next();
                        active_palette.apply_to(&mut ed);
                        continue;
                    }
                    ed.send_key(&key);
                }
                Event::Resized => {
                    let (w, h) = term.size()?;
                    double.resize(w, h);
                    ed.resize(w);
                }
                _ => {}
            }
        }
    }
}

#[derive(Clone, Copy)]
struct Chrome {
    background: Style,
    editor_bg: Style,
    header: Style,
    footer: Style,
}

fn chrome_for(palette: BuiltInPalette) -> Chrome {
    match palette {
        BuiltInPalette::Default => Chrome {
            background: Style::DEFAULT.with_bg(Color::rgb(18, 18, 18)),
            editor_bg: Style::DEFAULT.with_bg(Color::rgb(24, 24, 24)),
            header: Style::DEFAULT
                .with_bg(Color::rgb(45, 45, 45))
                .with_fg(Color::rgb(240, 240, 240)),
            footer: Style::DEFAULT
                .with_bg(Color::rgb(36, 36, 36))
                .with_fg(Color::rgb(220, 220, 220)),
        },
        BuiltInPalette::TokyoNight => Chrome {
            background: Style::DEFAULT.with_bg(Color::rgb(20, 21, 31)),
            editor_bg: Style::DEFAULT.with_bg(Color::rgb(26, 27, 38)),
            header: Style::DEFAULT
                .with_bg(Color::rgb(41, 46, 66))
                .with_fg(Color::rgb(192, 202, 245)),
            footer: Style::DEFAULT
                .with_bg(Color::rgb(31, 35, 53))
                .with_fg(Color::rgb(169, 177, 214)),
        },
        BuiltInPalette::GruvboxDark => Chrome {
            background: Style::DEFAULT.with_bg(Color::rgb(34, 33, 31)),
            editor_bg: Style::DEFAULT.with_bg(Color::rgb(40, 40, 40)),
            header: Style::DEFAULT
                .with_bg(Color::rgb(80, 73, 69))
                .with_fg(Color::rgb(251, 241, 199)),
            footer: Style::DEFAULT
                .with_bg(Color::rgb(60, 56, 54))
                .with_fg(Color::rgb(235, 219, 178)),
        },
        BuiltInPalette::SolarizedDark => Chrome {
            background: Style::DEFAULT.with_bg(Color::rgb(0, 36, 46)),
            editor_bg: Style::DEFAULT.with_bg(Color::rgb(0, 43, 54)),
            header: Style::DEFAULT
                .with_bg(Color::rgb(7, 54, 66))
                .with_fg(Color::rgb(147, 161, 161)),
            footer: Style::DEFAULT
                .with_bg(Color::rgb(0, 57, 70))
                .with_fg(Color::rgb(131, 148, 150)),
        },
        BuiltInPalette::Nord => Chrome {
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
