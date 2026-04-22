//! Spellcheck squiggles layered on top of the editor.
//!
//! Run with:
//!
//! ```text
//! cargo run -p extui-editor --example spelling -- /path/to/en.utf-8.spl
//! ```
//!
//! `.spl` files are Vim's spell dictionary format — fetch one from
//! `ftp.nluug.nl/pub/vim/runtime/spell/` or copy from an existing Vim
//! install. Press `Ctrl+Q` to exit.
//!
//! Demonstrates the incremental-overlay extension points:
//!
//! - `set_track_replacements` / `take_tracked_change` — each edit
//!   yields a merged replacement describing the affected window, so
//!   the squiggle list can drop/shift surviving ranges and rescan
//!   only the touched words instead of the whole buffer.
//! - `Replacement::project` — shift surviving misspellings from
//!   pre-edit into post-edit coords in a single step. Composed with
//!   a `Span::overlaps(rescan)` check to drop anything in the
//!   whitespace-expanded rescan region.
//! - `for_each_split_chunk` — iterate `&str` chunks over the rescan
//!   window, splitting at whitespace so each chunk contains whole
//!   words. Only the word straddling the internal gap (if any) is
//!   allocated; everything else is a borrowed slice.
//! - `visible_range_rects` — map misspelled byte ranges to screen
//!   rects, then overlay a red undercurl style via
//!   `DoubleBuffer::set_style`.

use std::io;
use std::path::PathBuf;
use std::time::Duration;

use extui::event::polling::{GlobalWakerConfig, initialize_global_waker};
use extui::event::{self, Event, Events, KeyCode, KeyModifiers};
use extui::{AnsiColor, DoubleBuffer, HAlign, Rect, Style, Terminal, TerminalFlags};
use extui_editor::{Editor, Mode, Replacement, Span, TrackedChange, bindings};

#[path = "common/themes.rs"]
mod palette;
use palette::{BuiltInPalette, SyntaxHighlighter};

const MIN_H: u16 = 6;
const MAX_H: u16 = 20;

const SQUIGGLE_PALETTE: u8 = 0;

const SAMPLE: &str = "\
# Spellcheck demo

The `vimspell` crate flags missspelled words as byte ranges. We overlay
red undercurl on the rendered cells after `Editor::render`.

Notice this paragrph contains some delibrate typpos the checker should
catch. Fix them and the squigles disapear on the next frame.

Code spans like `usze` are treated as prose by this simple example; a
richer integration would skip them via the syntax token stream.
";

fn main() -> io::Result<()> {
    let Some(spl_path) = std::env::args().nth(1).map(PathBuf::from) else {
        eprintln!(
            "usage: spelling <path-to-vim.spl>\n\n\
             Vim dictionaries typically live at:\n  \
             /usr/share/vim/vim*/spell/en.utf-8.spl\n  \
             ~/.vim/spell/en.utf-8.spl\n"
        );
        std::process::exit(1);
    };

    let dict_bytes = std::fs::read(&spl_path)?;
    let dict = match vimspell::Dictionary::parse(&dict_bytes) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("failed to parse {}: {e:?}", spl_path.display());
            std::process::exit(1);
        }
    };

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

    // Red undercurl. `\x1b[4:3m` = curly underline; `\x1b[58;5;196m` = red
    // underline color. Both are kitty/xterm extensions; terminals that
    // don't support them fall back to a plain underline.
    double.set_palette(SQUIGGLE_PALETTE, b"\x1b[4:3m\x1b[58;5;196m".to_vec());
    let squiggle = Style::palette(SQUIGGLE_PALETTE);

    let mut events = Events::default();
    let stdin = std::io::stdin();

    let mut ed = Editor::with_bindings(bindings::vim(bindings::VimOptions::default()));
    ed.set_height_bounds(MIN_H, MAX_H);
    ed.resize(w);
    let theme_palette = BuiltInPalette::TokyoNight;
    theme_palette.apply_to(&mut ed);
    ed.set_lines(SAMPLE);
    // Turn tracking on once; the first drain reports `Reset` so the
    // initial full scan runs through the same path as a wholesale
    // `set_lines`/`clear` mid-run would.
    ed.set_track_replacements(true);

    let mut hl = SyntaxHighlighter::new(tinyhl::Language::Markdown);
    hl.bind(&mut ed);
    let syntax = theme_palette.syntax();

    let mut misspelled: Vec<Span> = Vec::new();

    loop {
        // `take_tracked_change` is `&mut self`; drain before the
        // later `&self` calls (`text_len`, `for_each_split_chunk`).
        let change = ed.take_tracked_change();
        hl.apply_change(&mut ed, change);

        match change {
            TrackedChange::None => {}
            TrackedChange::Reset => {
                misspelled.clear();
                let full = Span::new(0, ed.text_len());
                scan_window(&mut misspelled, &ed, full, &dict);
            }
            TrackedChange::Merged(r) => {
                apply_delta(&mut misspelled, &ed, r, &dict);
            }
        }

        let mut screen = Rect {
            x: 0,
            y: 0,
            w: double.width(),
            h: double.height(),
        };

        let header = screen.take_top(1);
        let header_style = AnsiColor::Blue1.as_bg() | AnsiColor::White.as_fg();
        header.with(header_style).fill(&mut double);
        header
            .with(header_style)
            .with(HAlign::Left)
            .text(&mut double, " -- extui-editor spelling demo -- ");
        let mode_label = match ed.mode() {
            Mode::Normal => "NORMAL",
            Mode::Insert => "INSERT",
            Mode::Visual => "VISUAL",
            Mode::VisualLine => "VISUAL LINE",
            Mode::VisualBlock => "VISUAL BLOCK",
        };
        header.with(header_style).with(HAlign::Right).text(
            &mut double,
            &format!("[{mode_label}]  {} typos  Ctrl+Q: quit ", misspelled.len()),
        );

        let widget_h = ed.desired_height().min(screen.h.saturating_sub(2));
        let widget_rect = screen.take_top(widget_h as i32);
        widget_rect
            .with(AnsiColor::Grey[2].as_bg())
            .fill(&mut double);
        let text_style = ed.theme().text;
        hl.render(&mut ed, widget_rect, &mut double, &syntax, text_style);

        for m in &misspelled {
            for rect in ed.visible_range_rects(widget_rect, m.offset, m.end()) {
                double.set_style(rect, squiggle);
            }
        }

        let footer = screen.take_bottom(1);
        let footer_style = AnsiColor::Grey[8].as_bg() | AnsiColor::White.as_fg();
        footer.with(footer_style).fill(&mut double);
        footer.with(footer_style).with(HAlign::Left).text(
            &mut double,
            " i=insert  Esc=normal  u=undo  Ctrl-R=redo  (squiggles track edits) ",
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

// -------------------- incremental misspelling refresh --------------------

/// Rescan the post-edit whitespace-expanded window around `r`, then
/// drop / shift existing misspellings using the effective rescan
/// span (returned by `for_each_split_chunk`) and splice the fresh
/// ranges in sorted order.
///
/// Unlike the padded search case, the "drop zone" here is the
/// *rescan* window (whitespace-expanded), not just the edit region —
/// rescanning repopulates whatever lies inside it. We project every
/// surviving misspelling to post-edit coordinates and discard any
/// that fall inside the rescan span.
fn apply_delta(
    misspelled: &mut Vec<Span>,
    ed: &Editor,
    r: Replacement,
    dict: &vimspell::Dictionary,
) {
    // Rescan first. `for_each_split_chunk` expands the core edit
    // region outward to whitespace (or buffer edges) internally, and
    // returns the effective window so we can key the drop/splice
    // logic on it without recomputing.
    let core = Span::new(r.offset, r.new_len);
    let mut fresh = Vec::new();
    let rescan = scan_window(&mut fresh, ed, core, dict);

    misspelled.retain_mut(|m| match r.project(*m) {
        None => false, // overlaps the edit itself
        Some(shifted) => {
            if shifted.overlaps(&rescan) {
                false // inside the rescan window — repopulated above
            } else {
                *m = shifted;
                true
            }
        }
    });

    let insert_at = misspelled.partition_point(|m| m.end() <= rescan.offset);
    misspelled.splice(insert_at..insert_at, fresh);
}

/// Full-buffer scan — used on the initial prime and after any
/// `Reset` drain. Uses [`Editor::for_each_split_chunk`] with a
/// whitespace predicate so each chunk handed to `spell_check`
/// contains whole words — the straddling word (if any) is the only
/// allocation.
fn scan_window(out: &mut Vec<Span>, ed: &Editor, core: Span, dict: &vimspell::Dictionary) -> Span {
    ed.for_each_split_chunk(core, char::is_whitespace, |chunk, base| {
        for r in dict.spell_check(chunk) {
            out.push(Span::new(base + r.start as u32, (r.end - r.start) as u32));
        }
    })
}
