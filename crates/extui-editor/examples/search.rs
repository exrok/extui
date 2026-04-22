//! Vim-style `/` search built on top of `extui_editor::Editor`.
//!
//! Run with `cargo run -p extui-editor --example search`. Press
//! `Ctrl+Q` to exit.
//!
//! Demonstrates the incremental-overlay extension points:
//!
//! - `send_key(..) -> bool` — returns `false` for unmatched keys,
//!   letting the host bind `/`, `n`, `N` in Normal mode.
//! - `set_cursor_offset` / `cursor_offset` — programmatic cursor
//!   navigation for jumping to match starts.
//! - `visible_range_rects` — maps buffer byte ranges to screen rects,
//!   used here to paint match highlights after `render()` via
//!   `DoubleBuffer::set_style`.
//! - `set_track_replacements` / `take_tracked_change` — drive a
//!   drop-shift-rescan match list so each edit only rescans the
//!   window it affected.
//! - `Replacement::project` — shift surviving matches from pre-edit
//!   to post-edit coordinates in one line.
//! - `for_each_padded_chunk` — iterate `&str` chunks over a rescan
//!   window, bridging the gap with a `2 * (query.len() - 1)` byte
//!   allocation only when needed. No full-buffer materialization.
//!
//! Matching is a plain case-sensitive substring scan to keep the demo
//! small. A real layer would plug in regex here — the extension API
//! doesn't care.

use std::io;
use std::time::Duration;

use extui::event::polling::{GlobalWakerConfig, initialize_global_waker};
use extui::event::{self, Event, Events, KeyCode, KeyModifiers};
use extui::{AnsiColor, CursorShape, DoubleBuffer, HAlign, Rect, Style, Terminal, TerminalFlags};
use extui_editor::{Editor, Mode, Replacement, Span, TrackedChange, bindings};

#[path = "common/themes.rs"]
mod palette;
use palette::{BuiltInPalette, SyntaxHighlighter};

const MIN_H: u16 = 4;
const MAX_H: u16 = 20;

const MATCH_PALETTE: u8 = 0;
const CURRENT_PALETTE: u8 = 1;

const SAMPLE: &str = "\
# Search demo

Press / to open a search prompt, type a query, Enter to commit.
Use n and N in Normal mode to jump between matches.
Press Esc while typing to cancel without moving the cursor.

This example shows how `extui-editor` leaves search as a layer on
top. The editor exposes primitives like:

    set_cursor_offset       — jump to a match start
    visible_range_rects     — overlay match highlights post-render
    take_tracked_change     — drive incremental match-list refresh
    for_each_padded_chunk   — scan without materializing the buffer
    Replacement::project    — shift surviving matches past the edit

Try searching for 'match', 'the', or 'extui'.
";

enum UiMode {
    Editing,
    Searching,
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

    double.set_palette(MATCH_PALETTE, b"\x1b[30;43m".to_vec());
    double.set_palette(CURRENT_PALETTE, b"\x1b[30;103m".to_vec());
    let match_style = Style::palette(MATCH_PALETTE);
    let current_style = Style::palette(CURRENT_PALETTE);

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

    let mut ui = UiMode::Editing;
    let mut query = String::new();
    let mut last_query = String::new();
    let mut matches: Vec<Span> = Vec::new();
    let mut current: usize = 0;

    loop {
        // `take_tracked_change` is `&mut self`; drain first so later
        // `&self` calls (`text_len`, `for_each_padded_chunk`) can
        // borrow immutably.
        let change = ed.take_tracked_change();
        hl.apply_change(&mut ed, change);
        let query_changed = last_query != query;

        // A query change or a buffer reset invalidates the match list
        // wholesale. For incremental edits we drop/shift and rescan
        // only the affected window.
        if query_changed || matches!(change, TrackedChange::Reset) {
            matches.clear();
            let full = Span::new(0, ed.text_len());
            find_matches_in(&mut matches, &ed, full, &query);
            last_query = query.clone();
            if current >= matches.len() {
                current = 0;
            }
        } else if let TrackedChange::Merged(r) = change {
            apply_delta(&mut matches, &ed, r, &query);
            if current >= matches.len() {
                current = 0;
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
            .text(&mut double, " -- extui-editor search demo -- ");
        let mode_label = match ed.mode() {
            Mode::Normal => "NORMAL",
            Mode::Insert => "INSERT",
            Mode::Visual => "VISUAL",
            Mode::VisualLine => "VISUAL LINE",
            Mode::VisualBlock => "VISUAL BLOCK",
        };
        header
            .with(header_style)
            .with(HAlign::Right)
            .text(&mut double, &format!("[{mode_label}]  Ctrl+Q: quit "));

        let widget_h = ed.desired_height().min(screen.h.saturating_sub(2));
        let widget_rect = screen.take_top(widget_h as i32);
        widget_rect
            .with(AnsiColor::Grey[2].as_bg())
            .fill(&mut double);
        let text_style = ed.theme().text;
        hl.render(&mut ed, widget_rect, &mut double, &syntax, text_style);

        for (i, m) in matches.iter().enumerate() {
            let style = if i == current {
                current_style
            } else {
                match_style
            };
            for rect in ed.visible_range_rects(widget_rect, m.offset, m.end()) {
                double.set_style(rect, style);
            }
        }

        let footer = screen.take_bottom(1);
        let footer_style = AnsiColor::Grey[8].as_bg() | AnsiColor::White.as_fg();
        footer.with(footer_style).fill(&mut double);
        match ui {
            UiMode::Searching => {
                let prompt = format!(" /{query}");
                footer
                    .with(footer_style)
                    .with(HAlign::Left)
                    .text(&mut double, &prompt);
                let cursor_x = footer.x + prompt.chars().count() as u16;
                if cursor_x < footer.x + footer.w {
                    double.set_cursor(cursor_x, footer.y, CursorShape::SteadyBar);
                }
            }
            UiMode::Editing => {
                let msg = if query.is_empty() {
                    String::from(" / to search   n/N next/prev   u/Ctrl-R undo/redo ")
                } else if matches.is_empty() {
                    format!(" /{query}  [no matches] ")
                } else {
                    format!(" /{query}  [{}/{}] ", current + 1, matches.len())
                };
                footer
                    .with(footer_style)
                    .with(HAlign::Left)
                    .text(&mut double, &msg);
            }
        }

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
                    match ui {
                        UiMode::Editing => {
                            if !ed.send_key(&key) {
                                match (ed.mode(), key.code) {
                                    (Mode::Normal, KeyCode::Char('/')) => {
                                        ui = UiMode::Searching;
                                        query.clear();
                                        matches.clear();
                                        current = 0;
                                    }
                                    (Mode::Normal, KeyCode::Char('n')) => {
                                        jump(&mut ed, &matches, &mut current, 1);
                                    }
                                    (Mode::Normal, KeyCode::Char('N')) => {
                                        jump(&mut ed, &matches, &mut current, -1);
                                    }
                                    _ => {}
                                }
                            }
                        }
                        UiMode::Searching => match key.code {
                            KeyCode::Enter => {
                                if !matches.is_empty() {
                                    current = pick_nearest_forward(&matches, ed.cursor_offset());
                                    ed.set_cursor_offset(matches[current].offset);
                                }
                                ui = UiMode::Editing;
                            }
                            KeyCode::Esc => {
                                query.clear();
                                matches.clear();
                                ui = UiMode::Editing;
                            }
                            KeyCode::Backspace => {
                                query.pop();
                            }
                            KeyCode::Char(c) => {
                                query.push(c);
                            }
                            _ => {}
                        },
                    }
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

fn pick_nearest_forward(matches: &[Span], from: u32) -> usize {
    matches.iter().position(|m| m.offset >= from).unwrap_or(0)
}

fn jump(ed: &mut Editor, matches: &[Span], current: &mut usize, delta: i32) {
    if matches.is_empty() {
        return;
    }
    let n = matches.len() as i32;
    let next = ((*current as i32) + delta).rem_euclid(n) as usize;
    *current = next;
    ed.set_cursor_offset(matches[next].offset);
}

// -------------------- incremental match refresh --------------------

/// Drop or shift existing matches for the pre-edit coordinate space of
/// `r`, then rescan the post-edit `r.new_len` byte region (padded
/// internally by `for_each_padded_chunk`) and splice the fresh
/// matches in sorted order.
///
/// `Replacement::project` is the only shift math — everything else
/// is just choosing the rescan core and partitioning the splice
/// point. The previous hand-rolled version had a drop-boundary bug
/// precisely because it re-derived shift math inline.
fn apply_delta(matches: &mut Vec<Span>, ed: &Editor, r: Replacement, query: &str) {
    // Shift surviving matches; drop ones that overlap the replacement.
    matches.retain_mut(|m| match r.project(*m) {
        Some(shifted) => {
            *m = shifted;
            true
        }
        None => false,
    });

    let insert_at = matches.partition_point(|m| m.end() <= r.offset);
    let mut fresh = Vec::new();
    // Core is the post-edit modified region; `for_each_padded_chunk`
    // pads it by `query.len() - 1` on each side internally so any
    // match crossing the edit boundary is captured contiguously.
    let core = Span::new(r.offset, r.new_len);
    find_matches_in(&mut fresh, ed, core, query);
    matches.splice(insert_at..insert_at, fresh);
}

/// Scan the post-edit buffer for occurrences of `query` touching
/// `core` (padded by `query.len() - 1`) and push them into `out` in
/// ascending order. Uses [`Editor::for_each_padded_chunk`] so the
/// full buffer is never materialized — only a small bridge is
/// allocated when the padded window straddles the internal gap.
fn find_matches_in(out: &mut Vec<Span>, ed: &Editor, core: Span, query: &str) {
    if query.is_empty() {
        return;
    }
    let q = query.len() as u32;
    ed.for_each_padded_chunk(core, q, |chunk, base, max_start| {
        let mut i = 0;
        while let Some(off) = chunk[i..].find(query) {
            let start = (i + off) as u32 + base;
            // `max_start` partitions match-start positions across
            // left-borrow / bridge / right-borrow chunks so each
            // match is reported exactly once.
            if start < max_start {
                out.push(Span::new(start, q));
            }
            i += off + query.len();
        }
    });
}
