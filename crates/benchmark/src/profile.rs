//! Long-running deterministic workloads for external profilers
//! (`perf`, `samply`, etc). These intentionally skip the
//! `jsony_bench::Bencher` dance: no warm-up, no per-iter stat collection,
//! just a tight steady-state loop so a sampling profiler sees dense hits
//! on the target code path.

use extui::event::{KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers};
use extui::{Buffer, Rect};
use extui_editor::{Editor, Replacement, StyleRun, TextBuffer, TrackedChange};
use std::time::{Duration, Instant};

use crate::editor::BIG_RS;

const WIDTH: u16 = 120;
const HEIGHT: u16 = 40;
const RECT: Rect = Rect {
    x: 0,
    y: 0,
    w: WIDTH,
    h: HEIGHT,
};

struct BufferSource<'a>(&'a TextBuffer);

impl<'a> tinyhl::Source for BufferSource<'a> {
    fn len(&self) -> u32 {
        self.0.len() as u32
    }
    fn page(&self, offset: u32) -> (u32, &[u8]) {
        self.0.page(offset)
    }
}

pub struct ProfilePath {
    pub name: &'static str,
    pub run: fn(Budget),
}

#[derive(Copy, Clone)]
pub enum Budget {
    Iters(u64),
    Duration(Duration),
}

impl Budget {
    fn drive(self, mut body: impl FnMut(u64)) {
        match self {
            Budget::Iters(n) => {
                for i in 0..n {
                    body(i);
                }
            }
            Budget::Duration(d) => {
                let deadline = Instant::now() + d;
                let mut i: u64 = 0;
                while Instant::now() < deadline {
                    body(i);
                    i = i.wrapping_add(1);
                }
            }
        }
    }
}

fn key_char(c: char) -> KeyEvent {
    let modifiers = if c.is_ascii_uppercase() {
        KeyModifiers::SHIFT
    } else {
        KeyModifiers::NONE
    };
    KeyEvent {
        code: KeyCode::Char(c),
        modifiers,
        kind: KeyEventKind::Press,
        state: KeyEventState::empty(),
    }
}

fn key_ctrl(c: char) -> KeyEvent {
    KeyEvent {
        code: KeyCode::Char(c),
        modifiers: KeyModifiers::CONTROL,
        kind: KeyEventKind::Press,
        state: KeyEventState::empty(),
    }
}

struct ProfileState {
    db: Buffer,
    ed: Editor,
    hl: Option<tinyhl::Highlighter>,
    runs: Vec<StyleRun>,
}

fn make_editor(lang: Option<tinyhl::Language>, wrap: bool, text: &str) -> ProfileState {
    let db = Buffer::new(WIDTH, HEIGHT);
    let mut ed = Editor::new();
    ed.resize(WIDTH);
    ed.set_wrap(wrap);
    ed.set_lines(text);
    let hl = lang.map(|language| {
        ed.set_track_replacements(true);
        let mut hl = tinyhl::Highlighter::new(language);
        let src = BufferSource(ed.text_buffer());
        hl.rebuild(&src);
        let _ = ed.take_tracked_change();
        hl
    });
    ProfileState {
        db,
        ed,
        hl,
        runs: Vec::new(),
    }
}

fn refresh_runs(state: &mut ProfileState) {
    let Some(hl) = state.hl.as_mut() else {
        return;
    };
    match state.ed.take_tracked_change() {
        TrackedChange::None => {}
        TrackedChange::Reset => {
            let src = BufferSource(state.ed.text_buffer());
            hl.rebuild(&src);
        }
        TrackedChange::Merged(Replacement {
            offset,
            old_len,
            new_len,
        }) => {
            let src = BufferSource(state.ed.text_buffer());
            hl.apply_replacement(&src, tinyhl::Span::new(offset, old_len), new_len);
        }
    }
    state.runs.clear();
    if let Some(table) = hl.table() {
        let visible = state.ed.visible_byte_span(RECT);
        let span = tinyhl::Span::new(visible.offset, visible.len);
        let keyword_style = extui::Style::DEFAULT.with_fg(extui::Color::rgb(0xbb, 0x9a, 0xf7));
        for tok in table.query(span) {
            state.runs.push(StyleRun {
                offset: tok.span.offset,
                len: tok.span.len,
                style: keyword_style,
            });
        }
    }
}

fn paint(state: &mut ProfileState) {
    refresh_runs(state);
    state
        .ed
        .render_with_styles(RECT, &mut state.db, &state.runs);
    state.db.render_internal();
    std::hint::black_box(&state.db.buf);
    state.db.buf.clear();
}

fn render_highlighted_14k(budget: Budget) {
    let mut state = make_editor(Some(tinyhl::Language::Rust), false, BIG_RS);
    // Warm-up render so half_page_rows reflects the real viewport.
    paint(&mut state);
    budget.drive(|i| {
        let k = if i & 1 == 0 { 'd' } else { 'u' };
        state.ed.send_key(&key_ctrl(k));
        paint(&mut state);
    });
}

fn insert_burst(budget: Budget) {
    let mut state = make_editor(Some(tinyhl::Language::Rust), false, BIG_RS);
    state.ed.send_key(&key_char('i'));
    budget.drive(|_| {
        state.ed.send_key(&key_char('a'));
        state.ed.send_key(&KeyEvent {
            code: KeyCode::Backspace,
            modifiers: KeyModifiers::NONE,
            kind: KeyEventKind::Press,
            state: KeyEventState::empty(),
        });
        paint(&mut state);
    });
}

fn word_walk(budget: Budget) {
    let mut state = make_editor(Some(tinyhl::Language::Rust), false, BIG_RS);
    budget.drive(|_| {
        state.ed.send_key(&key_char('w'));
        paint(&mut state);
    });
}

fn wrap_recompute_14k(budget: Budget) {
    // Toggle wrap on each iter — forces WrapLayout to rebuild.
    let mut state = make_editor(Some(tinyhl::Language::Rust), true, BIG_RS);
    budget.drive(|i| {
        state.ed.set_wrap(i & 1 == 0);
        paint(&mut state);
    });
}

pub const PATHS: &[ProfilePath] = &[
    ProfilePath {
        name: "render_highlighted_14k",
        run: render_highlighted_14k,
    },
    ProfilePath {
        name: "insert_burst",
        run: insert_burst,
    },
    ProfilePath {
        name: "word_walk",
        run: word_walk,
    },
    ProfilePath {
        name: "wrap_recompute_14k",
        run: wrap_recompute_14k,
    },
];

pub fn find(name: &str) -> Option<&'static ProfilePath> {
    PATHS.iter().find(|p| p.name == name)
}

pub fn list() -> Vec<&'static str> {
    PATHS.iter().map(|p| p.name).collect()
}
