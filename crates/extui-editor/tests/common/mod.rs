//! Shared nvim oracle for differential tests.
//!
//! A single embedded `nvim --clean` is held behind a global mutex. Every
//! test acquires the mutex, resets the oracle to a known state, runs an
//! identical keystroke sequence through both our `Editor` and the
//! oracle, and compares the resulting buffer text and cursor position.
//!
//! Reusing one nvim (rather than spawning per test) keeps per-case cost
//! down to a handful of RPC round-trips.

use std::io;
use std::process::Command;
use std::sync::{Mutex, MutexGuard, OnceLock};

use extui::Rect;
use extui::event::{KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers};
use extui_editor::Editor;
use extui_neovim::{NeovimEmbed, NeovimWaker};

const GRID_W: u16 = 80;
const GRID_H: u16 = 24;

pub struct Oracle {
    nvim: NeovimEmbed,
}

static ORACLE: OnceLock<Mutex<Oracle>> = OnceLock::new();

/// Returns an exclusive handle on the shared nvim oracle. Tests run in
/// parallel across threads but serialize on this mutex.
pub fn oracle() -> MutexGuard<'static, Oracle> {
    ORACLE
        .get_or_init(|| Mutex::new(Oracle::spawn().expect("spawn nvim oracle")))
        .lock()
        .unwrap_or_else(|p| p.into_inner())
}

impl Oracle {
    fn spawn() -> io::Result<Self> {
        let mut cmd = Command::new("nvim");
        cmd.args(["--clean", "--embed"]);
        // Use a Custom no-op waker so we don't depend on the global
        // extui waker (tests don't run under a Terminal).
        let waker = NeovimWaker::custom(|| {});
        let mut nvim = NeovimEmbed::spawn_with(cmd, GRID_W, GRID_H, waker)?;
        // Keep cursor grid_row mapped 1:1 to buffer row.
        nvim.command("set nowrap")?;
        nvim.command("set noswapfile")?;
        nvim.command("set nohlsearch")?;
        // Keep insert-mode tab behavior aligned with extui-editor defaults.
        nvim.command(
            "set noautoindent nosmartindent nocindent expandtab tabstop=4 shiftwidth=4 softtabstop=4",
        )?;
        // Drain any initial UI-attach redraws.
        let _ = nvim.current_buffer_text()?;
        Ok(Self { nvim })
    }

    /// Resets nvim to a clean buffer containing `initial`, with cursor
    /// at (row 0, col 0). Uses `<Esc>` twice to bail out of any mode.
    pub fn reset(&mut self, initial: &str) -> io::Result<()> {
        self.nvim.send_input("<Esc><Esc>")?;
        // nvim_buf_set_lines wants a `&[&str]` — split the initial text.
        let lines: Vec<&str> = if initial.is_empty() {
            vec![""]
        } else {
            initial.split('\n').collect()
        };
        self.nvim.set_current_buffer_lines(&lines)?;
        // Send `gg0` to put the cursor at the very start.
        self.nvim.command("normal! gg0")?;
        // Clear the yank register so tests start from the same state.
        self.nvim.command("let @\" = ''")?;
        let _ = self.nvim.current_buffer_text()?; // sync barrier
        Ok(())
    }

    /// Sends `keys` to nvim (in nvim notation) and returns the resulting
    /// buffer text and cursor position. Blocks on an RPC round-trip, so
    /// all redraws are guaranteed to have been processed.
    pub fn run(&mut self, keys: &str) -> io::Result<Snapshot> {
        self.nvim.send_input(keys)?;
        let text = self.nvim.current_buffer_text()?;
        let rect = Rect {
            x: 0,
            y: 0,
            w: GRID_W,
            h: GRID_H,
        };
        let cursor = self.nvim.cursor_position(rect);
        Ok(Snapshot { text, cursor })
    }
}

#[derive(Debug, Clone)]
pub struct Snapshot {
    pub text: String,
    /// Screen position (column, row) as reported by nvim's grid. `None`
    /// if nvim is busy or the cursor fell outside the rect.
    pub cursor: Option<(u16, u16)>,
}

// -------------------------------------------------------------------------
// Key-notation parser (nvim style: <Esc>, <CR>, <C-r>, <lt>, etc.)
// -------------------------------------------------------------------------

/// Parse a nvim keystroke notation string into a sequence of extui
/// [`KeyEvent`]s. Supports the common tags used in our tests:
/// `<Esc>`, `<CR>`/`<Enter>`, `<BS>`, `<Tab>`, `<Space>`, `<lt>`, `<gt>`,
/// and modifier-prefixed forms like `<C-r>`, `<C-v>`, `<S-Tab>`.
pub fn parse_keys(s: &str) -> Vec<KeyEvent> {
    let mut out = Vec::new();
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'<' {
            if let Some(close) = find_close_angle(s, i) {
                let tag = &s[i + 1..close];
                if let Some(ev) = parse_tag(tag) {
                    out.push(ev);
                    i = close + 1;
                    continue;
                }
            }
        }
        let ch = s[i..].chars().next().unwrap();
        out.push(plain_char_event(ch));
        i += ch.len_utf8();
    }
    out
}

fn find_close_angle(s: &str, open: usize) -> Option<usize> {
    let bytes = s.as_bytes();
    let mut j = open + 1;
    while j < bytes.len() {
        if bytes[j] == b'>' {
            return Some(j);
        }
        j += 1;
    }
    None
}

fn parse_tag(tag: &str) -> Option<KeyEvent> {
    let mut mods = KeyModifiers::NONE;
    let mut rest = tag;
    loop {
        if let Some(t) = rest.strip_prefix("C-") {
            mods |= KeyModifiers::CONTROL;
            rest = t;
        } else if let Some(t) = rest.strip_prefix("c-") {
            mods |= KeyModifiers::CONTROL;
            rest = t;
        } else if let Some(t) = rest.strip_prefix("S-") {
            mods |= KeyModifiers::SHIFT;
            rest = t;
        } else if let Some(t) = rest.strip_prefix("s-") {
            mods |= KeyModifiers::SHIFT;
            rest = t;
        } else if let Some(t) = rest.strip_prefix("M-") {
            mods |= KeyModifiers::ALT;
            rest = t;
        } else if let Some(t) = rest.strip_prefix("m-") {
            mods |= KeyModifiers::ALT;
            rest = t;
        } else {
            break;
        }
    }
    let code = match rest {
        "Esc" | "esc" | "ESC" => KeyCode::Esc,
        "CR" | "cr" | "Enter" | "enter" | "Return" | "return" => KeyCode::Enter,
        "BS" | "bs" => KeyCode::Backspace,
        "Tab" | "tab" => KeyCode::Tab,
        "lt" | "LT" => KeyCode::Char('<'),
        "gt" | "GT" => KeyCode::Char('>'),
        "Bar" | "bar" => KeyCode::Char('|'),
        "Bslash" | "bslash" => KeyCode::Char('\\'),
        "Space" | "space" => KeyCode::Char(' '),
        s if s.chars().count() == 1 => KeyCode::Char(s.chars().next().unwrap()),
        _ => return None,
    };
    Some(KeyEvent {
        code,
        modifiers: mods,
        kind: KeyEventKind::Press,
        state: KeyEventState::empty(),
    })
}

fn plain_char_event(ch: char) -> KeyEvent {
    let mods = if ch.is_ascii_uppercase() {
        KeyModifiers::SHIFT
    } else {
        KeyModifiers::NONE
    };
    KeyEvent {
        code: KeyCode::Char(ch),
        modifiers: mods,
        kind: KeyEventKind::Press,
        state: KeyEventState::empty(),
    }
}

// -------------------------------------------------------------------------
// Differential assertion
// -------------------------------------------------------------------------

/// Run `keys` against both our [`Editor`] seeded with `initial` and the
/// shared nvim oracle. Panics if buffer text or cursor position diverge.
#[track_caller]
pub fn assert_matches_nvim(initial: &str, keys: &str) {
    let mut oracle = oracle();
    oracle.reset(initial).expect("oracle reset");
    let expected = oracle.run(keys).expect("oracle run");

    let mut ed = Editor::new();
    ed.resize(GRID_W);
    ed.set_lines(initial);
    for key in parse_keys(keys) {
        ed.send_key(&key);
    }

    assert_eq!(
        ed.text(),
        expected.text,
        "text mismatch for initial={initial:?} keys={keys:?}\nours  = {:?}\nnvim  = {:?}",
        ed.text(),
        expected.text
    );

    let ours = ed.cursor_display();
    if let Some((nvim_x, nvim_y)) = expected.cursor {
        assert_eq!(
            (ours.0 as u16, ours.1),
            (nvim_y, nvim_x),
            "cursor mismatch for initial={initial:?} keys={keys:?}\nours  = (row={}, dcol={})\nnvim  = (row={}, dcol={})",
            ours.0,
            ours.1,
            nvim_y,
            nvim_x,
        );
    }
}
