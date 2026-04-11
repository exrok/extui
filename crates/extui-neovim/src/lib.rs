//! Embed a headless Neovim instance inside an [`extui::DoubleBuffer`] region.
//!
//! [`NeovimEmbed`] spawns `nvim --embed`, attaches a UI of the given size,
//! and mirrors Neovim's `ext_linegrid` redraws into an internal grid that
//! can be painted into any [`Rect`] of a [`DoubleBuffer`]. Input events
//! from the host's main loop can be translated and forwarded back to
//! Neovim with [`NeovimEmbed::send_key`] and [`NeovimEmbed::send_mouse`].
//!
//! The crate does **not** depend on an async runtime. A single background
//! thread reads Neovim's stdout, parses MessagePack-RPC frames, and wakes
//! `extui`'s global waker each time a `flush` event is observed.
//!
//! # Requirements
//!
//! Before calling [`NeovimEmbed::spawn`], the host must have initialised
//! `extui`'s global waker (for example via
//! [`extui::event::polling::initialize_global_waker`] or
//! [`extui::event::polling::resize_waker`]). Otherwise the reader thread
//! has no way to interrupt the main `event::poll` loop.

use std::ffi::OsStr;
use std::io::{self, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::{Arc, Mutex};
use std::thread::JoinHandle;

use extui::event::polling::{self, Waker};
use extui::event::{KeyEvent, MouseEvent};
use extui::{DoubleBuffer, Rect};

pub mod grid;
pub mod input;
pub mod msgpack;
pub mod reader;

pub use extui::CursorShape;

use crate::grid::GridState;
use crate::msgpack::{self as mp, Writer};

const UI_ATTACH_MSGID: u32 = 0;

/// An embedded Neovim instance that renders into a sub-rect of an extui
/// [`DoubleBuffer`].
pub struct NeovimEmbed {
    child: Option<Child>,
    stdin: Option<ChildStdin>,
    state: Arc<Mutex<GridState>>,
    reader_handle: Option<JoinHandle<()>>,
    waker: &'static Waker,
    last_sent_size: (u16, u16),
    scratch_key: String,
    scratch_writer: Writer,
}

impl NeovimEmbed {
    /// Spawns `nvim --embed` and attaches a UI of `(width, height)` cells.
    ///
    /// # Errors
    ///
    /// Returns an error if the global waker is not initialised, if the
    /// `nvim` binary cannot be spawned, or if the initial `nvim_ui_attach`
    /// request cannot be written to the child's stdin.
    pub fn spawn(width: u16, height: u16) -> io::Result<Self> {
        let mut cmd = Command::new("nvim");
        cmd.arg("--embed");
        Self::spawn_with(cmd, width, height)
    }

    /// Spawns a caller-supplied command that is expected to behave like
    /// `nvim --embed` (for example to specify a custom binary path or
    /// extra arguments).
    ///
    /// The command's stdin and stdout will be overridden with
    /// [`Stdio::piped`]. Its stderr is inherited.
    ///
    /// # Errors
    ///
    /// Returns an error if the global waker is not initialised, if `cmd`
    /// cannot be spawned, or if the initial `nvim_ui_attach` request
    /// cannot be written to the child's stdin.
    pub fn spawn_with(mut cmd: Command, width: u16, height: u16) -> io::Result<Self> {
        let Some(waker) = polling::global_waker() else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "extui global waker is not initialised; call \
                 extui::event::polling::initialize_global_waker first",
            ));
        };

        cmd.stdin(Stdio::piped());
        cmd.stdout(Stdio::piped());
        let mut child = cmd.spawn()?;
        let Some(mut stdin) = child.stdin.take() else {
            return Err(io::Error::other("failed to capture nvim stdin"));
        };
        let Some(stdout) = child.stdout.take() else {
            return Err(io::Error::other("failed to capture nvim stdout"));
        };

        // Match Neovim's own TUI attach payload closely enough that the server
        // can compute cterm defaults and highlights against the correct
        // terminal model.
        let term_name = command_term_name(&cmd);
        let term_colors = infer_term_colors(term_name.as_deref());
        let mut writer = Writer::with_capacity(96);
        mp::encode_ui_attach(
            &mut writer,
            UI_ATTACH_MSGID,
            width,
            height,
            term_name.as_deref(),
            term_colors,
        );
        stdin.write_all(writer.bytes())?;
        stdin.flush()?;

        let state = Arc::new(Mutex::new(GridState::new(width, height)));
        let reader_state = Arc::clone(&state);
        let reader_handle = std::thread::Builder::new()
            .name("extui-neovim-reader".into())
            .spawn(move || reader::run(stdout, reader_state, waker))?;

        Ok(NeovimEmbed {
            child: Some(child),
            stdin: Some(stdin),
            state,
            reader_handle: Some(reader_handle),
            waker,
            last_sent_size: (width, height),
            scratch_key: String::new(),
            scratch_writer: Writer::with_capacity(128),
        })
    }

    /// Sends a `nvim_ui_try_resize` notification if the size has changed
    /// since the last call.
    ///
    /// # Errors
    ///
    /// Returns an error if writing to the child's stdin fails.
    pub fn resize(&mut self, width: u16, height: u16) -> io::Result<()> {
        if self.last_sent_size == (width, height) {
            return Ok(());
        }
        self.scratch_writer.clear();
        mp::encode_ui_try_resize(&mut self.scratch_writer, width, height);
        self.write_frame()?;
        self.last_sent_size = (width, height);
        Ok(())
    }

    /// Translates `key` and sends it as a `nvim_input` notification.
    ///
    /// Returns `Ok(false)` if the key code has no Neovim translation
    /// (modifier-only keys, media keys, etc.).
    ///
    /// # Errors
    ///
    /// Returns an error if writing to the child's stdin fails.
    pub fn send_key(&mut self, key: &KeyEvent) -> io::Result<bool> {
        self.scratch_key.clear();
        if !input::write_key(&mut self.scratch_key, key) {
            return Ok(false);
        }
        self.scratch_writer.clear();
        mp::encode_input(&mut self.scratch_writer, &self.scratch_key);
        self.write_frame()?;
        Ok(true)
    }

    /// Sends a raw key-notation string via `nvim_input`.
    ///
    /// # Errors
    ///
    /// Returns an error if writing to the child's stdin fails.
    pub fn send_input(&mut self, keys: &str) -> io::Result<()> {
        self.scratch_writer.clear();
        mp::encode_input(&mut self.scratch_writer, keys);
        self.write_frame()
    }

    /// Translates `mouse` against the embedding rect and sends it as a
    /// `nvim_input_mouse` notification.
    ///
    /// Returns `Ok(false)` if the event falls outside `rect` or has no
    /// Neovim equivalent (for example bare [`MouseEventKind::Moved`]).
    ///
    /// # Errors
    ///
    /// Returns an error if writing to the child's stdin fails.
    ///
    /// [`MouseEventKind::Moved`]: extui::event::MouseEventKind::Moved
    pub fn send_mouse(&mut self, mouse: &MouseEvent, rect: Rect) -> io::Result<bool> {
        let Some(t) = input::translate_mouse(mouse, rect.x, rect.y, rect.w, rect.h) else {
            return Ok(false);
        };
        self.scratch_writer.clear();
        mp::encode_input_mouse(
            &mut self.scratch_writer,
            t.button,
            t.action,
            &t.modifier,
            t.row,
            t.col,
        );
        self.write_frame()?;
        Ok(true)
    }

    /// Consumes the dirty flag and returns `true` if the internal grid
    /// has been updated since the last call.
    ///
    /// Call this after each return from [`extui::event::poll`] to decide
    /// whether to re-render.
    pub fn poll_updates(&mut self) -> bool {
        let Ok(mut guard) = self.state.lock() else {
            return false;
        };
        guard.take_dirty()
    }

    /// Paints the embedded grid into `rect` of `buf` and drives the
    /// terminal cursor to match Neovim's state.
    ///
    /// Cells are clipped to `rect`'s dimensions if the embedded grid is
    /// larger, and the remaining area is left untouched if smaller. The
    /// call also forwards Neovim's cursor position, shape, and busy
    /// state into [`DoubleBuffer::set_cursor`] / [`DoubleBuffer::hide_cursor`],
    /// so the host does not need to emit cursor escapes by hand.
    pub fn render(&self, rect: Rect, buf: &mut DoubleBuffer) {
        let Ok(guard) = self.state.lock() else {
            return;
        };
        guard.render(rect, buf);

        if guard.is_busy() {
            buf.hide_cursor();
            return;
        }
        let row = guard.cursor_row();
        let col = guard.cursor_col();
        if row >= rect.h || col >= rect.w {
            buf.hide_cursor();
            return;
        }
        buf.set_cursor(rect.x + col, rect.y + row, guard.cursor_shape());
    }

    /// Returns the cursor position translated into `buf` coordinates, or
    /// `None` if Neovim is currently busy or the cursor lies outside the
    /// visible portion of `rect`.
    pub fn cursor_position(&self, rect: Rect) -> Option<(u16, u16)> {
        let guard = self.state.lock().ok()?;
        if guard.is_busy() {
            return None;
        }
        let row = guard.cursor_row();
        let col = guard.cursor_col();
        if row >= rect.h || col >= rect.w {
            return None;
        }
        Some((rect.x + col, rect.y + row))
    }

    /// Returns the cursor shape advertised by Neovim for the active mode.
    pub fn cursor_shape(&self) -> CursorShape {
        self.state
            .lock()
            .map(|g| g.cursor_shape())
            .unwrap_or_default()
    }

    /// Returns `true` if the embedded Neovim process is still running.
    ///
    /// This checks both the shared `alive` flag (set to `false` by the
    /// reader thread on stdout EOF) and the child's exit status.
    pub fn is_alive(&mut self) -> bool {
        let Some(child) = self.child.as_mut() else {
            return false;
        };
        if matches!(child.try_wait(), Ok(Some(_))) {
            return false;
        }
        self.state.lock().map(|g| g.alive()).unwrap_or(false)
    }

    fn write_frame(&mut self) -> io::Result<()> {
        let Some(stdin) = self.stdin.as_mut() else {
            return Err(io::Error::other("nvim stdin is closed"));
        };
        stdin.write_all(self.scratch_writer.bytes())?;
        stdin.flush()
    }
}

fn command_term_name(cmd: &Command) -> Option<String> {
    for (name, value) in cmd.get_envs() {
        if name == OsStr::new("TERM") {
            return value
                .map(|value| value.to_string_lossy().into_owned())
                .filter(|value| !value.is_empty());
        }
    }
    std::env::var("TERM").ok().filter(|value| !value.is_empty())
}

fn infer_term_colors(term_name: Option<&str>) -> u16 {
    let Some(term_name) = term_name else {
        return 256;
    };
    if term_name.contains("256color") {
        256
    } else if term_name.contains("88color") {
        88
    } else if term_name.contains("16color") {
        16
    } else if term_name == "ansi" || term_name.ends_with("-mono") {
        8
    } else {
        256
    }
}

impl Drop for NeovimEmbed {
    fn drop(&mut self) {
        drop(self.stdin.take());
        if let Some(mut child) = self.child.take() {
            let _ = child.kill();
            let _ = child.wait();
        }
        if let Some(handle) = self.reader_handle.take() {
            let _ = self.waker.wake();
            let _ = handle.join();
        }
    }
}
