//! Embed a headless Neovim instance inside an [`extui::DoubleBuffer`] region.
//!
//! [`NeovimEmbed`] spawns `nvim --embed`, attaches a UI of the given size,
//! and mirrors Neovim's `ext_linegrid` redraws into an internal grid that
//! can be painted into any [`Rect`] of a [`DoubleBuffer`]. Input events
//! from the host's main loop can be translated and forwarded back to
//! Neovim with [`NeovimEmbed::send_key`] and [`NeovimEmbed::send_mouse`].
//!
//! The crate does **not** depend on an async runtime. A single background
//! thread reads Neovim's stdout, parses MessagePack-RPC frames, and
//! notifies the host each time a `flush` event is observed.
//!
//! # Waking
//!
//! The [`NeovimWaker`] enum controls how the background reader thread
//! signals the host. The default ([`NeovimWaker::Global`]) wakes extui's
//! global waker and requires
//! [`extui::event::polling::initialize_global_waker`] to have been called
//! first. [`NeovimWaker::custom`] accepts an arbitrary closure, making it
//! possible to drive the embed from tokio, a channel, or any other
//! notification mechanism without depending on extui's polling
//! infrastructure.
//!
//! Embedded UIs are attached as pure RPC clients, not as terminal UIs.
//! That means Neovim's builtin TUI startup probes for things like
//! `termguicolors` auto-detection do not run automatically. If the host has
//! already decided whether RGB should be enabled, call
//! [`NeovimEmbed::set_termguicolors`] explicitly.
//!
//! A typical setup keeps the host [`DoubleBuffer`] and Neovim in sync:
//!
//! ```ignore
//! use extui::{DoubleBuffer, rgb_supported_from_env};
//! use extui_neovim::NeovimEmbed;
//!
//! let mut buf = DoubleBuffer::new(width, height);
//! let rgb = rgb_supported_from_env();
//! buf.set_rgb_supported(rgb);
//!
//! let mut nvim = NeovimEmbed::spawn(width, height)?;
//! if rgb {
//!     nvim.set_termguicolors(true)?;
//! }
//! # Ok::<(), std::io::Error>(())
//! ```

use std::collections::{HashMap, VecDeque};
use std::ffi::OsStr;
use std::io::{self, Write};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc::{self, SyncSender};
use std::sync::{Arc, Mutex};
use std::thread::JoinHandle;
use std::time::Duration;

use extui::event::polling;
use extui::event::{KeyEvent, MouseEvent};
use extui::{DoubleBuffer, Rect};

pub mod grid;
pub mod input;
pub mod msgpack;
pub mod reader;

pub use extui::CursorShape;

use crate::grid::GridState;
pub use crate::grid::{MAX_DECORATIONS, UnderlineStyle};
use crate::msgpack::{self as mp, Writer};

const UI_ATTACH_MSGID: u32 = 0;
type PendingRequests = Arc<Mutex<HashMap<u32, SyncSender<Vec<u8>>>>>;
type Notifications = Arc<Mutex<VecDeque<String>>>;

/// Upper bound on how long [`NeovimEmbed::request`] waits for a reply.
///
/// # Hazard
///
/// `request` is a synchronous round trip that parks the calling thread, which
/// in a host TUI is the same thread that forwards keystrokes to neovim. Neovim
/// does not service non-`api-fast` requests while it sits in a *blocking* input
/// state: operator-pending, an unfinished mapping, waiting for the second half
/// of a digraph or for a register after `Ctrl-K` / `Ctrl-R`, or a modal prompt.
/// A request issued in that state is never answered until neovim receives more
/// input, but the only thread that could send that input is the one now parked
/// in the request. An unbounded wait there is therefore a hard deadlock: the
/// whole UI freezes with no way out. Time out instead so the caller can surface
/// an error and let the user unblock neovim.
const REQUEST_TIMEOUT: Duration = Duration::from_secs(2);

/// Controls how the background reader thread notifies the host that new
/// content is available.
///
/// The default variant ([`Global`](NeovimWaker::Global)) wakes the extui
/// global waker and is the right choice for any host that already uses
/// [`extui::event::poll`]. For hosts that drive their own event loop —
/// for example through tokio or a bare `libc::poll` set — use
/// [`NeovimWaker::custom`] to supply an arbitrary notification callback.
#[derive(Default, Clone)]
pub enum NeovimWaker {
    /// Wake the extui global waker.
    ///
    /// Requires [`extui::event::polling::initialize_global_waker`] (or
    /// [`extui::event::polling::resize_waker`]) to have been called
    /// before [`NeovimEmbed::spawn_with`].
    #[default]
    Global,
    /// Call a user-supplied closure.
    Custom(Arc<dyn Fn() + Send + Sync>),
}

impl NeovimWaker {
    /// Creates a [`Custom`](NeovimWaker::Custom) waker from a closure.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// use extui_neovim::NeovimWaker;
    ///
    /// // Wake a tokio Notify.
    /// let notify = std::sync::Arc::new(tokio::sync::Notify::new());
    /// let n = notify.clone();
    /// let waker = NeovimWaker::custom(move || n.notify_one());
    /// ```
    pub fn custom(f: impl Fn() + Send + Sync + 'static) -> Self {
        NeovimWaker::Custom(Arc::new(f))
    }

    fn wake(&self) {
        match self {
            NeovimWaker::Global => {
                if let Some(waker) = polling::global_waker() {
                    let _ = waker.wake();
                }
            }
            NeovimWaker::Custom(f) => f(),
        }
    }
}

/// An embedded Neovim instance that renders into a sub-rect of an extui
/// [`DoubleBuffer`].
pub struct NeovimEmbed {
    child: Option<Child>,
    stdin: Option<ChildStdin>,
    state: Arc<Mutex<GridState>>,
    pending_requests: PendingRequests,
    notifications: Notifications,
    reader_handle: Option<JoinHandle<()>>,
    waker: NeovimWaker,
    last_sent_size: (u16, u16),
    next_msgid: u32,
    scratch_key: String,
    scratch_writer: Writer,
}

impl NeovimEmbed {
    /// Spawns `nvim --embed` with the [`Global`](NeovimWaker::Global) waker
    /// and attaches a UI of `(width, height)` cells.
    ///
    /// # Errors
    ///
    /// Returns an error if the global waker is not initialised, if the
    /// `nvim` binary cannot be spawned, or if the initial `nvim_ui_attach`
    /// request cannot be written to the child's stdin.
    pub fn spawn(width: u16, height: u16) -> io::Result<Self> {
        let mut cmd = Command::new("nvim");
        cmd.arg("--embed");
        Self::spawn_with(cmd, width, height, NeovimWaker::default())
    }

    /// Spawns a caller-supplied command that is expected to behave like
    /// `nvim --embed` (for example to specify a custom binary path or
    /// extra arguments).
    ///
    /// The command's stdin and stdout will be overridden with
    /// [`Stdio::piped`]. Its stderr is inherited.
    ///
    /// The `waker` controls how the background reader thread notifies the
    /// host when new content arrives. See [`NeovimWaker`] for options.
    ///
    /// # Errors
    ///
    /// Returns an error if `waker` is [`NeovimWaker::Global`] and the
    /// global waker is not initialised, if `cmd` cannot be spawned, or if
    /// the initial `nvim_ui_attach` request cannot be written to the
    /// child's stdin.
    pub fn spawn_with(
        mut cmd: Command,
        width: u16,
        height: u16,
        waker: NeovimWaker,
    ) -> io::Result<Self> {
        if matches!(waker, NeovimWaker::Global) && polling::global_waker().is_none() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "extui global waker is not initialised; call \
                 extui::event::polling::initialize_global_waker first",
            ));
        }

        cmd.stdin(Stdio::piped());
        cmd.stdout(Stdio::piped());
        let mut child = cmd.spawn()?;
        let Some(mut stdin) = child.stdin.take() else {
            return Err(io::Error::other("failed to capture nvim stdin"));
        };
        let Some(stdout) = child.stdout.take() else {
            return Err(io::Error::other("failed to capture nvim stdout"));
        };

        // Advertise a pure RPC UI. We intentionally do not set the TTY flags
        // Neovim's builtin ui_client uses, because this embed does not forward
        // `ui_send` terminal queries back to a real terminal.
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
        let pending_requests = Arc::new(Mutex::new(HashMap::new()));
        let notifications: Notifications = Arc::new(Mutex::new(VecDeque::new()));
        let reader_state = Arc::clone(&state);
        let reader_pending_requests = Arc::clone(&pending_requests);
        let reader_notifications = Arc::clone(&notifications);
        let reader_waker = waker.clone();
        let reader_handle = std::thread::Builder::new()
            .name("extui-neovim-reader".into())
            .spawn(move || {
                reader::run(
                    stdout,
                    reader_state,
                    reader_pending_requests,
                    reader_notifications,
                    reader_waker,
                )
            })?;

        Ok(NeovimEmbed {
            child: Some(child),
            stdin: Some(stdin),
            state,
            pending_requests,
            notifications,
            reader_handle: Some(reader_handle),
            waker,
            last_sent_size: (width, height),
            next_msgid: 1,
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

    /// Executes an Ex command through `nvim_command`.
    ///
    /// # Errors
    ///
    /// Returns an error if writing to the child's stdin fails.
    pub fn command(&mut self, command: &str) -> io::Result<()> {
        self.scratch_writer.clear();
        mp::encode_command(&mut self.scratch_writer, command);
        self.write_frame()
    }

    /// Returns the current buffer contents joined by `\n`.
    ///
    /// This is a synchronous RPC round trip on the calling thread. It returns an
    /// [`io::ErrorKind::TimedOut`] error if neovim does not reply within
    /// [`REQUEST_TIMEOUT`], which happens when neovim is parked in a blocking
    /// input state (see [`REQUEST_TIMEOUT`] for the full hazard). A caller on a
    /// UI thread must handle that error rather than treat the read as infallible.
    ///
    /// # Errors
    ///
    /// Returns an error if the RPC request fails or times out, Neovim replies
    /// with an error, or the response payload is malformed.
    pub fn current_buffer_text(&mut self) -> io::Result<String> {
        let response = self.request(|writer, msgid| {
            mp::encode_current_buffer_get_lines_request(writer, msgid);
        })?;
        parse_current_buffer_text_response(&response)
    }

    /// Replaces the current buffer with a single blank line and returns to
    /// insert mode.
    ///
    /// # Errors
    ///
    /// Returns an error if writing to the child's stdin fails.
    pub fn clear_current_buffer(&mut self) -> io::Result<()> {
        self.scratch_writer.clear();
        mp::encode_current_buffer_set_lines(&mut self.scratch_writer, &[""]);
        self.write_frame()?;
        self.command("normal! gg0")?;
        self.command("startinsert")
    }

    /// Replaces the current buffer's contents with `lines`.
    ///
    /// Sends a single `nvim_buf_set_lines(0, 0, -1, true, lines)` notification.
    /// Unlike [`clear_current_buffer`](Self::clear_current_buffer) this does
    /// not move the cursor or toggle insert mode — callers are expected to
    /// follow up with [`command`](Self::command) as needed.
    ///
    /// # Errors
    ///
    /// Returns an error if writing to the child's stdin fails.
    pub fn set_current_buffer_lines(&mut self, lines: &[&str]) -> io::Result<()> {
        self.scratch_writer.clear();
        mp::encode_current_buffer_set_lines(&mut self.scratch_writer, lines);
        self.write_frame()
    }

    /// Drains and returns every `rpcnotify` method name Neovim has sent
    /// since the last call.
    ///
    /// The background reader thread captures any non-`"redraw"` notification
    /// frame and queues the method name. Call this from the host main loop
    /// alongside [`poll_updates`](Self::poll_updates) to react to
    /// `rpcnotify(1, 'name', ...)` calls issued from Lua or Vimscript.
    ///
    /// The notification payload (arguments) is not currently exposed.
    pub fn poll_notifications(&mut self) -> Vec<String> {
        let Ok(mut queue) = self.notifications.lock() else {
            return Vec::new();
        };
        queue.drain(..).collect()
    }

    /// Enables colored underlines, squiggles, and other Neovim
    /// decorations.
    ///
    /// Reserves the palette slot range
    /// `palette_offset..palette_offset + MAX_DECORATIONS` on the host
    /// [`DoubleBuffer`] for the embed's exclusive use. Each distinct
    /// combination of underline variant and color that Neovim asks for
    /// is assigned one slot from that range, and styled underlines
    /// render in the terminal with their intended color.
    ///
    /// Up to [`MAX_DECORATIONS`] distinct decorations are supported at
    /// once. Any further combinations fall back to a plain underline.
    ///
    /// The reserved slots must not be written to from outside the
    /// embed, otherwise the underline bytes will be overwritten.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// use extui::DoubleBuffer;
    /// use extui_neovim::NeovimEmbed;
    ///
    /// let mut buf = DoubleBuffer::new(80, 24);
    /// let mut nvim = NeovimEmbed::spawn(80, 24)?;
    /// nvim.enable_decorations(0);
    /// # Ok::<(), std::io::Error>(())
    /// ```
    pub fn enable_decorations(&mut self, palette_offset: u8) {
        if let Ok(mut guard) = self.state.lock() {
            guard.enable_decorations(palette_offset);
        }
    }

    /// Sets Neovim's `termguicolors` option through RPC.
    ///
    /// Use this after you decide RGB support in the host.
    ///
    /// Keep this aligned with [`DoubleBuffer::set_rgb_supported`]. If the host
    /// buffer is quantizing colors then leave `termguicolors` off. If the host
    /// buffer supports RGB then enable both.
    pub fn set_termguicolors(&mut self, enabled: bool) -> io::Result<()> {
        self.scratch_writer.clear();
        mp::encode_set_option_value_bool(&mut self.scratch_writer, "termguicolors", enabled);
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
        let Ok(mut guard) = self.state.lock() else {
            return;
        };
        guard.sync_palettes(buf);
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

    /// Returns `true` once Neovim has painted at least one non-whitespace
    /// character into the grid.
    ///
    /// Host TUIs that want to defer allocating screen space for the embed
    /// until it has something real to show should gate their layout on
    /// this, rather than on [`poll_updates`](Self::poll_updates): Neovim
    /// emits several redraw batches (default colors, highlight
    /// definitions, a cleared grid, a blank statusline) before the first
    /// real content lands, and reacting to the raw dirty flag flashes
    /// empty space.
    pub fn has_content(&self) -> bool {
        self.state.lock().map(|g| g.has_content()).unwrap_or(false)
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

    /// Sends an RPC request and blocks the calling thread until neovim replies.
    ///
    /// The wait is bounded by [`REQUEST_TIMEOUT`]. See that constant for why an
    /// unbounded wait here would deadlock a host UI thread rather than just run
    /// slow.
    fn request(&mut self, encode: impl FnOnce(&mut Writer, u32)) -> io::Result<Vec<u8>> {
        let msgid = self.next_request_id();
        let (tx, rx) = mpsc::sync_channel(1);
        self.pending_requests
            .lock()
            .map_err(|_| io::Error::other("neovim request table is poisoned"))?
            .insert(msgid, tx);

        self.scratch_writer.clear();
        encode(&mut self.scratch_writer, msgid);
        if let Err(err) = self.write_frame() {
            self.remove_pending(msgid);
            return Err(err);
        }

        match rx.recv_timeout(REQUEST_TIMEOUT) {
            Ok(response) => Ok(response),
            Err(mpsc::RecvTimeoutError::Timeout) => {
                // Neovim is wedged in a blocking input state and will not reply.
                // Drop the pending slot so a late reply is discarded rather than
                // matched against a future request id.
                self.remove_pending(msgid);
                Err(io::Error::new(io::ErrorKind::TimedOut, "neovim request timed out"))
            }
            // The reader thread cleared the table on stdout EOF: neovim is gone.
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                Err(io::Error::other("neovim request was interrupted"))
            }
        }
    }

    fn remove_pending(&self, msgid: u32) {
        if let Ok(mut pending) = self.pending_requests.lock() {
            pending.remove(&msgid);
        }
    }

    fn next_request_id(&mut self) -> u32 {
        let msgid = self.next_msgid;
        self.next_msgid = self.next_msgid.wrapping_add(1);
        if self.next_msgid == UI_ATTACH_MSGID {
            self.next_msgid = 1;
        }
        msgid
    }
}

fn parse_current_buffer_text_response(bytes: &[u8]) -> io::Result<String> {
    let mut response = parse_success_response(bytes)?;
    match response.peek_kind().map_err(msgpack_io_error)? {
        mp::Kind::Nil => {
            response.read_nil().map_err(msgpack_io_error)?;
            Ok(String::new())
        }
        mp::Kind::Array => {
            let len = response.read_array_len().map_err(msgpack_io_error)?;
            let mut out = String::new();
            for idx in 0..len {
                if idx > 0 {
                    out.push('\n');
                }
                out.push_str(response.read_str().map_err(msgpack_io_error)?);
            }
            Ok(out)
        }
        _ => Err(io::Error::other("unexpected nvim response payload")),
    }
}

fn parse_success_response(bytes: &[u8]) -> io::Result<mp::Reader<'_>> {
    let mut r = mp::Reader::new(bytes);
    let arr_len = r.read_array_len().map_err(msgpack_io_error)?;
    if arr_len < 4 {
        return Err(io::Error::other("invalid nvim response frame"));
    }
    if r.read_u64().map_err(msgpack_io_error)? != 1 {
        return Err(io::Error::other("unexpected non-response frame"));
    }
    let _msgid = r.read_u64().map_err(msgpack_io_error)?;
    match r.peek_kind().map_err(msgpack_io_error)? {
        mp::Kind::Nil => {
            r.read_nil().map_err(msgpack_io_error)?;
            Ok(r)
        }
        _ => Err(io::Error::other(read_rpc_error_message(&mut r)?)),
    }
}

fn read_rpc_error_message(r: &mut mp::Reader<'_>) -> io::Result<String> {
    match r.peek_kind().map_err(msgpack_io_error)? {
        mp::Kind::Str => Ok(r.read_str().map_err(msgpack_io_error)?.to_owned()),
        mp::Kind::Array => {
            let len = r.read_array_len().map_err(msgpack_io_error)?;
            let mut message = None;
            for _ in 0..len {
                match r.peek_kind().map_err(msgpack_io_error)? {
                    mp::Kind::Str => {
                        message = Some(r.read_str().map_err(msgpack_io_error)?.to_owned())
                    }
                    _ => r.skip().map_err(msgpack_io_error)?,
                }
            }
            Ok(message.unwrap_or_else(|| "neovim rpc error".to_string()))
        }
        _ => {
            r.skip().map_err(msgpack_io_error)?;
            Ok("neovim rpc error".to_string())
        }
    }
}

fn msgpack_io_error(err: mp::Error) -> io::Error {
    io::Error::other(err)
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
        if let Ok(mut pending) = self.pending_requests.lock() {
            pending.clear();
        }
        if let Some(mut child) = self.child.take() {
            let _ = child.kill();
            let _ = child.wait();
        }
        if let Some(handle) = self.reader_handle.take() {
            self.waker.wake();
            let _ = handle.join();
        }
    }
}
