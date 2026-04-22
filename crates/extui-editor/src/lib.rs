//! Embeddable text editor widget for [`extui`].
//!
//! [`Editor`] is a self-contained state machine. The host owns the
//! [`Terminal`] and [`DoubleBuffer`], forwards key events to
//! [`Editor::send_key`], and paints the editor into a [`Rect`] via
//! [`Editor::render`]. The widget reports its preferred height through
//! [`Editor::desired_height`] so hosts can size it inline.
//!
//! [`Editor::new`] installs the built-in Vim preset from [`bindings::vim`].
//! Use [`Editor::with_bindings`] to plug in a custom [`bindings::EditorBindings`]
//! graph; see the [`bindings`] module for the preset builders ([`bindings::vim`],
//! [`bindings::nano`], [`bindings::emacs`]).
//!
//! # Examples
//!
//! Compose the editor into an event loop:
//!
//! ```no_run
//! use extui::{DoubleBuffer, Rect};
//! use extui_editor::Editor;
//!
//! let mut editor = Editor::new();
//! editor.set_height_bounds(2, 10);
//! editor.resize(80);
//! editor.set_lines("hello\nworld");
//!
//! let mut buf = DoubleBuffer::new(80, 24);
//! let rect = Rect { x: 0, y: 0, w: 80, h: editor.desired_height() };
//! editor.render(rect, &mut buf);
//! ```
//!
//! In a full application the host forwards terminal key events to
//! [`Editor::send_key`] each frame; see
//! `examples/search.rs` for a complete event loop.
//!
//! [`extui`]: https://docs.rs/extui
//! [`Terminal`]: extui::Terminal
//! [`DoubleBuffer`]: extui::DoubleBuffer
//! [`Rect`]: extui::Rect

pub mod bindings;
mod buffer;
mod cursor;
mod editor;
mod history;
mod mode;
mod render;
mod theme;
mod visual;

pub use buffer::{Replacement, Span, TextBuffer, TrackedChange};
pub use editor::{Editor, TabSettings};
pub use mode::Mode;
pub use render::StyleRun;
pub use theme::{EditorTheme, SelectionTheme};
