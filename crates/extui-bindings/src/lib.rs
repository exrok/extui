//! Layered keybinding router for terminal UI applications.
//!
//! The crate splits key handling into three pieces:
//!
//! - [`InputKey`] and [`NamedKey`] — a compact, normalized key
//!   representation built from an [`extui::event::KeyEvent`] or parsed
//!   from text.
//! - [`parse_key`] and [`parse_sequence`] — vim- and Emacs-style text
//!   parsers for key tokens and whitespace-separated sequences.
//! - [`Router`] and [`RouterBuilder`] — a frozen
//!   `(layer, key) -> &[Entry]` lookup table built up binding by
//!   binding.
//!
//! The router owns no runtime state. Callers mint their own [`ActionId`]s,
//! track which [`LayerId`] is currently active, and pass both into
//! [`Router::lookup`] when a key arrives. Each bound entry carries an
//! opaque `u32` filter so callers can store multiple bindings for the
//! same `(layer, key)` and pick the one whose filter matches their
//! current context. See [`router`] for the details of layers, filters,
//! and chord sequences.
//!
//! # Examples
//!
//! ```
//! use extui::event::KeyModifiers;
//! use extui_bindings::{ActionId, InputKey, LayerId, Payload, RouterBuilder};
//!
//! const ANY: u32 = 0;
//! let move_down = ActionId(0);
//! let j = InputKey::char('j', KeyModifiers::empty());
//!
//! let mut builder = RouterBuilder::new();
//! builder.bind(LayerId::BASE, ANY, &[j], move_down);
//! let router = builder.build();
//!
//! let entry = router.lookup(LayerId::BASE, j).first().unwrap();
//! assert_eq!(entry.payload(), Payload::Action(move_down));
//! ```

pub mod key;
pub mod parse;
pub mod router;

pub use key::{InputKey, NamedKey};
pub use parse::{ParseError, parse_key, parse_sequence};
pub use router::{ActionId, Entry, LabelId, LayerId, Payload, Router, RouterBuilder};
