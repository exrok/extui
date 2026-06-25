//! Binding surface for [`Editor`].
//!
//! An editor keymap is an [`EditorRouter`]: a typed wrapper around
//! [`extui_bindings::Router`] with [`EditorAction`] packed directly
//! into the 32-bit [`ActionId`] so lookup decodes the action without
//! touching a side table.
//!
//! Three built-in presets cover the common cases:
//!
//! - [`vim`] — the default installed by [`Editor::new`].
//! - [`nano`] — modeless Insert-first preset.
//! - [`emacs`] — modeless Emacs-style preset.
//!
//! For a custom keymap, build a router with [`EditorRouterBuilder`]
//! and wrap it in [`EditorBindings`], then pass it to
//! [`Editor::with_bindings`].
//!
//! # Mode filters and layers
//!
//! Each binding carries a mode-bit [`Filter`]. A filter of
//! `MODE_INSERT | MODE_NORMAL` fires in either mode; [`Filter::ANY`]
//! matches every mode. Lookups overlap-match the active context
//! ([`EditorContext`]) against candidates.
//!
//! Operator-pending state (`d`, `c`, `y`, `gU`, `gu`, `g~`) uses
//! router layers: each operator has its own [`LayerId`]
//! ([`OP_DELETE`] … [`OP_CASE_TOGGLE`]), and the trigger key emits a
//! [`extui_bindings::Payload::Layer`] that the editor follows.
//!
//! [`Editor`]: crate::Editor
//! [`Editor::new`]: crate::Editor::new
//! [`Editor::with_bindings`]: crate::Editor::with_bindings

use std::{error::Error, fmt, str::FromStr};

use extui_bindings::{
    ActionId, Entry, InputKey, LabelId, LayerId, Router, RouterBuilder, parse_sequence,
};

use crate::Mode;

/// Mode bit for [`Mode::Normal`].
pub const MODE_NORMAL: u32 = 1 << 0;
/// Mode bit for [`Mode::Insert`].
pub const MODE_INSERT: u32 = 1 << 1;
/// Mode bit shared by [`Mode::Visual`], [`Mode::VisualLine`], and
/// [`Mode::VisualBlock`].
pub const MODE_VISUAL: u32 = 1 << 2;

/// Mode-bit filter attached to each editor binding.
///
/// OR mode bits together to enable a binding in multiple modes: a
/// filter of `MODE_INSERT | MODE_NORMAL` fires in either. Lookup is
/// overlap: any bit in common with the active [`EditorContext`]
/// makes the binding a candidate.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Filter {
    /// Bitset of mode bits that enable this binding.
    pub raw: u32,
}

impl Filter {
    /// Matches every mode. Has every bit set.
    pub const ANY: Filter = Filter { raw: u32::MAX };

    /// Wraps a mode-bit bitset as a [`Filter`].
    pub const fn new(raw: u32) -> Filter {
        Filter { raw }
    }

    /// Returns `true` when any mode bit in this filter is also set in
    /// `ctx`.
    pub const fn matches(self, ctx: u32) -> bool {
        (self.raw & ctx) != 0
    }
}

/// Context the editor feeds to [`EditorRouter::lookup`] on every key.
///
/// Only the mode bit participates in filtering. Operator-pending
/// state is carried by the router's current layer.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct EditorContext {
    /// Active mode bit: one of [`MODE_NORMAL`], [`MODE_INSERT`],
    /// [`MODE_VISUAL`].
    pub mode: u8,
}

impl EditorContext {
    /// Packs the context for comparison against a [`Filter`].
    pub const fn pack(&self) -> u32 {
        self.mode as u32
    }
}

/// Returns the mode bit for `mode`. All three visual sub-modes map to
/// [`MODE_VISUAL`].
pub const fn mode_bit(mode: Mode) -> u8 {
    match mode {
        Mode::Normal => MODE_NORMAL as u8,
        Mode::Insert => MODE_INSERT as u8,
        Mode::Visual | Mode::VisualLine | Mode::VisualBlock => MODE_VISUAL as u8,
    }
}

/// Layer for `d`-operator-pending state (`dw`, `diw`, `dd`, …).
pub const OP_DELETE: LayerId = LayerId::new(1);
/// Layer for `c`-operator-pending state.
pub const OP_CHANGE: LayerId = LayerId::new(2);
/// Layer for `y`-operator-pending state.
pub const OP_YANK: LayerId = LayerId::new(3);
/// Layer for `gU`-operator-pending state.
pub const OP_CASE_UPPER: LayerId = LayerId::new(4);
/// Layer for `gu`-operator-pending state.
pub const OP_CASE_LOWER: LayerId = LayerId::new(5);
/// Layer for `g~`-operator-pending state.
pub const OP_CASE_TOGGLE: LayerId = LayerId::new(6);

/// Operator the editor is currently waiting on a motion for.
///
/// Useful for status-bar rendering ("-- OPERATOR-PENDING --") and any
/// consumer that needs a semantic label for the active operator
/// layer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperatorState {
    Delete,
    Change,
    Yank,
    CaseUpper,
    CaseLower,
    CaseToggle,
}

/// Returns the [`OperatorState`] for an operator-pending layer, or
/// [`None`] for [`LayerId::BASE`] and any other non-operator layer.
pub fn layer_to_operator_state(layer: LayerId) -> Option<OperatorState> {
    Some(match layer {
        OP_DELETE => OperatorState::Delete,
        OP_CHANGE => OperatorState::Change,
        OP_YANK => OperatorState::Yank,
        OP_CASE_UPPER => OperatorState::CaseUpper,
        OP_CASE_LOWER => OperatorState::CaseLower,
        OP_CASE_TOGGLE => OperatorState::CaseToggle,
        _ => return None,
    })
}

/// A cursor motion.
///
/// Serves three roles: a bare motion ([`EditorAction::Motion`]), an
/// operator operand ([`EditorAction::DeleteMotion`] and friends),
/// and a visual-mode head update.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Motion {
    Left,
    Right,
    Up,
    Down,
    DisplayUp,
    DisplayDown,
    WordForward,
    WordBack,
    WordEnd,
    LineStart,
    LineEnd,
    LineFirstNonblank,
    FirstLine,
    LastLine,
    ParagraphForward,
    ParagraphBack,
    /// `<C-d>`: down by half the last rendered viewport height.
    HalfPageDown,
    /// `<C-u>`: mirror of [`Motion::HalfPageDown`].
    HalfPageUp,
    /// `f{char}` / `F{char}` / `t{char}` / `T{char}`.
    FindChar {
        forward: bool,
        till: bool,
    },
    /// `` `{char} `` (exact) or `'{char}` (linewise).
    Mark {
        linewise: bool,
    },
}

impl Motion {
    fn as_str(self) -> &'static str {
        match self {
            Motion::Left => "left",
            Motion::Right => "right",
            Motion::Up => "up",
            Motion::Down => "down",
            Motion::DisplayUp => "display-up",
            Motion::DisplayDown => "display-down",
            Motion::WordForward => "word-forward",
            Motion::WordBack => "word-back",
            Motion::WordEnd => "word-end",
            Motion::LineStart => "line-start",
            Motion::LineEnd => "line-end",
            Motion::LineFirstNonblank => "line-first-nonblank",
            Motion::FirstLine => "first-line",
            Motion::LastLine => "last-line",
            Motion::ParagraphForward => "paragraph-forward",
            Motion::ParagraphBack => "paragraph-back",
            Motion::HalfPageDown => "half-page-down",
            Motion::HalfPageUp => "half-page-up",
            Motion::FindChar {
                forward: true,
                till: false,
            } => "find-forward",
            Motion::FindChar {
                forward: false,
                till: false,
            } => "find-back",
            Motion::FindChar {
                forward: true,
                till: true,
            } => "till-forward",
            Motion::FindChar {
                forward: false,
                till: true,
            } => "till-back",
            Motion::Mark { linewise: false } => "mark",
            Motion::Mark { linewise: true } => "mark-line",
        }
    }
}

impl fmt::Display for Motion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str((*self).as_str())
    }
}

impl FromStr for Motion {
    type Err = ParseEditorBindingError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let motion = match s {
            "left" => Motion::Left,
            "right" => Motion::Right,
            "up" => Motion::Up,
            "down" => Motion::Down,
            "display-up" => Motion::DisplayUp,
            "display-down" => Motion::DisplayDown,
            "word-forward" => Motion::WordForward,
            "word-back" => Motion::WordBack,
            "word-end" => Motion::WordEnd,
            "line-start" => Motion::LineStart,
            "line-end" => Motion::LineEnd,
            "line-first-nonblank" => Motion::LineFirstNonblank,
            "first-line" => Motion::FirstLine,
            "last-line" => Motion::LastLine,
            "paragraph-forward" => Motion::ParagraphForward,
            "paragraph-back" => Motion::ParagraphBack,
            "half-page-down" => Motion::HalfPageDown,
            "half-page-up" => Motion::HalfPageUp,
            "find-forward" => Motion::FindChar {
                forward: true,
                till: false,
            },
            "find-back" => Motion::FindChar {
                forward: false,
                till: false,
            },
            "till-forward" => Motion::FindChar {
                forward: true,
                till: true,
            },
            "till-back" => Motion::FindChar {
                forward: false,
                till: true,
            },
            "mark" => Motion::Mark { linewise: false },
            "mark-line" => Motion::Mark { linewise: true },
            _ => return Err(ParseEditorBindingError::new("motion", s)),
        };
        Ok(motion)
    }
}

/// Paired delimiters for [`TextObject::Pair`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PairKind {
    Paren,
    Bracket,
    Brace,
    Angle,
}

impl PairKind {
    fn as_str(self) -> &'static str {
        match self {
            PairKind::Paren => "paren",
            PairKind::Bracket => "bracket",
            PairKind::Brace => "brace",
            PairKind::Angle => "angle",
        }
    }
}

impl fmt::Display for PairKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str((*self).as_str())
    }
}

impl FromStr for PairKind {
    type Err = ParseEditorBindingError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let kind = match s {
            "paren" => PairKind::Paren,
            "bracket" => PairKind::Bracket,
            "brace" => PairKind::Brace,
            "angle" => PairKind::Angle,
            _ => return Err(ParseEditorBindingError::new("pair kind", s)),
        };
        Ok(kind)
    }
}

/// A Vim-style text object.
///
/// Consumed by operators (`daw`, `ci"`, …) or visual-mode selection
/// updates (`vap`). The `around` flag distinguishes `a`-prefixed
/// (around, inclusive of surrounding whitespace or delimiters) from
/// `i`-prefixed (inner) object variants.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TextObject {
    /// `aw` / `iw` / `aW` / `iW`. `big` switches keyword-class
    /// grouping to whitespace-only (matches Vim's `W`).
    Word { around: bool, big: bool },
    /// `a"`, `i'`, `` a` ``, …
    Quote { around: bool, delimiter: char },
    /// `a(`, `i]`, `a{`, `i<`, …
    Pair { around: bool, kind: PairKind },
    /// `ap` / `ip`.
    Paragraph { around: bool },
}

impl fmt::Display for TextObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (around, tail) = match *self {
            TextObject::Word { around, big: false } => (around, ".word"),
            TextObject::Word { around, big: true } => (around, ".big-word"),
            TextObject::Quote { around, delimiter } => {
                let quote = quote_name(delimiter).ok_or(fmt::Error)?;
                f.write_str(scope_str(around))?;
                f.write_str(".quote.")?;
                return f.write_str(quote);
            }
            TextObject::Pair { around, kind } => {
                f.write_str(scope_str(around))?;
                f.write_str(".pair.")?;
                return f.write_str(kind.as_str());
            }
            TextObject::Paragraph { around } => (around, ".paragraph"),
        };
        f.write_str(scope_str(around))?;
        f.write_str(tail)
    }
}

fn scope_str(around: bool) -> &'static str {
    if around { "around" } else { "inner" }
}

impl FromStr for TextObject {
    type Err = ParseEditorBindingError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(rest) = s.strip_prefix("inner.") {
            return parse_text_object(false, rest);
        }
        if let Some(rest) = s.strip_prefix("around.") {
            return parse_text_object(true, rest);
        }
        Err(ParseEditorBindingError::new("text object", s))
    }
}

/// Case transformation for `gU`, `gu`, `g~` and their visual-mode
/// equivalents.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CaseTransform {
    /// Uppercase every ASCII letter in the range.
    Upper,
    /// Lowercase every ASCII letter in the range.
    Lower,
    /// Flip the case of every ASCII letter in the range.
    Toggle,
}

impl CaseTransform {
    fn as_str(self) -> &'static str {
        match self {
            CaseTransform::Upper => "upper",
            CaseTransform::Lower => "lower",
            CaseTransform::Toggle => "toggle",
        }
    }
}

impl fmt::Display for CaseTransform {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl FromStr for CaseTransform {
    type Err = ParseEditorBindingError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "upper" => CaseTransform::Upper,
            "lower" => CaseTransform::Lower,
            "toggle" => CaseTransform::Toggle,
            _ => return Err(ParseEditorBindingError::new("case transform", s)),
        })
    }
}

/// Actions the [`Editor`] can perform.
///
/// A binding binds an [`InputKey`] sequence at a given [`Filter`] and
/// [`LayerId`] to one of these variants. Actions round-trip through
/// string form via [`FromStr`] and [`fmt::Display`] for serialization
/// and debug tooling (e.g. `"motion.word-forward"`,
/// `"delete.object.inner.word"`).
///
/// [`Editor`]: crate::Editor
/// [`FromStr`]: std::str::FromStr
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EditorAction {
    EnterInsert,
    EnterInsertAfter,
    EnterInsertFirstNonblank,
    EnterInsertEol,
    OpenBelow,
    OpenAbove,

    EnterVisualChar,
    EnterVisualLine,
    EnterVisualBlock,
    ExitVisual,

    Motion(Motion),

    DeleteChar,
    Substitute,
    SubstituteLine,
    JoinLines,
    PasteAfter,
    PasteBefore,
    Undo,
    Redo,
    Replace,

    DeleteMotion(Motion),
    ChangeMotion(Motion),
    YankMotion(Motion),
    DeleteTextObject(TextObject),
    ChangeTextObject(TextObject),
    YankTextObject(TextObject),
    DeleteLine,
    ChangeLine,
    YankLine,

    ToggleCaseChar,
    CaseMotion(CaseTransform, Motion),
    CaseTextObject(CaseTransform, TextObject),
    CaseLine(CaseTransform),
    CaseSelection(CaseTransform),

    IncrementNumber,
    DecrementNumber,

    SetMark,

    ExitInsert,
    InsertChar,
    InsertNewline,
    InsertTab,
    BackspaceDelete,

    DeleteSelection,
    ChangeSelection,
    YankSelection,
    SurroundSelection,
    SelectTextObject(TextObject),

    NoOp,
}

const SIMPLE_ACTIONS: &[(&str, EditorAction)] = &[
    ("enter-insert", EditorAction::EnterInsert),
    ("enter-insert-after", EditorAction::EnterInsertAfter),
    (
        "enter-insert-first-nonblank",
        EditorAction::EnterInsertFirstNonblank,
    ),
    ("enter-insert-eol", EditorAction::EnterInsertEol),
    ("open-below", EditorAction::OpenBelow),
    ("open-above", EditorAction::OpenAbove),
    ("enter-visual-char", EditorAction::EnterVisualChar),
    ("enter-visual-line", EditorAction::EnterVisualLine),
    ("enter-visual-block", EditorAction::EnterVisualBlock),
    ("exit-visual", EditorAction::ExitVisual),
    ("delete-char", EditorAction::DeleteChar),
    ("substitute", EditorAction::Substitute),
    ("substitute-line", EditorAction::SubstituteLine),
    ("join-lines", EditorAction::JoinLines),
    ("paste-after", EditorAction::PasteAfter),
    ("paste-before", EditorAction::PasteBefore),
    ("undo", EditorAction::Undo),
    ("redo", EditorAction::Redo),
    ("replace", EditorAction::Replace),
    ("delete-line", EditorAction::DeleteLine),
    ("change-line", EditorAction::ChangeLine),
    ("yank-line", EditorAction::YankLine),
    ("toggle-case-char", EditorAction::ToggleCaseChar),
    ("increment-number", EditorAction::IncrementNumber),
    ("decrement-number", EditorAction::DecrementNumber),
    ("set-mark", EditorAction::SetMark),
    ("exit-insert", EditorAction::ExitInsert),
    ("insert-char", EditorAction::InsertChar),
    ("insert-newline", EditorAction::InsertNewline),
    ("insert-tab", EditorAction::InsertTab),
    ("backspace-delete", EditorAction::BackspaceDelete),
    ("delete-selection", EditorAction::DeleteSelection),
    ("change-selection", EditorAction::ChangeSelection),
    ("yank-selection", EditorAction::YankSelection),
    ("surround-selection", EditorAction::SurroundSelection),
    ("no-op", EditorAction::NoOp),
];

fn simple_action_name(action: EditorAction) -> Option<&'static str> {
    Some(match action {
        EditorAction::EnterInsert => "enter-insert",
        EditorAction::EnterInsertAfter => "enter-insert-after",
        EditorAction::EnterInsertFirstNonblank => "enter-insert-first-nonblank",
        EditorAction::EnterInsertEol => "enter-insert-eol",
        EditorAction::OpenBelow => "open-below",
        EditorAction::OpenAbove => "open-above",
        EditorAction::EnterVisualChar => "enter-visual-char",
        EditorAction::EnterVisualLine => "enter-visual-line",
        EditorAction::EnterVisualBlock => "enter-visual-block",
        EditorAction::ExitVisual => "exit-visual",
        EditorAction::DeleteChar => "delete-char",
        EditorAction::Substitute => "substitute",
        EditorAction::SubstituteLine => "substitute-line",
        EditorAction::JoinLines => "join-lines",
        EditorAction::PasteAfter => "paste-after",
        EditorAction::PasteBefore => "paste-before",
        EditorAction::Undo => "undo",
        EditorAction::Redo => "redo",
        EditorAction::Replace => "replace",
        EditorAction::DeleteLine => "delete-line",
        EditorAction::ChangeLine => "change-line",
        EditorAction::YankLine => "yank-line",
        EditorAction::ToggleCaseChar => "toggle-case-char",
        EditorAction::IncrementNumber => "increment-number",
        EditorAction::DecrementNumber => "decrement-number",
        EditorAction::SetMark => "set-mark",
        EditorAction::ExitInsert => "exit-insert",
        EditorAction::InsertChar => "insert-char",
        EditorAction::InsertNewline => "insert-newline",
        EditorAction::InsertTab => "insert-tab",
        EditorAction::BackspaceDelete => "backspace-delete",
        EditorAction::DeleteSelection => "delete-selection",
        EditorAction::ChangeSelection => "change-selection",
        EditorAction::YankSelection => "yank-selection",
        EditorAction::SurroundSelection => "surround-selection",
        EditorAction::NoOp => "no-op",
        _ => return None,
    })
}

impl fmt::Display for EditorAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = simple_action_name(*self) {
            return f.write_str(name);
        }
        match *self {
            EditorAction::Motion(m) => {
                f.write_str("motion.")?;
                f.write_str(m.as_str())
            }
            EditorAction::DeleteMotion(m) => {
                f.write_str("delete.motion.")?;
                f.write_str(m.as_str())
            }
            EditorAction::ChangeMotion(m) => {
                f.write_str("change.motion.")?;
                f.write_str(m.as_str())
            }
            EditorAction::YankMotion(m) => {
                f.write_str("yank.motion.")?;
                f.write_str(m.as_str())
            }
            EditorAction::DeleteTextObject(o) => {
                f.write_str("delete.object.")?;
                o.fmt(f)
            }
            EditorAction::ChangeTextObject(o) => {
                f.write_str("change.object.")?;
                o.fmt(f)
            }
            EditorAction::YankTextObject(o) => {
                f.write_str("yank.object.")?;
                o.fmt(f)
            }
            EditorAction::SelectTextObject(o) => {
                f.write_str("select.object.")?;
                o.fmt(f)
            }
            EditorAction::CaseLine(t) => {
                f.write_str("case-line.")?;
                f.write_str(t.as_str())
            }
            EditorAction::CaseSelection(t) => {
                f.write_str("case-selection.")?;
                f.write_str(t.as_str())
            }
            EditorAction::CaseMotion(t, m) => {
                f.write_str("case.")?;
                f.write_str(t.as_str())?;
                f.write_str(".motion.")?;
                f.write_str(m.as_str())
            }
            EditorAction::CaseTextObject(t, o) => {
                f.write_str("case.")?;
                f.write_str(t.as_str())?;
                f.write_str(".object.")?;
                o.fmt(f)
            }
            _ => Err(fmt::Error),
        }
    }
}

impl FromStr for EditorAction {
    type Err = ParseEditorBindingError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some((_, action)) = SIMPLE_ACTIONS.iter().find(|(name, _)| *name == s) {
            return Ok(*action);
        }
        parse_prefixed_action(s)
    }
}

/// Returns `true` for actions whose execution reads one more key as
/// a captured `char` argument.
///
/// Vim commands like `r<c>`, `f<c>`, `t<c>`, `m<c>`, and surround
/// operators behave this way: the binding fires on the operator key,
/// and the next keystroke is consumed as the character argument
/// rather than being routed through another lookup.
pub fn action_needs_capture(action: EditorAction) -> bool {
    matches!(
        action,
        EditorAction::Replace
            | EditorAction::SetMark
            | EditorAction::SurroundSelection
            | EditorAction::Motion(Motion::FindChar { .. } | Motion::Mark { .. })
            | EditorAction::DeleteMotion(Motion::FindChar { .. } | Motion::Mark { .. })
            | EditorAction::ChangeMotion(Motion::FindChar { .. } | Motion::Mark { .. })
            | EditorAction::YankMotion(Motion::FindChar { .. } | Motion::Mark { .. })
            | EditorAction::CaseMotion(_, Motion::FindChar { .. } | Motion::Mark { .. })
    )
}

// An `EditorAction` is packed into the 32 bits of `ActionId` as
// `[tag:4][payload:<=7][unused]`. 13 tags cover every variant; the
// widest payload is 7 bits (a 2-bit transform plus a 5-bit motion or
// text-object index).

const TAG_BITS: u32 = 4;
const TAG_MASK: u32 = (1 << TAG_BITS) - 1;

const TAG_SIMPLE: u32 = 0;
const TAG_MOTION: u32 = 1;
const TAG_DELETE_MOTION: u32 = 2;
const TAG_CHANGE_MOTION: u32 = 3;
const TAG_YANK_MOTION: u32 = 4;
const TAG_DELETE_OBJECT: u32 = 5;
const TAG_CHANGE_OBJECT: u32 = 6;
const TAG_YANK_OBJECT: u32 = 7;
const TAG_SELECT_OBJECT: u32 = 8;
const TAG_CASE_MOTION: u32 = 9;
const TAG_CASE_OBJECT: u32 = 10;
const TAG_CASE_LINE: u32 = 11;
const TAG_CASE_SELECTION: u32 = 12;

const TRANSFORM_BITS: u32 = 2;
const TRANSFORM_MASK: u32 = (1 << TRANSFORM_BITS) - 1;

fn pack_motion(m: Motion) -> u32 {
    match m {
        Motion::Left => 0,
        Motion::Right => 1,
        Motion::Up => 2,
        Motion::Down => 3,
        Motion::DisplayUp => 4,
        Motion::DisplayDown => 5,
        Motion::WordForward => 6,
        Motion::WordBack => 7,
        Motion::WordEnd => 8,
        Motion::LineStart => 9,
        Motion::LineEnd => 10,
        Motion::LineFirstNonblank => 11,
        Motion::FirstLine => 12,
        Motion::LastLine => 13,
        Motion::ParagraphForward => 14,
        Motion::ParagraphBack => 15,
        Motion::HalfPageDown => 16,
        Motion::HalfPageUp => 17,
        Motion::FindChar {
            forward: true,
            till: false,
        } => 18,
        Motion::FindChar {
            forward: false,
            till: false,
        } => 19,
        Motion::FindChar {
            forward: true,
            till: true,
        } => 20,
        Motion::FindChar {
            forward: false,
            till: true,
        } => 21,
        Motion::Mark { linewise: false } => 22,
        Motion::Mark { linewise: true } => 23,
    }
}

fn unpack_motion(n: u32) -> Motion {
    match n {
        0 => Motion::Left,
        1 => Motion::Right,
        2 => Motion::Up,
        3 => Motion::Down,
        4 => Motion::DisplayUp,
        5 => Motion::DisplayDown,
        6 => Motion::WordForward,
        7 => Motion::WordBack,
        8 => Motion::WordEnd,
        9 => Motion::LineStart,
        10 => Motion::LineEnd,
        11 => Motion::LineFirstNonblank,
        12 => Motion::FirstLine,
        13 => Motion::LastLine,
        14 => Motion::ParagraphForward,
        15 => Motion::ParagraphBack,
        16 => Motion::HalfPageDown,
        17 => Motion::HalfPageUp,
        18 => Motion::FindChar {
            forward: true,
            till: false,
        },
        19 => Motion::FindChar {
            forward: false,
            till: false,
        },
        20 => Motion::FindChar {
            forward: true,
            till: true,
        },
        21 => Motion::FindChar {
            forward: false,
            till: true,
        },
        22 => Motion::Mark { linewise: false },
        23 => Motion::Mark { linewise: true },
        _ => panic!("invalid packed motion: {n}"),
    }
}

fn pack_text_object(o: TextObject) -> u32 {
    match o {
        TextObject::Word {
            around: false,
            big: false,
        } => 0,
        TextObject::Word {
            around: true,
            big: false,
        } => 1,
        TextObject::Word {
            around: false,
            big: true,
        } => 2,
        TextObject::Word {
            around: true,
            big: true,
        } => 3,
        TextObject::Quote {
            around: false,
            delimiter: '"',
        } => 4,
        TextObject::Quote {
            around: true,
            delimiter: '"',
        } => 5,
        TextObject::Quote {
            around: false,
            delimiter: '\'',
        } => 6,
        TextObject::Quote {
            around: true,
            delimiter: '\'',
        } => 7,
        TextObject::Quote {
            around: false,
            delimiter: '`',
        } => 8,
        TextObject::Quote {
            around: true,
            delimiter: '`',
        } => 9,
        TextObject::Quote { delimiter, .. } => {
            panic!("unsupported quote delimiter: {delimiter:?}")
        }
        TextObject::Pair {
            around: false,
            kind: PairKind::Paren,
        } => 10,
        TextObject::Pair {
            around: true,
            kind: PairKind::Paren,
        } => 11,
        TextObject::Pair {
            around: false,
            kind: PairKind::Bracket,
        } => 12,
        TextObject::Pair {
            around: true,
            kind: PairKind::Bracket,
        } => 13,
        TextObject::Pair {
            around: false,
            kind: PairKind::Brace,
        } => 14,
        TextObject::Pair {
            around: true,
            kind: PairKind::Brace,
        } => 15,
        TextObject::Pair {
            around: false,
            kind: PairKind::Angle,
        } => 16,
        TextObject::Pair {
            around: true,
            kind: PairKind::Angle,
        } => 17,
        TextObject::Paragraph { around: false } => 18,
        TextObject::Paragraph { around: true } => 19,
    }
}

fn unpack_text_object(n: u32) -> TextObject {
    match n {
        0 => TextObject::Word {
            around: false,
            big: false,
        },
        1 => TextObject::Word {
            around: true,
            big: false,
        },
        2 => TextObject::Word {
            around: false,
            big: true,
        },
        3 => TextObject::Word {
            around: true,
            big: true,
        },
        4 => TextObject::Quote {
            around: false,
            delimiter: '"',
        },
        5 => TextObject::Quote {
            around: true,
            delimiter: '"',
        },
        6 => TextObject::Quote {
            around: false,
            delimiter: '\'',
        },
        7 => TextObject::Quote {
            around: true,
            delimiter: '\'',
        },
        8 => TextObject::Quote {
            around: false,
            delimiter: '`',
        },
        9 => TextObject::Quote {
            around: true,
            delimiter: '`',
        },
        10 => TextObject::Pair {
            around: false,
            kind: PairKind::Paren,
        },
        11 => TextObject::Pair {
            around: true,
            kind: PairKind::Paren,
        },
        12 => TextObject::Pair {
            around: false,
            kind: PairKind::Bracket,
        },
        13 => TextObject::Pair {
            around: true,
            kind: PairKind::Bracket,
        },
        14 => TextObject::Pair {
            around: false,
            kind: PairKind::Brace,
        },
        15 => TextObject::Pair {
            around: true,
            kind: PairKind::Brace,
        },
        16 => TextObject::Pair {
            around: false,
            kind: PairKind::Angle,
        },
        17 => TextObject::Pair {
            around: true,
            kind: PairKind::Angle,
        },
        18 => TextObject::Paragraph { around: false },
        19 => TextObject::Paragraph { around: true },
        _ => panic!("invalid packed text object: {n}"),
    }
}

fn pack_case_transform(t: CaseTransform) -> u32 {
    match t {
        CaseTransform::Upper => 0,
        CaseTransform::Lower => 1,
        CaseTransform::Toggle => 2,
    }
}

fn unpack_case_transform(n: u32) -> CaseTransform {
    match n {
        0 => CaseTransform::Upper,
        1 => CaseTransform::Lower,
        2 => CaseTransform::Toggle,
        _ => panic!("invalid packed case transform: {n}"),
    }
}

fn simple_index(a: EditorAction) -> Option<u32> {
    Some(match a {
        EditorAction::EnterInsert => 0,
        EditorAction::EnterInsertAfter => 1,
        EditorAction::EnterInsertFirstNonblank => 2,
        EditorAction::EnterInsertEol => 3,
        EditorAction::OpenBelow => 4,
        EditorAction::OpenAbove => 5,
        EditorAction::EnterVisualChar => 6,
        EditorAction::EnterVisualLine => 7,
        EditorAction::EnterVisualBlock => 8,
        EditorAction::ExitVisual => 9,
        EditorAction::DeleteChar => 10,
        EditorAction::Substitute => 11,
        EditorAction::SubstituteLine => 12,
        EditorAction::JoinLines => 13,
        EditorAction::PasteAfter => 14,
        EditorAction::PasteBefore => 15,
        EditorAction::Undo => 16,
        EditorAction::Redo => 17,
        EditorAction::Replace => 18,
        EditorAction::DeleteLine => 19,
        EditorAction::ChangeLine => 20,
        EditorAction::YankLine => 21,
        EditorAction::ToggleCaseChar => 22,
        EditorAction::IncrementNumber => 23,
        EditorAction::DecrementNumber => 24,
        EditorAction::SetMark => 25,
        EditorAction::ExitInsert => 26,
        EditorAction::InsertChar => 27,
        EditorAction::InsertNewline => 28,
        EditorAction::InsertTab => 29,
        EditorAction::BackspaceDelete => 30,
        EditorAction::DeleteSelection => 31,
        EditorAction::ChangeSelection => 32,
        EditorAction::YankSelection => 33,
        EditorAction::SurroundSelection => 34,
        EditorAction::NoOp => 35,
        _ => return None,
    })
}

fn unpack_simple(n: u32) -> EditorAction {
    match n {
        0 => EditorAction::EnterInsert,
        1 => EditorAction::EnterInsertAfter,
        2 => EditorAction::EnterInsertFirstNonblank,
        3 => EditorAction::EnterInsertEol,
        4 => EditorAction::OpenBelow,
        5 => EditorAction::OpenAbove,
        6 => EditorAction::EnterVisualChar,
        7 => EditorAction::EnterVisualLine,
        8 => EditorAction::EnterVisualBlock,
        9 => EditorAction::ExitVisual,
        10 => EditorAction::DeleteChar,
        11 => EditorAction::Substitute,
        12 => EditorAction::SubstituteLine,
        13 => EditorAction::JoinLines,
        14 => EditorAction::PasteAfter,
        15 => EditorAction::PasteBefore,
        16 => EditorAction::Undo,
        17 => EditorAction::Redo,
        18 => EditorAction::Replace,
        19 => EditorAction::DeleteLine,
        20 => EditorAction::ChangeLine,
        21 => EditorAction::YankLine,
        22 => EditorAction::ToggleCaseChar,
        23 => EditorAction::IncrementNumber,
        24 => EditorAction::DecrementNumber,
        25 => EditorAction::SetMark,
        26 => EditorAction::ExitInsert,
        27 => EditorAction::InsertChar,
        28 => EditorAction::InsertNewline,
        29 => EditorAction::InsertTab,
        30 => EditorAction::BackspaceDelete,
        31 => EditorAction::DeleteSelection,
        32 => EditorAction::ChangeSelection,
        33 => EditorAction::YankSelection,
        34 => EditorAction::SurroundSelection,
        35 => EditorAction::NoOp,
        _ => panic!("invalid packed simple action: {n}"),
    }
}

fn pack_action(a: EditorAction) -> ActionId {
    if let Some(idx) = simple_index(a) {
        return ActionId(TAG_SIMPLE | (idx << TAG_BITS));
    }
    let raw = match a {
        EditorAction::Motion(m) => TAG_MOTION | (pack_motion(m) << TAG_BITS),
        EditorAction::DeleteMotion(m) => TAG_DELETE_MOTION | (pack_motion(m) << TAG_BITS),
        EditorAction::ChangeMotion(m) => TAG_CHANGE_MOTION | (pack_motion(m) << TAG_BITS),
        EditorAction::YankMotion(m) => TAG_YANK_MOTION | (pack_motion(m) << TAG_BITS),
        EditorAction::DeleteTextObject(o) => TAG_DELETE_OBJECT | (pack_text_object(o) << TAG_BITS),
        EditorAction::ChangeTextObject(o) => TAG_CHANGE_OBJECT | (pack_text_object(o) << TAG_BITS),
        EditorAction::YankTextObject(o) => TAG_YANK_OBJECT | (pack_text_object(o) << TAG_BITS),
        EditorAction::SelectTextObject(o) => TAG_SELECT_OBJECT | (pack_text_object(o) << TAG_BITS),
        EditorAction::CaseMotion(t, m) => {
            TAG_CASE_MOTION
                | (pack_case_transform(t) << TAG_BITS)
                | (pack_motion(m) << (TAG_BITS + TRANSFORM_BITS))
        }
        EditorAction::CaseTextObject(t, o) => {
            TAG_CASE_OBJECT
                | (pack_case_transform(t) << TAG_BITS)
                | (pack_text_object(o) << (TAG_BITS + TRANSFORM_BITS))
        }
        EditorAction::CaseLine(t) => TAG_CASE_LINE | (pack_case_transform(t) << TAG_BITS),
        EditorAction::CaseSelection(t) => TAG_CASE_SELECTION | (pack_case_transform(t) << TAG_BITS),
        _ => unreachable!("simple action should have been handled above"),
    };
    ActionId(raw)
}

fn unpack_action(id: ActionId) -> EditorAction {
    let tag = id.0 & TAG_MASK;
    let payload = id.0 >> TAG_BITS;
    match tag {
        TAG_SIMPLE => unpack_simple(payload),
        TAG_MOTION => EditorAction::Motion(unpack_motion(payload)),
        TAG_DELETE_MOTION => EditorAction::DeleteMotion(unpack_motion(payload)),
        TAG_CHANGE_MOTION => EditorAction::ChangeMotion(unpack_motion(payload)),
        TAG_YANK_MOTION => EditorAction::YankMotion(unpack_motion(payload)),
        TAG_DELETE_OBJECT => EditorAction::DeleteTextObject(unpack_text_object(payload)),
        TAG_CHANGE_OBJECT => EditorAction::ChangeTextObject(unpack_text_object(payload)),
        TAG_YANK_OBJECT => EditorAction::YankTextObject(unpack_text_object(payload)),
        TAG_SELECT_OBJECT => EditorAction::SelectTextObject(unpack_text_object(payload)),
        TAG_CASE_MOTION => {
            let transform = unpack_case_transform(payload & TRANSFORM_MASK);
            let motion = unpack_motion(payload >> TRANSFORM_BITS);
            EditorAction::CaseMotion(transform, motion)
        }
        TAG_CASE_OBJECT => {
            let transform = unpack_case_transform(payload & TRANSFORM_MASK);
            let object = unpack_text_object(payload >> TRANSFORM_BITS);
            EditorAction::CaseTextObject(transform, object)
        }
        TAG_CASE_LINE => EditorAction::CaseLine(unpack_case_transform(payload)),
        TAG_CASE_SELECTION => EditorAction::CaseSelection(unpack_case_transform(payload)),
        _ => panic!("invalid action id tag: {tag}"),
    }
}

/// Compiled routing table.
///
/// Typed wrapper around [`Router`] that unpacks each entry's action
/// back to an [`EditorAction`]. Build one with
/// [`EditorRouterBuilder`].
pub struct EditorRouter {
    router: Router,
}

impl EditorRouter {
    /// Returns the binding that would fire for `key` at `layer` under
    /// `ctx`, or [`None`] when no binding matches.
    ///
    /// Scans candidates in newest-first order and returns the first
    /// one whose [`Filter`] overlaps `ctx`.
    #[inline]
    pub fn lookup(&self, ctx: &EditorContext, layer: LayerId, key: InputKey) -> Option<&Entry> {
        let ctx_bits = ctx.pack();
        for entry in self.router.lookup(layer, key) {
            if Filter::new(entry.filter()).matches(ctx_bits) {
                return Some(entry);
            }
        }
        None
    }

    /// Decodes an [`ActionId`] back into the [`EditorAction`] it was
    /// built from.
    pub fn action(&self, id: ActionId) -> EditorAction {
        unpack_action(id)
    }

    /// Resolves a [`LabelId`] to its interned string.
    pub fn label(&self, id: LabelId) -> &str {
        self.router.label(id)
    }

    /// Returns every interned label, indexed by [`LabelId`].
    pub fn labels(&self) -> &[String] {
        self.router.labels()
    }

    /// Returns every bound entry.
    pub fn entries(&self) -> &[Entry] {
        self.router.entries()
    }
}

/// Incremental builder for [`EditorRouter`].
///
/// Entries are deduped by `(layer, filter, key)`, so later
/// [`Self::bind`] calls with the same tuple overwrite earlier ones
/// ("last binding wins").
pub struct EditorRouterBuilder {
    builder: RouterBuilder,
}

impl Default for EditorRouterBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl EditorRouterBuilder {
    /// Creates an empty builder.
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    /// Creates a builder with space pre-allocated for `size` entries.
    pub fn with_capacity(size: usize) -> Self {
        Self {
            builder: RouterBuilder::with_capacity(size),
        }
    }

    /// Binds the key sequence `input` to `action` at `layer` under
    /// `filter`.
    pub fn bind(
        &mut self,
        layer: LayerId,
        filter: Filter,
        input: &[InputKey],
        action: EditorAction,
    ) {
        self.builder
            .bind(layer, filter.raw, input, pack_action(action));
    }

    /// Binds the key sequence `input` to a layer transition so its
    /// terminal key advances the router to `target`.
    pub fn bind_layer(
        &mut self,
        layer: LayerId,
        filter: Filter,
        input: &[InputKey],
        target: LayerId,
    ) {
        self.builder.bind_layer(layer, filter.raw, input, target);
    }

    /// Attaches `label` to the entry at `(layer, filter, input)`.
    pub fn label(
        &mut self,
        layer: LayerId,
        filter: Filter,
        input: &[InputKey],
        label: impl Into<String>,
    ) {
        self.builder.label(layer, filter.raw, input, label);
    }

    /// Removes the entry at `(layer, filter, input)`. No-op when the
    /// entry is absent.
    pub fn unbind(&mut self, layer: LayerId, filter: Filter, input: &[InputKey]) {
        self.builder.unbind(layer, filter.raw, input);
    }

    /// Freezes the builder into a runtime [`EditorRouter`].
    pub fn build(self) -> EditorRouter {
        EditorRouter {
            router: self.builder.build(),
        }
    }
}

/// A compiled binding graph paired with its primary mode.
///
/// Passed to [`Editor::with_bindings`]. The built-in presets
/// ([`vim`], [`nano`], [`emacs`]) produce values of this type.
///
/// [`Editor::with_bindings`]: crate::Editor::with_bindings
pub struct EditorBindings {
    pub(crate) router: EditorRouter,
    pub(crate) primary_mode: Mode,
}

impl EditorBindings {
    /// Creates a new binding graph with the given `primary_mode`.
    ///
    /// # Errors
    ///
    /// Returns [`EditorBindingsError::InvalidPrimaryMode`] when
    /// `primary_mode` is not [`Mode::Normal`] or [`Mode::Insert`].
    /// Visual modes never rest as the primary mode.
    pub fn new(router: EditorRouter, primary_mode: Mode) -> Result<Self, EditorBindingsError> {
        if !matches!(primary_mode, Mode::Normal | Mode::Insert) {
            return Err(EditorBindingsError::InvalidPrimaryMode(primary_mode));
        }
        Ok(Self {
            router,
            primary_mode,
        })
    }

    /// Destructures into the underlying router and primary mode.
    pub fn into_parts(self) -> (EditorRouter, Mode) {
        (self.router, self.primary_mode)
    }

    /// Returns the mode the editor rests in between actions.
    pub fn primary_mode(&self) -> Mode {
        self.primary_mode
    }
}

/// Error returned by [`EditorBindings::new`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EditorBindingsError {
    /// The supplied primary mode was a visual variant.
    InvalidPrimaryMode(Mode),
}

impl fmt::Display for EditorBindingsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EditorBindingsError::InvalidPrimaryMode(mode) => {
                write!(f, "primary mode must be Normal or Insert, got {mode:?}")
            }
        }
    }
}

impl Error for EditorBindingsError {}

/// Error returned by [`FromStr`] implementations for action and
/// motion types in this module.
///
/// [`FromStr`]: std::str::FromStr
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseEditorBindingError {
    kind: &'static str,
    input: String,
}

impl ParseEditorBindingError {
    fn new(kind: &'static str, input: &str) -> Self {
        Self {
            kind,
            input: input.to_string(),
        }
    }
}

impl fmt::Display for ParseEditorBindingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "unknown {} {}", self.kind, self.input)
    }
}

impl Error for ParseEditorBindingError {}

/// Options for [`vim`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VimOptions {
    /// When `true`, `s` in Visual mode invokes surround instead of
    /// change-selection, matching the `vim-surround` plugin.
    pub use_lower_s_for_visual_surround: bool,
}

impl Default for VimOptions {
    fn default() -> Self {
        Self {
            use_lower_s_for_visual_surround: false,
        }
    }
}

#[derive(Clone, Copy)]
enum Operator {
    Delete,
    Change,
    Yank,
}

#[derive(Clone, Copy)]
enum Wrap {
    Direct,
    Op(Operator),
    Case(CaseTransform),
}

impl Wrap {
    fn motion(self, m: Motion) -> EditorAction {
        match self {
            Wrap::Direct => EditorAction::Motion(m),
            Wrap::Op(Operator::Delete) => EditorAction::DeleteMotion(m),
            Wrap::Op(Operator::Change) => EditorAction::ChangeMotion(m),
            Wrap::Op(Operator::Yank) => EditorAction::YankMotion(m),
            Wrap::Case(t) => EditorAction::CaseMotion(t, m),
        }
    }

    fn text_object(self, o: TextObject) -> EditorAction {
        match self {
            Wrap::Direct => EditorAction::SelectTextObject(o),
            Wrap::Op(Operator::Delete) => EditorAction::DeleteTextObject(o),
            Wrap::Op(Operator::Change) => EditorAction::ChangeTextObject(o),
            Wrap::Op(Operator::Yank) => EditorAction::YankTextObject(o),
            Wrap::Case(t) => EditorAction::CaseTextObject(t, o),
        }
    }
}

const MOTION_KEYS: &[(&str, Motion)] = &[
    ("h", Motion::Left),
    ("l", Motion::Right),
    ("j", Motion::Down),
    ("k", Motion::Up),
    ("w", Motion::WordForward),
    ("b", Motion::WordBack),
    ("e", Motion::WordEnd),
    ("0", Motion::LineStart),
    ("$", Motion::LineEnd),
    ("^", Motion::LineFirstNonblank),
    ("{", Motion::ParagraphBack),
    ("}", Motion::ParagraphForward),
    ("G", Motion::LastLine),
];

const CAPTURED_MOTION_KEYS: &[(&str, Motion)] = &[
    (
        "f",
        Motion::FindChar {
            forward: true,
            till: false,
        },
    ),
    (
        "F",
        Motion::FindChar {
            forward: false,
            till: false,
        },
    ),
    (
        "t",
        Motion::FindChar {
            forward: true,
            till: true,
        },
    ),
    (
        "T",
        Motion::FindChar {
            forward: false,
            till: true,
        },
    ),
    ("`", Motion::Mark { linewise: false }),
    ("'", Motion::Mark { linewise: true }),
];

const TEXT_OBJECT_KEYS: &[(&str, TextObject)] = &[
    (
        "i w",
        TextObject::Word {
            around: false,
            big: false,
        },
    ),
    (
        "a w",
        TextObject::Word {
            around: true,
            big: false,
        },
    ),
    (
        "i W",
        TextObject::Word {
            around: false,
            big: true,
        },
    ),
    (
        "a W",
        TextObject::Word {
            around: true,
            big: true,
        },
    ),
    (
        "i \"",
        TextObject::Quote {
            around: false,
            delimiter: '"',
        },
    ),
    (
        "a \"",
        TextObject::Quote {
            around: true,
            delimiter: '"',
        },
    ),
    (
        "i '",
        TextObject::Quote {
            around: false,
            delimiter: '\'',
        },
    ),
    (
        "a '",
        TextObject::Quote {
            around: true,
            delimiter: '\'',
        },
    ),
    (
        "i `",
        TextObject::Quote {
            around: false,
            delimiter: '`',
        },
    ),
    (
        "a `",
        TextObject::Quote {
            around: true,
            delimiter: '`',
        },
    ),
    (
        "i (",
        TextObject::Pair {
            around: false,
            kind: PairKind::Paren,
        },
    ),
    (
        "a (",
        TextObject::Pair {
            around: true,
            kind: PairKind::Paren,
        },
    ),
    (
        "i )",
        TextObject::Pair {
            around: false,
            kind: PairKind::Paren,
        },
    ),
    (
        "a )",
        TextObject::Pair {
            around: true,
            kind: PairKind::Paren,
        },
    ),
    (
        "i b",
        TextObject::Pair {
            around: false,
            kind: PairKind::Paren,
        },
    ),
    (
        "a b",
        TextObject::Pair {
            around: true,
            kind: PairKind::Paren,
        },
    ),
    (
        "i [",
        TextObject::Pair {
            around: false,
            kind: PairKind::Bracket,
        },
    ),
    (
        "a [",
        TextObject::Pair {
            around: true,
            kind: PairKind::Bracket,
        },
    ),
    (
        "i ]",
        TextObject::Pair {
            around: false,
            kind: PairKind::Bracket,
        },
    ),
    (
        "a ]",
        TextObject::Pair {
            around: true,
            kind: PairKind::Bracket,
        },
    ),
    (
        "i {",
        TextObject::Pair {
            around: false,
            kind: PairKind::Brace,
        },
    ),
    (
        "a {",
        TextObject::Pair {
            around: true,
            kind: PairKind::Brace,
        },
    ),
    (
        "i }",
        TextObject::Pair {
            around: false,
            kind: PairKind::Brace,
        },
    ),
    (
        "a }",
        TextObject::Pair {
            around: true,
            kind: PairKind::Brace,
        },
    ),
    (
        "i B",
        TextObject::Pair {
            around: false,
            kind: PairKind::Brace,
        },
    ),
    (
        "a B",
        TextObject::Pair {
            around: true,
            kind: PairKind::Brace,
        },
    ),
    (
        "i lt",
        TextObject::Pair {
            around: false,
            kind: PairKind::Angle,
        },
    ),
    (
        "a lt",
        TextObject::Pair {
            around: true,
            kind: PairKind::Angle,
        },
    ),
    (
        "i gt",
        TextObject::Pair {
            around: false,
            kind: PairKind::Angle,
        },
    ),
    (
        "a gt",
        TextObject::Pair {
            around: true,
            kind: PairKind::Angle,
        },
    ),
    ("i p", TextObject::Paragraph { around: false }),
    ("a p", TextObject::Paragraph { around: true }),
];

/// Binder attaches every call to a single composed [`Filter`]. Presets that
/// want a binding live under "either of these mode bits" OR those bits
/// together (e.g. `Filter::new(MODE_INSERT | MODE_NORMAL)`) — the lookup
/// predicate is overlap, not subset.
struct Binder<'a> {
    builder: &'a mut EditorRouterBuilder,
    layer: LayerId,
    filter: Filter,
}

impl<'a> Binder<'a> {
    fn new(builder: &'a mut EditorRouterBuilder, layer: LayerId, filter: Filter) -> Self {
        Self {
            builder,
            layer,
            filter,
        }
    }

    #[track_caller]
    fn key(&mut self, seq: &str, action: EditorAction) -> &mut Self {
        let path = parse_sequence(seq).unwrap_or_else(|e| panic!("binding {seq:?}: {e}"));
        self.builder.bind(self.layer, self.filter, &path, action);
        self
    }

    #[track_caller]
    fn layer(&mut self, seq: &str, target: LayerId) -> &mut Self {
        let path = parse_sequence(seq).unwrap_or_else(|e| panic!("binding {seq:?}: {e}"));
        self.builder
            .bind_layer(self.layer, self.filter, &path, target);
        self
    }

    #[track_caller]
    #[allow(dead_code)]
    fn label(&mut self, seq: &str, label_text: impl Into<String>) -> &mut Self {
        let path = parse_sequence(seq).unwrap_or_else(|e| panic!("label {seq:?}: {e}"));
        self.builder
            .label(self.layer, self.filter, &path, label_text);
        self
    }

    fn bind_motions(&mut self, wrap: Wrap) -> &mut Self {
        for (seq, motion) in MOTION_KEYS {
            self.key(seq, wrap.motion(*motion));
        }
        self.key("g g", wrap.motion(Motion::FirstLine));
        self
    }

    fn bind_captured_motions(&mut self, wrap: Wrap) -> &mut Self {
        for (seq, motion) in CAPTURED_MOTION_KEYS {
            self.key(seq, wrap.motion(*motion));
        }
        self
    }

    fn bind_text_objects(&mut self, wrap: Wrap) -> &mut Self {
        for (seq, object) in TEXT_OBJECT_KEYS {
            self.key(seq, wrap.text_object(*object));
        }
        self
    }
}

fn linewise_action(op: Operator) -> EditorAction {
    match op {
        Operator::Delete => EditorAction::DeleteLine,
        Operator::Change => EditorAction::ChangeLine,
        Operator::Yank => EditorAction::YankLine,
    }
}

fn operator_key(op: Operator) -> &'static str {
    match op {
        Operator::Delete => "d",
        Operator::Change => "c",
        Operator::Yank => "y",
    }
}

fn operator_layer(op: Operator) -> LayerId {
    match op {
        Operator::Delete => OP_DELETE,
        Operator::Change => OP_CHANGE,
        Operator::Yank => OP_YANK,
    }
}

fn case_layer(t: CaseTransform) -> LayerId {
    match t {
        CaseTransform::Upper => OP_CASE_UPPER,
        CaseTransform::Lower => OP_CASE_LOWER,
        CaseTransform::Toggle => OP_CASE_TOGGLE,
    }
}

fn case_double_key(transform: CaseTransform) -> &'static str {
    match transform {
        CaseTransform::Upper => "U",
        CaseTransform::Lower => "u",
        CaseTransform::Toggle => "~",
    }
}

fn bind_operator_pending_set(builder: &mut EditorRouterBuilder, op: Operator) {
    // The operator layer is only reached from the Normal-mode entry binding
    // for its trigger key, so no mode gate is required at the layer itself.
    let mut b = Binder::new(builder, operator_layer(op), Filter::ANY);
    b.bind_motions(Wrap::Op(op))
        .bind_captured_motions(Wrap::Op(op))
        .bind_text_objects(Wrap::Op(op))
        .key(operator_key(op), linewise_action(op));
}

fn bind_case_operator_pending_set(builder: &mut EditorRouterBuilder, transform: CaseTransform) {
    let mut b = Binder::new(builder, case_layer(transform), Filter::ANY);
    b.bind_motions(Wrap::Case(transform))
        .bind_captured_motions(Wrap::Case(transform))
        .bind_text_objects(Wrap::Case(transform))
        .key(
            case_double_key(transform),
            EditorAction::CaseLine(transform),
        );
}

fn bind_vim_normal_no_op(builder: &mut EditorRouterBuilder) {
    let mut b = Binder::new(builder, LayerId::BASE, Filter::new(MODE_NORMAL));
    b.bind_motions(Wrap::Direct)
        .bind_captured_motions(Wrap::Direct)
        .key("g j", EditorAction::Motion(Motion::DisplayDown))
        .key("g k", EditorAction::Motion(Motion::DisplayUp))
        .layer("d", OP_DELETE)
        .layer("c", OP_CHANGE)
        .layer("y", OP_YANK)
        .layer("g U", OP_CASE_UPPER)
        .layer("g u", OP_CASE_LOWER)
        .layer("g ~", OP_CASE_TOGGLE)
        .key("i", EditorAction::EnterInsert)
        .key("a", EditorAction::EnterInsertAfter)
        .key("I", EditorAction::EnterInsertFirstNonblank)
        .key("A", EditorAction::EnterInsertEol)
        .key("o", EditorAction::OpenBelow)
        .key("O", EditorAction::OpenAbove)
        .key("v", EditorAction::EnterVisualChar)
        .key("V", EditorAction::EnterVisualLine)
        .key("C-v", EditorAction::EnterVisualBlock)
        .key("x", EditorAction::DeleteChar)
        .key("s", EditorAction::Substitute)
        .key("S", EditorAction::SubstituteLine)
        .key("J", EditorAction::JoinLines)
        .key("p", EditorAction::PasteAfter)
        .key("P", EditorAction::PasteBefore)
        .key("u", EditorAction::Undo)
        .key("C-r", EditorAction::Redo)
        .key("D", EditorAction::DeleteMotion(Motion::LineEnd))
        .key("C-k", EditorAction::DeleteMotion(Motion::LineEnd))
        .key("C", EditorAction::ChangeMotion(Motion::LineEnd))
        .key("Y", EditorAction::YankMotion(Motion::LineEnd))
        .key("~", EditorAction::ToggleCaseChar)
        .key("C-a", EditorAction::IncrementNumber)
        .key("C-x", EditorAction::DecrementNumber)
        .key("C-d", EditorAction::Motion(Motion::HalfPageDown))
        .key("C-u", EditorAction::Motion(Motion::HalfPageUp))
        .key("r", EditorAction::Replace)
        .key("m", EditorAction::SetMark);
}

fn bind_vim_insert(builder: &mut EditorRouterBuilder) {
    let mut b = Binder::new(builder, LayerId::BASE, Filter::new(MODE_INSERT));
    bind_cursor_navigation(&mut b);
    b.key("Esc", EditorAction::ExitInsert)
        .key("Enter", EditorAction::InsertNewline)
        .key("S-Enter", EditorAction::InsertNewline)
        .key("Tab", EditorAction::InsertTab)
        .key("Backspace", EditorAction::BackspaceDelete);
}

fn bind_vim_visual(builder: &mut EditorRouterBuilder, options: VimOptions) {
    let mut b = Binder::new(builder, LayerId::BASE, Filter::new(MODE_VISUAL));
    b.bind_motions(Wrap::Direct)
        .bind_captured_motions(Wrap::Direct)
        .key("g j", EditorAction::Motion(Motion::DisplayDown))
        .key("g k", EditorAction::Motion(Motion::DisplayUp))
        .bind_text_objects(Wrap::Direct)
        .key("Esc", EditorAction::ExitVisual)
        .key("v", EditorAction::EnterVisualChar)
        .key("V", EditorAction::EnterVisualLine)
        .key("C-v", EditorAction::EnterVisualBlock)
        .key("d", EditorAction::DeleteSelection)
        .key("x", EditorAction::DeleteSelection)
        .key("c", EditorAction::ChangeSelection)
        .key("S", EditorAction::SurroundSelection)
        .key("y", EditorAction::YankSelection)
        .key("u", EditorAction::CaseSelection(CaseTransform::Lower))
        .key("U", EditorAction::CaseSelection(CaseTransform::Upper))
        .key("~", EditorAction::CaseSelection(CaseTransform::Toggle))
        .key("m", EditorAction::SetMark);
    if options.use_lower_s_for_visual_surround {
        b.key("s", EditorAction::SurroundSelection);
    } else {
        b.key("s", EditorAction::ChangeSelection);
    }
}

/// Builds the built-in Vim preset.
///
/// Produces a binding graph covering Normal, Insert, and Visual modes
/// with Vim-style motions, operators, text objects, and surround.
/// Installed by [`Editor::new`].
///
/// [`Editor::new`]: crate::Editor::new
pub fn vim(options: VimOptions) -> EditorBindings {
    let mut builder = EditorRouterBuilder::new();

    bind_vim_normal_no_op(&mut builder);
    for op in [Operator::Delete, Operator::Change, Operator::Yank] {
        bind_operator_pending_set(&mut builder, op);
    }
    for t in [
        CaseTransform::Upper,
        CaseTransform::Lower,
        CaseTransform::Toggle,
    ] {
        bind_case_operator_pending_set(&mut builder, t);
    }
    bind_vim_insert(&mut builder);
    bind_vim_visual(&mut builder, options);

    EditorBindings::new(builder.build(), Mode::Normal).expect("built-in vim bindings must validate")
}

fn bind_cursor_navigation(b: &mut Binder<'_>) {
    b.key("Left", EditorAction::Motion(Motion::Left))
        .key("Right", EditorAction::Motion(Motion::Right))
        .key("Up", EditorAction::Motion(Motion::Up))
        .key("Down", EditorAction::Motion(Motion::Down))
        .key("Home", EditorAction::Motion(Motion::LineStart))
        .key("End", EditorAction::Motion(Motion::LineEnd));
}

fn bind_nano_insert(builder: &mut EditorRouterBuilder) {
    let mut b = Binder::new(
        builder,
        LayerId::BASE,
        Filter::new(MODE_INSERT | MODE_NORMAL),
    );
    bind_cursor_navigation(&mut b);
    b.key("C-a", EditorAction::Motion(Motion::LineStart))
        .key("C-e", EditorAction::Motion(Motion::LineEnd))
        .key("M-b", EditorAction::Motion(Motion::WordBack))
        .key("M-f", EditorAction::Motion(Motion::WordForward))
        .key("Enter", EditorAction::InsertNewline)
        .key("Tab", EditorAction::InsertTab)
        .key("Backspace", EditorAction::BackspaceDelete)
        .key("Delete", EditorAction::DeleteChar)
        .key("C-d", EditorAction::DeleteChar)
        .key("C-k", EditorAction::DeleteLine)
        .key("M-6", EditorAction::YankLine)
        .key("C-u", EditorAction::PasteBefore)
        .key("M-a", EditorAction::EnterVisualChar)
        .key("M-u", EditorAction::Undo)
        .key("M-e", EditorAction::Redo);
}

fn bind_nano_visual(builder: &mut EditorRouterBuilder) {
    let mut b = Binder::new(builder, LayerId::BASE, Filter::new(MODE_VISUAL));
    bind_cursor_navigation(&mut b);
    b.key("C-a", EditorAction::Motion(Motion::LineStart))
        .key("C-e", EditorAction::Motion(Motion::LineEnd))
        .key("M-b", EditorAction::Motion(Motion::WordBack))
        .key("M-f", EditorAction::Motion(Motion::WordForward))
        .key("Esc", EditorAction::ExitVisual)
        .key("M-a", EditorAction::EnterVisualChar)
        .key("C-k", EditorAction::DeleteSelection)
        .key("M-6", EditorAction::YankSelection)
        .key("M-u", EditorAction::Undo)
        .key("M-e", EditorAction::Redo);
}

/// Builds the built-in Nano-style preset.
///
/// Modeless (primary mode is [`Mode::Insert`]) with arrow-key
/// navigation, Nano's `^K` cut-line, `^U` paste, and `M-a` as a
/// Visual-mode toggle.
pub fn nano() -> EditorBindings {
    let mut builder = EditorRouterBuilder::new();

    bind_nano_insert(&mut builder);
    bind_nano_visual(&mut builder);

    EditorBindings::new(builder.build(), Mode::Insert)
        .expect("built-in nano bindings must validate")
}

fn bind_emacs_insert(builder: &mut EditorRouterBuilder) {
    let mut b = Binder::new(
        builder,
        LayerId::BASE,
        Filter::new(MODE_INSERT | MODE_NORMAL),
    );
    bind_cursor_navigation(&mut b);
    b.key("C-b", EditorAction::Motion(Motion::Left))
        .key("C-f", EditorAction::Motion(Motion::Right))
        .key("C-p", EditorAction::Motion(Motion::Up))
        .key("C-n", EditorAction::Motion(Motion::Down))
        .key("C-a", EditorAction::Motion(Motion::LineStart))
        .key("C-e", EditorAction::Motion(Motion::LineEnd))
        .key("M-b", EditorAction::Motion(Motion::WordBack))
        .key("M-f", EditorAction::Motion(Motion::WordForward))
        .key("Enter", EditorAction::InsertNewline)
        .key("Tab", EditorAction::InsertTab)
        .key("Backspace", EditorAction::BackspaceDelete)
        .key("C-h", EditorAction::BackspaceDelete)
        .key("Delete", EditorAction::DeleteChar)
        .key("C-d", EditorAction::DeleteChar)
        .key("M-d", EditorAction::DeleteMotion(Motion::WordForward))
        .key("C-k", EditorAction::DeleteMotion(Motion::LineEnd))
        .key("C-y", EditorAction::PasteBefore)
        .key("C-Space", EditorAction::EnterVisualChar)
        .key("C-x u", EditorAction::Undo)
        .key("C-x r", EditorAction::Redo);
}

fn bind_emacs_visual(builder: &mut EditorRouterBuilder) {
    let mut b = Binder::new(builder, LayerId::BASE, Filter::new(MODE_VISUAL));
    bind_cursor_navigation(&mut b);
    b.key("C-b", EditorAction::Motion(Motion::Left))
        .key("C-f", EditorAction::Motion(Motion::Right))
        .key("C-p", EditorAction::Motion(Motion::Up))
        .key("C-n", EditorAction::Motion(Motion::Down))
        .key("C-a", EditorAction::Motion(Motion::LineStart))
        .key("C-e", EditorAction::Motion(Motion::LineEnd))
        .key("M-b", EditorAction::Motion(Motion::WordBack))
        .key("M-f", EditorAction::Motion(Motion::WordForward))
        .key("Esc", EditorAction::ExitVisual)
        .key("C-g", EditorAction::ExitVisual)
        .key("C-Space", EditorAction::EnterVisualChar)
        .key("C-w", EditorAction::DeleteSelection)
        .key("M-w", EditorAction::YankSelection)
        .key("C-x u", EditorAction::Undo)
        .key("C-x r", EditorAction::Redo);
}

/// Builds the built-in Emacs-style preset.
///
/// Modeless (primary mode is [`Mode::Insert`]) with the classic
/// Emacs movement chords (`C-b`, `C-f`, `C-p`, `C-n`, `C-a`, `C-e`,
/// `M-b`, `M-f`), `C-Space` to begin a selection, `C-w` / `M-w` to
/// cut / copy, and `C-x u` / `C-x r` for undo / redo.
pub fn emacs() -> EditorBindings {
    let mut builder = EditorRouterBuilder::new();

    bind_emacs_insert(&mut builder);
    bind_emacs_visual(&mut builder);

    EditorBindings::new(builder.build(), Mode::Insert)
        .expect("built-in emacs bindings must validate")
}

type MotionCtor = fn(Motion) -> EditorAction;
type ObjectCtor = fn(TextObject) -> EditorAction;
type TransformCtor = fn(CaseTransform) -> EditorAction;

const MOTION_PREFIXES: &[(&str, MotionCtor)] = &[
    ("motion.", EditorAction::Motion),
    ("delete.motion.", EditorAction::DeleteMotion),
    ("change.motion.", EditorAction::ChangeMotion),
    ("yank.motion.", EditorAction::YankMotion),
];

const OBJECT_PREFIXES: &[(&str, ObjectCtor)] = &[
    ("delete.object.", EditorAction::DeleteTextObject),
    ("change.object.", EditorAction::ChangeTextObject),
    ("yank.object.", EditorAction::YankTextObject),
    ("select.object.", EditorAction::SelectTextObject),
];

const TRANSFORM_PREFIXES: &[(&str, TransformCtor)] = &[
    ("case-line.", EditorAction::CaseLine),
    ("case-selection.", EditorAction::CaseSelection),
];

fn parse_prefixed_action(s: &str) -> Result<EditorAction, ParseEditorBindingError> {
    let err = || ParseEditorBindingError::new("editor action", s);
    for (prefix, ctor) in MOTION_PREFIXES {
        if let Some(rest) = s.strip_prefix(prefix) {
            return Ok(ctor(rest.parse()?));
        }
    }
    for (prefix, ctor) in OBJECT_PREFIXES {
        if let Some(rest) = s.strip_prefix(prefix) {
            return Ok(ctor(rest.parse()?));
        }
    }
    for (prefix, ctor) in TRANSFORM_PREFIXES {
        if let Some(rest) = s.strip_prefix(prefix) {
            return Ok(ctor(rest.parse()?));
        }
    }
    if let Some(rest) = s.strip_prefix("case.") {
        let (transform, tail) = rest.split_once('.').ok_or_else(err)?;
        let transform: CaseTransform = transform.parse()?;
        if let Some(inner) = tail.strip_prefix("motion.") {
            return Ok(EditorAction::CaseMotion(transform, inner.parse()?));
        }
        if let Some(inner) = tail.strip_prefix("object.") {
            return Ok(EditorAction::CaseTextObject(transform, inner.parse()?));
        }
    }
    Err(err())
}

fn parse_text_object(around: bool, body: &str) -> Result<TextObject, ParseEditorBindingError> {
    match body {
        "word" => Ok(TextObject::Word { around, big: false }),
        "big-word" => Ok(TextObject::Word { around, big: true }),
        "paragraph" => Ok(TextObject::Paragraph { around }),
        _ => {
            if let Some(name) = body.strip_prefix("quote.") {
                let delimiter = quote_delimiter(name)
                    .ok_or_else(|| ParseEditorBindingError::new("text object", body))?;
                return Ok(TextObject::Quote { around, delimiter });
            }
            if let Some(name) = body.strip_prefix("pair.") {
                return Ok(TextObject::Pair {
                    around,
                    kind: name.parse()?,
                });
            }
            Err(ParseEditorBindingError::new("text object", body))
        }
    }
}

fn quote_name(delimiter: char) -> Option<&'static str> {
    match delimiter {
        '"' => Some("double"),
        '\'' => Some("single"),
        '`' => Some("backtick"),
        _ => None,
    }
}

fn quote_delimiter(name: &str) -> Option<char> {
    match name {
        "double" => Some('"'),
        "single" => Some('\''),
        "backtick" => Some('`'),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn action_id_roundtrip() {
        let samples = [
            EditorAction::Motion(Motion::WordForward),
            EditorAction::DeleteMotion(Motion::LineEnd),
            EditorAction::ChangeTextObject(TextObject::Pair {
                around: true,
                kind: PairKind::Angle,
            }),
            EditorAction::EnterInsert,
            EditorAction::DeleteSelection,
            EditorAction::CaseMotion(CaseTransform::Upper, Motion::WordForward),
        ];

        for action in samples {
            let encoded = action.to_string();
            let decoded = encoded.parse::<EditorAction>().unwrap();
            assert_eq!(decoded, action, "roundtrip failed for {encoded}");
        }
    }

    #[test]
    fn pack_unpack_covers_every_tag() {
        let samples = [
            EditorAction::EnterInsert,
            EditorAction::NoOp,
            EditorAction::Motion(Motion::WordForward),
            EditorAction::Motion(Motion::FindChar {
                forward: false,
                till: true,
            }),
            EditorAction::Motion(Motion::Mark { linewise: true }),
            EditorAction::DeleteMotion(Motion::LineEnd),
            EditorAction::ChangeMotion(Motion::WordBack),
            EditorAction::YankMotion(Motion::LastLine),
            EditorAction::DeleteTextObject(TextObject::Word {
                around: true,
                big: true,
            }),
            EditorAction::ChangeTextObject(TextObject::Pair {
                around: true,
                kind: PairKind::Angle,
            }),
            EditorAction::YankTextObject(TextObject::Quote {
                around: false,
                delimiter: '`',
            }),
            EditorAction::SelectTextObject(TextObject::Paragraph { around: true }),
            EditorAction::CaseMotion(CaseTransform::Upper, Motion::WordForward),
            EditorAction::CaseTextObject(
                CaseTransform::Toggle,
                TextObject::Pair {
                    around: false,
                    kind: PairKind::Brace,
                },
            ),
            EditorAction::CaseLine(CaseTransform::Lower),
            EditorAction::CaseSelection(CaseTransform::Toggle),
        ];

        for action in samples {
            let id = pack_action(action);
            assert_eq!(unpack_action(id), action, "roundtrip failed for {action}");
        }
    }

    #[test]
    fn invalid_primary_mode_is_rejected() {
        let builder = EditorRouterBuilder::new();
        let err = EditorBindings::new(builder.build(), Mode::Visual)
            .err()
            .unwrap();
        assert_eq!(err, EditorBindingsError::InvalidPrimaryMode(Mode::Visual));
    }
}
