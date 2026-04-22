//! End-to-end scenario mirroring a vim-style Normal mode against the
//! non-generic router. Captures (`r<c>`) are out of scope — callers own any
//! "next key is the captured value" state themselves.

use extui::event::{KeyCode, KeyEvent, KeyModifiers};
use extui_bindings::{ActionId, InputKey, LayerId, Payload, Router, RouterBuilder, parse_sequence};

const MODE_NORMAL: u64 = 1 << 0;
const MODE_INSERT: u64 = 1 << 1;

const OP_NONE: u64 = 1 << 4;
const OP_DELETE: u64 = 1 << 5;

/// Filter key carried on every binding; indexes [`FILTERS`]. The router
/// stores the `u32`; the caller does the subset match.
type FilterKey = u32;

const F_NORMAL_IDLE: FilterKey = 0;
const F_NORMAL_DELETE: FilterKey = 1;
const F_INSERT: FilterKey = 2;

const FILTERS: [u64; 3] = [MODE_NORMAL | OP_NONE, MODE_NORMAL | OP_DELETE, MODE_INSERT];

fn filter_matches(filter: FilterKey, ctx: u64) -> bool {
    let rule = FILTERS[filter as usize];
    (rule & ctx) == rule
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Action {
    MoveDown,
    MoveUp,
    MoveWordForward,
    MoveLineStart,
    GotoFirstLine,
    EnterOpDelete,
    DeleteWordForward,
    DeleteDown,
    DeleteLine,
    EnterInsert,
    ExitInsert,
}

fn seq(s: &str) -> Vec<InputKey> {
    parse_sequence(s).unwrap()
}

struct Bindings {
    router: Router,
    actions: Vec<Action>,
}

fn build_bindings() -> Bindings {
    let mut b = RouterBuilder::new();
    let mut actions: Vec<Action> = Vec::new();
    let bind = |b: &mut RouterBuilder,
                actions: &mut Vec<Action>,
                filter: FilterKey,
                seq_str: &str,
                action: Action| {
        let id = ActionId(actions.len() as u32);
        actions.push(action);
        b.bind(LayerId::BASE, filter, &seq(seq_str), id);
    };

    bind(&mut b, &mut actions, F_NORMAL_IDLE, "j", Action::MoveDown);
    bind(&mut b, &mut actions, F_NORMAL_IDLE, "k", Action::MoveUp);
    bind(
        &mut b,
        &mut actions,
        F_NORMAL_IDLE,
        "w",
        Action::MoveWordForward,
    );
    bind(
        &mut b,
        &mut actions,
        F_NORMAL_IDLE,
        "0",
        Action::MoveLineStart,
    );
    bind(
        &mut b,
        &mut actions,
        F_NORMAL_IDLE,
        "g g",
        Action::GotoFirstLine,
    );
    bind(
        &mut b,
        &mut actions,
        F_NORMAL_IDLE,
        "d",
        Action::EnterOpDelete,
    );
    bind(
        &mut b,
        &mut actions,
        F_NORMAL_IDLE,
        "i",
        Action::EnterInsert,
    );

    bind(
        &mut b,
        &mut actions,
        F_NORMAL_DELETE,
        "w",
        Action::DeleteWordForward,
    );
    bind(
        &mut b,
        &mut actions,
        F_NORMAL_DELETE,
        "j",
        Action::DeleteDown,
    );
    bind(
        &mut b,
        &mut actions,
        F_NORMAL_DELETE,
        "d",
        Action::DeleteLine,
    );

    bind(&mut b, &mut actions, F_INSERT, "Esc", Action::ExitInsert);

    Bindings {
        router: b.build(),
        actions,
    }
}

struct World {
    bindings: Bindings,
    ctx: u64,
    layer: LayerId,
}

impl World {
    fn new() -> Self {
        Self {
            bindings: build_bindings(),
            ctx: MODE_NORMAL | OP_NONE,
            layer: LayerId::BASE,
        }
    }

    fn send(&mut self, key: InputKey) -> Option<Action> {
        for entry in self.bindings.router.lookup(self.layer, key) {
            if !filter_matches(entry.filter(), self.ctx) {
                continue;
            }
            return match entry.payload() {
                Payload::Action(id) => {
                    let action = self.bindings.actions[id.0 as usize];
                    self.layer = LayerId::BASE;
                    Some(action)
                }
                Payload::Layer(l) => {
                    self.layer = l;
                    None
                }
            };
        }
        self.layer = LayerId::BASE;
        None
    }
}

fn k(c: char) -> InputKey {
    InputKey::char(c, KeyModifiers::empty())
}

#[test]
fn idle_motions_fire() {
    let mut w = World::new();
    assert_eq!(w.send(k('j')), Some(Action::MoveDown));
    assert_eq!(w.send(k('k')), Some(Action::MoveUp));
    assert_eq!(w.send(k('w')), Some(Action::MoveWordForward));
    assert_eq!(w.send(k('0')), Some(Action::MoveLineStart));
}

#[test]
fn chord_gg_fires_goto_first_line() {
    let mut w = World::new();
    assert_eq!(w.send(k('g')), None);
    assert!(w.layer.is_chord());
    assert_eq!(w.send(k('g')), Some(Action::GotoFirstLine));
    assert_eq!(w.layer, LayerId::BASE);
}

#[test]
fn operator_pending_reroutes_same_key() {
    let mut w = World::new();
    assert_eq!(w.send(k('d')), Some(Action::EnterOpDelete));

    w.ctx = MODE_NORMAL | OP_DELETE;
    assert_eq!(w.send(k('w')), Some(Action::DeleteWordForward));

    w.ctx = MODE_NORMAL | OP_NONE;
    assert_eq!(w.send(k('d')), Some(Action::EnterOpDelete));
    w.ctx = MODE_NORMAL | OP_DELETE;
    assert_eq!(w.send(k('d')), Some(Action::DeleteLine));
}

#[test]
fn insert_mode_has_its_own_bindings() {
    let mut w = World::new();
    w.ctx = MODE_INSERT;
    let esc = InputKey::from_event(&KeyEvent::new(KeyCode::Esc, KeyModifiers::empty())).unwrap();
    assert_eq!(w.send(esc), Some(Action::ExitInsert));
}

#[test]
fn unhandled_key_resets_layer() {
    let mut w = World::new();
    assert_eq!(w.send(k('g')), None);
    assert!(w.layer.is_chord());
    // `z` isn't bound in the chord layer; caller resets.
    assert_eq!(w.send(k('z')), None);
    assert_eq!(w.layer, LayerId::BASE);
}
