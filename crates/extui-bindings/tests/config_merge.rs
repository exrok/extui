//! Merging a user binding overlay over a shipped base, against the
//! non-generic router.

use extui::event::{KeyCode, KeyEvent, KeyModifiers};
use extui_bindings::{ActionId, InputKey, LayerId, Payload, Router, RouterBuilder, parse_sequence};

// Predicate bit layout chosen by this test suite.
const SCOPE_NORMAL: u64 = 1 << 0;
const SCOPE_INSERT: u64 = 1 << 1;

const OP_NONE: u64 = 1 << 4;
const OP_DELETE: u64 = 1 << 5;
const OP_YANK: u64 = 1 << 6;

/// Filter key stored on each binding; the router treats it as opaque.
type FilterKey = u32;

const F_NORMAL_IDLE: FilterKey = 0;
const F_NORMAL_DELETE: FilterKey = 1;
const F_NORMAL_YANK: FilterKey = 2;
const F_INSERT: FilterKey = 3;

const FILTERS: [u64; 4] = [
    SCOPE_NORMAL | OP_NONE,
    SCOPE_NORMAL | OP_DELETE,
    SCOPE_NORMAL | OP_YANK,
    SCOPE_INSERT,
];

fn filter_matches(filter: FilterKey, ctx: u64) -> bool {
    let rule = FILTERS[filter as usize];
    (rule & ctx) == rule
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Cmd {
    MoveDown,
    MoveUp,
    MoveWordForward,
    GotoFirstLine,
    GotoLastLine,
    EnterOpDelete,
    EnterOpYank,
    DeleteWord,
    DeleteLine,
    YankWord,
    YankLine,
    EnterInsert,
    ExitInsert,
    LeaderJump,
    UserQuit,
}

fn seq(s: &str) -> Vec<InputKey> {
    parse_sequence(s).unwrap()
}

struct Bench {
    builder: RouterBuilder,
    actions: Vec<Cmd>,
}

impl Bench {
    fn new() -> Self {
        Self {
            builder: RouterBuilder::new(),
            actions: Vec::new(),
        }
    }

    fn bind(&mut self, layer: LayerId, filter: FilterKey, path: &[InputKey], cmd: Cmd) {
        let id = ActionId(self.actions.len() as u32);
        self.actions.push(cmd);
        self.builder.bind(layer, filter, path, id);
    }

    fn label(&mut self, layer: LayerId, filter: FilterKey, path: &[InputKey], s: &str) {
        self.builder.label(layer, filter, path, s);
    }

    fn unbind(&mut self, layer: LayerId, filter: FilterKey, path: &[InputKey]) {
        self.builder.unbind(layer, filter, path);
    }

    fn build(self) -> (Router, Vec<Cmd>) {
        (self.builder.build(), self.actions)
    }
}

fn apply_base_config(b: &mut Bench) {
    b.bind(LayerId::BASE, F_NORMAL_IDLE, &seq("j"), Cmd::MoveDown);
    b.bind(LayerId::BASE, F_NORMAL_IDLE, &seq("k"), Cmd::MoveUp);
    b.bind(
        LayerId::BASE,
        F_NORMAL_IDLE,
        &seq("w"),
        Cmd::MoveWordForward,
    );
    b.bind(
        LayerId::BASE,
        F_NORMAL_IDLE,
        &seq("g g"),
        Cmd::GotoFirstLine,
    );
    b.bind(LayerId::BASE, F_NORMAL_IDLE, &seq("G"), Cmd::GotoLastLine);
    b.bind(LayerId::BASE, F_NORMAL_IDLE, &seq("d"), Cmd::EnterOpDelete);
    b.bind(LayerId::BASE, F_NORMAL_IDLE, &seq("i"), Cmd::EnterInsert);

    b.bind(LayerId::BASE, F_NORMAL_DELETE, &seq("w"), Cmd::DeleteWord);
    b.bind(LayerId::BASE, F_NORMAL_DELETE, &seq("d"), Cmd::DeleteLine);

    b.bind(LayerId::BASE, F_INSERT, &seq("Esc"), Cmd::ExitInsert);
}

/// Walk `keys` through the router starting at `layer`, returning the final
/// outcome (an action, a partial chord with its active layer, or None).
enum Outcome {
    Fired(Cmd),
    #[allow(dead_code)]
    Partial(LayerId),
    Unhandled,
}

fn lookup_match<'a>(
    router: &'a Router,
    ctx: u64,
    layer: LayerId,
    key: InputKey,
) -> Option<Payload> {
    for entry in router.lookup(layer, key) {
        if filter_matches(entry.filter(), ctx) {
            return Some(entry.payload());
        }
    }
    None
}

fn feed(router: &Router, actions: &[Cmd], ctx: u64, keys: &[InputKey]) -> Outcome {
    let mut layer = LayerId::BASE;
    for &key in keys {
        match lookup_match(router, ctx, layer, key) {
            Some(Payload::Action(id)) => return Outcome::Fired(actions[id.0 as usize]),
            Some(Payload::Layer(l)) => layer = l,
            None => return Outcome::Unhandled,
        }
    }
    Outcome::Partial(layer)
}

fn key_char(c: char) -> InputKey {
    InputKey::char(c, KeyModifiers::empty())
}

fn key_code(code: KeyCode) -> InputKey {
    InputKey::from_event(&KeyEvent::new(code, KeyModifiers::empty())).unwrap()
}

#[test]
fn user_rebinds_simple_key() {
    let mut b = Bench::new();
    apply_base_config(&mut b);

    // User overlay: `k` now quits. Same (layer, filter, key), last-defined wins.
    b.bind(LayerId::BASE, F_NORMAL_IDLE, &seq("k"), Cmd::UserQuit);

    let (r, actions) = b.build();
    let ctx = SCOPE_NORMAL | OP_NONE;

    assert!(matches!(
        feed(&r, &actions, ctx, &[key_char('j')]),
        Outcome::Fired(Cmd::MoveDown)
    ));
    assert!(matches!(
        feed(&r, &actions, ctx, &[key_char('k')]),
        Outcome::Fired(Cmd::UserQuit)
    ));
}

#[test]
fn user_replaces_chord_prefix_with_shorter_terminal() {
    let mut b = Bench::new();
    apply_base_config(&mut b);

    // User binds `g` alone; registered after `g g`, so last-defined wins.
    b.bind(LayerId::BASE, F_NORMAL_IDLE, &seq("g"), Cmd::GotoFirstLine);

    let (r, actions) = b.build();
    let ctx = SCOPE_NORMAL | OP_NONE;

    assert!(matches!(
        feed(&r, &actions, ctx, &[key_char('g')]),
        Outcome::Fired(Cmd::GotoFirstLine)
    ));
}

#[test]
fn user_retargets_operator_pending_to_yank() {
    let mut b = Bench::new();
    apply_base_config(&mut b);

    b.bind(LayerId::BASE, F_NORMAL_IDLE, &seq("d"), Cmd::EnterOpYank);
    b.bind(LayerId::BASE, F_NORMAL_YANK, &seq("w"), Cmd::YankWord);
    b.bind(LayerId::BASE, F_NORMAL_YANK, &seq("y"), Cmd::YankLine);

    let (r, actions) = b.build();

    assert!(matches!(
        feed(&r, &actions, SCOPE_NORMAL | OP_NONE, &[key_char('d')]),
        Outcome::Fired(Cmd::EnterOpYank)
    ));
    assert!(matches!(
        feed(&r, &actions, SCOPE_NORMAL | OP_YANK, &[key_char('w')]),
        Outcome::Fired(Cmd::YankWord)
    ));
}

#[test]
fn user_adds_leader_with_label() {
    let mut b = Bench::new();
    apply_base_config(&mut b);

    b.bind(
        LayerId::BASE,
        F_NORMAL_IDLE,
        &seq("Space j"),
        Cmd::LeaderJump,
    );
    b.label(LayerId::BASE, F_NORMAL_IDLE, &seq("Space"), "leader");
    // User overrides to a different title — last registration wins.
    b.label(LayerId::BASE, F_NORMAL_IDLE, &seq("Space"), "LEADER");

    let (r, actions) = b.build();
    let ctx = SCOPE_NORMAL | OP_NONE;

    let space_entry = r
        .lookup(LayerId::BASE, key_char(' '))
        .iter()
        .find(|e| filter_matches(e.filter(), ctx))
        .expect("space binding must be present");
    let Payload::Layer(leader_layer) = space_entry.payload() else {
        panic!("expected leader chord layer");
    };
    assert_eq!(r.label(space_entry.label().unwrap()), "LEADER");
    let leader_hit = r
        .lookup(leader_layer, key_char('j'))
        .iter()
        .find(|e| filter_matches(e.filter(), ctx))
        .expect("leader jump binding must be present");
    let Payload::Action(id) = leader_hit.payload() else {
        panic!("expected action");
    };
    assert_eq!(actions[id.0 as usize], Cmd::LeaderJump);
}

#[test]
fn merged_config_survives_round_trip() {
    let mut b = Bench::new();
    apply_base_config(&mut b);

    b.unbind(LayerId::BASE, F_NORMAL_IDLE, &seq("g g"));

    b.bind(LayerId::BASE, F_NORMAL_IDLE, &seq("d"), Cmd::EnterOpYank);
    b.bind(LayerId::BASE, F_NORMAL_YANK, &seq("w"), Cmd::YankWord);
    b.bind(LayerId::BASE, F_NORMAL_YANK, &seq("y"), Cmd::YankLine);

    b.bind(LayerId::BASE, F_NORMAL_IDLE, &seq("i"), Cmd::UserQuit);

    let (r, actions) = b.build();

    assert!(matches!(
        feed(&r, &actions, SCOPE_NORMAL | OP_NONE, &[key_char('d')]),
        Outcome::Fired(Cmd::EnterOpYank)
    ));
    assert!(matches!(
        feed(&r, &actions, SCOPE_NORMAL | OP_YANK, &[key_char('y')]),
        Outcome::Fired(Cmd::YankLine)
    ));
    assert!(matches!(
        feed(&r, &actions, SCOPE_NORMAL | OP_NONE, &[key_char('i')]),
        Outcome::Fired(Cmd::UserQuit)
    ));
    assert!(matches!(
        feed(&r, &actions, SCOPE_INSERT, &[key_code(KeyCode::Esc)]),
        Outcome::Fired(Cmd::ExitInsert)
    ));
}
