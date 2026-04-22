use extui::event::KeyModifiers;

use super::*;
use crate::InputKey;

// Filter keys index into FILTERS[] below. A Rule is a bitset; a binding
// under rule R fires iff every bit in R is set on the current ctx bitset.
const X: u64 = 1 << 0;
const Y: u64 = 1 << 1;
const Z: u64 = 1 << 2;

const F_X: u32 = 0;
const F_Y: u32 = 1;
const F_Z: u32 = 2;
const FILTERS: [u64; 3] = [X, Y, Z];

fn matches(filter: u32, ctx: u64) -> bool {
    let rule = FILTERS[filter as usize];
    (rule & ctx) == rule
}

fn k(c: char) -> InputKey {
    InputKey::char(c, KeyModifiers::empty())
}

#[derive(Debug, PartialEq, Eq)]
enum Step {
    Fired(&'static str),
    Enter(LayerId),
    Reject,
}

/// Simulates the caller's lookup loop: picks the first candidate whose
/// filter accepts `ctx` and either fires or advances.
fn step(
    router: &Router,
    actions: &[&'static str],
    ctx: u64,
    layer: LayerId,
    key: InputKey,
) -> (Option<&'static str>, LayerId, bool) {
    for entry in router.lookup(layer, key) {
        if !matches(entry.filter(), ctx) {
            continue;
        }
        return match entry.payload() {
            Payload::Action(id) => (Some(actions[id.0 as usize]), LayerId::BASE, true),
            Payload::Layer(l) => (None, l, true),
        };
    }
    (None, LayerId::BASE, false)
}

// Walk an entire sequence. Returns the Fired/Enter/Reject outcome for the
// final key; panics if a non-final key doesn't advance to a new layer.
fn run(router: &Router, actions: &[&'static str], ctx: u64, keys: &[InputKey]) -> Step {
    let mut layer = LayerId::BASE;
    for (i, &key) in keys.iter().enumerate() {
        let last = i == keys.len() - 1;
        let (fired, next, matched) = step(router, actions, ctx, layer, key);
        if last {
            return match (fired, matched) {
                (Some(a), _) => Step::Fired(a),
                (None, true) => Step::Enter(next),
                (None, false) => Step::Reject,
            };
        }
        if !matched {
            return Step::Reject;
        }
        if fired.is_some() {
            panic!("action fired mid-sequence at key {i}");
        }
        layer = next;
    }
    unreachable!("non-empty sequences only")
}

struct Bench {
    builder: RouterBuilder,
    actions: Vec<&'static str>,
}

impl Bench {
    fn new() -> Self {
        Self {
            builder: RouterBuilder::new(),
            actions: Vec::new(),
        }
    }

    fn bind(&mut self, layer: LayerId, filter: u32, input: &[InputKey], action: &'static str) {
        let id = ActionId(self.actions.len() as u32);
        self.actions.push(action);
        self.builder.bind(layer, filter, input, id);
    }

    fn bind_layer(&mut self, layer: LayerId, filter: u32, input: &[InputKey], target: LayerId) {
        self.builder.bind_layer(layer, filter, input, target);
    }

    fn unbind(&mut self, layer: LayerId, filter: u32, input: &[InputKey]) {
        self.builder.unbind(layer, filter, input);
    }

    fn build(self) -> (Router, Vec<&'static str>) {
        (self.builder.build(), self.actions)
    }
}

#[test]
fn unbound_chord_prefix_rejects_at_the_prefix_key() {
    let mut b = Bench::new();
    b.bind(LayerId::BASE, F_X, &[k('a'), k('b')], "AB");
    b.unbind(LayerId::BASE, F_X, &[k('a'), k('b')]);
    let (r, actions) = b.build();

    // With the only binding under this prefix removed, pressing 'a' has
    // nowhere to go. It must reject rather than advance into a dead chord.
    assert_eq!(run(&r, &actions, X, &[k('a')]), Step::Reject);
}

#[test]
fn unbind_one_branch_dead_under_its_filter() {
    let mut b = Bench::new();
    b.bind(LayerId::BASE, F_X, &[k('a'), k('b')], "AB");
    b.bind(LayerId::BASE, F_Y, &[k('a'), k('c')], "AC");
    b.unbind(LayerId::BASE, F_X, &[k('a'), k('b')]);
    let (r, actions) = b.build();

    // Y branch still works.
    assert_eq!(run(&r, &actions, Y, &[k('a'), k('c')]), Step::Fired("AC"));

    // X branch is fully gone: pressing 'a' under X-only must reject, not
    // pend into a chord layer with no terminal available under X.
    assert_eq!(run(&r, &actions, X, &[k('a')]), Step::Reject);
}

/// `bind_layer` sends 'a' into a user-defined mode under filter X. A LATER
/// `bind("a b", X, ...)` with the SAME filter must not silently destroy
/// the bind_layer entry. Either the two should coexist, or the builder
/// should reject the conflict — but the current code overwrites the
/// bind_layer's Layer(USER) with a synthetic chord Layer(L_a), making
/// USER unreachable via 'a' entirely.
#[test]
#[ignore]
fn fails_bind_layer_clobbered_by_chord_with_same_filter() {
    const USER: LayerId = LayerId::new(7);
    let mut b = Bench::new();
    b.bind_layer(LayerId::BASE, F_X, &[k('a')], USER);
    b.bind(LayerId::BASE, F_X, &[k('a'), k('b')], "AB");
    b.bind(USER, F_X, &[k('q')], "Q_IN_USER");
    let (r, actions) = b.build();

    // 'a' under X should land us in the user-defined mode; from there 'q'
    // fires Q_IN_USER. This path is the whole point of bind_layer.
    assert_eq!(run(&r, &actions, X, &[k('a')]), Step::Enter(USER));
    assert_eq!(
        run(&r, &actions, X, &[k('a'), k('q')]),
        Step::Fired("Q_IN_USER")
    );
}

#[test]
fn orphan_chord_does_not_strand_the_caller() {
    let mut b = Bench::new();
    b.bind(LayerId::BASE, F_X, &[k('a'), k('b')], "AB");
    b.unbind(LayerId::BASE, F_X, &[k('a'), k('b')]);
    let (r, actions) = b.build();

    // Caller-visible invariant: after stepping any prefix that leads
    // nowhere under the current ctx, the caller's layer must NOT be
    // left hovering on a dead chord.
    let (_, next, matched) = step(&r, &actions, X, LayerId::BASE, k('a'));
    assert!(
        !matched,
        "prefix key with no reachable terminal must reject"
    );
    assert_eq!(next, LayerId::BASE, "caller's layer must reset on reject");
}

// --------------------------------------------------------------------------
// Baseline tests (passing): shared-chord-layer merge for filter branches.
// --------------------------------------------------------------------------

#[test]
fn filter_branched_prefix_shares_chord_layer() {
    let mut b = Bench::new();
    b.bind(LayerId::BASE, F_X, &[k('a'), k('b')], "AB");
    b.bind(LayerId::BASE, F_Y, &[k('a'), k('c')], "AC");
    let (r, _) = b.build();

    let prefix_layers: Vec<LayerId> = r
        .entries()
        .iter()
        .filter(|e| e.layer() == LayerId::BASE && e.key() == k('a'))
        .map(|e| match e.payload() {
            Payload::Layer(l) => l,
            Payload::Action(_) => panic!("prefix entry must be a Layer"),
        })
        .collect();

    assert_eq!(prefix_layers.len(), 2);
    assert_eq!(prefix_layers[0], prefix_layers[1]);
    assert!(prefix_layers[0].is_chord());
}

#[test]
fn both_branches_reachable_when_both_filters_pass() {
    let mut b = Bench::new();
    b.bind(LayerId::BASE, F_X, &[k('a'), k('b')], "AB");
    b.bind(LayerId::BASE, F_Y, &[k('a'), k('c')], "AC");
    let (r, actions) = b.build();

    assert_eq!(
        run(&r, &actions, X | Y, &[k('a'), k('b')]),
        Step::Fired("AB")
    );
    assert_eq!(
        run(&r, &actions, X | Y, &[k('a'), k('c')]),
        Step::Fired("AC")
    );
}

#[test]
fn prefix_filter_gates_chord_entry() {
    let mut b = Bench::new();
    b.bind(LayerId::BASE, F_X, &[k('a'), k('a')], "AA");
    let (r, actions) = b.build();

    assert_eq!(run(&r, &actions, X, &[k('a'), k('a')]), Step::Fired("AA"));
    assert_eq!(run(&r, &actions, Y, &[k('a')]), Step::Reject);
}

#[test]
fn multi_level_shared_chord() {
    let mut b = Bench::new();
    b.bind(LayerId::BASE, F_X, &[k('a'), k('b'), k('c')], "ABC");
    b.bind(LayerId::BASE, F_Y, &[k('a'), k('b'), k('d')], "ABD");
    let (r, actions) = b.build();

    assert_eq!(
        run(&r, &actions, X | Y, &[k('a'), k('b'), k('c')]),
        Step::Fired("ABC")
    );
    assert_eq!(
        run(&r, &actions, X | Y, &[k('a'), k('b'), k('d')]),
        Step::Fired("ABD")
    );
}

#[test]
fn layer_entries_returns_every_binding_under_a_layer() {
    const MODE: LayerId = LayerId::new(3);
    let mut b = Bench::new();
    b.bind(LayerId::BASE, F_X, &[k('j')], "DOWN");
    b.bind(LayerId::BASE, F_X, &[k('k')], "UP");
    b.bind_layer(LayerId::BASE, F_X, &[k('g')], MODE);
    b.bind(MODE, F_X, &[k('g')], "TOP");
    b.bind(MODE, F_Y, &[k('e')], "WORD_END");
    let (r, _) = b.build();

    let base: Vec<InputKey> = r
        .layer_entries(LayerId::BASE)
        .iter()
        .map(|e| e.key())
        .collect();
    assert_eq!(base, vec![k('g'), k('j'), k('k')]);
    assert!(
        r.layer_entries(LayerId::BASE)
            .iter()
            .all(|e| e.layer() == LayerId::BASE)
    );

    let mode_keys: Vec<InputKey> = r.layer_entries(MODE).iter().map(|e| e.key()).collect();
    assert_eq!(mode_keys, vec![k('e'), k('g')]);
    assert!(r.layer_entries(MODE).iter().all(|e| e.layer() == MODE));

    assert!(r.layer_entries(LayerId::new(999)).is_empty());
}

#[test]
fn lookup_rejects_sentinel_key() {
    let mut b = Bench::new();
    b.bind(LayerId::BASE, F_X, &[k('j')], "DOWN");
    b.bind(LayerId::BASE, F_X, &[k('k')], "UP");
    let (r, _) = b.build();

    // A caller minting `InputKey(u64::MAX)` via the public tuple field must
    // not receive the whole layer range — the per-layer slot is internal.
    assert!(r.lookup(LayerId::BASE, InputKey(u64::MAX)).is_empty());
    assert_eq!(r.layer_entries(LayerId::BASE).len(), 2);
}

#[test]
fn layer_entries_covers_chord_layer() {
    let mut b = Bench::new();
    b.bind(LayerId::BASE, F_X, &[k('a'), k('b')], "AB");
    b.bind(LayerId::BASE, F_X, &[k('a'), k('c')], "AC");
    let (r, _) = b.build();

    let Payload::Layer(chord) = r.lookup(LayerId::BASE, k('a'))[0].payload() else {
        panic!("expected chord prefix");
    };
    assert!(chord.is_chord());

    let under_chord: Vec<InputKey> = r.layer_entries(chord).iter().map(|e| e.key()).collect();
    assert_eq!(under_chord, vec![k('b'), k('c')]);
}

#[test]
fn three_way_filter_branch() {
    let mut b = Bench::new();
    b.bind(LayerId::BASE, F_X, &[k('a'), k('b')], "AB");
    b.bind(LayerId::BASE, F_Y, &[k('a'), k('c')], "AC");
    b.bind(LayerId::BASE, F_Z, &[k('a'), k('d')], "AD");
    let (r, actions) = b.build();

    for &(bit, key, expected) in &[(X, k('b'), "AB"), (Y, k('c'), "AC"), (Z, k('d'), "AD")] {
        assert_eq!(
            run(&r, &actions, bit, &[k('a'), key]),
            Step::Fired(expected)
        );
    }
    assert_eq!(run(&r, &actions, 0, &[k('a')]), Step::Reject);
}
