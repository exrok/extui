//! Layered routing table from `(LayerId, InputKey)` to [`Entry`].
//!
//! # Model
//!
//! A [`Router`] is a frozen, read-only lookup structure produced by a
//! [`RouterBuilder`]. Each binding is keyed by a [`LayerId`] and a
//! terminal [`InputKey`], and carries:
//!
//! - a [`Payload`] — either an [`ActionId`] to fire or a [`LayerId`]
//!   to transition into;
//! - an opaque `u32` filter the caller assigns meaning to (a mode
//!   mask, a predicate index, etc.); and
//! - an optional [`LabelId`] pointing into a per-router string table.
//!
//! # Filters and conflict resolution
//!
//! Multiple bindings may share the same `(layer, key)` pair as long as
//! they differ by filter. [`Router::lookup`] returns every candidate in
//! newest-first order and the caller scans the slice to pick the first
//! whose filter matches its current context — the router itself never
//! inspects filter values.
//!
//! # Chords and layers
//!
//! Multi-key sequences ("chords") are decomposed into single-key
//! transitions: each non-terminal key in the sequence is bound to a
//! synthetic chord layer allocated by the builder, and the terminal key
//! carries the real payload. Callers that want named modes (Normal,
//! Insert, …) instead mint their own layers with [`LayerId::new`] and
//! route into them via [`RouterBuilder::bind_layer`]. The router holds
//! no "current layer" state — the caller tracks which layer is active
//! and passes it into [`Router::lookup`] on each key.
//!
//! # Labels
//!
//! Labels are attached to individual entries rather than to chord nodes,
//! so any bound key (including chord prefixes) can carry help text
//! retrievable through [`Router::label`].
//!
//! # Examples
//!
//! ```
//! use extui::event::KeyModifiers;
//! use extui_bindings::{ActionId, InputKey, LayerId, Payload, RouterBuilder};
//!
//! const ANY: u32 = 0;
//!
//! let go_down = ActionId(0);
//! let j = InputKey::char('j', KeyModifiers::empty());
//!
//! let mut builder = RouterBuilder::new();
//! builder.bind(LayerId::BASE, ANY, &[j], go_down);
//! let router = builder.build();
//!
//! let entry = router.lookup(LayerId::BASE, j).first().unwrap();
//! assert_eq!(entry.payload(), Payload::Action(go_down));
//! ```

use std::hash::{BuildHasher, Hasher};

use foldhash::fast::RandomState;
use hashbrown::{HashMap, HashTable};

use crate::key::InputKey;

/// An opaque action handle carried inside [`Payload::Action`].
///
/// The router never inspects or dereferences an `ActionId` — it is
/// simply returned from [`Router::lookup`] and the caller maps it back
/// to whatever concrete action (enum variant, function pointer, closure
/// index, …) it represents. Callers are responsible for keeping the
/// id space consistent across builds.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ActionId(pub u32);

/// An index into the per-router label table returned by
/// [`Router::labels`].
///
/// A `LabelId` is produced by [`RouterBuilder::label`] and exposed on
/// matching entries via [`Entry::label`]. Resolve one to its string
/// with [`Router::label`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LabelId(pub u32);

const NO_LABEL: u32 = u32::MAX;
const CHORD_BIT: u32 = 0x8000_0000;

/// Sentinel [`InputKey`] stashed in `slots` to index per-layer ranges.
///
/// The tag byte (bits 40..48) of an [`InputKey`] must be 0 (`Char`) or 1
/// (`Named`); `u64::MAX` sets the tag byte to `0xFF`, so this value cannot
/// collide with any key a real event can produce.
const LAYER_ENTRIES_KEY: InputKey = InputKey(u64::MAX);

/// Identifier for a routing layer.
///
/// [`LayerId::BASE`] is the root layer every lookup starts from by
/// default. Callers mint additional layers with [`LayerId::new`] to
/// model modes or submodes. Values with the high bit set are reserved
/// for synthetic chord-prefix layers the builder allocates internally.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LayerId(u32);

impl LayerId {
    /// The root layer, used as the default starting point for lookups.
    pub const BASE: LayerId = LayerId(0);

    /// Returns a user-defined [`LayerId`] for `id`.
    ///
    /// The high bit is masked off because it is reserved for chord
    /// layers minted by the builder; as a result user ids share the
    /// low 31 bits (about 2 billion distinct values).
    ///
    /// # Examples
    ///
    /// ```
    /// use extui_bindings::LayerId;
    ///
    /// let insert = LayerId::new(1);
    /// assert!(!insert.is_chord());
    /// ```
    pub const fn new(id: u32) -> LayerId {
        LayerId(id & !CHORD_BIT)
    }

    /// Returns `true` if this layer was allocated by the builder as a
    /// chord-prefix scaffold rather than minted by the caller.
    pub const fn is_chord(self) -> bool {
        (self.0 & CHORD_BIT) != 0
    }

    /// Returns the raw `u32` representation, including the chord bit.
    pub const fn to_raw(self) -> u32 {
        self.0
    }
}

/// The effect a matched [`Entry`] should have.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Payload {
    /// Fires the given action. The caller typically resets its current
    /// layer to [`LayerId::BASE`] after dispatching.
    Action(ActionId),
    /// Transitions into the given layer. The caller sets its current
    /// layer to this value and continues waiting for the next key.
    Layer(LayerId),
}

#[derive(Clone, Copy, Debug)]
enum BuildPayload {
    Action(ActionId),
    Layer(LayerId),
    JustLabel,
    Unbound,
}

impl BuildPayload {
    fn is_only_label(self) -> bool {
        matches!(self, BuildPayload::JustLabel)
    }

    fn is_unbind(self) -> bool {
        matches!(self, BuildPayload::Unbound)
    }
}

/// A single binding stored in a frozen [`Router`].
///
/// Entries are produced by [`RouterBuilder`] and surfaced through
/// [`Router::lookup`], [`Router::layer_entries`], and
/// [`Router::entries`]. All fields are accessed through the methods
/// below; the struct carries no public fields.
#[derive(Clone, Debug)]
pub struct Entry {
    key: InputKey,
    layer: LayerId,
    filter: u32,
    label_index: u32,
    payload: Payload,
}

impl Entry {
    /// Returns the terminal key this entry matches.
    pub fn key(&self) -> InputKey {
        self.key
    }

    /// Returns the layer this entry is registered in.
    pub fn layer(&self) -> LayerId {
        self.layer
    }

    /// Returns the opaque filter value the caller stored on this entry.
    ///
    /// The router never interprets this value — it is returned verbatim
    /// so callers can match it against their own context.
    pub fn filter(&self) -> u32 {
        self.filter
    }

    /// Returns the payload to apply when this entry is selected.
    pub fn payload(&self) -> Payload {
        self.payload
    }

    /// Returns the entry's [`LabelId`], or [`None`] if no label was
    /// attached.
    pub fn label(&self) -> Option<LabelId> {
        if self.label_index == NO_LABEL {
            None
        } else {
            Some(LabelId(self.label_index))
        }
    }
}

/// A frozen, read-only routing table produced by [`RouterBuilder::build`].
pub struct Router {
    labels: Vec<String>,
    entries: Vec<Entry>,
    /// Maps `(layer, key)` to the `[lo, hi)` slice in `entries` holding every
    /// candidate for that pair. Within each slot entries are stored
    /// newest-first so callers can take the first one whose filter matches.
    slots: HashMap<(LayerId, InputKey), (u32, u32), RandomState>,
}

impl Router {
    /// Returns the candidate entries for `(layer, key)` in newest-first
    /// order, or an empty slice if nothing is bound.
    ///
    /// The caller scans the returned slice and picks the first entry
    /// whose [`Entry::filter`] its current context accepts.
    ///
    /// # Examples
    ///
    /// ```
    /// use extui::event::KeyModifiers;
    /// use extui_bindings::{ActionId, InputKey, LayerId, RouterBuilder};
    ///
    /// let j = InputKey::char('j', KeyModifiers::empty());
    /// let mut builder = RouterBuilder::new();
    /// builder.bind(LayerId::BASE, 0, &[j], ActionId(0));
    /// let router = builder.build();
    ///
    /// assert_eq!(router.lookup(LayerId::BASE, j).len(), 1);
    /// let unbound = InputKey::char('q', KeyModifiers::empty());
    /// assert!(router.lookup(LayerId::BASE, unbound).is_empty());
    /// ```
    #[inline]
    pub fn lookup(&self, layer: LayerId, key: InputKey) -> &[Entry] {
        match self.slots.get(&(layer, key)) {
            Some(&(lo, hi)) => &self.entries[lo as usize..hi as usize],
            None => &[],
        }
    }

    /// Returns every entry registered in `layer`, or an empty slice if
    /// the layer has no bindings.
    ///
    /// Entries are ordered by key; within each key the candidates are
    /// newest-first, matching the order of [`Router::lookup`] and
    /// [`Router::entries`].
    #[inline]
    pub fn layer_entries(&self, layer: LayerId) -> &[Entry] {
        match self.slots.get(&(layer, LAYER_ENTRIES_KEY)) {
            Some(&(lo, hi)) => &self.entries[lo as usize..hi as usize],
            None => &[],
        }
    }

    /// Returns the interned string for `id`.
    ///
    /// # Panics
    ///
    /// Panics if `id` was not produced by this router's builder.
    pub fn label(&self, id: LabelId) -> &str {
        self.labels[id.0 as usize].as_str()
    }

    /// Returns every interned label string, indexed by [`LabelId`].
    pub fn labels(&self) -> &[String] {
        &self.labels
    }

    /// Returns every binding in the router, sorted by `(layer, key)`
    /// with each slot stored newest-first.
    pub fn entries(&self) -> &[Entry] {
        &self.entries
    }
}

struct BuildEntry {
    key: InputKey,
    layer: LayerId,
    order: u32,
    parent_order: u32,
    filter: u32,
    label_index: u32,
    payload: BuildPayload,
}

/// A mutable builder that produces a frozen [`Router`].
///
/// Bindings are added with [`bind`], [`bind_layer`], and [`label`], and
/// removed with [`unbind`]. When the binding set is complete, [`build`]
/// consumes the builder and returns the immutable [`Router`].
///
/// # Examples
///
/// ```
/// use extui::event::KeyModifiers;
/// use extui_bindings::{ActionId, InputKey, LayerId, RouterBuilder};
///
/// let mut builder = RouterBuilder::new();
/// builder.bind(
///     LayerId::BASE,
///     0,
///     &[InputKey::char('h', KeyModifiers::empty())],
///     ActionId(0),
/// );
/// let router = builder.build();
/// assert_eq!(router.entries().len(), 1);
/// ```
///
/// [`bind`]: RouterBuilder::bind
/// [`bind_layer`]: RouterBuilder::bind_layer
/// [`label`]: RouterBuilder::label
/// [`unbind`]: RouterBuilder::unbind
/// [`build`]: RouterBuilder::build
pub struct RouterBuilder {
    labels: Vec<String>,
    entries: Vec<BuildEntry>,
    index: HashTable<u32>,
    hasher: RandomState,
    counter: u32,
    chord_layer: HashMap<(LayerId, InputKey), u32>,
}

impl Default for RouterBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl RouterBuilder {
    /// Returns an empty `RouterBuilder`.
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    /// Returns an empty `RouterBuilder` pre-allocated for at least
    /// `size` bindings.
    ///
    /// Use this when the approximate number of bindings is known up
    /// front to avoid repeated reallocation while adding them.
    pub fn with_capacity(size: usize) -> Self {
        Self {
            labels: Vec::new(),
            entries: Vec::with_capacity(size),
            index: HashTable::with_capacity(size),
            hasher: RandomState::default(),
            counter: 0,
            chord_layer: HashMap::new(),
        }
    }

    /// Binds the key sequence `input` starting at `layer` to fire
    /// `action` when the terminal key is pressed.
    ///
    /// Chord-prefix layers are allocated automatically for every
    /// non-terminal key in `input`. `filter` is stored verbatim on the
    /// resulting entry and returned by [`Entry::filter`] at lookup time;
    /// the router never interprets it. If a binding already exists at
    /// the same `(layer, key, filter)` it is replaced and re-ordered to
    /// the newest position.
    ///
    /// Passing an empty `input` is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// use extui::event::KeyModifiers;
    /// use extui_bindings::{ActionId, InputKey, LayerId, Payload, RouterBuilder, parse_sequence};
    ///
    /// let delete_word = ActionId(1);
    /// let mut builder = RouterBuilder::new();
    /// builder.bind(LayerId::BASE, 0, &parse_sequence("d w").unwrap(), delete_word);
    /// let router = builder.build();
    ///
    /// // Walk the chord: `d` transitions, `w` fires.
    /// let d = InputKey::char('d', KeyModifiers::empty());
    /// let Payload::Layer(inner) = router.lookup(LayerId::BASE, d)[0].payload() else {
    ///     panic!("expected chord transition");
    /// };
    /// let w = InputKey::char('w', KeyModifiers::empty());
    /// assert_eq!(router.lookup(inner, w)[0].payload(), Payload::Action(delete_word));
    /// ```
    pub fn bind(&mut self, layer: LayerId, filter: u32, input: &[InputKey], action: ActionId) {
        self.assign(layer, filter, input, BuildPayload::Action(action), NO_LABEL);
    }

    /// Binds `input` so that pressing its terminal key transitions the
    /// caller into the user-defined `target` layer.
    ///
    /// This is how modes and submodes are wired: for example, binding
    /// `<Esc>` in an `Insert` layer to [`LayerId::BASE`] returns the
    /// caller to Normal mode. Chord scaffolding for multi-key `input`
    /// is created as with [`RouterBuilder::bind`].
    pub fn bind_layer(&mut self, layer: LayerId, filter: u32, input: &[InputKey], target: LayerId) {
        self.assign(layer, filter, input, BuildPayload::Layer(target), NO_LABEL);
    }

    /// Attaches a human-readable `label` to the entry reached by
    /// walking `input` from `layer`.
    ///
    /// Any intermediate chord-prefix layers are created on demand. If a
    /// binding already exists at the destination its payload is left
    /// untouched and the label is added on top; otherwise a
    /// label-only stub is recorded and discarded at [`build`] time
    /// unless a later bind promotes it to a real action or layer.
    ///
    /// [`build`]: RouterBuilder::build
    pub fn label(
        &mut self,
        layer: LayerId,
        key: u32,
        input: &[InputKey],
        label: impl Into<String>,
    ) {
        let id = self.labels.len() as u32;
        self.labels.push(label.into());
        self.assign(layer, key, input, BuildPayload::JustLabel, id);
    }

    /// Removes the binding at `(layer, filter, input)`, if any.
    ///
    /// This is a no-op if no matching binding exists. Chord-prefix
    /// scaffolding introduced by previous binds is not pruned — intact
    /// prefixes remain until [`build`] drops those that no longer lead
    /// to any action.
    ///
    /// [`build`]: RouterBuilder::build
    pub fn unbind(&mut self, layer: LayerId, filter: u32, input: &[InputKey]) {
        self.assign(layer, filter, input, BuildPayload::Unbound, NO_LABEL);
    }

    /// Consumes the builder and returns the frozen [`Router`].
    ///
    /// Dead chord-prefix scaffolding (layers with no descendant action)
    /// and label-only stubs that were never bound are dropped at this
    /// point.
    pub fn build(mut self) -> Router {
        // Sort so each `(layer, key)` slot ends up newest-first — the order
        // callers iterate at lookup time.
        self.entries.sort_unstable_by(|a, b| {
            (a.layer, a.key)
                .cmp(&(b.layer, b.key))
                .then(b.order.cmp(&a.order))
        });
        let mut parent_has_children = vec![false; self.counter as usize];
        // perf: If we reverse the sort order, we could this with a single retain_mut instead of double scnae.
        for e in self.entries.iter_mut().rev() {
            match e.payload {
                BuildPayload::Action(_) => {
                    if let Some(has_children) = parent_has_children.get_mut(e.parent_order as usize)
                    {
                        *has_children = true;
                    }
                }
                BuildPayload::Layer(target) => {
                    // Chord-prefix layers are scaffolding: keep only if some
                    // descendant still reaches a terminal Action. User-defined
                    // targets (`bind_layer` to a `LayerId::new(..)`) are always
                    // explicit intent and stay; they still mark their parent so
                    // chord scaffolding that routes into them survives too.
                    let kept = !target.is_chord()
                        || parent_has_children.get(e.order as usize) == Some(&true);
                    if kept {
                        if let Some(has_children) =
                            parent_has_children.get_mut(e.parent_order as usize)
                        {
                            *has_children = true;
                        }
                    } else {
                        e.payload = BuildPayload::JustLabel;
                    }
                }
                BuildPayload::JustLabel => (),
                BuildPayload::Unbound => (),
            }
        }
        self.entries.retain(|e| match e.payload {
            BuildPayload::Layer(_) => true,
            BuildPayload::Action(_) => true,
            BuildPayload::JustLabel => false,
            BuildPayload::Unbound => false,
        });
        let entries: Vec<Entry> = self
            .entries
            .into_iter()
            .map(|e| Entry {
                key: e.key,
                layer: e.layer,
                filter: e.filter,
                label_index: e.label_index,
                payload: match e.payload {
                    BuildPayload::Action(a) => Payload::Action(a),
                    BuildPayload::Layer(l) => Payload::Layer(l),
                    BuildPayload::JustLabel | BuildPayload::Unbound => {
                        unreachable!("retained above")
                    }
                },
            })
            .collect();
        let slot_count = count_slots(&entries);
        let mut slots: HashMap<(LayerId, InputKey), (u32, u32), RandomState> =
            HashMap::with_capacity_and_hasher(slot_count, RandomState::default());
        let mut i = 0;
        while i < entries.len() {
            let layer_lo = i;
            let layer = entries[i].layer;
            while i < entries.len() && entries[i].layer == layer {
                let key_lo = i;
                let key = entries[i].key;
                i += 1;
                while i < entries.len() && entries[i].layer == layer && entries[i].key == key {
                    i += 1;
                }
                slots.insert((layer, key), (key_lo as u32, i as u32));
            }
            slots.insert((layer, LAYER_ENTRIES_KEY), (layer_lo as u32, i as u32));
        }
        Router {
            labels: self.labels,
            entries,
            slots,
        }
    }

    fn assign(
        &mut self,
        mut layer: LayerId,
        filter: u32,
        input: &[InputKey],
        payload: BuildPayload,
        label_index: u32,
    ) -> Option<usize> {
        let [chord @ .., terminal] = input else {
            return None;
        };

        let mut parent_order = u32::MAX;
        for &key in chord {
            let hash = triple_hash(&self.hasher, layer, filter, key);
            match index_entry(
                layer,
                filter,
                key,
                hash,
                &self.hasher,
                &self.entries,
                &mut self.index,
            ) {
                hashbrown::hash_table::Entry::Occupied(slot) => {
                    let idx = *slot.get() as usize;
                    let entry = &mut self.entries[idx];
                    if let BuildPayload::Layer(id) = entry.payload
                        && id.is_chord()
                    {
                        layer = id;

                        parent_order = entry.order;
                        continue;
                    }
                    if payload.is_unbind() {
                        return None;
                    }
                    let order = bump(&mut self.counter);
                    let new_layer = intern_chord_layer(layer, key, &mut self.chord_layer);
                    entry.order = order;
                    entry.payload = BuildPayload::Layer(new_layer);
                    layer = new_layer;
                    parent_order = order;
                }
                hashbrown::hash_table::Entry::Vacant(slot) => {
                    if payload.is_unbind() {
                        return None;
                    }
                    let order = bump(&mut self.counter);
                    let new_layer = intern_chord_layer(layer, key, &mut self.chord_layer);
                    let idx = self.entries.len() as u32;
                    self.entries.push(BuildEntry {
                        key,
                        layer,
                        order,
                        parent_order,
                        filter,
                        label_index: NO_LABEL,
                        payload: BuildPayload::Layer(new_layer),
                    });
                    parent_order = order;
                    slot.insert(idx);
                    layer = new_layer;
                }
            }
        }

        let hash = triple_hash(&self.hasher, layer, filter, *terminal);
        match index_entry(
            layer,
            filter,
            *terminal,
            hash,
            &self.hasher,
            &self.entries,
            &mut self.index,
        ) {
            hashbrown::hash_table::Entry::Occupied(slot) => {
                let idx = *slot.get() as usize;
                let entry = &mut self.entries[idx];
                if label_index != NO_LABEL {
                    entry.label_index = label_index;
                }
                if !payload.is_only_label() {
                    entry.order = bump(&mut self.counter);
                    entry.payload = payload;
                }
                Some(idx)
            }
            hashbrown::hash_table::Entry::Vacant(slot) => {
                if payload.is_unbind() {
                    return None;
                }
                let order = bump(&mut self.counter);
                let idx = self.entries.len() as u32;
                self.entries.push(BuildEntry {
                    key: *terminal,
                    parent_order,
                    layer,
                    order,
                    filter,
                    label_index,
                    payload,
                });
                slot.insert(idx);
                Some(idx as usize)
            }
        }
    }
}

fn count_slots(entries: &[Entry]) -> usize {
    let mut keyed = 0;
    let mut layers = 0;
    let mut prev_layer = LayerId(u32::MAX);
    let mut prev_key = InputKey(u64::MAX);
    for e in entries {
        if e.layer != prev_layer {
            layers += 1;
            prev_layer = e.layer;
            prev_key = InputKey(u64::MAX);
        }
        if e.key != prev_key {
            keyed += 1;
            prev_key = e.key;
        }
    }
    keyed + layers
}

fn bump(counter: &mut u32) -> u32 {
    let v = *counter;
    *counter = counter
        .checked_add(1)
        .expect("Maximum number of binds is u32::MAX");
    v
}

fn intern_chord_layer(
    layer: LayerId,
    key: InputKey,
    map: &mut HashMap<(LayerId, InputKey), u32>,
) -> LayerId {
    let v = map.len() as u32;
    let id = map.entry((layer, key)).or_insert_with(|| {
        if v >= CHORD_BIT {
            panic!("Maximum number of chord layers is {}", CHORD_BIT);
        }
        v
    });
    LayerId(*id | CHORD_BIT)
}

fn triple_hash(hasher: &RandomState, layer: LayerId, filter: u32, key: InputKey) -> u64 {
    let mut h = hasher.build_hasher();
    h.write_u32(layer.0);
    h.write_u32(filter);
    h.write_u64(key.0);
    h.finish()
}

fn index_entry<'a>(
    layer: LayerId,
    filter: u32,
    key: InputKey,
    hash: u64,
    hasher: &RandomState,
    entries: &[BuildEntry],
    index: &'a mut HashTable<u32>,
) -> hashbrown::hash_table::Entry<'a, u32> {
    index.entry(
        hash,
        |idx| {
            let entry = &entries[*idx as usize];
            entry.layer == layer && entry.filter == filter && entry.key == key
        },
        |idx| {
            let entry = &entries[*idx as usize];
            triple_hash(hasher, entry.layer, entry.filter, entry.key)
        },
    )
}

#[cfg(test)]
mod tests;
