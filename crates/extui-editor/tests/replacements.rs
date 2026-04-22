//! Editor-level replacement tracking: the `set_track_replacements`
//! toggle accumulates per-edit replacements, which
//! `take_tracked_change` drains as a `TrackedChange` describing the
//! net change since the last drain.

#[allow(dead_code)]
mod common;

use common::parse_keys;
use extui_editor::{Editor, Replacement, TrackedChange};

fn feed(ed: &mut Editor, keys: &str) {
    for key in parse_keys(keys) {
        ed.send_key(&key);
    }
}

fn original_bytes_match(original: &str, rep: Replacement, final_text: &str) {
    let src_end = (rep.offset + rep.old_len) as usize;
    let final_end = (rep.offset + rep.new_len) as usize;
    assert_eq!(
        &original[..rep.offset as usize],
        &final_text[..rep.offset as usize],
        "prefix outside replaced range must be byte-identical",
    );
    assert_eq!(
        &original[src_end..],
        &final_text[final_end..],
        "suffix outside replaced range must be byte-identical",
    );
}

fn expect_merged(change: TrackedChange) -> Replacement {
    match change {
        TrackedChange::Merged(r) => r,
        other => panic!("expected TrackedChange::Merged, got {other:?}"),
    }
}

#[test]
fn first_drain_after_enabling_reports_reset() {
    let mut ed = Editor::new();
    ed.set_lines("hello");
    ed.set_track_replacements(true);
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    // Reset was one-shot — follow-up drain with no edits is None.
    assert_eq!(ed.take_tracked_change(), TrackedChange::None);
}

#[test]
fn tracking_disabled_by_default() {
    let mut ed = Editor::new();
    ed.set_lines("hello");
    feed(&mut ed, "iX");
    assert!(!ed.is_tracking_replacements());
    assert_eq!(ed.take_tracked_change(), TrackedChange::None);
}

#[test]
fn single_insert_is_reported_verbatim() {
    let mut ed = Editor::new();
    ed.set_lines("hello");
    ed.set_track_replacements(true);
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    feed(&mut ed, "iX<Esc>");
    assert_eq!(ed.text(), "Xhello");
    let rep = expect_merged(ed.take_tracked_change());
    assert_eq!(rep, Replacement::new(0, 0, 1));
    original_bytes_match("hello", rep, &ed.text());
}

#[test]
fn take_clears_accumulated_list() {
    let mut ed = Editor::new();
    ed.set_lines("hello");
    ed.set_track_replacements(true);
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    feed(&mut ed, "iX<Esc>");
    assert!(matches!(ed.take_tracked_change(), TrackedChange::Merged(_)));
    assert_eq!(
        ed.take_tracked_change(),
        TrackedChange::None,
        "second drain after a Merged must return None"
    );
}

#[test]
fn multiple_inserts_fold_into_single_replacement() {
    let mut ed = Editor::new();
    ed.set_lines("hello");
    ed.set_track_replacements(true);
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    // Type "abc" in insert mode at offset 0 → each char is one edit.
    feed(&mut ed, "iabc<Esc>");
    assert_eq!(ed.text(), "abchello");
    let rep = expect_merged(ed.take_tracked_change());
    assert_eq!(rep, Replacement::new(0, 0, 3));
    original_bytes_match("hello", rep, &ed.text());
}

#[test]
fn insert_then_delete_in_same_region_nets_correctly() {
    let mut ed = Editor::new();
    ed.set_lines("hello");
    ed.set_track_replacements(true);
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    // Insert "XY" at start, then backspace once: net insert "X".
    feed(&mut ed, "iXY<BS><Esc>");
    assert_eq!(ed.text(), "Xhello");
    let rep = expect_merged(ed.take_tracked_change());
    assert_eq!(rep, Replacement::new(0, 0, 1));
    original_bytes_match("hello", rep, &ed.text());
}

#[test]
fn disjoint_edits_bound_into_hull() {
    let mut ed = Editor::new();
    ed.set_lines("abcdef");
    ed.set_track_replacements(true);
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    // Insert 'X' at end of line, then 'Y' at start. Two disjoint edits.
    feed(&mut ed, "AX<Esc>gg0iY<Esc>");
    assert_eq!(ed.text(), "YabcdefX");
    let rep = expect_merged(ed.take_tracked_change());
    // Hull covers [0..6) replaced by 8 bytes ("YabcdefX").
    assert_eq!(rep, Replacement::new(0, 6, 8));
    original_bytes_match("abcdef", rep, &ed.text());
}

#[test]
fn disable_tracking_discards_pending_replacements() {
    let mut ed = Editor::new();
    ed.set_lines("hello");
    ed.set_track_replacements(true);
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    feed(&mut ed, "iX<Esc>");
    ed.set_track_replacements(false);
    // Disabled tracking — nothing to report.
    assert_eq!(ed.take_tracked_change(), TrackedChange::None);
    // Re-enabling primes again.
    ed.set_track_replacements(true);
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    assert_eq!(ed.take_tracked_change(), TrackedChange::None);
}

#[test]
fn set_track_replacements_is_idempotent() {
    let mut ed = Editor::new();
    ed.set_lines("hello");
    ed.set_track_replacements(true);
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    feed(&mut ed, "iX<Esc>");
    // Calling with the same value must NOT drop the accumulated edits.
    ed.set_track_replacements(true);
    let rep = expect_merged(ed.take_tracked_change());
    assert_eq!(rep, Replacement::new(0, 0, 1));
}

#[test]
fn set_lines_mid_run_signals_reset() {
    let mut ed = Editor::new();
    ed.set_lines("hello");
    ed.set_track_replacements(true);
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    feed(&mut ed, "iX<Esc>");
    ed.set_lines("fresh");
    // Reset takes priority over any pending merged replacement, and
    // drops the (now-stale) accumulated edits.
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    assert_eq!(ed.take_tracked_change(), TrackedChange::None);
}

#[test]
fn clear_mid_run_signals_reset() {
    let mut ed = Editor::new();
    ed.set_lines("hello");
    ed.set_track_replacements(true);
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    feed(&mut ed, "iX<Esc>");
    ed.clear();
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    assert_eq!(ed.take_tracked_change(), TrackedChange::None);
}

#[test]
fn reset_takes_priority_over_merged_between_drains() {
    let mut ed = Editor::new();
    ed.set_lines("hello");
    ed.set_track_replacements(true);
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    feed(&mut ed, "iX<Esc>");
    // Reset happens, then more edits stack on top.
    ed.set_lines("brand");
    feed(&mut ed, "iY<Esc>");
    // Both must collapse into a single Reset — the pre-set_lines edits
    // are incoherent against the new buffer.
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    // Subsequent edits after the drain should now report as Merged.
    feed(&mut ed, "AZ<Esc>");
    assert!(matches!(ed.take_tracked_change(), TrackedChange::Merged(_)));
}

#[test]
fn undo_is_recorded_as_further_replacements() {
    let mut ed = Editor::new();
    ed.set_lines("hello");
    ed.set_track_replacements(true);
    assert_eq!(ed.take_tracked_change(), TrackedChange::Reset);
    feed(&mut ed, "iXY<Esc>");
    assert_eq!(ed.text(), "XYhello");
    feed(&mut ed, "u");
    assert_eq!(ed.text(), "hello");
    let rep = expect_merged(ed.take_tracked_change());
    original_bytes_match("hello", rep, &ed.text());
    assert_eq!(rep.old_len, rep.new_len);
}
