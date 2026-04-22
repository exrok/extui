//! Differential tests: undo / redo vs real nvim.

mod common;

use common::assert_matches_nvim;

const LINES3: &str = "hello world\nfoo bar\nquick fox";

#[test]
fn undo_insert() {
    assert_matches_nvim(LINES3, "iXXX<Esc>u");
}
#[test]
fn undo_dd() {
    assert_matches_nvim(LINES3, "ddu");
}
#[test]
fn undo_dw() {
    assert_matches_nvim(LINES3, "dwu");
}
#[test]
fn undo_cc() {
    assert_matches_nvim(LINES3, "ccXYZ<Esc>u");
}
#[test]
fn undo_o_and_redo() {
    assert_matches_nvim(LINES3, "oNEW<Esc>u<C-r>");
}
#[test]
fn undo_chain() {
    assert_matches_nvim(LINES3, "ddddu");
}
#[test]
fn redo_after_undo() {
    assert_matches_nvim(LINES3, "iXYZ<Esc>u<C-r>");
}
#[test]
fn edit_after_undo_clears_redo() {
    assert_matches_nvim(LINES3, "iXYZ<Esc>uiABC<Esc><C-r>");
}
#[test]
fn undo_x() {
    assert_matches_nvim(LINES3, "xu");
}
#[test]
fn undo_r() {
    assert_matches_nvim(LINES3, "rZu");
}
#[test]
fn undo_big_j() {
    assert_matches_nvim(LINES3, "Ju");
}
#[test]
fn undo_p() {
    assert_matches_nvim(LINES3, "ddpu");
}
