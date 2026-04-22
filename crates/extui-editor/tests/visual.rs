//! Differential tests: visual modes vs real nvim.

mod common;

use common::assert_matches_nvim;

const LINES3: &str = "hello world\nfoo bar\nquick fox";

// Visual charwise
#[test]
fn v_delete() {
    assert_matches_nvim(LINES3, "vlld");
}
#[test]
fn v_yank() {
    assert_matches_nvim(LINES3, "vlly");
}
#[test]
fn v_then_escape() {
    assert_matches_nvim(LINES3, "vll<Esc>x");
}
#[test]
fn v_change() {
    assert_matches_nvim(LINES3, "vllcX<Esc>");
}
#[test]
fn v_across_lines() {
    assert_matches_nvim(LINES3, "vjld");
}

// Visual linewise
#[test]
fn big_v_delete() {
    assert_matches_nvim(LINES3, "Vd");
}
#[test]
fn big_v_yank() {
    assert_matches_nvim(LINES3, "Vy");
}
#[test]
fn big_v_multi_line_delete() {
    assert_matches_nvim(LINES3, "Vjd");
}
#[test]
fn big_v_change() {
    assert_matches_nvim(LINES3, "VcX<Esc>");
}

// Visual block
#[test]
fn block_delete_rect() {
    assert_matches_nvim("abcd\nefgh\nijkl", "<C-v>jld");
}
#[test]
fn block_yank() {
    assert_matches_nvim("abcd\nefgh\nijkl", "<C-v>jly");
}
#[test]
fn block_skips_short_lines() {
    assert_matches_nvim("hello\nhi\nhelper", "<C-v>jjld");
}
#[test]
fn block_change_replays_insert() {
    assert_matches_nvim("abcd\nefgh\nijkl", "<C-v>jlcXY<Esc>");
}
#[test]
fn block_change_replays_insert_on_short_lines() {
    assert_matches_nvim("hello\nhi\nhelper", "<C-v>jjlcZ<Esc>");
}

// Visual paste
#[test]
fn v_delete_then_paste() {
    assert_matches_nvim(LINES3, "vllydP");
}

// Visual escape and re-enter
#[test]
fn visual_toggle() {
    assert_matches_nvim(LINES3, "vllvx");
}
