//! Differential tests: normal-mode motions vs real nvim.

mod common;

use common::assert_matches_nvim;

const LINES3: &str = "hello world\nfoo bar baz\nquick fox";
const LINES5: &str = "alpha\nbeta\ngamma\ndelta\nepsilon";

#[test]
fn h_noop_at_col_0() {
    assert_matches_nvim(LINES3, "h");
}
#[test]
fn l_one() {
    assert_matches_nvim(LINES3, "l");
}
#[test]
fn l_count() {
    assert_matches_nvim(LINES3, "5l");
}
#[test]
fn l_past_eol_stops() {
    assert_matches_nvim(LINES3, "100l");
}

#[test]
fn j_one() {
    assert_matches_nvim(LINES3, "j");
}
#[test]
fn k_after_j() {
    assert_matches_nvim(LINES3, "jk");
}
#[test]
fn j_past_last_line() {
    assert_matches_nvim(LINES3, "20j");
}

#[test]
fn zero_to_line_start() {
    assert_matches_nvim(LINES3, "lll0");
}
#[test]
fn dollar_to_eol() {
    assert_matches_nvim(LINES3, "$");
}
#[test]
fn dollar_count_lines() {
    assert_matches_nvim(LINES3, "2$");
}

#[test]
fn w_forward() {
    assert_matches_nvim(LINES3, "w");
}
#[test]
fn w_count() {
    assert_matches_nvim(LINES3, "3w");
}
#[test]
fn w_across_lines() {
    assert_matches_nvim(LINES3, "5w");
}
#[test]
fn b_backward() {
    assert_matches_nvim(LINES3, "$b");
}
#[test]
fn e_to_end_of_word() {
    assert_matches_nvim(LINES3, "e");
}
#[test]
fn e_count() {
    assert_matches_nvim(LINES3, "3e");
}

#[test]
fn gg_to_first_line() {
    assert_matches_nvim(LINES5, "Ggg");
}
#[test]
fn big_g_to_last_line() {
    assert_matches_nvim(LINES5, "G");
}
#[test]
fn g_count() {
    assert_matches_nvim(LINES5, "3G");
}

// Count-prefixed motions.
#[test]
fn count_w_then_b() {
    assert_matches_nvim(LINES3, "3w2b");
}
#[test]
fn count_0_does_nothing() {
    assert_matches_nvim(LINES3, "w0");
}

// Empty-line word motions — verify empty line counts as a word.
#[test]
fn w_across_empty_line() {
    assert_matches_nvim("foo\n\nbar", "w");
}

// Word transitions between keyword and punctuation classes.
#[test]
fn w_class_transitions() {
    assert_matches_nvim("foo, bar", "w");
}

// Interaction with short lines — j/k preserves desired display column.
#[test]
fn j_preserves_desired_column() {
    // Start at col 8 on line 0, j to short line (line 1), j again to line 2
    // — cursor should return to col 8 on line 2 if possible.
    assert_matches_nvim("aaaaaaaaaaaa\nbb\ncccccccccc", "8ljj");
}
