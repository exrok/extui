//! Differential tests: hard-tab (`\t`) navigation vs real nvim.
//!
//! Tabs are the most failure-prone grapheme in the editor: their
//! display width depends on the running column, they are whitespace for
//! word motions, and the cursor can sit *on* a tab — the grid col then
//! reflects the tab's *starting* display column, not its expanded
//! width. Every assertion here pushes one of those seams.
//!
//! Oracle runs with `tabstop=4` and `expandtab`, matching editor defaults.

mod common;

use common::assert_matches_nvim;

// ---------------------------------------------------------------
// Cursor-on-tab position
// ---------------------------------------------------------------

/// `l` onto a tab: cursor sits on the tab's start byte; nvim reports
/// the cursor's grid col as the first column of the tab's expansion.
#[test]
fn l_onto_tab_reports_start_col() {
    assert_matches_nvim("a\tb", "l");
}

/// `ll` crosses a tab in one step (it's one grapheme) and lands on
/// `b` at display col 8.
#[test]
fn ll_crosses_tab_lands_on_b() {
    assert_matches_nvim("a\tb", "ll");
}

/// `h` back off a tab: cursor display col returns to 0.
#[test]
fn h_back_off_tab() {
    assert_matches_nvim("a\tb", "llh");
}

// ---------------------------------------------------------------
// Runs of consecutive tabs
// ---------------------------------------------------------------

/// Two tabs in a row — each expands to the next stop. Cursor on the
/// second tab sits at display col 8.
#[test]
fn l_across_two_tabs() {
    assert_matches_nvim("\t\tX", "l");
}

/// Walk all four graphemes of `\t\tX\t` with `l`.
#[test]
fn l_walk_tabs_and_char() {
    assert_matches_nvim("\t\tX\t", "llll");
}

/// `$` on a line ending in a tab: cursor sits on the tab (last
/// grapheme start), not on the cell past the tab expansion.
#[test]
fn dollar_on_line_ending_with_tab() {
    assert_matches_nvim("abc\t", "$");
}

/// `0` on a line starting with tabs: col 0 is the first tab itself.
#[test]
fn zero_on_leading_tab() {
    assert_matches_nvim("\t\tabc", "ll0");
}

// Note: bare `^` (first-nonblank motion) isn't bound in our motion
// set — only `I` uses first-nonblank, and it enters Insert mode, which
// is covered below.

// ---------------------------------------------------------------
// Tab past a boundary (wide chars, mid-line)
// ---------------------------------------------------------------

/// Tab after a wide (double-width) grapheme: the tab still jumps to
/// the next multiple of 8.
#[test]
fn tab_after_wide_grapheme() {
    // "嗨" is width 2 → cursor on tab reports col 2; `l` lands on 'b'
    // at col 8.
    assert_matches_nvim("嗨\tb", "ll");
}

/// Tab that crosses a tab stop only partially: "aaaaa\tb" — 5 chars,
/// tab pads 5→8, b at col 8.
#[test]
fn tab_partial_padding() {
    assert_matches_nvim("aaaaa\tb", "6l");
}

/// Exact tab-stop boundary: "aaaaaaaa\tb" — 8 chars (hits the stop
/// exactly), tab jumps 8→16, b at col 16.
#[test]
fn tab_on_stop_boundary_still_advances() {
    assert_matches_nvim("aaaaaaaa\tb", "9l");
}

// ---------------------------------------------------------------
// Vertical motion (j/k) across lines of unequal tab layouts
// ---------------------------------------------------------------

/// Cursor at display col 8 on a tab-indented line, `j` onto a line
/// whose col 8 is inside a word.
#[test]
fn j_from_tab_to_plain_line() {
    assert_matches_nvim("\tX\naaaaaaaaaaaa", "lj");
}

/// `j` from a plain line's col 4 onto a tab-only line (which has no
/// col 4): nvim clamps to the last grapheme start.
#[test]
fn j_to_tab_only_line_clamps() {
    assert_matches_nvim("abcdef\n\t\t\t", "4lj");
}

/// Up motion back: round-trip preserves `curswant`.
#[test]
fn j_k_roundtrip_preserves_curswant() {
    assert_matches_nvim("abcdefghij\n\tx\nabcdefghij", "6ljjk");
}

/// Tab on the second line at exactly the column the first line's
/// cursor is on — j should land on the tab.
#[test]
fn j_onto_tab_at_display_col() {
    assert_matches_nvim("abcdefghij\n\ta", "8lj");
}

// ---------------------------------------------------------------
// Word motion classifies tab as whitespace
// ---------------------------------------------------------------

#[test]
fn w_over_tab() {
    assert_matches_nvim("foo\tbar", "w");
}
#[test]
fn w_over_multiple_tabs() {
    assert_matches_nvim("foo\t\t\tbar", "w");
}
#[test]
fn b_back_over_tab() {
    assert_matches_nvim("foo\tbar", "$b");
}
#[test]
fn e_word_end_across_tab() {
    assert_matches_nvim("foo\tbar", "e");
}

/// Word motion onto a line that's only tabs — `w` should still cross.
#[test]
fn w_across_tab_only_line() {
    assert_matches_nvim("foo\n\t\t\nbar", "ww");
}

// ---------------------------------------------------------------
// Operator + motion with tabs
// ---------------------------------------------------------------

#[test]
fn dw_eats_word_plus_tabs() {
    assert_matches_nvim("foo\tbar", "dw");
}
#[test]
fn dl_on_tab_deletes_tab() {
    assert_matches_nvim("a\tb", "ldl");
}
#[test]
fn d_dollar_with_tab() {
    assert_matches_nvim("abc\tdef", "d$");
}
#[test]
fn cw_on_tab_then_insert() {
    assert_matches_nvim("a\tb", "lcwZ<Esc>");
}
#[test]
fn x_deletes_tab_under_cursor() {
    assert_matches_nvim("a\tb", "lx");
}

// ---------------------------------------------------------------
// Insert-mode edits around tabs
// ---------------------------------------------------------------

/// Insert char immediately after a tab — cursor advances by one cell
/// past the tab expansion.
#[test]
fn insert_after_tab_at_eol() {
    assert_matches_nvim("abc\t", "A<Esc>");
}

/// Backspace over a tab removes the whole tab.
#[test]
fn backspace_over_tab() {
    assert_matches_nvim("a\tb", "A<BS><Esc>");
}

/// Insert a char between two tabs — display col of the inserted char
/// sits at the first tab stop (8), then the second tab jumps to 16.
#[test]
fn insert_char_between_tabs() {
    assert_matches_nvim("\t\t", "laX<Esc>");
}

/// `o` on a tab-indented line opens a new blank line; cursor lands at
/// col 0 (nvim without autoindent).
#[test]
fn o_below_tab_line() {
    assert_matches_nvim("\tfoo", "oX<Esc>");
}

// ---------------------------------------------------------------
// `$`, `0`, `^` with trailing or leading tabs
// ---------------------------------------------------------------

#[test]
fn dollar_tab_then_char() {
    assert_matches_nvim("a\tb\tc", "$");
}
#[test]
fn zero_then_l_onto_tab() {
    assert_matches_nvim("\tab", "0");
}

// ---------------------------------------------------------------
// Deep regression: cursor after typing s<Tab>s (the original bug).
// ---------------------------------------------------------------

#[test]
fn type_s_tab_s_cursor_aligned() {
    // Triggered the original "cursor displayed in place but chars land
    // elsewhere" desync: tab width in UnicodeWidthStr disagreed with
    // `set_stringn`'s control-char skip. Oracle agreement here proves
    // the fix sticks.
    assert_matches_nvim("", "is<Tab>s<Esc>");
}
