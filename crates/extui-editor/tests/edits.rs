//! Differential tests: normal-mode edits vs real nvim.

mod common;

use common::{assert_matches_nvim, parse_keys};
use extui_editor::Editor;

const LINES3: &str = "hello world\nfoo bar\nquick fox";
const WORD_PUNCT: &str = "foo,  bar baz";
const DOUBLE_QUOTED: &str = "\"alpha beta\" tail";
const SINGLE_QUOTED: &str = "'alpha beta' tail";
const BACKTICK_QUOTED: &str = "`alpha beta` tail";
const PARENS: &str = "(alpha beta) tail";
const BRACKETS: &str = "[alpha beta] tail";
const BRACES: &str = "{alpha beta} tail";
const ANGLES: &str = "<alpha beta> tail";
const NESTED_PARENS: &str = "(alpha (beta) gamma)";

// -------- Single-stroke --------

#[test]
fn x_delete_char() {
    assert_matches_nvim(LINES3, "x");
}
#[test]
fn x_count() {
    assert_matches_nvim(LINES3, "3x");
}
#[test]
fn x_at_eol() {
    assert_matches_nvim(LINES3, "$x");
}
#[test]
fn r_replace_char() {
    assert_matches_nvim(LINES3, "rZ");
}
#[test]
fn s_substitute() {
    assert_matches_nvim(LINES3, "sX<Esc>");
}
#[test]
fn big_s_substitute_line() {
    assert_matches_nvim(LINES3, "SX<Esc>");
}
#[test]
fn big_j_join_lines() {
    assert_matches_nvim(LINES3, "J");
}
#[test]
fn big_j_count() {
    assert_matches_nvim(LINES3, "3J");
}
#[test]
fn big_j_trims_next_line_indent() {
    assert_matches_nvim("hello \n  world", "J");
}
#[test]
fn big_j_across_blank_line_count() {
    assert_matches_nvim("one\n\nthree", "3J");
}

// -------- Insert entries --------

#[test]
fn i_insert_at_cursor() {
    assert_matches_nvim(LINES3, "iX<Esc>");
}
#[test]
fn a_insert_after_cursor() {
    assert_matches_nvim(LINES3, "aX<Esc>");
}
#[test]
fn big_i_first_nonblank() {
    assert_matches_nvim("  hello", "IX<Esc>");
}
#[test]
fn big_a_insert_at_eol() {
    assert_matches_nvim(LINES3, "AX<Esc>");
}
#[test]
fn o_open_below() {
    assert_matches_nvim(LINES3, "oX<Esc>");
}
#[test]
fn big_o_open_above() {
    assert_matches_nvim(LINES3, "OX<Esc>");
}

// Multi-char insertion + backspace.
#[test]
fn type_and_backspace() {
    assert_matches_nvim("", "ihello<BS><BS>p<Esc>");
}
#[test]
fn insert_newline() {
    assert_matches_nvim("foo", "iX<CR>Y<Esc>");
}

#[test]
fn shift_enter_insert_newline() {
    let mut ed = Editor::new();
    ed.resize(80);
    ed.set_lines("foo");
    for key in parse_keys("iX<S-Enter>Y<Esc>") {
        ed.send_key(&key);
    }
    assert_eq!(ed.text(), "X\nYfoo");
}

// -------- Tab handling (must match configured tab expansion) --------

#[test]
fn insert_tab_between_chars() {
    assert_matches_nvim("", "is<Tab>s<Esc>");
}
#[test]
fn insert_tab_at_bol() {
    assert_matches_nvim("", "i<Tab>abc<Esc>");
}
#[test]
fn cursor_after_tab_past_stop() {
    // After typing 9 chars + <Tab>, four spaces place `X` at column 13.
    assert_matches_nvim("", "iabcdefghi<Tab>X<Esc>");
}
#[test]
fn motion_l_across_tab() {
    assert_matches_nvim("a\tb", "ll");
}
#[test]
fn motion_dollar_with_tab() {
    assert_matches_nvim("a\tb", "$");
}

// -------- Operators with motions --------

#[test]
fn dd_delete_line() {
    assert_matches_nvim(LINES3, "dd");
}
#[test]
fn dd_count() {
    assert_matches_nvim(LINES3, "2dd");
}
#[test]
fn dw_delete_word() {
    assert_matches_nvim(LINES3, "dw");
}
#[test]
fn dollar_delete() {
    assert_matches_nvim(LINES3, "d$");
}
#[test]
fn d_0() {
    assert_matches_nvim(LINES3, "lld0");
}
#[test]
fn d_gg() {
    assert_matches_nvim(LINES3, "jdgg");
}
#[test]
fn d_big_g() {
    assert_matches_nvim(LINES3, "jdG");
}
#[test]
fn dj() {
    assert_matches_nvim(LINES3, "dj");
}
#[test]
fn yw() {
    assert_matches_nvim(LINES3, "yw");
}
#[test]
fn yy_then_p() {
    assert_matches_nvim(LINES3, "yyp");
}
#[test]
fn yy_then_big_p() {
    assert_matches_nvim(LINES3, "yyP");
}
#[test]
fn cw_then_insert() {
    assert_matches_nvim(LINES3, "cwX<Esc>");
}
#[test]
fn ciw_then_insert() {
    assert_matches_nvim(LINES3, "llciwX<Esc>");
}
#[test]
fn caw_then_insert() {
    assert_matches_nvim("foo  bar baz", "lllcawX<Esc>");
}
#[test]
fn diw_deletes_current_word() {
    assert_matches_nvim(LINES3, "lldiw");
}
#[test]
fn daw_deletes_word_and_space() {
    assert_matches_nvim("foo  bar baz", "daw");
}
#[test]
fn yiw_then_put() {
    assert_matches_nvim(LINES3, "llyiwP");
}
#[test]
fn ciw_on_whitespace_run() {
    assert_matches_nvim("foo  bar baz", "lllciwX<Esc>");
}
#[test]
fn ciw_on_punctuation_run() {
    assert_matches_nvim("foo, bar", "lllciwX<Esc>");
}
#[test]
fn di_big_word() {
    assert_matches_nvim(WORD_PUNCT, "diW");
}
#[test]
fn da_big_word() {
    assert_matches_nvim(WORD_PUNCT, "daW");
}
#[test]
fn ci_double_quote() {
    assert_matches_nvim(DOUBLE_QUOTED, "lci\"X<Esc>");
}
#[test]
fn ca_double_quote() {
    assert_matches_nvim(DOUBLE_QUOTED, "lca\"X<Esc>");
}
#[test]
fn ci_single_quote() {
    assert_matches_nvim(SINGLE_QUOTED, "lci'X<Esc>");
}
#[test]
fn ci_backtick_quote() {
    assert_matches_nvim(BACKTICK_QUOTED, "lci`X<Esc>");
}
#[test]
fn ci_paren() {
    assert_matches_nvim(PARENS, "lci(X<Esc>");
}
#[test]
fn ci_paren_alias_close() {
    assert_matches_nvim(PARENS, "lci)X<Esc>");
}
#[test]
fn da_paren_alias_b() {
    assert_matches_nvim(PARENS, "dab");
}
#[test]
fn di_bracket() {
    assert_matches_nvim(BRACKETS, "ldi[");
}
#[test]
fn da_brace_alias_b_big() {
    assert_matches_nvim(BRACES, "daB");
}
#[test]
fn ci_angle() {
    assert_matches_nvim(ANGLES, "lci<lt>X<Esc>");
}
#[test]
fn nested_paren_count_selects_outer() {
    assert_matches_nvim(NESTED_PARENS, "wwl2di(");
}
#[test]
fn cc_line() {
    assert_matches_nvim(LINES3, "ccX<Esc>");
}

// -------- Paste with count --------

#[test]
fn p_with_count() {
    assert_matches_nvim(LINES3, "dd2p");
}

// -------- D / C / Y (eol variants) --------

#[test]
fn big_d_deletes_to_eol() {
    assert_matches_nvim(LINES3, "llD");
}
#[test]
fn big_c_changes_to_eol() {
    assert_matches_nvim(LINES3, "llCX<Esc>");
}
#[test]
fn big_y_yanks_line() {
    assert_matches_nvim(LINES3, "Yp");
}

// -------- ^ first non-blank --------

#[test]
fn caret_to_first_nonblank() {
    assert_matches_nvim("    hello", "$^");
}
#[test]
fn d_caret() {
    assert_matches_nvim("    hello", "$d^");
}

// -------- find char --------

#[test]
fn f_forward() {
    assert_matches_nvim("hello world", "fo");
}
#[test]
fn big_f_backward() {
    assert_matches_nvim("hello world", "$Fo");
}
#[test]
fn t_forward() {
    assert_matches_nvim("hello world", "to");
}
#[test]
fn df_inclusive() {
    assert_matches_nvim("hello world", "dfl");
}
#[test]
fn dt_exclusive() {
    assert_matches_nvim("hello world", "dtl");
}

// -------- paragraph motions & text objects --------

#[test]
fn paragraph_forward() {
    assert_matches_nvim("one\ntwo\n\nthree", "}");
}
#[test]
fn paragraph_back() {
    assert_matches_nvim("one\n\ntwo\nthree", "G{");
}
#[test]
fn dip_paragraph() {
    assert_matches_nvim("one\ntwo\n\nthree", "dip");
}
#[test]
fn dap_paragraph() {
    assert_matches_nvim("one\ntwo\n\nthree", "dap");
}

// -------- case operators --------

#[test]
fn gu_motion() {
    assert_matches_nvim("HELLO WORLD", "guw");
}
#[test]
fn big_gu_motion() {
    assert_matches_nvim("hello world", "gU$");
}
#[test]
fn g_tilde_motion() {
    assert_matches_nvim("Hello World", "g~$");
}
#[test]
fn guu_line() {
    assert_matches_nvim("HELLO\nWORLD", "guu");
}
#[test]
fn big_gg_uu_line() {
    assert_matches_nvim("hello\nworld", "gUU");
}
#[test]
fn tilde_toggles_char() {
    assert_matches_nvim("abc", "~");
}

// -------- increment/decrement --------

#[test]
fn ctrl_a_increments() {
    assert_matches_nvim("x = 41", "<C-a>");
}
#[test]
fn ctrl_x_decrements() {
    assert_matches_nvim("x = 10", "<C-x>");
}
#[test]
fn ctrl_a_with_count() {
    assert_matches_nvim("x = 10", "5<C-a>");
}

// -------- marks --------
// Note: the shared nvim oracle retains mark state across tests, so
// cross-test coupling keeps differential tests limited here. Marks are
// exercised directly in the editor unit tests.

#[test]
fn mark_jump_backtick() {
    assert_matches_nvim("hello world", "llmb$`b");
}

// -------- Count+operator --------

#[test]
fn count_dd() {
    assert_matches_nvim(LINES3, "3dd");
}
#[test]
fn count_dw() {
    assert_matches_nvim(LINES3, "2dw");
}
#[test]
fn count_ciw() {
    assert_matches_nvim("foo bar baz", "2ciwX<Esc>");
}
#[test]
fn count_caw() {
    assert_matches_nvim("foo  bar baz", "lll2cawX<Esc>");
}
