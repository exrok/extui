//! Smoke tests that verify the nvim oracle harness itself is working
//! before the larger differential suites rely on it.

mod common;

use common::{assert_matches_nvim, parse_keys};
use extui::event::KeyCode;

#[test]
fn parse_plain_text() {
    let evs = parse_keys("ab");
    assert_eq!(evs.len(), 2);
    assert!(matches!(evs[0].code, KeyCode::Char('a')));
    assert!(matches!(evs[1].code, KeyCode::Char('b')));
}

#[test]
fn parse_escape_tag() {
    let evs = parse_keys("<Esc>");
    assert_eq!(evs.len(), 1);
    assert!(matches!(evs[0].code, KeyCode::Esc));
}

#[test]
fn parse_ctrl_tag() {
    let evs = parse_keys("<C-r>");
    assert_eq!(evs.len(), 1);
    assert!(matches!(evs[0].code, KeyCode::Char('r')));
    assert!(
        evs[0]
            .modifiers
            .contains(extui::event::KeyModifiers::CONTROL)
    );
}

#[test]
fn parse_less_than_tag() {
    let evs = parse_keys("<lt>");
    assert_eq!(evs.len(), 1);
    assert!(matches!(evs[0].code, KeyCode::Char('<')));
}

#[test]
fn oracle_roundtrip_empty() {
    // Minimal smoke: empty buffer with no keys produces empty text.
    assert_matches_nvim("", "");
}

#[test]
fn oracle_roundtrip_hello() {
    assert_matches_nvim("", "ihello<Esc>");
}
