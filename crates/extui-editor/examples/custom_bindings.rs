//! Minimal example showing how to build editor bindings directly with
//! `extui-bindings` using the typed router wrapper.

use std::{io, str::FromStr};

use extui_bindings::{LayerId, parse_sequence};
use extui_editor::{
    Editor, Mode,
    bindings::{
        EditorAction, EditorBindings, EditorRouterBuilder, Filter, MODE_INSERT, MODE_NORMAL,
    },
};

fn main() -> io::Result<()> {
    let normal_bindings = [
        ("h", "motion.left"),
        ("l", "motion.right"),
        ("i", "enter-insert"),
    ];
    let insert_bindings = [
        ("Esc", "exit-insert"),
        ("Enter", "insert-newline"),
        ("Tab", "insert-tab"),
        ("Backspace", "backspace-delete"),
    ];

    let mut builder = EditorRouterBuilder::new();

    apply_bindings(&mut builder, MODE_NORMAL, &normal_bindings);
    apply_bindings(&mut builder, MODE_INSERT, &insert_bindings);

    let bindings =
        EditorBindings::new(builder.build(), Mode::Normal).expect("valid editor bindings");

    let mut editor = Editor::with_bindings(bindings);
    editor.set_lines("configured through extui-bindings");
    Ok(())
}

fn apply_bindings(builder: &mut EditorRouterBuilder, mode_bit: u32, bindings: &[(&str, &str)]) {
    let filter = Filter::new(mode_bit);
    for &(keys, action_id) in bindings {
        let action = EditorAction::from_str(action_id).expect("known action id");
        let path = parse_sequence(keys).expect("valid key binding");
        builder.bind(LayerId::BASE, filter, &path, action);
    }
}
