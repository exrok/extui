use std::io::Write;

use extui::{
    Color,
    vt::{self, Modifier},
};

fn main() {
    let mut buf = Vec::with_capacity(100);
    buf.extend_from_slice(b"hello");
    vt::style(
        &mut buf,
        Color(34).with_bg(Color(90)).with_modifier(Modifier::ITALIC),
        true,
    );
    buf.extend_from_slice(b"hello");
    println!("{}", buf.escape_ascii());
    let _ = std::io::stdout().write_all(&buf);
}
