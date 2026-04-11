use std::io::Write;

use extui::{
    AnsiColor,
    vt::{self, Modifier},
};

fn main() {
    let mut buf = Vec::with_capacity(100);
    buf.extend_from_slice(b"hello");
    vt::style(
        &mut buf,
        AnsiColor(34)
            .with_bg(AnsiColor(90))
            .with_modifier(Modifier::ITALIC),
        true,
    );
    buf.extend_from_slice(b"hello");
    println!("{}", buf.escape_ascii());
    let _ = std::io::stdout().write_all(&buf);
}
