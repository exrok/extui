# extui-editor

A configurable text editor widget for
[`extui`](https://github.com/exrok/extui). Embed it into any
`Buffer` region; the host owns the terminal and event loop.

```rust
use extui::{Buffer, Rect, Terminal, TerminalFlags};
use extui::event::{Event, Events};
use extui_editor::Editor;

let mut term = Terminal::open(TerminalFlags::RAW_MODE | TerminalFlags::ALT_SCREEN)?;
let (w, h) = term.size()?;
let mut buf = Buffer::new(w, h);

let mut editor = Editor::new();
editor.set_height_bounds(2, 10);   // min 2 rows, max 10; beyond max → scroll
// editor.set_single_line(true);   // optional: force a one-line buffer/widget
editor.resize(w);

loop {
    // Compose: the editor gets whatever Rect the host gives it. The
    // host can query desired_height() to make the widget grow inline.
    let widget_h = editor.desired_height();
    let rect = Rect { x: 0, y: 0, w, h: widget_h };
    editor.render(rect, &mut buf);
    buf.render(&mut term);

    // Forward input:
    // while let Some(Event::Key(k)) = events.next(term.is_raw()) {
    //     editor.send_key(&k);
    // }
}
```

Run the demo:

```
cargo run -p extui-editor --example inline
```

(Theme demo with built-in presets, including Tokyo Night:)

```
cargo run -p extui-editor --example themes
```

(Press `Ctrl+Q` to exit.)
