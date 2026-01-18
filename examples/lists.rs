use extui::{
    BoxStyle, Color, DoubleBuffer, HAlign, Rect, Style, TerminalFlags,
    event::{Event, KeyCode, KeyEvent, KeyModifiers},
    widget::ScrollBar,
};

struct Item {
    name: String,
    value: u32,
}

struct App {
    items: Vec<Item>,
    cursor: ListCursor,
}
#[derive(Clone, Copy)]
struct ListCursor {
    offset: usize,
    selected: usize,
    list_length: usize,
}
impl ListCursor {
    // shifts the offset (if required) such that selection fits into view
    fn selected_into_view(&mut self, max_visible: usize) {
        if self.selected < self.offset {
            self.offset = self.selected;
        } else if self.selected >= self.offset + max_visible {
            self.offset = self.selected + 1 - max_visible;
        }
    }
    fn down(&mut self) {
        if self.selected + 1 < self.list_length {
            self.selected += 1;
        }
    }
    fn up(&mut self) {
        if self.selected > 0 {
            self.selected -= 1;
        }
    }
}

fn list<'a, T>(
    mut rect: Rect,
    buf: &mut DoubleBuffer,
    item_height: u16,
    cursor: &mut ListCursor,
    items: &'a [T],
    mut render: impl FnMut(Rect, &mut DoubleBuffer, &'a T, bool),
) {
    cursor.selected_into_view((rect.h / item_height) as usize);
    for (i, item) in items.iter().enumerate().skip(cursor.offset) {
        if rect.is_empty() {
            break;
        }
        render(
            rect.take_top(item_height as i32),
            buf,
            item,
            i == cursor.selected,
        );
    }
}

enum Action {
    Continue,
    Terminate,
}

fn render(app: &mut App, dst: &mut DoubleBuffer) {
    let mut rect = dst.rect();
    let mut list_rect = rect.take_top(10);
    list(
        BoxStyle::LIGHT.render(list_rect, dst),
        dst,
        1,
        &mut app.cursor,
        &app.items,
        |mut rect, buf, item, selected| {
            let bg = if selected {
                Color(242).as_bg()
            } else {
                Style::default()
            };
            rect.take_right(5)
                .with(Color(4).as_fg() | bg)
                .fill(buf)
                .fmt(buf, item.value);
            rect.with(bg).fill(buf).text(buf, &item.name);
        },
    );
    ScrollBar {
        total: app.items.len() as u32,
        offset: app.cursor.offset as u32,
    }
    .render_thick(list_rect.take_right(1), dst);
    list_rect.take_top(1).display().skip(1).text(dst, "[title]");
    list_rect.take_bottom(1).with(HAlign::Right).fmt(
        dst,
        format_args!(
            "[{}/{}]",
            app.cursor.selected,
            app.items.len().saturating_sub(1)
        ),
    );
}

fn process_key(app: &mut App, key: KeyEvent) -> Action {
    match key.code {
        KeyCode::Char(ch) if key.modifiers.is_empty() => match ch {
            'j' => app.cursor.down(),
            'k' => app.cursor.up(),
            _ => (),
        },
        KeyCode::Char(ch) if key.modifiers == KeyModifiers::CONTROL => match ch {
            'c' => return Action::Terminate,
            _ => (),
        },
        _ => (),
    }

    Action::Continue
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mode = TerminalFlags::RAW_MODE
        | TerminalFlags::MOUSE_CAPTURE
        | TerminalFlags::ALT_SCREEN
        | TerminalFlags::HIDE_CURSOR
        | TerminalFlags::EXTENDED_KEYBOARD_INPUTS;

    let mut terminal = extui::Terminal::open(mode).expect("Valid TTY");
    let mut events = extui::event::Events::default();
    let (w, h) = terminal.size()?;
    let mut buffer = DoubleBuffer::new(w, h);
    let stdin = std::io::stdin();
    // setup app state
    let mut app = App {
        items: (0..30)
            .map(|i| Item {
                name: format!("Item {}", i),
                value: i,
            })
            .collect(),
        cursor: ListCursor {
            offset: 0,
            selected: 0,
            list_length: 30,
        },
    };

    loop {
        render(&mut app, &mut buffer);
        buffer.render(&mut terminal);

        if extui::event::poll(&stdin, None)?.is_readable() {
            events.read_from(&stdin)?;
        }
        while let Some(event) = events.next(terminal.is_raw()) {
            match event {
                Event::Key(key) => match process_key(&mut app, key) {
                    Action::Continue => continue,
                    Action::Terminate => return Ok(()),
                },
                Event::Resized => {
                    let (new_w, new_h) = terminal.size()?;
                    buffer.resize(new_w, new_h);
                }
                _ => (),
            }
        }
    }
}
