use extui::event::polling::GlobalWakerConfig;
use extui::event::{self, Event, Events, KeyCode, KeyEvent};
use extui::{Color, DoubleBuffer, Style, Terminal, TerminalFlags};

fn main() -> std::io::Result<()> {
    // Setup signal handlers
    extui::event::polling::initialize_global_waker(GlobalWakerConfig {
        resize: true,
        ..Default::default()
    })?;

    let mut term = Terminal::open(
        TerminalFlags::RAW_MODE | TerminalFlags::ALT_SCREEN | TerminalFlags::HIDE_CURSOR,
    )?;

    let (w, h) = term.size()?;
    let mut buf = DoubleBuffer::new(w, h);
    let mut events = Events::default();
    let stdin = std::io::stdin();
    let mut last_key = None::<KeyEvent>;

    loop {
        // Render
        buf.rect().with(Color::Blue1.as_bg()).fill(&mut buf);
        let mut rect = buf.rect();
        rect.take_top(1)
            .with(Style::DEFAULT)
            .text(&mut buf, "Press 'q' to quit");

        if let Some(key) = last_key {
            rect.take_top(1)
                .with(Color::Black.with_fg(Color::White))
                .text(&mut buf, "Last Key Pressed: ")
                .fmt(&mut buf, format_args!("{key:?}"));
        }

        buf.render(&mut term);

        // Poll for events
        if event::poll(&stdin, None)?.is_readable() {
            events.read_from(&stdin)?;
        }
        while let Some(ev) = events.next(term.is_raw()) {
            match ev {
                Event::FocusGained => {}
                Event::FocusLost => {}
                Event::Key(key) => {
                    if key.code == KeyCode::Char('q') {
                        return Ok(());
                    }
                    last_key = Some(key);
                }
                Event::Mouse(_mouse_event) => {}
                Event::Resized => {
                    let (new_w, new_h) = term.size()?;
                    buf.resize(new_w, new_h);
                }
            }
        }
    }
}
