use jsony_bench::{Bencher, Stat};
use ratatui::Terminal;
use ratatui::backend::{Backend, ClearType, CrosstermBackend, WindowSize};
use ratatui::buffer::Cell;
use ratatui::layout::{Position, Rect, Size};
use ratatui::style::{Color, Modifier, Style};
use std::cell::RefCell;
use std::io;

use crate::common::{BenchRecord, find_baseline, print_row};
use crate::render::{self, World};

pub const GROUP: &str = "ratatui";

const STATUSES: &[(Color, Color, &str)] = &[
    (Color::Indexed(10), Color::Indexed(230), " Todo "),
    (Color::Red, Color::Indexed(230), " Blck "),
    (Color::Indexed(6), Color::Indexed(230), " Hold "),
    (Color::Blue, Color::Indexed(230), " Prog "),
    (Color::Yellow, Color::Black, " Revw "),
    (Color::Green, Color::Indexed(230), " Done "),
    (Color::Indexed(4), Color::Indexed(230), " Cxld "),
];

struct CaptureBackend {
    inner: CrosstermBackend<Vec<u8>>,
    size: Size,
    cursor: Position,
}

impl CaptureBackend {
    fn new(width: u16, height: u16) -> Self {
        Self {
            inner: CrosstermBackend::new(Vec::new()),
            size: Size { width, height },
            cursor: Position::ORIGIN,
        }
    }

    fn byte_len(&mut self) -> u64 {
        self.inner.writer_mut().len() as u64
    }

    fn clear_bytes(&mut self) {
        self.inner.writer_mut().clear();
    }
}

impl Backend for CaptureBackend {
    type Error = io::Error;

    fn draw<'a, I>(&mut self, content: I) -> io::Result<()>
    where
        I: Iterator<Item = (u16, u16, &'a Cell)>,
    {
        self.inner.draw(content)
    }

    fn hide_cursor(&mut self) -> io::Result<()> {
        self.inner.hide_cursor()
    }

    fn show_cursor(&mut self) -> io::Result<()> {
        self.inner.show_cursor()
    }

    fn get_cursor_position(&mut self) -> io::Result<Position> {
        Ok(self.cursor)
    }

    fn set_cursor_position<P: Into<Position>>(&mut self, position: P) -> io::Result<()> {
        self.cursor = position.into();
        self.inner.set_cursor_position(self.cursor)
    }

    fn clear(&mut self) -> io::Result<()> {
        self.clear_region(ClearType::All)
    }

    fn clear_region(&mut self, clear_type: ClearType) -> io::Result<()> {
        self.inner.clear_region(clear_type)
    }

    fn size(&self) -> io::Result<Size> {
        Ok(self.size)
    }

    fn window_size(&mut self) -> io::Result<WindowSize> {
        Ok(WindowSize {
            columns_rows: self.size,
            pixels: Size::ZERO,
        })
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

pub struct Scenario {
    pub name: &'static str,
    pub force_full: bool,
    pub run: fn(&mut ratatui::Frame<'_>, &mut World, u64),
}

pub const SCENARIOS: &[Scenario] = &[
    Scenario {
        name: "first_paint",
        force_full: true,
        run: first_paint,
    },
    Scenario {
        name: "idempotent",
        force_full: false,
        run: idempotent,
    },
    Scenario {
        name: "cursor_move",
        force_full: false,
        run: cursor_move,
    },
    Scenario {
        name: "status_tick",
        force_full: false,
        run: status_tick,
    },
    Scenario {
        name: "log_tail",
        force_full: false,
        run: log_tail,
    },
    Scenario {
        name: "full_repaint",
        force_full: false,
        run: full_repaint,
    },
];

fn first_paint(frame: &mut ratatui::Frame<'_>, world: &mut World, _frame_no: u64) {
    draw(frame, world);
}

fn idempotent(frame: &mut ratatui::Frame<'_>, world: &mut World, _frame_no: u64) {
    draw(frame, world);
}

fn cursor_move(frame: &mut ratatui::Frame<'_>, world: &mut World, frame_no: u64) {
    world.cursor = (frame_no as usize) % world.tasks.len();
    draw(frame, world);
}

fn status_tick(frame: &mut ratatui::Frame<'_>, world: &mut World, frame_no: u64) {
    world.tick = frame_no;
    draw(frame, world);
}

fn log_tail(frame: &mut ratatui::Frame<'_>, world: &mut World, _frame_no: u64) {
    world.scroll = world.scroll.wrapping_add(1);
    draw_log(frame, world);
}

fn full_repaint(frame: &mut ratatui::Frame<'_>, world: &mut World, _frame_no: u64) {
    world.tasks.rotate_left(1);
    for task in &mut world.tasks {
        task.status = (task.status + 1) % STATUSES.len() as u8;
    }
    draw(frame, world);
}

fn draw(frame: &mut ratatui::Frame<'_>, world: &World) {
    let mut full = frame.area();
    if full.width < 30 || full.height < 5 {
        return;
    }

    let status_bar = take_bottom(&mut full, 1);
    let list_area = if world.panel && full.width > 60 {
        let panel_width = full.width * 2 / 5;
        let panel = take_right(&mut full, panel_width);
        draw_panel(frame.buffer_mut(), panel);
        full
    } else {
        full
    };

    let inner = draw_box(frame.buffer_mut(), list_area);
    draw_list(frame.buffer_mut(), inner, world);
    draw_status_bar(frame.buffer_mut(), status_bar, world);
}

fn draw_list(buf: &mut ratatui::buffer::Buffer, mut area: Rect, world: &World) {
    let title = take_top(&mut area, 1);
    write_centered(
        buf,
        title,
        " Tasks ",
        Style::new()
            .fg(Color::Indexed(230))
            .add_modifier(Modifier::BOLD),
    );

    let visible = (area.height as usize).min(world.tasks.len().saturating_sub(world.scroll));
    for vi in 0..visible {
        let ti = world.scroll + vi;
        let mut row = take_top(&mut area, 1);
        let task = world.tasks[ti];
        let is_cursor = ti == world.cursor;
        let (bg, fg, label) = STATUSES[task.status as usize % STATUSES.len()];
        let badge_style = Style::new().bg(bg).fg(fg);
        let name_style = if is_cursor {
            Style::new()
                .bg(Color::Indexed(5))
                .fg(Color::Indexed(230))
                .add_modifier(Modifier::BOLD)
        } else {
            Style::new().fg(Color::Indexed(24))
        };
        let time_style = Style::new()
            .fg(Color::Indexed(248))
            .add_modifier(Modifier::UNDERLINED);

        let badge = take_left(&mut row, label.len() as u16);
        write_text(buf, badge, 0, label, badge_style);
        let _gap = take_left(&mut row, 1);
        fill(buf, row, name_style);
        write_text(buf, row, 1, task.name, name_style);
        write_right(buf, row, &format!("{:>4}m ", task.minutes), time_style);
    }
}

fn draw_panel(buf: &mut ratatui::buffer::Buffer, area: Rect) {
    let mut inner = draw_box(buf, area);
    let title = take_top(&mut inner, 1);
    write_centered(buf, title, " Details ", Style::new().fg(Color::LightBlue));
    let rows = (inner.height as usize).min(16);
    for i in 0..rows {
        let row = take_top(&mut inner, 1);
        let color = if i % 2 == 0 {
            Color::Indexed(18)
        } else {
            Color::Indexed(22)
        };
        write_text(buf, row, 1, "* metadata value", Style::new().fg(color));
        write_right(
            buf,
            row,
            &format!("{:>3}%", (i * 7) % 100),
            Style::new().fg(Color::Indexed(14)),
        );
    }
}

fn draw_status_bar(buf: &mut ratatui::buffer::Buffer, area: Rect, world: &World) {
    let style = Style::new().bg(Color::Indexed(6)).fg(Color::Indexed(230));
    fill(buf, area, style);
    let running = world.tasks.iter().filter(|task| task.status == 3).count();
    let done = world.tasks.iter().filter(|task| task.status == 5).count();
    write_text(
        buf,
        area,
        1,
        &format!("R:{running} D:{done} tick={:06}", world.tick),
        style,
    );
    write_right(
        buf,
        area,
        &format!("[{}/{}] ", world.cursor + 1, world.tasks.len()),
        style,
    );
}

fn draw_log(frame: &mut ratatui::Frame<'_>, world: &World) {
    let mut area = frame.area();
    let len = world.tasks.len();
    for row_i in 0..area.height as usize {
        let task = world.tasks[(world.scroll + row_i) % len];
        let row = take_top(&mut area, 1);
        let lvl_style = match task.status % 4 {
            0 => Style::new().fg(Color::Indexed(20)),
            1 => Style::new().fg(Color::Yellow),
            2 => Style::new().fg(Color::LightRed),
            _ => Style::new().fg(Color::Green),
        };
        let lvl = match task.status % 4 {
            0 => "DEBUG",
            1 => "WARN ",
            2 => "ERROR",
            _ => "INFO ",
        };
        let buf = frame.buffer_mut();
        write_text(
            buf,
            row,
            0,
            &format!("[{:08}] ", task.id),
            Style::new().fg(Color::Indexed(12)),
        );
        write_text(buf, row, 11, lvl, lvl_style);
        write_text(buf, row, 17, task.name, Style::new().fg(Color::Indexed(24)));
        write_right(
            buf,
            row,
            &format!("(+{}ms) ", task.minutes),
            Style::new().fg(Color::Indexed(14)),
        );
    }
}

fn draw_box(buf: &mut ratatui::buffer::Buffer, area: Rect) -> Rect {
    if area.width < 2 || area.height < 2 {
        return area;
    }
    let style = Style::new();
    let right = area.x + area.width - 1;
    let bottom = area.y + area.height - 1;
    set_symbol(buf, area.x, area.y, "┌", style);
    set_symbol(buf, right, area.y, "┐", style);
    set_symbol(buf, area.x, bottom, "└", style);
    set_symbol(buf, right, bottom, "┘", style);
    for x in area.x + 1..right {
        set_symbol(buf, x, area.y, "─", style);
        set_symbol(buf, x, bottom, "─", style);
    }
    for y in area.y + 1..bottom {
        set_symbol(buf, area.x, y, "│", style);
        set_symbol(buf, right, y, "│", style);
    }
    Rect {
        x: area.x + 1,
        y: area.y + 1,
        width: area.width - 2,
        height: area.height - 2,
    }
}

fn fill(buf: &mut ratatui::buffer::Buffer, area: Rect, style: Style) {
    for y in area.y..area.y + area.height {
        for x in area.x..area.x + area.width {
            set_symbol(buf, x, y, " ", style);
        }
    }
}

fn write_text(
    buf: &mut ratatui::buffer::Buffer,
    area: Rect,
    offset: u16,
    text: &str,
    style: Style,
) {
    if offset >= area.width {
        return;
    }
    buf.set_stringn(
        area.x + offset,
        area.y,
        text,
        (area.width - offset) as usize,
        style,
    );
}

fn write_right(buf: &mut ratatui::buffer::Buffer, area: Rect, text: &str, style: Style) {
    let width = text.len().min(area.width as usize) as u16;
    let x = area.x + area.width.saturating_sub(width);
    buf.set_stringn(x, area.y, text, width as usize, style);
}

fn write_centered(buf: &mut ratatui::buffer::Buffer, area: Rect, text: &str, style: Style) {
    let width = text.len().min(area.width as usize) as u16;
    let x = area.x + area.width.saturating_sub(width) / 2;
    buf.set_stringn(x, area.y, text, width as usize, style);
}

fn set_symbol(buf: &mut ratatui::buffer::Buffer, x: u16, y: u16, symbol: &str, style: Style) {
    buf[(x, y)].set_symbol(symbol).set_style(style);
}

fn take_top(area: &mut Rect, height: u16) -> Rect {
    let height = height.min(area.height);
    let taken = Rect { height, ..*area };
    area.y += height;
    area.height -= height;
    taken
}

fn take_bottom(area: &mut Rect, height: u16) -> Rect {
    let height = height.min(area.height);
    area.height -= height;
    Rect {
        y: area.y + area.height,
        height,
        ..*area
    }
}

fn take_left(area: &mut Rect, width: u16) -> Rect {
    let width = width.min(area.width);
    let taken = Rect { width, ..*area };
    area.x += width;
    area.width -= width;
    taken
}

fn take_right(area: &mut Rect, width: u16) -> Rect {
    let width = width.min(area.width);
    area.width -= width;
    Rect {
        x: area.x + area.width,
        width,
        ..*area
    }
}

struct State {
    terminal: Terminal<CaptureBackend>,
    world: World,
    frame: u64,
}

fn draw_once(state: &mut State, scenario: &Scenario, frame_no: u64) {
    if scenario.force_full {
        state.terminal.clear().unwrap();
    }
    state
        .terminal
        .draw(|frame| (scenario.run)(frame, &mut state.world, frame_no))
        .unwrap();
}

fn measure_bytes(scenario: &Scenario, w: u16, h: u16) -> u64 {
    let mut state = State {
        terminal: Terminal::new(CaptureBackend::new(w, h)).unwrap(),
        world: World::new(256),
        frame: 0,
    };
    draw_once(&mut state, scenario, 0);
    state.terminal.backend_mut().clear_bytes();
    draw_once(&mut state, scenario, 1);
    state.terminal.backend_mut().byte_len()
}

fn run_scenario(bencher: &mut Bencher, scenario: &Scenario, w: u16, h: u16) -> Stat {
    let state = RefCell::new(State {
        terminal: Terminal::new(CaptureBackend::new(w, h)).unwrap(),
        world: World::new(256),
        frame: 0,
    });
    if !scenario.force_full {
        let mut s = state.borrow_mut();
        draw_once(&mut s, scenario, 0);
        s.terminal.backend_mut().clear_bytes();
    }
    bencher.bench_with_generator(
        || {
            let mut s = state.borrow_mut();
            s.frame = s.frame.wrapping_add(1);
            s.frame
        },
        |frame_no| {
            let mut s = state.borrow_mut();
            draw_once(&mut s, scenario, frame_no);
            let bytes = s.terminal.backend_mut().byte_len();
            std::hint::black_box(bytes);
            s.terminal.backend_mut().clear_bytes();
        },
    )
}

pub fn list() -> Vec<(&'static str, &'static str)> {
    SCENARIOS
        .iter()
        .map(|scenario| (GROUP, scenario.name))
        .collect()
}

pub fn run_matching(
    bencher: &mut Bencher,
    matches: &dyn Fn(&str, &str) -> bool,
    records: &mut Vec<BenchRecord>,
    baseline: Option<&[BenchRecord]>,
) {
    for &(size_name, w, h) in render::SIZES {
        let mut size_any = false;
        for scenario in SCENARIOS {
            if !matches(GROUP, scenario.name) {
                continue;
            }
            let stat = run_scenario(bencher, scenario, w, h);
            let bytes = measure_bytes(scenario, w, h);
            let record = BenchRecord {
                group: GROUP.to_string(),
                size: size_name.to_string(),
                width: w,
                height: h,
                scenario: scenario.name.to_string(),
                ns: f64::from(stat.nanos),
                cycles: f64::from(stat.cycles),
                inst: f64::from(stat.inst),
                branch: f64::from(stat.branch),
                bytes,
            };
            let base = find_baseline(baseline, &record);
            print_row(&record, base);
            records.push(record);
            size_any = true;
        }
        if size_any {
            println!();
        }
    }
}
