use extui::{AnsiColor, BoxStyle, DoubleBuffer, HAlign, Rect, Style, vt::Modifier};

pub const SIZES: &[(&str, u16, u16)] =
    &[("80x24", 80, 24), ("120x40", 120, 40), ("200x60", 200, 60)];

const STATUSES: &[(AnsiColor, AnsiColor, &str)] = &[
    (AnsiColor::Grey[10], AnsiColor::Cornsilk1, " Todo "),
    (AnsiColor::DarkRed, AnsiColor::Cornsilk1, " Blck "),
    (AnsiColor::Grey[6], AnsiColor::Cornsilk1, " Hold "),
    (AnsiColor::Blue1, AnsiColor::Cornsilk1, " Prog "),
    (AnsiColor::Yellow3, AnsiColor::Black, " Revw "),
    (AnsiColor::Green3, AnsiColor::Cornsilk1, " Done "),
    (AnsiColor::Grey[4], AnsiColor::Cornsilk1, " Cxld "),
];

const TASK_NAMES: &[&str] = &[
    "implement grapheme cluster renderer",
    "fix bug in auth middleware layer",
    "write documentation for render_diff",
    "cleanup old palette fast path code",
    "refactor database migration runner",
    "update dependencies to latest stable",
    "add unit tests for set_stringn path",
    "profile hot loop in render_internal",
    "review pull request #342 from alice",
    "deploy v0.2 to staging environment",
    "migrate config schema for RGB colors",
    "investigate flaky scroll_up test case",
    "reduce allocations in bench harness",
    "replace panic with Result in sys mod",
    "optimize the cell equality check path",
    "draft RFC for larger grapheme buffers",
];

const PALETTE_UNDERLINE: u8 = 0;

pub fn setup(db: &mut DoubleBuffer) {
    db.set_palette(PALETTE_UNDERLINE, b"\x1b[4:3m".to_vec());
}

#[derive(Clone, Copy)]
pub struct TaskRow {
    pub status: u8,
    pub name: &'static str,
    pub minutes: u32,
    pub id: u32,
}

#[derive(Clone)]
pub struct World {
    pub tasks: Vec<TaskRow>,
    pub cursor: usize,
    pub scroll: usize,
    pub tick: u64,
    pub panel: bool,
}

impl World {
    pub fn new(n_tasks: usize) -> Self {
        let mut tasks = Vec::with_capacity(n_tasks);
        for i in 0..n_tasks {
            tasks.push(TaskRow {
                status: (i % STATUSES.len()) as u8,
                name: TASK_NAMES[i % TASK_NAMES.len()],
                minutes: ((i * 37 + 12) % 360) as u32,
                id: i as u32,
            });
        }
        Self {
            tasks,
            cursor: 0,
            scroll: 0,
            tick: 0,
            panel: false,
        }
    }
}

pub fn draw(db: &mut DoubleBuffer, world: &World) {
    let mut full = db.rect();
    if full.w < 30 || full.h < 5 {
        return;
    }
    let status_bar = full.take_bottom(1i32);
    let list_area = if world.panel && full.w > 60 {
        let panel = full.take_right((full.w as i32) * 2 / 5);
        draw_panel(db, panel);
        full
    } else {
        full
    };
    let inner = BoxStyle::LIGHT.render(list_area, db);
    draw_list(db, inner, world);
    draw_status_bar(db, status_bar, world);
}

fn draw_list(db: &mut DoubleBuffer, mut area: Rect, world: &World) {
    area.take_top(1i32)
        .with(
            Style::DEFAULT
                .with_fg(AnsiColor::Cornsilk1)
                .with_modifier(Modifier::BOLD),
        )
        .with(HAlign::Center)
        .text(db, " Tasks ");

    let visible = (area.h as usize).min(world.tasks.len().saturating_sub(world.scroll));
    for vi in 0..visible {
        let ti = world.scroll + vi;
        let mut row_rect = area.take_top(1i32);
        let t = world.tasks[ti];
        let is_cursor = ti == world.cursor;
        let (bg, fg, label) = STATUSES[t.status as usize % STATUSES.len()];
        let badge_style = Style::DEFAULT.with_bg(bg).with_fg(fg);
        let name_style = if is_cursor {
            Style::DEFAULT
                .with_bg(AnsiColor::Grey[5])
                .with_fg(AnsiColor::Cornsilk1)
                .with_modifier(Modifier::BOLD)
        } else {
            Style::DEFAULT.with_fg(AnsiColor::Grey[24])
        };
        let time_style = Style::palette(PALETTE_UNDERLINE).with_fg(AnsiColor(248));

        let badge_rect = row_rect.take_left(label.len() as i32);
        badge_rect.with(badge_style).text(db, label);
        let _gap = row_rect.take_left(1i32);
        row_rect
            .with(name_style)
            .fill(db)
            .skip(1)
            .text(db, t.name)
            .with(HAlign::Right)
            .with(time_style)
            .fmt(db, format_args!("{:>4}m ", t.minutes));
    }
}

fn draw_panel(db: &mut DoubleBuffer, area: Rect) {
    let mut inner = BoxStyle::LIGHT.render(area, db);
    inner
        .take_top(1i32)
        .with(AnsiColor::LightSkyBlue1.as_fg())
        .with(HAlign::Center)
        .text(db, " Details ");
    let rows = (inner.h as usize).min(16);
    for i in 0..rows {
        let row = inner.take_top(1i32);
        let c = if i % 2 == 0 {
            AnsiColor::Grey[18]
        } else {
            AnsiColor::Grey[22]
        };
        row.with(c.as_fg())
            .skip(1)
            .text(db, "• metadata value")
            .with(HAlign::Right)
            .with(AnsiColor::Grey[14].as_fg())
            .fmt(db, format_args!("{:>3}%", (i * 7) % 100));
    }
}

fn draw_status_bar(db: &mut DoubleBuffer, area: Rect, world: &World) {
    let style = Style::DEFAULT
        .with_bg(AnsiColor::Grey[6])
        .with_fg(AnsiColor::Cornsilk1);
    let running = world.tasks.iter().filter(|t| t.status == 3).count();
    let done = world.tasks.iter().filter(|t| t.status == 5).count();
    area.with(style)
        .fill(db)
        .skip(1)
        .fmt(
            db,
            format_args!("R:{} D:{} tick={:06}", running, done, world.tick),
        )
        .with(HAlign::Right)
        .fmt(
            db,
            format_args!("[{}/{}] ", world.cursor + 1, world.tasks.len()),
        );
}

pub fn draw_log(db: &mut DoubleBuffer, world: &World) {
    let mut area = db.rect();
    let visible = area.h as usize;
    let len = world.tasks.len();
    for row_i in 0..visible {
        let t = world.tasks[(world.scroll + row_i) % len];
        let row = area.take_top(1i32);
        let lvl_style = match t.status % 4 {
            0 => Style::DEFAULT.with_fg(AnsiColor::Grey[20]),
            1 => Style::DEFAULT.with_fg(AnsiColor::Yellow3),
            2 => Style::DEFAULT.with_fg(AnsiColor::Red1),
            _ => Style::DEFAULT.with_fg(AnsiColor::Green3),
        };
        let lvl = match t.status % 4 {
            0 => "DEBUG",
            1 => "WARN ",
            2 => "ERROR",
            _ => "INFO ",
        };
        row.with(Style::DEFAULT.with_fg(AnsiColor::Grey[12]))
            .fmt(db, format_args!("[{:08}] ", t.id))
            .with(lvl_style)
            .text(db, lvl)
            .skip(1)
            .with(Style::DEFAULT.with_fg(AnsiColor::Grey[24]))
            .text(db, t.name)
            .with(HAlign::Right)
            .with(Style::DEFAULT.with_fg(AnsiColor::Grey[14]))
            .fmt(db, format_args!("(+{}ms) ", t.minutes));
    }
}

pub struct Scenario {
    pub name: &'static str,
    pub reset_each_frame: bool,
    pub run: fn(&mut DoubleBuffer, &mut World, u64),
}

pub const SCENARIOS: &[Scenario] = &[
    Scenario {
        name: "first_paint",
        reset_each_frame: true,
        run: first_paint,
    },
    Scenario {
        name: "idempotent",
        reset_each_frame: false,
        run: idempotent,
    },
    Scenario {
        name: "cursor_move",
        reset_each_frame: false,
        run: cursor_move,
    },
    Scenario {
        name: "status_tick",
        reset_each_frame: false,
        run: status_tick,
    },
    Scenario {
        name: "log_tail",
        reset_each_frame: false,
        run: log_tail,
    },
    Scenario {
        name: "panel_toggle",
        reset_each_frame: false,
        run: panel_toggle,
    },
    Scenario {
        name: "full_repaint",
        reset_each_frame: false,
        run: full_repaint,
    },
];

fn first_paint(db: &mut DoubleBuffer, world: &mut World, _frame: u64) {
    db.reset();
    draw(db, world);
}

fn idempotent(db: &mut DoubleBuffer, world: &mut World, _frame: u64) {
    draw(db, world);
}

fn cursor_move(db: &mut DoubleBuffer, world: &mut World, frame: u64) {
    world.cursor = (frame as usize) % world.tasks.len();
    draw(db, world);
}

fn status_tick(db: &mut DoubleBuffer, world: &mut World, frame: u64) {
    world.tick = frame;
    draw(db, world);
}

fn log_tail(db: &mut DoubleBuffer, world: &mut World, _frame: u64) {
    world.scroll = world.scroll.wrapping_add(1);
    db.scroll(1);
    draw_log(db, world);
}

fn panel_toggle(db: &mut DoubleBuffer, world: &mut World, frame: u64) {
    world.panel = frame % 2 == 0;
    draw(db, world);
}

fn full_repaint(db: &mut DoubleBuffer, world: &mut World, _frame: u64) {
    world.tasks.rotate_left(1);
    for t in &mut world.tasks {
        t.status = (t.status + 1) % STATUSES.len() as u8;
    }
    draw(db, world);
}
