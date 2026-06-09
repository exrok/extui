//! Compare direct `nvim` terminal output with the extui embed host output.
//!
//! This drives both sides through Alacritty's PTY and terminal emulator stack,
//! then writes raw ANSI captures plus an HTML diff so color/style mismatches are
//! visible without relying on ad-hoc parsers.
//!
//! Usage:
//! `cargo run -p extui-neovim --example compare -- [options] -- [nvim-args...]`

use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::io::{self, Read, Write};
use std::mem;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{Arc, Mutex};
use std::thread::sleep;
use std::time::{Duration, Instant};

use alacritty_terminal::event::{Event as AlacrittyEvent, EventListener, WindowSize};
use alacritty_terminal::index::{Column, Line};
use alacritty_terminal::term::cell::{Cell as TermCell, Flags};
use alacritty_terminal::term::color::Colors;
use alacritty_terminal::term::test::TermSize;
use alacritty_terminal::term::{Config as TermConfig, Term};
use alacritty_terminal::tty::{self, EventedReadWrite, Options, Shell};
use alacritty_terminal::vte::ansi::{Color, NamedColor, Processor, Rgb};
use extui::event::polling::{GlobalWakerConfig, initialize_global_waker};
use extui::vt::{BufferWrite, HIDE_CURSOR, MoveCursor, SHOW_CURSOR};
use extui::{Buffer, Rect, vt};
use extui_neovim::{NeovimEmbed, NeovimWaker};

const DEFAULT_COLS: u16 = 80;
const DEFAULT_ROWS: u16 = 24;
const DEFAULT_IDLE_MS: u64 = 300;
const DEFAULT_TIMEOUT_MS: u64 = 8_000;
const READ_CHUNK: usize = 16 * 1024;
const CELL_WIDTH_PX: u16 = 8;
const CELL_HEIGHT_PX: u16 = 16;

#[derive(Debug)]
struct Cli {
    cols: u16,
    rows: u16,
    idle: Duration,
    timeout: Duration,
    out_dir: PathBuf,
    out_explicit: bool,
    cwd: PathBuf,
    env: HashMap<String, String>,
    nvim_bin: String,
    term: String,
    colorterm: Option<String>,
    inherit_env: bool,
    nvim_args: Vec<String>,
}

impl Default for Cli {
    fn default() -> Self {
        Self {
            cols: DEFAULT_COLS,
            rows: DEFAULT_ROWS,
            idle: Duration::from_millis(DEFAULT_IDLE_MS),
            timeout: Duration::from_millis(DEFAULT_TIMEOUT_MS),
            cwd: std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")),
            out_dir: std::env::current_dir()
                .unwrap_or_else(|_| PathBuf::from("."))
                .join("target/extui-neovim-compare"),
            out_explicit: false,
            env: HashMap::new(),
            nvim_bin: "nvim".to_owned(),
            term: std::env::var("TERM").unwrap_or_else(|_| "xterm-256color".to_owned()),
            colorterm: std::env::var("COLORTERM").ok().filter(|s| !s.is_empty()),
            inherit_env: true,
            nvim_args: Vec::new(),
        }
    }
}

#[derive(Clone)]
struct CaptureListener {
    events: Arc<Mutex<Vec<AlacrittyEvent>>>,
}

impl EventListener for CaptureListener {
    fn send_event(&self, event: AlacrittyEvent) {
        if let Ok(mut events) = self.events.lock() {
            events.push(event);
        }
    }
}

struct VirtualTerminal {
    term: Term<CaptureListener>,
    parser: Processor,
    events: Arc<Mutex<Vec<AlacrittyEvent>>>,
    window_size: WindowSize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct SnapshotCell {
    text: String,
    fg: Rgb,
    bg: Rgb,
    underline: Option<Rgb>,
    flags: u16,
}

#[derive(Clone, Debug)]
struct ScreenSnapshot {
    cols: u16,
    rows: u16,
    cells: Vec<SnapshotCell>,
}

#[derive(Clone, Debug)]
struct Capture {
    raw: Vec<u8>,
    env: BTreeMap<String, String>,
    snapshot: ScreenSnapshot,
}

#[derive(Clone, Debug)]
struct CellDiff {
    x: u16,
    y: u16,
    direct: SnapshotCell,
    embed: SnapshotCell,
}

fn main() -> io::Result<()> {
    let Some(mut cli) = parse_args()? else {
        return Ok(());
    };
    cli.out_dir = resolve_out_dir(&cli)?;

    initialize_global_waker(GlobalWakerConfig::default())?;
    fs::create_dir_all(&cli.out_dir)?;

    let direct = run_direct(&cli)?;
    let embed = run_embed(&cli)?;
    let diffs = diff_snapshots(&direct.snapshot, &embed.snapshot);

    write_artifacts(&cli.out_dir, &direct, &embed, &diffs)?;

    println!(
        "mismatches: {} / {}",
        diffs.len(),
        direct.snapshot.cells.len()
    );
    println!("artifact dir: {}", cli.out_dir.display());
    println!("html diff: {}", cli.out_dir.join("diff.html").display());

    Ok(())
}

fn parse_args() -> io::Result<Option<Cli>> {
    let mut cli = Cli::default();
    let mut args = std::env::args().skip(1);

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--help" | "-h" => {
                print_usage();
                return Ok(None);
            }
            "--cols" => cli.cols = parse_u16("--cols", args.next())?,
            "--rows" => cli.rows = parse_u16("--rows", args.next())?,
            "--idle-ms" => cli.idle = Duration::from_millis(parse_u64("--idle-ms", args.next())?),
            "--timeout-ms" => {
                cli.timeout = Duration::from_millis(parse_u64("--timeout-ms", args.next())?)
            }
            "--out" => {
                cli.out_dir = parse_path("--out", args.next())?;
                cli.out_explicit = true;
            }
            "--cwd" => cli.cwd = parse_path("--cwd", args.next())?,
            "--env" => {
                let Some(pair) = args.next() else {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "--env requires NAME=VALUE",
                    ));
                };
                let Some((name, value)) = pair.split_once('=') else {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        format!("invalid --env value `{pair}`; expected NAME=VALUE"),
                    ));
                };
                cli.env.insert(name.to_owned(), value.to_owned());
            }
            "--nvim-bin" => {
                cli.nvim_bin = parse_string("--nvim-bin", args.next())?;
            }
            "--term" => {
                cli.term = parse_string("--term", args.next())?;
            }
            "--colorterm" => {
                cli.colorterm = Some(parse_string("--colorterm", args.next())?);
            }
            "--inherit-env" => cli.inherit_env = true,
            "--clean-env" => cli.inherit_env = false,
            "--" => {
                cli.nvim_args.extend(args);
                break;
            }
            other => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("unknown argument `{other}`"),
                ));
            }
        }
    }

    Ok(Some(cli))
}

fn resolve_out_dir(cli: &Cli) -> io::Result<PathBuf> {
    if cli.out_explicit || !cli.out_dir.exists() {
        return Ok(cli.out_dir.clone());
    }

    let parent = cli
        .out_dir
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."));
    let stem = cli
        .out_dir
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "invalid output path"))?;

    for index in 1..10_000u32 {
        let candidate = parent.join(format!("{stem}-{index}"));
        if !candidate.exists() {
            return Ok(candidate);
        }
    }

    Err(io::Error::new(
        io::ErrorKind::AlreadyExists,
        "could not find a free artifact directory name",
    ))
}

fn parse_u16(flag: &str, value: Option<String>) -> io::Result<u16> {
    let value = parse_string(flag, value)?;
    value.parse::<u16>().map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("{flag} expects an unsigned 16-bit integer"),
        )
    })
}

fn parse_u64(flag: &str, value: Option<String>) -> io::Result<u64> {
    let value = parse_string(flag, value)?;
    value.parse::<u64>().map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("{flag} expects an unsigned integer"),
        )
    })
}

fn parse_string(flag: &str, value: Option<String>) -> io::Result<String> {
    value.ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("{flag} requires a value"),
        )
    })
}

fn parse_path(flag: &str, value: Option<String>) -> io::Result<PathBuf> {
    Ok(PathBuf::from(parse_string(flag, value)?))
}

fn print_usage() {
    println!("usage: cargo run -p extui-neovim --example compare -- [options] -- [nvim-args...]");
    println!();
    println!("options:");
    println!("  --cols N          terminal width in cells (default {DEFAULT_COLS})");
    println!("  --rows N          terminal height in cells (default {DEFAULT_ROWS})");
    println!("  --idle-ms N       stop after N ms without new output (default {DEFAULT_IDLE_MS})");
    println!("  --timeout-ms N    hard timeout in ms (default {DEFAULT_TIMEOUT_MS})");
    println!("  --out PATH        artifact directory");
    println!("  --cwd PATH        working directory for both nvim instances");
    println!("  --nvim-bin PATH   nvim binary to execute");
    println!("  --term VALUE      TERM for both legs (default: current TERM)");
    println!("  --colorterm VAL   COLORTERM for both legs (default: current COLORTERM)");
    println!("  --inherit-env     preserve the parent process environment (default)");
    println!("  --clean-env       use a reduced environment instead");
    println!("  --env NAME=VALUE  extra env for both direct and embed children");
    println!("  --help            show this text");
}

impl VirtualTerminal {
    fn new(cols: u16, rows: u16) -> Self {
        let events = Arc::new(Mutex::new(Vec::new()));
        let listener = CaptureListener {
            events: Arc::clone(&events),
        };
        let size = TermSize::new(cols as usize, rows as usize);
        let term = Term::new(TermConfig::default(), &size, listener);
        Self {
            term,
            parser: Processor::new(),
            events,
            window_size: WindowSize {
                num_lines: rows,
                num_cols: cols,
                cell_width: CELL_WIDTH_PX,
                cell_height: CELL_HEIGHT_PX,
            },
        }
    }

    fn feed(&mut self, bytes: &[u8]) -> Vec<u8> {
        self.parser.advance(&mut self.term, bytes);
        self.drain_responses()
    }

    fn drain_responses(&mut self) -> Vec<u8> {
        let events = {
            let mut guard = self.events.lock().unwrap();
            mem::take(&mut *guard)
        };

        let mut out = Vec::new();
        for event in events {
            match event {
                AlacrittyEvent::PtyWrite(text) => out.extend_from_slice(text.as_bytes()),
                AlacrittyEvent::TextAreaSizeRequest(formatter) => {
                    out.extend_from_slice(formatter(self.window_size).as_bytes());
                }
                AlacrittyEvent::ColorRequest(index, formatter) => {
                    let rgb = color_from_index(self.term.colors(), index);
                    out.extend_from_slice(formatter(rgb).as_bytes());
                }
                _ => {}
            }
        }
        out
    }

    fn snapshot(&self, cols: u16, rows: u16) -> ScreenSnapshot {
        let mut cells = Vec::with_capacity(cols as usize * rows as usize);
        let colors = self.term.colors();

        for y in 0..rows {
            for x in 0..cols {
                let cell = &self.term.grid()[Line(y as i32)][Column(x as usize)];
                cells.push(normalize_cell(cell, colors));
            }
        }

        ScreenSnapshot { cols, rows, cells }
    }
}

fn run_direct(cli: &Cli) -> io::Result<Capture> {
    let env = effective_child_env(cli);
    let options = Options {
        shell: Some(build_direct_shell(cli, &env)),
        working_directory: Some(cli.cwd.clone()),
        ..Options::default()
    };

    let mut pty = tty::new(&options, window_size(cli.cols, cli.rows), 1)?;
    let mut term = VirtualTerminal::new(cli.cols, cli.rows);
    let mut raw = Vec::new();
    let mut scratch = vec![0; READ_CHUNK];
    let started = Instant::now();
    let mut last_activity = started;
    let mut saw_output = false;

    loop {
        match pty.reader().read(&mut scratch) {
            Ok(0) => break,
            Ok(n) => {
                saw_output = true;
                last_activity = Instant::now();
                raw.extend_from_slice(&scratch[..n]);
                let response = term.feed(&scratch[..n]);
                if !response.is_empty() {
                    write_all_nonblocking(pty.writer(), &response, cli.timeout)?;
                }
                continue;
            }
            Err(err) if err.kind() == io::ErrorKind::WouldBlock => {}
            Err(err) if err.kind() == io::ErrorKind::Interrupted => continue,
            Err(err) => return Err(err),
        }

        if saw_output && last_activity.elapsed() >= cli.idle {
            break;
        }
        if started.elapsed() >= cli.timeout {
            if !saw_output {
                return Err(io::Error::new(
                    io::ErrorKind::TimedOut,
                    "direct nvim produced no output before timeout",
                ));
            }
            break;
        }
        sleep(Duration::from_millis(10));
    }

    Ok(Capture {
        raw,
        env,
        snapshot: term.snapshot(cli.cols, cli.rows),
    })
}

fn run_embed(cli: &Cli) -> io::Result<Capture> {
    let env = effective_child_env(cli);
    let mut cmd = Command::new(&cli.nvim_bin);
    cmd.arg("--embed");
    cmd.current_dir(&cli.cwd);
    cmd.env_clear();
    for (name, value) in &env {
        cmd.env(name, value);
    }
    for arg in &cli.nvim_args {
        cmd.arg(arg);
    }

    let mut nvim = NeovimEmbed::spawn_with(cmd, cli.cols, cli.rows, NeovimWaker::default())?;
    if colorterm_implies_rgb(cli.colorterm.as_deref()) {
        nvim.set_termguicolors(true)?;
    }
    let mut terminal = VirtualTerminal::new(cli.cols, cli.rows);
    let mut raw = Vec::new();
    let mut frame_buf = Buffer::new(cli.cols, cli.rows);
    let rect = Rect {
        x: 0,
        y: 0,
        w: cli.cols,
        h: cli.rows,
    };

    raw.extend_from_slice(vt::ENABLE_ALT_SCREEN);
    let _ = terminal.feed(vt::ENABLE_ALT_SCREEN);

    let started = Instant::now();
    let mut last_activity = started;
    let mut saw_output = false;

    loop {
        let dirty = nvim.poll_updates();
        if dirty {
            saw_output = true;
            last_activity = Instant::now();

            nvim.render(rect, &mut frame_buf);
            frame_buf.render_internal();
            raw.extend_from_slice(frame_buf.write_buffer());
            let _ = terminal.feed(frame_buf.write_buffer());
            frame_buf.buf.clear();

            let cursor_bytes = render_cursor_bytes(&nvim, rect);
            raw.extend_from_slice(&cursor_bytes);
            let _ = terminal.feed(&cursor_bytes);
        }

        if saw_output && last_activity.elapsed() >= cli.idle {
            break;
        }
        if started.elapsed() >= cli.timeout {
            if !saw_output {
                return Err(io::Error::new(
                    io::ErrorKind::TimedOut,
                    "embedded nvim produced no output before timeout",
                ));
            }
            break;
        }
        if !nvim.is_alive() && saw_output {
            break;
        }
        sleep(Duration::from_millis(10));
    }

    Ok(Capture {
        raw,
        env,
        snapshot: terminal.snapshot(cli.cols, cli.rows),
    })
}

fn render_cursor_bytes(nvim: &NeovimEmbed, rect: Rect) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(16);
    if let Some((cx, cy)) = nvim.cursor_position(rect) {
        MoveCursor(cx, cy).write_to_buffer(&mut bytes);
        bytes.extend_from_slice(SHOW_CURSOR);
    } else {
        bytes.extend_from_slice(HIDE_CURSOR);
    }
    bytes
}

fn build_direct_shell(cli: &Cli, env: &BTreeMap<String, String>) -> Shell {
    let mut args = Vec::with_capacity(2 + env.len() + 1 + cli.nvim_args.len());
    args.push("-i".to_owned());
    for (name, value) in env {
        args.push(format!("{name}={value}"));
    }
    args.push(cli.nvim_bin.clone());
    args.extend(cli.nvim_args.iter().cloned());
    Shell::new("env".to_owned(), args)
}

fn effective_child_env(cli: &Cli) -> BTreeMap<String, String> {
    if cli.inherit_env {
        let mut env = BTreeMap::new();
        for (name, value) in std::env::vars() {
            env.insert(name, value);
        }
        env.insert("TERM".to_owned(), cli.term.clone());
        match &cli.colorterm {
            Some(colorterm) => {
                env.insert("COLORTERM".to_owned(), colorterm.clone());
            }
            None => {
                env.remove("COLORTERM");
            }
        }
        for (name, value) in &cli.env {
            env.insert(name.clone(), value.clone());
        }
        return env;
    }

    let mut env = BTreeMap::new();
    copy_parent_env(&mut env, "HOME");
    copy_parent_env(&mut env, "USER");
    copy_parent_env(&mut env, "LOGNAME");
    copy_parent_env(&mut env, "SHELL");
    copy_parent_env(&mut env, "PATH");
    copy_parent_env(&mut env, "LANG");
    copy_parent_env(&mut env, "LC_ALL");
    copy_parent_env(&mut env, "LC_CTYPE");
    copy_parent_env(&mut env, "XDG_CONFIG_HOME");
    copy_parent_env(&mut env, "XDG_DATA_HOME");
    copy_parent_env(&mut env, "XDG_STATE_HOME");
    copy_parent_env(&mut env, "XDG_CACHE_HOME");
    copy_parent_env(&mut env, "VIMRUNTIME");
    copy_parent_env(&mut env, "MYVIMRC");
    copy_parent_env(&mut env, "NVIM_APPNAME");
    copy_parent_env(&mut env, "TERM_PROGRAM");
    copy_parent_env(&mut env, "TERM_PROGRAM_VERSION");
    copy_parent_env(&mut env, "VTE_VERSION");
    copy_parent_env(&mut env, "KONSOLE_VERSION");
    copy_parent_env(&mut env, "TMUX");
    copy_parent_env(&mut env, "COLORFGBG");
    copy_parent_env(&mut env, "KITTY_WINDOW_ID");
    copy_parent_env(&mut env, "KITTY_PID");
    copy_parent_env(&mut env, "WEZTERM_EXECUTABLE");
    copy_parent_env(&mut env, "WEZTERM_PANE");
    copy_parent_env(&mut env, "ALACRITTY_WINDOW_ID");
    env.insert("TERM".to_owned(), cli.term.clone());
    if let Some(colorterm) = &cli.colorterm {
        env.insert("COLORTERM".to_owned(), colorterm.clone());
    } else {
        env.remove("COLORTERM");
    }
    for (name, value) in &cli.env {
        env.insert(name.clone(), value.clone());
    }
    env
}

fn copy_parent_env(env: &mut BTreeMap<String, String>, name: &str) {
    if let Ok(value) = std::env::var(name) {
        env.insert(name.to_owned(), value);
    }
}

fn colorterm_implies_rgb(value: Option<&str>) -> bool {
    matches!(value, Some(v) if v.eq_ignore_ascii_case("truecolor") || v.eq_ignore_ascii_case("24bit"))
}

fn window_size(cols: u16, rows: u16) -> WindowSize {
    WindowSize {
        num_lines: rows,
        num_cols: cols,
        cell_width: CELL_WIDTH_PX,
        cell_height: CELL_HEIGHT_PX,
    }
}

fn write_all_nonblocking(
    writer: &mut impl Write,
    mut bytes: &[u8],
    timeout: Duration,
) -> io::Result<()> {
    let started = Instant::now();
    while !bytes.is_empty() {
        match writer.write(bytes) {
            Ok(0) => {
                return Err(io::Error::new(
                    io::ErrorKind::WriteZero,
                    "short write while responding to PTY query",
                ));
            }
            Ok(n) => bytes = &bytes[n..],
            Err(err) if err.kind() == io::ErrorKind::WouldBlock => {
                if started.elapsed() >= timeout {
                    return Err(io::Error::new(
                        io::ErrorKind::TimedOut,
                        "timed out while responding to PTY query",
                    ));
                }
                sleep(Duration::from_millis(5));
            }
            Err(err) if err.kind() == io::ErrorKind::Interrupted => {}
            Err(err) => return Err(err),
        }
    }
    Ok(())
}

fn normalize_cell(cell: &TermCell, colors: &Colors) -> SnapshotCell {
    let flags = cell.flags;
    let is_spacer = flags.intersects(Flags::WIDE_CHAR_SPACER | Flags::LEADING_WIDE_CHAR_SPACER);

    let mut fg = resolve_color(colors, cell.fg);
    let mut bg = resolve_color(colors, cell.bg);
    if flags.contains(Flags::INVERSE) {
        std::mem::swap(&mut fg, &mut bg);
    }

    let hidden = flags.contains(Flags::HIDDEN);
    let mut text = if hidden || is_spacer {
        " ".to_owned()
    } else {
        let mut text = String::new();
        text.push(cell.c);
        if let Some(extra) = cell.zerowidth() {
            for ch in extra {
                text.push(*ch);
            }
        }
        text
    };
    if text.is_empty() {
        text.push(' ');
    }

    if hidden {
        fg = bg;
    }

    let underline = if flags.intersects(Flags::ALL_UNDERLINES) {
        Some(
            cell.underline_color()
                .map(|color| resolve_color(colors, color))
                .unwrap_or(fg),
        )
    } else {
        None
    };

    let visual_flags = (flags
        & (Flags::BOLD | Flags::ITALIC | Flags::DIM | Flags::ALL_UNDERLINES | Flags::STRIKEOUT))
        .bits();

    SnapshotCell {
        text,
        fg,
        bg,
        underline,
        flags: visual_flags,
    }
}

fn resolve_color(colors: &Colors, color: Color) -> Rgb {
    match color {
        Color::Spec(rgb) => rgb,
        Color::Indexed(index) => color_from_index(colors, index as usize),
        Color::Named(name) => color_from_index(colors, name as usize),
    }
}

fn color_from_index(colors: &Colors, index: usize) -> Rgb {
    colors[index].unwrap_or_else(|| default_color_for_index(index))
}

fn default_color_for_index(index: usize) -> Rgb {
    const ANSI: [Rgb; 16] = [
        Rgb {
            r: 0x00,
            g: 0x00,
            b: 0x00,
        },
        Rgb {
            r: 0xcd,
            g: 0x00,
            b: 0x00,
        },
        Rgb {
            r: 0x00,
            g: 0xcd,
            b: 0x00,
        },
        Rgb {
            r: 0xcd,
            g: 0xcd,
            b: 0x00,
        },
        Rgb {
            r: 0x00,
            g: 0x00,
            b: 0xee,
        },
        Rgb {
            r: 0xcd,
            g: 0x00,
            b: 0xcd,
        },
        Rgb {
            r: 0x00,
            g: 0xcd,
            b: 0xcd,
        },
        Rgb {
            r: 0xe5,
            g: 0xe5,
            b: 0xe5,
        },
        Rgb {
            r: 0x7f,
            g: 0x7f,
            b: 0x7f,
        },
        Rgb {
            r: 0xff,
            g: 0x00,
            b: 0x00,
        },
        Rgb {
            r: 0x00,
            g: 0xff,
            b: 0x00,
        },
        Rgb {
            r: 0xff,
            g: 0xff,
            b: 0x00,
        },
        Rgb {
            r: 0x5c,
            g: 0x5c,
            b: 0xff,
        },
        Rgb {
            r: 0xff,
            g: 0x00,
            b: 0xff,
        },
        Rgb {
            r: 0x00,
            g: 0xff,
            b: 0xff,
        },
        Rgb {
            r: 0xff,
            g: 0xff,
            b: 0xff,
        },
    ];
    const CUBE: [u8; 6] = [0x00, 0x5f, 0x87, 0xaf, 0xd7, 0xff];
    const DEFAULT_FG: Rgb = Rgb {
        r: 0xe5,
        g: 0xe5,
        b: 0xe5,
    };
    const DEFAULT_BG: Rgb = Rgb {
        r: 0x00,
        g: 0x00,
        b: 0x00,
    };

    match index {
        0..=15 => ANSI[index],
        16..=231 => {
            let idx = index - 16;
            let r = CUBE[idx / 36];
            let g = CUBE[(idx / 6) % 6];
            let b = CUBE[idx % 6];
            Rgb { r, g, b }
        }
        232..=255 => {
            let gray = 8 + ((index - 232) as u8 * 10);
            Rgb {
                r: gray,
                g: gray,
                b: gray,
            }
        }
        x if x == NamedColor::Foreground as usize => DEFAULT_FG,
        x if x == NamedColor::Background as usize => DEFAULT_BG,
        x if x == NamedColor::Cursor as usize => DEFAULT_FG,
        x if x == NamedColor::DimBlack as usize => dim_rgb(ANSI[0]),
        x if x == NamedColor::DimRed as usize => dim_rgb(ANSI[1]),
        x if x == NamedColor::DimGreen as usize => dim_rgb(ANSI[2]),
        x if x == NamedColor::DimYellow as usize => dim_rgb(ANSI[3]),
        x if x == NamedColor::DimBlue as usize => dim_rgb(ANSI[4]),
        x if x == NamedColor::DimMagenta as usize => dim_rgb(ANSI[5]),
        x if x == NamedColor::DimCyan as usize => dim_rgb(ANSI[6]),
        x if x == NamedColor::DimWhite as usize => dim_rgb(ANSI[7]),
        x if x == NamedColor::BrightForeground as usize => ANSI[15],
        x if x == NamedColor::DimForeground as usize => dim_rgb(DEFAULT_FG),
        _ => DEFAULT_BG,
    }
}

fn dim_rgb(rgb: Rgb) -> Rgb {
    Rgb {
        r: ((rgb.r as u16 * 2) / 3) as u8,
        g: ((rgb.g as u16 * 2) / 3) as u8,
        b: ((rgb.b as u16 * 2) / 3) as u8,
    }
}

fn diff_snapshots(direct: &ScreenSnapshot, embed: &ScreenSnapshot) -> Vec<CellDiff> {
    let mut diffs = Vec::new();
    for y in 0..direct.rows {
        for x in 0..direct.cols {
            let idx = y as usize * direct.cols as usize + x as usize;
            if direct.cells[idx] != embed.cells[idx] {
                diffs.push(CellDiff {
                    x,
                    y,
                    direct: direct.cells[idx].clone(),
                    embed: embed.cells[idx].clone(),
                });
            }
        }
    }
    diffs
}

fn write_artifacts(
    out_dir: &Path,
    direct: &Capture,
    embed: &Capture,
    diffs: &[CellDiff],
) -> io::Result<()> {
    fs::write(out_dir.join("direct.ansi"), &direct.raw)?;
    fs::write(out_dir.join("embed.ansi"), &embed.raw)?;
    fs::write(out_dir.join("direct.env"), format_env(&direct.env))?;
    fs::write(out_dir.join("embed.env"), format_env(&embed.env))?;
    fs::write(out_dir.join("direct.txt"), snapshot_text(&direct.snapshot))?;
    fs::write(out_dir.join("embed.txt"), snapshot_text(&embed.snapshot))?;
    fs::write(
        out_dir.join("report.txt"),
        diff_report(direct, embed, diffs),
    )?;
    fs::write(out_dir.join("diff.html"), diff_html(direct, embed, diffs))?;
    Ok(())
}

fn snapshot_text(snapshot: &ScreenSnapshot) -> String {
    let mut out = String::new();
    for y in 0..snapshot.rows {
        for x in 0..snapshot.cols {
            let idx = y as usize * snapshot.cols as usize + x as usize;
            out.push_str(&snapshot.cells[idx].text);
        }
        out.push('\n');
    }
    out
}

fn format_env(env: &BTreeMap<String, String>) -> String {
    let mut out = String::new();
    for (name, value) in env {
        out.push_str(name);
        out.push('=');
        out.push_str(value);
        out.push('\n');
    }
    out
}

fn diff_report(direct: &Capture, embed: &Capture, diffs: &[CellDiff]) -> String {
    let mut out = String::new();
    out.push_str(&format!(
        "mismatches: {} / {}\n",
        diffs.len(),
        direct.snapshot.cells.len()
    ));
    out.push('\n');
    out.push_str("[direct env]\n");
    out.push_str(&format_env(&direct.env));
    out.push('\n');
    out.push_str("[embed env]\n");
    out.push_str(&format_env(&embed.env));
    out.push('\n');
    for diff in diffs.iter().take(200) {
        out.push_str(&format!(
            "({}, {}): direct text={} fg={} bg={} ul={} flags={} | embed text={} fg={} bg={} ul={} flags={}\n",
            diff.x,
            diff.y,
            debug_text(&diff.direct.text),
            fmt_rgb(diff.direct.fg),
            fmt_rgb(diff.direct.bg),
            diff.direct
                .underline
                .map(fmt_rgb)
                .unwrap_or_else(|| "-".to_owned()),
            fmt_flags(diff.direct.flags),
            debug_text(&diff.embed.text),
            fmt_rgb(diff.embed.fg),
            fmt_rgb(diff.embed.bg),
            diff.embed
                .underline
                .map(fmt_rgb)
                .unwrap_or_else(|| "-".to_owned()),
            fmt_flags(diff.embed.flags),
        ));
    }
    out
}

fn diff_html(direct: &Capture, embed: &Capture, diffs: &[CellDiff]) -> String {
    let mut mismatch_map = vec![false; direct.snapshot.cells.len()];
    for diff in diffs {
        let idx = diff.y as usize * direct.snapshot.cols as usize + diff.x as usize;
        mismatch_map[idx] = true;
    }

    let mut html = String::new();
    html.push_str("<!doctype html><meta charset=\"utf-8\">");
    html.push_str("<title>extui-neovim diff</title>");
    html.push_str(
        "<style>\
         body{font:14px/1.35 ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;background:#111;color:#eee;margin:24px;}\
         .meta{margin-bottom:20px;}\
         .grid{display:grid;grid-template-columns:1fr 1fr;gap:24px;align-items:start;}\
         .panel{background:#181818;border:1px solid #333;padding:12px;overflow:auto;}\
         .title{font-weight:700;margin-bottom:10px;}\
         .screen{white-space:pre;line-height:1.1;}\
         .cell{display:inline-block;width:0.62em;height:1.1em;}\
         .mismatch{box-shadow:inset 0 0 0 1px rgba(255,64,64,.9);}\
         table{border-collapse:collapse;margin-top:20px;width:100%;}\
         th,td{border:1px solid #333;padding:4px 6px;text-align:left;vertical-align:top;}\
         th{background:#1f1f1f;}\
         </style>",
    );
    html.push_str("<div class=\"meta\">");
    html.push_str(&format!(
        "<div>mismatches: {} / {}</div>",
        diffs.len(),
        direct.snapshot.cells.len()
    ));
    html.push_str("</div>");
    html.push_str("<div class=\"grid\">");
    html.push_str(
        "<div class=\"panel\"><div class=\"title\">Direct nvim</div><div class=\"screen\">",
    );
    html.push_str(&render_snapshot_html(&direct.snapshot, &mismatch_map));
    html.push_str("</div></div>");
    html.push_str(
        "<div class=\"panel\"><div class=\"title\">extui embed</div><div class=\"screen\">",
    );
    html.push_str(&render_snapshot_html(&embed.snapshot, &mismatch_map));
    html.push_str("</div></div>");
    html.push_str("</div>");

    html.push_str("<table><thead><tr>");
    html.push_str("<th>Cell</th><th>Direct</th><th>Embed</th>");
    html.push_str("</tr></thead><tbody>");
    for diff in diffs.iter().take(200) {
        html.push_str("<tr>");
        html.push_str(&format!("<td>{}, {}</td>", diff.x, diff.y));
        html.push_str(&format!("<td>{}</td>", describe_cell_html(&diff.direct)));
        html.push_str(&format!("<td>{}</td>", describe_cell_html(&diff.embed)));
        html.push_str("</tr>");
    }
    html.push_str("</tbody></table>");

    html
}

fn render_snapshot_html(snapshot: &ScreenSnapshot, mismatch_map: &[bool]) -> String {
    let mut html = String::new();
    for y in 0..snapshot.rows {
        for x in 0..snapshot.cols {
            let idx = y as usize * snapshot.cols as usize + x as usize;
            let cell = &snapshot.cells[idx];
            html.push_str("<span class=\"cell");
            if mismatch_map[idx] {
                html.push_str(" mismatch");
            }
            html.push_str("\" style=\"");
            html.push_str(&format!(
                "color:{};background:{};",
                fmt_rgb(cell.fg),
                fmt_rgb(cell.bg)
            ));
            if cell.flags & Flags::BOLD.bits() != 0 {
                html.push_str("font-weight:700;");
            }
            if cell.flags & Flags::ITALIC.bits() != 0 {
                html.push_str("font-style:italic;");
            }
            if cell.flags & Flags::STRIKEOUT.bits() != 0 {
                html.push_str("text-decoration-line:line-through;");
            }
            if cell.flags & Flags::ALL_UNDERLINES.bits() != 0 {
                html.push_str("text-decoration-line:underline;");
                if cell.flags & Flags::DOUBLE_UNDERLINE.bits() != 0 {
                    html.push_str("text-decoration-style:double;");
                } else if cell.flags & Flags::UNDERCURL.bits() != 0 {
                    html.push_str("text-decoration-style:wavy;");
                } else if cell.flags & Flags::DOTTED_UNDERLINE.bits() != 0 {
                    html.push_str("text-decoration-style:dotted;");
                } else if cell.flags & Flags::DASHED_UNDERLINE.bits() != 0 {
                    html.push_str("text-decoration-style:dashed;");
                }
                if let Some(underline) = cell.underline {
                    html.push_str(&format!("text-decoration-color:{};", fmt_rgb(underline)));
                }
            }
            html.push_str("\">");
            html.push_str(&html_escape(&cell.text));
            html.push_str("</span>");
        }
        html.push('\n');
    }
    html
}

fn describe_cell_html(cell: &SnapshotCell) -> String {
    format!(
        "text={} fg={} bg={} ul={} flags={}",
        html_escape(&debug_text(&cell.text)),
        html_escape(&fmt_rgb(cell.fg)),
        html_escape(&fmt_rgb(cell.bg)),
        html_escape(
            &cell
                .underline
                .map(fmt_rgb)
                .unwrap_or_else(|| "-".to_owned()),
        ),
        html_escape(&fmt_flags(cell.flags)),
    )
}

fn debug_text(text: &str) -> String {
    if text == " " {
        "space".to_owned()
    } else {
        text.escape_default().collect()
    }
}

fn fmt_rgb(rgb: Rgb) -> String {
    format!("#{:02x}{:02x}{:02x}", rgb.r, rgb.g, rgb.b)
}

fn fmt_flags(bits: u16) -> String {
    let mut names = Vec::new();
    if bits & Flags::BOLD.bits() != 0 {
        names.push("bold");
    }
    if bits & Flags::DIM.bits() != 0 {
        names.push("dim");
    }
    if bits & Flags::ITALIC.bits() != 0 {
        names.push("italic");
    }
    if bits & Flags::UNDERLINE.bits() != 0 {
        names.push("underline");
    }
    if bits & Flags::DOUBLE_UNDERLINE.bits() != 0 {
        names.push("double-underline");
    }
    if bits & Flags::UNDERCURL.bits() != 0 {
        names.push("undercurl");
    }
    if bits & Flags::DOTTED_UNDERLINE.bits() != 0 {
        names.push("dotted-underline");
    }
    if bits & Flags::DASHED_UNDERLINE.bits() != 0 {
        names.push("dashed-underline");
    }
    if bits & Flags::STRIKEOUT.bits() != 0 {
        names.push("strike");
    }
    if names.is_empty() {
        "-".to_owned()
    } else {
        names.join(",")
    }
}

fn html_escape(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for ch in text.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&#39;"),
            _ => out.push(ch),
        }
    }
    out
}
