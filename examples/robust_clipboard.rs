use std::{
    io::{self, Write},
    process::{Command, Stdio},
    thread,
    time::Duration,
};

use extui::{
    AnsiColor, DoubleBuffer, Style, Terminal, TerminalFeatures, TerminalFlags,
    event::{self, Event, Events, KeyCode, KeyModifiers},
    vt::{BufferWrite, ClipboardSelection, SetClipboard},
};

const COPY_TEXT: &str = "Copied from extui robust_clipboard example.";
const QUERY_TIMEOUT: Duration = Duration::from_millis(100);

fn main() -> io::Result<()> {
    let flags = TerminalFlags::RAW_MODE | TerminalFlags::ALT_SCREEN | TerminalFlags::HIDE_CURSOR;
    #[cfg(feature = "bracketed-paste")]
    let flags = flags | TerminalFlags::BRACKETED_PASTE;

    let mut term = Terminal::open(flags)?;
    let stdin = std::io::stdin();
    let (w, h) = term.size()?;
    let mut buf = DoubleBuffer::new(w, h);
    let mut events = Events::default();
    let mut app = App::default();

    loop {
        render(&app, &mut buf);
        buf.render(&mut term);

        if event::poll(&stdin, Some(Duration::from_millis(100)))?.is_readable() {
            events.read_from(&stdin)?;
        }

        while let Some(ev) = events.next(term.is_raw()) {
            match ev {
                Event::Key(key) => {
                    if key.code == KeyCode::Char('q')
                        || (key.code == KeyCode::Char('c')
                            && key.modifiers.contains(KeyModifiers::CONTROL))
                    {
                        return Ok(());
                    }

                    if key.code == KeyCode::Char('c') {
                        match copy_text(&mut term, &stdin, &mut events, &mut app.features) {
                            Ok(report) => app.last_copy = report,
                            Err(err) => {
                                app.last_copy = CopyReport {
                                    backend: "failed",
                                    note: err.to_string(),
                                };
                            }
                        }
                    }
                }
                #[cfg(feature = "bracketed-paste")]
                Event::Paste(text) => {
                    app.last_paste = Some(text);
                }
                Event::Resized => {
                    let (w, h) = term.size()?;
                    buf.resize(w, h);
                }
                _ => {}
            }
        }
    }
}

#[derive(Default)]
struct App {
    features: Option<TerminalFeatures>,
    last_copy: CopyReport,
    last_paste: Option<String>,
}

#[derive(Default)]
struct CopyReport {
    backend: &'static str,
    note: String,
}

fn render(app: &App, buf: &mut DoubleBuffer) {
    buf.rect().with(AnsiColor::Black.as_bg()).fill(buf);

    let mut rect = buf.rect();
    rect.take_top(1)
        .with(AnsiColor::White.with_fg(AnsiColor::Black))
        .text(buf, " robust clipboard example ");
    rect.take_top(1).with(Style::DEFAULT).text(
        buf,
        "Press c to copy sample text. Press q or Ctrl-C to quit.",
    );

    let feature_status = match app.features {
        None => "not probed yet",
        Some(features)
            if features.contains(TerminalFeatures::OSC52_CLIPBOARD)
                && features.contains(TerminalFeatures::OSC52_CLIPBOARD_READ) =>
        {
            "OSC 52 copy supported; readback supported"
        }
        Some(features) if features.contains(TerminalFeatures::OSC52_CLIPBOARD) => {
            "OSC 52 copy supported; readback unavailable"
        }
        Some(_) => "OSC 52 copy support not detected",
    };

    rect.take_top(1)
        .with(AnsiColor::Grey[20].as_fg())
        .fmt(buf, format_args!("Copy text: {COPY_TEXT}"));
    rect.take_top(1)
        .with(AnsiColor::Grey[20].as_fg())
        .fmt(buf, format_args!("Feature detection: {feature_status}"));

    if app.last_copy.backend.is_empty() {
        rect.take_top(1)
            .with(AnsiColor::Grey[20].as_fg())
            .text(buf, "Last copy: none");
    } else {
        rect.take_top(1).with(AnsiColor::Green1.as_fg()).fmt(
            buf,
            format_args!(
                "Last copy: {} ({})",
                app.last_copy.backend, app.last_copy.note
            ),
        );
    }

    if let Some(paste) = &app.last_paste {
        rect.take_top(1)
            .with(AnsiColor::Grey[20].as_fg())
            .fmt(buf, format_args!("Last bracketed paste: {paste:?}"));
    }
}

fn copy_text(
    term: &mut Terminal,
    input: &std::io::Stdin,
    events: &mut Events,
    cached_features: &mut Option<TerminalFeatures>,
) -> io::Result<CopyReport> {
    let features = match *cached_features {
        Some(features) => features,
        None => {
            let detected = term.detect_features(
                input,
                events,
                TerminalFeatures::OSC52_CLIPBOARD | TerminalFeatures::OSC52_CLIPBOARD_READ,
                QUERY_TIMEOUT,
            )?;
            *cached_features = Some(detected);
            detected
        }
    };

    if features.contains(TerminalFeatures::OSC52_CLIPBOARD) {
        write_osc52_copy(term, COPY_TEXT)?;
        if features.contains(TerminalFeatures::OSC52_CLIPBOARD_READ) {
            if let Some(response) =
                term.read_clipboard(input, events, ClipboardSelection::Clipboard, QUERY_TIMEOUT)?
                && response.text == COPY_TEXT
            {
                return Ok(CopyReport {
                    backend: "OSC 52",
                    note: "verified by terminal readback".to_string(),
                });
            }

            match copy_with_cli(COPY_TEXT) {
                Ok(command) => {
                    return Ok(CopyReport {
                        backend: command,
                        note: "OSC 52 verification failed; used clipboard command".to_string(),
                    });
                }
                Err(cli_error) => {
                    return Ok(CopyReport {
                        backend: "OSC 52",
                        note: format!(
                            "sent via detected OSC 52 support; CLI fallback failed: {cli_error}"
                        ),
                    });
                }
            }
        }

        return Ok(CopyReport {
            backend: "OSC 52",
            note: "sent via DA1/XTGETTCAP-detected support".to_string(),
        });
    }

    match copy_with_cli(COPY_TEXT) {
        Ok(command) => Ok(CopyReport {
            backend: command,
            note: "OSC 52 support was not detected".to_string(),
        }),
        Err(cli_error) => Err(io::Error::other(format!(
            "OSC 52 support was not detected and CLI fallback failed: {cli_error}"
        ))),
    }
}

fn write_osc52_copy(term: &mut Terminal, text: &str) -> io::Result<()> {
    let mut out = Vec::new();
    SetClipboard {
        selection: ClipboardSelection::Clipboard,
        text,
    }
    .write_to_buffer(&mut out);
    term.write_all(&out)
}

struct ClipboardCommand {
    program: &'static str,
    args: &'static [&'static str],
}

#[cfg(target_os = "macos")]
const CLIPBOARD_COMMANDS: &[ClipboardCommand] = &[ClipboardCommand {
    program: "pbcopy",
    args: &[],
}];

#[cfg(target_os = "linux")]
const CLIPBOARD_COMMANDS: &[ClipboardCommand] = &[
    ClipboardCommand {
        program: "wl-copy",
        args: &[],
    },
    ClipboardCommand {
        program: "xsel",
        args: &["--clipboard", "--input"],
    },
    ClipboardCommand {
        program: "xclip",
        args: &["-selection", "clipboard"],
    },
];

#[cfg(not(any(target_os = "macos", target_os = "linux")))]
const CLIPBOARD_COMMANDS: &[ClipboardCommand] = &[];

fn copy_with_cli(text: &str) -> io::Result<&'static str> {
    let mut errors = Vec::new();

    for command in CLIPBOARD_COMMANDS {
        match run_clipboard_command(command, text) {
            Ok(()) => return Ok(command.program),
            Err(err) => errors.push(format!("{}: {err}", command.program)),
        }
    }

    if errors.is_empty() {
        Err(io::Error::other(
            "no clipboard commands configured for this OS",
        ))
    } else {
        Err(io::Error::other(errors.join("; ")))
    }
}

fn run_clipboard_command(command: &ClipboardCommand, text: &str) -> io::Result<()> {
    let mut child = Command::new(command.program)
        .args(command.args)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()?;

    {
        let mut stdin = child
            .stdin
            .take()
            .ok_or_else(|| io::Error::other("clipboard command stdin unavailable"))?;
        stdin.write_all(text.as_bytes())?;
    }

    for _ in 0..30 {
        if let Some(status) = child.try_wait()? {
            return if status.success() {
                Ok(())
            } else {
                Err(io::Error::other(format!("exited with {status}")))
            };
        }
        thread::sleep(Duration::from_millis(10));
    }

    // xsel/xclip may stay alive to own the X selection. Once stdin has been
    // accepted and the process remains running briefly, treat that as success.
    Ok(())
}
