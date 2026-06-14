mod bindings;
mod common;
mod editor;
mod profile;
mod ratatui_render;
mod render;

use jsony_bench::Bencher;
use std::path::PathBuf;
use std::time::Duration;

use common::{BenchRecord, print_header};

enum Cmd {
    Bench {
        patterns: Vec<String>,
        save: Option<PathBuf>,
        base: Option<PathBuf>,
    },
    List,
    Profile {
        name: String,
        budget: profile::Budget,
    },
    Help,
}

fn print_help() {
    println!(
        "usage:\n  \
         benchmark [bench] [PATTERN...] [--save PATH] [--base PATH]\n  \
         benchmark list\n  \
         benchmark profile <name> [--iters N] [--duration SECS]\n  \
         benchmark help\n\
         \n\
         bench     run matching benchmarks (default subcommand)\n  \
         PATTERN   select benches. Bare token (e.g. \"render\") matches\n            \
         a group exactly. \"group/name\" matches a group exactly and\n            \
         does a substring match on the scenario name (e.g.\n            \
         \"editor/insert\"). \"/name\" filters by name across all\n            \
         groups. No patterns runs everything. Patterns are OR'd.\n  \
         --save    write JSON records to PATH\n  \
         --base    load JSON baseline and print percent deltas\n\
         \n\
         list      print every `group/name` and profile path\n\
         \n\
         profile   run a named workload in a tight loop (no measurement),\n            \
         suitable for attaching perf/samply. Defaults are tuned per\n            \
         workload for roughly 3-5 seconds."
    );
}

fn parse_args() -> Result<Cmd, String> {
    let mut args = std::env::args().skip(1).peekable();

    let first = args.peek().map(String::as_str);
    match first {
        Some("help") | Some("-h") | Some("--help") => return Ok(Cmd::Help),
        Some("list") => {
            args.next();
            if args.next().is_some() {
                return Err("list takes no arguments".into());
            }
            return Ok(Cmd::List);
        }
        Some("profile") => {
            args.next();
            return parse_profile(args);
        }
        Some("bench") => {
            args.next();
        }
        _ => {}
    }

    // Default: bench
    let mut patterns = Vec::new();
    let mut save: Option<PathBuf> = None;
    let mut base: Option<PathBuf> = None;
    while let Some(a) = args.next() {
        match a.as_str() {
            "--save" => {
                save = Some(PathBuf::from(args.next().ok_or("--save requires a path")?));
            }
            "--base" | "--compare" => {
                base = Some(PathBuf::from(args.next().ok_or("--base requires a path")?));
            }
            s if s.starts_with("--") => return Err(format!("unknown flag: {s}")),
            _ => patterns.push(a),
        }
    }
    Ok(Cmd::Bench {
        patterns,
        save,
        base,
    })
}

fn parse_profile(
    mut args: std::iter::Peekable<impl Iterator<Item = String>>,
) -> Result<Cmd, String> {
    let name = args.next().ok_or("profile requires a <name> argument")?;
    let mut iters: Option<u64> = None;
    let mut duration: Option<Duration> = None;
    while let Some(a) = args.next() {
        match a.as_str() {
            "--iters" => {
                let v = args.next().ok_or("--iters requires a number")?;
                iters = Some(v.parse().map_err(|e| format!("--iters: {e}"))?);
            }
            "--duration" => {
                let v = args.next().ok_or("--duration requires seconds")?;
                let secs: f64 = v.parse().map_err(|e| format!("--duration: {e}"))?;
                duration = Some(Duration::from_secs_f64(secs));
            }
            other => return Err(format!("unknown arg for profile: {other}")),
        }
    }
    if iters.is_some() && duration.is_some() {
        return Err("pass only one of --iters or --duration".into());
    }
    let budget = match (iters, duration) {
        (_, Some(d)) => profile::Budget::Duration(d),
        (Some(n), _) => profile::Budget::Iters(n),
        (None, None) => profile::Budget::Iters(
            profile::find(&name)
                .map(|p| p.default_iters)
                .unwrap_or(1_000),
        ),
    };
    Ok(Cmd::Profile { name, budget })
}

fn load_baseline(path: &PathBuf) -> Option<Vec<BenchRecord>> {
    let data = match std::fs::read_to_string(path) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("could not read baseline {}: {}", path.display(), e);
            return None;
        }
    };
    match jsony::from_json::<Vec<BenchRecord>>(&data) {
        Ok(v) => Some(v),
        Err(e) => {
            eprintln!("could not parse baseline {}: {}", path.display(), e);
            None
        }
    }
}

fn matcher<'a>(patterns: &'a [String]) -> impl Fn(&str, &str) -> bool + 'a {
    move |group: &str, name: &str| {
        if patterns.is_empty() {
            return true;
        }
        patterns.iter().any(|p| match p.split_once('/') {
            // `group/name`: exact group match, substring match on name.
            // `group/`: all of a group. `/name`: name-substring in any group.
            Some((g, n)) => (g.is_empty() || group == g) && (n.is_empty() || name.contains(n)),
            // Bare token: exact group match. Use `/token` to filter on
            // name, or `group/token` for both.
            None => group == p.as_str(),
        })
    }
}

fn run_list() {
    for (g, n) in render::list() {
        println!("bench    {g}/{n}");
    }
    for (g, n) in ratatui_render::list() {
        println!("bench    {g}/{n}");
    }
    for (g, n) in editor::list() {
        println!("bench    {g}/{n}");
    }
    for (g, n) in bindings::list() {
        println!("bench    {g}/{n}");
    }
    for path in profile::PATHS {
        println!("profile  {} --iters {}", path.name, path.default_iters);
    }
}

fn run_bench(patterns: Vec<String>, save: Option<PathBuf>, base: Option<PathBuf>) {
    let baseline = base.as_ref().and_then(load_baseline);
    let mut bencher = Bencher::new();
    bencher.calibrate();

    print_header(baseline.is_some());

    let matches = matcher(&patterns);
    let mut records: Vec<BenchRecord> = Vec::new();

    render::run_matching(&mut bencher, &matches, &mut records, baseline.as_deref());
    ratatui_render::run_matching(&mut bencher, &matches, &mut records, baseline.as_deref());
    editor::run_matching(&mut bencher, &matches, &mut records, baseline.as_deref());
    bindings::run_matching(&mut bencher, &matches, &mut records, baseline.as_deref());

    if records.is_empty() {
        eprintln!("no benchmarks matched patterns: {:?}", patterns);
    }

    if let Some(path) = save {
        let json = jsony::to_json(&records);
        match std::fs::write(&path, json) {
            Ok(()) => eprintln!("saved {} records to {}", records.len(), path.display()),
            Err(e) => eprintln!("save failed: {e}"),
        }
    }
}

fn run_profile(name: String, budget: profile::Budget) {
    let Some(path) = profile::find(&name) else {
        eprintln!("unknown profile path: {name}");
        eprintln!("available:");
        for path in profile::PATHS {
            eprintln!("  {} --iters {}", path.name, path.default_iters);
        }
        std::process::exit(1);
    };
    (path.run)(budget);
}

fn main() {
    let cmd = match parse_args() {
        Ok(c) => c,
        Err(e) => {
            eprintln!("{e}");
            eprintln!();
            print_help();
            std::process::exit(2);
        }
    };
    match cmd {
        Cmd::Help => print_help(),
        Cmd::List => run_list(),
        Cmd::Bench {
            patterns,
            save,
            base,
        } => run_bench(patterns, save, base),
        Cmd::Profile { name, budget } => run_profile(name, budget),
    }
}
