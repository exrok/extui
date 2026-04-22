//! Replays a libfuzzer artifact against a fuzz target with verbose
//! tracing so a crash can be understood without attaching a debugger.

use std::env;
use std::fs;
use std::process::ExitCode;

fn usage() {
    eprintln!("usage: debug_artifact <target> <artifact-path>");
    eprintln!();
    eprintln!("Reads the raw bytes at <artifact-path> (such as one of the");
    eprintln!("files libfuzzer drops into fuzz/artifacts/<target>/) and");
    eprintln!("replays them through the corresponding tracer so each");
    eprintln!("mutation's before/after state and invariant check is shown.");
    eprintln!();
    eprintln!("Targets:");
    eprintln!("  rust_mutations");
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        usage();
        return ExitCode::from(2);
    }
    let target = &args[1];
    let path = &args[2];

    let data = match fs::read(path) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("error: reading {path}: {e}");
            return ExitCode::from(2);
        }
    };

    println!("# artifact : {path} ({} bytes)", data.len());
    println!("# target   : {target}");
    println!();

    let ok = match target.as_str() {
        "rust_mutations" => tinyhl_fuzz::trace_rust_mutations(&data),
        other => {
            eprintln!("error: unknown target {other:?}");
            usage();
            return ExitCode::from(2);
        }
    };

    if ok {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
