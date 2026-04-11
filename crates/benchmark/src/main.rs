mod scene;

use extui::DoubleBuffer;
use jsony::Jsony;
use jsony_bench::{Bencher, Stat};
use std::cell::RefCell;
use std::path::PathBuf;

use scene::{SCENARIOS, SIZES, Scenario, World, setup};

#[derive(Jsony, Clone)]
#[jsony(Json)]
struct BenchRecord {
    size: String,
    width: u16,
    height: u16,
    scenario: String,
    ns: f64,
    cycles: f64,
    inst: f64,
    branch: f64,
    bytes: u64,
}

struct BenchState {
    db: DoubleBuffer,
    world: World,
    frame: u64,
}

fn measure_bytes(scenario: &Scenario, w: u16, h: u16) -> usize {
    let mut db = DoubleBuffer::new(w, h);
    setup(&mut db);
    let mut world = World::new(256);
    (scenario.run)(&mut db, &mut world, 0);
    db.render_internal();
    db.buf.clear();
    (scenario.run)(&mut db, &mut world, 1);
    db.render_internal();
    db.buf.len()
}

fn run_scenario(bencher: &mut Bencher, scenario: &Scenario, w: u16, h: u16) -> Stat {
    let mut db = DoubleBuffer::new(w, h);
    setup(&mut db);
    let state = RefCell::new(BenchState {
        db,
        world: World::new(256),
        frame: 0,
    });
    if !scenario.reset_each_frame {
        let mut s = state.borrow_mut();
        let s = &mut *s;
        (scenario.run)(&mut s.db, &mut s.world, 0);
        s.db.render_internal();
        s.db.buf.clear();
    }
    bencher.bench_with_generator(
        || {
            let mut s = state.borrow_mut();
            s.frame = s.frame.wrapping_add(1);
            s.frame
        },
        |frame| {
            let mut s = state.borrow_mut();
            let s = &mut *s;
            (scenario.run)(&mut s.db, &mut s.world, frame);
            s.db.render_internal();
            std::hint::black_box(&s.db.buf);
            s.db.buf.clear();
        },
    )
}

fn pct(new: f64, old: f64) -> f64 {
    if old == 0.0 {
        0.0
    } else {
        (new - old) / old * 100.0
    }
}

fn print_header(has_base: bool) {
    print!(
        "{:<8} {:<14} {:>8} {:>9} {:>10} {:>9} {:>6} {:>10}",
        "size", "scenario", "ns", "cycles", "instr", "branch", "bytes", "cells/s"
    );
    if has_base {
        print!(
            " {:>7} {:>7} {:>7} {:>7}",
            "dns%", "dcyc%", "dins%", "dbyt%"
        );
    }
    println!();
    let width = if has_base { 84 + 32 } else { 84 };
    println!("{}", "-".repeat(width));
}

fn print_row(r: &BenchRecord, base: Option<&BenchRecord>) {
    let cells = (r.width as u64) * (r.height as u64);
    let cells_per_sec = if r.ns > 0.0 {
        (cells as f64) * 1e9 / r.ns
    } else {
        0.0
    };
    print!(
        "{:<8} {:<14} {:>8} {:>9} {:>10} {:>9} {:>6} {:>10.3e}",
        r.size,
        r.scenario,
        r.ns.round() as u64,
        r.cycles.round() as u64,
        r.inst.round() as u64,
        r.branch.round() as u64,
        r.bytes,
        cells_per_sec
    );
    if let Some(base) = base {
        let dns = pct(r.ns, base.ns);
        let dcyc = pct(r.cycles, base.cycles);
        let dins = pct(r.inst, base.inst);
        let dbyt = pct(r.bytes as f64, base.bytes as f64);
        print!(
            " {:>+6.1}% {:>+6.1}% {:>+6.1}% {:>+6.1}%",
            dns, dcyc, dins, dbyt
        );
    }
    println!();
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

fn main() {
    let mut save_path: Option<PathBuf> = None;
    let mut base_path: Option<PathBuf> = None;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--save" => save_path = args.next().map(PathBuf::from),
            "--base" | "--compare" => base_path = args.next().map(PathBuf::from),
            "--help" | "-h" => {
                println!(
                    "usage: benchmark [--save <path>] [--base <path>]\n\
                     \n  --save <path>   write results as JSON to <path>\n  \
                     --base <path>   load baseline JSON and show percent deltas"
                );
                return;
            }
            other => eprintln!("unknown arg: {}", other),
        }
    }

    let baseline = base_path.as_ref().and_then(load_baseline);

    let mut bencher = Bencher::new();
    bencher.calibrate();

    print_header(baseline.is_some());

    let mut records: Vec<BenchRecord> = Vec::new();
    for &(size_name, w, h) in SIZES {
        for scenario in SCENARIOS {
            let stat = run_scenario(&mut bencher, scenario, w, h);
            let bytes = measure_bytes(scenario, w, h) as u64;
            let record = BenchRecord {
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
            let base = baseline.as_ref().and_then(|b| {
                b.iter()
                    .find(|r| r.size == record.size && r.scenario == record.scenario)
            });
            print_row(&record, base);
            records.push(record);
        }
        println!();
    }

    if let Some(path) = save_path {
        let json = jsony::to_json(&records);
        match std::fs::write(&path, json) {
            Ok(()) => eprintln!("saved {} records to {}", records.len(), path.display()),
            Err(e) => eprintln!("save failed: {}", e),
        }
    }
}
