use jsony::Jsony;

#[derive(Jsony, Clone)]
#[jsony(Json)]
pub struct BenchRecord {
    pub group: String,
    pub size: String,
    pub width: u16,
    pub height: u16,
    pub scenario: String,
    pub ns: f64,
    pub cycles: f64,
    pub inst: f64,
    pub branch: f64,
    pub bytes: u64,
}

fn pct(new: f64, old: f64) -> f64 {
    if old == 0.0 {
        0.0
    } else {
        (new - old) / old * 100.0
    }
}

pub fn print_header(has_base: bool) {
    print!(
        "{:<8} {:<8} {:<18} {:>8} {:>9} {:>10} {:>9} {:>7} {:>10}",
        "group", "size", "scenario", "ns", "cycles", "instr", "branch", "bytes", "cells/s"
    );
    if has_base {
        print!(
            " {:>7} {:>7} {:>7} {:>7}",
            "dns%", "dcyc%", "dins%", "dbyt%"
        );
    }
    println!();
    let width = if has_base { 102 + 32 } else { 102 };
    println!("{}", "-".repeat(width));
}

pub fn print_row(r: &BenchRecord, base: Option<&BenchRecord>) {
    let cells = u64::from(r.width) * u64::from(r.height);
    let cells_per_sec = if r.ns > 0.0 {
        (cells as f64) * 1e9 / r.ns
    } else {
        0.0
    };
    print!(
        "{:<8} {:<8} {:<18} {:>8} {:>9} {:>10} {:>9} {:>7} {:>10.3e}",
        r.group,
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

pub fn find_baseline<'a>(
    baseline: Option<&'a [BenchRecord]>,
    r: &BenchRecord,
) -> Option<&'a BenchRecord> {
    baseline?
        .iter()
        .find(|b| b.group == r.group && b.scenario == r.scenario && b.size == r.size)
}
