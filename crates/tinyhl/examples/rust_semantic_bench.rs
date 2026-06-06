use std::cell::Cell;
use std::env;
use std::fs;
use std::hash::{BuildHasher, Hasher};
use std::path::{Path, PathBuf};
use std::time::Instant;

use foldhash::fast::{FixedState, FoldHasher};
use tinyhl::{
    Highlighter, Language, Overlays, RenderSpan, RenderSpans, SemanticKind, SemanticTable, Source,
    Span, TokenTable, kind,
};

const DEFAULT_ROOT: &str = "/code/toml-spanner";
const DEFAULT_MODE: Mode = Mode::Render;
const DEFAULT_SOURCE_MODE: SourceMode = SourceMode::Lines;
const DEFAULT_ITERATIONS: usize = 50;
const CHECKSUM_SEED: u64 = 0x7469_6e79_686c_0002;
const DEFAULT_EXPECTED_FILES: usize = 104;
const DEFAULT_EXPECTED_LINES: usize = 54_586;
const DEFAULT_EXPECTED_BYTES: usize = 1_770_935;
const DEFAULT_EXPECTED_CHECKSUM: u64 = 13_741_512_142_065_689_284;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Mode {
    Lex,
    Semantic,
    Rebuild,
    Render,
    Verify,
}

impl Mode {
    fn tag(self) -> u64 {
        match self {
            Mode::Lex => 1,
            Mode::Semantic => 2,
            Mode::Rebuild => 3,
            Mode::Render => 4,
            Mode::Verify => 5,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SourceMode {
    Contiguous,
    Lines,
}

impl SourceMode {
    fn tag(self) -> u64 {
        match self {
            SourceMode::Contiguous => 1,
            SourceMode::Lines => 2,
        }
    }
}

struct Args {
    root: PathBuf,
    mode: Mode,
    source_mode: SourceMode,
    iterations: usize,
    bytes_limit: Option<usize>,
}

impl Args {
    fn is_default_profile(&self) -> bool {
        self.root == PathBuf::from(DEFAULT_ROOT)
            && self.mode == DEFAULT_MODE
            && self.source_mode == DEFAULT_SOURCE_MODE
            && self.iterations == DEFAULT_ITERATIONS
            && self.bytes_limit.is_none()
    }
}

struct CorpusFile {
    text: String,
    lines: Vec<Line>,
    line_len: u32,
}

struct Line {
    start: u32,
    end: u32,
}

struct LineSource<'a> {
    text: &'a str,
    lines: &'a [Line],
    len: u32,
    next_line: Cell<usize>,
}

impl Source for LineSource<'_> {
    fn len(&self) -> u32 {
        self.len
    }

    fn page(&self, offset: u32) -> (u32, &[u8]) {
        if offset >= self.len {
            return (self.len, &[]);
        }
        let index = self.lines.partition_point(|line| line.end < offset);
        let Some(line) = self.lines.get(index) else {
            return (self.len, &[]);
        };
        self.next_line.set(index + 1);
        self.page_from_line(offset, line)
    }

    fn next_page(&self, offset: u32) -> (u32, &[u8]) {
        if offset >= self.len {
            return (self.len, &[]);
        }
        let index = self.next_line.get();
        if let Some(line) = self.lines.get(index) {
            if line.start == offset {
                self.next_line.set(index + 1);
                return self.page_from_line(offset, line);
            }
        }
        self.page(offset)
    }
}

impl LineSource<'_> {
    fn page_from_line(&self, offset: u32, line: &Line) -> (u32, &[u8]) {
        let bytes = self.text.as_bytes();
        let text_len = bytes.len() as u32;
        if offset < text_len {
            let end = if line.end < text_len {
                line.end + 1
            } else {
                line.end
            };
            if offset < end {
                return (offset, &bytes[offset as usize..end as usize]);
            }
        }
        (offset, b"\n")
    }
}

fn new_checksum(tag: u64) -> FoldHasher<'static> {
    let mut checksum = FixedState::with_seed(CHECKSUM_SEED).build_hasher();
    checksum.write_u64(tag);
    checksum
}

#[inline]
fn pack_pair_u32(a: u32, b: u32) -> u64 {
    u64::from(a) | (u64::from(b) << 32)
}

#[inline]
fn pack_span(span: Span) -> u64 {
    pack_pair_u32(span.offset, span.len)
}

#[inline]
fn semantic_code(semantic: Option<SemanticKind>) -> u64 {
    semantic.map_or(31, |kind| kind as u64)
}

#[inline]
fn delimiter_code(delimiter: Option<u16>) -> u64 {
    delimiter.map_or(0x1_0000, u64::from)
}

#[inline]
fn pack_render_meta(
    tag: u64,
    local_kind: u16,
    lang_tag: u8,
    semantic: Option<SemanticKind>,
    delimiter: Option<u16>,
    class: u8,
) -> u64 {
    (tag & 0x0f)
        | (u64::from(local_kind) << 4)
        | (u64::from(lang_tag & 0x3f) << 20)
        | (semantic_code(semantic) << 26)
        | (delimiter_code(delimiter) << 31)
        | (u64::from(class) << 48)
}

#[inline]
fn pack_token_meta(
    local_kind: u16,
    lang_tag: u8,
    flags: u8,
    nest: u8,
    semantic: Option<SemanticKind>,
    delimiter: Option<u16>,
) -> u64 {
    2 | (u64::from(local_kind) << 4)
        | (u64::from(lang_tag & 0x3f) << 20)
        | (u64::from(flags) << 26)
        | (u64::from(nest) << 34)
        | (semantic_code(semantic) << 42)
        | (delimiter_code(delimiter) << 47)
}

#[inline]
fn pack_line_meta(class: u8, line_index: usize) -> u64 {
    3 | (u64::from(class) << 4) | ((line_index as u64) << 12)
}

fn main() {
    let args = parse_args();
    let files = load_corpus(&args.root, args.bytes_limit);
    if files.is_empty() {
        eprintln!("no .rs files found under {}", args.root.display());
        std::process::exit(2);
    }

    let bytes: usize = files.iter().map(|file| file.text.len()).sum();
    let lines: usize = files.iter().map(|file| file.lines.len()).sum();
    let start = Instant::now();
    let mut checksum = new_checksum(0x6265_6e63_685f_7273);
    checksum.write_u64(
        args.mode.tag()
            | (args.source_mode.tag() << 8)
            | ((args.iterations as u64) << 16)
            | ((files.len() as u64) << 48),
    );
    checksum.write_u64(pack_pair_u32(bytes as u32, lines as u32));
    for file in &files {
        checksum.write_u64(pack_pair_u32(
            file.text.len() as u32,
            file.lines.len() as u32,
        ));
    }
    for _ in 0..args.iterations {
        for file in &files {
            checksum.write_u64(run_file(file, args.mode, args.source_mode));
        }
    }
    let checksum = checksum.finish();
    let elapsed = start.elapsed();
    let total_bytes = bytes.saturating_mul(args.iterations);
    let mb = total_bytes as f64 / (1024.0 * 1024.0);
    let secs = elapsed.as_secs_f64();
    if args.is_default_profile() {
        println!(
            "mode={:?} source={:?} files={} lines={} bytes={} iterations={} elapsed_s={:.6} mib_s={:.2}",
            args.mode,
            args.source_mode,
            files.len(),
            lines,
            bytes,
            args.iterations,
            secs,
            mb / secs,
        );
    } else {
        println!(
            "mode={:?} source={:?} files={} lines={} bytes={} iterations={} elapsed_s={:.6} mib_s={:.2} checksum={}",
            args.mode,
            args.source_mode,
            files.len(),
            lines,
            bytes,
            args.iterations,
            secs,
            mb / secs,
            checksum
        );
    }
    assert_default_profile(&args, files.len(), lines, bytes, checksum);
}

fn assert_default_profile(args: &Args, files: usize, lines: usize, bytes: usize, checksum: u64) {
    if !args.is_default_profile() || DEFAULT_EXPECTED_CHECKSUM == 0 {
        return;
    }

    if files != DEFAULT_EXPECTED_FILES
        || lines != DEFAULT_EXPECTED_LINES
        || bytes != DEFAULT_EXPECTED_BYTES
    {
        eprintln!(
            "default benchmark input changed: files={files} lines={lines} bytes={bytes}; \
             expected files={DEFAULT_EXPECTED_FILES} lines={DEFAULT_EXPECTED_LINES} bytes={DEFAULT_EXPECTED_BYTES}",
        );
        std::process::exit(1);
    }
    if checksum != DEFAULT_EXPECTED_CHECKSUM {
        eprintln!(
            "CRITICAL SYNTAX BEHAVIOUR ERROR checksum changed on same input, new checksum={checksum}"
        );
        std::process::exit(1);
    }
}

fn run_file(file: &CorpusFile, mode: Mode, source_mode: SourceMode) -> u64 {
    match source_mode {
        SourceMode::Contiguous => {
            let src: &dyn Source = &file.text.as_str();
            run_source(file, src, mode)
        }
        SourceMode::Lines => {
            let source = LineSource {
                text: &file.text,
                lines: &file.lines,
                len: file.line_len,
                next_line: Cell::new(0),
            };
            run_source(file, &source, mode)
        }
    }
}

fn run_source(file: &CorpusFile, src: &dyn Source, mode: Mode) -> u64 {
    match mode {
        Mode::Lex => {
            let table = TokenTable::new(Language::Rust, src);
            let mut checksum = new_checksum(0x6c65_785f_7273);
            checksum.write_u64(pack_pair_u32(
                table.token_count() as u32,
                table.chunk_count() as u32,
            ));
            checksum.write_u64(table.source_len() as u64);
            checksum.finish()
        }
        Mode::Semantic => {
            let table = TokenTable::new(Language::Rust, src);
            let semantic = SemanticTable::new(&table, src);
            let mut checksum = new_checksum(0x7365_6d61_6e74_6963);
            checksum.write_u64(pack_pair_u32(
                semantic.token_count() as u32,
                semantic.chunk_count() as u32,
            ));
            checksum.write_u64(pack_pair_u32(
                table.token_count() as u32,
                table.source_len(),
            ));
            checksum.finish()
        }
        Mode::Rebuild => {
            let mut highlighter = Highlighter::new(Language::Rust);
            highlighter.rebuild(src);
            let source_len = highlighter.source_len().unwrap_or(0);
            let (token_count, chunk_count) =
                highlighter.table().map_or((0usize, 0usize), |table| {
                    (table.token_count(), table.chunk_count())
                });
            let semantic_count = highlighter
                .semantic()
                .map_or(0, |semantic| semantic.token_count());
            let delimiter_count = highlighter
                .delimiters()
                .map_or(0, |delimiters| delimiters.token_count());
            let mut checksum = new_checksum(0x7265_6275_696c_64);
            checksum.write_u64(pack_pair_u32(token_count as u32, chunk_count as u32));
            checksum.write_u64(pack_pair_u32(semantic_count as u32, delimiter_count as u32));
            checksum.write_u64(source_len as u64);
            checksum.finish()
        }
        Mode::Render => run_exgit_render(src, &file.lines),
        Mode::Verify => {
            let mut highlighter = Highlighter::new(Language::Rust);
            highlighter.rebuild(src);
            let len = highlighter.source_len().unwrap_or(0);
            let full = Span::new(0, len);
            let mut checksum = new_checksum(0x7665_7269_6679);
            for span in RenderSpans::new(&highlighter, full) {
                checksum.write_u64(pack_span(span.span));
                checksum.write_u64(pack_render_meta(
                    1,
                    span.local_kind,
                    span.lang_tag,
                    span.semantic,
                    span.delimiter,
                    0,
                ));
            }
            let fallback;
            let table = match highlighter.table() {
                Some(table) => table,
                None => {
                    fallback = TokenTable::new(Language::Rust, src);
                    &fallback
                }
            };
            let mut overlays = Overlays::new(&highlighter, full);
            for token in table.query(full) {
                let (semantic, delimiter) = overlays.at(token.span.offset);
                checksum.write_u64(pack_span(token.span));
                checksum.write_u64(pack_token_meta(
                    token.local_kind(),
                    token.lang_tag(),
                    token.flags,
                    token.nest,
                    semantic,
                    delimiter,
                ));
            }
            checksum.finish()
        }
    }
}

fn run_exgit_render(src: &dyn Source, lines: &[Line]) -> u64 {
    let mut highlighter = Highlighter::new(Language::Rust);
    highlighter.rebuild(src);
    let Some(source_len) = highlighter.source_len() else {
        return 0;
    };
    let all = Span::new(0, source_len);
    let mut line_index = 0usize;
    let mut checksum = new_checksum(0x6578_6769_745f_7273);
    checksum.write_u64(source_len as u64);

    if let Some(spans) = highlighter.cached_render(all) {
        for &render_span in spans {
            render_exgit_span(render_span, lines, &mut line_index, &mut checksum);
        }
    } else {
        for render_span in highlighter.render(all) {
            render_exgit_span(render_span, lines, &mut line_index, &mut checksum);
        }
    }

    checksum.finish()
}

#[inline(always)]
fn render_exgit_span(
    render_span: RenderSpan,
    lines: &[Line],
    line_index: &mut usize,
    checksum: &mut FoldHasher<'static>,
) {
    let class = render_span_class(render_span);
    let start = render_span.span.offset;
    let end = render_span.span.end();

    while *line_index < lines.len() && lines[*line_index].end <= start {
        *line_index += 1;
    }

    if let Some(range) = lines.get(*line_index) {
        if start < range.end && end <= range.end {
            checksum.write_u64(pack_line_meta(class, *line_index));
            checksum.write_u64(pack_pair_u32(start - range.start, end - range.start));
            return;
        }
    }

    let mut current = *line_index;
    while current < lines.len() && lines[current].start < end {
        let range = &lines[current];
        let span_start = start.max(range.start) - range.start;
        let span_end = end.min(range.end) - range.start;
        if span_start < span_end {
            checksum.write_u64(pack_line_meta(class, current));
            checksum.write_u64(pack_pair_u32(span_start, span_end));
        }
        current += 1;
    }
}

#[inline(always)]
fn render_span_class(span: RenderSpan) -> u8 {
    if span.delimiter.is_some() {
        return 15;
    }

    match span.local_kind {
        kind::STRING
        | kind::TEMPLATE_STRING
        | kind::REGEX
        | kind::CHAR
        | kind::CDATA
        | kind::CODE_INLINE
        | kind::CODE_FENCE
        | kind::CODE_BLOCK => 1,
        kind::NUMBER => 2,
        kind::KEYWORD => match span.semantic {
            Some(SemanticKind::TypeDefinition | SemanticKind::TypeName) => 10,
            _ => 3,
        },
        kind::DOCTYPE | kind::AT_KEYWORD => 3,
        kind::COMMENT | kind::DOC_COMMENT => 4,
        kind::ERROR => 5,
        kind::TAG_NAME | kind::ATTR_NAME => 6,
        kind::ENTITY_REF | kind::HASH_TOKEN => 7,
        kind::HEADING_MARKER | kind::HEADING_TEXT => 8,
        kind::LINK_URL | kind::LINK_TEXT => 9,
        _ => match span.semantic {
            Some(semantic) => semantic_class(semantic),
            None => 14,
        },
    }
}

#[inline(always)]
fn semantic_class(kind: SemanticKind) -> u8 {
    match kind {
        SemanticKind::TypeDefinition | SemanticKind::TypeName => 10,
        SemanticKind::FunctionDefinition
        | SemanticKind::FunctionCall
        | SemanticKind::MethodDefinition
        | SemanticKind::MethodCall
        | SemanticKind::MacroCall => 11,
        SemanticKind::Parameter => 12,
        SemanticKind::Argument => 13,
        SemanticKind::VariableDefinition | SemanticKind::Lifetime => 14,
        SemanticKind::FieldDefinition | SemanticKind::Field => 16,
        SemanticKind::PathComponent => 17,
        SemanticKind::Variable | SemanticKind::FieldAccess | SemanticKind::MetaVariable => 14,
    }
}

fn load_corpus(root: &Path, bytes_limit: Option<usize>) -> Vec<CorpusFile> {
    let mut paths = Vec::new();
    collect_rs_files(root, &mut paths);
    paths.sort();

    let mut files = Vec::new();
    let mut total = 0usize;
    for path in paths {
        let Ok(text) = fs::read_to_string(&path) else {
            continue;
        };
        if text.len() > u32::MAX as usize {
            continue;
        }
        if let Some(limit) = bytes_limit {
            if total >= limit {
                break;
            }
            if total + text.len() > limit {
                continue;
            }
        }
        let (lines, line_len) = build_lines(&text);
        total += text.len();
        files.push(CorpusFile {
            text,
            lines,
            line_len,
        });
    }
    files
}

fn collect_rs_files(path: &Path, out: &mut Vec<PathBuf>) {
    let Ok(meta) = fs::metadata(path) else {
        return;
    };
    if meta.is_file() {
        if path.extension().and_then(|ext| ext.to_str()) == Some("rs") {
            out.push(path.to_path_buf());
        }
        return;
    }
    if !meta.is_dir() {
        return;
    }
    let Ok(entries) = fs::read_dir(path) else {
        return;
    };
    for entry in entries.flatten() {
        let file_name = entry.file_name();
        let name = file_name.to_string_lossy();
        if name == "target" || name == ".git" {
            continue;
        }
        collect_rs_files(&entry.path(), out);
    }
}

fn build_lines(text: &str) -> (Vec<Line>, u32) {
    let mut lines = Vec::new();
    let mut start = 0u32;
    for line in text.split('\n') {
        let len = line.len() as u32;
        let end = start + len;
        lines.push(Line { start, end });
        start = end + 1;
    }
    (lines, start)
}

fn parse_args() -> Args {
    let mut root = PathBuf::from(DEFAULT_ROOT);
    let mut mode = DEFAULT_MODE;
    let mut source_mode = DEFAULT_SOURCE_MODE;
    let mut iterations = DEFAULT_ITERATIONS;
    let mut bytes_limit = None;

    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--root" => root = PathBuf::from(next_value(&mut args, "--root")),
            "--mode" => {
                mode = match next_value(&mut args, "--mode").as_str() {
                    "lex" => Mode::Lex,
                    "semantic" => Mode::Semantic,
                    "rebuild" => Mode::Rebuild,
                    "render" => Mode::Render,
                    "verify" => Mode::Verify,
                    value => usage(&format!("unknown --mode {value:?}")),
                }
            }
            "--source" => {
                source_mode = match next_value(&mut args, "--source").as_str() {
                    "contiguous" => SourceMode::Contiguous,
                    "lines" => SourceMode::Lines,
                    value => usage(&format!("unknown --source {value:?}")),
                }
            }
            "--iterations" | "-i" => {
                iterations = next_value(&mut args, "--iterations")
                    .parse()
                    .unwrap_or_else(|_| usage("--iterations must be a positive integer"));
                if iterations == 0 {
                    usage("--iterations must be greater than zero");
                }
            }
            "--bytes-limit" => {
                bytes_limit = Some(
                    next_value(&mut args, "--bytes-limit")
                        .parse()
                        .unwrap_or_else(|_| usage("--bytes-limit must be a positive integer")),
                );
            }
            "-h" | "--help" => usage_success(),
            _ => usage(&format!("unknown argument {arg:?}")),
        }
    }

    Args {
        root,
        mode,
        source_mode,
        iterations,
        bytes_limit,
    }
}

fn next_value(args: &mut impl Iterator<Item = String>, flag: &str) -> String {
    args.next()
        .unwrap_or_else(|| usage(&format!("{flag} requires a value")))
}

fn usage(message: &str) -> ! {
    eprintln!("{message}");
    usage_success();
}

fn usage_success() -> ! {
    eprintln!(
        "usage: cargo run --release --example rust_semantic_bench -- \\
         [--root /code/toml-spanner] [--mode lex|semantic|rebuild|render|verify] \\
         [--source contiguous|lines] [--iterations 150] [--bytes-limit N]"
    );
    std::process::exit(2);
}
