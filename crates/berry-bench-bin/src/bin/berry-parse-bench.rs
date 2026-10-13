//! `berry-parse-bench`: parse-only timing of one lockfile.
//!
//! The file is read into memory once, then the selected parse API is timed
//! for N iterations (the parsed result is dropped *outside* the timed region
//! and the drop is timed separately). `--include-read` additionally times
//! `fs::read_to_string + parse`. `--stages` splits the work of one consumer
//! into the parse itself, a walk over every view (every entry, every field,
//! every list element), the canonical dump, and the drop.
//! `--api` selects what is timed: `berry::parse` with fresh buffers (`str`,
//! default), `berry::parse_with` with recycled buffers (`reuse`, zero
//! allocations in steady state) or `berry::parse_bytes` (`bytes`, UTF-8
//! validation included).
//!
//! ```text
//! cargo run --release -p berry-bench-bin --bin berry-parse-bench -- path/to/yarn.lock
//! cargo run --release -p berry-bench-bin --features alloc-count --bin berry-parse-bench -- path --alloc
//! ```

use berry::{Buffers, Lockfile};
use clap::{Parser, ValueEnum};
use std::fmt::Write as _;
use std::hint::black_box;
use std::path::PathBuf;
use std::time::Instant;

#[cfg(feature = "alloc-count")]
mod counting {
  use std::alloc::{GlobalAlloc, Layout, System};
  use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};

  pub struct Counting;

  static ALLOCS: AtomicUsize = AtomicUsize::new(0);
  static REALLOCS: AtomicUsize = AtomicUsize::new(0);
  static FREES: AtomicUsize = AtomicUsize::new(0);
  static BYTES: AtomicUsize = AtomicUsize::new(0);
  static LIVE: AtomicUsize = AtomicUsize::new(0);
  static PEAK: AtomicUsize = AtomicUsize::new(0);

  fn add_live(n: usize) {
    let live = LIVE.fetch_add(n, Relaxed) + n;
    PEAK.fetch_max(live, Relaxed);
  }

  // SAFETY: every method forwards unchanged arguments to `System`, which
  // upholds the `GlobalAlloc` contract; the counters are only bookkeeping and
  // never touch the returned memory.
  unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
      ALLOCS.fetch_add(1, Relaxed);
      BYTES.fetch_add(layout.size(), Relaxed);
      add_live(layout.size());
      // SAFETY: same layout the caller gave us.
      unsafe { System.alloc(layout) }
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
      ALLOCS.fetch_add(1, Relaxed);
      BYTES.fetch_add(layout.size(), Relaxed);
      add_live(layout.size());
      // SAFETY: same layout the caller gave us.
      unsafe { System.alloc_zeroed(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
      FREES.fetch_add(1, Relaxed);
      LIVE.fetch_sub(layout.size(), Relaxed);
      // SAFETY: `ptr`/`layout` are exactly what the caller got from `alloc`.
      unsafe { System.dealloc(ptr, layout) }
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
      REALLOCS.fetch_add(1, Relaxed);
      if new_size >= layout.size() {
        BYTES.fetch_add(new_size - layout.size(), Relaxed);
        add_live(new_size - layout.size());
      } else {
        LIVE.fetch_sub(layout.size() - new_size, Relaxed);
      }
      // SAFETY: arguments are forwarded unchanged from the caller.
      unsafe { System.realloc(ptr, layout, new_size) }
    }
  }

  #[derive(Clone, Copy, Debug, Default)]
  pub struct Snapshot {
    pub allocs: usize,
    pub reallocs: usize,
    pub frees: usize,
    pub bytes: usize,
    pub live: usize,
    pub peak: usize,
  }

  /// Reset counters; peak is re-based on the current live size.
  pub fn reset() {
    ALLOCS.store(0, Relaxed);
    REALLOCS.store(0, Relaxed);
    FREES.store(0, Relaxed);
    BYTES.store(0, Relaxed);
    PEAK.store(LIVE.load(Relaxed), Relaxed);
  }

  pub fn snapshot() -> Snapshot {
    Snapshot {
      allocs: ALLOCS.load(Relaxed),
      reallocs: REALLOCS.load(Relaxed),
      frees: FREES.load(Relaxed),
      bytes: BYTES.load(Relaxed),
      live: LIVE.load(Relaxed),
      peak: PEAK.load(Relaxed),
    }
  }
}

#[cfg(feature = "alloc-count")]
#[global_allocator]
static GLOBAL: counting::Counting = counting::Counting;

/// `--features mimalloc`: time under afm-tools' allocator.
#[cfg(all(feature = "mimalloc", not(feature = "alloc-count")))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[derive(Parser, Debug)]
#[allow(clippy::struct_excessive_bools)] // independent CLI switches
#[command(name = "berry-parse-bench")]
#[command(about = "Time berry::parse on one lockfile (file read once, parse only by default)")]
struct Args {
  /// Lockfile to parse
  #[arg(value_name = "LOCKFILE")]
  lockfile: PathBuf,

  /// Timed iterations
  #[arg(short = 'n', long, default_value_t = 20)]
  iters: usize,

  /// Untimed warmup iterations
  #[arg(short, long, default_value_t = 3)]
  warmup: usize,

  /// Also time `read_to_string + parse` for the same number of iterations
  #[arg(long)]
  include_read: bool,

  /// Print a per-stage breakdown (parse, walk every view, dump, drop)
  #[arg(long)]
  stages: bool,

  /// Report allocation counts of one parse (needs `--features alloc-count`)
  #[arg(long)]
  alloc: bool,

  /// Emit one JSON object instead of text
  #[arg(long)]
  json: bool,

  /// Which parse API to time (and to count allocations for)
  #[arg(long, value_enum, default_value_t = Api::Str)]
  api: Api,

  /// Size of the rayon global pool (default: `RAYON_NUM_THREADS` or one
  /// thread per CPU); 1 makes every parse sequential
  #[arg(long)]
  threads: Option<usize>,

  /// Print table totals (entries, descriptors, dependency lines, ...) and
  /// bytes per row, then exit
  #[arg(long)]
  table_stats: bool,

  /// Testing: time `berry::__parse_chunked` with chunks of about this many
  /// bytes instead of the selected API (fresh buffers)
  #[arg(long, value_name = "BYTES")]
  chunk_target: Option<usize>,

  /// Read, parse once and exit with no sanity pass or warmup. Intended as the
  /// process-level target for `hyperfine`.
  #[arg(long, conflicts_with_all = ["include_read", "stages", "alloc", "json"])]
  once: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
enum Api {
  /// `berry::parse(&str)` (fresh buffers every call)
  Str,
  /// `berry::parse_with` with buffers recycled between iterations
  Reuse,
  /// `berry::parse_bytes` (UTF-8 validation + parse) on the file's bytes
  Bytes,
  /// `berry::parse_file` on the path: open, map or read, validate, parse
  /// (the timed region includes the I/O; the drop includes the unmap)
  File,
}

#[derive(Debug, Clone, Copy, serde::Serialize)]
struct Stats {
  min_ms: f64,
  median_ms: f64,
  mean_ms: f64,
  max_ms: f64,
  mb_per_s_median: f64,
  mb_per_s_best: f64,
}

fn stats(samples: &mut [f64], bytes: usize) -> Stats {
  samples.sort_by(f64::total_cmp);
  let n = samples.len();
  let median = if n % 2 == 1 {
    samples[n / 2]
  } else {
    f64::midpoint(samples[n / 2 - 1], samples[n / 2])
  };
  let mean = samples.iter().sum::<f64>() / n as f64;
  let mb = bytes as f64 / 1_000_000.0;
  Stats {
    min_ms: samples[0] * 1e3,
    median_ms: median * 1e3,
    mean_ms: mean * 1e3,
    max_ms: samples[n - 1] * 1e3,
    mb_per_s_median: mb / median,
    mb_per_s_best: mb / samples[0],
  }
}

/// Peak resident set size in bytes (Linux `VmHWM`), if available.
fn peak_rss_bytes() -> Option<u64> {
  let status = std::fs::read_to_string("/proc/self/status").ok()?;
  let line = status.lines().find(|l| l.starts_with("VmHWM:"))?;
  let kb: u64 = line.split_whitespace().nth(1)?.parse().ok()?;
  Some(kb * 1024)
}

fn time_parse(text: &str, args: &Args, iters: usize) -> (Vec<f64>, Vec<f64>) {
  let (api, chunk) = (args.api, args.chunk_target);
  let mut parse = Vec::with_capacity(iters);
  let mut drop_t = Vec::with_capacity(iters);
  let mut buffers = Buffers::new();
  for _ in 0..iters {
    let t = Instant::now();
    if let Some(target) = chunk {
      let res = black_box(berry::__parse_chunked(black_box(text), target));
      parse.push(t.elapsed().as_secs_f64());
      let t = Instant::now();
      drop(res);
      drop_t.push(t.elapsed().as_secs_f64());
      continue;
    }
    match api {
      Api::Str => {
        let res = black_box(berry::parse(black_box(text)));
        parse.push(t.elapsed().as_secs_f64());
        let t = Instant::now();
        drop(res);
        drop_t.push(t.elapsed().as_secs_f64());
      }
      Api::Bytes => {
        let res = black_box(berry::parse_bytes(black_box(text.as_bytes())));
        parse.push(t.elapsed().as_secs_f64());
        let t = Instant::now();
        drop(res);
        drop_t.push(t.elapsed().as_secs_f64());
      }
      Api::File => {
        let res = black_box(berry::parse_file(&args.lockfile));
        parse.push(t.elapsed().as_secs_f64());
        let t = Instant::now();
        drop(res);
        drop_t.push(t.elapsed().as_secs_f64());
      }
      Api::Reuse => {
        let res = black_box(berry::parse_with(black_box(text), &mut buffers));
        parse.push(t.elapsed().as_secs_f64());
        let t = Instant::now();
        if let Ok(lockfile) = res {
          buffers = lockfile.into_buffers();
        }
        drop_t.push(t.elapsed().as_secs_f64());
      }
    }
  }
  (parse, drop_t)
}

/// Touch every field of every entry through the views (what a consumer that
/// reads everything pays on top of the parse); returns a checksum.
fn walk(lockfile: &Lockfile<'_>) -> usize {
  let mut acc = lockfile.metadata().version.len() + lockfile.metadata().cache_key.len();
  for e in lockfile {
    for d in e.descriptors() {
      acc += d.ident().name().len() + d.range().len();
    }
    acc += e.version().map_or(0, str::len) + e.resolution().map_or(0, str::len);
    acc += e.resolution_locator().map_or(0, |l| l.reference().len());
    acc += e.checksum().map_or(0, str::len) + e.conditions().map_or(0, str::len);
    acc += e.language_name().len() + e.link_type() as usize;
    for d in e.dependencies().iter().chain(e.peer_dependencies().iter()) {
      acc += d.ident().name().len() + d.range().len();
    }
    for (k, v) in e.bin() {
      acc += k.len() + v.len();
    }
    for (k, m) in e.dependencies_meta() {
      acc += k.name().len() + usize::from(m.built == Some(true));
    }
    for (k, m) in e.peer_dependencies_meta() {
      acc += k.name().len() + usize::from(m.optional);
    }
  }
  acc
}

#[derive(Debug, Default, serde::Serialize)]
struct StageTimes {
  /// `berry::parse`: preamble + scan of every entry into the span tables
  parse_ms: f64,
  /// reading every field of every entry through the views
  walk_ms: f64,
  /// writing the canonical dump (sorted, last-wins maps) to a `String`
  dump_ms: f64,
  drop_ms: f64,
  entries: usize,
}

/// Split one consumer run into stages (best of `reps` runs each).
fn stage_times(text: &str, reps: usize) -> StageTimes {
  let mut best = StageTimes {
    parse_ms: f64::MAX,
    walk_ms: f64::MAX,
    dump_ms: f64::MAX,
    drop_ms: f64::MAX,
    entries: 0,
  };
  for _ in 0..reps {
    let t = Instant::now();
    let Ok(lockfile) = black_box(berry::parse(black_box(text))) else {
      return StageTimes::default();
    };
    best.parse_ms = best.parse_ms.min(t.elapsed().as_secs_f64() * 1e3);
    let t = Instant::now();
    black_box(walk(black_box(&lockfile)));
    best.walk_ms = best.walk_ms.min(t.elapsed().as_secs_f64() * 1e3);
    let t = Instant::now();
    black_box(berry_test::canonical::to_string(&lockfile));
    best.dump_ms = best.dump_ms.min(t.elapsed().as_secs_f64() * 1e3);
    best.entries = lockfile.len();
    let t = Instant::now();
    drop(lockfile);
    best.drop_ms = best.drop_ms.min(t.elapsed().as_secs_f64() * 1e3);
  }
  best
}

#[cfg(feature = "alloc-count")]
#[derive(Debug, serde::Serialize)]
struct AllocReport {
  allocs: usize,
  reallocs: usize,
  frees: usize,
  bytes_allocated: usize,
  peak_live_bytes_during_parse: usize,
  retained_bytes_after_parse: usize,
  /// Span tables that outgrew their reserved window (`Lockfile::__spills`).
  spilled_tables: usize,
}

#[cfg(feature = "alloc-count")]
fn alloc_report(text: &str, args: &Args) -> AllocReport {
  let (api, chunk) = (args.api, args.chunk_target);
  // For reuse, warm the buffers first: the steady state is measured.
  let mut buffers = Buffers::new();
  if api == Api::Reuse
    && let Ok(lockfile) = berry::parse_with(text, &mut buffers)
  {
    buffers = lockfile.into_buffers();
  }
  let before = counting::snapshot().live;
  counting::reset();
  let res = match (chunk, api) {
    (Some(target), _) => black_box(berry::__parse_chunked(black_box(text), target)),
    (None, Api::Str) => black_box(berry::parse(black_box(text))),
    (None, Api::Reuse) => black_box(berry::parse_with(black_box(text), &mut buffers)),
    (None, Api::Bytes) => black_box(berry::parse_bytes(black_box(text.as_bytes()))),
    (None, Api::File) => {
      let res = berry::parse_file_with(&args.lockfile, &mut buffers);
      let s = counting::snapshot();
      let spills = res.as_ref().map_or(0, berry::Lockfile::__spills);
      drop(res);
      return report(s, before, spills);
    }
  };
  let s = counting::snapshot();
  let spills = res.as_ref().map_or(0, berry::Lockfile::__spills);
  drop(res);
  report(s, before, spills)
}

#[cfg(feature = "alloc-count")]
const fn report(s: counting::Snapshot, before: usize, spills: usize) -> AllocReport {
  AllocReport {
    allocs: s.allocs,
    reallocs: s.reallocs,
    frees: s.frees,
    bytes_allocated: s.bytes,
    peak_live_bytes_during_parse: s.peak - before,
    retained_bytes_after_parse: s.live - before,
    spilled_tables: spills,
  }
}

#[allow(clippy::too_many_lines)] // straight-line CLI glue and reporting
fn main() {
  let args = Args::parse();
  assert!(args.iters > 0, "--iters must be > 0");
  if let Some(n) = args.threads
    && let Err(e) = rayon::ThreadPoolBuilder::new()
      .num_threads(n)
      .build_global()
  {
    eprintln!("cannot configure the rayon pool: {e}");
    std::process::exit(2);
  }

  if args.once && args.api == Api::File {
    // The process-level target for `hyperfine`: open, map/read, parse, exit.
    match berry::parse_file(&args.lockfile) {
      Ok(lf) => {
        black_box(&lf);
        if !lf.remaining().trim().is_empty() {
          eprintln!("WARNING: {} bytes remaining unparsed", lf.remaining().len());
        }
      }
      Err(e) => {
        eprintln!("{e}");
        std::process::exit(1);
      }
    }
    return;
  }

  let t = Instant::now();
  let text = std::fs::read_to_string(&args.lockfile).unwrap_or_else(|e| {
    eprintln!("failed to read {}: {e}", args.lockfile.display());
    std::process::exit(2);
  });
  let first_read_ms = t.elapsed().as_secs_f64() * 1e3;
  let bytes = text.len();

  if args.table_stats {
    let Ok(lf) = berry::parse(&text) else {
      eprintln!("parse error");
      std::process::exit(1);
    };
    let mut t = [0usize; 7];
    for e in &lf {
      t[0] += 1;
      t[1] += e.descriptors().len();
      t[2] += e.dependencies().len();
      t[3] += e.peer_dependencies().len();
      t[4] += e.bin().len();
      t[5] += e.dependencies_meta().len();
      t[6] += e.peer_dependencies_meta().len();
    }
    let names = [
      "entries",
      "descriptors",
      "deps",
      "peers",
      "bins",
      "dmeta",
      "pmeta",
    ];
    for (n, c) in names.iter().zip(t) {
      println!(
        "{n}: {c} rows, {:.1} bytes per row",
        bytes as f64 / c.max(1) as f64
      );
    }
    return;
  }

  if args.once {
    let Ok(lf) = berry::parse(black_box(&text)) else {
      eprintln!("parse error");
      std::process::exit(1);
    };
    let rest = lf.remaining();
    if !rest.trim().is_empty() {
      eprintln!("WARNING: {} bytes remaining unparsed", rest.len());
    }
    black_box(&lf);
    drop(lf);
    return;
  }

  // Sanity check once, outside timing: report incomplete parses loudly.
  let (leftover, entries) = match berry::parse(&text) {
    Ok(lf) => (lf.remaining().trim().len(), lf.len()),
    Err(e) => {
      eprintln!("parse error: {e}");
      std::process::exit(1);
    }
  };

  if args.warmup > 0 {
    drop(time_parse(&text, &args, args.warmup));
  }

  let (mut parse_s, mut drop_s) = time_parse(&text, &args, args.iters);
  let parse = stats(&mut parse_s, bytes);
  let drop_stats = stats(&mut drop_s, bytes);

  let read_parse = args.include_read.then(|| {
    let mut samples = Vec::with_capacity(args.iters);
    for _ in 0..args.iters {
      let t = Instant::now();
      let s = std::fs::read_to_string(&args.lockfile).expect("read");
      let res = black_box(berry::parse(black_box(&s)));
      samples.push(t.elapsed().as_secs_f64());
      drop(res);
      drop(s);
    }
    stats(&mut samples, bytes)
  });

  let stages = args.stages.then(|| stage_times(&text, args.iters.min(10)));

  #[cfg(feature = "alloc-count")]
  let allocs = args.alloc.then(|| alloc_report(&text, &args));
  #[cfg(not(feature = "alloc-count"))]
  if args.alloc {
    eprintln!("--alloc requires building with --features alloc-count");
    std::process::exit(2);
  }

  let rss = peak_rss_bytes();

  if args.json {
    let mut obj = serde_json::json!({
      "file_bytes": bytes,
      "api": args.api,
      "threads": rayon::current_num_threads(),
      "entries": entries,
      "unparsed_non_whitespace_bytes": leftover,
      "iters": args.iters,
      "initial_read_ms": first_read_ms,
      "parse": parse,
      "drop": drop_stats,
      "peak_rss_bytes": rss,
    });
    if let Some(rp) = &read_parse {
      obj["read_plus_parse"] = serde_json::json!(rp);
    }
    if let Some(s) = &stages {
      obj["stages"] = serde_json::json!(s);
    }
    #[cfg(feature = "alloc-count")]
    if let Some(a) = &allocs {
      obj["alloc"] = serde_json::json!(a);
    }
    println!("{obj}");
    return;
  }

  let mut out = String::new();
  let _ = writeln!(
    out,
    "file: {} ({bytes} bytes, {entries} entries, {leftover} unparsed bytes)",
    args.lockfile.display()
  );
  let _ = writeln!(
    out,
    "parse only ({:?} API, {} iters, drop excluded): min {:.3} ms  median {:.3} ms  mean {:.3} ms  max {:.3} ms  | {:.1} MB/s median, {:.1} MB/s best",
    args.api,
    args.iters,
    parse.min_ms,
    parse.median_ms,
    parse.mean_ms,
    parse.max_ms,
    parse.mb_per_s_median,
    parse.mb_per_s_best
  );
  let _ = writeln!(
    out,
    "drop of result:                          min {:.3} ms  median {:.3} ms  mean {:.3} ms",
    drop_stats.min_ms, drop_stats.median_ms, drop_stats.mean_ms
  );
  if let Some(rp) = read_parse {
    let _ = writeln!(
      out,
      "read+parse:                              min {:.3} ms  median {:.3} ms  mean {:.3} ms  | {:.1} MB/s median",
      rp.min_ms, rp.median_ms, rp.mean_ms, rp.mb_per_s_median
    );
  }
  let _ = writeln!(
    out,
    "first read_to_string (cold-ish): {first_read_ms:.3} ms"
  );
  if let Some(s) = stages {
    let _ = writeln!(
      out,
      "stages (best of run, {} entries): parse {:.3} ms, walk every view {:.3} ms, canonical dump {:.3} ms, drop {:.3} ms",
      s.entries, s.parse_ms, s.walk_ms, s.dump_ms, s.drop_ms
    );
  }
  #[cfg(feature = "alloc-count")]
  if let Some(a) = allocs {
    let _ = writeln!(
      out,
      "allocations: {} allocs + {} reallocs, {} frees, {} bytes allocated, peak live {} bytes, retained {} bytes, {} spilled tables",
      a.allocs,
      a.reallocs,
      a.frees,
      a.bytes_allocated,
      a.peak_live_bytes_during_parse,
      a.retained_bytes_after_parse,
      a.spilled_tables
    );
  }
  if let Some(r) = rss {
    let _ = writeln!(out, "peak RSS: {:.1} MiB", r as f64 / 1_048_576.0);
  }
  print!("{out}");
}

#[cfg(all(test, feature = "alloc-count"))]
mod tests {
  use super::counting;

  // Other test threads may allocate concurrently, so only lower bounds are
  // asserted.
  #[test]
  fn counting_allocator_sees_alloc_realloc_and_free() {
    let before = counting::snapshot();
    let mut v: Vec<u8> = Vec::with_capacity(1 << 20);
    v.extend(std::iter::repeat_n(7u8, 1 << 20));
    v.reserve(1 << 20); // forces a realloc
    v.push(1);
    let mid = counting::snapshot();
    assert!(mid.allocs > before.allocs);
    assert!(mid.reallocs > before.reallocs);
    assert!(mid.bytes >= before.bytes + (2 << 20));
    assert!(mid.peak >= 2 << 20);
    drop(v);
    assert!(counting::snapshot().frees > mid.frees);
  }

  #[test]
  fn parse_roundtrip_has_no_leftover_with_alloc_counting() {
    let text = "# This file is generated by running \"yarn install\" inside your project.\n# Manual changes might be lost - proceed with caution!\n\n__metadata:\n  version: 8\n  cacheKey: 10\n\n\"a@npm:^1\":\n  version: 1.0.0\n  resolution: \"a@npm:1.0.0\"\n  languageName: node\n  linkType: hard\n";
    let lf = berry::parse(text).expect("parse");
    assert!(lf.remaining().trim().is_empty());
    assert_eq!(lf.len(), 1);
  }
}
