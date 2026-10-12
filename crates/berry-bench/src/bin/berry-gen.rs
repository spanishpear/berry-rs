//! `berry-gen`: emit a deterministic synthetic yarn berry (v8) lockfile.
//!
//! ```text
//! cargo run --release -p berry-bench --bin berry-gen            # ~24 MB default
//! cargo run --release -p berry-bench --bin berry-gen -- --size 1MB --output /tmp/small.lock
//! cargo run --release -p berry-bench --bin berry-gen -- --entries 500
//! ```

use berry_bench::synth::{Profile, Target, generate, parse_size};
use clap::Parser;
use std::path::PathBuf;

/// Default location (relative to the workspace root) of the large fixture.
pub const DEFAULT_OUTPUT: &str = "target/bench-fixtures/large-24mb.yarn.lock";

#[derive(Parser, Debug)]
#[command(name = "berry-gen")]
#[command(
  about = "Generate a deterministic synthetic yarn berry v8 lockfile (large-monorepo profile)"
)]
struct Args {
  /// Approximate output size, e.g. 24MB, 512KiB (default 24MB when --entries is not given)
  #[arg(long, conflicts_with = "entries")]
  size: Option<String>,

  /// Exact number of lockfile entries to emit
  #[arg(long)]
  entries: Option<usize>,

  /// RNG seed; the same seed and size always produce the same bytes
  #[arg(long, default_value_t = 0x00BE_FF11)]
  seed: u64,

  /// Output path (default: <workspace>/target/bench-fixtures/large-24mb.yarn.lock)
  #[arg(short, long)]
  output: Option<PathBuf>,
}

fn main() {
  let args = Args::parse();

  let target = match (args.entries, &args.size) {
    (Some(n), _) => Target::Entries(n),
    (None, Some(s)) => Target::Bytes(parse_size(s).unwrap_or_else(|e| {
      eprintln!("berry-gen: {e}");
      std::process::exit(2);
    })),
    (None, None) => Target::Bytes(24_000_000),
  };

  let output = args.output.unwrap_or_else(|| {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .parent()
      .and_then(|p| p.parent())
      .expect("workspace root")
      .join(DEFAULT_OUTPUT)
  });

  let (text, summary) = generate(args.seed, target, &Profile::default());

  if let Some(parent) = output.parent() {
    std::fs::create_dir_all(parent).expect("create output directory");
  }
  std::fs::write(&output, &text).expect("write lockfile");
  eprintln!(
    "berry-gen: wrote {} ({} bytes, {} entries, {} descriptors, {} lines, seed {})",
    output.display(),
    summary.bytes,
    summary.entries,
    summary.descriptors,
    summary.lines,
    args.seed
  );
}
