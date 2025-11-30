use berry::parse::parse_lockfile;
use berry_test::{load_fixture, load_fixture_from_path};
use clap::Parser;
use memory_stats::memory_stats;
use std::collections::HashMap;
use std::fs;
use std::hint::black_box;
use std::path::{Path, PathBuf};
use std::time::Instant;

#[derive(Parser)]
#[command(name = "berry-bench")]
#[command(about = "Quick benchmarking tool for berry lockfile parser")]
struct Args {
  /// Fixture file to benchmark
  #[arg(short, long)]
  fixture: Option<String>,

  /// Path to a lockfile to benchmark
  #[arg(long = "fixture-path", value_name = "PATH", conflicts_with_all = ["fixture", "all"])]
  fixture_path: Option<PathBuf>,

  /// Benchmark all fixtures
  #[arg(short, long)]
  all: bool,

  /// Output format (json, text)
  #[arg(long, default_value = "text")]
  format: String,

  /// Number of warmup runs
  #[arg(short, long, default_value = "3")]
  warmup: usize,

  /// Number of benchmark runs
  #[arg(short, long, default_value = "10")]
  runs: usize,

  /// Show detailed timing for each run
  #[arg(short, long)]
  verbose: bool,

  /// Path to a baseline JSON file to compare against
  #[arg(long)]
  baseline: Option<String>,

  /// Save current results as a baseline JSON file
  #[arg(long)]
  save_baseline: Option<String>,

  /// Allowed slowdown vs baseline for ms/KiB (e.g., 0.05 for 5%)
  #[arg(long, default_value = "0.05")]
  threshold_ratio_ms_per_kib: f64,

  /// Fail the process with non-zero exit code if a regression is detected
  #[arg(long)]
  fail_on_regression: bool,
}

#[derive(Clone)]
enum FixtureSource {
  FixtureName(String),
  ArbitraryPath(PathBuf),
}

#[derive(Clone)]
struct FixtureTarget {
  label: String,
  source: FixtureSource,
}

impl FixtureTarget {
  fn from_fixture_name(name: impl Into<String>) -> Self {
    let name = name.into();
    Self {
      label: name.clone(),
      source: FixtureSource::FixtureName(name),
    }
  }

  fn from_path(path: impl Into<PathBuf>) -> Self {
    let path = path.into();
    let label = path.file_name().and_then(|name| name.to_str()).map_or_else(
      || path.display().to_string(),
      std::string::ToString::to_string,
    );
    Self {
      label,
      source: FixtureSource::ArbitraryPath(path),
    }
  }

  fn source_path(&self) -> Option<&Path> {
    match &self.source {
      FixtureSource::ArbitraryPath(path) => Some(path.as_path()),
      FixtureSource::FixtureName(_) => None,
    }
  }

  fn load_contents(&self) -> String {
    match &self.source {
      FixtureSource::FixtureName(name) => load_fixture(name),
      FixtureSource::ArbitraryPath(path) => load_fixture_from_path(path),
    }
  }
}

#[derive(serde::Serialize, serde::Deserialize, Clone)]
struct BenchmarkResult {
  fixture: String,
  file_size: usize,
  mean_time_ms: f64,
  min_time_ms: f64,
  max_time_ms: f64,
  std_dev_ms: f64,
  #[serde(default)]
  std_error_ms: f64,
  #[serde(default)]
  ci_95_lower_ms: f64,
  #[serde(default)]
  ci_95_upper_ms: f64,
  runs: usize,
  heap_usage_bytes: Option<usize>,
  virtual_usage_bytes: Option<usize>,
  // Derived metrics
  time_per_kib_ms: f64,
  mb_per_s: f64,
}

#[derive(Default)]
struct StatsSummary {
  mean: f64,
  min: f64,
  max: f64,
  std_dev: f64,
  std_error: f64,
  ci_low: f64,
  ci_high: f64,
}

#[allow(clippy::cast_precision_loss)]
fn calculate_stats(times: &[f64]) -> StatsSummary {
  if times.is_empty() {
    return StatsSummary::default();
  }

  let mean = times.iter().sum::<f64>() / times.len() as f64;
  let min = times.iter().fold(f64::INFINITY, |a, &b| a.min(b));
  let max = times.iter().fold(f64::NEG_INFINITY, |a, &b| a.max(b));
  let variance = if times.len() > 1 {
    times.iter().map(|&x| (x - mean).powi(2)).sum::<f64>() / (times.len() - 1) as f64
  } else {
    0.0
  };
  let std_dev = variance.sqrt();
  let std_error = if times.len() > 1 {
    std_dev / (times.len() as f64).sqrt()
  } else {
    0.0
  };
  let ci_margin = 1.96 * std_error;
  let ci_low = (mean - ci_margin).max(0.0);
  let ci_high = mean + ci_margin;

  StatsSummary {
    mean,
    min,
    max,
    std_dev,
    std_error,
    ci_low,
    ci_high,
  }
}

fn benchmark_fixture(
  target: &FixtureTarget,
  warmup: usize,
  runs: usize,
  verbose: bool,
) -> BenchmarkResult {
  let fixture = target.load_contents();
  let file_size = fixture.len();
  let fixture_str = fixture.as_str();

  println!("Benchmarking {} ({file_size} bytes)...", target.label);
  if verbose && let Some(path) = target.source_path() {
    println!("  Source path: {}", path.display());
  }

  // Warmup runs
  for i in 0..warmup {
    let start = Instant::now();
    let result = parse_lockfile(black_box(fixture_str));
    let duration = start.elapsed();
    assert!(result.is_ok(), "Should parse {} successfully", target.label);

    if verbose {
      println!(
        "  Warmup {}: {:.3}ms - {} packages parsed",
        i + 1,
        duration.as_secs_f64() * 1000.0,
        result.unwrap().1.entries.len()
      );
    }
  }

  // Measure heap usage with a single run
  let before = memory_stats().unwrap();
  let result = parse_lockfile(black_box(fixture_str));
  let after = memory_stats().unwrap();

  let heap_usage = isize::try_from(after.physical_mem).expect("physical mem too large")
    - isize::try_from(before.physical_mem).expect("physical mem too large");
  let virtual_usage = isize::try_from(after.virtual_mem).expect("virtual mem too large")
    - isize::try_from(before.virtual_mem).expect("virtual mem too large");

  assert!(result.is_ok(), "Should parse {} successfully", target.label);

  if verbose {
    println!("  Heap usage: {heap_usage} bytes (physical), {virtual_usage} bytes (virtual)");
  }

  // Actual benchmark runs
  let mut times = Vec::new();

  for i in 0..runs {
    let start = Instant::now();
    let result = parse_lockfile(black_box(fixture_str));
    let duration = start.elapsed();
    let time_ms = duration.as_secs_f64() * 1000.0;
    times.push(time_ms);

    if verbose {
      println!("  Run {}: {:.3}ms", i + 1, time_ms);
    }

    assert!(result.is_ok(), "Should parse {} successfully", target.label);
  }

  let stats = calculate_stats(&times);

  // Derived metrics
  let kib = file_size as f64 / 1024.0;
  let time_per_kib_ms = if kib > 0.0 { stats.mean / kib } else { 0.0 };
  let mb = file_size as f64 / 1_000_000.0;
  let mb_per_s = if stats.mean > 0.0 {
    mb / (stats.mean / 1000.0)
  } else {
    f64::INFINITY
  };

  BenchmarkResult {
    fixture: target.label.clone(),
    file_size,
    mean_time_ms: stats.mean,
    min_time_ms: stats.min,
    max_time_ms: stats.max,
    std_dev_ms: stats.std_dev,
    std_error_ms: stats.std_error,
    ci_95_lower_ms: stats.ci_low,
    ci_95_upper_ms: stats.ci_high,
    runs,
    heap_usage_bytes: Some(heap_usage.unsigned_abs()),
    virtual_usage_bytes: Some(virtual_usage.unsigned_abs()),
    time_per_kib_ms,
    mb_per_s,
  }
}

fn load_baseline(path: &str) -> Option<Vec<BenchmarkResult>> {
  let Ok(contents) = fs::read_to_string(path) else {
    return None;
  };
  serde_json::from_str::<Vec<BenchmarkResult>>(&contents).ok()
}

fn save_baseline(path: &str, results: &[BenchmarkResult]) -> std::io::Result<()> {
  let data = serde_json::to_string_pretty(results).expect("serialize baseline");
  if let Some(parent) = Path::new(path).parent()
    && !parent.as_os_str().is_empty()
  {
    fs::create_dir_all(parent)?;
  }
  fs::write(path, data)
}

fn compare_with_baseline(
  baseline: &[BenchmarkResult],
  current: &[BenchmarkResult],
  threshold_ratio_ms_per_kib: f64,
) -> (bool, Vec<String>) {
  let baseline_map: HashMap<&str, &BenchmarkResult> =
    baseline.iter().map(|b| (b.fixture.as_str(), b)).collect();

  let mut regressions = Vec::new();
  let mut any_regressed = false;

  for cur in current {
    if let Some(base) = baseline_map.get(cur.fixture.as_str()) {
      // Compare normalized ms/KiB
      let ratio = if base.time_per_kib_ms > 0.0 {
        cur.time_per_kib_ms / base.time_per_kib_ms
      } else {
        1.0
      };
      if ratio > 1.0 + threshold_ratio_ms_per_kib {
        any_regressed = true;
        regressions.push(format!(
          "{} regressed: {:.1}% slower (ms/KiB: {:.3} -> {:.3})",
          cur.fixture,
          (ratio - 1.0) * 100.0,
          base.time_per_kib_ms,
          cur.time_per_kib_ms
        ));
      }
    }
  }

  (any_regressed, regressions)
}

fn discover_all_fixture_names() -> Vec<String> {
  // Locate the fixtures directory relative to this crate
  let fixtures_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .unwrap()
    .parent()
    .unwrap()
    .join("fixtures");

  let mut names: Vec<String> = fs::read_dir(&fixtures_dir)
    .unwrap_or_else(|e| {
      panic!(
        "Failed to read fixtures dir {}: {e}",
        fixtures_dir.display()
      )
    })
    .filter_map(|entry| {
      let entry = entry.ok()?;
      let path = entry.path();
      if path.extension()?.to_str()? == "lock" {
        path
          .file_name()?
          .to_str()
          .map(std::string::ToString::to_string)
      } else {
        None
      }
    })
    .collect();

  names.sort();
  names
}

fn print_results(results: &[BenchmarkResult], format: &str) {
  if format == "json" {
    println!("{}", serde_json::to_string_pretty(results).unwrap());
  } else {
    println!("\nBenchmark Results:");
    println!(
      "{:<28} {:>12} {:>20} {:>12} {:>12} {:>12} {:>12}",
      "Fixture", "Bytes", "Mean +/- CI95 (ms)", "Min (ms)", "Max (ms)", "ms/KiB", "MB/s"
    );
    println!("{:-<120}", "");

    for result in results {
      let ci_margin = (result.ci_95_upper_ms - result.ci_95_lower_ms).abs() / 2.0;
      let mean_column = format!("{:.3} +/- {:.3}", result.mean_time_ms, ci_margin);
      println!(
        "{:<28} {:>12} {:>20} {:>12.3} {:>12.3} {:>12.3} {:>12.2}",
        result.fixture,
        result.file_size,
        mean_column,
        result.min_time_ms,
        result.max_time_ms,
        result.time_per_kib_ms,
        result.mb_per_s
      );
    }
  }
}

fn main() {
  let args = Args::parse();

  let fixtures: Vec<FixtureTarget> = if let Some(path) = args.fixture_path {
    vec![FixtureTarget::from_path(path)]
  } else if let Some(fixture) = args.fixture {
    vec![FixtureTarget::from_fixture_name(fixture)]
  } else if args.all {
    discover_all_fixture_names()
      .into_iter()
      .map(FixtureTarget::from_fixture_name)
      .collect()
  } else {
    // Default to a few key fixtures
    vec![
      FixtureTarget::from_fixture_name("minimal-berry.lock"),
      FixtureTarget::from_fixture_name("workspaces.yarn.lock"),
      FixtureTarget::from_fixture_name("auxiliary-packages.yarn.lock"),
    ]
  };

  let mut results = Vec::new();

  for fixture in &fixtures {
    let result = benchmark_fixture(fixture, args.warmup, args.runs, args.verbose);
    results.push(result);
  }

  print_results(&results, &args.format);

  // Simple regression detection using normalized metric (ms per KiB)
  if results.len() > 1 {
    println!("\nPerformance Analysis (normalized by size):");

    let best = results
      .iter()
      .min_by(|a, b| a.time_per_kib_ms.partial_cmp(&b.time_per_kib_ms).unwrap())
      .unwrap();

    for result in &results {
      if result.fixture != best.fixture {
        let ratio = result.time_per_kib_ms / best.time_per_kib_ms;
        if ratio > 1.5 {
          println!(
            "⚠️  {} is {:.1}x slower than {} (ms/KiB: {:.3} vs {:.3})",
            result.fixture, ratio, best.fixture, result.time_per_kib_ms, best.time_per_kib_ms
          );
        } else {
          println!(
            "✅ {} looks fine (ms/KiB {:.3}, best {:.3})",
            result.fixture, result.time_per_kib_ms, best.time_per_kib_ms
          );
        }
      }
    }
  }

  // Baseline comparison and optional failure on regression
  if let Some(baseline_path) = &args.baseline {
    if let Some(baseline) = load_baseline(baseline_path) {
      println!(
        "\nBaseline Comparison (ms/KiB threshold: +{:.1}%)",
        args.threshold_ratio_ms_per_kib * 100.0
      );
      let (regressed, messages) =
        compare_with_baseline(&baseline, &results, args.threshold_ratio_ms_per_kib);
      if messages.is_empty() {
        println!("✅ No regressions vs baseline");
      } else {
        for msg in messages {
          println!("⚠️  {msg}");
        }
      }
      if regressed && args.fail_on_regression {
        eprintln!("\nError: performance regression detected vs baseline");
        std::process::exit(1);
      }
    } else {
      eprintln!("Could not load baseline from {baseline_path}");
    }
  }

  if let Some(save_path) = &args.save_baseline {
    if let Err(err) = save_baseline(save_path, &results) {
      eprintln!("Failed to save baseline to {save_path}: {err}");
    } else if args.verbose {
      println!("Saved baseline to {save_path}");
    }
  }
}
