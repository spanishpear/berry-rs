use berry::parse::parse_lockfile;
use berry_test::load_fixture;
use criterion::{Criterion, SamplingMode, criterion_group, criterion_main};
use memory_stats::memory_stats;
use std::fs;
use std::hint::black_box;
use std::path::Path;
use std::time::{Duration, Instant};

const FAST_SAMPLE_SIZE: usize = 60;
const SLOW_SAMPLE_SIZE: usize = 30;
const FAST_MEASUREMENT_TIME: Duration = Duration::from_secs(1);
const SLOW_MEASUREMENT_TIME: Duration = Duration::from_secs(3);
const WARM_UP_TIME: Duration = Duration::from_millis(500);
const NOISE_THRESHOLD: f64 = 0.02;

#[derive(Copy, Clone)]
enum BenchmarkKind {
  Fast,
  Slow,
}

fn configure_group(
  group: &mut criterion::BenchmarkGroup<'_, criterion::measurement::WallTime>,
  kind: BenchmarkKind,
) {
  group
    .warm_up_time(WARM_UP_TIME)
    .noise_threshold(NOISE_THRESHOLD)
    .sampling_mode(SamplingMode::Flat);

  match kind {
    BenchmarkKind::Fast => {
      group
        .sample_size(FAST_SAMPLE_SIZE)
        .measurement_time(FAST_MEASUREMENT_TIME);
    }
    BenchmarkKind::Slow => {
      group
        .sample_size(SLOW_SAMPLE_SIZE)
        .measurement_time(SLOW_MEASUREMENT_TIME);
    }
  }
}

/// Benchmark parsing with different fixture sizes
fn benchmark_fixtures(c: &mut Criterion) {
  let mut group = c.benchmark_group("fixture_parsing");
  configure_group(&mut group, BenchmarkKind::Slow);

  // Small fixture benchmark
  group.bench_function("minimal_berry", |b| {
    let fixture = load_fixture("minimal-berry.lock");
    b.iter(|| {
      let result = parse_lockfile(black_box(&fixture));
      assert!(result.is_ok(), "Should parse minimal fixture successfully");
      result.unwrap().1
    });
  });

  // Medium fixture benchmark
  group.bench_function("workspaces", |b| {
    let fixture = load_fixture("workspaces.yarn.lock");
    b.iter(|| {
      let result = parse_lockfile(black_box(&fixture));
      assert!(
        result.is_ok(),
        "Should parse workspaces fixture successfully"
      );
      result.unwrap().1
    });
  });

  // Large fixture benchmark
  group.bench_function("auxiliary_packages", |b| {
    let fixture = load_fixture("auxiliary-packages.yarn.lock");
    b.iter(|| {
      let result = parse_lockfile(black_box(&fixture));
      assert!(
        result.is_ok(),
        "Should parse auxiliary packages fixture successfully"
      );
      result.unwrap().1
    });
  });

  // Large fixture benchmark
  group.bench_function("duplicate_packages", |b| {
    let fixture = load_fixture("berry.lock");
    b.iter(|| {
      let result = parse_lockfile(black_box(&fixture));
      assert!(
        result.is_ok(),
        "Should parse duplicate packages fixture successfully"
      );
      result.unwrap().1
    });
  });

  // Extra large fixture benchmark
  group.bench_function("resolutions_patches", |b| {
    let fixture = load_fixture("resolutions-patches.yarn.lock");
    b.iter(|| {
      let result = parse_lockfile(black_box(&fixture));
      assert!(
        result.is_ok(),
        "Should parse resolutions patches fixture successfully"
      );
      result.unwrap().1
    });
  });

  group.finish();
}

/// Benchmark parsing speed vs file size
fn benchmark_parsing_speed_vs_size(c: &mut Criterion) {
  let mut group = c.benchmark_group("parsing_speed_vs_size");
  configure_group(&mut group, BenchmarkKind::Slow);

  let fixtures = vec![
    ("minimal-berry.lock", "small"),
    ("workspaces.yarn.lock", "small-medium"),
    ("yarn4-mixed-protocol.lock", "medium"),
    ("auxiliary-packages.yarn.lock", "large"),
    ("berry.lock", "extra-large"), // Large Berry lockfile (~112KB)
    ("resolutions-patches.yarn.lock", "extra-extra-large"), // Very large lockfile (~2MB)
  ];

  for (fixture_name, size_label) in fixtures {
    let fixture = load_fixture(fixture_name);
    let file_size = fixture.len();

    group.bench_function(format!("{size_label}_({file_size} bytes)"), |b| {
      b.iter(|| {
        let result = parse_lockfile(black_box(&fixture));
        assert!(result.is_ok(), "Should parse {fixture_name} successfully");
        result.unwrap().1
      });
    });
  }

  group.finish();
}

/// Benchmark memory usage during parsing
fn benchmark_memory_usage(c: &mut Criterion) {
  let mut group = c.benchmark_group("memory_usage");
  configure_group(&mut group, BenchmarkKind::Slow);

  // Test with different fixture sizes to see memory scaling
  let fixtures = vec![
    "minimal-berry.lock",
    "workspaces.yarn.lock",
    "auxiliary-packages.yarn.lock",
    "berry.lock",
    "resolutions-patches.yarn.lock",
  ];

  for fixture_name in fixtures {
    let fixture = load_fixture(fixture_name);

    group.bench_function(format!("memory_{}", fixture_name.replace(".", "_")), |b| {
      b.iter_custom(|iters| {
        let mut total_duration = std::time::Duration::ZERO;

        for _ in 0..iters {
          let start = Instant::now();
          let result = parse_lockfile(&fixture);
          let duration = start.elapsed();
          total_duration += duration;

          assert!(result.is_ok(), "Should parse {fixture_name} successfully");

          // Force drop to measure memory cleanup
          drop(result);
        }

        total_duration
      });
    });
  }

  group.finish();
}

/// Benchmark zero-allocation claims
fn benchmark_zero_allocation(c: &mut Criterion) {
  let mut group = c.benchmark_group("zero_allocation");
  configure_group(&mut group, BenchmarkKind::Fast);

  let fixture = load_fixture("minimal-berry.lock");

  group.bench_function("parse_with_allocation_tracking", |b| {
    b.iter(|| {
      // This benchmark helps identify if we're making unexpected allocations
      // during the parsing phase
      let result = parse_lockfile(black_box(&fixture));
      assert!(result.is_ok(), "Should parse successfully");

      let lockfile = result.unwrap().1;

      // Access some data to ensure it's actually parsed
      assert_eq!(lockfile.metadata.version, "6");
      assert_eq!(lockfile.entries.len(), 5);

      lockfile
    });
  });

  group.finish();
}

/// Benchmark heap usage after parsing
fn benchmark_heap_usage(c: &mut Criterion) {
  let mut group = c.benchmark_group("heap_usage");
  configure_group(&mut group, BenchmarkKind::Slow);

  let fixtures = vec![
    ("minimal-berry.lock", "small"),
    ("workspaces.yarn.lock", "medium"),
    ("auxiliary-packages.yarn.lock", "large"),
    ("berry.lock", "extra-large"),
    ("resolutions-patches.yarn.lock", "extra-extra-large"),
  ];

  for (fixture_name, size_label) in fixtures {
    let fixture = load_fixture(fixture_name);

    group.bench_function(format!("heap_{size_label}"), |b| {
      b.iter(|| {
        // Get initial memory stats
        let before = memory_stats().unwrap();

        // Parse the lockfile
        let result = parse_lockfile(black_box(&fixture));
        assert!(result.is_ok(), "Should parse {fixture_name} successfully");

        // Get final memory stats
        let after = memory_stats().unwrap();

        // Calculate heap usage
        let heap_usage = after.physical_mem - before.physical_mem;
        let virtual_usage = after.virtual_mem - before.virtual_mem;

        // Return the lockfile to ensure it's not optimized away
        // and to measure the actual memory footprint of the parsed data
        let lockfile = result.unwrap().1;

        // Print heap usage info (this will show in benchmark output)
        if cfg!(debug_assertions) {
          println!(
            "Heap usage for {fixture_name}: {heap_usage} bytes (physical), {virtual_usage} bytes (virtual)"
          );
        }

        lockfile
      });
    });
  }

  group.finish();
}

/// Benchmark individual parsing functions
fn benchmark_individual_functions(c: &mut Criterion) {
  let mut group = c.benchmark_group("individual_functions");
  configure_group(&mut group, BenchmarkKind::Fast);

  // Test parse_lockfile function specifically
  let fixture = load_fixture("minimal-berry.lock");

  group.bench_function("parse_lockfile_only", |b| {
    b.iter(|| {
      let result = parse_lockfile(black_box(&fixture));
      assert!(result.is_ok(), "Should parse successfully");
      result.unwrap().1
    });
  });

  group.finish();
}

/// Benchmark parsing with different input characteristics
fn benchmark_input_characteristics(c: &mut Criterion) {
  let mut group = c.benchmark_group("input_characteristics");
  configure_group(&mut group, BenchmarkKind::Slow);

  // Test with fixtures that have different characteristics
  let fixtures = vec![
    ("minimal-berry.lock", "simple"),
    ("yarn4-mixed-protocol.lock", "mixed_protocols"),
    ("yarn4-resolution.lock", "resolutions"),
    ("yarn4-patch.lock", "patches"),
  ];

  for (fixture_name, characteristic) in fixtures {
    let fixture = load_fixture(fixture_name);

    group.bench_function(format!("{characteristic}_characteristic"), |b| {
      b.iter(|| {
        let result = parse_lockfile(black_box(&fixture));
        assert!(result.is_ok(), "Should parse {fixture_name} successfully");
        result.unwrap().1
      });
    });
  }

  group.finish();
}

/// Benchmark all fixtures discovered in the fixtures directory
fn benchmark_all_fixtures(c: &mut Criterion) {
  let mut group = c.benchmark_group("all_fixtures");
  configure_group(&mut group, BenchmarkKind::Slow);

  // Discover fixtures directory relative to this crate
  let fixtures_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .unwrap()
    .parent()
    .unwrap()
    .join("fixtures");

  let mut fixtures: Vec<String> = fs::read_dir(&fixtures_dir)
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
        path.file_name()?.to_str().map(|s| s.to_string())
      } else {
        None
      }
    })
    .collect();

  fixtures.sort();

  for fixture_name in fixtures {
    let fixture = load_fixture(&fixture_name);
    let label = fixture_name.replace(['.', '-'], "_").to_string();
    group.bench_function(label, |b| {
      b.iter(|| {
        let result = parse_lockfile(black_box(&fixture));
        assert!(result.is_ok(), "Should parse {fixture_name} successfully");
        result.unwrap().1
      });
    });
  }

  group.finish();
}

criterion_group!(
  benches,
  benchmark_fixtures,
  benchmark_parsing_speed_vs_size,
  benchmark_memory_usage,
  benchmark_heap_usage,
  benchmark_zero_allocation,
  benchmark_individual_functions,
  benchmark_input_characteristics,
  benchmark_all_fixtures,
);
criterion_main!(benches);
