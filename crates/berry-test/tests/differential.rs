//! Mutation fuzzing of lockfile fixtures and the edge corpus.
//! Longer runs: `BERRY_FUZZ_ITERS=200000 BERRY_FUZZ_SEED=7 cargo test --release -p berry-test --test differential`.

use berry_test::differential::{Rng, candidate, mutate, robust};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

fn fixtures_dir() -> PathBuf {
  Path::new(env!("CARGO_MANIFEST_DIR")).join("../../fixtures")
}

fn files_with_suffix(dir: &Path, suffix: &str) -> Vec<PathBuf> {
  let mut v: Vec<PathBuf> = std::fs::read_dir(dir)
    .unwrap_or_else(|e| panic!("read_dir {}: {e}", dir.display()))
    .filter_map(|e| e.ok().map(|e| e.path()))
    .filter(|p| p.is_file() && p.to_string_lossy().ends_with(suffix))
    .collect();
  v.sort();
  v
}

fn read(p: &Path) -> String {
  std::fs::read_to_string(p).unwrap_or_else(|e| panic!("read {}: {e}", p.display()))
}

#[test]
fn mutation_fuzz_never_panics_or_hangs() {
  let iters: u64 = std::env::var("BERRY_FUZZ_ITERS")
    .ok()
    .and_then(|v| v.parse().ok())
    .unwrap_or(6_000);
  let seed: u64 = std::env::var("BERRY_FUZZ_SEED")
    .ok()
    .and_then(|v| v.parse().ok())
    .unwrap_or(0x5eed);

  let dir = fixtures_dir();
  let mut seeds: Vec<String> = files_with_suffix(&dir.join("edge"), ".yarn.lock")
    .iter()
    .map(|p| read(p))
    .collect();
  for p in files_with_suffix(&dir, ".lock") {
    if std::fs::metadata(&p).map_or(u64::MAX, |m| m.len()) <= 16 * 1024 {
      seeds.push(read(&p));
    }
  }

  // Hang detector: the parser must always make progress.
  let progress = Arc::new(AtomicU64::new(0));
  {
    let progress = Arc::clone(&progress);
    std::thread::spawn(move || {
      let mut last = u64::MAX;
      loop {
        std::thread::sleep(Duration::from_mins(1));
        let now = progress.load(Ordering::Relaxed);
        if now == last && now < iters {
          eprintln!("mutation fuzz: no progress for 60 s at case {now} (seed {seed}): hang");
          std::process::abort();
        }
        last = now;
      }
    });
  }

  let mut rng = Rng::new(seed);
  let mut failures = Vec::new();
  for i in 0..iters {
    let base = &seeds[rng.below(seeds.len())];
    let input = mutate(base, &mut rng);
    if let Err(e) = robust(&candidate(&input)) {
      failures.push(format!("case {i}: {e}\n  input: {input:?}"));
      if failures.len() >= 5 {
        break;
      }
    }
    progress.store(i + 1, Ordering::Relaxed);
  }
  assert!(failures.is_empty(), "seed {seed}:\n{}", failures.join("\n"));
}
