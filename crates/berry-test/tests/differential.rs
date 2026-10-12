//! Equivalence of `berry` with the frozen reference parser (`berry_ref`).
//!
//! * every public fixture and every valid file of the edge corpus
//!   (`fixtures/edge`); the other edge files must not panic,
//! * golden outputs of the reference (edge corpus: full text; fixtures: hash),
//!   so an accidental change of the oracle itself is caught too,
//! * a deterministic mutation fuzzer around the corpus: no panic, no hang.
//!
//! Regenerate goldens (only after a deliberate oracle change) with
//! `BERRY_BLESS=1 cargo test -p berry-test --test differential`.
//! Longer fuzz runs: `BERRY_FUZZ_ITERS=200000 BERRY_FUZZ_SEED=7 cargo test --release ...`.

use berry_test::differential::{
  Outcome, Rng, candidate, check, fnv1a64, is_valid_corpus_file, mutate, reference, robust,
};
use std::fmt::Write as _;
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

fn bless() -> bool {
  std::env::var_os("BERRY_BLESS").is_some()
}

#[test]
fn edge_corpus_matches_reference_and_goldens() {
  let edge = fixtures_dir().join("edge");
  let golden_dir = edge.join("golden");
  let files = files_with_suffix(&edge, ".yarn.lock");
  assert!(
    files.len() >= 50,
    "edge corpus missing? found {}",
    files.len()
  );
  let mut failures = String::new();
  for path in &files {
    let name = path.file_name().unwrap().to_string_lossy().into_owned();
    let stem = name.trim_end_matches(".yarn.lock");
    let src = read(path);
    let r = reference(&src);
    let golden_path = golden_dir.join(format!("{stem}.txt"));
    if bless() {
      std::fs::create_dir_all(&golden_dir).unwrap();
      std::fs::write(&golden_path, r.golden_text()).unwrap();
    } else {
      let golden = read(&golden_path);
      if golden != r.golden_text() {
        let _ = writeln!(
          failures,
          "{name}: reference output differs from its golden file"
        );
      }
    }
    let c = candidate(&src);
    let verdict = if is_valid_corpus_file(&name) {
      check(&r, &c)
    } else {
      robust(&c)
    };
    if let Err(e) = verdict {
      let _ = writeln!(failures, "{name}: {e}");
    }
  }
  assert!(failures.is_empty(), "\n{failures}");
}

#[test]
fn fixtures_match_reference_and_golden_hashes() {
  let dir = fixtures_dir();
  let hashes_path = dir.join("edge/golden/fixture-hashes.txt");
  let mut hashes = String::new();
  let mut failures = String::new();
  for path in files_with_suffix(&dir, ".lock") {
    let name = path.file_name().unwrap().to_string_lossy().into_owned();
    let src = read(&path);
    let r = reference(&src);
    let Outcome::Ok(dump) = &r else {
      let _ = writeln!(failures, "{name}: reference did not parse: {r:?}");
      continue;
    };
    let _ = writeln!(
      hashes,
      "{name} {} {:016x}",
      dump.len(),
      fnv1a64(dump.as_bytes())
    );
    if let Err(e) = check(&r, &candidate(&src)) {
      let _ = writeln!(failures, "{name}: {e}");
    }
  }
  if bless() {
    std::fs::write(&hashes_path, &hashes).unwrap();
  } else if read(&hashes_path) != hashes {
    let _ = writeln!(
      failures,
      "reference fixture dumps differ from {}",
      hashes_path.display()
    );
  }
  assert!(failures.is_empty(), "\n{failures}");
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
