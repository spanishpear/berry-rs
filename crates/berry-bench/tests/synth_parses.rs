//! The synthetic generator must only emit lockfiles the parser fully accepts.

use berry_bench::synth::{Profile, Target, generate};
use berry_test::differential::same_result;

fn assert_parses_fully(seed: u64, entries: usize) {
  let (text, summary) = generate(seed, Target::Entries(entries), &Profile::default());
  let lockfile = berry::parse(&text).expect("synthetic lockfile should parse");
  let rest = lockfile.remaining();
  assert!(
    rest.trim().is_empty(),
    "seed {seed}, {entries} entries: {} bytes left unparsed",
    rest.len()
  );
  assert_eq!(lockfile.len(), summary.entries);
  assert_eq!(lockfile.metadata().version, "8");
}

#[test]
fn small_variants_parse_fully() {
  for entries in [1, 2, 10, 100, 1_000] {
    assert_parses_fully(1, entries);
  }
}

#[test]
fn several_seeds_parse_fully() {
  for seed in 0..8 {
    assert_parses_fully(seed, 2_000);
  }
}

#[test]
fn byte_target_variant_parses_fully() {
  let (text, summary) = generate(3, Target::Bytes(2_000_000), &Profile::default());
  let lockfile = berry::parse(&text).expect("parse");
  assert!(lockfile.remaining().trim().is_empty());
  assert_eq!(lockfile.len(), summary.entries);
}

#[test]
fn chunked_and_parallel_parses_of_a_large_generated_lockfile_match_sequential() {
  let (text, _) = generate(5, Target::Bytes(3_000_000), &Profile::default());
  let want = berry::__parse_chunked(&text, usize::MAX);
  let dump = berry_test::canonical::to_string(want.as_ref().expect("parse"));
  let got = berry::parse(&text);
  assert!(berry_test::canonical::to_string(got.as_ref().unwrap()) == dump);
  for threads in [1, 2, 7, 32] {
    let pool = rayon::ThreadPoolBuilder::new()
      .num_threads(threads)
      .build()
      .unwrap();
    pool.install(|| {
      same_result(&want, &berry::parse(&text)).unwrap();
      for target in [4 << 10, 16 << 10, 256 << 10] {
        let got = berry::__parse_chunked(&text, target);
        if let Err(e) = same_result(&want, &got) {
          panic!("{threads} threads, target {target}: {e}");
        }
      }
    });
  }
}
