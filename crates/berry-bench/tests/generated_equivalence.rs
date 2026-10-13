//! Differential test on many small generated lockfiles: the parser (default
//! and with forced chunking) must match the frozen reference on every seed,
//! and must not panic after CRLF/whitespace/mutation transforms.

use berry_bench::synth::{Profile, Target, generate};
use berry_test::differential::{
  Outcome, Rng, candidate, candidate_with, check, fingerprint, mutate, reference, robust,
};

fn assert_equivalent(label: &str, src: &str) {
  let r = reference(src);
  if let Err(e) = check(&r, &candidate(src)) {
    panic!("{label}: {e}");
  }
  for target in [64, 1024] {
    let c = candidate_with(src, |s| berry::__parse_chunked(s, target));
    if let Err(e) = check(&r, &c) {
      panic!("{label}: chunk target {target}: {e}");
    }
  }
}

fn assert_robust(label: &str, src: &str) {
  if let Err(e) = robust(&candidate(src)) {
    panic!("{label}: {e}");
  }
  let want = fingerprint(berry::parse(src));
  for target in [64, 1024] {
    assert!(
      fingerprint(berry::__parse_chunked(src, target)) == want,
      "{label}: chunk target {target} differs from sequential"
    );
  }
}

fn seeds() -> u64 {
  std::env::var("BERRY_GEN_SEEDS")
    .ok()
    .and_then(|v| v.parse().ok())
    .unwrap_or(300)
}

#[test]
fn generated_lockfiles_match_reference() {
  for seed in 0..seeds() {
    let entries = 1 + (seed as usize * 7) % 40;
    let (text, _) = generate(seed, Target::Entries(entries), &Profile::default());
    assert!(matches!(reference(&text), Outcome::Ok(_)), "seed {seed}");
    assert_equivalent(&format!("seed {seed} entries {entries}"), &text);
  }
}

#[test]
fn generated_lockfiles_with_transforms_do_not_panic() {
  for seed in 0..seeds().min(100) {
    let (text, _) = generate(seed, Target::Entries(12), &Profile::default());
    let variants = [
      ("crlf", text.replace('\n', "\r\n")),
      ("no-final-eol", text.trim_end_matches('\n').to_owned()),
      ("bom", format!("\u{feff}{text}")),
      ("trailing-ws", text.replace(":\n", ": \n")),
      ("extra-blank", text.replace("\n\n", "\n\n\n")),
      ("truncated", text[..text.len() / 2].to_owned()),
    ];
    for (name, v) in &variants {
      assert_robust(&format!("seed {seed} {name}"), v);
    }
  }
}

#[test]
fn mutated_generated_lockfiles_do_not_panic() {
  let mut rng = Rng::new(0x9e11);
  for seed in 0..seeds().min(150) {
    let (text, _) = generate(seed, Target::Entries(8), &Profile::default());
    for i in 0..4 {
      let m = mutate(&text, &mut rng);
      assert_robust(&format!("seed {seed} mutation {i}: {m:?}"), &m);
    }
  }
}
