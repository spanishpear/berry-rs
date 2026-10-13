//! The default parse is parallel above a size threshold (rayon). Whatever the
//! pool, thread count or nesting, the result must be the sequential one:
//! same entries, stop offset, remaining text, error kind and offset.

use berry::{Buffers, Error, Lockfile};
use berry_test::differential::same_result;
use rayon::ThreadPoolBuilder;
use rayon::prelude::*;
use std::path::Path;
use std::sync::OnceLock;

const THREADS: [usize; 6] = [1, 2, 3, 4, 8, 32];
const TARGETS: [usize; 4] = [64, 1 << 10, 16 << 10, 256 << 10];

type Parsed<'a> = Result<Lockfile<'a>, Error>;

fn fixture(name: &str) -> String {
  let p = Path::new(env!("CARGO_MANIFEST_DIR"))
    .join("../../fixtures")
    .join(name);
  std::fs::read_to_string(p).unwrap()
}

/// A ~6 MB lockfile (above both thresholds): the body of the largest fixture
/// three times over (entries may repeat; the parser does not care).
fn large() -> &'static str {
  static LARGE: OnceLock<String> = OnceLock::new();
  LARGE.get_or_init(|| {
    let src = fixture("resolutions-patches.yarn.lock");
    let body_at = src.find("\n\n\"").unwrap() + 2;
    let (head, body) = src.split_at(body_at);
    let body = body.trim_end_matches('\n');
    format!("{head}{body}\n\n{body}\n\n{body}\n")
  })
}

fn sequential(src: &str) -> Parsed<'_> {
  berry::__parse_chunked(src, usize::MAX)
}

#[track_caller]
fn assert_same(label: &str, want: &Parsed<'_>, got: &Parsed<'_>) {
  if let Err(e) = same_result(want, got) {
    panic!("{label}: {e}");
  }
}

fn pool(threads: usize) -> rayon::ThreadPool {
  ThreadPoolBuilder::new()
    .num_threads(threads)
    .build()
    .unwrap()
}

/// `src` with `what` inserted as its own paragraph at the first entry
/// boundary after `fraction` of the input.
#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
fn with_paragraph_at(src: &str, fraction: f64, what: &str) -> String {
  let at = (src.len() as f64 * fraction) as usize;
  let at = at + src[at..].find("\n\n").unwrap() + 2;
  format!("{}{what}\n\n{}", &src[..at], &src[at..])
}

#[test]
fn large_input_is_split_and_matches_sequential() {
  let src = large();
  assert!(src.len() > berry::__PARALLEL_MIN);
  let want = sequential(src);
  let got = berry::parse(src);
  if rayon::current_num_threads() > 1 {
    assert!(got.as_ref().unwrap().__segments() > 1);
  }
  assert_same("default", &want, &got);
  assert_eq!(
    berry_test::canonical::to_string(want.as_ref().unwrap()),
    berry_test::canonical::to_string(got.as_ref().unwrap())
  );
  assert_eq!(want.unwrap().__segments(), 1);
}

#[test]
fn every_thread_count_and_chunk_size_matches_sequential() {
  let inputs = [
    fixture("berry.lock"),
    fixture("auxiliary-packages.yarn.lock"),
    fixture("mixed-keys.yarn.lock"),
    fixture("resolutions-patches.yarn.lock"),
    large().to_owned(),
  ];
  for src in &inputs {
    let want = sequential(src);
    for threads in THREADS {
      pool(threads).install(|| {
        assert_same(&format!("{threads} threads"), &want, &berry::parse(src));
        for t in TARGETS {
          let got = berry::__parse_chunked(src, t);
          assert_same(&format!("{threads} threads, chunk target {t}"), &want, &got);
        }
      });
    }
  }
}

#[test]
fn thresholds_decide_the_split() {
  let medium = fixture("resolutions-patches.yarn.lock");
  assert!(medium.len() < berry::__PARALLEL_MIN);
  pool(8).install(|| {
    assert_eq!(berry::parse(&medium).unwrap().__segments(), 1);
    assert!(berry::parse(large()).unwrap().__segments() > 1);
  });
  // A one-thread pool never splits.
  pool(1).install(|| assert_eq!(berry::parse(large()).unwrap().__segments(), 1));
  // At most 2 x threads chunks.
  pool(3).install(|| assert!(berry::parse(large()).unwrap().__segments() <= 6));
}

#[test]
fn stop_and_error_points_are_the_sequential_ones() {
  let src = large();
  let bogus = "\"x@npm:1\":\n  bogus: 1";
  let bad_link = "\"x@npm:1\":\n  linkType: HARD";
  let cases = [
    with_paragraph_at(src, 0.4, "not an entry"),
    with_paragraph_at(src, 0.7, bogus),
    with_paragraph_at(&with_paragraph_at(src, 0.7, bogus), 0.3, "junk"),
    with_paragraph_at(&with_paragraph_at(src, 0.7, "junk"), 0.3, bad_link),
    with_paragraph_at(src, 0.999, "trailing junk"),
  ];
  let mut kinds = Vec::new();
  for case in &cases {
    let want = sequential(case);
    kinds.push(
      want
        .as_ref()
        .map(|lf| lf.remaining().len())
        .map_err(Error::kind),
    );
    for threads in [2, 8, 32] {
      pool(threads).install(|| {
        assert_same(&format!("{threads} threads"), &want, &berry::parse(case));
        let chunked = berry::__parse_chunked(case, 4096);
        assert_same(&format!("{threads} threads, 4 KiB"), &want, &chunked);
      });
    }
    assert_same("global pool", &want, &berry::parse(case));
  }
  assert!(matches!(kinds[0], Ok(n) if n > 0));
  assert_eq!(kinds[1], Err(berry::ErrorKind::UnknownProperty));
  assert!(matches!(kinds[2], Ok(n) if n > 0));
  assert_eq!(kinds[3], Err(berry::ErrorKind::InvalidLinkType));
  assert!(matches!(kinds[4], Ok(n) if n > 0));
}

#[test]
fn nested_parses_inside_rayon_tasks_match_sequential() {
  let src = large();
  let want = sequential(src);
  let p = pool(8);
  for concurrent in [8, 32] {
    let results: Vec<_> = p.install(|| {
      (0..concurrent)
        .into_par_iter()
        .map(|_| berry::parse(src))
        .collect()
    });
    for r in &results {
      assert_same(&format!("{concurrent} nested in par_iter"), &want, r);
    }
    let mut results: Vec<_> = (0..concurrent).map(|_| None).collect();
    p.scope(|s| {
      for r in &mut results {
        s.spawn(|_| *r = Some(berry::parse(src)));
      }
    });
    for r in &results {
      assert_same(
        &format!("{concurrent} nested in scope"),
        &want,
        r.as_ref().unwrap(),
      );
    }
  }
  // Nested in tasks of the global pool.
  let results: Vec<_> = (0..4).into_par_iter().map(|_| berry::parse(src)).collect();
  for r in &results {
    assert_same("nested in the global pool", &want, r);
  }
}

#[test]
fn parses_from_plain_threads_match_sequential() {
  let src = large();
  let want = sequential(src);
  std::thread::scope(|s| {
    let handles: Vec<_> = (0..4).map(|_| s.spawn(|| berry::parse(src))).collect();
    for h in handles {
      assert_same("std thread", &want, &h.join().unwrap());
    }
  });
}

#[test]
fn recycled_buffers_across_sizes_and_pools_match_sequential() {
  let inputs = [
    large().to_owned(),
    fixture("berry.lock"),
    fixture("resolutions-patches.yarn.lock"),
    large().to_owned(),
    String::new(),
    with_paragraph_at(large(), 0.5, "junk"),
    large().to_owned(),
  ];
  let wants: Vec<_> = inputs.iter().map(|s| sequential(s)).collect();
  let mut buffers = Buffers::new();
  for threads in [32, 2, 1, 8] {
    pool(threads).install(|| {
      for (src, want) in inputs.iter().zip(&wants) {
        let got = berry::parse_with(src, &mut buffers);
        assert_same(&format!("{threads} threads, recycled"), want, &got);
        if let Ok(lf) = got {
          buffers = lf.into_buffers();
        }
      }
    });
  }
}

/// `large()` cut at the last entry boundary before `len` bytes.
fn prefix_of_large(len: usize) -> &'static str {
  let src = large();
  let cut = src[..len.min(src.len())]
    .rfind("\n\n")
    .map_or(src.len(), |i| i + 2);
  &src[..cut]
}

/// Inputs around every size threshold, and large bodies that are not
/// lockfile entries (blank, whitespace-only, junk) or use other encodings
/// of the same text (CRLF, BOM, non-ASCII ranges), parse as the sequential
/// loop does with any thread count, as `&str` and as bytes.
#[test]
fn inputs_around_the_thresholds_match_sequential_at_any_thread_count() {
  let head_end = large().find("\n\n\"").unwrap() + 2;
  let head = &large()[..head_end];
  let mut inputs: Vec<(String, String)> = Vec::new();
  let at = berry::__PARALLEL_MIN;
  for len in [at - 1, at, at + 1, at + 4096] {
    // Pad the cut prefix to exactly `len` bytes with blank lines.
    let p = prefix_of_large(len);
    let pad = "\n".repeat(len.saturating_sub(p.len()));
    inputs.push((format!("{len} bytes"), format!("{p}{pad}")));
  }
  let mb = 1 << 20;
  inputs.extend([
    ("blank body".into(), format!("{head}{}", "\n".repeat(mb))),
    (
      "whitespace-only body".into(),
      format!("{head}{}", " \n\t\n\n  \n\r\n".repeat(mb / 10)),
    ),
    (
      "junk body".into(),
      format!("{head}{}", "junk line\n\nmore: junk\n\n".repeat(mb / 24)),
    ),
    ("crlf".into(), prefix_of_large(3 * mb).replace('\n', "\r\n")),
    ("bom".into(), format!("\u{feff}{}", prefix_of_large(3 * mb))),
    (
      "non-ascii ranges".into(),
      prefix_of_large(3 * mb).replace("npm:^", "npm:\u{e9}\u{3000}^"),
    ),
    (
      "missing final newline".into(),
      prefix_of_large(3 * mb).trim_end().to_owned(),
    ),
  ]);
  for (label, src) in &inputs {
    let want = sequential(src);
    for threads in THREADS {
      pool(threads).install(|| {
        assert_same(
          &format!("{label}, {threads} threads"),
          &want,
          &berry::parse(src),
        );
        assert_same(
          &format!("{label}, {threads} threads, bytes"),
          &want,
          &berry::parse_bytes(src.as_bytes()),
        );
      });
    }
  }
}
