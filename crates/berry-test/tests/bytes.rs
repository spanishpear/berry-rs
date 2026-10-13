//! `parse_bytes`: UTF-8 validation (parallel for large inputs) followed by
//! the parse. Valid input must parse exactly like `parse(&str)`; invalid
//! input must fail with `InvalidUtf8` at `std`'s `valid_up_to`, wherever the
//! validation pieces are cut.

use berry::{Buffers, ErrorKind};
use berry_test::differential::{Rng, same_result};
use rayon::ThreadPoolBuilder;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

fn fixtures_dir() -> PathBuf {
  Path::new(env!("CARGO_MANIFEST_DIR")).join("../../fixtures")
}

fn corpus() -> Vec<(String, String)> {
  let mut out = Vec::new();
  for dir in [fixtures_dir(), fixtures_dir().join("edge")] {
    for e in std::fs::read_dir(&dir).unwrap() {
      let p = e.unwrap().path();
      if p.is_file() && p.to_string_lossy().ends_with(".lock") {
        let name = p.file_name().unwrap().to_string_lossy().into_owned();
        out.push((name, std::fs::read_to_string(&p).unwrap()));
      }
    }
  }
  out.sort();
  out
}

/// ~6 MB with non-ASCII text spread through it.
fn large() -> &'static str {
  static LARGE: OnceLock<String> = OnceLock::new();
  LARGE.get_or_init(|| {
    let src =
      std::fs::read_to_string(fixtures_dir().join("resolutions-patches.yarn.lock")).unwrap();
    let body_at = src.find("\n\n\"").unwrap() + 2;
    let (head, body) = src.split_at(body_at);
    let body = body.trim_end_matches('\n');
    let odd = "\"\u{e9}t\u{e9}@npm:1\":\n  version: \u{3000}1\n  checksum: \u{1f600}\n";
    format!("{head}{body}\n\n{odd}\n{body}\n\n{odd}\n{body}\n")
  })
}

fn std_offset(b: &[u8]) -> Option<usize> {
  std::str::from_utf8(b).err().map(|e| e.valid_up_to())
}

#[track_caller]
fn assert_invalid_at(b: &[u8], label: &str) {
  let want = std_offset(b).expect("input must be invalid");
  let e = berry::parse_bytes(b).unwrap_err();
  assert_eq!(
    (e.kind(), e.offset()),
    (ErrorKind::InvalidUtf8, want),
    "{label}"
  );
  for piece in [1 << 10, 64 << 10, 1 << 20] {
    let e = berry::__validate_utf8(b, piece).unwrap_err();
    assert_eq!(e.offset(), want, "{label}: piece {piece}");
  }
}

#[test]
fn valid_input_parses_like_str() {
  let mut buffers = Buffers::new();
  for (name, src) in corpus()
    .iter()
    .map(|(n, s)| (n.as_str(), s.as_str()))
    .chain([("large", large())])
  {
    let want = berry::parse(src);
    let got = berry::parse_bytes(src.as_bytes());
    if let Err(e) = same_result(&want, &got) {
      panic!("{name}: {e}");
    }
    let got = berry::parse_bytes_with(src.as_bytes(), &mut buffers);
    if let Err(e) = same_result(&want, &got) {
      panic!("{name}, recycled: {e}");
    }
    if let Ok(lf) = got {
      assert_eq!(
        lf.source().as_ptr(),
        src.as_ptr(),
        "{name}: borrowed, not copied"
      );
      buffers = lf.into_buffers();
    }
  }
  assert!(berry::parse_bytes(b"").is_err());
  assert_eq!(berry::__validate_utf8(b"", 1).unwrap(), "");
  let ascii = "abc\n".repeat(1 << 18);
  assert_eq!(
    berry::__validate_utf8(ascii.as_bytes(), 4096).unwrap(),
    ascii
  );
}

#[test]
fn invalid_input_reports_the_std_offset_wherever_the_pieces_are_cut() {
  let src = large().as_bytes();
  let mut rng = Rng::new(0x07f8);
  let newlines: Vec<usize> = src
    .iter()
    .enumerate()
    .filter(|&(_, &c)| c == b'\n')
    .map(|(i, _)| i)
    .collect();
  let non_ascii = src.iter().position(|&c| c >= 0x80).unwrap();
  let mut cases: Vec<(String, Vec<u8>)> = Vec::new();
  // Right after a '\n' (a piece start), on a '\n' (a piece end), random.
  for _ in 0..12 {
    let nl = newlines[rng.below(newlines.len())];
    for (what, at, byte) in [
      ("after nl", nl + 1, 0xff),
      ("on nl", nl, 0x80),
      ("random", rng.below(src.len()), 0xfe),
    ] {
      let mut b = src.to_vec();
      b[at] = byte;
      cases.push((format!("{what} at {at}"), b));
    }
  }
  // A multi-byte character truncated right before a '\n'.
  let mut b = src.to_vec();
  let nl = newlines[newlines.len() / 2];
  b[nl - 1] = 0xe3;
  cases.push(("truncated before nl".to_owned(), b));
  // Inside the first non-ASCII character; at the very end.
  let mut b = src.to_vec();
  b[non_ascii + 1] = b'x';
  cases.push(("inside a character".to_owned(), b));
  let mut b = src.to_vec();
  b.push(0xc3);
  cases.push(("truncated at the end".to_owned(), b));
  // Two corruptions: the earlier one wins.
  let mut b = src.to_vec();
  b[src.len() * 3 / 4] = 0xff;
  b[src.len() / 4] = 0xff;
  cases.push(("two".to_owned(), b));

  for threads in [1, 2, 8] {
    ThreadPoolBuilder::new()
      .num_threads(threads)
      .build()
      .unwrap()
      .install(|| {
        for (label, b) in &cases {
          assert_invalid_at(b, &format!("{threads} threads, {label}"));
        }
      });
  }
  for (label, b) in &cases {
    assert_invalid_at(b, label);
  }
}

#[test]
fn small_invalid_inputs_report_the_std_offset() {
  for (name, src) in corpus().into_iter().filter(|(_, s)| s.len() <= 64 << 10) {
    let b = src.as_bytes();
    for at in (0..b.len()).step_by(7) {
      let mut m = b.to_vec();
      m[at] = 0xff;
      let e = berry::parse_bytes(&m).unwrap_err();
      assert_eq!(
        (e.kind(), Some(e.offset())),
        (ErrorKind::InvalidUtf8, std_offset(&m)),
        "{name} at {at}"
      );
    }
  }
}
