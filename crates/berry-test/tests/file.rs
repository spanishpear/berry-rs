//! `parse_file`: read path (small files, non-regular files) and mapped path
//! (regular files from 256 KiB). Both must parse exactly like `parse(&str)`
//! of the same text, and errors carry the path, line and column.

use berry::{Buffers, ErrorKind, FileError};
use berry_test::differential::same_result;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

fn fixtures_dir() -> PathBuf {
  Path::new(env!("CARGO_MANIFEST_DIR")).join("../../fixtures")
}

fn tmp(name: &str) -> PathBuf {
  let dir =
    Path::new(env!("CARGO_TARGET_TMPDIR")).join(format!("berry-file-{}", std::process::id()));
  std::fs::create_dir_all(&dir).unwrap();
  dir.join(name)
}

fn write(name: &str, contents: &[u8]) -> PathBuf {
  let p = tmp(name);
  std::fs::write(&p, contents).unwrap();
  p
}

/// ~6 MB, above the mapping and parallel thresholds.
fn large() -> &'static str {
  static LARGE: OnceLock<String> = OnceLock::new();
  LARGE.get_or_init(|| {
    let src =
      std::fs::read_to_string(fixtures_dir().join("resolutions-patches.yarn.lock")).unwrap();
    let body_at = src.find("\n\n\"").unwrap() + 2;
    let (head, body) = src.split_at(body_at);
    let body = body.trim_end_matches('\n');
    format!("{head}{body}\n\n{body}\n\n{body}\n")
  })
}

#[track_caller]
fn parse_error(
  r: Result<berry::Lockfile<'static>, FileError>,
) -> (ErrorKind, usize, usize, usize, String) {
  match r {
    Err(e @ FileError::Parse { .. }) => {
      let FileError::Parse {
        error,
        line,
        column,
        ..
      } = &e
      else {
        unreachable!()
      };
      (error.kind(), error.offset(), *line, *column, e.to_string())
    }
    Err(e) => panic!("expected a parse error, got {e}"),
    Ok(_) => panic!("expected a parse error, got a lockfile"),
  }
}

#[test]
fn files_parse_like_their_text() {
  let mut paths: Vec<PathBuf> = std::fs::read_dir(fixtures_dir())
    .unwrap()
    .map(|e| e.unwrap().path())
    .filter(|p| p.to_string_lossy().ends_with(".lock"))
    .collect();
  paths.push(write("large.yarn.lock", large().as_bytes()));
  let mut buffers = Buffers::new();
  for p in &paths {
    let text = std::fs::read_to_string(p).unwrap();
    let want = berry::parse(&text);
    let got = berry::parse_file(p).map_err(|e| e.parse_error().unwrap());
    if let Err(e) = same_result(&want, &got) {
      panic!("{}: {e}", p.display());
    }
    let got = berry::parse_file_with(p, &mut buffers).map_err(|e| e.parse_error().unwrap());
    if let Err(e) = same_result(&want, &got) {
      panic!("{}, recycled: {e}", p.display());
    }
    if let Ok(lf) = got {
      assert_eq!(lf.source(), text);
      buffers = lf.into_buffers();
    }
  }
}

#[test]
fn mapped_files_parse_alike_under_any_pool() {
  let p = write("large-pools.yarn.lock", large().as_bytes());
  let want = berry::parse(large());
  for threads in [1, 2, 8] {
    let pool = rayon::ThreadPoolBuilder::new()
      .num_threads(threads)
      .build()
      .unwrap();
    pool.install(|| {
      let got = berry::parse_file(&p).map_err(|e| e.parse_error().unwrap());
      same_result(&want, &got).unwrap();
    });
  }
}

#[test]
fn the_read_buffer_is_reused() {
  let a = write(
    "small-a.yarn.lock",
    std::fs::read(fixtures_dir().join("berry.lock"))
      .unwrap()
      .as_slice(),
  );
  let b = write(
    "small-b.yarn.lock",
    std::fs::read(fixtures_dir().join("workspaces.yarn.lock"))
      .unwrap()
      .as_slice(),
  );
  let mut buffers = Buffers::new();
  let first = berry::parse_file_with(&a, &mut buffers).unwrap();
  let ptr = first.source().as_ptr();
  buffers = first.into_buffers();
  let second = berry::parse_file_with(&b, &mut buffers).unwrap();
  assert_eq!(
    second.source().as_ptr(),
    ptr,
    "the second (smaller) file reuses the read buffer"
  );
  assert!(!second.is_empty());
}

#[test]
fn lockfiles_from_files_are_static_and_send() {
  let p = write("static.yarn.lock", large().as_bytes());
  let small = write(
    "static-small.yarn.lock",
    &std::fs::read(fixtures_dir().join("berry.lock")).unwrap(),
  );
  let lockfiles = {
    let paths = [p, small.clone()];
    [
      berry::parse_file(&paths[0]).unwrap(),
      berry::parse_file(&paths[1]).unwrap(),
    ]
  };
  std::fs::remove_file(&small).unwrap(); // the read copy does not need the file
  let handle =
    std::thread::spawn(move || lockfiles.iter().map(berry::Lockfile::len).sum::<usize>());
  let n = handle.join().unwrap();
  assert!(n > 0);
}

#[test]
fn io_errors_name_the_path() {
  let missing = tmp("missing.yarn.lock");
  let e = berry::parse_file(&missing).unwrap_err();
  assert!(matches!(e, FileError::Io { .. }));
  assert_eq!(e.path(), missing);
  assert!(e.to_string().starts_with(&missing.display().to_string()));
  assert!(std::error::Error::source(&e).is_some());
  let dir = tmp("");
  assert!(matches!(
    berry::parse_file(&dir).unwrap_err(),
    FileError::Io { .. }
  ));
}

#[test]
fn parse_errors_carry_path_line_and_column() {
  let bad = "  bogus: 1";
  let small = format!(
    "{}\n\"x@npm:1\":\n{bad}\n",
    std::fs::read_to_string(fixtures_dir().join("berry.lock"))
      .unwrap()
      .trim_end()
  );
  let big = format!("{}\n\"x@npm:1\":\n{bad}\n", large().trim_end());
  for (name, text) in [("bad-small.yarn.lock", small), ("bad-large.yarn.lock", big)] {
    let p = write(name, text.as_bytes());
    let (kind, offset, line, column, msg) = parse_error(berry::parse_file(&p));
    assert_eq!(kind, ErrorKind::UnknownProperty, "{name}");
    assert_eq!(offset, text.rfind("bogus").unwrap(), "{name}");
    assert_eq!((line, column), (text.lines().count(), 3), "{name}");
    assert_eq!(
      msg,
      format!("{}:{line}:3: unknown package property `bogus`", p.display())
    );
    // Same detail as the `&str` API's message.
    let e = berry::parse(&text).unwrap_err();
    assert_eq!(
      e.describe(&text),
      format!("unknown package property `bogus` at line {line}, column 3")
    );
  }
  // An invalid linkType names the value found, on both paths.
  for (name, base) in [
    (
      "bad-link-small.yarn.lock",
      std::fs::read_to_string(fixtures_dir().join("berry.lock")).unwrap(),
    ),
    ("bad-link-large.yarn.lock", large().to_owned()),
  ] {
    let text = format!("{}\n\"x@npm:1\":\n  linkType: HARD\n", base.trim_end());
    let p = write(name, text.as_bytes());
    let (kind, _, line, column, msg) = parse_error(berry::parse_file(&p));
    assert_eq!(kind, ErrorKind::InvalidLinkType, "{name}");
    let e = berry::parse(&text).unwrap_err();
    assert_eq!(
      msg,
      format!("{}:{line}:{column}: {}", p.display(), e.detail(&text))
    );
    assert!(msg.ends_with("(found \"HARD\")"), "{msg}");
  }
}

#[test]
fn invalid_utf8_is_reported_on_both_paths() {
  for (name, text) in [
    (
      "utf8-small.yarn.lock",
      std::fs::read(fixtures_dir().join("berry.lock")).unwrap(),
    ),
    ("utf8-large.yarn.lock", large().as_bytes().to_vec()),
  ] {
    let mut b = text.clone();
    let at = b.len() * 2 / 3;
    b[at] = 0xff;
    let p = write(name, &b);
    let (kind, offset, line, _, msg) = parse_error(berry::parse_file(&p));
    assert_eq!((kind, offset), (ErrorKind::InvalidUtf8, at), "{name}");
    assert_eq!(line, memchr_count(&b[..at]) + 1, "{name}");
    assert!(msg.ends_with(": invalid UTF-8"), "{msg}");
  }
}

#[test]
fn empty_and_special_files_use_the_read_path() {
  let empty = write("empty.yarn.lock", b"");
  assert_eq!(
    parse_error(berry::parse_file(&empty)).0,
    ErrorKind::InvalidHeader
  );
  // Size 0 in metadata but not empty (procfs), and a character device.
  for special in ["/proc/self/status", "/dev/null"] {
    if Path::new(special).exists() {
      assert_eq!(
        parse_error(berry::parse_file(special)).0,
        ErrorKind::InvalidHeader,
        "{special}"
      );
    }
  }
}

#[cfg(unix)]
#[test]
fn a_fifo_is_read_to_its_end() {
  let fifo = tmp("pipe.yarn.lock");
  drop(std::fs::remove_file(&fifo));
  let made = std::process::Command::new("mkfifo").arg(&fifo).status();
  if !made.is_ok_and(|s| s.success()) {
    eprintln!("mkfifo unavailable, skipping");
    return;
  }
  let text = large().to_owned();
  let writer = {
    let (fifo, text) = (fifo.clone(), text.clone());
    std::thread::spawn(move || std::fs::write(fifo, text).unwrap())
  };
  let got = berry::parse_file(&fifo).map_err(|e| e.parse_error().unwrap());
  writer.join().unwrap();
  same_result(&berry::parse(&text), &got).unwrap();
  assert_eq!(got.unwrap().source(), text);
}

fn memchr_count(b: &[u8]) -> usize {
  #[allow(clippy::naive_bytecount)] // a test helper
  b.iter().filter(|&&c| c == b'\n').count()
}
