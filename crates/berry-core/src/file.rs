//! [`crate::parse_file`]: read or map a lockfile, validate it and parse it.
//!
//! Regular files of [`MAP_MIN`] bytes or more are memory-mapped read-only, so
//! the parse reads the page cache directly and the validation and chunk scans
//! fault pages in parallel. Everything else is read into a reusable buffer.
//! See [`crate::parse_file`] for the contract on concurrent writes.

use crate::error::{Error, ErrorKind};
use crate::lockfile::{Buffers, Lockfile, Source};
use crate::pipeline::{Plan, auto_chunks};
use crate::scan::MAX_INPUT_LEN;
use crate::utf8;
use memmap2::{Advice, Mmap};
use rayon::prelude::*;
use std::fmt;
use std::fs::File;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

/// Regular files smaller than this are read rather than mapped (mapping
/// costs more than copying a small file: measured at the process level,
/// 300 KB map 1.4 vs read 1.5 ms, 1.9 MB 3.5 vs 4.4 ms).
pub const MAP_MIN: usize = 256 * 1024;

/// A read-only file mapping that was validated as UTF-8 when created.
pub struct MappedText {
  map: Mmap,
}

impl MappedText {
  #[inline]
  pub fn as_str(&self) -> &str {
    // SAFETY: `map_file` validated the mapping as UTF-8, and the caller
    // promises that nothing writes the file while it is mapped.
    unsafe { std::str::from_utf8_unchecked(&self.map) }
  }
}

/// Error of [`crate::parse_file`].
#[derive(Debug)]
#[non_exhaustive]
pub enum FileError {
  /// Opening, inspecting, mapping or reading the file failed.
  Io {
    /// The path given to `parse_file`.
    path: PathBuf,
    /// The I/O error.
    source: io::Error,
  },
  /// The file was read but is not a valid lockfile (or not UTF-8, or too
  /// large).
  Parse {
    /// The path given to `parse_file`.
    path: PathBuf,
    /// The parse error (kind and byte offset).
    error: Error,
    /// 1-based line of the error offset.
    line: usize,
    /// 1-based column (in characters) of the error offset.
    column: usize,
    /// The message of [`Error::detail`]: the kind plus the offending key or
    /// value, as in [`Error::describe`] for the same input.
    detail: String,
  },
}

impl FileError {
  /// The path given to `parse_file`.
  #[must_use]
  pub fn path(&self) -> &Path {
    match self {
      Self::Io { path, .. } | Self::Parse { path, .. } => path,
    }
  }

  /// The parse error, if this is one.
  #[must_use]
  pub const fn parse_error(&self) -> Option<Error> {
    match self {
      Self::Parse { error, .. } => Some(*error),
      Self::Io { .. } => None,
    }
  }

  #[cold]
  #[inline(never)]
  fn io(path: &Path, source: io::Error) -> Self {
    Self::Io {
      path: path.to_owned(),
      source,
    }
  }

  /// `text` is the input, or for invalid UTF-8 at least its valid prefix.
  #[cold]
  #[inline(never)]
  fn parse(path: &Path, error: Error, text: &str) -> Self {
    let (line, column) = error.line_col(text);
    Self::Parse {
      path: path.to_owned(),
      error,
      line,
      column,
      detail: error.detail(text),
    }
  }
}

impl fmt::Display for FileError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Io { path, source } => write!(f, "{}: {source}", path.display()),
      Self::Parse {
        path,
        line,
        column,
        detail,
        ..
      } => write!(f, "{}:{line}:{column}: {detail}", path.display()),
    }
  }
}

impl std::error::Error for FileError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Self::Io { source, .. } => Some(source),
      Self::Parse { error, .. } => Some(error),
    }
  }
}

/// Read or map `path`, validate and parse it (see [`crate::parse_file`]).
pub fn parse_file_with(path: &Path, buffers: &mut Buffers) -> Result<Lockfile<'static>, FileError> {
  let mut file = File::open(path).map_err(|e| FileError::io(path, e))?;
  let meta = file.metadata().map_err(|e| FileError::io(path, e))?;
  let len = usize::try_from(meta.len()).unwrap_or(usize::MAX);
  if meta.is_file() && len > MAX_INPUT_LEN {
    return Err(too_large(path));
  }
  if meta.is_file()
    && len >= MAP_MIN
    && let Some(mapped) = map_file(&file, path)?
  {
    return Lockfile::parse_source(Source::Mapped(mapped), buffers, Plan::Auto)
      .map_err(|(e, text)| FileError::parse(path, e, text.as_str()));
  }
  read_file(
    &mut file,
    path,
    buffers,
    if meta.is_file() { len } else { 0 },
  )
}

/// Map `file` and validate it. `Ok(None)`: mapping failed, use `read`.
fn map_file(file: &File, path: &Path) -> Result<Option<MappedText>, FileError> {
  // SAFETY: the mapping is read-only. Another process truncating or
  // rewriting the file would change it under us; `parse_file` documents that
  // the caller must not let that happen while the `Lockfile` is alive.
  let Ok(map) = (unsafe { Mmap::map(file) }) else {
    return Ok(None);
  };
  if map.len() > MAX_INPUT_LEN {
    return Err(too_large(path));
  }
  prefetch(&map);
  match utf8::validate(&map) {
    Ok(_) => Ok(Some(MappedText { map })),
    Err(e) => Err(invalid_utf8(path, e, &map)),
  }
}

#[cold]
fn invalid_utf8(path: &Path, e: Error, bytes: &[u8]) -> FileError {
  let valid = bytes
    .get(..e.offset())
    .and_then(|b| std::str::from_utf8(b).ok());
  FileError::parse(path, e, valid.unwrap_or(""))
}

/// Fill the page tables of an uncached mapping, one range per parse chunk in
/// parallel. Failed hints are ignored. On a 24 MB lockfile this saves 3 ms
/// when the file is not cached and would cost 0.7 ms when it is.
fn prefetch(map: &Mmap) {
  if looks_cached(map) {
    return;
  }
  let chunks = auto_chunks(map.len());
  if chunks == 1 {
    drop(map.advise(Advice::WillNeed));
    return;
  }
  let step = map.len().div_ceil(chunks);
  (0..chunks).into_par_iter().for_each(|i| {
    let at = (i * step).min(map.len());
    let n = step.min(map.len() - at);
    #[cfg(target_os = "linux")]
    if map.advise_range(Advice::PopulateRead, at, n).is_ok() {
      return;
    }
    drop(map.advise_range(Advice::WillNeed, at, n));
  });
}

/// Pages of a mapping sampled by [`looks_cached`].
#[cfg(target_os = "linux")]
const CACHE_SAMPLES: usize = 16;

/// `true` if [`CACHE_SAMPLES`] pages spread over `map` are all in the page
/// cache (`mincore(2)`, a few microseconds): then nothing needs to be read
/// from the device and a prefetch only costs time. A file that is only
/// partly cached is likely to miss a sample and gets the prefetch. Any
/// `mincore` failure counts as not cached.
#[cfg(target_os = "linux")]
pub fn looks_cached(map: &[u8]) -> bool {
  // Sample offsets are multiples of this, so they are page-aligned for page
  // sizes up to 64 KiB (larger pages make `mincore` fail: not cached).
  const ALIGN: usize = 64 * 1024;
  let stride = map.len() / CACHE_SAMPLES;
  (0..CACHE_SAMPLES).all(|k| {
    let off = (k * stride) / ALIGN * ALIGN;
    if off >= map.len() {
      return true;
    }
    let mut v = 0u8;
    // SAFETY: `mincore` only looks up the page of an address (it fails for
    // an unaligned or unmapped one, never reads it) and writes one status
    // byte per page of the length, here exactly one, into `v`.
    let r = unsafe { libc::mincore(map.as_ptr().add(off).cast_mut().cast(), 1, &raw mut v) };
    r == 0 && v & 1 == 1
  })
}

/// Without `mincore`, assume a prefetch is worth it.
#[cfg(target_os = "macos")]
pub fn looks_cached(_map: &[u8]) -> bool {
  false
}

/// The `read(2)` path: into the recycled buffer, then validate and parse.
fn read_file(
  file: &mut File,
  path: &Path,
  buffers: &mut Buffers,
  size_hint: usize,
) -> Result<Lockfile<'static>, FileError> {
  let mut text = std::mem::take(&mut buffers.text);
  text.clear();
  let _ = text.try_reserve_exact(size_hint.saturating_add(1));
  if let Err(e) = read_text(file, path, &mut text) {
    text.clear();
    buffers.text = text;
    return Err(e);
  }
  // SAFETY: `read_text` validated every byte of `text` as UTF-8.
  let text = unsafe { String::from_utf8_unchecked(text) };
  Lockfile::parse_source(Source::Owned(text), buffers, Plan::Auto).map_err(|(e, text)| {
    let err = FileError::parse(path, e, text.as_str());
    if let Source::Owned(text) = text {
      buffers.text = text.into_bytes();
      buffers.text.clear();
    }
    err
  })
}

fn read_text(file: &mut File, path: &Path, text: &mut Vec<u8>) -> Result<(), FileError> {
  file.read_to_end(text).map_err(|e| FileError::io(path, e))?;
  if text.len() > MAX_INPUT_LEN {
    return Err(too_large(path));
  }
  match utf8::validate(text) {
    Ok(_) => Ok(()),
    Err(e) => Err(invalid_utf8(path, e, text)),
  }
}

#[cold]
#[inline(never)]
fn too_large(path: &Path) -> FileError {
  FileError::Parse {
    path: path.to_owned(),
    error: Error::new(ErrorKind::InputTooLarge, 0),
    line: 1,
    column: 1,
    detail: ErrorKind::InputTooLarge.as_str().to_owned(),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::io::Write;

  #[test]
  fn a_file_just_written_and_read_looks_cached() {
    let path = std::env::temp_dir().join(format!("berry-cached-{}.txt", std::process::id()));
    let mut f = File::create(&path).unwrap();
    let line = "x".repeat(99) + "\n";
    for _ in 0..10_000 {
      f.write_all(line.as_bytes()).unwrap();
    }
    drop(f);
    let text = std::fs::read(&path).unwrap();
    let file = File::open(&path).unwrap();
    // SAFETY: test file, written above and not modified while mapped.
    let map = unsafe { Mmap::map(&file) }.unwrap();
    assert_eq!(&map[..], &text[..]);
    if cfg!(target_os = "linux") {
      assert!(looks_cached(&map));
    }
    drop(map);
    std::fs::remove_file(&path).unwrap();
    // Any slice: no crash, no access (heap memory, empty, one byte).
    let _ = looks_cached(&text);
    assert_eq!(looks_cached(&[]), cfg!(target_os = "linux"));
    let _ = looks_cached(&text[1..2]);
  }
}
