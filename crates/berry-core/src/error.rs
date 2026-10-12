//! Compact parse errors.
//!
//! [`Error`] is 8 bytes and `Copy`: a byte offset into the parsed input plus an
//! [`ErrorKind`]. It carries no `String` and no copy of the remaining input, so
//! constructing one on the error path is free and the happy path is unaffected.
//! Human-readable details (line, column, offending key) are computed lazily from
//! the source text with [`Error::line_col`] and [`Error::describe`].

use std::fmt;

/// What went wrong.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
#[repr(u8)]
pub enum ErrorKind {
  /// A package property `key: value` whose key is not a known simple property
  /// (`version`, `resolution`, `languageName`, `linkType`, `checksum`,
  /// `conditions`). The offset points at the key.
  UnknownProperty,
  /// `linkType` with a value other than exactly `hard` or `soft`. The offset
  /// points at the `linkType` key.
  InvalidLinkType,
  /// The two `# ...` header comment lines and the blank line after them are
  /// missing or different.
  InvalidHeader,
  /// The `__metadata:` block (two `key: value` lines) is missing or malformed.
  InvalidMetadata,
  /// The input is `u32::MAX` bytes (4 GiB - 1) or larger: offsets are stored
  /// as `u32` and `u32::MAX` itself is reserved.
  InputTooLarge,
  /// The input given as bytes is not UTF-8. The offset is the length of the
  /// longest valid prefix (`std::str::Utf8Error::valid_up_to`).
  InvalidUtf8,
}

impl ErrorKind {
  /// Short static description.
  #[must_use]
  pub const fn as_str(self) -> &'static str {
    match self {
      Self::UnknownProperty => "unknown package property",
      Self::InvalidLinkType => "invalid linkType (expected `hard` or `soft`)",
      Self::InvalidHeader => "missing or invalid yarn lockfile header",
      Self::InvalidMetadata => "missing or invalid __metadata block",
      Self::InputTooLarge => "input too large (u32::MAX bytes or more)",
      Self::InvalidUtf8 => "invalid UTF-8",
    }
  }
}

/// A parse error: kind plus byte offset into the input.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Error {
  offset: u32,
  kind: ErrorKind,
}

impl Error {
  /// Create an error at byte `offset` (saturated to `u32::MAX`).
  #[cold]
  #[inline(never)]
  #[must_use]
  pub fn new(kind: ErrorKind, offset: usize) -> Self {
    Self {
      offset: u32::try_from(offset).unwrap_or(u32::MAX),
      kind,
    }
  }

  /// The kind of error.
  #[must_use]
  pub const fn kind(&self) -> ErrorKind {
    self.kind
  }

  /// Byte offset into the input the error refers to.
  #[must_use]
  pub const fn offset(&self) -> usize {
    self.offset as usize
  }

  /// 1-based line and column (in chars) of the error within `src`, the input
  /// that produced it. Offsets past the end are clamped to the end.
  #[must_use]
  pub fn line_col(&self, src: &str) -> (usize, usize) {
    let mut off = self.offset().min(src.len());
    while !src.is_char_boundary(off) {
      off -= 1;
    }
    let before = &src[..off];
    let line = before.bytes().filter(|&b| b == b'\n').count() + 1;
    let line_start = before.rfind('\n').map_or(0, |i| i + 1);
    let col = before[line_start..].chars().count() + 1;
    (line, col)
  }

  /// A message with line/column and the offending text, e.g.
  /// `unknown package property `someKey` at line 12, column 3`.
  #[must_use]
  pub fn describe(&self, src: &str) -> String {
    let (line, col) = self.line_col(src);
    format!("{} at line {line}, column {col}", self.detail(src))
  }

  /// The message of [`describe`](Self::describe) without the location: the
  /// kind, plus the offending key (`UnknownProperty`) or value
  /// (`InvalidLinkType`) read from `src`, e.g.
  /// ``unknown package property `someKey` ``.
  #[cold]
  #[must_use]
  pub fn detail(&self, src: &str) -> String {
    let mut off = self.offset().min(src.len());
    while !src.is_char_boundary(off) {
      off -= 1;
    }
    let rest = &src[off..];
    let token_end = rest.find([':', '\n', '\r']).unwrap_or(rest.len());
    let token = &rest[..token_end];
    match self.kind {
      ErrorKind::InvalidLinkType => {
        let value = rest[token_end..].strip_prefix(':').map_or("", |v| {
          v.split(['\n', '\r']).next().unwrap_or("").trim_start()
        });
        format!("{} (found {value:?})", self.kind.as_str())
      }
      ErrorKind::UnknownProperty => format!("{} `{token}`", self.kind.as_str()),
      _ => self.kind.as_str().to_owned(),
    }
  }
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} at byte offset {}", self.kind.as_str(), self.offset)
  }
}

impl std::error::Error for Error {}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn error_is_small_and_copy() {
    assert_eq!(std::mem::size_of::<Error>(), 8);
    let e = Error::new(ErrorKind::UnknownProperty, 3);
    let f = e;
    assert_eq!(e, f);
  }

  #[test]
  fn offset_saturates() {
    let e = Error::new(ErrorKind::InputTooLarge, usize::MAX);
    assert_eq!(e.offset(), u32::MAX as usize);
  }

  #[test]
  fn line_col_and_describe() {
    let src = "a\nbb\n  someKey: v\n";
    let off = src.find("someKey").unwrap();
    let e = Error::new(ErrorKind::UnknownProperty, off);
    assert_eq!(e.line_col(src), (3, 3));
    assert_eq!(
      e.describe(src),
      "unknown package property `someKey` at line 3, column 3"
    );
    assert_eq!(
      e.to_string(),
      format!("unknown package property at byte offset {off}")
    );

    let src = "x\n  linkType: HARD\n";
    let e = Error::new(ErrorKind::InvalidLinkType, src.find("linkType").unwrap());
    assert_eq!(
      e.describe(src),
      "invalid linkType (expected `hard` or `soft`) (found \"HARD\") at line 2, column 3"
    );
    assert_eq!(
      e.detail(src),
      "invalid linkType (expected `hard` or `soft`) (found \"HARD\")"
    );
    assert_eq!(
      Error::new(ErrorKind::InvalidHeader, 0).detail(src),
      ErrorKind::InvalidHeader.as_str()
    );
  }

  #[test]
  fn line_col_handles_unicode_and_out_of_range() {
    let src = "\u{e9}\u{e9}x";
    // offset 1 is inside the first char: clamped back to 0
    assert_eq!(
      Error::new(ErrorKind::InvalidHeader, 1).line_col(src),
      (1, 1)
    );
    assert_eq!(
      Error::new(ErrorKind::InvalidHeader, 4).line_col(src),
      (1, 3)
    );
    assert_eq!(
      Error::new(ErrorKind::InvalidHeader, 99).line_col(src),
      (1, 4)
    );
    assert_eq!(Error::new(ErrorKind::InvalidHeader, 0).line_col(""), (1, 1));
    assert!(
      Error::new(ErrorKind::InvalidHeader, 99)
        .describe("")
        .contains("line 1")
    );
  }
}
