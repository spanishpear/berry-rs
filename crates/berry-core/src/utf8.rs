//! UTF-8 validation of byte input, in parallel for large inputs.
//!
//! The input is split recursively (`rayon::join`, no allocation) right after
//! a `'\n'`. A `'\n'` byte never occurs inside a multi-byte sequence, so every
//! piece of a valid input starts on a character boundary and is valid on its
//! own, and for an invalid input the first invalid piece starts at a point the
//! whole-input decoder also passes through. The reported offset is the
//! minimum over the pieces of `start + valid_up_to`, which is exactly
//! `std::str::from_utf8(input).unwrap_err().valid_up_to()`.

use crate::error::{Error, ErrorKind};
use crate::pipeline::auto_chunks;

/// Validate `b` as UTF-8 in as many pieces as the parse will use chunks.
pub fn validate(b: &[u8]) -> Result<&str, Error> {
  validate_in(b, b.len().div_ceil(auto_chunks(b.len())))
}

/// [`validate`] with pieces of about `piece` bytes (0: one piece).
pub fn validate_in(b: &[u8], piece: usize) -> Result<&str, Error> {
  let valid_up_to = if piece == 0 || b.len() <= piece {
    std::str::from_utf8(b).err().map(|e| e.valid_up_to())
  } else {
    first_invalid(b, 0, piece)
  };
  if let Some(at) = valid_up_to {
    return Err(invalid_utf8(at));
  }
  // SAFETY: every byte of `b` was checked: `valid_up_to` is `None` only if
  // `std::str::from_utf8` accepted `b` or each piece of a partition of `b`
  // (`first_invalid`), and those pieces are cut right after a `'\n'` byte,
  // which ends a character, so their concatenation `b` is valid UTF-8 too.
  Ok(unsafe { std::str::from_utf8_unchecked(b) })
}

#[cold]
#[inline(never)]
fn invalid_utf8(at: usize) -> Error {
  Error::new(ErrorKind::InvalidUtf8, at)
}

/// Offset (relative to the input start, `base` is the offset of `b`) of the
/// first byte that is not part of a valid UTF-8 prefix of the input, if any.
fn first_invalid(b: &[u8], base: usize, piece: usize) -> Option<usize> {
  if b.len() > piece
    && let Some(nl) = memchr::memchr(b'\n', b.get(b.len() / 2..).unwrap_or(&[]))
  {
    let cut = b.len() / 2 + nl + 1;
    if cut < b.len() {
      let (left, right) = b.split_at(cut);
      let (l, r) = rayon::join(
        || first_invalid(left, base, piece),
        || first_invalid(right, base + cut, piece),
      );
      return l.or(r);
    }
  }
  std::str::from_utf8(b).err().map(|e| base + e.valid_up_to())
}

#[cfg(test)]
mod tests {
  use super::*;

  fn std_offset(b: &[u8]) -> Option<usize> {
    std::str::from_utf8(b).err().map(|e| e.valid_up_to())
  }

  fn check(b: &[u8]) {
    for piece in [0, 1, 2, 3, 5, 16, 64, 1 << 20] {
      let got = validate_in(b, piece).err().map(|e| e.offset());
      assert_eq!(got, std_offset(b), "{b:?} piece {piece}");
      if let Ok(s) = validate_in(b, piece) {
        assert_eq!(s.as_bytes(), b);
      }
    }
  }

  #[test]
  fn matches_std_on_small_inputs() {
    let cases: [&[u8]; 14] = [
      b"",
      b"\n",
      b"ascii only\nlines\n",
      "\u{e9}\n\u{3000}\n\u{1f600}\n".as_bytes(),
      b"\xff",
      b"\n\xff",
      b"a\n\xffb\n",
      b"a\n\xc3",                    // truncated 2-byte char at the end
      b"a\n\xc3\nb",                 // truncated char right before '\n'
      b"\xe3\x80\n\x80",             // truncated 3-byte char, then a continuation byte
      b"ok\nok\nok\n\xed\xa0\x80\n", // surrogate
      b"\xf4\x90\x80\x80\n",         // above U+10FFFF
      b"\xc0\xaf\n",                 // overlong
      b"x\n\n\n\n\xe2\x82",
    ];
    for b in cases {
      check(b);
    }
  }

  #[test]
  fn every_single_corruption_of_a_mixed_text_matches_std() {
    let text = "a\u{e9}b\nline two \u{3000}\n\u{1f600}\n\nlast\u{e9}".repeat(3);
    for at in 0..text.len() {
      for bad in [0xff_u8, 0x80, 0xc3, 0xe3, b'\n'] {
        let mut b = text.clone().into_bytes();
        b[at] = bad;
        check(&b);
      }
    }
  }
}
