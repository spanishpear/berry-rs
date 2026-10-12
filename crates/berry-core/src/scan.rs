//! The lockfile grammar: a byte-cursor scanner for the entries Yarn writes.
//!
//! Each entry is a key line of descriptors, then two-space indented
//! `key: value` lines and blocks of four-space indented lines. Results are
//! `u32` spans into the text, written to the span tables of [`crate::table`].
//! Text that is not an entry ends the parse there ([`Outcome::Stopped`]).

use crate::error::{Error, ErrorKind};
use crate::table::{Index, Table};
use memchr::memchr;

/// A byte range `start..start + len` into the source.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
  pub start: u32,
  pub len: u32,
}

/// Offsets are `u32`, and `u32::MAX` marks [`Span::NONE`].
pub const MAX_INPUT_LEN: usize = u32::MAX as usize - 1;

impl Span {
  /// An absent value.
  pub const NONE: Self = Self {
    start: u32::MAX,
    len: 0,
  };

  #[inline]
  pub fn new(s: usize, e: usize) -> Self {
    debug_assert!(s <= e && e <= MAX_INPUT_LEN);
    Self {
      start: len32(s),
      len: len32(e - s),
    }
  }

  #[inline]
  pub fn get(self, src: &str) -> &str {
    let s = self.start as usize;
    &src[s..s + self.len as usize]
  }

  #[inline]
  pub fn opt(self, src: &str) -> Option<&str> {
    (self != Self::NONE).then(|| self.get(src))
  }
}

/// A name and a range: an entry descriptor or a dependency or bin line.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NameRange {
  pub name: Span,
  pub range: Span,
}

/// `[start, end)` rows of a side table.
pub type Range32 = (u32, u32);

/// One entry: its scalar fields and its rows in the side tables.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EntryRec {
  pub descs: Range32,
  pub deps: Range32,
  pub peers: Range32,
  pub bins: Range32,
  pub dmeta: Range32,
  pub pmeta: Range32,
  pub version: Span,
  pub resolution: Span,
  pub language: Span,
  pub checksum: Span,
  pub conditions: Span,
  pub link_soft: bool,
}

impl EntryRec {
  const fn empty() -> Self {
    Self {
      descs: (0, 0),
      deps: (0, 0),
      peers: (0, 0),
      bins: (0, 0),
      dmeta: (0, 0),
      pmeta: (0, 0),
      version: Span::NONE,
      resolution: Span::NONE,
      language: Span::NONE,
      checksum: Span::NONE,
      conditions: Span::NONE,
      link_soft: false,
    }
  }
}

/// The three `Option<bool>` of a dependency's meta, two bits each at these
/// shifts: `0` is `None`, `2` is `Some(false)`, `3` is `Some(true)`.
pub type PackedMeta = u8;

pub const META_BUILT: u8 = 0;
pub const META_OPTIONAL: u8 = 2;
pub const META_UNPLUGGED: u8 = 4;

#[inline]
pub const fn unpack_meta(m: PackedMeta, shift: u8) -> Option<bool> {
  match (m >> shift) & 3 {
    0 => None,
    v => Some(v == 3),
  }
}

const fn pack_meta(m: PackedMeta, shift: u8, v: bool) -> PackedMeta {
  (m & !(3 << shift)) | ((2 | v as u8) << shift)
}

/// Lengths and offsets are bounded by [`MAX_INPUT_LEN`].
#[inline]
#[allow(clippy::cast_possible_truncation)]
pub const fn len32(n: usize) -> u32 {
  n as u32
}

/// `memchr(needle, b)`, inlined for the first 32 bytes: most searches here
/// end within a short name or value, where the call to `memchr` costs more
/// than the search.
#[inline]
fn find(needle: u8, b: &[u8]) -> Option<usize> {
  const ONES: u64 = u64::from_ne_bytes([1; 8]);
  const HIGH: u64 = ONES << 7;
  let mut s = 0;
  while s < 32
    && let Some(word) = b.get(s..).and_then(<[u8]>::first_chunk)
  {
    let x = u64::from_le_bytes(*word) ^ (ONES * u64::from(needle));
    // The lowest byte of `x` that is zero gets its high bit set (higher
    // bytes may get false positives).
    let zero = x.wrapping_sub(ONES) & !x & HIGH;
    if zero != 0 {
      return Some(s + (zero.trailing_zeros() / 8) as usize);
    }
    s += 8;
  }
  memchr(needle, b.get(s..)?).map(|i| s + i)
}

/// The end of the line at `p` (its `'\n'`, or the end of the input).
#[inline]
fn line_end(b: &[u8], p: usize) -> usize {
  b.get(p..)
    .and_then(|rest| find(b'\n', rest))
    .map_or(b.len(), |i| p + i)
}

/// `true` if the text at `p` starts with `s`.
#[inline]
fn at(b: &[u8], p: usize, s: &[u8]) -> bool {
  b.get(p..).is_some_and(|rest| rest.starts_with(s))
}

/// The start of the line after the one ending at `le`.
#[inline]
fn next_line(b: &[u8], le: usize) -> usize {
  (le + 1).min(b.len())
}

/// `b[s..e]` without surrounding `"`.
#[inline]
fn unquote(b: &[u8], mut s: usize, mut e: usize) -> Span {
  while s < e && b[s] == b'"' {
    s += 1;
  }
  while e > s && b[e - 1] == b'"' {
    e -= 1;
  }
  Span::new(s, e)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Key {
  Version,
  Resolution,
  LanguageName,
  LinkType,
  Checksum,
  Conditions,
  Dependencies,
  PeerDependencies,
  Bin,
  DependenciesMeta,
  PeerDependenciesMeta,
}

/// The property key at `k0` and the position of its `':'`, picked by its
/// first byte and confirmed with one comparison.
#[inline]
fn known_key(b: &[u8], k0: usize) -> Option<(Key, usize)> {
  let rest = b.get(k0..)?;
  let is = |name: &[u8]| rest.starts_with(name) && rest.get(name.len()) == Some(&b':');
  let (name, key): (&[u8], Key) = match *rest.first()? {
    b'v' => (b"version", Key::Version),
    b'r' => (b"resolution", Key::Resolution),
    b'c' if rest.get(1) == Some(&b'h') => (b"checksum", Key::Checksum),
    b'c' => (b"conditions", Key::Conditions),
    b'l' if rest.get(1) == Some(&b'a') => (b"languageName", Key::LanguageName),
    b'l' => (b"linkType", Key::LinkType),
    b'd' if is(b"dependencies") => (b"dependencies", Key::Dependencies),
    b'd' => (b"dependenciesMeta", Key::DependenciesMeta),
    b'p' if is(b"peerDependencies") => (b"peerDependencies", Key::PeerDependencies),
    b'p' => (b"peerDependenciesMeta", Key::PeerDependenciesMeta),
    b'b' => (b"bin", Key::Bin),
    _ => return None,
  };
  is(name).then_some((key, k0 + name.len()))
}

/// The header and `__metadata` block before the first entry.
#[derive(Debug, Clone, Copy)]
pub struct Preamble {
  pub version: Span,
  pub cache_key: Span,
  /// Offset of the first entry.
  pub body: usize,
}

const HEADER: &[u8] = b"# This file is generated by running \"yarn install\" inside your project.\n# Manual changes might be lost - proceed with caution!\n\n";

pub fn preamble(src: &str) -> Result<Preamble, Error> {
  let b = src.as_bytes();
  if !b.starts_with(HEADER) {
    return Err(Error::new(ErrorKind::InvalidHeader, 0));
  }
  let bad = || Error::new(ErrorKind::InvalidMetadata, HEADER.len());
  let mut p = HEADER.len() + b"__metadata:\n".len();
  if !at(b, HEADER.len(), b"__metadata:\n") {
    return Err(bad());
  }
  let version = metadata_value(b, &mut p).ok_or_else(bad)?;
  let cache_key = metadata_value(b, &mut p).ok_or_else(bad)?;
  while b.get(p) == Some(&b'\n') {
    p += 1;
  }
  Ok(Preamble {
    version,
    cache_key,
    body: p,
  })
}

/// The value of the `  key: value` line at `p`, quotes stripped; advances
/// `p` past the line.
fn metadata_value(b: &[u8], p: &mut usize) -> Option<Span> {
  let s = *p + 2;
  let le = s + find(b'\n', b.get(s..)?)?;
  let colon = s + memchr(b':', &b[s..le])?;
  if !at(b, *p, b"  ") || b.get(colon + 1) != Some(&b' ') {
    return None;
  }
  *p = le + 1;
  Some(unquote(b, colon + 2, le))
}

/// How the entry loop over one chunk ended.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Outcome {
  /// Every entry starting in the chunk parsed; the last one ended here (at
  /// or past the chunk end).
  Reached(u32),
  /// The text at this offset is not an entry.
  Stopped(u32),
  Failed(Error),
}

/// Parse the entries starting in `[start, stop)` of `src` into `ix`. The
/// last one may run past `stop`.
pub fn scan_range(src: &str, start: usize, stop: usize, ix: &mut Index<'_>) -> Outcome {
  let b = src.as_bytes();
  let mut pos = start;
  while pos < stop {
    match entry(b, pos, ix) {
      Ok(next) => pos = next,
      Err(end) => return end,
    }
  }
  Outcome::Reached(len32(pos))
}

/// Scan the entry at `pos` and return the start of the next one. On `Err`,
/// rows already written for this entry stay in the side tables, but no
/// entry refers to them.
fn entry(b: &[u8], pos: usize, ix: &mut Index<'_>) -> Result<usize, Outcome> {
  let stop = || Outcome::Stopped(len32(pos));
  let (list, list_end, mut p) = key_line(b, pos).ok_or_else(stop)?;
  let mut rec = EntryRec::empty();
  let starts = [
    ix.descs.len(),
    ix.deps.len(),
    ix.peers.len(),
    ix.bins.len(),
    ix.dmeta.len(),
    ix.pmeta.len(),
  ];
  descriptors(b, list, list_end, &mut ix.descs).ok_or_else(stop)?;
  while b.get(p) == Some(&b' ') {
    if b.get(p + 1) != Some(&b' ') {
      return Err(stop());
    }
    let k0 = p + 2;
    let fail = |kind| Outcome::Failed(Error::new(kind, k0));
    let (key, colon) = known_key(b, k0).ok_or_else(|| fail(ErrorKind::UnknownProperty))?;
    let next = match (key, b.get(colon + 1)) {
      (Key::Dependencies, Some(b'\n')) => pairs(b, colon + 2, &mut ix.deps),
      (Key::PeerDependencies, Some(b'\n')) => pairs(b, colon + 2, &mut ix.peers),
      (Key::Bin, Some(b'\n')) => pairs(b, colon + 2, &mut ix.bins),
      (Key::DependenciesMeta, Some(b'\n')) => meta(b, colon + 2, &mut ix.dmeta),
      (Key::PeerDependenciesMeta, Some(b'\n')) => meta(b, colon + 2, &mut ix.pmeta),
      (_, Some(b' ')) => {
        let (v, le) = (colon + 2, line_end(b, colon + 2));
        match key {
          Key::Version => rec.version = unquote(b, v, le),
          Key::Resolution => rec.resolution = unquote(b, v, le),
          Key::LanguageName => rec.language = Span::new(v, le),
          Key::Checksum => rec.checksum = Span::new(v, le),
          Key::Conditions => rec.conditions = Span::new(v, le),
          Key::LinkType => {
            rec.link_soft = match &b[v..le] {
              b"hard" => false,
              b"soft" => true,
              _ => return Err(fail(ErrorKind::InvalidLinkType)),
            };
          }
          _ => return Err(stop()),
        }
        Some(next_line(b, le))
      }
      _ => None,
    };
    p = next.ok_or_else(stop)?;
  }
  while b.get(p) == Some(&b'\n') {
    p += 1;
  }
  let rows = |start: usize, t: usize| (len32(start), len32(t));
  rec.descs = rows(starts[0], ix.descs.len());
  rec.deps = rows(starts[1], ix.deps.len());
  rec.peers = rows(starts[2], ix.peers.len());
  rec.bins = rows(starts[3], ix.bins.len());
  rec.dmeta = rows(starts[4], ix.dmeta.len());
  rec.pmeta = rows(starts[5], ix.pmeta.len());
  ix.entries.push(rec);
  Ok(p)
}

/// The descriptor list of the key line at `pos` and the start of the line
/// after the key. Yarn writes `"a@1, b@2":`, `a@1:` (no special characters)
/// or, for keys over 1024 bytes, `? "a@1, ..."` followed by a `:` line.
fn key_line(b: &[u8], pos: usize) -> Option<(usize, usize, usize)> {
  let le = pos + find(b'\n', b.get(pos..)?)?;
  match &b[pos..le] {
    [b'?', b' ', b'"', list @ .., b'"'] if at(b, le + 1, b":\n") => {
      Some((pos + 3, pos + 3 + list.len(), le + 3))
    }
    [b'"', list @ .., b'"', b':'] => Some((pos + 1, pos + 1 + list.len(), le + 1)),
    [list @ .., b':'] => Some((pos, pos + list.len(), le + 1)),
    _ => None,
  }
}

/// `name@range` items of `b[s..e]`, separated by `", "`. A name ends at its
/// first `@` after the first byte (a scoped name starts with `@`).
fn descriptors(b: &[u8], mut q: usize, e: usize, out: &mut Table<'_, NameRange>) -> Option<()> {
  loop {
    let end = memchr(b',', &b[q..e]).map_or(e, |i| q + i);
    let at = q + 1 + memchr(b'@', b.get(q + 1..end)?)?;
    // berry 0.6 trims trailing whitespace inside a quoted range; Yarn keeps it.
    let range = b[at + 1..end].trim_ascii_end();
    out.push(NameRange {
      name: Span::new(q, at),
      range: Span::new(at + 1, at + 1 + range.len()),
    });
    if end == e {
      return Some(());
    }
    q = end + 1;
    while q < e && b[q] == b' ' {
      q += 1;
    }
  }
}

/// The `    name: value` lines of a `dependencies`, `peerDependencies` or
/// `bin` block at `p`, quotes stripped. Returns the end of the block.
fn pairs(b: &[u8], mut p: usize, out: &mut Table<'_, NameRange>) -> Option<usize> {
  while at(b, p, b"    ") {
    let s = p + 4;
    let colon = s + find(b':', &b[s..])?;
    if b.get(colon + 1) != Some(&b' ') {
      return None;
    }
    let le = line_end(b, colon + 2);
    out.push(NameRange {
      name: unquote(b, s, colon),
      range: unquote(b, colon + 2, le),
    });
    p = next_line(b, le);
  }
  Some(p)
}

const META_KEYS: [(&[u8], u8); 3] = [
  (b"built: ", META_BUILT),
  (b"optional: ", META_OPTIONAL),
  (b"unplugged: ", META_UNPLUGGED),
];

/// The `    name:` lines of a `dependenciesMeta` or `peerDependenciesMeta`
/// block at `p`, each with its `      key: true|false` lines. Returns the end
/// of the block.
fn meta(b: &[u8], mut p: usize, out: &mut Table<'_, (Span, PackedMeta)>) -> Option<usize> {
  while at(b, p, b"    ") {
    let s = p + 4;
    let le = line_end(b, s);
    if !b[s..le].ends_with(b":") {
      return None;
    }
    let name = unquote(b, s, le - 1);
    p = next_line(b, le);
    let mut m = 0;
    while at(b, p, b"      ") {
      let le = line_end(b, p + 6);
      let line = &b[p + 6..le];
      let (shift, value) = META_KEYS
        .iter()
        .find_map(|&(key, shift)| Some((shift, line.strip_prefix(key)?)))?;
      m = pack_meta(m, shift, value == b"true");
      p = next_line(b, le);
    }
    out.push((name, m));
  }
  Some(p)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{ErrorKind, Lockfile};

  const HEAD: &str = "# This file is generated by running \"yarn install\" inside your project.\n# Manual changes might be lost - proceed with caution!\n\n__metadata:\n  version: 8\n  cacheKey: 10\n\n";

  const ENTRY: &str = "\"@s/a@npm:^1.0.0, @s/a@npm:^1.1.0 \":\n  version: 1.1.0\n  resolution: \"@s/a@npm:1.1.0\"\n  dependencies:\n    b: \"npm:^2\"\n    \"@s/c\": npm:3\n  peerDependencies:\n    d: \"*\"\n  dependenciesMeta:\n    b:\n      built: true\n      unplugged: false\n  peerDependenciesMeta:\n    d:\n      optional: true\n  bin:\n    a: ./a.js\n  checksum: 10/ab\n  conditions: os=linux\n  languageName: node\n  linkType: soft\n";

  fn parse(body: &str) -> Result<Lockfile<'static>, Error> {
    let src: &'static str = Box::leak(format!("{HEAD}{body}").into_boxed_str());
    crate::parse(src)
  }

  #[test]
  fn every_field_of_an_entry() {
    let lf = parse(&format!("{ENTRY}\nnext@npm:1:\n  version: 2\n")).unwrap();
    assert_eq!((lf.len(), lf.remaining()), (2, ""));
    let e = lf.entry(0).unwrap();
    let descs: Vec<_> = e.descriptors().iter().map(|d| d.to_string()).collect();
    assert_eq!(descs, ["@s/a@npm:^1.0.0", "@s/a@npm:^1.1.0"]);
    assert_eq!(e.version(), Some("1.1.0"));
    assert_eq!(e.resolution(), Some("@s/a@npm:1.1.0"));
    let deps: Vec<_> = e.dependencies().iter().map(|d| d.to_string()).collect();
    assert_eq!(deps, ["b@npm:^2", "@s/c@npm:3"]);
    assert_eq!(e.peer_dependencies().get("d").unwrap().range(), "*");
    let m = e.dependencies_meta().get("b").unwrap();
    assert_eq!(
      (m.built, m.optional, m.unplugged),
      (Some(true), None, Some(false))
    );
    assert!(e.peer_dependencies_meta().get("d").unwrap().optional);
    assert_eq!(e.bin().get("a"), Some("./a.js"));
    assert_eq!(
      (e.checksum(), e.conditions()),
      (Some("10/ab"), Some("os=linux"))
    );
    assert_eq!(e.language_name(), "node");
    assert_eq!(e.link_type(), crate::LinkType::Soft);
    assert_eq!(
      lf.entry(1)
        .unwrap()
        .descriptors()
        .first()
        .unwrap()
        .to_string(),
      "next@npm:1"
    );
  }

  #[test]
  fn preamble_errors() {
    assert_eq!(
      crate::parse("").unwrap_err().kind(),
      ErrorKind::InvalidHeader
    );
    for meta in [
      "",
      "__metadata:\n",
      "__metadata:\n  version: 8\n",
      "__metadata:\n version: 8\n  cacheKey: 1\n",
    ] {
      let src = format!("{}{meta}", &HEAD[..HEADER.len()]);
      assert_eq!(
        crate::parse(&src).unwrap_err().kind(),
        ErrorKind::InvalidMetadata,
        "{meta:?}"
      );
    }
    let lf = crate::parse("# This file is generated by running \"yarn install\" inside your project.\n# Manual changes might be lost - proceed with caution!\n\n__metadata:\n  version: \"8\"\n  cacheKey: 10c0\n").unwrap();
    assert_eq!(
      (lf.metadata().version, lf.metadata().cache_key),
      ("8", "10c0")
    );
    assert!(lf.is_empty());
  }

  #[test]
  fn wrapped_key_line() {
    let lf = parse("? \"w@npm:1, w@npm:2\"\n:\n  version: 1\n").unwrap();
    assert_eq!(lf.entry(0).unwrap().descriptors().len(), 2);
    assert_eq!(lf.entry(0).unwrap().version(), Some("1"));
  }

  #[test]
  fn unknown_keys_and_link_types_are_errors() {
    let e = parse("a@1:\n  version: 1\n  bogus: x\n").unwrap_err();
    assert_eq!(e.kind(), ErrorKind::UnknownProperty);
    assert_eq!(e.offset(), HEAD.len() + 20);
    let e = parse("a@1:\n  linkType: HARD\n").unwrap_err();
    assert_eq!(e.kind(), ErrorKind::InvalidLinkType);
  }

  #[test]
  fn text_that_is_not_an_entry_ends_the_parse() {
    let lf = parse("a@1:\n  version: 1\n\nnot an entry\n").unwrap();
    assert_eq!(lf.len(), 1);
    assert_eq!(lf.remaining(), "not an entry\n");
  }

  /// Every prefix and every one-byte deletion parses or fails without a panic.
  #[test]
  fn truncations_and_deletions_never_panic() {
    let src = format!("{HEAD}{ENTRY}\n? \"w@npm:1\"\n:\n  version: 1\n");
    for end in 0..=src.len() {
      if let Some(prefix) = src.get(..end) {
        drop(crate::parse(prefix));
      }
    }
    for i in 0..src.len() {
      let mut s = src.clone();
      s.remove(i);
      drop(crate::parse(&s));
    }
  }

  #[test]
  fn find_matches_memchr() {
    for len in 0..80 {
      for at in 0..len {
        for c in [b'\n', b':', 0, 0x8a, b'a'] {
          let mut b = vec![b'x'; len];
          b[at] = c;
          if at + 3 < len {
            b[at + 3] = b'\n';
          }
          for needle in *b"\n:" {
            assert_eq!(find(needle, &b), memchr(needle, &b), "{len} {at} {c}");
          }
        }
      }
    }
  }

  #[test]
  fn meta_packs_three_options() {
    let m = pack_meta(pack_meta(0, META_BUILT, true), META_UNPLUGGED, false);
    assert_eq!(unpack_meta(m, META_BUILT), Some(true));
    assert_eq!(unpack_meta(m, META_OPTIONAL), None);
    assert_eq!(unpack_meta(m, META_UNPLUGGED), Some(false));
    assert_eq!(
      unpack_meta(pack_meta(m, META_BUILT, false), META_BUILT),
      Some(false)
    );
  }
}
