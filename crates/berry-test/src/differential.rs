//! Differential testing of `berry` against the frozen reference parser (`berry_ref`).
//!
//! For a valid Yarn lockfile ([`check`]) the candidate must give the
//! reference's canonical dump byte for byte, or fail when the reference
//! fails. For any other input ([`robust`]) it must not panic, and its views
//! must agree with each other ([`view_consistency`]).

use std::cell::Cell;
use std::panic::{self, AssertUnwindSafe};
use std::sync::Once;

/// Result of running one parser on one input.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
  /// Parsed; the canonical dump (includes the unconsumed remainder).
  Ok(String),
  /// Returned an error: its text (reference: nom's `Debug`; candidate:
  /// `Kind@offset`) and the byte offset it points to.
  Err(String, usize),
  /// Panicked; the panic message.
  Panic(String),
}

impl Outcome {
  /// One-line description used in golden files and failure messages.
  #[must_use]
  pub fn golden_text(&self) -> String {
    match self {
      Self::Ok(dump) => dump.clone(),
      Self::Err(e, _) => format!("# error\n{e}\n"),
      Self::Panic(msg) => format!("# panic\n{msg}\n"),
    }
  }
}

thread_local! {
  static QUIET: Cell<bool> = const { Cell::new(false) };
}

static HOOK: Once = Once::new();

/// Install (once per process) a panic hook that stays silent while the
/// reference parser runs, and otherwise defers to the default hook.
fn install_quiet_hook() {
  HOOK.call_once(|| {
    let default = panic::take_hook();
    panic::set_hook(Box::new(move |info| {
      if !QUIET.with(Cell::get) {
        default(info);
      }
    }));
  });
}

fn panic_message(payload: &(dyn std::any::Any + Send)) -> String {
  payload
    .downcast_ref::<&str>()
    .map(|s| (*s).to_owned())
    .or_else(|| payload.downcast_ref::<String>().cloned())
    .unwrap_or_else(|| "<non-string panic payload>".to_owned())
}

/// Byte offset of a nom error's input within `src` (`src.len()` if it is not
/// a subslice).
fn nom_offset(src: &str, e: &nom::Err<nom::error::Error<&str>>) -> usize {
  let input = match e {
    nom::Err::Error(e) | nom::Err::Failure(e) => e.input,
    nom::Err::Incomplete(_) => return src.len(),
  };
  (input.as_ptr() as usize)
    .checked_sub(src.as_ptr() as usize)
    .filter(|&off| off + input.len() <= src.len())
    .unwrap_or(src.len())
}

/// Run the frozen reference parser.
#[must_use]
pub fn reference(src: &str) -> Outcome {
  install_quiet_hook();
  QUIET.with(|q| q.set(true));
  let r = panic::catch_unwind(AssertUnwindSafe(
    || match berry_ref::parse::parse_lockfile(src) {
      Ok((rest, lf)) => Outcome::Ok(crate::canonical_ref::to_string(&lf, rest)),
      Err(e) => Outcome::Err(format!("{e:?}"), nom_offset(src, &e)),
    },
  ));
  QUIET.with(|q| q.set(false));
  r.unwrap_or_else(|p| Outcome::Panic(panic_message(p.as_ref())))
}

/// Run the parser under test on a `berry::Lockfile` result.
fn outcome(src: &str, parsed: Result<berry::Lockfile<'_>, berry::Error>) -> Outcome {
  match parsed {
    Ok(lockfile) => {
      if let Err(e) = view_consistency(&lockfile) {
        return Outcome::Panic(format!("views inconsistent: {e}"));
      }
      if lockfile.source() != src {
        return Outcome::Panic("source() differs from the input".to_owned());
      }
      Outcome::Ok(crate::canonical::to_string(&lockfile))
    }
    Err(e) => Outcome::Err(format!("{:?}@{}", e.kind(), e.offset()), e.offset()),
  }
}

/// Run the parser under test (`berry::parse`).
#[must_use]
pub fn candidate(src: &str) -> Outcome {
  candidate_with(src, berry::parse)
}

/// Run the parser under test through `parse_fn` (e.g. a forced chunking).
pub fn candidate_with(
  src: &str,
  parse_fn: impl FnOnce(&str) -> Result<berry::Lockfile<'_>, berry::Error>,
) -> Outcome {
  install_quiet_hook();
  let r = panic::catch_unwind(AssertUnwindSafe(|| outcome(src, parse_fn(src))));
  r.unwrap_or_else(|p| Outcome::Panic(panic_message(p.as_ref())))
}

/// Everything observable about a parsed lockfile, for comparing two parses
/// of the same input.
///
/// The dump covers every field; the quadratic view-consistency check is
/// optional (skip it when comparing thousands of splits of one input).
#[must_use]
pub fn fingerprint_ok(lf: &berry::Lockfile<'_>, consistency: bool) -> String {
  if consistency && let Err(e) = view_consistency(lf) {
    panic!("views inconsistent: {e}");
  }
  format!(
    "ok len={} end={} rest={}\n{}",
    lf.len(),
    lf.entries_end(),
    lf.remaining().len(),
    crate::canonical::to_string(lf)
  )
}

/// Everything observable about a parse result (see [`fingerprint_ok`]).
#[must_use]
pub fn fingerprint(r: Result<berry::Lockfile<'_>, berry::Error>) -> String {
  match r {
    Ok(lf) => fingerprint_ok(&lf, true),
    Err(e) => format!("err {:?}@{}", e.kind(), e.offset()),
  }
}

/// Compare two parse results of the same input view by view (cheaper than
/// [`fingerprint`] for large inputs: no dump, no sorting). `Err` names the
/// first difference.
pub fn same_result(
  a: &Result<berry::Lockfile<'_>, berry::Error>,
  b: &Result<berry::Lockfile<'_>, berry::Error>,
) -> Result<(), String> {
  match (a, b) {
    (Ok(a), Ok(b)) => same_lockfile(a, b),
    (Err(x), Err(y)) if x.kind() == y.kind() && x.offset() == y.offset() => Ok(()),
    (x, y) => Err(format!(
      "outcomes differ: {:?} vs {:?}",
      x.as_ref().map(berry::Lockfile::len),
      y.as_ref().map(berry::Lockfile::len)
    )),
  }
}

/// [`same_result`] for two parsed lockfiles.
pub fn same_lockfile(a: &berry::Lockfile<'_>, b: &berry::Lockfile<'_>) -> Result<(), String> {
  if (a.len(), a.entries_end(), a.remaining()) != (b.len(), b.entries_end(), b.remaining()) {
    return Err(format!(
      "len/end/rest differ: {:?} vs {:?}",
      (a.len(), a.entries_end(), a.remaining().len()),
      (b.len(), b.entries_end(), b.remaining().len())
    ));
  }
  if a.metadata() != b.metadata() {
    return Err("preamble differs".to_owned());
  }
  for (n, (x, y)) in a.entries().zip(b.entries()).enumerate() {
    let same = x.descriptors().iter().eq(y.descriptors().iter())
      && (x.version(), x.resolution(), x.checksum(), x.conditions())
        == (y.version(), y.resolution(), y.checksum(), y.conditions())
      && (x.language_name(), x.link_type()) == (y.language_name(), y.link_type())
      && x.dependencies().iter().eq(y.dependencies().iter())
      && x
        .peer_dependencies()
        .iter()
        .eq(y.peer_dependencies().iter())
      && x.bin().iter().eq(y.bin().iter())
      && x
        .dependencies_meta()
        .iter()
        .eq(y.dependencies_meta().iter())
      && x
        .peer_dependencies_meta()
        .iter()
        .eq(y.peer_dependencies_meta().iter());
    if !same {
      return Err(format!("entry {n} differs"));
    }
  }
  Ok(())
}

/// Every view accessor must agree with the others.
///
/// `get` with the last occurrence in iteration order, `last_wins` with a
/// last-wins reduction, `lookup` with `get`, random access with iteration,
/// and `Display` with the parts.
pub fn view_consistency(lf: &berry::Lockfile<'_>) -> Result<(), String> {
  if lf.entries().len() != lf.len() || lf.entries().count() != lf.len() {
    return Err("entry count".to_owned());
  }
  if lf
    .entries()
    .rev()
    .zip((0..lf.len()).rev())
    .any(|(e, i)| lf.entry(i).map(|x| x.version()) != Some(e.version()))
  {
    return Err("reverse iteration".to_owned());
  }
  for (n, e) in lf.entries().enumerate() {
    let fail = |what: &str| Err(format!("entry {n}: {what}"));
    let Some(at) = lf.entry(n) else {
      return fail("entry(i) missing");
    };
    if at.version() != e.version() || at.descriptors().len() != e.descriptors().len() {
      return fail("entry(i) differs from iteration");
    }
    for d in e.descriptors() {
      if d.raw() != Some(d.to_string().as_str()) {
        return fail("descriptor raw/Display");
      }
      if d.to_string() != format!("{}@{}", d.ident(), d.range()) {
        return fail("descriptor Display");
      }
    }
    if e.descriptors().first() != e.descriptors().get(0) {
      return fail("descriptors first");
    }
    for deps in [e.dependencies(), e.peer_dependencies()] {
      let all: Vec<_> = deps.iter().collect();
      let lookup = deps.lookup();
      for d in &all {
        let last = all.iter().rev().find(|x| x.ident() == d.ident()).copied();
        if deps.get(d.ident().as_str()) != last || lookup.get(d.ident().as_str()) != last {
          return fail("dependency get/lookup");
        }
        if d.raw().is_some() || d.to_string() != format!("{}@{}", d.ident(), d.range()) {
          return fail("dependency raw/Display");
        }
      }
      let lw: Vec<_> = deps.last_wins().collect();
      let expected: Vec<_> = all
        .iter()
        .enumerate()
        .filter(|(i, d)| !all[i + 1..].iter().any(|x| x.ident() == d.ident()))
        .map(|(_, d)| *d)
        .collect();
      if lw != expected || lookup.len() != expected.len() {
        return fail("last_wins");
      }
    }
    let bins: Vec<_> = e.bin().iter().collect();
    for (k, _) in &bins {
      if e.bin().get(k) != bins.iter().rev().find(|b| b.0 == *k).map(|b| b.1) {
        return fail("bin get");
      }
    }
    let dm: Vec<_> = e.dependencies_meta().iter().collect();
    for (k, _) in &dm {
      if e.dependencies_meta().get(k.as_str()) != dm.iter().rev().find(|m| m.0 == *k).map(|m| m.1) {
        return fail("dependencies_meta get");
      }
    }
    let pm: Vec<_> = e.peer_dependencies_meta().iter().collect();
    for (k, _) in &pm {
      if e.peer_dependencies_meta().get(k.as_str())
        != pm.iter().rev().find(|m| m.0 == *k).map(|m| m.1)
      {
        return fail("peer_dependencies_meta get");
      }
    }
    if format!("{e:?}").is_empty() {
      return fail("Debug");
    }
  }
  Ok(())
}

/// First differing line of two dumps, for failure messages.
fn first_difference(a: &str, b: &str) -> String {
  for (n, (la, lb)) in a.lines().zip(b.lines()).enumerate() {
    if la != lb {
      return format!("line {}:\n  reference: {la}\n  candidate: {lb}", n + 1);
    }
  }
  format!(
    "one dump is a prefix of the other ({} vs {} lines)",
    a.lines().count(),
    b.lines().count()
  )
}

fn short(s: &str) -> &str {
  &s[..s.char_indices().nth(400).map_or(s.len(), |(i, _)| i)]
}

/// Check the contract for a valid lockfile; `Err` carries a human-readable
/// mismatch.
pub fn compare(src: &str) -> Result<(), String> {
  check(&reference(src), &candidate(src))
}

/// The contract for a valid lockfile: the same dump, or both fail.
pub fn check(reference: &Outcome, candidate: &Outcome) -> Result<(), String> {
  match (reference, candidate) {
    (_, Outcome::Panic(p)) => Err(format!("candidate panicked: {p}")),
    (Outcome::Ok(a), Outcome::Ok(b)) if a == b => Ok(()),
    (Outcome::Ok(a), Outcome::Ok(b)) => Err(format!("dumps differ at {}", first_difference(a, b))),
    (Outcome::Err(..), Outcome::Err(..)) => Ok(()),
    (r, c) => Err(format!(
      "outcome kinds differ:\n  reference: {}\n  candidate: {}",
      short(&r.golden_text()),
      short(&c.golden_text())
    )),
  }
}

/// The contract for any other input: no panic, consistent views.
pub fn robust(candidate: &Outcome) -> Result<(), String> {
  match candidate {
    Outcome::Panic(p) => Err(format!("candidate panicked: {p}")),
    _ => Ok(()),
  }
}

/// Edge-corpus files in shapes Yarn never writes: only [`robust`] applies.
///
/// CRLF, tabs, inline or repeated meta, unknown keys, odd indentation or
/// blank lines, no final newline, non-ASCII names, top-level blocks, ...
pub const INVALID_EDGE_FILES: &[&str] = &[
  "bad_indent",
  "bad_linktype",
  "bin_cr",
  "bin_shapes",
  "bom",
  "crlf",
  "dep_at_eof",
  "dep_no_space_after_colon",
  "dep_value_empty",
  "empty",
  "empty_linktype",
  "header_only",
  "meta_all_orders",
  "meta_inline_junk_after",
  "meta_inline_wrong_order",
  "meta_nested_blank_between",
  "metadata_only_noeol",
  "no_final_eol_after_blank_block",
  "non_ascii",
  "one_space_indent",
  "peer_meta_two_lines",
  "resolutions_constraints",
  "resolutions_only",
  "scoped_odd",
  "simple_form_block_key",
  "tabs_after_colon",
  "three_blank",
  "trailing_junk",
  "trailing_space_linktype",
  "unknown_after_eof_dep",
  "unknown_key",
  "unquoted_keys",
  "vt_ff_whitespace",
  "wrapped_key_inline",
];

/// `true` for fixture and edge-corpus file names of valid Yarn lockfiles.
#[must_use]
pub fn is_valid_corpus_file(file_name: &str) -> bool {
  !INVALID_EDGE_FILES.contains(&file_name.trim_end_matches(".yarn.lock"))
}

/// 64-bit FNV-1a, used to pin large golden dumps without committing them.
#[must_use]
pub fn fnv1a64(bytes: &[u8]) -> u64 {
  let mut h: u64 = 0xcbf2_9ce4_8422_2325;
  for &b in bytes {
    h ^= u64::from(b);
    h = h.wrapping_mul(0x0000_0100_0000_01b3);
  }
  h
}

/// A tiny deterministic PRNG (splitmix64) for the mutation fuzzer.
#[derive(Debug, Clone)]
pub struct Rng(u64);

impl Rng {
  #[must_use]
  pub const fn new(seed: u64) -> Self {
    Self(seed)
  }

  pub const fn next_u64(&mut self) -> u64 {
    self.0 = self.0.wrapping_add(0x9e37_79b9_7f4a_7c15);
    let mut z = self.0;
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    z ^ (z >> 31)
  }

  /// Uniform in `0..n` (`n > 0`).
  #[allow(clippy::cast_possible_truncation)]
  pub const fn below(&mut self, n: usize) -> usize {
    (self.next_u64() % n as u64) as usize
  }
}

/// Fragments the mutator splices in: structural bytes and whole lines that
/// exercise every branch of the grammar (and its error paths).
const FRAGMENTS: &[&str] = &[
  "\n",
  "\n\n",
  " ",
  "  ",
  "    ",
  "\t",
  "\r",
  "\r\n",
  "\"",
  ":",
  ": ",
  "@",
  "/",
  ",",
  ", ",
  "?",
  "? ",
  "{",
  "}",
  "#",
  "`",
  ";",
  "a",
  "\u{e9}",
  "\u{a0}",
  "\u{3000}",
  "\u{b}",
  "\u{c}",
  "npm:",
  "  version: 1.0.0\n",
  "  resolution: \"x@npm:1.0.0\"\n",
  "  resolution: \"@s/x@npm:1.0.0\"\n",
  "  dependencies:\n",
  "  peerDependencies:\n",
  "  bin:\n",
  "  dependenciesMeta:\n",
  "  peerDependenciesMeta:\n",
  "    dep: npm:1\n",
  "    \"@s/dep\": \"npm:^2\"\n",
  "    dep:\n      optional: true\n",
  "    dep: { built: true, optional: false }\n",
  "  linkType: soft\n",
  "  linkType: hard\n",
  "  linkType: other\n",
  "  unknownKey: value\n",
  "  languageName: node\n",
  "  checksum: 10/abc\n",
  "  conditions: os=linux\n",
  "\"n@npm:1\":\n",
  "n@npm:1, n@npm:2:\n",
  "? \"w@npm:1\"\n:\n",
  "resolutions:\n  a@1: npm:1\n",
  "constraints:\n  a@1: npm:1\n",
];

/// Apply 1..=4 random edits to `src` (always at char boundaries, so the
/// result is valid UTF-8).
#[must_use]
pub fn mutate(src: &str, rng: &mut Rng) -> String {
  let mut s = src.to_owned();
  let edits = 1 + rng.below(4);
  for _ in 0..edits {
    let boundaries: Vec<usize> = s
      .char_indices()
      .map(|(i, _)| i)
      .chain(std::iter::once(s.len()))
      .collect();
    let at = boundaries[rng.below(boundaries.len())];
    let line_start = s[..at].rfind('\n').map_or(0, |i| i + 1);
    let line_end = s[at..].find('\n').map_or(s.len(), |i| at + i + 1);
    match rng.below(9) {
      0 | 1 => s.insert_str(at, FRAGMENTS[rng.below(FRAGMENTS.len())]),
      2 => {
        // delete one char
        if at < s.len() {
          s.remove(at);
        }
      }
      3 => {
        // delete the current line
        s.replace_range(line_start..line_end, "");
      }
      4 => {
        // duplicate the current line
        let line = s[line_start..line_end].to_owned();
        s.insert_str(line_start, &line);
      }
      5 => {
        // truncate
        s.truncate(at);
      }
      6 => {
        // swap the current line with the next one
        let next_end = s[line_end..]
          .find('\n')
          .map_or(s.len(), |i| line_end + i + 1);
        let a = s[line_start..line_end].to_owned();
        let b = s[line_end..next_end].to_owned();
        s.replace_range(line_start..next_end, &(b + &a));
      }
      7 => {
        // change the indentation of the current line
        let indent = ["", " ", "  ", "   ", "    ", "      ", "\t"][rng.below(7)];
        let body = s[line_start..line_end]
          .trim_start_matches([' ', '\t'])
          .to_owned();
        s.replace_range(line_start..line_end, &(indent.to_owned() + &body));
      }
      _ => {
        // replace one char with a fragment
        if at < s.len() {
          let ch_len = s[at..].chars().next().map_or(0, char::len_utf8);
          s.replace_range(at..at + ch_len, FRAGMENTS[rng.below(FRAGMENTS.len())]);
        }
      }
    }
  }
  s
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn reference_panics_are_caught() {
    let src = "# This file is generated by running \"yarn install\" inside your project.\n\
               # Manual changes might be lost - proceed with caution!\n\n\
               __metadata:\n  version: 8\n  cacheKey: 10\n\n\
               \"a@npm:1\":\n  bogus: x\n";
    assert!(matches!(reference(src), Outcome::Panic(_)));
  }

  #[test]
  fn check_rules() {
    let ok = Outcome::Ok("x".into());
    let ok2 = Outcome::Ok("y".into());
    let rerr = Outcome::Err("nom".into(), 3);
    let cerr = Outcome::Err("InvalidMetadata@4".into(), 4);
    let panic = Outcome::Panic("boom".into());
    assert!(check(&ok, &ok).is_ok());
    assert!(check(&ok, &ok2).is_err());
    assert!(check(&rerr, &cerr).is_ok());
    assert!(check(&ok, &cerr).is_err());
    assert!(check(&rerr, &ok).is_err());
    assert!(check(&ok, &panic).is_err());
    assert!(robust(&cerr).is_ok() && robust(&ok).is_ok() && robust(&panic).is_err());
    assert!(is_valid_corpus_file("canonical.yarn.lock"));
    assert!(!is_valid_corpus_file("crlf.yarn.lock"));
  }

  #[test]
  fn mutate_is_deterministic_and_utf8() {
    let src = "a\u{e9}b\nline two\n\u{3000}x\n";
    let mut r1 = Rng::new(7);
    let mut r2 = Rng::new(7);
    for _ in 0..500 {
      let a = mutate(src, &mut r1);
      let b = mutate(src, &mut r2);
      assert_eq!(a, b);
    }
  }

  #[test]
  fn fnv_known_values() {
    assert_eq!(fnv1a64(b""), 0xcbf2_9ce4_8422_2325);
    assert_eq!(fnv1a64(b"a"), 0xaf63_dc4c_8601_ec8c);
  }
}
