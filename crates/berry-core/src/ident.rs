//! Package names, descriptors, ranges and locators.
//!
//! Types from
//! <https://github.com/yarnpkg/berry/blob/master/packages/yarnpkg-core/sources/types.ts#L19>.
//! They are `Copy` views that borrow the lockfile text: nothing is allocated
//! and every derived part (scope, name, protocol, selector) is computed on
//! access with the same rules as berry 0.6.

use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};

/// Split a package name into scope and name exactly as berry 0.6 did.
///
/// A name that starts with `@` and contains a `/` is scoped, with the scope
/// running up to the first `/` (`"@babel/code-frame"` -> `(Some("@babel"),
/// "code-frame")`); anything else is unscoped and the name is the whole text
/// (`"@noslash"` -> `(None, "@noslash")`).
#[inline]
#[must_use]
pub fn split_name(full: &str) -> (Option<&str>, &str) {
  let b = full.as_bytes();
  if b.first() == Some(&b'@')
    && let Some(i) = memchr::memchr(b'/', &b[1..])
  {
    // `1 + i` is the ASCII '/', so both slices are on char boundaries.
    return (Some(&full[..=i]), &full[i + 2..]);
  }
  (None, full)
}

/// A package name as written in the lockfile: `@scope/name` or `name`.
///
/// [`scope`](Self::scope) and [`name`](Self::name) split it with the berry 0.6
/// rule ([`split_name`]). Equality and hashing use the full text (the split is
/// a bijection, so this is the same as comparing `(scope, name)`); ordering is
/// by `(scope, name)` with unscoped names first.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident<'a> {
  full: &'a str,
}

impl<'a> Ident<'a> {
  /// The ident of a full name (`@scope/name` or `name`).
  #[inline]
  #[must_use]
  pub const fn new(full: &'a str) -> Self {
    Self { full }
  }

  /// The full name as written, e.g. `"@babel/core"` or `"lodash"` (quotes
  /// stripped).
  #[inline]
  #[must_use]
  pub const fn as_str(&self) -> &'a str {
    self.full
  }

  /// The scope including its `@`, e.g. `Some("@babel")` for `@babel/core`.
  #[inline]
  #[must_use]
  pub fn scope(&self) -> Option<&'a str> {
    split_name(self.full).0
  }

  /// The name without the scope, e.g. `"core"` for `@babel/core`.
  #[inline]
  #[must_use]
  pub fn name(&self) -> &'a str {
    split_name(self.full).1
  }

  /// `(scope, name)` in one split.
  #[inline]
  #[must_use]
  pub fn parts(&self) -> (Option<&'a str>, &'a str) {
    split_name(self.full)
  }
}

impl Ord for Ident<'_> {
  fn cmp(&self, other: &Self) -> Ordering {
    self.parts().cmp(&other.parts())
  }
}

impl PartialOrd for Ident<'_> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl fmt::Display for Ident<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(self.full)
  }
}

impl fmt::Debug for Ident<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let (scope, name) = self.parts();
    f.debug_struct("Ident")
      .field("scope", &scope)
      .field("name", &name)
      .finish()
  }
}

impl AsRef<str> for Ident<'_> {
  fn as_ref(&self) -> &str {
    self.full
  }
}

/// The range of a descriptor (`npm:^1.2.3`, `workspace:.`, `*`, ...), split
/// at its first `:` into protocol and selector.
///
/// Every accessor returns slices of the lockfile (`&'a str`), not of the
/// `Range` value, so a `Range` can be built on demand and dropped.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Range<'a> {
  raw: &'a str,
  /// Index of the first ':' in `raw`.
  sep: Option<usize>,
}

impl<'a> Range<'a> {
  /// Create a Range from a raw string slice like "npm:^1.2.3" or "*".
  #[inline]
  #[must_use]
  pub fn from_raw(raw: &'a str) -> Self {
    Self {
      raw,
      sep: memchr::memchr(b':', raw.as_bytes()),
    }
  }

  /// The raw range string as stored in the lockfile.
  #[inline]
  #[must_use]
  pub const fn raw(&self) -> &'a str {
    self.raw
  }

  /// The protocol substring if present (e.g., "npm", "workspace", "patch").
  #[inline]
  #[must_use]
  pub fn protocol_str(&self) -> Option<&'a str> {
    self.sep.map(|i| &self.raw[..i])
  }

  /// The selector part (e.g., "^1.2.3", "packages/a", or the full raw when no
  /// protocol).
  #[inline]
  #[must_use]
  pub fn selector(&self) -> &'a str {
    self.sep.map_or(self.raw, |i| &self.raw[i + 1..])
  }
}

/// Same output as the derived `Debug` of berry 0.6.0.
impl fmt::Debug for Range<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Range")
      .field("raw", &self.raw)
      .field("protocol_sep_index", &self.sep)
      .finish()
  }
}

impl fmt::Display for Range<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(self.raw)
  }
}

/// Known protocols supported by Yarn descriptors
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Protocol {
  Npm,
  Workspace,
  Patch,
  Git,
  File,
  Portal,
  Exec,
  Link,
  Unknown,
}

impl<'a> Range<'a> {
  /// Returns a coarse-grained protocol classification without allocations.
  #[must_use]
  pub fn protocol(&self) -> Protocol {
    match self.protocol_str() {
      Some("npm") => Protocol::Npm,
      Some("workspace") => Protocol::Workspace,
      Some("patch") => Protocol::Patch,
      Some("file") => Protocol::File,
      Some("portal") => Protocol::Portal,
      Some("exec") => Protocol::Exec,
      Some("link") => Protocol::Link,
      Some(p) if p.starts_with("git") => Protocol::Git,
      Some(_) | None => Protocol::Unknown,
    }
  }

  fn selector_if(&self, p: Protocol) -> Option<&'a str> {
    (self.protocol() == p).then(|| self.selector())
  }

  /// If protocol is npm, returns the semver range selector (e.g., "^1.2.3").
  #[must_use]
  pub fn as_npm_range(&self) -> Option<&'a str> {
    self.selector_if(Protocol::Npm)
  }

  /// If protocol is workspace, returns the relative workspace path.
  #[must_use]
  pub fn as_workspace_path(&self) -> Option<&'a str> {
    self.selector_if(Protocol::Workspace)
  }

  /// If protocol is link, returns the path.
  #[must_use]
  pub fn as_link_path(&self) -> Option<&'a str> {
    self.selector_if(Protocol::Link)
  }

  /// If protocol is file, returns the path.
  #[must_use]
  pub fn as_file_path(&self) -> Option<&'a str> {
    self.selector_if(Protocol::File)
  }

  /// If protocol is portal, returns the path.
  #[must_use]
  pub fn as_portal_path(&self) -> Option<&'a str> {
    self.selector_if(Protocol::Portal)
  }

  /// If protocol is exec, returns the command string.
  #[must_use]
  pub fn as_exec_command(&self) -> Option<&'a str> {
    self.selector_if(Protocol::Exec)
  }

  /// If protocol is git, returns (url, optional fragment) where fragment is after '#'.
  #[must_use]
  pub fn as_git_url_and_fragment(&self) -> Option<(&'a str, Option<&'a str>)> {
    match self.protocol() {
      Protocol::Git => {
        // Return full URL including scheme (e.g., git+ssh://...), not just selector
        let raw = self.raw();
        raw.find('#').map_or(Some((raw, None)), |pos| {
          Some((&raw[..pos], Some(&raw[pos + 1..])))
        })
      }
      _ => None,
    }
  }

  /// If protocol is patch, returns (inner, optional source) split at '#'.
  #[must_use]
  pub fn as_patch_inner_and_source(&self) -> Option<(&'a str, Option<&'a str>)> {
    match self.protocol() {
      Protocol::Patch => {
        let sel = self.selector();
        sel.find('#').map_or(Some((sel, None)), |pos| {
          Some((&sel[..pos], Some(&sel[pos + 1..])))
        })
      }
      _ => None,
    }
  }
}

/// A descriptor: an [`Ident`] plus a range, e.g. `@babel/core@npm:^7.0.0`
/// (an entry key) or `"@babel/core": "npm:^7.0.0"` (a dependency line).
///
/// Yarn's `parseRange` to turn a descriptor string into this data structure,
/// `makeDescriptor` to create a new one from an ident and a range, or
/// `stringifyDescriptor` to generate a string representation of it.
///
/// Equality and hashing compare the ident and the range text.
#[derive(Clone, Copy)]
pub struct Descriptor<'a> {
  /// Lockfile text from the start of the name to the end of the range.
  text: &'a str,
  /// Length of the name at the start of `text`.
  name_len: u32,
  /// Start of the range within `text` (`>= name_len`).
  range_start: u32,
}

impl<'a> Descriptor<'a> {
  /// `text` runs from the name's start to the range's end; the name is
  /// `text[..name_len]` and the range `text[range_start..]`.
  #[inline]
  pub(crate) fn from_text(text: &'a str, name_len: u32, range_start: u32) -> Self {
    debug_assert!(name_len <= range_start && range_start as usize <= text.len());
    Self {
      text,
      name_len,
      range_start,
    }
  }

  /// The name (`@scope/name` or `name`).
  #[inline]
  #[must_use]
  pub fn ident(&self) -> Ident<'a> {
    Ident::new(self.text.get(..self.name_len as usize).unwrap_or(""))
  }

  /// The raw range string, e.g. `"npm:^1.0.0"`.
  #[inline]
  #[must_use]
  pub fn range(&self) -> &'a str {
    self.text.get(self.range_start as usize..).unwrap_or("")
  }

  /// The range split into protocol and selector.
  #[inline]
  #[must_use]
  pub fn range_struct(&self) -> Range<'a> {
    Range::from_raw(self.range())
  }

  /// The descriptor exactly as written, `name@range`, when name and range are
  /// adjacent in the lockfile: always for entry descriptors (the key line),
  /// never for dependency lines (`name: range`). Same text as
  /// [`Display`](fmt::Display), without allocating.
  #[inline]
  #[must_use]
  pub fn raw(&self) -> Option<&'a str> {
    let n = self.name_len as usize;
    (self.range_start as usize == n + 1 && self.text.as_bytes().get(n) == Some(&b'@'))
      .then_some(self.text)
  }
}

impl PartialEq for Descriptor<'_> {
  fn eq(&self, other: &Self) -> bool {
    self.ident() == other.ident() && self.range() == other.range()
  }
}

impl Eq for Descriptor<'_> {}

impl Hash for Descriptor<'_> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.ident().hash(state);
    self.range().hash(state);
  }
}

/// `name@range`, Yarn's `stringifyDescriptor`.
impl fmt::Display for Descriptor<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.raw() {
      Some(raw) => f.write_str(raw),
      None => write!(f, "{}@{}", self.ident().as_str(), self.range()),
    }
  }
}

/// Same output as the derived `Debug` of berry 0.6.0.
impl fmt::Debug for Descriptor<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Descriptor")
      .field("ident", &self.ident())
      .field("range", &self.range_struct())
      .finish()
  }
}

/// An ident plus the reference of exactly one package: `lodash@npm:4.17.21`.
///
/// Unlike a descriptor's range, a reference matches one package. Derived from
/// an entry's `resolution` ([`crate::EntryView::resolution_locator`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Locator<'a> {
  ident: Ident<'a>,
  /// A package reference uniquely identifies a package (eg. `1.2.3`).
  reference: &'a str,
}

impl<'a> Locator<'a> {
  /// Create a new Locator from an Ident and a reference (borrowed)
  #[must_use]
  pub const fn new(ident: Ident<'a>, reference: &'a str) -> Self {
    Self { ident, reference }
  }

  /// Returns the Ident of the Locator (e.g. `@scope/package`)
  #[must_use]
  pub const fn ident(&self) -> Ident<'a> {
    self.ident
  }

  /// Returns the reference of the Locator (e.g. `1.2.3`)
  #[must_use]
  pub const fn reference(&self) -> &'a str {
    self.reference
  }

  /// Split a `resolution` value (quotes already stripped) into a locator
  /// exactly as berry 0.6 did: at the first `@`, or for a scoped name at the
  /// first `@` after the first `/`; leading `@`s are trimmed from the
  /// reference. `None` if there is no such `@`.
  #[must_use]
  pub fn from_resolution(raw: &'a str) -> Option<Self> {
    let b = raw.as_bytes();
    let at = if b.first() == Some(&b'@') {
      memchr::memchr(b'/', b).and_then(|sl| memchr::memchr(b'@', &b[sl..]).map(|i| sl + i))
    } else {
      memchr::memchr(b'@', b)
    }?;
    // `at` is an ASCII '@', so both halves are on char boundaries.
    let (name, reference) = raw.split_at(at);
    Some(Self::new(
      Ident::new(name),
      reference.trim_start_matches('@'),
    ))
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn views_are_small_and_debug_is_readable() {
    assert_eq!(std::mem::size_of::<Ident<'_>>(), 16);
    assert_eq!(std::mem::size_of::<Descriptor<'_>>(), 24);
    assert_eq!(
      format!("{:?}", Range::from_raw("npm:^1")),
      "Range { raw: \"npm:^1\", protocol_sep_index: Some(3) }"
    );
    assert_eq!(
      format!("{:?}", Range::from_raw("*")),
      "Range { raw: \"*\", protocol_sep_index: None }"
    );
    assert_eq!(
      format!("{:?}", Ident::new("@s/a")),
      "Ident { scope: Some(\"@s\"), name: \"a\" }"
    );
    let r = Range::from_raw(":lead");
    assert_eq!((r.protocol_str(), r.selector()), (Some(""), "lead"));
  }

  #[test]
  fn ident_accessors_order_and_display() {
    let a = Ident::new("@b/a");
    assert_eq!((a.scope(), a.name(), a.as_str()), (Some("@b"), "a", "@b/a"));
    assert_eq!(a.to_string(), "@b/a");
    let mut v = [
      Ident::new("@a/z"),
      Ident::new("z"),
      Ident::new("@a/b"),
      Ident::new("a"),
    ];
    v.sort();
    let s: Vec<_> = v.iter().map(Ident::as_str).collect();
    assert_eq!(
      s,
      ["a", "z", "@a/b", "@a/z"],
      "unscoped first, then by scope and name"
    );
    assert_eq!(Ident::new("x"), Ident::new("x"));
    assert_ne!(Ident::new("@x/y"), Ident::new("y"));
  }

  #[test]
  fn descriptor_parts_raw_and_display() {
    // Entry key: name and range adjacent.
    let d = Descriptor::from_text("@s/a@npm:^1", 4, 5);
    assert_eq!(d.ident().as_str(), "@s/a");
    assert_eq!(d.range(), "npm:^1");
    assert_eq!(d.raw(), Some("@s/a@npm:^1"));
    assert_eq!(d.to_string(), "@s/a@npm:^1");
    assert_eq!(d.range_struct().protocol(), Protocol::Npm);
    // Dependency line: `"@s/a": "npm:^1` (quotes and colon between).
    let text = "@s/a\": \"npm:^1";
    let dep = Descriptor::from_text(text, 4, 8);
    assert_eq!(dep.range(), "npm:^1");
    assert_eq!(dep.raw(), None);
    assert_eq!(dep.to_string(), "@s/a@npm:^1");
    assert_eq!(d, dep, "same ident and range");
    let mut h1 = std::collections::hash_map::DefaultHasher::new();
    let mut h2 = std::collections::hash_map::DefaultHasher::new();
    d.hash(&mut h1);
    dep.hash(&mut h2);
    assert_eq!(h1.finish(), h2.finish());
    // Name ending in '@'-free text followed by ':' is never raw.
    assert_eq!(Descriptor::from_text("a: b", 1, 3).raw(), None);
    // Out-of-range offsets never panic.
    let bad = Descriptor::from_text("ab", 1, 2);
    assert_eq!((bad.ident().as_str(), bad.range()), ("a", ""));
    assert!(!format!("{bad:?}").is_empty());
  }

  /// Matches berry 0.6's derived `Debug` (`range` is a `Range`, not a `&str`).
  #[test]
  fn descriptor_debug_matches_0_6() {
    // Structs shaped like 0.6's, with the derived `Debug`.
    #[allow(dead_code)]
    mod v06 {
      #[derive(Debug)]
      pub struct Ident<'a> {
        pub scope: Option<&'a str>,
        pub name: &'a str,
      }
      #[derive(Debug)]
      pub struct Range<'a> {
        pub raw: &'a str,
        pub protocol_sep_index: Option<usize>,
      }
      #[derive(Debug)]
      pub struct Descriptor<'a> {
        pub ident: Ident<'a>,
        pub range: Range<'a>,
      }
    }
    let d = Descriptor::from_text("@s/a@npm:^1", 4, 5);
    assert_eq!(
      format!("{d:?}"),
      "Descriptor { ident: Ident { scope: Some(\"@s\"), name: \"a\" }, \
       range: Range { raw: \"npm:^1\", protocol_sep_index: Some(3) } }"
    );
    let dep = Descriptor::from_text("a\": \"*", 1, 5);
    assert_eq!(
      format!("{dep:?}"),
      "Descriptor { ident: Ident { scope: None, name: \"a\" }, \
       range: Range { raw: \"*\", protocol_sep_index: None } }"
    );
    // Pretty form too.
    let old = v06::Descriptor {
      ident: v06::Ident {
        scope: Some("@s"),
        name: "a",
      },
      range: v06::Range {
        raw: "npm:^1",
        protocol_sep_index: Some(3),
      },
    };
    assert_eq!(format!("{d:#?}"), format!("{old:#?}"));
    assert_eq!(format!("{d:?}"), format!("{old:?}"));
  }

  /// `split_name` must reproduce berry 0.6's `parse_name_to_ident`.
  #[test]
  fn split_name_matches_the_0_6_rule() {
    fn reference(name_part: &str) -> (Option<&str>, &str) {
      name_part
        .strip_prefix('@')
        .map_or((None, name_part), |stripped| {
          stripped
            .split_once('/')
            .map_or((None, name_part), |(scope, name)| {
              (Some(&name_part[..=scope.len()]), name)
            })
        })
    }
    let names = [
      "",
      "@",
      "@/",
      "/x",
      "x/",
      "a",
      "@a",
      "@a/",
      "@/b",
      "@a/b",
      "@a/b/c",
      "a/b",
      "@@a/b",
      "@\u{e9}/\u{fc}",
      "\u{e9}",
      "@s/@x",
      "@a//b",
      "a@b/c",
    ];
    for n in names {
      assert_eq!(split_name(n), reference(n), "{n:?}");
      let i = Ident::new(n);
      assert_eq!((i.scope(), i.name()), reference(n), "{n:?}");
    }
  }

  /// `from_resolution` must reproduce berry 0.6's split in
  /// `parse_package_properties`.
  #[test]
  fn from_resolution_matches_the_0_6_rule() {
    fn reference(raw: &str) -> Option<(Option<&str>, &str, &str)> {
      let at_index = if raw.starts_with('@') {
        raw
          .find('/')
          .and_then(|slash| raw[slash..].find('@').map(|i| slash + i))
      } else {
        raw.find('@')
      }?;
      let (name_part, reference_with_at) = raw.split_at(at_index);
      let ident = Ident::new(name_part);
      Some((
        ident.scope(),
        ident.name(),
        reference_with_at.trim_start_matches('@'),
      ))
    }
    for raw in [
      "a@npm:1",
      "@s/a@npm:1",
      "a@@npm:1",
      "@s/a",
      "plain",
      "@noslash@1",
      "@s/a@",
      "",
      "@",
      "@@",
      "@/@",
      "a/b@c",
      "@s/a/b@npm:1@2",
      "\u{e9}@\u{fc}",
      "@\u{e9}/x@@@y",
    ] {
      let got =
        Locator::from_resolution(raw).map(|l| (l.ident().scope(), l.ident().name(), l.reference()));
      assert_eq!(got, reference(raw), "{raw:?}");
    }
  }

  #[test]
  fn test_range_no_protocol() {
    let r = Range::from_raw("*");
    assert_eq!(r.raw(), "*");
    assert_eq!(r.protocol_str(), None);
    assert_eq!(r.selector(), "*");
  }

  #[test]
  fn test_range_with_npm_protocol() {
    let r = Range::from_raw("npm:^2.0.0");
    assert_eq!(r.raw(), "npm:^2.0.0");
    assert_eq!(r.protocol_str(), Some("npm"));
    assert_eq!(r.selector(), "^2.0.0");
    assert_eq!(r.protocol(), Protocol::Npm);
  }

  #[test]
  fn test_range_with_patch_protocol() {
    let raw = "patch:is-odd@npm%3A3.0.1#~/.yarn/patches/x.patch";
    let r = Range::from_raw(raw);
    assert_eq!(r.raw(), raw);
    assert_eq!(r.protocol_str(), Some("patch"));
    assert!(r.selector().starts_with("is-odd@npm%3A3.0.1#"));
    assert_eq!(r.protocol(), Protocol::Patch);
  }

  #[test]
  fn test_range_with_link_protocol() {
    let r = Range::from_raw("link:./packages/a");
    assert_eq!(r.protocol_str(), Some("link"));
    assert_eq!(r.selector(), "./packages/a");
    assert_eq!(r.protocol(), Protocol::Link);
  }

  #[test]
  fn test_range_with_git_protocol() {
    let r = Range::from_raw("git+ssh://host/repo.git#v1");
    assert_eq!(r.protocol_str(), Some("git+ssh"));
    assert_eq!(r.protocol(), Protocol::Git);
    let (url, frag) = r.as_git_url_and_fragment().unwrap();
    assert_eq!(url, "git+ssh://host/repo.git");
    assert_eq!(frag, Some("v1"));
  }

  #[test]
  fn test_range_protocol_specific_accessors() {
    let npm = Range::from_raw("npm:^1.2.3");
    assert_eq!(npm.as_npm_range(), Some("^1.2.3"));

    let ws = Range::from_raw("workspace:packages/a");
    assert_eq!(ws.as_workspace_path(), Some("packages/a"));

    let link = Range::from_raw("link:./local");
    assert_eq!(link.as_link_path(), Some("./local"));

    let file = Range::from_raw("file:../tarballs/pkg.tgz");
    assert_eq!(file.as_file_path(), Some("../tarballs/pkg.tgz"));

    let portal = Range::from_raw("portal:./inner");
    assert_eq!(portal.as_portal_path(), Some("./inner"));

    let exec = Range::from_raw("exec:node ./script.js");
    assert_eq!(exec.as_exec_command(), Some("node ./script.js"));

    let patch = Range::from_raw("patch:is-odd@npm%3A3.0.1#~/.yarn/patches/is-odd-npm-3.0.1.patch");
    let (inner, src) = patch.as_patch_inner_and_source().unwrap();
    assert!(inner.starts_with("is-odd@npm%3A3.0.1"));
    assert!(src.unwrap().starts_with("~/.yarn/patches/"));
  }
}
