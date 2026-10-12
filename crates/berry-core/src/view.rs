//! Borrowed views of one lockfile entry and its lists.
//!
//! Every view is `Copy` and borrows the [`Lockfile`](crate::Lockfile): it holds
//! the lockfile text and a reference to the parsed span tables, and slices the
//! text on access. Nothing is allocated, except by
//! [`Dependencies::lookup`] (one sorted `Vec`, for many lookups) and
//! [`Dependencies::last_wins`] on unsorted lists.
//!
//! Lists are in source order and keep duplicate keys as written. berry 0.6
//! stored them in `HashMap`s, where the last occurrence of a key won; `get`
//! (and [`Dependencies::last_wins`]) give that same last-wins result.

use crate::ident::{Descriptor, Ident, Locator};
use crate::scan::{
  EntryRec, META_BUILT, META_OPTIONAL, META_UNPLUGGED, NameRange, PackedMeta, Range32, Span,
  unpack_meta,
};
use crate::table::{SegTabs, Slabs};
use std::fmt;
use std::iter::FusedIterator;

/// The type of link to use for a package.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LinkType {
  /// The package manager owns the location (typically things within the cache)
  /// e.g. `PnP` linker may unplug packages
  Hard,

  /// The package manager doesn't own the location (symlinks, workspaces, etc),
  /// so the linkers aren't allowed to do anything with them except use them as
  /// they are.
  Soft,
}

impl TryFrom<&str> for LinkType {
  type Error = ();

  fn try_from(s: &str) -> Result<Self, Self::Error> {
    match s {
      "hard" => Ok(Self::Hard),
      "soft" => Ok(Self::Soft),
      _ => Err(()),
    }
  }
}

/// One `dependenciesMeta` value
/// ([Manifest.ts](https://github.com/yarnpkg/berry/blob/master/packages/yarnpkg-core/sources/Manifest.ts#L19)).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DependencyMeta {
  pub built: Option<bool>,
  pub optional: Option<bool>,
  pub unplugged: Option<bool>,
}

/// One `peerDependenciesMeta` value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PeerDependencyMeta {
  pub optional: bool,
}

#[inline]
fn table<T>(v: &[T], r: Range32) -> &[T] {
  v.get(r.0 as usize..r.1 as usize).unwrap_or(&[])
}

#[inline]
fn descriptor<'l>(src: &'l str, d: &NameRange) -> Descriptor<'l> {
  let s = d.name.start as usize;
  let e = (d.range.start + d.range.len) as usize;
  Descriptor::from_text(
    src.get(s..e).unwrap_or(""),
    d.name.len,
    d.range.start.saturating_sub(d.name.start),
  )
}

#[inline]
fn dependency_meta(m: PackedMeta) -> DependencyMeta {
  DependencyMeta {
    built: unpack_meta(m, META_BUILT),
    optional: unpack_meta(m, META_OPTIONAL),
    unplugged: unpack_meta(m, META_UNPLUGGED),
  }
}

#[inline]
fn peer_dependency_meta(m: PackedMeta) -> PeerDependencyMeta {
  PeerDependencyMeta {
    optional: unpack_meta(m, META_OPTIONAL) == Some(true),
  }
}

/// One entry of a [`Lockfile`](crate::Lockfile): its descriptors (the key
/// line) and its package fields.
#[derive(Clone, Copy)]
pub struct EntryView<'l> {
  src: &'l str,
  slabs: &'l Slabs,
  tabs: &'l SegTabs,
  rec: &'l EntryRec,
}

impl<'l> EntryView<'l> {
  #[inline]
  pub(crate) const fn new(
    src: &'l str,
    slabs: &'l Slabs,
    tabs: &'l SegTabs,
    rec: &'l EntryRec,
  ) -> Self {
    Self {
      src,
      slabs,
      tabs,
      rec,
    }
  }

  /// The entry's descriptors (the comma-separated key line), in order.
  #[inline]
  #[must_use]
  pub fn descriptors(&self) -> Descriptors<'l> {
    Descriptors {
      src: self.src,
      items: table(self.slabs.descs(self.tabs), self.rec.descs),
    }
  }

  /// `version`, quotes stripped.
  #[inline]
  #[must_use]
  pub fn version(&self) -> Option<&'l str> {
    self.rec.version.opt(self.src)
  }

  /// `resolution`, quotes stripped.
  #[inline]
  #[must_use]
  pub fn resolution(&self) -> Option<&'l str> {
    self.rec.resolution.opt(self.src)
  }

  /// `resolution` split into ident and reference
  /// ([`Locator::from_resolution`]); `None` without a resolution or without
  /// an `@` to split at.
  #[inline]
  #[must_use]
  pub fn resolution_locator(&self) -> Option<Locator<'l>> {
    self.resolution().and_then(Locator::from_resolution)
  }

  /// `languageName` (raw value), `"unknown"` if absent.
  #[inline]
  #[must_use]
  pub fn language_name(&self) -> &'l str {
    self.rec.language.opt(self.src).unwrap_or("unknown")
  }

  /// `linkType` (`Hard` if absent).
  #[inline]
  #[must_use]
  pub const fn link_type(&self) -> LinkType {
    if self.rec.link_soft {
      LinkType::Soft
    } else {
      LinkType::Hard
    }
  }

  /// `checksum` (raw value).
  #[inline]
  #[must_use]
  pub fn checksum(&self) -> Option<&'l str> {
    self.rec.checksum.opt(self.src)
  }

  /// `conditions` (raw value).
  #[inline]
  #[must_use]
  pub fn conditions(&self) -> Option<&'l str> {
    self.rec.conditions.opt(self.src)
  }

  /// `dependencies` lines, in source order.
  #[inline]
  #[must_use]
  pub fn dependencies(&self) -> Dependencies<'l> {
    Dependencies {
      src: self.src,
      items: table(self.slabs.deps(self.tabs), self.rec.deps),
    }
  }

  /// `peerDependencies` lines, in source order.
  #[inline]
  #[must_use]
  pub fn peer_dependencies(&self) -> Dependencies<'l> {
    Dependencies {
      src: self.src,
      items: table(self.slabs.peers(self.tabs), self.rec.peers),
    }
  }

  /// `bin` lines `(name, path)`, in source order.
  #[inline]
  #[must_use]
  pub fn bin(&self) -> Bins<'l> {
    Bins {
      src: self.src,
      items: table(self.slabs.bins(self.tabs), self.rec.bins),
    }
  }

  /// `dependenciesMeta` items, in source order.
  #[inline]
  #[must_use]
  pub fn dependencies_meta(&self) -> DependenciesMeta<'l> {
    DependenciesMeta {
      src: self.src,
      items: table(self.slabs.dmeta(self.tabs), self.rec.dmeta),
    }
  }

  /// `peerDependenciesMeta` items, in source order.
  #[inline]
  #[must_use]
  pub fn peer_dependencies_meta(&self) -> PeerDependenciesMeta<'l> {
    PeerDependenciesMeta {
      src: self.src,
      items: table(self.slabs.pmeta(self.tabs), self.rec.pmeta),
    }
  }
}

impl fmt::Debug for EntryView<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Entry")
      .field("descriptors", &self.descriptors())
      .field("version", &self.version())
      .field("resolution", &self.resolution())
      .field("language_name", &self.language_name())
      .field("link_type", &self.link_type())
      .field("checksum", &self.checksum())
      .field("conditions", &self.conditions())
      .field("dependencies", &self.dependencies())
      .field("peer_dependencies", &self.peer_dependencies())
      .field("bin", &self.bin())
      .field("dependencies_meta", &self.dependencies_meta())
      .field("peer_dependencies_meta", &self.peer_dependencies_meta())
      .finish()
  }
}

/// Defines a `Copy` list view over a span table plus its iterator.
macro_rules! list_view {
  (
    $(#[$doc:meta])* $name:ident, $iter:ident, $rec:ty, $item:ty,
    |$src:ident, $r:ident| $make:expr
  ) => {
    $(#[$doc])*
    #[derive(Clone, Copy)]
    pub struct $name<'l> {
      src: &'l str,
      items: &'l [$rec],
    }

    impl<'l> $name<'l> {
      /// Number of items (duplicates included).
      #[inline]
      #[must_use]
      pub const fn len(&self) -> usize {
        self.items.len()
      }

      /// `true` if there are no items.
      #[inline]
      #[must_use]
      pub const fn is_empty(&self) -> bool {
        self.items.is_empty()
      }

      /// The items in source order.
      #[inline]
      #[must_use]
      pub fn iter(&self) -> $iter<'l> {
        $iter {
          src: self.src,
          inner: self.items.iter(),
        }
      }

      #[inline]
      #[allow(clippy::redundant_closure_call)]
      fn make($src: &'l str, $r: &'l $rec) -> $item {
        $make
      }
    }

    impl<'l> IntoIterator for $name<'l> {
      type Item = $item;
      type IntoIter = $iter<'l>;
      #[inline]
      fn into_iter(self) -> $iter<'l> {
        self.iter()
      }
    }

    impl<'l> IntoIterator for &$name<'l> {
      type Item = $item;
      type IntoIter = $iter<'l>;
      #[inline]
      fn into_iter(self) -> $iter<'l> {
        self.iter()
      }
    }

    impl fmt::Debug for $name<'_> {
      fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
      }
    }

    #[doc = concat!("Iterator over a [`", stringify!($name), "`].")]
    #[derive(Clone)]
    pub struct $iter<'l> {
      src: &'l str,
      inner: std::slice::Iter<'l, $rec>,
    }

    impl<'l> Iterator for $iter<'l> {
      type Item = $item;
      #[inline]
      fn next(&mut self) -> Option<$item> {
        let src = self.src;
        self.inner.next().map(|r| $name::make(src, r))
      }
      #[inline]
      fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
      }
      #[inline]
      fn nth(&mut self, n: usize) -> Option<$item> {
        let src = self.src;
        self.inner.nth(n).map(|r| $name::make(src, r))
      }
    }

    impl DoubleEndedIterator for $iter<'_> {
      #[inline]
      fn next_back(&mut self) -> Option<Self::Item> {
        let src = self.src;
        self.inner.next_back().map(|r| $name::make(src, r))
      }
    }

    impl ExactSizeIterator for $iter<'_> {}
    impl FusedIterator for $iter<'_> {}

    impl fmt::Debug for $iter<'_> {
      fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(stringify!($iter))
          .field("remaining", &self.inner.len())
          .finish()
      }
    }
  };
}

list_view!(
  /// An entry's descriptors ([`EntryView::descriptors`]).
  Descriptors, DescriptorIter, NameRange, Descriptor<'l>,
  |src, d| descriptor(src, d)
);

list_view!(
  /// `dependencies` or `peerDependencies` lines ([`EntryView::dependencies`]),
  /// in source order, each as a [`Descriptor`] (`ident()` is the key,
  /// `range()` the value).
  Dependencies, DependencyIter, NameRange, Descriptor<'l>,
  |src, d| descriptor(src, d)
);

list_view!(
  /// `bin` lines `(name, path)` ([`EntryView::bin`]), in source order.
  Bins, BinIter, NameRange, (&'l str, &'l str),
  |src, b| (b.name.get(src), b.range.get(src))
);

list_view!(
  /// `dependenciesMeta` items ([`EntryView::dependencies_meta`]), in source
  /// order.
  DependenciesMeta, DependencyMetaIter, (Span, PackedMeta), (Ident<'l>, DependencyMeta),
  |src, m| (Ident::new(m.0.get(src)), dependency_meta(m.1))
);

list_view!(
  /// `peerDependenciesMeta` items ([`EntryView::peer_dependencies_meta`]),
  /// in source order.
  PeerDependenciesMeta, PeerDependencyMetaIter, (Span, PackedMeta), (Ident<'l>, PeerDependencyMeta),
  |src, m| (Ident::new(m.0.get(src)), peer_dependency_meta(m.1))
);

impl<'l> Descriptors<'l> {
  /// Descriptor `i`.
  #[inline]
  #[must_use]
  pub fn get(&self, i: usize) -> Option<Descriptor<'l>> {
    self.items.get(i).map(|d| descriptor(self.src, d))
  }

  /// The first descriptor (every parsed entry has at least one).
  #[inline]
  #[must_use]
  pub fn first(&self) -> Option<Descriptor<'l>> {
    self.get(0)
  }
}

impl<'l> Dependencies<'l> {
  #[inline]
  fn name(&self, d: &NameRange) -> &'l str {
    d.name.get(self.src)
  }

  /// The dependency named `name` (`"pkg"` or `"@scope/pkg"`); the last one if
  /// the name is listed more than once, as in berry 0.6's map.
  ///
  /// A reverse linear scan without allocation (entries have ~12 dependencies
  /// on average). For many lookups on one long list use
  /// [`lookup`](Self::lookup).
  #[inline]
  #[must_use]
  pub fn get(&self, name: &str) -> Option<Descriptor<'l>> {
    self
      .items
      .iter()
      .rev()
      .find(|d| self.name(d) == name)
      .map(|d| descriptor(self.src, d))
  }

  /// Each name once, with its last value (exactly the items berry 0.6 kept in
  /// its `HashMap`), in source order of those last occurrences.
  ///
  /// Allocation-free when the names are strictly sorted, as Yarn writes
  /// them; an unsorted list costs one allocation and O(n log n).
  #[must_use]
  pub fn last_wins(&self) -> LastWins<'l> {
    LastWins::new(*self)
  }

  /// A by-name lookup table for many [`get`](DependencyLookup::get)s: built
  /// in O(n log n) with one allocation, then O(log n) per lookup, with the
  /// same results as [`get`](Self::get).
  #[must_use]
  pub fn lookup(&self) -> DependencyLookup<'l> {
    DependencyLookup::new(*self)
  }
}

impl<'l> Bins<'l> {
  /// The path of the binary `name` (last occurrence).
  #[inline]
  #[must_use]
  pub fn get(&self, name: &str) -> Option<&'l str> {
    self
      .items
      .iter()
      .rev()
      .find(|b| b.name.get(self.src) == name)
      .map(|b| b.range.get(self.src))
  }
}

impl DependenciesMeta<'_> {
  /// The metadata of the dependency `name` (last occurrence).
  #[inline]
  #[must_use]
  pub fn get(&self, name: &str) -> Option<DependencyMeta> {
    self
      .items
      .iter()
      .rev()
      .find(|m| m.0.get(self.src) == name)
      .map(|m| dependency_meta(m.1))
  }
}

impl PeerDependenciesMeta<'_> {
  /// The metadata of the peer dependency `name` (last occurrence).
  #[inline]
  #[must_use]
  pub fn get(&self, name: &str) -> Option<PeerDependencyMeta> {
    self
      .items
      .iter()
      .rev()
      .find(|m| m.0.get(self.src) == name)
      .map(|m| peer_dependency_meta(m.1))
  }
}

/// Iterator returned by [`Dependencies::last_wins`].
pub struct LastWins<'l> {
  deps: Dependencies<'l>,
  pos: usize,
  /// `None`: strictly sorted, so every item is kept.
  keep: Option<Vec<bool>>,
}

impl<'l> LastWins<'l> {
  fn new(deps: Dependencies<'l>) -> Self {
    let items = deps.items;
    let sorted = items
      .windows(2)
      .all(|w| deps.name(&w[0]) < deps.name(&w[1]));
    let keep = (!sorted).then(|| {
      let mut order: Vec<usize> = (0..items.len()).collect();
      // Same name: later items first, so the first of each run is the last
      // occurrence.
      order.sort_unstable_by(|&a, &b| {
        deps
          .name(&items[a])
          .cmp(deps.name(&items[b]))
          .then(b.cmp(&a))
      });
      let mut flags = vec![false; items.len()];
      let mut prev: Option<&str> = None;
      for i in order {
        let name = deps.name(&items[i]);
        if prev != Some(name) {
          flags[i] = true;
          prev = Some(name);
        }
      }
      flags
    });
    Self { deps, pos: 0, keep }
  }
}

impl<'l> Iterator for LastWins<'l> {
  type Item = Descriptor<'l>;
  fn next(&mut self) -> Option<Descriptor<'l>> {
    while self.pos < self.deps.items.len() {
      let i = self.pos;
      self.pos += 1;
      if self.keep.as_ref().is_none_or(|k| k[i]) {
        return Some(descriptor(self.deps.src, &self.deps.items[i]));
      }
    }
    None
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    let rest = self.deps.items.len() - self.pos;
    let min = if self.keep.is_none() {
      rest
    } else {
      usize::from(rest > 0)
    };
    (min, Some(rest))
  }
}

impl FusedIterator for LastWins<'_> {}

impl fmt::Debug for LastWins<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("LastWins")
      .field("remaining", &(self.deps.items.len() - self.pos))
      .finish_non_exhaustive()
  }
}

/// Name-sorted lookup table over one list of dependency lines
/// ([`Dependencies::lookup`]). Holds the last occurrence of each name.
#[derive(Clone)]
pub struct DependencyLookup<'l> {
  src: &'l str,
  /// Sorted by name, one line per name (its last occurrence).
  lines: Vec<&'l NameRange>,
}

impl<'l> DependencyLookup<'l> {
  fn new(deps: Dependencies<'l>) -> Self {
    let src = deps.src;
    let mut lines: Vec<&'l NameRange> = deps.items.iter().collect();
    // Same name: later lines (larger start offset) first, so `dedup_by`, which
    // keeps the first of each run, keeps the last occurrence.
    lines.sort_unstable_by(|a, b| {
      a.name
        .get(src)
        .cmp(b.name.get(src))
        .then(b.name.start.cmp(&a.name.start))
    });
    lines.dedup_by(|later, kept| later.name.get(src) == kept.name.get(src));
    Self { src, lines }
  }

  /// The dependency named `name` (last occurrence), in O(log n).
  #[must_use]
  pub fn get(&self, name: &str) -> Option<Descriptor<'l>> {
    self
      .lines
      .binary_search_by(|d| d.name.get(self.src).cmp(name))
      .ok()
      .map(|i| descriptor(self.src, self.lines[i]))
  }

  /// Number of distinct names.
  #[must_use]
  pub fn len(&self) -> usize {
    self.lines.len()
  }

  /// `true` if there are no dependencies.
  #[must_use]
  pub fn is_empty(&self) -> bool {
    self.lines.is_empty()
  }

  /// The distinct dependencies, sorted by name (last occurrence of each).
  pub fn iter(&self) -> impl ExactSizeIterator<Item = Descriptor<'l>> + '_ {
    self.lines.iter().map(|d| descriptor(self.src, d))
  }
}

impl fmt::Debug for DependencyLookup<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list().entries(self.iter()).finish()
  }
}
