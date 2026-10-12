//! Storage of the span tables: one buffer per row kind for a whole parse.
//!
//! Each kind has one [`Slab`] for all segments. Before the scan every
//! segment gets a window of each slab ([`Slabs::place`]); the workers then
//! write their windows in parallel through disjoint `&mut` slices. A parse
//! thus allocates seven buffers whatever the number of chunks or threads,
//! and none when the buffers are reused. A table that outgrows its window
//! moves to its own `Vec` (the spill).
//!
//! Slabs are never initialized as a whole, so unused window capacity is
//! never touched. Reading a window back is the one `unsafe` operation
//! ([`assume_init`]); it is sound by the invariant on [`Win`].

use crate::scan::{EntryRec, NameRange, PackedMeta, Span};
use std::mem::MaybeUninit;
use std::sync::atomic::{AtomicU64, Ordering};

/// # Safety
///
/// Every element of `s` must have been written.
#[inline]
unsafe fn assume_init<T>(s: &[MaybeUninit<T>]) -> &[T] {
  // SAFETY: `MaybeUninit<T>` has the layout of `T`, and the caller
  // guarantees every element is initialized.
  unsafe { &*(std::ptr::from_ref(s) as *const [T]) }
}

/// Source of slab epochs; 0 means "never placed".
static NEXT_EPOCH: AtomicU64 = AtomicU64::new(1);

/// One segment's part of one table: the window `[off, off + cap)` of its
/// slab, or `spill` once the window overflowed.
///
/// Invariant: if `spilled` is false and the owning [`SegTabs::epoch`] equals
/// the slab's epoch, the slab elements `[off, off + len)` were written during
/// that epoch. [`Slabs::place`] sets `off` and `cap` with `len = 0`, only a
/// [`Table`] over exactly this window raises `len`, and a slab is never
/// replaced without a new epoch.
#[derive(Debug)]
pub struct Win<T> {
  off: usize,
  cap: usize,
  len: usize,
  spilled: bool,
  spill: Vec<T>,
}

impl<T> Default for Win<T> {
  fn default() -> Self {
    Self {
      off: 0,
      cap: 0,
      len: 0,
      spilled: false,
      spill: Vec::new(),
    }
  }
}

impl<T> Win<T> {
  fn clear(&mut self) {
    self.len = 0;
    self.spilled = false;
    self.spill.clear();
  }

  fn spill_bytes(&self) -> usize {
    self.spill.capacity() * size_of::<T>()
  }
}

/// A table being written, starting empty. Its length is written back to its
/// [`Win`] on drop.
pub struct Table<'w, T: Copy> {
  /// `win[..len]` is initialized. Empty once spilled.
  win: &'w mut [MaybeUninit<T>],
  len: usize,
  state: &'w mut Win<T>,
}

impl<'w, T: Copy> Table<'w, T> {
  /// `window` is the slab range of `state`, or empty.
  fn new(state: &'w mut Win<T>, window: &'w mut [MaybeUninit<T>]) -> Self {
    state.clear();
    Self {
      win: window,
      len: 0,
      state,
    }
  }

  #[inline]
  pub const fn len(&self) -> usize {
    self.len
  }

  #[inline]
  pub fn push(&mut self, v: T) {
    if let Some(slot) = self.win.get_mut(self.len) {
      slot.write(v);
      self.len += 1;
    } else {
      self.push_spilled(v);
    }
  }

  #[cold]
  #[inline(never)]
  fn push_spilled(&mut self, v: T) {
    if !self.state.spilled {
      let win = std::mem::take(&mut self.win);
      // SAFETY: `push` wrote `win[..len]`.
      let written = unsafe { assume_init(&win[..self.len]) };
      self.state.spill.extend_from_slice(written);
      self.state.spilled = true;
    }
    self.state.spill.push(v);
    self.len += 1;
  }
}

impl<T: Copy> Drop for Table<'_, T> {
  fn drop(&mut self) {
    self.state.len = self.len;
  }
}

/// Defines the per-kind structs, so that the seven kinds are listed once.
macro_rules! tables {
  ($($name:ident: $t:ty),* $(,)?) => {
    /// The tables of one segment while it is scanned.
    pub struct Index<'w> {
      $(pub $name: Table<'w, $t>,)*
    }

    /// One segment's windows of every table.
    #[derive(Debug, Default)]
    pub struct SegTabs {
      /// Epoch of the [`Slabs`] the windows were placed in.
      epoch: u64,
      $($name: Win<$t>,)*
    }

    impl SegTabs {
      pub fn clear(&mut self) {
        $(self.$name.clear();)*
      }

      /// Number of tables that outgrew their window.
      pub fn spills(&self) -> usize {
        0 $(+ usize::from(self.$name.spilled))*
      }

      pub fn spill_bytes(&self) -> usize {
        0 $(+ self.$name.spill_bytes())*
      }

      /// Open the tables, empty. A window of `wins` that is not exactly
      /// this segment's is not used (the table then spills).
      pub fn open<'w>(&'w mut self, wins: Wins<'w>) -> Index<'w> {
        let ok = self.epoch != 0 && self.epoch == wins.epoch;
        Index {
          $($name: {
            let w = &mut self.$name;
            let window = if ok && wins.base.$name == w.off && wins.$name.len() >= w.cap {
              &mut wins.$name[..w.cap]
            } else {
              &mut []
            };
            Table::new(w, window)
          },)*
        }
      }
    }

    /// Mutable windows of every slab over consecutive segments.
    pub struct Wins<'w> {
      epoch: u64,
      /// Slab offset of the first element of each slice.
      base: Offsets,
      $($name: &'w mut [MaybeUninit<$t>],)*
    }

    #[derive(Clone, Copy, Default)]
    struct Offsets {
      $($name: usize,)*
    }

    impl<'w> Wins<'w> {
      pub fn none() -> Self {
        Self {
          epoch: 0,
          base: Offsets::default(),
          $($name: &mut [],)*
        }
      }

      /// Split before the windows of `at`.
      pub fn split_at(self, at: &SegTabs) -> (Self, Self) {
        let mut left = Self::none();
        let mut right = Self::none();
        left.epoch = self.epoch;
        right.epoch = self.epoch;
        left.base = self.base;
        $({
          let k = at.$name.off.saturating_sub(self.base.$name).min(self.$name.len());
          let (l, r) = self.$name.split_at_mut(k);
          left.$name = l;
          right.$name = r;
          right.base.$name = self.base.$name + k;
        })*
        (left, right)
      }
    }

    /// The table buffers of a parse.
    #[derive(Default)]
    pub struct Slabs {
      epoch: u64,
      /// Next free offset of each slab while windows are placed.
      cursor: Offsets,
      $($name: Slab<$t>,)*
    }

    impl Slabs {
      /// Make every slab hold at least `want` elements and start placing
      /// windows from offset 0. A failed allocation leaves the slab empty:
      /// its tables spill.
      pub fn begin(&mut self, want: [usize; 7]) {
        let [$($name),*] = want;
        if self.epoch == 0 || $(self.$name.0.len() < $name)||* {
          $(if self.$name.0.len() < $name {
            self.$name = Slab::with_len($name);
          })*
          self.epoch = NEXT_EPOCH.fetch_add(1, Ordering::Relaxed);
        }
        self.cursor = Offsets::default();
      }

      /// Give `tabs` the next windows of up to `want` elements per kind.
      pub fn place(&mut self, tabs: &mut SegTabs, want: [usize; 7]) {
        let [$($name),*] = want;
        tabs.epoch = self.epoch;
        $({
          let w = &mut tabs.$name;
          let off = self.cursor.$name;
          w.off = off;
          w.cap = $name.min(self.$name.0.len().saturating_sub(off));
          w.clear();
          self.cursor.$name = off + w.cap;
        })*
      }

      /// The whole of every slab, to split down to segments.
      pub fn wins(&mut self) -> Wins<'_> {
        Wins {
          epoch: self.epoch,
          base: Offsets::default(),
          $($name: &mut self.$name.0[..],)*
        }
      }

      /// Exactly the windows of `tabs`.
      pub fn wins_for(&mut self, tabs: &SegTabs) -> Wins<'_> {
        if tabs.epoch != self.epoch {
          return Wins::none();
        }
        let mut base = Offsets::default();
        $(base.$name = tabs.$name.off;)*
        Wins {
          epoch: self.epoch,
          base,
          $($name: {
            let w = &tabs.$name;
            self.$name.0.get_mut(w.off..w.off + w.cap).unwrap_or(&mut [])
          },)*
        }
      }

      pub fn capacity_bytes(&self) -> usize {
        0 $(+ self.$name.0.capacity() * size_of::<$t>())*
      }

      $(
        #[inline]
        pub fn $name<'a>(&'a self, tabs: &'a SegTabs) -> &'a [$t] {
          self.$name.get(self.epoch, tabs.epoch, &tabs.$name)
        }
      )*
    }
  };
}

tables! {
  entries: EntryRec,
  descs: NameRange,
  deps: NameRange,
  peers: NameRange,
  bins: NameRange,
  dmeta: (Span, PackedMeta),
  pmeta: (Span, PackedMeta),
}

/// One kind's buffer, never initialized as a whole (see [`Win`]).
pub struct Slab<T>(Vec<MaybeUninit<T>>);

impl<T> Default for Slab<T> {
  fn default() -> Self {
    Self(Vec::new())
  }
}

impl<T> Slab<T> {
  /// `n` elements of uninitialized storage, or none if the allocation fails.
  fn with_len(n: usize) -> Self {
    let mut v = Vec::new();
    if v.try_reserve_exact(n).is_ok() {
      v.resize_with(n, MaybeUninit::uninit);
    }
    Self(v)
  }

  #[inline]
  fn get<'a>(&'a self, epoch: u64, tabs_epoch: u64, w: &'a Win<T>) -> &'a [T] {
    if w.spilled {
      return &w.spill;
    }
    if epoch == 0 || tabs_epoch != epoch {
      return &[];
    }
    match self.0.get(w.off..w.off.saturating_add(w.len)) {
      // SAFETY: same epoch and not spilled: by the invariant on `Win`, these
      // elements were written.
      Some(s) => unsafe { assume_init(s) },
      None => &[],
    }
  }
}

/// Rows to reserve for a chunk of `bytes` bytes, in table order.
///
/// Large lockfiles average about 900 bytes per entry, but their densest
/// chunks have 260, and one dependency line per 41 to 70 bytes. Unused
/// capacity is never touched, so these bounds are generous.
pub const fn wanted(bytes: usize) -> [usize; 7] {
  let entries = bytes / 192 + 16;
  [
    entries,
    entries * 2 + bytes / 256 + 64,
    bytes / 32 + 16,
    bytes / 256 + 16,
    bytes / 256 + 16,
    bytes / 256 + 16,
    bytes / 256 + 16,
  ]
}

#[cfg(test)]
mod tests {
  use super::*;

  fn rec(i: u32) -> NameRange {
    NameRange {
      name: Span { start: i, len: 1 },
      range: Span { start: i, len: 2 },
    }
  }

  fn names(t: &[NameRange]) -> Vec<u32> {
    t.iter().map(|r| r.name.start).collect()
  }

  #[test]
  fn windows_fill_spill_and_read_back() {
    let mut slabs = Slabs::default();
    let mut segs: Vec<SegTabs> = (0..3).map(|_| SegTabs::default()).collect();
    slabs.begin([0, 0, 6, 0, 0, 0, 0]);
    for s in &mut segs {
      slabs.place(s, [0, 0, 2, 0, 0, 0, 0]);
    }
    {
      let (a, rest) = slabs.wins().split_at(&segs[1]);
      let (b, c) = rest.split_at(&segs[2]);
      let [s0, s1, s2] = &mut segs[..] else {
        unreachable!()
      };
      let (mut i0, mut i1, mut i2) = (s0.open(a), s1.open(b), s2.open(c));
      i0.deps.push(rec(1));
      for i in 10..14 {
        i1.deps.push(rec(i)); // overflows after two
      }
      i2.deps.push(rec(20));
      i2.deps.push(rec(21));
    }
    assert_eq!(names(slabs.deps(&segs[0])), [1]);
    assert_eq!(names(slabs.deps(&segs[1])), [10, 11, 12, 13]);
    assert_eq!(names(slabs.deps(&segs[2])), [20, 21]);
    assert_eq!(segs.iter().map(SegTabs::spills).sum::<usize>(), 1);
    // Reopening starts empty.
    {
      let wins = slabs.wins_for(&segs[1]);
      segs[1].open(wins).deps.push(rec(30));
    }
    assert_eq!(names(slabs.deps(&segs[1])), [30]);
    assert_eq!(segs[1].spills(), 0);
    // A new layout in a larger slab invalidates the old placement.
    slabs.begin([0, 0, 100, 0, 0, 0, 0]);
    let mut fresh = SegTabs::default();
    slabs.place(&mut fresh, [0, 0, 100, 0, 0, 0, 0]);
    assert!(slabs.deps(&segs[0]).is_empty());
    assert!(slabs.deps(&fresh).is_empty());
  }

  #[test]
  fn foreign_or_mismatched_windows_are_not_used() {
    let mut a = Slabs::default();
    let mut b = Slabs::default();
    a.begin([0, 0, 4, 0, 0, 0, 0]);
    b.begin([0, 0, 4, 0, 0, 0, 0]);
    let mut t = SegTabs::default();
    a.place(&mut t, [0, 0, 4, 0, 0, 0, 0]);
    t.open(b.wins()).deps.push(rec(7));
    assert_eq!(t.spills(), 1);
    assert_eq!(names(a.deps(&t)), [7]);
    a.place(&mut t, [0, 0, 4, 0, 0, 0, 0]);
    assert_eq!(t.spills(), 0);
    assert!(b.deps(&t).is_empty());
  }

  #[test]
  fn exhausted_or_failed_slabs_give_empty_windows() {
    let mut slabs = Slabs::default();
    slabs.begin([0, 0, 3, 0, 0, 0, 0]);
    let (mut first, mut second) = (SegTabs::default(), SegTabs::default());
    slabs.place(&mut first, [0, 0, 3, 0, 0, 0, 0]);
    slabs.place(&mut second, [0, 0, 3, 0, 0, 0, 0]);
    {
      let (w1, w2) = slabs.wins().split_at(&second);
      let (mut i1, mut i2) = (first.open(w1), second.open(w2));
      i1.deps.push(rec(1));
      i2.deps.push(rec(2));
    }
    assert_eq!((first.spills(), second.spills()), (0, 1));
    assert_eq!(names(slabs.deps(&second)), [2]);
    let mut big = Slabs::default();
    big.begin([0, 0, usize::MAX / 2, 0, 0, 0, 0]);
    let mut t = SegTabs::default();
    big.place(&mut t, [0, 0, 10, 0, 0, 0, 0]);
    {
      let w = big.wins_for(&t);
      t.open(w).deps.push(rec(3));
    }
    assert_eq!(names(big.deps(&t)), [3]);
  }
}
