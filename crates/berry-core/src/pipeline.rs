//! The parse driver: the preamble, then the body in chunks scanned in
//! parallel, each into its own segment of the span tables.
//!
//! Chunks start after a blank line, where Yarn starts every entry. The
//! stitch then walks the chunks in order with the position the sequential
//! loop would be at (the cursor): a chunk starting at the cursor is used as
//! scanned, a chunk behind it (an entry ran across it) is cleared, and a
//! chunk the cursor is inside is scanned again from the cursor. The result
//! is the sequential one for any split.

use crate::error::{Error, ErrorKind};
use crate::scan::{self, MAX_INPUT_LEN, Outcome, Preamble, len32, scan_range};
use crate::table::{self, SegTabs, Slabs, Wins};

/// One chunk of the body and its tables (with segment-local row indices).
#[derive(Debug)]
pub struct Segment {
  pub tabs: SegTabs,
  /// Index of the segment's first entry in the lockfile.
  pub base: u32,
  /// The entries starting in `[start, stop)` belong here.
  pub start: u32,
  pub stop: u32,
  pub outcome: Outcome,
}

impl Segment {
  const fn len(&self) -> usize {
    (self.stop - self.start) as usize
  }
}

impl Default for Segment {
  fn default() -> Self {
    Self {
      tabs: SegTabs::default(),
      base: 0,
      start: 0,
      stop: 0,
      outcome: Outcome::Reached(0),
    }
  }
}

/// How to cut the body into chunks.
#[derive(Debug, Clone, Copy)]
pub enum Plan<'p> {
  /// [`auto_chunks`] chunks.
  Auto,
  /// Chunks of about this many bytes, snapped forward to entry boundaries.
  Target(usize),
  /// Chunks starting at these offsets, moved forward to a char boundary;
  /// offsets outside the body or out of order are ignored.
  Starts(&'p [usize]),
}

pub struct Run {
  pub pre: Preamble,
  /// `store.segs[..used]` hold the entries.
  pub used: usize,
  pub len: usize,
  pub entries_end: u32,
  pub rest_offset: u32,
}

/// Parse `src` into `slabs` and `segs`, reusing their storage.
pub fn run(
  src: &str,
  slabs: &mut Slabs,
  segs: &mut Vec<Segment>,
  plan: Plan<'_>,
) -> Result<Run, Error> {
  if src.len() > MAX_INPUT_LEN {
    return Err(Error::new(ErrorKind::InputTooLarge, 0));
  }
  let pre = scan::preamble(src)?;
  let n = plan_chunks(src, pre.body, plan, segs);
  let segs = &mut segs[..n];
  place_tables(slabs, segs);
  scan(src, segs, slabs.wins());
  let (used, len, entries_end) = stitch(src, slabs, segs)?;
  let tail = src
    .get(entries_end as usize..)
    .unwrap_or("")
    .trim_ascii_start();
  Ok(Run {
    pre,
    used,
    len,
    entries_end,
    rest_offset: len32(src.len() - tail.len()),
  })
}

/// Bodies smaller than this are parsed on the calling thread. Starting the
/// rayon pool costs about 1.5 ms, as much as parsing this much text alone.
pub const PARALLEL_MIN: usize = 2_500_000;

/// One chunk below [`PARALLEL_MIN`] or on a one-thread pool, otherwise two
/// per thread. The size is checked first: `rayon::current_num_threads`
/// starts the global pool.
pub fn auto_chunks(body: usize) -> usize {
  if body < PARALLEL_MIN {
    return 1;
  }
  match rayon::current_num_threads() {
    1 => 1,
    threads => 2 * threads,
  }
}

/// Write the chunks of `src[body..]` into `segs[..n]` and return `n >= 1`.
/// Each stop is the next start; the last is `src.len()`.
fn plan_chunks(src: &str, body: usize, plan: Plan<'_>, segs: &mut Vec<Segment>) -> usize {
  let b = src.as_bytes();
  let target = match plan {
    Plan::Auto => {
      let chunks = auto_chunks(src.len() - body);
      segs.reserve(chunks.saturating_sub(segs.len()));
      (src.len() - body).div_ceil(chunks)
    }
    Plan::Target(target) => target.max(1),
    Plan::Starts(_) => 0,
  };
  let mut n = 0;
  let mut put = |segs: &mut Vec<Segment>, start: usize| {
    if segs.len() == n {
      segs.push(Segment::default());
    }
    segs[n].start = len32(start);
    n += 1;
  };
  put(segs, body);
  match plan {
    Plan::Auto | Plan::Target(_) => {
      let mut start = body;
      while let Some(next) = next_boundary(b, start.saturating_add(target)) {
        put(segs, next);
        start = next;
      }
    }
    Plan::Starts(starts) => {
      let mut last = body;
      for &s in starts {
        let mut s = s;
        while s < src.len() && !src.is_char_boundary(s) {
          s += 1;
        }
        if s > last && s < src.len() {
          put(segs, s);
          last = s;
        }
      }
    }
  }
  for i in 0..n {
    segs[i].stop = if i + 1 < n {
      segs[i + 1].start
    } else {
      len32(src.len())
    };
  }
  n
}

/// The first position at or after `from` that follows a blank line and is
/// not whitespace.
fn next_boundary(b: &[u8], from: usize) -> Option<usize> {
  // A boundary at `p >= from` has its "\n\n" at `p - 2 >= from - 2`.
  let mut at = from.saturating_sub(2);
  loop {
    let i = at + memchr::memmem::find(b.get(at..)?, b"\n\n")?;
    let mut p = i + 2;
    while b.get(p) == Some(&b'\n') {
      p += 1;
    }
    match b.get(p)? {
      b' ' | b'\t' | b'\r' => at = p,
      _ if p >= from => return Some(p),
      _ => at = p - 1,
    }
  }
}

/// Size the slabs and give every chunk its windows.
fn place_tables(slabs: &mut Slabs, segs: &mut [Segment]) {
  let mut total = [0; 7];
  for seg in segs.iter() {
    for (t, w) in total.iter_mut().zip(table::wanted(seg.len())) {
      *t += w;
    }
  }
  slabs.begin(total);
  for seg in segs.iter_mut() {
    let want = table::wanted(seg.len());
    slabs.place(&mut seg.tabs, want);
  }
}

/// Scan each chunk into its own windows, in parallel if there are several.
fn scan(src: &str, segs: &mut [Segment], wins: Wins<'_>) {
  match segs {
    [] => {}
    [seg] => scan_chunk(src, seg, wins),
    _ => {
      let (a, b) = segs.split_at_mut(segs.len() / 2);
      let (wa, wb) = wins.split_at(&b[0].tabs);
      rayon::join(|| scan(src, a, wa), || scan(src, b, wb));
    }
  }
}

fn scan_chunk(src: &str, seg: &mut Segment, wins: Wins<'_>) {
  let mut ix = seg.tabs.open(wins);
  seg.outcome = scan_range(src, seg.start as usize, seg.stop as usize, &mut ix);
}

/// Walk the chunks with the sequential cursor (see the module docs).
/// Returns `(used segments, entries, entries_end)`.
fn stitch(
  src: &str,
  slabs: &mut Slabs,
  segs: &mut [Segment],
) -> Result<(usize, usize, u32), Error> {
  let mut cursor = segs.first().map_or(0, |s| s.start);
  let mut len = 0usize;
  for (i, seg) in segs.iter_mut().enumerate() {
    seg.base = len32(len);
    if seg.start != cursor {
      if seg.stop <= cursor {
        seg.tabs.clear();
        continue;
      }
      repair(src, slabs, seg, cursor);
    }
    len += slabs.entries(&seg.tabs).len();
    match seg.outcome {
      Outcome::Reached(end) => cursor = end,
      Outcome::Stopped(at) => return Ok((i + 1, len, at)),
      Outcome::Failed(e) => return Err(e),
    }
  }
  Ok((segs.len(), len, cursor))
}

/// Scan the chunk again from `cursor`, which is inside it.
#[cold]
#[inline(never)]
fn repair(src: &str, slabs: &mut Slabs, seg: &mut Segment, cursor: u32) {
  seg.start = cursor;
  let wins = slabs.wins_for(&seg.tabs);
  scan_chunk(src, seg, wins);
}

#[cfg(test)]
mod tests {
  use super::*;

  fn boundaries(b: &[u8]) -> Vec<usize> {
    (2..=b.len())
      .filter(|&p| &b[p - 2..p] == b"\n\n" && b.get(p).is_some_and(|c| !b" \t\r\n".contains(c)))
      .collect()
  }

  #[test]
  fn next_boundary_is_the_first_boundary_at_or_after() {
    let inputs: [&[u8]; 8] = [
      b"a:\n  x: 1\n\nb:\n  x: 1\n\n\nc:\n\n \nd\n\n\te\n\n",
      b"\n\n\n\nx",
      b"\n\nx\n\ny",
      b"xx\n\n",
      b"",
      b"\n",
      b"\n\n \n\n\r\n\nz",
      "\u{e9}\n\n\u{e9}\n\n\n\u{3000}".as_bytes(),
    ];
    for b in inputs {
      let all = boundaries(b);
      for from in 0..=b.len() + 2 {
        let want = all.iter().copied().find(|&p| p >= from);
        assert_eq!(next_boundary(b, from), want, "{b:?} from {from}");
      }
    }
  }

  fn plan(src: &str, body: usize, p: Plan<'_>) -> Vec<(u32, u32)> {
    let mut segs = Vec::new();
    let n = plan_chunks(src, body, p, &mut segs);
    segs[..n].iter().map(|s| (s.start, s.stop)).collect()
  }

  #[test]
  fn chunk_plans_cover_the_body_in_order() {
    let src = "head\n\na:\n  x: 1\n\nb:\n  x: 1\n\n\u{e9}c:\n  x: 1\n";
    let body = 6;
    assert_eq!(plan(src, body, Plan::Auto), [(6, len32(src.len()))]);
    for target in [0, 1, 2, 7, 64, 1 << 20, usize::MAX] {
      let p = plan(src, body, Plan::Target(target));
      assert_eq!(p[0].0, 6);
      assert_eq!(p.last().unwrap().1 as usize, src.len());
      for w in p.windows(2) {
        assert!(w[0].0 < w[1].0 && w[0].1 == w[1].0, "{target}: {p:?}");
      }
    }
    assert_eq!(plan(src, body, Plan::Target(1)).len(), 3);
    for at in 0..src.len() + 2 {
      let p = plan(src, body, Plan::Starts(&[at, at, 3]));
      assert!(p.iter().all(|&(s, _)| src.is_char_boundary(s as usize)));
      assert_eq!(
        p.len(),
        1 + usize::from(at > body && at < src.len()),
        "{at}"
      );
    }
    // An empty body is one empty chunk.
    assert_eq!(plan("x", 1, Plan::Target(1)), [(1, 1)]);
  }

  const HEAD: &str = "# This file is generated by running \"yarn install\" inside your project.\n# Manual changes might be lost - proceed with caution!\n\n__metadata:\n  version: 8\n  cacheKey: 10\n\n";

  struct Store {
    slabs: Slabs,
    segs: Vec<Segment>,
  }

  fn run_split(src: &str, starts: &[usize]) -> (Store, Result<Run, Error>) {
    let (mut slabs, mut segs) = (Slabs::default(), Vec::new());
    let r = run(src, &mut slabs, &mut segs, Plan::Starts(starts));
    (Store { slabs, segs }, r)
  }

  #[test]
  fn stitch_uses_skips_and_repairs_chunks() {
    let src = format!("{HEAD}a@1:\n  version: 1\n\nb@1:\n  version: 2\n\nc@1:\n  version: 3\n");
    let b = src.find("b@1").unwrap();
    let c = src.find("c@1").unwrap();
    // Chunk starts on entry starts: all used as scanned.
    let (store, r) = run_split(&src, &[b, c]);
    let r = r.unwrap();
    assert_eq!((r.used, r.len), (3, 3));
    let starts: Vec<_> = store
      .segs
      .iter()
      .map(|s| (s.start as usize, s.base))
      .collect();
    assert_eq!(starts, [(HEAD.len(), 0), (b, 1), (c, 2)]);
    // A start inside entry `a` is repaired from the end of `a` (= b).
    let (store, r) = run_split(&src, &[HEAD.len() + 3, c]);
    assert_eq!(r.unwrap().len, 3);
    assert_eq!(store.segs[1].start as usize, b);
    assert_eq!(store.slabs.entries(&store.segs[1].tabs).len(), 1);
    // A chunk inside entry `b` that ends before the cursor is skipped.
    let (store, r) = run_split(&src, &[b, b + 2, b + 4, c]);
    assert_eq!(r.unwrap().len, 3);
    assert_eq!(store.slabs.entries(&store.segs[2].tabs).len(), 0);
    assert_eq!(store.slabs.entries(&store.segs[3].tabs).len(), 0);
    assert_eq!((store.segs[4].start as usize, store.segs[4].base), (c, 2));
  }

  #[test]
  fn first_stop_or_error_in_stitch_order_wins() {
    let src = format!("{HEAD}a@1:\n  version: 1\n\njunk\n\nb@1:\n  bogus: 2\n");
    let junk = src.find("junk").unwrap();
    let b = src.find("b@1").unwrap();
    let (store, r) = run_split(&src, &[junk, b]);
    let r = r.unwrap();
    assert_eq!((r.used, r.len, r.entries_end as usize), (2, 1, junk));
    assert!(matches!(store.segs[2].outcome, Outcome::Failed(_)));
    let src = format!("{HEAD}a@1:\n  bogus: 1\n\njunk\n");
    let junk = src.find("junk").unwrap();
    let (_, r) = run_split(&src, &[junk]);
    assert_eq!(r.err().map(|e| e.kind()), Some(ErrorKind::UnknownProperty));
  }

  #[test]
  fn chunk_counts_follow_the_size_rules() {
    let pool = |n| {
      rayon::ThreadPoolBuilder::new()
        .num_threads(n)
        .build()
        .unwrap()
    };
    pool(8).install(|| {
      assert_eq!(auto_chunks(PARALLEL_MIN - 1), 1);
      assert_eq!(auto_chunks(PARALLEL_MIN), 16);
    });
    pool(32).install(|| assert_eq!(auto_chunks(24_000_000), 64));
    pool(1).install(|| assert_eq!(auto_chunks(24_000_000), 1));
  }

  #[test]
  fn plans_reuse_segments() {
    let src = "a:\n  x: 1\n\nb:\n  x: 1\n\nc:\n  x: 1\n";
    let mut segs = Vec::new();
    assert_eq!(plan_chunks(src, 0, Plan::Target(1), &mut segs), 3);
    assert_eq!(plan_chunks(src, 0, Plan::Auto, &mut segs), 1);
    assert_eq!(segs.len(), 3);
    assert_eq!((segs[0].start, segs[0].stop), (0, len32(src.len())));
  }
}
