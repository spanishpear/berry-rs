//! Deterministic synthetic yarn berry (lockfile v8) generator.
//!
//! The generator exists so that parser benchmarks have a realistic, large
//! (~24 MB) input that does not need to be checked in and can be reproduced
//! byte-for-byte anywhere: `berry-gen` with the same seed and size always
//! emits the same file.
//!
//! The statistical shape follows a "large-monorepo profile": roughly half of
//! the entries are workspace packages with long dependency lists, the other
//! half are registry packages, plus a handful of patch / portal entries and a
//! few degenerate stub entries. Every number in [`Profile`] is a rounded
//! parameter. All package names are pseudo-words produced from a syllable
//! table; no real-world names are embedded.
//!
//! The output only uses constructs that the current `berry` parser accepts, so
//! it can be used to compare parser implementations against each other.

use std::collections::HashSet;
use std::fmt::Write as _;

// ---------------------------------------------------------------------------
// Deterministic RNG
// ---------------------------------------------------------------------------

/// `SplitMix64`: tiny, fast, and identical on every platform.
#[derive(Clone, Debug)]
pub struct Rng(u64);

impl Rng {
  #[must_use]
  pub const fn new(seed: u64) -> Self {
    Self(seed)
  }

  pub fn next_u64(&mut self) -> u64 {
    self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
    let mut z = self.0;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
  }

  /// Uniform in `[0, 1)`.
  #[allow(clippy::cast_precision_loss)]
  pub fn unit(&mut self) -> f64 {
    (self.next_u64() >> 11) as f64 / (1u64 << 53) as f64
  }

  /// Uniform in `[0, n)`; `n` must be non-zero.
  #[allow(clippy::cast_possible_truncation)]
  pub fn below(&mut self, n: usize) -> usize {
    debug_assert!(n > 0);
    ((u128::from(self.next_u64()) * n as u128) >> 64) as usize
  }

  /// Uniform in `[lo, hi]`.
  pub fn range(&mut self, lo: usize, hi: usize) -> usize {
    lo + self.below(hi - lo + 1)
  }

  pub fn chance(&mut self, p: f64) -> bool {
    self.unit() < p
  }

  pub fn pick<'a, T>(&mut self, items: &'a [T]) -> &'a T {
    &items[self.below(items.len())]
  }

  /// Sample an index from a table of (rounded) weights.
  pub fn weighted(&mut self, weights: &[f64]) -> usize {
    let total: f64 = weights.iter().sum();
    let mut x = self.unit() * total;
    for (i, w) in weights.iter().enumerate() {
      if x < *w {
        return i;
      }
      x -= *w;
    }
    weights.len() - 1
  }

  /// Geometric number of trials until the first success (>= 1).
  #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
  pub fn geometric(&mut self, p: f64) -> usize {
    let u = 1.0 - self.unit();
    (u.ln() / (1.0 - p).ln()).floor() as usize + 1
  }

  /// Pareto-distributed value with the given minimum and shape.
  pub fn pareto(&mut self, min: f64, alpha: f64) -> f64 {
    let u = 1.0 - self.unit();
    min / u.powf(1.0 / alpha)
  }

  /// Index in `[0, n)` biased towards 0 (popular items first).
  #[allow(
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    clippy::cast_precision_loss
  )]
  pub fn skewed(&mut self, n: usize, skew: f64) -> usize {
    let u = self.unit().powf(skew);
    ((u * n as f64) as usize).min(n - 1)
  }
}

// ---------------------------------------------------------------------------
// Profile parameters (all rounded)
// ---------------------------------------------------------------------------

/// Tunable, rounded shape parameters of the "large-monorepo profile".
#[derive(Clone, Debug)]
pub struct Profile {
  /// Average serialised bytes per entry (used to turn `--size` into an entry count).
  pub bytes_per_entry: f64,
  pub workspace_fraction: f64,
  pub patch_fraction: f64,
  pub portal_fraction: f64,
  /// Entries that only carry a `dependencies` block (no version/resolution/...).
  pub stub_fraction: f64,
  /// Header descriptor-count weights for index 1.., workspace entries.
  pub ws_header_weights: [f64; 4],
  /// Header descriptor-count weights for index 1.., registry entries.
  pub npm_header_weights: [f64; 4],
  /// Probability that a header with >= 4 keys has an extreme (20+) key count.
  pub header_tail_extreme: f64,
  pub ws_scoped: f64,
  pub npm_scoped: f64,
  pub deps_given_ws: f64,
  pub deps_given_npm: f64,
  /// Fraction of workspace dependency blocks with >= 20 lines.
  pub ws_deps_long: f64,
  /// Geometric success probability for registry dependency counts.
  pub npm_deps_geometric_p: f64,
  /// Fraction of registry dependency blocks with exactly one line.
  pub npm_deps_single: f64,
  pub ws_dep_root_protocol: f64,
  pub peer_deps: f64,
  pub peer_meta_given_peers: f64,
  pub deps_meta_given_deps: f64,
  pub bin: f64,
  pub conditions: f64,
  /// Fraction of hard-link registry entries without a checksum line.
  pub hard_without_checksum: f64,
}

impl Default for Profile {
  fn default() -> Self {
    Self {
      bytes_per_entry: 890.0,
      workspace_fraction: 0.49,
      patch_fraction: 0.003,
      portal_fraction: 0.001,
      stub_fraction: 0.0001,
      ws_header_weights: [0.20, 0.72, 0.06, 0.02],
      npm_header_weights: [0.79, 0.13, 0.055, 0.025],
      header_tail_extreme: 0.02,
      ws_scoped: 0.88,
      npm_scoped: 0.6,
      deps_given_ws: 0.86,
      deps_given_npm: 0.73,
      ws_deps_long: 0.47,
      npm_deps_geometric_p: 0.15,
      npm_deps_single: 0.3,
      ws_dep_root_protocol: 0.3,
      peer_deps: 0.12,
      peer_meta_given_peers: 0.14,
      deps_meta_given_deps: 0.008,
      bin: 0.054,
      conditions: 0.076,
      hard_without_checksum: 0.07,
    }
  }
}

// ---------------------------------------------------------------------------
// Pseudo-word names
// ---------------------------------------------------------------------------

const ONSETS: &[&str] = &[
  "b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "r", "s", "t", "v", "w", "z", "br",
  "cl", "dr", "fl", "gr", "pl", "st", "tr", "sk", "sh", "th", "qu",
];
const VOWELS: &[&str] = &[
  "a", "e", "i", "o", "u", "a", "e", "o", "ae", "io", "ou", "ea",
];
const CODAS: &[&str] = &["", "", "", "n", "r", "l", "s", "x", "m", "k", "d"];

fn word(rng: &mut Rng, syllables: usize) -> String {
  let mut w = String::new();
  for _ in 0..syllables {
    w.push_str(rng.pick(ONSETS));
    w.push_str(rng.pick(VOWELS));
    w.push_str(rng.pick(CODAS));
  }
  w
}

/// A pseudo package basename such as `vorlin-kesta-dru`.
fn base_name(rng: &mut Rng) -> String {
  let mut parts = 1 + rng.weighted(&[0.20, 0.34, 0.28, 0.13, 0.05]);
  if rng.chance(0.025) {
    // rare very long names
    parts += rng.range(3, 6);
  }
  let mut name = String::new();
  for i in 0..parts {
    if i > 0 {
      name.push(if rng.chance(0.04) { '_' } else { '-' });
    }
    let syl = 1 + rng.weighted(&[0.2, 0.5, 0.3]);
    name.push_str(&word(rng, syl));
  }
  if rng.chance(0.03) {
    name.push_str(".js");
  }
  name
}

struct NameGen {
  used: HashSet<String>,
  scopes: Vec<String>,
}

impl NameGen {
  fn new(rng: &mut Rng, scope_count: usize) -> Self {
    let scopes = (0..scope_count)
      .map(|_| format!("@{}", word(rng, 3)))
      .collect();
    Self {
      used: HashSet::new(),
      scopes,
    }
  }

  /// Unique pseudo name; scoped names pick one of a limited set of scopes.
  fn unique(&mut self, rng: &mut Rng, scoped: bool) -> String {
    loop {
      let base = base_name(rng);
      let name = if scoped {
        let scope = rng.pick(&self.scopes).clone();
        format!("{scope}/{base}")
      } else {
        base
      };
      if self.used.insert(name.clone()) {
        return name;
      }
    }
  }
}

// ---------------------------------------------------------------------------
// Versions and ranges
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Version {
  major: u32,
  minor: u32,
  patch: u32,
  pre: Option<u32>,
}

impl Version {
  fn random(rng: &mut Rng) -> Self {
    let major = match rng.weighted(&[0.25, 0.55, 0.20]) {
      0 => 0,
      1 => rng.range(1, 9) as u32,
      _ => rng.range(10, 40) as u32,
    };
    let minor = rng.skewed(30, 1.6) as u32;
    let patch = rng.skewed(40, 1.6) as u32;
    let pre = rng.chance(0.004).then(|| rng.range(0, 12) as u32);
    Self {
      major,
      minor,
      patch,
      pre,
    }
  }

  fn text(self) -> String {
    match self.pre {
      Some(n) => format!("{}.{}.{}-rc.{n}", self.major, self.minor, self.patch),
      None => format!("{}.{}.{}", self.major, self.minor, self.patch),
    }
  }

  /// A semver range that this version satisfies (not the exact version).
  fn range_variant(self, rng: &mut Rng) -> String {
    let (ma, mi, pa) = (self.major, self.minor, self.patch);
    match rng.weighted(&[0.45, 0.12, 0.10, 0.08, 0.08, 0.05, 0.04, 0.04, 0.04, 0.002]) {
      0 => format!("^{ma}.{mi}.{pa}"),
      1 => format!("^{ma}.{}.{}", rng.below(mi as usize + 1), 0),
      2 => format!("~{ma}.{mi}.{pa}"),
      3 => format!("^{ma}.0.0"),
      4 => format!("{ma}.{mi}.{pa}"),
      5 => format!("~{ma}.{mi}.{}", rng.below(pa as usize + 1)),
      6 => format!(">={ma}.{}.{}", rng.below(mi as usize + 1), 0),
      7 => format!("^{}.0.0 || ^{ma}.0.0", ma.saturating_sub(1)),
      9 => (0..rng.range(6, 14))
        .map(|i| format!("^{}.{}.0", ma.saturating_sub(i as u32), mi))
        .collect::<Vec<_>>()
        .join(" || "),
      _ => format!(
        "^{ma}.{}.{}",
        rng.below(mi as usize + 1),
        rng.below(pa as usize + 1)
      ),
    }
  }
}

/// Quote a YAML-ish scalar the way yarn's `syml` stringifier would.
fn needs_quotes(s: &str) -> bool {
  let Some(first) = s.chars().next() else {
    return true;
  };
  if "-?:,][{}#&*!|>'\"%@` \t\r\n".contains(first) {
    return true;
  }
  if s.ends_with(' ') || s.contains("  ") {
    return true;
  }
  s.chars().any(|c| ",][{}:#\r\n".contains(c))
}

fn push_scalar(out: &mut String, s: &str) {
  if needs_quotes(s) {
    out.push('"');
    out.push_str(s);
    out.push('"');
  } else {
    out.push_str(s);
  }
}

// ---------------------------------------------------------------------------
// Skeletons (pass 1) and bodies (pass 2)
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Kind {
  Workspace,
  Npm,
  Patch,
  Portal,
  Stub,
}

struct Skeleton {
  kind: Kind,
  name: String,
  version: Option<Version>,
  /// Workspace path or portal path.
  path: String,
  /// Full descriptor ranges without the leading `name@`, e.g. `npm:^1.2.3`.
  ranges: Vec<String>,
}

/// Summary of what was generated, for the CLI to print.
#[derive(Clone, Copy, Debug, Default)]
pub struct Summary {
  pub entries: usize,
  pub bytes: usize,
  pub lines: usize,
  pub descriptors: usize,
}

fn header_count(rng: &mut Rng, weights: &[f64; 4], p: &Profile) -> usize {
  let i = rng.weighted(weights);
  if i < 3 {
    return i + 1;
  }
  // 4+ keys: mostly 4..=10, rare long tail.
  if rng.chance(p.header_tail_extreme) {
    rng.range(20, 48)
  } else {
    4 + rng.geometric(0.4).min(14) - 1
  }
}

fn fake_hash(rng: &mut Rng, hex_len: usize) -> String {
  let mut s = String::with_capacity(hex_len);
  while s.len() < hex_len {
    let _ = write!(s, "{:016x}", rng.next_u64());
  }
  s.truncate(hex_len);
  s
}

/// Pass 1: decide kinds, names, versions and descriptor ranges.
fn build_skeletons(rng: &mut Rng, p: &Profile, entries: usize) -> Vec<Skeleton> {
  #[allow(
    clippy::cast_precision_loss,
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss
  )]
  let count = |f: f64| (entries as f64 * f).round() as usize;
  let ws_n = count(p.workspace_fraction);
  let patch_n = count(p.patch_fraction);
  let portal_n = count(p.portal_fraction);
  let stub_n = count(p.stub_fraction).max(if entries >= 1000 { 3 } else { 0 });
  let fixed = ws_n + patch_n + portal_n + stub_n;
  let npm_n = entries.saturating_sub(fixed);
  let ws_n = entries.saturating_sub(npm_n + patch_n + portal_n + stub_n);

  let mut names = NameGen::new(rng, 6);
  let mut out: Vec<Skeleton> = Vec::with_capacity(entries);

  // Workspaces.
  let roots = ["packages", "apps", "libs", "services", "tools"];
  for _ in 0..ws_n {
    let scoped = rng.chance(p.ws_scoped);
    let name = names.unique(rng, scoped);
    let slug = name.rsplit('/').next().unwrap_or(&name).to_owned();
    let path = if rng.chance(0.4) {
      format!("{}/{}/{slug}", rng.pick(&roots), word(rng, 2))
    } else {
      format!("{}/{slug}", rng.pick(&roots))
    };
    let want = header_count(rng, &p.ws_header_weights, p);
    let mut ranges = vec![format!("workspace:{path}")];
    let mut extras: Vec<String> = vec![
      "workspace:*".into(),
      "workspace:^".into(),
      "workspace:~".into(),
      "workspace:.".into(),
    ];
    // Distinct extra descriptors in a stable, rarely-colliding order.
    for i in 0.. {
      if ranges.len() >= want {
        break;
      }
      if let Some(r) = extras.first().cloned() {
        extras.remove(0);
        ranges.push(r);
      } else {
        ranges.push(format!("workspace:^{}.{}.0", i / 10, i % 10));
      }
    }
    out.push(Skeleton {
      kind: Kind::Workspace,
      name,
      version: None,
      path,
      ranges,
    });
  }

  // Registry packages: a few versions per name.
  let mut remaining = npm_n;
  while remaining > 0 {
    let scoped = rng.chance(p.npm_scoped);
    let name = names.unique(rng, scoped);
    let nv = rng.geometric(0.55).min(12).min(remaining);
    let mut seen: HashSet<Version> = HashSet::new();
    for _ in 0..nv {
      let mut v = Version::random(rng);
      let mut guard = 0;
      while !seen.insert(v) && guard < 50 {
        v = Version::random(rng);
        guard += 1;
      }
      let want = header_count(rng, &p.npm_header_weights, p);
      let mut ranges: Vec<String> = Vec::new();
      let mut tries = 0;
      while ranges.len() < want && tries < want * 6 {
        tries += 1;
        let r = format!("npm:{}", v.range_variant(rng));
        if !ranges.contains(&r) {
          ranges.push(r);
        }
      }
      // Always include the exact-version descriptor as a fallback.
      if ranges.is_empty() {
        ranges.push(format!("npm:{}", v.text()));
      }
      out.push(Skeleton {
        kind: Kind::Npm,
        name: name.clone(),
        version: Some(v),
        path: String::new(),
        ranges,
      });
      remaining -= 1;
    }
  }

  // Patch entries.
  for _ in 0..patch_n {
    let scoped = rng.chance(0.4);
    let name = names.unique(rng, scoped);
    let v = Version::random(rng);
    let builtin = rng.chance(0.5);
    let source = if builtin {
      format!("optional!builtin<compat/{}>", name.replace('/', "-"))
    } else {
      format!(
        "~/.yarn/patches/{}-npm-{}-{}.patch",
        name.trim_start_matches('@').replace('/', "-"),
        v.text(),
        fake_hash(rng, 10)
      )
    };
    let want = 1 + usize::from(rng.chance(0.3)) + usize::from(rng.chance(0.1));
    let mut ranges: Vec<String> = Vec::new();
    for _ in 0..want * 4 {
      if ranges.len() >= want {
        break;
      }
      let src_range = v.range_variant(rng).replace(" || ", "%20%7C%7C%20");
      let r = format!("patch:{name}@npm%3A{src_range}#{source}");
      if !ranges.contains(&r) {
        ranges.push(r);
      }
    }
    out.push(Skeleton {
      kind: Kind::Patch,
      name,
      version: Some(v),
      path: source,
      ranges,
    });
  }

  // Portal entries.
  for _ in 0..portal_n {
    let scoped = rng.chance(0.5);
    let name = names.unique(rng, scoped);
    let slug = name.rsplit('/').next().unwrap_or(&name).to_owned();
    let path = format!("../{}/{slug}", word(rng, 2));
    let host = word(rng, 2);
    let range = format!("portal:{path}::locator={host}%40workspace%3A.");
    out.push(Skeleton {
      kind: Kind::Portal,
      name,
      version: None,
      path,
      ranges: vec![range],
    });
  }

  // Stubs.
  for _ in 0..stub_n {
    let name = names.unique(rng, false);
    let v = Version::random(rng);
    out.push(Skeleton {
      kind: Kind::Stub,
      name,
      version: Some(v),
      path: String::new(),
      // bare ranges without a protocol
      ranges: vec!["*".to_owned(), "latest".to_owned()],
    });
  }

  // Deterministic shuffle so kinds are interleaved in generation order
  // (final file order is by header key anyway).
  for i in (1..out.len()).rev() {
    let j = rng.below(i + 1);
    out.swap(i, j);
  }
  out
}

struct Pools {
  /// Indexes into skeletons.
  ws: Vec<usize>,
  npm: Vec<usize>,
}

fn dep_count_ws(rng: &mut Rng, p: &Profile) -> usize {
  if rng.chance(p.ws_deps_long) {
    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    let n = rng.pareto(20.0, 2.0) as usize;
    n.min(1775)
  } else {
    rng.range(1, 19)
  }
}

fn dep_count_npm(rng: &mut Rng, p: &Profile) -> usize {
  if rng.chance(p.npm_deps_single) {
    1
  } else {
    (1 + rng.geometric(p.npm_deps_geometric_p)).min(60)
  }
}

/// Pick `n` distinct targets (index into `pool`), popularity biased.
fn pick_distinct(rng: &mut Rng, pool: &[usize], n: usize, skew: f64) -> Vec<usize> {
  let n = n.min(pool.len());
  let mut chosen: HashSet<usize> = HashSet::with_capacity(n);
  let mut order = Vec::with_capacity(n);
  let mut attempts = 0;
  while order.len() < n && attempts < n * 8 + 16 {
    attempts += 1;
    let idx = pool[rng.skewed(pool.len(), skew)];
    if chosen.insert(idx) {
      order.push(idx);
    }
  }
  // Fill any shortfall (very large blocks) by scanning the pool in order.
  if order.len() < n {
    for &idx in pool {
      if order.len() >= n {
        break;
      }
      if chosen.insert(idx) {
        order.push(idx);
      }
    }
  }
  order
}

fn push_name_key(out: &mut String, name: &str) {
  if name.starts_with('@') {
    out.push('"');
    out.push_str(name);
    out.push('"');
  } else {
    out.push_str(name);
  }
}

const BIN_DIRS: &[&str] = &["bin/", "./bin/", "lib/", "./lib/", "dist/", "./dist/", ""];
const CONDITIONS: &[&str] = &[
  "os=darwin",
  "os=darwin & cpu=arm64",
  "os=darwin & cpu=x64",
  "os=linux & cpu=x64 & libc=glibc",
  "os=linux & cpu=x64 & libc=musl",
  "os=linux & cpu=arm64 & libc=glibc",
  "os=linux & cpu=arm64 & libc=musl",
  "os=linux & cpu=arm",
  "os=win32 & cpu=x64",
  "os=win32 & cpu=arm64",
  "os=freebsd & cpu=x64",
  "os=android & cpu=arm64",
  "cpu=wasm32",
  "(os=linux | os=darwin | os=freebsd | os=win32)",
];

/// Pass 2: render one entry's body (everything after the header line).
#[allow(clippy::too_many_lines)]
fn render_body(
  rng: &mut Rng,
  p: &Profile,
  sk: &[Skeleton],
  pools: &Pools,
  me: usize,
  out: &mut String,
) {
  let s = &sk[me];
  let linkage_soft = matches!(s.kind, Kind::Workspace | Kind::Portal);

  // version + resolution
  if s.kind != Kind::Stub {
    match s.kind {
      Kind::Workspace => {
        out.push_str("  version: 0.0.0-use.local\n");
        let _ = writeln!(out, "  resolution: \"{}@workspace:{}\"", s.name, s.path);
      }
      Kind::Npm => {
        let v = s.version.expect("npm version").text();
        let _ = writeln!(out, "  version: {v}");
        let _ = writeln!(out, "  resolution: \"{}@npm:{v}\"", s.name);
      }
      Kind::Patch => {
        let v = s.version.expect("patch version").text();
        let hash = fake_hash(rng, 6);
        let _ = writeln!(out, "  version: {v}");
        let _ = writeln!(
          out,
          "  resolution: \"{}@patch:{}@npm%3A{v}#{}::version={v}&hash={hash}\"",
          s.name, s.name, s.path
        );
      }
      Kind::Portal => {
        let _ = writeln!(out, "  version: 0.0.0-use.local");
        let _ = writeln!(out, "  resolution: \"{}@{}\"", s.name, s.ranges[0]);
      }
      Kind::Stub => unreachable!(),
    }
  }

  // dependencies
  let has_deps = match s.kind {
    Kind::Workspace => rng.chance(p.deps_given_ws),
    Kind::Npm => rng.chance(p.deps_given_npm),
    Kind::Patch => rng.chance(0.8),
    Kind::Stub => true,
    Kind::Portal => rng.chance(0.3),
  };
  let mut dep_names: Vec<String> = Vec::new();
  if has_deps {
    let mut lines: Vec<(String, String)> = Vec::new();
    if s.kind == Kind::Workspace {
      let n = dep_count_ws(rng, p);
      for idx in pick_distinct(rng, &pools.ws, n, 1.8) {
        if idx == me {
          continue;
        }
        let target = &sk[idx];
        let range = if rng.chance(p.ws_dep_root_protocol) {
          format!("root:{}", rng.pick(&["*", "^", ".", "./src", "~"]))
        } else if rng.chance(0.85) {
          "workspace:*".to_owned()
        } else {
          "workspace:^".to_owned()
        };
        lines.push((target.name.clone(), range));
      }
    } else {
      let n = if s.kind == Kind::Stub {
        rng.range(1, 3)
      } else {
        dep_count_npm(rng, p)
      };
      for idx in pick_distinct(rng, &pools.npm, n, 2.2) {
        if idx == me {
          continue;
        }
        let target = &sk[idx];
        lines.push((target.name.clone(), rng.pick(&target.ranges).clone()));
      }
    }
    lines.sort();
    lines.dedup_by(|a, b| a.0 == b.0);
    if !lines.is_empty() {
      out.push_str("  dependencies:\n");
      for (name, range) in &lines {
        out.push_str("    ");
        push_name_key(out, name);
        let _ = writeln!(out, ": \"{range}\"");
        dep_names.push(name.clone());
      }
    }
  }

  // peerDependencies (+ meta)
  let mut peer_names: Vec<String> = Vec::new();
  if matches!(s.kind, Kind::Npm | Kind::Workspace) && rng.chance(p.peer_deps) {
    let n = match rng.weighted(&[
      0.6575, 0.2069, 0.0695, 0.0476, 0.0083, 0.0046, 0.0018, 0.0022, 0.0, 0.0, 0.0006, 0.0003,
      0.0006,
    ]) {
      12 => rng.range(20, 41),
      i => i + 1,
    };
    let mut lines: Vec<(String, String)> = Vec::new();
    for idx in pick_distinct(rng, &pools.npm, n, 2.5) {
      if idx == me {
        continue;
      }
      let target = &sk[idx];
      let v = target.version.unwrap_or(Version {
        major: 1,
        minor: 0,
        patch: 0,
        pre: None,
      });
      let range = match rng.weighted(&[0.35, 0.25, 0.2, 0.1, 0.1]) {
        0 => "*".to_owned(),
        1 => format!("^{}.0.0", v.major),
        2 => format!("^{}.0.0 || ^{}.0.0", v.major.saturating_sub(1), v.major),
        3 => format!(">={}", v.major),
        _ => format!("^{}.{}.0", v.major, v.minor),
      };
      lines.push((target.name.clone(), range));
    }
    lines.sort();
    lines.dedup_by(|a, b| a.0 == b.0);
    if !lines.is_empty() {
      out.push_str("  peerDependencies:\n");
      for (name, range) in &lines {
        out.push_str("    ");
        push_name_key(out, name);
        out.push_str(": ");
        push_scalar(out, range);
        out.push('\n');
        peer_names.push(name.clone());
      }
    }
  }

  // dependenciesMeta
  if !dep_names.is_empty() && rng.chance(p.deps_meta_given_deps) {
    let n = match rng.weighted(&[0.2, 0.2, 0.6]) {
      0 => 1,
      1 => 2,
      #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
      _ => (rng.pareto(3.0, 0.7) as usize).min(26),
    }
    .min(dep_names.len());
    out.push_str("  dependenciesMeta:\n");
    for name in dep_names.iter().take(n) {
      out.push_str("    ");
      push_name_key(out, name);
      out.push_str(":\n");
      match rng.weighted(&[0.85, 0.10, 0.05]) {
        0 => out.push_str("      optional: true\n"),
        1 => out.push_str("      built: false\n"),
        _ => out.push_str("      unplugged: true\n"),
      }
    }
  }

  // peerDependenciesMeta
  if !peer_names.is_empty() && rng.chance(p.peer_meta_given_peers) {
    let n = match rng.weighted(&[0.58, 0.22, 0.08, 0.05, 0.03, 0.02, 0.01, 0.01]) {
      7 => rng.range(8, 24),
      i => i + 1,
    }
    .min(peer_names.len());
    out.push_str("  peerDependenciesMeta:\n");
    for name in peer_names.iter().take(n) {
      out.push_str("    ");
      push_name_key(out, name);
      out.push_str(":\n      optional: true\n");
    }
  }

  // bin
  if s.kind == Kind::Npm && rng.chance(p.bin) {
    let n = 1
      + rng.weighted(&[
        0.87, 0.073, 0.03, 0.008, 0.004, 0.007, 0.003, 0.0, 0.004, 0.001,
      ]);
    out.push_str("  bin:\n");
    let mut used: HashSet<String> = HashSet::new();
    for i in 0..n {
      let bin_name = if i == 0 {
        s.name.rsplit('/').next().unwrap_or(&s.name).to_owned()
      } else {
        word(rng, 2)
      };
      if !used.insert(bin_name.clone()) {
        continue;
      }
      let dir = rng.pick(BIN_DIRS);
      let file = word(rng, 2);
      let ext = if rng.chance(0.7) { ".js" } else { "" };
      let _ = writeln!(out, "    {bin_name}: {dir}{file}{ext}");
    }
  }

  // checksum
  let checksum = match s.kind {
    Kind::Npm => !rng.chance(p.hard_without_checksum),
    _ => false,
  };
  if checksum {
    let _ = writeln!(out, "  checksum: 10/{}", fake_hash(rng, 128));
  }

  // conditions
  if matches!(s.kind, Kind::Npm | Kind::Patch)
    && rng.chance(if s.kind == Kind::Patch {
      0.4
    } else {
      p.conditions
    })
  {
    let _ = writeln!(out, "  conditions: {}", rng.pick(CONDITIONS));
  }

  if s.kind != Kind::Stub {
    let lang = if s.kind == Kind::Workspace {
      "unknown"
    } else {
      "node"
    };
    let _ = writeln!(out, "  languageName: {lang}");
    let _ = writeln!(
      out,
      "  linkType: {}",
      if linkage_soft { "soft" } else { "hard" }
    );
  }
}

/// Render the whole lockfile for exactly `entries` entries.
fn render(seed: u64, entries: usize, profile: &Profile) -> (String, Summary) {
  let mut rng = Rng::new(seed);
  let skeletons = build_skeletons(&mut rng, profile, entries);
  let pools = Pools {
    ws: skeletons
      .iter()
      .enumerate()
      .filter(|(_, s)| s.kind == Kind::Workspace)
      .map(|(i, _)| i)
      .collect(),
    npm: skeletons
      .iter()
      .enumerate()
      .filter(|(_, s)| matches!(s.kind, Kind::Npm))
      .map(|(i, _)| i)
      .collect(),
  };
  // Guard: tiny variants may lack one of the pools.
  // Popular targets (front of each pool) tend to have short names.
  let (mut ws_pool, mut npm_pool) = (pools.ws, pools.npm);
  ws_pool.sort_by_key(|&i| (skeletons[i].name.len(), i));
  npm_pool.sort_by_key(|&i| (skeletons[i].name.len(), i));
  let pools = Pools {
    ws: if ws_pool.is_empty() {
      npm_pool.clone()
    } else {
      ws_pool.clone()
    },
    npm: if npm_pool.is_empty() {
      ws_pool
    } else {
      npm_pool
    },
  };

  let mut rendered: Vec<(String, String)> = Vec::with_capacity(skeletons.len());
  let mut descriptors = 0usize;
  for (i, s) in skeletons.iter().enumerate() {
    let mut descs: Vec<String> = s.ranges.iter().map(|r| format!("{}@{r}", s.name)).collect();
    descs.sort();
    descriptors += descs.len();
    let mut header = String::from("\"");
    header.push_str(&descs.join(", "));
    header.push_str("\":\n");
    let mut body = String::with_capacity(512);
    render_body(&mut rng, profile, &skeletons, &pools, i, &mut body);
    rendered.push((descs.join(", "), header + &body));
  }
  rendered.sort_by(|a, b| a.0.cmp(&b.0));

  let mut out = String::with_capacity(rendered.iter().map(|r| r.1.len() + 1).sum::<usize>() + 256);
  out.push_str("# This file is generated by running \"yarn install\" inside your project.\n");
  out.push_str("# Manual changes might be lost - proceed with caution!\n\n");
  out.push_str("__metadata:\n  version: 8\n  cacheKey: 10\n");
  for (_, text) in &rendered {
    out.push('\n');
    out.push_str(text);
  }
  let lines = out.bytes().filter(|b| *b == b'\n').count();
  let summary = Summary {
    entries: rendered.len(),
    bytes: out.len(),
    lines,
    descriptors,
  };
  (out, summary)
}

/// What to generate.
#[derive(Clone, Copy, Debug)]
pub enum Target {
  /// Exactly this many entries.
  Entries(usize),
  /// Approximately this many bytes (within about 1%).
  Bytes(usize),
}

/// Generate a synthetic lockfile; deterministic for a given `seed` and `target`.
#[must_use]
#[allow(
  clippy::cast_precision_loss,
  clippy::cast_possible_truncation,
  clippy::cast_sign_loss
)]
pub fn generate(seed: u64, target: Target, profile: &Profile) -> (String, Summary) {
  match target {
    Target::Entries(n) => render(seed, n.max(1), profile),
    Target::Bytes(bytes) => {
      let mut entries = ((bytes as f64 / profile.bytes_per_entry) as usize).max(1);
      let mut best = render(seed, entries, profile);
      for _ in 0..4 {
        let ratio = bytes as f64 / best.1.bytes as f64;
        if (ratio - 1.0).abs() < 0.005 {
          break;
        }
        entries = ((entries as f64 * ratio) as usize).max(1);
        best = render(seed, entries, profile);
      }
      best
    }
  }
}

/// Parse sizes like `24MB`, `24MiB`, `512KB`, `1048576`.
pub fn parse_size(s: &str) -> Result<usize, String> {
  let t = s.trim();
  let split = t
    .find(|c: char| !(c.is_ascii_digit() || c == '.'))
    .unwrap_or(t.len());
  let (num, unit) = t.split_at(split);
  let n: f64 = num.parse().map_err(|_| format!("invalid size: {s}"))?;
  let mult: f64 = match unit.trim().to_ascii_lowercase().as_str() {
    "" | "b" => 1.0,
    "kb" => 1e3,
    "mb" => 1e6,
    "gb" => 1e9,
    "kib" => 1024.0,
    "mib" => 1024.0 * 1024.0,
    "gib" => 1024.0 * 1024.0 * 1024.0,
    other => return Err(format!("unknown size unit: {other}")),
  };
  #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
  Ok((n * mult) as usize)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn deterministic_for_same_seed() {
    let p = Profile::default();
    let a = generate(7, Target::Entries(300), &p).0;
    let b = generate(7, Target::Entries(300), &p).0;
    assert_eq!(a, b);
    let c = generate(8, Target::Entries(300), &p).0;
    assert_ne!(a, c);
  }

  #[test]
  fn starts_with_berry_v8_preamble() {
    let (text, summary) = generate(1, Target::Entries(50), &Profile::default());
    assert!(text.starts_with(
      "# This file is generated by running \"yarn install\" inside your project.\n# Manual changes might be lost - proceed with caution!\n\n__metadata:\n  version: 8\n  cacheKey: 10\n\n"
    ));
    assert_eq!(summary.entries, 50);
    assert!(text.ends_with('\n'));
  }

  #[test]
  fn size_target_is_close() {
    let (text, _) = generate(1, Target::Bytes(1_000_000), &Profile::default());
    let ratio = text.len() as f64 / 1_000_000.0;
    // heavy-tailed entries make small targets lumpy
    assert!((0.9..1.1).contains(&ratio), "ratio {ratio}");
  }

  #[test]
  fn parse_size_units() {
    assert_eq!(parse_size("24MB").unwrap(), 24_000_000);
    assert_eq!(parse_size("1KiB").unwrap(), 1024);
    assert_eq!(parse_size("100").unwrap(), 100);
    assert!(parse_size("1xb").is_err());
  }
}
