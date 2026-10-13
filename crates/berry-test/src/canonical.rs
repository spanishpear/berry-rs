//! Canonical, deterministic text dump of a parsed lockfile.
//!
//! This is the equivalence oracle used to compare two parser implementations:
//! if the dumps of the same input are byte-identical, no parsed field differs.
//!
//! Properties:
//!
//! * Every field reachable through the public API is printed: lockfile
//!   metadata, top-level `resolutions` / `constraints` (always `<none>` for
//!   `berry`: Yarn never writes those blocks), every entry's descriptors
//!   (ident, raw range, protocol classification, selector) and every package
//!   field (version, resolution, resolution locator, language name, link
//!   type, checksum, conditions, all dependency lists, all meta lists, bin
//!   list).
//! * Key/value lists are printed with berry 0.6's map semantics (one item per
//!   key, the last one written) sorted by key; entry and descriptor order
//!   (which is semantic) is preserved as parsed.
//! * Strings use Rust `{:?}` escaping so that whitespace and quotes are
//!   unambiguous (e.g. trailing spaces or `\r` are visible).
//!
//! [`write`] dumps a `berry::Lockfile` (0.7) through its views;
//! [`canonical_dump_impl!`](crate::canonical_dump_impl) generates the same
//! dump for a crate with the berry 0.6 data model (the frozen `berry_ref`
//! oracle, [`crate::canonical_ref`]). Both must stay byte-identical: when a
//! field is added to the model, add it to both, otherwise the oracle would
//! silently ignore it.

/// Version tag of the dump format; bump when the layout changes.
pub const FORMAT_VERSION: u32 = 1;

/// Generates the canonical dump functions for a crate with the berry 0.6 data
/// model (`Lockfile { metadata, entries, .. }` with `HashMap` fields).
///
/// It is instantiated for the frozen `berry_ref` oracle
/// ([`crate::canonical_ref`]); [`write`] produces the same bytes from a berry
/// 0.7 `Lockfile`.
#[macro_export]
macro_rules! canonical_dump_impl {
  ($krate:ident) => {
    use std::collections::HashMap;
    use std::io::{self, Write};
    use $krate::ident::{Descriptor, Ident};
    use $krate::lockfile::Lockfile;
    use $krate::metadata::{DependencyMeta, PeerDependencyMeta};

    pub use $crate::canonical::FORMAT_VERSION;

    fn opt_str(s: Option<&str>) -> String {
      s.map_or_else(|| "<none>".to_owned(), |s| format!("{s:?}"))
    }

    fn ident(i: &Ident<'_>) -> String {
      format!("scope={} name={:?}", opt_str(i.scope()), i.name())
    }

    fn descriptor(d: &Descriptor<'_>) -> String {
      let r = d.range_struct();
      format!(
        "{} range={:?} protocol_str={} protocol={:?} selector={:?}",
        ident(d.ident()),
        r.raw(),
        opt_str(r.protocol_str()),
        r.protocol(),
        r.selector(),
      )
    }

    fn sorted_by_ident<'m, 'a, V>(map: &'m HashMap<Ident<'a>, V>) -> Vec<(&'m Ident<'a>, &'m V)> {
      let mut items: Vec<_> = map.iter().collect();
      items.sort_by(|a, b| (a.0.scope(), a.0.name()).cmp(&(b.0.scope(), b.0.name())));
      items
    }

    fn dep_meta(m: &DependencyMeta) -> String {
      format!(
        "built={:?} optional={:?} unplugged={:?}",
        m.built, m.optional, m.unplugged
      )
    }

    fn peer_meta(m: &PeerDependencyMeta) -> String {
      format!("optional={:?}", m.optional)
    }

    fn kv_block(
      out: &mut impl Write,
      name: &str,
      block: Option<&[(&str, &str)]>,
    ) -> io::Result<()> {
      match block {
        None => writeln!(out, "{name}: <none>"),
        Some(items) => {
          writeln!(out, "{name}: {}", items.len())?;
          for (i, (k, v)) in items.iter().enumerate() {
            writeln!(out, "  {name}[{i}] key={k:?} value={v:?}")?;
          }
          Ok(())
        }
      }
    }

    /// Write the canonical dump of `lockfile` to `out`.
    ///
    /// `remaining` is the input the parser left unconsumed; it is part of the
    /// dump so that a parser that stops early is detected.
    pub fn write_lockfile(
      out: &mut impl Write,
      lockfile: &Lockfile<'_>,
      remaining: &str,
    ) -> io::Result<()> {
      writeln!(out, "# berry canonical dump v{FORMAT_VERSION}")?;
      writeln!(out, "metadata.version: {:?}", lockfile.metadata.version)?;
      writeln!(out, "metadata.cache_key: {:?}", lockfile.metadata.cache_key)?;
      kv_block(out, "resolutions", lockfile.resolutions.as_deref())?;
      kv_block(out, "constraints", lockfile.constraints.as_deref())?;
      writeln!(
        out,
        "remaining: len={} non_whitespace={} head={:?}",
        remaining.len(),
        !remaining.trim().is_empty(),
        &remaining[..remaining
          .char_indices()
          .nth(200)
          .map_or(remaining.len(), |(i, _)| i)]
      )?;
      writeln!(out, "entries: {}", lockfile.entries.len())?;

      for (n, entry) in lockfile.entries.iter().enumerate() {
        writeln!(out, "entry {n}")?;
        writeln!(out, "  descriptors: {}", entry.descriptors.len())?;
        for (i, d) in entry.descriptors.iter().enumerate() {
          writeln!(out, "    descriptor[{i}] {}", descriptor(d))?;
        }

        let p = &entry.package;
        writeln!(out, "  version: {}", opt_str(p.version))?;
        writeln!(out, "  resolution: {}", opt_str(p.resolution))?;
        match &p.resolution_locator {
          None => writeln!(out, "  resolution_locator: <none>")?,
          Some(l) => writeln!(
            out,
            "  resolution_locator: {} reference={:?}",
            ident(l.ident()),
            l.reference()
          )?,
        }
        writeln!(out, "  language_name: {:?}", p.language_name)?;
        writeln!(out, "  link_type: {:?}", p.link_type)?;
        writeln!(out, "  checksum: {}", opt_str(p.checksum))?;
        writeln!(out, "  conditions: {}", opt_str(p.conditions))?;

        writeln!(out, "  dependencies: {}", p.dependencies.len())?;
        for (k, v) in sorted_by_ident(&p.dependencies) {
          writeln!(out, "    {} => {}", ident(k), descriptor(v))?;
        }
        writeln!(out, "  dependencies_meta: {}", p.dependencies_meta.len())?;
        for (k, v) in sorted_by_ident(&p.dependencies_meta) {
          writeln!(
            out,
            "    {} => {}",
            ident(k),
            v.as_ref().map_or_else(|| "<none>".to_owned(), dep_meta)
          )?;
        }
        writeln!(out, "  peer_dependencies: {}", p.peer_dependencies.len())?;
        for (k, v) in sorted_by_ident(&p.peer_dependencies) {
          writeln!(out, "    {} => {}", ident(k), descriptor(v))?;
        }
        writeln!(
          out,
          "  peer_dependencies_meta: {}",
          p.peer_dependencies_meta.len()
        )?;
        for (k, v) in sorted_by_ident(&p.peer_dependencies_meta) {
          writeln!(out, "    {} => {}", ident(k), peer_meta(v))?;
        }
        writeln!(out, "  bin: {}", p.bin.len())?;
        let mut bins: Vec<_> = p.bin.iter().collect();
        bins.sort();
        for (name, path) in bins {
          writeln!(out, "    {name:?} => {path:?}")?;
        }
      }
      Ok(())
    }

    /// Convenience wrapper: canonical dump as a `String`.
    #[must_use]
    pub fn to_string(lockfile: &Lockfile<'_>, remaining: &str) -> String {
      let mut buf = Vec::new();
      write_lockfile(&mut buf, lockfile, remaining).expect("writing to a Vec cannot fail");
      String::from_utf8(buf).expect("dump is valid UTF-8")
    }
  };
}

use berry::{Descriptor, Ident, Lockfile};
use std::io::{self, Write};

fn opt_str(s: Option<&str>) -> String {
  s.map_or_else(|| "<none>".to_owned(), |s| format!("{s:?}"))
}

fn ident(i: Ident<'_>) -> String {
  format!("scope={} name={:?}", opt_str(i.scope()), i.name())
}

fn descriptor(d: &Descriptor<'_>) -> String {
  let r = d.range_struct();
  format!(
    "{} range={:?} protocol_str={} protocol={:?} selector={:?}",
    ident(d.ident()),
    r.raw(),
    opt_str(r.protocol_str()),
    r.protocol(),
    r.selector(),
  )
}

/// One item per key, the last one in `items` (berry 0.6 `HashMap::insert`
/// semantics), sorted by key.
pub fn last_wins_sorted<T, K: Ord + Eq>(
  items: impl Iterator<Item = T>,
  key: impl Fn(&T) -> K,
) -> Vec<T> {
  let mut v: Vec<(K, usize, T)> = items.enumerate().map(|(i, t)| (key(&t), i, t)).collect();
  // Same key: later items first, so `dedup_by` (which keeps the first of a
  // run) keeps the last occurrence.
  v.sort_by(|a, b| a.0.cmp(&b.0).then(b.1.cmp(&a.1)));
  v.dedup_by(|later, kept| later.0 == kept.0);
  v.into_iter().map(|(_, _, t)| t).collect()
}

/// Write the canonical dump of `lockfile` to `out` (including its unparsed
/// [`remaining`](Lockfile::remaining) text, so a parser that stops early is
/// detected).
pub fn write(out: &mut impl Write, lockfile: &Lockfile<'_>) -> io::Result<()> {
  let remaining = lockfile.remaining();
  writeln!(out, "# berry canonical dump v{FORMAT_VERSION}")?;
  writeln!(out, "metadata.version: {:?}", lockfile.metadata().version)?;
  writeln!(
    out,
    "metadata.cache_key: {:?}",
    lockfile.metadata().cache_key
  )?;
  // berry 0.7 does not parse top-level blocks; Yarn does not write them.
  writeln!(out, "resolutions: <none>")?;
  writeln!(out, "constraints: <none>")?;
  writeln!(
    out,
    "remaining: len={} non_whitespace={} head={:?}",
    remaining.len(),
    !remaining.trim().is_empty(),
    &remaining[..remaining
      .char_indices()
      .nth(200)
      .map_or(remaining.len(), |(i, _)| i)]
  )?;
  writeln!(out, "entries: {}", lockfile.len())?;

  for (n, e) in lockfile.entries().enumerate() {
    writeln!(out, "entry {n}")?;
    writeln!(out, "  descriptors: {}", e.descriptors().len())?;
    for (i, d) in e.descriptors().iter().enumerate() {
      writeln!(out, "    descriptor[{i}] {}", descriptor(&d))?;
    }
    writeln!(out, "  version: {}", opt_str(e.version()))?;
    writeln!(out, "  resolution: {}", opt_str(e.resolution()))?;
    match e.resolution_locator() {
      None => writeln!(out, "  resolution_locator: <none>")?,
      Some(l) => writeln!(
        out,
        "  resolution_locator: {} reference={:?}",
        ident(l.ident()),
        l.reference()
      )?,
    }
    writeln!(out, "  language_name: {:?}", e.language_name())?;
    writeln!(out, "  link_type: {:?}", e.link_type())?;
    writeln!(out, "  checksum: {}", opt_str(e.checksum()))?;
    writeln!(out, "  conditions: {}", opt_str(e.conditions()))?;

    let deps = last_wins_sorted(e.dependencies().iter(), Descriptor::ident);
    writeln!(out, "  dependencies: {}", deps.len())?;
    for d in &deps {
      writeln!(out, "    {} => {}", ident(d.ident()), descriptor(d))?;
    }
    let dmeta = last_wins_sorted(e.dependencies_meta().iter(), |(k, _)| *k);
    writeln!(out, "  dependencies_meta: {}", dmeta.len())?;
    for (k, m) in &dmeta {
      writeln!(
        out,
        "    {} => built={:?} optional={:?} unplugged={:?}",
        ident(*k),
        m.built,
        m.optional,
        m.unplugged
      )?;
    }
    let peers = last_wins_sorted(e.peer_dependencies().iter(), Descriptor::ident);
    writeln!(out, "  peer_dependencies: {}", peers.len())?;
    for d in &peers {
      writeln!(out, "    {} => {}", ident(d.ident()), descriptor(d))?;
    }
    let pmeta = last_wins_sorted(e.peer_dependencies_meta().iter(), |(k, _)| *k);
    writeln!(out, "  peer_dependencies_meta: {}", pmeta.len())?;
    for (k, m) in &pmeta {
      writeln!(out, "    {} => optional={:?}", ident(*k), m.optional)?;
    }
    let bins = last_wins_sorted(e.bin().iter(), |(k, _)| *k);
    writeln!(out, "  bin: {}", bins.len())?;
    for (name, path) in bins {
      writeln!(out, "    {name:?} => {path:?}")?;
    }
  }
  Ok(())
}

/// [`write`] into a `String`.
#[must_use]
pub fn to_string(lockfile: &Lockfile<'_>) -> String {
  let mut buf = Vec::new();
  write(&mut buf, lockfile).expect("writing to a Vec cannot fail");
  String::from_utf8(buf).expect("dump is valid UTF-8")
}

#[cfg(test)]
mod tests {
  use super::*;

  const SAMPLE: &str = r#"# This file is generated by running "yarn install" inside your project.
# Manual changes might be lost - proceed with caution!

__metadata:
  version: 8
  cacheKey: 10

"@scope/pkg@npm:^1.0.0, @scope/pkg@npm:^1.2.0":
  version: 1.2.3
  resolution: "@scope/pkg@npm:1.2.3"
  dependencies:
    zeta: "npm:^1.0.0"
    "@a/alpha": "npm:^2.0.0"
    mid: "npm:*"
  peerDependencies:
    react: ^18.0.0
  dependenciesMeta:
    zeta:
      optional: true
      built: false
  peerDependenciesMeta:
    react:
      optional: true
  bin:
    zed: bin/zed.js
    abe: bin/abe.js
  checksum: 10/deadbeef
  conditions: os=darwin & cpu=arm64
  languageName: node
  linkType: hard

"app@workspace:packages/app":
  version: 0.0.0-use.local
  resolution: "app@workspace:packages/app"
  languageName: unknown
  linkType: soft
"#;

  fn dump(src: &str) -> String {
    to_string(&berry::parse(src).expect("parse"))
  }

  #[test]
  fn dump_equals_the_reference_dump() {
    let (rest, lf) = berry_ref::parse::parse_lockfile(SAMPLE).expect("reference");
    assert_eq!(dump(SAMPLE), crate::canonical_ref::to_string(&lf, rest));
  }

  #[test]
  fn dump_contains_every_field() {
    let dump = dump(SAMPLE);
    for needle in [
      "metadata.version: \"8\"",
      "metadata.cache_key: \"10\"",
      "resolutions: <none>",
      "constraints: <none>",
      "entries: 2",
      "descriptor[1] scope=\"@scope\" name=\"pkg\" range=\"npm:^1.2.0\" protocol_str=\"npm\" protocol=Npm selector=\"^1.2.0\"",
      "version: \"1.2.3\"",
      "resolution: \"@scope/pkg@npm:1.2.3\"",
      "resolution_locator: scope=\"@scope\" name=\"pkg\" reference=\"npm:1.2.3\"",
      "language_name: \"node\"",
      "link_type: Hard",
      "checksum: \"10/deadbeef\"",
      "conditions: \"os=darwin & cpu=arm64\"",
      "scope=<none> name=\"zeta\" => scope=<none> name=\"zeta\" range=\"npm:^1.0.0\"",
      "scope=<none> name=\"react\" => scope=<none> name=\"react\" range=\"^18.0.0\" protocol_str=<none> protocol=Unknown selector=\"^18.0.0\"",
      "scope=<none> name=\"zeta\" => built=Some(false) optional=Some(true) unplugged=None",
      "scope=<none> name=\"react\" => optional=true",
      "\"zed\" => \"bin/zed.js\"",
      "link_type: Soft",
      "language_name: \"unknown\"",
    ] {
      assert!(dump.contains(needle), "missing {needle:?} in:\n{dump}");
    }
  }

  #[test]
  fn maps_are_sorted() {
    let dump = dump(SAMPLE);
    // Sorted by (scope, name) with unscoped idents first.
    let a = dump.find("scope=\"@a\" name=\"alpha\" =>").expect("alpha");
    let m = dump.find("scope=<none> name=\"mid\" =>").expect("mid");
    let z = dump
      .find("scope=<none> name=\"zeta\" => scope")
      .expect("zeta");
    assert!(m < z && z < a);
    assert!(dump.find("\"abe\" =>").unwrap() < dump.find("\"zed\" =>").unwrap());
  }

  #[test]
  fn leftover_input_is_visible() {
    let input = format!("{SAMPLE}garbage line\n");
    let dump = dump(&input);
    assert!(dump.contains("non_whitespace=true"));
    assert!(dump.contains("garbage line"));
  }

  #[test]
  fn last_wins_sorted_keeps_the_last_occurrence() {
    let v = last_wins_sorted(
      [("b", 1), ("a", 2), ("b", 3), ("c", 4), ("a", 5)].into_iter(),
      |p| p.0,
    );
    assert_eq!(v, [("a", 5), ("b", 3), ("c", 4)]);
  }
}
