use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

use berry::lockfile::Lockfile;
use berry::parse::parse_lockfile;
use clap::Parser;
use serde_json::{Map, Value};

#[derive(Parser)]
#[command(name = "berry-compare")]
#[command(about = "Compare berry-core parser with @yarnpkg/parsers", long_about = None)]
struct Args {
  /// Path to the yarn.lock file
  #[arg(value_name = "FILE")]
  lockfile: PathBuf,

  /// Output detailed diffs (default: summary only)
  #[arg(short, long)]
  verbose: bool,
}

fn main() -> anyhow::Result<()> {
  let args = Args::parse();

  if !args.lockfile.exists() {
    eprintln!("Error: File not found: {}", args.lockfile.display());
    std::process::exit(1);
  }

  println!("Comparing parser outputs for: {}", args.lockfile.display());
  println!();

  // Parse with berry-core
  let berry_result = parse_with_berry(&args.lockfile)?;
  println!("✓ Successfully parsed with berry-core");
  println!("  Entries found: {}", berry_result.len());

  // Parse with @yarnpkg/parsers
  let yarnpkg_result = parse_with_yarnpkg(&args.lockfile)?;
  println!("✓ Successfully parsed with @yarnpkg/parsers");
  println!("  Entries found: {}", yarnpkg_result.len());
  println!();

  // Compare results
  compare_results(&berry_result, &yarnpkg_result, args.verbose)?;

  Ok(())
}

fn parse_with_berry(lockfile_path: &PathBuf) -> anyhow::Result<BTreeMap<String, Value>> {
  let content = fs::read_to_string(lockfile_path)?;
  match parse_lockfile(&content) {
    Ok((_rest, lockfile)) => Ok(parse_entries(lockfile)),
    Err(e) => anyhow::bail!("berry-core parser error: {e}"),
  }
}

fn parse_entries(lockfile: Lockfile) -> BTreeMap<String, Value> {
  let mut result = BTreeMap::new();

  for entry in lockfile.entries {
    let mut pkg_obj = Map::new();

    if let Some(version) = entry.package.version {
      pkg_obj.insert("version".to_owned(), Value::String(version.to_owned()));
    }

    if let Some(resolution) = entry.package.resolution {
      pkg_obj.insert("resolution".to_owned(), Value::String(resolution.to_owned()));
    }

    if !entry.package.dependencies.is_empty() {
      let mut deps = Map::new();
      for (dep_name, dep_range) in &entry.package.dependencies {
        let dep_ident_str = dep_name.scope.map_or_else(
          || dep_name.name.to_string(),
          |scope| format!("{}/{}", scope, dep_name.name),
        );
        deps.insert(dep_ident_str, Value::String(dep_range.range.raw.to_string()));
      }
      pkg_obj.insert("dependencies".to_owned(), Value::Object(deps));

      let mut deps_meta = Map::new();
      for (dep_name, dep_meta_info) in &entry.package.dependencies_meta {
        if let Some(meta) = dep_meta_info {
          let dep_ident_str = dep_name.scope.map_or_else(
            || dep_name.name.to_string(),
            |scope| format!("{}/{}", scope, dep_name.name),
          );
          let mut meta_obj = Map::new();
          if let Some(built) = meta.built {
            meta_obj.insert("built".to_string(), Value::String(built.to_string()));
          }
          if let Some(optional) = meta.optional {
            meta_obj.insert("optional".to_string(), Value::String(optional.to_string()));
          }
          deps_meta.insert(dep_ident_str, Value::Object(meta_obj));
        }
      }
      if !deps_meta.is_empty() {
        pkg_obj.insert("dependenciesMeta".to_owned(), Value::Object(deps_meta));
      }
    }

    if !entry.package.peer_dependencies.is_empty() {
      let mut peers = Map::new();
      for (peer_name, peer_meta) in &entry.package.peer_dependencies {
        let peer_ident_str = peer_name.scope.map_or_else(
          || peer_name.name.to_string(),
          |scope| format!("{}/{}", scope, peer_name.name),
        );
        peers.insert(peer_ident_str, Value::String(peer_meta.range.raw.to_string()));
      }
      pkg_obj.insert("peerDependencies".to_owned(), Value::Object(peers));

      let mut peers_meta = Map::new();
      for (peer_name, peer_meta_info) in &entry.package.peer_dependencies_meta {
        let peer_ident_str = peer_name.scope.map_or_else(
          || peer_name.name.to_string(),
          |scope| format!("{}/{}", scope, peer_name.name),
        );
        let mut meta_obj = Map::new();
        meta_obj.insert("optional".to_string(), Value::String(peer_meta_info.optional.to_string()));
        peers_meta.insert(peer_ident_str, Value::Object(meta_obj));
      }
      if !peers_meta.is_empty() {
        pkg_obj.insert("peerDependenciesMeta".to_owned(), Value::Object(peers_meta));
      }
    }

    if let Some(checksum) = entry.package.checksum {
      pkg_obj.insert("checksum".to_owned(), Value::String(checksum.to_owned()));
    }

    if let Some(conditions) = entry.package.conditions {
      pkg_obj.insert("conditions".to_owned(), Value::String(conditions.to_owned()));
    }

    if !entry.package.bin.is_empty() {
      let mut bin_obj = Map::new();
      for (bin_name, bin_path) in &entry.package.bin {
        bin_obj.insert((*bin_name).to_string(), Value::String((*bin_path).to_string()));
      }
      pkg_obj.insert("bin".to_owned(), Value::Object(bin_obj));
    }

    pkg_obj.insert(
      "languageName".to_owned(),
      Value::String(entry.package.language_name.to_string()),
    );

    let link_type_str = match entry.package.link_type {
      berry::package::LinkType::Hard => "hard",
      berry::package::LinkType::Soft => "soft",
    };
    pkg_obj.insert(
      "linkType".to_owned(),
      Value::String(link_type_str.to_owned()),
    );

    // Create a composite key from all descriptors that resolve to the same package.
    // This matches Yarn's format: when multiple range descriptors resolve to the same version,
    // they are grouped under a single comma-separated key.
    // Special rule: if any descriptor in the group has an explicit non-npm protocol,
    // then npm protocol prefixes are omitted from npm descriptors in that group.
    // Example: "@babel/highlight@npm:^7.10.4, @babel/highlight@npm:^7.18.6"
    // But: "eslint-config-custom@*, eslint-config-custom@workspace:..." (no npm: on the * descriptor)

    // Check if any descriptor has a non-npm protocol
    let has_non_npm_protocol = entry.descriptors.iter().any(|d| d.range.raw.contains(':'));

    let mut descriptor_keys: Vec<String> = entry
      .descriptors
      .iter()
      .map(|d| format_ident_and_range_with_context(d, has_non_npm_protocol))
      .collect();
    descriptor_keys.sort();
    let composite_key = descriptor_keys.join(", ");
    result.insert(composite_key, Value::Object(pkg_obj));
  }

  result
}

/// Format a descriptor as a key, with optional context about the descriptor group.
/// When a descriptor group contains both npm and non-npm protocols, Yarn omits the "npm:"
/// prefix from npm descriptors for compatibility reasons.
fn format_ident_and_range_with_context(
  descriptor: &berry::ident::Descriptor,
  has_non_npm_protocol: bool,
) -> String {
  let range_with_protocol = if descriptor.range.raw.contains(':')
    || has_non_npm_protocol
    || descriptor.range.raw == "latest"
  {
    descriptor.range.raw.to_string()
  } else {
    format!("npm:{}", descriptor.range.raw)
  };

  descriptor.ident.scope.map_or_else(
    || format!("{}@{}", descriptor.ident.name, range_with_protocol),
    |scope| format!("{}/{}@{}", scope, descriptor.ident.name, range_with_protocol),
  )
}

/// Format a single descriptor (legacy - used when context not available)
#[allow(dead_code)]
fn format_ident_and_range(descriptor: &berry::ident::Descriptor) -> String {
  format_ident_and_range_with_context(descriptor, false)
}

fn parse_with_yarnpkg(lockfile_path: &PathBuf) -> anyhow::Result<BTreeMap<String, Value>> {
  let script_path = std::env::current_exe()?
    .parent()
    .and_then(|p| p.parent())
    .and_then(|p| p.parent())
    .map(|p| p.join("crates/berry-compare/parse-with-yarnpkg.js"))
    .ok_or_else(|| anyhow::anyhow!("Could not determine script path"))?;

  let output = Command::new("node")
    .arg(&script_path)
    .arg(lockfile_path)
    .output()?;

  if !output.status.success() {
    let stderr = String::from_utf8_lossy(&output.stderr);
    anyhow::bail!("@yarnpkg/parsers error:\n{stderr}");
  }

  let stdout = String::from_utf8(output.stdout)?;
  let json_value: Value = serde_json::from_str(&stdout)?;

  // Extract entries as BTreeMap
  if let Value::Object(map) = json_value {
    let mut result = BTreeMap::new();
    for (key, value) in map {
      result.insert(key, value);
    }
    Ok(result)
  } else {
    anyhow::bail!("@yarnpkg/parsers returned non-object result");
  }
}

fn compare_results(
  berry: &BTreeMap<String, Value>,
  yarnpkg: &BTreeMap<String, Value>,
  verbose: bool,
) -> anyhow::Result<()> {
  let mut differences = Vec::new();

  // Check for entries in berry but not in yarnpkg
  for (key, berry_value) in berry {
    match yarnpkg.get(key) {
      Some(yarnpkg_value) => {
        if berry_value != yarnpkg_value {
          differences.push(ComparisonDiff {
            entry: key.clone(),
            kind: DiffKind::ValueMismatch,
            berry_value: Some(berry_value.clone()),
            yarnpkg_value: Some(yarnpkg_value.clone()),
          });
        }
      }
      None => {
        differences.push(ComparisonDiff {
          entry: key.clone(),
          kind: DiffKind::OnlyInBerry,
          berry_value: Some(berry_value.clone()),
          yarnpkg_value: None,
        });
      }
    }
  }

  // Check for entries in yarnpkg but not in berry
  for key in yarnpkg.keys() {
    if !berry.contains_key(key) && let Some(yarnpkg_value) = yarnpkg.get(key) {
      differences.push(ComparisonDiff {
        entry: key.clone(),
        kind: DiffKind::OnlyInYarnpkg,
        berry_value: None,
        yarnpkg_value: Some(yarnpkg_value.clone()),
      });
    }
  }

  // Print summary
  println!("═══════════════════════════════════════════════════════════════");
  println!("COMPARISON SUMMARY");
  println!("═══════════════════════════════════════════════════════════════");
  println!("Total entries in berry-core:    {}", berry.len());
  println!("Total entries in @yarnpkg:     {}", yarnpkg.len());
  println!("Differences found:              {}", differences.len());
  println!();

  if differences.is_empty() {
    println!("✓ All entries match perfectly!");
    return Ok(());
  }

  // Categorize differences
  let mut mismatches = Vec::new();
  let mut only_berry = Vec::new();
  let mut only_yarnpkg = Vec::new();

  for diff in &differences {
    match diff.kind {
      DiffKind::ValueMismatch => mismatches.push(diff),
      DiffKind::OnlyInBerry => only_berry.push(diff),
      DiffKind::OnlyInYarnpkg => only_yarnpkg.push(diff),
    }
  }

  // Print categorized differences
  if !mismatches.is_empty() {
    println!("VALUE MISMATCHES ({}):", mismatches.len());
    println!("─────────────────────────────────────────────────────────────");
    for diff in &mismatches {
      println!("Entry: {}", diff.entry);
      if verbose {
        if let (Some(berry_val), Some(yarnpkg_val)) = (&diff.berry_value, &diff.yarnpkg_value) {
          println!("  Berry:    {}", serde_json::to_string_pretty(berry_val)?);
          println!("  Yarnpkg:  {}", serde_json::to_string_pretty(yarnpkg_val)?);
        }
      } else {
        println!("  (use --verbose for details)");
      }
      println!();
    }
  }

  if !only_berry.is_empty() {
    println!("ONLY IN BERRY ({}):", only_berry.len());
    println!("─────────────────────────────────────────────────────────────");
    for diff in &only_berry {
      println!("  • {}", diff.entry);
    }
    println!();
  }

  if !only_yarnpkg.is_empty() {
    println!("ONLY IN @YARNPKG ({}):", only_yarnpkg.len());
    println!("─────────────────────────────────────────────────────────────");
    for diff in &only_yarnpkg {
      println!("  • {}", diff.entry);
    }
    println!();
  }

  // Exit with error if there are differences
  if !differences.is_empty() {
    std::process::exit(1);
  }

  Ok(())
}

#[derive(Debug)]
enum DiffKind {
  ValueMismatch,
  OnlyInBerry,
  OnlyInYarnpkg,
}

#[derive(Debug)]
struct ComparisonDiff {
  entry: String,
  kind: DiffKind,
  berry_value: Option<Value>,
  yarnpkg_value: Option<Value>,
}
