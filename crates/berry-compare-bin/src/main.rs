//! A tool to compare berry-core parsing with @yarnpkg/parsers
//!
//! Parses a lockfile with both parsers, compares the results,
//! and measures the performance of each.

use berry::lockfile::Entry;
use berry::parse::parse_lockfile;
use berry_test::load_fixture_from_path;
use clap::Parser;
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::Instant;

#[derive(Parser)]
#[command(name = "berry-compare")]
#[command(about = "Compare berry-core and @yarnpkg/parsers lockfile parsing")]
struct Args {
  /// Path to the lockfile to compare
  #[arg(short, long)]
  fixture: PathBuf,

  /// Number of iterations for timing
  #[arg(short, long, default_value = "5")]
  iterations: usize,
}

/// A simplified representation of a parsed package for comparison
#[derive(Debug, Deserialize)]
struct CompareEntry {
  descriptors: Vec<String>,
  version: Option<String>,
  resolution: Option<String>,
  language_name: String,
  link_type: String,
  checksum: Option<String>,
  conditions: Option<String>,
  dependencies: HashMap<String, String>,
  dependencies_meta: HashMap<String, DependencyMetaCompare>,
  peer_dependencies: HashMap<String, String>,
  peer_dependencies_meta: HashMap<String, PeerDependencyMetaCompare>,
  bin: HashMap<String, String>,
}

/// Simplified dependency meta for comparison
#[derive(Debug, Deserialize, PartialEq, Eq)]
struct DependencyMetaCompare {
  built: Option<bool>,
  optional: Option<bool>,
  unplugged: Option<bool>,
}

/// Simplified peer dependency meta for comparison
#[derive(Debug, Deserialize, PartialEq, Eq)]
struct PeerDependencyMetaCompare {
  optional: bool,
}

/// Output from the Node.js parser script
#[derive(Debug, Deserialize)]
struct JsParserOutput {
  entries: Vec<CompareEntry>,
}

/// Convert a berry Entry to `CompareEntry`
fn entry_to_compare(entry: &Entry<'_>) -> CompareEntry {
  let mut descriptors: Vec<String> = entry
    .descriptors
    .iter()
    .map(|d| {
      let ident = d.ident();
      let name = ident.scope().map_or_else(
        || ident.name().to_string(),
        |scope| format!("{}/{}", scope, ident.name()),
      );
      format!("{}@{}", name, d.range())
    })
    .collect();
  descriptors.sort();

  let dependencies: HashMap<String, String> = entry
    .package
    .dependencies
    .iter()
    .map(|(ident, desc)| {
      let name = ident.scope().map_or_else(
        || ident.name().to_string(),
        |scope| format!("{}/{}", scope, ident.name()),
      );
      (name, desc.range().to_string())
    })
    .collect();

  let peer_dependencies: HashMap<String, String> = entry
    .package
    .peer_dependencies
    .iter()
    .map(|(ident, desc)| {
      let name = ident.scope().map_or_else(
        || ident.name().to_string(),
        |scope| format!("{}/{}", scope, ident.name()),
      );
      (name, desc.range().to_string())
    })
    .collect();

  let bin: HashMap<String, String> = entry
    .package
    .bin
    .iter()
    .map(|(k, v)| ((*k).to_string(), (*v).to_string()))
    .collect();

  let dependencies_meta: HashMap<String, DependencyMetaCompare> = entry
    .package
    .dependencies_meta
    .iter()
    .filter_map(|(ident, meta)| {
      meta.as_ref().map(|m| {
        let name = ident.scope().map_or_else(
          || ident.name().to_string(),
          |scope| format!("{}/{}", scope, ident.name()),
        );
        (
          name,
          DependencyMetaCompare {
            built: m.built,
            optional: m.optional,
            unplugged: m.unplugged,
          },
        )
      })
    })
    .collect();

  let peer_dependencies_meta: HashMap<String, PeerDependencyMetaCompare> = entry
    .package
    .peer_dependencies_meta
    .iter()
    .map(|(ident, meta)| {
      let name = ident.scope().map_or_else(
        || ident.name().to_string(),
        |scope| format!("{}/{}", scope, ident.name()),
      );
      (
        name,
        PeerDependencyMetaCompare {
          optional: meta.optional,
        },
      )
    })
    .collect();

  CompareEntry {
    descriptors,
    version: entry.package.version.map(String::from),
    resolution: entry.package.resolution.map(String::from),
    language_name: entry.package.language_name.to_string(),
    link_type: match entry.package.link_type {
      berry::package::LinkType::Hard => "hard".to_string(),
      berry::package::LinkType::Soft => "soft".to_string(),
    },
    checksum: entry.package.checksum.map(String::from),
    conditions: entry.package.conditions.map(String::from),
    dependencies,
    dependencies_meta,
    peer_dependencies,
    peer_dependencies_meta,
    bin,
  }
}

/// Generate a Node.js script to parse the given lockfile
fn generate_js_script(fixture_path: &Path) -> String {
  let path_str = fixture_path.display();
  format!(
    r"const fs = require('fs');

let parseSyml;
try {{
    parseSyml = require('@yarnpkg/parsers').parseSyml;
}} catch (e) {{
    console.error('Error: @yarnpkg/parsers not installed. Run: npm install -g @yarnpkg/parsers');
    process.exit(1);
}}

const content = fs.readFileSync('{path_str}', 'utf8');
const parsed = parseSyml(content);

const entries = [];

for (const [key, value] of Object.entries(parsed)) {{
    if (key === '__metadata') continue;

    const descriptors = key.split(', ').map(d => d.trim()).sort();

    const dependencies = {{}};
    if (value.dependencies) {{
        for (const [name, range] of Object.entries(value.dependencies)) {{
            dependencies[name] = String(range);
        }}
    }}

    const peerDependencies = {{}};
    if (value.peerDependencies) {{
        for (const [name, range] of Object.entries(value.peerDependencies)) {{
            peerDependencies[name] = String(range);
        }}
    }}

    const bin = {{}};
    if (value.bin) {{
        for (const [name, path] of Object.entries(value.bin)) {{
            bin[name] = String(path);
        }}
    }}

    const dependenciesMeta = {{}};
    if (value.dependenciesMeta) {{
        for (const [name, meta] of Object.entries(value.dependenciesMeta)) {{
            // Handle string true/false or boolean values
            const toBoolOrNull = (v) => {{
                if (v === undefined) return null;
                if (typeof v === 'string') return v === 'true';
                return v;
            }};
            dependenciesMeta[name] = {{
                built: toBoolOrNull(meta.built),
                optional: toBoolOrNull(meta.optional),
                unplugged: toBoolOrNull(meta.unplugged),
            }};
        }}
    }}

    const peerDependenciesMeta = {{}};
    if (value.peerDependenciesMeta) {{
        for (const [name, meta] of Object.entries(value.peerDependenciesMeta)) {{
            // Handle string true/false or boolean values
            let optional = meta.optional;
            if (typeof optional === 'string') {{
                optional = optional === 'true';
            }}
            peerDependenciesMeta[name] = {{
                optional: optional || false,
            }};
        }}
    }}

    entries.push({{
        descriptors,
        version: value.version ? String(value.version) : null,
        resolution: value.resolution ? String(value.resolution) : null,
        language_name: value.languageName || 'unknown',
        link_type: value.linkType || 'hard',
        checksum: value.checksum ? String(value.checksum) : null,
        conditions: value.conditions ? String(value.conditions) : null,
        dependencies,
        dependencies_meta: dependenciesMeta,
        peer_dependencies: peerDependencies,
        peer_dependencies_meta: peerDependenciesMeta,
        bin,
    }});
}}

console.log(JSON.stringify({{ entries }}));
"
  )
}

fn get_npm_global_prefix() -> Option<String> {
  let output = Command::new("npm")
    .args(["config", "get", "prefix"])
    .output()
    .ok()?;
  if output.status.success() {
    Some(String::from_utf8_lossy(&output.stdout).trim().to_string())
  } else {
    None
  }
}

fn run_js_parser(fixture_path: &Path) -> Result<JsParserOutput, String> {
  let script = generate_js_script(fixture_path);
  let script_path = std::env::temp_dir().join("berry-compare-parser.js");
  fs::write(&script_path, &script).map_err(|e| format!("Failed to write temp script: {e}"))?;

  let mut cmd = Command::new("node");
  cmd
    .arg(&script_path)
    .stdout(Stdio::piped())
    .stderr(Stdio::piped());

  // Set NODE_PATH to include global node_modules
  if let Some(prefix) = get_npm_global_prefix() {
    let node_path = format!("{prefix}/lib/node_modules");
    cmd.env("NODE_PATH", node_path);
  }

  let child = cmd
    .spawn()
    .map_err(|e| format!("Failed to spawn node: {e}"))?;

  let output = child
    .wait_with_output()
    .map_err(|e| format!("Failed to wait on node: {e}"))?;

  if !output.status.success() {
    let stderr = String::from_utf8_lossy(&output.stderr);
    return Err(format!("Node script failed: {stderr}"));
  }

  let stdout = String::from_utf8_lossy(&output.stdout);
  serde_json::from_str(&stdout).map_err(|e| format!("Failed to parse JSON: {e}\nOutput: {stdout}"))
}

fn time_rust_parser(contents: &str, iterations: usize) -> (Vec<Entry<'_>>, f64) {
  let mut total_time = 0.0;
  let mut entries = Vec::new();

  for _ in 0..iterations {
    let start = Instant::now();
    let result = parse_lockfile(contents).expect("Failed to parse lockfile");
    total_time += start.elapsed().as_secs_f64() * 1000.0;
    entries = result.1.entries;
  }

  let avg_time = total_time / iterations as f64;
  (entries, avg_time)
}

fn time_js_parser(fixture_path: &Path, iterations: usize) -> (JsParserOutput, f64) {
  let mut total_time = 0.0;
  let mut output = None;

  for _ in 0..iterations {
    let start = Instant::now();
    let result = run_js_parser(fixture_path).expect("Failed to run JS parser");
    total_time += start.elapsed().as_secs_f64() * 1000.0;
    output = Some(result);
  }

  let avg_time = total_time / iterations as f64;
  (output.unwrap(), avg_time)
}

#[derive(Debug)]
struct Difference {
  resolution: String,
  field: String,
  rust_value: String,
  js_value: String,
}

#[allow(clippy::too_many_lines)]
fn compare_entries(rust_entries: &[CompareEntry], js_entries: &[CompareEntry]) -> Vec<Difference> {
  let mut differences = Vec::new();

  // Build lookup by resolution
  let rust_by_res: HashMap<&str, &CompareEntry> = rust_entries
    .iter()
    .filter_map(|e| e.resolution.as_ref().map(|r| (r.as_str(), e)))
    .collect();

  let js_by_res: HashMap<&str, &CompareEntry> = js_entries
    .iter()
    .filter_map(|e| e.resolution.as_ref().map(|r| (r.as_str(), e)))
    .collect();

  // Check rust entries against JS
  for (resolution, rust_entry) in &rust_by_res {
    if let Some(js_entry) = js_by_res.get(resolution) {
      // Compare descriptors
      if rust_entry.descriptors != js_entry.descriptors {
        differences.push(Difference {
          resolution: (*resolution).to_string(),
          field: "descriptors".to_string(),
          rust_value: format!("{:?}", rust_entry.descriptors),
          js_value: format!("{:?}", js_entry.descriptors),
        });
      }

      // Compare fields
      if rust_entry.version != js_entry.version {
        differences.push(Difference {
          resolution: (*resolution).to_string(),
          field: "version".to_string(),
          rust_value: format!("{:?}", rust_entry.version),
          js_value: format!("{:?}", js_entry.version),
        });
      }

      if rust_entry.language_name != js_entry.language_name {
        differences.push(Difference {
          resolution: (*resolution).to_string(),
          field: "languageName".to_string(),
          rust_value: rust_entry.language_name.clone(),
          js_value: js_entry.language_name.clone(),
        });
      }

      if rust_entry.link_type != js_entry.link_type {
        differences.push(Difference {
          resolution: (*resolution).to_string(),
          field: "linkType".to_string(),
          rust_value: rust_entry.link_type.clone(),
          js_value: js_entry.link_type.clone(),
        });
      }

      if rust_entry.checksum != js_entry.checksum {
        differences.push(Difference {
          resolution: (*resolution).to_string(),
          field: "checksum".to_string(),
          rust_value: format!("{:?}", rust_entry.checksum),
          js_value: format!("{:?}", js_entry.checksum),
        });
      }

      // Compare dependencies
      for (dep_name, rust_range) in &rust_entry.dependencies {
        if let Some(js_range) = js_entry.dependencies.get(dep_name) {
          if rust_range != js_range {
            differences.push(Difference {
              resolution: (*resolution).to_string(),
              field: format!("dependencies[{dep_name}]"),
              rust_value: rust_range.clone(),
              js_value: js_range.clone(),
            });
          }
        } else {
          differences.push(Difference {
            resolution: (*resolution).to_string(),
            field: format!("dependencies[{dep_name}]"),
            rust_value: rust_range.clone(),
            js_value: "(missing)".to_string(),
          });
        }
      }

      for (dep_name, js_range) in &js_entry.dependencies {
        if !rust_entry.dependencies.contains_key(dep_name) {
          differences.push(Difference {
            resolution: (*resolution).to_string(),
            field: format!("dependencies[{dep_name}]"),
            rust_value: "(missing)".to_string(),
            js_value: js_range.clone(),
          });
        }
      }

      // Compare peer dependencies
      for (dep_name, rust_range) in &rust_entry.peer_dependencies {
        if let Some(js_range) = js_entry.peer_dependencies.get(dep_name) {
          if rust_range != js_range {
            differences.push(Difference {
              resolution: (*resolution).to_string(),
              field: format!("peerDependencies[{dep_name}]"),
              rust_value: rust_range.clone(),
              js_value: js_range.clone(),
            });
          }
        } else {
          differences.push(Difference {
            resolution: (*resolution).to_string(),
            field: format!("peerDependencies[{dep_name}]"),
            rust_value: rust_range.clone(),
            js_value: "(missing)".to_string(),
          });
        }
      }

      for (dep_name, js_range) in &js_entry.peer_dependencies {
        if !rust_entry.peer_dependencies.contains_key(dep_name) {
          differences.push(Difference {
            resolution: (*resolution).to_string(),
            field: format!("peerDependencies[{dep_name}]"),
            rust_value: "(missing)".to_string(),
            js_value: js_range.clone(),
          });
        }
      }

      // Compare bin entries
      for (bin_name, rust_path) in &rust_entry.bin {
        if let Some(js_path) = js_entry.bin.get(bin_name) {
          if rust_path != js_path {
            differences.push(Difference {
              resolution: (*resolution).to_string(),
              field: format!("bin[{bin_name}]"),
              rust_value: rust_path.clone(),
              js_value: js_path.clone(),
            });
          }
        } else {
          differences.push(Difference {
            resolution: (*resolution).to_string(),
            field: format!("bin[{bin_name}]"),
            rust_value: rust_path.clone(),
            js_value: "(missing)".to_string(),
          });
        }
      }

      for (bin_name, js_path) in &js_entry.bin {
        if !rust_entry.bin.contains_key(bin_name) {
          differences.push(Difference {
            resolution: (*resolution).to_string(),
            field: format!("bin[{bin_name}]"),
            rust_value: "(missing)".to_string(),
            js_value: js_path.clone(),
          });
        }
      }

      // Compare conditions
      if rust_entry.conditions != js_entry.conditions {
        differences.push(Difference {
          resolution: (*resolution).to_string(),
          field: "conditions".to_string(),
          rust_value: format!("{:?}", rust_entry.conditions),
          js_value: format!("{:?}", js_entry.conditions),
        });
      }

      // Compare dependencies_meta
      for (dep_name, rust_meta) in &rust_entry.dependencies_meta {
        if let Some(js_meta) = js_entry.dependencies_meta.get(dep_name) {
          if rust_meta != js_meta {
            differences.push(Difference {
              resolution: (*resolution).to_string(),
              field: format!("dependenciesMeta[{dep_name}]"),
              rust_value: format!("{rust_meta:?}"),
              js_value: format!("{js_meta:?}"),
            });
          }
        } else {
          differences.push(Difference {
            resolution: (*resolution).to_string(),
            field: format!("dependenciesMeta[{dep_name}]"),
            rust_value: format!("{rust_meta:?}"),
            js_value: "(missing)".to_string(),
          });
        }
      }

      for (dep_name, js_meta) in &js_entry.dependencies_meta {
        if !rust_entry.dependencies_meta.contains_key(dep_name) {
          differences.push(Difference {
            resolution: (*resolution).to_string(),
            field: format!("dependenciesMeta[{dep_name}]"),
            rust_value: "(missing)".to_string(),
            js_value: format!("{js_meta:?}"),
          });
        }
      }

      // Compare peer_dependencies_meta
      for (dep_name, rust_meta) in &rust_entry.peer_dependencies_meta {
        if let Some(js_meta) = js_entry.peer_dependencies_meta.get(dep_name) {
          if rust_meta != js_meta {
            differences.push(Difference {
              resolution: (*resolution).to_string(),
              field: format!("peerDependenciesMeta[{dep_name}]"),
              rust_value: format!("{rust_meta:?}"),
              js_value: format!("{js_meta:?}"),
            });
          }
        } else {
          differences.push(Difference {
            resolution: (*resolution).to_string(),
            field: format!("peerDependenciesMeta[{dep_name}]"),
            rust_value: format!("{rust_meta:?}"),
            js_value: "(missing)".to_string(),
          });
        }
      }

      for (dep_name, js_meta) in &js_entry.peer_dependencies_meta {
        if !rust_entry.peer_dependencies_meta.contains_key(dep_name) {
          differences.push(Difference {
            resolution: (*resolution).to_string(),
            field: format!("peerDependenciesMeta[{dep_name}]"),
            rust_value: "(missing)".to_string(),
            js_value: format!("{js_meta:?}"),
          });
        }
      }
    } else {
      differences.push(Difference {
        resolution: (*resolution).to_string(),
        field: "(entry)".to_string(),
        rust_value: "present".to_string(),
        js_value: "(missing)".to_string(),
      });
    }
  }

  // Check for entries in JS but not in Rust
  for resolution in js_by_res.keys() {
    if !rust_by_res.contains_key(resolution) {
      differences.push(Difference {
        resolution: (*resolution).to_string(),
        field: "(entry)".to_string(),
        rust_value: "(missing)".to_string(),
        js_value: "present".to_string(),
      });
    }
  }

  differences
}

fn main() {
  let args = Args::parse();

  let fixture_path = args.fixture.canonicalize().unwrap_or_else(|e| {
    eprintln!(
      "Error: Cannot find fixture file {}: {}",
      args.fixture.display(),
      e
    );
    std::process::exit(1);
  });

  let contents = load_fixture_from_path(&fixture_path);
  let file_size = contents.len();

  println!("Comparing parsers for: {}", fixture_path.display());
  println!("File size: {file_size} bytes");
  println!("Iterations: {}", args.iterations);
  println!();

  // Time Rust parser
  print!("Running Rust parser... ");
  std::io::stdout().flush().unwrap();
  let (rust_entries, rust_time) = time_rust_parser(&contents, args.iterations);
  println!("done");

  // Time JS parser
  print!("Running JS parser... ");
  std::io::stdout().flush().unwrap();
  let (js_output, js_time) = time_js_parser(&fixture_path, args.iterations);
  println!("done");

  // Convert rust entries for comparison
  let rust_compare: Vec<CompareEntry> = rust_entries.iter().map(entry_to_compare).collect();

  println!();
  println!("=== Performance ===");
  println!("Rust:  {rust_time:.3} ms avg");
  println!("JS:    {js_time:.3} ms avg");
  let speedup = js_time / rust_time;
  println!("Speedup: {speedup:.2}x");

  println!();
  println!("=== Entry Counts ===");
  println!("Rust:  {} entries", rust_compare.len());
  println!("JS:    {} entries", js_output.entries.len());

  // Compare entries
  let differences = compare_entries(&rust_compare, &js_output.entries);

  println!();
  println!("=== Differences ===");
  if differences.is_empty() {
    println!("No differences found");
  } else {
    println!("{} differences found:", differences.len());
    for (i, diff) in differences.iter().enumerate().take(20) {
      println!(
        "  {}. [{}] {}: rust={}, js={}",
        i + 1,
        diff.resolution,
        diff.field,
        diff.rust_value,
        diff.js_value
      );
    }
    if differences.len() > 20 {
      println!("  ... and {} more", differences.len() - 20);
    }
  }
}
