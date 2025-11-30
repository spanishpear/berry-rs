#![deny(clippy::all)]
//! End-to-end integration tests for the Berry lockfile parser
//!
//! This crate provides comprehensive integration tests using real Yarn lockfile
//! fixtures to validate the parsing functionality across various lockfile formats.

use std::path::Path;

/// Load a fixture file from the fixtures directory
pub fn load_fixture(filename: &str) -> String {
  let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .unwrap()
    .parent()
    .unwrap()
    .join("fixtures")
    .join(filename);

  std::fs::read_to_string(&fixture_path).unwrap_or_else(|e| {
    panic!(
      "Failed to read fixture file {}: {}",
      fixture_path.display(),
      e
    )
  })
}

/// Load a fixture file from a path
pub fn load_fixture_from_path(fixture_path: &Path) -> String {
  std::fs::read_to_string(fixture_path).unwrap_or_else(|e| {
    panic!(
      "Failed to read fixture file {}: {}",
      fixture_path.display(),
      e
    )
  })
}

#[cfg(test)]
mod tests {
  use super::*;
  use berry::parse::parse_lockfile;
  use rstest::rstest;
  use std::collections::HashMap;
  use std::path::PathBuf;

  #[rstest]
  fn test_parse_lockfile_fixtures(#[files("../../fixtures/*.lock")] fixture_path: PathBuf) {
    let contents = load_fixture_from_path(&fixture_path);

    // Get the filename for better error messages
    let filename = fixture_path
      .file_name()
      .and_then(|name| name.to_str())
      .unwrap_or("unknown");

    // Verify we can load the fixture
    assert!(!contents.is_empty(), "Fixture should not be empty");

    println!("Testing fixture: {filename}");

    let result = parse_lockfile(&contents);
    assert!(
      result.is_ok(),
      "Should successfully parse lockfile: {filename}"
    );

    let (remaining, lockfile) = result.unwrap();

    // Critical validation: ensure the entire file was parsed
    if !remaining.is_empty() {
      println!(
        "WARNING: {} bytes remaining unparsed in {}",
        remaining.len(),
        filename
      );
      println!(
        "First 200 chars of unparsed content: '{}'",
        &remaining[..remaining.len().min(200)]
      );

      // Allow only whitespace and newlines to remain unparsed
      let trimmed_remaining = remaining.trim();
      assert!(
        trimmed_remaining.is_empty(),
        "Too much content remaining unparsed ({} bytes) in {}: '{}'",
        remaining.len(),
        filename,
        &trimmed_remaining[..trimmed_remaining.len().min(200)]
      );
    }

    // Verify we parsed at least some packages
    assert!(
      !lockfile.entries.is_empty(),
      "Should parse at least one package from {filename}"
    );

    println!(
      "Successfully parsed {} packages from {filename}",
      lockfile.entries.len()
    );
  }

  // TODO: get this test passing, then remove it
  #[test]
  fn test_specific_minimal_berry_lockfile() {
    let contents = load_fixture("minimal-berry.lock");

    // Specific test for the minimal berry lockfile
    assert!(!contents.is_empty(), "Fixture should not be empty");
    assert!(
      contents.contains("__metadata"),
      "Should contain metadata section"
    );
    assert!(
      contents.contains("workspace:"),
      "Should contain workspace packages"
    );

    let result = parse_lockfile(&contents);
    assert!(
      result.is_ok(),
      "Should successfully parse minimal berry lockfile"
    );

    let lockfile = result.unwrap().1;
    dbg!(&lockfile);
    assert_eq!(lockfile.metadata.version, "6");
    assert_eq!(lockfile.entries.len(), 5);
  }

  #[test]
  fn test_fixture_discovery() {
    // Verify that we can find fixture files
    let fixtures_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
      .parent()
      .unwrap()
      .parent()
      .unwrap()
      .join("fixtures");

    assert!(fixtures_dir.exists(), "Fixtures directory should exist");

    let lock_files: Vec<_> = std::fs::read_dir(&fixtures_dir)
      .unwrap()
      .filter_map(|entry| {
        let entry = entry.ok()?;
        let path = entry.path();
        if path.extension()? == "lock" {
          Some(path)
        } else {
          None
        }
      })
      .collect();

    assert!(
      !lock_files.is_empty(),
      "Should find at least one .lock file"
    );
    println!("Found {} .lock files", lock_files.len());

    for lock_file in &lock_files {
      println!("  - {}", lock_file.file_name().unwrap().to_str().unwrap());
    }
  }

  /// Helper to construct full package name from Ident (scope/name or just name)
  /// Strips any surrounding quotes for normalized comparison
  fn full_ident_name(ident: &berry::ident::Ident) -> String {
    let name = ident.name().trim_matches('"');
    ident.scope().map_or_else(
      || name.to_string(),
      |scope| {
        let scope = scope.trim_matches('"');
        format!("{scope}/{name}")
      },
    )
  }

  /// Comparison test that validates berry's output against yarn-lock-parser as a reference.
  /// This ensures no data is silently skipped during parsing.
  #[rstest]
  fn test_compare_with_yarn_lock_parser(#[files("../../fixtures/*.lock")] fixture_path: PathBuf) {
    let contents = load_fixture_from_path(&fixture_path);
    let filename = fixture_path
      .file_name()
      .and_then(|name| name.to_str())
      .unwrap_or("unknown");

    println!("\n=== Comparing parsers for: {filename} ===");

    // Parse with berry
    let berry_result = parse_lockfile(&contents);
    assert!(berry_result.is_ok(), "Berry failed to parse {filename}");
    let (_, berry_lockfile) = berry_result.unwrap();

    // Parse with yarn-lock-parser (may fail on some valid lockfiles that berry handles)
    let yarn_result = yarn_lock_parser::parse_str(&contents);
    let yarn_lockfile = match yarn_result {
      Ok(lockfile) => lockfile,
      Err(e) => {
        println!("  yarn-lock-parser cannot parse this fixture: {e:?}");
        println!(
          "  Skipping comparison (berry parsed {} entries successfully)",
          berry_lockfile.entries.len()
        );
        // Still verify berry parsed something reasonable
        assert!(
          !berry_lockfile.entries.is_empty(),
          "Berry should parse at least one entry from {filename}"
        );
        return;
      }
    };

    // Compare entry counts
    println!(
      "  Entry counts - berry: {}, yarn-lock-parser: {}",
      berry_lockfile.entries.len(),
      yarn_lockfile.entries.len()
    );
    assert_eq!(
      berry_lockfile.entries.len(),
      yarn_lockfile.entries.len(),
      "Entry count mismatch in {filename}: berry={}, yarn-lock-parser={}",
      berry_lockfile.entries.len(),
      yarn_lockfile.entries.len()
    );

    // Build a lookup map for yarn-lock-parser entries by resolution string
    // yarn-lock-parser uses "resolved" field which corresponds to berry's "resolution"
    let yarn_entries_by_resolved: HashMap<&str, &yarn_lock_parser::Entry> = yarn_lockfile
      .entries
      .iter()
      .map(|e| (e.resolved, e))
      .collect();

    // Compare each entry
    for berry_entry in &berry_lockfile.entries {
      let resolution = berry_entry.package.resolution.unwrap_or("");

      // Find matching yarn-lock-parser entry
      let yarn_entry = yarn_entries_by_resolved.get(resolution);

      if let Some(yarn_entry) = yarn_entry {
        // Compare version
        let berry_version = berry_entry.package.version.unwrap_or("");
        assert_eq!(
          berry_version, yarn_entry.version,
          "Version mismatch for {resolution} in {filename}: berry='{}', yarn-lock-parser='{}'",
          berry_version, yarn_entry.version
        );

        // Compare checksum/integrity
        let berry_checksum = berry_entry.package.checksum.unwrap_or("");
        // yarn-lock-parser may have empty integrity for some packages
        if !yarn_entry.integrity.is_empty() || !berry_checksum.is_empty() {
          assert_eq!(
            berry_checksum, yarn_entry.integrity,
            "Checksum mismatch for {resolution} in {filename}: berry='{}', yarn-lock-parser='{}'",
            berry_checksum, yarn_entry.integrity
          );
        }

        // Compare dependency counts
        let berry_dep_count = berry_entry.package.dependencies.len();
        let yarn_dep_count = yarn_entry.dependencies.len();
        assert_eq!(
          berry_dep_count, yarn_dep_count,
          "Dependency count mismatch for {resolution} in {filename}: berry={berry_dep_count}, yarn-lock-parser={yarn_dep_count}"
        );

        // Compare each dependency name and range
        for (yarn_dep_name, yarn_dep_range) in &yarn_entry.dependencies {
          // Find matching berry dependency by full ident name (including scope)
          let berry_dep = berry_entry
            .package
            .dependencies
            .values()
            .find(|d| full_ident_name(d.ident()) == *yarn_dep_name);

          assert!(
            berry_dep.is_some(),
            "Missing dependency '{yarn_dep_name}' for {resolution} in {filename}"
          );

          let berry_dep = berry_dep.unwrap();
          // Normalize ranges by stripping quotes for comparison
          let berry_range = berry_dep.range().trim_matches('"');
          let yarn_range = yarn_dep_range.trim_matches('"');
          assert_eq!(
            berry_range, yarn_range,
            "Dependency range mismatch for '{yarn_dep_name}' in {resolution} ({filename}): berry='{berry_range}', yarn-lock-parser='{yarn_range}'"
          );
        }

        // Compare descriptor counts - ensures no descriptors are dropped
        let berry_desc_count = berry_entry.descriptors.len();
        let yarn_desc_count = yarn_entry.descriptors.len();
        assert_eq!(
          berry_desc_count,
          yarn_desc_count,
          "Descriptor count mismatch for {resolution} in {filename}: berry={}, yarn-lock-parser={}. \
           Berry descriptors: {:?}, yarn-lock-parser descriptors: {:?}",
          berry_desc_count,
          yarn_desc_count,
          berry_entry
            .descriptors
            .iter()
            .map(|d| format!("{}@{}", full_ident_name(d.ident()), d.range()))
            .collect::<Vec<_>>(),
          yarn_entry
            .descriptors
            .iter()
            .map(|(n, r)| format!("{n}@{r}"))
            .collect::<Vec<_>>()
        );

        // Verify each descriptor name exists in berry (the two parsers store ranges differently:
        // yarn-lock-parser stores resolved version, berry stores original requested range)
        for (yarn_desc_name, _) in &yarn_entry.descriptors {
          let berry_has_name = berry_entry
            .descriptors
            .iter()
            .any(|d| full_ident_name(d.ident()) == *yarn_desc_name);

          assert!(
            berry_has_name,
            "Missing descriptor for package '{yarn_desc_name}' in {resolution} ({filename}). Berry has: {:?}",
            berry_entry
              .descriptors
              .iter()
              .map(|d| full_ident_name(d.ident()))
              .collect::<Vec<_>>()
          );
        }
      } else {
        // Some entries may have empty resolution (workspace packages, etc.)
        // Verify we at least have descriptors
        assert!(
          !berry_entry.descriptors.is_empty(),
          "Entry with no resolution should have descriptors in {filename}"
        );
      }
    }

    println!(
      "  All {} entries validated successfully!",
      berry_lockfile.entries.len()
    );
  }

  /// Test that validates all expected fields are being parsed (completeness check).
  /// This ensures the parser doesn't silently skip any property types.
  #[rstest]
  fn test_field_completeness(#[files("../../fixtures/*.lock")] fixture_path: PathBuf) {
    let contents = load_fixture_from_path(&fixture_path);
    let filename = fixture_path
      .file_name()
      .and_then(|name| name.to_str())
      .unwrap_or("unknown");

    let result = parse_lockfile(&contents);
    assert!(result.is_ok(), "Failed to parse {filename}");
    let (_, lockfile) = result.unwrap();

    // Track which field types we've seen
    let mut seen_version = false;
    let mut seen_resolution = false;
    let mut seen_checksum = false;
    let mut seen_dependencies = false;
    let mut seen_peer_dependencies = false;
    let mut seen_dependencies_meta = false;
    let mut seen_peer_dependencies_meta = false;
    let mut seen_bin = false;
    let mut seen_conditions = false;
    let mut seen_hard_link = false;
    let mut seen_soft_link = false;

    for entry in &lockfile.entries {
      if entry.package.version.is_some() {
        seen_version = true;
      }
      if entry.package.resolution.is_some() {
        seen_resolution = true;
      }
      if entry.package.checksum.is_some() {
        seen_checksum = true;
      }
      if !entry.package.dependencies.is_empty() {
        seen_dependencies = true;
      }
      if !entry.package.peer_dependencies.is_empty() {
        seen_peer_dependencies = true;
      }
      if !entry.package.dependencies_meta.is_empty() {
        seen_dependencies_meta = true;
      }
      if !entry.package.peer_dependencies_meta.is_empty() {
        seen_peer_dependencies_meta = true;
      }
      if !entry.package.bin.is_empty() {
        seen_bin = true;
      }
      if entry.package.conditions.is_some() {
        seen_conditions = true;
      }
      if entry.package.link_type == berry::package::LinkType::Hard {
        seen_hard_link = true;
      }
      if entry.package.link_type == berry::package::LinkType::Soft {
        seen_soft_link = true;
      }
    }

    println!("\n=== Field completeness for {filename} ===");
    println!("  version: {seen_version}");
    println!("  resolution: {seen_resolution}");
    println!("  checksum: {seen_checksum}");
    println!("  dependencies: {seen_dependencies}");
    println!("  peerDependencies: {seen_peer_dependencies}");
    println!("  dependenciesMeta: {seen_dependencies_meta}");
    println!("  peerDependenciesMeta: {seen_peer_dependencies_meta}");
    println!("  bin: {seen_bin}");
    println!("  conditions: {seen_conditions}");
    println!("  linkType hard: {seen_hard_link}");
    println!("  linkType soft: {seen_soft_link}");

    // Every lockfile should have at least these basic fields
    assert!(seen_version, "No version field found in {filename}");
    assert!(seen_resolution, "No resolution field found in {filename}");
  }

  /// Test an arbitrary yarn.lock file passed via `BERRY_TEST_FIXTURE` environment variable.
  ///
  /// Usage:
  /// ```bash
  /// BERRY_TEST_FIXTURE=/path/to/yarn.lock cargo test -p berry-test test_arbitrary_fixture -- --nocapture
  /// ```
  ///
  /// Or with nextest:
  /// ```bash
  /// BERRY_TEST_FIXTURE=/path/to/yarn.lock cargo nextest run -p berry-test test_arbitrary_fixture
  /// ```
  #[test]
  fn test_arbitrary_fixture() {
    let Ok(fixture_path) = std::env::var("BERRY_TEST_FIXTURE") else {
      println!("BERRY_TEST_FIXTURE not set, skipping arbitrary fixture test");
      println!(
        "Usage: BERRY_TEST_FIXTURE=/path/to/yarn.lock cargo test -p berry-test test_arbitrary_fixture -- --nocapture"
      );
      return;
    };

    let path = std::path::Path::new(&fixture_path);
    assert!(path.exists(), "Fixture file does not exist: {fixture_path}");

    let contents =
      std::fs::read_to_string(path).unwrap_or_else(|e| panic!("Failed to read fixture: {e}"));

    println!("\n=== Testing arbitrary fixture: {fixture_path} ===");
    println!("  File size: {} bytes", contents.len());

    // Parse with berry
    let berry_result = parse_lockfile(&contents);
    assert!(
      berry_result.is_ok(),
      "Berry failed to parse: {:?}",
      berry_result.err()
    );
    let (remaining, berry_lockfile) = berry_result.unwrap();

    // Validate entire file was consumed
    let remaining_trimmed = remaining.trim();
    assert!(
      remaining_trimmed.is_empty(),
      "Berry left {} bytes unparsed: '{}'",
      remaining.len(),
      &remaining_trimmed[..remaining_trimmed.len().min(200)]
    );

    println!("  Berry parsed {} entries", berry_lockfile.entries.len());
    println!("  Metadata version: {}", berry_lockfile.metadata.version);

    // Try parsing with yarn-lock-parser for comparison
    match yarn_lock_parser::parse_str(&contents) {
      Ok(yarn_lockfile) => {
        println!(
          "  yarn-lock-parser parsed {} entries",
          yarn_lockfile.entries.len()
        );

        // Compare entry counts
        assert_eq!(
          berry_lockfile.entries.len(),
          yarn_lockfile.entries.len(),
          "Entry count mismatch: berry={}, yarn-lock-parser={}",
          berry_lockfile.entries.len(),
          yarn_lockfile.entries.len()
        );

        // Build lookup and compare
        let yarn_by_resolved: HashMap<&str, &yarn_lock_parser::Entry> = yarn_lockfile
          .entries
          .iter()
          .map(|e| (e.resolved, e))
          .collect();

        let mut mismatches = Vec::new();
        for berry_entry in &berry_lockfile.entries {
          let resolution = berry_entry.package.resolution.unwrap_or("");
          if let Some(yarn_entry) = yarn_by_resolved.get(resolution) {
            // Check version
            let berry_v = berry_entry.package.version.unwrap_or("");
            if berry_v != yarn_entry.version {
              mismatches.push(format!(
                "Version mismatch for {resolution}: berry='{berry_v}', yarn='{}'",
                yarn_entry.version
              ));
            }
            // Check dependency count
            if berry_entry.package.dependencies.len() != yarn_entry.dependencies.len() {
              mismatches.push(format!(
                "Dep count mismatch for {resolution}: berry={}, yarn={}",
                berry_entry.package.dependencies.len(),
                yarn_entry.dependencies.len()
              ));
            }
            // Check descriptor count
            if berry_entry.descriptors.len() != yarn_entry.descriptors.len() {
              mismatches.push(format!(
                "Descriptor count mismatch for {resolution}: berry={}, yarn={}",
                berry_entry.descriptors.len(),
                yarn_entry.descriptors.len()
              ));
            }
          }
        }

        if mismatches.is_empty() {
          println!("  All entries validated successfully!");
        } else {
          println!("  Found {} mismatches:", mismatches.len());
          for m in &mismatches[..mismatches.len().min(10)] {
            println!("    - {m}");
          }
          if mismatches.len() > 10 {
            println!("    ... and {} more", mismatches.len() - 10);
          }
          panic!("Validation failed with {} mismatches", mismatches.len());
        }
      }
      Err(e) => {
        println!("  yarn-lock-parser cannot parse this file: {e:?}");
        println!("  (This may be expected for complex lockfiles with wrapped lines)");
        println!("  Berry-only validation passed!");
      }
    }
  }
}
