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

  /// Helper to construct full package name from Ident (scope/name or just name).
  /// Strips any surrounding quotes for normalized comparison.
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

  /// Basic parsing test: ensures each fixture parses completely without error.
  /// Entry count validation is handled by `test_compare_with_yarn_lock_parser`.
  #[rstest]
  fn test_parse_lockfile_fixtures(#[files("../../fixtures/*.lock")] fixture_path: PathBuf) {
    let contents = load_fixture_from_path(&fixture_path);
    let filename = fixture_path
      .file_name()
      .and_then(|name| name.to_str())
      .unwrap_or("unknown");

    assert!(!contents.is_empty(), "Fixture should not be empty");

    let result = parse_lockfile(&contents);
    assert!(
      result.is_ok(),
      "Failed to parse {filename}: {:?}",
      result.err()
    );

    let (remaining, lockfile) = result.unwrap();

    // Validate entire file was consumed (only whitespace allowed remaining)
    let trimmed_remaining = remaining.trim();
    assert!(
      trimmed_remaining.is_empty(),
      "Unparsed content ({} bytes) in {filename}: '{}'",
      remaining.len(),
      &trimmed_remaining[..trimmed_remaining.len().min(200)]
    );

    assert!(
      !lockfile.entries.is_empty(),
      "Should parse at least one package from {filename}"
    );

    println!("✓ {filename}: {} packages parsed", lockfile.entries.len());
  }

  /// Comparison test that validates berry's output against yarn-lock-parser as a reference.
  #[rstest]
  fn test_compare_with_yarn_lock_parser(#[files("../../fixtures/*.lock")] fixture_path: PathBuf) {
    let contents = load_fixture_from_path(&fixture_path);
    let filename = fixture_path
      .file_name()
      .and_then(|name| name.to_str())
      .unwrap_or("unknown");

    let berry_result = parse_lockfile(&contents);
    assert!(berry_result.is_ok(), "Berry failed to parse {filename}");
    let (_, berry_lockfile) = berry_result.unwrap();

    // Parse with yarn-lock-parser (may fail on some valid lockfiles that berry handles)
    let yarn_lockfile = match yarn_lock_parser::parse_str(&contents) {
      Ok(lockfile) => lockfile,
      Err(e) => {
        println!(
          "⚠ {filename}: yarn-lock-parser failed ({e:?}), berry parsed {} entries",
          berry_lockfile.entries.len()
        );
        assert!(
          !berry_lockfile.entries.is_empty(),
          "Berry should parse at least one entry from {filename}"
        );
        return;
      }
    };

    // Compare entry counts
    assert_eq!(
      berry_lockfile.entries.len(),
      yarn_lockfile.entries.len(),
      "Entry count mismatch in {filename}: berry={}, yarn-lock-parser={}",
      berry_lockfile.entries.len(),
      yarn_lockfile.entries.len()
    );

    // Build lookup map by resolution
    let yarn_entries_by_resolved: HashMap<&str, &yarn_lock_parser::Entry> = yarn_lockfile
      .entries
      .iter()
      .map(|e| (e.resolved, e))
      .collect();

    // Compare each entry
    for berry_entry in &berry_lockfile.entries {
      let resolution = berry_entry.package.resolution.unwrap_or("");

      if let Some(yarn_entry) = yarn_entries_by_resolved.get(resolution) {
        // Compare version
        let berry_version = berry_entry.package.version.unwrap_or("");
        assert_eq!(
          berry_version, yarn_entry.version,
          "Version mismatch for {resolution} in {filename}"
        );

        // Compare checksum/integrity
        let berry_checksum = berry_entry.package.checksum.unwrap_or("");
        if !yarn_entry.integrity.is_empty() || !berry_checksum.is_empty() {
          assert_eq!(
            berry_checksum, yarn_entry.integrity,
            "Checksum mismatch for {resolution} in {filename}"
          );
        }

        // Compare dependencies
        assert_eq!(
          berry_entry.package.dependencies.len(),
          yarn_entry.dependencies.len(),
          "Dependency count mismatch for {resolution} in {filename}"
        );

        for (yarn_dep_name, yarn_dep_range) in &yarn_entry.dependencies {
          let berry_dep = berry_entry
            .package
            .dependencies
            .values()
            .find(|d| full_ident_name(d.ident()) == *yarn_dep_name);

          assert!(
            berry_dep.is_some(),
            "Missing dependency '{yarn_dep_name}' for {resolution} in {filename}"
          );

          let berry_range = berry_dep.unwrap().range().trim_matches('"');
          let yarn_range = yarn_dep_range.trim_matches('"');
          assert_eq!(
            berry_range, yarn_range,
            "Dependency range mismatch for '{yarn_dep_name}' in {resolution} ({filename})"
          );
        }

        // Compare descriptors
        assert_eq!(
          berry_entry.descriptors.len(),
          yarn_entry.descriptors.len(),
          "Descriptor count mismatch for {resolution} in {filename}"
        );

        for (yarn_desc_name, _) in &yarn_entry.descriptors {
          let berry_has_name = berry_entry
            .descriptors
            .iter()
            .any(|d| full_ident_name(d.ident()) == *yarn_desc_name);

          assert!(
            berry_has_name,
            "Missing descriptor '{yarn_desc_name}' for {resolution} in {filename}"
          );
        }
      } else {
        assert!(
          !berry_entry.descriptors.is_empty(),
          "Entry with no resolution should have descriptors in {filename}"
        );
      }
    }

    println!(
      "✓ {filename}: {} entries validated against yarn-lock-parser",
      berry_lockfile.entries.len()
    );
  }

  /// Test that validates all expected fields are being parsed.
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

    let mut seen_version = false;
    let mut seen_resolution = false;

    for entry in &lockfile.entries {
      if entry.package.version.is_some() {
        seen_version = true;
      }
      if entry.package.resolution.is_some() {
        seen_resolution = true;
      }
    }

    assert!(seen_version, "No version field found in {filename}");
    assert!(seen_resolution, "No resolution field found in {filename}");
  }

  /// Test an arbitrary yarn.lock file via `BERRY_TEST_FIXTURE` environment variable.
  ///
  /// Usage:
  /// ```bash
  /// BERRY_TEST_FIXTURE=/path/to/yarn.lock cargo test -p berry-test test_arbitrary_fixture -- --nocapture
  /// ```
  #[test]
  fn test_arbitrary_fixture() {
    let Ok(fixture_path) = std::env::var("BERRY_TEST_FIXTURE") else {
      println!("BERRY_TEST_FIXTURE not set, skipping");
      return;
    };

    let path = std::path::Path::new(&fixture_path);
    assert!(path.exists(), "Fixture file does not exist: {fixture_path}");

    let contents =
      std::fs::read_to_string(path).unwrap_or_else(|e| panic!("Failed to read fixture: {e}"));

    println!("Testing: {fixture_path} ({} bytes)", contents.len());

    let berry_result = parse_lockfile(&contents);
    assert!(
      berry_result.is_ok(),
      "Berry failed to parse: {:?}",
      berry_result.err()
    );
    let (remaining, berry_lockfile) = berry_result.unwrap();

    let remaining_trimmed = remaining.trim();
    assert!(
      remaining_trimmed.is_empty(),
      "Unparsed content ({} bytes): '{}'",
      remaining.len(),
      &remaining_trimmed[..remaining_trimmed.len().min(200)]
    );

    println!(
      "Parsed {} entries (metadata version: {})",
      berry_lockfile.entries.len(),
      berry_lockfile.metadata.version
    );

    // Try yarn-lock-parser comparison if available
    match yarn_lock_parser::parse_str(&contents) {
      Ok(yarn_lockfile) => {
        assert_eq!(
          berry_lockfile.entries.len(),
          yarn_lockfile.entries.len(),
          "Entry count mismatch: berry={}, yarn-lock-parser={}",
          berry_lockfile.entries.len(),
          yarn_lockfile.entries.len()
        );
        println!("✓ Validated against yarn-lock-parser");
      }
      Err(e) => {
        println!("⚠ yarn-lock-parser failed: {e:?}");
        validate_berry_only(&berry_lockfile);
      }
    }
  }

  /// Validates berry output when yarn-lock-parser is unavailable.
  fn validate_berry_only(lockfile: &berry::lockfile::Lockfile) {
    let mut entries_with_version = 0;
    let mut entries_with_resolution = 0;

    for entry in &lockfile.entries {
      assert!(
        !entry.descriptors.is_empty(),
        "Entry has no descriptors: {:?}",
        entry.package.resolution
      );

      for desc in &entry.descriptors {
        assert!(!desc.ident().name().is_empty(), "Descriptor has empty name");
      }

      if entry.package.version.is_some() {
        entries_with_version += 1;
      }
      if entry.package.resolution.is_some() {
        entries_with_resolution += 1;
      }
    }

    #[allow(clippy::cast_precision_loss)]
    let version_ratio = entries_with_version as f64 / lockfile.entries.len() as f64;
    assert!(
      version_ratio > 0.9,
      "Too few entries have version: {:.1}%",
      version_ratio * 100.0
    );

    println!(
      "✓ Berry-only validation: {}/{} with version, {}/{} with resolution",
      entries_with_version,
      lockfile.entries.len(),
      entries_with_resolution,
      lockfile.entries.len()
    );
  }
}

#[cfg(test)]
mod verify_parse_fix {
    use berry::parse::parse_lockfile;

    #[test]
    fn test_many_entries_with_various_properties() {
        let content = r#"# This file is generated by running "yarn install" inside your project.
# Manual changes might be lost - proceed with caution!

__metadata:
  version: 6
  cacheKey: 8

"@adobe/css-tools@npm:^4.0.1, @adobe/css-tools@npm:^4.4.0":
  version: 4.4.0
  resolution: "@adobe/css-tools@npm:4.4.0"
  checksum: 10/abc123
  languageName: node
  linkType: hard

"debug@npm:4.3.4":
  version: 4.3.4
  resolution: "debug@npm:4.3.4"
  dependencies:
    ms: 2.1.2
  languageName: node
  linkType: hard

"ms@npm:2.1.2":
  version: 2.1.2
  resolution: "ms@npm:2.1.2"
  languageName: node
  linkType: hard

"react@npm:18.0.0":
  version: 18.0.0
  resolution: "react@npm:18.0.0"
  peerDependencies:
    react-dom: ^18.0.0
  languageName: node
  linkType: hard

"react-dom@npm:18.0.0":
  version: 18.0.0
  resolution: "react-dom@npm:18.0.0"
  dependencies:
    react: 18.0.0
  peerDependencies:
    react: ^18.0.0
  languageName: node
  linkType: hard
"#;

        match parse_lockfile(content) {
            Ok((remaining, lockfile)) => {
                println!("Successfully parsed {} entries", lockfile.entries.len());
                println!("Remaining content: {:?}", remaining);
                
                assert_eq!(lockfile.entries.len(), 5, "Should parse all 5 entries");
                assert!(remaining.trim().is_empty(), "Should consume entire file");
            }
            Err(e) => {
                panic!("Parse failed: {:?}", e);
            }
        }
    }
}

#[cfg(test)]
mod validate_blank_line_fix {
    use berry::parse::parse_lockfile;

    #[test]
    fn test_multiple_entries_with_blank_lines_between() {
        // This test validates that the parser correctly handles blank lines
        // between entries, which was causing fold_many0 to fail
        let content = r#"# This file is generated by running "yarn install" inside your project.
# Manual changes might be lost - proceed with caution!

__metadata:
  version: 6
  cacheKey: 8

"entry1@npm:1.0.0":
  version: 1.0.0
  resolution: "entry1@npm:1.0.0"
  languageName: node
  linkType: hard

"entry2@npm:1.0.0":
  version: 1.0.0
  resolution: "entry2@npm:1.0.0"
  languageName: node
  linkType: hard

"entry3@npm:1.0.0":
  version: 1.0.0
  resolution: "entry3@npm:1.0.0"
  languageName: node
  linkType: hard

"entry4@npm:1.0.0":
  version: 1.0.0
  resolution: "entry4@npm:1.0.0"
  dependencies:
    entry3: 1.0.0
  languageName: node
  linkType: hard

"entry5@npm:1.0.0":
  version: 1.0.0
  resolution: "entry5@npm:1.0.0"
  languageName: node
  linkType: hard

"entry6@npm:1.0.0":
  version: 1.0.0
  resolution: "entry6@npm:1.0.0"
  peerDependencies:
    entry5: 1.0.0
  languageName: node
  linkType: hard

"entry7@npm:1.0.0":
  version: 1.0.0
  resolution: "entry7@npm:1.0.0"
  languageName: node
  linkType: hard

"entry8@npm:1.0.0":
  version: 1.0.0
  resolution: "entry8@npm:1.0.0"
  bin:
    entry8: bin/entry8.js
  languageName: node
  linkType: hard

"entry9@npm:1.0.0":
  version: 1.0.0
  resolution: "entry9@npm:1.0.0"
  languageName: node
  linkType: hard

"entry10@npm:1.0.0":
  version: 1.0.0
  resolution: "entry10@npm:1.0.0"
  languageName: node
  linkType: hard
"#;

        match parse_lockfile(content) {
            Ok((remaining, lockfile)) => {
                println!("Successfully parsed {} entries", lockfile.entries.len());
                println!("Remaining: {:?}", remaining);
                
                assert_eq!(lockfile.entries.len(), 10, "Should parse all 10 entries");
                assert!(remaining.trim().is_empty(), "Should consume entire file");
                
                // Verify that entries with dependencies, peerDependencies, and bin are parsed correctly
                assert!(!lockfile.entries[3].package.dependencies.is_empty(), "entry4 should have dependencies");
                assert!(!lockfile.entries[5].package.peer_dependencies.is_empty(), "entry6 should have peerDependencies");
                assert!(!lockfile.entries[7].package.bin.is_empty(), "entry8 should have bin");
            }
            Err(e) => {
                panic!("Parse failed: {:?}", e);
            }
        }
    }

    #[test]
    fn test_consecutive_entries_with_multi_descriptors() {
        // Test that multi-descriptor entries work correctly in sequence
        let content = r#"# This file is generated by running "yarn install" inside your project.
# Manual changes might be lost - proceed with caution!

__metadata:
  version: 6
  cacheKey: 8

"@adobe/css-tools@npm:^4.0.1, @adobe/css-tools@npm:^4.4.0":
  version: 4.4.0
  resolution: "@adobe/css-tools@npm:4.4.0"
  languageName: node
  linkType: hard

"@airbnb/node-memwatch@npm:^3.0.0":
  version: 3.0.0
  resolution: "@airbnb/node-memwatch@npm:3.0.0"
  languageName: node
  linkType: hard

"@algolia/client-abtesting@npm:5.2.3":
  version: 5.2.3
  resolution: "@algolia/client-abtesting@npm:5.2.3"
  dependencies:
    "@algolia/client-common": 5.2.3
  languageName: node
  linkType: hard

"@algolia/client-common@npm:5.2.3":
  version: 5.2.3
  resolution: "@algolia/client-common@npm:5.2.3"
  languageName: node
  linkType: hard

"@atlaskit/adf-schema@npm:51.4.0":
  version: 51.4.0
  resolution: "@atlaskit/adf-schema@npm:51.4.0"
  languageName: node
  linkType: hard
"#;

        match parse_lockfile(content) {
            Ok((remaining, lockfile)) => {
                println!("Successfully parsed {} entries with multi-descriptors", lockfile.entries.len());
                
                assert_eq!(lockfile.entries.len(), 5, "Should parse all 5 entries");
                assert_eq!(lockfile.entries[0].descriptors.len(), 2, "First entry should have 2 descriptors");
                assert_eq!(lockfile.entries[1].descriptors.len(), 1, "Second entry should have 1 descriptor");
                assert!(!lockfile.entries[2].package.dependencies.is_empty(), "Third entry should have dependencies");
                assert!(remaining.trim().is_empty(), "Should consume entire file");
            }
            Err(e) => {
                panic!("Parse failed: {:?}", e);
            }
        }
    }
}

#[cfg(test)]
mod test_afm_full {
    use berry::parse::parse_lockfile;
    use std::fs;

    #[test]
    fn test_afm_full_lock_parsing() {
        // Try multiple possible paths since tests can run from different directories
        let content = fs::read_to_string("../../fixtures/afm-full.yarn.lock")
            .or_else(|_| fs::read_to_string("fixtures/afm-full.yarn.lock"))
            .or_else(|_| fs::read_to_string("crates/../fixtures/afm-full.yarn.lock"))
            .expect("Failed to read afm-full.yarn.lock from any known path");
        
        match parse_lockfile(&content) {
            Ok((remaining, lockfile)) => {
                println!("Successfully parsed {} entries from afm-full.yarn.lock", lockfile.entries.len());
                println!("Remaining bytes: {}", remaining.len());
                
                // Look for the css-tools entry
                let css_tools_entries: Vec<_> = lockfile.entries
                    .iter()
                    .filter(|entry| {
                        entry.descriptors.iter().any(|d| d.ident.name == "css-tools")
                    })
                    .collect();
                
                println!("Found {} css-tools entries", css_tools_entries.len());
                for entry in &css_tools_entries {
                    for desc in &entry.descriptors {
                        println!("  Scope: {:?}, Name: {:?}, Range: {:?}", 
                                 desc.ident.scope, desc.ident.name, desc.range.raw);
                    }
                }
                
                // The test passes if we find the entry
                assert!(!css_tools_entries.is_empty(), "Should find @adobe/css-tools entries");
            }
            Err(e) => {
                panic!("Parse failed: {:?}", e);
            }
        }
    }
}
