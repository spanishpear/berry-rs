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
