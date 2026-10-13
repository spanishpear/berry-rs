#![deny(clippy::all)]
//! End-to-end integration tests for the Berry lockfile parser
//!
//! This crate provides comprehensive integration tests using real Yarn lockfile
//! fixtures to validate the parsing functionality across various lockfile formats.

pub mod canonical;
pub mod differential;

/// The canonical dump of the frozen reference parser (`berry_ref`), produced by
/// the same code as [`canonical`] so the two outputs are directly comparable.
pub mod canonical_ref {
  crate::canonical_dump_impl!(berry_ref);
}

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
  use rstest::rstest;
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

    let result = berry::parse(&contents);
    assert!(
      result.is_ok(),
      "Should successfully parse lockfile: {filename}"
    );

    let lockfile = result.unwrap();
    let remaining = lockfile.remaining();

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
      !lockfile.is_empty(),
      "Should parse at least one package from {filename}"
    );

    println!(
      "Successfully parsed {} packages from {filename}",
      lockfile.len()
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

    let result = berry::parse(&contents);
    assert!(
      result.is_ok(),
      "Should successfully parse minimal berry lockfile"
    );

    let lockfile = result.unwrap();
    dbg!(&lockfile);
    assert_eq!(lockfile.metadata().version, "6");
    assert_eq!(lockfile.len(), 5);
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
}
