//! Node bindings for the `berry` lockfile parser.
//! Note - this is a slim public API file.

use napi::bindgen_prelude::Buffer;
use napi_derive::napi;

// NOTE: this is in lib.rs for now ,but eventually we may want to separate
// out the bindings, into its own crate

/// Parse a lockfile and return its number of package entries. Throws on
/// invalid UTF-8 and on parse errors (with line and column).
#[napi]
pub fn parse(file_contents: Buffer) -> napi::Result<u32> {
  let lockfile = berry::parse_bytes(&file_contents).map_err(|e| {
    // The text before the error offset is valid, so line and column are exact.
    napi::Error::from_reason(e.describe(&String::from_utf8_lossy(&file_contents)))
  })?;
  Ok(u32::try_from(lockfile.len()).unwrap_or(u32::MAX))
}

/// Read (or memory-map) and parse the lockfile at `path`; returns its number
/// of package entries. Throws on I/O errors and on parse errors
/// (`path:line:column: message`).
#[napi]
pub fn parse_file(path: String) -> napi::Result<u32> {
  let lockfile = berry::parse_file(&path).map_err(|e| napi::Error::from_reason(e.to_string()))?;
  Ok(u32::try_from(lockfile.len()).unwrap_or(u32::MAX))
}
