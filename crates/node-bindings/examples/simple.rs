//! Parse a bundled fixture with the `berry` crate and print a summary.
fn main() {
  let file_contents = include_str!("../../../fixtures/berry.lock");
  match berry::parse(file_contents) {
    Ok(lockfile) => {
      println!(
        "lockfile v{} with {} entries",
        lockfile.metadata().version,
        lockfile.len()
      );
      for entry in lockfile.entries().take(3) {
        println!(
          "  {:?} -> {:?}",
          entry.descriptors().first(),
          entry.version()
        );
      }
    }
    Err(e) => eprintln!("{}", e.describe(file_contents)),
  }
}
