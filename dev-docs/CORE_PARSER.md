# Core Parser Module

The core parser (`crates/berry-core/`) reads the lockfiles Yarn v3/v4 write.
It produces `u32` span tables over the input, so a parse allocates a constant
handful of buffers and accessors are borrowed views.

## Module Structure

```
crates/berry-core/src/
├── lib.rs           # crate-root API: parse(_with), parse_bytes(_with), parse_file(_with), re-exports
├── file.rs          # (private) parse_file: mmap (>= 256 KiB) or read(2), FileError
├── lockfile.rs      # (private) owning Lockfile<'a>, Buffers, Entries iterator, Metadata
├── view.rs          # (private) EntryView, list views (Descriptors, Dependencies, Bins, ...), LinkType, DependencyMeta
├── pipeline.rs      # (private) chunk plan, parallel chunk scans (rayon), stitch
├── scan.rs          # (private) the grammar: header, __metadata and entries, scanned into span tables
├── table.rs         # (private) span-table storage: one buffer per kind per parse, one window per chunk
├── utf8.rs          # (private) parallel UTF-8 validation for parse_bytes / parse_file
├── error.rs         # (private) 8-byte Error { offset, kind }, lazy line/column + description
└── ident.rs         # Ident, Range, Protocol, Descriptor, Locator (views over the source text)
```

## Parsing pipeline

1. `scan::preamble` checks the two header comment lines and reads the two
   `__metadata` values.
2. `scan::entry` reads one entry in the shape Yarn writes: a key line (`a@1:`,
   `"a@1, b@2":` or, for keys over 1024 bytes, `? "..."` and a `:` line), then
   two-space `key: value` lines and four-space blocks (`dependencies`,
   `peerDependencies`, `bin`, and the nested `dependenciesMeta` /
   `peerDependenciesMeta` forms). Keys are recognised by their first byte plus
   one comparison; short searches use plain `u64` arithmetic (no intrinsics),
   then `memchr`.
3. An unknown property key is `ErrorKind::UnknownProperty` and a `linkType`
   other than `hard`/`soft` is `ErrorKind::InvalidLinkType`. Any other text that
   is not an entry ends the parse there: `Lockfile::remaining` is the rest.
   Input Yarn does not write (CRLF, tabs, inline meta, top-level
   `resolutions:` blocks, ...) never panics, hangs or causes UB, but its result
   is unspecified.
4. Bodies of 2.5 MB or more are cut into two chunks per rayon thread, at entry
   starts (blank line + non-whitespace byte), and scanned in parallel. UTF-8
   validation (`parse_bytes`, `parse_file`) uses the same split. A sequential
   stitch walks the chunks with the position the sequential loop would be at:
   chunks starting there are used as they are, chunks behind it are cleared,
   others are scanned again from it. The result is identical to a sequential
   parse for every split, including the stop and error point.
5. The per-chunk span tables ("segments") are the final storage of the
   returned `berry::Lockfile`; every accessor (`EntryView::dependencies()`,
   `Descriptor::ident()`, ...) slices the source text on access. The seven
   table kinds each have one buffer for the whole parse (`table.rs`), sized
   from the chunk sizes and carved into one window per chunk, so a parse makes
   8 allocations whatever the number of chunks, threads or entries, and none
   with `parse_with` and recycled `Buffers`. A table that outgrows its window
   moves to its own vector.

Lists keep source order and duplicates; `Dependencies::get`, `Bins::get` and
`Dependencies::last_wins` give the 0.6 (`HashMap`, last wins) semantics.

The differential tests in `crates/berry-test` compare the canonical dump with a
frozen copy of the 0.6 parser (`crates/berry-ref`) on every Yarn-shaped input,
and check that everything else parses without a panic.

## Known limitations

Edge cases outside normal lockfile use; none needs action in CI or local dev.

- Parsing panics if the global rayon pool cannot spawn its threads (rayon's own behaviour).
- `parse_file` on a non-regular file (pipe, device) reads it to the end before the size check.
- Inputs over ~100 MB that are not a lockfile can make large up-front table reservations (sized from the input length).
- `parse` called from inside a rayon task waits with `join`, so that worker may run other queued tasks meanwhile (normal rayon behaviour).
- node-bindings `parse(Buffer)`: do not pass a `SharedArrayBuffer`-backed buffer that a Worker mutates during the call.
- The error detail string (offending key, found value) is not length-capped.
- `parse_file` maps large files: see its docs for the caller contract while Yarn rewrites `yarn.lock`.

## Usage Examples

### Basic Parsing

```rust
let content = std::fs::read_to_string("yarn.lock")?;
match berry::parse(&content) {
    Ok(lockfile) => {
        println!("Packages: {}", lockfile.len());
        println!("Version: {}", lockfile.metadata().version);
    }
    Err(e) => eprintln!("Parse error: {}", e.describe(&content)),
}
```

### Access Package Data

```rust
for entry in &lockfile {
    for d in entry.descriptors() {
        println!("{d} -> {:?}", entry.version());
    }
    // source order, duplicates kept; `last_wins()` gives the 0.6 map contents
    for dep in entry.dependencies().last_wins() {
        println!("  {}@{}", dep.ident(), dep.range());
    }
}
```
