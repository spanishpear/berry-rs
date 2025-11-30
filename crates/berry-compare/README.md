# berry-compare

A utility crate to compare the output of `berry-core`'s lockfile parser with `@yarnpkg/parsers`.

## Purpose

This tool parses a yarn.lock file with both parsers and generates a detailed comparison report showing:
- Value mismatches for entries that exist in both parsers
- Entries that only appear in berry-core
- Entries that only appear in @yarnpkg/parsers

## Usage

```bash
cargo run --release -- <path-to-lock-file> [--verbose]
```

### Examples

```bash
# Compare with minimal output
cargo run --release -- ../../fixtures/berry.lock

# Compare with detailed diffs
cargo run --release -- ../../fixtures/afm-full.yarn.lock --verbose
```

## How It Works

1. **Berry Parser**: Uses `berry-core`'s `parse_lockfile` function to parse the lockfile
2. **Yarnpkg Parser**: Calls the Node.js `@yarnpkg/parsers` via the `parse-with-yarnpkg.js` script
3. **Comparison**: Both results are converted to JSON format and compared entry-by-entry
4. **Reporting**: Differences are categorized and reported with optional detailed diffs

## Output Format

The tool outputs:
- Summary statistics (total entries, differences found)
- Categorized differences:
  - **VALUE MISMATCHES**: Entries with different data
  - **ONLY IN BERRY**: Entries parsed by berry-core but not @yarnpkg/parsers
  - **ONLY IN @YARNPKG**: Entries parsed by @yarnpkg/parsers but not berry-core

### Exit Codes

- `0`: Parsers match perfectly
- `1`: Differences found

## Dependencies

- `berry`: The berry-core parser
- `serde_json`: For JSON serialization/comparison
- `clap`: For CLI argument parsing
- Node.js 18+: For running @yarnpkg/parsers

## Known Issues

- Multiple descriptors for a single entry are expanded into separate entries, which may cause apparent differences
- Peer dependencies format differs slightly between parsers
- @yarnpkg/parsers includes `__metadata` while berry-core doesn't
