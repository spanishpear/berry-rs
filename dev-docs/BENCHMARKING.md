# Benchmarking Guide

Technical documentation for Berry's benchmarking infrastructure.

## Setup

### Prerequisites

```bash
rustup update
cargo build --workspace
cargo test --workspace
```

### Quick Start

```bash
# Quick performance test
cargo run --bin berry-bench-bin -- -f minimal-berry.lock -v

# Run all benchmarks
cargo run --bin berry-bench-bin -- --all -r 5

# Save a baseline for regression checks
cargo run --bin berry-bench-bin -- --all -r 10 --format json --save-baseline .bench/baseline.json

# Compare against baseline with 5% threshold and fail on regression (for CI)
cargo run --bin berry-bench-bin -- --all -r 10 --baseline .bench/baseline.json --threshold-ratio-ms-per-kib 0.05 --fail-on-regression

# Detailed Criterion benchmarks
cargo bench --package berry-bench --bench parser_benchmarks -- --quick
```

#### Baselines & named Criterion benchmarks

- **Record or update a baseline:** `cargo bench --package berry-bench --bench parser_benchmarks -- --save-baseline main`. This writes fresh results under `target/criterion/**/base` for the `main` label.
- **Compare against the baseline:** rerun the benchmark with `--baseline main`; Criterion will print deltas, p-values, and CI ranges, flagging statistically significant regressions or improvements. Add `--save-baseline main` too if you want to refresh the stored baseline after comparison.
- **Focus on a single benchmark:** append the benchmark ID, e.g. `cargo bench --bench parser_benchmarks fixture_parsing/minimal_berry`. Any substring filter works (`heap_usage`, `input_characteristics`), and you can still combine it with `--baseline`.

**Which tool to use?** Reach for `berry-bench-bin` when you want a fast, human-friendly gut check (single fixtures, ad-hoc lockfiles via `--fixture-path`, baseline comparisons) while iterating locally. Swap to the Criterion suite whenever you need statistically rigorous evidence—e.g., validating parser changes before merging, capturing p-values/CI ranges, or producing reports that will influence performance gates in CI.

## Performance and Equivalence Harness

For parser work on large lockfiles there are four pieces that fit together: a synthetic
24 MB lockfile, a full-fidelity dump (the equivalence oracle), a parse-only timing binary and
a script that compares two checkouts. None of them needs a private lockfile; any real
`yarn.lock` can be passed in by path.

### Synthetic lockfile (`berry-gen`)

`berry-gen` writes a deterministic (seeded) yarn berry v8 lockfile that follows a rounded
"large-monorepo profile": about 27k entries, roughly half of them workspace entries with
long dependency lists, multi-descriptor headers, quoted and unquoted keys, patch and portal
entries, optional `bin`, `conditions`, `dependenciesMeta` and `peerDependenciesMeta` blocks.
All names are pseudo-words from the RNG; the same seed and size always give the same bytes.

```bash
# ~24 MB -> target/bench-fixtures/large-24mb.yarn.lock (gitignored via /target)
cargo run --release -p berry-bench --bin berry-gen

# smaller variants
cargo run --release -p berry-bench --bin berry-gen -- --size 1MB -o /tmp/small.yarn.lock
cargo run --release -p berry-bench --bin berry-gen -- --entries 500 -o /tmp/tiny.yarn.lock
```

The profile parameters live in `crates/berry-bench/src/synth.rs` (`Profile`). Tests in
`crates/berry-bench/tests/synth_parses.rs` assert that the parser consumes generated files
completely, so a generator change that produces unparseable input fails CI.

### Canonical dump (`berry-dump-bin`)

`berry-dump-bin <lockfile>` prints every parsed field in a deterministic text form (see
`crates/berry-test/src/canonical.rs`): metadata, `resolutions`/`constraints`, each entry's
descriptors (ident, raw range, protocol, selector) and every `Package` field including all
dependency, meta and `bin` maps (sorted by key because `HashMap` order is random), plus the
unparsed remainder. Two parsers are equivalent on an input exactly when the dumps are
byte-identical.

```bash
cargo run --release --bin berry-dump-bin -- path/to/yarn.lock -o dump.txt
cargo run --release --bin berry-dump-bin -- --format debug path/to/yarn.lock   # raw {:#?}
```

When a field is added to the data model, add it to `canonical.rs` too, otherwise the oracle
silently ignores it.

### Parse-only timing (`berry-parse-bench`)

Reads the file into memory once, then times one parse API for N iterations. The result is
dropped outside the timed region and the drop is reported separately.

```bash
cargo run --release -p berry-bench-bin --bin berry-parse-bench -- path/to/yarn.lock
#   -n 50 --warmup 5     iteration counts
#   --api str|reuse|bytes|file
#                        berry::parse (default), parse_with + recycled Buffers (0 allocations),
#                        parse_bytes (UTF-8 validation + parse), parse_file (I/O in the timed
#                        region, unmap in the drop)
#   --threads N          size of the rayon global pool (1: every parse is sequential)
#   --chunk-target B     time berry::__parse_chunked with ~B-byte chunks instead
#   --include-read       also time read_to_string + parse
#   --stages             parse / walk every view / canonical dump / drop
#   --table-stats        table row counts and bytes per row, then exit
#   --json               machine-readable output (used by bench-compare.sh)
#   --once               one parse + exit, no warmup (hyperfine target; --api file: parse_file)

# allocation counts (counting global allocator; separate feature so timings are unaffected)
cargo run --release -p berry-bench-bin --features alloc-count --bin berry-parse-bench -- path/to/yarn.lock --alloc

# timings under mimalloc (v3, the allocator of afm-tools); measurement only, not a library feature
cargo run --release -p berry-bench-bin --features mimalloc --bin berry-parse-bench -- path/to/yarn.lock
```

It reports min/median/mean/max in ms, MB/s (decimal megabytes) and peak RSS (`VmHWM`). With
`--alloc`: allocation, reallocation and free counts, total bytes allocated, peak live heap
during the parse, bytes retained by the result and the number of span tables that outgrew
their reserved window (`Lockfile::__spills`, expected 0 for real lockfiles).

The numbers in this repository are taken with `RUSTFLAGS=-Ctarget-cpu=x86-64` (what a
consumer of the crate gets; the repo's `.cargo/config.toml` builds for the native CPU).

For Criterion, `cargo bench -p berry-bench --bench parser_benchmarks -- large_lockfile` runs
parse-only and parse+drop on the synthetic 24 MB file with byte throughput. Set
`BERRY_BENCH_LOCKFILE=/path/to/yarn.lock` to benchmark any other file instead.

### Comparing two checkouts (`scripts/bench-compare.sh`)

```bash
scripts/bench-compare.sh path/to/yarn.lock               # equivalence + speed
scripts/bench-compare.sh path/to/yarn.lock --alloc       # plus allocation counts
scripts/bench-compare.sh path/to/yarn.lock --no-speed    # only the equivalence diff
```

The script builds `BASELINE_DIR` (default `/home/ubuntu/contrib/berry-wt/baseline`) and
`CANDIDATE_DIR` (default: the checkout containing the script) in `--release`, diffs the
canonical dumps, then runs hyperfine (process level) and interleaved in-process rounds of
`berry-parse-bench`. **It exits 1 if the dumps differ**, 0 if identical, 2 on usage or
build errors. Inputs that fail to parse identically in both trees count as equivalent.
Dumps are deleted on exit (`--keep` retains them) and only hashes and counts are printed
unless `--show-diff` is given, so it is safe to run on a lockfile whose contents are private.

The baseline is a worktree on a branch (`perf-baseline`) that carries the same harness
commits on top of the unchanged `main` parser, so both sides are built with identical tooling:

```bash
git worktree add ../berry-wt/baseline -b perf-baseline main
git -C ../berry-wt/baseline cherry-pick <harness commits>                   # no parser changes
diff -r ../berry-wt/baseline/crates/berry-core/src crates/berry-core/src    # parser delta
```

Build flags matter: `.cargo/config.toml` sets `-Ctarget-cpu=native` and frame pointers, which
downstream consumers do not inherit. Set `RUSTFLAGS` (for example `RUSTFLAGS=""`) to measure
what a consumer gets. Differences below about 1% between two identical trees are noise; very
small fixtures (a few microseconds) show large percentage noise and are only meaningful for
the equivalence diff.

Notes on interpreting results:

- Profiling tools (`samply`, `perf`) may be unavailable in sandboxes (`perf_event_open`
  blocked). Use `--stages` for a coarse breakdown and `--alloc` for allocation behaviour.
- Consumers often build the parser in a dev profile. Set `opt-level = 3` for `berry` and its
  dependencies (`memchr`, `rayon`) and disable debug assertions and overflow checks to get
  near release speed.

## Benchmark Categories

### Fixture Parsing

Tests parsing performance with different file sizes:

```bash
# Small fixtures (1-10 packages)
cargo run --bin berry-bench-bin -- -f minimal-berry.lock
cargo run --bin berry-bench-bin -- -f workspaces.yarn.lock

# Medium fixtures (10-1000 packages)
cargo run --bin berry-bench-bin -- -f auxiliary-packages.yarn.lock

# Large fixtures (1000+ packages)
cargo run --bin berry-bench-bin -- -f berry.lock
cargo run --bin berry-bench-bin -- -f resolutions-patches.yarn.lock
```

**Performance targets**:

- Small files (< 1KB): < 1ms
- Medium files (< 100KB): < 10ms
- Large files (< 1MB): < 100ms

### Memory Usage

Tracks heap usage and validates zero-allocation claims:

```bash
# Memory usage with verbose output
cargo run --bin berry-bench-bin -- -f minimal-berry.lock -v

# Criterion memory benchmarks
cargo bench --package berry-bench --bench parser_benchmarks
```

**Memory targets**:

- Zero allocations during parsing phase
- Minimal heap usage for final data structures
- Consistent memory patterns across similar file sizes

### Input Characteristics

Tests different lockfile features:

```bash
# Test different characteristics
cargo run --bin berry-bench-bin -- -f yarn4-mixed-protocol.lock
cargo run --bin berry-bench-bin -- -f yarn4-resolution.lock
cargo run --bin berry-bench-bin -- -f yarn4-patch.lock
```

## Interpreting Results

### CLI Tool Output

```
Benchmarking minimal-berry.lock (1152 bytes)...
  Warmup 1: 0.165ms
  Warmup 2: 0.132ms
  Warmup 3: 0.134ms
  Heap usage: 20480 bytes (physical), 0 bytes (virtual)
  Run 1: 0.133ms
  Run 2: 0.133ms
  Run 3: 0.131ms

Benchmark Results:
Fixture                   Size (bytes) Mean (ms)    Min (ms)     Max (ms)     Heap (bytes)
------------------------------------------------------------------------------------------
minimal-berry.lock        1152         0.132        0.131        0.133        20480

Performance Analysis:
  minimal-berry.lock performance looks normal (1.0x vs fastest)
```

**Key metrics**:

- **Mean (ms)**: Average parsing time across all runs
- **Min/Max (ms)**: Range of parsing times
- **Heap (bytes)**: Physical memory usage
- **Performance Analysis**: Regression detection results

### Criterion Output

```
fixture_parsing/minimal_berry
                        time:   [6.1249  b5s 6.2624  b5s 6.2968  b5s]
                        change: [-3.4204% -0.9236% +1.4829%] (p = 0.85 > 0.05)
                        No change in performance detected.

heap_usage/heap_small   time:   [1.2025 ms 1.2383 ms 1.2472 ms]
```

**Key metrics**:

- **Time range**: 95% confidence interval
- **Change**: Performance change from baseline
- **p-value**: Statistical significance (p < 0.05 indicates significant change)

## Regression Detection

### CLI Tool Regression Detection (relative-to-best and baseline)

Automatically detects regressions by comparing against fastest fixture:

```bash
cargo run --bin berry-bench-bin -- --all -r 5
```

**Output**:

```
Performance Analysis:
  workspaces.yarn.lock performance looks normal (1.0x vs fastest)
  minimal-berry.lock is 2.8x slower than workspaces.yarn.lock (potential regression)
```

**Thresholds**:

- **Warning**: >50% slower than fastest fixture
- **Critical**: >100% slower than fastest fixture

Additionally, compare against a stored baseline using normalized ms/KiB:

```bash
cargo run --bin berry-bench-bin -- --all --baseline .bench/baseline.json --threshold-ratio-ms-per-kib 0.05 --fail-on-regression
```

This exits with code 1 if any fixture regresses more than 5% in ms/KiB.

### Criterion Regression Detection

Automatically detects statistically significant performance changes:

```
change: [+22.753% +25.201% +27.608%] (p = 0.00 < 0.05)
Performance has regressed.
```

**Interpretation**:

- **p < 0.05**: Statistically significant change
- **Positive change**: Performance regression
- **Negative change**: Performance improvement

## Memory Analysis

### Heap Usage Tracking

Tracks both physical and virtual memory:

```bash
# Verbose memory output
cargo run --bin berry-bench-bin -- -f minimal-berry.lock -v
```

**Output**:

```
Heap usage: 20480 bytes (physical), 0 bytes (virtual)
```

**Interpretation**:

- **Physical memory**: Actual heap usage in bytes
- **Virtual memory**: Virtual memory allocation
- **Zero virtual memory**: Often indicates zero-allocation success

### Zero-Allocation Validation

Some fixtures show 0 bytes heap usage, validating zero-allocation claims:

```
yarn4-mixed-protocol.lock: 0 bytes heap usage
yarn4-patch.lock: 0 bytes heap usage
```

## Performance Optimization

### Development Workflow

1. **Establish baseline**:

   ```bash
   cargo run --bin berry-bench-bin -- --all -r 10
   ```

2. **Make changes**: Implement optimizations

3. **Test performance**:

   ```bash
   cargo run --bin berry-bench-bin -- --all -r 10
   ```

4. **Compare results**: Look for improvements or regressions

5. **Detailed analysis** (if needed):
   ```bash
   cargo bench --package berry-bench --bench parser_benchmarks
   ```

### Optimization

- Use `&str` instead of `String` during parsing
- Defer allocation until final data structures
- Parse everything in one pass

### Adding New Fixtures

1. Add fixture to `fixtures/` directory
2. Update benchmark lists in both CLI and Criterion benchmarks
3. Test parsing to ensure compatibility
4. Run benchmarks to establish baseline

#### Custom Benchmark Categories

```rust
// Add to crates/berry-bench/benches/parser_benchmarks.rs
fn benchmark_custom_category(c: &mut Criterion) {
    let mut group = c.benchmark_group("custom_category");

    // Your custom benchmarks here

    group.finish();
}
```

```bash
# Compare against baseline
cargo run --bin berry-bench-bin -- --all --format json > current-results.json
diff baseline-results.json current-results.json
```

### Debug Tools

```bash
# Debug parsing issues
RUST_LOG=debug cargo test

# Memory profiling
cargo run --bin berry-bench-bin -- -f large-fixture.lock -v

# Detailed Criterion analysis
cargo bench --package berry-bench --bench parser_benchmarks -- --verbose
```

## Profiling

Use `perf` or `samply` to generate flamegraphs and find optimization opportunities.

The project is configured with:

- Frame pointers enabled (`.cargo/config.toml`)
- A dedicated `profiling` profile with debug symbols (`Cargo.toml`)

Use `--profile profiling` instead of `--release` to get readable symbols.

### Using samply (Recommended)

samply produces interactive flamegraphs in Firefox Profiler:

```bash
# Build with profiling profile (release + debug symbols)
cargo build --profile profiling --bin berry-bench-bin

# Profile in browser
samply record ./target/profiling/berry-bench-bin -f resolutions-patches.yarn.lock -r 100
```

### Profiling Large Lockfiles

For profiling with large lockfiles (10MB+), place your lockfile in `fixtures/` and run:

```bash
# Build with profiling profile
cargo build --profile profiling --bin berry-bench-bin

# Profile with samply
samply record ./target/profiling/berry-bench-bin -f your-large-file.lock -r 50
```

### Using perf (Alternative)

TODO - show dropped CPU cycles from jgenset 1bi

```bash
# Build with profiling profile (sort of the same as release but we use it for some extra cargo settings)
cargo build --profile profiling --bin berry-bench-bin

# Record profile
perf record -g --call-graph dwarf \
  ./target/profiling/berry-bench-bin -f resolutions-patches.yarn.lock -r 100

# View report
perf report
```

## Related

- [Criterion Documentation](https://docs.rs/criterion/) - Statistical benchmarking framework
- [Memory Stats Documentation](https://docs.rs/memory-stats/) - Memory usage tracking
- [Core Parser](CORE_PARSER.md) - Parser implementation details
