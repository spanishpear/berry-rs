# Berry - High-Performance Yarn Lockfile Parser

A high-performance parser for Yarn v3/v4 lockfiles. This parser focuses on performance, with minimal allocation and future use in WASM or with napi-rs.

## Layout

```
crates/
├── berry-core/          # Main parser library
├── berry-test/          # Integration tests
├── berry-bench/         # Criterion microbenchmarks
├── berry-bench-bin/     # CLI benchmarking tool
└── node-bindings/       # Node.js bindings (WIP)
```

## Benchmarking

The project includes basic benchmarking infrastructure for performance monitoring and regression detection. Claude wrote that part, apologies.

### Quick Performance Testing

```bash
# Test a specific fixture
cargo run --bin berry-bench-bin -- -f minimal-berry.lock -v

# Test all working fixtures
cargo run --bin berry-bench-bin -- --all -r 10

# Get JSON output for CI integration
cargo run --bin berry-bench-bin -- --all --format json
```

### Detailed Performance Analysis

```bash
# Run comprehensive Criterion benchmarks
cargo bench --package berry-bench

# Quick benchmark run
cargo bench --package berry-bench --bench parser_benchmarks -- --quick
```

## Development

### Building

```bash
# Build all crates
cargo build --workspace

# Build with optimizations
cargo build --release --workspace
```

### Testing

```bash
# Run all testss
cargo nextest run

# Run integration tests
cargo nextest run -package berry-test

# Run benchmarks
cargo bench --workspace
```

### Code Quality

```bash
# Check code quality
cargo clippy --workspace

```

See [CONTRIBUTING.md](dev-docs/CONTRIBUTING.md) for development guidelines and benchmarking information.

## License

MIT OR Apache-2.0

## Links

- [Task List](.cursor/tasks/BERRY_LOCKFILE_PARSER.md) - Detailed development progress
- [Benchmarking Plan](.cursor/tasks/BENCHMARKING_PLAN.md) - Comprehensive benchmarking strategy
- [Dev Documentation](dev-docs/) - Development guides and documentation
