# Contributing to Berry

Welcome to the Berry project! This document provides comprehensive guidelines for contributing to the high-performance Yarn lockfile parser.

## Project Overview

Berry is a high-performance, zero-allocation parser for Yarn v3/v4 lockfiles, built with Rust. The project focuses on:

- **Performance**: Sub-millisecond parsing for most lockfiles
- **Memory Efficiency**: Zero-allocation parsing with minimal heap usage
- **Modularity**: Clean architecture for WASM and Node.js integration
- **Reliability**: Comprehensive testing and benchmarking

## Architecture

```
crates/
├── berry-core/          # Main parser library
├── berry-test/          # Integration tests
├── berry-bench/         # Criterion microbenchmarks
├── berry-bench-bin/     # CLI benchmarking tool
└── node-bindings/       # Node.js bindings (planned)
```

## Development Setup

### Prerequisites

- **Rust**: Latest stable version (1.70+)
- **Cargo**: With workspace support
- **Git**: For version control

### Initial Setup

```bash
# Clone the repository
git clone <repository-url>
cd berry

# Build all crates
cargo build --workspace

# Run tests
cargo test --workspace

# Check code quality
cargo clippy --workspace
```

## Benchmarking Infrastructure

Berry includes a comprehensive benchmarking system to ensure performance and detect regressions.

### Overview

The benchmarking infrastructure consists of two main components:

1. **Criterion Microbenchmarks** (`crates/berry-bench/`)

   - Statistical benchmarking with confidence intervals
   - Beautiful HTML reports
   - Regression detection
   - Memory usage tracking

2. **CLI Benchmarking Tool** (`crates/berry-bench-bin/`)
   - Quick performance testing for development
   - Multiple fixture support
   - Regression detection
   - JSON output for CI integration

### Running Benchmarks

#### Quick Performance Testing

```bash
# Test a specific fixture
cargo run --bin berry-bench-bin -- -f minimal-berry.lock -v

# Test all working fixtures
cargo run --bin berry-bench-bin -- --all -r 10

# Get JSON output for CI integration
cargo run --bin berry-bench-bin -- --all --format json
```

#### Detailed Performance Analysis

```bash
# Run comprehensive Criterion benchmarks
cargo bench --package berry-bench

# Quick benchmark run
cargo bench --package berry-bench --bench parser_benchmarks -- --quick

# Generate HTML reports
cargo bench --package berry-bench -- --html
```

### Benchmark Categories

#### 1. Fixture Parsing

- **Small fixtures** (1-10 packages): `minimal-berry.lock`, `workspaces.yarn.lock`
- **Medium fixtures** (10-1000 packages): `yarn4-mixed-protocol.lock`, `auxiliary-packages.yarn.lock`
- **Large fixtures** (1000+ packages): `berry.lock`, `resolutions-patches.yarn.lock`

#### 2. Memory Usage

- **Heap usage tracking**: Physical and virtual memory measurement
- **Zero-allocation validation**: Verify no allocations during parsing
- **Memory scaling**: Correlation between file size and memory usage

#### 3. Input Characteristics

- **Simple lockfiles**: Basic dependency structures
- **Mixed protocols**: npm, workspace, and other protocols
- **Resolutions**: Complex resolution scenarios
- **Patches**: Patch protocol handling

### Performance Targets

- **Parsing speed**: < 1ms for small files (< 1KB), < 10ms for medium files (< 100KB), < 100ms for large files (< 1MB)
- **Memory usage**: Zero allocations during parsing phase, minimal allocations for final data structures
- **Regression detection**: Automated alerts for >5% performance degradation

### Interpreting Results

#### CLI Tool Output

```
Benchmark Results:
Fixture                   Size (bytes) Mean (ms)    Min (ms)     Max (ms)     Heap (bytes)
------------------------------------------------------------------------------------------
minimal-berry.lock        1152         0.132        0.131        0.133        20480
workspaces.yarn.lock      2005         0.048        0.046        0.050        8192
auxiliary-packages.yarn.lock 40540        0.082        0.080        0.085        20480

Performance Analysis:
  workspaces.yarn.lock performance looks normal (1.0x vs fastest)
  minimal-berry.lock is 2.8x slower than workspaces.yarn.lock (potential regression)
```

#### Criterion Output

```
fixture_parsing/minimal_berry
                        time:   [6.1249  b5s 6.2624  b5s 6.2968  b5s]
                        change: [-3.4204% -0.9236% +1.4829%] (p = 0.85 > 0.05)
                        No change in performance detected.

heap_usage/heap_small   time:   [1.2025 ms 1.2383 ms 1.2472 ms]
```

### Memory Analysis

The benchmarking system tracks:

- **Physical memory**: Actual heap usage in bytes
- **Virtual memory**: Virtual memory allocation
- **Allocation patterns**: Zero-allocation validation
- **Memory scaling**: Correlation with file size

### Regression Detection

The system automatically detects:

1. **Performance regressions**: >50% slower than baseline
2. **Statistical significance**: p < 0.05 in Criterion tests
3. **Memory usage increases**: Unexpected heap usage growth
4. **Zero-allocation violations**: Unexpected allocations during parsing

## Testing

### Running Tests

```bash
# Run all tests
cargo test --workspace

# Run specific crate tests
cargo test --package berry-core
cargo test --package berry-test

# Run with verbose output
cargo test --workspace -- --nocapture
```

### Test Categories

#### Unit Tests

- Individual parsing function tests
- Edge case handling
- Error condition validation

#### Integration Tests

- End-to-end lockfile parsing
- Real fixture validation
- Cross-platform compatibility

#### Benchmark Tests

- Performance regression detection
- Memory usage validation
- Statistical significance testing

## Code Quality

### Code Style

```bash
# Format code
cargo fmt --workspace

# Check code quality
cargo clippy --workspace

# Check for security issues
cargo audit
```

### Commit Guidelines

- **Feature commits**: `feat: add multi-descriptor support`
- **Bug fixes**: `fix: resolve parsing issue with large fixtures`
- **Performance**: `perf: optimize dependency parsing`
- **Documentation**: `docs: update benchmarking guide`
- **Tests**: `test: add edge case validation`

### Pull Request Process

1. **Create feature branch**: `git checkout -b feature/your-feature`
2. **Make changes**: Follow code style guidelines
3. **Add tests**: Include unit and integration tests
4. **Run benchmarks**: Ensure no performance regressions
5. **Update documentation**: Update relevant docs
6. **Submit PR**: Include detailed description and benchmark results

## Performance Guidelines

### Zero-Allocation Principles

1. **Use borrowed strings**: `&str` instead of `String` during parsing
2. **Avoid intermediate collections**: Use `fold_many0` instead of `many0`
3. **Defer allocation**: Only allocate when building final data structures
4. **Single-pass parsing**: Parse everything in one go

### Optimization Strategies

1. **Profile first**: Use benchmarks to identify bottlenecks
2. **Measure impact**: Always benchmark before and after changes
3. **Consider trade-offs**: Performance vs. memory vs. complexity
4. **Document decisions**: Explain optimization choices

### Common Pitfalls

1. **Premature optimization**: Optimize only after profiling
2. **Ignoring benchmarks**: Always run benchmarks before committing
3. **Memory leaks**: Ensure proper cleanup in long-running scenarios
4. **Over-engineering**: Keep solutions simple and maintainable

## Monitoring Performance

### Development Workflow

1. **Before changes**: Run benchmarks to establish baseline
2. **During development**: Use CLI tool for quick feedback
3. **Before commit**: Run full benchmark suite
4. **After merge**: Monitor for regressions

### CI/CD Integration

```yaml
# Example GitHub Actions workflow
name: Performance Benchmarks
on: [push, pull_request]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      - run: cargo bench --workspace
      - run: cargo run --bin berry-bench-bin -- --all
```

## Debugging

### Common Issues

1. **Parsing failures**: Check fixture format and parser logic
2. **Performance regressions**: Compare with baseline benchmarks
3. **Memory issues**: Use heap usage tracking to identify leaks
4. **Test failures**: Check fixture availability and format

### Debug Tools

```bash
# Run with debug output
RUST_LOG=debug cargo test

# Profile with flamegraph
cargo install flamegraph
cargo flamegraph --bench parser_benchmarks

# Memory profiling
cargo run --bin berry-bench-bin -- -f large-fixture.lock -v
```

## Additional Resources

- [Task List](.cursor/tasks/BERRY_LOCKFILE_PARSER.md) - Detailed development progress
- [Benchmarking Plan](.cursor/tasks/BENCHMARKING_PLAN.md) - Comprehensive benchmarking strategy
- [Criterion Documentation](https://docs.rs/criterion/) - Benchmarking framework

## Getting Help

- **Issues**: Use GitHub issues for bugs and feature requests
- **Discussions**: Use GitHub discussions for questions and ideas
- **Benchmarks**: Share benchmark results and performance analysis
- **Contributions**: Follow this guide for code contributions

---

Thank you for contributing to Berry! Your work helps make this parser faster, more reliable, and more useful for the community.
