# Berry - High-Performance Yarn Lockfile Parser

A high-performance parser for Yarn v3/v4 lockfiles. It scans the file once into flat span tables and parses large files in parallel, with a fixed 8 allocations per parse (none when buffers are reused).
A 24 MB lockfile parses in about 2 ms on 32 cores; see [BENCHMARKING.md](dev-docs/BENCHMARKING.md).

## Layout

```
crates/
├── berry-core/          # Main parser library
├── berry-test/          # Integration tests, canonical dump
├── berry-bench/         # Criterion benchmarks, berry-gen synthetic lockfiles
├── berry-bench-bin/     # CLI benchmarking tools (berry-parse-bench)
├── berry-compare-bin/   # Compare with @yarnpkg/parsers
├── berry-dump-bin/      # Dump every parsed field
└── node-bindings/       # Node.js bindings (WIP)
```

See [CONTRIBUTING.md](dev-docs/CONTRIBUTING.md) for development guidelines.
See [BENCHMARKING.md](dev-docs/BENCHMARKING.md) for benchmarking.


## License

MIT OR Apache-2.0
