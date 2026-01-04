# Berry - High-Performance Yarn Lockfile Parser

A high-performance parser for Yarn v3/v4 lockfiles. This parser focuses on performance, with minimal allocation and _ideally__ - future use in WASM or with napi-rs.
For a large ~300k line lockfile, we are able to parse it in ~50ms, and future improvements will make that faster 🥳

## Layout

```
crates/
├── berry-core/          # Main parser library
├── berry-test/          # Integration tests
├── berry-bench/         # Criterion microbenchmarks
├── berry-bench-bin/     # CLI benchmarking tool
└── node-bindings/       # Node.js bindings (WIP)
```

See [CONTRIBUTING.md](dev-docs/CONTRIBUTING.md) for development guidelines.
See [BENCHMARKING.md](dev-docs/BENCHMARKING.md) for development guidelines.


## License

MIT OR Apache-2.0
