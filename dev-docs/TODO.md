# Development TODO
(this was used in initial scaffolding, do not treat it as source of truth, many things may be wrong)


## Current Status

**Yarn v4 Compatibility**:

- ❌ **Other advanced protocols**: `git:`, `file:`, `portal:`, `exec:`, `link:` (0% support)
- ❌ **Remaining features**: `resolutions`, `constraints` (70% support)

---

## High Priority

- **Protocol-specific parsing** - Support for `git:`, `file:`, `portal:`, `exec:`, `link:` protocols
- **Resolutions and constraints** - Handle `resolutions` and `constraints` sections

## Medium Priority

- **String interning** - Use string interning for common values (language names, link types)
- **Custom allocator** - Implement custom allocator for final data structures
- **Streaming parsing** - Support for parsing large lockfiles in chunks
- **Parallel parsing** - Explore parallel parsing for large dependency trees

### Benchmarking Enhancements

- **Large fixture benchmarks** - Add support for very large lockfiles (>100KB)
- **Memory allocation tracking** - Implement detailed allocation counting during parsing

### Integration & Deployment

- **WASM compilation** - Add `wasm-bindgen` attributes for WASM compilation
- **NAPI-RS integration** - Set up napi-rs project for Node.js integration

## Low Priority

### Advanced Features

- **Patch locator support** - Handle `::locator=workspace%3A.` syntax (??TBD)
- **Complex patch resolution** - Support patch resolution with version and hash

### Error Handling & Robustness

- **Custom error types** - Consider custom error types with context
- **Line/column information** - Add precise error location information
- **User-friendly error messages** - Create helpful error messages for debugging
- **Malformed input handling** - Graceful handling of corrupted lockfiles
- **Error recovery** - Ability to continue parsing after encountering errors

### Documentation

- **API documentation** - Add extensive `rustdoc` comments and examples

### Production Deployment

- **Production deployment** - Package and distribute the library

## Known Issues

### Parser Issues

- **Fix public API** - Should the package struct take `&str` instead of `String`? ??
- **URL-encoded patch paths** - Handle URL encoding in patch protocol (e.g., `npm%3A3.0.1`)
- **Builtin patch support** - Support `~builtin<compat/typescript>` syntax
- **Optional patch support** - Support `optional!builtin<compat/fsevents>` syntax
