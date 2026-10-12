# berry-dump-bin

Parse a lockfile and dump every parsed field.

The default output is the **canonical dump** from `berry_test::canonical`:
deterministic text covering metadata, every entry's descriptors and every
package field and map (maps sorted by key), plus whatever input the parser
left unconsumed. Two parser implementations are equivalent on
an input if and only if their canonical dumps are byte-identical.

## Usage

```bash
# any path
cargo run --release --bin berry-dump-bin -- path/to/yarn.lock > dump.txt
cargo run --release --bin berry-dump-bin -- path/to/yarn.lock -o dump.txt

# bundled fixture
cargo run --bin berry-dump-bin -- --fixture resolutions-patches.yarn.lock

# the parsed lockfile's `{:#?}` output
cargo run --bin berry-dump-bin -- --format debug path/to/yarn.lock
```

Exit status: 0 on success (a warning is printed if input is left unparsed),
1 on a parse error, 2 on I/O or usage errors.
