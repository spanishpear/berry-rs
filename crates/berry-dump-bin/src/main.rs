use clap::{Parser, ValueEnum};
use std::io::{BufWriter, Write};
use std::path::PathBuf;

#[derive(Clone, Copy, Debug, ValueEnum)]
enum Format {
  /// Canonical, deterministic text dump of every parsed field (equivalence oracle)
  Canonical,
  /// Rust `{:#?}` of the parsed lockfile
  Debug,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum Api {
  /// `berry::parse(&str)`
  Str,
  /// `berry::parse_with`, parsing twice and dumping the second parse made
  /// with the recycled buffers of the first
  Reuse,
  /// `berry::parse_bytes` on the file's bytes
  Bytes,
  /// `berry::parse_file` on the path (mmap or read)
  File,
}

#[derive(Parser, Debug)]
#[command(name = "berry-dump")]
#[command(about = "Parse a Yarn Berry lockfile and dump every parsed field")]
struct Args {
  /// Path to a lockfile to parse
  #[arg(value_name = "LOCKFILE")]
  lockfile: Option<PathBuf>,

  /// Use a bundled fixture name instead of a path (reads from repo fixtures/)
  #[arg(short, long, value_name = "NAME")]
  fixture: Option<String>,

  /// Output format
  #[arg(long, value_enum, default_value_t = Format::Canonical)]
  format: Format,

  /// Parse API to go through; every API must give the identical dump
  #[arg(long, value_enum, default_value_t = Api::Str)]
  api: Api,

  /// Testing: force chunks of about this many bytes (`berry::__parse_chunked`);
  /// the dump must be identical
  #[arg(long, value_name = "BYTES")]
  chunk_target: Option<usize>,

  /// Write the dump to this file instead of stdout
  #[arg(short, long, value_name = "PATH")]
  output: Option<PathBuf>,
}

fn read_file(p: &PathBuf) -> String {
  std::fs::read_to_string(p).unwrap_or_else(|e| {
    eprintln!("failed to read {}: {e}", p.display());
    std::process::exit(2);
  })
}

fn main() {
  let args = Args::parse();

  let contents = if let Some(fixture) = &args.fixture {
    let fixtures_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
      .parent()
      .unwrap()
      .parent()
      .unwrap()
      .join("fixtures");
    let path = fixtures_dir.join(fixture);
    read_file(&path)
  } else if let Some(path) = &args.lockfile {
    read_file(path)
  } else {
    eprintln!("Provide a lockfile path or --fixture <name>");
    std::process::exit(2);
  };

  let sink = || -> BufWriter<Box<dyn Write>> {
    let sink: Box<dyn Write> = match &args.output {
      Some(path) => Box::new(std::fs::File::create(path).unwrap_or_else(|e| {
        eprintln!("failed to create {}: {e}", path.display());
        std::process::exit(2);
      })),
      None => Box::new(std::io::stdout().lock()),
    };
    BufWriter::with_capacity(1 << 20, sink)
  };
  let finish = |written: std::io::Result<()>, remaining: &str| {
    if let Err(e) = written {
      eprintln!("failed to write dump: {e}");
      std::process::exit(2);
    }
    if !remaining.trim().is_empty() {
      eprintln!("WARNING: {} bytes remaining unparsed", remaining.len());
    }
  };

  let mut buffers = berry::Buffers::new();
  if matches!(args.api, Api::Reuse)
    && let Ok(first) = berry::parse_with(&contents, &mut buffers)
  {
    buffers = first.into_buffers();
  }
  let parsed = match (args.chunk_target, args.api) {
    (Some(t), _) => berry::__parse_chunked(&contents, t),
    (None, Api::Bytes) => berry::parse_bytes(contents.as_bytes()),
    (None, Api::File) => {
      let path = args
        .lockfile
        .as_ref()
        .expect("--api file needs a LOCKFILE path");
      match berry::parse_file(path) {
        Ok(lf) => {
          let mut out = sink();
          let written = match args.format {
            Format::Canonical => berry_test::canonical::write(&mut out, &lf),
            Format::Debug => writeln!(out, "lockfile: {lf:#?}"),
          }
          .and_then(|()| out.flush());
          finish(written, lf.remaining());
          return;
        }
        Err(e) => {
          eprintln!("Parse error: {e}");
          std::process::exit(1);
        }
      }
    }
    (None, _) => berry::parse_with(&contents, &mut buffers),
  };
  let lockfile = parsed.unwrap_or_else(|e| {
    eprintln!("Parse error: {}", e.describe(&contents));
    std::process::exit(1);
  });
  let mut out = sink();
  let written = match args.format {
    Format::Canonical => berry_test::canonical::write(&mut out, &lockfile),
    Format::Debug => writeln!(out, "lockfile: {lockfile:#?}"),
  }
  .and_then(|()| out.flush());
  finish(written, lockfile.remaining());
}
