use nom::IResult;
use nom::{
  Parser,
  branch::alt,
  bytes::complete::{tag, take_until, take_while, take_while1},
  character::complete::{char, newline, space0, space1},
  combinator::{map, opt, recognize},
  multi::fold_many0,
  sequence::{delimited, preceded, terminated},
};
use smallvec::SmallVec;
use memchr::memchr;
use rustc_hash::FxHashMap;

use crate::ident::{Descriptor, Ident};
use crate::locator::Locator;
use crate::lockfile::{
  Entry, Lockfile, parse_constraints, parse_metadata, parse_resolutions, parse_yarn_header,
};
use crate::metadata::{DependencyMeta, PeerDependencyMeta};
use crate::package::{LinkType, Package};

/// Parse a package entry into a full lockfile Entry
fn parse_entry(input: &str) -> IResult<&str, Entry<'_>> {
  map(parse_package_entry, |(descriptors, package)| {
    Entry::new(descriptors, package)
  })
  .parse(input)
}

/// Entrypoint for parsing a yarn lockfile
pub fn parse_lockfile(file_contents: &str) -> IResult<&str, Lockfile<'_>> {
  let (rest, (_, _)) = parse_yarn_header(file_contents)?;
  let (rest, metadata) = parse_metadata(rest)?;

  // Consume any blank lines after metadata
  let (rest, _) = opt(newline).parse(rest)?;

  // Optionally parse resolutions and constraints if present
  let (rest, resolutions) = match parse_resolutions(rest) {
    Ok((rest2, r)) => (rest2, Some(r)),
    Err(_) => (rest, None),
  };

  let (rest, constraints) = match parse_constraints(rest) {
    Ok((rest2, c)) => (rest2, Some(c)),
    Err(_) => (rest, None),
  };

  // Parse all package entries as full Entries
  // Use fold_many0 to avoid intermediate Vec allocation from many0
  let (rest, entries) = fold_many0(parse_entry, Vec::new, |mut entries, entry| {
    entries.push(entry);
    entries
  })
  .parse(rest)?;

  // Consume any trailing content (backticks, semicolons, whitespace, etc.)
  // perf: use take_while instead of many0 - zero allocation
  let (rest, _) =
    take_while(|c: char| c == '`' || c == ';' || c == '\n' || c == ' ' || c == '\t' || c == '\r')
      .parse(rest)?;

  Ok((
    rest,
    Lockfile {
      metadata,
      entries,
      resolutions,
      constraints,
    },
  ))
}

/// Parse a single package entry from the lockfile
///
/// Example input:
///
/// ```text
/// "debug@npm:1.0.0":
///   version: 1.0.0
///   resolution: "debug@npm:1.0.0"
///   dependencies:
///     ms: 0.6.2
///   checksum: edfec8784737afbeea43cc78c3f56c33b88d3e751cc7220ae7a1c5370ff099e7352703275bdb56ea9967f92961231ce0625f8234d82259047303849671153f03
///   languageName: node
///   linkType: hard
/// ```
pub fn parse_package_entry(
  input: &str,
) -> IResult<&str, (SmallVec<[Descriptor<'_>; 4]>, Package<'_>)> {
  let (rest, descriptors) = parse_descriptor_line(input)?;
  let (rest, _) = newline.parse(rest)?; // consume newline after descriptor
  let (rest, package) = parse_package_properties(rest)?;

  Ok((rest, (descriptors, package)))
}

/// Parse a package descriptor line like: "debug@npm:1.0.0":, eslint-config-turbo@latest:, or ? "conditional@npm:1.0.0":
/// Uses `SmallVec<[Descriptor; 4]>` since most descriptor lines have 1-3 descriptors
/// Uses `SmallVec`<[Descriptor; 4]> since most descriptor lines have 1-3 descriptors
pub fn parse_descriptor_line(input: &str) -> IResult<&str, SmallVec<[Descriptor<'_>; 4]>> {
  // Check for optional '? ' prefix for wrapped-line descriptors
  let (rest, has_line_wrap_marker) = opt(tag("? ")).parse(input)?;
  let is_wrapped_line = has_line_wrap_marker.is_some();

  // Handle both quoted and unquoted descriptors
  // Optimisation: when descriptors are prefixed with ? they are often "wrapped", so we check for that first
  let (rest, descriptor_string) = if is_wrapped_line {
    // For wrapped-line descriptors, try newline-wrapped format first
    alt((
      // Handle very long descriptor lines that wrap: "very long descriptor..."\n:
      delimited(
        char('"'),
        take_until("\""),
        terminated(char('"'), preceded(newline, char(':'))),
      ),
      delimited(char('"'), take_until("\":"), tag("\":")), // Quoted: "package@npm:version":
      terminated(take_until(":"), char(':')),              // Unquoted: package@latest:
    ))
    .parse(rest)?
  } else {
    // For normal descriptors, skip the newline-wrapped check entirely (performance optimisation)
    alt((
      delimited(char('"'), take_until("\":"), tag("\":")), // Quoted: "package@npm:version":
      terminated(take_until(":"), char(':')),              // Unquoted: package@latest:
    ))
    .parse(rest)?
  };

  // Parse first descriptor (borrowed data)
  let (remaining, first_data) = parse_single_descriptor(descriptor_string)?;

  // Parse subsequent descriptors into SmallVec of borrowed data
  // Most descriptor lines have 1-3 descriptors, so capacity of 3 for additional is plenty
  let (remaining, additional_data) = fold_many0(
    preceded((space0, char(','), space0), parse_single_descriptor),
    SmallVec::<[_; 3]>::new,
    |mut acc, data| {
      acc.push(data);
      acc
    },
  )
  .parse(remaining)?;

  if !remaining.is_empty() {
    // Return an error if descriptor parsing didn't consume the entire string
    // This ensures we catch parsing bugs early rather than silently ignoring content
    return Err(nom::Err::Error(nom::error::Error::new(
      remaining,
      nom::error::ErrorKind::Complete,
    )));
  }

  // Convert all borrowed data to Descriptors in a single pass
  // Using SmallVec<[Descriptor; 4]> avoids heap allocation for common case (1-4 descriptors)
  let mut descriptors: SmallVec<[Descriptor<'_>; 4]> =
    SmallVec::with_capacity(1 + additional_data.len());
  descriptors.push(convert_to_descriptor(first_data));
  for data in additional_data {
    descriptors.push(convert_to_descriptor(data));
  }

  Ok((rest, descriptors))
}

/// Convert parsed descriptor data (borrowed strings) to Descriptor
/// All strings remain borrowed from the input - zero allocation!
#[inline]
fn convert_to_descriptor<'a>((name_part, full_range): (&'a str, &'a str)) -> Descriptor<'a> {
fn convert_to_descriptor<'a>(
  (name_part, _protocol, range): (&'a str, &'a str, &'a str),
) -> Descriptor<'a> {
  let ident = parse_name_to_ident(name_part);
  Descriptor::new(ident, full_range)
  // For the range, we need to find or construct the full range string
  // Since we're borrowing, we need to handle this carefully
  // The range is either just `range` (no protocol) or we need to find it in the original
  // Actually, the original descriptor_string contains the full range, so we can slice it
  // But for now, let's use the parsed parts
  // If protocol is empty, range is the full range
  // If protocol is not empty, we need to reconstruct or find original
  //
  // For zero-copy, we need to work with the original string
  // The simplest approach: find the @ in name_part and take everything after
  // But that's already done by parse_single_descriptor
  //
  // The issue: Descriptor::new takes range_raw: &'a str
  // We have protocol and range separately, need to combine them
  // But we can't allocate! So we need to find the original substring
  //
  // Actually, looking at how parse_single_descriptor works, the range returned
  // is the full range including protocol:selector or just selector
  // Let me check - no, it returns (name_part, protocol, range) separately
  //
  // For true zero-copy, we'd need to restructure to keep the original range string
  // For now, let's construct it - this is ONE allocation per descriptor
  // Still much better than before
  // TODO: Optimize this to avoid allocation by returning full range slice from parse_single_descriptor
  // For now, use the range part (which is typically just the selector)
  Descriptor::new(ident, range)
}

/// Parse a single descriptor string like `"debug@npm:1.0.0"`, `"c@*"`, or `"is-odd@patch:is-odd@npm%3A3.0.1#~/.yarn/patches/is-odd-npm-3.0.1-93c3c3f41b.patch"`
/// Returns borrowed strings to avoid allocations during parsing
/// Returns (`name_part`, `full_range`) where `full_range` includes protocol if present (e.g., "npm:^1.0.0")
fn parse_single_descriptor(input: &str) -> IResult<&str, (&str, &str)> {
  // Parse package name first
  let (after_name, name_part) = parse_package_name(input)?;
/// Returns `(name_part, full_range)` where `full_range` includes protocol if present
fn parse_single_descriptor(input: &str) -> IResult<&str, (&str, &str, &str)> {
  // Find the @ that separates name from range
  // For scoped packages like @babel/core@npm:1.0.0, we need the LAST @ before range
  // Strategy: find package name first, then take rest as range

  // Parse @ separator
  let (after_at, _) = char('@').parse(after_name)?;

  // Everything after @ until comma or quote is the full range (including protocol if present)
  let (remaining, full_range) = take_while1(|c: char| c != ',' && c != '"').parse(after_at)?;

  // Trim trailing whitespace from range to match JS parser behavior (e.g., "npm:^1.0.4 " -> "npm:^1.0.4")
  let full_range = full_range.trim_end();

  Ok((remaining, (name_part, full_range)))
}

/// Helper function to parse name part into Ident (zero-copy)
#[inline]
fn parse_name_to_ident(name_part: &str) -> Ident<'_> {
  name_part.strip_prefix('@').map_or_else(
    || Ident::new(None, name_part),
    |stripped| {
      // Scoped package: @babel/code-frame -> scope="@babel", name="code-frame"
      stripped.split_once('/').map_or_else(
        // Malformed scoped package (no slash), treat as simple name
        || Ident::new(None, name_part),
        |(scope_without_at, name)| {
          // We need to include @ in scope, but we only have scope_without_at
          // The original name_part has it: "@scope/name"
          // scope starts at index 0, ends at index 1+scope_without_at.len()
          let scope_end = 1 + scope_without_at.len();
          let scope = &name_part[..scope_end];
          Ident::new(Some(scope), name)
        },
      )
    },
  )
}

/// Parse a package name, which can be scoped (@babel/code-frame) or simple (debug)
fn parse_package_name(input: &str) -> IResult<&str, &str> {
  alt((
    // Scoped package: @scope/name (both scope and name can contain dots)
    recognize((
      char('@'),
      take_while1(|c: char| c.is_alphanumeric() || c == '-' || c == '_' || c == '.'),
      char('/'),
      take_while1(|c: char| c.is_alphanumeric() || c == '-' || c == '_' || c == '.'),
    )),
    // non-scoped package name: debug (can contain dots like fs.realpath)
    take_while1(|c: char| c.is_alphanumeric() || c == '-' || c == '_' || c == '.'),
  ))
  .parse(input)
}

/// Parse indented key-value properties for a package
/// perf: use `fold_many0` to build the Package directly without intermediate Vec allocation
/// Parse protocol part like npm, workspace, git, etc.
fn parse_protocol(input: &str) -> IResult<&str, &str> {
  // Support common protocol tokens including git+ssh
  take_while1(|c: char| c.is_alphanumeric() || c == '-' || c == '_' || c == '+').parse(input)
}

/// Parse patch range which can be complex like:
/// - "is-odd@npm%3A3.0.1#~/.yarn/patches/is-odd-npm-3.0.1-93c3c3f41b.patch"
/// - "typescript@npm%3A^5.8.3#optional!builtin<compat/typescript>"
fn parse_patch_range(input: &str) -> IResult<&str, &str> {
  take_while1(|c: char| c != ',' && c != '"').parse(input)
}

/// Parse indented key-value properties for a package using high-performance line scanning
pub fn parse_package_properties(input: &str) -> IResult<&str, Package<'_>> {
  let mut package = Package::new("unknown", LinkType::Hard);
  let mut remaining = input;

  loop {
    let line_end = memchr(b'\n', remaining.as_bytes()).unwrap_or(remaining.len());
    let line = &remaining[..line_end];

    // Check if blank line (end of block)
    if line.trim().is_empty() {
      remaining = if line_end < remaining.len() {
        &remaining[line_end + 1..]
      } else {
        &remaining[line_end..]
      };
      break;
    }

    // Skip to next line
    remaining = if line_end < remaining.len() {
      &remaining[line_end + 1..]
    } else {
      &remaining[line_end..]
    };

    // Parse the line based on content
    if let Some(rest) = line.strip_prefix("  ") {
      if let Some(rest) = rest.strip_prefix("version:") {
        package.version = Some(rest.trim().trim_matches('"'));
      } else if let Some(rest) = rest.strip_prefix("resolution:") {
        let raw = rest.trim().trim_matches('"');
        let at_index = if raw.starts_with('@') {
          raw.find('/').and_then(|slash| raw[slash..].find('@').map(|i| slash + i))
        } else {
          raw.find('@')
        };
        if let Some(at_index) = at_index {
          let (name_part, reference_with_at) = raw.split_at(at_index);
          let ident = parse_name_to_ident(name_part);
          let reference = reference_with_at.trim_start_matches('@');
          package.resolution_locator = Some(Locator::new(ident, reference));
        }
        package.resolution = Some(raw);
      } else if let Some(rest) = rest.strip_prefix("languageName:") {
        package.language_name = rest.trim();
      } else if let Some(rest) = rest.strip_prefix("linkType:") {
        let link_type_str = rest.trim();
        package.link_type =
          LinkType::try_from(link_type_str).unwrap_or_else(|()| panic!("Invalid link type: {link_type_str}"));
      } else if let Some(rest) = rest.strip_prefix("checksum:") {
        package.checksum = Some(rest.trim());
      } else if let Some(rest) = rest.strip_prefix("conditions:") {
        package.conditions = Some(rest.trim());
      } else if rest.strip_prefix("dependencies:").is_some() {
        let (new_remaining, dependencies) = parse_dependencies_block_scanner(remaining)?;
        remaining = new_remaining;
        let mut deps_map = FxHashMap::with_capacity_and_hasher(dependencies.len(), rustc_hash::FxBuildHasher);
        for (dep_name, dep_range) in dependencies {
          let ident = parse_name_to_ident(dep_name);
          let descriptor = Descriptor::new(ident, dep_range);
          deps_map.insert(ident, descriptor);
        }
        package.dependencies = deps_map;
      } else if rest.strip_prefix("peerDependencies:").is_some() {
        let (new_remaining, peer_dependencies) = parse_peer_dependencies_block_scanner(remaining)?;
        remaining = new_remaining;
        let mut peer_deps_map = FxHashMap::with_capacity_and_hasher(peer_dependencies.len(), rustc_hash::FxBuildHasher);
        for (dep_name, dep_range) in peer_dependencies {
          let ident = parse_name_to_ident(dep_name);
          let descriptor = Descriptor::new(ident, dep_range);
          peer_deps_map.insert(ident, descriptor);
        }
        package.peer_dependencies = peer_deps_map;
      } else if rest.strip_prefix("bin:").is_some() {
        let (new_remaining, binaries) = parse_bin_block_scanner(remaining)?;
        remaining = new_remaining;
        let mut bin_map = FxHashMap::with_capacity_and_hasher(binaries.len(), rustc_hash::FxBuildHasher);
        for (bin_name, bin_path) in binaries {
          bin_map.insert(bin_name, bin_path);
        }
        package.bin = bin_map;
      } else if rest.strip_prefix("dependenciesMeta:").is_some() {
        let (new_remaining, meta) = parse_dependencies_meta_block_scanner(remaining)?;
        remaining = new_remaining;
        let mut meta_map = FxHashMap::with_capacity_and_hasher(meta.len(), rustc_hash::FxBuildHasher);
        for (dep_name, dep_meta) in meta {
          let ident = parse_name_to_ident(dep_name);
          meta_map.insert(ident, Some(dep_meta));
        }
        package.dependencies_meta = meta_map;
      } else if rest.strip_prefix("peerDependenciesMeta:").is_some() {
        let (new_remaining, meta) = parse_peer_dependencies_meta_block_scanner(remaining)?;
        remaining = new_remaining;
        let mut peer_meta_map = FxHashMap::with_capacity_and_hasher(meta.len(), rustc_hash::FxBuildHasher);
        for (dep_name, dep_meta) in meta {
          let ident = parse_name_to_ident(dep_name);
          peer_meta_map.insert(ident, dep_meta);
        }
        package.peer_dependencies_meta = peer_meta_map;
      }
      package
    },
  )
  .parse(input)?;

  // Consume any trailing whitespace and blank lines
  let (rest, ()) = fold_many0(
    alt((tag("\n"), tag(" "), tag("\t"), tag("\r"))),
    || (),
    |(), _| (),
  )
  .parse(rest)?;

  Ok((rest, package))
}

/// Parse a single property line with 2-space indentation
fn parse_property_line(input: &str) -> IResult<&str, PropertyValue<'_>> {
  // Try simple property first
  if let Ok((rest, (key, value))) = parse_simple_property(input) {
    return Ok((rest, PropertyValue::Simple(key, value)));
  }

  // Try dependencies block
  if let Ok((rest, dependencies)) = parse_dependencies_block(input) {
    return Ok((rest, PropertyValue::Dependencies(dependencies)));
  }

  // Try peer dependencies block
  if let Ok((rest, peer_dependencies)) = parse_peer_dependencies_block(input) {
    return Ok((rest, PropertyValue::PeerDependencies(peer_dependencies)));
  }

  // Try bin block
  if let Ok((rest, binaries)) = parse_bin_block(input) {
    return Ok((rest, PropertyValue::Bin(binaries)));
  }

  // Try dependenciesMeta block
  if let Ok((rest, meta)) = parse_dependencies_meta_block(input) {
    return Ok((rest, PropertyValue::DependenciesMeta(meta)));
  }

  // Try peerDependenciesMeta block
  if let Ok((rest, meta)) = parse_peer_dependencies_meta_block(input) {
    return Ok((rest, PropertyValue::PeerDependenciesMeta(meta)));
  }

  // If nothing matches, return an error
  Err(nom::Err::Error(nom::error::Error::new(
    input,
    nom::error::ErrorKind::Alt,
  )))
}

/// Enum to represent different types of property values
#[derive(Debug)]
enum PropertyValue<'a> {
  Simple(&'a str, &'a str),
  Dependencies(Vec<(&'a str, &'a str)>),
  PeerDependencies(Vec<(&'a str, &'a str)>),
  Bin(Vec<(&'a str, &'a str)>),
  DependenciesMeta(Vec<(&'a str, DependencyMeta)>),
  PeerDependenciesMeta(Vec<(&'a str, PeerDependencyMeta)>),
}

/// Parse a simple key-value property line
pub fn parse_simple_property(input: &str) -> IResult<&str, (&str, &str)> {
  let (rest, (_, key, _, _, value, _)) = (
    tag("  "), // 2-space indentation
    take_while1(|c: char| c.is_alphanumeric() || c == '_'),
    char(':'),
    space1,
    is_not("\r\n"),
    opt(newline),
  )
    .parse(input)?;

  Ok((rest, (key, value)))
}

/// Parse a dependencies block
fn parse_dependencies_block(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
  let (rest, (_, _, dependencies)) = (
    tag("  dependencies:"),
    newline,
    fold_many0(parse_dependency_line, Vec::new, |mut acc, item| {
      acc.push(item);
      acc
    }),
  )
    .parse(input)?;

  Ok((rest, dependencies))
}

/// Parse a peerDependencies block
fn parse_peer_dependencies_block(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
  let (rest, (_, _, peer_dependencies)) = (
    tag("  peerDependencies:"),
    newline,
    fold_many0(parse_dependency_line, Vec::new, |mut acc, item| {
      acc.push(item);
      acc
    }),
  )
    .parse(input)?;

  Ok((rest, peer_dependencies))
}

/// Parse a bin block
fn parse_bin_block(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
  let (rest, (_, _, binaries)) = (
    tag("  bin:"),
    newline,
    fold_many0(parse_bin_line, Vec::new, |mut acc, item| {
      acc.push(item);
      acc
    }),
  )
    .parse(input)?;

  Ok((rest, binaries))
}

/// Parse a dependenciesMeta block
fn parse_dependencies_meta_block(input: &str) -> IResult<&str, Vec<(&str, DependencyMeta)>> {
  let (rest, (_, _, meta)) = (
    tag("  dependenciesMeta:"),
    newline,
    fold_many0(
      alt((
        parse_dependency_meta_entry_inline,
        parse_dependency_meta_entry_nested,
      )),
      Vec::new,
      |mut acc, item| {
        acc.push(item);
        acc
      },
    ),
  )
    .parse(input)?;

  Ok((rest, meta))
}

/// Parse a peerDependenciesMeta block
fn parse_peer_dependencies_meta_block(
  input: &str,
) -> IResult<&str, Vec<(&str, PeerDependencyMeta)>> {
  let (rest, (_, _, meta)) = (
    tag("  peerDependenciesMeta:"),
    newline,
    fold_many0(
      alt((
        parse_peer_dependency_meta_entry_inline,
        parse_peer_dependency_meta_entry_nested,
      )),
      Vec::new,
      |mut acc, item| {
        acc.push(item);
        acc
      },
    ),
  )
    .parse(input)?;

  Ok((rest, meta))
}

/// Parse a single dependency line with 4-space indentation
fn parse_dependency_line(input: &str) -> IResult<&str, (&str, &str)> {
  let (rest, (_, dep_name, _, _, dep_range, _)) = (
    tag("    "),
    alt((
      delimited(
        char('"'),
        take_while1(|c: char| {
          c.is_alphanumeric() || c == '-' || c == '_' || c == '@' || c == '/' || c == '.'
        }),
        char('"'),
      ),
      take_while1(|c: char| {
        c.is_alphanumeric() || c == '-' || c == '_' || c == '@' || c == '/' || c == '.'
      }),
    )),
    char(':'),
    space1,
    take_until("\n"),
    opt(newline),
  )
    .parse(input)?;

  let clean_range = dep_range.trim().trim_matches('"');
  Ok((rest, (dep_name, clean_range)))
}

/// Parse a single bin line with 4-space indentation
fn parse_bin_line(input: &str) -> IResult<&str, (&str, &str)> {
  let (rest, (_, bin_name, _, _, bin_path, _)) = (
    tag("    "),
    take_while1(|c: char| {
      c.is_alphanumeric() || c == '-' || c == '_' || c == '@' || c == '/' || c == '.'
    }),
    char(':'),
    space1,
    is_not("\r\n"),
    newline,
  )
    .parse(input)?;

  let clean_path = bin_path.trim().trim_matches('"');
  Ok((rest, (bin_name, clean_path)))
}

/// Parse a single dependency meta line with inline format
fn parse_dependency_meta_entry_inline(input: &str) -> IResult<&str, (&str, DependencyMeta)> {
  let (rest, (_, dep_name, _, _, meta_content, _)) = (
    tag("    "),
    alt((
      delimited(
        char('"'),
        take_while1(|c: char| {
          c.is_alphanumeric() || c == '-' || c == '_' || c == '@' || c == '/' || c == '.'
        }),
        char('"'),
      ),
      take_while1(|c: char| {
        c.is_alphanumeric() || c == '-' || c == '_' || c == '@' || c == '/' || c == '.'
      }),
    )),
    char(':'),
    space1,
    parse_meta_object,
    opt(newline),
  )
    .parse(input)?;

  Ok((rest, (dep_name, meta_content)))
}

/// Parse a single dependency meta entry with nested format
fn parse_dependency_meta_entry_nested(input: &str) -> IResult<&str, (&str, DependencyMeta)> {
  let (rest, (_, dep_name, _, _, meta_content, _)) = (
    tag("    "),
    alt((
      delimited(
        char('"'),
        take_while1(|c: char| {
          c.is_alphanumeric() || c == '-' || c == '_' || c == '@' || c == '/' || c == '.'
        }),
        char('"'),
      ),
      take_while1(|c: char| {
        c.is_alphanumeric() || c == '-' || c == '_' || c == '@' || c == '/' || c == '.'
      }),
    )),
    char(':'),
    newline,
    parse_dependency_meta_object_indented,
    opt(newline),
  )
    .parse(input)?;

  Ok((rest, (dep_name, meta_content)))
}

/// Parse a dependency meta object with 6-space indentation
fn parse_dependency_meta_object_indented(input: &str) -> IResult<&str, DependencyMeta> {
  fn parse_indented_bool_line<'a>(
    prop: &'static str,
  ) -> impl Fn(&'a str) -> IResult<&'a str, bool> {
    move |input: &str| {
      let (rest, (_, _, _, _, value, _)) = (
        tag("      "),
        tag(prop),
        char(':'),
        space1,
        alt((tag("true"), tag("false"))),
        newline,
      )
        .parse(input)?;
      Ok((rest, value == "true"))
    }
  }

  Ok((remaining, package))
}

fn parse_dependencies_block_scanner(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
  let mut dependencies = Vec::new();
  let mut remaining = input;

  loop {
    let line_end = memchr(b'\n', remaining.as_bytes()).unwrap_or(remaining.len());
    let line = &remaining[..line_end];

    if !line.starts_with("    ") {
      break;
    }

    if line.trim().is_empty() {
      remaining = if line_end < remaining.len() {
        &remaining[line_end + 1..]
      } else {
        &remaining[line_end..]
      };
      break;
    }

    let rest = &line[4..];
    if let Some(colon_idx) = rest.find(':') {
      let dep_name = rest[..colon_idx].trim();
      let dep_range = rest[colon_idx + 1..].trim().trim_matches('"');
      dependencies.push((dep_name, dep_range));
    }

    remaining = if line_end < remaining.len() {
      &remaining[line_end + 1..]
    } else {
      &remaining[line_end..]
    };
  }

  Ok((remaining, dependencies))
}

fn parse_peer_dependencies_block_scanner(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
  parse_dependencies_block_scanner(input)
}

fn parse_bin_block_scanner(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
  let mut binaries = Vec::new();
  let mut remaining = input;

  loop {
    let line_end = memchr(b'\n', remaining.as_bytes()).unwrap_or(remaining.len());
    let line = &remaining[..line_end];

    if !line.starts_with("    ") {
      break;
    }

    if line.trim().is_empty() {
      remaining = if line_end < remaining.len() {
        &remaining[line_end + 1..]
      } else {
        &remaining[line_end..]
      };
      break;
    }

    let rest = &line[4..];
    if let Some(colon_idx) = rest.find(':') {
      let bin_name = rest[..colon_idx].trim();
      let bin_path = rest[colon_idx + 1..].trim().trim_matches('"');
      binaries.push((bin_name, bin_path));
    }

    remaining = if line_end < remaining.len() {
      &remaining[line_end + 1..]
    } else {
      &remaining[line_end..]
    };
  }

  Ok((remaining, binaries))
}

fn parse_dependencies_meta_block_scanner(input: &str) -> IResult<&str, Vec<(&str, DependencyMeta)>> {
  let mut meta = Vec::new();
  let mut remaining = input;

  loop {
    let line_end = memchr(b'\n', remaining.as_bytes()).unwrap_or(remaining.len());
    let line = &remaining[..line_end];

    if !line.starts_with("    ") {
      break;
    }

    if line.trim().is_empty() {
      remaining = if line_end < remaining.len() {
        &remaining[line_end + 1..]
      } else {
        &remaining[line_end..]
      };
      break;
    }

    let rest = &line[4..];

    if let Some(colon_idx) = rest.find(':') {
      let dep_name = rest[..colon_idx].trim().trim_matches('"');
      let value_part = rest[colon_idx + 1..].trim();

      if value_part.starts_with('{') {
        if let Ok((_, meta_obj)) = parse_meta_object(value_part) {
          meta.push((dep_name, meta_obj));
        }
      } else {
        remaining = if line_end < remaining.len() {
          &remaining[line_end + 1..]
        } else {
          &remaining[line_end..]
        };

        let mut dep_meta = DependencyMeta {
          built: None,
          optional: None,
          unplugged: None,
        };

        loop {
          let meta_line_end = memchr(b'\n', remaining.as_bytes()).unwrap_or(remaining.len());
          let meta_line = &remaining[..meta_line_end];

          if !meta_line.starts_with("      ") || meta_line.trim().is_empty() {
            break;
          }

          let meta_rest = &meta_line[6..];
          if let Some(colon_idx) = meta_rest.find(':') {
            let prop_name = meta_rest[..colon_idx].trim();
            let prop_value = meta_rest[colon_idx + 1..].trim();
            match prop_name {
              "built" => dep_meta.built = Some(prop_value == "true"),
              "optional" => dep_meta.optional = Some(prop_value == "true"),
              "unplugged" => dep_meta.unplugged = Some(prop_value == "true"),
              _ => {}
            }
          }

          remaining = if meta_line_end < remaining.len() {
            &remaining[meta_line_end + 1..]
          } else {
            &remaining[meta_line_end..]
          };
        }

        meta.push((dep_name, dep_meta));
        continue;
      }
    }

    remaining = if line_end < remaining.len() {
      &remaining[line_end + 1..]
    } else {
      &remaining[line_end..]
    };
  }

  Ok((remaining, meta))
}

fn parse_peer_dependencies_meta_block_scanner(input: &str) -> IResult<&str, Vec<(&str, PeerDependencyMeta)>> {
  let mut meta = Vec::new();
  let mut remaining = input;

  loop {
    let line_end = memchr(b'\n', remaining.as_bytes()).unwrap_or(remaining.len());
    let line = &remaining[..line_end];

    if !line.starts_with("    ") {
      break;
    }

    if line.trim().is_empty() {
      remaining = if line_end < remaining.len() {
        &remaining[line_end + 1..]
      } else {
        &remaining[line_end..]
      };
      break;
    }

    let rest = &line[4..];

    if let Some(colon_idx) = rest.find(':') {
      let dep_name = rest[..colon_idx].trim().trim_matches('"');
      let value_part = rest[colon_idx + 1..].trim();

      if value_part.starts_with('{') {
        if let Ok((_, meta_obj)) = parse_peer_meta_object(value_part) {
          meta.push((dep_name, meta_obj));
        }
        remaining = if line_end < remaining.len() {
          &remaining[line_end + 1..]
        } else {
          &remaining[line_end..]
        };
      } else {
        remaining = if line_end < remaining.len() {
          &remaining[line_end + 1..]
        } else {
          &remaining[line_end..]
        };

        let mut peer_meta = PeerDependencyMeta { optional: false };

        loop {
          let meta_line_end = memchr(b'\n', remaining.as_bytes()).unwrap_or(remaining.len());
          let meta_line = &remaining[..meta_line_end];

          if !meta_line.starts_with("      ") || meta_line.trim().is_empty() {
            break;
          }

          let meta_rest = &meta_line[6..];
          if let Some(colon_idx) = meta_rest.find(':') {
            let prop_name = meta_rest[..colon_idx].trim();
            let prop_value = meta_rest[colon_idx + 1..].trim();
            if prop_name == "optional" {
              peer_meta.optional = prop_value == "true";
            }
          }

          remaining = if meta_line_end < remaining.len() {
            &remaining[meta_line_end + 1..]
          } else {
            &remaining[meta_line_end..]
          };
        }

        meta.push((dep_name, peer_meta));
      }
    } else {
      remaining = if line_end < remaining.len() {
        &remaining[line_end + 1..]
      } else {
        &remaining[line_end..]
      };
    }
  }

  Ok((remaining, meta))
}


/// Parse a peer dependency meta object with inline format
fn parse_peer_meta_object(input: &str) -> IResult<&str, PeerDependencyMeta> {
  let (rest, _) = char('{')(input)?;
  let (rest, _) = space0(rest)?;
  let (rest, optional) = parse_bool_property_inline("optional")(rest)?;
  let (rest, _) = space0(rest)?;
  let (rest, _) = char('}')(rest)?;

  Ok((rest, PeerDependencyMeta { optional }))
}

/// Parse a boolean property for inline format
fn parse_bool_property_inline(prop_name: &str) -> impl Fn(&str) -> IResult<&str, bool> + '_ {
  move |input| {
    let (rest, (_, _, _, value)) = (
      tag(prop_name),
      char(':'),
      space1,
      alt((tag("true"), tag("false"))),
    )
      .parse(input)?;

    Ok((rest, value == "true"))
  }
}

/// Parse a dependency meta object like "{ built: true, optional: false }"
fn parse_meta_object(input: &str) -> IResult<&str, DependencyMeta> {
  let (rest, _) = char('{')(input)?;
  let (rest, _) = space0(rest)?;

  let (rest, built) = opt(parse_bool_property_inline("built")).parse(rest)?;
  let (rest, _) = opt((space0, char(','), space0)).parse(rest)?;

  let (rest, optional) = opt(parse_bool_property_inline("optional")).parse(rest)?;
  let (rest, _) = opt((space0, char(','), space0)).parse(rest)?;

  let (rest, unplugged) = opt(parse_bool_property_inline("unplugged")).parse(rest)?;
  let (rest, _) = space0(rest)?;

  let (rest, _) = char('}')(rest)?;

  Ok((
    rest,
    DependencyMeta {
      built,
      optional,
      unplugged,
    },
  ))
}

#[cfg(test)]
mod tests {
  use super::*;


  #[test]
  fn test_parse_descriptor_line_simple() {
    let input = r#""debug@npm:1.0.0":"#;
    let result = parse_descriptor_line(input);
    assert!(result.is_ok());
    let (remaining, descriptors) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 1);

    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "debug");
    assert_eq!(descriptor.ident().scope(), None);
  }

  #[test]
  fn test_parse_descriptor_line_scoped_package() {
    let input = r#""@babel/code-frame@npm:7.12.11":"#;
    let result = parse_descriptor_line(input);
    assert!(result.is_ok());
    let (remaining, descriptors) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 1);

    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "code-frame");
    assert_eq!(descriptor.ident().scope(), Some("@babel"));
  }

  #[test]
  fn test_parse_package_properties_minimal() {
    let input = r#"  version: 1.0.0
  resolution: "debug@npm:1.0.0"
  languageName: node
  linkType: hard
"#;
    let result = parse_package_properties(input);
    assert!(result.is_ok());
    let (remaining, package) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(package.version, Some("1.0.0"));
    assert_eq!(package.resolution, Some("debug@npm:1.0.0"));
    assert_eq!(package.language_name, "node");
    assert_eq!(package.link_type, LinkType::Hard);
  }

  #[test]
  fn test_parse_multiple_packages() {
    let input = r#"# This file is generated by running "yarn install" inside your project.
# Manual changes might be lost - proceed with caution!

__metadata:
  version: 8
  cacheKey: 10

"@actions/http-client@npm:^2.2.0":
  version: 2.2.3
  resolution: "@actions/http-client@npm:2.2.3"
  languageName: node
  linkType: hard

"@actions/io@npm:^1.0.1, @actions/io@npm:^1.1.3":
  version: 1.1.3
  resolution: "@actions/io@npm:1.1.3"
  languageName: node
  linkType: hard
"#;

    let result = parse_lockfile(input);
    assert!(result.is_ok());
    let (remaining, lockfile) = result.unwrap();
    assert_eq!(lockfile.entries.len(), 2);
    assert!(remaining.is_empty());
  }

  #[test]
  fn test_parse_descriptor_line_multi_descriptor() {
    let input = r#""c@*, c@workspace:packages/c":"#;
    let result = parse_descriptor_line(input);
    assert!(result.is_ok());
    let (remaining, descriptors) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 2);

    assert_eq!(descriptors[0].ident().name(), "c");
    assert_eq!(descriptors[1].ident().name(), "c");
  }

  #[test]
  fn test_parse_peer_dependencies_meta_real_format() {
    let input = r"  peerDependenciesMeta:
    graphql-ws:
      optional: true
    react:
      optional: true
";

    let result = parse_peer_dependencies_meta_block(input);
    assert!(result.is_ok());
    let (remaining, meta) = result.unwrap();
    assert_eq!(meta.len(), 2);
    assert!(remaining.is_empty());
  }

  #[test]
  fn test_descriptor_range_includes_npm_protocol() {
    let input = r#""debug@npm:^4.3.0":"#;
    let result = parse_descriptor_line(input);
    assert!(result.is_ok());
    let (_, descriptors) = result.unwrap();
    assert_eq!(descriptors.len(), 1);

    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "debug");
    assert_eq!(descriptor.range(), "npm:^4.3.0");
  }

  #[test]
  fn test_descriptor_range_includes_npm_protocol_scoped() {
    let input = r#""@babel/core@npm:^7.0.0":"#;
    let result = parse_descriptor_line(input);
    assert!(result.is_ok());
    let (_, descriptors) = result.unwrap();
    assert_eq!(descriptors.len(), 1);

    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "core");
    assert_eq!(descriptor.ident().scope(), Some("@babel"));
    assert_eq!(descriptor.range(), "npm:^7.0.0");
  }

  #[test]
  fn test_descriptor_range_no_protocol() {
    let input = r#""c@*":"#;
    let result = parse_descriptor_line(input);
    assert!(result.is_ok());
    let (_, descriptors) = result.unwrap();
    assert_eq!(descriptors.len(), 1);

    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "c");
    assert_eq!(descriptor.range(), "*");
  }

  #[test]
  fn test_descriptor_range_workspace_protocol() {
    let input = r#""my-package@workspace:packages/my-package":"#;
    let result = parse_descriptor_line(input);
    assert!(result.is_ok());
    let (_, descriptors) = result.unwrap();
    assert_eq!(descriptors.len(), 1);

    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "my-package");
    assert_eq!(descriptor.range(), "workspace:packages/my-package");
  }

  #[test]
  fn test_descriptor_range_multiple_with_npm_protocol() {
    let input = r#""prompts@npm:^2.0.1, prompts@npm:^2.4.2":"#;
    let result = parse_descriptor_line(input);
    assert!(result.is_ok());
    let (_, descriptors) = result.unwrap();
    assert_eq!(descriptors.len(), 2);

    assert_eq!(descriptors[0].ident().name(), "prompts");
    assert_eq!(descriptors[0].range(), "npm:^2.0.1");

    assert_eq!(descriptors[1].ident().name(), "prompts");
    assert_eq!(descriptors[1].range(), "npm:^2.4.2");
  }

  #[test]
  fn test_descriptor_range_mixed_protocols() {
    let input = r#""c@*, c@workspace:packages/c":"#;
    let result = parse_descriptor_line(input);
    assert!(result.is_ok());
    let (_, descriptors) = result.unwrap();
    assert_eq!(descriptors.len(), 2);

    assert_eq!(descriptors[0].ident().name(), "c");
    assert_eq!(descriptors[0].range(), "*");

    assert_eq!(descriptors[1].ident().name(), "c");
    assert_eq!(descriptors[1].range(), "workspace:packages/c");
  }

  #[test]
  fn test_descriptor_range_patch_protocol() {
    let input = r#""is-odd@patch:is-odd@npm%3A3.0.1#~/.yarn/patches/is-odd.patch":"#;
    let result = parse_descriptor_line(input);
    assert!(result.is_ok());
    let (_, descriptors) = result.unwrap();
    assert_eq!(descriptors.len(), 1);

    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "is-odd");
    assert_eq!(
      descriptor.range(),
      "patch:is-odd@npm%3A3.0.1#~/.yarn/patches/is-odd.patch"
    );
  }

  #[test]
  fn test_descriptor_range_trims_trailing_whitespace() {
    // Some lockfiles have trailing spaces in descriptor ranges (e.g., "@oclif/screen@npm^1.0.4 ")
    let input = r#""@oclif/screen@npm:^1.0.4 ":"#;
    let result = parse_descriptor_line(input);
    assert!(result.is_ok());
    let (_, descriptors) = result.unwrap();
    assert_eq!(descriptors.len(), 1);

    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "screen");
    assert_eq!(descriptor.ident().scope(), Some("@oclif"));
    // Trailing whitespace should be trimmed to match JS parser behavior
    assert_eq!(descriptor.range(), "npm:^1.0.4");
  }
}
