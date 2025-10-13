use nom::IResult;
use nom::{
  Parser,
  branch::alt,
  bytes::complete::{is_not, tag, take_until, take_while1},
  character::complete::{char, newline, space0, space1},
  combinator::{map, opt, recognize},
  multi::{fold_many0, many0},
  sequence::{delimited, preceded, terminated},
};

use crate::ident::{Descriptor, Ident};
use crate::locator::Locator;
use crate::lockfile::{
  Entry, Lockfile, parse_constraints, parse_metadata, parse_resolutions, parse_yarn_header,
};
use crate::metadata::{DependencyMeta, PeerDependencyMeta};
use crate::package::{LinkType, Package};

/// Parse a package entry into a full lockfile Entry
fn parse_entry(input: &str) -> IResult<&str, Entry> {
  map(parse_package_entry, |(descriptors, package)| {
    Entry::new(descriptors, package)
  })
  .parse(input)
}

/// Entrypoint for parsing a yarn lockfile
pub fn parse_lockfile(file_contents: &str) -> IResult<&str, Lockfile> {
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
  let (rest, entries) = many0(parse_entry).parse(rest)?;

  // Consume any trailing content (backticks, semicolons, whitespace, etc.)
  let (rest, _) = many0(alt((
    tag("`"),
    tag(";"),
    tag("\n"),
    tag(" "),
    tag("\t"),
    tag("\r"),
  )))
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
pub fn parse_package_entry(input: &str) -> IResult<&str, (Vec<Descriptor>, Package)> {
  let (rest, descriptors) = parse_descriptor_line(input)?;
  let (rest, _) = newline.parse(rest)?; // consume newline after descriptor
  let (rest, package) = parse_package_properties(rest)?;

  Ok((rest, (descriptors, package)))
}

/// Parse a package descriptor line like: "debug@npm:1.0.0":, eslint-config-turbo@latest:, or ? "conditional@npm:1.0.0":
pub fn parse_descriptor_line(input: &str) -> IResult<&str, Vec<Descriptor>> {
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
      terminated(take_until(":"), char(':')), // Unquoted: package@latest:
    ))
    .parse(rest)?
  } else {
    // For normal descriptors, skip the newline-wrapped check entirely (performance optimisation)
    alt((
      delimited(char('"'), take_until("\":"), tag("\":")), // Quoted: "package@npm:version":
      terminated(take_until(":"), char(':')), // Unquoted: package@latest:
    ))
    .parse(rest)?
  };

  // Parse comma-separated descriptors using fold_many0 to avoid allocations
  let (remaining, descriptor_data) = {
    // Parse first descriptor
    let (remaining, first_descriptor) = parse_single_descriptor(descriptor_string)?;

    // Parse subsequent descriptors with separators
    let (remaining, descriptors) = fold_many0(
      preceded((space0, char(','), space0), parse_single_descriptor),
      Vec::new,
      |mut acc, descriptor| {
        acc.push(descriptor);
        acc
      },
    )
    .parse(remaining)?;

    // Combine first descriptor with the rest
    let mut all_descriptors = vec![first_descriptor];
    all_descriptors.extend(descriptors);

    (remaining, all_descriptors)
  };

  // Convert borrowed strings to owned Descriptors (only allocation point)
  let descriptors: Vec<Descriptor> = descriptor_data
    .into_iter()
    .map(|(name_part, protocol, range)| {
      let ident = parse_name_to_ident(name_part);
      let full_range = if protocol.is_empty() {
        range.to_string()
      } else {
        format!("{protocol}:{range}")
      };
      Descriptor::new(ident, full_range)
    })
    .collect();

  if !remaining.is_empty() {
    // For debugging: show what wasn't consumed (only in debug builds)
    #[cfg(debug_assertions)]
    eprintln!(
      "Warning: Descriptor parsing didn't consume entire string. Remaining: {:?}",
      &remaining[..remaining.len().min(100)]
    );

    // For now, we'll accept partial parsing and continue
    // This allows the parser to be more resilient to edge cases
  }

  Ok((rest, descriptors))
}

/// Parse a single descriptor string like "debug@npm:1.0.0", "c@*", or "is-odd@patch:is-odd@npm%3A3.0.1#~/.yarn/patches/is-odd-npm-3.0.1-93c3c3f41b.patch"
/// Returns borrowed strings to avoid allocations during parsing
fn parse_single_descriptor(input: &str) -> IResult<&str, (&str, &str, &str)> {
  // Try patch protocol format first (e.g., patch:is-odd@npm%3A3.0.1#~/.yarn/patches/...)
  if let Ok((remaining, (name_part, _, protocol, _, patch_range))) = (
    parse_package_name,
    char('@'),
    parse_protocol,
    char(':'),
    parse_patch_range,
  )
    .parse(input)
    && protocol == "patch"
  {
    return Ok((remaining, (name_part, protocol, patch_range)));
  }

  // Try protocol:range format (e.g., npm:1.0.0)
  if let Ok((remaining, (name_part, _, protocol, _, range))) = (
    parse_package_name,
    char('@'),
    parse_protocol,
    char(':'),
    take_while1(|c: char| c != ',' && c != '"'),
  )
    .parse(input)
  {
    return Ok((remaining, (name_part, protocol, range)));
  }

  // Try simple range format (e.g., * for c@*)
  if let Ok((remaining, (name_part, _, range))) = (
    parse_package_name,
    char('@'),
    take_while1(|c: char| c != ',' && c != '"'),
  )
    .parse(input)
  {
    return Ok((remaining, (name_part, "", range)));
  }

  Err(nom::Err::Error(nom::error::Error::new(
    input,
    nom::error::ErrorKind::Alt,
  )))
}

/// Helper function to parse name part into Ident
fn parse_name_to_ident(name_part: &str) -> Ident {
  name_part.strip_prefix('@').map_or_else(
    || Ident::new(None, name_part.to_string()),
    |stripped| {
      // Scoped package: @babel/code-frame
      let parts: Vec<&str> = stripped.splitn(2, '/').collect();
      if parts.len() == 2 {
        Ident::new(Some(format!("@{}", parts[0])), parts[1].to_string())
      } else {
        // Malformed scoped package, treat as simple name
        Ident::new(None, name_part.to_string())
      }
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

/// Parse protocol part like npm, workspace, git, etc.
/// TODO: should we validate the protocol? e.g. npm, workspace, git, file, root, etc.
fn parse_protocol(input: &str) -> IResult<&str, &str> {
  // Support common protocol tokens including git+ssh
  take_while1(|c: char| c.is_alphanumeric() || c == '-' || c == '_' || c == '+').parse(input)
}

/// Parse patch range which can be complex like:
/// - "is-odd@npm%3A3.0.1#~/.yarn/patches/is-odd-npm-3.0.1-93c3c3f41b.patch"
/// - "typescript@npm%3A^5.8.3#optional!builtin<compat/typescript>"
///
/// Returns borrowed string to avoid allocations
fn parse_patch_range(input: &str) -> IResult<&str, &str> {
  take_while1(|c: char| c != ',' && c != '"').parse(input)
}

/// Parse indented key-value properties for a package
pub fn parse_package_properties(input: &str) -> IResult<&str, Package> {
  let (rest, properties) = many0(parse_property_line).parse(input)?;

  // Consume any trailing whitespace and blank lines
  let (rest, _) = many0(alt((tag("\n"), tag(" "), tag("\t"), tag("\r")))).parse(rest)?;

  // Build the package from the parsed properties
  let mut package = Package::new("unknown".to_string(), LinkType::Hard);

  for property_value in properties {
    match property_value {
      PropertyValue::Simple(key, value) => match key {
        "version" => {
          package.version = Some(value.trim_matches('"').to_string());
        }
        "resolution" => {
          let raw = value.trim_matches('"').to_string();
          // Best-effort parse of the resolution into a Locator: split on '@' first occurrence
          // Examples: "debug@npm:1.0.0", "a@workspace:packages/a"
          if let Some(at_index) = raw.find('@') {
            let (name_part, reference) = raw.split_at(at_index);
            let ident = parse_name_to_ident(name_part);
            // split_at keeps the '@' on the right; remove it
            let reference = reference.trim_start_matches('@').to_string();
            package.resolution_locator = Some(Locator::new(ident, reference));
          }
          package.resolution = Some(raw);
        }
        "languageName" => {
          package.language_name = crate::package::LanguageName::new(value.to_string());
        }
        "linkType" => {
          package.link_type =
            LinkType::try_from(value).unwrap_or_else(|()| panic!("Invalid link type: {value}"));
        }
        "checksum" => {
          package.checksum = Some(value.to_string());
        }
        "conditions" => {
          package.conditions = Some(value.to_string());
        }
        _ => {
          panic!("Unknown property encountered in package entry: {key}");
        }
      },
      PropertyValue::Dependencies(dependencies) => {
        // Store the parsed dependencies in the package
        for (dep_name, dep_range) in dependencies {
          let ident = parse_dependency_name_to_ident(dep_name);
          let descriptor = Descriptor::new(ident, dep_range.to_string());
          package
            .dependencies
            .insert(descriptor.ident().clone(), descriptor);
        }
      }
      PropertyValue::PeerDependencies(peer_dependencies) => {
        // Store the parsed peer dependencies in the package
        for (dep_name, dep_range) in peer_dependencies {
          let ident = parse_dependency_name_to_ident(dep_name);
          let descriptor = Descriptor::new(ident, dep_range.to_string());
          package
            .peer_dependencies
            .insert(descriptor.ident().clone(), descriptor);
        }
      }
      PropertyValue::Bin(binaries) => {
        // Store the parsed binary executables in the package
        for (bin_name, bin_path) in binaries {
          package
            .bin
            .insert(bin_name.to_string(), bin_path.to_string());
        }
      }
      PropertyValue::DependenciesMeta(meta) => {
        // Store the parsed dependency metadata in the package
        for (dep_name, dep_meta) in meta {
          let ident = parse_dependency_name_to_ident(dep_name);
          package.dependencies_meta.insert(ident, Some(dep_meta));
        }
      }
      PropertyValue::PeerDependenciesMeta(meta) => {
        // Store the parsed peer dependency metadata in the package
        for (dep_name, dep_meta) in meta {
          let ident = parse_dependency_name_to_ident(dep_name);
          package.peer_dependencies_meta.insert(ident, dep_meta);
        }
      }
    }
  }

  Ok((rest, package))
}

/// Parse a single property line with 2-space indentation
/// Examples:
/// "  version: 1.0.0"
/// "  resolution: \"debug@npm:1.0.0\""
/// "  linkType: hard"
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

  // Unknown properties should not be silently skipped

  // If nothing matches, return an error
  Err(nom::Err::Error(nom::error::Error::new(
    input,
    nom::error::ErrorKind::Alt,
  )))
}

/// Enum to represent different types of property values
#[derive(Debug)]
enum PropertyValue<'a> {
  /// A simple key-value property
  Simple(&'a str, &'a str),
  /// A dependencies block
  Dependencies(Vec<(&'a str, &'a str)>), // Use Vec instead of HashMap to avoid allocations
  /// A peer dependencies block
  PeerDependencies(Vec<(&'a str, &'a str)>), // Use Vec instead of HashMap to avoid allocations
  /// A bin block
  Bin(Vec<(&'a str, &'a str)>), // Binary executables: name -> path
  /// A dependencies meta block
  DependenciesMeta(Vec<(&'a str, DependencyMeta)>), // Dependency metadata
  /// A peer dependencies meta block
  PeerDependenciesMeta(Vec<(&'a str, PeerDependencyMeta)>), // Peer dependency metadata
}

/// Parse a simple key-value property line
///
/// # Examples
/// ```
/// use crate::berry_core::parse::parse_simple_property;
/// ```rust,ignore
/// let input = r#"  version: 1.0.0"#;
/// let result = parse_simple_property(input);
/// assert!(result.is_ok());
/// let (remaining, (key, value)) = result.unwrap();
/// assert_eq!(remaining, "");
/// assert_eq!(key, "version");
/// assert_eq!(value, "1.0.0");
/// ```
pub fn parse_simple_property(input: &str) -> IResult<&str, (&str, &str)> {
  let (rest, (_, key, _, _, value, _)) = (
    tag("  "), // 2-space indentation
    take_while1(|c: char| c.is_alphanumeric() || c == '_'),
    char(':'),
    space1,
    is_not("\r\n"), // Stop at newline, don't stop at hash (comments)
    opt(newline),   // Optional newline (file might end without one)
  )
    .parse(input)?;

  Ok((rest, (key, value)))
}

/// Parse a dependencies block and process dependencies without collecting them
/// This uses `fold_many0` to avoid Vec allocations
fn parse_dependencies_block(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
  let (rest, (_, _, dependencies)) = (
    tag("  dependencies:"), // 2-space indented dependencies
    newline,
    fold_many0(parse_dependency_line, Vec::new, |mut acc, item| {
      acc.push(item);
      acc
    }),
  )
    .parse(input)?;

  Ok((rest, dependencies))
}

/// Parse a peerDependencies block and process dependencies without collecting them
/// This uses `fold_many0` to avoid Vec allocations
fn parse_peer_dependencies_block(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
  let (rest, (_, _, peer_dependencies)) = (
    tag("  peerDependencies:"), // 2-space indented peer dependencies
    newline,
    fold_many0(parse_dependency_line, Vec::new, |mut acc, item| {
      acc.push(item);
      acc
    }),
  )
    .parse(input)?;

  Ok((rest, peer_dependencies))
}

/// Parse a bin block and process binary executables without collecting them
/// This uses `fold_many0` to avoid Vec allocations
fn parse_bin_block(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
  let (rest, (_, _, binaries)) = (
    tag("  bin:"), // 2-space indented bin
    newline,
    fold_many0(parse_bin_line, Vec::new, |mut acc, item| {
      acc.push(item);
      acc
    }),
  )
    .parse(input)?;

  Ok((rest, binaries))
}

/// Parse a dependenciesMeta block and process dependency metadata
fn parse_dependencies_meta_block(input: &str) -> IResult<&str, Vec<(&str, DependencyMeta)>> {
  let (rest, (_, _, meta)) = (
    tag("  dependenciesMeta:"), // 2-space indented dependenciesMeta
    newline,
    fold_many0(
      alt((
        parse_dependency_meta_entry_inline, // Try inline format first
        parse_dependency_meta_entry_nested, // Then try nested format
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

/// Parse a peerDependenciesMeta block and process peer dependency metadata
fn parse_peer_dependencies_meta_block(
  input: &str,
) -> IResult<&str, Vec<(&str, PeerDependencyMeta)>> {
  let (rest, (_, _, meta)) = (
    tag("  peerDependenciesMeta:"), // 2-space indented peerDependenciesMeta
    newline,
    fold_many0(
      alt((
        parse_peer_dependency_meta_entry_inline, // Try inline format first
        parse_peer_dependency_meta_entry_nested, // Then try nested format
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
/// Example: "    ms: 0.6.2" or "    "@actions/io": "npm:^1.0.1""
fn parse_dependency_line(input: &str) -> IResult<&str, (&str, &str)> {
  let (rest, (_, dep_name, _, _, dep_range, _)) = (
    // FIXME: is this part of the spec?
    tag("    "), // 4-space indentation for dependencies
    // Handle both quoted and unquoted dependency names
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
    take_until("\n"), // Take until newline, not just non-newline chars
    opt(newline),     // Optional newline (last dependency might not have one)
  )
    .parse(input)?;

  // Trim whitespace and remove quotes from the range
  let clean_range = dep_range.trim().trim_matches('"');

  Ok((rest, (dep_name, clean_range)))
}

/// Parse a single bin line with 4-space indentation
/// Example: "    loose-envify: cli.js"
fn parse_bin_line(input: &str) -> IResult<&str, (&str, &str)> {
  let (rest, (_, bin_name, _, _, bin_path, _)) = (
    tag("    "), // 4-space indentation for bin entries
    take_while1(|c: char| {
      c.is_alphanumeric() || c == '-' || c == '_' || c == '@' || c == '/' || c == '.'
    }),
    char(':'),
    space1,
    is_not("\r\n"),
    newline,
  )
    .parse(input)?;

  // Trim whitespace and remove quotes from the path
  let clean_path = bin_path.trim().trim_matches('"');

  Ok((rest, (bin_name, clean_path)))
}

/// Parse a single dependency meta line with 4-space indentation
/// Example: "    typescript: { built: true }"
fn parse_dependency_meta_entry_inline(input: &str) -> IResult<&str, (&str, DependencyMeta)> {
  let (rest, (_, dep_name, _, _, meta_content, _)) = (
    tag("    "), // 4-space indentation for meta entries
    // Allow quoted or unquoted dependency names
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
    opt(newline), // Optional newline after each entry
  )
    .parse(input)?;

  Ok((rest, (dep_name, meta_content)))
}

/// Parse a single dependency meta entry with nested indentation format
/// Example:
///     typescript:
///       built: true
///       optional: false
fn parse_dependency_meta_entry_nested(input: &str) -> IResult<&str, (&str, DependencyMeta)> {
  let (rest, (_, dep_name, _, _, meta_content, _)) = (
    tag("    "), // 4-space indentation for meta entries
    // Allow quoted or unquoted dependency names
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
    opt(newline), // Optional newline after each entry
  )
    .parse(input)?;

  Ok((rest, (dep_name, meta_content)))
}

/// Parse a dependency meta object with 6-space indentation, supporting built/optional/unplugged
fn parse_dependency_meta_object_indented(input: &str) -> IResult<&str, DependencyMeta> {
  // Helper to parse a single 6-space-indented boolean line for a given property name
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

  // Accumulate any number of built/optional/unplugged lines in any order
  let init = || (None, None, None);
  let (rest, (built, optional, unplugged)) = fold_many0(
    alt((
      map(parse_indented_bool_line("built"), |v| (Some(v), None, None)),
      map(parse_indented_bool_line("optional"), |v| {
        (None, Some(v), None)
      }),
      map(parse_indented_bool_line("unplugged"), |v| {
        (None, None, Some(v))
      }),
    )),
    init,
    |(b_acc, o_acc, u_acc), (b, o, u)| (b.or(b_acc), o.or(o_acc), u.or(u_acc)),
  )
  .parse(input)?;

  Ok((
    rest,
    DependencyMeta {
      built,
      optional,
      unplugged,
    },
  ))
}

/// Parse a single peer dependency meta entry with inline object format
/// Example: "    react: { optional: true }"
fn parse_peer_dependency_meta_entry_inline(
  input: &str,
) -> IResult<&str, (&str, PeerDependencyMeta)> {
  let (rest, (_, dep_name, _, _, meta_content, _)) = (
    tag("    "), // 4-space indentation for meta entries
    // Allow quoted or unquoted dependency names
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
    parse_peer_meta_object,
    opt(newline),
  )
    .parse(input)?;

  Ok((rest, (dep_name, meta_content)))
}

/// Parse a single peer dependency meta entry with nested indentation format
/// Example:
///     graphql-ws:
///       optional: true
fn parse_peer_dependency_meta_entry_nested(
  input: &str,
) -> IResult<&str, (&str, PeerDependencyMeta)> {
  let (rest, (_, dep_name, _, _, meta_content, _)) = (
    tag("    "), // 4-space indentation for meta entries
    // Allow quoted or unquoted dependency names
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
    parse_peer_meta_object_indented,
    opt(newline), // Optional newline after each entry
  )
    .parse(input)?;

  Ok((rest, (dep_name, meta_content)))
}

/// Parse a peer dependency meta object with inline format like "{ optional: true }"
fn parse_peer_meta_object(input: &str) -> IResult<&str, PeerDependencyMeta> {
  let (rest, _) = char('{')(input)?;
  let (rest, _) = space0(rest)?;

  let (rest, optional) = parse_bool_property_inline("optional")(rest)?;
  let (rest, _) = space0(rest)?; // Consume any spaces before closing brace

  let (rest, _) = char('}')(rest)?;

  Ok((rest, PeerDependencyMeta { optional }))
}

/// Parse a boolean property for inline format like "optional: true" (no newline)
fn parse_bool_property_inline(prop_name: &str) -> impl Fn(&str) -> IResult<&str, bool> {
  move |input| {
    let (rest, (_, _, _, value)) = (
      tag(prop_name),
      char(':'),
      space1,
      alt((tag("true"), tag("false"))),
    )
      .parse(input)?;

    let bool_value = value == "true";
    Ok((rest, bool_value))
  }
}

/// Parse a peer dependency meta object with 6-space indentation
/// Example:
///       optional: true
fn parse_peer_meta_object_indented(input: &str) -> IResult<&str, PeerDependencyMeta> {
  let (rest, (_, _, _, optional, _)) = (
    tag("      "), // 6-space indentation for meta object properties
    tag("optional:"),
    space1,
    alt((tag("true"), tag("false"))),
    newline,
  )
    .parse(input)?;

  let optional_bool = optional == "true";

  Ok((
    rest,
    PeerDependencyMeta {
      optional: optional_bool,
    },
  ))
}

/// Parse a dependency meta object like "{ built: true, optional: false }"
fn parse_meta_object(input: &str) -> IResult<&str, DependencyMeta> {
  let (rest, _) = char('{')(input)?;
  let (rest, _) = space0(rest)?;

  // Parse properties with optional commas using inline format (no newlines)
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

/// Parse a dependency name into an Ident
/// This handles both scoped (@scope/name) and non-scoped (name) packages
fn parse_dependency_name_to_ident(dep_name: &str) -> Ident {
  dep_name.strip_prefix('@').map_or_else(
    || Ident::new(None, dep_name.to_string()),
    |stripped| {
      // Scoped package: @babel/code-frame
      let parts: Vec<&str> = stripped.splitn(2, '/').collect();
      if parts.len() == 2 {
        Ident::new(Some(format!("@{}", parts[0])), parts[1].to_string())
      } else {
        // Malformed scoped package, treat as simple name
        Ident::new(None, dep_name.to_string())
      }
    },
  )
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_parse_dependency_line_simple() {
    let input = "    ms: 0.6.2\n";
    let result = parse_dependency_line(input);

    assert!(
      result.is_ok(),
      "Should successfully parse simple dependency line"
    );
    let (remaining, (dep_name, dep_range)) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(dep_name, "ms");
    assert_eq!(dep_range, "0.6.2");
  }

  #[test]
  fn test_parse_dependency_line_scoped_package() {
    let input = "    @babel/code-frame: ^7.12.11\n";
    let result = parse_dependency_line(input);

    assert!(
      result.is_ok(),
      "Should successfully parse scoped package dependency"
    );
    let (remaining, (dep_name, dep_range)) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(dep_name, "@babel/code-frame");
    assert_eq!(dep_range, "^7.12.11");
  }

  #[test]
  fn test_parse_dependency_line_complex_range() {
    let input = "    lodash: ^3.0.0 || ^4.0.0\n";
    let result = parse_dependency_line(input);

    assert!(
      result.is_ok(),
      "Should successfully parse complex version range"
    );
    let (remaining, (dep_name, dep_range)) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(dep_name, "lodash");
    assert_eq!(dep_range, "^3.0.0 || ^4.0.0");
  }

  #[test]
  fn test_parse_dependencies_block_single_dependency() {
    let input = r"  dependencies:
    ms: 0.6.2
";
    let result = parse_dependencies_block(input);

    assert!(
      result.is_ok(),
      "Should successfully parse single dependency block"
    );
    let (remaining, dependencies) = result.unwrap();
    assert_eq!(remaining, "");

    // Verify the parsed dependencies
    assert_eq!(dependencies.len(), 1);
    assert_eq!(dependencies[0], ("ms", "0.6.2"));
  }

  #[test]
  fn test_parse_dependencies_block_multiple_dependencies() {
    let input = r"  dependencies:
    ms: 0.6.2
    lodash: ^4.17.0
    @babel/core: ^7.12.0
";
    let result = parse_dependencies_block(input);

    assert!(
      result.is_ok(),
      "Should successfully parse multiple dependencies"
    );
    let (remaining, dependencies) = result.unwrap();
    assert_eq!(remaining, "");

    // Verify the parsed dependencies
    assert_eq!(dependencies.len(), 3);
    assert_eq!(dependencies[0], ("ms", "0.6.2"));
    assert_eq!(dependencies[1], ("lodash", "^4.17.0"));
    assert_eq!(dependencies[2], ("@babel/core", "^7.12.0"));
  }

  #[test]
  fn test_parse_dependencies_block_empty() {
    let input = r"  dependencies:
";
    let result = parse_dependencies_block(input);

    assert!(
      result.is_ok(),
      "Should successfully parse empty dependencies block"
    );
    let (remaining, dependencies) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(dependencies.len(), 0);
  }

  #[test]
  fn test_parse_peer_dependencies_block() {
    let input = r"  peerDependencies:
    lodash: ^3.0.0 || ^4.0.0
    react: ^16.0.0 || ^17.0.0
";
    let result = parse_peer_dependencies_block(input);

    assert!(
      result.is_ok(),
      "Should successfully parse peer dependencies block"
    );
    let (remaining, peer_dependencies) = result.unwrap();
    assert_eq!(remaining, "");

    // Verify the parsed peer dependencies
    assert_eq!(peer_dependencies.len(), 2);
    assert_eq!(peer_dependencies[0], ("lodash", "^3.0.0 || ^4.0.0"));
    assert_eq!(peer_dependencies[1], ("react", "^16.0.0 || ^17.0.0"));
  }

  #[test]
  fn test_parse_descriptor_line_simple() {
    let input = r#""debug@npm:1.0.0":"#;
    let result = parse_descriptor_line(input);

    assert!(result.is_ok(), "Should successfully parse descriptor line");
    let (remaining, descriptors) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 1);

    // Verify the parsed descriptor
    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "debug");
    assert_eq!(descriptor.ident().scope(), None);
    assert_eq!(descriptor.range(), "npm:1.0.0");
    assert_eq!(descriptor.range_struct().protocol_str(), Some("npm"));
    assert_eq!(descriptor.range_struct().selector(), "1.0.0");
  }

  #[test]
  fn test_parse_descriptor_line_scoped_package() {
    let input = r#""@babel/code-frame@npm:7.12.11":"#;
    let result = parse_descriptor_line(input);

    assert!(
      result.is_ok(),
      "Should successfully parse scoped package descriptor"
    );
    let (remaining, descriptors) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 1);

    // Verify the parsed descriptor
    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "code-frame");
    assert_eq!(descriptor.ident().scope(), Some("@babel"));
    assert_eq!(descriptor.range(), "npm:7.12.11");
    assert_eq!(descriptor.range_struct().protocol_str(), Some("npm"));
    assert_eq!(descriptor.range_struct().selector(), "7.12.11");
  }

  #[test]
  fn test_parse_descriptor_line_workspace() {
    let input = r#""a@workspace:packages/a":"#;
    let result = parse_descriptor_line(input);

    assert!(
      result.is_ok(),
      "Should successfully parse workspace descriptor"
    );
    let (remaining, descriptors) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 1);

    // Verify the parsed descriptor
    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "a");
    assert_eq!(descriptor.ident().scope(), None);
    assert_eq!(descriptor.range(), "workspace:packages/a");
    assert_eq!(descriptor.range_struct().protocol_str(), Some("workspace"));
    assert_eq!(descriptor.range_struct().selector(), "packages/a");
  }

  #[test]
  fn test_parse_package_properties_minimal() {
    let input = r#"  version: 1.0.0
  resolution: "debug@npm:1.0.0"
  languageName: node
  linkType: hard
"#;
    let result = parse_package_properties(input);

    assert!(
      result.is_ok(),
      "Should successfully parse minimal package properties"
    );
    let (remaining, package) = result.unwrap();
    assert_eq!(remaining, "");

    // Verify the parsed package properties
    assert_eq!(package.version, Some("1.0.0".to_string()));
    assert_eq!(package.resolution, Some("debug@npm:1.0.0".to_string()));
    assert_eq!(package.language_name.as_ref(), "node");
    assert_eq!(package.link_type, LinkType::Hard);
    assert_eq!(package.checksum, None);
  }

  #[test]
  fn test_parse_package_properties_with_dependencies() {
    let input = r#"  version: 1.0.0
  resolution: "debug@npm:1.0.0"
  dependencies:
    ms: 0.6.2
  languageName: node
  linkType: hard
"#;
    let result = parse_package_properties(input);

    assert!(
      result.is_ok(),
      "Should successfully parse package properties with dependencies"
    );
    let (remaining, package) = result.unwrap();
    assert_eq!(remaining, "");

    // Verify the parsed package properties
    assert_eq!(package.version, Some("1.0.0".to_string()));
    assert_eq!(package.resolution, Some("debug@npm:1.0.0".to_string()));
    assert_eq!(package.language_name.as_ref(), "node");
    assert_eq!(package.link_type, LinkType::Hard);
  }

  #[test]
  fn test_parse_package_properties_with_checksum() {
    let input = r#"  version: 1.0.0
  resolution: "debug@npm:1.0.0"
  checksum: edfec8784737afbeea43cc78c3f56c33b88d3e751cc7220ae7a1c5370ff099e7352703275bdb56ea9967f92961231ce0625f8234d82259047303849671153f03
  languageName: node
  linkType: hard
"#;
    let result = parse_package_properties(input);

    assert!(
      result.is_ok(),
      "Should successfully parse package properties with checksum"
    );
    let (remaining, package) = result.unwrap();
    assert_eq!(remaining, "");

    // Verify the parsed package properties
    assert_eq!(package.version, Some("1.0.0".to_string()));
    assert_eq!(package.resolution, Some("debug@npm:1.0.0".to_string()));
    assert_eq!(package.language_name.as_ref(), "node");
    assert_eq!(package.link_type, LinkType::Hard);
    assert_eq!(package.checksum, Some("edfec8784737afbeea43cc78c3f56c33b88d3e751cc7220ae7a1c5370ff099e7352703275bdb56ea9967f92961231ce0625f8234d82259047303849671153f03".to_string()));
  }

  #[test]
  fn test_parse_full_package_entry() {
    let input = r#""debug@npm:1.0.0":
  version: 1.0.0
  resolution: "debug@npm:1.0.0"
  dependencies:
    ms: 0.6.2
  checksum: edfec8784737afbeea43cc78c3f56c33b88d3e751cc7220ae7a1c5370ff099e7352703275bdb56ea9967f92961231ce0625f8234d82259047303849671153f03
  languageName: node
  linkType: hard

"#;
    let result = parse_package_entry(input);

    assert!(
      result.is_ok(),
      "Should successfully parse complete package entry"
    );
    let (remaining, (descriptors, package)) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 1);

    // Verify the parsed descriptor
    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "debug");
    assert_eq!(descriptor.ident().scope(), None);
    assert_eq!(descriptor.range(), "npm:1.0.0");

    // Verify the parsed package
    assert_eq!(package.version, Some("1.0.0".to_string()));
    assert_eq!(package.resolution, Some("debug@npm:1.0.0".to_string()));
    // resolution_locator is parsed
    let locator = package
      .resolution_locator
      .as_ref()
      .expect("locator present");
    assert_eq!(locator.ident().name(), "debug");
    assert_eq!(locator.ident().scope(), None);
    assert_eq!(locator.reference(), "npm:1.0.0");
    assert_eq!(package.language_name.as_ref(), "node");
    assert_eq!(package.link_type, LinkType::Hard);
    assert_eq!(package.checksum, Some("edfec8784737afbeea43cc78c3f56c33b88d3e751cc7220ae7a1c5370ff099e7352703275bdb56ea9967f92961231ce0625f8234d82259047303849671153f03".to_string()));
  }

  #[test]
  fn test_parse_workspace_package_entry() {
    let input = r#""a@workspace:packages/a":
  version: 0.0.0-use.local
  resolution: "a@workspace:packages/a"
  languageName: unknown
  linkType: soft

"#;
    let result = parse_package_entry(input);

    assert!(
      result.is_ok(),
      "Should successfully parse workspace package entry"
    );
    let (remaining, (descriptors, package)) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 1);

    // Verify the parsed descriptor
    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "a");
    assert_eq!(descriptor.ident().scope(), None);
    assert_eq!(descriptor.range(), "workspace:packages/a");

    // Verify the parsed package
    assert_eq!(package.version, Some("0.0.0-use.local".to_string()));
    assert_eq!(
      package.resolution,
      Some("a@workspace:packages/a".to_string())
    );
    let locator = package
      .resolution_locator
      .as_ref()
      .expect("locator present");
    assert_eq!(locator.ident().name(), "a");
    assert_eq!(locator.reference(), "workspace:packages/a");
    assert_eq!(package.language_name.as_ref(), "unknown");
    assert_eq!(package.link_type, LinkType::Soft);
    assert_eq!(package.checksum, None);
  }

  #[test]
  fn test_parse_descriptor_line_multi_descriptor() {
    let input = r#""c@*, c@workspace:packages/c":"#;
    let result = parse_descriptor_line(input);

    assert!(
      result.is_ok(),
      "Should successfully parse multi-descriptor line"
    );
    let (remaining, descriptors) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 2);

    // Verify the first descriptor: c@*
    let first_descriptor = &descriptors[0];
    assert_eq!(first_descriptor.ident().name(), "c");
    assert_eq!(first_descriptor.ident().scope(), None);
    assert_eq!(first_descriptor.range(), "*");

    // Verify the second descriptor: c@workspace:packages/c
    let second_descriptor = &descriptors[1];
    assert_eq!(second_descriptor.ident().name(), "c");
    assert_eq!(second_descriptor.ident().scope(), None);
    assert_eq!(second_descriptor.range(), "workspace:packages/c");
  }

  #[test]
  fn test_parse_descriptor_line_complex_multi_descriptor() {
    let input = r#""lodash@npm:^3.0.0 || ^4.0.0, lodash@npm:^4.17.0":"#;
    let result = parse_descriptor_line(input);

    assert!(
      result.is_ok(),
      "Should successfully parse complex multi-descriptor line"
    );
    let (remaining, descriptors) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 2);

    // Verify the first descriptor
    let first_descriptor = &descriptors[0];
    assert_eq!(first_descriptor.ident().name(), "lodash");
    assert_eq!(first_descriptor.ident().scope(), None);
    assert_eq!(first_descriptor.range(), "npm:^3.0.0 || ^4.0.0");

    // Verify the second descriptor
    let second_descriptor = &descriptors[1];
    assert_eq!(second_descriptor.ident().name(), "lodash");
    assert_eq!(second_descriptor.ident().scope(), None);
    assert_eq!(second_descriptor.range(), "npm:^4.17.0");
  }

  #[test]
  fn test_parse_descriptor_line_patch_protocol() {
    let input =
      r#""is-odd@patch:is-odd@npm%3A3.0.1#~/.yarn/patches/is-odd-npm-3.0.1-93c3c3f41b.patch":"#;
    let result = parse_descriptor_line(input);

    assert!(
      result.is_ok(),
      "Should successfully parse patch protocol descriptor"
    );
    let (remaining, descriptors) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 1);

    // Verify the parsed descriptor
    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "is-odd");
    assert_eq!(descriptor.ident().scope(), None);
    assert_eq!(
      descriptor.range(),
      "patch:is-odd@npm%3A3.0.1#~/.yarn/patches/is-odd-npm-3.0.1-93c3c3f41b.patch"
    );
    assert_eq!(descriptor.range_struct().protocol_str(), Some("patch"));
    assert!(
      descriptor
        .range_struct()
        .selector()
        .starts_with("is-odd@npm%3A3.0.1#")
    );
  }

  #[test]
  fn test_parse_descriptor_line_builtin_patch() {
    let input =
      r#""typescript@patch:typescript@npm%3A^5.8.3#optional!builtin<compat/typescript>":"#;
    let result = parse_descriptor_line(input);

    assert!(
      result.is_ok(),
      "Should successfully parse builtin patch protocol descriptor"
    );
    let (remaining, descriptors) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 1);

    // Verify the parsed descriptor
    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "typescript");
    assert_eq!(descriptor.ident().scope(), None);
    assert_eq!(
      descriptor.range(),
      "patch:typescript@npm%3A^5.8.3#optional!builtin<compat/typescript>"
    );
    assert_eq!(descriptor.range_struct().protocol_str(), Some("patch"));
    assert!(
      descriptor
        .range_struct()
        .selector()
        .starts_with("typescript@npm%3A^5.8.3#optional!builtin<compat/typescript>")
    );
  }

  #[test]
  fn test_parse_patch_package_entry() {
    let input = r#""is-odd@patch:is-odd@npm%3A3.0.1#~/.yarn/patches/is-odd-npm-3.0.1-93c3c3f41b.patch":
  version: 3.0.1
  resolution: "is-odd@patch:is-odd@npm%3A3.0.1#~/.yarn/patches/is-odd-npm-3.0.1-93c3c3f41b.patch::version=3.0.1&hash=9b90ad"
  dependencies:
    is-number: "npm:^6.0.0"
  checksum: 4cd944e688e02e147969d6c1784bad1156f6084edbbd4d688f6a37b5fc764671aa99679494fc0bfaf623919bea2779e724fffc31c6ee0432b7c91f174526e5fe
  languageName: node
  linkType: hard

"#;
    let result = parse_package_entry(input);

    assert!(
      result.is_ok(),
      "Should successfully parse patch package entry"
    );
    let (remaining, (descriptors, package)) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 1);

    // Verify the parsed descriptor
    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "is-odd");
    assert_eq!(descriptor.ident().scope(), None);
    assert_eq!(
      descriptor.range(),
      "patch:is-odd@npm%3A3.0.1#~/.yarn/patches/is-odd-npm-3.0.1-93c3c3f41b.patch"
    );

    // Verify the parsed package
    assert_eq!(package.version, Some("3.0.1".to_string()));
    assert_eq!(
      package.resolution,
      Some("is-odd@patch:is-odd@npm%3A3.0.1#~/.yarn/patches/is-odd-npm-3.0.1-93c3c3f41b.patch::version=3.0.1&hash=9b90ad".to_string())
    );
    assert_eq!(package.language_name.as_ref(), "node");
    assert_eq!(package.link_type, LinkType::Hard);
    assert_eq!(package.checksum, Some("4cd944e688e02e147969d6c1784bad1156f6084edbbd4d688f6a37b5fc764671aa99679494fc0bfaf623919bea2779e724fffc31c6ee0432b7c91f174526e5fe".to_string()));
  }

  #[test]
  fn test_parse_package_properties_with_bin() {
    let input = r#"  version: 1.4.0
  resolution: "loose-envify@npm:1.4.0"
  dependencies:
    js-tokens: "npm:^3.0.0 || ^4.0.0"
  bin:
    loose-envify: cli.js
  checksum: 10/6517e24e0cad87ec9888f500c5b5947032cdfe6ef65e1c1936a0c48a524b81e65542c9c3edc91c97d5bddc806ee2a985dbc79be89215d613b1de5db6d1cfe6f4
  languageName: node
  linkType: hard
"#;
    let result = parse_package_properties(input);

    assert!(
      result.is_ok(),
      "Should successfully parse package properties with bin field"
    );
    let (remaining, package) = result.unwrap();
    assert_eq!(remaining, "");

    // Verify the parsed package properties
    assert_eq!(package.version, Some("1.4.0".to_string()));
    assert_eq!(
      package.resolution,
      Some("loose-envify@npm:1.4.0".to_string())
    );
    assert_eq!(package.language_name.as_ref(), "node");
    assert_eq!(package.link_type, LinkType::Hard);
    assert_eq!(package.checksum, Some("10/6517e24e0cad87ec9888f500c5b5947032cdfe6ef65e1c1936a0c48a524b81e65542c9c3edc91c97d5bddc806ee2a985dbc79be89215d613b1de5db6d1cfe6f4".to_string()));

    // Verify the bin field is correctly stored
    assert_eq!(package.bin.len(), 1);
    assert_eq!(package.bin.get("loose-envify"), Some(&"cli.js".to_string()));
  }

  #[test]
  fn test_parse_package_properties_with_conditions() {
    let input = r#"  version: 1.4.0
  resolution: "loose-envify@npm:1.4.0"
  conditions: os=linux & cpu=x64 & libc=glibc
  languageName: node
  linkType: hard
"#;
    let result = parse_package_properties(input);

    assert!(
      result.is_ok(),
      "Should successfully parse package properties with conditions field"
    );
    let (remaining, package) = result.unwrap();
    assert_eq!(remaining, "");

    // Verify the parsed package properties
    assert_eq!(package.version, Some("1.4.0".to_string()));
    assert_eq!(
      package.resolution,
      Some("loose-envify@npm:1.4.0".to_string())
    );
    assert_eq!(
      package.conditions,
      Some("os=linux & cpu=x64 & libc=glibc".to_string())
    );
    assert_eq!(package.language_name.as_ref(), "node");
    assert_eq!(package.link_type, LinkType::Hard);
  }

  #[test]
  fn test_parse_package_properties_with_multiple_bin() {
    let input = r#"  version: 1.0.0
  resolution: "test-package@npm:1.0.0"
  bin:
    test-cli: bin/cli.js
    test-server: bin/server.js
    test-utils: bin/utils.js
  languageName: node
  linkType: hard
"#;
    let result = parse_package_properties(input);

    assert!(
      result.is_ok(),
      "Should successfully parse package properties with multiple bin entries"
    );
    let (remaining, package) = result.unwrap();
    assert_eq!(remaining, "");

    // Verify the bin field is correctly stored
    assert_eq!(package.bin.len(), 3);
    assert_eq!(package.bin.get("test-cli"), Some(&"bin/cli.js".to_string()));
    assert_eq!(
      package.bin.get("test-server"),
      Some(&"bin/server.js".to_string())
    );
    assert_eq!(
      package.bin.get("test-utils"),
      Some(&"bin/utils.js".to_string())
    );
  }

  #[test]
  fn test_parse_package_properties_with_dependencies_meta() {
    let input = r#"  version: 1.0.0
  resolution: "test-package@npm:1.0.0"
  dependenciesMeta:
    typescript: { built: true, optional: false }
    react: { built: false, optional: true, unplugged: true }
  languageName: node
  linkType: hard
"#;
    let result = parse_package_properties(input);

    assert!(
      result.is_ok(),
      "Should successfully parse package properties with dependenciesMeta"
    );
    let (remaining, package) = result.unwrap();
    assert_eq!(remaining, "");

    // Verify the dependenciesMeta field is correctly stored
    assert_eq!(package.dependencies_meta.len(), 2);

    let typescript_meta = package
      .dependencies_meta
      .get(&Ident::new(None, "typescript".to_string()))
      .unwrap()
      .as_ref()
      .unwrap();
    assert_eq!(typescript_meta.built, Some(true));
    assert_eq!(typescript_meta.optional, Some(false));
    assert_eq!(typescript_meta.unplugged, None);

    let react_meta = package
      .dependencies_meta
      .get(&Ident::new(None, "react".to_string()))
      .unwrap()
      .as_ref()
      .unwrap();
    assert_eq!(react_meta.built, Some(false));
    assert_eq!(react_meta.optional, Some(true));
    assert_eq!(react_meta.unplugged, Some(true));
  }

  #[test]
  fn test_parse_package_properties_with_peer_dependencies_meta() {
    let input = r#"  version: 1.0.0
  resolution: "test-package@npm:1.0.0"
  peerDependenciesMeta:
    react: { optional: true }
    vue: { optional: true }
  languageName: node
  linkType: hard
"#;
    let result = parse_package_properties(input);

    assert!(
      result.is_ok(),
      "Should successfully parse package properties with peerDependenciesMeta"
    );
    let (remaining, package) = result.unwrap();
    assert_eq!(remaining, "");

    // Verify the peerDependenciesMeta field is correctly stored
    assert_eq!(package.peer_dependencies_meta.len(), 2);

    let react_meta = package
      .peer_dependencies_meta
      .get(&Ident::new(None, "react".to_string()))
      .unwrap();
    assert!(react_meta.optional);

    let vue_meta: &PeerDependencyMeta = package
      .peer_dependencies_meta
      .get(&Ident::new(None, "vue".to_string()))
      .unwrap();
    assert!(vue_meta.optional);
  }

  #[test]
  fn test_parse_multiple_packages() {
    // Test parsing multiple packages with a simple example
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

    match result {
      Ok((remaining, lockfile)) => {
        println!("Successfully parsed {} entries", lockfile.entries.len());
        // print first 100 chars of remaining
        println!(
          "First 100 chars of remaining: '{}'",
          &remaining[..100.min(remaining.len())]
        );
        assert_eq!(lockfile.entries.len(), 2, "Should parse 2 entries");
        assert!(remaining.is_empty(), "Should consume all input");
      }
      Err(e) => {
        println!("Parse error: {e:?}");
        panic!("Failed to parse multiple packages");
      }
    }
  }

  #[test]
  fn test_parse_peer_dependencies_meta_real_format() {
    // Test parsing the actual format found in resolutions-patches.yarn.lock
    // This demonstrates the parsing issue where the parser fails on real-world data
    let input = r"  peerDependenciesMeta:
    graphql-ws:
      optional: true
    react:
      optional: true
    react-dom:
      optional: true
    subscriptions-transport-ws:
      optional: true
";

    let result = parse_peer_dependencies_meta_block(input);

    match result {
      Ok((remaining, meta)) => {
        println!("Successfully parsed peerDependenciesMeta: {meta:?}");
        println!("Remaining: '{remaining}'");
        assert_eq!(meta.len(), 4, "Should parse 4 peer dependency meta entries");
        assert!(remaining.is_empty(), "Should consume all input");
      }
      Err(e) => {
        println!("Failed to parse peerDependenciesMeta: {e:?}");
        panic!("Parser should now handle real-world peerDependenciesMeta format: {e:?}");
      }
    }
  }

  #[test]
  fn test_parse_package_with_peer_dependencies_meta_real_format() {
    // Test parsing a complete package entry with the real peerDependenciesMeta format
    let input = r#"  version: 6.0.0
  resolution: "@actions/github@npm:6.0.0"
  dependencies:
    "@actions/http-client": "npm:^2.2.0"
    "@octokit/core": "npm:^5.0.1"
    "@octokit/plugin-paginate-rest": "npm:^9.0.0"
    "@octokit/plugin-rest-endpoint-methods": "npm:^10.0.0"
  peerDependenciesMeta:
    graphql-ws:
      optional: true
    react:
      optional: true
    react-dom:
      optional: true
    subscriptions-transport-ws:
      optional: true
  checksum: 10/d3744a5416c7ba33057b1ed247fa4b30da167a6b490898968e6e03870424906c3b4b1910829dc5b26622393e3f203b6ad26e7f6a2c2e9505dc0f9e915432482a
  languageName: node
  linkType: hard
"#;

    let result = parse_package_properties(input);

    match result {
      Ok((remaining, package)) => {
        println!("Successfully parsed package with peerDependenciesMeta");
        println!("Remaining: '{remaining}'");
        assert_eq!(
          package.peer_dependencies_meta.len(),
          4,
          "Should parse 4 peer dependency meta entries"
        );
        assert!(remaining.is_empty(), "Should consume all input");
      }
      Err(e) => {
        println!("Failed to parse package with peerDependenciesMeta: {e:?}");
        panic!("Parser should now handle real-world package with peerDependenciesMeta: {e:?}");
      }
    }
  }

  #[test]
  fn test_parse_descriptor_line_conditional_package() {
    let input = r#"? "resolve@patch:resolve@npm%3A^1.0.0#optional!builtin<compat/resolve>, resolve@patch:resolve@npm%3A^1.1.4#optional!builtin<compat/resolve>":"#;
    let result = parse_descriptor_line(input);

    assert!(
      result.is_ok(),
      "Should successfully parse conditional package descriptor with ? prefix"
    );
    let (remaining, descriptors) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 2);

    // Verify the first descriptor
    let first_descriptor = &descriptors[0];
    assert_eq!(first_descriptor.ident().name(), "resolve");
    assert_eq!(first_descriptor.ident().scope(), None);
    assert_eq!(
      first_descriptor.range(),
      "patch:resolve@npm%3A^1.0.0#optional!builtin<compat/resolve>"
    );

    // Verify the second descriptor
    let second_descriptor = &descriptors[1];
    assert_eq!(second_descriptor.ident().name(), "resolve");
    assert_eq!(second_descriptor.ident().scope(), None);
    assert_eq!(
      second_descriptor.range(),
      "patch:resolve@npm%3A^1.1.4#optional!builtin<compat/resolve>"
    );
  }

  #[test]
  fn test_parse_conditional_package_entry() {
    let input = r#"? "resolve@patch:resolve@npm%3A^1.0.0#optional!builtin<compat/resolve>":
  version: 1.22.10
  resolution: "resolve@patch:resolve@npm%3A1.22.10#optional!builtin<compat/resolve>::version=1.22.10&hash=c3c19d"
  dependencies:
    is-core-module: "npm:^2.16.0"
    path-parse: "npm:^1.0.7"
    supports-preserve-symlinks-flag: "npm:^1.0.0"
  bin:
    resolve: bin/resolve
  checksum: 10/dc5c99fb47807d3771be3135ac6bdb892186973d0895ab17838f0b85bb575e03111214aa16cb68b6416df3c1dd658081a066dd7a9af6e668c28b0025080b615c
  languageName: node
  linkType: hard

"#;
    let result = parse_package_entry(input);

    assert!(
      result.is_ok(),
      "Should successfully parse complete conditional package entry"
    );
    let (remaining, (descriptors, package)) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 1);

    // Verify the parsed descriptor
    let descriptor = &descriptors[0];
    assert_eq!(descriptor.ident().name(), "resolve");
    assert_eq!(descriptor.ident().scope(), None);
    assert_eq!(
      descriptor.range(),
      "patch:resolve@npm%3A^1.0.0#optional!builtin<compat/resolve>"
    );

    // Verify the parsed package
    assert_eq!(package.version, Some("1.22.10".to_string()));
    assert_eq!(
      package.resolution,
      Some("resolve@patch:resolve@npm%3A1.22.10#optional!builtin<compat/resolve>::version=1.22.10&hash=c3c19d".to_string())
    );
    assert_eq!(package.language_name.as_ref(), "node");
    assert_eq!(package.link_type, LinkType::Hard);
    assert_eq!(package.dependencies.len(), 3);
    assert_eq!(package.bin.len(), 1);
    assert_eq!(package.bin.get("resolve"), Some(&"bin/resolve".to_string()));
  }

  #[test]
  fn test_parse_descriptor_line_wrapped_long_line() {
    let input = r#"? "@babel/runtime@npm:^7.0.0, @babel/runtime@npm:^7.1.2, @babel/runtime@npm:^7.10.0"
:"#;
    let result = parse_descriptor_line(input);

    assert!(
      result.is_ok(),
      "Should successfully parse long, wrapped-line descriptor"
    );
    let (remaining, descriptors) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 3, "Should parse 3 descriptors");

    // Verify first descriptor
    assert_eq!(descriptors[0].ident().name(), "runtime");
    assert_eq!(descriptors[0].ident().scope(), Some("@babel"));
    assert_eq!(descriptors[0].range(), "npm:^7.0.0");

    // Verify second descriptor
    assert_eq!(descriptors[1].ident().name(), "runtime");
    assert_eq!(descriptors[1].range(), "npm:^7.1.2");

    // Verify third descriptor
    assert_eq!(descriptors[2].ident().name(), "runtime");
    assert_eq!(descriptors[2].range(), "npm:^7.10.0");
  }

  #[test]
  fn test_parse_descriptor_line_short_no_wrap() {
    // Test that short lines with `?` prefix still work (without newline wrap)
    let input = r#"? "resolve@patch:resolve@npm%3A^1.0.0#optional!builtin<compat/resolve>":"#;
    let result = parse_descriptor_line(input);

    assert!(
      result.is_ok(),
      "Should successfully parse short conditional package without wrap"
    );
    let (remaining, descriptors) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 1);
    assert_eq!(descriptors[0].ident().name(), "resolve");
  }

  #[test]
  fn test_parse_package_entry_long_wrapped_line() {
    // Test a complete package entry with the long wrapped descriptor pattern
    let input = r#"? "@babel/runtime@npm:^7.0.0, @babel/runtime@npm:^7.1.2, @babel/runtime@npm:^7.10.0, @babel/runtime@npm:^7.12.0"
:
  version: 7.28.4
  resolution: "@babel/runtime@npm:7.28.4"
  checksum: 10/6c9a70452322ea80b3c9b2a412bcf60771819213a67576c8cec41e88a95bb7bf01fc983754cda35dc19603eef52df22203ccbf7777b9d6316932f9fb77c25163
  languageName: node
  linkType: hard

"#;
    let result = parse_package_entry(input);

    assert!(
      result.is_ok(),
      "Should successfully parse complete package entry with long wrapped descriptor"
    );
    let (remaining, (descriptors, package)) = result.unwrap();
    assert_eq!(remaining, "");
    assert_eq!(descriptors.len(), 4, "Should parse 4 descriptors");

    // Verify descriptors
    assert_eq!(descriptors[0].ident().name(), "runtime");
    assert_eq!(descriptors[0].ident().scope(), Some("@babel"));
    assert_eq!(descriptors[0].range(), "npm:^7.0.0");
    assert_eq!(descriptors[3].range(), "npm:^7.12.0");

    // Verify the parsed package
    assert_eq!(package.version, Some("7.28.4".to_string()));
    assert_eq!(
      package.resolution,
      Some("@babel/runtime@npm:7.28.4".to_string())
    );
    assert_eq!(package.language_name.as_ref(), "node");
    assert_eq!(package.link_type, LinkType::Hard);
  }
}
