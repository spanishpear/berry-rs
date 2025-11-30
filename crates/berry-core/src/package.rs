use crate::ident::{Descriptor, Ident};
use crate::locator::Locator;
use crate::metadata::{DependencyMeta, PeerDependencyMeta};
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// The type of link to use for a package
pub enum LinkType {
  /// The package manager owns the location (typically things within the cache)
  /// e.g. `PnP` linker may unplug packages
  Hard,

  /// The package manager doesn't own the location (symlinks, workspaces, etc),
  /// so the linkers aren't allowed to do anything with them except use them as
  /// they are.
  Soft,
}

impl TryFrom<&str> for LinkType {
  type Error = ();

  fn try_from(s: &str) -> Result<Self, Self::Error> {
    match s {
      "hard" => Ok(Self::Hard),
      "soft" => Ok(Self::Soft),
      _ => Err(()),
    }
  }
}

/// Zero-copy package representation using borrowed strings
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Package<'a> {
  /// Version of the package, if available
  pub version: Option<&'a str>,

  /// Resolution string for the package (raw, for round-trip)
  pub resolution: Option<&'a str>,
  /// Parsed resolution into a Locator (ident + reference)
  pub resolution_locator: Option<Locator<'a>>,

  /// The "language" of the package (eg. `node`), for use with multi-linkers.
  pub language_name: &'a str,

  /// Type of filesystem link for a package
  pub link_type: LinkType,

  /// Checksum for the package
  pub checksum: Option<&'a str>,

  /// A set of constraints indicating whether the package supports the host environments
  pub conditions: Option<&'a str>,

  /// A map of the package's dependencies. There's no distinction between prod
  /// dependencies and dev dependencies, because those have already been merged
  /// during the resolution process
  pub dependencies: FxHashMap<Ident<'a>, Descriptor<'a>>,

  /// Map with additional information about direct dependencies
  pub dependencies_meta: FxHashMap<Ident<'a>, Option<DependencyMeta>>,

  /// Map of packages peer dependencies
  pub peer_dependencies: FxHashMap<Ident<'a>, Descriptor<'a>>,

  /// Map with additional information about peer dependencies
  pub peer_dependencies_meta: FxHashMap<Ident<'a>, PeerDependencyMeta>,

  /// all bin entries for the package
  ///
  /// We don't need binaries in resolution, but we do need them to keep `yarn run` fast
  /// else we have to parse and read all of the zipfiles
  pub bin: FxHashMap<&'a str, &'a str>,
}

impl<'a> Package<'a> {
  pub fn new(language_name: &'a str, link_type: LinkType) -> Self {
    Self {
      version: None,
      resolution: None,
      resolution_locator: None,
      language_name,
      link_type,
      checksum: None,
      conditions: None,
      dependencies: FxHashMap::default(),
      dependencies_meta: FxHashMap::default(),
      peer_dependencies: FxHashMap::default(),
      peer_dependencies_meta: FxHashMap::default(),
      bin: FxHashMap::default(),
    }
  }

  #[must_use]
  pub fn with_version(mut self, version: &'a str) -> Self {
    self.version = Some(version);
    self
  }

  #[must_use]
  pub fn with_resolution(mut self, resolution: &'a str) -> Self {
    self.resolution = Some(resolution);
    self
  }

  #[must_use]
  pub fn with_checksum(mut self, checksum: &'a str) -> Self {
    self.checksum = Some(checksum);
    self
  }
}

pub type LockfileEntry<'a> = Package<'a>;
