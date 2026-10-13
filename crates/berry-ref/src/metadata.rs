// https://github.com/yarnpkg/berry/blob/master/packages/yarnpkg-core/sources/Manifest.ts#L25
// note: this smells like option, but realistically it is an extra property
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PeerDependencyMeta {
  pub optional: bool,
}

// https://github.com/yarnpkg/berry/blob/master/packages/yarnpkg-core/sources/Manifest.ts#L19
// note: this smells like misuse of option, but realistically it is an extra property that
// may exist, and it may be true/false
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DependencyMeta {
  pub built: Option<bool>,
  pub optional: Option<bool>,
  pub unplugged: Option<bool>,
}
