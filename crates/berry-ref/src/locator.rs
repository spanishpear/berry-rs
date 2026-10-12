use crate::ident::Ident;

// Locators are just like idents (including their `identHash`), except that
// they also contain a reference and an additional comparator hash. They are
// in this regard very similar to descriptors except that each descriptor may
// reference multiple valid candidate packages whereas each locators can only
// reference a single package.
//
// This interesting property means that each locator can be safely turned into
// a descriptor - but not the other way
// around (except in very specific cases).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Locator<'a> {
  ident: Ident<'a>,
  /// A package reference uniquely identifies a package (eg. `1.2.3`).
  reference: &'a str,
}

impl<'a> Locator<'a> {
  /// Create a new Locator from an Ident and a reference (borrowed)
  pub const fn new(ident: Ident<'a>, reference: &'a str) -> Self {
    Self { ident, reference }
  }

  /// Returns the Ident of the Locator (e.g. `@scope/package`)
  pub const fn ident(&self) -> &Ident<'a> {
    &self.ident
  }

  /// Returns the reference of the Locator (e.g. `1.2.3`)
  pub const fn reference(&self) -> &'a str {
    self.reference
  }
}
