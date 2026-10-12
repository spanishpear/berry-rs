//! # berry-ref
//!
//! A frozen, unmodified copy of the berry 0.6 nom parser as of `main` @ 4224191
//! (`crates/berry-core/src/{ident,locator,lockfile,metadata,package,parse}.rs`).
//!
//! It exists only as the reference oracle for differential tests: the optimized
//! parser in `berry` must produce a byte-identical canonical dump on every input
//! this crate accepts without panicking. Do not change the parser code in this
//! crate; if the reference behaviour ever needs to change, that is a deliberate
//! semantic change of `berry` and must be recorded as such.
//!
//! Known reference behaviour that `berry` intentionally does NOT reproduce: this
//! parser panics on an unknown property key and on a `linkType` other than
//! `hard`/`soft`; `berry` returns an error instead.
#![deny(clippy::all)]
#![allow(clippy::pedantic, clippy::nursery)]
pub mod ident;
pub mod locator;
pub mod lockfile;
pub mod metadata;
pub mod package;
pub mod parse;
