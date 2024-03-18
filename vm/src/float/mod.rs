//! This module controls access to two different floating point implementations (F56 and F32Wrap).
//! F32Wrap is implemented in this file, and F56 is implemented in its own file.
//! The purpose of this module is to allow the Value module to switch between the two implementations

mod float_32;
mod float_56;

pub use self::float_32::F32Wrap;
pub use self::float_56::F56;
