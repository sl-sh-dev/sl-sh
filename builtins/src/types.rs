//! TODO PC ISSUE #8 need explanation for the emulation for TryFrom/TryInto/AsRef/AsMut
//! My notes:
//! #. To convert a slosh &Value to an owned type implement `impl SlFrom<&Value> for OwnedType`,
//!     this allows rust native functions annotated with the bridge macro to receive normal
//!     rust types.
//! #. To convert a slosh &Value to a reference type implement `impl SlAsRef<&Value> for RefType`.
//! #. To convert a slosh &Value to a mutable reference type implement `impl SlAsMut<&Value> for MutRefType`.
//! #. To convert some rust type back to a value that the rust native function
//!     annotated by the bridge macro returns implement `impl SlFrom<&Value> for RustType`.
//!     TODO PC ISSUE #8 blanket impl so impl `SlFrom<Value>` works, and taking a ref isn't required?
//! #. To avoid allocations when converting a slosh &Value back to a rust type that was mutated
//!     don't return anything. If it is necessary for the API to return some value,
//!     TODO PC ISSUE #7 annotated or lifetime?
//!
//!
//! ## rosetta stone for bridge macros
//! Rust Type                   | Slosh Type & Traits   <br>&emsp; <br> S -> R Convert Slosh -> Rust <br> &emsp; - Occurs when coercing slush arguments to the parameter types in the signature of the annotated Rust function. <br> R -> S Convert Rust -> Slosh <br> &emsp; - Occurs when coercing some returned Rust type to a Slosh type. |
//! ----------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------|
//! [`String`]                  | [Value]`::String`           |
//!                             |                             | S -> R
//!                             |                             |     &emsp;- [`SlInto`] [`String`] for `&`[`Value`]
//!                             |                             | R -> S
//!                             |                             |     &emsp;- [`SlFrom`] `&`[`Value`] for [`String`]
//!                             |                             |
//! `&`[`String`]               | [`Value`]`::String`         |
//!                             |                             | S -> R
//!                             |                             |     &emsp;- [`SlInto`] `&`[`String`] for `&`[`Value`]
//!                             |                             | R -> S
//!                             |                             |     &emsp;- take [`String`]
//!                             |                             |     &emsp;* uses Clone unless TODO PC ISSUE #7 the extant value problem
//!                             |                             |
//! `&mut `[`String`]           | [`Value`]`::String`         |
//!                             |                             | S -> R
//!                             |                             |     &emsp;- [`SlAsMut`] [`String`] for `&`[`Value`]
//!                             |                             | R -> S
//!                             |                             |     &emsp;- take `&mut `[`String`]
//!                             |                             |     &emsp;* uses Clone unless TODO PC ISSUE #7 the extant value problem
//!                             |                             |
//! `&`[`str`]                  | [`Value`]`::String` / [`Value`]`::StringConst` |
//!                             |                             | S -> R
//!                             |                             |     &emsp;- [`SlAsRef`] [`str`] for `&`[`Value`]
//!                             |                             | R -> S
//!                             |                             |     &emsp;- [`SlFrom`] for [`Value`]
//!                             |                             |     &emsp;* uses Clone unless TODO PC ISSUE #7 - the extant value problem
//!                             |                             |     &emsp;- TODO PC ISSUE #7 adjacent is it even possible to call vm.alloc_string_ro on something that was *newly* created in the current fcn and returned as a RO value OR should that be made as a custom type so the user can declare their intent.
//!                             |                             |
//! [`char`]                    | [`Value`]`::CodePoint`      |
//!                             |                             | S -> R
//!                             |                             |     &emsp;- [`SlInto`] [`char`] for `&`[`Value`]
//!                             |                             | R -> S
//!                             |                             |     &emsp;- [`SlFrom`] `&`[`Value`] for [`char`]
//!                             |                             |
//! [SloshChar]               |  [`Value`]`::CharClusterLong` / [`Value`]`::CharCluster` / [`Value`]`::CodePoint` |
//!                             |                             | S -> R
//!                             |                             |     &emsp;- [`SlIntoRef`] [`SloshChar`] for `&`[`Value`]
//!                             |                             | R -> S
//!                             |                             |     &emsp;- [`SlFromRef`] `&`[`Value`] for [`SloshChar`]
//!                             |                             |
//! [`LooseString`]             | [`Value`]`::String` / [`Value`]`::CodePoint` / [`Value`]`::CharCluster` / [`Value`]`::CharClusterLong` / [`Value`]`::Symbol` / [`Value`]`::Keyword` / [`Value`]`::StringConst` |
//!                             |                             | S -> R
//!                             |                             |     &emsp;- [`SlIntoRef`] [`LooseString`] for `&`[`Value`]
//!                             |                             | R -> S
//!                             |                             |     &emsp;* Note: Always does an allocation and returns a [`Value`]`::String` type.
//!                             |                             |     &emsp;- [`SlFromRef`] `&`[`Value`] for [`LooseString`]
//!                             |                             |
//! [`bool`]                    | [`Value`]::True / [`Value`]::False / [`Value`]::Nil |
//!                             |                             | S -> R
//!                             |                             |     &emsp;- [`Into`] [`bool`] for `&`[`Value`]
//!                             |                             | R -> S
//!                             |                             |     &emsp;- [`SlFrom`] `&`[`Value`] for [`primitives`]
//!                             |                             |
//!                             |                             |
//!                             |                             |
//!                             |                             |
//!                             |                             |
//!                             |                             |
//!  [`Value`]::Byte(u8),
//!  [`Value`]::Int([u8; 7]), // Store a 7 byte int (i56...).
//!  [`Value`]::Float(F56),
//!
//!  [`Value`]::Pair(Handle),
//!  [`Value`]::List(Handle, u16),
//!  [`Value`]::Vector(Handle),
//!  [`Value`]::Map(Handle),
//!
//!  [`Value`]::Symbol(Interned),
//!  [`Value`]::Keyword(Interned),
//!  [`Value`]::Special(Interned), // Intended for symbols that are compiled.
//!  [`Value`]::Builtin(u32),
//!  [`Value`]::Undefined,
//!  [`Value`]::Nil,
//!
//!  [`Value`]::Bytes(Handle),
//!  [`Value`]::Lambda(Handle),
//!  [`Value`]::Closure(Handle),
//!  [`Value`]::Continuation(Handle),
//!  [`Value`]::CallFrame(Handle),
//!  [`Value`]::Error(Handle),

#[cfg(test)]
mod tests {
    #[test]
    fn macro_passing_tests() {
        let t = trybuild::TestCases::new();
        t.pass("tests/*pass.rs");
    }

    #[test]
    fn macro_failing_tests() {
        let t = trybuild::TestCases::new();
        t.compile_fail("trybuild/tests/*fail.rs");
    }
}
