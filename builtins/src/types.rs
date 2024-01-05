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
//! [`String`]                  | [Value]`::String`         |
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
//! Value::Int32                |                             |
//! Value::UInt32               |                             |
//! Value::Int64                |                             |
//! Value::UInt64               |                             |
//! Value::Float64              |                             |
//! Value::Byte                 |                             |
//! Value::Symbol               |                             |
//! Value::Keyword              |                             |
//! Value::Special              |                             |
//! Value::Builtin              |                             |
//! Value::True                 |                             |
//! Value::False                |                             |
//! Value::Nil                  |                             |
//! Value::Undefined            |                             |
//! Value::Vector               |                             |
//! Value::PersistentVec        |                             |
//! Value::VecNode              |                             |
//! Value::PersistentMap        |                             |
//! Value::MapNode              |                             |
//! Value::Map                  |                             |
//! Value::Bytes                |                             |
//! Value::Pair                 |                             |
//! Value::List                 |                             |
//! Value::Lambda               |                             |
//! Value::Closure              |                             |
//! Value::Continuation         |                             |
//! Value::CallFrame            |                             |
//! Value::Value                |                             |
//! Value::Error                |                             |
//! Value::StringConst          |                             |

mod numbers;
pub mod string_char;

use compile_state::state::SloshVm;
use slvm::VMResult;
#[cfg(doc)]
use {
    bridge_types::{LooseString, SloshChar},
    slvm::Value,
};

pub trait SlFrom<T>: Sized {
    /// Converts to this type from the input type.
    fn sl_from(value: T, vm: &mut SloshVm) -> VMResult<Self>;
}

pub trait SlInto<T>: Sized {
    /// Converts this type into the (usually inferred) input type.
    fn sl_into(self, vm: &mut SloshVm) -> VMResult<T>;
}

impl<T, U> SlInto<U> for T
where
    U: SlFrom<T>,
{
    fn sl_into(self, vm: &mut SloshVm) -> VMResult<U> {
        U::sl_from(self, vm)
    }
}

pub trait SlFromRef<'a, T>: Sized
where
    Self: 'a,
{
    /// Converts to this type from the input type.
    fn sl_from_ref(value: T, vm: &'a mut SloshVm) -> VMResult<Self>;
}

pub trait SlIntoRef<'a, T>: Sized
where
    T: 'a,
{
    /// Converts to this type from the input type.
    fn sl_into_ref(self, vm: &'a mut SloshVm) -> VMResult<T>;
}

impl<'a, T, U> SlIntoRef<'a, U> for T
where
    U: SlFromRef<'a, T>,
    U: 'a,
{
    fn sl_into_ref(self, vm: &'a mut SloshVm) -> VMResult<U> {
        U::sl_from_ref(self, vm)
    }
}

pub trait SlAsRef<'a, T: ?Sized> {
    /// Converts this type into a shared reference of the (usually inferred) input type.
    fn sl_as_ref(&self, vm: &'a mut SloshVm) -> VMResult<&'a T>;
}

// SlAsRef lifts over &
impl<'a, T: ?Sized, U: ?Sized> SlAsRef<'a, U> for &'a T
where
    T: SlAsRef<'a, U>,
{
    #[inline]
    fn sl_as_ref(&self, vm: &'a mut SloshVm) -> VMResult<&'a U> {
        <T as SlAsRef<'a, U>>::sl_as_ref(*self, vm)
    }
}

// SlAsRef lifts over &mut
impl<'a, T: ?Sized, U: ?Sized> SlAsRef<'a, U> for &'a mut T
where
    T: SlAsRef<'a, U>,
{
    #[inline]
    fn sl_as_ref(&self, vm: &'a mut SloshVm) -> VMResult<&'a U> {
        <T as SlAsRef<'a, U>>::sl_as_ref(*self, vm)
    }
}

pub trait SlAsMut<'a, T: ?Sized> {
    /// Converts this type into a mutable reference of the (usually inferred) input type.
    fn sl_as_mut(&mut self, vm: &'a mut SloshVm) -> VMResult<&'a mut T>;
}

// SlAsMut lifts over &mut
impl<'a, T: ?Sized, U: ?Sized> SlAsMut<'a, U> for &'a mut T
where
    T: SlAsMut<'a, U>,
{
    #[inline]
    fn sl_as_mut(&mut self, vm: &'a mut SloshVm) -> VMResult<&'a mut U> {
        (*self).sl_as_mut(vm)
    }
}
