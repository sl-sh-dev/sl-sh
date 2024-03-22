//! My notes:
//! #. To convert a rust value to an slosh type `impl SlFrom<T> for Value`,
//! #. To convert a slosh &Value to an owned type implement `impl SlFromRef<'a, &Value> for RustType`,
//!     this allows rust native functions annotated with the bridge macro to receive normal
//!     rust types.
//! #. To convert a slosh &Value to a mutable reference type implement `impl SlAsMut<&Value> for MutRefType`.
//! #. To convert some rust type back to a value that the rust native function
//!     annotated by the bridge macro returns implement `impl SlFrom<&Value> for RustType`.
//! TODO PC ISSUE #7 - returning values via annotated or lifetime?
//!  To avoid allocations when converting a slosh &Value back to a rust type that was mutated
//!  don't return anything. If it is necessary for the API to return some value.
//! ...either use an annotation on an input argument `fn myfun(#[likethis] returnme: &mut String, someotherval: String) -> VMResult<()>`
//! or a lifetime... might be easier to do the annotation.
//!
//!
//! ## rosetta stone for bridge macros
//! Rust Type                   | Slosh Type & Traits   <br>&emsp; <br> S -> R Convert Slosh -> Rust <br> &emsp; - Occurs when coercing slush arguments to the parameter types in the signature of the annotated Rust function. <br> R -> S Convert Rust -> Slosh <br> &emsp; - Occurs when coercing some returned Rust type to a Slosh type. |
//! ----------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------|
//! [`String`]                  | [Value]`::String`           |
//!                             |                             | S -> R
//!                             |                             |     &emsp;- [`SlIntoRef`] [`String`] for `&`[`Value`]
//!                             |                             | R -> S
//!                             |                             |     &emsp;- [`SlFrom`] `&`[`Value`] for [`String`]
//!                             |                             |
//! `&`[`String`]               | [`Value`]`::String`         |
//!                             |                             | S -> R
//!                             |                             |     &emsp;- [`SlIntoRef`] `&`[`String`] for `&`[`Value`]
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
//!                             |                             |     &emsp;- [`SlAsRef`] &[`str`] for `&`[`Value`]
//!                             |                             |     &emsp;- [`SlIntoRef`] &[`str`] for `&`[`Value`]
//!                             |                             | R -> S
//!                             |                             |     &emsp;- [`SlFrom`] for [`Value`]
//!                             |                             |     &emsp;* uses Clone unless TODO PC ISSUE #7 - the extant value problem
//!                             |                             |     &emsp;- TODO PC ISSUE #7 adjacent is it even possible to call vm.alloc_string_ro on something that was *newly* created in the current fcn and returned as a RO value OR should that be made as a custom type so the user can declare their intent.
//!                             |                             |
//! [`char`]                    | [`Value`]`::CodePoint`      |
//!                             |                             | S -> R
//!                             |                             |     &emsp;- [`SlIntoRef`] [`char`] for `&`[`Value`]
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
//!                             |                             |     &emsp;- [`SlIntoRef`] [`bool`] for `&`[`Value`]
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

use bridge_types::BridgedType;
#[cfg(doc)]
use bridge_types::{LooseString, SloshChar};

use compile_state::state::SloshVm;

use slvm::{VMResult, Value};
pub mod numbers;
pub mod primitives;
pub mod text;

/// Use mutable [`SloshVm`] to take a rust value and convert it to a [`BridgedType`].
pub trait SlFrom<T>: Sized
where
    Self: BridgedType,
{
    /// Converts to this type from the input type.
    fn sl_from(value: T, vm: &mut SloshVm) -> VMResult<Self>;
}

impl<T> SlFrom<Vec<T>> for Value where T: SlInto<Value> {
    fn sl_from(value: Vec<T>, vm: &mut SloshVm) -> VMResult<Self> {
        let mut u = Vec::with_capacity(value.len());
        for v in value {
            u.push(v.sl_into(vm)?);
        }
        Ok(vm.alloc_vector(u))
    }
}

impl<'a, T> SlFromRef<'a, slvm::Value> for Vec<T> where T: SlFromRef<'a, Value> + 'a + ?Sized {
    fn sl_from_ref(value: slvm::Value, vm: &'a SloshVm) -> VMResult<Self> {
        let mut res = vec![];
        for val in value.iter(vm) {
            let v = val.clone();
            let t: T = v.sl_into_ref(vm)?;
            res.push(t);
        }
        Ok(res)
    }
}

/// Inverse of [`SlFrom`]
pub trait SlInto<T>: Sized
where
    T: BridgedType,
{
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

pub trait SlFromRef<'a, T: BridgedType>
where
    Self: Sized,
{
    /// Converts to this type from the input type.
    fn sl_from_ref(value: T, vm: &'a SloshVm) -> VMResult<Self>;
}

pub trait SlIntoRef<'a, T>: Sized
where
    T: 'a,
    Self: BridgedType,
{
    /// Converts to this type from the input type.
    fn sl_into_ref(self, vm: &'a SloshVm) -> VMResult<T>;
}

impl<'a, T, U> SlIntoRef<'a, U> for T
where
    T: BridgedType,
    U: SlFromRef<'a, T>,
    U: 'a,
{
    fn sl_into_ref(self, vm: &'a SloshVm) -> VMResult<U> {
        U::sl_from_ref(self, vm)
    }
}

pub trait SlFromRefMut<'a, T: BridgedType>
    where
        Self: Sized,
{
    /// Converts to this type from the input type.
    fn sl_from_ref_mut(value: T, vm: &'a mut SloshVm) -> VMResult<Self>;
}

pub trait SlIntoRefMut<'a, T>: Sized
    where
        T: 'a,
        Self: BridgedType,
{
    /// Converts to this type from the input type.
    fn sl_into_ref_mut(self, vm: &'a mut SloshVm) -> VMResult<T>;
}


impl<'a, T, U> SlIntoRefMut<'a, U> for T
    where
        T: BridgedType,
        U: SlFromRefMut<'a, T>,
        U: 'a,
{
    fn sl_into_ref_mut(self, vm: &'a mut SloshVm) -> VMResult<U> {
        U::sl_from_ref_mut(self, vm)
    }
}


/// Converts a [`BridgedType`] to some rust type
pub trait SlAsRef<'a, T: ?Sized>
where
    Self: BridgedType,
{
    // where Self: BridgedType,
    /// Converts this type into a shared reference of the (usually inferred) input type.
    fn sl_as_ref(&self, vm: &'a SloshVm) -> VMResult<&'a T>;
}

// SlAsRef lifts over &
impl<'a, T: ?Sized, U: ?Sized> SlAsRef<'a, U> for &'a T
where
    T: SlAsRef<'a, U>,
    &'a T: BridgedType,
{
    #[inline]
    fn sl_as_ref(&self, vm: &'a SloshVm) -> VMResult<&'a U> {
        <T as SlAsRef<'a, U>>::sl_as_ref(*self, vm)
    }
}

// SlAsRef lifts over &mut
impl<'a, T: ?Sized, U: ?Sized> SlAsRef<'a, U> for &'a mut T
where
    T: SlAsRef<'a, U> + BridgedType,
    &'a mut T: BridgedType,
{
    #[inline]
    fn sl_as_ref(&self, vm: &'a SloshVm) -> VMResult<&'a U> {
        <T as SlAsRef<'a, U>>::sl_as_ref(*self, vm)
    }
}

pub trait SlAsMut<'a, T: ?Sized>
where
    Self: BridgedType,
{
    /// Converts this type into a mutable reference of the (usually inferred) input type.
    fn sl_as_mut(&mut self, vm: &'a mut SloshVm) -> VMResult<&'a mut T>;
}

// SlAsMut lifts over &mut
impl<'a, T: ?Sized, U: ?Sized> SlAsMut<'a, U> for &'a mut T
where
    T: SlAsMut<'a, U>,
    &'a mut T: BridgedType,
{
    #[inline]
    fn sl_as_mut(&mut self, vm: &'a mut SloshVm) -> VMResult<&'a mut U> {
        (*self).sl_as_mut(vm)
    }
}

impl SlFrom<Value> for Value {
    fn sl_from(value: Value, _vm: &mut SloshVm) -> VMResult<Self> {
        Ok(value)
    }
}
