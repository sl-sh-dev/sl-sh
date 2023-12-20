//! TODO PC need explanation for the emulation for TryFrom/TryInto/AsRef/AsMut
//! My notes:
//! #. To convert a slosh &Value to an owned type implement `impl SlFrom<&Value> for OwnedType`,
//!     this allows rust native functions annotated with the bridge macro to receive normal
//!     rust types.
//! #. To convert a slosh &Value to a reference type implement `impl SlAsRef<&Value> for RefType`.
//! #. To convert a slosh &Value to a mutable reference type implement `impl SlAsMut<&Value> for MutRefType`.
//! #. To convert some rust type back to a value that the rust native function
//!     annotated by the bridge macro returns implement `impl SlFrom<&Value> for RustType`.
//!     TODO PC blanket impl so impl `SlFrom<Value>` works, and taking a ref isn't required?
//! #. To avoid allocations when converting a slosh &Value back to a rust type that was mutated
//!     don't return anything. If it is necessary for the API to return some value,
//!     TODO PC annotated or liftime? AKA [the extant value problem]
//!
//!
//! ## rosetta stone for bridge macros
//! Rust Type                   | Slosh Type & Traits   <br>&emsp; <br> S -> R Convert Slosh -> Rust <br> &emsp; - Occurs when coercing slush arguments to the parameter types in the signature of the annotated Rust function. <br> R -> S Convert Rust -> Slosh <br> &emsp; - Occurs when coercing some returned Rust type to a Slosh type. |
//! ----------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------|
//! [`String`]                  | [`Value`]`::String`         |
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
//!                             |                             |     &emsp;* uses Clone unless TODO PC [the extant value problem]
//!                             |                             |
//! `&mut `[`String`]           | [`Value`]`::String`         |
//!                             |                             | S -> R
//!                             |                             |     &emsp;- [`SlAsMut`] [`String`] for `&`[`Value`]
//!                             |                             | R -> S
//!                             |                             |     &emsp;- take `&mut `[`String`]
//!                             |                             |     &emsp;* uses Clone unless TODO PC [the extant value problem]
//!                             |                             |
//! `&`[`str`]                  | [`Value`]`::String` / [`Value`]`::StringConst` |
//!                             |                             | S -> R
//!                             |                             |     &emsp;- [`SlAsRef`] [`str`] for `&`[`Value`]
//!                             |                             | R -> S
//!                             |                             |     &emsp;- [`SlFrom`] for [`Value`]
//!                             |                             |     &emsp;* uses Clone unless TODO PC [the extant value problem]
//!                             |                             |     &emsp;- TODO PC is it even possible to call vm.alloc_string_ro on something that was *newly* created in the current fcn and returned as a RO value OR should that be made as a custom type so the user can declare their intent.
//!                             |                             |
//! [`char`]                    | [`Value`]`::CodePoint`      |
//!                             |                             | S -> R
//!                             |                             |     &emsp;- [`SlInto`] [`char`] for `&`[`Value`]
//!                             |                             | R -> S
//!                             |                             |     &emsp;- [`SlFrom`] `&`[`Value`] for [`char`]
//!                             |                             |
//! [`SloshChar`]               |  [`Value`]`::CharClusterLong` / [`Value`]`::CharCluster` / [`Value`]`::CodePoint` |
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
//!                             |                             |
//! Value::StringConst          |                             |
//! Value::CharCluster          |                             |
//! Value::CharClusterLong      |                             |
//! Value::Byte                 |                             |
//! Value::Int32                |                             |
//! Value::UInt32               |                             |
//! Value::Int64                |                             |
//! Value::UInt64               |                             |
//! Value::Float64              |                             |
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

use std::borrow::Cow;
use bridge_types::{LooseString, SloshChar};
use compile_state::state::SloshVm;
use slvm::{SLOSH_CHAR, Value, VMError, VMResult};

pub trait SlFrom<T>: Sized {
    /// Converts to this type from the input type.
    fn sl_from(value: T, vm: &mut SloshVm) -> VMResult<Self>;
}

pub trait SlInto<T>: Sized {
    /// Converts this type into the (usually inferred) input type.
    fn sl_into(self, vm: &mut SloshVm) -> VMResult<T>;
}

impl<T, U> SlInto<U> for T
where U: SlFrom<T> {
    fn sl_into(self, vm: &mut SloshVm) -> VMResult<U> {
        U::sl_from(self, vm)
    }
}

pub trait SlFromRef<'a, T>: Sized where Self: 'a {
    /// Converts to this type from the input type.
    fn sl_from_ref(value: T, vm: &'a mut SloshVm) -> VMResult<Self>;
}

pub trait SlIntoRef<'a, T>: Sized where T: 'a {
    /// Converts to this type from the input type.
    fn sl_into_ref(self, vm: &'a mut SloshVm) -> VMResult<T>;
}

impl<'a, T, U> SlIntoRef<'a, U> for T
    where U: SlFromRef<'a, T>, U: 'a {
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

impl<'a> SlFromRef<'a, &Value> for LooseString<'a, str> {
    fn sl_from_ref(value: &Value, vm: &'a mut SloshVm) -> VMResult<Self> {
        match value {
            Value::String(h) => {
                Ok(LooseString::Borrowed(vm.get_string(*h)))
            }
            Value::CodePoint(char) => {
                Ok(LooseString::Owned(char.to_string()))
            }
            Value::CharCluster(l, c) => {
                Ok(LooseString::Owned(format!("{}", String::from_utf8_lossy(&c[0..*l as usize]))))
            }
            Value::CharClusterLong(h) => {
                let ch = vm.get_string(*h);
                Ok(LooseString::Borrowed(ch))
            }
            Value::Symbol(i) => {
                Ok(LooseString::Borrowed(vm.get_interned(*i)))
            },
            Value::Keyword(i) => {
                Ok(LooseString::Borrowed(vm.get_interned(*i)))
            },
            Value::StringConst(i) => {
                Ok(LooseString::Borrowed(vm.get_interned(*i)))
            },
            _ => {
                Err(VMError::new_vm("Wrong type, expected something that can be loosely cast to a String."))
            }
        }
    }
}

impl<'a> SlFromRef<'a, LooseString<'a, str>> for Value {
    fn sl_from_ref(value: LooseString<'a, str>, vm: &'a mut SloshVm) -> VMResult<Self> {
        match value {
            LooseString::Borrowed(s) => {
                Ok(vm.alloc_string(s.to_string()))
            }
            LooseString::Owned(s) => {
                Ok(vm.alloc_string(s))
            }
        }
    }
}

impl SlFrom<&Value> for char {
    fn sl_from(value: &Value, _vm: &mut SloshVm) -> VMResult<Self> {
        match value {
            Value::CodePoint(char) => {
                Ok(*char)
            }
            _ => {
                Err(VMError::new_vm("Wrong type, expected something that can be cast to a char."))
            }
        }
    }
}

impl SlFrom<char> for Value {
    fn sl_from(value: char, _vm: &mut SloshVm) -> VMResult<Self> {
        Ok(Value::CodePoint(value))
    }
}

impl<'a> SlAsRef<'a, str> for &Value {
    fn sl_as_ref(&self, vm: &'a mut SloshVm) -> VMResult<&'a str> {
        match self {
            Value::String(h) => {
                Ok(vm.get_string(*h))
            }
            Value::StringConst(i) => {
                Ok(vm.get_interned(*i))
            }
            _ => {
                Err(VMError::new_vm("Wrong type, expected something that can be cast to a &str."))
            }

        }
    }
}

impl<'a> SlFromRef<'a, &Value> for SloshChar<'a> {
    fn sl_from_ref(value: &Value, vm: &'a mut SloshVm) -> VMResult<Self> {
        match value {
            Value::CodePoint(ch) => {
                Ok(SloshChar::Char(*ch))
            }
            Value::CharCluster(l, c) => {
                Ok(SloshChar::String(Cow::Owned(format!("{}", String::from_utf8_lossy(&c[0..*l as usize])))))
            }
            Value::CharClusterLong(h) => {
                Ok(SloshChar::String(Cow::Borrowed(vm.get_string(*h))))
            }
            _ => {
                Err(VMError::new_vm(format!("Wrong type, expected something that can be cast to a {SLOSH_CHAR}.")))
            }
        }
    }
}

impl<'a> SlFromRef<'a, SloshChar<'a>> for Value {
    fn sl_from_ref(value: SloshChar, vm: &'a mut SloshVm) -> VMResult<Self> {
        match value {
            SloshChar::Char(ch) => {
                Ok(Value::CodePoint(ch))
            }
            SloshChar::String(cow) => {
                match cow {
                    Cow::Borrowed(s) => {
                        Ok(vm.alloc_char(s))
                    }
                    Cow::Owned(s) => {
                        Ok(vm.alloc_char(s.as_str()))
                    }
                }
            }
        }
    }
}

impl<'a> SlAsMut<'a, String> for &Value {
    fn sl_as_mut(&mut self, vm: &'a mut SloshVm) -> VMResult<&'a mut String> {
        match self {
            Value::String(h) => {
                Ok(vm.get_string_mut(*h))
            }
            _ => {
                Err(VMError::new_vm("Wrong type, expected something that can be cast to a &mut String."))
            }
        }
    }
}

impl SlFrom<String> for Value {
    fn sl_from(value: String, vm: &mut SloshVm) -> VMResult<Self> {
        Ok(vm.alloc_string(value))
    }
}

impl<T> SlFrom<&T> for Value where T: ToString + ?Sized {
    fn sl_from(value: &T, vm: &mut SloshVm) -> VMResult<Self> {
        Ok(vm.alloc_string(value.to_string()))
    }
}

impl<T> SlFrom<&mut T> for Value where T: ToString + ?Sized {
    fn sl_from(value: &mut T, vm: &mut SloshVm) -> VMResult<Self> {
        Ok(vm.alloc_string(value.to_string()))
    }
}

impl SlFrom<&Value> for String {
    fn sl_from(value: &Value, vm: &mut SloshVm) -> VMResult<Self> {
        match value {
            Value::String(h) => {
                Ok(vm.get_string(*h).to_string())
            }
            _ => {
                Err(VMError::new_vm("Wrong type, expected something that can be cast to a string."))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use compile_state::state::new_slosh_vm;
    use crate::gensym;

    pub const CODE_POINT: char = 'न';
    pub const CHAR_CLUSTER: &'static str = "ते";
    pub const CHAR_CLUSTER_LONG: &'static str = "👩‍💻";

    pub fn create_char_cluster(vm: &mut SloshVm) -> Value {
        let val = vm.alloc_char(CHAR_CLUSTER);
        assert!(matches!(val, Value::CharCluster(_, _)));
        val
    }

    pub fn create_char_cluster_long(vm: &mut SloshVm) -> Value {
        let val = vm.alloc_char(CHAR_CLUSTER_LONG);
        assert!(matches!(val, Value::CharClusterLong(_)));
        val
    }

    pub fn create_code_point() -> Value {
        Value::CodePoint(CODE_POINT)
    }

    pub fn create_string(vm: &mut SloshVm) -> Value {
        let val = vm.alloc_string(CHAR_CLUSTER.to_string() + CHAR_CLUSTER_LONG + CODE_POINT.to_string().as_str());
        assert!(matches!(val, Value::String(_)));
        val
    }

    pub fn create_symbol(vm: &mut SloshVm) -> Value {
        let v = vec![];
        let val = gensym(vm, v.as_slice()).unwrap();
        assert!(matches!(val, Value::Symbol(_)));
        val
    }

    pub fn create_keyword(vm: &mut SloshVm) -> Value {
        let v = vec![];
        let val = gensym(vm, v.as_slice()).unwrap();
        match val {
            Value::Symbol(i) => {
                let val = Value::Keyword(i);
                assert!(matches!(val, Value::Keyword(_)));
                val
            }
            _ => {
                unreachable!("gensym should always return a symbol.")
            }
        }
    }

    pub fn create_string_const(vm: &mut SloshVm) -> Value {
        //TODO PC need more clarification on distincution between
        // 1. mutable Value::String
        // 2. immutable Value::String
        // 3. and Value::StringConst
        let val = vm.intern_static("read_only");
        let val = Value::StringConst(val);
        assert!(matches!(val, Value::StringConst(_)));
        val
    }

    #[test]
    fn try_str_trim() {
        let mut vm = new_slosh_vm();
        let to_trim = " hello world ";
        let val = str_trim_test(&mut vm, to_trim.to_string()).unwrap();
        match val {
            Value::String(handle) => {
                let to_test = vm.get_string(handle);
                assert_eq!(to_test, "hello world");
            }
            _ => {
                panic!("Should return a string!")
            }
        }
    }

    #[test]
    fn try_str_mut() {
        let mut vm = new_slosh_vm();
        let to_mutate = " hello world ";
        let test_str = vm.alloc_string(to_mutate.to_string());
        let args = &[test_str];
        str_test_mut(&mut vm, args).unwrap();
        match args[0] {
            Value::String(handle) => {
                let to_test = vm.get_string(handle);
                assert_eq!(to_test, " hello world 0");
            }
            _ => {
                panic!("Should return a string!")
            }
        }
    }

    fn str_test_mut(vm: &mut SloshVm, args: &[Value]) -> VMResult<()> {
        let fn_name = "str_trim";
        const PARAMS_LEN: usize = 1usize;
        let arg_types: [bridge_types::Param; PARAMS_LEN] =
            [bridge_types::Param {
                handle: bridge_types::TypeHandle::Direct,
                passing_style: bridge_types::PassingStyle::MutReference,
            }];

        let param = arg_types[0usize];
        match param.handle {
            bridge_types::TypeHandle::Direct =>
                match args.get(0usize) {
                    None => {
                        return Err(crate::VMError::new_vm(&*{
                            let res =
                                format!("{} not given enough arguments, expected at least {} arguments, got {}.", fn_name, 1usize, args.len());
                            res
                        }));
                    }
                    Some(mut arg_0) => {
                        {
                            match args.get(PARAMS_LEN) {
                                Some(_) if
                                PARAMS_LEN == 0 ||
                                    arg_types[PARAMS_LEN - 1].handle !=
                                        bridge_types::TypeHandle::VarArgs => {
                                    return Err(crate::VMError::new_vm(&*{
                                        let res =
                                            format!("{} given too many arguments, expected at least {} arguments, got {}.",
                                                    fn_name, 1usize, args.len());
                                        res
                                    }));
                                }
                                _ => {
                                    {
                                        let arg: &mut String = arg_0.sl_as_mut(vm)?;
                                        arg.push_str("0");
                                        Ok(())
                                    }
                                }
                            }
                        }
                    }
                },
            _ => {
                return Err(crate::VMError::new_vm(&*{
                    let res =
                        format!("{} failed to parse its arguments, internal error.",
                                fn_name);
                    res
                }));
            }
        }
    }

    fn str_trim_test(vm: &mut SloshVm, test_str: String) -> VMResult<Value> {
        let test_str = vm.alloc_string(test_str);
        let args = [test_str];
        let fn_name = "str_trim";
        const PARAMS_LEN: usize = 1usize;
        let arg_types: [bridge_types::Param; PARAMS_LEN] =
            [bridge_types::Param {
                handle: bridge_types::TypeHandle::Direct,
                passing_style: bridge_types::PassingStyle::Value,
            }];

        let param = arg_types[0usize];
        match param.handle {
            bridge_types::TypeHandle::Direct =>
                match args.get(0usize) {
                    None => {
                        return Err(crate::VMError::new_vm(&*{
                            let res =
                                format!("{} not given enough arguments, expected at least {} arguments, got {}.", fn_name, 1usize, args.len());
                            res
                        }));
                    }
                    Some(arg_0) => {
                        {
                            match args.get(PARAMS_LEN) {
                                Some(_) if
                                PARAMS_LEN == 0 ||
                                    arg_types[PARAMS_LEN - 1].handle !=
                                        bridge_types::TypeHandle::VarArgs => {
                                    return Err(crate::VMError::new_vm(&*{
                                        let res =
                                            format!("{} given too many arguments, expected at least {} arguments, got {}.",
                                                    fn_name, 1usize, args.len());
                                        res
                                    }));
                                }
                                _ => {
                                    return {
                                        let arg: String = arg_0.sl_into(vm)?;
                                        arg.trim().to_string().sl_into(vm)
                                    }
                                }
                            }
                        }
                    }
                },
            _ => {
                return Err(crate::VMError::new_vm(&*{
                    let res =
                        format!("{} failed to parse its arguments, internal error.",
                                fn_name);
                    res
                }));
            }
        }
    }

    #[test]
    fn test_string_conversions_value_to_rust() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;
        let test_string = &mut "hello world".to_string();
        let val: Value = test_string.sl_into(vm).expect("&mut String can be converted to Value");
        assert!(matches!(val, Value::String(_)));

        let _s: String = (&val).sl_into(vm).expect("&Value::String can be converted to String");
        let _s: &str = (&val).sl_as_ref(vm).expect("&Value::String can be converted to &str");
        let _s: &mut String = (&val).sl_as_mut(vm).expect("&Value::String can be converted to &mut String");
    }

    #[test]
    fn test_string_conversions_rust_to_value() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;

        let test_string = "hello world";
        let val: Value = test_string.sl_into(vm).expect("&str can be converted to Value");
        assert!(matches!(val, Value::String(_)));

        let test_string = "hello world".to_string();
        let val: Value = test_string.sl_into(vm).expect("String can be converted to Value");
        assert!(matches!(val, Value::String(_)));

        let test_string = "hello world".to_string();
        let val: Value = (&test_string).sl_into(vm).expect("&String can be converted to Value");
        assert!(matches!(val, Value::String(_)));

        let mut test_string = "hello world".to_string();
        let val: Value = (&mut test_string).sl_into(vm).expect("&String can be converted to Value");
        assert!(matches!(val, Value::String(_)));
    }

    #[test]
    fn test_char_conversions_value_to_rust() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;

        let val = create_code_point();
        let _c: char = (&val).sl_into(vm).expect("&Value::CodePoint can be converted to char");
    }

    #[test]
    fn test_char_conversions_rust_to_value() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;

        let val: Value = CODE_POINT.sl_into(vm).expect("char can be converted to Value");
        assert!(matches!(val, Value::CodePoint(_)));
    }

    #[test]
    fn test_char_cluster_conversions_value_to_rust() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;

        let val = create_char_cluster(vm);
        let _c: SloshChar = (&val).sl_into_ref(vm).expect("&Value::CharCluster can be converted to SloshChar");

        let val = create_char_cluster_long(vm);
        let _c: SloshChar = (&val).sl_into_ref(vm).expect("&Value::CharClusterLong can be converted to SloshChar");
    }

    #[test]
    fn test_char_cluster_conversions_rust_to_value() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;

        let rust_char_cluster = SloshChar::String(Cow::Owned(CHAR_CLUSTER.to_string()));
        let val: Value = SlFromRef::sl_from_ref(rust_char_cluster, vm).expect("&SloshChar can be converted to &Value");
        assert!(matches!(val, Value::CharCluster(_, _)));

        let rust_char_cluster = SloshChar::String(Cow::Borrowed(CHAR_CLUSTER_LONG));
        let val: Value = SlFromRef::sl_from_ref(rust_char_cluster, vm).expect("&SloshChar can be converted to &Value");
        assert!(matches!(val, Value::CharClusterLong(_)));
    }

    pub fn get_values_that_can_be_cast_to_loose_strings(vm: &mut SloshVm) -> Vec<Value> {
        vec![
            create_string(vm),
            create_code_point(),
            create_string_const(vm),
            create_char_cluster(vm),
            create_char_cluster_long(vm),
            create_symbol(vm),
            create_keyword(vm),
        ]
    }

    #[test]
    fn test_loose_string_conversions_value_to_rust() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;

        let loose_strings_as_vals = get_values_that_can_be_cast_to_loose_strings(vm);

        for val in loose_strings_as_vals {
            let _loose_string: LooseString<str> = (&val).sl_into_ref(vm).expect("This value should be convertable to a LooseString");
        }
    }

    #[test]
    fn test_loose_string_conversion_rust_to_value() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;

        let sample = LooseString::Owned("hello world".to_string());
        let val: Value = SlFromRef::sl_from_ref(sample, vm).expect("This LooseString should be convertable to a Value");
        assert!(matches!(val, Value::String(_)));
    }
}