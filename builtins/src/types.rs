//! [`RustProcedure`] and [`RustProcedureRefMut`] are traits that are used to implement type conversions
//! from [`Value`] to Rust types that take a callback function so the various arguments to rust
//! native functions can be curried by recursively applied callbacks doing so -in place- to avoid
//! needing to copy the data.
//!
//!  TODO PC ( this is why the fun() is applied at the core of each statement that matches self in a rust procedure )
//!     however, in the new world... if every single tuple item in the [`Value`] enum is copy then
//!     maybe it doesn't matter anymore and this strategy can be abandoned.
//!
//! Is TryIntoExpression still needed?
//! It looks like nothing can be converted From Rust Type to Value without vm. So, something new will
//! need to be figured out here.

use std::marker::PhantomData;
use compile_state::state::SloshVm;
use slvm::{Value, VMError, VMResult};

// TODO PC Can this move to it's own crate maybe? maybe one that's a la carte with slsh-proc-macros?
// TODO PC is this even needed anymore
pub struct TypedWrapper<'a, T: ?Sized, U>(&'a U, PhantomData<T>);

impl<'a, T: ?Sized, U> TypedWrapper<'a, T, U> {
    pub fn new(src: &'a U) -> TypedWrapper<T, U> {
        TypedWrapper(src, PhantomData::default())
    }
}

pub trait SlTryFrom<T>: Sized {
    /// Converts to this type from the input type.
    fn sl_try_from(value: T, vm: &mut SloshVm) -> VMResult<Self>;
}

pub trait SlTryInto<T>: Sized {
    /// Converts this type into the (usually inferred) input type.
    fn sl_try_into(self, vm: &mut SloshVm) -> VMResult<T>;
}

impl<T, U> SlTryInto<U> for T
where U: SlTryFrom<T> {
    fn sl_try_into(self, vm: &mut SloshVm) -> VMResult<U> {
        U::sl_try_from(self, vm)
    }
}

impl SlTryFrom<String> for Value {
    fn sl_try_from(value: String, vm: &mut SloshVm) -> VMResult<Self> {
        Ok(vm.alloc_string(value))
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

pub trait RustProcedure<T, F>
    where
        Self: Sized,
        F: FnOnce(T, &mut SloshVm) -> VMResult<Value> + ?Sized,
{
    fn apply(&self, vm: &mut SloshVm, fn_name: &str, fun: F) -> VMResult<Value>;
}

/// Used by sl_sh_fn macro to embed information at runtime about the parameters of
/// the rust native function, specifically whether it is a normal Type, or some
/// supported wrapped type, e.g. Optional.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TypeHandle {
    Direct,
    Optional,
    VarArgs,
}

/// Used by sl_sh_fn macro to embed information at runtime about the parameters of
/// the rust native function, specifically whether it is going to pass the value (a move),
/// a reference, or mutable reference.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PassingStyle {
    Value,
    Reference,
    MutReference,
}

/// Struct used by sl_sh_fn macro to embed information in an array at runtime about each of
/// the parameters of the rust native function.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Param {
    pub handle: TypeHandle,
    pub passing_style: PassingStyle,
}

pub struct ErrorStrings {}

impl ErrorStrings {
    pub fn mismatched_type(fn_name: &str, expected: &str, got: &str) -> String {
        format!("{fn_name}: mismatched type input, expected {expected}, got {got}.")
    }
}

#[macro_export]
macro_rules! try_inner_string {
    ($fn_name:ident, $expression:expr, $name:ident, $eval:expr) => {{
        use $crate::ErrorStrings;
        match &$expression.get().data {
            Value::String($name, _) => $eval,
            Value::Symbol($name, _) => $eval,
            Value::Char($name) => $eval,
            _ => {
                return Err($crate::LispError::new(ErrorStrings::mismatched_type(
                    $fn_name,
                    &format!(
                        "{}, {}, or {}, ",
                        Value::String(Default::default(), Default::default()).to_string(),
                        Value::Symbol(Default::default(), Default::default()).to_string(),
                        Value::Char(Default::default()).to_string()
                    ),
                    &$expression.to_string(),
                )))
            }
        }
    }};
}

impl<F> RustProcedure<String, F> for TypedWrapper<'_, String, Value>
    where
        F: FnOnce(String, &mut SloshVm) -> VMResult<Value>,
{
    fn apply(&self, vm: &mut SloshVm, fn_name: &str, fun: F) -> VMResult<Value> {
        // TODO PC which other of these types do we consider to be "cast"-able to a
        // string in the context of Rust functions that implement "this" macro.
        match self.0 {
            Value::String(h) => {
                let h = vm.get_string(*h);
                fun(h.to_string(), vm)
            }
            Value::CodePoint(char) => {
                let s = *char;
                let s = s.to_string();
                fun(s, vm)
            }
            Value::CharCluster(l, c) => {
                let s = format!("{}", String::from_utf8_lossy(&c[0..*l as usize]));
                fun(s, vm)
            }
            Value::CharClusterLong(h) => {
                let ch = vm.get_string(*h);
                fun(ch.to_string(), vm)
            }
            Value::Symbol(i) => {
                let s = vm.get_interned(*i).to_string();
                fun(s, vm)
            },
            Value::Keyword(i) => {
                let s = format!(":{}", vm.get_interned(*i));
                fun(s, vm)
            },
            Value::StringConst(i) => {
                let s = format!("\"{}\"", vm.get_interned(*i));
                fun(s, vm)
            },
            _ => {
                Err(VMError::new("conv", "Wrong type, expected something that can be cast to a string."))
            }
        }
    }
}

impl<'a> SlAsRef<'a, str> for Value {
    fn sl_as_ref(&self, vm: &'a mut SloshVm) -> VMResult<&'a str> {
        match self {
            Value::String(h) => {
                Ok(vm.get_string(*h))
            }
            _ => {
                Err(VMError::new("conv", "Wrong type, expected something that can be cast to a &str."))
            }

        }
    }
}

impl SlTryFrom<Value> for String {
    fn sl_try_from(value: Value, vm: &mut SloshVm) -> VMResult<Self> {
        match value {
            Value::String(h) => {
                Ok(vm.get_string(h).to_string())
            }
            Value::CodePoint(char) => {
                let s = char;
                Ok(s.encode_utf8(&mut [0; 4]).to_string())
            }
            Value::CharCluster(l, c) => {
                Ok(format!("{}", String::from_utf8_lossy(&c[0..l as usize])))
            }
            Value::CharClusterLong(h) => {
                Ok(vm.get_string(h).to_string())
            }
            Value::Symbol(i) => {
                Ok(vm.get_interned(i).to_string())
            },
            Value::Keyword(i) => {
                Ok(vm.get_interned(i).to_string())
            },
            Value::StringConst(i) => {
                Ok(vm.get_interned(i).to_string())
            },
            _ => {
                Err(VMError::new("conv", "Wrong type, expected something that can be cast to a string."))
            }
        }
    }
}



#[cfg(test)]
mod test {
    use compile_state::state::new_slosh_vm;
    use super::*;

    fn str_trim(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
        let mut i = registers.iter();
        let right = vm.intern("right");
        let left = vm.intern("left");
        match (i.next(), i.next(), i.next()) {
            (Some(string), None, None) => {
                let string = string.get_string(vm)?.trim().to_string();
                Ok(vm.alloc_string(string))
            }
            (Some(string), Some(Value::Keyword(i)), None) if *i == right => {
                let string = string.get_string(vm)?.trim_end().to_string();
                Ok(vm.alloc_string(string))
            }
            (Some(string), Some(Value::Keyword(i)), None) if *i == left => {
                let string = string.get_string(vm)?.trim_start().to_string();
                Ok(vm.alloc_string(string))
            }
            _ => Err(VMError::new_vm(
                "str-trim: takes one argument with optional left/right keyword".to_string(),
            )),
        }
    }


    #[test]
    fn try_orig_str_trim() {
        let to_trim = " hello world ";
        let mut vm = new_slosh_vm();
        let test_str = vm.alloc_string(to_trim.to_string());
        let args = [test_str];
        let val = str_trim(&mut vm, &args).unwrap();
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
    fn try_str_trim() {
        let mut vm = new_slosh_vm();
        let to_trim = " hello world ";
        let val = str_trim_test(&mut vm, to_trim.to_string()).unwrap();
        match val {
            Value::String(handle) => {
                println!("handle2: {:?}", handle);
                let to_test = vm.get_string(handle);
                assert_eq!(to_test, "hello world");
            }
            _ => {
                panic!("Should return a string!")
            }
        }
    }

    fn str_trim_test(vm: &mut SloshVm, test_str: String) -> VMResult<Value> {
        let test_str = vm.alloc_string(test_str);
        let args = [test_str];
        let fn_name = "str_trim";
        const PARAMS_LEN: usize = 1usize;
        let arg_types: [crate::types::Param; PARAMS_LEN] =
            [crate::types::Param {
                handle: crate::types::TypeHandle::Direct,
                passing_style: crate::types::PassingStyle::Value,
            }];

        let param = arg_types[0usize];
        match param.handle {
            crate::types::TypeHandle::Direct =>
                match args.get(0usize) {
                    None => {
                        return Err(crate::VMError::new("conv", &*{
                            let res =
                                format!("{} not given enough arguments, expected at least {} arguments, got {}.", fn_name, 1usize, args.len());
                            res
                        }));
                    }
                    Some(arg_0) => {
                        {
                            let typed_data:
                                crate::types::TypedWrapper<String,
                                    crate::Value> =
                                crate::types::TypedWrapper::new(&arg_0);
                            let callback =
                                |arg_0: String, vm: &mut SloshVm|
                                 -> crate::VMResult<crate::Value>
                                    {
                                        match args.get(PARAMS_LEN) {
                                            Some(_) if
                                            PARAMS_LEN == 0 ||
                                                arg_types[PARAMS_LEN - 1].handle !=
                                                    crate::types::TypeHandle::VarArgs => {
                                                return Err(crate::VMError::new("conv", &*{
                                                    let res =
                                                        format!("{} given too many arguments, expected at least {} arguments, got {}.",
                                                                fn_name, 1usize, args.len());
                                                    res
                                                }));
                                            }
                                            _ => {
                                                return {
                                                    let arg: String = arg_0;
                                                    arg.trim().to_string().sl_try_into(vm)
                                                }
                                            }
                                        }
                                    };
                            typed_data.apply(vm, fn_name, callback)
                        }
                    }
                },
            _ => {
                return Err(crate::VMError::new("conv", &*{
                    let res =
                        format!("{} failed to parse its arguments, internal error.",
                                fn_name);
                    res
                }));
            }
        }
    }
}

// impl<F> RustProcedureRefMut<String, F> for TypedWrapper<'_, String, Value>
//     where
//         F: FnOnce(&mut String, &mut SloshVm) -> VMResult<Value>,
// {
//     fn apply_ref_mut(&mut self, vm: &mut SloshVm, fn_name: &str, fun: F) -> VMResult<Value> {
//         // TODO PC which other of these types do we consider to be "cast"-able to a
//         // string in the context of Rust functions that implement "this" macro.
//         match self.0 {
//             Value::String(h) => {
//                 let h = vm.get_string_mut(*h);
//                 fun(h, vm)
//             }
//             Value::CodePoint(char) |
//             Value::CodePoint(char) => {
//                 let s = *char;
//                 let mut s = s.to_string();
//                 fun(&mut s, vm)
//             }
//             Value::CharCluster(l, c) => {
//                 let mut s = format!("{}", String::from_utf8_lossy(&c[0..*l as usize]));
//                 fun(&mut s, vm)
//             }
//             Value::CharClusterLong(h) => {
//                 let ch = vm.get_string_mut(*h);
//                 fun(ch, vm)
//             }
//             Value::Symbol(i) => {
//                 let mut s = vm.get_interned(*i).to_string();
//                 fun(&mut s, vm)
//             },
//             Value::Keyword(i) => {
//                 let mut s = format!(":{}", vm.get_interned(*i));
//                 fun(&mut s, vm)
//             },
//             Value::StringConst(i) => {
//                 let mut s = format!("\"{}\"", vm.get_interned(*i));
//                 fun(&mut s, vm)
//             },
//             _ => {
//                 Err(VMError::new("conv", format!("{fn_name}: Wrong type, expected something that can be cast to a string.")))
//             }
//         }
//     }
// }


// impl<F> RustProcedure<&Value, F> for TypedWrapper<'_, Value, Value>
//     where
//         F: FnOnce(&Value) -> VMResult<Value>,
// {
//     fn apply(&self, _vm: &mut SloshVm, _fn_name: &str, fun: F) -> VMResult<Value> {
//         fun(&mut self.0.clone())
//     }
// }

// impl<F> RustProcedure<i64, F> for TypedWrapper<'_, i64, Value>
//     where
//         F: FnOnce(i64) -> VMResult<Value>,
// {
//     fn apply(&self, fn_name: &str, fun: F) -> VMResult<Value> {
//         try_inner_int!(fn_name, self.0, num, fun(num))
//     }
// }

// impl<F> RustProcedure<f64, F> for TypedWrapper<'_, f64, Value>
//     where
//         F: FnOnce(f64) -> VMResult<Value>,
// {
//     fn apply(&self, fn_name: &str, fun: F) -> VMResult<Value> {
//         match &self.0.get().data {
//             Value::Float(f) => fun(*f),
//             Value::Int(i) => fun(*i as f64),
//             _ => {
//                 let expected = Value::Float(f64::default()).to_string()
//                     + ", or "
//                     + &Value::Int(i64::default()).to_string();
//                 Err(VMError::new(ErrorStrings::mismatched_type(
//                     fn_name,
//                     &expected,
//                     &self.0.to_string(),
//                 )))
//             }
//         }
//     }
// }

// impl<F> RustProcedure<Rc<RefCell<FileState>>, F>
// for TypedWrapper<'_, Rc<RefCell<FileState>>, Value>
//     where
//         F: FnOnce(Rc<RefCell<FileState>>) -> VMResult<Value>,
// {
//     fn apply(&self, fn_name: &str, fun: F) -> VMResult<Value> {
//         try_inner_file!(fn_name, self.0, file, fun(file))
//     }
// }


// pub trait TryIntoExpression<T>: Sized
//     where
//         Self: ToString + TryInto<T>,
// {
//     type Error;
//
//     fn human_readable_dest_type(&self) -> String;
//
//     fn try_into_for(self, fn_name: &str) -> VMResult<T> {
//         let hr_src_type = self.to_string();
//         let hr_dest_type = self.human_readable_dest_type();
//         let t = self.try_into();
//         match t {
//             Ok(t) => Ok(t),
//             Err(_) => Err(VMError::new("conv", ErrorStrings::mismatched_type(
//                 fn_name,
//                 &hr_dest_type,
//                 &hr_src_type,
//             ))),
//         }
//     }
// }
//
// impl TryIntoExpression<Value> for Value {
//     type Error = VMError;
//
//     fn human_readable_dest_type(&self) -> String {
//         self.display_value()
//     }
// }
//
// impl TryIntoExpression<String> for Value {
//     type Error = VMError;
//
//     fn human_readable_dest_type(&self) -> String {
//
//         ExpEnum::String(Cow::from(String::default()), Default::default()).to_string()
//     }
// }
//
// impl From<String> for Expression {
//     fn from(src: String) -> Self {
//         Expression::alloc_data(ExpEnum::String(src.into(), None))
//     }
// }

//
// impl TryFrom<Expression> for String {
//     type Error = LispError;
//
//     fn try_from(value: Expression) -> Result<Self, Self::Error> {
//         match &value.get().data {
//             ExpEnum::String(cow, _) => Ok(cow.to_string()),
//             ExpEnum::Symbol(sym, _) => Ok(sym.to_string()),
//             ExpEnum::Char(ch) => Ok(ch.to_string()),
//             _ => Err(LispError::new(
//                 "Can only convert String from ExpEnum::String.",
//             )),
//         }
//     }
// }

// impl<F> RustProcedure<&str, F> for TypedWrapper<'_, &str, Value>
//     where
//         F: FnOnce(&str) -> VMResult<Value>,
// {
//     fn apply(&self, mut vm: &mut SloshVm, fn_name: &str, fun: F) -> VMResult<Value> {
//         // TODO PC which other of these types do we consider to be "cast"-able to a
//         // string in the context of Rust functions that implement "this" macro.
//         match self.0 {
//             Value::String(h) => {
//                 let h = vm.get_string_mut(*h);
//                 fun(h)
//             }
//             Value::CodePoint(char) |
//             Value::CodePoint(char) => {
//                 let s = *char;
//                 let mut s = s.to_string();
//                 fun(&mut s)
//             }
//             Value::CharCluster(l, c) => {
//                 let mut s = format!("{}", String::from_utf8_lossy(&c[0..*l as usize]));
//                 fun(&mut s)
//             }
//             Value::CharClusterLong(h) => {
//                 let ch = vm.get_string_mut(*h);
//                 fun(ch)
//             }
//             Value::Symbol(i) => {
//                 let mut s = vm.get_interned(*i).to_string();
//                 fun(&mut s)
//             },
//             Value::Keyword(i) => {
//                 let mut s = format!(":{}", vm.get_interned(*i));
//                 fun(&mut s)
//             },
//             Value::StringConst(i) => {
//                 let mut s = format!("\"{}\"", vm.get_interned(*i));
//                 fun(&mut s)
//             },
//             _ => {
//                 Err(VMError::new("conv", "Wrong type, expected something that can be cast to a string."))
//             }
//         }
//     }
// }

// impl<F> RustProcedureRefMut<HashMap<&str, Value>, F>
// for TypedWrapper<'_, HashMap<&str, Value>, Value>
//     where
//         F: FnOnce(&mut HashMap<&str, Value>) -> VMResult<Value>,
// {
//     fn apply_ref_mut(&mut self, fn_name: &str, fun: F) -> VMResult<Value> {
//         try_inner_hash_map_mut!(fn_name, self.0, arg, fun(arg))
//     }
// }
//
// impl<F> RustProcedure<HashMap<&str, Value>, F>
// for TypedWrapper<'_, HashMap<&str, Value>, Value>
//     where
//         F: FnOnce(HashMap<&str, Value>) -> VMResult<Value>,
// {
//     fn apply(&self, fn_name: &str, fun: F) -> VMResult<Value> {
//         try_inner_hash_map!(fn_name, self.0, arg, fun(arg.clone()))
//     }
// }

//// still struggling w/ compiler about how TryFromSlosh<&str> is going to work.
//// since the Value enum is not actually the "actual" thing that owns the data we do not necessarily
//// need the approach in sl-sh where a closure was used to prevent needing to return the inner data
//// from the Expression enum... but it does need to work!
//pub trait TryFromSlosh<'a, T> where T: 'a {
//    fn try_from_slosh(&'a self, vm: &mut SloshVm, val: &Value) -> VMResult<T>;
//}
//
//pub trait TryIntoSlosh {
//    fn try_into_slosh(self, vm: &mut SloshVm) -> VMResult<Value>;
//}
//
//impl TryIntoSlosh for String {
//    fn try_into_slosh(self, vm: &mut SloshVm) -> VMResult<Value> {
//        Ok(vm.alloc_string(self))
//    }
//}
//
//impl TryFromSlosh<'_, String> for TypedWrapper<'_, String, Value> {
//    fn try_from_slosh(&self, vm: &mut SloshVm, val: &Value) -> VMResult<String> {
//        vm_to_string(vm, val)
//    }
//}
//
//impl<'b> TryFromSlosh<'b, &'b str> for TypedWrapper<'b, &'b str, Value> {
//    fn try_from_slosh(&'b self, vm: &'b mut SloshVm, val: &'b Value) -> VMResult<&'b str> {
//        vm_to_string_ref(vm, val)
//    }
//}

// pub trait From<T>: Sized {
//     /// Converts to this type from the input type.
//     fn from(value: T) -> Self;
// }

// pub trait Into<T>: Sized {
//     /// Converts this type into the (usually inferred) input type.
//     #[must_use]
//     #[stable(feature = "rust1", since = "1.0.0")]
//     fn into(self) -> T;
// }


//// From implies Into
//#[stable(feature = "rust1", since = "1.0.0")]
//impl<T, U> Into<U> for T
//    where
//        U: From<T>,
//{
//    /// Calls `U::from(self)`.
//    ///
//    /// That is, this conversion is whatever the implementation of
//    /// <code>[From]&lt;T&gt; for U</code> chooses to do.
//    #[inline]
//    fn into(self) -> U {
//        U::from(self)
//    }
//}

//pub trait RustProcedureRefMut<T, F>
//    where
//        Self: Sized,
//        F: FnOnce(&mut T, &mut SloshVm) -> VMResult<Value> + ?Sized,
//{
//    fn apply_ref_mut(&mut self, vm: &mut SloshVm, fn_name: &str, fun: F) -> VMResult<Value>;
//}