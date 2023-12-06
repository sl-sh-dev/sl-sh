use std::marker::PhantomData;
use compile_state::state::SloshVm;
use slvm::{Handle, Value, VMError, VMResult};

// [`RustProcedure`] and [`RustProcedureRefMut`] are traits that are used to implement type conversions
// from [`Value`] to Rust types that take a callback function so the various arguments to rust
// native functions can be curried by recursively applied callbacks doing so -in place- to avoid
// needing to copy the data.
//
//  TODO PC ( this is why the fun() is applied at the core of each statement that matches self in a rust procedure )
//     however, in the new world... if every single tuple item in the [`Value`] enum is copy then
//     maybe it doesn't matter anymore and this strategy can be abandoned.
//
// Is TryIntoExpression still needed?
// It looks like nothing can be converted From Rust Type to Value without vm. So, something new will
// need to be figured out here.

/// Simple wrapper so the macro can infer the type of the Value at runtime to see if the value
/// provided to the lisp environment was the type of value the rust function expected.
pub struct TypedWrapper<'a, T: ?Sized, U>(&'a U, PhantomData<T>);

impl<'a, T: ?Sized, U> TypedWrapper<'a, T, U> {
    pub fn new(src: &'a U) -> TypedWrapper<T, U> {
        TypedWrapper(src, PhantomData::default())
    }
}

/// Trait used to curry the arguments of some T to any [`Value`] by applying T to F.
pub trait RustProcedure<T, F>
    where
        Self: Sized,
        F: FnOnce(T) -> VMResult<Value> + ?Sized,
{
    fn apply(&self, vm: &mut SloshVm, fn_name: &str, fun: F) -> VMResult<Value>;
}

/// Trait used to curry the mutable reference of some T to any [`Value`] by applying T to F.
pub trait RustProcedureRefMut<T, F>
    where
        Self: Sized,
        F: FnOnce(&mut T) -> VMResult<Value> + ?Sized,
{
    fn apply_ref_mut(&mut self, vm: &mut SloshVm, fn_name: &str, fun: F) -> VMResult<Value>;
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

impl<F> RustProcedure<&str, F> for TypedWrapper<'_, &str, Value>
    where
        F: FnOnce(&str) -> VMResult<Value>,
{
    fn apply(&self, mut vm: &mut SloshVm, fn_name: &str, fun: F) -> VMResult<Value> {
        // TODO PC which other of these types do we consider to be "cast"-able to a
        // string in the context of Rust functions that implement "this" macro.
        match self.0 {
            Value::String(h) => {
                let h = vm.get_string_mut(*h);
                fun(h)
            }
            Value::CodePoint(char) |
            Value::CodePoint(char) => {
                let s = *char;
                let mut s = s.to_string();
                fun(&mut s)
            }
            Value::CharCluster(l, c) => {
                let mut s = format!("{}", String::from_utf8_lossy(&c[0..*l as usize]));
                fun(&mut s)
            }
            Value::CharClusterLong(h) => {
                let ch = vm.get_string_mut(*h);
                fun(ch)
            }
            Value::Symbol(i) => {
                let mut s = vm.get_interned(*i).to_string();
                fun(&mut s)
            },
            Value::Keyword(i) => {
                let mut s = format!(":{}", vm.get_interned(*i));
                fun(&mut s)
            },
            Value::StringConst(i) => {
                let mut s = format!("\"{}\"", vm.get_interned(*i));
                fun(&mut s)
            },
            _ => {
                Err(VMError::new("conv", "Wrong type, expected something that can be cast to a string."))
            }
        }
    }
}

impl<F> RustProcedure<String, F> for TypedWrapper<'_, String, Value>
    where
        F: FnOnce(String) -> VMResult<Value>,
{
    fn apply(&self, mut vm: &mut SloshVm, fn_name: &str, fun: F) -> VMResult<Value> {
        // TODO PC which other of these types do we consider to be "cast"-able to a
        // string in the context of Rust functions that implement "this" macro.
        match self.0 {
            Value::String(h) => {
                let h = vm.get_string(*h);
                fun(h.to_string())
            }
            Value::CodePoint(char) |
            Value::CodePoint(char) => {
                let s = *char;
                let s = s.to_string();
                fun(s)
            }
            Value::CharCluster(l, c) => {
                let s = format!("{}", String::from_utf8_lossy(&c[0..*l as usize]));
                fun(s)
            }
            Value::CharClusterLong(h) => {
                let ch = vm.get_string(*h);
                fun(ch.to_string())
            }
            Value::Symbol(i) => {
                let s = vm.get_interned(*i).to_string();
                fun(s)
            },
            Value::Keyword(i) => {
                let s = format!(":{}", vm.get_interned(*i));
                fun(s)
            },
            Value::StringConst(i) => {
                let s = format!("\"{}\"", vm.get_interned(*i));
                fun(s)
            },
            _ => {
                Err(VMError::new("conv", "Wrong type, expected something that can be cast to a string."))
            }
        }
    }
}

impl<F> RustProcedureRefMut<String, F> for TypedWrapper<'_, String, Value>
    where
        F: FnOnce(&mut String) -> VMResult<Value>,
{
    fn apply_ref_mut(&mut self, vm: &mut SloshVm, fn_name: &str, fun: F) -> VMResult<Value> {
        // TODO PC which other of these types do we consider to be "cast"-able to a
        // string in the context of Rust functions that implement "this" macro.
        match self.0 {
            Value::String(h) => {
                let h = vm.get_string_mut(*h);
                fun(h)
            }
            Value::CodePoint(char) |
            Value::CodePoint(char) => {
                let s = *char;
                let mut s = s.to_string();
                fun(&mut s)
            }
            Value::CharCluster(l, c) => {
                let mut s = format!("{}", String::from_utf8_lossy(&c[0..*l as usize]));
                fun(&mut s)
            }
            Value::CharClusterLong(h) => {
                let ch = vm.get_string_mut(*h);
                fun(ch)
            }
            Value::Symbol(i) => {
                let mut s = vm.get_interned(*i).to_string();
                fun(&mut s)
            },
            Value::Keyword(i) => {
                let mut s = format!(":{}", vm.get_interned(*i));
                fun(&mut s)
            },
            Value::StringConst(i) => {
                let mut s = format!("\"{}\"", vm.get_interned(*i));
                fun(&mut s)
            },
            _ => {
                Err(VMError::new("conv", format!("{fn_name}: Wrong type, expected something that can be cast to a string.")))
            }
        }
    }
}


impl<F> RustProcedure<&Value, F> for TypedWrapper<'_, Value, Value>
    where
        F: FnOnce(&Value) -> VMResult<Value>,
{
    fn apply(&self, _vm: &mut SloshVm, _fn_name: &str, fun: F) -> VMResult<Value> {
        fun(&mut self.0.clone())
    }
}

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
    fn try_me() {
        str_trim_test().unwrap();
    }

    fn str_trim_test() -> VMResult<Value> {
        let mut vm = new_slosh_vm();
        let args = [];
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
                    // TODO PC
                    // now that we do not have to run this callback inside the branch, since Value is copy... can we do something
                    // different entirely so we don't have to surrender the vm into the callback function.
                    Some(arg_0) => {
                        {
                            use crate::types::RustProcedure;
                            let typed_data:
                                crate::types::TypedWrapper<String,
                                    crate::Value> =
                                crate::types::TypedWrapper::new(&arg_0);
                            let callback =
                                |arg_0: String|
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
                                                    let res: VMResult<String> =
                                                        {
                                                            {
                                                                Ok(arg.trim().to_string())
                                                            }
                                                        };
                                                    res
                                                }.map(Into::into);
                                            }
                                        }
                                    };
                            typed_data.apply(&mut vm, fn_name, callback)
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
