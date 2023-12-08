use std::marker::PhantomData;
use compile_state::state::SloshVm;
use slvm::{Value, VMError, VMResult};

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

/// Simple wrapper so the macro can infer the type of the Value at runtime to see if the value
/// provided to the lisp environment was the type of value the rust function expected.
pub struct TypedWrapper<'a, T: ?Sized + 'a, U>(&'a U, PhantomData<T>);


/// Type wrapper to use in [`RustProcedure`] and [`RustProcedureRefMut`] declarations for
/// partial application.
pub trait VmToRustType<'a, T, F>
    where
        Self: Sized,
        T: 'a,
        F: FnOnce(&'a mut SloshVm) -> T,
{
    fn apply(&self, fun: F) -> VMResult<Value>;
}

impl<'a, T: ?Sized + 'a, U> TypedWrapper<'a, T, U> {
    pub fn new(src: &'a U) -> TypedWrapper<T, U> {
        TypedWrapper(src, PhantomData::default())
    }
}

pub trait TryFromSlosh<T> {
    fn try_from_slosh(&self, vm: &mut SloshVm, val: &Value) -> VMResult<T>;
}

pub trait TryIntoSlosh {
    fn try_into_slosh(self, vm: &mut SloshVm) -> VMResult<Value>;
}

impl TryIntoSlosh for String {
    fn try_into_slosh(self, vm: &mut SloshVm) -> VMResult<Value> {
        Ok(vm.alloc_string(self))
    }
}

impl TryFromSlosh<String> for TypedWrapper<'_, String, Value> {
    fn try_from_slosh(&self, vm: &mut SloshVm, val: &Value) -> VMResult<String> {
        vm_to_string(vm, val)
    }
}

fn vm_to_string(vm: &mut SloshVm, val: &Value) -> VMResult<String> {
    match val {
        Value::String(h) => {
            Ok(vm.get_string(*h).to_string())
        }
        Value::CodePoint(char) => {
            let s = *char;
            Ok(s.encode_utf8(&mut [0; 4]).to_string())
        }
        Value::CharCluster(l, c) => {
            Ok(format!("{}", String::from_utf8_lossy(&c[0..*l as usize])))
        }
        Value::CharClusterLong(h) => {
            Ok(vm.get_string(*h).to_string())
        }
        Value::Symbol(i) => {
            Ok(vm.get_interned(*i).to_string())
        },
        Value::Keyword(i) => {
            Ok(vm.get_interned(*i).to_string())
        },
        Value::StringConst(i) => {
            Ok(vm.get_interned(*i).to_string())
        },
        _ => {
            Err(VMError::new("conv", "Wrong type, expected something that can be cast to a string."))
        }
    }
}

#[cfg(test)]
mod test {
    use compile_state::state::new_slosh_vm;
    use super::*;


    fn str_trim(vm: &mut SloshVm, registers: &[Value]) -> VMResult<String> {
        let mut i = registers.iter();
        let right = vm.intern("right");
        let left = vm.intern("left");
        match (i.next(), i.next(), i.next()) {
            (Some(string), None, None) => {
                let string = string.get_string(vm)?.trim().to_string();
                Ok(string)
            }
            (Some(string), Some(Value::Keyword(i)), None) if *i == right => {
                let string = string.get_string(vm)?.trim_end().to_string();
                Ok(string)
            }
            (Some(string), Some(Value::Keyword(i)), None) if *i == left => {
                let string = string.get_string(vm)?.trim_start().to_string();
                Ok(string)
            }
            _ => Err(VMError::new_vm(
                "str-trim: takes one argument with optional left/right keyword".to_string(),
            )),
        }
    }

    #[test]
    fn try_me() {
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
                    // TODO PC
                    // now that we do not have to run this callback inside the branch, since Value is copy... can we do something
                    // different entirely so we don't have to surrender the vm into the callback function.
                    Some(arg_0) => {
                        {
                            use crate::types::TryIntoSlosh;
                            use crate::types::TryFromSlosh;
                            let typed_data:
                                crate::types::TypedWrapper<String,
                                    crate::Value> =
                                crate::types::TypedWrapper::new(&arg_0);
                            let arg_0: String = typed_data.try_from_slosh(vm, arg_0)?;
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
                                        let res: VMResult<Value> =
                                            {
                                                {
                                                    arg.trim().to_string().try_into_slosh(vm)
                                                }
                                            };
                                        res
                                    };
                                }
                            }
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
