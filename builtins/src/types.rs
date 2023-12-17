//! TODO PC need explanation for the emulation for TryFrom/TryInto/AsRef/AsMut
use compile_state::state::SloshVm;
use slvm::{Value, VMError, VMResult};

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

// TODO PC work out how the LooseString stuff work as a Cow type
// is what
// #[macro_export]
//macro_rules! try_inner_string {
//    ($fn_name:ident, $expression:expr, $name:ident, $eval:expr) => {{
//        use $crate::ErrorStrings;
//        match &$expression.get().data {
//            Value::String($name, _) => $eval,
//            Value::Symbol($name, _) => $eval,
//            Value::Char($name) => $eval,
//            _ => {
//                return Err($crate::LispError::new(ErrorStrings::mismatched_type(
//                    $fn_name,
//                    &format!(
//                        "{}, {}, or {}, ",
//                        Value::String(Default::default(), Default::default()).to_string(),
//                        Value::Symbol(Default::default(), Default::default()).to_string(),
//                        Value::Char(Default::default()).to_string()
//                    ),
//                    &$expression.to_string(),
//                )))
//            }
//        }
//    }};
//}
//
//impl<'a> SlFrom<&Value> for LooseString<'a, str> {
//    fn sl_from(value: &Value, vm: &'a mut SloshVm) -> VMResult<LooseString<'a, str>> {
//        // TODO PC which other of these types do we consider to be "cast"-able to a
//        // string in the context of Rust functions that implement "this" macro.
//        match value {
//            Value::String(h) => {
//                Ok(LooseString::Borrowed(vm.get_string(*h)))
//            }
//            Value::CodePoint(char) => {
//                Ok(LooseString::Owned(char.to_string()))
//            }
//            Value::CharCluster(l, c) => {
//                let s = format!("{}", String::from_utf8_lossy(&c[0..*l as usize]));
//                Ok(LooseString::Owned(s))
//            }
//            Value::CharClusterLong(h) => {
//                let ch = vm.get_string(*h);
//                Ok(LooseString::Borrowed(ch))
//            }
//            Value::Symbol(i) => {
//                Ok(LooseString::Borrowed(vm.get_interned(*i)))
//            },
//            Value::Keyword(i) => {
//                let s = format!(":{}", vm.get_interned(*i));
//                Ok(LooseString::Owned(s))
//            },
//            Value::StringConst(i) => {
//                let s = format!("\"{}\"", vm.get_interned(*i));
//                Ok(LooseString::Owned(s))
//            },
//            _ => {
//                Err(VMError::new("conv", "Wrong type, expected something that can be cast to a string."))
//            }
//        }
//    }
//}

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
                Err(VMError::new("conv", "Wrong type, expected something that can be cast to a &str."))
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
                Err(VMError::new("conv", "Wrong type, expected something that can be cast to a char."))
            }
        }
    }
}

impl SlFrom<char> for Value {
    fn sl_from(value: char, vm: &mut SloshVm) -> VMResult<Self> {
        // TODO PC resolve w/ sstanfield how to do this more efficiently
        Ok(vm.alloc_char(value.to_string().as_str()))
    }
}

impl SlFrom<String> for Value {
    fn sl_from(value: String, vm: &mut SloshVm) -> VMResult<Self> {
        Ok(vm.alloc_string(value))
    }
}

impl<'a> SlAsMut<'a, String> for Value {
    fn sl_as_mut(&mut self, vm: &'a mut SloshVm) -> VMResult<&'a mut String> {
        match self {
            Value::String(h) => {
                Ok(vm.get_string_mut(*h))
            }
            _ => {
                Err(VMError::new("conv", "Wrong type, expected something that can be cast to a &mut String."))
            }
        }
    }
}

// TODO PC preference would be for String to just be Value::String & Value::StringConst
// and let LooseString handle the rest, also avoids needless allocations the user of
// the macro may not care for.
impl SlFrom<&Value> for String {
    fn sl_from(value: &Value, vm: &mut SloshVm) -> VMResult<Self> {
        match value {
            Value::String(h) => {
                Ok(vm.get_string(*h).to_string())
            }
            Value::CodePoint(char) => {
                let s = char;
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
                        return Err(crate::VMError::new("conv", &*{
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
                                    return Err(crate::VMError::new("conv", &*{
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