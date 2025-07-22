use crate::lisp_adapters::{SlAsMut, SlAsRef, SlFrom, SlFromRef, SlFromRefMut};
use crate::{BridgeResult, BridgeError};
use bridge_types::{ErrorStrings, LooseString, SloshChar};
use compile_state::state::SloshVm;
use slvm::value::ValueType;
use slvm::{Value, ValueTypes};
use std::borrow::Cow;

impl<'a> SlFrom<Cow<'a, str>> for Value {
    fn sl_from(value: Cow<'a, str>, vm: &mut SloshVm) -> BridgeResult<Self> {
        Value::sl_from(value.to_string(), vm)
    }
}

impl<'a> SlFrom<SloshChar<'a>> for Value {
    fn sl_from(value: SloshChar<'a>, vm: &mut SloshVm) -> BridgeResult<Self> {
        Value::sl_from(value.to_string(), vm)
    }
}

impl<'a> SlFromRef<'a, Value> for LooseString<'a> {
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> BridgeResult<Self> {
        match value {
            Value::String(h) => Ok(LooseString::Borrowed(vm.get_string(h))),
            Value::CodePoint(char) => Ok(LooseString::Owned(char.to_string())),
            Value::CharCluster(l, c) => Ok(LooseString::Owned(format!(
                "{}",
                String::from_utf8_lossy(&c[0..l as usize])
            ))),
            Value::CharClusterLong(h) => {
                let ch = vm.get_string(h);
                Ok(LooseString::Borrowed(ch))
            }
            Value::Symbol(i) => Ok(LooseString::Borrowed(vm.get_interned(i))),
            Value::Keyword(i) => Ok(LooseString::Borrowed(vm.get_interned(i))),
            Value::StringConst(i) => Ok(LooseString::Borrowed(vm.get_interned(i))),
            _ => Err(BridgeError::Error(
                ErrorStrings::fix_me_mismatched_type(
                    String::from(ValueTypes::from([
                        ValueType::String,
                        ValueType::StringConst,
                        ValueType::Symbol,
                        ValueType::Keyword,
                        ValueType::CharCluster,
                        ValueType::CharClusterLong,
                        ValueType::CodePoint,
                    ])),
                    value.display_type(vm),
                ),
            )),
        }
    }
}

impl<'a> SlFromRef<'a, Value> for char {
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> BridgeResult<Self> {
        match value {
            Value::CodePoint(char) => Ok(char),
            _ => Err(BridgeError::Error(
                ErrorStrings::fix_me_mismatched_type_with_context(
                    String::from(ValueTypes::from([ValueType::CodePoint])),
                    value.display_type(vm),
                    "Provided value can not be more than one byte, e.g. a char.",
                ),
            )),
        }
    }
}

impl SlFrom<char> for Value {
    fn sl_from(value: char, _vm: &mut SloshVm) -> BridgeResult<Self> {
        Ok(Value::CodePoint(value))
    }
}

/// This delegates to [`SlAsRef`] appropriately.
impl<'a> SlFromRef<'a, Value> for &'a str {
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> BridgeResult<Self> {
        (&value).sl_as_ref(vm)
    }
}

impl<'a> SlAsRef<'a, str> for &Value {
    fn sl_as_ref(&self, vm: &'a SloshVm) -> BridgeResult<&'a str> {
        match self {
            Value::String(h) => Ok(vm.get_string(*h)),
            Value::StringConst(i) => Ok(vm.get_interned(*i)),
            _ => Err(BridgeError::Error(
                ErrorStrings::fix_me_mismatched_type(
                    String::from(ValueTypes::from([
                        ValueType::String,
                        ValueType::StringConst,
                    ])),
                    self.display_type(vm),
                ),
            )),
        }
    }
}

impl<'a> SlFromRef<'a, Value> for SloshChar<'a> {
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> BridgeResult<Self> {
        match value {
            Value::CodePoint(ch) => Ok(SloshChar::Char(ch)),
            Value::CharCluster(l, c) => Ok(SloshChar::String(Cow::Owned(format!(
                "{}",
                String::from_utf8_lossy(&c[0..l as usize])
            )))),
            Value::CharClusterLong(h) => Ok(SloshChar::String(Cow::Borrowed(vm.get_string(h)))),
            _ => Err(BridgeError::Error(
                ErrorStrings::fix_me_mismatched_type(
                    String::from(ValueTypes::from([
                        ValueType::CharCluster,
                        ValueType::CharClusterLong,
                        ValueType::CodePoint,
                    ])),
                    value.display_type(vm),
                ),
            )),
        }
    }
}

/// This delegates to [`SlAsMut`] appropriately.
impl<'a> SlFromRefMut<'a, Value> for &'a mut String {
    fn sl_from_ref_mut(value: Value, vm: &'a mut SloshVm) -> BridgeResult<Self> {
        (&value).sl_as_mut(vm)
    }
}

impl<'a> SlAsMut<'a, String> for &Value {
    fn sl_as_mut(&mut self, vm: &'a mut SloshVm) -> BridgeResult<&'a mut String> {
        match self {
            Value::String(h) => vm.get_string_mut(*h).map_err(|e| BridgeError::Error(e.to_string())),
            _ => Err(BridgeError::Error(
                ErrorStrings::fix_me_mismatched_type(
                    <&'static str>::from(ValueType::String),
                    self.display_type(vm),
                ),
            )),
        }
    }
}

impl SlFrom<String> for Value {
    fn sl_from(value: String, vm: &mut SloshVm) -> BridgeResult<Self> {
        Ok(vm.alloc_string(value))
    }
}

impl<T> SlFrom<&T> for Value
where
    T: ToString + ?Sized,
{
    fn sl_from(value: &T, vm: &mut SloshVm) -> BridgeResult<Self> {
        Ok(vm.alloc_string(value.to_string()))
    }
}

impl<T> SlFrom<&mut T> for Value
where
    T: ToString + ?Sized,
{
    fn sl_from(value: &mut T, vm: &mut SloshVm) -> BridgeResult<Self> {
        Ok(vm.alloc_string(value.to_string()))
    }
}

impl<'a> SlFromRef<'a, Value> for String {
    fn sl_from_ref(value: Value, vm: &'a SloshVm) -> BridgeResult<Self> {
        match value {
            Value::String(h) => Ok(vm.get_string(h).to_string()),
            Value::StringConst(i) => Ok(vm.get_interned(i).to_string()),
            _ => Err(BridgeError::Error(
                ErrorStrings::fix_me_mismatched_type(
                    <&'static str>::from(ValueType::String),
                    value.display_type(vm),
                ),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lisp_adapters::{SlAsMut, SlAsRef, SlInto, SlIntoRef};
    use compile_state::state::new_slosh_vm;

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
        let val = vm.alloc_string(
            CHAR_CLUSTER.to_string() + CHAR_CLUSTER_LONG + CODE_POINT.to_string().as_str(),
        );
        assert!(matches!(val, Value::String(_)));
        val
    }

    pub fn create_symbol(vm: &mut SloshVm) -> Value {
        let val = Value::Symbol(vm.intern(&format!("create-symbol-test")));
        assert!(matches!(val, Value::Symbol(_)));
        val
    }

    pub fn create_keyword(vm: &mut SloshVm) -> Value {
        let val = Value::Symbol(vm.intern(&format!("create-keywork-test")));
        match val {
            Value::Symbol(i) => {
                let val = Value::Keyword(i);
                assert!(matches!(val, Value::Keyword(_)));
                val
            }
            _ => {
                unreachable!("should always return a symbol.")
            }
        }
    }

    pub fn create_string_const(vm: &mut SloshVm) -> Value {
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
    fn try_conversion_error() {
        let mut vm = new_slosh_vm();
        let value = create_string(&mut vm);
        let c: BridgeResult<char> = value.sl_into_ref(&vm);
        assert!(c.is_err());
        let expected_err = BridgeError::Error(ErrorStrings::fix_me_mismatched_type_with_context(
            String::from(ValueTypes::from([ValueType::CodePoint])),
            value.display_type(&mut vm),
            "Provided value can not be more than one byte, e.g. a char.",
        ));
        assert_eq!(c.err().unwrap().to_string(), expected_err.to_string());
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

    fn str_test_mut(vm: &mut SloshVm, args: &[Value]) -> BridgeResult<()> {
        let fn_name = "str_trim";
        const PARAMS_LEN: usize = 1usize;
        let arg_types: [bridge_types::Param; PARAMS_LEN] = [bridge_types::Param {
            handle: bridge_types::TypeHandle::Direct,
            passing_style: bridge_types::PassingStyle::MutReference,
        }];

        let param = arg_types[0usize];
        match param.handle {
            bridge_types::TypeHandle::Direct => match args.get(0usize) {
                None => {
                    Err(BridgeError::Error(
                        format!("{} not given enough arguments, expected at least {} arguments, got {}.", fn_name, 1usize, args.len())
                    ))
                }
                Some(mut arg_0) => match args.get(PARAMS_LEN) {
                    Some(_)
                        if PARAMS_LEN == 0
                            || arg_types[PARAMS_LEN - 1].handle
                                != bridge_types::TypeHandle::VarArgs =>
                    {
                        let res =
                            format!("{} given too many arguments, expected at least {} arguments, got {}.",
                                    fn_name, 1usize, args.len());
                        Err(BridgeError::Error(res))
                    }
                    _ => {
                        let arg: &mut String = arg_0.sl_as_mut(vm)?;
                        arg.push_str("0");
                        Ok(())
                    }
                },
            },
            _ => {
                Err(BridgeError::Error(format!("{} failed to parse its arguments, internal error.", fn_name)))
            }
        }
    }

    fn str_trim_test(vm: &mut SloshVm, test_str: String) -> BridgeResult<Value> {
        let test_str = vm.alloc_string(test_str);
        let args = [test_str];
        let fn_name = "str_trim";
        const PARAMS_LEN: usize = 1usize;
        let arg_types: [bridge_types::Param; PARAMS_LEN] = [bridge_types::Param {
            handle: bridge_types::TypeHandle::Direct,
            passing_style: bridge_types::PassingStyle::Value,
        }];

        let param = arg_types[0usize];
        match param.handle {
            bridge_types::TypeHandle::Direct => match args.get(0usize) {
                None => {
                    return Err(BridgeError::Error(
                        format!("{} not given enough arguments, expected at least {} arguments, got {}.", fn_name, 1usize, args.len())
                    ));
                }
                Some(arg_0) => match args.get(PARAMS_LEN) {
                    Some(_)
                        if PARAMS_LEN == 0
                            || arg_types[PARAMS_LEN - 1].handle
                                != bridge_types::TypeHandle::VarArgs =>
                    {
                        Err(BridgeError::Error(
                                            format!("{} given too many arguments, expected at least {} arguments, got {}.",
                                                    fn_name, 1usize, args.len())
                        ))
                    }
                    _ => {
                        {
                            let arg: String = (*arg_0).sl_into_ref(vm)?;
                            arg.trim().to_string().sl_into(vm)
                        };
                    }
                },
            },
            _ => {
                Err(BridgeError::Error(
                    format!("{} failed to parse its arguments, internal error.", fn_name)
                ))
            }
        }
    }

    #[test]
    fn test_string_conversions_value_to_rust() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;
        let test_string = &mut "hello world".to_string();
        let val: Value = test_string
            .sl_into(vm)
            .expect("&mut String can be converted to Value");
        assert!(matches!(val, Value::String(_)));

        let _s: String = val
            .sl_into_ref(vm)
            .expect("&Value::String can be converted to String");
        let kwd_val = create_keyword(vm);

        let e: BridgeResult<String> = kwd_val.sl_into_ref(vm);
        e.expect_err("Can not convert keyword to String");

        let _s: &str = (&val)
            .sl_as_ref(vm)
            .expect("&Value::String can be converted to &str");

        let e: BridgeResult<&str> = (&kwd_val).sl_as_ref(vm);
        e.expect_err("Can not convert keyword to &str");

        let _s: &mut String = (&val)
            .sl_as_mut(vm)
            .expect("&Value::String can be converted to &mut String");

        let e: BridgeResult<&mut String> = (&kwd_val).sl_as_mut(vm);
        e.expect_err("Can not convert keyword to &mut String");
    }

    #[test]
    fn test_string_conversions_rust_to_value() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;

        let test_string = "hello world";
        let val: Value = test_string
            .sl_into(vm)
            .expect("&str can be converted to Value");
        assert!(matches!(val, Value::String(_)));

        let test_string = "hello world".to_string();
        let val: Value = test_string
            .sl_into(vm)
            .expect("String can be converted to Value");
        assert!(matches!(val, Value::String(_)));

        let test_string = "hello world".to_string();
        let val: Value = (&test_string)
            .sl_into(vm)
            .expect("&String can be converted to Value");
        assert!(matches!(val, Value::String(_)));

        let mut test_string = "hello world".to_string();
        let val: Value = (&mut test_string)
            .sl_into(vm)
            .expect("&String can be converted to Value");
        assert!(matches!(val, Value::String(_)));
    }

    #[test]
    fn test_char_conversions_value_to_rust() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;

        let val = create_code_point();
        let _c: char = val
            .sl_into_ref(vm)
            .expect("&Value::CodePoint can be converted to char");
    }

    #[test]
    fn test_char_conversions_rust_to_value() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;

        let val: Value = CODE_POINT
            .sl_into(vm)
            .expect("char can be converted to Value");
        assert!(matches!(val, Value::CodePoint(_)));
    }

    #[test]
    fn test_char_cluster_conversions_value_to_rust() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;

        let val = create_char_cluster(vm);
        let _c: SloshChar = val
            .sl_into_ref(vm)
            .expect("&Value::CharCluster can be converted to SloshChar");

        let val = create_char_cluster_long(vm);
        let _c: SloshChar = val
            .sl_into_ref(vm)
            .expect("&Value::CharClusterLong can be converted to SloshChar");
    }

    #[test]
    fn test_char_cluster_conversions_rust_to_value() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;

        let rust_char_cluster = SloshChar::String(Cow::Owned(CHAR_CLUSTER.to_string()));
        let val: Value =
            Value::sl_from(rust_char_cluster, vm).expect("&SloshChar can be converted to &Value");
        assert!(matches!(val, Value::String(_)));

        let rust_char_cluster = SloshChar::String(Cow::Borrowed(CHAR_CLUSTER_LONG));
        let val: Value =
            Value::sl_from(rust_char_cluster, vm).expect("&SloshChar can be converted to &Value");
        assert!(matches!(val, Value::String(_)));
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
            let _loose_string: LooseString = val
                .sl_into_ref(vm)
                .expect("This value should be convertible to a LooseString");
        }
    }

    #[test]
    fn test_loose_string_conversion_rust_to_value() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;

        let sample = LooseString::Owned("hello world".to_string());
        let val: Value =
            Value::sl_from(sample, vm).expect("This LooseString should be convertible to a Value");
        assert!(matches!(val, Value::String(_)));
    }
}
