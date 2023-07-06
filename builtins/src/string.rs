use crate::{add_builtin, SloshVm};
use slvm::{VMError, VMResult, Value};

fn str_trim(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), None) = (i.next(), i.next()) {
        let string = string.get_string(vm)?.trim().to_string();
        Ok(vm.alloc_string(string))
    } else {
        Err(VMError::new_vm("str-trim: takes one argument".to_string()))
    }
}

fn str_ltrim(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), None) = (i.next(), i.next()) {
        let string = string.get_string(vm)?.trim_start().to_string();
        Ok(vm.alloc_string(string))
    } else {
        Err(VMError::new_vm("str-ltrim: takes one argument".to_string()))
    }
}

fn str_rtrim(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), None) = (i.next(), i.next()) {
        let string = string.get_string(vm)?.trim_end().to_string();
        Ok(vm.alloc_string(string))
    } else {
        Err(VMError::new_vm("str-rtrim: takes one argument".to_string()))
    }
}

fn str_replace(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), Some(from), Some(to), None) = (i.next(), i.next(), i.next(), i.next()) {
        let from = from.get_string(vm)?;
        let to = to.get_string(vm)?;
        let new_string = string.get_string(vm)?.replace(from, to);
        Ok(vm.alloc_string(new_string))
    } else {
        Err(VMError::new_vm(
            "str-replace: takes three arguments".to_string(),
        ))
    }
}

fn str_contains(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), Some(pat), None) = (i.next(), i.next(), i.next()) {
        let string = string.get_string(vm)?;
        let pat = pat.get_string(vm)?;
        if string.contains(pat) {
            Ok(Value::True)
        } else {
            Ok(Value::False)
        }
    } else {
        Err(VMError::new_vm(
            "str-contains: Invalid arguments".to_string(),
        ))
    }
}

fn str_push(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let Some(buffer) = i.next() {
        if let Value::String(handle) = *buffer {
            // Need to break the lifetime away from vm else we can not do anything else the with vm...
            // We need to make sure GC does not happen (avoid any chance of removing the underlying
            // object) even though this handle should be safe since we are here.
            // Also, can NOT call get_string or get_string_mut on this handle while holding this
            // reference without UB (need to make sure this is the only reference to this string in existence)..
            let buffer = unsafe { &mut *(vm.get_string_mut(handle) as *mut String) };
            for next in i {
                match next {
                    Value::String(h) => {
                        if *h == handle {
                            // Not only does this not make sense it will be UB to have two references to this string.
                            return Err(VMError::new_value(
                                "str-push!: can not push a string onto itself!".to_string(),
                            ));
                        } else {
                            buffer.push_str(vm.get_string(*h));
                        }
                    }
                    _ => buffer.push_str(&next.pretty_value(vm)),
                }
            }
            Ok(Value::String(handle))
        } else {
            Err(VMError::new_vm(
                "str-push!: first arg must be a string (not a const)".to_string(),
            ))
        }
    } else {
        Err(VMError::new_vm(
            "str-push!: takes a string with 0 more arguments to append".to_string(),
        ))
    }
}

fn str_clear(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(buffer), None) = (i.next(), i.next()) {
        if let Value::String(handle) = *buffer {
            let buffer = vm.get_string_mut(handle);
            buffer.clear();
            Ok(Value::String(handle))
        } else {
            Err(VMError::new_vm(
                "str-clear!: takes one arg, must be a string (not a const)".to_string(),
            ))
        }
    } else {
        Err(VMError::new_vm(
            "str-push!: takes a single arg, a string to clear".to_string(),
        ))
    }
}

pub fn add_str_builtins(env: &mut SloshVm) {
    add_builtin(
        env,
        "str-replace",
        str_replace,
        r#"Usage: (str-replace string old-pattern new-pattern) -> string

Replace occurances of second string with third in the first string.

Section: string

Example:
(test::assert-equal "some yyy string" (str-replace "some xxx string" "xxx" "yyy"))
(test::assert-equal "some yyy string yyy" (str-replace "some xxx string xxx" "xxx" "yyy"))
(test::assert-equal "yyy some yyy string yyy" (str-replace "xxx some xxx string xxx" "xxx" "yyy"))
"#,
    );
    add_builtin(
        env,
        "str-trim",
        str_trim,
        r#"Usage: (str-trim string) -> string

Trim right and left whitespace from string.

Section: string

Example:
(test::assert-equal "some string" (str-trim "   some string"))
(test::assert-equal "some string" (str-trim "   some string   "))
(test::assert-equal "some string" (str-trim (str "   some string   ")))
(test::assert-equal "some string" (str-trim "some string   "))
(test::assert-equal "some string" (str-trim "some string"))
"#,
    );
    add_builtin(
        env,
        "str-rtrim",
        str_rtrim,
        r#"Usage: (str-rtrim string) -> string

Trim right whitespace from string.

Section: string

Example:
(test::assert-equal "   some string" (str-rtrim "   some string"))
(test::assert-equal "   some string" (str-rtrim "   some string   "))
(test::assert-equal "   some string" (str-rtrim (str "   some string   ")))
(test::assert-equal "some string" (str-rtrim "some string   "))
(test::assert-equal "some string" (str-rtrim "some string"))
"#,
    );
    add_builtin(
        env,
        "str-ltrim",
        str_ltrim,
        r#"Usage: (str-ltrim string) -> string

Trim left whitespace from string.

Section: string

Example:
(test::assert-equal "some string" (str-ltrim "   some string"))
(test::assert-equal "some string   " (str-ltrim "   some string   "))
(test::assert-equal "some string   " (str-ltrim (str "   some string   ")))
(test::assert-equal "some string   " (str-ltrim "some string   "))
(test::assert-equal "some string" (str-ltrim "some string"))
"#,
    );
    add_builtin(
        env,
        "str-contains",
        str_contains,
        r#"Usage: (str-contains string pattern) -> #t/#f

True if string contains pattern (pattern will be converted to a string first).

Section: string

Example:
(test::assert-true (str-contains "Stausomething" "Stau"))
(test::assert-false (str-contains "Stausomething" "StaU"))
(test::assert-true (str-contains "Stausomething" "some"))
(test::assert-false (str-contains "Stausomething" "Some"))
(test::assert-true (str-contains "Stausomething" "thing"))
(test::assert-false (str-contains "Stausomething" "Thing"))
(test::assert-true (str-contains "StausomeΣthing" "someΣ"))
"#,
    );
    add_builtin(
        env,
        "str-push!",
        str_push,
        r#"Usage: (str-push! string arg0 ... argN) -> string

Push the args (as strings) onto the string.  This is a destructive form.

Arguments will be turned into strings.  Returns the string it was given.

Section: string

Example:
(test::assert-equal "stringsome" (str-push! (str "string") "some"))
(def test-str-push (str "def-string"))
(test::assert-equal "def-stringsome" (str-push! test-str-push "some"))
(test::assert-equal "def-stringsome" test-str-push)
"#,
    );
    add_builtin(
        env,
        "str-clear!",
        str_clear,
        r#"Usage: (str-clear! string) -> string

Clears a string.  This is a destructive form.

Returns the string it was given.

Section: string

Example:
(test::assert-equal "" (str-clear! (str "string")))
(def test-str-clear (str "def-string"))
(test::assert-equal "" (str-clear! test-str-clear))
(test::assert-equal "" test-str-clear)
"#,
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use compile_state::state::new_slosh_vm;

    fn check_str(vm: &SloshVm, str_val: Value, against: &str) {
        if let Value::String(handle) = str_val {
            assert_eq!(vm.get_string(handle), against);
        } else {
            panic!("Not a string!");
        }
    }

    #[test]
    fn test_str_push() -> VMResult<()> {
        let mut vm = new_slosh_vm();
        let dest = vm.alloc_string("XXX".to_string());
        let add = vm.alloc_string(" 123".to_string());
        let res = str_push(&mut vm, &vec![dest, add])?;
        assert_eq!(dest, res);
        check_str(&vm, dest, "XXX 123");

        Ok(())
    }
}
