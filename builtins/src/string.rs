use crate::{add_builtin, SloshVm};
use slvm::{Handle, VMError, VMResult, Value};
use unicode_segmentation::UnicodeSegmentation;

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

fn str_trim_bang(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    let right = vm.intern("right");
    let left = vm.intern("left");
    match (i.next(), i.next(), i.next()) {
        (Some(Value::String(handle)), None, None) => {
            let buffer = vm.get_string_mut(*handle);
            let trimmed = buffer.trim_end();
            buffer.truncate(trimmed.len());
            let trimmed = buffer.trim_start();
            buffer.replace_range(..(buffer.len() - trimmed.len()), "");
            Ok(Value::String(*handle))
        }
        (Some(Value::String(handle)), Some(Value::Keyword(i)), None) if *i == right => {
            let buffer = vm.get_string_mut(*handle);
            let trimmed = buffer.trim_end();
            buffer.truncate(trimmed.len());
            Ok(Value::String(*handle))
        }
        (Some(Value::String(handle)), Some(Value::Keyword(i)), None) if *i == left => {
            let buffer = vm.get_string_mut(*handle);
            let trimmed = buffer.trim_start();
            buffer.replace_range(..(buffer.len() - trimmed.len()), "");
            Ok(Value::String(*handle))
        }
        _ => Err(VMError::new_vm(
            "str-trim!: takes a non-const string with optional left/right keyword".to_string(),
        )),
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

fn str_map_inner(vm: &mut SloshVm, func: Value, string: Value) -> VMResult<String> {
    let tmp_str;
    let string = match string {
        Value::StringConst(i) => vm.get_interned(i),
        Value::CodePoint(ch) => {
            tmp_str = format!("{ch}");
            &tmp_str
        }
        Value::CharCluster(l, c) => {
            tmp_str = format!("{}", String::from_utf8_lossy(&c[0..l as usize]));
            &tmp_str
        }
        //Value::CharClusterLong(_) => "Char".to_string(), // XXX TODO- move this to Object?
        Value::String(handle) => {
            tmp_str = vm.get_string(handle).to_string();
            &tmp_str
        }
        _ => {
            return Err(VMError::new_vm(
                "str-map: first arg must be a string".to_string(),
            ))
        }
    };
    let mut res = String::new();
    for ch in UnicodeSegmentation::graphemes(string, true) {
        let param = if ch.len() < 7 {
            let mut buf = [0_u8; 6];
            let ch_bytes = ch.as_bytes();
            buf[0..ch_bytes.len()].copy_from_slice(ch_bytes);
            Value::CharCluster(ch.len() as u8, buf)
        } else {
            vm.alloc_string(ch.to_string())
        };
        let val = match func {
            Value::Lambda(handle) => {
                let func = vm.get_lambda(handle);
                vm.do_call(func, &[param], None)?
            }
            Value::Closure(handle) => {
                let (func, caps) = vm.get_closure(handle);
                let caps: Vec<Handle> = caps.to_vec();
                vm.do_call(func, &[param], Some(&caps[..]))?
            }
            Value::Builtin(idx) => vm.get_builtin(idx)(vm, &[param])?,
            _ => {
                return Err(VMError::new_vm(
                    "str-map: second arg must be callable".to_string(),
                ))
            }
        };
        match val {
            Value::StringConst(i) => res.push_str(vm.get_interned(i)),
            Value::String(h) => res.push_str(vm.get_string(h)),
            Value::CodePoint(ch) => res.push(ch),
            Value::CharCluster(l, c) => res.push_str(&String::from_utf8_lossy(&c[0..l as usize])),
            //Value::CharClusterLong(_) => "Char".to_string(), // XXX TODO- move this to Object?
            _ => {
                return Err(VMError::new_vm(
                    "str-map: callable must return a string or char".to_string(),
                ))
            }
        }
    }
    Ok(res)
}

fn str_map(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), Some(func), None) = (i.next(), i.next(), i.next()) {
        let res = str_map_inner(vm, *func, *string)?;
        Ok(vm.alloc_string(res))
    } else {
        Err(VMError::new_vm(
            "str-map: takes a string and a lambda".to_string(),
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
        r#"Usage: (str-trim string [:right | :left]) -> string

Trim right and/or left whitespace from string.  With no optional keywork trims both, otherwise :right
or :left specify right or left trimming.

Section: string

Example:
(test::assert-equal "some string" (str-trim "   some string"))
(test::assert-equal "some string" (str-trim "   some string   "))
(test::assert-equal "some string" (str-trim (str "   some string   ")))
(test::assert-equal "some string" (str-trim "some string   "))
(test::assert-equal "some string" (str-trim "some string"))

(test::assert-equal "   some string" (str-trim "   some string" :right))
(test::assert-equal "   some string" (str-trim "   some string   " :right))
(test::assert-equal "   some string" (str-trim (str "   some string   " :right)))
(test::assert-equal "some string" (str-trim "some string   " :right))
(test::assert-equal "some string" (str-trim "some string" :right))

(test::assert-equal "some string" (str-trim "   some string" :left))
(test::assert-equal "some string   " (str-trim "   some string   " :left))
(test::assert-equal "some string   " (str-trim (str "   some string   " :left)))
(test::assert-equal "some string   " (str-trim "some string   " :left))
(test::assert-equal "some string" (str-trim "some string" :left))
"#,
    );
    add_builtin(
        env,
        "str-trim!",
        str_trim_bang,
        r#"Usage: (str-trim! string [:right | :left]) -> string

Trim right and/or left whitespace from string iiin place.  With no optional keywork trims both,
otherwise :right or :left specify right or left trimming.

This is a destructive operation (unlike str-trim) and requires a actual non-const string as it's first
argument.  It returns this string on success.

Section: string

Example:
(test::assert-equal "some string" (str-trim! (str "   some string")))
(test::assert-equal "some string" (str-trim! (str  "   some string   ")))
(test::assert-equal "some string" (str-trim! (str  (str "   some string   "))))
(test::assert-equal "some string" (str-trim! (str  "some string   ")))
(test::assert-equal "some string" (str-trim! (str  "some string")))

(test::assert-equal "   some string" (str-trim! (str  "   some string") :right))
(test::assert-equal "   some string" (str-trim! (str  "   some string   ") :right))
(test::assert-equal "   some string" (str-trim! (str  (str "   some string   ") :right)))
(test::assert-equal "some string" (str-trim! (str  "some string   ") :right))
(test::assert-equal "some string" (str-trim! (str  "some string") :right))

(test::assert-equal "some string" (str-trim! (str  "   some string") :left))
(test::assert-equal "some string   " (str-trim! (str  "   some string   ") :left))
(test::assert-equal "some string   " (str-trim! (str  (str "   some string   ") :left)))
(test::assert-equal "some string   " (str-trim! (str  "some string   ") :left))
(test::assert-equal "some string" (str-trim! (str  "some string") :left))
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
    add_builtin(
        env,
        "str-map",
        str_map,
        r#"Usage: (str-map string lambda) -> string

Make a new string by applying lambda to each char in input string.

Section: string

Example:
(test::assert-equal "XstringXstrX" (str-map "xstringxstrx" (fn (ch) (if (= #\x ch) #\X ch))))
(def test-str-map (str-map "xstringxstrx" (fn (ch) (if (= #\x ch) #\X ch))))
(test::assert-equal "XstringXstrX" test-str-map)
(test::assert-true (string? test-str-map))
(def test-str-map (str-map (str "xstringxstrx") (fn (ch) (if (= #\x ch) #\X ch))))
(test::assert-equal "XstringXstrX" test-str-map)
(test::assert-true (string? test-str-map))
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
