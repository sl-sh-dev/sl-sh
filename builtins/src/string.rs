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
            let buffer = vm.get_string_mut(*handle)?;
            let trimmed = buffer.trim_end();
            buffer.truncate(trimmed.len());
            let trimmed = buffer.trim_start();
            buffer.replace_range(..(buffer.len() - trimmed.len()), "");
            Ok(Value::String(*handle))
        }
        (Some(Value::String(handle)), Some(Value::Keyword(i)), None) if *i == right => {
            let buffer = vm.get_string_mut(*handle)?;
            let trimmed = buffer.trim_end();
            buffer.truncate(trimmed.len());
            Ok(Value::String(*handle))
        }
        (Some(Value::String(handle)), Some(Value::Keyword(i)), None) if *i == left => {
            let buffer = vm.get_string_mut(*handle)?;
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
        let pat = pat.pretty_value(vm);
        if string.contains(&pat) {
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
            let buffer = unsafe { &mut *(vm.get_string_mut(handle)? as *mut String) };
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
            Err(VMError::new_vm(format!(
                "str-push!: first arg must be a string (not a const), got a {}",
                buffer.display_type(vm)
            )))
        }
    } else {
        Err(VMError::new_vm(
            "str-push!: takes a string with 0 more arguments to append".to_string(),
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
        Value::CharClusterLong(handle) => {
            tmp_str = vm.get_string(handle).to_string();
            &tmp_str
        }
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
        let param = vm.alloc_char(ch);
        // Dont use '?' or return early until the heap_unsticky() call below.
        vm.heap_sticky(param);
        let val = match func {
            Value::Lambda(handle) => {
                let func = vm.get_lambda(handle);
                vm.do_call(func, &[param], None)
            }
            Value::Closure(handle) => {
                let (func, caps) = vm.get_closure(handle);
                let caps: Vec<Handle> = caps.to_vec();
                vm.do_call(func, &[param], Some(&caps[..]))
            }
            Value::Builtin(idx) => vm.get_builtin(idx)(vm, &[param]),
            _ => Err(VMError::new_vm(
                "str-map: second arg must be callable".to_string(),
            )),
        };
        vm.heap_unsticky(param);
        match val? {
            Value::StringConst(i) => res.push_str(vm.get_interned(i)),
            Value::String(h) => res.push_str(vm.get_string(h)),
            Value::CodePoint(ch) => res.push(ch),
            Value::CharCluster(l, c) => res.push_str(&String::from_utf8_lossy(&c[0..l as usize])),
            Value::CharClusterLong(h) => res.push_str(vm.get_string(h)),
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

fn str_empty(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), None) = (i.next(), i.next()) {
        match string {
            Value::StringConst(i) => {
                if vm.get_interned(*i).is_empty() {
                    Ok(Value::True)
                } else {
                    Ok(Value::False)
                }
            }
            Value::String(h) => {
                if vm.get_string(*h).is_empty() {
                    Ok(Value::True)
                } else {
                    Ok(Value::False)
                }
            }
            _ => Err(VMError::new_vm(format!(
                "str-empty?: takes a string, got a {}",
                string.display_type(vm)
            ))),
        }
    } else {
        Err(VMError::new_vm("str-empty?: takes a string".to_string()))
    }
}

fn str_starts_with(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), Some(pat), None) = (i.next(), i.next(), i.next()) {
        let string = string.pretty_value(vm);
        let pat = pat.pretty_value(vm);
        if string.starts_with(&pat) {
            Ok(Value::True)
        } else {
            Ok(Value::False)
        }
    } else {
        Err(VMError::new_vm(
            "str-starts-with: takes a string and a pattern".to_string(),
        ))
    }
}

fn str_split(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(string), Some(pat), None) = (i.next(), i.next(), i.next()) {
        let string = string.pretty_value(vm);
        let pat = pat.pretty_value(vm);
        let mut splits = Vec::new();
        vm.pause_gc();
        if pat == ":whitespace" {
            for s in string.split_whitespace() {
                splits.push(vm.alloc_string(s.to_string()));
            }
        } else {
            for s in string.split(&pat) {
                splits.push(vm.alloc_string(s.to_string()));
            }
        }
        let ret = vm.alloc_vector(splits);
        vm.unpause_gc();
        Ok(ret)
    } else {
        Err(VMError::new_vm(
            "str-split: takes a string and a pattern".to_string(),
        ))
    }
}

fn char_whitespace(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    if let (Some(ch), None) = (i.next(), i.next()) {
        match ch {
            Value::CodePoint(ch) => {
                if ch.is_whitespace() {
                    Ok(Value::True)
                } else {
                    Ok(Value::False)
                }
            }
            Value::CharCluster(l, a) => {
                // TODO- should probably convert this back to a utf string...
                if *l == 1 && a[0].is_ascii_whitespace() {
                    Ok(Value::True)
                } else {
                    Ok(Value::False)
                }
            }
            Value::CharClusterLong(h) => {
                let ch = vm.get_string(*h);
                if ch.len() == 1 && ch.chars().next().unwrap().is_whitespace() {
                    Ok(Value::True)
                } else {
                    Ok(Value::False)
                }
            }
            _ => Err(VMError::new_vm(
                "char-whitespace?: takes a character".to_string(),
            )),
        }
    } else {
        Err(VMError::new_vm(
            "char-whitespace?: takes a character".to_string(),
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

Trim right and/or left whitespace from string in place.  With no optional keywork trims both,
otherwise :right or :left specify right or left trimming.

This is a destructive operation (unlike str-trim) and requires an actual non-const string as it's first
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
    add_builtin(
        env,
        "char-whitespace?",
        char_whitespace,
        r"Usage: (char-whitespace? char) -> t/nil

Returns true if a character is whitespace, false/nil otherwise.

Section: char

Example:
(test::assert-true (char-whitespace? \ ))
(test::assert-true (char-whitespace? \tab))
(test::assert-false (char-whitespace? \s))
",
    );
    add_builtin(
        env,
        "str-empty?",
        str_empty,
        r#"Usage: (str-empty? string) -> #t/#f

Is a string empty?  Let's find out...

Section: string

Example:
(test::assert-true (str-empty? ""))
(test::assert-true (str-empty? (str-trim "   ")))
(test::assert-false (str-empty? " "))
(test::assert-false (str-empty? "string"))
"#,
    );
    add_builtin(
        env,
        "str-starts-with",
        str_starts_with,
        r#"Usage: (str-starts-with string pattern) -> #t/#f

True if string start with pattern.

Section: string

Example:
(test::assert-true (str-starts-with "Stausomething" "Stau"))
(test::assert-false (str-starts-with "Stausomething" "StaU"))
"#,
    );
    add_builtin(
        env,
        "str-split",
        str_split,
        r#"Usage: (str-split string split-pattern) -> vector

Use a pattern to split a string (:whitespace to split on whitespace).

Section: string

Example:
(test::assert-equal '("some" "yyy" "string") (str-split "somexxxyyyxxxstring" "xxx"))
(test::assert-equal '("some" "yyy" "string" "") (str-split "somexxxyyyxxxstringxxx" "xxx"))
(test::assert-equal '("" "some" "yyy" "string" "") (str-split "xxxsomexxxyyyxxxstringxxx" "xxx"))
(test::assert-equal '("some" "yyy" "string") (str-split "some yyy string" :whitespace))
(test::assert-equal '("somexxxyyyxxxstring") (str-split "somexxxyyyxxxstring" :whitespace))
(test::assert-equal '("somexxxyyyxxxstring") (str-split "somexxxyyyxxxstring" "zzz"))
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
