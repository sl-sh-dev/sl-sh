use crate::LispResult;
use crate::VarArgs;
use sl_sh_proc_macros::sl_sh_fn;
use std::borrow::Cow;
use std::collections::HashMap;
use std::hash::{BuildHasher, Hash, Hasher};

use unicode_segmentation::UnicodeSegmentation;

use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::types::*;
use crate::{param_eval, params_done};
use std::collections::hash_map::DefaultHasher;

fn as_string(environment: &mut Environment, exp: &Expression) -> Result<String, LispError> {
    exp.as_string(environment)
}

/// Usage: (str-trim string) -> string
///
/// Trim right and left whitespace from string.
///
/// Section: string
///
/// Example:
/// (test::assert-equal "some string" (str-trim "   some string"))
/// (test::assert-equal "some string" (str-trim "   some string   "))
/// (test::assert-equal "some string" (str-trim (str "   some string   ")))
/// (test::assert-equal "some string" (str-trim "some string   "))
/// (test::assert-equal "some string" (str-trim "some string"))
#[sl_sh_fn(fn_name = "str-trim")]
fn str_trim(arg: String) -> LispResult<Expression> {
    Ok(Expression::alloc_data(ExpEnum::String(
        arg.trim().to_string().into(),
        None,
    )))
}

/// Usage: (str-ltrim string) -> string
///
/// Trim left whitespace from string.
///
/// Section: string
///
/// Example:
/// (test::assert-equal "some string" (str-ltrim "   some string"))
/// (test::assert-equal "some string   " (str-ltrim "   some string   "))
/// (test::assert-equal "some string   " (str-ltrim (str "   some string   ")))
/// (test::assert-equal "some string   " (str-ltrim "some string   "))
/// (test::assert-equal "some string" (str-ltrim "some string"))
#[sl_sh_fn(fn_name = "str-ltrim")]
fn str_ltrim(arg: String) -> LispResult<Expression> {
    Ok(Expression::alloc_data(ExpEnum::String(
        arg.trim_start().to_string().into(),
        None,
    )))
}

/// Usage: (str-rtrim string) -> string
///
/// Trim right whitespace from string.
///
/// Section: string
///
/// Example:
/// (test::assert-equal "   some string" (str-rtrim "   some string"))
/// (test::assert-equal "   some string" (str-rtrim "   some string   "))
/// (test::assert-equal "   some string" (str-rtrim (str "   some string   ")))
/// (test::assert-equal "some string" (str-rtrim "some string   "))
/// (test::assert-equal "some string" (str-rtrim "some string"))
#[sl_sh_fn(fn_name = "str-rtrim")]
fn str_rtrim(arg: String) -> LispResult<Expression> {
    Ok(Expression::alloc_data(ExpEnum::String(
        arg.trim_end().to_string().into(),
        None,
    )))
}

/// Usage: (str-replace string old-pattern new-pattern) -> string
///
/// Replace occurrences of second string with third in the first string.
///
/// Section: string
///
/// Example:
/// (test::assert-equal "some yyy string" (str-replace "some xxx string" "xxx" "yyy"))
/// (test::assert-equal "some yyy string yyy" (str-replace "some xxx string xxx" "xxx" "yyy"))
/// (test::assert-equal "yyy some yyy string yyy" (str-replace "xxx some xxx string xxx" "xxx" "yyy"))
#[sl_sh_fn(fn_name = "str-replace")]
fn str_replace(source: String, old_pattern: &str, new_pattern: &str) -> LispResult<Expression> {
    let new_str = source.replace(old_pattern, new_pattern);
    Ok(Expression::alloc_data(ExpEnum::String(
        new_str.into(),
        None,
    )))
}

/// Usage: (str-split split-pattern string) -> vector
///
/// Use a pattern to split a string (:whitespace to split on whitespace).
///
/// Section: string
///
/// Example:
/// (test::assert-equal '("some" "yyy" "string") (str-split "xxx" "somexxxyyyxxxstring"))
/// (test::assert-equal '("some" "yyy" "string" "") (str-split "xxx" "somexxxyyyxxxstringxxx"))
/// (test::assert-equal '("" "some" "yyy" "string" "") (str-split "xxx" "xxxsomexxxyyyxxxstringxxx"))
/// (test::assert-equal '("some" "yyy" "string") (str-split :whitespace "some yyy string"))
/// (test::assert-equal '("somexxxyyyxxxstring") (str-split :whitespace "somexxxyyyxxxstring"))
/// (test::assert-equal '("somexxxyyyxxxstring") (str-split "zzz" "somexxxyyyxxxstring"))
#[sl_sh_fn(fn_name = "str-split")]
fn str_split(pat: &str, text: &str) -> LispResult<Expression> {
    let mut split_list: Vec<Expression> = Vec::new();
    if pat == ":whitespace" {
        for s in text.split_whitespace() {
            split_list.push(Expression::alloc_data(ExpEnum::String(
                s.to_string().into(),
                None,
            )));
        }
    } else {
        for s in text.split(&pat) {
            split_list.push(Expression::alloc_data(ExpEnum::String(
                s.to_string().into(),
                None,
            )));
        }
    }
    Ok(Expression::with_list(split_list))
}

/// Usage: (str-rsplit split-pattern string) -> vector
///
/// Use a pattern to split a string into reverse order.
///
/// Section: string
///
/// Example:
/// (test::assert-equal '(\"some\" \"yyy\" \"string\") (str-rsplit \"xxx\" \"stringxxxyyyxxxsome\"))
/// (test::assert-equal '(\"\" \"some\" \"yyy\" \"string\") (str-rsplit \"xxx\" \"stringxxxyyyxxxsomexxx\"))
/// (test::assert-equal '(\"some\" \"yyy\" \"string\") (str-rsplit \" \" \"string yyy some\"))
/// (test::assert-equal '(\"somexxxyyyxxxstring\") (str-rsplit :whitespace \"somexxxyyyxxxstring\"))
/// (test::assert-equal '(\"somexxxyyyxxxstring\") (str-rsplit \"zzz\" \"somexxxyyyxxxstring\"))
#[sl_sh_fn(fn_name = "str-rsplit")]
fn str_rsplit(pat: &str, text: &str) -> LispResult<Expression> {
    let mut split_list: Vec<Expression> = Vec::new();
    for s in text.rsplit(&pat) {
        split_list.push(Expression::alloc_data(ExpEnum::String(
            s.to_string().into(),
            None,
        )));
    }
    Ok(Expression::with_list(split_list))
}

/// Usage: (str-splitn n split-pattern string) -> vector
///
/// Use a pattern to split a string with at most n items.
///
/// Section: string
///
/// Example:
/// (test::assert-equal '("some" "yyy" "string") (str-splitn 3 "xxx" "somexxxyyyxxxstring"))
/// (test::assert-equal '("some" "yyy" "string") (str-splitn 4 "xxx" "somexxxyyyxxxstring"))
/// (test::assert-equal '("some" "yyy" "stringxxxother") (str-splitn 3 "xxx" "somexxxyyyxxxstringxxxother"))
/// (test::assert-equal '("somexxxyyyxxxstringxxxother") (str-splitn 1 "xxx" "somexxxyyyxxxstringxxxother"))
/// (test::assert-equal '() (str-splitn 0 "xxx" "somexxxyyyxxxstringxxxzero"))
#[sl_sh_fn(fn_name = "str-splitn")]
fn str_splitn(n: i64, pat: &str, text: &str) -> LispResult<Expression> {
    if n < 0 {
        Err(LispError::new(
            "str-splitn first form must be a positive integer",
        ))
    } else {
        let mut split_list: Vec<Expression> = Vec::new();
        for s in text.splitn(n as usize, &pat) {
            split_list.push(Expression::alloc_data(ExpEnum::String(
                s.to_string().into(),
                None,
            )));
        }
        Ok(Expression::with_list(split_list))
    }
}

/// Usage: (str-rsplitn n split-pattern string) -> vector
///
/// Use a pattern to split a string with at most n items returned in reverse order.
///
/// Section: string
///
/// Example:
/// (test::assert-equal '("some" "yyy" "string") (str-rsplitn 3 "xxx" "stringxxxyyyxxxsome"))
/// (test::assert-equal '("some" "yyy" "string") (str-rsplitn 4 "xxx" "stringxxxyyyxxxsome"))
/// (test::assert-equal '("other" "string" "somexxxyyy") (str-rsplitn 3 "xxx" "somexxxyyyxxxstringxxxother"))
/// (test::assert-equal '("somexxxyyyxxxstringxxxother") (str-rsplitn 1 "xxx" "somexxxyyyxxxstringxxxother"))
/// (test::assert-equal '() (str-rsplitn 0 "xxx" "somexxxyyyxxxstringxxxzero"))
#[sl_sh_fn(fn_name = "str-rsplitn")]
fn str_rsplitn(n: usize, pat: &str, text: &str) -> LispResult<Vec<String>> {
    let mut split_list = Vec::new();
    for s in text.rsplitn(n, &pat) {
        split_list.push(s.to_string());
    }
    Ok(split_list)
}

/// Usage: (str-cat-list join-str sequence) -> string
///
/// Build a string by concatenating a sequence of strings by join-str.
///
/// Section: string
///
/// Example:
/// (test::assert-equal "stringxxxyyyxxxsome" (str-cat-list "xxx" '("string" "yyy" "some")))
/// (test::assert-equal "string yyy some" (str-cat-list " " '("string" "yyy" "some")))
/// (test::assert-equal "stringyyysome" (str-cat-list "" '("string" "yyy" "some")))
#[sl_sh_fn(fn_name = "str-cat-list")]
fn str_cat_list(join_str: String, list: Vec<String>) -> String {
    let mut new_str = String::new();
    let mut first = true;
    for exp in list {
        if !first {
            new_str.push_str(&join_str);
        }
        new_str.push_str(&exp);
        first = false;
    }
    new_str
}

/// Usage: (str-sub string start [length]) -> string
///
/// Return a substring from a string given start (0 based) and optional length.
/// If length is 0 or not provided produces the rest of the string from start to
/// string end.
///
/// Section: string
///
/// Example:
/// (test::assert-equal "string" (str-sub "stringxxxyyyxxxsome" 0 6))
/// (test::assert-equal "some" (str-sub "stringxxxyyyxxxsome" 15 4))
/// (test::assert-equal "yyy" (str-sub "stringxxxyyyxxxsome" 9 3))
/// (test::assert-equal "some" (str-sub "stringxxxyyyxxxsome" 15))
#[sl_sh_fn(fn_name = "str-sub")]
fn str_sub(s: &str, start: usize, length: Option<usize>) -> LispResult<String> {
    let len = if let Some(length) = length {
        length as usize
    } else {
        0usize
    };
    if (start + len) <= s.len() {
        if len > 0 {
            Ok(s[start..(start + len)].to_string())
        } else {
            Ok(s[start..].to_string())
        }
    } else {
        Err(LispError::new("str-sub index out of range"))
    }
}

/// Usage: (str-append string string) -> string
///
/// Make a new string by appending two strings.
///
/// Section: string
///
/// Example:
/// (test::assert-equal "stringsome" (str-append "string" "some"))
/// (test::assert-equal "string" (str-append "string" ""))
/// (test::assert-equal "string " (str-append "string" " "))
#[sl_sh_fn(fn_name = "str-append")]
fn str_append(start: &str, end: &str) -> String {
    let mut new_string = String::with_capacity(start.len() + end.len());
    new_string.push_str(start);
    new_string.push_str(end);
    new_string
}

fn builtin_str(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let gpo = set_grab_proc_output(environment, true);

    let mut res = String::new();
    for a in args {
        let a = eval(gpo.environment, a)?;
        res.push_str(&a.as_string(gpo.environment)?);
    }
    Ok(Expression::alloc_data(ExpEnum::String(res.into(), None)))
}

pub fn builtin_do_unstr(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let gpo = set_grab_proc_output(environment, false);
    crate::builtins::builtin_do(gpo.environment, args)
}

/// Usage: (str-empty? string) -> t/nil
///
/// Is a string empty?  Let's find out...
///
/// Section: string
///
/// Example:
/// (test::assert-true (str-empty? ""))
/// (test::assert-true (str-empty? (str-trim "   ")))
/// (test::assert-false (str-empty? " "))
/// (test::assert-false (str-empty? "string"))
#[sl_sh_fn(fn_name = "str-empty?")]
fn str_empty(string: &str) -> bool {
    string.is_empty()
}

/// Usage: (str-nth n string) -> char
///
/// Get the nth char of a string or nil if not found.
///
/// Section: string
///
/// Example:
/// (test::assert-equal #\a (str-nth 2 "stau"))
/// (test::assert-equal #\s (str-nth 0 "stau"))
/// (test::assert-equal #\u (str-nth 3 "stau"))
/// (test::assert-equal nil (str-nth 9 "stau"))
#[sl_sh_fn(fn_name = "str-nth")]
fn str_nth(idx: i64, string: &str) -> LispResult<Expression> {
    for (i, ch) in UnicodeSegmentation::graphemes(string, true).enumerate() {
        if i as i64 == idx {
            return Ok(Expression::alloc_data(ExpEnum::Char(ch.to_string().into())));
        }
    }
    Ok(Expression::make_nil())
}

/// Usage: (str-lower string) -> string
///
/// Get all lower case string from a string.
///
/// Section: string
///
/// Example:
/// (test::assert-equal "stau" (str-lower "STAU"))
/// (test::assert-equal "stau" (str-lower "stau"))
/// (test::assert-equal "stau" (str-lower "Stau"))
/// (test::assert-equal "stau" (str-lower "StaU"))
/// (test::assert-equal "stau" (str-lower "sTaU"))
#[sl_sh_fn(fn_name = "str-lower")]
fn str_lower(string: String) -> String {
    string.to_ascii_lowercase()
}

/// Usage: (str-upper string) -> string
///
/// Get all upper case string from a string.
///
/// Section: string
///
/// Example:
/// (test::assert-equal "STAU" (str-upper "STAU"))
/// (test::assert-equal "STAU" (str-upper "stau"))
/// (test::assert-equal "STAU" (str-upper "Stau"))
/// (test::assert-equal "STAU" (str-upper "StaU"))
/// (test::assert-equal "STAU" (str-upper "sTaU"))
#[sl_sh_fn(fn_name = "str-upper")]
fn str_upper(string: String) -> String {
    string.to_ascii_uppercase()
}

/// Usage: (str-bytes string) -> int
///
/// Return number of bytes in a string (may be more then length).
///
/// Strings are utf8 so it chars and bytes may not be a one to one match.
///
/// Section: string
///
/// Example:
/// (test::assert-equal 4 (str-bytes "Stau"))
/// (test::assert-equal 0 (str-bytes ""))
/// ; Note 5 chars and 6 bytes because of the final char.
/// (test::assert-equal 6 (str-bytes "StauΣ"))
#[sl_sh_fn(fn_name = "str-bytes")]
fn str_bytes(string: &str) -> usize {
    string.len()
}

/// Usage: (str-starts-with pattern string) -> t/nil
///
/// True if string start with pattern (both strings).
///
/// Section: string
///
/// Example:
/// (test::assert-true (str-starts-with "Stau" "Stausomething"))
/// (test::assert-false (str-starts-with "StaU" "Stausomething"))
#[sl_sh_fn(fn_name = "str-starts-with")]
fn str_starts_with(pat: &str, text: &str) -> bool {
    text.starts_with(&pat)
}

/// Usage: (str-contains pattern string) -> t/nil
///
/// True if string contains pattern (both strings).
///
/// Section: string
///
/// Example:
/// (test::assert-true (str-contains "Stau" "Stausomething"))
/// (test::assert-false (str-contains "StaU" "Stausomething"))
/// (test::assert-true (str-contains "some" "Stausomething"))
/// (test::assert-false (str-contains "Some" "Stausomething"))
/// (test::assert-true (str-contains "thing" "Stausomething"))
/// (test::assert-false (str-contains "Thing" "Stausomething"))
/// (test::assert-true (str-contains "someΣ" "StausomeΣthing"))
#[sl_sh_fn(fn_name = "str-contains")]
fn str_contains(pat: &str, text: &str) -> bool {
    text.contains(&pat)
}

/// Usage: (str-push! string arg0 ... argN) -> string
///
/// Push the args (as strings) onto the string.  This is a destructive form.
///
/// Arguments will be turned into strings.  Returns the string it was given.
///
/// Section: string
///
/// Example:
/// (test::assert-equal "stringsome" (str-push! (str "string") "some"))
/// (def test-str-push (str "def-string"))
/// (test::assert-equal "def-stringsome" (str-push! test-str-push "some"))
/// (test::assert-equal "def-stringsome" test-str-push)
#[sl_sh_fn(fn_name = "str-push!")]
fn str_push(target: Expression, strings: VarArgs<String>) -> LispResult<Expression> {
    let mut target_d = target.get_mut();
    if let ExpEnum::String(res, chars) = &mut target_d.data {
        for a in strings {
            res.to_mut().push_str(&a);
        }
        *chars = None; // maintain the iterator invariant.
        drop(target_d);
        Ok(target)
    } else {
        Err(LispError::new(
            "str-push! takes a string buffer as first form",
        ))
    }
}

/// Usage: (str-clear! string) -> string
///
/// Clears a string.  This is a destructive form.
///
/// Returns the string it was given.
///
/// Section: string
///
/// Example:
/// (test::assert-equal "" (str-clear! (str "string")))
/// (def test-str-clear (str "def-string"))
/// (test::assert-equal "" (str-clear! test-str-clear))
/// (test::assert-equal "" test-str-clear)
#[sl_sh_fn(fn_name = "str-clear!")]
fn str_clear(s: Expression) -> LispResult<Expression> {
    let mut s_d = s.get_mut();
    if let ExpEnum::String(res, chars) = &mut s_d.data {
        *chars = None; // maintain the iterator invariant.
        res.to_mut().clear();
        drop(s_d);
        Ok(s)
    } else {
        Err(LispError::new(
            "str-clear! takes a string buffer as first form",
        ))
    }
}

fn str_map_inner(
    environment: &mut Environment,
    func: &Lambda,
    string: &str,
) -> Result<String, LispError> {
    let mut res = String::new();
    for ch in UnicodeSegmentation::graphemes(string, true) {
        let list = vec![
            Expression::alloc_data(ExpEnum::Lambda(func.clone())),
            Expression::alloc_data(ExpEnum::Char(ch.to_string().into())),
        ];
        let a = eval(environment, Expression::with_list(list))?;
        res.push_str(&as_string(environment, &a)?);
    }
    Ok(res)
}

fn builtin_str_map(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(func) = args.next() {
        if let Some(string) = args.next() {
            if args.next().is_none() {
                let func = eval(environment, func)?;
                let string = eval(environment, string)?;
                let func_d = func.get();
                if let ExpEnum::Lambda(func) = &func_d.data {
                    if let ExpEnum::String(string, _) = &string.get().data {
                        return Ok(Expression::alloc_data(ExpEnum::String(
                            str_map_inner(environment, func, string)?.into(),
                            None,
                        )));
                    }
                }
            }
        }
    }
    Err(LispError::new("str-map takes lambda and a string"))
}

fn builtin_str_iter_start(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(string) = args.next() {
        if args.next().is_none() {
            let string_outer = eval(environment, string)?;
            let mut string_d = string_outer.get_mut();
            if let ExpEnum::String(string, chars) = &mut string_d.data {
                // This unsafe should be fine as long as the iterator is invalidated (set to None)
                // on ANY change to string.
                let nstr = unsafe { &*(string.as_ref() as *const str) };
                *chars = Some(Box::new(
                    UnicodeSegmentation::graphemes(nstr, true)
                        .map(Cow::Borrowed)
                        .peekable(),
                ));
                drop(string_d);
                return Ok(string_outer);
            }
        }
    }
    Err(LispError::new("str-iter-start takes a string"))
}

fn builtin_str_iter_next(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(string) = args.next() {
        if args.next().is_none() {
            let string = eval(environment, string)?;
            let mut string_d = string.get_mut();
            if let ExpEnum::String(_, None) = &string_d.data {
                return Err(LispError::new("str-iter-next: not an iterator"));
            }
            if let ExpEnum::String(_string, Some(ch_iter)) = &mut string_d.data {
                if let Some(ch) = ch_iter.next() {
                    // If this is the reader text stream then advance line/column.
                    if let Some(tags) = &mut string_d.meta_tags {
                        if tags.contains("--reader-text-stream--") {
                            if ch == "\n" {
                                environment.reader_state.line += 1;
                                environment.reader_state.column = 0;
                            } else {
                                environment.reader_state.column += 1;
                            }
                        }
                    }
                    return Ok(Expression::alloc_data(ExpEnum::Char(ch.to_string().into())));
                } else {
                    return Ok(Expression::make_nil());
                }
            }
        }
    }
    Err(LispError::new("str-iter-next takes a string"))
}

fn builtin_str_iter_peek(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(string) = args.next() {
        if args.next().is_none() {
            let string = eval(environment, string)?;
            let mut string_d = string.get_mut();
            if let ExpEnum::String(_, None) = &string_d.data {
                return Err(LispError::new("str-iter-peek: not an iterator"));
            }
            if let ExpEnum::String(_string, Some(ch_iter)) = &mut string_d.data {
                if let Some(ch) = ch_iter.peek() {
                    return Ok(Expression::alloc_data(ExpEnum::Char(ch.to_string().into())));
                } else {
                    return Ok(Expression::make_nil());
                }
            }
        }
    }
    Err(LispError::new("str-iter-peek takes a string"))
}

fn builtin_str_iter_empty(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(string) = args.next() {
        if args.next().is_none() {
            let string = eval(environment, string)?;
            let mut string_d = string.get_mut();
            if let ExpEnum::String(_, None) = &string_d.data {
                return Ok(Expression::make_true());
            }
            if let ExpEnum::String(_, Some(ch_iter)) = &mut string_d.data {
                return if ch_iter.peek().is_none() {
                    Ok(Expression::make_true())
                } else {
                    Ok(Expression::make_nil())
                };
            }
        }
    }
    Err(LispError::new("str-iter-empty takes a string"))
}

fn builtin_char_lower(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(ch) = args.next() {
        if args.next().is_none() {
            if let ExpEnum::Char(ch) = &eval(environment, ch)?.get().data {
                return Ok(Expression::alloc_data(ExpEnum::Char(
                    ch.to_lowercase().into(),
                )));
            }
        }
    }
    Err(LispError::new(
        "char-lower takes a single char and produces it's ascii lowercase char",
    ))
}

fn builtin_char_upper(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(ch) = args.next() {
        if args.next().is_none() {
            if let ExpEnum::Char(ch) = &eval(environment, ch)?.get().data {
                return Ok(Expression::alloc_data(ExpEnum::Char(
                    ch.to_uppercase().into(),
                )));
            }
        }
    }
    Err(LispError::new(
        "char-upper takes a single char and produces it's ascii upper char",
    ))
}

fn builtin_char_is_whitespace(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(ch) = args.next() {
        if args.next().is_none() {
            if let ExpEnum::Char(ch) = &eval(environment, ch)?.get().data {
                return if ch.len() == 1 && ch.chars().next().unwrap().is_whitespace() {
                    Ok(Expression::alloc_data(ExpEnum::True))
                } else {
                    Ok(Expression::make_nil())
                };
            }
        }
    }
    Err(LispError::new(
        "char-whitespace? takes a single char and returns true if it is whitespace",
    ))
}

fn builtin_char_int(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let next_arg = param_eval(environment, args, "char->int")?;
    let next_arg_d = next_arg.get();
    params_done(args, "char->int")?;
    let to_int = |s: &str| -> Result<Expression, LispError> {
        let mut count = 0;
        for _s in <str as UnicodeSegmentation>::graphemes(s, true) {
            count += 1;
        }
        match count {
            0 => Ok(Expression::alloc_data(ExpEnum::Int(0))),
            1 => {
                let mut int_val: i64 = 0;
                let mut overflow = false;
                for c in s.chars() {
                    let (add, overflowed) = int_val.overflowing_add(c as i64);
                    if overflowed {
                        overflow = true;
                        break;
                    }
                    int_val += add;
                }
                if overflow {
                    Err(LispError::new("overflow occurred in char->in tadding unicode scalar values that compose provided grapheme interpreted as unsigned 32 byte integers: {:?}."))
                } else {
                    Ok(Expression::alloc_data(ExpEnum::Int(int_val)))
                }
            }
            _ => Err(LispError::new(
                "char->int takes one grapheme, multiple were provided.",
            )),
        }
    };
    match &next_arg_d.data {
        ExpEnum::String(s, _) => to_int(s),
        ExpEnum::Char(s) => to_int(s),
        ExpEnum::CodePoint(c) => Ok(Expression::alloc_data(ExpEnum::Int(*c as i64))),
        _ => Err(LispError::new(
            "char->int expects one argument of type Char or String",
        )),
    }
}

fn builtin_codepoints(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let next_arg = param_eval(environment, args, "codepoints")?;
    let next_arg_d = next_arg.get();
    params_done(args, "codepoints")?;
    let with_str = |s: &str| {
        let mut codepoints: Vec<Expression> = Vec::new();
        for c in s.chars() {
            codepoints.push(Expression::alloc_data(ExpEnum::CodePoint(c)));
        }
        Ok(Expression::alloc_data(ExpEnum::Vector(codepoints)))
    };
    match &next_arg_d.data {
        ExpEnum::String(s, _) => with_str(s),
        ExpEnum::Char(s) => with_str(s),
        ExpEnum::CodePoint(c) => with_str(&format!("{}", c)),
        _ => Err(LispError::new(
            "codepoints expects one argument of type Char or String",
        )),
    }
}

fn builtin_str_float(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let next_arg = param_eval(environment, args, "str-hash")?;
    let next_arg_d = next_arg.get();
    params_done(args, "str-hash")?;
    let with_str = |s: &str| {
        let mut h = DefaultHasher::new();
        s.hash(&mut h);
        let hash: u64 = h.finish();
        Ok(Expression::alloc_data(ExpEnum::Float(hash as f64)))
    };
    match &next_arg_d.data {
        ExpEnum::String(s, _) => with_str(s),
        ExpEnum::Char(s) => with_str(s),
        _ => Err(LispError::new(
            "str->float expects one argument of type Char or String",
        )),
    }
}

pub fn add_str_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    intern_str_trim(interner, data);
    intern_str_ltrim(interner, data);
    intern_str_rtrim(interner, data);
    intern_str_replace(interner, data);
    intern_str_split(interner, data);
    intern_str_rsplit(interner, data);
    intern_str_splitn(interner, data);
    intern_str_rsplitn(interner, data);
    intern_str_cat_list(interner, data);
    intern_str_sub(interner, data);
    intern_str_append(interner, data);
    data.insert(
        interner.intern("do-unstr"),
        Expression::make_special(
            builtin_do_unstr,
            r#"Usage: (do-unstr arg0 ... argN) -> argN

Like do except if in an 'str' form then any processes will send their output to
*stdout* instead of being captured as a string ('undoes' a str for processes).
Note: does not convert arguments into strings, works like do.

Section: string

Example:
; out> uses do-unstr so use a simplified one for these tests.
(defmacro tst-out> (file body) `(dyn *stdout* (open ,file :create :truncate) ,body))

(test::assert-equal "string 50 test
" (str "string" " " 50 " " (syscall 'echo "test")))
(test::assert-equal "string 50 test2
" (str "string" " " 50 " " (tst-out> "/tmp/test-str-echo" (syscall 'echo "test2"))))
(test::assert-equal "string 50 " (str "string" " " 50 " " (do-unstr (tst-out> "/tmp/test-str-echo" (syscall 'echo "test"))))
"#,
        ),
    );
    data.insert(
        interner.intern("str"),
        Expression::make_function(
            builtin_str,
            r#"Usage: (str arg0 ... argN) -> string

Make a new string with it's arguments.

Arguments will be turned into strings.  If an argument is a process then the
output of the process will be captured and put into the string.

Section: string

Example:
(test::assert-equal "stringsome" (str "string" "some"))
(test::assert-equal "string" (str "string" ""))
(test::assert-equal "string 50" (str "string" " " 50))
(test::assert-equal "string 50 test
" (str "string" " " 50 " " (syscall 'echo "test")))
"#,
        ),
    );
    intern_str_empty(interner, data);
    intern_str_nth(interner, data);
    intern_str_lower(interner, data);
    intern_str_upper(interner, data);
    intern_str_bytes(interner, data);
    intern_str_starts_with(interner, data);
    intern_str_contains(interner, data);
    intern_str_push(interner, data);
    intern_str_clear(interner, data);
    data.insert(
        interner.intern("str-map"),
        Expression::make_function(
            builtin_str_map,
            r#"Usage: (str-map lambda string) -> string

Make a new string by applying lambda to each char.

Section: string

Example:
(test::assert-equal "XstringXstrX" (str-map (fn (ch) (if (= #\x ch) #\X ch)) "xstringxstrx"))
(def test-str-map (str-map (fn (ch) (if (= #\x ch) #\X ch)) "xstringxstrx"))
(test::assert-equal "XstringXstrX" test-str-map)
(test::assert-true (string? test-str-map))
(def test-str-map (str-map (fn (ch) (if (= #\x ch) #\X ch)) (str "xstringxstrx")))
(test::assert-equal "XstringXstrX" test-str-map)
(test::assert-true (string? test-str-map))
"#,
        ),
    );
    data.insert(
        interner.intern("str-iter-start"),
        Expression::make_function(
            builtin_str_iter_start,
            r#"Usage: (str-iter-start string) -> string

Starts or resets the iterator over a string.  Returns the input string with it's
iteration start created and at the first position.  Using the str-iter-* functions
is the proper way to get the chars of a string (since they are UTF a char is not
a fixed size so direct indexing is very inefficient).

Section: string

Example:
(def test-iter-start "test")
(test::assert-true (str-iter-empty? test-iter-start))
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(test::assert-equal #\t (str-iter-next! test-iter-start))
(test::assert-equal #\e (str-iter-next! test-iter-start))
(test::assert-equal #\s (str-iter-next! test-iter-start))
(test::assert-equal #\t (str-iter-next! test-iter-start))
(test::assert-true (str-iter-empty? test-iter-start))
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(test::assert-equal #\t (str-iter-next! test-iter-start))
(test::assert-equal #\e (str-iter-next! test-iter-start))
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(test::assert-equal #\t (str-iter-next! test-iter-start))
(test::assert-equal #\e (str-iter-next! test-iter-start))
(test::assert-equal #\s (str-iter-next! test-iter-start))
(test::assert-equal #\t (str-iter-next! test-iter-start))
(test::assert-true (str-iter-empty? test-iter-start))
"#,
        ),
    );
    data.insert(
        interner.intern("str-iter-next!"),
        Expression::make_function(
            builtin_str_iter_next,
            r#"Usage: (str-iter-next! string) -> char

Returns the next char in the iterator for string.  Returns nil if iteration
is done.

Section: string

Example:
(def test-iter-start "y̆ΛλΣσ")
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(def test-iter-one (str-iter-next! test-iter-start))
(test::assert-equal #\y̆ test-iter-one)
(test::assert-true (= #\y̆ test-iter-one))
(test::assert-false (= #\y test-iter-one))
(test::assert-equal #\Λ (str-iter-next! test-iter-start))
(test::assert-equal #\λ (str-iter-next! test-iter-start))
(test::assert-equal #\Σ (str-iter-next! test-iter-start))
(test::assert-equal #\σ (str-iter-next! test-iter-start))
(test::assert-true (str-iter-empty? test-iter-start))
"#,
        ),
    );
    data.insert(
        interner.intern("str-iter-peek"),
        Expression::make_function(
            builtin_str_iter_peek,
            r#"Usage: (str-iter-peek string) -> char

Returns the char that next will return in the iterator for string.  Returns nil if iteration
is done.  Does not advance the iterator.

Section: string

Example:
(def test-iter-start "y̆ΛλΣσ")
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(def test-iter-one (str-iter-next! test-iter-start))
(test::assert-equal #\y̆ test-iter-one)
(test::assert-true (= #\y̆ test-iter-one))
(test::assert-false (= #\y test-iter-one))
(test::assert-equal #\Λ (str-iter-peek test-iter-start))
(test::assert-equal #\Λ (str-iter-next! test-iter-start))
(test::assert-equal #\λ (str-iter-peek test-iter-start))
(test::assert-equal #\λ (str-iter-next! test-iter-start))
(test::assert-equal #\Σ (str-iter-peek test-iter-start))
(test::assert-equal #\Σ (str-iter-next! test-iter-start))
(test::assert-equal #\σ (str-iter-peek test-iter-start))
(test::assert-equal #\σ (str-iter-next! test-iter-start))
(test::assert-true (str-iter-empty? test-iter-start))
"#,
        ),
    );
    data.insert(
        interner.intern("str-iter-empty?"),
        Expression::make_function(
            builtin_str_iter_empty,
            r#"Usage: (str-iter-empty? string) -> t/nil

Returns true if the iterator for the string is empty or finished.

Section: string

Example:
(def test-iter-start "test")
(test::assert-true (str-iter-empty? test-iter-start))
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(test::assert-equal #\t (str-iter-next! test-iter-start))
(test::assert-equal #\e (str-iter-next! test-iter-start))
(test::assert-equal #\s (str-iter-next! test-iter-start))
(test::assert-equal #\t (str-iter-next! test-iter-start))
(test::assert-true (str-iter-empty? test-iter-start))

(def test-iter-start "test")
(test::assert-true (str-iter-empty? test-iter-start))
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(test::assert-equal #\t (str-iter-next! test-iter-start))
(test::assert-equal #\e (str-iter-next! test-iter-start))
(str-push! test-iter-start "one")
(test::assert-true (str-iter-empty? test-iter-start))
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(str-clear! test-iter-start)
(test::assert-true (str-iter-empty? test-iter-start))
"#,
        ),
    );

    data.insert(
        interner.intern("char-lower"),
        Expression::make_function(
            builtin_char_lower,
            r#"Usage: (char-lower char) -> char

Get lower case (utf) character for a character.

Section: char

Example:
(test::assert-equal #\a (char-lower #\A))
(test::assert-equal #\a (char-lower #\a))
(test::assert-not-equal #\a (char-lower #\Z))
(test::assert-equal #\λ (char-lower #\Λ))
(test::assert-equal #\λ (char-lower #\λ))
"#,
        ),
    );
    data.insert(
        interner.intern("char-upper"),
        Expression::make_function(
            builtin_char_upper,
            r#"Usage: (char-upper char) -> char

Get upper case (utf) character for a character.

Section: char

Example:
(test::assert-equal #\A (char-upper #\A))
(test::assert-equal #\A (char-upper #\a))
(test::assert-not-equal #\A (char-upper #\Z))
(test::assert-equal #\Λ (char-upper #\λ))
(test::assert-equal #\Λ (char-upper #\Λ))
"#,
        ),
    );
    data.insert(
        interner.intern("char-whitespace?"),
        Expression::make_function(
            builtin_char_is_whitespace,
            r#"Usage: (char-whitespace? char) -> t/nil

Returns true if a character is whitespace, false/nil otherwise.

Section: char

Example:
(test::assert-true (char-whitespace? #\ ))
(test::assert-true (char-whitespace? #\tab))
(test::assert-false (char-whitespace? #\s))
"#,
        ),
    );

    data.insert(
        interner.intern("char->int"),
        Expression::make_function(
            builtin_char_int,
            "Usage: (char->int a-char)

Reads a char or string, which may be composed of one or more unicode scalar values,
(see (doc 'codepoints) for more information) and returns a sum of the values.
This is not a hashing function, and only accepts one grapheme in the form of a
string or character. Graphemes composed of the same unicode scalar values will
result in the same integer value.'

Section: string

Example:
(test::assert-error-msg (char->int (make-vec)) \"char->int expects one argument of type Char or String\")
(test::assert-error-msg (char->int) \"char->int: Missing required argument, see (doc 'char->int) for usage.\")
(test::assert-error-msg (char->int \"a\" \"b\") \"char->int: Too many arguments, see (doc 'char->int) for usage.\")
(test::assert-error-msg (char->int \"ab\") \"char->int takes one grapheme, multiple were provided.\")
(test::assert-error-msg (char->int \"λ⚙\") \"char->int takes one grapheme, multiple were provided.\")
(test::assert-equal 97 (char->int \"a\"))
(test::assert-equal 97 (char->int #\\a))
(test::assert-equal 7101 (char->int #\\स्))
(test::assert-equal 9881 (char->int (str \"\\\" \"u{2699}\")))
",
        ),
    );

    data.insert(
        interner.intern("codepoints"),
        Expression::make_function(
            builtin_codepoints,
            "Usage: (codepoints string)

Returns array of unicode scalars for each char in string. Note, a char
is not a grapheme. The hindi word namaste (\"न\" \"म\" \"स्\" \"ते\")
written in Devanagari script is 4 graphemes, but 6 unicode scalar values,
(\"u{928}\" \"u{92e}\" \"u{938}\" \"u{94d}\" \"u{924}\" \"u{947}\");
[reference](https://doc.rust-lang.org/book/ch08-02-strings.html#bytes-and-scalar-values-and-grapheme-clusters-oh-my).

Section: string

Example:
(test::assert-error-msg (codepoints) \"codepoints: Missing required argument, see (doc 'codepoints) for usage.\")
(test::assert-error-msg (codepoints (make-vec)) \"codepoints expects one argument of type Char or String\")
(test::assert-error-msg (codepoints \"a\" \"b\") \"codepoints: Too many arguments, see (doc 'codepoints) for usage.\")
(test::assert-equal (vec (str \"\\\" \"u{2699}\")) (codepoints \"⚙\"))
(test::assert-equal (vec (str \"\\\" \"u{938}\") ((str \"\\\" \"u{94d}\"))) (codepoints \"स्\"))
(test::assert-equal (vec (str \"\\\" \"u{938}\") ((str \"\\\" \"u{94d}\"))) (codepoints #\\स्))
(test::assert-equal (vec (str \"\\\" \"u{61}\")) (codepoints \"a\"))
(test::assert-equal (vec (str \"\\\" \"u{61}\")) (codepoints #\\a))
",
        ),
    );

    data.insert(
        interner.intern("str->float"),
        Expression::make_function(
            builtin_str_float,
            "Usage: (str->float s)

Accepts values of type String or Char and returns hash of value as Float. Hash is not cryptographically secure.

Section: string

Example:
(test::assert-error-msg (str-hash) \"str-hash: Missing required argument, see (doc 'str-hash) for usage.\")
(test::assert-error-msg (str-hash (make-vec)) \"str->float expects one argument of type Char or String\")
(test::assert-error-msg (str-hash \"a\" \"b\") \"str-hash: Too many arguments, see (doc 'str-hash) for usage.\")
(test::assert-equal (vec (str \"\\\" \"u{2699}\")) (codepoints \"⚙\"))
(test::assert-equal 8186225505942432000 (str-hash \"a\"))
(test::assert-equal 8186225505942432000 (str-hash #\\a))
",
        ),
    );
}
