use std::borrow::Cow;
use std::collections::HashMap;
use std::hash::{BuildHasher, Hash, Hasher};

use unicode_segmentation::UnicodeSegmentation;

use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::{param_eval, params_done};
use crate::types::*;
use std::collections::hash_map::DefaultHasher;

fn as_string(environment: &mut Environment, exp: &Expression) -> Result<String, LispError> {
    exp.as_string(environment)
}

fn builtin_str_trim(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            let arg = arg.as_string(environment)?;
            return Ok(Expression::alloc_data(ExpEnum::String(
                arg.trim().to_string().into(),
                None,
            )));
        }
    }
    Err(LispError::new("str-trim takes one form"))
}

fn builtin_str_ltrim(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            let arg = arg.as_string(environment)?;
            return Ok(Expression::alloc_data(ExpEnum::String(
                arg.trim_start().to_string().into(),
                None,
            )));
        }
    }
    Err(LispError::new("str-ltrim takes one form"))
}

fn builtin_str_rtrim(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            let arg = arg.as_string(environment)?;
            return Ok(Expression::alloc_data(ExpEnum::String(
                arg.trim_end().to_string().into(),
                None,
            )));
        }
    }
    Err(LispError::new("str-rtrim takes one form"))
}

fn builtin_str_replace(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg0) = args.next() {
        if let Some(arg1) = args.next() {
            if let Some(arg2) = args.next() {
                if args.next().is_none() {
                    let arg0 = &eval(environment, arg0)?;
                    let arg0 = arg0.as_string(environment)?;
                    let arg1 = &eval(environment, arg1)?;
                    let arg1 = arg1.as_string(environment)?;
                    let arg2 = &eval(environment, arg2)?;
                    let arg2 = arg2.as_string(environment)?;
                    let new_str = arg0.replace(&arg1, &arg2);
                    return Ok(Expression::alloc_data(ExpEnum::String(
                        new_str.into(),
                        None,
                    )));
                }
            }
        }
    }
    Err(LispError::new("str-replace takes three forms"))
}

fn builtin_str_split(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(pat) = args.next() {
        if let Some(text) = args.next() {
            if args.next().is_none() {
                let pat = eval(environment, pat)?;
                let pat = as_string(environment, &pat)?;
                let text = eval(environment, text)?;
                let text = as_string(environment, &text)?;
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
                return Ok(Expression::with_list(split_list));
            }
        }
    }
    Err(LispError::new("str-split takes two forms"))
}

fn builtin_str_rsplit(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(pat) = args.next() {
        if let Some(text) = args.next() {
            if args.next().is_none() {
                let pat = eval(environment, pat)?;
                let pat = as_string(environment, &pat)?;
                let text = eval(environment, text)?;
                let text = as_string(environment, &text)?;
                let mut split_list: Vec<Expression> = Vec::new();
                for s in text.rsplit(&pat) {
                    split_list.push(Expression::alloc_data(ExpEnum::String(
                        s.to_string().into(),
                        None,
                    )));
                }
                return Ok(Expression::with_list(split_list));
            }
        }
    }
    Err(LispError::new("str-rsplit takes two forms"))
}

fn builtin_str_splitn(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(n) = args.next() {
        if let Some(pat) = args.next() {
            if let Some(text) = args.next() {
                if args.next().is_none() {
                    let n = if let ExpEnum::Int(n) = eval(environment, n)?.get().data {
                        if n < 0 {
                            return Err(LispError::new(
                                "str-splitn first form must be a positive integer",
                            ));
                        }
                        n
                    } else {
                        return Err(LispError::new("str-splitn first form must be an integer"));
                    };
                    let pat = eval(environment, pat)?;
                    let pat = as_string(environment, &pat)?;
                    let text = eval(environment, text)?;
                    let text = as_string(environment, &text)?;
                    let mut split_list: Vec<Expression> = Vec::new();
                    for s in text.splitn(n as usize, &pat) {
                        split_list.push(Expression::alloc_data(ExpEnum::String(
                            s.to_string().into(),
                            None,
                        )));
                    }
                    return Ok(Expression::with_list(split_list));
                }
            }
        }
    }
    Err(LispError::new("str-splitn takes three forms"))
}

fn builtin_str_rsplitn(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(n) = args.next() {
        if let Some(pat) = args.next() {
            if let Some(text) = args.next() {
                if args.next().is_none() {
                    let n = if let ExpEnum::Int(n) = eval(environment, n)?.get().data {
                        if n < 0 {
                            return Err(LispError::new(
                                "str-splitn first form must be a positive integer",
                            ));
                        }
                        n
                    } else {
                        return Err(LispError::new("str-rsplitn first form must be an integer"));
                    };
                    let pat = eval(environment, pat)?;
                    let pat = as_string(environment, &pat)?;
                    let text = eval(environment, text)?;
                    let text = as_string(environment, &text)?;
                    let mut split_list: Vec<Expression> = Vec::new();
                    for s in text.rsplitn(n as usize, &pat) {
                        split_list.push(Expression::alloc_data(ExpEnum::String(
                            s.to_string().into(),
                            None,
                        )));
                    }
                    return Ok(Expression::with_list(split_list));
                }
            }
        }
    }
    Err(LispError::new("str-rsplitn takes three forms"))
}

fn builtin_str_cat_list(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(join_str) = args.next() {
        let join_str = eval(environment, join_str)?;
        let join_str = as_string(environment, &join_str)?;
        if let Some(list) = args.next() {
            if args.next().is_none() {
                let mut new_str = String::new();
                let list = eval(environment, list)?;
                match &list.get().data {
                    ExpEnum::Vector(list) => {
                        let mut first = true;
                        for s in list {
                            if !first {
                                new_str.push_str(&join_str);
                            }
                            new_str.push_str(&as_string(environment, &s)?);
                            first = false;
                        }
                    }
                    ExpEnum::Pair(_, _) => {
                        // Includes nil
                        let list = list.iter();
                        let mut first = true;
                        for s in list {
                            if !first {
                                new_str.push_str(&join_str);
                            }
                            new_str.push_str(&as_string(environment, &s)?);
                            first = false;
                        }
                    }
                    _ => {
                        return Err(LispError::new("str-cat-list second form must be a list"));
                    }
                }
                return Ok(Expression::alloc_data(ExpEnum::String(
                    new_str.into(),
                    None,
                )));
            }
        }
    }
    Err(LispError::new("str-cat-list takes two forms"))
}

fn builtin_str_sub(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(string) = args.next() {
        if let Some(start) = args.next() {
            let length = args.next();
            if args.next().is_none() {
                let string = eval(environment, string)?;
                let start = eval(environment, start)?;
                let length = if let Some(length) = length {
                    Some(eval(environment, length)?)
                } else {
                    None
                };
                let start = if let ExpEnum::Int(i) = start.get().data {
                    i as usize
                } else {
                    return Err(LispError::new("str-sub second form (start) must be an int"));
                };
                let len = if let Some(length) = length {
                    if let ExpEnum::Int(i) = length.get().data {
                        i as usize
                    } else {
                        return Err(LispError::new(
                            "str-sub third form (length) must be an int if provided",
                        ));
                    }
                } else {
                    0
                };
                let string_d = string.get();
                if let ExpEnum::String(s, _) = &string_d.data {
                    if (start + len) <= s.len() {
                        if len > 0 {
                            return Ok(Expression::alloc_data(ExpEnum::String(
                                s[start..(start + len)].to_string().into(),
                                None,
                            )));
                        } else {
                            return Ok(Expression::alloc_data(ExpEnum::String(
                                s[start..].to_string().into(),
                                None,
                            )));
                        }
                    } else {
                        return Err(LispError::new("str-sub index out of range"));
                    }
                } else {
                    return Err(LispError::new("str-sub first form must be an String"));
                }
            }
        }
    }
    Err(LispError::new(
        "str-sub takes three forms (String start-index [length])",
    ))
}

fn builtin_str_append(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(start) = args.next() {
        if let Some(end) = args.next() {
            if args.next().is_none() {
                let start = eval(environment, start)?;
                let end = eval(environment, end)?;
                let end_d = end.get();
                if let ExpEnum::String(end, _) = &end_d.data {
                    if let ExpEnum::String(start, _) = &start.get().data {
                        let mut new_string = String::with_capacity(start.len() + end.len());
                        new_string.push_str(&start);
                        new_string.push_str(&end);
                        return Ok(Expression::alloc_data(ExpEnum::String(
                            new_string.into(),
                            None,
                        )));
                    } else {
                        return Err(LispError::new("str-append forms must both be strings"));
                    }
                } else {
                    return Err(LispError::new("str-append forms must both be strings"));
                }
            }
        }
    }
    Err(LispError::new("str-append takes two strings"))
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

fn builtin_str_empty(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(string) = args.next() {
        if args.next().is_none() {
            let empty = match &eval(environment, string)?.get().data {
                ExpEnum::String(string, _) => string.is_empty(),
                _ => true,
            };
            return if empty {
                Ok(Expression::alloc_data(ExpEnum::True))
            } else {
                Ok(Expression::make_nil())
            };
        }
    }
    Err(LispError::new("str-empty? takes a string"))
}

fn builtin_str_nth(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(idx) = args.next() {
        if let Some(string) = args.next() {
            if args.next().is_none() {
                if let ExpEnum::Int(idx) = eval(environment, idx)?.get().data {
                    if let ExpEnum::String(string, _) = &eval(environment, string)?.get().data {
                        for (i, ch) in
                            UnicodeSegmentation::graphemes(string.as_ref(), true).enumerate()
                        {
                            if i as i64 == idx {
                                return Ok(Expression::alloc_data(ExpEnum::Char(
                                    ch.to_string().into(),
                                )));
                            }
                        }
                    } else {
                        return Err(LispError::new("str-nth second argument not a string"));
                    }
                    return Err(LispError::new("str-nth index out of range"));
                }
            }
        }
    }
    Err(LispError::new("str-nth takes two forms (int and string)"))
}

fn builtin_str_lower(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(string) = args.next() {
        if args.next().is_none() {
            if let ExpEnum::String(string, _) = &eval(environment, string)?.get().data {
                return Ok(Expression::alloc_data(ExpEnum::String(
                    string.to_ascii_lowercase().into(),
                    None,
                )));
            }
        }
    }
    Err(LispError::new("str-lower takes a string"))
}

fn builtin_str_upper(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(string) = args.next() {
        if args.next().is_none() {
            if let ExpEnum::String(string, _) = &eval(environment, string)?.get().data {
                return Ok(Expression::alloc_data(ExpEnum::String(
                    string.to_ascii_uppercase().into(),
                    None,
                )));
            }
        }
    }
    Err(LispError::new("str-upper takes a string"))
}

fn builtin_str_bytes(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            if let ExpEnum::String(string, _) = &eval(environment, arg)?.get().data {
                return Ok(Expression::alloc_data(ExpEnum::Int(string.len() as i64)));
            };
        }
    }
    Err(LispError::new("str-bytes takes a string"))
}

fn builtin_str_starts_with(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(pat) = args.next() {
        if let Some(text) = args.next() {
            if args.next().is_none() {
                let pat = eval(environment, pat)?;
                let pat = as_string(environment, &pat)?;
                let text = eval(environment, text)?;
                let text = as_string(environment, &text)?;
                return if text.starts_with(&pat) {
                    Ok(Expression::alloc_data(ExpEnum::True))
                } else {
                    Ok(Expression::make_nil())
                };
            }
        }
    }
    Err(LispError::new(
        "str-start-with takes two forms (pattern string)",
    ))
}

fn builtin_str_contains(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(pat) = args.next() {
        if let Some(text) = args.next() {
            if args.next().is_none() {
                let pat = eval(environment, pat)?;
                let pat = as_string(environment, &pat)?;
                let text = eval(environment, text)?;
                let text = as_string(environment, &text)?;
                return if text.contains(&pat) {
                    Ok(Expression::alloc_data(ExpEnum::True))
                } else {
                    Ok(Expression::make_nil())
                };
            }
        }
    }
    Err(LispError::new(
        "str-contains takes two forms (pattern string)",
    ))
}

fn builtin_str_push(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg0) = args.next() {
        let s = eval(environment, arg0)?;
        let mut evalled_args = Vec::new();
        if let ExpEnum::String(_, _) = &s.get().data {
            // Do this otherwise the get_mut on s can lead to hard to track panics in some code.
            for a in args {
                evalled_args.push(eval(environment, a)?);
            }
        }
        let mut s_d = s.get_mut();
        if let ExpEnum::String(res, chars) = &mut s_d.data {
            for a in evalled_args {
                res.to_mut().push_str(&as_string(environment, &a)?);
            }
            *chars = None; // maintian the iterator invariant.
            drop(s_d);
            Ok(s)
        } else {
            Err(LispError::new(
                "str-push! takes a string buffer as first form",
            ))
        }
    } else {
        Err(LispError::new("str-push! takes at least one form"))
    }
}

fn builtin_str_clear(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            let s = eval(environment, arg0)?;
            let mut s_d = s.get_mut();
            if let ExpEnum::String(res, chars) = &mut s_d.data {
                *chars = None; // maintian the iterator invariant.
                res.to_mut().clear();
                drop(s_d);
                Ok(s)
            } else {
                Err(LispError::new(
                    "str-clear! takes a string buffer as first form",
                ))
            }
        } else {
            Err(LispError::new("str-clear! takes only one form"))
        }
    } else {
        Err(LispError::new("str-clear! takes one form"))
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
                            str_map_inner(environment, func, &string)?.into(),
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
                        .map(|s| Cow::Borrowed(s))
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
        for _s in <str as UnicodeSegmentation>::graphemes(s.as_ref(), true) {
            count = count + 1;
        }
        match count {
            0 => Ok(Expression::alloc_data(ExpEnum::Int(0))),
            1 => {
                let mut int_val: u32 = 0;
                let mut overflow = false;
                for c in s.chars() {
                    let (add, overflowed) = int_val.overflowing_add(c as u32);
                    if overflowed {
                        overflow = true;
                        break;
                    }
                    int_val = int_val + add;
                }
                if overflow {
                    Err(LispError::new("error, overflow occurred adding unicode scalar values that compose provided grapheme interpreted as unsigned 32 byte integers: {:?}."))
                } else {
                    Ok(Expression::alloc_data(ExpEnum::Int(int_val as i64)))
                }
            }
            _ => Err(LispError::new(
                "function takes one grapheme, multiple were provided.",
            )),
        }
    };
    match &next_arg_d.data {
        ExpEnum::String(s, _) => {
            to_int(s)
        },
        ExpEnum::Char(s) => {
            to_int(s)
        },
        _ => Err(LispError::new("expected one argument of type Char or String")),
    }
}

fn builtin_unicode_scalars(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let next_arg = param_eval(environment, args, "unicode-scalars")?;
    let next_arg_d = next_arg.get();
    params_done(args, "unicode-scalars")?;
    let with_str = |s: &str| {
        let mut unicode_scalars: Vec<String> = Vec::new();
        for c in s.chars() {
            unicode_scalars.push(format!("{}", c.escape_unicode()));
        }

        let mut strings = Vec::with_capacity(unicode_scalars.len());
        for s in unicode_scalars {
            strings.push(Expression::alloc_data(ExpEnum::String(
                s.to_string().into(),
                None,
            )));
        }
        Ok(Expression::alloc_data(ExpEnum::Vector(strings)))
    };
    match &next_arg_d.data {
        ExpEnum::String(s, _) => with_str(s),
        ExpEnum::Char(s) => with_str(s),
        _ => Err(LispError::new("expected one argument of type Char or String")),
    }
}

fn builtin_str_hash(
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
        _ => Err(LispError::new("expected one argument of type Char or String")),
    }
}

pub fn add_str_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("str-trim"),
        Expression::make_function(
            builtin_str_trim,
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
        ),
    );
    data.insert(
        interner.intern("str-ltrim"),
        Expression::make_function(
            builtin_str_ltrim,
            r#"Usage: (str-ltrim string) -> string

Trim left whitspace from string.

Section: string

Example:
(test::assert-equal "some string" (str-ltrim "   some string"))
(test::assert-equal "some string   " (str-ltrim "   some string   "))
(test::assert-equal "some string   " (str-ltrim (str "   some string   ")))
(test::assert-equal "some string   " (str-ltrim "some string   "))
(test::assert-equal "some string" (str-ltrim "some string"))
"#,
        ),
    );
    data.insert(
        interner.intern("str-rtrim"),
        Expression::make_function(
            builtin_str_rtrim,
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
        ),
    );
    data.insert(
        interner.intern("str-replace"),
        Expression::make_function(
            builtin_str_replace,
            r#"Usage: (str-replace string old-pattern new-pattern) -> string

Replace occurances of second string with third in the first string.

Section: string

Example:
(test::assert-equal "some yyy string" (str-replace "some xxx string" "xxx" "yyy"))
(test::assert-equal "some yyy string yyy" (str-replace "some xxx string xxx" "xxx" "yyy"))
(test::assert-equal "yyy some yyy string yyy" (str-replace "xxx some xxx string xxx" "xxx" "yyy"))
"#,
        ),
    );
    data.insert(
        interner.intern("str-split"),
        Expression::make_function(
            builtin_str_split,
            r#"Usage: (str-split split-pattern string) -> vector

Use a pattern to split a string (:whitespace to split on whitespace).

Section: string

Example:
(test::assert-equal '("some" "yyy" "string") (str-split "xxx" "somexxxyyyxxxstring"))
(test::assert-equal '("some" "yyy" "string" "") (str-split "xxx" "somexxxyyyxxxstringxxx"))
(test::assert-equal '("" "some" "yyy" "string" "") (str-split "xxx" "xxxsomexxxyyyxxxstringxxx"))
(test::assert-equal '("some" "yyy" "string") (str-split :whitespace "some yyy string"))
(test::assert-equal '("somexxxyyyxxxstring") (str-split :whitespace "somexxxyyyxxxstring"))
(test::assert-equal '("somexxxyyyxxxstring") (str-split "zzz" "somexxxyyyxxxstring"))
"#,
        ),
    );
    data.insert(
        interner.intern("str-rsplit"),
        Expression::make_function(
            builtin_str_rsplit,
            "Usage: (str-rsplit split-pattern string) -> vector
 
Use a pattern to split a string into reverse order.

Section: string

Example:
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-rsplit \"xxx\" \"stringxxxyyyxxxsome\"))
(test::assert-equal '(\"\" \"some\" \"yyy\" \"string\") (str-rsplit \"xxx\" \"stringxxxyyyxxxsomexxx\"))
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-rsplit \" \" \"string yyy some\"))
(test::assert-equal '(\"somexxxyyyxxxstring\") (str-rsplit :whitespace \"somexxxyyyxxxstring\"))
(test::assert-equal '(\"somexxxyyyxxxstring\") (str-rsplit \"zzz\" \"somexxxyyyxxxstring\"))
"
        ),
    );
    data.insert(
        interner.intern("str-splitn"),
        Expression::make_function(
            builtin_str_splitn,
            r#"Usage: (str-splitn n split-pattern string) -> vector

Use a pattern to split a string with at most n items.

Section: string

Example:
(test::assert-equal '("some" "yyy" "string") (str-splitn 3 "xxx" "somexxxyyyxxxstring"))
(test::assert-equal '("some" "yyy" "string") (str-splitn 4 "xxx" "somexxxyyyxxxstring"))
(test::assert-equal '("some" "yyy" "stringxxxother") (str-splitn 3 "xxx" "somexxxyyyxxxstringxxxother"))
(test::assert-equal '("somexxxyyyxxxstringxxxother") (str-splitn 1 "xxx" "somexxxyyyxxxstringxxxother"))
(test::assert-equal '() (str-splitn 0 "xxx" "somexxxyyyxxxstringxxxzero"))
"#
        ),
    );
    data.insert(
        interner.intern("str-rsplitn"),
        Expression::make_function(
            builtin_str_rsplitn,
            r#"Usage: (str-rsplitn n split-pattern string) -> vector

Use a pattern to split a string with at most n items returned in reverse order.

Section: string

Example:
(test::assert-equal '("some" "yyy" "string") (str-rsplitn 3 "xxx" "stringxxxyyyxxxsome"))
(test::assert-equal '("some" "yyy" "string") (str-rsplitn 4 "xxx" "stringxxxyyyxxxsome"))
(test::assert-equal '("other" "string" "somexxxyyy") (str-rsplitn 3 "xxx" "somexxxyyyxxxstringxxxother"))
(test::assert-equal '("somexxxyyyxxxstringxxxother") (str-rsplitn 1 "xxx" "somexxxyyyxxxstringxxxother"))
(test::assert-equal '() (str-rsplitn 0 "xxx" "somexxxyyyxxxstringxxxzero"))
"#
        ),
    );
    data.insert(
        interner.intern("str-cat-list"),
        Expression::make_function(
            builtin_str_cat_list,
            r#"Usage: (str-cat-list join-pattern sequence) -> string

Build a string by concatting a sequence with a join string.

Section: string

Example:
(test::assert-equal "stringxxxyyyxxxsome" (str-cat-list "xxx" '("string" "yyy" "some")))
(test::assert-equal "string yyy some" (str-cat-list " " '("string" "yyy" "some")))
(test::assert-equal "stringyyysome" (str-cat-list "" '("string" "yyy" "some")))
"#,
        ),
    );
    data.insert(
        interner.intern("str-sub"),
        Expression::make_function(
            builtin_str_sub,
            r#"Usage: (str-sub string start [length]) -> string

Return a substring from a string given start (0 based) and optional length.
If length is 0 or not provided produces the rest of the string from start to
string end.

Section: string

Example:
(test::assert-equal "string" (str-sub "stringxxxyyyxxxsome" 0 6))
(test::assert-equal "some" (str-sub "stringxxxyyyxxxsome" 15 4))
(test::assert-equal "yyy" (str-sub "stringxxxyyyxxxsome" 9 3))
(test::assert-equal "some" (str-sub "stringxxxyyyxxxsome" 15))
"#,
        ),
    );
    data.insert(
        interner.intern("str-append"),
        Expression::make_function(
            builtin_str_append,
            r#"Usage: (str-append string string) -> string

Make a new string by appending two strings.

Section: string

Example:
(test::assert-equal "stringsome" (str-append "string" "some"))
(test::assert-equal "string" (str-append "string" ""))
(test::assert-equal "string " (str-append "string" " "))
"#,
        ),
    );
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
    data.insert(
        interner.intern("str-empty?"),
        Expression::make_function(
            builtin_str_empty,
            r#"Usage: (str-empty? string) -> t/nil

Is a string empty?  Let's find out...

Section: string

Example:
(test::assert-true (str-empty? ""))
(test::assert-true (str-empty? (str-trim "   ")))
(test::assert-false (str-empty? " "))
(test::assert-false (str-empty? "string"))
"#,
        ),
    );
    data.insert(
        interner.intern("str-nth"),
        Expression::make_function(
            builtin_str_nth,
            r#"Usage: (str-nth n string) -> char

Get the nth char of a string.

Section: string

Example:
(test::assert-equal #\a (str-nth 2 "stau"))
(test::assert-equal #\s (str-nth 0 "stau"))
(test::assert-equal #\u (str-nth 3 "stau"))
"#,
        ),
    );
    data.insert(
        interner.intern("str-lower"),
        Expression::make_function(
            builtin_str_lower,
            r#"Usage: (str-lower string) -> string

Get all lower case string from a string.

Section: string

Example:
(test::assert-equal "stau" (str-lower "STAU"))
(test::assert-equal "stau" (str-lower "stau"))
(test::assert-equal "stau" (str-lower "Stau"))
(test::assert-equal "stau" (str-lower "StaU"))
(test::assert-equal "stau" (str-lower "sTaU"))
"#,
        ),
    );
    data.insert(
        interner.intern("str-upper"),
        Expression::make_function(
            builtin_str_upper,
            r#"Usage: (str-upper string) -> string

Get all upper case string from a string.

Section: string

Example:
(test::assert-equal "STAU" (str-upper "STAU"))
(test::assert-equal "STAU" (str-upper "stau"))
(test::assert-equal "STAU" (str-upper "Stau"))
(test::assert-equal "STAU" (str-upper "StaU"))
(test::assert-equal "STAU" (str-upper "sTaU"))
"#,
        ),
    );
    data.insert(
        interner.intern("str-bytes"),
        Expression::make_function(
            builtin_str_bytes,
            r#"Usage: (str-bytes string) -> int

Return number of bytes in a string (may be more then length).

Strings are utf8 so it chars and bytes may not be a one to one match.

Section: string

Example:
(test::assert-equal 4 (str-bytes "Stau"))
(test::assert-equal 0 (str-bytes ""))
; Note 5 chars and 6 bytes because of the final char.
(test::assert-equal 6 (str-bytes "StauΣ"))
"#,
        ),
    );
    data.insert(
        interner.intern("str-starts-with"),
        Expression::make_function(
            builtin_str_starts_with,
            r#"Usage: (str-starts-with pattern string) -> t/nil

True if string start with pattern (both strings).

Section: string

Example:
(test::assert-true (str-starts-with "Stau" "Stausomething"))
(test::assert-false (str-starts-with "StaU" "Stausomething"))
"#,
        ),
    );
    data.insert(
        interner.intern("str-contains"),
        Expression::make_function(
            builtin_str_contains,
            r#"Usage: (str-contains pattern string) -> t/nil

True if string contains pattern (both strings).

Section: string

Example:
(test::assert-true (str-contains "Stau" "Stausomething"))
(test::assert-false (str-contains "StaU" "Stausomething"))
(test::assert-true (str-contains "some" "Stausomething"))
(test::assert-false (str-contains "Some" "Stausomething"))
(test::assert-true (str-contains "thing" "Stausomething"))
(test::assert-false (str-contains "Thing" "Stausomething"))
(test::assert-true (str-contains "someΣ" "StausomeΣthing"))
"#,
        ),
    );
    data.insert(
        interner.intern("str-push!"),
        Expression::make_function(
            builtin_str_push,
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
        ),
    );
    data.insert(
        interner.intern("str-clear!"),
        Expression::make_function(
            builtin_str_clear,
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
        ),
    );
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
(see (doc 'unicode-scalars) for more information) and returns a sum of the values.
This is not a hashing function, and only accepts one grapheme in the form of a
string or character. Graphemes composed of the same unicode scalar values will
result in the same integer value.'

Section: string

Example:
(test::assert-error-msg (unicode-scalars (make-vec)) \"expected one argument of type Char or String\")
(test::assert-error-msg (char->int) \"char->int: Missing required argument, see (doc 'char->int) for usage.\")
(test::assert-error-msg (char->int \"a\" \"b\") \"char->int: Too many arguments, see (doc 'char->int) for usage.\")
(test::assert-error-msg (char->int \"ab\") \"function takes one grapheme, multiple were provided.\")
(test::assert-error-msg (char->int \"λ⚙\") \"function takes one grapheme, multiple were provided.\")
(test::assert-equal 97 (char->int \"a\"))
(test::assert-equal 97 (char->int #\\a))
(test::assert-equal 7101 (char->int #\\स्))
(test::assert-equal 9881 (char->int (str \"\\\" \"u{2699}\")))
",
        ),
    );

    data.insert(
        interner.intern("unicode-scalars"),
        Expression::make_function(
            builtin_unicode_scalars,
            "Usage: (unicode-scalars string)

Returns array of unicode scalars for each char in string. Note, a char
is not a grapheme. The hindi word namaste (\"न\" \"म\" \"स्\" \"ते\")
written in Devanagari script is 4 graphemes, but 6 unicode scalar values,
(\"u{928}\" \"u{92e}\" \"u{938}\" \"u{94d}\" \"u{924}\" \"u{947}\");
[reference](https://doc.rust-lang.org/book/ch08-02-strings.html#bytes-and-scalar-values-and-grapheme-clusters-oh-my).

Section: string

Example:
(test::assert-error-msg (unicode-scalars) \"unicode-scalars: Missing required argument, see (doc 'unicode-scalars) for usage.\")
(test::assert-error-msg (unicode-scalars (make-vec)) \"expected one argument of type Char or String\")
(test::assert-error-msg (unicode-scalars \"a\" \"b\") \"unicode-scalars: Too many arguments, see (doc 'unicode-scalars) for usage.\")
(test::assert-equal (vec (str \"\\\" \"u{2699}\")) (unicode-scalars \"⚙\"))
(test::assert-equal (vec (str \"\\\" \"u{938}\") ((str \"\\\" \"u{94d}\"))) (unicode-scalars \"स्\"))
(test::assert-equal (vec (str \"\\\" \"u{938}\") ((str \"\\\" \"u{94d}\"))) (unicode-scalars #\\स्))
(test::assert-equal (vec (str \"\\\" \"u{61}\")) (unicode-scalars \"a\"))
(test::assert-equal (vec (str \"\\\" \"u{61}\")) (unicode-scalars #\\a))
",
        ),
    );

    data.insert(
        interner.intern("str-hash"),
        Expression::make_function(
            builtin_str_hash,
            "Usage: (str-hash s)

Accepts values of type String or Char and returns hash of value. Hash is not cryptographically secure.

Section: string

Example:
(test::assert-error-msg (str-hash) \"str-hash: Missing required argument, see (doc 'str-hash) for usage.\")
(test::assert-error-msg (str-hash (make-vec)) \"expected one argument of type Char or String\")
(test::assert-error-msg (str-hash \"a\" \"b\") \"str-hash: Too many arguments, see (doc 'str-hash) for usage.\")
(test::assert-equal (vec (str \"\\\" \"u{2699}\")) (unicode-scalars \"⚙\"))
(test::assert-equal 8186225505942432000 (str-hash \"a\"))
(test::assert-equal 8186225505942432000 (str-hash #\\a))
",
        ),
    );
}
