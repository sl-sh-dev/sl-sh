use std::borrow::Cow;
use std::collections::HashMap;
use std::hash::BuildHasher;

use unicode_segmentation::UnicodeSegmentation;

use crate::environment::*;
use crate::eval::*;
use crate::interner::*;
use crate::types::*;

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
    if let Some(arg0) = args.next() {
        if let Some(arg1) = args.next() {
            if let Some(arg2) = args.next() {
                if args.next().is_none() {
                    let arg0 = eval(environment, arg0)?;
                    let arg1 = eval(environment, arg1)?;
                    let arg2 = eval(environment, arg2)?;
                    let start = if let ExpEnum::Int(i) = arg0.get().data {
                        i as usize
                    } else {
                        return Err(LispError::new("str-sub first form must be an int"));
                    };
                    let len = if let ExpEnum::Int(i) = arg1.get().data {
                        i as usize
                    } else {
                        return Err(LispError::new("str-sub second form must be an int"));
                    };
                    let arg2_d = arg2.get();
                    if let ExpEnum::String(s, _) = &arg2_d.data {
                        if (start + len) <= s.len() {
                            return Ok(Expression::alloc_data(ExpEnum::String(
                                s[start..(start + len)].to_string().into(),
                                None,
                            )));
                        } else {
                            return Err(LispError::new("str-sub index out of range"));
                        }
                    } else {
                        return Err(LispError::new("str-sub third form must be an String"));
                    }
                }
            }
        }
    }
    Err(LispError::new(
        "str-sub takes three forms (int, int String)",
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
    let old_out = environment.state.stdout_status.clone();
    let old_err = environment.state.stderr_status.clone();
    environment.state.stdout_status = Some(IOState::Pipe);
    environment.state.stderr_status = Some(IOState::Pipe);

    // Get out of a pipe for the str call if in one...
    let data_in = environment.data_in.clone();
    environment.data_in = None;
    let in_pipe = environment.in_pipe;
    environment.in_pipe = false;
    let pipe_pgid = environment.state.pipe_pgid;
    environment.state.pipe_pgid = None;

    // Do not use ?, make sure to reset environment state even on error.
    let mut res = String::new();
    for a in args {
        match eval(environment, a) {
            Err(err) => {
                environment.state.stdout_status = old_out;
                environment.state.stderr_status = old_err;
                return Err(err);
            }
            Ok(a) => {
                match as_string(environment, &a) {
                    Err(err) => {
                        environment.state.stdout_status = old_out;
                        environment.state.stderr_status = old_err;
                        return Err(err);
                    }
                    Ok(s) => res.push_str(&s),
                };
            }
        }
    }
    environment.state.stdout_status = old_out;
    environment.state.stderr_status = old_err;
    environment.data_in = data_in;
    environment.in_pipe = in_pipe;
    environment.state.pipe_pgid = pipe_pgid;
    Ok(Expression::alloc_data(ExpEnum::String(res.into(), None)))
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
                        //for (i, ch) in string.chars().enumerate() {
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
        let mut list = Vec::with_capacity(2);
        list.push(Expression::alloc_data(ExpEnum::Lambda(func.clone())));
        list.push(Expression::alloc_data(ExpEnum::Char(ch.to_string().into())));
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
    Err(LispError::new("str-iter takes a string"))
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
                            if let Some(reader_state) = &mut environment.reader_state {
                                if ch == "\n" {
                                    reader_state.line += 1;
                                    reader_state.column = 0;
                                } else {
                                    reader_state.column += 1;
                                }
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

pub fn builtin_str_ignore_expand(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let save_ignore = environment.str_ignore_expand;
    environment.str_ignore_expand = true;
    let mut ret = Ok(Expression::make_nil());
    for arg in args {
        ret = eval(environment, arg);
        if ret.is_err() {
            break;
        }
    }
    environment.str_ignore_expand = save_ignore;
    ret
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

pub fn add_str_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("str-trim"),
        Expression::make_function(
            builtin_str_trim,
            "Usage: (str-trim string) -> string
 
Trim right and left whitespace from string.

Section: string

Example:
(test::assert-equal \"some string\" (str-trim \"   some string\"))
(test::assert-equal \"some string\" (str-trim \"   some string   \"))
(test::assert-equal \"some string\" (str-trim (str \"   some string   \")))
(test::assert-equal \"some string\" (str-trim \"some string   \"))
(test::assert-equal \"some string\" (str-trim \"some string\"))
",
        ),
    );
    data.insert(
        interner.intern("str-ltrim"),
        Expression::make_function(
            builtin_str_ltrim,
            "Usage: (str-ltrim string) -> string
 
Trim left whitspace from string.

Section: string

Example:
(test::assert-equal \"some string\" (str-ltrim \"   some string\"))
(test::assert-equal \"some string   \" (str-ltrim \"   some string   \"))
(test::assert-equal \"some string   \" (str-ltrim (str \"   some string   \")))
(test::assert-equal \"some string   \" (str-ltrim \"some string   \"))
(test::assert-equal \"some string\" (str-ltrim \"some string\"))
",
        ),
    );
    data.insert(
        interner.intern("str-rtrim"),
        Expression::make_function(
            builtin_str_rtrim,
            "Usage: (str-rtrim string) -> string
 
Trim right whitespace from string.

Section: string

Example:
(test::assert-equal \"   some string\" (str-rtrim \"   some string\"))
(test::assert-equal \"   some string\" (str-rtrim \"   some string   \"))
(test::assert-equal \"   some string\" (str-rtrim (str \"   some string   \")))
(test::assert-equal \"some string\" (str-rtrim \"some string   \"))
(test::assert-equal \"some string\" (str-rtrim \"some string\"))
",
        ),
    );
    data.insert(
        interner.intern("str-replace"),
        Expression::make_function(
            builtin_str_replace,
            "Usage: (str-replace string old-pattern new-pattern) -> string
 
Replace occurances of second string with third in the first string.

Section: string

Example:
(test::assert-equal \"some yyy string\" (str-replace \"some xxx string\" \"xxx\" \"yyy\"))
(test::assert-equal \"some yyy string yyy\" (str-replace \"some xxx string xxx\" \"xxx\" \"yyy\"))
(test::assert-equal \"yyy some yyy string yyy\" (str-replace \"xxx some xxx string xxx\" \"xxx\" \"yyy\"))
"
        ),
    );
    data.insert(
        interner.intern("str-split"),
        Expression::make_function(
            builtin_str_split,
            "Usage: (str-split split-pattern string) -> vector
 
Use a pattern to split a string (:whitespace to split on whitespace).

Section: string

Example:
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-split \"xxx\" \"somexxxyyyxxxstring\"))
(test::assert-equal '(\"some\" \"yyy\" \"string\" \"\") (str-split \"xxx\" \"somexxxyyyxxxstringxxx\"))
(test::assert-equal '(\"\" \"some\" \"yyy\" \"string\" \"\") (str-split \"xxx\" \"xxxsomexxxyyyxxxstringxxx\"))
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-split :whitespace \"some yyy string\"))
(test::assert-equal '(\"somexxxyyyxxxstring\") (str-split :whitespace \"somexxxyyyxxxstring\"))
(test::assert-equal '(\"somexxxyyyxxxstring\") (str-split \"zzz\" \"somexxxyyyxxxstring\"))
"
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
            "Usage: (str-splitn n split-pattern string) -> vector
 
Use a pattern to split a string with at most n items.

Section: string

Example:
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-splitn 3 \"xxx\" \"somexxxyyyxxxstring\"))
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-splitn 4 \"xxx\" \"somexxxyyyxxxstring\"))
(test::assert-equal '(\"some\" \"yyy\" \"stringxxxother\") (str-splitn 3 \"xxx\" \"somexxxyyyxxxstringxxxother\"))
(test::assert-equal '(\"somexxxyyyxxxstringxxxother\") (str-splitn 1 \"xxx\" \"somexxxyyyxxxstringxxxother\"))
(test::assert-equal '() (str-splitn 0 \"xxx\" \"somexxxyyyxxxstringxxxzero\"))
"
        ),
    );
    data.insert(
        interner.intern("str-rsplitn"),
        Expression::make_function(
            builtin_str_rsplitn,
            "Usage: (str-rsplitn n split-pattern string) -> vector
 
Use a pattern to split a string with at most n items returned in reverse order.

Section: string

Example:
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-rsplitn 3 \"xxx\" \"stringxxxyyyxxxsome\"))
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-rsplitn 4 \"xxx\" \"stringxxxyyyxxxsome\"))
(test::assert-equal '(\"other\" \"string\" \"somexxxyyy\") (str-rsplitn 3 \"xxx\" \"somexxxyyyxxxstringxxxother\"))
(test::assert-equal '(\"somexxxyyyxxxstringxxxother\") (str-rsplitn 1 \"xxx\" \"somexxxyyyxxxstringxxxother\"))
(test::assert-equal '() (str-rsplitn 0 \"xxx\" \"somexxxyyyxxxstringxxxzero\"))
"
        ),
    );
    data.insert(
        interner.intern("str-cat-list"),
        Expression::make_function(
            builtin_str_cat_list,
            "Usage: (str-cat-list join-pattern sequence) -> string

Build a string by concatting a sequence with a join string.

Section: string

Example:
(test::assert-equal \"stringxxxyyyxxxsome\" (str-cat-list \"xxx\" '(\"string\" \"yyy\" \"some\")))
(test::assert-equal \"string yyy some\" (str-cat-list \" \" '(\"string\" \"yyy\" \"some\")))
(test::assert-equal \"stringyyysome\" (str-cat-list \"\" '(\"string\" \"yyy\" \"some\")))
",
        ),
    );
    data.insert(
        interner.intern("str-sub"),
        Expression::make_function(
            builtin_str_sub,
            "Usage: (str-sub start length string) -> string

Return a substring from a string given start (0 based) and length.

Section: string

Example:
(test::assert-equal \"string\" (str-sub 0 6 \"stringxxxyyyxxxsome\"))
(test::assert-equal \"some\" (str-sub 15 4 \"stringxxxyyyxxxsome\"))
(test::assert-equal \"yyy\" (str-sub 9 3 \"stringxxxyyyxxxsome\"))
",
        ),
    );
    data.insert(
        interner.intern("str-append"),
        Expression::make_function(
            builtin_str_append,
            "Usage: (str-append string string) -> string

Make a new string by appending two strings.

Section: string

Example:
(test::assert-equal \"stringsome\" (str-append \"string\" \"some\"))
(test::assert-equal \"string\" (str-append \"string\" \"\"))
(test::assert-equal \"string \" (str-append \"string\" \" \"))
",
        ),
    );
    data.insert(
        interner.intern("str"),
        Expression::make_function(
            builtin_str,
            "Usage: (str arg0 ... argN) -> string

Make a new string with it's arguments.

Arguments will be turned into strings.  If an argument is a process then the
output of the process will be captured and put into the string.

Section: string

Example:
(test::assert-equal \"stringsome\" (str \"string\" \"some\"))
(test::assert-equal \"string\" (str \"string\" \"\"))
(test::assert-equal \"string 50\" (str \"string\" \" \" 50))
(test::assert-equal \"string 50 test\n\" (str \"string\" \" \" 50 \" \" (echo \"test\")))
",
        ),
    );
    data.insert(
        interner.intern("str-empty?"),
        Expression::make_function(
            builtin_str_empty,
            "Usage: (str-empty? string) -> t/nil

Is a string empty?  Let's find out...

Section: string

Example:
(test::assert-true (str-empty? \"\"))
(test::assert-true (str-empty? (str-trim \"   \")))
(test::assert-false (str-empty? \" \"))
(test::assert-false (str-empty? \"string\"))
",
        ),
    );
    data.insert(
        interner.intern("str-nth"),
        Expression::make_function(
            builtin_str_nth,
            "Usage: (str-nth n string) -> char

Get the nth char of a string.

Section: string

Example:
(test::assert-equal #\\a (str-nth 2 \"stau\"))
(test::assert-equal #\\s (str-nth 0 \"stau\"))
(test::assert-equal #\\u (str-nth 3 \"stau\"))
",
        ),
    );
    data.insert(
        interner.intern("str-lower"),
        Expression::make_function(
            builtin_str_lower,
            "Usage: (str-lower string) -> string

Get all lower case string from a string.

Section: string

Example:
(test::assert-equal \"stau\" (str-lower \"STAU\"))
(test::assert-equal \"stau\" (str-lower \"stau\"))
(test::assert-equal \"stau\" (str-lower \"Stau\"))
(test::assert-equal \"stau\" (str-lower \"StaU\"))
(test::assert-equal \"stau\" (str-lower \"sTaU\"))
",
        ),
    );
    data.insert(
        interner.intern("str-upper"),
        Expression::make_function(
            builtin_str_upper,
            "Usage: (str-upper string) -> string

Get all upper case string from a string.

Section: string

Example:
(test::assert-equal \"STAU\" (str-upper \"STAU\"))
(test::assert-equal \"STAU\" (str-upper \"stau\"))
(test::assert-equal \"STAU\" (str-upper \"Stau\"))
(test::assert-equal \"STAU\" (str-upper \"StaU\"))
(test::assert-equal \"STAU\" (str-upper \"sTaU\"))
",
        ),
    );
    data.insert(
        interner.intern("str-bytes"),
        Expression::make_function(
            builtin_str_bytes,
            "Usage: (str-bytes string) -> int

Return number of bytes in a string (may be more then length).

Strings are utf8 so it chars and bytes may not be a one to one match.

Section: string

Example:
(test::assert-equal 4 (str-bytes \"Stau\"))
(test::assert-equal 0 (str-bytes \"\"))
; Note 5 chars and 6 bytes because of the final char.
(test::assert-equal 6 (str-bytes \"StauΣ\"))
",
        ),
    );
    data.insert(
        interner.intern("str-starts-with"),
        Expression::make_function(
            builtin_str_starts_with,
            "Usage: (str-starts-with pattern string) -> t/nil

True if string start with pattern (both strings).

Section: string

Example:
(test::assert-true (str-starts-with \"Stau\" \"Stausomething\"))
(test::assert-false (str-starts-with \"StaU\" \"Stausomething\"))
",
        ),
    );
    data.insert(
        interner.intern("str-contains"),
        Expression::make_function(
            builtin_str_contains,
            "Usage: (str-contains pattern string) -> t/nil

True if string contains pattern (both strings).

Section: string

Example:
(test::assert-true (str-contains \"Stau\" \"Stausomething\"))
(test::assert-false (str-contains \"StaU\" \"Stausomething\"))
(test::assert-true (str-contains \"some\" \"Stausomething\"))
(test::assert-false (str-contains \"Some\" \"Stausomething\"))
(test::assert-true (str-contains \"thing\" \"Stausomething\"))
(test::assert-false (str-contains \"Thing\" \"Stausomething\"))
(test::assert-true (str-contains \"someΣ\" \"StausomeΣthing\"))
",
        ),
    );
    data.insert(
        interner.intern("str-push!"),
        Expression::make_function(
            builtin_str_push,
            "Usage: (str-push! string arg0 ... argN) -> string

Push the args (as strings) onto the string.  This is a destructive form.

Arguments will be turned into strings.  Returns the string it was given.

Section: string

Example:
(test::assert-equal \"stringsome\" (str-push! (str \"string\") \"some\"))
(def test-str-push (str \"def-string\"))
(test::assert-equal \"def-stringsome\" (str-push! test-str-push \"some\"))
(test::assert-equal \"def-stringsome\" test-str-push)
",
        ),
    );
    data.insert(
        interner.intern("str-clear!"),
        Expression::make_function(
            builtin_str_clear,
            "Usage: (str-clear! string) -> string

Clears a string.  This is a destructive form.

Returns the string it was given.

Section: string

Example:
(test::assert-equal \"\" (str-clear! (str \"string\")))
(def test-str-clear (str \"def-string\"))
(test::assert-equal \"\" (str-clear! test-str-clear))
(test::assert-equal \"\" test-str-clear)
",
        ),
    );
    data.insert(
        interner.intern("str-map"),
        Expression::make_function(
            builtin_str_map,
            "Usage: (str-map lambda string) -> string

Make a new string by applying lambda to each char.

Section: string

Example:
(test::assert-equal \"XstringXstrX\" (str-map (fn (ch) (if (= #\\x ch) #\\X ch)) \"xstringxstrx\"))
(def test-str-map (str-map (fn (ch) (if (= #\\x ch) #\\X ch)) \"xstringxstrx\"))
(test::assert-equal \"XstringXstrX\" test-str-map)
(test::assert-true (string? test-str-map))
(def test-str-map (str-map (fn (ch) (if (= #\\x ch) #\\X ch)) (str \"xstringxstrx\")))
(test::assert-equal \"XstringXstrX\" test-str-map)
(test::assert-true (string? test-str-map))
",
        ),
    );
    data.insert(
        interner.intern("str-iter-start"),
        Expression::make_function(
            builtin_str_iter_start,
            "Usage: (str-iter-start string) -> string

Starts or resets the iterator over a string.  Returns the input string with it's
iteration start created and at the first position.  Using the str-iter-* functions
is the proper way to get the chars of a string (since they are UTF a char is not
a fixed size so direct indexing is very inefficient).

Section: string

Example:
(def test-iter-start \"test\")
(test::assert-true (str-iter-empty? test-iter-start))
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(test::assert-equal #\\t (str-iter-next! test-iter-start))
(test::assert-equal #\\e (str-iter-next! test-iter-start))
(test::assert-equal #\\s (str-iter-next! test-iter-start))
(test::assert-equal #\\t (str-iter-next! test-iter-start))
(test::assert-true (str-iter-empty? test-iter-start))
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(test::assert-equal #\\t (str-iter-next! test-iter-start))
(test::assert-equal #\\e (str-iter-next! test-iter-start))
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(test::assert-equal #\\t (str-iter-next! test-iter-start))
(test::assert-equal #\\e (str-iter-next! test-iter-start))
(test::assert-equal #\\s (str-iter-next! test-iter-start))
(test::assert-equal #\\t (str-iter-next! test-iter-start))
(test::assert-true (str-iter-empty? test-iter-start))
",
        ),
    );
    data.insert(
        interner.intern("str-iter-next!"),
        Expression::make_function(
            builtin_str_iter_next,
            "Usage: (str-iter-next! string) -> char

Returns the next char in the iterator for string.  Returns nil if iteration
is done.

Section: string

Example:
(def test-iter-start \"y̆ΛλΣσ\")
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(def test-iter-one (str-iter-next! test-iter-start))
(test::assert-equal #\\y̆ test-iter-one)
(test::assert-true (= #\\y̆ test-iter-one))
(test::assert-false (= #\\y test-iter-one))
(test::assert-equal #\\Λ (str-iter-next! test-iter-start))
(test::assert-equal #\\λ (str-iter-next! test-iter-start))
(test::assert-equal #\\Σ (str-iter-next! test-iter-start))
(test::assert-equal #\\σ (str-iter-next! test-iter-start))
(test::assert-true (str-iter-empty? test-iter-start))
",
        ),
    );
    data.insert(
        interner.intern("str-iter-peek"),
        Expression::make_function(
            builtin_str_iter_peek,
            "Usage: (str-iter-peek string) -> char

Returns the char that next will return in the iterator for string.  Returns nil if iteration
is done.  Does not advance the iterator.

Section: string

Example:
(def test-iter-start \"y̆ΛλΣσ\")
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(def test-iter-one (str-iter-next! test-iter-start))
(test::assert-equal #\\y̆ test-iter-one)
(test::assert-true (= #\\y̆ test-iter-one))
(test::assert-false (= #\\y test-iter-one))
(test::assert-equal #\\Λ (str-iter-peek test-iter-start))
(test::assert-equal #\\Λ (str-iter-next! test-iter-start))
(test::assert-equal #\\λ (str-iter-peek test-iter-start))
(test::assert-equal #\\λ (str-iter-next! test-iter-start))
(test::assert-equal #\\Σ (str-iter-peek test-iter-start))
(test::assert-equal #\\Σ (str-iter-next! test-iter-start))
(test::assert-equal #\\σ (str-iter-peek test-iter-start))
(test::assert-equal #\\σ (str-iter-next! test-iter-start))
(test::assert-true (str-iter-empty? test-iter-start))
",
        ),
    );
    data.insert(
        interner.intern("str-iter-empty?"),
        Expression::make_function(
            builtin_str_iter_empty,
            "Usage: (str-iter-empty? string) -> t/nil

Returns true if the iterator for the string is empty or finished.

Section: string

Example:
(def test-iter-start \"test\")
(test::assert-true (str-iter-empty? test-iter-start))
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(test::assert-equal #\\t (str-iter-next! test-iter-start))
(test::assert-equal #\\e (str-iter-next! test-iter-start))
(test::assert-equal #\\s (str-iter-next! test-iter-start))
(test::assert-equal #\\t (str-iter-next! test-iter-start))
(test::assert-true (str-iter-empty? test-iter-start))

(def test-iter-start \"test\")
(test::assert-true (str-iter-empty? test-iter-start))
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(test::assert-equal #\\t (str-iter-next! test-iter-start))
(test::assert-equal #\\e (str-iter-next! test-iter-start))
(str-push! test-iter-start \"one\")
(test::assert-true (str-iter-empty? test-iter-start))
(str-iter-start test-iter-start)
(test::assert-false (str-iter-empty? test-iter-start))
(str-clear! test-iter-start)
(test::assert-true (str-iter-empty? test-iter-start))
",
        ),
    );
    data.insert(
        interner.intern("str-ignore-expand"),
        Expression::make_function(
            builtin_str_ignore_expand,
            "Usage: (str-ignore-expand exp0 ... expN) -> [final expression]

Like do but any strings in the form will not be expanded.

Section: string

Example:
(export 'TST-IGNORE \"TST\")
(test::assert-equal \"some TST stuff\" \"some $TST-IGNORE stuff\")
(test::assert-equal \"some \\$TST-IGNORE stuff\" (str-ignore-expand \"some $TST-IGNORE stuff\"))
",
        ),
    );

    data.insert(
        interner.intern("char-lower"),
        Expression::make_function(
            builtin_char_lower,
            "Usage: (char-lower char) -> char

Get lower case (utf) character for a character.

Section: char

Example:
(test::assert-equal #\\a (char-lower #\\A))
(test::assert-equal #\\a (char-lower #\\a))
(test::assert-not-equal #\\a (char-lower #\\Z))
(test::assert-equal #\\λ (char-lower #\\Λ))
(test::assert-equal #\\λ (char-lower #\\λ))
",
        ),
    );
    data.insert(
        interner.intern("char-upper"),
        Expression::make_function(
            builtin_char_upper,
            "Usage: (char-upper char) -> char

Get upper case (utf) character for a character.

Section: char

Example:
(test::assert-equal #\\A (char-upper #\\A))
(test::assert-equal #\\A (char-upper #\\a))
(test::assert-not-equal #\\A (char-upper #\\Z))
(test::assert-equal #\\Λ (char-upper #\\λ))
(test::assert-equal #\\Λ (char-upper #\\Λ))
",
        ),
    );
    data.insert(
        interner.intern("char-whitespace?"),
        Expression::make_function(
            builtin_char_is_whitespace,
            "Usage: (char-whitespace? char) -> t/nil

Returns true if a character is whitespace, false/nil otherwise.

Section: char

Example:
(test::assert-true (char-whitespace? #\\ ))
(test::assert-true (char-whitespace? #\\tab))
(test::assert-false (char-whitespace? #\\s))
",
        ),
    );
}
