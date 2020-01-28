use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;
use std::rc::Rc;

use crate::environment::*;
use crate::eval::*;
use crate::types::*;

fn as_string(environment: &mut Environment, exp: &Expression) -> io::Result<String> {
    exp.as_string(environment)
}

fn builtin_str_trim(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            let arg = arg.as_string(environment)?;
            return Ok(Expression::Atom(Atom::String(arg.trim().to_string())));
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-trim takes one form",
    ))
}

fn builtin_str_ltrim(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            let arg = arg.as_string(environment)?;
            return Ok(Expression::Atom(Atom::String(arg.trim_start().to_string())));
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-ltrim takes one form",
    ))
}

fn builtin_str_rtrim(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            let arg = arg.as_string(environment)?;
            return Ok(Expression::Atom(Atom::String(arg.trim_end().to_string())));
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-rtrim takes one form",
    ))
}

fn builtin_str_replace(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
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
                    return Ok(Expression::Atom(Atom::String(new_str)));
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-replace takes three forms",
    ))
}

fn builtin_str_split(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
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
                        split_list.push(Expression::Atom(Atom::String(s.to_string())));
                    }
                } else {
                    for s in text.split(&pat) {
                        split_list.push(Expression::Atom(Atom::String(s.to_string())));
                    }
                }
                return Ok(Expression::with_list(split_list));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-split takes two forms",
    ))
}

fn builtin_str_rsplit(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(pat) = args.next() {
        if let Some(text) = args.next() {
            if args.next().is_none() {
                let pat = eval(environment, pat)?;
                let pat = as_string(environment, &pat)?;
                let text = eval(environment, text)?;
                let text = as_string(environment, &text)?;
                let mut split_list: Vec<Expression> = Vec::new();
                for s in text.rsplit(&pat) {
                    split_list.push(Expression::Atom(Atom::String(s.to_string())));
                }
                return Ok(Expression::with_list(split_list));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-rsplit takes two forms",
    ))
}

fn builtin_str_splitn(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(n) = args.next() {
        if let Some(pat) = args.next() {
            if let Some(text) = args.next() {
                if args.next().is_none() {
                    let n = if let Expression::Atom(Atom::Int(n)) = eval(environment, n)? {
                        if n < 0 {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "str-splitn first form must be a positive integer",
                            ));
                        }
                        n
                    } else {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "str-splitn first form must be an integer",
                        ));
                    };
                    let pat = eval(environment, pat)?;
                    let pat = as_string(environment, &pat)?;
                    let text = eval(environment, text)?;
                    let text = as_string(environment, &text)?;
                    let mut split_list: Vec<Expression> = Vec::new();
                    for s in text.splitn(n as usize, &pat) {
                        split_list.push(Expression::Atom(Atom::String(s.to_string())));
                    }
                    return Ok(Expression::with_list(split_list));
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-splitn takes three forms",
    ))
}

fn builtin_str_rsplitn(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(n) = args.next() {
        if let Some(pat) = args.next() {
            if let Some(text) = args.next() {
                if args.next().is_none() {
                    let n = if let Expression::Atom(Atom::Int(n)) = eval(environment, n)? {
                        if n < 0 {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "str-splitn first form must be a positive integer",
                            ));
                        }
                        n
                    } else {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "str-rsplitn first form must be an integer",
                        ));
                    };
                    let pat = eval(environment, pat)?;
                    let pat = as_string(environment, &pat)?;
                    let text = eval(environment, text)?;
                    let text = as_string(environment, &text)?;
                    let mut split_list: Vec<Expression> = Vec::new();
                    for s in text.rsplitn(n as usize, &pat) {
                        split_list.push(Expression::Atom(Atom::String(s.to_string())));
                    }
                    return Ok(Expression::with_list(split_list));
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-rsplitn takes three forms",
    ))
}

fn builtin_str_cat_list(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(join_str) = args.next() {
        let join_str = eval(environment, join_str)?;
        let join_str = as_string(environment, &join_str)?;
        if let Some(list) = args.next() {
            if args.next().is_none() {
                let mut new_str = String::new();
                let list = eval(environment, list)?;
                match list {
                    Expression::Vector(list) => {
                        let mut first = true;
                        for s in list.borrow().iter() {
                            if !first {
                                new_str.push_str(&join_str);
                            }
                            new_str.push_str(&as_string(environment, s)?);
                            first = false;
                        }
                    }
                    Expression::Pair(_) => {
                        // Includes nil
                        let list = list.iter();
                        let mut first = true;
                        for s in list {
                            if !first {
                                new_str.push_str(&join_str);
                            }
                            new_str.push_str(&as_string(environment, s)?);
                            first = false;
                        }
                    }
                    _ => {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "str-cat-list second form must be a list",
                        ));
                    }
                }
                return Ok(Expression::Atom(Atom::String(new_str)));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-cat-list takes two forms",
    ))
}

fn builtin_str_sub(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg0) = args.next() {
        if let Some(arg1) = args.next() {
            if let Some(arg2) = args.next() {
                if args.next().is_none() {
                    let arg0 = eval(environment, arg0)?;
                    let arg1 = eval(environment, arg1)?;
                    let arg2 = eval(environment, arg2)?;
                    let start = if let Expression::Atom(Atom::Int(i)) = arg0 {
                        i as usize
                    } else {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "str-sub first form must be an int",
                        ));
                    };
                    let len = if let Expression::Atom(Atom::Int(i)) = arg1 {
                        i as usize
                    } else {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "str-sub second form must be an int",
                        ));
                    };
                    if let Expression::Atom(Atom::String(s)) = &arg2 {
                        if (start + len) <= s.len() {
                            return Ok(Expression::Atom(Atom::String(
                                s.as_str()[start..(start + len)].to_string(),
                            )));
                        } else {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "str-sub index out of range",
                            ));
                        }
                    } else {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "str-sub third form must be an String",
                        ));
                    }
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-sub takes three forms (int, int String)",
    ))
}

fn builtin_str_append(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(start) = args.next() {
        if let Some(end) = args.next() {
            if args.next().is_none() {
                let start = eval(environment, start)?;
                let end = eval(environment, end)?;
                if let Expression::Atom(Atom::String(end)) = end {
                    if let Expression::Atom(Atom::String(start)) = start {
                        let mut new_string = String::with_capacity(start.len() + end.len());
                        new_string.push_str(&start);
                        new_string.push_str(&end);
                        return Ok(Expression::Atom(Atom::String(new_string)));
                    } else {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "str-append forms must both be strings",
                        ));
                    }
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "str-append forms must both be strings",
                    ));
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-append takes two strings",
    ))
}

fn builtin_str(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
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
        match eval(environment, &a) {
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
    Ok(Expression::Atom(Atom::String(res)))
}

fn builtin_str_empty(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(string) = args.next() {
        if args.next().is_none() {
            let string = match eval(environment, &string)? {
                Expression::Atom(Atom::String(string)) => string,
                Expression::Atom(Atom::StringBuf(string)) => string.borrow().to_string(),
                _ => "".to_string(),
            };
            return if string.is_empty() {
                Ok(Expression::Atom(Atom::True))
            } else {
                Ok(Expression::nil())
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-empty? takes a string",
    ))
}

fn builtin_str_nth(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(idx) = args.next() {
        if let Some(string) = args.next() {
            if args.next().is_none() {
                if let Expression::Atom(Atom::Int(idx)) = eval(environment, &idx)? {
                    let string = match eval(environment, &string)? {
                        Expression::Atom(Atom::String(string)) => string,
                        Expression::Atom(Atom::StringBuf(string)) => string.borrow().to_string(),
                        _ => "".to_string(),
                    };
                    for (i, ch) in string.chars().enumerate() {
                        if i as i64 == idx {
                            return Ok(Expression::Atom(Atom::Char(ch)));
                        }
                    }
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "str-nth index out of range",
                    ));
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-nth takes two forms (int and string)",
    ))
}

fn builtin_str_lower(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(string) = args.next() {
        if args.next().is_none() {
            match eval(environment, &string)? {
                Expression::Atom(Atom::String(string)) => {
                    return Ok(Expression::Atom(Atom::String(string.to_ascii_lowercase())))
                }
                Expression::Atom(Atom::StringBuf(string)) => {
                    return Ok(Expression::Atom(Atom::String(
                        string.borrow().to_ascii_lowercase(),
                    )))
                }
                _ => {}
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-lower takes a string",
    ))
}

fn builtin_str_upper(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(string) = args.next() {
        if args.next().is_none() {
            match eval(environment, &string)? {
                Expression::Atom(Atom::String(string)) => {
                    return Ok(Expression::Atom(Atom::String(string.to_ascii_uppercase())))
                }
                Expression::Atom(Atom::StringBuf(string)) => {
                    return Ok(Expression::Atom(Atom::String(
                        string.borrow().to_ascii_uppercase(),
                    )))
                }
                _ => {}
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-upper takes a string",
    ))
}

fn builtin_str_bytes(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            match eval(environment, &arg)? {
                Expression::Atom(Atom::String(string)) => {
                    return Ok(Expression::Atom(Atom::Int(string.len() as i64)))
                }
                Expression::Atom(Atom::StringBuf(string)) => {
                    return Ok(Expression::Atom(Atom::Int(string.borrow().len() as i64)))
                }
                _ => {}
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-bytes takes a string",
    ))
}

fn builtin_str_starts_with(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(pat) = args.next() {
        if let Some(text) = args.next() {
            if args.next().is_none() {
                let pat = eval(environment, pat)?;
                let pat = as_string(environment, &pat)?;
                let text = eval(environment, text)?;
                let text = as_string(environment, &text)?;
                return if text.starts_with(&pat) {
                    Ok(Expression::Atom(Atom::True))
                } else {
                    Ok(Expression::nil())
                };
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-start-with takes two forms (pattern string)",
    ))
}

fn builtin_str_contains(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(pat) = args.next() {
        if let Some(text) = args.next() {
            if args.next().is_none() {
                let pat = eval(environment, pat)?;
                let pat = as_string(environment, &pat)?;
                let text = eval(environment, text)?;
                let text = as_string(environment, &text)?;
                return if text.contains(&pat) {
                    Ok(Expression::Atom(Atom::True))
                } else {
                    Ok(Expression::nil())
                };
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-contains takes two forms (pattern string)",
    ))
}

fn builtin_str_buf(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
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
        match eval(environment, &a) {
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
    Ok(Expression::Atom(Atom::StringBuf(Rc::new(RefCell::new(
        res,
    )))))
}

fn builtin_str_buf_push(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    // Do not use ?, make sure to reset environment state even on error.
    if let Some(arg0) = args.next() {
        if let Expression::Atom(Atom::StringBuf(res_in)) = eval(environment, arg0)? {
            let mut res = res_in.borrow_mut();
            for a in args {
                let a = eval(environment, &a)?;
                res.push_str(&as_string(environment, &a)?);
            }
            Ok(Expression::Atom(Atom::StringBuf(res_in.clone())))
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "str-buf-push! takes a string buffer as first form",
            ))
        }
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "str-buf-push! takes at least one form",
        ))
    }
}

fn builtin_str_buf_clear(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    // Do not use ?, make sure to reset environment state even on error.
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            if let Expression::Atom(Atom::StringBuf(res_in)) = eval(environment, arg0)? {
                let mut res = res_in.borrow_mut();
                res.clear();
                Ok(Expression::Atom(Atom::StringBuf(res_in.clone())))
            } else {
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    "str-buf-clear! takes a string buffer as first form",
                ))
            }
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "str-buf-clear! takes only one form",
            ))
        }
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "str-buf-clear! takes one form",
        ))
    }
}

fn str_map_inner(environment: &mut Environment, func: Lambda, string: &str) -> io::Result<String> {
    let mut res = String::new();
    for ch in string.chars() {
        let mut list = Vec::with_capacity(2);
        list.push(Expression::Atom(Atom::Lambda(func.clone())));
        list.push(Expression::Atom(Atom::Char(ch)));
        let a = eval(environment, &Expression::with_list(list))?;
        res.push_str(&as_string(environment, &a)?);
    }
    Ok(res)
}

fn builtin_str_map(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(func) = args.next() {
        if let Some(string) = args.next() {
            if args.next().is_none() {
                let func = eval(environment, func)?;
                let string = eval(environment, string)?;
                if let Expression::Atom(Atom::Lambda(func)) = func {
                    match string {
                        Expression::Atom(Atom::String(string)) => {
                            return Ok(Expression::Atom(Atom::String(str_map_inner(
                                environment,
                                func,
                                &string,
                            )?)));
                        }
                        Expression::Atom(Atom::StringBuf(string)) => {
                            return Ok(Expression::Atom(Atom::String(str_map_inner(
                                environment,
                                func,
                                &string.borrow(),
                            )?)));
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-map takes lambda and a string",
    ))
}

fn builtin_str_buf_map(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(func) = args.next() {
        if let Some(string) = args.next() {
            if args.next().is_none() {
                let func = eval(environment, func)?;
                let string = eval(environment, string)?;
                if let Expression::Atom(Atom::Lambda(func)) = func {
                    match string {
                        Expression::Atom(Atom::String(string)) => {
                            let res = str_map_inner(environment, func, &string)?;
                            return Ok(Expression::Atom(Atom::StringBuf(Rc::new(RefCell::new(
                                res,
                            )))));
                        }
                        Expression::Atom(Atom::StringBuf(string)) => {
                            let res = str_map_inner(environment, func, &string.borrow())?;
                            return Ok(Expression::Atom(Atom::StringBuf(Rc::new(RefCell::new(
                                res,
                            )))));
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-buf-map takes lambda and a string",
    ))
}

pub fn builtin_str_ignore_expand(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let save_ignore = environment.str_ignore_expand;
    environment.str_ignore_expand = true;
    let mut ret = Ok(Expression::nil());
    for arg in args {
        ret = eval(environment, &arg);
        if ret.is_err() {
            break;
        }
    }
    environment.str_ignore_expand = save_ignore;
    ret
}

fn builtin_char_lower(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(ch) = args.next() {
        if args.next().is_none() {
            if let Expression::Atom(Atom::Char(ch)) = eval(environment, ch)? {
                return Ok(Expression::Atom(Atom::Char(ch.to_ascii_lowercase())));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "char-lower takes a single char and produces it's ascii lowercase char",
    ))
}

fn builtin_char_upper(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(ch) = args.next() {
        if args.next().is_none() {
            if let Expression::Atom(Atom::Char(ch)) = eval(environment, ch)? {
                return Ok(Expression::Atom(Atom::Char(ch.to_ascii_uppercase())));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "char-upper takes a single char and produces it's ascii upper char",
    ))
}

fn builtin_char_is_whitespace(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(ch) = args.next() {
        if args.next().is_none() {
            if let Expression::Atom(Atom::Char(ch)) = eval(environment, ch)? {
                return if ch.is_whitespace() {
                    Ok(Expression::Atom(Atom::True))
                } else {
                    Ok(Expression::nil())
                };
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "char-whitespace? takes a single char and returns true if it is whitespace",
    ))
}

type CharTestFunc = fn(char, char) -> bool;

fn char_test(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
    ch_test: CharTestFunc,
) -> io::Result<Expression> {
    let mut last_ch = None;
    for arg in args {
        if let Expression::Atom(Atom::Char(ch)) = eval(environment, &arg)? {
            if let Some(last_ch) = last_ch {
                if !ch_test(last_ch, ch) {
                    return Ok(Expression::nil());
                }
            }
            last_ch = Some(ch);
        } else {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "char= only works on chars",
            ));
        }
    }
    Ok(Expression::Atom(Atom::True))
}

pub fn add_str_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Reference>, S>) {
    data.insert(
        "str-trim".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_trim,
            "Trim right and left whitespace from string.",
        )),
    );
    data.insert(
        "str-ltrim".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_ltrim,
            "Trim left whitspace from string.",
        )),
    );
    data.insert(
        "str-rtrim".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_rtrim,
            "Trim right whitespace from string.",
        )),
    );
    data.insert(
        "str-replace".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_replace,
            "Replace occurances of second string with third in the first string.",
        )),
    );
    data.insert(
        "str-split".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_split,
            "Use a pattern to split a string (:whitespace to split on whitespace).",
        )),
    );
    data.insert(
        "str-rsplit".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_rsplit,
            "Use a pattern to split a string into reverse order.",
        )),
    );
    data.insert(
        "str-splitn".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_splitn,
            "Use a pattern to split a string with at most n items.",
        )),
    );
    data.insert(
        "str-rsplitn".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_rsplitn,
            "Use a pattern to split a string with at most n items returned in reverse order.",
        )),
    );
    data.insert(
        "str-cat-list".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_cat_list,
            "Build a string by concatting a list with a join string.",
        )),
    );
    data.insert(
        "str-sub".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_sub,
            "Return a substring from a string given start and length.",
        )),
    );
    data.insert(
        "str-append".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_append,
            "Make a new string by appending two strings.",
        )),
    );
    data.insert(
        "str".to_string(),
        Rc::new(Expression::make_function(
            builtin_str,
            "Make a new string with it's arguments.",
        )),
    );
    data.insert(
        "str-empty?".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_empty,
            "Is a string empty?",
        )),
    );
    data.insert(
        "str-nth".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_nth,
            "Get the nth char of a string.",
        )),
    );
    data.insert(
        "str-lower".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_lower,
            "Get all lower case string from a string.",
        )),
    );
    data.insert(
        "str-upper".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_upper,
            "Get all upper case string from a string.",
        )),
    );
    data.insert(
        "str-bytes".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_bytes,
            "Return number of bytes in a string (may be more then length).",
        )),
    );
    data.insert(
        "str-starts-with".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_starts_with,
            "True if the second form starts with the first (as strings).",
        )),
    );
    data.insert(
        "str-contains".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_contains,
            "True if the second form contains the first (as strings).",
        )),
    );
    data.insert(
        "str-buf".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_buf,
            "Make a new string buffer with it's arguments.",
        )),
    );
    data.insert(
        "str-buf-push!".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_buf_push,
            "Push the forms (as strings) onto the first argument (a string buffer).",
        )),
    );
    data.insert(
        "str-buf-clear!".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_buf_clear,
            "Clear a string buffer.",
        )),
    );
    data.insert(
        "str-map".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_map,
            "Make a new string by applying lambda to each char.",
        )),
    );
    data.insert(
        "str-buf-map".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_buf_map,
            "Make a new string by applying lambda to each char.",
        )),
    );
    data.insert(
        "str-ignore-expand".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_ignore_expand,
            "Like progn but any strings in the form will not be expanded.",
        )),
    );

    data.insert(
        "char-lower".to_string(),
        Rc::new(Expression::make_function(
            builtin_char_lower,
            "Get ascii lower case character for a character.",
        )),
    );
    data.insert(
        "char-upper".to_string(),
        Rc::new(Expression::make_function(
            builtin_char_upper,
            "Get ascii upper case character for a character.",
        )),
    );
    data.insert(
        "char-whitespace?".to_string(),
        Rc::new(Expression::make_function(
            builtin_char_is_whitespace,
            "Returns true if a character is whitespace, false/nil otherwise.",
        )),
    );
    data.insert(
        "char=".to_string(),
        Rc::new(Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = &Expression>|
             -> io::Result<Expression> {
                char_test(environment, args, |ch1, ch2| ch1 == ch2)
            },
            "Test chars for equality.",
        )),
    );
    data.insert(
        "char!=".to_string(),
        Rc::new(Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = &Expression>|
             -> io::Result<Expression> {
                char_test(environment, args, |ch1, ch2| ch1 != ch2)
            },
            "Test chars for non equality.",
        )),
    );
    data.insert(
        "char>".to_string(),
        Rc::new(Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = &Expression>|
             -> io::Result<Expression> {
                char_test(environment, args, |ch1, ch2| ch1 > ch2)
            },
            "Test chars for greater than.",
        )),
    );
    data.insert(
        "char<".to_string(),
        Rc::new(Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = &Expression>|
             -> io::Result<Expression> {
                char_test(environment, args, |ch1, ch2| ch1 < ch2)
            },
            "Test chars for less than.",
        )),
    );
    data.insert(
        "char>=".to_string(),
        Rc::new(Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = &Expression>|
             -> io::Result<Expression> {
                char_test(environment, args, |ch1, ch2| ch1 >= ch2)
            },
            "Test chars for greater then or equal.",
        )),
    );
    data.insert(
        "char<=".to_string(),
        Rc::new(Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = &Expression>|
             -> io::Result<Expression> {
                char_test(environment, args, |ch1, ch2| ch1 <= ch2)
            },
            "Test chars for less than or equal.",
        )),
    );
}
