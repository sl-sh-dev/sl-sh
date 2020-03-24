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
                    } else if let Expression::Atom(Atom::StringRef(s)) = &arg2 {
                        if (start + len) <= s.len() {
                            return Ok(Expression::Atom(Atom::String(
                                s[start..(start + len)].to_string(),
                            )));
                        } else {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "str-sub index out of range",
                            ));
                        }
                    } else if let Expression::Atom(Atom::StringBuf(s)) = &arg2 {
                        let s = s.borrow();
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
            let empty = match eval(environment, &string)? {
                Expression::Atom(Atom::String(string)) => string.is_empty(),
                Expression::Atom(Atom::StringRef(string)) => string.is_empty(),
                Expression::Atom(Atom::StringBuf(string)) => string.borrow().is_empty(),
                _ => true,
            };
            return if empty {
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
                    match eval(environment, &string)? {
                        Expression::Atom(Atom::String(string)) => {
                            for (i, ch) in string.chars().enumerate() {
                                if i as i64 == idx {
                                    return Ok(Expression::Atom(Atom::Char(ch)));
                                }
                            }
                        }
                        Expression::Atom(Atom::StringRef(string)) => {
                            for (i, ch) in string.chars().enumerate() {
                                if i as i64 == idx {
                                    return Ok(Expression::Atom(Atom::Char(ch)));
                                }
                            }
                        }
                        Expression::Atom(Atom::StringBuf(string)) => {
                            for (i, ch) in string.borrow().chars().enumerate() {
                                if i as i64 == idx {
                                    return Ok(Expression::Atom(Atom::Char(ch)));
                                }
                            }
                        }
                        _ => {
                            return Err(io::Error::new(
                                io::ErrorKind::Other,
                                "str-nth second argument not a string",
                            ));
                        }
                    };
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
                Expression::Atom(Atom::StringRef(string)) => {
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
                Expression::Atom(Atom::StringRef(string)) => {
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
                Expression::Atom(Atom::StringRef(string)) => {
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
                        Expression::Atom(Atom::StringRef(string)) => {
                            return Ok(Expression::Atom(Atom::String(str_map_inner(
                                environment,
                                func,
                                string,
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
                        Expression::Atom(Atom::StringRef(string)) => {
                            let res = str_map_inner(environment, func, string)?;
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

fn char_test_short(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
    ch_test: CharTestFunc,
    short: bool,
) -> io::Result<Expression> {
    let mut last_ch = None;
    for arg in args {
        if let Expression::Atom(Atom::Char(ch)) = eval(environment, &arg)? {
            if let Some(last_ch) = last_ch {
                let test_res = ch_test(last_ch, ch);
                if short && test_res {
                    return Ok(Expression::Atom(Atom::True));
                }
                if !test_res {
                    return Ok(Expression::nil());
                }
            }
            last_ch = Some(ch);
        } else {
            return Err(io::Error::new(io::ErrorKind::Other, "only works on chars"));
        }
    }
    Ok(Expression::Atom(Atom::True))
}

fn char_test(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
    ch_test: CharTestFunc,
) -> io::Result<Expression> {
    char_test_short(environment, args, ch_test, false)
}

pub fn add_str_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, Rc<Reference>, S>,
) {
    data.insert(
        interner.intern("str-trim"),
        Rc::new(Expression::make_function(
            builtin_str_trim,
            "Usage: (str-trim string) -> string
 
Trim right and left whitespace from string.

Example:
(test::assert-equal \"some string\" (str-trim \"   some string\"))
(test::assert-equal \"some string\" (str-trim \"   some string   \"))
(test::assert-equal \"some string\" (str-trim (str-buf \"   some string   \")))
(test::assert-equal \"some string\" (str-trim \"some string   \"))
(test::assert-equal \"some string\" (str-trim \"some string\"))
",
        )),
    );
    data.insert(
        interner.intern("str-ltrim"),
        Rc::new(Expression::make_function(
            builtin_str_ltrim,
            "Usage: (str-ltrim string) -> string
 
Trim left whitspace from string.

Example:
(test::assert-equal \"some string\" (str-ltrim \"   some string\"))
(test::assert-equal \"some string   \" (str-ltrim \"   some string   \"))
(test::assert-equal \"some string   \" (str-ltrim (str-buf \"   some string   \")))
(test::assert-equal \"some string   \" (str-ltrim \"some string   \"))
(test::assert-equal \"some string\" (str-ltrim \"some string\"))
",
        )),
    );
    data.insert(
        interner.intern("str-rtrim"),
        Rc::new(Expression::make_function(
            builtin_str_rtrim,
            "Usage: (str-rtrim string) -> string
 
Trim right whitespace from string.

Example:
(test::assert-equal \"   some string\" (str-rtrim \"   some string\"))
(test::assert-equal \"   some string\" (str-rtrim \"   some string   \"))
(test::assert-equal \"   some string\" (str-rtrim (str-buf \"   some string   \")))
(test::assert-equal \"some string\" (str-rtrim \"some string   \"))
(test::assert-equal \"some string\" (str-rtrim \"some string\"))
",
        )),
    );
    data.insert(
        interner.intern("str-replace"),
        Rc::new(Expression::make_function(
            builtin_str_replace,
            "Usage: (str-replace string old-pattern new-pattern) -> string
 
Replace occurances of second string with third in the first string.

Example:
(test::assert-equal \"some yyy string\" (str-replace \"some xxx string\" \"xxx\" \"yyy\"))
(test::assert-equal \"some yyy string yyy\" (str-replace \"some xxx string xxx\" \"xxx\" \"yyy\"))
(test::assert-equal \"yyy some yyy string yyy\" (str-replace \"xxx some xxx string xxx\" \"xxx\" \"yyy\"))
",
        )),
    );
    data.insert(
        interner.intern("str-split"),
        Rc::new(Expression::make_function(
            builtin_str_split,
            "Usage: (str-split split-pattern string) -> vector
 
Use a pattern to split a string (:whitespace to split on whitespace).

Example:
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-split \"xxx\" \"somexxxyyyxxxstring\"))
(test::assert-equal '(\"some\" \"yyy\" \"string\" \"\") (str-split \"xxx\" \"somexxxyyyxxxstringxxx\"))
(test::assert-equal '(\"\" \"some\" \"yyy\" \"string\" \"\") (str-split \"xxx\" \"xxxsomexxxyyyxxxstringxxx\"))
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-split :whitespace \"some yyy string\"))
(test::assert-equal '(\"somexxxyyyxxxstring\") (str-split :whitespace \"somexxxyyyxxxstring\"))
(test::assert-equal '(\"somexxxyyyxxxstring\") (str-split \"zzz\" \"somexxxyyyxxxstring\"))
",
        )),
    );
    data.insert(
        interner.intern("str-rsplit"),
        Rc::new(Expression::make_function(
            builtin_str_rsplit,
            "Usage: (str-rsplit split-pattern string) -> vector
 
Use a pattern to split a string into reverse order.

Example:
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-rsplit \"xxx\" \"stringxxxyyyxxxsome\"))
(test::assert-equal '(\"\" \"some\" \"yyy\" \"string\") (str-rsplit \"xxx\" \"stringxxxyyyxxxsomexxx\"))
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-rsplit \" \" \"string yyy some\"))
(test::assert-equal '(\"somexxxyyyxxxstring\") (str-rsplit :whitespace \"somexxxyyyxxxstring\"))
(test::assert-equal '(\"somexxxyyyxxxstring\") (str-rsplit \"zzz\" \"somexxxyyyxxxstring\"))
",
        )),
    );
    data.insert(
        interner.intern("str-splitn"),
        Rc::new(Expression::make_function(
            builtin_str_splitn,
            "Usage: (str-splitn n split-pattern string) -> vector
 
Use a pattern to split a string with at most n items.

Example:
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-splitn 3 \"xxx\" \"somexxxyyyxxxstring\"))
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-splitn 4 \"xxx\" \"somexxxyyyxxxstring\"))
(test::assert-equal '(\"some\" \"yyy\" \"stringxxxother\") (str-splitn 3 \"xxx\" \"somexxxyyyxxxstringxxxother\"))
(test::assert-equal '(\"somexxxyyyxxxstringxxxother\") (str-splitn 1 \"xxx\" \"somexxxyyyxxxstringxxxother\"))
(test::assert-equal '() (str-splitn 0 \"xxx\" \"somexxxyyyxxxstringxxxzero\"))
",
        )),
    );
    data.insert(
        interner.intern("str-rsplitn"),
        Rc::new(Expression::make_function(
            builtin_str_rsplitn,
            "Usage: (str-rsplitn n split-pattern string) -> vector
 
Use a pattern to split a string with at most n items returned in reverse order.

Example:
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-rsplitn 3 \"xxx\" \"stringxxxyyyxxxsome\"))
(test::assert-equal '(\"some\" \"yyy\" \"string\") (str-rsplitn 4 \"xxx\" \"stringxxxyyyxxxsome\"))
(test::assert-equal '(\"other\" \"string\" \"somexxxyyy\") (str-rsplitn 3 \"xxx\" \"somexxxyyyxxxstringxxxother\"))
(test::assert-equal '(\"somexxxyyyxxxstringxxxother\") (str-rsplitn 1 \"xxx\" \"somexxxyyyxxxstringxxxother\"))
(test::assert-equal '() (str-rsplitn 0 \"xxx\" \"somexxxyyyxxxstringxxxzero\"))
",
        )),
    );
    data.insert(
        interner.intern("str-cat-list"),
        Rc::new(Expression::make_function(
            builtin_str_cat_list,
            "Usage: (str-cat-list join-pattern sequence) -> string

Build a string by concatting a sequence with a join string.

Example:
(test::assert-equal \"stringxxxyyyxxxsome\" (str-cat-list \"xxx\" '(\"string\" \"yyy\" \"some\")))
(test::assert-equal \"string yyy some\" (str-cat-list \" \" '(\"string\" \"yyy\" \"some\")))
(test::assert-equal \"stringyyysome\" (str-cat-list \"\" '(\"string\" \"yyy\" \"some\")))
",
        )),
    );
    data.insert(
        interner.intern("str-sub"),
        Rc::new(Expression::make_function(
            builtin_str_sub,
            "Usage: (str-sub start length string) -> string

Return a substring from a string given start (0 based) and length.

Example:
(test::assert-equal \"string\" (str-sub 0 6 \"stringxxxyyyxxxsome\"))
(test::assert-equal \"some\" (str-sub 15 4 \"stringxxxyyyxxxsome\"))
(test::assert-equal \"yyy\" (str-sub 9 3 \"stringxxxyyyxxxsome\"))
",
        )),
    );
    data.insert(
        interner.intern("str-append"),
        Rc::new(Expression::make_function(
            builtin_str_append,
            "Usage: (str-append string string) -> string

Make a new string by appending two strings.

Example:
(test::assert-equal \"stringsome\" (str-append \"string\" \"some\"))
(test::assert-equal \"string\" (str-append \"string\" \"\"))
(test::assert-equal \"string \" (str-append \"string\" \" \"))
",
        )),
    );
    data.insert(
        interner.intern("str"),
        Rc::new(Expression::make_function(
            builtin_str,
            "Usage: (str arg0 ... argN) -> string

Make a new string with it's arguments.

Arguments will be turned into strings.  If an argument is a process then the
output of the process will be captured and put into the string.

Example:
(test::assert-equal \"stringsome\" (str \"string\" \"some\"))
(test::assert-equal \"string\" (str \"string\" \"\"))
(test::assert-equal \"string 50\" (str \"string\" \" \" 50))
(test::assert-equal \"string 50 test\n\" (str \"string\" \" \" 50 \" \" (echo \"test\")))
",
        )),
    );
    data.insert(
        interner.intern("str-empty?"),
        Rc::new(Expression::make_function(
            builtin_str_empty,
            "Usage: (str-empty?) -> t/nil

Is a string empty?  Let's find out...

Example:
(test::assert-true (str-empty? \"\"))
(test::assert-true (str-empty? (str-trim \"   \")))
(test::assert-false (str-empty? \" \"))
(test::assert-false (str-empty? \"string\"))
",
        )),
    );
    data.insert(
        interner.intern("str-nth"),
        Rc::new(Expression::make_function(
            builtin_str_nth,
            "Usage: (str-nth n string) -> char

Get the nth char of a string.

Example:
(test::assert-equal #\\a (str-nth 2 \"stau\"))
(test::assert-equal #\\s (str-nth 0 \"stau\"))
(test::assert-equal #\\u (str-nth 3 \"stau\"))
",
        )),
    );
    data.insert(
        interner.intern("str-lower"),
        Rc::new(Expression::make_function(
            builtin_str_lower,
            "Usage: (str-lower string) -> string

Get all lower case string from a string.

Example:
(test::assert-equal \"stau\" (str-lower \"STAU\"))
(test::assert-equal \"stau\" (str-lower \"stau\"))
(test::assert-equal \"stau\" (str-lower \"Stau\"))
(test::assert-equal \"stau\" (str-lower \"StaU\"))
(test::assert-equal \"stau\" (str-lower \"sTaU\"))
",
        )),
    );
    data.insert(
        interner.intern("str-upper"),
        Rc::new(Expression::make_function(
            builtin_str_upper,
            "Usage: (str-upper string) -> string

Get all upper case string from a string.

Example:
(test::assert-equal \"STAU\" (str-upper \"STAU\"))
(test::assert-equal \"STAU\" (str-upper \"stau\"))
(test::assert-equal \"STAU\" (str-upper \"Stau\"))
(test::assert-equal \"STAU\" (str-upper \"StaU\"))
(test::assert-equal \"STAU\" (str-upper \"sTaU\"))
",
        )),
    );
    data.insert(
        interner.intern("str-bytes"),
        Rc::new(Expression::make_function(
            builtin_str_bytes,
            "Usage: (str-bytes string) -> int

Return number of bytes in a string (may be more then length).

Strings are utf8 so it chars and bytes may not be a one to one match.

Example:
(test::assert-equal 4 (str-bytes \"Stau\"))
(test::assert-equal 0 (str-bytes \"\"))
; Note 5 chars and 6 bytes because of the final char.
(test::assert-equal 6 (str-bytes \"StauΣ\"))
",
        )),
    );
    data.insert(
        interner.intern("str-starts-with"),
        Rc::new(Expression::make_function(
            builtin_str_starts_with,
            "Usage: (str-starts-with pattern string) -> t/nil

True if string start with pattern (both strings).

Example:
(test::assert-true (str-starts-with \"Stau\" \"Stausomething\"))
(test::assert-false (str-starts-with \"StaU\" \"Stausomething\"))
",
        )),
    );
    data.insert(
        interner.intern("str-contains"),
        Rc::new(Expression::make_function(
            builtin_str_contains,
            "Usage: (str-contains pattern string) -> t/nil

True if string contains pattern (both strings).

Example:
(test::assert-true (str-contains \"Stau\" \"Stausomething\"))
(test::assert-false (str-contains \"StaU\" \"Stausomething\"))
(test::assert-true (str-contains \"some\" \"Stausomething\"))
(test::assert-false (str-contains \"Some\" \"Stausomething\"))
(test::assert-true (str-contains \"thing\" \"Stausomething\"))
(test::assert-false (str-contains \"Thing\" \"Stausomething\"))
(test::assert-true (str-contains \"someΣ\" \"StausomeΣthing\"))
",
        )),
    );
    data.insert(
        interner.intern("str-buf"),
        Rc::new(Expression::make_function(
            builtin_str_buf,
            "Usage: (str-buf arg0 ... argN) -> string-buffer

Make a new string buffer with it's arguments.

Arguments will be turned into strings.  If an argument is a process then the
output of the process will be captured and put into the string buffer.

Example:
(test::assert-equal \"stringsome\" (str-buf \"string\" \"some\"))
(test::assert-equal \"StringBuf\" (type (str-buf \"string\" \"some\")))
(test::assert-true (string-buf? (str-buf \"string\" \"some\")))
(test::assert-equal \"string\" (str-buf \"string\" \"\"))
(test::assert-equal \"string 50\" (str-buf \"string\" \" \" 50))
(test::assert-equal \"string 50 test\n\" (str-buf \"string\" \" \" 50 \" \" (echo \"test\")))
",
        )),
    );
    data.insert(
        interner.intern("str-buf-push!"),
        Rc::new(Expression::make_function(
            builtin_str_buf_push,
            "Usage: (str-buf-push! string-buffer arg0 ... argN) -> string-buffer

Push the args (as strings) onto the string-buffer.  This is a destructive form.

Arguments will be turned into strings.  Returns the string-buffer it was given.

Example:
(test::assert-equal \"stringsome\" (str-buf-push! (str-buf \"string\") \"some\"))
(def 'test-str-buf-push (str-buf \"def-string\"))
(test::assert-equal \"def-stringsome\" (str-buf-push! test-str-buf-push \"some\"))
(test::assert-equal \"def-stringsome\" test-str-buf-push)
",
        )),
    );
    data.insert(
        interner.intern("str-buf-clear!"),
        Rc::new(Expression::make_function(
            builtin_str_buf_clear,
            "Usage: (str-buf-clear! string-buffer) -> string-buffer

Clears a string-buffer.  This is a destructive form.

Returns the string-buffer it was given.

Example:
(test::assert-equal \"\" (str-buf-clear! (str-buf \"string\")))
(def 'test-str-buf-clear (str-buf \"def-string\"))
(test::assert-equal \"\" (str-buf-clear! test-str-buf-clear))
(test::assert-equal \"\" test-str-buf-clear)
",
        )),
    );
    data.insert(
        interner.intern("str-map"),
        Rc::new(Expression::make_function(
            builtin_str_map,
            "Usage: (str-map lambda string) -> string

Make a new string by applying lambda to each char.

Example:
(test::assert-equal \"XstringXstrX\" (str-map (fn (ch) (if (char= #\\x ch) #\\X ch)) \"xstringxstrx\"))
(def 'test-str-map (str-map (fn (ch) (if (char= #\\x ch) #\\X ch)) \"xstringxstrx\"))
(test::assert-equal \"XstringXstrX\" test-str-map)
(test::assert-true (string? test-str-map))
(def 'test-str-map (str-map (fn (ch) (if (char= #\\x ch) #\\X ch)) (str-buf \"xstringxstrx\")))
(test::assert-equal \"XstringXstrX\" test-str-map)
(test::assert-true (string? test-str-map))
",
        )),
    );
    data.insert(
        interner.intern("str-buf-map"),
        Rc::new(Expression::make_function(
            builtin_str_buf_map,
            "Usage: (str-buf-map lambda string) -> string-buffer

Make a new string buffer by applying lambda to each char.

Example:
(def 'test-str-buf-map (str-buf-map (fn (ch) (if (char= #\\x ch) #\\X ch)) \"xstringxstrx\"))
(test::assert-equal \"XstringXstrX\" test-str-buf-map)
(test::assert-true (string-buf? test-str-buf-map))
(def 'test-str-buf-map (str-buf-map (fn (ch) (if (char= #\\x ch) #\\X ch)) (str-buf \"xstringxstrx\")))
(test::assert-equal \"XstringXstrX\" test-str-buf-map)
(test::assert-true (string-buf? test-str-buf-map))
",
        )),
    );
    data.insert(
        interner.intern("str-ignore-expand"),
        Rc::new(Expression::make_function(
            builtin_str_ignore_expand,
            "Usage: (str-ignore-expand exp0 ... expN) -> [final expression]

Like progn but any strings in the form will not be expanded.

Example:
(export 'TST-IGNORE \"TST\")
(test::assert-equal \"some TST stuff\" \"some $TST-IGNORE stuff\")
(test::assert-equal \"some \\$TST-IGNORE stuff\" (str-ignore-expand \"some $TST-IGNORE stuff\"))
",
        )),
    );

    data.insert(
        interner.intern("char-lower"),
        Rc::new(Expression::make_function(
            builtin_char_lower,
            "Usage: (char-lower char) -> char

Get ascii lower case character for a character.

Example:
(test::assert-equal #\\a (char-lower #\\A))
(test::assert-equal #\\a (char-lower #\\a))
(test::assert-not-equal #\\a (char-lower #\\Z))
",
        )),
    );
    data.insert(
        interner.intern("char-upper"),
        Rc::new(Expression::make_function(
            builtin_char_upper,
            "Usage: (char-upper char) -> char

Get ascii upper case character for a character.

Example:
(test::assert-equal #\\A (char-upper #\\A))
(test::assert-equal #\\A (char-upper #\\a))
(test::assert-not-equal #\\A (char-upper #\\Z))
",
        )),
    );
    data.insert(
        interner.intern("char-whitespace?"),
        Rc::new(Expression::make_function(
            builtin_char_is_whitespace,
            "Usage: (char-whitespace? char) -> t/nil

Returns true if a character is whitespace, false/nil otherwise.

Example:
(test::assert-true (char-whitespace? #\\ ))
(test::assert-true (char-whitespace? #\\tab))
(test::assert-false (char-whitespace? #\\s))
",
        )),
    );
    data.insert(
        interner.intern("char="),
        Rc::new(Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = &Expression>|
             -> io::Result<Expression> {
                char_test(environment, args, |ch1, ch2| ch1 == ch2)
            },
            "Usage: (char= char0 char1 ... charN) -> t/nil

Test chars for equality.

Example:
(test::assert-true (char= #\\  #\\ ))
(test::assert-true (char= #\\a #\\a))
(test::assert-true (char= #\\a #\\a #\\a))
(test::assert-false (char= #\\z #\\a))
(test::assert-false (char= #\\z #\\a #\\a))
(test::assert-false (char= #\\z #\\Z))
(test::assert-true (char= #\\a (char-lower #\\A)))
",
        )),
    );
    data.insert(
        interner.intern("char!="),
        Rc::new(Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = &Expression>|
             -> io::Result<Expression> {
                char_test_short(environment, args, |ch1, ch2| ch1 != ch2, true)
            },
            "Usage: (char!= char0 char1 ... charN) -> t/nil

Test chars for in-equality.

Example:
(test::assert-false (char!= #\\  #\\ ))
(test::assert-false (char!= #\\a #\\a))
(test::assert-false (char!= #\\a #\\a #\\a))
(test::assert-true (char!= #\\z #\\a))
(test::assert-true (char!= #\\z #\\a #\\a))
(test::assert-true (char!= #\\z #\\Z))
(test::assert-false (char!= #\\a (char-lower #\\A)))
",
        )),
    );
    data.insert(
        interner.intern("char>"),
        Rc::new(Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = &Expression>|
             -> io::Result<Expression> {
                char_test(environment, args, |ch1, ch2| ch1 > ch2)
            },
            "Usage: (char> char0 char1 ... charN) -> t/nil

Test chars for greater than.

Example:
(test::assert-false (char> #\\  #\\ ))
(test::assert-false (char> #\\a #\\b))
(test::assert-false (char> #\\a #\\b #\\a))
(test::assert-true (char> #\\z #\\a))
(test::assert-true (char> #\\c #\\b #\\a))
(test::assert-true (char> #\\z #\\Z))
(test::assert-false (char> #\\a (char-lower #\\A)))
",
        )),
    );
    data.insert(
        interner.intern("char<"),
        Rc::new(Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = &Expression>|
             -> io::Result<Expression> {
                char_test(environment, args, |ch1, ch2| ch1 < ch2)
            },
            "Usage: (char< char0 char1 ... charN) -> t/nil

Test chars for less than.

Example:
(test::assert-false (char< #\\  #\\ ))
(test::assert-false (char< #\\b #\\b))
(test::assert-false (char< #\\a #\\b #\\a))
(test::assert-true (char< #\\a #\\z))
(test::assert-true (char< #\\a #\\b #\\c))
(test::assert-true (char< #\\Z #\\z))
(test::assert-true (char< #\\A (char-lower #\\A)))
",
        )),
    );
    data.insert(
        interner.intern("char>="),
        Rc::new(Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = &Expression>|
             -> io::Result<Expression> {
                char_test(environment, args, |ch1, ch2| ch1 >= ch2)
            },
            "Usage: (char>= char0 char1 ... charN) -> t/nil

Test chars for greater than or equal.

Example:
(test::assert-true (char>= #\\  #\\ ))
(test::assert-false (char>= #\\a #\\b))
(test::assert-false (char>= #\\a #\\b #\\a))
(test::assert-true (char>= #\\z #\\a))
(test::assert-true (char>= #\\c #\\b #\\a))
(test::assert-true (char>= #\\z #\\Z))
(test::assert-true (char>= #\\a (char-lower #\\A)))
",
        )),
    );
    data.insert(
        interner.intern("char<="),
        Rc::new(Expression::make_function(
            |environment: &mut Environment,
             args: &mut dyn Iterator<Item = &Expression>|
             -> io::Result<Expression> {
                char_test(environment, args, |ch1, ch2| ch1 <= ch2)
            },
            "Usage: (char<= char0 char1 ... charN) -> t/nil

Test chars for less than or equal.

Example:
(test::assert-true (char<= #\\  #\\ ))
(test::assert-true (char<= #\\b #\\b))
(test::assert-false (char<= #\\a #\\b #\\a))
(test::assert-true (char<= #\\a #\\z))
(test::assert-false (char<= #\\z #\\a))
(test::assert-true (char<= #\\a #\\b #\\c))
(test::assert-true (char<= #\\Z #\\z))
(test::assert-true (char<= #\\A (char-lower #\\A)))
",
        )),
    );
}
