use std::collections::HashMap;
use std::hash::BuildHasher;
use std::io;
use std::rc::Rc;

use crate::builtins_util::*;
use crate::environment::*;
use crate::eval::*;
use crate::types::*;

fn as_string(environment: &mut Environment, exp: &Expression) -> io::Result<String> {
    exp.as_string(environment)
}

fn builtin_str_trim(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-trim takes one form",
        ));
    }
    let arg = as_string(environment, &args[0])?;
    Ok(Expression::Atom(Atom::String(arg.trim().to_string())))
}

fn builtin_str_ltrim(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-ltrim takes one form",
        ));
    }
    let arg = as_string(environment, &args[0])?;
    Ok(Expression::Atom(Atom::String(arg.trim_start().to_string())))
}

fn builtin_str_rtrim(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-rtrim takes one form",
        ));
    }
    let arg = as_string(environment, &args[0])?;
    Ok(Expression::Atom(Atom::String(arg.trim_end().to_string())))
}

fn builtin_str_replace(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 3 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-replace takes three forms",
        ));
    }
    let arg0 = as_string(environment, &args[0])?;
    let arg1 = as_string(environment, &args[1])?;
    let arg2 = as_string(environment, &args[2])?;
    let new_str = arg0.replace(&arg1, &arg2);
    Ok(Expression::Atom(Atom::String(new_str)))
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

fn builtin_str_cat_list(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-cat-list takes two forms",
        ));
    }
    let join_str = as_string(environment, &args[0])?;
    let mut new_str = String::new();
    if let Expression::Vector(list) = &args[1] {
        let mut first = true;
        for s in list.borrow().iter() {
            if !first {
                new_str.push_str(&join_str);
            }
            new_str.push_str(&as_string(environment, s)?);
            first = false;
        }
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-cat-list second form must be a list",
        ));
    }
    Ok(Expression::Atom(Atom::String(new_str)))
}

fn builtin_str_sub(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 3 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-sub takes three forms (int, int String)",
        ));
    }
    let start = if let Expression::Atom(Atom::Int(i)) = args[0] {
        i as usize
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-sub first form must be an int",
        ));
    };
    let len = if let Expression::Atom(Atom::Int(i)) = args[1] {
        i as usize
    } else {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-sub second form must be an int",
        ));
    };
    let arg3 = &args[2];
    if let Expression::Atom(Atom::String(s)) = &arg3 {
        if (start + len) <= s.len() {
            Ok(Expression::Atom(Atom::String(
                s.as_str()[start..(start + len)].to_string(),
            )))
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "str-sub index out of range",
            ))
        }
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "str-sub third form must be an String",
        ))
    }
}

fn builtin_str_append(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    let mut args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "str-append takes two strings",
        ));
    }
    let end = args.pop().unwrap();
    let start = args.pop().unwrap();
    if let Expression::Atom(Atom::String(end)) = end {
        if let Expression::Atom(Atom::String(start)) = start {
            let mut new_string = String::with_capacity(start.len() + end.len());
            new_string.push_str(&start);
            new_string.push_str(&end);
            Ok(Expression::Atom(Atom::String(new_string)))
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "str-append forms must both be strings",
            ))
        }
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "str-append forms must both be strings",
        ))
    }
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
            if let Expression::Atom(Atom::String(string)) = eval(environment, &string)? {
                return if string.is_empty() {
                    Ok(Expression::Atom(Atom::True))
                } else {
                    Ok(Expression::Atom(Atom::Nil))
                };
            }
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
                    if let Expression::Atom(Atom::String(string)) = eval(environment, &string)? {
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
    if let Some(ch) = args.next() {
        if args.next().is_none() {
            if let Expression::Atom(Atom::String(string)) = eval(environment, ch)? {
                return Ok(Expression::Atom(Atom::String(string.to_ascii_lowercase())));
            }
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
    if let Some(ch) = args.next() {
        if args.next().is_none() {
            if let Expression::Atom(Atom::String(string)) = eval(environment, ch)? {
                return Ok(Expression::Atom(Atom::String(string.to_ascii_uppercase())));
            }
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
            if let Expression::Atom(Atom::String(string)) = eval(environment, arg)? {
                return Ok(Expression::Atom(Atom::Int(string.len() as i64)));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "str-bytes takes a string",
    ))
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
                    return Ok(Expression::Atom(Atom::Nil));
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

pub fn add_str_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Expression>, S>) {
    data.insert(
        "str-trim".to_string(),
        Rc::new(Expression::Func(builtin_str_trim)),
    );
    data.insert(
        "str-ltrim".to_string(),
        Rc::new(Expression::Func(builtin_str_ltrim)),
    );
    data.insert(
        "str-rtrim".to_string(),
        Rc::new(Expression::Func(builtin_str_rtrim)),
    );
    data.insert(
        "str-replace".to_string(),
        Rc::new(Expression::Func(builtin_str_replace)),
    );
    data.insert(
        "str-split".to_string(),
        Rc::new(Expression::make_function(
            builtin_str_split,
            "Use a pattern to split a string (:whitespace to split on whitespace).",
        )),
    );
    data.insert(
        "str-cat-list".to_string(),
        Rc::new(Expression::Func(builtin_str_cat_list)),
    );
    data.insert(
        "str-sub".to_string(),
        Rc::new(Expression::Func(builtin_str_sub)),
    );
    data.insert(
        "str-append".to_string(),
        Rc::new(Expression::Func(builtin_str_append)),
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
