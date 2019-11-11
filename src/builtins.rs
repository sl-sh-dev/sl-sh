use nix::{
    sys::{
        signal::{self, Signal},
        termios,
    },
    unistd::{self, Pid},
};
use std::collections::{hash_map, HashMap};
use std::env;
use std::fs;
use std::hash::BuildHasher;
use std::io::{self, Write};
use std::rc::Rc;

use crate::builtins_util::*;
use crate::config::VERSION_STRING;
use crate::environment::*;
use crate::eval::*;
use crate::process::*;
use crate::reader::*;
use crate::types::*;

fn builtin_eval(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, &arg)?;
            return match arg {
                Expression::Atom(Atom::String(s)) => match read(&s, false) {
                    Ok(ast) => eval(environment, &ast),
                    Err(err) => Err(io::Error::new(io::ErrorKind::Other, err.reason)),
                },
                _ => eval(environment, &arg),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "eval can only have one form",
    ))
}

fn builtin_fncall(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let mut call_list = Vec::new();
    for arg in args {
        call_list.push(arg.clone());
    }
    if call_list.is_empty() {
        return Err(io::Error::new(io::ErrorKind::Other, "fn_call: empty call"));
    }
    let command = eval(environment, &call_list[0])?;
    fn_call(environment, &command, Box::new(call_list[1..].iter()))
}

fn builtin_apply(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let mut call_list = Vec::new();
    let mut last_arg: Option<&Expression> = None;
    for arg in args {
        if let Some(a) = last_arg {
            call_list.push(a);
        }
        last_arg = Some(arg);
    }
    let tlist;
    let list_borrow;
    let last_evaled;
    if let Some(alist) = last_arg {
        last_evaled = eval(environment, alist)?;
        let itr = match last_evaled {
            Expression::Vector(list) => {
                tlist = list.clone();
                list_borrow = tlist.borrow();
                Box::new(list_borrow.iter())
            }
            Expression::Pair(_, _) => last_evaled.iter(),
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "apply: last arg not a list",
                ))
            }
        };
        for a in itr {
            call_list.push(a);
        }
    }
    if call_list.is_empty() {
        return Err(io::Error::new(io::ErrorKind::Other, "apply: empty call"));
    }
    let command = eval(environment, &call_list[0])?;
    fn_call(
        environment,
        &command,
        Box::new(call_list[1..].iter().copied()),
    )
}

fn builtin_unwind_protect(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(protected) = args.next() {
        let result = eval(environment, protected);
        for a in args {
            if let Err(err) = eval(environment, a) {
                eprintln!(
                    "ERROR in unwind-protect cleanup form {}, {} will continue cleanup",
                    a, err
                );
            }
        }
        result
    } else {
        Ok(Expression::Atom(Atom::Nil))
    }
}

fn builtin_err(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return Err(io::Error::new(
                io::ErrorKind::Other,
                arg.as_string(environment)?,
            ));
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "err can only have one form",
    ))
}

fn builtin_load(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            let contents = fs::read_to_string(arg.as_string(environment)?)?;
            let ast = read(&contents, false);
            return match ast {
                Ok(ast) => {
                    let ast = match ast {
                        Expression::Vector(olist) => {
                            let mut list = olist.borrow_mut();
                            if let Some(first) = list.get(0) {
                                match first {
                                    Expression::Vector(_) => {
                                        let mut v = Vec::with_capacity(list.len() + 1);
                                        v.push(Expression::Atom(Atom::Symbol("progn".to_string())));
                                        for l in list.drain(..) {
                                            v.push(l);
                                        }
                                        Expression::with_list(v)
                                    }
                                    Expression::Pair(_, _) => {
                                        let mut v = Vec::with_capacity(list.len() + 1);
                                        v.push(Expression::Atom(Atom::Symbol("progn".to_string())));
                                        for l in list.drain(..) {
                                            v.push(l);
                                        }
                                        Expression::with_list(v)
                                    }
                                    _ => {
                                        drop(list);
                                        Expression::Vector(olist)
                                    }
                                }
                            } else {
                                drop(list);
                                Expression::Vector(olist)
                            }
                        }
                        _ => ast,
                    };
                    eval(environment, &ast)
                }
                Err(err) => Err(io::Error::new(io::ErrorKind::Other, err.reason)),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "load needs one argument",
    ))
}

fn builtin_length(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return match &arg {
                Expression::Atom(Atom::Nil) => Ok(Expression::Atom(Atom::Int(0))),
                Expression::Atom(Atom::String(s)) => {
                    Ok(Expression::Atom(Atom::Int(s.len() as i64)))
                }
                Expression::Atom(_) => Ok(Expression::Atom(Atom::Int(1))),
                Expression::Vector(list) => {
                    Ok(Expression::Atom(Atom::Int(list.borrow().len() as i64)))
                }
                Expression::Pair(_e1, e2) => {
                    let mut len = 0;
                    let mut e_next = e2.clone();
                    loop {
                        match &*e_next.clone().borrow() {
                            Expression::Pair(_e1, e2) => {
                                e_next = e2.clone();
                                len += 1;
                            }
                            Expression::Atom(Atom::Nil) => {
                                len += 1;
                                break;
                            }
                            _ => {
                                len += 1;
                                break;
                            }
                        }
                    }
                    Ok(Expression::Atom(Atom::Int(len)))
                }
                Expression::HashMap(map) => {
                    Ok(Expression::Atom(Atom::Int(map.borrow().len() as i64)))
                }
                _ => Ok(Expression::Atom(Atom::Int(0))),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "length takes one form",
    ))
}

fn builtin_if(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(if_form) = args.next() {
        if let Some(then_form) = args.next() {
            return match eval(environment, if_form)? {
                Expression::Atom(Atom::Nil) => {
                    if let Some(else_form) = args.next() {
                        eval(environment, else_form)
                    } else {
                        Ok(Expression::Atom(Atom::Nil))
                    }
                }
                _ => eval(environment, then_form),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "if needs exactly two or three expressions",
    ))
}

fn args_out(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
    add_newline: bool,
    pretty: bool,
    writer: &mut dyn Write,
) -> io::Result<()> {
    for a in args {
        let aa = eval(environment, a)?;
        // If we have a standalone string do not quote it...
        let pretty = if let Expression::Atom(Atom::String(_)) = aa {
            false
        } else {
            pretty
        };
        if pretty {
            aa.pretty_printf(environment, writer)?;
        } else {
            aa.writef(environment, writer)?;
        }
    }
    if add_newline {
        writer.write_all("\n".as_bytes())?;
    }
    Ok(())
}

fn print_to_oe(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
    add_newline: bool,
    pretty: bool,
    default_error: bool,
    key: &str,
) -> io::Result<()> {
    let out = get_expression(environment, key);
    match out {
        Some(out) => {
            if let Expression::File(f) = &*out {
                match f {
                    FileState::Stdout => {
                        let stdout = io::stdout();
                        let mut out = stdout.lock();
                        args_out(environment, args, add_newline, pretty, &mut out)?;
                    }
                    FileState::Stderr => {
                        let stdout = io::stderr();
                        let mut out = stdout.lock();
                        args_out(environment, args, add_newline, pretty, &mut out)?;
                    }
                    FileState::Write(f) => {
                        args_out(environment, args, add_newline, pretty, &mut *f.borrow_mut())?;
                    }
                    _ => {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "ERROR: Can not print to a non-writable file.",
                        ));
                    }
                }
            } else {
                let msg = format!("ERROR: {} is not a file!", key);
                return Err(io::Error::new(io::ErrorKind::Other, msg));
            }
        }
        None => {
            if default_error {
                let stdout = io::stderr();
                let mut out = stdout.lock();
                args_out(environment, args, add_newline, pretty, &mut out)?;
            } else {
                let stdout = io::stdout();
                let mut out = stdout.lock();
                args_out(environment, args, add_newline, pretty, &mut out)?;
            }
        }
    }
    Ok(())
}

fn print(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
    add_newline: bool,
) -> io::Result<Expression> {
    match &environment.state.stdout_status {
        Some(IOState::Null) => { /* Nothing to do... */ }
        _ => {
            print_to_oe(environment, args, add_newline, true, false, "*stdout*")?;
        }
    };
    Ok(Expression::Atom(Atom::Nil))
}

pub fn eprint(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
    add_newline: bool,
) -> io::Result<Expression> {
    match &environment.state.stderr_status {
        Some(IOState::Null) => { /* Nothing to do... */ }
        _ => {
            print_to_oe(environment, args, add_newline, true, true, "*stderr*")?;
        }
    };
    Ok(Expression::Atom(Atom::Nil))
}

fn builtin_print(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    print(environment, args, false)
}

fn builtin_println(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    print(environment, args, true)
}

fn builtin_eprint(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    eprint(environment, args, false)
}

fn builtin_eprintln(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    eprint(environment, args, true)
}

fn builtin_format(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let mut res = String::new();
    for a in args {
        res.push_str(&eval(environment, a)?.as_string(environment)?);
    }
    Ok(Expression::Atom(Atom::String(res)))
}

pub fn builtin_progn(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let mut args = list_to_args(environment, args, true)?;
    if args.is_empty() {
        Ok(Expression::Atom(Atom::Nil))
    } else {
        Ok(args.pop().unwrap())
    }
}

fn proc_set_vars2(
    _environment: &mut Environment,
    key: &Expression,
    val: &Expression,
) -> io::Result<(String, Expression)> {
    let key = match key {
        Expression::Atom(Atom::Symbol(s)) => s.clone(),
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "first form (binding key) must evaluate to a symbol",
            ));
        }
    };
    let mut val = val.clone();
    if let Expression::Atom(Atom::String(vs)) = val {
        let vs = match expand_tilde(&vs) {
            Some(v) => v,
            None => vs,
        };
        val = Expression::Atom(Atom::String(vs));
    }
    Ok((key, val))
}

fn proc_set_vars(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<(String, Expression)> {
    let mut args = args.iter();
    proc_set_vars2(environment, args.next().unwrap(), args.next().unwrap())
}

fn builtin_set(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "set can only have two expressions",
        ))
    } else {
        let (key, val) = proc_set_vars(environment, &args)?;
        if let hash_map::Entry::Occupied(mut entry) = environment.dynamic_scope.entry(key.clone()) {
            entry.insert(Rc::new(val.clone()));
            Ok(val)
        } else if let Some(scope) = get_symbols_scope(environment, &key) {
            scope.borrow_mut().data.insert(key, Rc::new(val.clone()));
            Ok(val)
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "set's first form must evaluate to an existing symbol",
            ))
        }
    }
}

fn builtin_export(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "export can only have two expressions",
        ))
    } else {
        let mut args = args.iter();
        let key = args.next().unwrap();
        let key = match key {
            Expression::Atom(Atom::Symbol(s)) => s.clone(),
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "export's first form must evaluate to a symbol",
                ));
            }
        };
        let val = args.next().unwrap();
        let val = match val {
            Expression::Process(ProcessState::Running(_pid)) => Expression::Atom(Atom::String(
                val.as_string(environment)
                    .unwrap_or_else(|_| "PROCESS FAILED".to_string()),
            )),
            Expression::Process(ProcessState::Over(_pid, _exit_status)) => {
                Expression::Atom(Atom::String(
                    val.as_string(environment)
                        .unwrap_or_else(|_| "PROCESS FAILED".to_string()),
                ))
            }
            Expression::Func(_) => Expression::Atom(Atom::String("::FUNCTION::".to_string())),
            Expression::File(FileState::Stdin) => Expression::Atom(Atom::String(
                val.as_string(environment)
                    .unwrap_or_else(|_| "STDIN FAILED".to_string()),
            )),
            Expression::File(FileState::Stdout) => {
                Expression::Atom(Atom::String("::STDOUT::".to_string()))
            }
            Expression::File(FileState::Stderr) => {
                Expression::Atom(Atom::String("::STDERR::".to_string()))
            }
            Expression::File(FileState::Read(_)) => Expression::Atom(Atom::String(
                val.as_string(environment)
                    .unwrap_or_else(|_| "FILE READ FAILED".to_string()),
            )),
            Expression::File(FileState::Write(_)) => {
                Expression::Atom(Atom::String("::WRITE FILE::".to_string()))
            }
            Expression::File(FileState::Closed) => {
                Expression::Atom(Atom::String("::CLOSED FILE::".to_string()))
            }
            _ => val.clone(),
        };
        let val = val.as_string(environment)?;
        let val = match expand_tilde(&val) {
            Some(v) => v,
            None => val,
        };
        if !val.is_empty() {
            env::set_var(key, val.clone());
        } else {
            env::remove_var(key);
        }
        Ok(Expression::Atom(Atom::String(val)))
    }
}

fn builtin_unexport(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = eval(environment, key)?;
            if let Expression::Atom(Atom::Symbol(k)) = key {
                env::remove_var(k);
                return Ok(Expression::Atom(Atom::Nil));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "unexport can only have one expression (symbol)",
    ))
}

fn builtin_def(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "def can only have two expressions",
        ))
    } else {
        let (key, val) = proc_set_vars(environment, &args)?;
        set_expression_current(environment, key, Rc::new(val.clone()));
        Ok(val)
    }
}

fn builtin_undef(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = eval(environment, key)?;
            if let Expression::Atom(Atom::Symbol(k)) = key {
                remove_expression_current(environment, &k);
                return Ok(Expression::Atom(Atom::Nil));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "undef can only have one expression (symbol)",
    ))
}

fn builtin_dyn(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, false)?;
    if args.len() != 3 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "dyn requires three expressions (symbol, value, form to evaluate)",
        ))
    } else {
        let arg0 = eval(environment, &args[0])?;
        let arg1 = eval(environment, &args[1])?;
        let (key, val) = proc_set_vars2(environment, &arg0, &arg1)?;
        let old_val = if environment.dynamic_scope.contains_key(&key) {
            Some(environment.dynamic_scope.remove(&key).unwrap())
        } else {
            None
        };
        environment
            .dynamic_scope
            .insert(key.clone(), Rc::new(val.clone()));
        let res = eval(environment, &args[2]);
        if let Some(old_val) = old_val {
            environment.dynamic_scope.insert(key.clone(), old_val);
        } else {
            environment.dynamic_scope.remove(&key);
        }
        res
    }
}

fn builtin_is_global_scope(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if !args.is_empty() {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "is-global-scope take no forms",
        ))
    } else if environment.current_scope.len() == 1 {
        Ok(Expression::Atom(Atom::True))
    } else {
        Ok(Expression::Atom(Atom::Nil))
    }
}

fn builtin_to_symbol(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "to-symbol take one form",
        ))
    } else {
        match &args[0] {
            Expression::Atom(Atom::String(s)) => Ok(Expression::Atom(Atom::Symbol(s.clone()))),
            Expression::Atom(Atom::Symbol(s)) => Ok(Expression::Atom(Atom::Symbol(s.clone()))),
            Expression::Atom(Atom::Int(i)) => Ok(Expression::Atom(Atom::Symbol(format!("{}", i)))),
            Expression::Atom(Atom::Float(f)) => {
                Ok(Expression::Atom(Atom::Symbol(format!("{}", f))))
            }
            _ => Err(io::Error::new(
                io::ErrorKind::Other,
                "to-symbol can only convert strings, symbols, ints and floats to a symbol",
            )),
        }
    }
}

fn builtin_fn(environment: &mut Environment, parts: &[Expression]) -> io::Result<Expression> {
    if parts.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "fn can only have two forms",
        ))
    } else {
        let mut parts = parts.iter();
        let params = parts.next().unwrap();
        let body = parts.next().unwrap();
        Ok(Expression::Atom(Atom::Lambda(Lambda {
            params: Box::new(params.clone()),
            body: Box::new(body.clone()),
            capture: environment.current_scope.last().unwrap().clone(),
        })))
    }
}

fn builtin_quote(
    _environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            return Ok(arg.clone());
        }
    }
    Err(io::Error::new(io::ErrorKind::Other, "quote takes one form"))
}

fn replace_commas(
    environment: &mut Environment,
    list: &mut dyn Iterator<Item = &Expression>,
    is_vector: bool,
) -> io::Result<Expression> {
    let mut output: Vec<Expression> = Vec::new(); //with_capacity(list.len());
    let mut comma_next = false;
    let mut amp_next = false;
    for exp in list {
        let exp = match exp {
            Expression::Vector(tlist) => {
                replace_commas(environment, &mut tlist.borrow().iter(), is_vector)?
            }
            Expression::Pair(_, _) => replace_commas(environment, &mut exp.iter(), is_vector)?,
            _ => exp.clone(),
        };
        if let Expression::Atom(Atom::Symbol(symbol)) = &exp {
            if symbol == "," {
                comma_next = true;
            } else if symbol == ",@" {
                amp_next = true;
            } else if comma_next {
                output.push(eval(environment, &exp)?);
                comma_next = false;
            } else if amp_next {
                let nl = eval(environment, &exp)?;
                if let Expression::Vector(new_list) = nl {
                    for item in new_list.borrow().iter() {
                        output.push(item.clone());
                    }
                } else if let Expression::Pair(_, _) = nl {
                    for item in nl.iter() {
                        output.push(item.clone());
                    }
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        ",@ must be applied to a list",
                    ));
                }
                amp_next = false;
            } else {
                output.push(exp);
            }
        } else if comma_next {
            output.push(eval(environment, &exp)?);
            comma_next = false;
        } else if amp_next {
            let nl = eval(environment, &exp)?;
            if let Expression::Vector(new_list) = nl {
                for item in new_list.borrow_mut().drain(..) {
                    output.push(item);
                }
            } else if let Expression::Pair(_, _) = nl {
                for item in nl.iter() {
                    output.push(item.clone());
                }
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    ",@ must be applied to a list",
                ));
            }
            amp_next = false;
        } else {
            output.push(exp);
        }
    }
    if is_vector {
        Ok(Expression::with_list(output))
    } else {
        Ok(Expression::cons_from_vec(&mut output))
    }
}

fn builtin_bquote(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            return match arg {
                Expression::Vector(list) => {
                    replace_commas(environment, &mut Box::new(list.borrow().iter()), true)
                }
                Expression::Pair(_, _) => replace_commas(environment, &mut arg.iter(), false),
                _ => Ok(arg.clone()),
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "bquote takes one form",
    ))
}

/*fn builtin_spawn(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let mut new_args: Vec<Expression> = Vec::with_capacity(args.len());
    for a in args {
        new_args.push(a.clone());
    }
    let mut data: HashMap<String, Expression> = HashMap::new();
    clone_symbols(
        &environment.current_scope.last().unwrap().borrow(),
        &mut data,
    );
    let _child = std::thread::spawn(move || {
        let mut enviro = build_new_spawn_scope(data);
        let _args = to_args(&mut enviro, &new_args).unwrap();
        if let Err(err) = reap_procs(&enviro) {
            eprintln!("Error waiting on spawned processes: {}", err);
        }
    });
    //let res = child.join()
    Ok(Expression::Atom(Atom::Nil))
}*/

fn builtin_and(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let mut last_exp = Expression::Atom(Atom::True);
    for arg in args {
        let arg = eval(environment, &arg)?;
        match arg {
            Expression::Atom(Atom::Nil) => return Ok(Expression::Atom(Atom::Nil)),
            _ => last_exp = arg,
        }
    }
    Ok(last_exp)
}

fn builtin_or(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    for arg in args {
        let arg = eval(environment, &arg)?;
        match arg {
            Expression::Atom(Atom::Nil) => {}
            _ => return Ok(arg),
        }
    }
    Ok(Expression::Atom(Atom::Nil))
}

fn builtin_not(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() != 1 {
        return Err(io::Error::new(io::ErrorKind::Other, "not takes one form"));
    }
    if let Expression::Atom(Atom::Nil) = &args[0] {
        Ok(Expression::Atom(Atom::True))
    } else {
        Ok(Expression::Atom(Atom::Nil))
    }
}

fn builtin_is_def(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, false)?;
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "is-def takes one form (symbol)",
        ));
    }
    if let Expression::Atom(Atom::Symbol(s)) = &args[0] {
        if is_expression(environment, &s) {
            Ok(Expression::Atom(Atom::True))
        } else {
            Ok(Expression::Atom(Atom::Nil))
        }
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "is-def takes a symbol to lookup",
        ))
    }
}

fn builtin_macro(
    _environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(params) = args.next() {
        if let Some(body) = args.next() {
            if args.next().is_none() {
                return Ok(Expression::Atom(Atom::Macro(Macro {
                    params: Box::new(params.clone()),
                    body: Box::new(body.clone()),
                })));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "macro can only have two forms (bindings and body)",
    ))
}

fn do_expansion(
    environment: &mut Environment,
    command: &Expression,
    parts: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Expression::Atom(Atom::Symbol(command)) = command {
        if let Some(exp) = get_expression(environment, &command) {
            if let Expression::Atom(Atom::Macro(sh_macro)) = &*exp {
                let new_scope = match environment.current_scope.last() {
                    Some(last) => build_new_scope(Some(last.clone())),
                    None => build_new_scope(None),
                };
                environment.current_scope.push(new_scope.clone());
                let args: Vec<Expression> = parts.cloned().collect();
                let ib: Box<(dyn Iterator<Item = &Expression>)> = Box::new(args.iter());
                if let Err(err) = setup_args(environment, None, &sh_macro.params, ib, false) {
                    environment.current_scope.pop();
                    return Err(err);
                }
                let expansion = eval(environment, &sh_macro.body);
                if let Err(err) = expansion {
                    environment.current_scope.pop();
                    return Err(err);
                }
                let expansion = expansion.unwrap();
                environment.current_scope.pop();
                Ok(expansion)
            } else {
                let msg = format!("expand-macro: {} not a macro", command);
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        } else {
            let msg = format!("expand-macro: {} not a macro", command);
            Err(io::Error::new(io::ErrorKind::Other, msg))
        }
    } else {
        let msg = format!(
            "expand-macro first item must be a symbol, found {}",
            command.to_string()
        );
        Err(io::Error::new(io::ErrorKind::Other, msg))
    }
}

fn builtin_expand_macro(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            return if let Expression::Vector(list) = arg0 {
                let list = list.borrow();
                let (command, parts) = match list.split_first() {
                    Some((c, p)) => (c, p),
                    None => {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "expand-macro needs the macro name and parameters",
                        ));
                    }
                };
                do_expansion(environment, command, &mut parts.iter())
            } else if let Expression::Pair(e1, e2) = arg0 {
                //let parts = exp_to_args(environment, &*e2.borrow(), false)?;
                do_expansion(environment, &e1.borrow(), &mut *e2.borrow().iter())
            } else {
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    "expand-macro can only have one form (list defining the macro call)",
                ))
            };
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "expand-macro can only have one form (list defining the macro call)",
    ))
}

fn builtin_recur(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let mut arg_list: Vec<Expression> = Vec::new();
    let mut arg_num = 0;
    for a in args {
        let a = eval(environment, a)?;
        arg_list.push(a);
        arg_num += 1;
    }
    environment.state.recur_num_args = Some(arg_num);
    Ok(Expression::with_list(arg_list))
}

fn builtin_gensym(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if !args.is_empty() {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "gensym takes to arguments",
        ))
    } else {
        let gensym_count = &mut environment.state.gensym_count;
        *gensym_count += 1;
        Ok(Expression::Atom(Atom::Symbol(format!(
            "gs::{}",
            *gensym_count
        ))))
    }
}

fn builtin_jobs(environment: &mut Environment, _args: &[Expression]) -> io::Result<Expression> {
    for (i, job) in environment.jobs.borrow().iter().enumerate() {
        println!(
            "[{}]\t{}\t{:?}\t{:?}",
            i,
            job.status.to_string(),
            job.pids,
            job.names
        );
    }
    Ok(Expression::Atom(Atom::Nil))
}

fn get_stopped_pid(environment: &mut Environment, args: &[Expression]) -> Option<u32> {
    if !args.is_empty() {
        let arg = &args[0];
        if let Expression::Atom(Atom::Int(ji)) = arg {
            let ji = *ji as usize;
            let jobs = &*environment.jobs.borrow();
            if ji < jobs.len() {
                let pid = jobs[ji].pids[0];
                let mut stop_idx: Option<u32> = None;
                for (i, sp) in environment.stopped_procs.borrow().iter().enumerate() {
                    if *sp == pid {
                        stop_idx = Some(i as u32);
                        break;
                    }
                }
                if let Some(idx) = stop_idx {
                    environment.stopped_procs.borrow_mut().remove(idx as usize);
                }
                Some(pid)
            } else {
                eprintln!("Error job id out of range.");
                None
            }
        } else {
            eprintln!("Error job id must be integer.");
            None
        }
    } else {
        environment.stopped_procs.borrow_mut().pop()
    }
}

fn builtin_bg(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() > 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "bg can only have one optional form (job id)",
        ))
    } else {
        let opid = get_stopped_pid(environment, &args);
        if let Some(pid) = opid {
            let ppid = Pid::from_raw(pid as i32);
            if let Err(err) = signal::kill(ppid, Signal::SIGCONT) {
                eprintln!("Error sending sigcont to wake up process: {}.", err);
            } else {
                mark_job_running(environment, pid);
            }
        }
        Ok(Expression::Atom(Atom::Nil))
    }
}

fn builtin_fg(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() > 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "fg can only have one optional form (job id)",
        ))
    } else {
        let opid = get_stopped_pid(environment, &args);
        if let Some(pid) = opid {
            let term_settings = termios::tcgetattr(nix::libc::STDIN_FILENO).unwrap();
            let ppid = Pid::from_raw(pid as i32);
            if let Err(err) = signal::kill(ppid, Signal::SIGCONT) {
                eprintln!("Error sending sigcont to wake up process: {}.", err);
            } else {
                if let Err(err) = unistd::tcsetpgrp(nix::libc::STDIN_FILENO, ppid) {
                    let msg = format!("Error making {} foreground in parent: {}", pid, err);
                    eprintln!("{}", msg);
                }
                mark_job_running(environment, pid);
                wait_pid(environment, pid, Some(&term_settings));
            }
        }
        Ok(Expression::Atom(Atom::Nil))
    }
}

fn builtin_version(
    _environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if args.next().is_some() {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "version takes no arguments",
        ))
    } else {
        Ok(Expression::Atom(Atom::String(VERSION_STRING.to_string())))
    }
}

fn builtin_command(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let old_form = environment.form_type;
    environment.form_type = FormType::ExternalOnly;
    let mut last_eval = Ok(Expression::Atom(Atom::Nil));
    for a in args {
        last_eval = eval(environment, a);
        if let Err(err) = last_eval {
            environment.form_type = old_form;
            return Err(err);
        }
    }
    environment.form_type = old_form;
    last_eval
}

fn builtin_run_bg(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    environment.run_background = true;
    let mut last_eval = Ok(Expression::Atom(Atom::Nil));
    for a in args {
        last_eval = eval(environment, a);
        if let Err(err) = last_eval {
            environment.run_background = false;
            return Err(err);
        }
    }
    environment.run_background = false;
    last_eval
}

fn builtin_form(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let old_form = environment.form_type;
    environment.form_type = FormType::FormOnly;
    let mut last_eval = Ok(Expression::Atom(Atom::Nil));
    for a in args {
        last_eval = eval(environment, a);
        if let Err(err) = last_eval {
            environment.form_type = old_form;
            return Err(err);
        }
    }
    environment.form_type = old_form;
    last_eval
}

fn builtin_loose_symbols(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    let old_loose_syms = environment.loose_symbols;
    environment.loose_symbols = true;
    let mut last_eval = Ok(Expression::Atom(Atom::Nil));
    for a in args {
        last_eval = eval(environment, a);
        if let Err(err) = last_eval {
            environment.loose_symbols = old_loose_syms;
            return Err(err);
        }
    }
    environment.loose_symbols = old_loose_syms;
    last_eval
}

fn builtin_exit(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = list_to_args(environment, args, true)?;
    if args.len() > 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "exit can only take an optional integer (exit code- defaults to 0)",
        ))
    } else if args.len() == 1 {
        if let Expression::Atom(Atom::Int(exit_code)) = &args[0] {
            environment.exit_code = Some(*exit_code as i32);
            Ok(Expression::Atom(Atom::Nil))
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "exit can only take an optional integer (exit code- defaults to 0)",
            ))
        }
    } else {
        environment.exit_code = Some(0);
        Ok(Expression::Atom(Atom::Nil))
    }
}

fn builtin_ns_create(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if environment
        .current_scope
        .last()
        .unwrap()
        .borrow()
        .name
        .is_none()
    {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "ns-create can only create a namespace when not in a lexical scope",
        ));
    }
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = match eval(environment, key)? {
                Expression::Atom(Atom::Symbol(sym)) => sym,
                Expression::Atom(Atom::String(s)) => s,
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "ns-create: namespace must be a symbol or string",
                    ))
                }
            };
            let scope = match build_new_namespace(environment, &key) {
                Ok(scope) => scope,
                Err(msg) => return Err(io::Error::new(io::ErrorKind::Other, msg)),
            };
            environment.current_scope.push(scope);
            return Ok(Expression::Atom(Atom::Nil));
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "ns-create takes one arg, the name of the new namespace",
    ))
}

fn builtin_ns_enter(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = &Expression>,
) -> io::Result<Expression> {
    if environment
        .current_scope
        .last()
        .unwrap()
        .borrow()
        .name
        .is_none()
    {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "ns-enter can only enter a namespace when not in a lexical scope",
        ));
    }
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = match eval(environment, key)? {
                Expression::Atom(Atom::Symbol(sym)) => sym,
                Expression::Atom(Atom::String(s)) => s,
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "ns-enter: namespace must be a symbol or string",
                    ))
                }
            };
            let scope = match get_namespace(environment, &key) {
                Some(scope) => scope,
                None => {
                    let msg = format!("Error, namespace {} does not exist!", key);
                    return Err(io::Error::new(io::ErrorKind::Other, msg));
                }
            };
            environment.current_scope.push(scope);
            return Ok(Expression::Atom(Atom::Nil));
        }
    }
    Err(io::Error::new(
        io::ErrorKind::Other,
        "ns-enter takes one arg, the name of the namespace to enter",
    ))
}

macro_rules! ensure_tonicity {
    ($check_fn:expr, $values:expr, $type:ty, $type_two:ty) => {{
        let first = $values.first().ok_or(io::Error::new(
            io::ErrorKind::Other,
            "expected at least one value",
        ))?;
        let rest = &$values[1..];
        fn f(prev: $type, xs: &[$type_two]) -> bool {
            match xs.first() {
                Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
                None => true,
            }
        };
        if f(first, rest) {
            Ok(Expression::Atom(Atom::True))
        } else {
            Ok(Expression::Atom(Atom::Nil))
        }
    }};
}

macro_rules! ensure_tonicity_all {
    ($check_fn:expr) => {{
        |environment: &mut Environment, args: &[Expression]| -> io::Result<Expression> {
            let mut args: Vec<Expression> = list_to_args(environment, args, true)?;
            if let Ok(ints) = parse_list_of_ints(environment, &mut args) {
                ensure_tonicity!($check_fn, ints, &i64, i64)
            } else if let Ok(floats) = parse_list_of_floats(environment, &mut args) {
                ensure_tonicity!($check_fn, floats, &f64, f64)
            } else {
                let strings = parse_list_of_strings(environment, &mut args)?;
                ensure_tonicity!($check_fn, strings, &str, String)
            }
        }
    }};
}

pub fn add_builtins<S: BuildHasher>(data: &mut HashMap<String, Rc<Expression>, S>) {
    data.insert(
        "eval".to_string(),
        Rc::new(Expression::make_function(
            builtin_eval,
            "Evalute the provided expression",
        )),
    );
    data.insert(
        "fncall".to_string(),
        Rc::new(Expression::make_function(
            builtin_fncall,
            "Call the provided function with the suplied arguments",
        )),
    );
    data.insert(
        "apply".to_string(),
        Rc::new(Expression::make_function(
            builtin_apply,
            "Call the provided function with the suplied arguments, last is a list that will be expanded",
        )),
    );
    data.insert(
        "unwind-protect".to_string(),
        Rc::new(Expression::make_function(
            builtin_unwind_protect,
            "After evaluation first form, make sure the following cleanup forms run (returns first form's result)"
        )),
    );
    data.insert(
        "err".to_string(),
        Rc::new(Expression::make_function(
            builtin_err,
            "Raise an error with the supplied message",
        )),
    );
    data.insert(
        "load".to_string(),
        Rc::new(Expression::make_function(
            builtin_load,
            "Read and eval a file.",
        )),
    );
    data.insert(
        "length".to_string(),
        Rc::new(Expression::make_function(
            builtin_length,
            "Return length of suplied expression.",
        )),
    );
    data.insert(
        "if".to_string(),
        Rc::new(Expression::make_special(
            builtin_if,
            "If then else conditional.",
        )),
    );
    data.insert(
        "print".to_string(),
        Rc::new(Expression::make_function(
            builtin_print,
            "Print the arguments.",
        )),
    );
    data.insert(
        "println".to_string(),
        Rc::new(Expression::make_function(
            builtin_println,
            "Print the arguments and then a newline.",
        )),
    );
    data.insert(
        "eprint".to_string(),
        Rc::new(Expression::make_function(
            builtin_eprint,
            "Print the arguments to stderr.",
        )),
    );
    data.insert(
        "eprintln".to_string(),
        Rc::new(Expression::make_function(
            builtin_eprintln,
            "Print the arguments to stderr and then a newline.",
        )),
    );
    data.insert(
        "format".to_string(),
        Rc::new(Expression::make_function(
            builtin_format,
            "Build a formatted string from arguments.",
        )),
    );
    data.insert(
        "progn".to_string(),
        Rc::new(Expression::Func(builtin_progn)),
    );
    data.insert("set".to_string(), Rc::new(Expression::Func(builtin_set)));
    data.insert(
        "export".to_string(),
        Rc::new(Expression::Func(builtin_export)),
    );
    data.insert(
        "unexport".to_string(),
        Rc::new(Expression::make_function(
            builtin_unexport,
            "Remove a var from the current shell environment.",
        )),
    );
    data.insert("def".to_string(), Rc::new(Expression::Func(builtin_def)));
    data.insert(
        "undef".to_string(),
        Rc::new(Expression::make_function(
            builtin_undef,
            "Remove a symbol from the current scope (if it exists).",
        )),
    );
    data.insert("dyn".to_string(), Rc::new(Expression::Func(builtin_dyn)));
    data.insert(
        "is-global-scope".to_string(),
        Rc::new(Expression::Func(builtin_is_global_scope)),
    );
    data.insert(
        "to-symbol".to_string(),
        Rc::new(Expression::Func(builtin_to_symbol)),
    );
    data.insert("fn".to_string(), Rc::new(Expression::Func(builtin_fn)));
    data.insert(
        "quote".to_string(),
        Rc::new(Expression::make_special(builtin_quote, "")),
    );
    data.insert(
        "bquote".to_string(),
        Rc::new(Expression::make_special(builtin_bquote, "")),
    );
    data.insert(
        "and".to_string(),
        Rc::new(Expression::make_special(builtin_and, "")),
    );
    data.insert(
        "or".to_string(),
        Rc::new(Expression::make_special(builtin_or, "")),
    );
    data.insert("not".to_string(), Rc::new(Expression::Func(builtin_not)));
    data.insert("null".to_string(), Rc::new(Expression::Func(builtin_not)));
    data.insert(
        "is-def".to_string(),
        Rc::new(Expression::Func(builtin_is_def)),
    );
    data.insert(
        "macro".to_string(),
        Rc::new(Expression::make_function(builtin_macro, "Define a macro.")),
    );
    data.insert(
        "expand-macro".to_string(),
        Rc::new(Expression::make_special(builtin_expand_macro, "")),
    );
    data.insert(
        "recur".to_string(),
        Rc::new(Expression::make_function(builtin_recur, "")),
    );
    data.insert(
        "gensym".to_string(),
        Rc::new(Expression::Func(builtin_gensym)),
    );
    data.insert("jobs".to_string(), Rc::new(Expression::Func(builtin_jobs)));
    data.insert("bg".to_string(), Rc::new(Expression::Func(builtin_bg)));
    data.insert("fg".to_string(), Rc::new(Expression::Func(builtin_fg)));
    data.insert(
        "version".to_string(),
        Rc::new(Expression::make_function(
            builtin_version,
            "Produce executable version as string.",
        )),
    );
    data.insert(
        "command".to_string(),
        Rc::new(Expression::make_special(
            builtin_command,
            "Only execute system commands not forms within this form.",
        )),
    );
    data.insert(
        "run-bg".to_string(),
        Rc::new(Expression::make_special(
            builtin_run_bg,
            "Any system commands started within form will be in the background.",
        )),
    );
    data.insert(
        "form".to_string(),
        Rc::new(Expression::make_special(
            builtin_form,
            "Do not execute system commands within this form.",
        )),
    );
    data.insert(
        "loose-symbols".to_string(),
        Rc::new(Expression::make_special(
            builtin_loose_symbols,
            "Within this form any undefined symbols become strings.",
        )),
    );
    data.insert("exit".to_string(), Rc::new(Expression::Func(builtin_exit)));
    data.insert(
        "ns-create".to_string(),
        Rc::new(Expression::make_function(
            builtin_ns_create,
            "Creates and enters a new a namespace.",
        )),
    );
    data.insert(
        "ns-enter".to_string(),
        Rc::new(Expression::make_function(
            builtin_ns_enter,
            "Enters an existing namespace.",
        )),
    );

    data.insert(
        "=".to_string(),
        Rc::new(Expression::Func(
            |environment: &mut Environment, args: &[Expression]| -> io::Result<Expression> {
                let mut args: Vec<Expression> = to_args(environment, args)?;
                if let Ok(ints) = parse_list_of_ints(environment, &mut args) {
                    ensure_tonicity!(|a, b| a == b, ints, &i64, i64)
                } else if let Ok(floats) = parse_list_of_floats(environment, &mut args) {
                    ensure_tonicity!(|a, b| ((a - b) as f64).abs() < 0.000_001, floats, &f64, f64)
                } else {
                    let strings = parse_list_of_strings(environment, &mut args)?;
                    ensure_tonicity!(|a, b| a == b, strings, &str, String)
                }
            },
        )),
    );
    data.insert(
        ">".to_string(),
        Rc::new(Expression::Func(ensure_tonicity_all!(|a, b| a > b))),
    );
    data.insert(
        ">=".to_string(),
        Rc::new(Expression::Func(ensure_tonicity_all!(|a, b| a >= b))),
    );
    data.insert(
        "<".to_string(),
        Rc::new(Expression::Func(ensure_tonicity_all!(|a, b| a < b))),
    );
    data.insert(
        "<=".to_string(),
        Rc::new(Expression::Func(ensure_tonicity_all!(|a, b| a <= b))),
    );
}
