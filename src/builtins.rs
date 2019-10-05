use nix::{
    sys::{
        signal::{self, Signal},
        termios,
    },
    unistd::{self, Pid},
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::hash::BuildHasher;
use std::io;
use std::rc::Rc;

use crate::builtins_util::*;
use crate::config::VERSION_STRING;
use crate::environment::*;
use crate::process::*;
use crate::reader::*;
use crate::shell::*;
use crate::types::*;

fn builtin_eval(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "eval can only have one form",
        ))
    } else {
        let args = to_args(environment, args)?;
        match &args[0] {
            Expression::Atom(Atom::String(s)) => match read(&s) {
                Ok(ast) => eval(environment, &ast),
                Err(err) => Err(io::Error::new(io::ErrorKind::Other, err.reason)),
            },
            _ => eval(environment, &args[0]),
        }
    }
}

fn builtin_load(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let mut args: Vec<Expression> = to_args(environment, args)?;
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "load needs one argument",
        ))
    } else {
        let contents = fs::read_to_string(args.pop().unwrap().make_string(environment)?)?;
        let ast = read(&contents);
        match ast {
            Ok(ast) => {
                let ast = match ast {
                    Expression::List(olist) => {
                        let mut list = olist.borrow_mut();
                        if let Some(first) = list.get(0) {
                            match first {
                                Expression::List(_) => {
                                    let mut v = Vec::with_capacity(list.len() + 1);
                                    v.push(Expression::Atom(Atom::Symbol("progn".to_string())));
                                    for l in list.drain(..) {
                                        v.push(l);
                                    }
                                    Expression::with_list(v)
                                }
                                _ => {
                                    drop(list);
                                    Expression::List(olist)
                                }
                            }
                        } else {
                            drop(list);
                            Expression::List(olist)
                        }
                    }
                    _ => ast,
                };
                eval(environment, &ast)
            }
            Err(err) => Err(io::Error::new(io::ErrorKind::Other, err.reason)),
        }
    }
}

fn builtin_if(environment: &mut Environment, parts: &[Expression]) -> io::Result<Expression> {
    let plen = parts.len();
    if plen != 2 && plen != 3 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "if needs exactly two or three expressions",
        ))
    } else {
        let mut parts = parts.iter();
        match eval(environment, parts.next().unwrap())? {
            Expression::Atom(Atom::True) => eval(environment, parts.next().unwrap()),
            Expression::Atom(Atom::Nil) => {
                if plen == 3 {
                    parts.next().unwrap();
                    eval(environment, parts.next().unwrap())
                } else {
                    Ok(Expression::Atom(Atom::Nil))
                }
            }
            _ => Err(io::Error::new(
                io::ErrorKind::Other,
                "if must evaluate to true or false",
            )),
        }
    }
}

fn builtin_print(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args: Vec<Expression> = to_args(environment, args)?;
    print(environment, &args, false)
}

fn builtin_println(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args: Vec<Expression> = to_args(environment, args)?;
    print(environment, &args, true)
}

fn builtin_format(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let args = to_args_str(environment, args)?;
    let mut res = String::new();
    for a in args {
        res.push_str(&a);
    }
    Ok(Expression::Atom(Atom::String(res)))
}

pub fn builtin_progn(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let mut last_eval = Expression::Atom(Atom::Nil);
    for a in args {
        last_eval = eval(environment, a)?;
    }
    Ok(last_eval)
}

fn builtin_set(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "set can only have two expressions",
        ))
    } else {
        let mut args = args.iter();
        let key = eval(environment, args.next().unwrap())?;
        let key = match key {
            Expression::Atom(Atom::Symbol(s)) => s.clone(),
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "set's first form must evaluate to a symbol",
                ));
            }
        };
        if let Some(scope) = get_symbols_scope(environment, &key) {
            let val = eval(environment, args.next().unwrap())?;
            let mut val = match val {
                Expression::Atom(atom) => Expression::Atom(atom),
                Expression::List(list) => Expression::List(list),
                Expression::Process(ProcessState::Running(_pid)) => Expression::Atom(Atom::String(
                    val.make_string(environment)
                        .unwrap_or_else(|_| "PROCESS FAILED".to_string()),
                )),
                Expression::Process(ProcessState::Over(_pid, _exit_status)) => {
                    Expression::Atom(Atom::String(
                        val.make_string(environment)
                            .unwrap_or_else(|_| "PROCESS FAILED".to_string()),
                    ))
                }
                Expression::Func(_) => Expression::Atom(Atom::String("::FUNCTION::".to_string())),
            };
            if key.starts_with('$') {
                // Should use export, force this?
                let val = val.make_string(environment)?;
                let val = match expand_tilde(&val) {
                    Some(v) => v,
                    None => val,
                };
                if !val.is_empty() {
                    env::set_var(key[1..].to_string(), val);
                } else {
                    env::remove_var(key[1..].to_string());
                }
            } else {
                if let Expression::Atom(Atom::String(vs)) = val {
                    let vs = match expand_tilde(&vs) {
                        Some(v) => v,
                        None => vs,
                    };
                    val = Expression::Atom(Atom::String(vs));
                }
                scope.borrow_mut().data.insert(key, Rc::new(val.clone()));
            }
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
    if args.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "export can only have two expressions",
        ))
    } else {
        let mut args = args.iter();
        let key = eval(environment, args.next().unwrap())?;
        let key = match key {
            Expression::Atom(Atom::Symbol(s)) => s.clone(),
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "export's first form must evaluate to a symbol",
                ));
            }
        };
        let val = eval(environment, args.next().unwrap())?;
        let val = match val {
            Expression::Atom(atom) => Expression::Atom(atom),
            Expression::List(list) => Expression::List(list),
            Expression::Process(ProcessState::Running(_pid)) => Expression::Atom(Atom::String(
                val.make_string(environment)
                    .unwrap_or_else(|_| "PROCESS FAILED".to_string()),
            )),
            Expression::Process(ProcessState::Over(_pid, _exit_status)) => {
                Expression::Atom(Atom::String(
                    val.make_string(environment)
                        .unwrap_or_else(|_| "PROCESS FAILED".to_string()),
                ))
            }
            Expression::Func(_) => Expression::Atom(Atom::String("::FUNCTION::".to_string())),
        };
        let val = val.make_string(environment)?;
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

fn builtin_def(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 2 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "def can only have two expressions",
        ))
    } else {
        let mut args = args.iter();
        let key = eval(environment, args.next().unwrap())?;
        let key = match key {
            Expression::Atom(Atom::Symbol(s)) => s.clone(),
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "def's first form must evaluate to a symbol",
                ));
            }
        };
        let val = eval(environment, args.next().unwrap())?;
        let mut val = match val {
            Expression::Atom(atom) => Expression::Atom(atom),
            Expression::List(list) => Expression::List(list),
            Expression::Process(ProcessState::Running(_pid)) => Expression::Atom(Atom::String(
                val.make_string(environment)
                    .unwrap_or_else(|_| "PROCESS FAILED".to_string()),
            )),
            Expression::Process(ProcessState::Over(_pid, _exit_status)) => {
                Expression::Atom(Atom::String(
                    val.make_string(environment)
                        .unwrap_or_else(|_| "PROCESS FAILED".to_string()),
                ))
            }
            Expression::Func(_) => Expression::Atom(Atom::String("::FUNCTION::".to_string())),
        };
        if let Expression::Atom(Atom::String(vs)) = val {
            let vs = match expand_tilde(&vs) {
                Some(v) => v,
                None => vs,
            };
            val = Expression::Atom(Atom::String(vs));
        }
        set_expression_global(environment, key, Rc::new(val.clone()));
        Ok(val)
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

fn builtin_let(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() < 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "let requires at least two forms",
        ));
    }
    let mut data: HashMap<String, Rc<Expression>> = HashMap::new();
    match &args[0] {
        Expression::Atom(Atom::Nil) => {}
        Expression::List(list) => {
            for binding in list.borrow().iter() {
                if let Expression::List(binding_pair) = binding {
                    let binding_pair = binding_pair.borrow();
                    if binding_pair.is_empty() || binding_pair.len() > 2 {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "let bindings must be a symbol and/or a form",
                        ));
                    }
                    if let Expression::Atom(Atom::Symbol(s)) = binding_pair.get(0).unwrap() {
                        if binding_pair.len() == 2 {
                            data.insert(
                                s.clone(),
                                Rc::new(eval(environment, binding_pair.get(1).unwrap())?),
                            );
                        } else {
                            data.insert(s.clone(), Rc::new(Expression::Atom(Atom::Nil)));
                        }
                    }
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        "let bindings must be lists",
                    ));
                }
            }
        }
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "let first form must be a list",
            ))
        }
    }
    let new_scope = Rc::new(RefCell::new(Scope::with_data(Some(environment), data)));
    environment.current_scope.push(new_scope.clone());
    let mut args = match to_args(environment, &args[1..]) {
        Ok(args) => args,
        Err(err) => {
            environment.current_scope.pop();
            return Err(err);
        }
    };
    environment.current_scope.pop();
    match args.pop() {
        Some(a) => Ok(a),
        None => Ok(Expression::Atom(Atom::Nil)),
    }
}

fn builtin_quote(_environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(io::ErrorKind::Other, "quote takes one form"));
    }
    Ok(args.get(0).unwrap().clone())
}

fn replace_commas(environment: &mut Environment, list: &[Expression]) -> io::Result<Expression> {
    let mut output: Vec<Expression> = Vec::with_capacity(list.len());
    let mut comma_next = false;
    let mut amp_next = false;
    for exp in list {
        let exp = if let Expression::List(tlist) = exp {
            replace_commas(environment, &tlist.borrow())?
        } else {
            exp.clone()
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
                if let Expression::List(new_list) = nl {
                    for item in new_list.borrow_mut().drain(..) {
                        output.push(item);
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
            if let Expression::List(new_list) = nl {
                for item in new_list.borrow_mut().drain(..) {
                    output.push(item);
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
    Ok(Expression::with_list(output))
}

fn builtin_bquote(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "bquote takes one form",
        ));
    }
    if let Expression::List(list) = &args[0] {
        replace_commas(environment, &list.borrow())
    } else {
        Ok(args.get(0).unwrap().clone())
    }
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

fn builtin_and(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() < 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "and needs at least two forms",
        ));
    }
    let mut last_exp = Expression::Atom(Atom::Nil);
    for arg in args {
        let val = eval(environment, arg)?;
        match val {
            Expression::Atom(Atom::Nil) => return Ok(Expression::Atom(Atom::Nil)),
            _ => last_exp = val,
        }
    }
    Ok(last_exp)
}

fn builtin_or(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() < 2 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "or needs at least two forms",
        ));
    }
    for arg in args {
        let val = eval(environment, arg)?;
        match val {
            Expression::Atom(Atom::Nil) => {}
            _ => return Ok(val),
        }
    }
    Ok(Expression::Atom(Atom::Nil))
}

fn builtin_not(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(io::ErrorKind::Other, "not takes one form"));
    }
    let val = eval(environment, &args[0])?;
    if let Expression::Atom(Atom::Nil) = val {
        Ok(Expression::Atom(Atom::True))
    } else {
        Ok(Expression::Atom(Atom::Nil))
    }
}

fn builtin_is_def(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
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

fn builtin_get_type(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "get-type takes one form",
        ));
    }
    let arg0 = eval(environment, &args[0])?;
    Ok(Expression::Atom(Atom::String(arg0.display_type())))
}

fn builtin_defmacro(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 3 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "defmacro can only have three forms (symbol, bindings and body)",
        ))
    } else {
        let mut args = args.iter();
        let name = args.next().unwrap();
        let params = args.next().unwrap();
        let body = args.next().unwrap();
        if let Expression::Atom(Atom::Symbol(s)) = name {
            let m = Rc::new(Expression::Atom(Atom::Macro(Macro {
                params: Box::new(params.clone()),
                body: Box::new(body.clone()),
            })));
            set_expression_global(environment, s.clone(), m);
            Ok(Expression::Atom(Atom::Nil))
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "defmacro first argument must be a symbol",
            ))
        }
    }
}

fn builtin_expand_macro(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "expand-macro can only have one form (list defining the macro call)",
        ))
    } else if let Expression::List(list) = &args[0] {
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
        if let Expression::Atom(Atom::Symbol(command)) = command {
            if let Some(exp) = get_expression(environment, &command) {
                if let Expression::Atom(Atom::Macro(sh_macro)) = &*exp {
                    let new_scope = match environment.current_scope.last() {
                        Some(last) => build_new_scope(Some(last.clone())),
                        None => build_new_scope(None),
                    };
                    environment.current_scope.push(new_scope.clone());
                    if let Err(err) = setup_args(environment, None, &sh_macro.params, parts, false)
                    {
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
            Err(io::Error::new(
                io::ErrorKind::Other,
                "expand-macro first item must be a symbol",
            ))
        }
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "expand-macro can only have one form (list defining the macro call)",
        ))
    }
}

fn builtin_recur(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    let mut args = to_args(environment, args)?;
    let mut arg_list: Vec<Expression> = Vec::with_capacity(args.len());
    for a in args.drain(..) {
        arg_list.push(a.clone());
    }
    environment.state.recur_num_args = Some(arg_list.len());
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
    for pid in environment.stopped_procs.borrow().iter() {
        println!("{}", pid);
    }
    Ok(Expression::Atom(Atom::Nil))
}

fn builtin_fg(environment: &mut Environment, _args: &[Expression]) -> io::Result<Expression> {
    let mut opid: Option<u32> = None;
    if let Some(pid) = environment.stopped_procs.borrow_mut().pop() {
        opid = Some(pid);
    }
    if let Some(pid) = opid {
        let term_settings = termios::tcgetattr(nix::libc::STDIN_FILENO).unwrap();
        let ppid = Pid::from_raw(pid as i32);
        if let Err(err) = signal::kill(ppid, Signal::SIGCONT) {
            eprintln!("Error sending sigcont to wake up process: {}.", err);
        } else {
            if let Err(err) = unistd::tcsetpgrp(nix::libc::STDIN_FILENO, ppid) {
                let msg = format!("Error making {} foreground in parent: {}", pid, err);
                eprintln!("{}", msg);
                //return Err(io::Error::new(io::ErrorKind::Other, msg));
            }
            wait_pid(environment, pid, Some(&term_settings));
        }
    }
    Ok(Expression::Atom(Atom::Nil))
}

fn builtin_version(_environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if !args.is_empty() {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "version takes to arguments",
        ))
    } else {
        Ok(Expression::Atom(Atom::String(VERSION_STRING.to_string())))
    }
}

fn builtin_command(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "command can only have one form (call defining the external command and arguments)",
        ))
    } else if let Expression::List(list) = &args[0] {
        let list = list.borrow();
        let (command, parts) = match list.split_first() {
            Some((c, p)) => (c, p),
            None => {
                eprintln!("No valid command.");
                return Err(io::Error::new(io::ErrorKind::Other, "No valid command."));
            }
        };
        match command {
            Expression::Atom(Atom::Symbol(command)) => {
                if command.is_empty() {
                    return Ok(Expression::Atom(Atom::Nil));
                }
                do_command(environment, command, parts)
            }
            _ => {
                let msg = format!(
                    "Not a valid command {}, must be a symbol.",
                    command.make_string(environment)?
                );
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        }
    } else {
        Err(io::Error::new(io::ErrorKind::Other, "command takes a list"))
    }
}

fn builtin_run_bg(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "run-bg can only have one form (call defining the external command and arguments)",
        ))
    } else if let Expression::List(list) = &args[0] {
        let list = list.borrow();
        let (command, parts) = match list.split_first() {
            Some((c, p)) => (c, p),
            None => {
                eprintln!("No valid command.");
                return Err(io::Error::new(io::ErrorKind::Other, "No valid command."));
            }
        };
        match command {
            Expression::Atom(Atom::Symbol(command)) => {
                if command.is_empty() {
                    return Ok(Expression::Atom(Atom::Nil));
                }
                environment.run_background = true;
                let result = do_command(environment, command, parts);
                environment.run_background = false;
                result
            }
            _ => {
                let msg = format!(
                    "Not a valid command {}, must be a symbol.",
                    command.make_string(environment)?
                );
                Err(io::Error::new(io::ErrorKind::Other, msg))
            }
        }
    } else {
        Err(io::Error::new(io::ErrorKind::Other, "run-bg takes a list"))
    }
}

fn builtin_form(environment: &mut Environment, args: &[Expression]) -> io::Result<Expression> {
    if args.len() != 1 {
        Err(io::Error::new(
            io::ErrorKind::Other,
            "form can only have one form (call defining the form and arguments)",
        ))
    } else if let Expression::List(_list) = &args[0] {
        let old_form = environment.form_type;
        environment.form_type = FormType::FormOnly;
        let result = eval(environment, &args[0]);
        environment.form_type = old_form;
        result
    } else {
        Err(io::Error::new(io::ErrorKind::Other, "form takes a list"))
    }
}

fn builtin_loose_symbols(
    environment: &mut Environment,
    args: &[Expression],
) -> io::Result<Expression> {
    let old_loose_syms = environment.loose_symbols;
    environment.loose_symbols = true;
    let mut last_eval = Expression::Atom(Atom::Nil);
    for a in args {
        last_eval = eval(environment, a)?;
    }
    environment.loose_symbols = old_loose_syms;
    Ok(last_eval)
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
            let mut args: Vec<Expression> = to_args(environment, args)?;
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
    data.insert("eval".to_string(), Rc::new(Expression::Func(builtin_eval)));
    data.insert("load".to_string(), Rc::new(Expression::Func(builtin_load)));
    data.insert("if".to_string(), Rc::new(Expression::Func(builtin_if)));
    data.insert(
        "print".to_string(),
        Rc::new(Expression::Func(builtin_print)),
    );
    data.insert(
        "println".to_string(),
        Rc::new(Expression::Func(builtin_println)),
    );
    data.insert(
        "format".to_string(),
        Rc::new(Expression::Func(builtin_format)),
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
    data.insert("def".to_string(), Rc::new(Expression::Func(builtin_def)));
    data.insert("fn".to_string(), Rc::new(Expression::Func(builtin_fn)));
    data.insert("let".to_string(), Rc::new(Expression::Func(builtin_let)));
    data.insert(
        "quote".to_string(),
        Rc::new(Expression::Func(builtin_quote)),
    );
    data.insert(
        "bquote".to_string(),
        Rc::new(Expression::Func(builtin_bquote)),
    );
    data.insert("and".to_string(), Rc::new(Expression::Func(builtin_and)));
    data.insert("or".to_string(), Rc::new(Expression::Func(builtin_or)));
    data.insert("not".to_string(), Rc::new(Expression::Func(builtin_not)));
    data.insert("null".to_string(), Rc::new(Expression::Func(builtin_not)));
    data.insert(
        "is-def".to_string(),
        Rc::new(Expression::Func(builtin_is_def)),
    );
    data.insert(
        "get-type".to_string(),
        Rc::new(Expression::Func(builtin_get_type)),
    );
    data.insert(
        "defmacro".to_string(),
        Rc::new(Expression::Func(builtin_defmacro)),
    );
    data.insert(
        "expand-macro".to_string(),
        Rc::new(Expression::Func(builtin_expand_macro)),
    );
    data.insert(
        "recur".to_string(),
        Rc::new(Expression::Func(builtin_recur)),
    );
    data.insert(
        "gensym".to_string(),
        Rc::new(Expression::Func(builtin_gensym)),
    );
    data.insert("jobs".to_string(), Rc::new(Expression::Func(builtin_jobs)));
    data.insert("fg".to_string(), Rc::new(Expression::Func(builtin_fg)));
    data.insert(
        "version".to_string(),
        Rc::new(Expression::Func(builtin_version)),
    );
    data.insert(
        "command".to_string(),
        Rc::new(Expression::Func(builtin_command)),
    );
    data.insert(
        "run-bg".to_string(),
        Rc::new(Expression::Func(builtin_run_bg)),
    );
    data.insert("form".to_string(), Rc::new(Expression::Func(builtin_form)));
    data.insert(
        "loose-symbols".to_string(),
        Rc::new(Expression::Func(builtin_loose_symbols)),
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
