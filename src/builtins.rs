use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::hash::BuildHasher;
use std::io::{self, Write};
use std::path::Path;
use std::rc::Rc;
use std::str::from_utf8;

use unicode_segmentation::UnicodeSegmentation;

use crate::builtins_util::*;
use crate::config::VERSION_STRING;
use crate::environment::*;
use crate::eval::*;
use crate::gc::*;
use crate::interner::*;
use crate::pretty_print::*;
use crate::reader::*;
use crate::symbols::*;
use crate::types::*;

const CORE_LISP: &[u8] = include_bytes!("../lisp/core.lisp");
const STRUCT_LISP: &[u8] = include_bytes!("../lisp/struct.lisp");
const ITERATOR_LISP: &[u8] = include_bytes!("../lisp/iterator.lisp");
const COLLECTION_LISP: &[u8] = include_bytes!("../lisp/collection.lisp");
const SEQ_LISP: &[u8] = include_bytes!("../lisp/seq.lisp");
const SHELL_LISP: &[u8] = include_bytes!("../lisp/shell.lisp");
const ENDFIX_LISP: &[u8] = include_bytes!("../lisp/endfix.lisp");
const GETOPTS_LISP: &[u8] = include_bytes!("../lisp/getopts.lisp");
const TEST_LISP: &[u8] = include_bytes!("../lisp/test.lisp");
const SLSH_STD_LISP: &[u8] = include_bytes!("../lisp/slsh-std.lisp");
const SLSHRC: &[u8] = include_bytes!("../lisp/slshrc");

fn builtin_eval(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            let arg_d = arg.get();
            // XXX TODO- do not do anything special with strings, the calling code should use read
            // on them itself...
            let ret = match &arg_d.data {
                ExpEnum::String(s, _) => match read(environment, &s, None, false) {
                    Ok(ast) => {
                        drop(arg_d);
                        eval(environment, ast)
                    }
                    Err(err) => Err(LispError::new(err.reason)),
                },
                _ => {
                    drop(arg_d);
                    eval(environment, &arg)
                }
            };
            return ret;
        }
    }
    Err(LispError::new("eval can only have one form"))
}

//XXX TODO- remove once eval str suppor is gone.
fn _builtin_eval_in_scope(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return eval(environment, &arg);
        }
    }
    Err(LispError::new("eval can only have one form"))
}

fn apply_fn_call(
    environment: &mut Environment,
    command: Expression,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    match command.get().data.clone() {
        ExpEnum::Symbol(command_sym, _) => {
            if let Some(exp) = get_expression(environment, command.clone()) {
                match exp.get().data.clone() {
                    ExpEnum::Function(c) if !c.is_special_form => {
                        let old_sup = environment.supress_eval;
                        environment.supress_eval = true;
                        let ret = (c.func)(environment, args); //&mut *args);
                        environment.supress_eval = old_sup;
                        ret
                    }
                    ExpEnum::Lambda(_) => {
                        if environment.allow_lazy_fn {
                            //                        make_lazy(environment, exp.clone(), args)
                            Ok(Expression::alloc(ExpObj {
                                data: ExpEnum::LazyFn(
                                    exp.clone().into(),
                                    args.map(|a| a.into()).collect(),
                                ),
                                meta: None,
                                meta_tags: None,
                                analyzed: RefCell::new(true),
                            }))
                        } else {
                            call_lambda(environment, exp.clone(), args, false)
                        }
                    }
                    _ => {
                        let msg = format!(
                            "Symbol {} of type {} is not callable (or is macro or special form).1",
                            command_sym,
                            command.display_type()
                        );
                        Err(LispError::new(msg))
                    }
                }
            } else {
                let msg = format!("Symbol {} not found.", command_sym,);
                Err(LispError::new(msg))
            }
        }
        ExpEnum::Lambda(_) => {
            if environment.allow_lazy_fn {
                //let mut parms: Vec<Handle> = Vec::collect(args);
                Ok(Expression::alloc(ExpObj {
                    data: ExpEnum::LazyFn(command.clone().into(), args.map(|a| a.into()).collect()),
                    meta: None,
                    meta_tags: None,
                    analyzed: RefCell::new(true),
                }))
            //make_lazy(environment, command.clone(), args)
            } else {
                call_lambda(environment, command.clone(), args, false)
            }
        }
        //ExpEnum::Macro(m) => exec_macro(environment, &m, args),
        ExpEnum::Function(c) if !c.is_special_form => {
            let old_sup = environment.supress_eval;
            environment.supress_eval = true;
            let ret = (c.func)(environment, args); //&mut *args);
            environment.supress_eval = old_sup;
            ret
        }
        _ => {
            let msg = format!(
                "Called an invalid command {}, type {}.",
                command.make_string(environment)?,
                command.display_type()
            );
            Err(LispError::new(msg))
        }
    }
}

fn builtin_apply(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut call_list: Vec<Handle> = Vec::new();
    let mut last_arg: Option<Expression> = None;
    for arg in args {
        if let Some(a) = last_arg {
            call_list.push(eval(environment, a)?.into());
        }
        last_arg = Some(arg);
    }
    let last_evaled;
    if let Some(alist) = last_arg {
        last_evaled = eval(environment, alist)?;
        let last_d = last_evaled.get();
        let itr = match &last_d.data {
            ExpEnum::Vector(list) => Box::new(ListIter::new_list(&list)),
            ExpEnum::Pair(_, _) => last_evaled.iter(),
            ExpEnum::Nil => last_evaled.iter(),
            _ => return Err(LispError::new("apply: last arg not a list")),
        };
        for a in itr {
            /*let b = ExpEnum::Pair(
                Expression::alloc_data_h(ExpEnum::Quote),
                Expression::alloc_data_h(ExpEnum::Pair(a.into(), Expression::make_nil_h())),
            );*/
            call_list.push(a.into()); //Expression::alloc_data_h(b));
        }
    }
    let mut args = box_slice_it(&call_list[..]);
    if let Some(command) = args.next() {
        apply_fn_call(environment, command, &mut args)
    } else {
        Err(LispError::new("apply: empty call"))
    }
}

fn builtin_unwind_protect(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(protected) = args.next() {
        let result = eval(environment, protected);
        for a in args {
            if let Err(err) = eval(environment, &a) {
                eprintln!(
                    "ERROR in unwind-protect cleanup form {}, {} will continue cleanup",
                    a, err
                );
            }
        }
        result
    } else {
        Ok(Expression::alloc_data(ExpEnum::Nil))
    }
}

fn builtin_err(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            return Err(LispError::new(arg.as_string(environment)?));
        }
    }
    Err(LispError::new("err can only have one form"))
}

pub fn load(environment: &mut Environment, file_name: &str) -> Result<Expression, LispError> {
    let file_name = match expand_tilde(&file_name) {
        Some(f) => f,
        None => file_name.to_string(),
    };
    let file_path = if let Some(lp) = lookup_expression(environment, "*load-path*") {
        let lp_d = lp.get();
        let p_itr = match &lp_d.data {
            ExpEnum::Vector(vec) => Box::new(ListIter::new_list(&vec)),
            _ => lp.iter(),
        };
        let mut path_out = file_name.clone();
        for l in p_itr {
            let path_name = match &l.get().data {
                ExpEnum::Symbol(sym, _) => Some((*sym).to_string()),
                ExpEnum::String(s, _) => Some(s.to_string()),
                _ => None,
            };
            if let Some(path_name) = path_name {
                let path_str = if path_name.ends_with('/') {
                    format!("{}{}", path_name, file_name)
                } else {
                    format!("{}/{}", path_name, file_name)
                };
                let path = Path::new(&path_str);
                if path.exists() {
                    path_out = path_str;
                    break;
                }
            }
        }
        path_out
    } else {
        file_name
    };
    let path = Path::new(&file_path);
    let file_name = Some(environment.interner.intern(&file_path));
    let ast = if path.exists() {
        let contents = fs::read_to_string(file_path)?;
        read_list_wrap(environment, &contents, file_name)
    } else {
        fn to_str(input: &'static [u8]) -> &'static str {
            from_utf8(input).expect("Builtin file is not valid UTF8!")
        }
        match &file_path[..] {
            "core.lisp" => read_list_wrap(environment, to_str(CORE_LISP), file_name),
            "struct.lisp" => read_list_wrap(environment, to_str(STRUCT_LISP), file_name),
            "iterator.lisp" => read_list_wrap(environment, to_str(ITERATOR_LISP), file_name),
            "collection.lisp" => read_list_wrap(environment, to_str(COLLECTION_LISP), file_name),
            "seq.lisp" => read_list_wrap(environment, to_str(SEQ_LISP), file_name),
            "shell.lisp" => read_list_wrap(environment, to_str(SHELL_LISP), file_name),
            "endfix.lisp" => read_list_wrap(environment, to_str(ENDFIX_LISP), file_name),
            "getopts.lisp" => read_list_wrap(environment, to_str(GETOPTS_LISP), file_name),
            "test.lisp" => read_list_wrap(environment, to_str(TEST_LISP), file_name),
            "slsh-std.lisp" => read_list_wrap(environment, to_str(SLSH_STD_LISP), file_name),
            "slshrc" => read_list_wrap(environment, to_str(SLSHRC), file_name),
            _ => {
                let msg = format!("{} not found", file_path);
                return Err(LispError::new(msg));
            }
        }
    };
    match ast {
        Ok(ast) => {
            let old_loose_syms = environment.loose_symbols;
            // Do not use loose symbols in scripts even if loading from the repl.
            environment.loose_symbols = false;
            let mut res: Option<Expression> = None;
            match &ast.get().data {
                ExpEnum::Vector(list) => {
                    for l in list {
                        let l: Expression = l.into();
                        res = Some(eval(environment, &l)?);
                    }
                }
                ExpEnum::Pair(_, _) => {
                    for l in ast.iter() {
                        res = Some(eval(environment, l)?);
                    }
                }
                _ => {
                    res = Some(eval(environment, &ast)?);
                }
            }
            environment.loose_symbols = old_loose_syms;
            Ok(res.unwrap_or_else(Expression::make_nil))
        }
        Err(err) => Err(LispError::new(err.reason)),
    }
}

fn builtin_load(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval(environment, arg)?;
            let file_name = arg.as_string(environment)?;
            return load(environment, &file_name);
        }
    }
    Err(LispError::new("load needs one argument"))
}

fn builtin_length(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            let arg = eval_no_values(environment, arg)?;
            return match &arg.get().data {
                ExpEnum::String(s, _) => {
                    let mut i = 0;
                    // Need to walk the chars to get the length in utf8 chars not bytes.
                    for _ in UnicodeSegmentation::graphemes(s.as_ref(), true) {
                        i += 1;
                    }
                    Ok(Expression::alloc_data(ExpEnum::Int(i64::from(i))))
                }
                ExpEnum::Vector(list) => {
                    Ok(Expression::alloc_data(ExpEnum::Int(list.len() as i64)))
                }
                ExpEnum::Pair(_, e2) => {
                    let e2: Expression = e2.into();
                    let mut len = 0;
                    let mut e_next = e2;
                    loop {
                        let e2 = if let ExpEnum::Pair(_, e2) = &e_next.get().data {
                            let e2: Expression = e2.into();
                            Some(e2.clone())
                        } else {
                            None
                        };
                        match e2 {
                            Some(e2) => {
                                len += 1;
                                e_next = e2.clone();
                            }
                            None => {
                                len += 1;
                                break;
                            }
                        }
                    }
                    Ok(Expression::alloc_data(ExpEnum::Int(len)))
                }
                ExpEnum::Nil => Ok(Expression::alloc_data(ExpEnum::Int(0))),
                ExpEnum::HashMap(map) => Ok(Expression::alloc_data(ExpEnum::Int(map.len() as i64))),
                _ => Err(LispError::new(format!(
                    "expression of type {} has no length",
                    arg.display_type()
                ))),
            };
        }
    }
    Err(LispError::new("length takes one form"))
}

fn builtin_if(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut args = args.peekable();
    let mut next_arg = args.next();
    while let Some(arg) = next_arg {
        if args.peek().is_some() {
            let cond = eval(environment, arg)?;
            if cond.is_nil() {
                args.next();
            } else {
                return eval_nr(environment, args.next().unwrap());
            }
        } else {
            return eval_nr(environment, arg);
        }
        next_arg = args.next();
        if next_arg.is_none() {
            return Ok(Expression::make_nil());
        }
    }
    Err(LispError::new("if: requires expressions"))
}

fn args_out(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    add_newline: bool,
    pretty: bool,
    writer: &mut dyn Write,
) -> Result<(), LispError> {
    for a in args {
        let aa = eval(environment, a)?;
        // If we have a standalone string do not quote it...
        let pretty = match &aa.get().data {
            ExpEnum::String(_, _) => false,
            _ => pretty,
        };
        if pretty {
            pretty_printf(&aa, environment, writer)?;
        } else {
            aa.writef(environment, writer)?;
        }
    }
    if add_newline {
        writer.write_all(b"\n")?;
    }
    Ok(())
}

fn print_to_oe(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    add_newline: bool,
    pretty: bool,
    default_error: bool,
    key: &str,
) -> Result<(), LispError> {
    let out = lookup_expression(environment, key);
    match out {
        Some(out) => {
            if let ExpEnum::File(f) = &out.get().data {
                let mut f_borrow = f.borrow_mut();
                match &mut *f_borrow {
                    FileState::Stdout => {
                        let stdout = io::stdout();
                        let mut out = stdout.lock();
                        drop(f_borrow);
                        args_out(environment, args, add_newline, pretty, &mut out)?;
                    }
                    FileState::Stderr => {
                        let stdout = io::stderr();
                        let mut out = stdout.lock();
                        drop(f_borrow);
                        args_out(environment, args, add_newline, pretty, &mut out)?;
                    }
                    FileState::Write(f) => {
                        // Don't call args_out here our we will buy a borrow error...
                        for a in args {
                            let aa = eval(environment, a)?;
                            // If we have a standalone string do not quote it...
                            let pretty = match &aa.get().data {
                                ExpEnum::String(_, _) => false,
                                _ => pretty,
                            };
                            if pretty {
                                pretty_printf(&aa, environment, f)?;
                            } else {
                                aa.writef(environment, f)?;
                            }
                        }
                        if add_newline {
                            f.write_all(b"\n")?;
                        }
                    }
                    _ => {
                        return Err(LispError::new(
                            "ERROR: Can not print to a non-writable file.",
                        ));
                    }
                }
            } else {
                let msg = format!("ERROR: {} is not a file!", key);
                return Err(LispError::new(msg));
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
    args: &mut dyn Iterator<Item = Expression>,
    add_newline: bool,
) -> Result<Expression, LispError> {
    match &environment.state.stdout_status {
        Some(IOState::Null) => { /* Nothing to do... */ }
        _ => {
            print_to_oe(environment, args, add_newline, true, false, "*stdout*")?;
        }
    };
    Ok(Expression::alloc_data(ExpEnum::Nil))
}

pub fn eprint(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    add_newline: bool,
) -> Result<Expression, LispError> {
    match &environment.state.stderr_status {
        Some(IOState::Null) => { /* Nothing to do... */ }
        _ => {
            print_to_oe(environment, args, add_newline, true, true, "*stderr*")?;
        }
    };
    Ok(Expression::alloc_data(ExpEnum::Nil))
}

fn builtin_print(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    print(environment, args, false)
}

fn builtin_println(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    print(environment, args, true)
}

fn builtin_eprint(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    eprint(environment, args, false)
}

fn builtin_eprintln(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    eprint(environment, args, true)
}

fn builtin_format(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut res = String::new();
    for a in args {
        res.push_str(&eval(environment, a)?.as_string(environment)?);
    }
    Ok(Expression::alloc_data(ExpEnum::String(res.into(), None)))
}

pub fn builtin_do(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut ret: Option<Expression> = None;
    for arg in args {
        if let Some(ret) = ret {
            ret.resolve(environment)?;
        }
        ret = Some(eval_nr(environment, arg.clone_root())?);
    }
    Ok(ret.unwrap_or_else(Expression::make_nil))
}

pub fn builtin_quote(
    _environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg) = args.next() {
        if args.next().is_none() {
            return Ok(arg);
        }
    }
    Err(LispError::new("quote: takes one form"))
}
/*
fn copy_clear_symbol(exp: Expression) -> Expression {
    let mut iexp: Expression = exp.clone();
    // Don't reuse structure for symbols or bad stuff can happen...
    {
        let iexp_d = iexp.get();
        if let ExpEnum::Symbol(sym, _) = &iexp_d.data {
            let sym = <&str>::clone(sym);
            drop(iexp_d);
            // XXX TODO- better copy?
            iexp = ExpEnum::Symbol(sym, SymLoc::None).into();
        }
    }
    iexp
}
*/
fn replace_commas(
    environment: &mut Environment,
    list: &mut dyn Iterator<Item = Expression>,
    is_vector: bool,
    meta: Option<ExpMeta>,
) -> Result<Expression, LispError> {
    let mut output: Vec<Handle> = Vec::new();
    let mut comma_next = false;
    let mut amp_next = false;
    for exp in list {
        let meta = exp.meta();
        let exp_d = exp.get();
        let exp = match &exp_d.data {
            ExpEnum::Vector(list) => {
                let mut i = Box::new(ListIter::new_list(&list));
                replace_commas(environment, &mut i, is_vector, meta)?
            }
            ExpEnum::Pair(_, _) => replace_commas(environment, &mut exp.iter(), is_vector, meta)?,
            _ => exp.clone(),
        };
        if let ExpEnum::Symbol(symbol, _) = &exp_d.data {
            if symbol == &"," {
                comma_next = true;
            } else if symbol == &",@" {
                amp_next = true;
            } else if comma_next {
                drop(exp_d);
                output.push(eval(environment, exp)?.into());
                comma_next = false;
            } else if amp_next {
                drop(exp_d);
                let nl = eval(environment, exp)?;
                match &nl.get().data {
                    ExpEnum::Vector(new_list) => {
                        for item in new_list {
                            output.push(item.clone());
                        }
                    }
                    ExpEnum::Pair(_, _) => {
                        for item in nl.iter() {
                            output.push(item.into());
                        }
                    }
                    ExpEnum::Nil => {}
                    _ => {
                        return Err(LispError::new(",@ must be applied to a list"));
                    }
                }
                amp_next = false;
            } else {
                // Make a new symbol vs sharing structure- can be a problem for
                // patched symbols later.
                let exp2: Expression = ExpEnum::Symbol(symbol, SymLoc::None).into();
                exp2.get_mut().meta = exp.get().meta;
                exp2.get_mut().meta_tags = exp.get().meta_tags.clone();
                output.push(exp2.into());
            }
        } else if comma_next {
            drop(exp_d);
            output.push(eval(environment, exp)?.into());
            comma_next = false;
        } else if amp_next {
            drop(exp_d);
            let nl = eval(environment, exp)?;
            match &nl.get().data {
                ExpEnum::Vector(new_list) => {
                    for item in new_list {
                        output.push(item.clone_no_root());
                    }
                }
                ExpEnum::Pair(_, _) => {
                    for item in nl.iter() {
                        output.push(item.into());
                    }
                }
                ExpEnum::Nil => {}
                _ => {
                    return Err(LispError::new(",@ must be applied to a list"));
                }
            }
            amp_next = false;
        } else {
            output.push(exp.into());
        }
    }
    if is_vector {
        Ok(Expression::with_list_meta(output, meta))
    } else {
        Ok(Expression::cons_from_vec(&output, meta))
    }
}

pub fn builtin_bquote(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let ret = if let Some(arg) = args.next() {
        let meta = arg.meta();
        match &arg.get().data {
            ExpEnum::Symbol(s, _) if s == &"," => {
                if let Some(exp) = args.next() {
                    Ok(eval(environment, exp)?)
                } else {
                    Ok(Expression::alloc_data(ExpEnum::Nil))
                }
            }
            ExpEnum::Vector(list) => replace_commas(
                environment,
                &mut Box::new(ListIter::new_list(list)),
                true,
                meta,
            ),
            ExpEnum::Pair(_, _) => replace_commas(environment, &mut arg.iter(), false, meta),
            _ => Ok(arg.clone()),
        }
    } else {
        Err(LispError::new("back-quote: takes one form"))
    };
    if args.next().is_some() {
        Err(LispError::new("back-quote: takes one form"))
    } else {
        ret
    }
}

fn builtin_and(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut last_exp = None;
    for arg in args {
        let arg = eval(environment, arg)?;
        if arg.is_nil() {
            return Ok(Expression::alloc_data(ExpEnum::Nil));
        } else {
            last_exp = Some(arg);
        }
    }
    Ok(last_exp.unwrap_or_else(Expression::make_true))
}

fn builtin_or(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    for arg in args {
        let arg = eval(environment, arg)?;
        if !arg.is_nil() {
            return Ok(arg);
        }
    }
    Ok(Expression::alloc_data(ExpEnum::Nil))
}

fn builtin_not(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            let arg0 = eval(environment, arg0)?;
            return if arg0.is_nil() {
                Ok(Expression::alloc_data(ExpEnum::True))
            } else {
                Ok(Expression::alloc_data(ExpEnum::Nil))
            };
        }
    }
    Err(LispError::new("not takes one form"))
}

fn do_expansion(
    environment: &mut Environment,
    command_in: &Expression,
    parts: &mut dyn Iterator<Item = Expression>,
) -> Result<Option<Expression>, LispError> {
    let com_d = command_in.get();
    if let ExpEnum::Symbol(_, _) = &com_d.data {
        drop(com_d);
        if let Some(exp) = get_expression_look(environment, command_in.clone(), true) {
            let exp_d = exp.get();
            if let ExpEnum::Macro(_sh_macro) = &exp_d.data {
                drop(exp_d);
                let expansion =
                    call_lambda(environment, exp, parts, false)?.resolve(environment)?;
                Ok(Some(expansion))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

fn expand_macro_internal(
    environment: &mut Environment,
    arg: &Expression,
    one: bool,
    depth: usize,
) -> Result<Option<Expression>, LispError> {
    if depth > 500 {
        return Err(LispError::new("Macro expand recursion to deep!"));
    }
    let arg_d = arg.get();
    if let ExpEnum::Vector(list) = &arg_d.data {
        let (command, parts) = match list.split_first() {
            Some((c, p)) => (c, p),
            None => {
                return Ok(None);
            }
        };
        let expansion = do_expansion(
            environment,
            &command.clone_no_root().into(),
            &mut Box::new(ListIter::new_slice(parts)),
        )?;
        if let Some(expansion) = expansion {
            if !one {
                if let Some(new_expansion) = expand_macro(environment, &expansion, one, depth)? {
                    Ok(Some(new_expansion))
                } else {
                    Ok(Some(expansion))
                }
            } else {
                Ok(Some(expansion))
            }
        } else {
            Ok(None)
        }
    } else if let ExpEnum::Pair(e1, e2) = &arg_d.data {
        let e1: Expression = e1.into();
        let e2: Expression = e2.into();
        let e2_d = e2.get();
        let mut e2_iter = if let ExpEnum::Vector(list) = &e2_d.data {
            Box::new(ListIter::new_list(&list))
        } else {
            drop(e2_d);
            e2.iter()
        };
        let expansion = do_expansion(environment, &e1, &mut e2_iter)?;
        if let Some(expansion) = expansion {
            if !one {
                if let Some(new_expansion) = expand_macro(environment, &expansion, one, depth)? {
                    Ok(Some(new_expansion))
                } else {
                    Ok(Some(expansion))
                }
            } else {
                Ok(Some(expansion))
            }
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

pub(crate) fn expand_macro(
    environment: &mut Environment,
    arg: impl AsRef<Expression>,
    one: bool,
    depth: usize,
) -> Result<Option<Expression>, LispError> {
    let arg = arg.as_ref();
    let lazy = environment.allow_lazy_fn;
    environment.allow_lazy_fn = false;
    let res = expand_macro_internal(environment, arg, one, depth + 1);
    environment.allow_lazy_fn = lazy;
    res
}

pub(crate) fn expand_macro_all(
    environment: &mut Environment,
    arg: &Expression,
) -> Result<Expression, LispError> {
    if let Some(exp_outer) = expand_macro(environment, arg, false, 0)? {
        let exp_outer_c = exp_outer.clone();
        let exp_d = exp_outer_c.get();
        if let ExpEnum::Vector(list) = &exp_d.data {
            let mut nv: Vec<Handle> = Vec::new();
            for item in list {
                nv.push(expand_macro_all(environment, &item.clone_no_root().into())?.into());
            }
            drop(exp_d);
            exp_outer.get_mut().data.replace(ExpEnum::Vector(nv));
            // XXX
            gc_mut().down_root(&exp_outer);
        } else if let ExpEnum::Pair(_, _) = &exp_d.data {
            let mut nv: Vec<Handle> = Vec::new();
            drop(exp_d);
            for item in exp_outer.iter() {
                nv.push(expand_macro_all(environment, &item)?.into());
            }
            exp_outer
                .get_mut()
                .data
                .replace(ExpEnum::cons_from_vec(&mut nv));
            // XXX
            gc_mut().down_root(&exp_outer);
        }
        Ok(exp_outer)
    } else {
        let arg2 = arg;
        let arg_d = arg.get();
        if let ExpEnum::Vector(list) = &arg_d.data {
            let mut nv: Vec<Handle> = Vec::new();
            for item in list {
                nv.push(expand_macro_all(environment, &item.clone_no_root().into())?.into());
            }
            drop(arg_d);
            arg.get_mut().data.replace(ExpEnum::Vector(nv));
            // XXX
            gc_mut().down_root(arg);
        } else if let ExpEnum::Pair(_, _) = &arg_d.data {
            let mut nv = Vec::new();
            drop(arg_d);
            for item in arg2.iter() {
                nv.push(expand_macro_all(environment, &item)?.into());
            }
            arg.get_mut().data.replace(ExpEnum::cons_from_vec(&mut nv));
            // XXX
            gc_mut().down_root(arg);
        }
        Ok(arg2.clone())
    }
}

fn builtin_expand_macro(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            let exp_macro = eval(environment, arg0)?;
            return if let Some(exp) = expand_macro(environment, &exp_macro, false, 0)? {
                Ok(exp)
            } else {
                Ok(exp_macro)
            };
        }
    }
    Err(LispError::new(
        "expand-macro can only have one form (list defining the macro call)",
    ))
}

fn builtin_expand_macro1(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            let exp_macro = eval(environment, arg0)?;
            return if let Some(exp) = expand_macro(environment, &exp_macro, true, 0)? {
                Ok(exp)
            } else {
                Ok(exp_macro)
            };
        }
    }
    Err(LispError::new(
        "expand-macro1 can only have one form (list defining the macro call)",
    ))
}

fn builtin_expand_macro_all(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(arg0) = args.next() {
        if args.next().is_none() {
            let exp_macro = eval(environment, arg0)?;
            return expand_macro_all(environment, &exp_macro);
        }
    }
    Err(LispError::new(
        "expand-macro-all can only have one form (list defining the macro call)",
    ))
}

fn builtin_recur(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut arg_list: Vec<Handle> = Vec::new();
    let mut arg_num = 0;
    for a in args {
        let a = eval(environment, a)?;
        arg_list.push(a.into());
        arg_num += 1;
    }
    environment.state.recur_num_args = Some(arg_num);
    Ok(Expression::with_list(arg_list))
}

fn builtin_gensym(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if args.next().is_some() {
        Err(LispError::new("gensym takes to arguments"))
    } else {
        let gensym_count = &mut environment.state.gensym_count;
        *gensym_count += 1;
        Ok(Expression::alloc_data(ExpEnum::Symbol(
            environment
                .interner
                .intern(&format!("gs@@{}", *gensym_count)),
            SymLoc::None,
        )))
    }
}

fn builtin_version(
    _environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if args.next().is_some() {
        Err(LispError::new("version takes no arguments"))
    } else {
        Ok(Expression::alloc_data(ExpEnum::String(
            VERSION_STRING.into(),
            None,
        )))
    }
}

fn builtin_command(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let old_form = environment.form_type;
    environment.form_type = FormType::ExternalOnly;
    let mut last_eval = Ok(Expression::alloc_data(ExpEnum::Nil));
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

fn builtin_form(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let old_form = environment.form_type;
    environment.form_type = FormType::FormOnly;
    let mut last_eval = Ok(Expression::alloc_data(ExpEnum::Nil));
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
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let old_loose_syms = environment.loose_symbols;
    environment.loose_symbols = true;
    let mut last_eval = Ok(Expression::alloc_data(ExpEnum::Nil));
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

fn builtin_get_error(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut ret = None;
    for arg in args {
        match eval(environment, arg) {
            Ok(exp) => ret = Some(exp),
            Err(err) => {
                let err_sym = Expression::alloc_data_h(ExpEnum::Symbol(
                    environment.interner.intern(":error"),
                    SymLoc::None,
                ));
                let msg = format!("{}", err);
                let err_msg = Expression::alloc_data_h(ExpEnum::String(msg.into(), None));
                let res = if let Some(backtrace) = err.backtrace {
                    vec![err_sym, err_msg, Expression::with_list(backtrace).into()]
                } else {
                    vec![err_sym, err_msg, Expression::make_nil_h()]
                };
                return Ok(Expression::cons_from_vec(&res, None));
            }
        }
    }
    let ok = Expression::alloc_data_h(ExpEnum::Symbol(
        environment.interner.intern(":ok"),
        SymLoc::None,
    ));
    Ok(Expression::alloc_data_h(ExpEnum::Pair(
        ok,
        ret.unwrap_or_else(Expression::make_nil).into(),
    ))
    .into())
}

fn add_usage(doc_str: &mut String, sym: &str, exp: &Expression) {
    let exp_d = exp.get();
    let p_iter = match &exp_d.data {
        ExpEnum::Lambda(f) => f.params.iter(),
        ExpEnum::Macro(m) => m.params.iter(),
        _ => return,
    };
    doc_str.push_str("\n\nUsage: (");
    doc_str.push_str(sym);
    for arg in p_iter {
        doc_str.push(' ');
        doc_str.push_str(arg);
    }
    doc_str.push(')');
}

fn make_doc(
    _environment: &mut Environment,
    exp: &Expression,
    key: &str,
    namespace: Rc<RefCell<Namespace>>,
) -> Result<String, LispError> {
    let mut new_docs = String::new();
    new_docs.push_str(key);
    new_docs.push_str("\nType: ");
    new_docs.push_str(&exp.display_type());
    new_docs.push_str("\nNamespace: ");
    new_docs.push_str(namespace.borrow().name());
    if let Some(doc_str) = namespace.borrow().get_docs(key) {
        if !doc_str.contains("Usage:") {
            add_usage(&mut new_docs, key, &exp);
        }
        new_docs.push_str("\n\n");
        new_docs.push_str(&doc_str);
    } else {
        add_usage(&mut new_docs, key, &exp);
    }
    new_docs.push('\n');
    Ok(new_docs)
}

fn get_doc(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
    is_raw: bool,
) -> Result<Expression, LispError> {
    if let Some(key) = args.next() {
        if args.next().is_none() {
            let key = eval(environment, key)?;
            let key_d = &key.get().data;
            let key = match key_d {
                ExpEnum::Symbol(s, _) => s,
                _ => {
                    return Err(LispError::new("doc: first form must evaluate to a symbol"));
                }
            };
            if key.contains("::") {
                // namespace reference.
                let mut key_i = key.splitn(2, "::");
                if let Some(namespace) = key_i.next() {
                    if let Some(key) = key_i.next() {
                        let namespace = if namespace == "ns" {
                            if let Some(exp) = lookup_expression(environment, "*ns*") {
                                match &exp.get().data {
                                    ExpEnum::String(s, _) => s.to_string(),
                                    _ => "NO_NAME".to_string(),
                                }
                            } else {
                                "NO_NAME".to_string()
                            }
                        } else {
                            namespace.to_string()
                        };
                        if let Some(namespace) = get_namespace(environment, &namespace) {
                            if is_raw {
                                if let Some(doc_string) = namespace.borrow().get_docs(key) {
                                    return Ok(Expression::alloc_data(ExpEnum::String(
                                        doc_string.to_string().into(),
                                        None,
                                    )));
                                } else {
                                    return Ok(Expression::alloc_data(ExpEnum::String(
                                        "".into(),
                                        None,
                                    )));
                                }
                            } else if let Some(exp) = namespace.borrow().get(key) {
                                return Ok(Expression::alloc_data(ExpEnum::String(
                                    make_doc(environment, &exp, key, namespace.clone())?.into(),
                                    None,
                                )));
                            }
                        }
                    }
                }
                return Ok(Expression::alloc_data(ExpEnum::String("".into(), None)));
            } else {
                let namespaces = get_symbol_namespaces(environment, &key);
                if namespaces.is_empty() {
                    return Err(LispError::new(
                        "doc: first form must evaluate to an existing global symbol",
                    ));
                } else if is_raw {
                    let mut doc_final = String::new();
                    for namespace in namespaces {
                        if let Some(doc_string) = namespace.borrow().get_docs(key) {
                            doc_final.push_str(doc_string);
                            doc_final.push('\n');
                        }
                    }
                    return Ok(Expression::alloc_data(ExpEnum::String(
                        doc_final.into(),
                        None,
                    )));
                } else {
                    let mut doc_final = String::new();
                    for namespace in namespaces {
                        if let Some(exp) = namespace.borrow().get(key) {
                            doc_final.push_str(&make_doc(
                                environment,
                                &exp,
                                key,
                                namespace.clone(),
                            )?);
                            doc_final.push('\n');
                        }
                    }
                    return Ok(Expression::alloc_data(ExpEnum::String(
                        doc_final.into(),
                        None,
                    )));
                }
            }
        }
    }
    Err(LispError::new("doc: requires a single symbol to lookup."))
}

fn builtin_doc(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    get_doc(environment, args, false)
}

fn builtin_doc_raw(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    get_doc(environment, args, true)
}

pub fn builtin_block(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut ret: Option<Expression> = None;
    if let Some(name) = args.next() {
        let name_d = &name.get().data;
        let name = if let ExpEnum::Symbol(n, _) = name_d {
            n
        } else {
            return Err(LispError::new(
                "block: Name must be a symbol (not evaluated).",
            ));
        };
        for arg in args {
            ret = if let Some(ret) = ret {
                Some(ret.resolve(environment)?)
            } else {
                None
            };
            if environment.return_val.is_none() {
                ret = Some(eval_nr(environment, arg)?);
            }
            let mut returned = false;
            if let Some((ret_name, exp)) = &environment.return_val {
                if let Some(ret_name) = ret_name {
                    if name == ret_name {
                        returned = true;
                        ret = Some(exp.clone());
                    }
                } else {
                    returned = true;
                    ret = Some(exp.clone());
                }
            }
            if returned {
                environment.return_val = None;
                return Ok(ret.unwrap_or_else(Expression::make_nil));
            }
            if environment.return_val.is_some() {
                break;
            }
        }
        Ok(ret.unwrap_or_else(Expression::make_nil))
    } else {
        Err(LispError::new("block: requires a name."))
    }
}

pub fn builtin_return_from(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(name) = args.next() {
        let name = if let ExpEnum::Symbol(n, _) = &name.get().data {
            Some(*n)
        } else if name.is_nil() {
            None
        } else {
            return Err(LispError::new(
                "return-from: Name should be a symbol or nil (not evaluated).",
            ));
        };
        if let Some(exp) = args.next() {
            if args.next().is_none() {
                let ret = eval_nr(environment, exp)?;
                environment.return_val = Some((name, ret));
            } else {
                return Err(LispError::new(
        "return-from: Requires a block name and optional expression, provided extra form(s).",
                ));
            }
        }
        Ok(Expression::alloc_data(ExpEnum::Nil))
    } else {
        Err(LispError::new("block: requires a name."))
    }
}

pub fn builtin_intern_stats(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if args.next().is_some() {
        Err(LispError::new("intern-stats: takes no arguments."))
    } else {
        println!(
            "allocated bytes: {}\nused bytes: {}\nsymbols interned: {}",
            environment.interner.capacity(),
            environment.interner.used(),
            environment.interner.len()
        );
        Ok(Expression::alloc_data(ExpEnum::Nil))
    }
}

pub fn builtin_meta_line_no(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    match &args.next() {
        None => {
            if let Some(meta) = &environment.last_meta {
                Ok(Expression::alloc_data(ExpEnum::Int(meta.line as i64)))
            } else {
                Ok(Expression::alloc_data(ExpEnum::Nil))
            }
        }
        Some(arg) => {
            let exp = eval(environment, arg)?;
            if args.next().is_some() {
                return Err(LispError::new(
                    "meta-column-no: takes zero or one argument.",
                ));
            }
            let exp_d = exp.get();
            if let Some(meta) = &exp_d.meta {
                Ok(Expression::alloc_data(ExpEnum::Int(meta.line as i64)))
            } else {
                Ok(Expression::alloc_data(ExpEnum::Nil))
            }
        }
    }
}

pub fn builtin_meta_column_no(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    match &args.next() {
        None => {
            if let Some(meta) = &environment.last_meta {
                Ok(Expression::alloc_data(ExpEnum::Int(meta.col as i64)))
            } else {
                Ok(Expression::alloc_data(ExpEnum::Nil))
            }
        }
        Some(arg) => {
            let exp = eval(environment, arg)?;
            if args.next().is_some() {
                return Err(LispError::new(
                    "meta-column-no: takes zero or one argument.",
                ));
            }
            let exp_d = exp.get();
            if let Some(meta) = &exp_d.meta {
                Ok(Expression::alloc_data(ExpEnum::Int(meta.col as i64)))
            } else {
                Ok(Expression::alloc_data(ExpEnum::Nil))
            }
        }
    }
}

pub fn builtin_meta_file_name(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    match &args.next() {
        None => {
            if let Some(meta) = &environment.last_meta {
                Ok(Expression::alloc_data(ExpEnum::String(
                    meta.file.into(),
                    None,
                )))
            } else {
                Ok(Expression::alloc_data(ExpEnum::Nil))
            }
        }
        Some(arg) => {
            let exp = eval(environment, arg)?;
            if args.next().is_some() {
                return Err(LispError::new(
                    "meta-file-name: takes zero or one argument.",
                ));
            }
            let exp_d = exp.get();
            if let Some(meta) = &exp_d.meta {
                Ok(Expression::alloc_data(ExpEnum::String(
                    meta.file.into(),
                    None,
                )))
            } else {
                Ok(Expression::alloc_data(ExpEnum::Nil))
            }
        }
    }
}

pub fn builtin_meta_add_tags(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    fn put_tag(exp: &Expression, tag: Expression) -> Result<(), LispError> {
        if let ExpEnum::Symbol(s, _) = &tag.get().data {
            let mut exp_d = exp.get_mut();
            if let Some(tags) = &mut exp_d.meta_tags {
                tags.insert(s);
            } else {
                let mut tags: HashSet<&'static str> = HashSet::new();
                tags.insert(s);
                exp_d.meta_tags = Some(tags);
            }
        } else {
            return Err(LispError::new("meta-add-tags: Found a non-symbol!"));
        }
        Ok(())
    }
    if let Some(exp) = args.next() {
        let exp = eval(environment, exp)?;
        for arg in args {
            let arg = eval(environment, arg)?;
            let arg_d = arg.get();
            match &arg_d.data {
                ExpEnum::Pair(_, _) => {
                    for tag in arg.iter() {
                        put_tag(&exp, tag)?;
                    }
                }
                ExpEnum::Vector(v) => {
                    for tag in v {
                        put_tag(&exp, tag.into())?;
                    }
                }
                ExpEnum::Symbol(_, _) => {
                    put_tag(&exp, arg.clone())?;
                }
                _ => {
                    return Err(LispError::new(
                        "meta-add-tags: Takes an expression and symbols, vectors or lists of tags (symbols)",
                    ));
                }
            }
        }
    }
    Ok(Expression::alloc_data(ExpEnum::Nil))
}

pub fn builtin_meta_tag_set(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(exp) = args.next() {
        let exp = eval(environment, exp)?;
        if let Some(tag) = args.next() {
            if args.next().is_none() {
                let tag = eval(environment, tag)?;
                let tag_d = tag.get();
                if let ExpEnum::Symbol(s, _) = &tag_d.data {
                    let exp_d = exp.get();
                    if let Some(tags) = &exp_d.meta_tags {
                        if tags.contains(s) {
                            return Ok(Expression::make_true());
                        }
                    }
                } else {
                    return Err(LispError::new(
                        "meta-tag?: Takes an expression and a tag (symbol)",
                    ));
                }
            } else {
                return Err(LispError::new(
                    "meta-tag?: Takes an expression and a tag check for..",
                ));
            }
        }
    }
    Ok(Expression::make_nil())
}

macro_rules! ensure_tonicity {
    ($check_fn:expr, $values:expr, $type:ty, $type_two:ty) => {{
        let first = $values
            .first()
            .ok_or(LispError::new("expected at least one value"))?;
        let rest = &$values[1..];
        fn f(prev: $type, xs: &[$type_two]) -> bool {
            match xs.first() {
                Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
                None => true,
            }
        };
        if f(first, rest) {
            Ok(Expression::alloc_data(ExpEnum::True))
        } else {
            Ok(Expression::alloc_data(ExpEnum::Nil))
        }
    }};
}

macro_rules! ensure_tonicity_all {
    ($environment:expr, $args:expr, $check_fn:expr) => {{
        let mut list: Vec<Expression> = Vec::new();
        for arg in $args {
            list.push(eval($environment, arg)?);
        }
        if let Ok(ints) = parse_list_of_ints($environment, &mut list) {
            ensure_tonicity!($check_fn, ints, &i64, i64)
        } else if let Ok(floats) = parse_list_of_floats($environment, &mut list) {
            ensure_tonicity!($check_fn, floats, &f64, f64)
        } else {
            let strings = parse_list_of_strings($environment, &mut list)?;
            ensure_tonicity!($check_fn, strings, &str, String)
        }
    }};
}

pub fn builtin_equal(
    environment: &mut Environment,
    parts: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    let mut args: Vec<Expression> = Vec::new();
    for a in parts {
        args.push(eval(environment, a)?);
    }
    if let Ok(ints) = parse_list_of_ints(environment, &mut args) {
        ensure_tonicity!(|a, b| a == b, ints, &i64, i64)
    } else if let Ok(floats) = parse_list_of_floats(environment, &mut args) {
        ensure_tonicity!(|a, b| ((a - b) as f64).abs() < 0.000_001, floats, &f64, f64)
    } else {
        let strings = parse_list_of_strings(environment, &mut args)?;
        ensure_tonicity!(|a, b| a == b, strings, &str, String)
    }
}

pub fn builtin_less_than(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    ensure_tonicity_all!(environment, args, |a, b| a < b)
}

pub fn builtin_greater_than(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    ensure_tonicity_all!(environment, args, |a, b| a > b)
}

pub fn builtin_less_than_equal(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    ensure_tonicity_all!(environment, args, |a, b| a <= b)
}

pub fn builtin_greater_than_equal(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    ensure_tonicity_all!(environment, args, |a, b| a >= b)
}

pub fn add_builtins<S: BuildHasher>(
    interner: &mut Interner,
    data: &mut HashMap<&'static str, (Expression, String), S>,
) {
    data.insert(
        interner.intern("eval"),
        Expression::make_function(
            builtin_eval,
            "Usage: (eval expression)

Evaluate the provided expression.

If expression is a string read it to make an ast first to evaluate otherwise
evaluate the expression (note eval is a function not a special form, the
provided expression will be evaluated as part of call).

Section: core

Example:
(def test-eval-one nil)
(eval \"(set! test-eval-one \\\"ONE\\\")\")
(test::assert-equal \"ONE\" test-eval-one)
(eval '(set! test-eval-one \"TWO\"))
(test::assert-equal \"TWO\" test-eval-one)
",
        ),
    );
    data.insert(
        interner.intern("apply"),
        Expression::make_function(
            builtin_apply,
            "Usage: (apply function arg* list)

Call the provided function with the suplied arguments, last is a list that will be expanded.

Section: core

Example:
(def test-apply-one (apply str '(\"O\" \"NE\")))
(test::assert-equal \"ONE\" test-apply-one)
(test::assert-equal 10 (apply + 1 '(2 7)))
",
        ),
    );
    data.insert(
        interner.intern("unwind-protect"),
        Expression::make_function(
            builtin_unwind_protect,
            "Usage: (unwind-protect protected cleanup*) -> [protected result]

After evaluation first form, make sure the following cleanup forms run (returns first form's result).

Section: core

Example:
(def test-unwind-one nil)
(def test-unwind-err (get-error
    (unwind-protect (err \"Some protected error\") (set! test-unwind-one \"got it\"))))
(test::assert-equal :error (car test-unwind-err))
(test::assert-equal \"Some protected error\" (cadr test-unwind-err))
(test::assert-equal \"got it\" test-unwind-one)

(def test-unwind-one nil)
(def test-unwind-two nil)
(def test-unwind-three nil)
(def test-unwind-four nil)
(def test-unwind-err (get-error
    (unwind-protect
        (do (set! test-unwind-one \"set one\")(err \"Some protected error two\")(set! test-unwind-two \"set two\"))
        (set! test-unwind-three \"set three\")(set! test-unwind-four \"set four\"))))
(test::assert-equal :error (car test-unwind-err))
(test::assert-equal \"Some protected error two\" (cadr test-unwind-err))
(test::assert-equal \"set one\" test-unwind-one)
(test::assert-equal nil test-unwind-two)
(test::assert-equal \"set three\" test-unwind-three)
(test::assert-equal \"set four\" test-unwind-four)
",
        ),
    );
    data.insert(
        interner.intern("err"),
        Expression::make_function(
            builtin_err,
            "Usage: (err string) -> raises an error

Raise an error with the supplied string.

Section: core

Example:
(def test-err-err (get-error (err \"Test Error\")))
(test::assert-equal :error (car test-err-err))
(test::assert-equal \"Test Error\" (cadr test-err-err))
",
        ),
    );
    data.insert(
        interner.intern("load"),
        Expression::make_function(
            builtin_load,
            "Usage: (load path) -> [last form value]

Read and eval a file (from path- a string).

Section: scripting

Example:
(def test-load-one nil)
(def test-load-two nil)
(def test-load-fn (open \"/tmp/slsh-test-load.testing\" :create :truncate))
(write-line test-load-fn \"(set! test-load-one \\\"LOAD TEST\\\") '(1 2 3)\")
(close test-load-fn)
(set! test-load-two (load \"/tmp/slsh-test-load.testing\"))
(test::assert-equal \"LOAD TEST\" test-load-one)
(test::assert-equal '(1 2 3) test-load-two)
",
        ),
    );
    data.insert(
        interner.intern("length"),
        Expression::make_function(
            builtin_length,
            "Usage: (length expression) -> int

Return length of suplied expression.

Section: core

Example:
(test::assert-equal 0 (length nil))
(test::assert-equal 5 (length \"12345\"))
; Note the unicode symbol is only one char even though it is more then one byte.
(test::assert-equal 6 (length \"12345Σ\"))
(test::assert-equal 3 (length '(1 2 3)))
(test::assert-equal 3 (length '#(1 2 3)))
(test::assert-equal 3 (length (list 1 2 3)))
(test::assert-equal 3 (length (vec 1 2 3)))
(test::assert-error (length 100))
(test::assert-error (length 100.0))
(test::assert-error (length #\\x))
",
        ),
    );
    data.insert(
        interner.intern("if"),
        Expression::make_special(
            builtin_if,
            "Usage: (if p1 a1 p2 a2 ... pn an?) -> [evaled form result]

If conditional.  Will evaluate p1 and if true (i.e. not nil) then return the
evaluation of a1, if nil evaluate p2 and so on.  On an odd number of arguments
(an is missing) then evaluate and return pn.  Return nil if no predicate is
true.  This degenerates into the traditional (if predicate then-form else-form).

Section: conditional

Example:
(def test-if-one
    (if t \"ONE TRUE\" \"ONE FALSE\"))
(def test-if-two
    (if nil \"TWO TRUE\" \"TWO FALSE\"))
(test::assert-equal \"ONE TRUE\" test-if-one)
(test::assert-equal \"TWO FALSE\" test-if-two)

(def test-if-one2
    (if t \"ONE2 TRUE\"))
(def test-if-two2
    (if nil \"TWO2 TRUE\"))
(test::assert-equal \"ONE2 TRUE\" test-if-one2)
(test::assert-equal nil test-if-two2)

(def test-if-one2
    (if nil \"ONE FALSE\" t \"ONE TRUE\" t \"ONE TRUE2\"))
(def test-if-two2
    (if nil \"TWO TRUE\" nil \"TWO FALSE\" t \"TWO TRUE2\"))
(def test-if-three2
    (if nil \"THREE TRUE\" nil \"THREE FALSE\" \"THREE DEFAULT\"))
(test::assert-equal \"ONE TRUE\" test-if-one2)
(test::assert-equal \"TWO TRUE2\" test-if-two2)
(test::assert-equal \"THREE DEFAULT\" test-if-three2)
(test::assert-false (if nil))
(test::assert-false (if nil t nil t nil t))
",
        ),
    );
    data.insert(
        interner.intern("print"),
        Expression::make_function(
            builtin_print,
            "Usage: (print arg0 ... argN) -> nil

Print the arguments (as strings) to *stdout*.

Section: core

Example:
; Use a file for stdout for test.
(dyn *stdout* (open \"/tmp/sl-sh.print.test\" :create :truncate) (do (print \"Print test out\")(print \" two\") (close *stdout*)))
(test::assert-equal \"Print test out two\" (read-line (open \"/tmp/sl-sh.print.test\" :read)))
",
        ),
    );
    data.insert(
        interner.intern("println"),
        Expression::make_function(
            builtin_println,
            "Usage: (println arg0 ... argN) -> nil

Print the arguments (as strings) to *stdout* and then a newline.

Section: core

Example:
; Use a file for stdout for test.
(dyn *stdout* (open \"/tmp/sl-sh.println.test\" :create :truncate) (do (println \"Println test out\")(println \"line two\") (close *stdout*)))
(def topen (open \"/tmp/sl-sh.println.test\" :read))
(test::assert-equal \"Println test out\n\" (read-line topen))
(test::assert-equal \"line two\n\" (read-line topen))
",
        ),
    );
    data.insert(
        interner.intern("eprint"),
        Expression::make_function(
            builtin_eprint,
            "Usage: (eprint arg0 ... argN) -> nil

Print the arguments (as strings) to *stderr*.

Section: core

Example:
; Use a file for stderr for test.
(dyn *stderr* (open \"/tmp/sl-sh.eprint.test\" :create :truncate) (do (eprint \"eprint test out\")(eprint \" two\") (close *stderr*)))
(test::assert-equal \"eprint test out two\" (read-line (open \"/tmp/sl-sh.eprint.test\" :read)))
",
        ),
    );
    data.insert(
        interner.intern("eprintln"),
        Expression::make_function(
            builtin_eprintln,
            "Usage: (eprintln arg0 ... argN) -> nil

Print the arguments (as strings) to *stderr* and then a newline.

Section: core

Example:
; Use a file for stderr for test.
(dyn *stderr* (open \"/tmp/sl-sh.eprintln.test\" :create :truncate) (do (eprintln \"eprintln test out\")(eprintln \"line two\") (close *stderr*)))
(def topen (open \"/tmp/sl-sh.eprintln.test\" :read))
(test::assert-equal \"eprintln test out\n\" (read-line topen))
(test::assert-equal \"line two\n\" (read-line topen))
"
        ),
    );
    data.insert(
        interner.intern("format"),
        Expression::make_function(
            builtin_format,
            "Usage: (format arg0 ... argN) -> string

Build a formatted string from arguments.

Arguments will be turned into strings.

Section: core

Example:
(test::assert-equal \"stringsome\" (format \"string\" \"some\"))
(test::assert-equal \"string\" (format \"string\" \"\"))
(test::assert-equal \"string 50\" (format \"string\" \" \" 50))
(test::assert-equal \"string 50 100.5\" (format \"string\" \" \" 50 \" \" 100.5))
",
        ),
    );
    data.insert(
        interner.intern("do"),
        Expression::make_special(
            builtin_do,
            "Usage: (do exp0 ... expN) -> expN

Evaluatate each form and return the last.

Section: core

Example:
(def test-do-one nil)
(def test-do-two nil)
(def test-do-three (do (set! test-do-one \"One\")(set! test-do-two \"Two\")\"Three\"))
(test::assert-equal \"One\" test-do-one)
(test::assert-equal \"Two\" test-do-two)
(test::assert-equal \"Three\" test-do-three)
",
        ),
    );
    data.insert(
        interner.intern("fn"),
        Expression::make_special_fn(
            "Usage: (fn (param*) expr*) -> exprN

Create a function (lambda).

Section: core

Example:
(def test-fn1 nil)
(def test-fn2 nil)
(def test-fn3 nil)
(def test-fn-empty ((fn ())))
(test::assert-false test-fn-empty)
((fn () (set! test-fn1 1)))
(test::assert-equal 1 test-fn1)
((fn () (set! test-fn1 10)(set! test-fn2 2)))
(test::assert-equal 10 test-fn1)
(test::assert-equal 2 test-fn2)
((fn () (set! test-fn1 11)(set! test-fn2 20)(set! test-fn3 3)))
(test::assert-equal 11 test-fn1)
(test::assert-equal 20 test-fn2)
(test::assert-equal 3 test-fn3)
((fn (x y z) (set! test-fn1 x)(set! test-fn2 y)(set! test-fn3 z)) 12 21 30)
(test::assert-equal 12 test-fn1)
(test::assert-equal 21 test-fn2)
(test::assert-equal 30 test-fn3)
(test::assert-equal 63 ((fn (x y z) (set! test-fn1 x)(set! test-fn2 y)(set! test-fn3 z)(+ x y z)) 12 21 30))
",
        ),
    );
    data.insert(
        interner.intern("quote"),
        Expression::make_special_quote(
            "Usage: 'expression -> expression

Return expression without evaluation.
The reader macro 'expression will expand to (quote expression).

Section: core

Example:
(test::assert-equal (list 1 2 3) (quote (1 2 3)))
(test::assert-equal (list 1 2 3) '(1 2 3))
(test::assert-equal '(1 2 3) (quote (1 2 3)))
",
        ),
    );
    data.insert(
        interner.intern("back-quote"),
        Expression::make_special_backquote(
            "Usage: `expression -> expression

Return expression without evaluation.
Always use the ` reader macro or expansion will not work
(i.e. (back-quote expression) will not do , expansion).

Backquote (unlike quote) allows for symbol/form evaluation using , or ,@.

Section: core

Example:
(test::assert-equal (list 1 2 3) `(1 2 3))
(test::assert-equal `(1 2 3) '(1 2 3))
(def test-bquote-one 1)
(def test-bquote-list '(1 2 3))
(test::assert-equal (list 1 2 3) `(,test-bquote-one 2 3))
(test::assert-equal (list 1 2 3) `(,@test-bquote-list))
",
        ),
    );
    /*data.insert(
        "spawn"),
        Expression::Func(builtin_spawn)),
    );*/
    data.insert(
        interner.intern("and"),
        Expression::make_special(builtin_and,
        "Usage: (and exp0 ... expN) -> [nil or expN result]

Evaluates each form until one produces nil (false), produces nil if any form is nil or the result of the last expression.

The and form will stop evaluating when the first expression produces nil.

Section: conditional

Example:
(test::assert-false (and nil (err \"and- can not happen\")))
(test::assert-equal \"and- done\" (and t \"and- done\"))
(test::assert-equal \"and- done\" (and t t \"and- done\"))
(test::assert-equal 6 (and t t (+ 1 2 3)))
(test::assert-equal 6 (and (/ 10 5) (* 5 2) (+ 1 2 3)))
"),
    );
    data.insert(
        interner.intern("or"),
        Expression::make_special(
            builtin_or,
            "Usage: (or exp0 ... expN) -> [nil or first non nil expression]

Evaluates each form until one produces a non-nil result, produces nil if all expressions are nil.

The or form will stop evaluating when the first expression produces non-nil.

Section: conditional

Example:
(test::assert-true (or nil nil t (err \"and- can not happen\")))
(test::assert-false (or nil nil nil))
(test::assert-equal \"or- done\" (or nil \"or- done\"))
(test::assert-equal \"or- done\" (or nil nil \"or- done\"))
(test::assert-equal 6 (or nil nil (+ 1 2 3)))
(test::assert-equal 2 (or (/ 10 5) (* 5 2) (+ 1 2 3)))
",
        ),
    );
    data.insert(
        interner.intern("not"),
        Expression::make_function(
            builtin_not,
            "Usage: (not expression)

Return true if expression is nil.

Section: conditional

Example:
(test::assert-true (not nil))
(test::assert-false (not 10))
(test::assert-false (not t))
(test::assert-false (not (+ 1 2 3)))
",
        ),
    );
    data.insert(
        interner.intern("null"),
        Expression::make_function(
            builtin_not,
            "Usage: (null expression)

Return true if expression is nil (null).

Section: conditional

Example:
(test::assert-true (null nil))
(test::assert-false (null 10))
(test::assert-false (null t))
(test::assert-false (null (+ 1 2 3)))
",
        ),
    );
    data.insert(
        interner.intern("macro"),
        Expression::make_special_macro(
            "Usage: (macro (args) `(apply + ,@args))

Define an anonymous macro.

Section: core

Example:
(def test-macro1 nil)
(def test-macro2 nil)
(def test-macro3 nil)
(def test-macro-empty ((macro ())))
(test::assert-false test-macro-empty)
((macro () '(set! test-macro1 1)))
(test::assert-equal 1 test-macro1)
((macro () (set! test-macro1 10)'(set! test-macro2 2)))
(test::assert-equal 10 test-macro1)
(test::assert-equal 2 test-macro2)
((macro () (set! test-macro1 11)(set! test-macro2 20)'(set! test-macro3 3)))
(test::assert-equal 11 test-macro1)
(test::assert-equal 20 test-macro2)
(test::assert-equal 3 test-macro3)
((macro (x y z) (set! test-macro1 x)(set! test-macro2 y)`(set! test-macro3 ,z)) 12 21 30)
(test::assert-equal 12 test-macro1)
(test::assert-equal 21 test-macro2)
(test::assert-equal 30 test-macro3)
(test::assert-equal 63 ((macro (x y z) (set! test-macro1 x)(set! test-macro2 y)(set! test-macro3 z)`(+ ,x ,y ,z)) 12 21 30))
(def test-mac nil)
(def mac-var 2)
(lex
  (var mac-var 3)
  (set! test-mac (macro (x) (set! test-macro2 100)(test::assert-equal 3 mac-var)`(* ,mac-var ,x))))
(set! test-macro1 (test-mac 10))
(test::assert-equal 30 test-macro1)
(test::assert-equal 100 test-macro2)
",
        ),
    );
    data.insert(
        interner.intern("expand-macro"),
        Expression::make_function(
            builtin_expand_macro,
            "Usage: (expand-macro expression)

Expands a macro expression.  If that expansion is also a macro then expand it recursively.

Just returns the expression if not a macro.

Section: core

Example:
(test::assert-equal '(def xx \"value\") (expand-macro '(def xx \"value\")))

(defmacro mac-test-for
    (bind in in_list body) (do
	(if (not (= in 'in)) (err \"Invalid test-mac-for: (test-mac-for [v] in [iterator] (body))\"))
    `((fn (,bind)
        (if (> (length ,in_list) 0)
            (root::loop (plist) (,in_list) (do
                (set! ,bind (root::first plist))
                (,@body)
                (if (> (length plist) 1) (recur (root::rest plist)))))))nil)))

(test::assert-equal '(
    (fn
        (i)
        (if
            (> (length '(1 2 3)) 0)
            (root::loop
                (plist)
                ('(1 2 3))
                (do
                    (set! i (root::first plist)) nil
                    (if
                        (> (length plist) 1)
                        (recur (root::rest plist))))))) nil)
    (expand-macro '(mac-test-for i in '(1 2 3) ())))

(test::assert-equal '(1 2 3) (expand-macro '(1 2 3)))
",
        ),
    );
    data.insert(
            interner.intern("expand-macro1"),
            Expression::make_function(
                builtin_expand_macro1,
                "Usage: (expand-macro1 expression)

    Expands a macro expression.  Only expand the first macro.

    Just returns the expression if not a macro.

    Section: core

    Example:
    (test::assert-equal '(def xx \"value\") (expand-macro1 '(def xx \"value\")))

    (defmacro mac-test-for
        (bind in in_list body) (do
        (if (not (= in 'in)) (err \"Invalid test-mac-for: (test-mac-for [v] in [iterator] (body))\"))
        `((fn (,bind)
            (if (> (length ,in_list) 0)
                (root::loop (plist) (,in_list) (do
                    (set! ,bind (root::first plist))
                    (,@body)
                    (if (> (length plist) 1) (recur (root::rest plist)))))))nil)))

    (test::assert-equal '((fn
        (i)
        (if
            (> (length '(1 2 3)) 0)
            (root::loop
                (plist)
                ('(1 2 3))
                (do
                    (set! i (root::first plist)) nil
                    (if
                        (> (length plist) 1)
                        (recur (root::rest plist)))))))nil)
        (expand-macro1 '(mac-test-for i in '(1 2 3) ())))

    (test::assert-equal '(1 2 3) (expand-macro1 '(1 2 3)))
    ",
            ),
        );
    data.insert(
        interner.intern("expand-macro-all"),
        Expression::make_function(
            builtin_expand_macro_all,
            "Usage: (expand-macro-all expression)

Expands a macro expression like expand-macro but also expand any embedded macros.  

Just returns the expression if not a macro.

Section: core

Example:
(test::assert-equal '(def xx \"value\") (expand-macro-all '(def xx \"value\")))

(defmacro mac-test-for
    (bind in in_list body) (do
	(if (not (= in 'in)) (err \"Invalid test-mac-for: (test-mac-for [v] in [iterator] (body))\"))
    `((fn (,bind)
        (if (> (length ,in_list) 0)
            (root::loop (plist) (,in_list) (do
                (set! ,bind (root::first plist))
                (,@body)
                (if (> (length plist) 1) (recur (root::rest plist)))))))nil)))

(test::assert-equal '(
    (fn
        (i)
        (if
            (> (length '(1 2 3)) 0)
            (
                (fn
                    (plist)
                    (do
                        (set! i (root::first plist)) nil
                        (if
                            (> (length plist) 1)
                            (recur (root::rest plist)))))
                '(1 2 3)))) nil)
    (expand-macro-all '(mac-test-for i in '(1 2 3) ())))

(test::assert-equal '(1 2 3) (expand-macro-all '(1 2 3)))
",
        ),
    );
    data.insert(
        interner.intern("recur"),
        Expression::make_function(
            builtin_recur,
            "Usage: (recur &rest)

Recursively call the enclosing function with the given parameters.  Recur uses
tail call optimization and must be in the tail position or it is an error.  For
a named function it would be equivalent to a normal recursive call in a tail
position but it requires a tail position and does not need a name (a normal
recursive call would work in a non-tail position but could blow the stack if
it is to deep- unlike a recur or tail position recursive call).
NOTE: potential footgun, the let macro expands to a lambda (fn) and a recur used
inside the let would bind with the let not the enclosing lambda (this would
apply to any macro that also expands to a lamda- this is by design with the
loop macro but would be unexpected with let).

Section: core

Example:
(def tot 0)
(loop (idx) (3) (do
    (set! tot (+ tot 1))
    (if (> idx 1) (recur (- idx 1)))))
(assert-equal 3 tot)
(set! tot 0)
((fn (idx) (do
    (set! tot (+ tot 1))
    (if (> idx 1) (recur (- idx 1)))))5)
(assert-equal 5 tot)
",
        ),
    );
    data.insert(
        interner.intern("gensym"),
        Expression::make_function(
            builtin_gensym,
            "Usage: (gensym) -> symbol

Generate a unique symbol.

Gensym uses a prefix of gs@@ followed by an incrementing counter.
It is useful to generate unique symbol names when writing macros (for instance).

Section: core

Example:
(def test-gensym-one (gensym))
(def test-gensym-two (gensym))
(def test-gensym-three (gensym))
(test::assert-true (str-starts-with \"gs@@\" (sym->str test-gensym-one)))
(test::assert-true (str-starts-with \"gs@@\" (sym->str test-gensym-two)))
(test::assert-true (str-starts-with \"gs@@\" (sym->str test-gensym-three)))
(test::assert-true (symbol? (gensym)))
(test::assert-true (symbol? test-gensym-one))
(test::assert-true (symbol? test-gensym-two))
(test::assert-true (symbol? test-gensym-three))
",
        ),
    );
    data.insert(
        interner.intern("version"),
        Expression::make_function(
            builtin_version,
            "Usage: (version)

Produce executable version as string.

Section: shell

Example:
(test::assert-true (string? (version)))
",
        ),
    );
    data.insert(
        interner.intern("command"),
        Expression::make_special(
            builtin_command,
            "Usage: (command exp0 ... expN)

Only execute system commands not forms within this form.

Section: shell

Example:
(test::assert-equal \"Failed to execute [str string]: No such file or directory (os error 2)\" (cadr (get-error (command (str \"string\")))))
(test::assert-equal \"Some String\n\" (str (command (echo \"Some String\"))))
"
        ),
    );
    data.insert(
        interner.intern("form"),
        Expression::make_special(
            builtin_form,
            "Usage: (form exp0 ... expN)

Like do but do not execute system commands within this form.

Section: shell

Example:
(test::assert-equal \"Not a valid form true, not found.\" (cadr (get-error (form (true)))))
(test::assert-equal \"Some String\" (form (str \"Some String\")))
",
        ),
    );
    data.insert(
        interner.intern("loose-symbols"),
        Expression::make_special(
            builtin_loose_symbols,
            "Usage: (loose-symbols exp0 ... expN)

Within this form any undefined symbols become strings.

Section: shell

Example:
(test::assert-equal \"Some_Result\" (loose-symbols Some_Result))
(test::assert-equal \"Some Result\" (loose-symbols Some\\ Result))
",
        ),
    );
    data.insert(
        interner.intern("get-error"),
        Expression::make_function(
            builtin_get_error,
            "Usage: (get-error exp0 ... expN) -> pair

Evaluate each form (like do) but on error return (:error msg backtrace) instead of aborting.
On success return (:ok . expN-result).

If there is no error will return the value of the last expression as the cdr of
the pair.  Always returns a pair with the first value either being :ok or :error.

Section: core

Example:
(def get-error-t1 (get-error (err \"Some Error\")))
(test::assert-equal :error (car get-error-t1)) 
(test::assert-equal \"Some Error\" (cadr get-error-t1)) 
(test::assert-true (vec? (caddr get-error-t1)))
(test::assert-equal '(:ok . \"Some String\") (get-error \"Some String\"))
(test::assert-equal '(:ok . \"Some Other String\") (get-error (def test-get-error \"Some \") (str test-get-error \"Other String\")))
"
        ),
    );
    data.insert(
        interner.intern("doc"),
        Expression::make_function(
            builtin_doc,
            "Usage: (doc symbol)

Return the doc string for a symbol or nil if no string.

Section: core

Example:
;(doc 'car)
t
",
        ),
    );
    data.insert(
        interner.intern("doc-raw"),
        Expression::make_function(
            builtin_doc_raw,
            "Usage: (doc-raw symbol)

Return the raw (unexpanded) doc string for a symbol or nil if no string.

Section: core

Example:
;(doc-raw 'car)
t
",
        ),
    );

    data.insert(
        interner.intern("block"),
        Expression::make_special(
            builtin_block,
            "Usage: (block name form*)

Create a block with name (name is not evaluated), if no return-from encountered then
return last expression (like do).

Section: core

Example:
(test::assert-equal '(4 5) (block xxx '(1 2) (return-from xxx '(4 5)) '(a b) '(2 3)))
(test::assert-equal '(4 5) (block xxx '(1 2) (return-from nil '(4 5)) '(a b) '(2 3)))
(test::assert-equal '(5 6) (block xxx '(1 2) (block yyy (return-from xxx '(5 6)) '(a b)) '(2 3)))
(test::assert-equal '(5 6) (block xxx '(1 2) (block yyy ((fn (p) (return-from xxx p)) '(5 6)) '(a b)) '(2 3)))
(test::assert-equal '(2 3) (block xxx '(1 2) (block yyy (return-from yyy t) '(a b)) '(2 3)))
"
        ),
    );

    data.insert(
        interner.intern("return-from"),
        Expression::make_special(
            builtin_return_from,
            "Usage: (return-from name expression?)

Causes enclosing block with name (name is not evaluated) to evaluate to expression.

Section: core

Example:
(test::assert-equal '(4 5) (block xxx '(1 2) (return-from xxx '(4 5)) '(a b) '(2 3)))
(test::assert-equal '(4 5) (block xxx '(1 2) (return-from nil '(4 5)) '(a b) '(2 3)))
(test::assert-equal '(5 6) (block xxx '(1 2) (block yyy (return-from xxx '(5 6)) '(a b)) '(2 3)))
(test::assert-equal '(5 6) (block xxx '(1 2) (block yyy ((fn (p) (return-from xxx p)) '(5 6)) '(a b)) '(2 3)))
(test::assert-equal '(2 3) (block xxx '(1 2) (block yyy (return-from yyy t) '(a b)) '(2 3)))
"
        ),
    );

    data.insert(
        interner.intern("intern-stats"),
        Expression::make_special(
            builtin_intern_stats,
            "Usage: (intern-stats)

Prints the stats for interned symbols.

Section: core

Example:
;(intern-stats)
t
",
        ),
    );

    data.insert(
        interner.intern("meta-line-no"),
        Expression::make_special(
            builtin_meta_line_no,
            "Usage: (meta-line-no)

Line number from the file this came from.

Section: core

Example:
;(meta-line-no)
t
",
        ),
    );

    data.insert(
        interner.intern("meta-column-no"),
        Expression::make_special(
            builtin_meta_column_no,
            "Usage: (meta-column-no)

Column number from the file this came from.

Section: core

Example:
;(meta-column-no)
t
",
        ),
    );

    data.insert(
        interner.intern("meta-file-name"),
        Expression::make_special(
            builtin_meta_file_name,
            "Usage: (meta-file-name)

File name of the file this came from.

Section: core

Example:
;(meta-file-name)
t
",
        ),
    );

    data.insert(
        interner.intern("meta-add-tags"),
        Expression::make_function(
            builtin_meta_add_tags,
            "Usage: (meta-add-tags expression tag*)

Adds multiple meta tags to an expression (see meta-add-tag).  It will work with 
symbols or vectors or lists of symbols (or any combination).
This is intended for helping with structs and interfaces in lisp, you probably
do not want to use it.

Section: core

Example:
(def meta-add-tags-var '(1 2 3))
(meta-add-tags meta-add-tags-var :tag1)
(test::assert-true (meta-tag? meta-add-tags-var :tag1))
(test::assert-false (meta-tag? meta-add-tags-var :tag2))
(meta-add-tags meta-add-tags-var :tag2 '(:tag3 :tag4) '#(:tag5 :tag6) :tag7)
(test::assert-true (meta-tag? meta-add-tags-var :tag2))
(test::assert-true (meta-tag? meta-add-tags-var :tag3))
(test::assert-true (meta-tag? meta-add-tags-var :tag4))
(test::assert-true (meta-tag? meta-add-tags-var :tag5))
(test::assert-true (meta-tag? meta-add-tags-var :tag6))
(test::assert-true (meta-tag? meta-add-tags-var :tag7))
",
        ),
    );

    data.insert(
        interner.intern("meta-tag?"),
        Expression::make_function(
            builtin_meta_tag_set,
            "Usage: (meta-tag? expression tag)

True if expression has the meta tag 'tag' set.  This is intended for helping
with structs and interfaces in lisp, you probably do not want to use it.

Section: core

Example:
(def meta-add-tag-var '(1 2 3))
(meta-add-tags meta-add-tag-var :tag1)
(test::assert-true (meta-tag? meta-add-tag-var :tag1))
(test::assert-false (meta-tag? meta-add-tag-var :tag2))
",
        ),
    );

    data.insert(
        interner.intern("="),
        Expression::make_function(
            builtin_equal,
            "Usage: (= val0 ... valN)

Equals.  Works for int, float or string.

Section: conditional

Example:
(test::assert-false (= 1 2))
(test::assert-true (= 2 2))
(test::assert-true (= 2 2 2))
(test::assert-false (= 3 2 2))
(test::assert-false (= 3.0 2.0))
(test::assert-true (= 2.0 2.0))
(test::assert-true (= 2.0 2.0 2.0))
(test::assert-false (= 3.0 2.0 2.0))
(test::assert-false (= 2.1 2.0 3.0))
(test::assert-false (= 2 1))
(test::assert-false (= 3 2 1))
(test::assert-false (= 1.1 1.0))
(test::assert-true (= 1.1 1.1))
(test::assert-false (= 3 2 3))
(test::assert-false (= \"aab\" \"aaa\"))
(test::assert-true (= \"aaa\" \"aaa\"))
(test::assert-true (= \"aaa\" \"aaa\" \"aaa\"))
(test::assert-false (= \"aaa\" \"aaaa\" \"aaa\"))
(test::assert-false (= \"ccc\" \"aab\" \"aaa\"))
(test::assert-false (= \"aaa\" \"aab\"))
",
        ),
    );
    data.insert(
        interner.intern(">"),
        Expression::make_function(
            builtin_greater_than,
            "Usage: (> val0 ... valN)

Greater than.  Works for int, float or string.

Section: conditional

Example:
(test::assert-false (> 1 2))
(test::assert-false (> 2 2))
(test::assert-false (> 2 2 2))
(test::assert-false (> 3 2 2))
(test::assert-true (> 3.0 2.0))
(test::assert-false (> 2.0 2.0))
(test::assert-false (> 2.0 2.0 2.0))
(test::assert-false (> 3.0 2.0 2.0))
(test::assert-false (> 2.1 2.0 3.0))
(test::assert-true (> 2 1))
(test::assert-true (> 3 2 1))
(test::assert-true (> 1.1 1.0))
(test::assert-false (> 3 2 3))
(test::assert-true (> \"aab\" \"aaa\"))
(test::assert-false (> \"aaa\" \"aaa\"))
(test::assert-true (> \"ccc\" \"aab\" \"aaa\"))
(test::assert-false (> \"aaa\" \"aab\"))
",
        ),
    );
    data.insert(
        interner.intern(">="),
        Expression::make_function(
            builtin_greater_than_equal,
            "Usage: (>= val0 ... valN)

Greater than or equal.  Works for int, float or string.

Section: conditional

Example:
(test::assert-false (>= 1 2))
(test::assert-true (>= 2 2))
(test::assert-true (>= 2 2 2))
(test::assert-true (>= 3 2 2))
(test::assert-true (>= 3.0 2.0))
(test::assert-true (>= 2.0 2.0))
(test::assert-true (>= 2.0 2.0 2.0))
(test::assert-true (>= 3.0 2.0 2.0))
(test::assert-false (>= 2.1 2.0 3.0))
(test::assert-true (>= 2 1))
(test::assert-true (>= 1.1 1.0))
(test::assert-false (>= 3 2 3))
(test::assert-true (>= \"aab\" \"aaa\"))
(test::assert-true (>= \"aaa\" \"aaa\"))
(test::assert-true (>= \"ccc\" \"aab\" \"aaa\"))
(test::assert-false (>= \"aaa\" \"aab\"))
",
        ),
    );
    data.insert(
        interner.intern("<"),
        Expression::make_function(
            builtin_less_than,
            "Usage: (< val0 ... valN)

Less than.  Works for int, float or string.

Section: conditional

Example:
(test::assert-true (< 1 2))
(test::assert-true (< 1 2 3 4))
(test::assert-false (< 2 2))
(test::assert-false (< 2 2 2))
(test::assert-false (< 2 2 3))
(test::assert-true (< 1.0 2.0))
(test::assert-false (< 2.0 2.0))
(test::assert-false (< 2.0 2.0 2.0))
(test::assert-false (< 2.0 2.0 3.0))
(test::assert-false (< 2.1 2.0 3.0))
(test::assert-false (< 2 1))
(test::assert-false (< 3 2 3))
(test::assert-true (< \"aaa\" \"aab\"))
(test::assert-false (< \"aaa\" \"aaa\"))
(test::assert-true (< \"aaa\" \"aab\" \"ccc\"))
(test::assert-false (< \"baa\" \"aab\"))
",
        ),
    );
    data.insert(
        interner.intern("<="),
        Expression::make_function(
            builtin_less_than_equal,
            "Usage: (<= val0 ... valN)

Less than or equal.  Works for int, float or string.

Section: conditional

Example:
(test::assert-true (<= 1 2))
(test::assert-true (<= 2 2))
(test::assert-true (<= 2 2 2))
(test::assert-true (<= 2 2 3))
(test::assert-true (<= 1.0 2.0))
(test::assert-true (<= 2.0 2.0))
(test::assert-true (<= 2.0 2.0 2.0))
(test::assert-true (<= 2.0 2.0 3.0))
(test::assert-false (<= 2.1 2.0 3.0))
(test::assert-false (<= 2 1))
(test::assert-false (<= 3 2 3))
(test::assert-true (<= \"aaa\" \"aab\"))
(test::assert-true (<= \"aaa\" \"aaa\"))
(test::assert-true (<= \"aaa\" \"aab\" \"ccc\"))
(test::assert-false (<= \"baa\" \"aab\"))
",
        ),
    );
    fn to_cow(input: &'static [u8]) -> Cow<'static, str> {
        Cow::Borrowed(from_utf8(input).expect("Builtin file is not valid UTF8!"))
    }
    data.insert(
        interner.intern("*core-src*"),
        (
            ExpEnum::String(to_cow(CORE_LISP), None).into(),
            "Usage: (print *core-src*)

The builtin source code for core.lisp.

Section: core

Example:
;(print *core-src*)
t
"
            .to_string(),
        ),
    );
    data.insert(
        interner.intern("*struct-src*"),
        (
            ExpEnum::String(to_cow(STRUCT_LISP), None).into(),
            "Usage: (print *struct-src*)

The builtin source code for struct.lisp.

Section: core

Example:
;(print *struct-src*)
t
"
            .to_string(),
        ),
    );
    data.insert(
        interner.intern("*iterator-src*"),
        (
            ExpEnum::String(to_cow(ITERATOR_LISP), None).into(),
            "Usage: (print *iterator-src*)

The builtin source code for iterator.lisp.

Section: core

Example:
;(print *iterator-src*)
t
"
            .to_string(),
        ),
    );
    data.insert(
        interner.intern("*collection-src*"),
        (
            ExpEnum::String(to_cow(COLLECTION_LISP), None).into(),
            "Usage: (print *collection-src*)

The builtin source code for collection.lisp.

Section: core

Example:
;(print *collection-src*)
t
"
            .to_string(),
        ),
    );
    data.insert(
        interner.intern("*seq-src*"),
        (
            ExpEnum::String(to_cow(SEQ_LISP), None).into(),
            "Usage: (print *seq-src*)

The builtin source code for seq.lisp.

Section: core

Example:
;(print *seq-src*)
t
"
            .to_string(),
        ),
    );
    data.insert(
        interner.intern("*shell-src*"),
        (
            ExpEnum::String(to_cow(SHELL_LISP), None).into(),
            "Usage: (print *shell-src*)

The builtin source code for shell.lisp.

Section: core

Example:
;(print *shell-src*)
t
"
            .to_string(),
        ),
    );
    data.insert(
        interner.intern("*endfix-src*"),
        (
            ExpEnum::String(to_cow(ENDFIX_LISP), None).into(),
            "Usage: (print *endfix-src*)

The builtin source code for endfix.lisp.

Section: core

Example:
;(print *endfix-src*)
t
"
            .to_string(),
        ),
    );
    data.insert(
        interner.intern("*test-src*"),
        (
            ExpEnum::String(to_cow(TEST_LISP), None).into(),
            "Usage: (print *test-src*)

The builtin source code for test.lisp.

Section: core

Example:
;(print *test-src*)
t
"
            .to_string(),
        ),
    );
    data.insert(
        interner.intern("*slsh-std-src*"),
        (
            ExpEnum::String(to_cow(SLSH_STD_LISP), None).into(),
            "Usage: (print *slsh-std-src*)

The builtin source code for slsh-std.lisp.

Section: core

Example:
;(print *slsh-std-src*)
t
"
            .to_string(),
        ),
    );
    data.insert(
        interner.intern("*slshrc-src*"),
        (
            ExpEnum::String(to_cow(SLSHRC), None).into(),
            "Usage: (print *slshrc-src*)

The builtin source code for slshrc.

Section: core

Example:
;(print *slshrc-src*)
t
"
            .to_string(),
        ),
    );
}
