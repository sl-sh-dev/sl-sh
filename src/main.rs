//extern crate jemallocator;

//#[global_allocator]
//static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

use ::sl_sh::config::*;
use ::sl_sh::shell::*;
use ::sl_sh::signals::*;
use ::sl_sh::types::LispError;
use nix::{
    libc,
    sys::signal::{self, Signal},
    unistd,
};

extern crate static_assertions;

fn check_int_functions() {
    let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
    let arg_1 = vec![
        Expression::alloc_data(ExpEnum::Int(2)),
        Expression::alloc_data(ExpEnum::Int(2)),
        Expression::alloc_data(ExpEnum::Int(2)),
        Expression::alloc_data(ExpEnum::Int(2)),
    ];
    let exp =
        builtin_ints_to_float(sl_sh::ArgType::Exp(arg_0), sl_sh::ArgType::VarArgs(arg_1)).unwrap();
    assert_eq!(16.0, exp);
    println!("var args function working!");

    let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
    let f = builtin_opt_fallible_int_to_float(
        sl_sh::ArgType::Exp(arg_0),
        sl_sh::ArgType::Opt(Some(Expression::alloc_data(ExpEnum::Int(8)))),
    )
    .unwrap();
    assert_eq!(16.0, f);
    println!("Opt fallible function that takes Some and returns bare float is ok.");

    let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
    let f =
        builtin_opt_fallible_int_to_float(sl_sh::ArgType::Exp(arg_0), sl_sh::ArgType::Opt(None))
            .unwrap();
    assert_eq!(8.0, f);
    println!("Opt fallible function that takes none and returns bare float is ok.");
    // non fallibe opts (refers to function they call, builtin_ always returns LispResult
    let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
    let f = builtin_opt_int_to_float(
        sl_sh::ArgType::Exp(arg_0),
        sl_sh::ArgType::Opt(Some(Expression::alloc_data(ExpEnum::Int(8)))),
    )
    .unwrap();
    assert_eq!(16.0, f);
    println!("Opt function that takes Some and returns bare float is ok.");

    let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
    let f =
        builtin_opt_int_to_float(sl_sh::ArgType::Exp(arg_0), sl_sh::ArgType::Opt(None)).unwrap();
    assert_eq!(8.0, f);
    println!("Opt function that takes none and returns bare float is ok.");

    let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
    let exp: Expression =
        builtin_opt_int_to_float(sl_sh::ArgType::Exp(arg_0), sl_sh::ArgType::Opt(None))
            .unwrap()
            .into();
    let exp_d = exp.get();
    match exp_d.data {
        ExpEnum::Float(f) => {
            assert_eq!(8.0, f);
            println!("tricky optional int to float working");
        }
        _ => {
            panic!("Should be float.");
        }
    }

    let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
    let arg_1 = Expression::alloc_data(ExpEnum::Int(8));
    let exp: Expression =
        builtin_int_to_float(sl_sh::ArgType::Exp(arg_0), sl_sh::ArgType::Exp(arg_1))
            .unwrap()
            .into();
    let exp_d = exp.get();
    match exp_d.data {
        ExpEnum::Float(f) => {
            assert_eq!(16.0, f);
            println!("basic int to float working");
        }
        _ => {
            panic!("Should befloat.");
        }
    }
    let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
    let exp: Expression = builtin_one_int_to_float(sl_sh::ArgType::Exp(arg_0))
        .unwrap()
        .into();
    let exp_d = exp.get();
    match exp_d.data {
        ExpEnum::Float(f) => {
            assert_eq!(8.0, f);
            println!("basic int to float working");
        }
        _ => {
            panic!("Should befloat.");
        }
    }
    let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
    builtin_print_the_int(sl_sh::ArgType::Exp(arg_0)).unwrap();

    let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
    assert!(builtin_print_the_fallible_int(sl_sh::ArgType::Exp(arg_0)).is_ok());

    let arg_0 = Expression::alloc_data(ExpEnum::Int(9));
    assert!(builtin_print_the_fallible_int(sl_sh::ArgType::Exp(arg_0)).is_err());
}

fn main() -> Result<(), LispError> {
    check_int_functions();
    if let Some(config) = get_config() {
        if config.command.is_none() && config.script.is_none() {
            /* See if we are running interactively.  */
            let shell_terminal = libc::STDIN_FILENO;
            if let Ok(true) = unistd::isatty(shell_terminal) {
                /* Loop until we are in the foreground.  */
                let mut shell_pgid = unistd::getpgrp();
                while unistd::tcgetpgrp(shell_terminal) != Ok(shell_pgid) {
                    //kill (- shell_pgid, SIGTTIN);
                    if let Err(err) = signal::kill(shell_pgid, Signal::SIGTTIN) {
                        eprintln!("Error sending sigttin: {}.", err);
                    }
                    shell_pgid = unistd::getpgrp();
                }

                mask_signals();

                /* Put ourselves in our own process group.  */
                let pgid = unistd::getpid();
                if let Err(err) = unistd::setpgid(pgid, pgid) {
                    match err {
                        nix::errno::Errno::EPERM => { /* ignore */ }
                        _ => {
                            eprintln!("Couldn't put the shell in its own process group: {}\n", err)
                        }
                    }
                }
                /* Grab control of the terminal.  */
                if let Err(err) = unistd::tcsetpgrp(shell_terminal, pgid) {
                    let msg = format!("Couldn't grab control of terminal: {}\n", err);
                    eprintln!("{}", msg);
                    return Err(LispError::new(msg));
                }

                if !install_sigint_handler() {
                    std::process::exit(1)
                }

                let code = start_interactive(true);
                std::process::exit(code);
            } else {
                // No tty, just read stdin and do something with it..
                let code = start_interactive(false);
                std::process::exit(code);
            }
        } else if config.command.is_some() {
            let command = config.command.unwrap();
            if let Err(err) = run_one_command(&command, &config.args) {
                eprintln!("Error running {}: {}", command, err);
                return Err(err);
            }
        } else if config.script.is_some() {
            let script = config.script.unwrap();
            let code = run_one_script(&script, &config.args);
            std::process::exit(code);
        }
    }
    Ok(())
}

//use std::collections::HashMap;
//fn hash_clear(exp: sl_sh::Expression) -> sl_sh::LispResult<sl_sh::Expression> {
//    let mut map_d = exp.get_mut();
//    let thing = &mut map_d.data;
//    try_inner_exp_enum!(
//        &mut map_d.data,
//        sl_sh::ExpEnum::HashMap(inner_map),
//        {
//            inner_map.clear();
//            return Ok(exp.clone());
//        },
//        "meow"
//    );
//}

use sl_sh::{ExpEnum, Expression, LispResult};
//use sl_sh::{ExpEnum, LispResult};

#[derive(Debug, Clone)]
pub enum ArgType {
    Exp(Expression),
    Opt(Option<Expression>),
    VarArgs(Vec<Expression>),
}

/// my docs
#[sl_sh_proc_macros::sl_sh_fn2(fn_name = "printint")]
fn print_the_int(int: i64) {
    println!("the int!: {}.", int as f64);
}

/// my docs
#[sl_sh_proc_macros::sl_sh_fn2(fn_name = "printint")]
fn print_the_fallible_int(int: i64) -> LispResult<()> {
    if int % 2 == 0 {
        println!("the fallible int!: {}.", int as f64);
        Ok(())
    } else {
        Err(LispError::new("int % 2 must be equal to 0."))
    }
}

/// my docs
#[sl_sh_proc_macros::sl_sh_fn2(fn_name = "oneintofloat")]
fn one_int_to_float(int: i64) -> LispResult<f64> {
    Ok(int as f64)
}

/// my docs
#[sl_sh_proc_macros::sl_sh_fn2(fn_name = "intofloat")]
fn int_to_float(int0: i64, int1: i64) -> LispResult<f64> {
    Ok((int0 + int1) as f64)
}

/// my docs
#[sl_sh_proc_macros::sl_sh_fn2(fn_name = "optintofloat")]
fn opt_fallible_int_to_float(int: i64, ints: Option<i64>) -> LispResult<f64> {
    Ok(ints.map_or(int as f64, |new| (int + new) as f64))
}

/// my docs
#[sl_sh_proc_macros::sl_sh_fn2(fn_name = "optintofloat")]
fn opt_int_to_float(int: i64, ints: Option<i64>) -> f64 {
    ints.map_or(int as f64, |new| (int + new) as f64)
}

/// my docs
#[sl_sh_proc_macros::sl_sh_fn2(fn_name = "intstofloat")]
fn ints_to_float(int: i64, ints: Vec<i64>) -> LispResult<f64> {
    Ok(ints.iter().fold(int as f64, |sum, next| sum + *next as f64))
}

///// my docs
//#[sl_sh_proc_macros::sl_sh_fn2(fn_name = "my_hash_clear")]
//fn my_hash_clear(inner_map: &mut HashMap<&str, Expression>) -> LispResult<()> {
//    inner_map.clear();
//    Ok(())
//}

//#[cfg(test)]
//mod test {
//    use super::*;
//}
