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

fn main() -> Result<(), LispError> {
    let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
    let arg_1 = vec![
        Expression::alloc_data(ExpEnum::Int(2)),
        Expression::alloc_data(ExpEnum::Int(2)),
        Expression::alloc_data(ExpEnum::Int(2)),
        Expression::alloc_data(ExpEnum::Int(2)),
    ];
    let exp =
        builtin_ints_to_float(sl_sh::ArgType::Exp(arg_0), sl_sh::ArgType::VarArgs(arg_1)).unwrap();
    let exp_d = exp.get();
    match exp_d.data {
        ExpEnum::Float(f) => {
            assert_eq!(16.0, f);
            println!("variadic ints to float working");
        }
        _ => {
            panic!("Should befloat.");
        }
    }

    let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
    let arg_1 = Expression::alloc_data(ExpEnum::Int(8));
    let exp =
        builtin_opt_int_to_float(sl_sh::ArgType::Exp(arg_0), sl_sh::ArgType::Opt(Some(arg_1)))
            .unwrap();
    let exp_d = exp.get();
    match exp_d.data {
        ExpEnum::Float(f) => {
            assert_eq!(16.0, f);
            println!("optional int to float working");
        }
        _ => {
            panic!("Should befloat.");
        }
    }

    let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
    let exp =
        builtin_opt_int_to_float(sl_sh::ArgType::Exp(arg_0), sl_sh::ArgType::Opt(None)).unwrap();
    let exp_d = exp.get();
    match exp_d.data {
        ExpEnum::Float(f) => {
            assert_eq!(8.0, f);
            println!("tricky optional int to float working");
        }
        _ => {
            panic!("Should befloat.");
        }
    }

    let arg_0 = Expression::alloc_data(ExpEnum::Int(8));
    let arg_1 = Expression::alloc_data(ExpEnum::Int(8));
    let exp = builtin_int_to_float(sl_sh::ArgType::Exp(arg_0), sl_sh::ArgType::Exp(arg_1)).unwrap();
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

pub type LispResult<T> = Result<T, LispError>;

use sl_sh::{ExpEnum, Expression};
//use sl_sh::{ExpEnum, LispResult};

#[derive(Debug, Clone)]
pub enum ArgType {
    Exp(Expression),
    Opt(Option<Expression>),
    VarArgs(Vec<Expression>),
}

/// my docs
#[sl_sh_proc_macros::sl_sh_fn2(fn_name = "intofloat")]
fn int_to_float(int: i64, ints: i64) -> sl_sh::LispResult<f64> {
    Ok((int + ints) as f64)
}

/// my docs
#[sl_sh_proc_macros::sl_sh_fn2(fn_name = "optintofloat")]
fn opt_int_to_float(int: i64, ints: Option<i64>) -> sl_sh::LispResult<f64> {
    Ok(ints.map_or(int as f64, |new| (int + new) as f64))
}

/// my docs
#[sl_sh_proc_macros::sl_sh_fn2(fn_name = "intstofloat")]
fn ints_to_float(int: i64, ints: Vec<i64>) -> sl_sh::LispResult<f64> {
    Ok(ints.iter().fold(int as f64, |sum, next| sum + *next as f64))
}

#[cfg(test)]
mod test {
    use super::*;

    fn builtin_opt_int_to_float(
        arg_0: sl_sh::ArgType,
        arg_1: sl_sh::ArgType,
    ) -> sl_sh::LispResult<Expression> {
        match arg_1 {
            sl_sh::ArgType::Opt(arg_1) => match arg_1 {
                None => {
                    let arg_1 = None;
                    match arg_0 {
                        sl_sh::ArgType::Exp(arg_0) => match arg_0.get().data {
                            sl_sh::ExpEnum::Int(arg_0) => {
                                opt_int_to_float(arg_0, arg_1).map(Into::into)
                            }
                            _ => {
                                return Err(
                                            LispError::new(
                                                "sl_sh_fn macro is broken, ArgType::Exp can't be parsed as ArgType::Exp",
                                            ),
                                        );
                            }
                        },
                        _ => {
                            return Err(
                                    LispError::new(
                                        "sl_sh_fn macro is broken, apparently ArgType::Exp can't be parsed as ArgType::Exp",
                                    ),
                                );
                        }
                    }
                }
                Some(arg_1) => match arg_1.get().data {
                    sl_sh::ExpEnum::Int(arg_1) => {
                        let arg_1 = Some(arg_1);
                        match arg_0 {
                            sl_sh::ArgType::Exp(arg_0) => match arg_0.get().data {
                                sl_sh::ExpEnum::Int(arg_0) => {
                                    opt_int_to_float(arg_0, arg_1).map(Into::into)
                                }
                                _ => {
                                    return Err(
                                                    LispError::new(
                                                        "sl_sh_fn macro is broken, ArgType::Exp can't be parsed as ArgType::Exp",
                                                    ),
                                                );
                                }
                            },
                            _ => {
                                return Err(
                                            LispError::new(
                                                "sl_sh_fn macro is broken, apparently ArgType::Exp can't be parsed as ArgType::Exp",
                                            ),
                                        );
                            }
                        }
                    }
                    _ => {
                        return Err(
                                    LispError::new(
                                        "sl_sh_fn macro is broken, ArgType::Opt can't be parsed as ArgType::Opt",
                                    ),
                                );
                    }
                },
            },
            _ => {
                return Err(
                    LispError::new(
                        "sl_sh_fn macro is broken, apparently ArgType::Opt can't be parsed as ArgType::Opt",
                    ),
                );
            }
        }
    }
}
