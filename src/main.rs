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
    let arg_1 = Expression::alloc_data(ExpEnum::Int(8));
    let exp = builtin_int_to_float(sl_sh::ArgType::Exp(arg_0), sl_sh::ArgType::Exp(arg_1)).unwrap();
    let exp_d = exp.get();
    match exp_d.data {
        ExpEnum::Float(f) => {
            assert_eq!(16.0, f);
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

#[macro_export]
macro_rules! try_inner_exp_enum {
    ($expression:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?, $eval:expr, $err:expr) => {
        match $expression {
            $( $pattern )|+ $( if $guard )? => $eval,
            _ => return Err(LispError::new($err))
        }
    };
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
    //Ok(ints.iter().fold(int as f64, |sum, next| sum + *next as f64))
    Ok((int + ints) as f64)
}

//fn builtin_int_to_float(
//    exp_0: sl_sh::ArgType,
//    exp_1: sl_sh::ArgType,
//) -> sl_sh::LispResult<sl_sh::types::Expression> {
//    try_exp_enum!(
//        exp_0,
//        sl_sh::ArgType::Exp(exp_0),
//        {
//            try_exp_enum!(
//                exp_1,
//                sl_sh::ArgType::VarArgs(exp_1),
//                //{ builtin_int_to_float(arg0, arg1).map(Into::into) }?,
//                {
//                    try_exp_enum!(
//                        exp_0.get().data,
//                        sl_sh::ExpEnum::Int(exp_0),
//                        {
//                            let iter = exp_1
//                                .iter()
//                                .map(|exp_1| {
//                                    let int = try_inner_exp_enum!(
//                                        exp_1.get().data,
//                                        sl_sh::ExpEnum::Int(int_1),
//                                        { int_1 },
//                                        "Not an int_1 in this vec!"
//                                    );
//                                    Ok(int)
//                                })
//                                .collect::<sl_sh::LispResult<Vec<i64>>>()?;
//                            int_to_float(exp_0, &iter).map(Into::into)
//                        }?,
//                        "err turning first arg to expenum::int"
//                    )
//                }?,
//                "err turning exp to varargs"
//            )
//        }?,
//        "err turning exp to exp"
//    )
//}

// TODO vectors!?
// aren't they done?

//fn arg_translate_ints_2_float(arg_0: ArgType, arg_1: ArgType) -> crate::LispResult<Expression> {
//    let exp_0 = try_inner_exp_enum!(arg_0, ArgType::Exp(exp), exp, "err");
//    let exp_1 = try_inner_exp_enum!(arg_1, ArgType::VarArgs(exp), exp, "err");
//    arg_unwrap_ints_2_float(exp_0, exp_1).map(Into::into)
//}
//
//fn arg_unwrap_ints_2_float(exp_0: Expression, exp_1: Vec<Expression>) -> crate::LispResult<f64> {
//    Ok({
//        let float = try_inner_exp_enum!(
//            exp_0.get().data,
//            ExpEnum::Int(int_0),
//            {
//                ints_2_float(
//                    int_0,
//                    exp_1
//                        .iter()
//                        .map(|exp_1| {
//                            let int = try_inner_exp_enum!(
//                                exp_1.get().data,
//                                ExpEnum::Int(int_1),
//                                { int_1 },
//                                "Not an int_1!"
//                            );
//                            int
//                        })
//                        .collect::<Vec<i64>>()?,
//                )
//            },
//            "Not an int_0!"
//        );
//        float
//    })
//}
//
//fn ints_2_float(my_int: i64, my_o_ints: Vec<i64>) -> f64 {
//    my_o_ints
//        .iter()
//        .fold(my_int as f64, |sum, val| sum + val as f64)
//}
//
