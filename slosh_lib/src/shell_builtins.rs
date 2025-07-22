use crate::SHELL_ENV;
use bridge_adapters::add_builtin;
use bridge_macros::sl_sh_fn;
use bridge_types::LooseInt;
use compile_state::state::SloshVm;
use shell::platform::{FromFileDesc, Platform, Sys};
use slvm::io::HeapIo;
use slvm::{VMError, VMResult, Value};
use std::collections::HashSet;
use std::env::VarError;
use std::fs::File;
use std::io::BufRead;
use std::process;
use std::{env, io};

fn sh(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut result = Vec::new();
    let mut new_regs = Vec::with_capacity(registers.len());
    new_regs.extend_from_slice(registers);
    let mut fds_close = HashSet::new();
    for r in new_regs.iter_mut() {
        if let Value::Keyword(i) = r {
            let key = vm.get_interned(*i);
            if key.ends_with("<") {
                match Sys::anon_pipe() {
                    Ok((inp, outp)) => {
                        let mut key = key.to_string();
                        key.push_str(&format!("&{inp}"));
                        fds_close.insert(inp);
                        *r = vm.alloc_string(key);
                        let file = HeapIo::from_file(unsafe { File::from_file_desc(outp) });
                        result.push(vm.alloc_io(file));
                    }
                    Err(e) => return Err(VMError::new_compile(format!("sh: pipe failed {e}"))),
                }
            }
            if key.ends_with(">") {
                match Sys::anon_pipe() {
                    Ok((inp, outp)) => {
                        let mut key = key.to_string();
                        key.push_str(&format!("&{outp}"));
                        fds_close.insert(outp);
                        *r = vm.alloc_string(key);
                        let file = HeapIo::from_file(unsafe { File::from_file_desc(inp) });
                        file.to_buf_reader().map_err(|e| {
                            VMError::new_compile(format!("sh: to buf reader failed {e:?}"))
                        })?;
                        result.push(vm.alloc_io(file));
                    }
                    Err(e) => return Err(VMError::new_compile(format!("sh: pipe failed {e}"))),
                }
            }
        }
    }
    let mut command = String::new();
    for exp in new_regs {
        match exp {
            Value::String(h) => command.push_str(vm.get_string(h)),
            Value::StringConst(i) => command.push_str(vm.get_interned(i)),
            _ => command.push_str(&exp.display_value(vm)),
        }
        command.push(' ');
    }
    if command.is_empty() {
        return Err(VMError::new_compile("sh: empty command"));
    }
    let mut fork_res = Ok(0);
    let mut run_res = Err(io::Error::other("broken parse"));
    SHELL_ENV.with(|jobs_ref| {
        let jobs = &mut jobs_ref.borrow_mut();
        run_res = shell::parse::parse_line(jobs, &command)
    });
    let mut run = run_res
        .map_err(|e| VMError::new_compile(format!("sh: {e}")))?
        .into_run();
    if !fds_close.is_empty() {
        run.fds_to_internal(&fds_close);
    }
    let background = false; // !result.is_empty();
    SHELL_ENV.with(|jobs_ref| {
        let jobs = &mut jobs_ref.borrow_mut();
        fork_res = shell::run::run_job(&run, jobs, background);
    });
    let fork_res = fork_res.map_err(|e| VMError::new_compile(format!("sh 2: {e}")))?;
    if result.is_empty() {
        Ok(fork_res.into())
    } else {
        result.insert(0, fork_res.into());
        Ok(vm.alloc_vector(result))
    }
}

/// Usage: (exit & exit-code) => no-return
///
/// Exit the shell with the specified exit code.
///
/// Arguments:
/// - exit-code: An integer (optional). The exit status code (defaults to 0).
/// - no-return: This function never returns.
///
/// Section: core
///
/// Example:
/// #t
#[allow(unreachable_code)]
#[sl_sh_fn(fn_name = "exit")]
fn exit(val: Option<LooseInt>) -> VMResult<Value> {
    let exit_code = if let Some(val) = val {
        let val = slvm::from_i56(&val.0);
        val as i32
    } else {
        0
    };
    process::exit(exit_code);
}

fn sh_str(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut command = String::new();
    for exp in registers {
        match exp {
            Value::String(h) => command.push_str(vm.get_string(*h)),
            Value::StringConst(i) => command.push_str(vm.get_interned(*i)),
            _ => command.push_str(&exp.display_value(vm)),
        }
        command.push(' ');
    }
    if command.is_empty() {
        return Err(VMError::new_compile("$sh: empty command"));
    }
    let mut fork_res = Ok(0);
    let mut run_res = Err(io::Error::other("broken parse"));
    SHELL_ENV.with(|jobs_ref| {
        let jobs = &mut jobs_ref.borrow_mut();
        run_res = shell::parse::parse_line(jobs, &command)
    });
    let mut run = run_res
        .map_err(|e| VMError::new_compile(format!("$sh: {e}")))?
        .into_run();
    let (input, output) = Sys::anon_pipe()?;
    run.push_stdout_front(Some(output));
    SHELL_ENV.with(|jobs_ref| {
        let jobs = &mut jobs_ref.borrow_mut();
        fork_res = shell::run::run_job(&run, jobs, true);
    });
    fork_res.map_err(|e| VMError::new_compile(format!("$sh: {e}")))?;
    let lines = io::BufReader::new(unsafe { File::from_file_desc(input) }).lines();
    let mut val = String::new();
    for (i, line) in lines.enumerate() {
        if i > 0 {
            val.push(' ');
        }
        let line = line?;
        val.push_str(line.trim());
    }
    Ok(vm.alloc_string(val))
}

fn env_var(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if let (Some(name), None) = (registers.first(), registers.get(1)) {
        let name = match name {
            Value::String(h) => vm.get_string(*h),
            Value::StringConst(i) => vm.get_interned(*i),
            Value::Symbol(i) => vm.get_interned(*i),
            _ => {
                return Err(VMError::new_compile(
                    "env: requires string or symbol argument",
                ));
            }
        };
        match env::var(name) {
            Ok(val) => Ok(vm.alloc_string(val)),
            Err(VarError::NotPresent) => Ok(vm.alloc_string("".to_string())),
            Err(err) => Err(VMError::new_compile(format!(
                "env: error finding env var {name}: {err}"
            ))),
        }
    } else {
        Err(VMError::new_compile(
            "$sh: wrong number of args, expected one",
        ))
    }
}

pub fn add_shell_builtins(env: &mut SloshVm) {
    intern_exit(env);
    add_builtin(
        env,
        "$sh",
        sh_str,
        "Runs a shell command and returns a string of the output with newlines removed.",
    );
    add_builtin(
        env,
        "sh",
        sh,
        "Runs a shell command and returns it's status.",
    );
    add_builtin(env, "env", env_var, "Retrieves an environment variable.");
}
