use crate::SHELL_ENV;
use bridge_adapters::add_builtin;
use compile_state::state::SloshVm;
use shell::platform::{FromFileDesc, Platform, Sys};
use slvm::io::HeapIo;
use slvm::{VMError, VMResult, Value};
use std::env::VarError;
use std::fs::File;
use std::io::{BufRead, ErrorKind};
use std::{env, io};

fn sh(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let pipe_key = vm.intern_static(">");
    let mut reg_start = 0;
    let mut reg_end = registers.len();
    let pipe_in = match registers.first() {
        Some(Value::Keyword(k)) if *k == pipe_key => {
            reg_start = 1;
            true
        }
        _ => false,
    };
    let pipe_out = match registers.last() {
        Some(Value::Keyword(k)) if *k == pipe_key => {
            if reg_end > reg_start {
                reg_end -= 1;
            }
            true
        }
        _ => false,
    };
    /*let pipe_err = match registers.last() {
        Some(Value::Keyword(k)) if *k == pipe_key => true,
        _ => false,
    };*/
    let mut command = String::new();
    for exp in registers[reg_start..reg_end].iter() {
        match exp {
            Value::String(h) => command.push_str(vm.get_string(*h)),
            Value::StringConst(i) => command.push_str(vm.get_interned(*i)),
            _ => command.push_str(&exp.display_value(vm)),
        }
        command.push(' ');
    }
    if command.is_empty() {
        return Err(VMError::new_compile("sh: empty command"));
    }
    let mut fork_res = Ok(0);
    let mut run_res = Err(io::Error::new(ErrorKind::Other, "broken parse"));
    SHELL_ENV.with(|jobs_ref| {
        let jobs = &mut jobs_ref.borrow_mut();
        run_res = shell::parse::parse_line(jobs, &command)
    });
    let mut run = run_res
        .map_err(|e| VMError::new_compile(format!("sh: {e}")))?
        .into_run();
    let (stdinp, stdoutp) = match (pipe_in, pipe_out) {
        (true, true) => {
            let stdinp = match Sys::anon_pipe() {
                Ok((inp, outp)) => {
                    run.push_stdin_front(Some(inp));
                    unsafe { File::from_file_desc(outp) }
                }
                Err(e) => return Err(VMError::new_compile(format!("sh: pipe failed {e}"))),
            };
            match Sys::anon_pipe() {
                Ok((inp, outp)) => {
                    run.push_stdout_front(Some(outp));
                    (Some(stdinp), Some(unsafe { File::from_file_desc(inp) }))
                }
                Err(e) => return Err(VMError::new_compile(format!("sh: pipe failed {e}"))),
            }
        }
        (true, false) => match Sys::anon_pipe() {
            Ok((inp, outp)) => {
                run.push_stdin_front(Some(inp));
                (Some(unsafe { File::from_file_desc(outp) }), None)
            }
            Err(e) => return Err(VMError::new_compile(format!("sh: pipe failed {e}"))),
        },
        (false, true) => match Sys::anon_pipe() {
            Ok((inp, outp)) => {
                run.push_stdout_front(Some(outp));
                (None, Some(unsafe { File::from_file_desc(inp) }))
            }
            Err(e) => return Err(VMError::new_compile(format!("sh: pipe failed {e}"))),
        },
        (false, false) => (None, None),
    };
    let background = stdinp.is_some() || stdoutp.is_some();
    SHELL_ENV.with(|jobs_ref| {
        let jobs = &mut jobs_ref.borrow_mut();
        fork_res = shell::run::run_job(&run, jobs, background);
    });
    match (stdinp, stdoutp) {
        (None, None) => fork_res
            .map(|i| i.into())
            .map_err(|e| VMError::new_compile(format!("sh: {e}"))),
        (Some(stdinp), None) => {
            let v = vec![vm.alloc_io(HeapIo::from_file(stdinp))];
            Ok(vm.alloc_vector(v))
        }
        (None, Some(stdoutp)) => {
            let outp = HeapIo::from_file(stdoutp);
            outp.to_buf_reader()
                .map_err(|e| VMError::new_compile(format!("sh: to buf reader failed {e:?}")))?;
            let v = vec![vm.alloc_io(outp)];
            Ok(vm.alloc_vector(v))
        }
        (Some(stdinp), Some(stdoutp)) => {
            let v = vec![
                vm.alloc_io(HeapIo::from_file(stdinp)),
                vm.alloc_io(HeapIo::from_file(stdoutp)),
            ];
            Ok(vm.alloc_vector(v))
        }
    }
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
    let mut run_res = Err(io::Error::new(ErrorKind::Other, "broken parse"));
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
                ))
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
