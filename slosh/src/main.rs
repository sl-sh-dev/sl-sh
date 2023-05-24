extern crate sl_liner;

use std::cell::RefCell;
use std::ffi::OsString;
use std::fs::File;
use std::io::{BufRead, ErrorKind};
use std::os::fd::FromRawFd;
use std::sync::Arc;
use std::{env, io};

use nix::unistd::{gethostname, Uid};

use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;

use compile_state::state::*;
use sl_compiler::compile::*;
use sl_compiler::reader::*;

use builtins::collections::{make_hash, vec_slice, vec_to_list};
use builtins::print::{dasm, display_value, pr, prn};
use builtins::string::{str_contains, str_ltrim, str_replace, str_rtrim, str_trim};
use builtins::{gensym, get_prop, set_prop, sizeof_heap_object, sizeof_value};
use sl_liner::{Context, Prompt};
use slvm::Chunk;

mod config;
pub mod debug;

use config::*;
use debug::*;
use sl_compiler::pass1::pass1;
use slvm::Value;

thread_local! {
    /// Env (job control status, etc) for the shell.
    pub static SHELL_ENV: RefCell<shell::jobs::Jobs> = RefCell::new(shell::jobs::Jobs::new(true));
}

fn load_one_expression(
    vm: &mut SloshVm,
    exp: Value,
    name: &'static str,
    doc_string: Option<Value>,
) -> VMResult<(Arc<Chunk>, Option<Value>)> {
    let line_num = vm.line_num();
    let mut state = CompileState::new_state(name, line_num, None);
    state.chunk.dbg_args = Some(Vec::new());
    state.doc_string = doc_string;
    if let Err(e) = pass1(vm, &mut state, exp) {
        println!(
            "Compile error (pass one), {}, line {}: {}",
            name,
            vm.line_num(),
            e
        );
        return Err(e);
    }
    if let Err(e) = compile(vm, &mut state, exp, 0) {
        println!(
            "Compile error, {} line {}: {} exp: {}",
            name,
            vm.line_num(),
            e,
            exp.display_value(vm)
        );
        return Err(e);
    }
    if let Err(e) = state.chunk.encode0(RET, vm.own_line()) {
        println!("Compile error, {} line {}: {}", name, vm.line_num(), e);
        return Err(e);
    }
    state.chunk.extra_regs = state.max_regs;
    Ok((Arc::new(state.chunk), state.doc_string))
}

fn load_internal(vm: &mut SloshVm, name: &'static str) -> VMResult<Value> {
    let file = std::fs::File::open(name)?;

    let mut last = Value::Nil;
    let mut reader = Reader::from_file(file, vm, name, 1, 0);
    let mut doc_string = None;
    while let Some(exp) = reader.next() {
        let reader_vm = reader.vm();
        let exp = exp.map_err(|e| VMError::new("read", e.to_string()))?;
        reader_vm.heap_sticky(exp);

        let result = load_one_expression(reader_vm, exp, name, doc_string);

        reader_vm.heap_unsticky(exp);
        let (chunk, new_doc_string) = result?;
        doc_string = new_doc_string;
        last = reader_vm.execute(chunk)?;
    }
    Ok(last)
}

fn load(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_compile(
            "load: wrong number of args, expected one",
        ));
    }
    let name = match registers[0].unref(vm) {
        Value::StringConst(i) => vm.get_interned(i),
        Value::String(h) => {
            let s = vm.get_string(h);
            let s = s.to_string();
            let s_i = vm.intern(&s);
            vm.get_interned(s_i)
        }
        _ => return Err(VMError::new_vm("load: Not a string.")),
    };
    let olf_line_num = vm.line_num();
    vm.set_line_num(1);
    let r = load_internal(vm, name);
    vm.set_line_num(olf_line_num);
    r
}

fn eval(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if let (Some(exp), None) = (registers.get(0), registers.get(1)) {
        let mut env = new_slosh_vm();
        let line_num = env.line_num();
        let mut state = CompileState::new_state("none/eval", line_num, None);
        state.chunk.dbg_args = Some(Vec::new());
        pass1(&mut env, &mut state, *exp)?;
        compile(&mut env, &mut state, *exp, 0)?;
        state.chunk.encode0(RET, env.own_line())?;
        let chunk = Arc::new(state.chunk.clone());
        Ok(vm.do_call(chunk, &[Value::Nil], None)?)
    } else {
        Err(VMError::new_compile(
            "eval: wrong number of args, expected one",
        ))
    }
}

fn sh_str(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if let (Some(exp), None) = (registers.get(0), registers.get(1)) {
        let run = match exp {
            Value::String(h) => vm.get_string(*h),
            Value::StringConst(i) => vm.get_interned(*i),
            _ => return Err(VMError::new_compile("$sh: requires string argument")),
        };
        let mut run = shell::parse::parse_line(run)
            .map_err(|e| VMError::new_compile(format!("$sh: {e}")))?
            .into_run();
        let (input, output) = shell::unix::anon_pipe()?;
        run.push_stdout_front(Some(output));
        let mut fork_res = Ok(0);
        SHELL_ENV.with(|jobs| {
            //let mut job = jobs.borrow_mut().new_job();
            //fork_res = shell::unix::fork_run(&run, &mut job, &mut *jobs.borrow_mut());
            fork_res = shell::run::run_job(&run, &mut jobs.borrow_mut(), true);
        });
        fork_res.map_err(|e| VMError::new_compile(format!("$sh: {e}")))?;
        let lines = io::BufReader::new(unsafe { File::from_raw_fd(input) }).lines();
        let mut val = String::new();
        for (i, line) in lines.enumerate() {
            if i > 0 {
                val.push(' ');
            }
            let line = line?;
            val.push_str(line.trim());
        }
        Ok(vm.alloc_string(val))
    } else {
        Err(VMError::new_compile(
            "$sh: wrong number of args, expected one",
        ))
    }
}

fn version(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_compile("version: requires no argument"));
    }
    Ok(vm.alloc_string(VERSION_STRING.to_string()))
}

const PROMPT_FN: &str = "prompt";

pub fn add_builtin(
    env: &mut SloshVm,
    name: &str,
    func: CallFuncSig<CompileEnvironment>,
    doc_string: &str,
) {
    let si = env.set_global_builtin(name, func);
    let key = env.intern("doc-string");
    let s = env.alloc_string(doc_string.to_string());
    env.set_global_property(si, key, s);
}

pub fn add_docstring(env: &mut SloshVm, name: &str, doc_string: &str) {
    let si = env.set_named_global(name, Value::Undefined);
    let key = env.intern("doc-string");
    let s = env.alloc_string(doc_string.to_string());
    env.set_global_property(si, key, s);
}

pub fn setup_vecs(env: &mut SloshVm) {
    add_docstring(
        env,
        "vec",
        "Usage: (vec item1 item2 .. itemN)

Make a new vector with items.

Section: vector

Example:
(test::assert-equal '() (vec))
(test::assert-equal '(1 2 3) (vec 1 2 3))
",
    );
    add_docstring(
        env,
        "make-vec",
        "Usage: (make-vec capacity default)

Make a new vector with capacity and default item(s).

Section: vector

Example:
(test::assert-equal '() (make-vec))
(test::assert-equal '(x x x) (make-vec 3 'x))
(test::assert-equal '(nil nil nil nil nil) (make-vec 5 nil))
(test::assert-equal '() (make-vec 5))
",
    );
    add_docstring(
        env,
        "vec-nth",
        "Usage: (vec-nth vector index) -> object

Get the nth element (0 based) of a vector. If you need the equivalent operation
on a list use [nth](root::nth).

Section: vector

Example:
(test::assert-equal 5 (vec-nth '#(1 2 3 4 5 6) 4))
(test::assert-equal 1 (vec-nth '#(1 2 3 4 5 6) 0))
(test::assert-equal 3 (vec-nth '#(1 2 3 4 5 6) 2))
(test::assert-equal 6 (vec-nth '#(1 2 3 4 5 6) 5))
",
    );
    add_docstring(
        env,
        "vec-set!",
        "Usage: (vec-set! vector index value) -> vector

Set the nth index (0 based) of a vector to value. This is destructive! If you
need the equivalent operation on a list use [setnth!](root::setnth!).

Section: vector

Example:
(def test-setnth-vec (vec 1 2 3))
(test::assert-equal '(1 5 3) (vec-set! test-setnth-vec 1 5))
(test::assert-equal '(7 5 3) (vec-set! test-setnth-vec 0 7))
(test::assert-equal '(7 5 9) (vec-set! test-setnth-vec 2 9))
",
    );
    add_docstring(
        env,
        "vec-push!",
        "Usage: (vec-push! vector object) -> vector

Pushes the provided object onto the end of the vector.  This is destructive!

Section: vector

Example:
(def test-push-vec (vec))
(test::assert-equal '(1) (vec-push! test-push-vec 1))
(test::assert-equal '(1) test-push-vec)
(test::assert-equal '(1 2) (vec-push! test-push-vec 2))
(test::assert-equal '(1 2) test-push-vec)
(test::assert-equal '(1 2 3) (vec-push! test-push-vec 3))
(test::assert-equal '(1 2 3) test-push-vec)
",
    );
    add_docstring(
        env,
        "vec-pop!",
        "Usage: (vec-pop! vector) -> object

Pops the last object off of the end of the vector.  This is destructive!

Section: vector

Example:
(def test-pop-vec (vec 1 2 3))
(test::assert-equal 3 (vec-pop! test-pop-vec))
(test::assert-equal '(1 2) test-pop-vec)
(test::assert-equal 2 (vec-pop! test-pop-vec))
(test::assert-equal '(1) test-pop-vec)
(test::assert-equal 1 (vec-pop! test-pop-vec))
(test::assert-equal '() test-pop-vec)
",
    );
    add_builtin(
        env,
        "vec-slice",
        vec_slice,
        "Usage: (vec-slice vector start end?)

Returns a slice of a vector (0 based indexes, end is exclusive).

Section: vector

Example:
(test::assert-equal '(5 6) (vec-slice '#(1 2 3 4 5 6) 4 6))
(test::assert-equal '(1 2 3) (vec-slice '#(1 2 3 4 5 6) 0 3))
(test::assert-equal '(3 4 5) (vec-slice '#(1 2 3 4 5 6) 2 5))
(test::assert-equal '(3 4 5 6) (vec-slice '#(1 2 3 4 5 6) 2))
",
    );
    add_builtin(
        env,
        "vec->list",
        vec_to_list,
        "Usage: (vec->list vector)

Convert a vector to a list.

Section: vector
",
    );

    add_docstring(
        env,
        "vec-empty?",
        "Usage: (vec-empty? vector)

True if the vector is empty.

Section: vector

Example:
(test::assert-true (vec-empty? '#()))
(test::assert-false (vec-empty? '#(1 2 3)))
",
    );
    add_docstring(
        env,
        "vec-clear!",
        "Usage: (vec-clear! vector)

Clears a vector.  This is destructive!

Section: vector

Example:
(def test-clear-vec (vec 1 2 3))
(test::assert-false (vec-empty? test-clear-vec))
(vec-clear! test-clear-vec)
(test::assert-true (vec-empty? test-clear-vec))
",
    );
    add_docstring(
        env,
        "vec-remove!",
        "Usage: (vec-remove! vector index) -> vector

Remove the element at index from vector, shifting all elements after it to the left.
This is destructive!

Section: vector

Example:
(def test-remove-nth-vec (vec 1 2 3))
(test::assert-equal '(1 2 3) test-remove-nth-vec)
(vec-remove! test-remove-nth-vec 1)
(test::assert-equal '(1 3) test-remove-nth-vec)
(vec-remove! test-remove-nth-vec 1)
(test::assert-equal '(1) test-remove-nth-vec)
(vec-remove! test-remove-nth-vec 0)
(test::assert-equal '() test-remove-nth-vec)
",
    );
    add_docstring(
        env,
        "vec-insert!",
        "Usage: (vec-insert! vector index new-element) -> vector

Inserts new-element at index and moves following elements right in vector.  This is destructive!

Section: vector

Example:
(def test-insert-nth-vec (vec 1 2 3))
(test::assert-equal '(1 2 3) test-insert-nth-vec)
(vec-insert! test-insert-nth-vec 1 5)
(test::assert-equal '(1 5 2 3) test-insert-nth-vec)
(vec-insert! test-insert-nth-vec 2 6)
(test::assert-equal '(1 5 6 2 3) test-insert-nth-vec)
(vec-insert! test-insert-nth-vec 0 4)
(test::assert-equal '(4 1 5 6 2 3) test-insert-nth-vec)
",
    );
    add_builtin(
        env,
        "$sh",
        sh_str,
        "Runs a shell command and returns a string of the output with newlines removed.",
    );
    add_builtin(
        env,
        "version",
        version,
        "Return the software version string.",
    );
}

fn get_prompt(env: &mut SloshVm) -> String {
    let i_val = env.intern("__prompt");
    if let Some(idx) = env.global_intern_slot(i_val) {
        match env.get_global(idx) {
            Value::Lambda(h) => {
                let l = env.get_lambda(h);
                match env.do_call(l, &[], None) {
                    Ok(v) => match v {
                        Value::StringConst(i) => env.get_interned(i).to_string(),
                        Value::String(h) => env.get_string(h).to_string(),
                        _ => v.display_value(env),
                    },
                    Err(e) => {
                        eprintln!("Error getting prompt: {e}");
                        "slosh> ".to_string()
                    }
                }
            }
            _ => env.get_global(idx).display_value(env),
        }
    } else {
        "slosh> ".to_string()
    }
}

fn main() {
    if let Some(config) = get_config() {
        let mut env = new_slosh_vm();
        setup_vecs(&mut env);
        env.set_global_builtin("pr", pr);
        env.set_global_builtin("prn", prn);
        env.set_global_builtin("dasm", dasm);
        env.set_global_builtin("load", load);
        env.set_global_builtin("make-hash", make_hash);
        env.set_global_builtin("get-prop", get_prop);
        env.set_global_builtin("set-prop", set_prop);
        env.set_global_builtin("eval", eval);
        env.set_global_builtin("sizeof-heap-object", sizeof_heap_object);
        env.set_global_builtin("sizeof-value", sizeof_value);
        env.set_global_builtin("gensym", gensym);
        env.set_global_builtin("str-replace", str_replace);
        env.set_global_builtin("str-trim", str_trim);
        env.set_global_builtin("str-rtrim", str_rtrim);
        env.set_global_builtin("str-ltrim", str_ltrim);
        env.set_global_builtin("str-contains", str_contains);
        let uid = Uid::current();
        let euid = Uid::effective();
        env::set_var("UID", format!("{}", uid));
        env::set_var("EUID", format!("{}", euid));
        env.set_named_global("*uid*", Value::UInt32(uid.into()));
        env.set_named_global("*euid*", Value::UInt32(euid.into()));
        env.set_named_global("*last-status*", Value::Int32(0));
        // Initialize the HOST variable
        let host: OsString = gethostname().ok().unwrap_or_else(|| "???".into());
        env::set_var("HOST", &host);
        if let Ok(dir) = env::current_dir() {
            env::set_var("PWD", dir);
        }
        if config.command.is_none() && config.script.is_none() {
            let mut con = Context::new();

            if let Err(e) = con.history.set_file_name_and_load_history("history") {
                println!("Error loading history: {e}");
            }
            shell::run::setup_shell_tty(0);
            //let mut jobs = shell::jobs::Jobs::new(true);
            SHELL_ENV.with(|jobs| {
                jobs.borrow_mut().cap_term();
            });
            loop {
                SHELL_ENV.with(|jobs| {
                    jobs.borrow_mut().reap_procs();
                });
                let prompt = get_prompt(&mut env);
                let res = match con.read_line(Prompt::from(prompt), None) {
                    Ok(input) => input,
                    Err(err) => match err.kind() {
                        ErrorKind::UnexpectedEof => {
                            break;
                        }
                        ErrorKind::Interrupted => {
                            continue;
                        }
                        _ => {
                            // Usually can just restore the tty and be back in action.
                            SHELL_ENV.with(|jobs| {
                                jobs.borrow_mut().restore_terminal();
                            });
                            eprintln!("Error on input: {err}");
                            continue;
                        }
                    },
                };

                if res.is_empty() {
                    continue;
                }

                con.history.push(&res).expect("Failed to push history.");
                if res.starts_with('(') {
                    exec_expression(res, &mut env);
                } else {
                    SHELL_ENV.with(|jobs| {
                        match shell::run::run_one_command(&res, &mut jobs.borrow_mut()) {
                            Ok(status) => {
                                env.set_named_global("*last-status*", Value::Int32(status));
                            }
                            Err(err) => eprintln!("ERROR executing {res}: {err}"),
                        }
                    });
                }
            }
        } else if let Some(script) = config.script {
            let script = env.intern(&script);
            let script = env.get_interned(script);
            match load_internal(&mut env, script) {
                Ok(res) => println!("{}", res.display_value(&env)),
                Err(err) => println!("ERROR: {err}"),
            }
        }
    }
}

fn exec_expression(res: String, env: &mut SloshVm) {
    let reader = Reader::from_string(res, env, "", 1, 0);
    let exps: Result<Vec<Value>, ReadError> = reader.collect();
    match exps {
        Ok(exps) => {
            for exp in exps {
                let line_num = env.line_num();
                let mut state = CompileState::new_state(PROMPT_FN, line_num, None);
                if let Err(e) = pass1(env, &mut state, exp) {
                    println!("Compile error, line {}: {}", env.line_num(), e);
                }
                if let Err(e) = compile(env, &mut state, exp, 0) {
                    println!("Compile error, line {}: {}", env.line_num(), e);
                }
                if let Err(e) = state.chunk.encode0(RET, env.own_line()) {
                    println!("Compile error, line {}: {}", env.line_num(), e);
                }
                let chunk = Arc::new(state.chunk.clone());
                if let Err(err) = env.execute(chunk.clone()) {
                    println!("ERROR: {}", err.display(env));
                    if let Some(err_frame) = env.err_frame() {
                        let line = err_frame.current_line().unwrap_or(0);
                        println!(
                            "{} line: {} ip: {:#010x}",
                            err_frame.chunk.file_name,
                            line,
                            err_frame.current_offset()
                        );
                    }
                    debug(env);
                } else {
                    let reg = env.get_stack(0);
                    println!("{}", display_value(env, reg));
                }
            }
        }
        Err(err) => println!("Reader error: {err}"),
    }
}
