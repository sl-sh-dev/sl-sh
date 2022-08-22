extern crate sl_liner;

use std::io::ErrorKind;
use std::sync::Arc;

use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;
use slvm::vm::*;

use sl_compiler::compile::*;
use sl_compiler::reader::*;
use sl_compiler::state::*;

use builtins::collections::{make_hash, setup_vecs};
use builtins::print::{dasm, display_value, pr, prn};
use builtins::{get_prop, set_prop, sizeof_heap_object, sizeof_value};
use sl_liner::{Context, Prompt};
use slvm::Chunk;

pub mod debug;
use debug::*;
use sl_compiler::pass1::pass1;

fn load_one_expression(
    vm: &mut Vm,
    exp: Value,
    name: &'static str,
    doc_string: Option<Value>,
) -> VMResult<(Arc<Chunk>, Option<Value>)> {
    let mut env = CompileEnvironment::new(vm);
    let line_num = env.line_num();
    let mut state = CompileState::new_state(name, line_num, None);
    state.chunk.dbg_args = Some(Vec::new());
    state.doc_string = doc_string;
    if let Err(e) = pass1(&mut env, &mut state, exp) {
        println!(
            "Compile error (pass one), {}, line {}: {}",
            name,
            env.line_num(),
            e
        );
        return Err(e);
    }
    if let Err(e) = compile(&mut env, &mut state, exp, 0) {
        println!(
            "Compile error, {} line {}: {} exp: {}",
            name,
            env.line_num(),
            e,
            exp.display_value(env.vm())
        );
        return Err(e);
    }
    if let Err(e) = state.chunk.encode0(RET, env.own_line()) {
        println!("Compile error, {} line {}: {}", name, env.line_num(), e);
        return Err(e);
    }
    state.chunk.extra_regs = state.max_regs;
    Ok((Arc::new(state.chunk), state.doc_string))
}

fn load(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
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
    let file = std::fs::File::open(name)?;

    let mut last = Value::Nil;
    let mut reader = Reader::from_file(file, vm, name, 1, 0);
    let mut doc_string = None;
    while let Some(exp) = reader.next() {
        let reader_vm = reader.vm();
        let exp = exp.map_err(|e| VMError::new("read", e.to_string()))?;
        if let Some(handle) = exp.get_handle() {
            reader_vm.heap_sticky(handle);
        }

        let result = load_one_expression(reader_vm, exp, name, doc_string);

        if let Some(handle) = exp.get_handle() {
            reader_vm.heap_unsticky(handle);
        }
        let (chunk, new_doc_string) = result?;
        doc_string = new_doc_string;
        reader_vm.execute(chunk)?;
        last = reader_vm.get_stack(0);
    }
    Ok(last)
}

fn eval(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if let (Some(exp), None) = (registers.get(0), registers.get(1)) {
        let mut env = CompileEnvironment::new(vm);
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
            "compile: wrong number of args, expected one",
        ))
    }
}

const PROMPT_FN: &str = "prompt";

fn main() {
    let mut con = Context::new();

    if let Err(e) = con.history.set_file_name_and_load_history("history") {
        println!("Error loading history: {}", e);
    }
    let mut vm = Vm::new();
    vm.set_global("pr", Value::Builtin(CallFunc { func: pr }));
    vm.set_global("prn", Value::Builtin(CallFunc { func: prn }));
    vm.set_global("dasm", Value::Builtin(CallFunc { func: dasm }));
    vm.set_global("load", Value::Builtin(CallFunc { func: load }));
    vm.set_global("make-hash", Value::Builtin(CallFunc { func: make_hash }));
    vm.set_global("get-prop", Value::Builtin(CallFunc { func: get_prop }));
    vm.set_global("set-prop", Value::Builtin(CallFunc { func: set_prop }));
    vm.set_global("eval", Value::Builtin(CallFunc { func: eval }));
    vm.set_global(
        "sizeof-heap-object",
        Value::Builtin(CallFunc {
            func: sizeof_heap_object,
        }),
    );
    vm.set_global(
        "sizeof-value",
        Value::Builtin(CallFunc { func: sizeof_value }),
    );
    setup_vecs(&mut vm);
    let mut env = CompileEnvironment::new(&mut vm);
    loop {
        let res = match con.read_line(Prompt::from("slosh> "), None) {
            Ok(input) => input,
            Err(err) => match err.kind() {
                ErrorKind::UnexpectedEof => {
                    break;
                }
                ErrorKind::Interrupted => {
                    continue;
                }
                _ => {
                    eprintln!("Error on input: {}", err);
                    continue;
                }
            },
        };

        if res.is_empty() {
            continue;
        }

        con.history.push(&res).expect("Failed to push history.");
        let reader = Reader::from_string(res, env.vm_mut(), "", 1, 0);
        let exps: Result<Vec<Value>, ReadError> = reader.collect();
        match exps {
            Ok(exps) => {
                for exp in exps {
                    let line_num = env.line_num();
                    let mut state = CompileState::new_state(PROMPT_FN, line_num, None);
                    if let Err(e) = pass1(&mut env, &mut state, exp) {
                        println!("Compile error, line {}: {}", env.line_num(), e);
                    }
                    if let Err(e) = compile(&mut env, &mut state, exp, 0) {
                        println!("Compile error, line {}: {}", env.line_num(), e);
                    }
                    if let Err(e) = state.chunk.encode0(RET, env.own_line()) {
                        println!("Compile error, line {}: {}", env.line_num(), e);
                    }
                    let chunk = Arc::new(state.chunk.clone());
                    if let Err(err) = env.vm_mut().execute(chunk) {
                        println!("ERROR: {}", err.display(env.vm()));
                        if let Some(err_frame) = env.vm().err_frame() {
                            let ip = err_frame.current_ip;
                            let line = err_frame.chunk.offset_to_line(ip).unwrap_or(0);
                            println!(
                                "{} line: {} ip: {:#010x}",
                                err_frame.chunk.file_name, line, ip
                            );
                        }
                        debug(env.vm_mut());
                    } else {
                        let reg = env.vm().get_stack(0);
                        println!("{}", display_value(env.vm_mut(), reg));
                    }
                }
            }
            Err(err) => println!("Reader error: {}", err),
        }
    }
}
