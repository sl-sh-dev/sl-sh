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

use sl_liner::{Context, Prompt};
use slvm::Chunk;

pub mod debug;
use debug::*;

pub mod print;
use print::*;

fn value_str(vm: &mut Vm, val: Value) -> String {
    pretty_value(vm, val)
}

fn value_dsp_str(vm: &mut Vm, val: Value) -> String {
    display_value(vm, val)
}

fn pr(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    for v in registers {
        print!("{}", value_str(vm, *v));
    }
    Ok(Value::Nil)
}

fn prn(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    for v in registers {
        print!("{}", value_str(vm, *v));
    }
    println!();
    Ok(Value::Nil)
}

fn dasm(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_compile(
            "dasm: wrong number of args, expected one",
        ));
    }
    match registers[0].unref(vm) {
        Value::Lambda(handle) => {
            let l = vm.get_lambda(handle);
            l.disassemble_chunk(vm, 0)?;
            Ok(Value::Nil)
        }
        Value::Closure(handle) => {
            let (l, _) = vm.get_closure(handle);
            l.disassemble_chunk(vm, 0)?;
            Ok(Value::Nil)
        }
        _ => Err(VMError::new_vm("DASM: Not a callable.")),
    }
}

fn load_one_expression(vm: &mut Vm, exp: Value, name: &'static str) -> VMResult<Arc<Chunk>> {
    let mut env = CompileEnvironment::new(vm);
    let line_num = env.line_num();
    let mut state = CompileState::new_state(env.vm_mut(), name, line_num, None);
    state.chunk.dbg_args = Some(Vec::new());
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
    Ok(Arc::new(state.chunk))
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

    let mut reader_state = ReaderState::new();
    reader_state.file_name = name;
    let mut last = Value::Nil;
    let mut reader = ReadIter::from_file(file, vm, reader_state);
    while let Some(exp) = reader.next() {
        let reader_vm = reader.vm();
        let exp = exp.map_err(|e| VMError::new("read", e.to_string()))?;
        if let Some(handle) = exp.get_handle() {
            reader_vm.heap_sticky(handle);
        }

        let chunk = load_one_expression(reader_vm, exp, name);

        if let Some(handle) = exp.get_handle() {
            reader_vm.heap_unsticky(handle);
        }
        reader_vm.execute(chunk?)?;
        last = reader_vm.get_stack(0);
    }
    Ok(last)
}

fn vec_slice(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    let (vector, start, end) = match registers.len() {
        2 => {
            if let (Value::Vector(vector), Ok(start)) = (registers[0], registers[1].get_int()) {
                let v = vm.get_vector(vector);
                (v, start as usize, v.len())
            } else {
                return Err(VMError::new_vm("vec-slice: Invalid arguments".to_string()));
            }
        }
        3 => {
            if let (Value::Vector(vector), Ok(start), Ok(end)) =
                (registers[0], registers[1].get_int(), registers[2].get_int())
            {
                let v = vm.get_vector(vector);
                (v, start as usize, end as usize)
            } else {
                return Err(VMError::new_vm("vec-slice: Invalid arguments".to_string()));
            }
        }
        _ => {
            return Err(VMError::new_vm(
                "vec-slice: Invalid arguments (requires two or three)".to_string(),
            ))
        }
    };
    let len = vector.len();
    if start == len && end <= len {
        Ok(vm.alloc_vector(Vec::new()))
    } else if start >= len || end > len {
        Err(VMError::new_vm(
            "vec-slice: Invalid arguments- out of bounds".to_string(),
        ))
    } else {
        let new_vec = vector[start..end].to_vec();
        Ok(vm.alloc_vector(new_vec))
    }
}

fn vec_to_list(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_vm(
            "vec->list: Invalid arguments (requires one vector)".to_string(),
        ));
    }
    if let Value::Vector(vhandle) = registers[0] {
        let vector = vm.get_vector(vhandle).to_vec();

        let mut last = Value::Nil;
        for item in vector.iter().rev() {
            let old_last = last;
            last = vm.alloc_pair(*item, old_last);
        }
        Ok(last)
    } else {
        Err(VMError::new_vm(
            "vec->list: Invalid arguments (requires one vector)".to_string(),
        ))
    }
}

fn get_prop(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 2 {
        return Err(VMError::new_vm(
            "get-prop: Invalid arguments (object symbol)".to_string(),
        ));
    }
    let key = match registers[1] {
        Value::Keyword(key) => key,
        Value::Symbol(key) => key,
        _ => return Err(VMError::new_vm("get-prop: key must be a symbol")),
    };
    if let Value::Global(idx) = registers[0] {
        Ok(vm.get_global_property(idx, key).unwrap_or(Value::Nil))
    } else {
        let handle = registers[0].get_handle().ok_or_else(|| {
            VMError::new_vm("get-prop: Not a heap object or global symbol".to_string())
        })?;
        Ok(vm
            .get_heap_property_interned(handle, key)
            .unwrap_or(Value::Nil))
    }
}

fn set_prop(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 3 {
        return Err(VMError::new_vm(
            "set-prop: Invalid arguments (object symbol value)".to_string(),
        ));
    }
    let key = match registers[1] {
        Value::Keyword(key) => key,
        Value::Symbol(key) => key,
        _ => return Err(VMError::new_vm("set-prop: key must be a symbol")),
    };
    if let Value::Global(idx) = registers[0] {
        vm.set_global_property(idx, key, registers[2]);
        Ok(registers[2])
    } else {
        let handle = registers[0].get_handle().ok_or_else(|| {
            VMError::new_vm("set-prop: Not a heap object or global symbol".to_string())
        })?;
        vm.set_heap_property_interned(handle, key, registers[2]);
        Ok(registers[2])
    }
}

fn sizeof_heap_object(_vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm(
            "sizeof-heap-object: takes no arguments".to_string(),
        ));
    }
    Ok(Value::UInt(Vm::sizeof_heap_object() as u64))
}

fn sizeof_value(_vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm(
            "sizeof-value: takes no arguments".to_string(),
        ));
    }
    Ok(Value::UInt(std::mem::size_of::<Value>() as u64))
}

fn eval(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if let (Some(exp), None) = (registers.get(0), registers.get(1)) {
        let mut env = CompileEnvironment::new(vm);
        let line_num = env.line_num();
        let mut state = CompileState::new_state(env.vm_mut(), "none/eval", line_num, None);
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
    vm.set_global("vec-slice", Value::Builtin(CallFunc { func: vec_slice }));
    vm.set_global("vec->list", Value::Builtin(CallFunc { func: vec_to_list }));
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
        let reader_state = ReaderState::new();
        let reader = ReadIter::from_string(res, env.vm_mut(), reader_state);
        let exps: Result<Vec<Value>, ReadError> = reader.collect();
        match exps {
            Ok(exps) => {
                for exp in exps {
                    let line_num = env.line_num();
                    let mut state =
                        CompileState::new_state(env.vm_mut(), PROMPT_FN, line_num, None);
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
                        println!("{}", value_dsp_str(env.vm_mut(), reg));
                    }
                }
            }
            Err(err) => println!("Reader error: {}", err),
        }
    }
}
