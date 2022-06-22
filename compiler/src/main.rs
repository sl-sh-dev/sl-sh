use std::sync::Arc;

use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;
use slvm::vm::*;

use sl_compiler::compile::*;
use sl_compiler::config::*;
use sl_compiler::reader::*;
use sl_compiler::state::*;

fn line_num(line: &Option<&mut u32>) -> u32 {
    match line {
        Some(line) => **line,
        None => 0,
    }
}

fn own_line(line: &Option<&mut u32>) -> Option<u32> {
    line.as_ref().map(|l| **l)
}

fn pr(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    for v in registers {
        print!("{}", v.pretty_value(vm));
    }
    Ok(Value::Nil)
}

fn prn(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    for v in registers {
        print!("{}", v.pretty_value(vm));
    }
    println!();
    Ok(Value::Nil)
}

fn eval(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_compile(
            "compile: wrong number of args, expected one",
        ));
    }
    if let Some(exp) = registers.get(0) {
        let mut linenum = 1;
        let mut line = Some(&mut linenum);
        let mut state = CompileState::new_state(vm, "none", line_num(&line), None);
        state.chunk.dbg_args = Some(Vec::new());
        pass1(vm, &mut state, *exp).unwrap();
        compile(vm, &mut state, *exp, 0, &mut line).unwrap();
        state.chunk.encode0(RET, own_line(&line)).unwrap();
        let chunk = Arc::new(state.chunk.clone());
        Ok(vm.do_call(chunk, &[Value::Nil], None)?)
    } else {
        Err(VMError::new_compile("boo"))
    }
}

fn main() {
    let config = if let Some(c) = get_config() {
        c
    } else {
        return;
    };
    let mut vm = Vm::new();
    vm.set_global("pr", Value::Builtin(CallFunc { func: pr }));
    vm.set_global("prn", Value::Builtin(CallFunc { func: prn }));
    vm.set_global("eval", Value::Builtin(CallFunc { func: eval }));
    let mut reader_state = ReaderState::new();
    let file_intern = vm.intern(&config.script);
    reader_state.file_name = vm.get_interned(file_intern);
    //let mut state = CompileState::new();
    let txt = std::fs::read_to_string(&config.script).unwrap();
    let exps = read_all(&mut vm, &mut reader_state, &txt).unwrap();
    let mut linenum = 1;
    let mut line = Some(&mut linenum);
    let file_i = vm.intern(&config.script);
    for exp in exps {
        if let Value::Pair(h) = exp {
            let (_, _) = vm.get_pair(h);
            if let (Some(line), Some(Value::UInt(dline))) =
                (&mut line, vm.get_heap_property(h, "dbg-line"))
            {
                **line = dline as u32;
            }
        }
        let file_name = vm.get_interned(file_i);
        let mut state = CompileState::new_state(&mut vm, file_name, line_num(&line), None);
        state.chunk.dbg_args = Some(Vec::new());
        pass1(&mut vm, &mut state, exp).unwrap();
        compile(&mut vm, &mut state, exp, 0, &mut line).unwrap();
        state.chunk.encode0(RET, own_line(&line)).unwrap();
        if config.dump {
            state.chunk.disassemble_chunk(&vm, 0).unwrap();
        }
        if config.run {
            let chunk = Arc::new(state.chunk.clone());
            if let Err(err) = vm.execute(chunk) {
                println!("ERROR: {}", err);
                vm.dump_globals();
                //state.chunk.disassemble_chunk(&vm).unwrap();
            }
        }
    }
    //state.chunk.encode0(RET, line).unwrap();
    //println!("Compile: {}", txt);
    if config.globals_pre {
        vm.dump_globals();
    }
    //if config.dump {
    //    state.chunk.disassemble_chunk(&vm, 0).unwrap();
    //}
    /*    if config.run {
        let chunk = Rc::new(state.chunk.clone());
        if let Err(err) = vm.execute(chunk) {
            println!("ERROR: {}", err);
            vm.dump_globals();
            //state.chunk.disassemble_chunk(&vm).unwrap();
        }
    }*/
    if config.globals_post {
        vm.dump_globals();
    }
    //println!("\n\nPOST exec:\n");
    //vm.dump_globals();
    //chunk.disassemble_chunk(&vm).unwrap();
}
