use std::sync::Arc;

use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;
use slvm::vm::*;

use sl_compiler::compile::*;
use sl_compiler::config::*;
use sl_compiler::reader::*;
use sl_compiler::state::*;

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
        let mut state = CompileState::new_state(vm, "none", 1, None);
        state.chunk.dbg_args = Some(Vec::new());
        let mut env = CompileEnvironment::new(vm);
        pass1(&mut env, &mut state, *exp).unwrap();
        compile(&mut env, &mut state, *exp, 0).unwrap();
        state.chunk.encode0(RET, env.own_line()).unwrap();
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
    let mut env = CompileEnvironment::new(&mut vm);
    env.vm_mut()
        .set_global("pr", Value::Builtin(CallFunc { func: pr }));
    env.vm_mut()
        .set_global("prn", Value::Builtin(CallFunc { func: prn }));
    env.vm_mut()
        .set_global("eval", Value::Builtin(CallFunc { func: eval }));
    let mut reader_state = ReaderState::new();
    let file_intern = env.vm_mut().intern(&config.script);
    reader_state.file_name = env.vm().get_interned(file_intern);
    //let mut state = CompileState::new();
    let txt = std::fs::read_to_string(&config.script).unwrap();
    let exps = read_all(env.vm_mut(), &mut reader_state, &txt).unwrap();
    let file_i = env.vm_mut().intern(&config.script);
    for exp in exps {
        let file_name = env.vm().get_interned(file_i);
        let line_num = env.line_num();
        let mut state = CompileState::new_state(env.vm_mut(), file_name, line_num, None);
        env.set_line_val(&mut state, exp);
        state.chunk.dbg_args = Some(Vec::new());
        pass1(&mut env, &mut state, exp).unwrap();
        compile(&mut env, &mut state, exp, 0).unwrap();
        state.chunk.encode0(RET, env.own_line()).unwrap();
        if config.dump {
            state.chunk.disassemble_chunk(env.vm(), 0).unwrap();
        }
        if config.run {
            let chunk = Arc::new(state.chunk.clone());
            if let Err(err) = env.vm_mut().execute(chunk) {
                println!("ERROR: {}", err);
                env.vm().dump_globals();
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
