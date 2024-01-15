use compile_state::state::{CompileState, SloshVm, SloshVmTrait};
use shell::builtins::expand_tilde;
use sl_compiler::pass1::pass1;
use sl_compiler::{compile, Reader};
use slvm::{Chunk, VMError, VMResult, Value, RET};
use std::borrow::Cow;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::Arc;

const fn from_utf8(bytes: &[u8]) -> &str {
    if let Ok(s) = std::str::from_utf8(bytes) {
        s
    } else {
        panic!("not valid utf8!")
    }
}

//const CORE_LISP: &[u8] = include_bytes!("../lisp/core.slosh");
const CORE_LISP: &str = from_utf8(include_bytes!("../../lisp/core.slosh"));
const COLORS_LISP: &str = from_utf8(include_bytes!("../../lisp/sh-color.slosh"));
pub const SLSHRC: &str = from_utf8(include_bytes!("../../init.slosh"));

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

pub(crate) fn load_internal(vm: &mut SloshVm, name: &'static str) -> VMResult<Value> {
    let fname = if name.starts_with('/') || name.starts_with('.') {
        Ok(Cow::Borrowed(name))
    } else {
        let i_g = vm.intern("*load-path*");
        if let Some(g) = vm.global_intern_slot(i_g) {
            if let Value::Vector(h) = vm.get_global(g) {
                let paths = vm.get_vector(h);
                let mut found = None;
                for path in paths {
                    match path {
                        Value::StringConst(i) => {
                            let mut p = PathBuf::new();
                            p.push(vm.get_interned(*i));
                            p.push(name);
                            if p.exists() {
                                if let Ok(p) = p.into_os_string().into_string() {
                                    found = Some(p.into());
                                    break;
                                }
                            }
                        }
                        Value::String(h) => {
                            let mut p = PathBuf::new();
                            p.push(vm.get_string(*h));
                            p.push(name);
                            if p.exists() {
                                if let Ok(p) = p.into_os_string().into_string() {
                                    found = Some(p.into());
                                    break;
                                }
                            }
                        }
                        _ => {}
                    }
                }
                if let Some(p) = found {
                    Ok(p)
                } else {
                    Err(VMError::new(
                        "io",
                        format!("{name}: not found on *load-path*!"),
                    ))
                }
            } else {
                Err(VMError::new(
                    "io",
                    format!("{name}: *load-path* not a vector!"),
                ))
            }
        } else {
            Err(VMError::new("io", format!("{name}: *load-path* not set!")))
        }
    };
    let mut reader = match fname {
        Ok(fname) => match std::fs::File::open(&*fname) {
            Ok(file) => Reader::from_file(file, vm, name, 1, 0),
            Err(e) => match name {
                "core.slosh" => Reader::from_static_string(CORE_LISP, vm, name, 1, 0),
                "sh-color.slosh" => Reader::from_static_string(COLORS_LISP, vm, name, 1, 0),
                "init.slosh" => Reader::from_static_string(SLSHRC, vm, name, 1, 0),
                _ => {
                    return Err(VMError::new("io", format!("{name}: {e}")));
                }
            },
        },
        Err(e) => match name {
            "core.slosh" => Reader::from_static_string(CORE_LISP, vm, name, 1, 0),
            "sh-color.slosh" => Reader::from_static_string(COLORS_LISP, vm, name, 1, 0),
            "init.slosh" => Reader::from_static_string(SLSHRC, vm, name, 1, 0),
            _ => {
                return Err(VMError::new("io", format!("{name}: {e}")));
            }
        },
    };

    let mut last = Value::Nil;
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
    let name = if name.contains('~') {
        let name_path = PathBuf::from_str(name).expect("PathBuf from_str failed!");
        let name_exp = expand_tilde(name_path.clone());
        if name_exp == name_path {
            name
        } else {
            let s_i = vm.intern(name_exp.to_string_lossy().as_ref());
            vm.get_interned(s_i)
        }
    } else {
        name
    };
    let olf_line_num = vm.line_num();
    vm.set_line_num(1);
    let r = load_internal(vm, name);
    vm.set_line_num(olf_line_num);
    r
}

fn eval(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if let (Some(exp), None) = (registers.first(), registers.get(1)) {
        let line_num = 1;
        let mut state = CompileState::new_state("none/eval", line_num, None);
        state.chunk.dbg_args = Some(Vec::new());
        pass1(vm, &mut state, *exp)?;
        compile(vm, &mut state, *exp, 0)?;
        state.chunk.encode0(RET, vm.own_line())?;
        let chunk = Arc::new(state.chunk.clone());
        vm.do_call(chunk, &[], None)
    } else {
        Err(VMError::new_compile(
            "eval: wrong number of args, expected one",
        ))
    }
}

pub fn add_load_builtins(env: &mut SloshVm) {
    env.set_global_builtin("load", load);
    env.set_global_builtin("eval", eval);
}
