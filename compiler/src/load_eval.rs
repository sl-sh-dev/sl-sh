use crate::pass1::pass1;
use crate::{compile, Reader};
use compile_state::state::{CompileEnvironment, CompileState, SloshVm, SloshVmTrait};
use slvm::{CallFuncSig, Chunk, VMError, VMResult, Value, RET};
use std::borrow::Cow;
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;
use std::{env, fs};

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

/// With the given reader, for each sexp compile then load and execute.
pub fn run_reader(reader: &mut Reader) -> VMResult<Value> {
    let mut last = Value::False;
    while let Some(exp) = reader.next() {
        let reader_vm = reader.vm();
        let exp = exp
            .map_err(|e| VMError::new("read", e.to_string()))
            .unwrap();
        reader_vm.heap_sticky(exp);

        let result = load_one_expression(reader_vm, exp, "", None);

        reader_vm.heap_unsticky(exp);
        let (chunk, _new_doc_string) = result.unwrap();
        last = reader_vm.execute(chunk)?;
    }
    Ok(last)
}

pub fn load_one_expression(
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

pub fn load_internal(vm: &mut SloshVm, name: &'static str) -> VMResult<Value> {
    let fname = if fs::metadata::<&Path>(name.as_ref()).is_ok() {
        Ok(Cow::Borrowed(name))
    } else {
        find_first_instance_of_file_in_load_path(vm, name)
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

/// Find file name the first time it appears in the global variable *load-path*.
///
/// *load-path* is a vector of paths and the paths are searched in index order
/// for the file name of the path to be loaded.
fn find_first_instance_of_file_in_load_path<'a>(
    vm: &'a mut SloshVm,
    name: &str,
) -> VMResult<Cow<'a, str>> {
    let i_g = vm.intern("*load-path*");
    if let Some(g) = vm.global_intern_slot(i_g) {
        if let Value::Vector(h) = vm.get_global(g) {
            let paths = vm.get_vector(h);
            let mut found = None;
            for path in paths {
                let path = match path {
                    Value::StringConst(i) => Some(vm.get_interned(*i)),
                    Value::String(h) => Some(vm.get_string(*h)),
                    _ => None,
                };
                if let Some(path) = path {
                    let mut p = PathBuf::new();
                    p.push(path);
                    p.push(name);
                    if p.exists() && !p.is_dir() {
                        if let Ok(p) = p.into_os_string().into_string() {
                            found = Some(p.into());
                            break;
                        }
                    }
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
}

/// Takes a PathBuf, if it contains ~ then replaces it with HOME and returns the new PathBuf, else
/// returns path.  If path is not utf-8 then it will not expand ~.
pub fn expand_tilde(path: PathBuf) -> PathBuf {
    if let Some(path_str) = path.to_str() {
        if path_str.contains('~') {
            let home: PathBuf = match env::var_os("HOME") {
                Some(val) => val.into(),
                None => "/".into(),
            };
            let mut new_path = OsString::new();
            let mut last_ch = ' ';
            let mut buf = [0_u8; 4];
            let mut quoted = false;
            for ch in path_str.chars() {
                if ch == '\'' && last_ch != '\\' {
                    quoted = !quoted;
                }
                if ch == '~' && !quoted && (last_ch == ' ' || last_ch == ':' || last_ch == '=') {
                    // Strip the trailing / if it exists.
                    new_path.push(home.components().as_path().as_os_str());
                } else {
                    new_path.push(ch.encode_utf8(&mut buf));
                }
                last_ch = ch;
            }
            new_path.into()
        } else {
            path
        }
    } else {
        path
    }
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

fn eval_exp(vm: &mut SloshVm, exp: Value) -> VMResult<Value> {
    let line_num = 1;
    let mut state = CompileState::new_state("none/eval", line_num, None);
    state.chunk.dbg_args = Some(Vec::new());
    pass1(vm, &mut state, exp)?;
    compile(vm, &mut state, exp, 0)?;
    state.chunk.encode0(RET, vm.own_line())?;
    let chunk = Arc::new(state.chunk.clone());
    let l = vm.alloc_lambda(chunk.clone());
    vm.heap_sticky(l);
    let ret = vm.do_call(chunk, &[], None);
    vm.heap_unsticky(l);
    ret
}

/// Builtin eval implementation.  Tries to avoid compilation when possible (uses apply machinery).
fn eval(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if let (Some(exp), None) = (registers.first(), registers.get(1)) {
        eval_exp(vm, *exp)
    } else {
        Err(VMError::new_compile(
            "eval: wrong number of args, expected one",
        ))
    }
}

fn quote_list(vm: &mut SloshVm, exp: Value) -> Value {
    if matches!(exp, Value::List(_, _) | Value::Pair(_)) {
        let cdr = vm.alloc_pair_ro(exp, Value::Nil);
        let q_i = vm.specials().quote;
        vm.alloc_pair_ro(Value::Symbol(q_i), cdr)
    } else {
        exp
    }
}

fn contains_list(args: &[Value]) -> bool {
    for exp in args {
        if matches!(exp, Value::List(_, _) | Value::Pair(_)) {
            return true;
        }
    }
    false
}

/// Call lambda with args, this is re-entrant.
fn apply_callable(vm: &mut SloshVm, lambda: Value, args: &[Value]) -> VMResult<Value> {
    match lambda {
        Value::Symbol(i) | Value::Special(i) if i == vm.specials().quote => {
            if let Some(arg) = args.first() {
                Ok(*arg)
            } else {
                Err(VMError::new_vm(
                    "apply: invalid quote, requires one arg.".to_string(),
                ))
            }
        }
        Value::Symbol(i) => {
            // Unknown symbol, check the global namespace.
            let lambda = if let Some(slot) = vm.global_intern_slot(i) {
                vm.get_global(slot)
            } else {
                return Err(VMError::new_vm(format!(
                    "apply: Not a callable, unknown symbol {}.",
                    vm.get_interned(i)
                )));
            };
            apply_callable(vm, lambda, args)
        }
        Value::Special(_i) => {
            let mut args_t;
            let mut args = if contains_list(args) {
                args_t = args.to_vec();
                for i in args_t.iter_mut() {
                    // quote any lists so they do not get compiled...
                    *i = quote_list(vm, *i);
                }
                args_t
            } else {
                args.to_vec()
            };
            args.insert(0, lambda);
            // We have to compile compiled forms...
            let exp = vm.alloc_list_ro(args);
            vm.heap_sticky(exp);
            let res = eval_exp(vm, exp);
            vm.heap_unsticky(exp);
            res
        }
        Value::Builtin(i) => {
            let b = vm.get_builtin(i);
            (b)(vm, args)
        }
        Value::Lambda(h) => {
            let l = vm.get_lambda(h);
            vm.do_call(l, args, None)
        }
        Value::Closure(h) => {
            let (l, caps) = vm.get_closure(h);
            let caps = caps.to_vec();
            vm.do_call(l, args, Some(&caps[..]))
        }
        Value::Continuation(_handle) => {
            // It probably does not make sense to use apply with a continuation, it can only take
            // one argument so just call it with it's arg...  But if someone does it is still
            // a callable so basically eval it.
            if args.len() != 1 {
                return Err(VMError::new_vm("Continuation takes one argument."));
            }
            let mut args = args.to_vec();
            args.insert(0, lambda);
            let exp = vm.alloc_list_ro(args);
            vm.heap_sticky(exp);
            let res = eval_exp(vm, exp);
            vm.heap_unsticky(exp);
            res
        }
        Value::Value(handle) => {
            // Need to deref and try again.
            apply_callable(vm, vm.get_value(handle), args)
        }
        _ => Err(VMError::new_vm(format!(
            "apply: Not a callable {lambda:?}."
        ))),
    }
}

fn apply_inner(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let last_idx = registers.len() - 1;
    // The allocation(s) here are sub-optimal.  Should be able to use the stack to avoid these but
    // it is tricky.
    let v: Vec<Value> = if last_idx > 0 {
        let mut v: Vec<Value> = registers[0..last_idx].to_vec();
        let spread = registers[last_idx].iter_all(vm);
        v.extend(spread);
        v
    } else {
        registers.to_vec()
    };
    apply_callable(vm, v[0], &v[1..])
}

fn apply(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.is_empty() {
        return Err(VMError::new_compile(
            "apply: wrong number of args, expected at least one",
        ));
    }
    apply_inner(vm, registers)
}

fn read_all(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if let (Some(exp), None) = (registers.first(), registers.get(1)) {
        let string_as_code = match exp {
            Value::CharCluster(l, c) => {
                format!("{}", String::from_utf8_lossy(&c[0..*l as usize]))
            }
            Value::CharClusterLong(h) => vm.get_string(*h).to_string(),
            Value::StringConst(i) => vm.get_interned(*i).to_string(),
            Value::String(h) => vm.get_string(*h).to_string(),
            _ => {
                return Err(VMError::new_compile(
                    "read: only accepts strings as arguments.",
                ));
            }
        };
        let reader = Reader::from_string(string_as_code, vm, "", 1, 0);
        let mut vals = vec![];
        for exp in reader {
            let exp = exp.map_err(|e| VMError::new("read", e.to_string()))?;
            vals.push(exp);
        }
        Ok(vm.alloc_vector(vals))
    } else {
        Err(VMError::new_compile(
            "eval: wrong number of args, expected one",
        ))
    }
}

fn add_compiler_builtin(
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

pub fn add_load_builtins(env: &mut SloshVm) {
    env.set_global_builtin("load", load);
    add_compiler_builtin(
        env,
        "eval",
        eval,
        r#"Usage: (eval expression)

Evaluate the provided expression.  If expression is a list it will be compiled and executed and the result returned
other values will just be returned (i.e. (eval 1) = 1, (eval "test") = "test", (eval [1 2 3]) = [1 2 3], etc).

Note eval is a function not a special form, the provided expression will be evaluated as part of a call.

Section: core

Example:
(test::assert-equal "ONE" (eval "ONE"))
(test::assert-equal 10 (eval 10))
(test::assert-equal [1 2 3] (eval [1 2 3]))
(test::assert-equal 10 (eval '(+ 1 2 7)))
(test::assert-equal 10 (eval '(apply + 1 2 7)))
(test::assert-equal 10 (eval '(apply + 1 '(2 7))))
(test::assert-equal 10 (eval '(apply + '(1 2 7))))
(test::assert-equal 10 (eval '(apply + 1 [2 7])))
(test::assert-equal 10 (eval '(apply + [1 2 7])))
"#,
    );
    env.set_global_builtin("read-all", read_all);
    add_compiler_builtin(
        env,
        "apply",
        apply,
        r#"Usage: (apply function arg* list)

Call the provided function with the supplied arguments, if last is a list or vector then it will
be "spread" as arguments.  For instance (apply pr 1 2 3 [4 5 6]) is equivalent to (pr 1 2 3 4 5 6).

Section: core

Example:
(def test-apply-one (apply str "O" "NE"))
(test::assert-equal "ONE" test-apply-one)
(test::assert-equal 10 (apply + 1 2 7))
(test::assert-equal 10 (apply + 1 [2 7]))
(test::assert-equal 10 (apply + 1 '(2 7)))
(test::assert-equal 10 (apply + [1 2 7]))
(test::assert-equal 10 (apply + '(1 2 7)))
(def test-apply-fn1 (fn (& args) (apply + args)))
(test::assert-equal 10 (apply test-apply-fn1 1 2 7))
(test::assert-equal 10 (apply test-apply-fn1 1 [2 7]))
(test::assert-equal 10 (apply test-apply-fn1 1 '(2 7)))
(test::assert-equal 10 (apply test-apply-fn1 [1 2 7]))
(test::assert-equal 10 (apply test-apply-fn1 '(1 2 7)))
(def test-apply-fn2 (fn (x y z) (+ x y z)))
(test::assert-equal 10 (apply test-apply-fn2 1 2 7))
(test::assert-equal 10 (apply test-apply-fn2 1 [2 7]))
(test::assert-equal 10 (apply test-apply-fn2 1 '(2 7)))
(test::assert-equal 10 (apply test-apply-fn2 [1 2 7]))
(test::assert-equal 10 (apply test-apply-fn2 '(1 2 7)))
"#,
    );
}
