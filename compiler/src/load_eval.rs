use crate::pass1::pass1;
use crate::{ReadError, Reader, compile};
use compile_state::state::{CompileEnvironment, CompileState, Namespace, SloshVm, SloshVmTrait};
use slvm::{CallFuncSig, Chunk, RET, VMError, VMResult, Value};
use std::borrow::Cow;
use std::ffi::OsString;
use std::io::{Seek, SeekFrom};
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

const CORE_LISP: &str = from_utf8(include_bytes!("../../lisp/core.slosh"));
const ITER_LISP: &str = from_utf8(include_bytes!("../../lisp/iterator.slosh"));
const TEST_LISP: &str = from_utf8(include_bytes!("../../lisp/test.slosh"));
const COLORS_LISP: &str = from_utf8(include_bytes!("../../lisp/sh-color.slosh"));
pub const SLSHRC: &str = from_utf8(include_bytes!("../../init.slosh"));

/// Execute a chunk that may not be rooted, will make sure any allocated consts don't get GCed
/// out from under it...
pub fn exec_unrooted_chunk(vm: &mut SloshVm, chunk: Arc<Chunk>) -> VMResult<Value> {
    for constant in &chunk.constants {
        vm.heap_sticky(*constant);
    }
    let res = vm.execute(chunk.clone());
    for constant in &chunk.constants {
        vm.heap_unsticky(*constant);
    }
    res
}

/// With the given reader, for each sexp compile then load and execute.
pub fn run_reader(reader: &mut Reader) -> VMResult<Value> {
    let mut last = Value::False;
    while let Some(exp) = reader.next() {
        let reader_vm = reader.vm();
        let exp = exp
            .map_err(|e| VMError::new("read", e.to_string()))
            .unwrap();
        reader_vm.heap_sticky(exp);
        let line_num = reader_vm.line_num();
        let mut state = CompileState::new_state("", line_num, None);
        state.chunk.dbg_args = Some(Vec::new());
        let result = load_one_expression(reader_vm, &mut state, 0, exp, "", None, true);
        reader_vm.heap_unsticky(exp);

        result?;
        last = exec_unrooted_chunk(reader_vm, Arc::new(state.chunk))?;
    }
    reader.vm().env_mut().set_namespace(Namespace::default());
    Ok(last)
}

pub fn load_one_expression(
    vm: &mut SloshVm,
    state: &mut CompileState,
    result: usize,
    exp: Value,
    name: &'static str,
    doc_string: Option<Value>,
    terminal: bool,
) -> VMResult<Option<Value>> {
    state.doc_string = doc_string;
    if let Err(e) = pass1(vm, state, exp) {
        println!(
            "Compile error (pass one), {}, line {}: {}",
            name,
            vm.line_num(),
            e
        );
        return Err(e);
    }
    if let Err(e) = compile(vm, state, exp, result) {
        println!(
            "Compile error, {} line {}: {} exp: {}",
            name,
            vm.line_num(),
            e,
            exp.display_value(vm)
        );
        return Err(e);
    }
    if terminal {
        if let Err(e) = state.chunk.encode0(RET, vm.own_line()) {
            println!("Compile error, {} line {}: {}", name, vm.line_num(), e);
            return Err(e);
        }
    }
    Ok(state.doc_string)
}

pub fn load_internal(vm: &mut SloshVm, name: &'static str) -> VMResult<Value> {
    let mut reader = reader_for_file(vm, name)?;
    let mut doc_string = None;
    let mut last = Value::Nil;
    while let Some(exp) = reader.next() {
        let reader_vm = reader.vm();
        let exp = exp.map_err(|e| VMError::new("read", e.to_string()))?;
        reader_vm.heap_sticky(exp);

        let line_num = reader_vm.line_num();
        let mut state = CompileState::new_state(name, line_num, None);
        state.chunk.dbg_args = Some(Vec::new());
        let result = load_one_expression(reader_vm, &mut state, 0, exp, name, doc_string, true);

        reader_vm.heap_unsticky(exp);
        let new_doc_string = result?;
        doc_string = new_doc_string;
        last = exec_unrooted_chunk(reader_vm, Arc::new(state.chunk))?;
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

pub fn get_load_name(vm: &mut SloshVm, value: Value) -> VMResult<&'static str> {
    let name = match value {
        Value::StringConst(i) => vm.get_interned(i),
        Value::String(h) => {
            let s = vm.get_string(h);
            let s = s.to_string();
            let s_i = vm.intern(&s);
            vm.get_interned(s_i)
        }
        v => {
            let v = v.display_type(vm);
            let v = "Not a string: ".to_string() + v;
            return Err(VMError::new_vm(v));
        }
    };
    if name.contains('~') {
        let name_path = PathBuf::from_str(name).expect("PathBuf from_str failed!");
        let name_exp = expand_tilde(name_path.clone());
        if name_exp == name_path {
            Ok(name)
        } else {
            let s_i = vm.intern(name_exp.to_string_lossy().as_ref());
            Ok(vm.get_interned(s_i))
        }
    } else {
        Ok(name)
    }
}

fn reader_for_file<'vm>(vm: &'vm mut SloshVm, name: &'static str) -> VMResult<Reader<'vm>> {
    let fname = if fs::metadata::<&Path>(name.as_ref()).is_ok() {
        Ok(Cow::Borrowed(name))
    } else {
        find_first_instance_of_file_in_load_path(vm, name)
    };
    let reader = match fname {
        Ok(fname) => match std::fs::File::open(&*fname) {
            Ok(file) => Reader::from_file(file, vm, name, 1, 0),
            Err(e) => match name {
                "core.slosh" => Reader::from_static_string(CORE_LISP, vm, name, 1, 0),
                "iterator.slosh" => Reader::from_static_string(ITER_LISP, vm, name, 1, 0),
                "test.slosh" => Reader::from_static_string(TEST_LISP, vm, name, 1, 0),
                "sh-color.slosh" => Reader::from_static_string(COLORS_LISP, vm, name, 1, 0),
                "init.slosh" => Reader::from_static_string(SLSHRC, vm, name, 1, 0),
                _ => {
                    return Err(VMError::new("io", format!("{name}: {e}")));
                }
            },
        },
        Err(e) => match name {
            "core.slosh" => Reader::from_static_string(CORE_LISP, vm, name, 1, 0),
            "iterator.slosh" => Reader::from_static_string(ITER_LISP, vm, name, 1, 0),
            "test.slosh" => Reader::from_static_string(TEST_LISP, vm, name, 1, 0),
            "sh-color.slosh" => Reader::from_static_string(COLORS_LISP, vm, name, 1, 0),
            "init.slosh" => Reader::from_static_string(SLSHRC, vm, name, 1, 0),
            _ => {
                return Err(VMError::new("io", format!("{name}: {e}")));
            }
        },
    };
    Ok(reader)
}

pub fn load(
    vm: &mut SloshVm,
    state: &mut CompileState,
    name: &'static str,
    result: usize,
) -> VMResult<()> {
    let mut doc_string = None;
    let mut reader = reader_for_file(vm, name)?;
    while let Some(exp) = reader.next() {
        let reader_vm = reader.vm();
        let exp = exp.map_err(|e| VMError::new("read", e.to_string()))?;
        reader_vm.heap_sticky(exp);

        let result = load_one_expression(reader_vm, state, result, exp, name, doc_string, false);

        reader_vm.heap_unsticky(exp);
        let new_doc_string = result?;
        doc_string = new_doc_string;
    }
    Ok(())
}

fn run_script(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_compile(
            "run-script: wrong number of args, expected one",
        ));
    }
    let name = get_load_name(vm, registers[0].unref(vm))?;
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
pub fn apply_callable(vm: &mut SloshVm, lambda: Value, args: &[Value]) -> VMResult<Value> {
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

fn read(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    fn read_inner(
        next: Option<Result<Value, ReadError>>,
        err_val: Option<Value>,
    ) -> VMResult<Value> {
        match next {
            Some(r) => r.map_or_else(
                |e| err_val.map_or_else(|| Err(VMError::new("read", e.to_string())), Ok),
                Ok,
            ),
            None => err_val.map_or_else(|| Err(VMError::new_compile("read: nothing to read.")), Ok),
        }
    }
    if let (Some(exp), err_val) = (registers.first(), registers.get(1)) {
        match exp {
            Value::StringConst(i) => {
                let string_as_code = vm.get_interned(*i);
                let mut reader = Reader::from_static_string(string_as_code, vm, "", 1, 0);
                read_inner(reader.next(), err_val.copied())
            }
            Value::String(h) => {
                let string_as_code = vm.get_string(*h).to_string();
                let mut reader = Reader::from_string(string_as_code, vm, "", 1, 0);
                read_inner(reader.next(), err_val.copied())
            }
            Value::Io(h) => {
                let io = vm.get_io(*h).clone();
                let mut reader = Reader::from_reader(io, vm, "", 1, 0);
                let r = read_inner(reader.next(), err_val.copied());
                // The grapheme reader will have consumed one extra char so we need to seek back over
                // it so the file is left in the "correct" position to read the next form.
                let mut chars = reader.into_char_iter();
                let io = vm.get_io(*h);
                let pos = io.get_io().stream_position(); // Don't combine this with the if below...
                if let Ok(pos) = pos {
                    // If our input object does not support seek (like stdin) then don't fail.
                    if let Some(Ok(ch)) = chars.next() {
                        io.get_io().seek(SeekFrom::Start(pos - ch.len() as u64))?;
                    }
                }
                r
            }
            _ => Err(VMError::new_compile(
                "read: only accepts strings or IO objects as arguments.",
            )),
        }
    } else {
        Err(VMError::new_compile(
            "read: wrong number of args, expected one or two",
        ))
    }
}

fn read_all(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    fn read_all_inner(mut reader: Reader) -> VMResult<Value> {
        let mut vals = vec![];
        for exp in &mut reader {
            let exp = exp.map_err(|e| VMError::new("read", e.to_string()))?;
            vals.push(exp);
        }
        Ok(reader.vm().alloc_vector(vals))
    }
    if let (Some(exp), None) = (registers.first(), registers.get(1)) {
        vm.pause_gc();
        let r = match exp {
            Value::StringConst(i) => {
                let string_as_code = vm.get_interned(*i);
                let reader = Reader::from_static_string(string_as_code, vm, "", 1, 0);
                read_all_inner(reader)
            }
            Value::String(h) => {
                let string_as_code = vm.get_string(*h).to_string();
                let reader = Reader::from_string(string_as_code, vm, "", 1, 0);
                read_all_inner(reader)
            }
            Value::Io(h) => {
                let io = vm.get_io(*h).clone();
                let reader = Reader::from_reader(io, vm, "", 1, 0);
                read_all_inner(reader)
            }
            _ => Err(VMError::new(
                "read",
                "read-all: only accepts strings or IO objects as arguments.",
            )),
        };
        vm.unpause_gc();
        r
    } else {
        Err(VMError::new_compile(
            "read-all: wrong number of args, expected one",
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
    add_compiler_builtin(
        env,
        "run-script",
        run_script,
        r#"Usage: (run-script path) -> [last form value]

Read and eval a file (from path- a string).

Section: scripting

Example:
(def test-load::test-fn)
(with-temp-file (fn (tmp)
    (let (tst-file (fopen tmp :create))
        (defer (fclose tst-file))
        (fprn tst-file "(with-ns test-load")
        (fprn tst-file "    (defn test-fn () '(1 2 3)))"))
    (run-script tmp)
    (test::assert-equal '(1 2 3) (test-load::test-fn))))
"#,
    );
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
    add_compiler_builtin(
        env,
        "read",
        read,
        r#"Usage: (read file|string end-exp?) -> expression

Read a file or string and return the next object (symbol, string, list, etc).
Raises an error if the file or string has been read unless end-exp is provided
then returns that on the end condition.
Note: When reading a string read always starts at the beginning of the string.

Section: file

Example:
(with-temp-file (fn (tmp)
    (let (tst-file (fopen tmp :create :truncate)
          test-str nil)
        (fprn tst-file "(1 2 3)(x y z)")
        (fclose tst-file)
        (set! tst-file (fopen tmp :read))
        (test::assert-equal '(1 2 3) (read tst-file))
        (test::assert-equal '(x y z) (read tst-file))
        (test::assert-error (read tst-file))
        (fclose tst-file)
        (set! tst-file (fopen tmp :read))
        (test::assert-equal '(1 2 3) (read tst-file :done))
        (test::assert-equal '(x y z) (read tst-file :done))
        (test::assert-equal :done (read tst-file :done))
        (fclose tst-file)
        (test::assert-equal '(4 5 6) (read "(4 5 6)"))
        (set! test-str "7 8 9")
        (test::assert-equal 7 (read test-str))
        (test::assert-equal 7 (read test-str))
        (test::assert-equal '(x y z) (read "(x y z)")))))"#,
    );
    add_compiler_builtin(
        env,
        "read-all",
        read_all,
        r#"Usage: (read-all file|string) -> vec

Read a file or string and return the vector of contained expressions.  This reads the entire
file or string and will wrap it in an outer vector (always returns a vector).

Unlike most lisp readers this one will put loose symbols in a vector (i.e. you
enter things at the repl without the enclosing parens).

If the read item is empty (including a comment) then will return an empty vector.

Section: file

Example:
(with-temp-file (fn (tmp)
    (let (tst-file (fopen tmp :create :truncate)
          test-str nil)
        (fprn tst-file "(1 2 3)(x y z)")
        (fclose tst-file)
        (set! tst-file (fopen tmp :read))
        (test::assert-equal ['(1 2 3)'(x y z)] (read-all tst-file))
        (fclose tst-file)
        (test::assert-equal ['(4 5 6)] (read-all "(4 5 6)"))
        (test::assert-equal [7 8 9] (read-all "7 8 9"))
        (test::assert-equal ['(x y z)] (read-all "(x y z)"))
        (test::assert-equal [] (read-all ";(x y z)")))))"#,
    );
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
