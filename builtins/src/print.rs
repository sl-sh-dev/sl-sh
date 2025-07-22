use crate::SloshVm;
use bridge_adapters::add_builtin;
use bridge_macros::sl_sh_fn;
use compile_state::state::{CompileState, SloshVmTrait};
use sl_compiler::compile;
use sl_compiler::pass1::pass1;
use slvm::{Interned, VMError, VMResult, Value};
use std::io::{Write, stderr, stdout};

fn is_sym(vm: &SloshVm, name: &str, intern: Interned) -> bool {
    if let Some(i) = vm.get_if_interned(name) {
        if intern == i {
            return true;
        }
    }
    false
}

fn quotey(vm: &SloshVm, car: Value, buf: &mut String) -> bool {
    if let Value::Symbol(i) = car {
        if is_sym(vm, "quote", i) {
            buf.push('\'');
            true
        } else if is_sym(vm, "back-quote", i) {
            buf.push('`');
            true
        } else if is_sym(vm, "unquote", i) {
            buf.push(',');
            true
        } else if is_sym(vm, "unquote-splice", i) {
            buf.push_str(",@");
            true
        } else if is_sym(vm, "unquote-splice!", i) {
            buf.push_str(",.");
            true
        } else {
            false
        }
    } else {
        false
    }
}

fn list_out(vm: &SloshVm, res: &mut String, lst: Value) {
    let mut first = true;
    let mut cdr = lst;
    loop {
        if let Value::Nil = cdr {
            break;
        }
        if !first {
            res.push(' ');
        } else {
            first = false;
        }
        match cdr {
            Value::Pair(_) | Value::List(_, _) => {
                let (car, ncdr) = cdr.get_pair(vm).expect("pair/list not a pair/list");
                res.push_str(&display_value(vm, car));
                cdr = ncdr;
            }
            _ => {
                res.push_str(". ");
                res.push_str(&display_value(vm, cdr));
                break;
            }
        }
    }
}

pub fn display_value(vm: &SloshVm, val: Value) -> String {
    match &val {
        Value::Pair(_) | Value::List(_, _) => {
            let (car, cdr) = val.get_pair(vm).expect("pair/list not a pair/list");
            let mut res = String::new();
            if quotey(vm, car, &mut res) {
                if let Some((cadr, Value::Nil)) = cdr.get_pair(vm) {
                    res.push_str(&display_value(vm, cadr));
                } else {
                    res.push_str(&display_value(vm, cdr));
                }
            } else {
                res.push('(');
                list_out(vm, &mut res, val);
                res.push(')');
            }
            res
        }
        _ => val.display_value(vm),
    }
}

pub fn pretty_value(vm: &SloshVm, val: Value) -> String {
    match &val {
        Value::StringConst(i) => vm.get_interned(*i).to_string(),
        Value::CodePoint(ch) => format!("{ch}"),
        Value::CharCluster(l, c) => {
            format!("{}", String::from_utf8_lossy(&c[0..*l as usize]))
        }
        Value::CharClusterLong(h) => vm.get_string(*h).to_string(),
        Value::String(h) => vm.get_string(*h).to_string(),
        _ => display_value(vm, val),
    }
}

pub fn pr(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    for v in registers {
        print!("{}", pretty_value(vm, *v));
    }
    stdout().flush()?;
    Ok(Value::Nil)
}

pub fn epr(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    for v in registers {
        eprint!("{}", pretty_value(vm, *v));
    }
    stderr().flush()?;
    Ok(Value::Nil)
}

pub fn prn(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    for v in registers {
        print!("{}", pretty_value(vm, *v));
    }
    println!();
    Ok(Value::Nil)
}

pub fn fpr(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut args = registers.iter();
    if let Some(Value::Io(h)) = args.next() {
        let mut file = vm.get_io(*h).get_io();
        for v in args {
            write!(file, "{}", pretty_value(vm, *v))?;
        }
        Ok(Value::Nil)
    } else {
        Err(VMError::new(
            "io",
            "fpr: require a writable IO object as first parameter",
        ))
    }
}

pub fn fprn(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut args = registers.iter();
    if let Some(Value::Io(h)) = args.next() {
        let mut file = vm.get_io(*h).get_io();
        for v in args {
            write!(file, "{}", pretty_value(vm, *v))?;
        }
        writeln!(file)?;
        Ok(Value::Nil)
    } else {
        Err(VMError::new(
            "io",
            "fprn: require a writable IO object as first parameter",
        ))
    }
}

pub fn eprn(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    for v in registers {
        eprint!("{}", pretty_value(vm, *v));
    }
    eprintln!();
    Ok(Value::Nil)
}

/// Usage: (dump-globals)
///
/// Prints the global variables to stdout.
///
/// Section: core
///
/// Example:
/// #t
#[sl_sh_fn(fn_name = "dump-globals", takes_env = true)]
pub fn dump_globals(environment: &mut SloshVm) -> VMResult<Value> {
    environment.dump_globals();
    Ok(Value::Nil)
}

/// TODO PC make the other builtins.
pub fn dasm(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_compile(
            "dasm: wrong number of args, expected one",
        ));
    }
    let exp = registers[0].unref(vm);
    match exp {
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
        Value::List(_handle, _start_idx) => {
            let mut state = CompileState::new_state("", 1, None);
            pass1(vm, &mut state, exp)?;
            compile(vm, &mut state, exp, 0)?;
            state.chunk.disassemble_chunk(vm, 0)?;
            Ok(Value::Nil)
        }
        Value::Pair(_handle) => {
            let mut state = CompileState::new_state("", 1, None);
            pass1(vm, &mut state, exp)?;
            compile(vm, &mut state, exp, 0)?;
            state.chunk.disassemble_chunk(vm, 0)?;
            Ok(Value::Nil)
        }
        _ => Err(VMError::new_vm("DASM: Not a callable.")),
    }
}

pub fn add_print_builtins(env: &mut SloshVm) {
    add_builtin(
        env,
        "pr",
        pr,
        r#"Usage: (pr & values) => nil

Print values to standard output without a newline.

Arguments:
- values: Any type. Zero or more values to print.
- nil: Always returns nil.

Prints the string representation of each value in sequence.
Strings are printed without quotes.

Section: io

Example:
(pr "Hello" " " "World") ; prints: Hello World
(pr 42 " is the answer") ; prints: 42 is the answer
"#,
    );
    add_builtin(
        env,
        "epr",
        epr,
        r#"Usage: (epr & values) => nil

Print values to standard error without a newline.

Arguments:
- values: Any type. Zero or more values to print.
- nil: Always returns nil.

Prints the string representation of each value to stderr.
Strings are printed without quotes.

Section: io

Example:
(epr "Error: " "File not found") ; prints to stderr: Error: File not found
"#,
    );
    add_builtin(
        env,
        "prn",
        prn,
        r#"Usage: (prn & values) => nil

Print values to standard output followed by a newline.

Arguments:
- values: Any type. Zero or more values to print.
- nil: Always returns nil.

Prints the string representation of each value in sequence,
then prints a newline character. Strings are printed without quotes.

Section: io

Example:
(prn "Hello World") ; prints: Hello World\n
(prn "The answer is" 42) ; prints: The answer is42\n
"#,
    );
    add_builtin(
        env,
        "eprn",
        eprn,
        r#"Usage: (eprn & values) => nil

Print values to standard error followed by a newline.

Arguments:
- values: Any type. Zero or more values to print.
- nil: Always returns nil.

Prints the string representation of each value to stderr,
then prints a newline character. Strings are printed without quotes.

Section: io

Example:
(eprn "Error:" "File not found") ; prints to stderr: Error:File not found\n
"#,
    );
    add_builtin(
        env,
        "fpr",
        fpr,
        r#"Usage: (fpr file-handle & values) => nil

Print values to a file without a newline.

Arguments:
- file-handle: A file handle. The file to write to (must be writable).
- values: Any type. Zero or more values to print.
- nil: Always returns nil.

Writes the string representation of each value to the file.
Strings are printed without quotes.

Section: io

Example:
(let ((f (fopen "output.txt" :create :truncate)))
    (fpr f "Hello" " " "World")
    (fclose f))
"#,
    );
    add_builtin(
        env,
        "fprn",
        fprn,
        r#"Usage: (fprn file-handle & values) => nil

Print values to a file followed by a newline.

Arguments:
- file-handle: A file handle. The file to write to (must be writable).
- values: Any type. Zero or more values to print.
- nil: Always returns nil.

Writes the string representation of each value to the file,
then writes a newline character. Strings are printed without quotes.

Section: io

Example:
(let ((f (fopen "output.txt" :create :truncate)))
    (fprn f "Hello World")
    (fprn f "Line 2")
    (fclose f))
"#,
    );
    add_builtin(
        env,
        "dasm",
        dasm,
        r#"Usage: (dasm function-or-expression) => nil

Disassemble a function or compile and disassemble an expression.

Arguments:
- function-or-expression: A function or list. The item to disassemble.
- nil: Always returns nil.

If given a lambda or closure, disassembles its bytecode.
If given a list expression, compiles it first then disassembles.
Prints the disassembly to standard output.

Section: core

Example:
(dasm (fn (x) (+ x 1))) ; disassembles the function
(dasm '(+ 2 3)) ; compiles and disassembles the expression
"#,
    );
    intern_dump_globals(env);
}
