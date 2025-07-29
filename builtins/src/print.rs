use crate::SloshVm;
use crate::vm_inspect::{disassemble_value, dump_call_stack, dump_regs, dump_stack};
use bridge_macros::sl_sh_fn;
use compile_state::state::SloshVmTrait;
use slvm::{Chunk, Interned, VMError, VMResult, Value};
use std::io::{Write, stderr, stdout};
use std::sync::Arc;

fn is_sym(vm: &SloshVm, name: &str, intern: Interned) -> bool {
    if let Some(i) = vm.get_if_interned(name)
        && intern == i
    {
        return true;
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
/// (dump-globals)  ; prints all global variables and their values
#[sl_sh_fn(fn_name = "dump-globals", takes_env = true)]
pub fn dump_globals(environment: &mut SloshVm) -> VMResult<Value> {
    environment.dump_globals();
    Ok(Value::Nil)
}

/// Usage: (dasm callable)
///
/// Disassembles a callable (function, closure, or expression) and prints the bytecode.
/// Shows the compiled bytecode instructions, register usage, and constants.
///
/// Section: core
///
/// Example:
/// (dasm (fn (x) (+ x 1)))
pub fn dasm(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.len() != 1 {
        return Err(VMError::new_compile(
            "dasm: wrong number of args, expected one",
        ));
    }
    let exp = registers[0].unref(vm);
    disassemble_value(vm, exp)?;
    Ok(Value::Nil)
}

/// Usage: (dump-regs)
///
/// Dump the registers for the current call frame. Shows register names (if debug info available)
/// and values. Heap-allocated values are marked with '^'.
///
/// Section: core
///
/// Example:
/// (def test-fn (fn (x y)
///   (dump-regs)  ; shows x in R1, y in R2, plus any scratch registers
///   (+ x y)))
/// (test::assert-equal 5 (test-fn 2 3))
#[sl_sh_fn(takes_env = true, fn_name = "dump-regs")]
pub fn builtin_dump_regs(environment: &mut SloshVm) -> VMResult<Value> {
    if let Some(frame) = environment.call_frame() {
        println!("Previous Call Frames Regs (NOTE: tail calls will be 'missing' Call Frames):");
        dump_regs(environment, frame);
        println!();
    }
    let lambda = if let Some(val) = environment.this_fn() {
        match val {
            Value::Lambda(h) => environment.get_lambda(h),
            Value::Closure(h) => {
                let (l, _) = environment.get_closure(h);
                l
            }
            _ => Arc::new(Chunk::new("", 0)),
        }
    } else {
        Arc::new(Chunk::new("", 0))
    };
    let mut reg_names = lambda.dbg_args.as_ref().map(|iargs| iargs.iter());
    let regs = environment.get_current_registers();
    for (i, r) in regs.iter().enumerate() {
        let aname = if i == 0 {
            "params/result"
        } else if let Some(reg_names) = reg_names.as_mut() {
            if let Some(n) = reg_names.next() {
                environment.get_interned(*n)
            } else {
                "[SCRATCH]"
            }
        } else {
            "[SCRATCH]"
        };
        if let Value::Value(_) = r {
            println!(
                "{:#03} ^{:#20}: {:#12} {}",
                i,
                aname,
                r.display_type(environment),
                r.pretty_value(environment)
            );
        } else {
            println!(
                "{:#03}  {:#20}: {:#12} {}",
                i,
                aname,
                r.display_type(environment),
                r.pretty_value(environment)
            );
        }
    }
    Ok(Value::Nil)
}

/// Usage: (dump-stack)
///
/// Dump the call stack frames. Shows each active call frame with file name,
/// line number, and instruction pointer.
///
/// Section: core
///
/// Example:
/// (def recursive-fn (fn (n)
///   (if (= n 0)
///     (dump-stack)  ; shows call stack at deepest point
///     (recursive-fn (- n 1)))))
/// (recursive-fn 3)
#[sl_sh_fn(takes_env = true, fn_name = "dump-stack")]
pub fn builtin_dump_stack(environment: &mut SloshVm) -> VMResult<Value> {
    dump_call_stack(environment);
    Ok(Value::Nil)
}

/// Usage: (dump-regs-raw)
///
/// Dump all registers in the VM stack. Shows raw stack contents without
/// interpreting call frame boundaries.
///
/// Section: core
///
/// Example:
/// (dump-regs-raw)  ; prints entire VM stack contents
#[sl_sh_fn(takes_env = true, fn_name = "dump-regs-raw")]
pub fn dump_regs_raw(environment: &mut SloshVm) -> VMResult<Value> {
    dump_stack(environment);
    Ok(Value::Nil)
}

pub fn add_print_builtins(env: &mut SloshVm) {
    bridge_adapters::add_builtin(
        env,
        "pr",
        pr,
        r#"Usage: (pr value1 value2 ...)

Print values to stdout without a newline. Values are printed in their "pretty" form,
meaning strings are printed without quotes and character values without delimiters.

Section: core

Example:
(pr "Hello" " " "World")  ; prints: Hello World
(pr 'symbol :keyword 42)   ; prints: symbol:keyword42
"#,
    );
    bridge_adapters::add_builtin(
        env,
        "epr",
        epr,
        r#"Usage: (epr value1 value2 ...)

Print values to stderr without a newline. Values are printed in their "pretty" form,
meaning strings are printed without quotes and character values without delimiters.

Section: core

Example:
(epr "Error: " error-msg)  ; prints to stderr: Error: <error message>
"#,
    );
    bridge_adapters::add_builtin(
        env,
        "prn",
        prn,
        r#"Usage: (prn value1 value2 ...)

Print values to stdout followed by a newline. Values are printed in their "pretty" form,
meaning strings are printed without quotes and character values without delimiters.

Section: core

Example:
(prn "Hello World")        ; prints: Hello World\n
(prn "Line 1")
(prn "Line 2")             ; prints each on separate lines
"#,
    );
    bridge_adapters::add_builtin(
        env,
        "eprn",
        eprn,
        r#"Usage: (eprn value1 value2 ...)

Print values to stderr followed by a newline. Values are printed in their "pretty" form,
meaning strings are printed without quotes and character values without delimiters.

Section: core

Example:
(eprn "Error occurred!")   ; prints to stderr: Error occurred!\n
(eprn "Debug:" x "=" y)    ; prints to stderr: Debug:<x value>=<y value>\n
"#,
    );
    bridge_adapters::add_builtin(
        env,
        "dasm",
        dasm,
        r#"Usage: (dasm callable)

Disassemble a callable (function, closure, or expression) and print its bytecode.
Shows the compiled bytecode instructions, register usage, and constants.

Section: core

Example:
(dasm (fn (x) (+ x 1)))
; Output:
; INPUTS: 2 args/optional/rest 1/0/false
; EXTRA REGS: 1
; 0x00000000  MOV(0x05)    R(0x02) R(0x01)
; 0x00000003  REGI(0x11)   R(0x03) 0x01
; 0x00000006  ADD          R(0x02) R(0x03)
; 0x00000009  SRET(0x03)   R(0x02)

(def my-func (fn (a b) (* a b)))
(dasm my-func)  ; disassemble a named function
"#,
    );
    bridge_adapters::add_builtin(
        env,
        "fpr",
        fpr,
        r#"Usage: (fpr file-handle value1 value2 ...)

Print values to a file handle without a newline. The first argument must be
a writable IO object. Values are printed in their "pretty" form.

Section: core

Example:
(with-temp-file (tmp-file tmp-name)
  (fpr tmp-file "Hello" " " "World")
  (fpr tmp-file "!")
  (flush tmp-file)
  (test::assert-equal "Hello World!" (slurp tmp-name)))

; Or with a regular file:
(def out (open "output.txt" :create :write :truncate))
(fpr out "Hello" " " "World")
(close out)
"#,
    );
    bridge_adapters::add_builtin(
        env,
        "fprn",
        fprn,
        r#"Usage: (fprn file-handle value1 value2 ...)

Print values to a file handle followed by a newline. The first argument must be
a writable IO object. Values are printed in their "pretty" form.

Section: core

Example:
(with-temp-file (tmp-file tmp-name)
  (fprn tmp-file "Line 1")
  (fprn tmp-file "Line 2")
  (flush tmp-file)
  (test::assert-equal "Line 1\nLine 2\n" (slurp tmp-name)))

; Or with a regular file:
(def log (open "app.log" :create :write :append))
(fprn log "Log entry:" (time))
(fprn log "Status: OK")
(close log)
"#,
    );
    intern_builtin_dump_regs(env);
    intern_builtin_dump_stack(env);
    intern_dump_regs_raw(env);
    intern_dump_globals(env);
}
