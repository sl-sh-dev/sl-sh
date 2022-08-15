use std::cell::RefCell;
use std::rc::Rc;

use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;
use slvm::Interned;

use crate::compile::util::get_args_iter;
use crate::state::*;
use crate::{compile, CompileEnvironment};

fn setup_dbg(state: &mut CompileState, reg: usize, scratch: Interned, name: Interned) {
    if let Some(dbg_args) = state.chunk.dbg_args.as_mut() {
        if dbg_args.len() < reg - 1 {
            dbg_args.resize(reg - 1, scratch);
        }
        if reg < dbg_args.len() {
            // This register will have multiple names, maybe concat them or something.
            dbg_args[reg] = name;
        } else {
            dbg_args.push(name);
        }
    }
}

fn let_inner(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    old_tail: bool,
) -> VMResult<()> {
    let start_defers = state.defers;
    let symbols = Rc::new(RefCell::new(Symbols::with_let(
        state.symbols.clone(),
        result,
    )));
    state.symbols = symbols.clone();
    let mut cdr_iter = cdr.iter();
    let args = cdr_iter.next().unwrap(); // unwrap safe, length is at least 1
    let mut right_exps: Vec<(usize, Value)> = Vec::new();
    let mut destructures = Vec::new();
    let mut all_optionals = Vec::new();
    let scratch = env.vm_mut().intern("[SCRATCH]");
    let rest_i = env.vm_mut().intern("&");
    let opt_i = env.vm_mut().intern("%");
    let opt_set_i = env.vm_mut().intern("%=");
    env.set_line_val(state, *args);
    let args: Vec<Value> = get_args_iter(env, *args, "let")?.collect();
    let mut args_iter = args.iter();
    while let Some(a) = args_iter.next() {
        env.set_line_val(state, *a);
        match a {
            Value::Symbol(i) => {
                let reg = symbols.borrow_mut().insert(*i) + 1;
                setup_dbg(state, reg, scratch, *i);
                if let Some(r) = args_iter.next() {
                    right_exps.push((reg, *r));
                } else {
                    return Err(VMError::new_compile("symbol must have a value"));
                }
            }
            Value::Vector(h) => {
                let vector = env.vm().get_vector(*h);
                let reg = symbols.borrow_mut().reserve_reg() + 1;
                setup_dbg(state, reg, scratch, scratch);
                if let Some(r) = args_iter.next() {
                    right_exps.push((reg, *r));
                } else {
                    return Err(VMError::new_compile("must have a value"));
                }
                let mut stack = vec![(vector, reg)];
                let mut start_reg = reg + 1;
                while let Some((vector, reg)) = stack.pop() {
                    let mut len = vector.len();
                    let mut rest = false;
                    let mut opt = false;
                    let mut opt_set_next = false;
                    let mut rest_cnt = 0;
                    let mut opt_comps = Vec::new();
                    for name in vector {
                        if opt_set_next {
                            len -= 1;
                            opt_set_next = false;
                            if let Some((reg, _)) = opt_comps.pop() {
                                opt_comps.push((reg, *name));
                            }
                        } else {
                            match name {
                                Value::Symbol(i) if *i == rest_i => {
                                    len -= 1;
                                    rest = true;
                                }
                                Value::Symbol(i) if *i == opt_i => {
                                    len -= 1;
                                    opt = true;
                                }
                                Value::Symbol(i) if *i == opt_set_i => {
                                    if opt {
                                        len -= 1;
                                        opt_set_next = true;
                                    } else {
                                        return Err(VMError::new_compile(
                                            "%= only valid for optionals (after %)",
                                        ));
                                    }
                                }
                                Value::Symbol(i) => {
                                    let reg = symbols.borrow_mut().insert(*i) + 1;
                                    setup_dbg(state, reg, scratch, *i);
                                    if rest {
                                        rest_cnt += 1;
                                    } else if opt {
                                        opt_comps.push((reg, Value::Nil));
                                    }
                                }
                                Value::Vector(h) => {
                                    let vector = env.vm().get_vector(*h);
                                    let reg = symbols.borrow_mut().reserve_reg() + 1;
                                    setup_dbg(state, reg, scratch, scratch);
                                    stack.push((vector, reg));
                                }
                                _ => return Err(VMError::new_compile("not a valid destructure")),
                            }
                        }
                        if rest_cnt > 1 {
                            return Err(VMError::new_compile(
                                "not a valid destructure (invalid &)",
                            ));
                        }
                    }
                    if rest && rest_cnt == 0 {
                        // Allow extras but don't need them.
                        let reg = symbols.borrow_mut().reserve_reg() + 1;
                        setup_dbg(state, reg, scratch, scratch);
                    }
                    if opt_set_next {
                        return Err(VMError::new_compile("not a valid destructure (invalid %=)"));
                    }
                    destructures.push((start_reg as u16, len as u16, reg as u16, rest));
                    if !opt_comps.is_empty() {
                        all_optionals.push(opt_comps);
                    }
                    start_reg += len;
                }
            }
            _ => return Err(VMError::new_compile("must be a symbol")),
        }
    }
    let mut free_reg = result;
    for (reg, val) in right_exps {
        compile(env, state, val, reg)?;
        free_reg = reg + 1;
    }
    for (start_reg, len, reg, rest) in destructures {
        if rest {
            state.chunk.encode3(
                LDSCR,
                start_reg as u16,
                len as u16,
                reg as u16,
                env.own_line(),
            )?;
        } else {
            state.chunk.encode3(
                LDSC,
                start_reg as u16,
                len as u16,
                reg as u16,
                env.own_line(),
            )?;
        }
        let temp_max = (start_reg + len) as usize;
        if temp_max > free_reg {
            free_reg = temp_max;
        }
    }
    for opt_comps in all_optionals {
        for (target_reg, default) in opt_comps.into_iter() {
            state
                .chunk
                .encode1(JMPNU, target_reg as u16, env.own_line())?;
            let encode_offset = state.chunk.code.len();
            state.chunk.encode_jump_offset(0)?;
            let start_offset = state.chunk.code.len();
            compile(env, state, default, free_reg)?;
            state
                .chunk
                .encode2(MOV, target_reg as u16, free_reg as u16, env.own_line())?;
            state.chunk.reencode_jump_offset(
                encode_offset,
                (state.chunk.code.len() - start_offset) as i32,
            )?;
        }
    }
    let last_thing = if cdr.len() > 1 { cdr.len() - 2 } else { 0 };
    for (i, r) in cdr_iter.enumerate() {
        if i == last_thing {
            state.tail = old_tail;
        }
        compile(env, state, *r, free_reg)?;
    }
    if free_reg != result {
        state
            .chunk
            .encode2(MOV, result as u16, free_reg as u16, env.own_line())?;
    }
    for _ in start_defers..state.defers {
        state.chunk.encode0(DFRPOP, env.own_line())?;
    }
    Ok(())
}

pub(crate) fn compile_let(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    if cdr.is_empty() {
        return Err(VMError::new_compile(
            "Too few arguments, need at least 1 got 0.",
        ));
    }
    let old_symbols = state.symbols.clone();
    let old_tail = state.tail;
    state.tail = false;
    let old_defers = state.defers;
    let result = let_inner(env, state, cdr, result, old_tail);
    state.tail = old_tail;
    state.symbols = old_symbols;
    state.defers = old_defers;
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{assert_vals, exec, exec_compile_error, read_test};
    use builtins::print::prn;
    use slvm::Vm;

    #[test]
    fn test_let() {
        let mut vm = Vm::new();
        vm.set_global("prn", Value::Builtin(CallFunc { func: prn }));

        let result = exec(&mut vm, "(do (def x 3) (let (x 10) (set! x 1)) x)");
        let expected = read_test(&mut vm, "3");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let (x 10) x)");
        let expected = read_test(&mut vm, "10");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let (x 10) (set! x 5) x)");
        let expected = read_test(&mut vm, "5");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let (x 10 y (+ x 10)) (set! x 5) `(~x ~y))");
        let expected = read_test(&mut vm, "(5 20)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def x 5) (let (x 10 y (+ x 10)) (set! x 15) `(~x ~y)))",
        );
        let expected = read_test(&mut vm, "(15 20)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def x 5) (let (x 10 y (+ x 10)) (set! x 15) `(~x ~y)))",
        );
        let expected = read_test(&mut vm, "(15 20)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let (fnx (fn (x) (if (= x 0) #t (fny (- x 1))))\
                                      fny (fn (y) (if (= y 0) #t (fnx (- y 1)))))\
                                   (fnx 10))",
        );
        let expected = read_test(&mut vm, "#t");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do\
                                  (def x 1)\
                                  (def y 2)\
                                  (def z 3)\
                                  (let (xl 10 \
                                        yl 20 \
                                        zl 30)\
                                    (defer (set! x 5))\
                                    (defer (set! y 6))\
                                    (defer (set! z 7))\
                                    (set! x xl)\
                                    (set! y yl)\
                                    (set! z zl)\
                                    (list x y z)))",
        );
        let expected = read_test(&mut vm, "(10 20 30)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do\
                                  (def x 1)\
                                  (def y 2)\
                                  (def z 3)\
                                  (let (xl 10 \
                                        yl 20 \
                                        zl 30)\
                                    (defer (set! x 5))\
                                    (defer (set! y 6))\
                                    (defer (set! z 7))\
                                    (set! x xl)\
                                    (set! y yl)\
                                    (set! z zl))\
                                  (list x y z))",
        );
        let expected = read_test(&mut vm, "(5 6 7)");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let (x y) x)");
        let expected = read_test(&mut vm, "6");
        assert_vals(&vm, expected, result);

        exec_compile_error(&mut vm, "(let (x) (set! x 5) x)");
        exec_compile_error(&mut vm, "(let (x y_undef) (set! x 5) x)");
        exec_compile_error(&mut vm, "(let (x 10 y_undef) (set! x 5) x)");

        exec_compile_error(&mut vm, "(let (x_undef) (set! x 5) x)");
        exec_compile_error(&mut vm, "(let (x_undef y_undef) (set! x 5) x)");
        exec_compile_error(&mut vm, "(let (x_undef 10 y_undef) (set! x 5) x)");
    }

    #[test]
    fn test_let_destructure() {
        let mut vm = Vm::new();
        vm.set_global("prn", Value::Builtin(CallFunc { func: prn }));

        let result = exec(&mut vm, "(let ([a b % c d] '(1 2)) (list a b c d))");
        let expected = read_test(&mut vm, "(1 2 nil nil)");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let ([a b % c %= :none d] '(1 2)) (list a b c d))");
        let expected = read_test(&mut vm, "(1 2 :none nil)");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let ([a [b % c %= :none] % d] '(1 [2])) (list a b c d))");
        let expected = read_test(&mut vm, "(1 2 :none nil)");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let ([a [b % c %= :none] % d & rest] '(1 [2])) (list a b c d rest))");
        let expected = read_test(&mut vm, "(1 2 :none nil nil)");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let ([a [b % c %= :none] % d & rest] '(1 [2] 3)) (list a b c d rest))");
        let expected = read_test(&mut vm, "(1 2 :none 3 nil)");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let ([a [b % c %= :none] % d & rest] '(1 [2] 3 4)) (list a b c d rest))");
        let expected = read_test(&mut vm, "(1 2 :none 3 (4))");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let ([a [b % c %= :none] % d %= \"d\" & rest] '(1 [2 3])) (list a b c d rest))");
        let expected = read_test(&mut vm, "(1 2 3 \"d\" nil)");
        assert_vals(&vm, expected, result);
    }
}
