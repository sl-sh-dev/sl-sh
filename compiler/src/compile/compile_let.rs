use compile_state::state::{CompileState, Symbols};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use slvm::opcodes::*;
use slvm::{Interned, VMError, VMResult, Value};

use crate::compile::destructure::{
    resolve_destruct_containers, setup_dbg, DestructState, DestructType,
};
use crate::compile::util::get_args_iter;
use crate::{compile, SloshVm, SloshVmTrait};

type RightSideExp = (Option<Interned>, Option<usize>, Value, Option<DestructType>);

/// Compile a value "out of the way" of the let bindings and then set the target register.
/// For calls that use multiple regs and destructuring lets in use registers can get walked over
/// without this.
/// Use SET not MOV, we don't want to replace a value that has been captured and SET honors this, MOV
/// does not.
fn compile_let_value(
    env: &mut SloshVm,
    state: &mut CompileState,
    val: Value,
    reg: u16,
) -> VMResult<()> {
    let scratch_reg = state.reserved_regs() + 1;
    compile(env, state, val, scratch_reg)?;
    if reg as usize != scratch_reg {
        state.chunk.encode2(SET, reg, scratch_reg as u16, None)?;
    }
    Ok(())
}

fn add_right_side_exp(
    env: &mut SloshVm,
    right_exps: &mut Vec<RightSideExp>,
    a: Value,
    value: Value,
    state: &mut CompileState,
    symbols: Rc<RefCell<Symbols>>,
    allow_shadows: bool,
) -> VMResult<()> {
    let a = resolve_destruct_containers(env, a);
    match a {
        Value::Symbol(i) => {
            if symbols.borrow().contains_symbol(i) {
                if allow_shadows {
                    let reg = symbols.borrow_mut().reserve_reg();
                    right_exps.push((Some(i), Some(reg), value, None));
                } else {
                    let r = symbols.borrow().get(i).expect("we have the symbol!");
                    right_exps.push((Some(i), Some(r), value, None));
                }
            } else {
                let reg = symbols.borrow_mut().insert(i);
                if let Some(lets) = &mut state.lets {
                    lets.insert(i, reg);
                }
                setup_dbg(env, state, reg, i);
                right_exps.push((None, Some(reg), value, None));
            }
        }
        Value::Vector(h) => {
            let reg = symbols.borrow_mut().reserve_reg();
            setup_dbg(env, state, reg, env.specials().scratch);
            let dtype = DestructType::Vector(h, reg);
            right_exps.push((None, Some(reg), value, Some(dtype)));
        }
        Value::Map(h) => {
            let reg = symbols.borrow_mut().reserve_reg();
            setup_dbg(env, state, reg, env.specials().scratch);
            let dtype = DestructType::Map(h, reg);
            right_exps.push((None, Some(reg), value, Some(dtype)));
        }
        _ => {
            return Err(VMError::new_compile(
                "must be a symbol or destructure pattern",
            ))
        }
    }
    Ok(())
}

fn compile_right_exps(
    env: &mut SloshVm,
    right_exps: Vec<RightSideExp>,
    state: &mut CompileState,
    symbols: Rc<RefCell<Symbols>>,
    free_reg: &mut usize,
) -> VMResult<()> {
    let mut destruct_state = DestructState::new();
    for (interned, reg, val, destruct_type) in right_exps {
        match (interned, reg, destruct_type) {
            (Some(interned), Some(reg), None) => {
                // Use the reserved but unnamed reg.  Do this so we can access any
                // previous version of this name before we shadow it.
                setup_dbg(env, state, reg, interned);
                compile_let_value(env, state, val, reg as u16)?;
                symbols.borrow_mut().insert_reserved(interned, reg);
                if let Some(lets) = &mut state.lets {
                    lets.insert(interned, reg);
                }
                if *free_reg < reg + 1 {
                    *free_reg = reg + 1;
                }
            }
            (None, Some(reg), None) => {
                compile_let_value(env, state, val, reg as u16)?;
                if *free_reg < reg + 1 {
                    *free_reg = reg + 1;
                }
            }
            (None, Some(reg), Some(dtype)) => {
                if *free_reg < reg + 1 {
                    *free_reg = reg + 1;
                }
                destruct_state.do_destructure(env, state, dtype)?;
                compile_let_value(env, state, val, reg as u16)?;
                destruct_state.compile(env, state, free_reg)?;
                *free_reg = state.reserved_regs();
            }
            _ => panic!("Broken let compile, both interned and a reg!"),
        }
    }
    Ok(())
}

fn let_inner(
    env: &mut SloshVm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    old_tail: bool,
) -> VMResult<()> {
    let start_defers = state.defers;
    let symbols = Rc::new(RefCell::new(Symbols::with_let(state.symbols.clone())));
    state.symbols = symbols.clone();
    let mut first_reg = symbols.borrow().regs_count();
    while first_reg <= result {
        // Make sure we do not step on the result or any other regs in temp use below it.
        first_reg = symbols.borrow_mut().reserve_reg();
    }
    let mut cdr_iter = cdr.iter();
    let args = cdr_iter.next().unwrap(); // unwrap safe, length is at least 1
    let mut right_exps: Vec<RightSideExp> = Vec::new();
    let args: Vec<Value> = get_args_iter(env, *args, "let")?.collect();
    let mut args_iter = args.iter();
    while let Some(a) = args_iter.next() {
        let value = if let Some(r) = args_iter.next() {
            *r
        } else {
            return Err(VMError::new_compile(format!(
                "let: symbol {} must have a value",
                a.display_value(env)
            )));
        };
        add_right_side_exp(
            env,
            &mut right_exps,
            *a,
            value,
            state,
            symbols.clone(),
            true,
        )?;
    }
    let mut free_reg = result;
    compile_right_exps(env, right_exps, state, symbols.clone(), &mut free_reg)?;
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
    for i in first_reg..symbols.borrow().regs_count() {
        if i != result {
            // TODO- should probably add a bulk opcode for this sort of clearing.
            state.chunk.encode1(CLRREG, i as u16, env.own_line())?;
        }
    }
    Ok(())
}

pub(crate) fn compile_let(
    env: &mut SloshVm,
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
    let old_lets = state.lets.take();
    state.lets = Some(HashMap::new());
    state.tail = false;
    let old_defers = state.defers;
    let result = let_inner(env, state, cdr, result, old_tail);
    state.tail = old_tail;
    state.symbols = old_symbols;
    state.defers = old_defers;
    state.lets = old_lets;
    result
}

fn let_while_inner(
    env: &mut SloshVm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    let start_defers = state.defers;
    let symbols = Rc::new(RefCell::new(Symbols::with_let(state.symbols.clone())));
    state.symbols = symbols.clone();
    let mut first_reg = symbols.borrow().regs_count();
    let mut cdr_iter = cdr.iter();
    let init_bindings = cdr_iter
        .next()
        .expect("had enough params (>= 3) must have init_bindings"); // unwrap safe, length is at least 1
    let init_bindings: Vec<Value> = get_args_iter(env, *init_bindings, "let-while")?.collect();
    let init_bindings_iter = init_bindings.iter();
    let loop_bindings = cdr_iter
        .next()
        .expect("had enough params (>= 3) must have loop_bindings"); // unwrap safe, length is at least 2
    let loop_bindings: Vec<Value> = get_args_iter(env, *loop_bindings, "let-while")?.collect();
    let mut init_loop_bindings_iter = init_bindings_iter.chain(loop_bindings.iter());
    let mut right_exps: Vec<RightSideExp> = Vec::new();
    while let Some(a) = init_loop_bindings_iter.next() {
        while first_reg <= result {
            // Make sure we do not step on the result or any other regs in temp use below it.
            first_reg = symbols.borrow_mut().reserve_reg();
        }
        let value = if let Some(r) = init_loop_bindings_iter.next() {
            *r
        } else {
            return Err(VMError::new_compile(format!(
                "let: symbol {} must have a value",
                a.display_value(env)
            )));
        };
        add_right_side_exp(
            env,
            &mut right_exps,
            *a,
            value,
            state,
            symbols.clone(),
            true,
        )?;
    }
    let mut free_reg = result;
    compile_right_exps(env, right_exps, state, symbols.clone(), &mut free_reg)?;

    let jmp_cond = state.chunk.add_jump(0);
    state.chunk.encode1(JMP, jmp_cond as u16, env.own_line())?;

    if let Some(conditional) = cdr_iter.next() {
        let jmp_loop_start = state.chunk.add_jump(state.chunk.code.len() as u32);
        for r in cdr_iter {
            compile(env, state, *r, free_reg)?;
        }

        let mut right_exps: Vec<RightSideExp> = Vec::new();
        let mut loop_bindings_iter = loop_bindings.iter();
        while let Some(a) = loop_bindings_iter.next() {
            let value = if let Some(r) = loop_bindings_iter.next() {
                *r
            } else {
                return Err(VMError::new_compile(format!(
                    "let: symbol {} must have a value",
                    a.display_value(env)
                )));
            };
            add_right_side_exp(
                env,
                &mut right_exps,
                *a,
                value,
                state,
                symbols.clone(),
                false,
            )?;
        }
        let mut free_reg2 = result;
        compile_right_exps(env, right_exps, state, symbols.clone(), &mut free_reg2)?;

        state
            .chunk
            .update_jump(jmp_cond, state.chunk.code.len() as u32);
        compile(env, state, *conditional, free_reg)?;
        state
            .chunk
            .encode2(JMPT, free_reg as u16, jmp_loop_start as u16, env.own_line())?;
    } else {
        return Err(VMError::new_compile(format!(
            "missing conditional, line {}",
            env.line_num()
        )));
    }

    if free_reg != result {
        state
            .chunk
            .encode2(MOV, result as u16, free_reg as u16, env.own_line())?;
    }
    for _ in start_defers..state.defers {
        state.chunk.encode0(DFRPOP, env.own_line())?;
    }
    for i in first_reg..symbols.borrow().regs_count() {
        if i != result {
            // TODO- should probably add a bulk opcode for this sort of clearing.
            state.chunk.encode1(CLRREG, i as u16, env.own_line())?;
        }
    }
    Ok(())
}

pub(crate) fn compile_let_while(
    env: &mut SloshVm,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
) -> VMResult<()> {
    if cdr.len() < 3 {
        return Err(VMError::new_compile(
            "Too few arguments, need at least 3 (init-bindings, loop-bindings, conditional).",
        ));
    }
    let old_symbols = state.symbols.clone();
    let old_tail = state.tail;
    let old_lets = state.lets.take();
    state.lets = Some(HashMap::new());
    state.tail = false;
    let old_defers = state.defers;
    let result = let_while_inner(env, state, cdr, result);
    state.tail = old_tail;
    state.symbols = old_symbols;
    state.defers = old_defers;
    state.lets = old_lets;
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{assert_vals, exec, exec_compile_error, exec_runtime_error, read_test};
    use builtins::print::{dasm, prn};
    use compile_state::state::new_slosh_vm;

    #[test]
    fn test_let() {
        let mut env = new_slosh_vm();
        env.set_global_builtin("prn", prn);
        env.set_global_builtin("dasm", dasm);

        let result = exec(&mut env, "(let (a 1, b 2, c 3) `(~a ~b ~c))");
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let (a 1, b 2, c 3) (let (b 20, c (+ b 10)) `(~a ~b ~c)))",
        );
        let expected = read_test(&mut env, "(1 20 30)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let (a 1, b 2, c 3) (let (x (+ b 1), b 20, c (+ b 10)) `(~a ~x ~b ~c)))",
        );
        let expected = read_test(&mut env, "(1 3 20 30)");
        assert_vals(&env, expected, result);

        let result = exec(&mut env, "(do (def x 3) (let (x 10) (set! x 1)) x)");
        let expected = read_test(&mut env, "3");
        assert_vals(&env, expected, result);

        let result = exec(&mut env, "(let (x 10) x)");
        let expected = read_test(&mut env, "10");
        assert_vals(&env, expected, result);

        let result = exec(&mut env, "(let (x 10) (set! x 5) x)");
        let expected = read_test(&mut env, "5");
        assert_vals(&env, expected, result);

        let result = exec(&mut env, "(let (x 10 y (+ x 10)) (set! x 5) `(~x ~y))");
        let expected = read_test(&mut env, "(5 20)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(do (def x 5) (let (x 10 y (+ x 10)) (set! x 15) `(~x ~y)))",
        );
        let expected = read_test(&mut env, "(15 20)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(do (def x 5) (let (x 10 y (+ x 10)) (set! x 15) `(~x ~y)))",
        );
        let expected = read_test(&mut env, "(15 20)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let (fnx (fn (x) (if (= x 0) #t (fny (- x 1))))\
                        fny (fn (y) (if (= y 0) #t (fnx (- y 1)))))\
                       (fnx 10))",
        );
        let expected = read_test(&mut env, "#t");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let (fny (fn (y) y))\
                       (let (fnx (fn (x) (if (= x 0) #t (fny (- x 1))))\
                             fny (fn (y) (if (= y 0) #t (fnx (- y 1)))))\
                       (fny 10)))",
        );
        let expected = read_test(&mut env, "8");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(do (def fnx (fn () (let (fny (fn (y) y))\
                      (let (fnx (fn (x) (if (= x 0) #t (fny (- x 1))))\
                            fny (fn (y) (if (= y 0) #t (fnx (- y 1)))))\
                        (fny 10))))) (dasm fnx) (fnx))",
        );
        let expected = read_test(&mut env, "8");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
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
        let expected = read_test(&mut env, "(10 20 30)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
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
        let expected = read_test(&mut env, "(5 6 7)");
        assert_vals(&env, expected, result);

        let result = exec(&mut env, "(let (x y) x)");
        let expected = read_test(&mut env, "6");
        assert_vals(&env, expected, result);

        exec_compile_error(&mut env, "(let (x) (set! x 5) x)");
        exec_compile_error(&mut env, "(let (x y_undef) (set! x 5) x)");
        exec_compile_error(&mut env, "(let (x 10 y_undef) (set! x 5) x)");

        exec_compile_error(&mut env, "(let (x_undef) (set! x 5) x)");
        exec_compile_error(&mut env, "(let (x_undef y_undef) (set! x 5) x)");
        exec_compile_error(&mut env, "(let (x_undef 10 y_undef) (set! x 5) x)");
    }

    #[test]
    fn test_let_destructure() {
        let mut env = new_slosh_vm();
        env.set_global_builtin("prn", prn);

        let result = exec(
            &mut env,
            "\
        (def x '(1 2 3))\
        (let ([a b c] x) `(~a ~b ~c))",
        );
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);

        let result = exec(&mut env, "(let ([a b % c d] '(1 2)) (list a b c d))");
        let expected = read_test(&mut env, "(1 2 nil nil)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ([a b c] '(1 2 3), [x y z] [4 5 6]) (list a b c x y z))",
        );
        let expected = read_test(&mut env, "(1 2 3 4 5 6)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ([a b % c] '(1 2), [x % y := 10 z := 11] [4 5]) (list a b c x y z))",
        );
        let expected = read_test(&mut env, "(1 2 nil 4 5 11)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ([a b % c := :none d] '(1 2)) (list a b c d))",
        );
        let expected = read_test(&mut env, "(1 2 :none nil)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ([a [b % c := :none] % d] `(1 ~[2])) (list a b c d))",
        );
        let expected = read_test(&mut env, "(1 2 :none nil)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ([a [b % c := :none] % d & rest] (list 1 [2])) (list a b c d rest))",
        );
        let expected = read_test(&mut env, "(1 2 :none nil nil)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ([a [b % c := :none] % d & rest] (list 1 [2] 3)) (list a b c d rest))",
        );
        let expected = read_test(&mut env, "(1 2 :none 3 nil)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ([a [b % c := :none] % d & rest] `(1 ~[2] 3 4)) (list a b c d rest))",
        );
        let expected = read_test(&mut env, "(1 2 :none 3 (4))");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ([a [b % c := :none] % d := \"d\" & rest] (list 1 [2 3])) (list a b c d rest))",
        );
        let expected = read_test(&mut env, "(1 2 3 \"d\" nil)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ([a [b % c := :none] % d := \"d\" &] (list 1 [2 3] 4 5 6 7)) (list a b c d))",
        );
        let expected = read_test(&mut env, "(1 2 3 4)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ({a :one, b 'two, c \"three\"} {:one 1, 'two 2, \"three\" 3}) (list a b c))",
        );
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ({a :one, b 'two, c \"three\" [d e] :vec} {:one 1, 'two 2, \"three\" 3, :vec '(4 5)}) (list a b c d e))",
        );
        let expected = read_test(&mut env, "(1 2 3 4 5)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ([x y {a :one, b 'two, c \"three\"} z] [10 11 {:one 1, 'two 2, \"three\" 3} 12]) (list a b c x y z))",
        );
        let expected = read_test(&mut env, "(1 2 3 10 11 12)");
        assert_vals(&env, expected, result);

        exec_runtime_error(&mut env, "(let ([a b c] '()) nil)");
        exec_runtime_error(&mut env, "(let ([a b c] []) nil)");
        exec_runtime_error(&mut env, "(let ([a b c] '(1)) nil)");
        exec_runtime_error(&mut env, "(let ([a b c] [1]) nil)");
        exec_runtime_error(&mut env, "(let ([a b c] '(1 2)) nil)");
        exec_runtime_error(&mut env, "(let ([a b c] [1 2]) nil)");

        exec_runtime_error(&mut env, "(let ({a :a, b :b, c :c} {}) nil)");
        exec_runtime_error(&mut env, "(let ({a :a, b :b, c :c} {:a 1}) nil)");
        exec_runtime_error(&mut env, "(let ({a :a, b :b, c :c} {:a 1, :b 2}) nil)");
        exec_runtime_error(&mut env, "(let ({a :a, b :b, c :c} {:a 1, :c 3}) nil)");
        exec_runtime_error(&mut env, "(let ({a :a, b :b, c :c} {:b 2, :c 3}) nil)");

        exec_runtime_error(
            &mut env,
            "(let ([x {a :a, b :b, c :c} y] [10 {:b 2, :c 3} 11]) nil)",
        );
        exec_runtime_error(
            &mut env,
            "(let ([x {a :a, b :b, c :c} y] [10 {:a 1 :b 2, :c 3}]) nil)",
        );

        let result = exec(&mut env, "(let ({a 1, b 0, c 2} [1 2 3]) (list a b c))");
        let expected = read_test(&mut env, "(2 1 3)");
        assert_vals(&env, expected, result);

        let result = exec(&mut env, "(let ({a 1, b 0, c 2} '(1 2 3)) (list a b c))");
        let expected = read_test(&mut env, "(2 1 3)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ({a 1, b 0, c 2} (list 1 2 3)) (list a b c))",
        );
        let expected = read_test(&mut env, "(2 1 3)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ({a :a, b :b, c :c} [:a 1, :b 2, :c 3]) `(~a ~b ~c))",
        );
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ({a :a, b :b, c :c} '(:a 1, :b 2, :c 3)) `(~a ~b ~c))",
        );
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let ({a :a, b :b, c :c} (list :a 1, :b 2, :c 3)) `(~a ~b ~c))",
        );
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);
    }

    #[test]
    fn test_let_shadow() {
        let mut env = new_slosh_vm();
        env.set_global_builtin("prn", prn);
        env.set_global_builtin("dasm", dasm);

        let result = exec(
            &mut env,
            "(let (a1 10, b1 20, c1 30, a (- a1 9), b (- b1 18), c (- c1 27)) `(~a ~b ~c))",
        );
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let (a 10, b 20, c 30, a (- a 9), b (- b 18), c (- c 27)) `(~a ~b ~c))",
        );
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let (a 1, b 2, c 3, {a :a, b :b, c :c} (list :a a, :b b, :c c)) `(~a ~b ~c))",
        );
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let (a1 10, b1 11, c1 12)(let (a 1, b 2, c 3, {a :a, b :b, c :c} (list :a a, :b b, :c c)) `(~a ~b ~c)))",
        );
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let (a 10, b 2, c 0)(let (a (- a 9), b b, c (+ c 3)) `(~a ~b ~c)))",
        );
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let (a 10, b 11, c 12)(let (a 1, b 2, c 3, [a b c] (list a, b, c)) `(~a ~b ~c)))",
        );
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let (a 10, b1 11, c1 12)(let (a 1, b 2, c 3, {a :a, b :b, c :c} (list :a a, :b b, :c c)) `(~a ~b ~c)))",
        );
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(let (a 10, b 11, c 12)(let (a 1, b 2, c 3, {a :a, b :b, c :c} (make-hash :a a, :b b, :c c)) `(~a ~b ~c)))",
        );
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);

        let result = exec(
            &mut env,
            "(do (def fnx (fn () (let (a 10, b 11, c 12)(let (a 1, b 2, c 3, {a :a, b :b, c :c} (make-hash :a a, :b b, :c c)) `(~a ~b ~c)))))(fnx))",
        );
        let expected = read_test(&mut env, "(1 2 3)");
        assert_vals(&env, expected, result);
    }
}
