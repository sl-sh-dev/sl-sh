use std::cell::RefCell;
use std::rc::Rc;

use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;
use slvm::Interned;

use crate::compile::destructure::{setup_dbg, DestructState, DestructType};
use crate::compile::util::get_args_iter;
use crate::state::*;
use crate::{compile, CompileEnvironment};

type RightSideExp = (Option<Interned>, Option<usize>, Value, Option<DestructType>);

fn let_inner(
    env: &mut CompileEnvironment,
    state: &mut CompileState,
    cdr: &[Value],
    result: usize,
    old_tail: bool,
) -> VMResult<()> {
    let start_defers = state.defers;
    let symbols = Rc::new(RefCell::new(Symbols::with_let(state.symbols.clone())));
    state.symbols = symbols.clone();
    let mut cdr_iter = cdr.iter();
    let args = cdr_iter.next().unwrap(); // unwrap safe, length is at least 1
    let mut right_exps: Vec<RightSideExp> = Vec::new();
    let mut destruct_state = DestructState::new();
    env.set_line_val(state, *args);
    let args: Vec<Value> = get_args_iter(env, *args, "let")?.collect();
    let mut args_iter = args.iter();
    while let Some(a) = args_iter.next() {
        env.set_line_val(state, *a);
        let value = if let Some(r) = args_iter.next() {
            *r
        } else {
            return Err(VMError::new_compile("symbol must have a value"));
        };
        match a {
            Value::Symbol(i) => {
                if symbols.borrow().contains_symbol(*i) {
                    let reg = symbols.borrow_mut().reserve_reg();
                    right_exps.push((Some(*i), Some(reg), value, None));
                } else {
                    let reg = symbols.borrow_mut().insert(*i);
                    setup_dbg(env, state, reg, *i);
                    right_exps.push((None, Some(reg), value, None));
                }
            }
            Value::Vector(h) => {
                let reg = symbols.borrow_mut().reserve_reg();
                setup_dbg(env, state, reg, env.specials().scratch);
                let dtype = DestructType::Vector(*h, reg);
                right_exps.push((None, Some(reg), value, Some(dtype)));
            }
            Value::Map(h) => {
                let reg = symbols.borrow_mut().reserve_reg();
                setup_dbg(env, state, reg, env.specials().scratch);
                let dtype = DestructType::Map(*h, reg);
                right_exps.push((None, Some(reg), value, Some(dtype)));
            }
            _ => return Err(VMError::new_compile("must be a symbol")),
        }
    }
    let mut free_reg = result;
    for (interned, reg, val, destruct_type) in right_exps {
        match (interned, reg, destruct_type) {
            (Some(interned), Some(reg), None) => {
                // Use the reserved but unnamed reg.  Do this so we can access any
                // previous version of this name before we shadow it.
                setup_dbg(env, state, reg, interned);
                compile(env, state, val, reg)?;
                symbols.borrow_mut().insert_reserved(interned, reg);
                if free_reg < reg + 1 {
                    free_reg = reg + 1;
                }
            }
            (None, Some(reg), None) => {
                compile(env, state, val, reg)?;
                if free_reg < reg + 1 {
                    free_reg = reg + 1;
                }
            }
            (None, Some(reg), Some(dtype)) => {
                compile(env, state, val, reg)?;
                if free_reg < reg + 1 {
                    free_reg = reg + 1;
                }
                destruct_state.do_destructure(env, state, dtype)?;
                destruct_state.compile(env, state, &mut free_reg)?;
                free_reg = state.reserved_regs();
            }
            _ => panic!("Broken let compile, both interned and a reg!"),
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
    use crate::test_utils::{assert_vals, exec, exec_compile_error, exec_runtime_error, read_test};
    use builtins::collections::make_hash;
    use builtins::print::{dasm, prn};
    use slvm::Vm;

    #[test]
    fn test_let() {
        let mut vm = Vm::new();
        vm.set_global("prn", Value::Builtin(CallFunc { func: prn }));
        vm.set_global("dasm", Value::Builtin(CallFunc { func: dasm }));

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
            "(let (fny (fn (y) y))\
                       (let (fnx (fn (x) (if (= x 0) #t (fny (- x 1))))\
                             fny (fn (y) (if (= y 0) #t (fnx (- y 1)))))\
                       (fny 10)))",
        );
        let expected = read_test(&mut vm, "8");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def fnx (fn () (let (fny (fn (y) y))\
                      (let (fnx (fn (x) (if (= x 0) #t (fny (- x 1))))\
                            fny (fn (y) (if (= y 0) #t (fnx (- y 1)))))\
                        (fny 10))))) (dasm fnx) (fnx))",
        );
        let expected = read_test(&mut vm, "8");
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

        let result = exec(
            &mut vm,
            "(let ([a b c] '(1 2 3), [x y z] [4 5 6]) (list a b c x y z))",
        );
        let expected = read_test(&mut vm, "(1 2 3 4 5 6)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let ([a b % c] '(1 2), [x % y := 10 z := 11] [4 5]) (list a b c x y z))",
        );
        let expected = read_test(&mut vm, "(1 2 nil 4 5 11)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let ([a b % c := :none d] '(1 2)) (list a b c d))",
        );
        let expected = read_test(&mut vm, "(1 2 :none nil)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let ([a [b % c := :none] % d] '(1 [2])) (list a b c d))",
        );
        let expected = read_test(&mut vm, "(1 2 :none nil)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let ([a [b % c := :none] % d & rest] '(1 [2])) (list a b c d rest))",
        );
        let expected = read_test(&mut vm, "(1 2 :none nil nil)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let ([a [b % c := :none] % d & rest] '(1 [2] 3)) (list a b c d rest))",
        );
        let expected = read_test(&mut vm, "(1 2 :none 3 nil)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let ([a [b % c := :none] % d & rest] '(1 [2] 3 4)) (list a b c d rest))",
        );
        let expected = read_test(&mut vm, "(1 2 :none 3 (4))");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let ([a [b % c := :none] % d := \"d\" & rest] '(1 [2 3])) (list a b c d rest))",
        );
        let expected = read_test(&mut vm, "(1 2 3 \"d\" nil)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let ([a [b % c := :none] % d := \"d\" &] '(1 [2 3] 4 5 6 7)) (list a b c d))",
        );
        let expected = read_test(&mut vm, "(1 2 3 4)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let ({a :one, b 'two, c \"three\"} {:one 1, two 2, \"three\" 3}) (list a b c))",
        );
        let expected = read_test(&mut vm, "(1 2 3)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let ({a :one, b 'two, c \"three\" [d e] :vec} {:one 1, two 2, \"three\" 3, :vec (4 5)}) (list a b c d e))",
        );
        let expected = read_test(&mut vm, "(1 2 3 4 5)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let ([x y {a :one, b 'two, c \"three\"} z] [10 11 {:one 1, two 2, \"three\" 3} 12]) (list a b c x y z))",
        );
        let expected = read_test(&mut vm, "(1 2 3 10 11 12)");
        assert_vals(&vm, expected, result);

        exec_runtime_error(&mut vm, "(let ([a b c] '()) nil)");
        exec_runtime_error(&mut vm, "(let ([a b c] []) nil)");
        exec_runtime_error(&mut vm, "(let ([a b c] '(1)) nil)");
        exec_runtime_error(&mut vm, "(let ([a b c] [1]) nil)");
        exec_runtime_error(&mut vm, "(let ([a b c] '(1 2)) nil)");
        exec_runtime_error(&mut vm, "(let ([a b c] [1 2]) nil)");

        exec_runtime_error(&mut vm, "(let ({a :a, b :b, c :c} {}) nil)");
        exec_runtime_error(&mut vm, "(let ({a :a, b :b, c :c} {:a 1}) nil)");
        exec_runtime_error(&mut vm, "(let ({a :a, b :b, c :c} {:a 1, :b 2}) nil)");
        exec_runtime_error(&mut vm, "(let ({a :a, b :b, c :c} {:a 1, :c 3}) nil)");
        exec_runtime_error(&mut vm, "(let ({a :a, b :b, c :c} {:b 2, :c 3}) nil)");

        exec_runtime_error(
            &mut vm,
            "(let ([x {a :a, b :b, c :c} y] [10 {:b 2, :c 3} 11]) nil)",
        );
        exec_runtime_error(
            &mut vm,
            "(let ([x {a :a, b :b, c :c} y] [10 {:a 1 :b 2, :c 3}]) nil)",
        );

        let result = exec(&mut vm, "(let ({a 1, b 0, c 2} [1 2 3]) (list a b c))");
        let expected = read_test(&mut vm, "(2 1 3)");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let ({a 1, b 0, c 2} '(1 2 3)) (list a b c))");
        let expected = read_test(&mut vm, "(2 1 3)");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let ({a 1, b 0, c 2} (list 1 2 3)) (list a b c))");
        let expected = read_test(&mut vm, "(2 1 3)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let ({a :a, b :b, c :c} [:a 1, :b 2, :c 3]) `(~a ~b ~c))",
        );
        let expected = read_test(&mut vm, "(1 2 3)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let ({a :a, b :b, c :c} '(:a 1, :b 2, :c 3)) `(~a ~b ~c))",
        );
        let expected = read_test(&mut vm, "(1 2 3)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let ({a :a, b :b, c :c} (list :a 1, :b 2, :c 3)) `(~a ~b ~c))",
        );
        let expected = read_test(&mut vm, "(1 2 3)");
        assert_vals(&vm, expected, result);
    }

    #[test]
    fn test_let_shadow() {
        let mut vm = Vm::new();
        vm.set_global("prn", Value::Builtin(CallFunc { func: prn }));
        vm.set_global("dasm", Value::Builtin(CallFunc { func: dasm }));
        vm.set_global("make-hash", Value::Builtin(CallFunc { func: make_hash }));

        let result = exec(
            &mut vm,
            "(let (a1 10, b1 20, c1 30, a (- a1 9), b (- b1 18), c (- c1 27)) `(~a ~b ~c))",
        );
        let expected = read_test(&mut vm, "(1 2 3)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let (a 10, b 20, c 30, a (- a 9), b (- b 18), c (- c 27)) `(~a ~b ~c))",
        );
        let expected = read_test(&mut vm, "(1 2 3)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let (a 1, b 2, c 3, {a :a, b :b, c :c} (list :a a, :b b, :c c)) `(~a ~b ~c))",
        );
        let expected = read_test(&mut vm, "(1 2 3)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let (a1 10, b1 11, c1 12)(let (a 1, b 2, c 3, {a :a, b :b, c :c} (list :a a, :b b, :c c)) `(~a ~b ~c)))",
        );
        let expected = read_test(&mut vm, "(1 2 3)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let (a 10, b 2, c 0)(let (a (- a 9), b b, c (+ c 3)) `(~a ~b ~c)))",
        );
        let expected = read_test(&mut vm, "(1 2 3)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let (a 10, b 11, c 12)(let (a 1, b 2, c 3, [a b c] (list a, b, c)) `(~a ~b ~c)))",
        );
        let expected = read_test(&mut vm, "(1 2 3)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let (a 10, b1 11, c1 12)(let (a 1, b 2, c 3, {a :a, b :b, c :c} (list :a a, :b b, :c c)) `(~a ~b ~c)))",
        );
        let expected = read_test(&mut vm, "(1 2 3)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(let (a 10, b 11, c 12)(let (a 1, b 2, c 3, {a :a, b :b, c :c} (make-hash :a a, :b b, :c c)) `(~a ~b ~c)))",
        );
        let expected = read_test(&mut vm, "(1 2 3)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def fnx (fn () (let (a 10, b 11, c 12)(let (a 1, b 2, c 3, {a :a, b :b, c :c} (make-hash :a a, :b b, :c c)) `(~a ~b ~c)))))(fnx))",
        );
        let expected = read_test(&mut vm, "(1 2 3)");
        assert_vals(&vm, expected, result);
    }
}
