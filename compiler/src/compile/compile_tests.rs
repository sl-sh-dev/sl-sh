#[cfg(test)]
mod tests {
    use super::super::*;
    use crate::test_utils::{assert_vals, exec, read_test};
    use builtins::print::{dasm, prn};
    use slvm::Vm;

    #[test]
    fn test_def_set() {
        let mut vm = Vm::new();
        let mut env = CompileEnvironment::new(&mut vm);
        env.set_global_builtin("prn", prn);
        let result = exec(&mut env, "(do (def x 3) x)");
        let expected = read_test(env.vm_mut(), "3");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "(do (def x 3) (set! x 1) x)");
        let expected = read_test(env.vm_mut(), "1");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "(do (def x 3) (set! x \"xxx\") x)");
        let expected = read_test(env.vm_mut(), "\"xxx\"");
        assert_vals(env.vm(), expected, result);
    }

    #[test]
    fn test_if() {
        let mut vm = Vm::new();
        let mut env = CompileEnvironment::new(&mut vm);
        let result = exec(&mut env, "(if #t 1 2)");
        let expected = read_test(env.vm_mut(), "1");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "(if #f 1 2)");
        let expected = read_test(env.vm_mut(), "2");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "(if () 1 2)");
        let expected = read_test(env.vm_mut(), "2");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "(if nil 1 2)");
        let expected = read_test(env.vm_mut(), "2");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "(if #f 1 2 3)");
        let expected = read_test(env.vm_mut(), "3");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "(if #f 1 #f 3 4)");
        let expected = read_test(env.vm_mut(), "4");
        assert_vals(env.vm(), expected, result);
    }

    #[test]
    fn test_fn() {
        let mut vm = Vm::new();
        let mut env = CompileEnvironment::new(&mut vm);
        env.set_global_builtin("prn", prn);
        env.set_global_builtin("dasm", dasm);
        let result = exec(&mut env, "((fn () 1))");
        let expected = read_test(env.vm_mut(), "1");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "((fn (x) x) 2)");
        let expected = read_test(env.vm_mut(), "2");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "(do (def fnx (fn (x) x)) (fnx 2))");
        let expected = read_test(env.vm_mut(), "2");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "(do (def fnx (fn (% x := 3) x)) (fnx 2))");
        let expected = read_test(env.vm_mut(), "2");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "(do (def fnx (fn (% x := 3) x)) (fnx))");
        let expected = read_test(env.vm_mut(), "3");
        assert_vals(env.vm(), expected, result);

        let result = exec(
            &mut env,
            "(do (def fnx (fn (% x := (list 1 2 3)) x)) (fnx))",
        );
        let expected = read_test(env.vm_mut(), "(1 2 3)");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "(do (def fnx (fn (% x) x)) (fnx))");
        let expected = read_test(env.vm_mut(), "nil");
        assert_vals(env.vm(), expected, result);

        let result = exec(
            &mut env,
            "(do (def fnx (fn (a b % x := `(~a ~b)) x)) (fnx 3 5))",
        );
        //let result = exec(&mut vm, "(do (def fnx (fn (a b % x := (list a b)) x)) (fnx 3 5))");
        let expected = read_test(env.vm_mut(), "(3 5)");
        assert_vals(env.vm(), expected, result);

        let result = exec(
            &mut env,
            "(do (def fnx (fn (a b [x y]) `(~a ~b ~x ~y))) (fnx 1 2 '(3 4)))",
        );
        let expected = read_test(env.vm_mut(), "(1 2 3 4)");
        assert_vals(env.vm(), expected, result);

        let result = exec(
            &mut env,
            "(do (def fnx (fn (a b [x y % z := 10]) `(~a ~b ~x ~y ~z))) (fnx 1 2 '(3 4)))",
        );
        let expected = read_test(env.vm_mut(), "(1 2 3 4 10)");
        assert_vals(env.vm(), expected, result);

        let result = exec(
            &mut env,
            "(do (def fnx (fn (a b [x [y y2] % z := 10]) `(~a ~b ~x ~y ~y2 ~z))) (dasm fnx) (fnx 1 2 '(3 [4 5])))",
        );
        let expected = read_test(env.vm_mut(), "(1 2 3 4 5 10)");
        assert_vals(env.vm(), expected, result);

        let result = exec(
            &mut env,
            "(do (def fnx (fn (a b [x [y y2] % z := 10] [d d2]) `(~a ~b ~x ~y ~y2 ~z ~d ~d2))) (dasm fnx) (fnx 1 2 '(3 [4 5]) [23 24]))",
        );
        let expected = read_test(env.vm_mut(), "(1 2 3 4 5 10 23 24)");
        assert_vals(env.vm(), expected, result);

        let result = exec(
            &mut env,
            "(do (def fnx (fn (a b [x [y y2] % z := 10] % [d d2] := [20 21]) `(~a ~b ~x ~y ~y2 ~z ~d ~d2))) (fnx 1 2 '(3 [4 5])))",
        );
        let expected = read_test(env.vm_mut(), "(1 2 3 4 5 10 20 21)");
        assert_vals(env.vm(), expected, result);

        let result = exec(
            &mut env,
            "(do (def fnx (fn (a b & {k1 :key1, k2 :key2}) `(~a ~b ~k1 ~k2))) (fnx 1 2, :key2 5, :key1 6))",
        );
        let expected = read_test(env.vm_mut(), "(1 2 6 5)");
        assert_vals(env.vm(), expected, result);

        let result = exec(
            &mut env,
            "(do (def fnx (fn (a b & {k1 :key1, k2 :key2, :or {:key2 55}}) `(~a ~b ~k1 ~k2))) (fnx 1 2, :key1 6))",
        );
        let expected = read_test(env.vm_mut(), "(1 2 6 55)");
        assert_vals(env.vm(), expected, result);

        let result = exec(
            &mut env,
            "(do (def fnx (fn (a b & {k1 :key1, k2 :key2, :or {:key1 66}}) `(~a ~b ~k1 ~k2))) (fnx 1 2, :key2 5))",
        );
        let expected = read_test(env.vm_mut(), "(1 2 66 5)");
        assert_vals(env.vm(), expected, result);

        let result = exec(
            &mut env,
            "(do (def fnx (fn (a b & {k1 :key1, k2 :key2, :or {:key1 66, :key2 55}}) `(~a ~b ~k1 ~k2))) (dasm fnx)(fnx 1 2))",
        );
        let expected = read_test(env.vm_mut(), "(1 2 66 55)");
        assert_vals(env.vm(), expected, result);
    }

    #[test]
    fn test_captures() {
        let mut vm = Vm::new();
        let mut env = CompileEnvironment::new(&mut vm);
        env.set_global_builtin("prn", prn);
        env.set_global_builtin("dasm", dasm);

        let result = exec(&mut env, "(do ((fn (a b c) ((fn () (let (x 10, y 20, z 30) (set! x 11) (def fnx (fn () (set! a 5)(set! b 6)`(~a ~b ~c ~x ~y ~z))))(let (s 51, t 52, u 53)(def fny (fn () (list s t u)))))))1 2 3)(fnx))");
        let expected = read_test(env.vm_mut(), "(5 6 3 11 20 30)");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "(do ((fn (a b c) ((fn () (let (x 10, y 20, z 30) (set! x 11) (def fnx (fn () (set! a 5)(set! b 6)(list a b c x y z))))(let (s 51, t 52, u 53)(def fny (fn () (list s t u)))))))1 2 3)(fnx))");
        let expected = read_test(env.vm_mut(), "(5 6 3 11 20 30)");
        assert_vals(env.vm(), expected, result);

        //let result = crate::test_utils::exec_with_dump(&mut vm, "((fn (a b c) ((fn () (let (x 10, y 20, z 30) (set! x 11) (def fnx (fn () (set! a 5)(set! b 6)(set! c 7)`(~a ~b ~c ~x ~y ~z))))(let (s 51, t 52, u 53)(def fny (fn () `(~s ~t ~u)))))))1 2 3)(fnx)(fny)");
        let result = exec(&mut env, "((fn (a b c) ((fn () (let (x 10, y 20, z 30) (set! x 11) (def fnx (fn () (set! a 5)(set! b 6)(set! c 7)`(~a ~b ~c ~x ~y ~z))))(let (s 51, t 52, u 53)(def fny (fn () `(~s ~t ~u)))))))1 2 3)(fnx)(fny)");
        let expected = read_test(env.vm_mut(), "(51 52 53)");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "(def x 1)((fn (x) (def fnx (fn () x))) 2)(fnx)");
        let expected = read_test(env.vm_mut(), "2");
        assert_vals(env.vm(), expected, result);

        let result = exec(
            &mut env,
            "(def x 1)((fn () (let (x 3) (def fnx (fn () x)))))(fnx)",
        );
        let expected = read_test(env.vm_mut(), "3");
        assert_vals(env.vm(), expected, result);
    }

    #[test]
    fn test_on_error() {
        let mut vm = Vm::new();
        let mut env = CompileEnvironment::new(&mut vm);
        env.set_global_builtin("prn", prn);
        env.set_global_builtin("dasm", dasm);

        let def_macro = r#"
        (def defmacro
            (macro (name args & body)
        `(def ~name (macro ~args ~@body))))"#;

        let get_error = r#"
        (defmacro get-error (& body)
        `(let (old-error (on-error nil))
            (defer (on-error old-error))
            (call/cc (fn (k) (on-error (fn (key val) (k (cons key val))))
            (cons :ok (do ~@body))))))"#;
        exec(&mut env, def_macro);
        exec(&mut env, get_error);

        let result = exec(&mut env, "(let (x 1, y 5) (cons :ok (+ x y)))");
        let expected = read_test(env.vm_mut(), "(:ok . 6)");
        assert_vals(env.vm(), expected, result);

        let result = exec(&mut env, "(get-error (let (x 1, y 5) (+ x y)))");
        let expected = read_test(env.vm_mut(), "(:ok . 6)");
        assert_vals(env.vm(), expected, result);

        let result = exec(
            &mut env,
            "(get-error (let (x 1, y 5) (err :test \"error\")(+ x y)))",
        );
        let expected = read_test(env.vm_mut(), "(:test . \"error\")");
        assert_vals(env.vm(), expected, result);
    }
}
