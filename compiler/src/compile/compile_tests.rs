#[cfg(test)]
mod tests {
    use super::super::*;
    use crate::test_utils::{assert_vals, exec, read_test};
    use builtins::print::{dasm, prn};
    use slvm::Vm;

    #[test]
    fn test_def_set() {
        let mut vm = Vm::new();
        vm.set_global("prn", Value::Builtin(CallFunc { func: prn }));
        let result = exec(&mut vm, "(do (def x 3) x)");
        let expected = read_test(&mut vm, "3");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(do (def x 3) (set! x 1) x)");
        let expected = read_test(&mut vm, "1");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(do (def x 3) (set! x \"xxx\") x)");
        let expected = read_test(&mut vm, "\"xxx\"");
        assert_vals(&vm, expected, result);
    }

    #[test]
    fn test_if() {
        let mut vm = Vm::new();
        let result = exec(&mut vm, "(if #t 1 2)");
        let expected = read_test(&mut vm, "1");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(if #f 1 2)");
        let expected = read_test(&mut vm, "2");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(if () 1 2)");
        let expected = read_test(&mut vm, "2");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(if nil 1 2)");
        let expected = read_test(&mut vm, "2");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(if #f 1 2 3)");
        let expected = read_test(&mut vm, "3");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(if #f 1 #f 3 4)");
        let expected = read_test(&mut vm, "4");
        assert_vals(&vm, expected, result);
    }

    #[test]
    fn test_fn() {
        let mut vm = Vm::new();
        vm.set_global("prn", Value::Builtin(CallFunc { func: prn }));
        vm.set_global("dasm", Value::Builtin(CallFunc { func: dasm }));
        let result = exec(&mut vm, "((fn () 1))");
        let expected = read_test(&mut vm, "1");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "((fn (x) x) 2)");
        let expected = read_test(&mut vm, "2");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(do (def fnx (fn (x) x)) (fnx 2))");
        let expected = read_test(&mut vm, "2");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(do (def fnx (fn (% x := 3) x)) (fnx 2))");
        let expected = read_test(&mut vm, "2");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(do (def fnx (fn (% x := 3) x)) (fnx))");
        let expected = read_test(&mut vm, "3");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(do (def fnx (fn (% x := (list 1 2 3)) x)) (fnx))");
        let expected = read_test(&mut vm, "(1 2 3)");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(do (def fnx (fn (% x) x)) (fnx))");
        let expected = read_test(&mut vm, "nil");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def fnx (fn (a b % x := `(~a ~b)) x)) (fnx 3 5))",
        );
        //let result = exec(&mut vm, "(do (def fnx (fn (a b % x := (list a b)) x)) (fnx 3 5))");
        let expected = read_test(&mut vm, "(3 5)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def fnx (fn (a b [x y]) `(~a ~b ~x ~y))) (fnx 1 2 '(3 4)))",
        );
        let expected = read_test(&mut vm, "(1 2 3 4)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def fnx (fn (a b [x y % z := 10]) `(~a ~b ~x ~y ~z))) (fnx 1 2 '(3 4)))",
        );
        let expected = read_test(&mut vm, "(1 2 3 4 10)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def fnx (fn (a b [x [y y2] % z := 10]) `(~a ~b ~x ~y ~y2 ~z))) (dasm fnx) (fnx 1 2 '(3 [4 5])))",
        );
        let expected = read_test(&mut vm, "(1 2 3 4 5 10)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            //"(do (def fnx (fn (a b [x [y y2] % z := 10] % [d d2] := [20 21]) `(~a ~b ~x ~y ~y2 ~z ~d ~d2))) (dasm fnx) (fnx 1 2 '(3 [4 5])))",
            "(do (def fnx (fn (a b [x [y y2] % z := 10] [d d2]) `(~a ~b ~x ~y ~y2 ~z ~d ~d2))) (dasm fnx) (fnx 1 2 '(3 [4 5]) [23 24]))",
        );
        let expected = read_test(&mut vm, "(1 2 3 4 5 10 23 24)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def fnx (fn (a b [x [y y2] % z := 10] % [d d2] := [20 21]) `(~a ~b ~x ~y ~y2 ~z ~d ~d2))) (dasm fnx) (fnx 1 2 '(3 [4 5])))",
        );
        let expected = read_test(&mut vm, "(1 2 3 4 5 10 20 21)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def fnx (fn (a b & {k1 :key1, k2 :key2}) `(~a ~b ~k1 ~k2))) (fnx 1 2, :key2 5, :key1 6))",
        );
        let expected = read_test(&mut vm, "(1 2 6 5)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def fnx (fn (a b & {k1 :key1, k2 :key2, :or {:key2 55}}) `(~a ~b ~k1 ~k2))) (fnx 1 2, :key1 6))",
        );
        let expected = read_test(&mut vm, "(1 2 6 55)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def fnx (fn (a b & {k1 :key1, k2 :key2, :or {:key1 66}}) `(~a ~b ~k1 ~k2))) (fnx 1 2, :key2 5))",
        );
        let expected = read_test(&mut vm, "(1 2 66 5)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def fnx (fn (a b & {k1 :key1, k2 :key2, :or {:key1 66, :key2 55}}) `(~a ~b ~k1 ~k2))) (dasm fnx)(fnx 1 2))",
        );
        let expected = read_test(&mut vm, "(1 2 66 55)");
        assert_vals(&vm, expected, result);
    }
}
