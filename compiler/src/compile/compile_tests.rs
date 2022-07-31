#[cfg(test)]
mod tests {
    use super::super::*;
    use crate::test_utils::{assert_vals, exec, prn, read_test};
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
        let result = exec(&mut vm, "((fn () 1))");
        let expected = read_test(&mut vm, "1");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "((fn (x) x) 2)");
        let expected = read_test(&mut vm, "2");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(do (def fnx (fn (x) x)) (fnx 2))");
        let expected = read_test(&mut vm, "2");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(do (def fnx (fn ((x 3)) x)) (fnx 2))");
        let expected = read_test(&mut vm, "2");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(do (def fnx (fn ((x 3)) x)) (fnx))");
        let expected = read_test(&mut vm, "3");
        assert_vals(&vm, expected, result);
    }
}
