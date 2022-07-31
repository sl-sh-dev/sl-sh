#[cfg(test)]
mod tests {
    use super::super::*;
    use crate::print::pretty_value;
    use crate::{ReadError, Reader};
    use slvm::RET;
    use std::sync::Arc;

    fn read_test(vm: &mut Vm, text: &'static str) -> Value {
        let reader = Reader::from_string(text.to_string(), vm, "", 1, 0);
        let exps: Vec<Value> = reader.collect::<Result<Vec<Value>, ReadError>>().unwrap();
        // Don't exit early without unpausing....
        vm.pause_gc();
        let res = if exps.len() == 1 {
            exps[0]
        } else {
            vm.alloc_vector_ro(exps)
        };
        vm.unpause_gc();
        res
    }

    fn exec(vm: &mut Vm, input: &'static str) -> Value {
        let exp = read_test(vm, input);
        let mut env = CompileEnvironment::new(vm);
        let mut state = CompileState::new(env.vm_mut());
        compile(&mut env, &mut state, exp, 0).unwrap();
        state.chunk.encode0(RET, Some(1)).unwrap();
        vm.execute(Arc::new(state.chunk)).unwrap();
        vm.stack()[0]
    }

    fn assert_vals(vm: &Vm, val1: Value, val2: Value) {
        let res = vm
            .is_equal_pair(val1, val2)
            .unwrap_or(Value::False)
            .is_true();
        if !res {
            println!(
                "Value {} != {}",
                val1.display_value(vm),
                val2.display_value(vm)
            );
            println!("Debug {:?} / {:?}", val1, val2);
        }
        assert!(res);
    }

    fn prn(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
        for v in registers {
            print!("{}", pretty_value(vm, *v));
        }
        println!();
        Ok(Value::Nil)
    }

    #[test]
    fn test_def_let_set() {
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

        let result = exec(&mut vm, "(do (def x 3) (let ((x 10)) (set! x 1)) x)");
        let expected = read_test(&mut vm, "3");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let ((x 10)) x)");
        let expected = read_test(&mut vm, "10");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let ((x 10)) (set! x 5) x)");
        let expected = read_test(&mut vm, "5");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let* ((x 10)(y (+ x 10))) (set! x 5) `(,x ,y))");
        let expected = read_test(&mut vm, "(5 20)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def x 5) (let* ((x 10)(y (+ x 10))) (set! x 15) `(,x ,y)))",
        );
        let expected = read_test(&mut vm, "(15 20)");
        assert_vals(&vm, expected, result);

        let result = exec(
            &mut vm,
            "(do (def x 5) (let ((x 10)(y (+ x 10))) (set! x 15) `(,x ,y)))",
        );
        let expected = read_test(&mut vm, "(15 15)");
        assert_vals(&vm, expected, result);

        let result = exec(&mut vm, "(let* ((fnx (fn (x) (if (= x 0) #t (fny (- x 1)))))(fny (fn (y) (if (= y 0) #t (fnx (- y 1)))))) (fnx 10))");
        let expected = read_test(&mut vm, "#t");
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
