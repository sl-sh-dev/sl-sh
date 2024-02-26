//use bencher::{black_box, Bencher};

//fn benchmark(bench: &mut Bencher) {
//    bench.iter(|| 1 + 1);
//}
//    #[bench]
//    fn benchmark(bench: &mut Bencher) {
//            let mut env = &mut new_slosh_vm();
//            let code = r#"((fn (n x)
//                (let (su 0.0
//                              mu 10.0
//                                      pu 0.0
//                                              pol (make-vec 100 0.0))
//                     (dotimes-i i n
//                              (set! su 0.0)
//                                      (dotimes-i j 100
//                                                    (set! mu (/ (+ mu 2.0) 2.0))
//                                                                 (set! pol.~j mu))
//                                              (dotimes-i j 100
//                                                         (set! su (+ pol.~j (* su x))))
//                                                          (set! pu (+ pu su)))
//                         pu)) 100 0.01)
//                         "#;
//            set_builtins(&mut env);
//            _ = exec(&mut env, "(load \"init.slosh\")");
//            let v = exec(&mut env, code);
//            match v {
//                        Value::Float(f) => println!("return: {}", f.0),
//                        _ => println!("return: {v:?}"),
//                    }
//            println!("return: {v:?}");
//        }
//
//
