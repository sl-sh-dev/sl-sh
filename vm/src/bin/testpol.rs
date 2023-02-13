use std::sync::Arc;

use slvm::chunk::*;
use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;
use slvm::vm::*;

fn main() -> Result<(), VMError> {
    // algorithm from http://dan.corlan.net/bench.html
    // Do a lot of loops and simple math and to see how we stack up.
    /*
    (defn eval-pol (n x)
      (let ((su 0.0) (mu 10.0) (pu 0.0)
            (pol (make-vec 100 0.0)))
        (dotimes-i i n
          (do
            (set! su 0.0)
            (dotimes-i j 100
               (do
                 (set! mu (/ (+ mu 2.0) 2.0))
                 (vec-set! pol j mu)))
            (dotimes-i j 100
              (set! su (+ (vec-nth pol j) (* su x))))
            (set! pu (+ pu su))))
        (println pu)))
             */
    let mut vm = Vm::new();
    //vm.pause_gc();
    let mut chunk = Chunk::new("no_file", 1);
    chunk.extra_regs = 150;
    let n = chunk.add_constant(Value::Int32(500_000)) as u16;
    let x = chunk.add_constant(vm.alloc_f64(0.2)) as u16;
    let su = chunk.add_constant(vm.alloc_f64(0.0)) as u16;
    let mu = chunk.add_constant(vm.alloc_f64(10.0)) as u16;
    let pu = chunk.add_constant(vm.alloc_f64(0.0)) as u16;
    let zero = chunk.add_constant(Value::Int32(0)) as u16;
    //let five_hundred = chunk.add_constant(Value::Int(500)) as u16;
    let zerof = chunk.add_constant(vm.alloc_f64(0.0)) as u16;
    let twof = chunk.add_constant(vm.alloc_f64(2.0)) as u16;
    let hundred = chunk.add_constant(Value::Int32(100)) as u16;
    //let hundred = chunk.add_constant(Value::Int(600)) as u16;
    let one = chunk.add_constant(Value::Int32(1)) as u16;
    chunk.encode2(CONST, 1, n, Some(1))?;
    chunk.encode2(CONST, 2, x, None)?;
    chunk.encode2(CONST, 3, su, None)?;
    chunk.encode2(CONST, 4, mu, None)?;
    chunk.encode2(CONST, 5, pu, None)?;
    chunk.encode2(CONST, 6, zero, None)?; // i
    chunk.encode2(CONST, 7, zero, None)?; // j
                                          //chunk.encode2(CONST, 7, five_hundred, None)?; // j
    chunk.encode2(CONST, 8, twof, None)?; // 2.0
    chunk.encode2(CONST, 100, hundred, None)?;
    chunk.encode2(CONST, 101, one, None)?;
    chunk.encode2(CONST, 103, zerof, None)?;

    chunk.encode3(VECMKD, 10, 100, 103, None)?; // pols
                                                //chunk.encode2(VECELS, 10, 100, None)?;
                                                // loop i .. n
    chunk.add_jump(chunk.code.len() as u32);
    chunk.encode2(CONST, 3, zerof, None)?;
    chunk.encode2(CONST, 7, zero, None)?; // j
                                          //chunk.encode2(CONST, 7, five_hundred, None)?; // j
                                          // loop j .. 100
                                          // (set! mu (/ (+ mu 2.0) 2.0))
    chunk.add_jump(chunk.code.len() as u32);
    chunk.encode2(ADD, 4, 8, None)?;
    chunk.encode2(DIV, 4, 8, None)?;
    // (vec-set! pol j mu)))
    chunk.encode3(VECSTH, 10, 4, 7, None)?;
    //chunk.encode2(MOVI, 7, 4, None)?;

    chunk.encode2(INC, 7, 1, None)?;
    chunk.encode3(JMPLT, 7, 100, 1, None)?;
    //chunk.encode_jump_offset(-19)?;
    //chunk.encode_jump_offset(-18)?;

    chunk.encode2(CONST, 7, zero, None)?; // j
                                          //chunk.encode2(CONST, 7, five_hundred, None)?; // j
                                          // (dotimes-i j 100 (j2)
                                          //   (set! su (+ (vec-nth pol j) (* su x))))
    chunk.add_jump(chunk.code.len() as u32);
    chunk.encode2(MUL, 3, 2, None)?;
    chunk.encode3(VECNTH, 10, 51, 7, None)?;
    //chunk.encode2(MOVII, 51, 7, None)?;
    chunk.encode2(ADD, 3, 51, None)?;

    chunk.encode2(INC, 7, 1, None)?;
    chunk.encode3(JMPLT, 7, 100, 2, None)?;
    //chunk.encode_jump_offset(-19)?;
    //chunk.encode_jump_offset(-18)?;
    // (set! pu (+ pu su))))
    chunk.encode2(ADD, 5, 3, None)?;

    chunk.encode2(INC, 6, 1, None)?;
    chunk.encode3(JMPLT, 6, 1, 0, None)?;
    //chunk.encode_jump_offset(-59)?;
    //chunk.encode_jump_offset(-57)?;

    chunk.encode0(RET, None)?;

    //chunk.disassemble_chunk(&vm, 1)?;
    //assert!(false);

    //chunk.disassemble_chunk(&vm, 0);
    let chunk = Arc::new(chunk);
    vm.execute(chunk)?;
    let result = vm.get_stack(5).get_float(&vm)?;
    println!("{result}");

    Ok(())
}
