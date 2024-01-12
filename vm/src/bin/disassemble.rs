use slvm::opcodes::*;
use slvm::{Chunk, FxHasher, Handle, VMError, Value, Vm};
use std::collections::HashSet;
use std::hash::Hasher;
use std::sync::Arc;

fn main() -> Result<(), VMError> {
    let mut chunk = Chunk::new("no_file", 1);
    println!("Handle size: {}", std::mem::size_of::<Handle>());
    println!("Value size: {}", std::mem::size_of::<Value>());
    println!("usize: {}", std::mem::size_of::<usize>());
    //println!("Object size: {}", std::mem::size_of::<slvm::heap::Object>());
    println!("Chunk size: {}", std::mem::size_of::<slvm::chunk::Chunk>());
    println!(
        "CallFrame size: {}",
        std::mem::size_of::<slvm::heap::CallFrame>()
    );
    println!("Vec<Value> size: {}", std::mem::size_of::<Vec<Value>>());
    println!(
        "Cow size: {}",
        std::mem::size_of::<std::borrow::Cow<'static, str>>()
    );
    println!("Arc<usize> size: {}", std::mem::size_of::<Arc<usize>>());
    println!("Max opcode: {MAX_OP_CODE}");
    /*    chunk.push_simple(RET, 1)?;
    chunk.push_const(0, 2)?;
    chunk.push_const(128, 2)?;
    chunk.push_const(255, 3)?;
    chunk.push_const(256, 4)?;
    chunk.push_const(257, 4)?;
    chunk.push_const(u16::MAX as usize, 5)?;
    chunk.push_const((u16::MAX as usize) + 1, 5)?;
    chunk.push_const(u32::MAX as usize, 10)?;
    chunk.push_simple(ADD, 11)?;
    chunk.push_const(0, 11)?;
    chunk.push_simple(SUB, 11)?;
    chunk.push_simple(CONS, 12)?;
    chunk.push_simple(CAR, 12)?;
    chunk.push_u16(LIST, 10, 13)?;*/
    chunk.encode2(MOV, 10, 15, Some(1))?;
    chunk.encode2(CONST, 10, 15, Some(1))?;
    chunk.encode2(CONST, 0x8fff, 0x9fff, Some(1))?;
    chunk.encode2(REFI, 1, 2, Some(2))?;
    chunk.encode3(CONS, 1, 2, 3, None)?;
    chunk.encode2(DEF, 1, 2, Some(4))?;
    let jmp_idx = chunk.add_jump(1);
    chunk.encode1(JMP, jmp_idx as u16, Some(5))?;
    let jmp_idx = chunk.add_jump(chunk.code.len() as u32 - 12);
    chunk.encode2(JMPT, 1, jmp_idx as u16, Some(5))?;
    let vm = Vm::new();
    chunk.disassemble_chunk(&vm, 0)?;

    let mut set = HashSet::new();
    for i in 0..1_000_000 {
        let mut fx = FxHasher::default();
        fx.write_u64(i);
        let finish = fx.finish();
        if set.contains(&finish) {
            println!("Collision: {finish:#016x}");
        } else {
            set.insert(finish);
        }
        //println!("{}: {:#016x}", i, finish)
    }
    Ok(())
}
