//use std::iter::Iterator;
//use std::borrow::Borrow;

use slvm::chunk::*;
use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;
use slvm::vm::*;
use std::sync::Arc;

fn main() -> Result<(), VMError> {
    let mut chunk = Chunk::new("no_file", 1);
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
    println!("Max opcode: {}", MAX_OP_CODE);
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
    chunk.encode2(REF, 1, 2, Some(2))?;
    chunk.encode3(CONS, 1, 2, 3, None)?;
    chunk.encode2(DEF, 1, 2, Some(4))?;
    chunk.encode0(JMP, Some(5))?;
    chunk.encode_jump_offset(1)?;
    chunk.encode1(JMPT, 1, Some(5))?;
    chunk.encode_jump_offset(-12)?;
    let vm = Vm::new();
    chunk.disassemble_chunk(&vm, 0)?;
    Ok(())
}
