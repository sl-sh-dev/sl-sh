//use std::iter::Iterator;
//use std::borrow::Borrow;

use slvm::chunk::*;
use slvm::error::*;
use slvm::opcodes::*;
use slvm::value::*;

fn main() -> Result<(), VMError> {
    let mut chunk = Chunk::new("no_file", 1, Namespace::new_ref("disassemble"));
    chunk.push_simple(RET, 1)?;
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
    chunk.push_u16(LIST, 10, 13)?;
    chunk.disassemble_chunk()?;
    Ok(())
}
