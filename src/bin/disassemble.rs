//use std::iter::Iterator;
//use std::borrow::Borrow;

use slvm::opcodes::*;
//use slvm::value::*;
use slvm::chunk::*;
use slvm::error::*;

fn main() -> Result<(), VMError> {
    let mut chunk = Chunk::with_file("no_file", 1);
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
    chunk.disassemble_chunk()?;
    Ok(())
}
