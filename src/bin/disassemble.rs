//use std::iter::Iterator;
//use std::borrow::Borrow;

use slvm::chunk::*;
use slvm::error::*;
use slvm::interner::*;
use slvm::opcodes::*;
use slvm::value::*;

fn main() -> Result<(), VMError> {
    let mut interner = Interner::with_capacity(8);
    let mut chunk = Chunk::with_namespace(
        "no_file",
        1,
        Namespace::new_ref(interner.intern("disassemble")),
    );
    println!("Value size: {}", std::mem::size_of::<Value>());
    println!("usize: {}", std::mem::size_of::<usize>());
    println!("opt usize: {}", std::mem::size_of::<Option<usize>>());
    println!("&st usize: {}", std::mem::size_of::<&'static str>());
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
    chunk.encode2(STORE, 10, 15, 1)?;
    chunk.encode2(STORE_K, 10, 15, 1)?;
    chunk.encode2(STORE_K, 0x8fff, 0x9fff, 1)?;
    chunk.encode2(REF, 1, 2, 2)?;
    chunk.encode2(REF_K, 1, 2, 2)?;
    chunk.encode3(REFNS, 1, 2, 3, 2)?;
    chunk.encode3(REFNS_K, 1, 2, 3, 2)?;
    chunk.encode2(BIND, 1, 2, 4)?;
    chunk.encode2(BIND_K, 1, 2, 4)?;
    chunk.encode3(BINDNS, 1, 2, 3, 4)?;
    chunk.encode3(BINDNS_K, 1, 2, 3, 4)?;
    chunk.disassemble_chunk()?;
    Ok(())
}
