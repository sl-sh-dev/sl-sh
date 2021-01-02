use std::cmp::Ordering;

use crate::error::*;
use crate::opcodes::*;
use crate::value::*;

macro_rules! decode_u8_enum {
    ($code:expr) => {{
        if let Some((_, val)) = $code.next() {
            Ok(val)
        } else {
            Err(VMError::new_chunk(
                "Error decoding a u8 from chunk stream, missing operand.",
            ))
        }
    }};
}

#[macro_export]
macro_rules! decode_u16 {
    ($code:expr) => {{
        if let Some(idx1) = $code.next() {
            if let Some(idx2) = $code.next() {
                Ok(((*idx1 as u16) << 8) | (*idx2 as u16))
            } else {
                Err(VMError::new_chunk(
                    "Error decoding a u16 from chunk stream.",
                ))
            }
        } else {
            Err(VMError::new_chunk(
                "Error decoding a u16 from chunk stream.",
            ))
        }
    }};
}

macro_rules! decode_u16_enum {
    ($code:expr) => {{
        if let Some((_, idx1)) = $code.next() {
            if let Some((_, idx2)) = $code.next() {
                Ok(((idx1 as u16) << 8) | (idx2 as u16))
            } else {
                Err(VMError::new_chunk(
                    "Error decoding a u16 from chunk stream.",
                ))
            }
        } else {
            Err(VMError::new_chunk(
                "Error decoding a u16 from chunk stream.",
            ))
        }
    }};
}

#[macro_export]
macro_rules! decode_u32 {
    ($code:expr) => {{
        if let Some(idx1) = $code.next() {
            if let Some(idx2) = $code.next() {
                if let Some(idx3) = $code.next() {
                    if let Some(idx4) = $code.next() {
                        Ok(((*idx1 as u32) << 24)
                            | ((*idx2 as u32) << 16)
                            | ((*idx3 as u32) << 8)
                            | (*idx4 as u32))
                    } else {
                        Err(VMError::new_chunk(
                            "Error decoding a u32 from chunk stream.",
                        ))
                    }
                } else {
                    Err(VMError::new_chunk(
                        "Error decoding a u32 from chunk stream.",
                    ))
                }
            } else {
                Err(VMError::new_chunk(
                    "Error decoding a u32 from chunk stream.",
                ))
            }
        } else {
            Err(VMError::new_chunk(
                "Error decoding a u32 from chunk stream.",
            ))
        }
    }};
}

macro_rules! decode_u32_enum {
    ($code:expr) => {{
        if let Some((_, idx1)) = $code.next() {
            if let Some((_, idx2)) = $code.next() {
                if let Some((_, idx3)) = $code.next() {
                    if let Some((_, idx4)) = $code.next() {
                        Ok(((idx1 as u32) << 24)
                            | ((idx2 as u32) << 16)
                            | ((idx3 as u32) << 8)
                            | (idx4 as u32))
                    } else {
                        Err(VMError::new_chunk(
                            "Error decoding a u32 from chunk stream.",
                        ))
                    }
                } else {
                    Err(VMError::new_chunk(
                        "Error decoding a u32 from chunk stream.",
                    ))
                }
            } else {
                Err(VMError::new_chunk(
                    "Error decoding a u32 from chunk stream.",
                ))
            }
        } else {
            Err(VMError::new_chunk(
                "Error decoding a u32 from chunk stream.",
            ))
        }
    }};
}

pub struct Chunk {
    pub code: Vec<u8>,
    pub file_name: &'static str,
    start_line: u32,
    last_line: u32,
    line_numbers: Vec<u8>,
    pub constants: Vec<Value>,
    pub namespace: NamespaceRef,
}

impl Chunk {
    pub fn new(file_name: &'static str, start_line: u32, namespace: NamespaceRef) -> Self {
        Chunk {
            code: Vec::new(),
            file_name,
            start_line,
            last_line: start_line,
            line_numbers: Vec::new(),
            constants: Vec::new(),
            namespace,
        }
    }

    fn encode_line_number(&mut self, offsets: u8, line_number: u32) -> VMResult<()> {
        match line_number.cmp(&self.last_line) {
            Ordering::Equal => {
                if let Some(line) = self.line_numbers.pop() {
                    let current_offsets: u16 = (line & 0x3f) as u16;
                    if current_offsets + offsets as u16 > 0x3f {
                        self.line_numbers.push(0x3f);
                        self.line_numbers
                            .push((offsets - (0x3f - current_offsets) as u8) | 0x80);
                    } else {
                        self.line_numbers
                            .push((current_offsets as u8 + offsets) | 0x80);
                    }
                } else {
                    self.line_numbers.push(offsets | 0x80);
                }
                Ok(())
            }
            Ordering::Less => Err(VMError::new_chunk("Line numbers can not go backwards!")),
            Ordering::Greater => {
                let mut delta = line_number - self.last_line;
                while delta > 1 {
                    if delta > 0x3f {
                        self.line_numbers.push(0x7f); // 0x3f plus the 3rd bit of the high byte.
                        delta -= 0x3f;
                    } else {
                        self.line_numbers.push((delta - 1) as u8 | 0x40);
                        delta = 0;
                    }
                }
                self.last_line = line_number;
                self.line_numbers.push(offsets | 0x80);
                Ok(())
            }
        }
    }

    pub fn offset_to_line(&self, offset: usize) -> Option<u32> {
        let mut line = self.start_line;
        let mut current: usize = 0;
        for o in &self.line_numbers {
            if (o & 0x40) > 0 {
                line += (o & 0x3f) as u32;
            } else {
                current += (o & 0x3f) as usize;
            }
            if offset < current {
                return Some(line);
            }
            if (o & 0x80) > 0 {
                line += 1;
            }
        }
        None
    }

    pub fn line_to_offset(&self, line: u32) -> Option<usize> {
        let mut current_line = self.start_line;
        let mut offset: usize = 0;
        for o in &self.line_numbers {
            if current_line == line {
                return Some(offset);
            }
            if (o & 0x40) > 0 {
                current_line += (o & 0x3f) as u32;
            } else {
                offset += (o & 0x3f) as usize;
            }
            if (o & 0x80) > 0 {
                current_line += 1;
            }
        }
        None
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn push_simple(&mut self, op_code: OpCode, line_number: u32) -> VMResult<()> {
        self.encode_line_number(1, line_number)?;
        self.code.push(op_code);
        Ok(())
    }

    pub fn push_u16(&mut self, op_code: OpCode, operand: u16, line_number: u32) -> VMResult<()> {
        self.encode_line_number(3, line_number)?;
        self.code.push(op_code);
        self.code.push(((operand & 0xFF00) >> 8) as u8);
        self.code.push((operand & 0x00FF) as u8);
        Ok(())
    }

    pub fn push_const(&mut self, offset: usize, line_number: u32) -> VMResult<()> {
        if offset <= u8::MAX as usize {
            self.encode_line_number(2, line_number)?;
            self.code.push(CONST);
            self.code.push(offset as u8);
        } else if offset <= u16::MAX as usize {
            self.encode_line_number(3, line_number)?;
            self.code.push(CONST2);
            self.code.push(((offset & 0xFF00) >> 8) as u8);
            self.code.push((offset & 0x00FF) as u8);
        } else if offset <= u32::MAX as usize {
            self.encode_line_number(5, line_number)?;
            self.code.push(CONST4);
            self.code.push(((offset & 0xFF000000) >> 24) as u8);
            self.code.push(((offset & 0x00FF0000) >> 16) as u8);
            self.code.push(((offset & 0x0000FF00) >> 8) as u8);
            self.code.push((offset & 0x000000FF) as u8);
        } else {
            return Err(VMError::new_chunk(format!(
                "CONST ERROR: offset {:#018x} to large, max size is {:#010x}",
                offset,
                u32::MAX
            )));
        }
        Ok(())
    }

    pub fn push_load(&mut self, offset: usize, line_number: u32) -> VMResult<()> {
        if offset <= u8::MAX as usize {
            self.encode_line_number(2, line_number)?;
            self.code.push(LOAD);
            self.code.push(offset as u8);
        } else if offset <= u16::MAX as usize {
            self.encode_line_number(3, line_number)?;
            self.code.push(LOAD2);
            self.code.push(((offset & 0xFF00) >> 8) as u8);
            self.code.push((offset & 0x00FF) as u8);
        } else {
            return Err(VMError::new_chunk(format!(
                "LOAD ERROR: offset {:#018x} to large, max size is {:#06x}",
                offset,
                u16::MAX
            )));
        }
        Ok(())
    }

    pub fn push_store(&mut self, offset: usize, line_number: u32) -> VMResult<()> {
        if offset <= u8::MAX as usize {
            self.encode_line_number(2, line_number)?;
            self.code.push(STORE);
            self.code.push(offset as u8);
        } else if offset <= u16::MAX as usize {
            self.encode_line_number(3, line_number)?;
            self.code.push(STORE2);
            self.code.push(((offset & 0xFF00) >> 8) as u8);
            self.code.push((offset & 0x00FF) as u8);
        } else {
            return Err(VMError::new_chunk(format!(
                "STORE ERROR: offset {:#018x} to large, max size is {:#06x}",
                offset,
                u16::MAX
            )));
        }
        Ok(())
    }

    fn disassemble_instruction<I>(chunk: I, op: OpCode) -> VMResult<()>
    where
        I: IntoIterator<Item = (usize, u8)>,
    {
        let mut code = chunk.into_iter();
        match op {
            RET => {
                println!("RET");
                Ok(())
            }
            CONST => {
                print!("CONST   \t");
                println!("{:#04x} ", decode_u8_enum!(code)?);
                Ok(())
            }
            CONST2 => {
                print!("CONST2  \t");
                println!("{:#06x} ", decode_u16_enum!(code)?);
                Ok(())
            }
            CONST4 => {
                print!("CONST4  \t");
                println!("{:#010x} ", decode_u32_enum!(code)?);
                Ok(())
            }
            LOAD => {
                print!("LOAD    \t");
                println!("{:#04x} ", decode_u8_enum!(code)?);
                Ok(())
            }
            LOAD2 => {
                print!("LOAD2   \t");
                println!("{:#06x} ", decode_u16_enum!(code)?);
                Ok(())
            }
            POP => {
                println!("POP");
                Ok(())
            }
            STORE => {
                print!("STORE   \t");
                println!("{:#04x} ", decode_u8_enum!(code)?);
                Ok(())
            }
            STORE2 => {
                print!("STORE2  \t");
                println!("{:#06x} ", decode_u16_enum!(code)?);
                Ok(())
            }
            ADD => {
                println!("ADD");
                Ok(())
            }
            SUB => {
                println!("SUB");
                Ok(())
            }
            MUL => {
                println!("MUL");
                Ok(())
            }
            DIV => {
                println!("DIV");
                Ok(())
            }
            CONS => {
                println!("CONS");
                Ok(())
            }
            CAR => {
                println!("CAR");
                Ok(())
            }
            CDR => {
                println!("CDR");
                Ok(())
            }
            XAR => {
                println!("XAR");
                Ok(())
            }
            XDR => {
                println!("XDR");
                Ok(())
            }
            LIST => {
                print!("LIST    \t");
                println!("{:#06x} ", decode_u16_enum!(code)?);
                Ok(())
            }
            _ => Err(VMError::new_chunk(format!("ERROR: unknow opcode {}", op))),
        }
    }

    pub fn disassemble_chunk(&self) -> VMResult<()> {
        let mut code = self.code.iter().cloned().enumerate();
        let mut op = code.next();
        let mut last_line = 0;
        while let Some((idx, curr_op)) = op {
            print!("{:#010x} ", idx);
            if let Some(line_number) = self.offset_to_line(idx) {
                if last_line != line_number {
                    print!("{:>6} ", line_number);
                    last_line = line_number;
                } else {
                    print!("     | ");
                }
            } else {
                print!("     | ");
            }
            Chunk::disassemble_instruction(&mut code, curr_op)?;
            op = code.next();
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_const() {
        let mut chunk = Chunk::new("no_file", 0, Namespace::new_ref("test"));
        chunk.push_const(0, 2).unwrap();
        chunk.push_const(128, 2).unwrap();
        chunk.push_const(255, 3).unwrap();
        chunk.push_const(256, 4).unwrap();
        chunk.push_const(257, 4).unwrap();
        chunk.push_const(u16::MAX as usize, 5).unwrap();
        chunk.push_const((u16::MAX as usize) + 1, 5).unwrap();
        chunk.push_const(u32::MAX as usize, 10).unwrap();
        assert!(chunk.push_const((u32::MAX as usize) + 1, 10).is_err());
        let mut code = chunk.code.iter();
        assert!(*code.next().unwrap() == CONST);
        assert!(*code.next().unwrap() == 0);
        assert!(*code.next().unwrap() == CONST);
        assert!(*code.next().unwrap() == 128);
        assert!(*code.next().unwrap() == CONST);
        assert!(*code.next().unwrap() == 255);
        assert!(*code.next().unwrap() == CONST2);
        assert!(decode_u16!(code).unwrap() == 256);
        assert!(*code.next().unwrap() == CONST2);
        assert!(decode_u16!(code).unwrap() == 257);
        assert!(*code.next().unwrap() == CONST2);
        assert!(decode_u16!(code).unwrap() == u16::MAX);
        assert!(*code.next().unwrap() == CONST4);
        assert!(decode_u32!(code).unwrap() == (u16::MAX as u32) + 1);
        assert!(*code.next().unwrap() == CONST4);
        assert!(decode_u32!(code).unwrap() == u32::MAX);
    }

    #[test]
    fn test_line_numbers() {
        let mut chunk = Chunk::new("no_file", 1, Namespace::new_ref("test"));
        chunk.push_const(0, 1).unwrap();
        chunk.push_const(128, 2).unwrap();
        chunk.push_const(255, 3).unwrap();
        chunk.push_const(250, 4).unwrap();
        chunk.push_const(27, 4).unwrap();
        chunk.push_const(256, 30).unwrap();
        chunk.push_const(256, 200).unwrap();
        assert!(chunk.offset_to_line(0).unwrap() == 1);
        assert!(chunk.offset_to_line(1).unwrap() == 1);
        assert!(chunk.offset_to_line(2).unwrap() == 2);
        assert!(chunk.offset_to_line(3).unwrap() == 2);
        assert!(chunk.offset_to_line(4).unwrap() == 3);
        assert!(chunk.offset_to_line(5).unwrap() == 3);
        assert!(chunk.offset_to_line(6).unwrap() == 4);
        assert!(chunk.offset_to_line(7).unwrap() == 4);
        assert!(chunk.offset_to_line(8).unwrap() == 4);
        assert!(chunk.offset_to_line(9).unwrap() == 4);
        assert!(chunk.offset_to_line(10).unwrap() == 30);
        assert!(chunk.offset_to_line(11).unwrap() == 30);
        assert!(chunk.offset_to_line(12).unwrap() == 30);
        assert!(chunk.offset_to_line(13).unwrap() == 200);
        assert!(chunk.offset_to_line(14).unwrap() == 200);
        assert!(chunk.offset_to_line(15).unwrap() == 200);
        assert!(chunk.line_to_offset(1).unwrap() == 0);
        assert!(chunk.line_to_offset(2).unwrap() == 2);
        assert!(chunk.line_to_offset(3).unwrap() == 4);
        assert!(chunk.line_to_offset(4).unwrap() == 6);
        assert!(chunk.line_to_offset(30).unwrap() == 10);
        assert!(chunk.line_to_offset(200).unwrap() == 13);
        assert!(chunk.line_to_offset(15).is_none());
        assert!(chunk.line_to_offset(0).is_none());
        assert!(chunk.line_to_offset(201).is_none());
        assert!(chunk.line_to_offset(101).is_none());
        assert!(chunk.push_simple(RET, 1).is_err());
    }
}
