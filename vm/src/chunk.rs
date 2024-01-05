use std::cmp::Ordering;

use crate::error::*;
use crate::opcodes::*;
use crate::{Value, Interned};

#[macro_use]
pub mod disassemble;

#[derive(Clone, Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub file_name: &'static str,
    start_line: u32,
    last_line: u32,
    line_numbers: Vec<u8>,
    pub constants: Vec<Value>,
    pub jump_table: Vec<u32>,
    pub captures: Option<Vec<u32>>,
    // Registers holding input (arguments and closed over values) plus 1 for the result.
    pub input_regs: usize,
    // Number of registers needed beyond input_regs for computations.
    pub extra_regs: usize,
    pub args: u16,
    pub opt_args: u16,
    pub rest: bool,

    pub dbg_args: Option<Vec<Interned>>,
}

impl Chunk {
    pub fn new(file_name: &'static str, start_line: u32) -> Self {
        Chunk {
            code: Vec::new(),
            file_name,
            start_line,
            last_line: start_line,
            line_numbers: Vec::new(),
            constants: Vec::new(),
            jump_table: Vec::new(),
            captures: None,
            input_regs: 0,
            extra_regs: 0,
            args: 0,
            opt_args: 0,
            rest: false,
            dbg_args: None,
        }
    }

    fn encode_operand(&mut self, op: u16, wide: bool) {
        if wide {
            self.code.push(((op & 0xFF00) >> 8) as u8);
        }
        self.code.push((op & 0x00FF) as u8);
    }

    fn encode_line_number(&mut self, offsets: u8, line_number: Option<u32>) -> VMResult<()> {
        let line_number = if let Some(ln) = line_number {
            ln
        } else {
            self.last_line
        };
        match line_number.cmp(&self.last_line) {
            Ordering::Equal => {
                if let Some(line) = self.line_numbers.pop() {
                    if (line & 0x40) == 0 {
                        let current_offsets: u16 = (line & 0x3f) as u16;
                        if current_offsets + offsets as u16 > 0x3f {
                            self.line_numbers.push(0x3f);
                            self.line_numbers
                                .push((offsets - (0x3f - current_offsets) as u8) | (line & 0x80));
                        } else {
                            self.line_numbers
                                .push((current_offsets as u8 + offsets) | (line & 0x80));
                        }
                    } else {
                        self.line_numbers.push(line);
                        self.line_numbers.push(offsets);
                    }
                } else {
                    self.line_numbers.push(offsets);
                }
                Ok(())
            }
            Ordering::Less => Err(VMError::new_chunk("Line numbers can not go backwards!")),
            Ordering::Greater => {
                let mut delta = line_number - self.last_line;
                while delta > 1 {
                    if delta > 0x3f {
                        self.line_numbers.push(0x7f); // 0x3f plus the 3rd bit of the high nibble.
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
            if (o & 0x80) > 0 {
                line += 1;
            }
            if offset < current {
                return Some(line);
            }
        }
        None
    }

    pub fn line_to_offset(&self, line: u32) -> Option<usize> {
        if line > self.last_line {
            return None;
        }
        let mut current_line = self.start_line;
        let mut offset: usize = 0;
        for o in &self.line_numbers {
            if (o & 0x80) > 0 {
                current_line += 1;
            }
            if current_line == line {
                return Some(offset);
            }
            if (o & 0x40) > 0 {
                current_line += (o & 0x3f) as u32;
            } else {
                offset += (o & 0x3f) as usize;
            }
        }
        None
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        for (i, c) in self.constants.iter().enumerate() {
            if *c == value {
                return i;
            }
        }
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn add_jump(&mut self, offset: u32) -> usize {
        /*for (i, c) in self.jump_table.iter().enumerate() {
            if *c == offset {
                return i;
            }
        }*/
        self.jump_table.push(offset);
        self.jump_table.len() - 1
    }

    pub fn update_jump(&mut self, jmp: usize, offset: u32) {
        self.jump_table[jmp] = offset;
    }

    pub fn encode0(&mut self, op_code: OpCode, line_number: Option<u32>) -> VMResult<()> {
        self.encode_line_number(1, line_number)?;
        self.code.push(op_code);
        Ok(())
    }

    pub fn encode1(&mut self, opcode: OpCode, op1: u16, line_number: Option<u32>) -> VMResult<()> {
        let mut bytes: u8 = 2;
        let mut wide = false;
        if op1 > u8::MAX as u16 {
            wide = true;
            bytes = 3;
            self.encode_line_number(1, line_number)?;
            self.code.push(WIDE);
        }

        self.encode_line_number(bytes, line_number)?;
        self.code.push(opcode);
        self.encode_operand(op1, wide);

        Ok(())
    }

    pub fn encode2(
        &mut self,
        opcode: OpCode,
        op1: u16,
        op2: u16,
        line_number: Option<u32>,
    ) -> VMResult<()> {
        let mut bytes: u8 = 3;
        let mut wide = false;
        if op1 > u8::MAX as u16 || op2 > u8::MAX as u16 {
            wide = true;
            bytes = 5;
            self.encode_line_number(1, line_number)?;
            self.code.push(WIDE);
        }

        self.encode_line_number(bytes, line_number)?;
        self.code.push(opcode);
        self.encode_operand(op1, wide);
        self.encode_operand(op2, wide);

        Ok(())
    }

    pub fn encode_def(
        &mut self,
        reg: u16,
        global: u32,
        line_number: Option<u32>,
        is_defv: bool,
    ) -> VMResult<()> {
        let mut bytes: u8 = 4;
        let mut wide = false;
        if reg > u8::MAX as u16 || global > u16::MAX as u32 {
            wide = true;
            bytes = 7;
            self.encode_line_number(1, line_number)?;
            self.code.push(WIDE);
        }

        self.encode_line_number(bytes, line_number)?;
        if is_defv {
            self.code.push(DEFV);
        } else {
            self.code.push(DEF);
        }
        self.encode_operand(reg, wide);
        if wide {
            self.code.push(((global & 0xFF00_0000) >> 24) as u8);
            self.code.push(((global & 0x00FF_0000) >> 16) as u8);
        }
        self.code.push(((global & 0x0000_FF00) >> 8) as u8);
        self.code.push((global & 0x0000_00FF) as u8);

        Ok(())
    }

    pub fn encode_refi(&mut self, reg: u16, global: u32, line_number: Option<u32>) -> VMResult<()> {
        let mut bytes: u8 = 4;
        let mut wide = false;
        if reg > u8::MAX as u16 || global > u16::MAX as u32 {
            wide = true;
            bytes = 7;
            self.encode_line_number(1, line_number)?;
            self.code.push(WIDE);
        }

        self.encode_line_number(bytes, line_number)?;
        self.code.push(REFI);
        self.encode_operand(reg, wide);
        if wide {
            self.code.push(((global & 0xFF00_0000) >> 24) as u8);
            self.code.push(((global & 0x00FF_0000) >> 16) as u8);
        }
        self.code.push(((global & 0x0000_FF00) >> 8) as u8);
        self.code.push((global & 0x0000_00FF) as u8);

        Ok(())
    }

    pub fn encode_callg(
        &mut self,
        global: u32,
        num_args: u16,
        first_reg: u16,
        line_number: Option<u32>,
    ) -> VMResult<()> {
        let mut bytes: u8 = 5;
        let mut wide = false;
        if num_args > u8::MAX as u16 || first_reg > u8::MAX as u16 || global > u16::MAX as u32 {
            wide = true;
            bytes = 9;
            self.encode_line_number(1, line_number)?;
            self.code.push(WIDE);
        }

        self.encode_line_number(bytes, line_number)?;
        self.code.push(CALLG);
        if wide {
            self.code.push(((global & 0xFF00_0000) >> 24) as u8);
            self.code.push(((global & 0x00FF_0000) >> 16) as u8);
        }
        self.code.push(((global & 0x0000_FF00) >> 8) as u8);
        self.code.push((global & 0x0000_00FF) as u8);
        self.encode_operand(num_args, wide);
        self.encode_operand(first_reg, wide);

        Ok(())
    }

    pub fn encode_tcallg(
        &mut self,
        global: u32,
        num_args: u16,
        line_number: Option<u32>,
    ) -> VMResult<()> {
        let mut bytes: u8 = 4;
        let mut wide = false;
        if num_args > u8::MAX as u16 || global > u16::MAX as u32 {
            wide = true;
            bytes = 7;
            self.encode_line_number(1, line_number)?;
            self.code.push(WIDE);
        }

        self.encode_line_number(bytes, line_number)?;
        self.code.push(TCALLG);
        if wide {
            self.code.push(((global & 0xFF00_0000) >> 24) as u8);
            self.code.push(((global & 0x00FF_0000) >> 16) as u8);
        }
        self.code.push(((global & 0x0000_FF00) >> 8) as u8);
        self.code.push((global & 0x0000_00FF) as u8);
        self.encode_operand(num_args, wide);

        Ok(())
    }

    pub fn encode3(
        &mut self,
        opcode: OpCode,
        op1: u16,
        op2: u16,
        op3: u16,
        line_number: Option<u32>,
    ) -> VMResult<()> {
        let mut bytes: u8 = 4;
        let mut wide = false;
        if op1 > u8::MAX as u16 || op2 > u8::MAX as u16 || op3 > u8::MAX as u16 {
            wide = true;
            bytes = 7;
            self.encode_line_number(1, line_number)?;
            self.code.push(WIDE);
        }

        self.encode_line_number(bytes, line_number)?;
        self.code.push(opcode);
        self.encode_operand(op1, wide);
        self.encode_operand(op2, wide);
        self.encode_operand(op3, wide);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode0() {
        let mut chunk = Chunk::new("no_file", 0);
        chunk.encode0(RET, Some(1)).unwrap();
        chunk.encode0(CAR, None).unwrap();
        chunk.encode0(RET, None).unwrap();
        let mut code = chunk.code.iter();

        assert!(*code.next().unwrap() == RET);
        assert!(*code.next().unwrap() == CAR);
        assert!(*code.next().unwrap() == RET);
        assert!(code.next().is_none());
    }

    #[test]
    fn test_encode1() {
        let mut chunk = Chunk::new("no_file", 0);
        chunk.encode1(CAR, 0, Some(1)).unwrap();
        chunk.encode1(CAR, 128, None).unwrap();
        chunk.encode1(CAR, 255, None).unwrap();
        chunk.encode1(CAR, 256, None).unwrap();
        chunk.encode1(CAR, 256, None).unwrap();
        chunk.encode1(CAR, u16::MAX, None).unwrap();
        let mut code = chunk.code.iter();

        assert!(*code.next().unwrap() == CAR);
        assert!(*code.next().unwrap() == 0);

        assert!(*code.next().unwrap() == CAR);
        assert!(*code.next().unwrap() == 128);

        assert!(*code.next().unwrap() == CAR);
        assert!(*code.next().unwrap() == 255);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CAR);
        assert!(decode_chunk_u16!(code).unwrap() == 256);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CAR);
        assert!(decode_chunk_u16!(code).unwrap() == 256);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CAR);
        assert!(decode_chunk_u16!(code).unwrap() == u16::MAX);
    }

    #[test]
    fn test_encode2() {
        let mut chunk = Chunk::new("no_file", 0);
        chunk.encode2(MOV, 0, 0, Some(1)).unwrap();
        chunk.encode2(MOV, 128, 128, None).unwrap();
        chunk.encode2(MOV, 255, 255, None).unwrap();
        chunk.encode2(MOV, 256, 256, None).unwrap();
        chunk.encode2(MOV, 2, 256, None).unwrap();
        chunk.encode2(MOV, 256, 1, None).unwrap();
        chunk.encode2(MOV, 257, 257, None).unwrap();
        chunk.encode2(MOV, u16::MAX, u16::MAX, None).unwrap();
        let mut code = chunk.code.iter();

        assert!(*code.next().unwrap() == MOV);
        assert!(*code.next().unwrap() == 0);
        assert!(*code.next().unwrap() == 0);

        assert!(*code.next().unwrap() == MOV);
        assert!(*code.next().unwrap() == 128);
        assert!(*code.next().unwrap() == 128);

        assert!(*code.next().unwrap() == MOV);
        assert!(*code.next().unwrap() == 255);
        assert!(*code.next().unwrap() == 255);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == MOV);
        assert!(decode_chunk_u16!(code).unwrap() == 256);
        assert!(decode_chunk_u16!(code).unwrap() == 256);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == MOV);
        assert!(decode_chunk_u16!(code).unwrap() == 2);
        assert!(decode_chunk_u16!(code).unwrap() == 256);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == MOV);
        assert!(decode_chunk_u16!(code).unwrap() == 256);
        assert!(decode_chunk_u16!(code).unwrap() == 1);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == MOV);
        assert!(decode_chunk_u16!(code).unwrap() == 257);
        assert!(decode_chunk_u16!(code).unwrap() == 257);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == MOV);
        assert!(decode_chunk_u16!(code).unwrap() == u16::MAX);
        assert!(decode_chunk_u16!(code).unwrap() == u16::MAX);
    }

    #[test]
    fn test_encode3() {
        let mut chunk = Chunk::new("no_file", 0);
        chunk.encode3(CONS, 0, 0, 0, Some(1)).unwrap();
        chunk.encode3(CONS, 128, 128, 128, None).unwrap();
        chunk.encode3(CONS, 255, 255, 255, None).unwrap();
        chunk.encode3(CONS, 256, 256, 256, None).unwrap();
        chunk.encode3(CONS, 2, 256, 256, None).unwrap();
        chunk.encode3(CONS, 256, 1, 1, None).unwrap();
        chunk.encode3(CONS, 257, 257, 257, None).unwrap();
        chunk
            .encode3(CONS, u16::MAX, u16::MAX, u16::MAX, Some(1))
            .unwrap();
        let mut code = chunk.code.iter();

        assert!(*code.next().unwrap() == CONS);
        assert!(*code.next().unwrap() == 0);
        assert!(*code.next().unwrap() == 0);
        assert!(*code.next().unwrap() == 0);

        assert!(*code.next().unwrap() == CONS);
        assert!(*code.next().unwrap() == 128);
        assert!(*code.next().unwrap() == 128);
        assert!(*code.next().unwrap() == 128);

        assert!(*code.next().unwrap() == CONS);
        assert!(*code.next().unwrap() == 255);
        assert!(*code.next().unwrap() == 255);
        assert!(*code.next().unwrap() == 255);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CONS);
        assert!(decode_chunk_u16!(code).unwrap() == 256);
        assert!(decode_chunk_u16!(code).unwrap() == 256);
        assert!(decode_chunk_u16!(code).unwrap() == 256);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CONS);
        assert!(decode_chunk_u16!(code).unwrap() == 2);
        assert!(decode_chunk_u16!(code).unwrap() == 256);
        assert!(decode_chunk_u16!(code).unwrap() == 256);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CONS);
        assert!(decode_chunk_u16!(code).unwrap() == 256);
        assert!(decode_chunk_u16!(code).unwrap() == 1);
        assert!(decode_chunk_u16!(code).unwrap() == 1);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CONS);
        assert!(decode_chunk_u16!(code).unwrap() == 257);
        assert!(decode_chunk_u16!(code).unwrap() == 257);
        assert!(decode_chunk_u16!(code).unwrap() == 257);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CONS);
        assert!(decode_chunk_u16!(code).unwrap() == u16::MAX);
        assert!(decode_chunk_u16!(code).unwrap() == u16::MAX);
        assert!(decode_chunk_u16!(code).unwrap() == u16::MAX);
    }

    #[test]
    fn test_line_numbers() {
        let mut chunk = Chunk::new("no_file", 1);
        chunk.encode2(MOV, 1, 2, Some(1)).unwrap();
        chunk.encode2(MOV, 1, 2, Some(2)).unwrap();
        chunk.encode2(MOV, 1, 2, Some(3)).unwrap();
        chunk.encode2(MOV, 1, 2, Some(4)).unwrap();
        chunk.encode2(MOV, 1, 2, Some(4)).unwrap();
        chunk.encode2(MOV, 1, 2, Some(30)).unwrap();
        chunk.encode2(MOV, 1, 2, Some(200)).unwrap();
        assert!(chunk.offset_to_line(0).unwrap() == 1);
        assert!(chunk.offset_to_line(1).unwrap() == 1);
        assert!(chunk.offset_to_line(2).unwrap() == 1);

        assert_eq!(chunk.offset_to_line(3).unwrap(), 2);
        assert_eq!(chunk.offset_to_line(4).unwrap(), 2);
        assert_eq!(chunk.offset_to_line(5).unwrap(), 2);

        assert!(chunk.offset_to_line(6).unwrap() == 3);
        assert!(chunk.offset_to_line(7).unwrap() == 3);
        assert!(chunk.offset_to_line(8).unwrap() == 3);

        assert!(chunk.offset_to_line(9).unwrap() == 4);
        assert!(chunk.offset_to_line(10).unwrap() == 4);
        assert!(chunk.offset_to_line(11).unwrap() == 4);

        assert!(chunk.offset_to_line(12).unwrap() == 4);
        assert!(chunk.offset_to_line(13).unwrap() == 4);
        assert!(chunk.offset_to_line(14).unwrap() == 4);

        assert!(chunk.offset_to_line(15).unwrap() == 30);
        assert!(chunk.offset_to_line(16).unwrap() == 30);
        assert!(chunk.offset_to_line(17).unwrap() == 30);

        assert_eq!(chunk.offset_to_line(18).unwrap(), 200);
        assert_eq!(chunk.offset_to_line(19).unwrap(), 200);
        assert_eq!(chunk.offset_to_line(20).unwrap(), 200);

        assert_eq!(chunk.line_to_offset(1).unwrap(), 0);
        assert_eq!(chunk.line_to_offset(2).unwrap(), 3);
        assert_eq!(chunk.line_to_offset(3).unwrap(), 6);
        assert_eq!(chunk.line_to_offset(4).unwrap(), 9);
        assert_eq!(chunk.line_to_offset(30).unwrap(), 15);
        assert_eq!(chunk.line_to_offset(200).unwrap(), 18);
        assert!(chunk.line_to_offset(15).is_none());
        assert!(chunk.line_to_offset(0).is_none());
        assert!(chunk.line_to_offset(201).is_none());
        assert!(chunk.line_to_offset(101).is_none());
        assert!(chunk.encode0(RET, Some(1)).is_err());
    }
}
