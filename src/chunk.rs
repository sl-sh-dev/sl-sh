use std::cmp::Ordering;

use crate::error::*;
use crate::heap::*;
use crate::interner::Interned;
use crate::opcodes::*;
use crate::value::*;
use crate::vm::*;

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

macro_rules! disassemble_operand {
    ($code:expr, $register:expr, $wide:expr) => {{
        if $register {
            if $wide {
                print!("R({:#06x})", decode_u16_enum!($code)?);
            } else {
                print!("R({:#04x})", decode_u8_enum!($code)?);
            }
        } else {
            if $wide {
                print!("K({:#06x})", decode_u16_enum!($code)?);
            } else {
                print!("K({:#04x})", decode_u8_enum!($code)?);
            }
        }
    }};
}

macro_rules! disassemble_immediate {
    ($code:expr, $wide:expr) => {{
        if $wide {
            print!("{:#06x}", decode_u16_enum!($code)?);
        } else {
            print!("{:#04x}", decode_u8_enum!($code)?);
        }
    }};
}

macro_rules! disassemble_immediate_big {
    ($code:expr, $wide:expr) => {{
        if $wide {
            print!("{:#010x}", decode_u32_enum!($code)?);
        } else {
            print!("{:#06x}", decode_u16_enum!($code)?);
        }
    }};
}

#[derive(Clone, Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub file_name: &'static str,
    start_line: u32,
    last_line: u32,
    line_numbers: Vec<u8>,
    pub constants: Vec<Value>,
    pub namespace: Option<Interned>,
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
            namespace: None,
        }
    }

    pub fn with_namespace(file_name: &'static str, start_line: u32, namespace: Interned) -> Self {
        Chunk {
            code: Vec::new(),
            file_name,
            start_line,
            last_line: start_line,
            line_numbers: Vec::new(),
            constants: Vec::new(),
            namespace: Some(namespace),
        }
    }

    fn encode_operand(&mut self, op: u16, wide: bool) {
        if wide {
            self.code.push(((op & 0xFF00) >> 8) as u8);
        }
        self.code.push((op & 0x00FF) as u8);
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
        for (i, c) in self.constants.iter().enumerate() {
            if *c == value {
                return i;
            }
        }
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn encode0(&mut self, op_code: OpCode, line_number: u32) -> VMResult<()> {
        self.encode_line_number(1, line_number)?;
        self.code.push(op_code);
        Ok(())
    }

    pub fn encode1(&mut self, opcode: OpCode, op1: u16, line_number: u32) -> VMResult<()> {
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
        line_number: u32,
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

    pub fn encode_refi(&mut self, reg: u16, global: u32, line_number: u32) -> VMResult<()> {
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
        line_number: u32,
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

    pub fn encode3(
        &mut self,
        opcode: OpCode,
        op1: u16,
        op2: u16,
        op3: u16,
        line_number: u32,
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

    fn disassemble_instruction<I>(chunk: I, op: OpCode) -> VMResult<()>
    where
        I: IntoIterator<Item = (usize, u8)>,
    {
        let mut code = chunk.into_iter();
        let wide = (op & 0x80) != 0;
        match op & 0x7F {
            NOP => {
                println!("NOP");
                Ok(())
            }
            HALT => {
                println!("HALT");
                Ok(())
            }
            RET => {
                println!("RET");
                Ok(())
            }
            SRET => {
                print!("SRET   \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            WIDE => {
                println!("WIDE");
                Ok(())
            }
            MOV => {
                print!("MOV    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            SET => {
                print!("SET    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            CONST => {
                print!("CONST  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, false, wide);
                println!();
                Ok(())
            }
            REF => {
                print!("REF    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                print!("G[");
                disassemble_operand!(code, true, wide);
                print!("]");
                println!();
                Ok(())
            }
            DEF => {
                print!("DEF    \t");
                print!("G[");
                disassemble_operand!(code, true, wide);
                print!("]");
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            DEFV => {
                print!("DEFV   \t");
                print!("G[");
                disassemble_operand!(code, true, wide);
                print!("]");
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            REFI => {
                print!("REFI   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                print!("G[");
                disassemble_immediate_big!(code, wide);
                print!("]");
                println!();
                Ok(())
            }
            CALL => {
                print!("CALL   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            CALLG => {
                print!("CALLG  \t");
                print!("G[");
                disassemble_immediate_big!(code, wide);
                print!("]");
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            TCALL => {
                print!("CALL   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            JMP => {
                print!("JMP    \t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            JMPF => {
                print!("JMPF   \t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            JMPB => {
                print!("JMPB   \t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            JMPFT => {
                print!("JMPFT  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            JMPBT => {
                print!("JMPBT  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            JMPFF => {
                print!("JMPFF  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            JMPBF => {
                print!("JMPBF  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            JMP_T => {
                print!("JMP_T  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            JMP_F => {
                print!("JMP_F  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            JMPEQ => {
                print!("JMPEQ  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            JMPLT => {
                print!("JMPLT  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            JMPGT => {
                print!("JMPGT  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            ADD => {
                print!("ADD    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            SUB => {
                print!("SUB    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            MUL => {
                print!("MUL    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            DIV => {
                print!("DIV    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            ADDM => {
                print!("ADDM   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            SUBM => {
                print!("SUBM   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            MULM => {
                print!("MULM   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            DIVM => {
                print!("DIVM   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            INC => {
                print!("INC    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            DEC => {
                print!("DEC    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(())
            }
            CONS => {
                print!("CONS   \t");
                //R(A) = conscell(R(B), R(C))
                print!("R(");
                disassemble_operand!(code, true, wide);
                print!(")");
                print!("\tconscell(R(");
                disassemble_operand!(code, true, wide);
                print!("), R(");
                disassemble_operand!(code, true, wide);
                print!(")");
                println!();
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
            VECMK => {
                print!("VECMK  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            VECELS => {
                print!("VECELS \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            VECPSH => {
                print!("VECPSH \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            VECPOP => {
                print!("VECPOP \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            VECNTH => {
                print!("VECNTH \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            VECSTH => {
                print!("VECSTH \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            VECMKD => {
                print!("VECMKD \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(())
            }
            _ => Err(VMError::new_chunk(format!("ERROR: unknow opcode {}", op))),
        }
    }

    pub fn disassemble_chunk(&self, vm: &Vm) -> VMResult<()> {
        println!("CONSTANTS:");
        for (i, v) in self.constants.iter().enumerate() {
            println!("{}: {}", i, v.display_value(vm));
            if let Value::Reference(h) = v {
                if let Object::Lambda(l) = vm.get(*h) {
                    println!("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx");
                    l.disassemble_chunk(vm)?;
                    println!("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx");
                }
            }
        }
        println!();
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
    fn test_encode0() {
        let mut chunk = Chunk::new("no_file", 0);
        chunk.encode0(RET, 1).unwrap();
        chunk.encode0(CAR, 1).unwrap();
        chunk.encode0(RET, 1).unwrap();
        let mut code = chunk.code.iter();

        assert!(*code.next().unwrap() == RET);
        assert!(*code.next().unwrap() == CAR);
        assert!(*code.next().unwrap() == RET);
        assert!(code.next().is_none());
    }

    #[test]
    fn test_encode1() {
        let mut chunk = Chunk::new("no_file", 0);
        chunk.encode1(CAR, 0, 1).unwrap();
        chunk.encode1(CAR, 128, 1).unwrap();
        chunk.encode1(CAR, 255, 1).unwrap();
        chunk.encode1(CAR, 256, 1).unwrap();
        chunk.encode1(CAR, 256, 1).unwrap();
        chunk.encode1(CAR, u16::MAX, 1).unwrap();
        let mut code = chunk.code.iter();

        assert!(*code.next().unwrap() == CAR);
        assert!(*code.next().unwrap() == 0);

        assert!(*code.next().unwrap() == CAR);
        assert!(*code.next().unwrap() == 128);

        assert!(*code.next().unwrap() == CAR);
        assert!(*code.next().unwrap() == 255);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CAR);
        assert!(decode_u16!(code).unwrap() == 256);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CAR);
        assert!(decode_u16!(code).unwrap() == 256);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CAR);
        assert!(decode_u16!(code).unwrap() == u16::MAX);
    }

    #[test]
    fn test_encode2() {
        let mut chunk = Chunk::new("no_file", 0);
        chunk.encode2(MOV, 0, 0, 1).unwrap();
        chunk.encode2(MOV, 128, 128, 1).unwrap();
        chunk.encode2(MOV, 255, 255, 1).unwrap();
        chunk.encode2(MOV, 256, 256, 1).unwrap();
        chunk.encode2(MOV, 2, 256, 1).unwrap();
        chunk.encode2(MOV, 256, 1, 1).unwrap();
        chunk.encode2(MOV, 257, 257, 1).unwrap();
        chunk.encode2(MOV, u16::MAX, u16::MAX, 1).unwrap();
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
        assert!(decode_u16!(code).unwrap() == 256);
        assert!(decode_u16!(code).unwrap() == 256);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == MOV);
        assert!(decode_u16!(code).unwrap() == 2);
        assert!(decode_u16!(code).unwrap() == 256);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == MOV);
        assert!(decode_u16!(code).unwrap() == 256);
        assert!(decode_u16!(code).unwrap() == 1);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == MOV);
        assert!(decode_u16!(code).unwrap() == 257);
        assert!(decode_u16!(code).unwrap() == 257);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == MOV);
        assert!(decode_u16!(code).unwrap() == u16::MAX);
        assert!(decode_u16!(code).unwrap() == u16::MAX);
    }

    #[test]
    fn test_encode3() {
        let mut chunk = Chunk::new("no_file", 0);
        chunk.encode3(CONS, 0, 0, 0, 1).unwrap();
        chunk.encode3(CONS, 128, 128, 128, 1).unwrap();
        chunk.encode3(CONS, 255, 255, 255, 1).unwrap();
        chunk.encode3(CONS, 256, 256, 256, 1).unwrap();
        chunk.encode3(CONS, 2, 256, 256, 1).unwrap();
        chunk.encode3(CONS, 256, 1, 1, 1).unwrap();
        chunk.encode3(CONS, 257, 257, 257, 1).unwrap();
        chunk
            .encode3(CONS, u16::MAX, u16::MAX, u16::MAX, 1)
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
        assert!(decode_u16!(code).unwrap() == 256);
        assert!(decode_u16!(code).unwrap() == 256);
        assert!(decode_u16!(code).unwrap() == 256);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CONS);
        assert!(decode_u16!(code).unwrap() == 2);
        assert!(decode_u16!(code).unwrap() == 256);
        assert!(decode_u16!(code).unwrap() == 256);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CONS);
        assert!(decode_u16!(code).unwrap() == 256);
        assert!(decode_u16!(code).unwrap() == 1);
        assert!(decode_u16!(code).unwrap() == 1);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CONS);
        assert!(decode_u16!(code).unwrap() == 257);
        assert!(decode_u16!(code).unwrap() == 257);
        assert!(decode_u16!(code).unwrap() == 257);

        assert!(*code.next().unwrap() == WIDE);
        assert!(*code.next().unwrap() == CONS);
        assert!(decode_u16!(code).unwrap() == u16::MAX);
        assert!(decode_u16!(code).unwrap() == u16::MAX);
        assert!(decode_u16!(code).unwrap() == u16::MAX);
    }

    #[test]
    fn test_line_numbers() {
        let mut chunk = Chunk::new("no_file", 1);
        chunk.encode2(MOV, 1, 2, 1).unwrap();
        chunk.encode2(MOV, 1, 2, 2).unwrap();
        chunk.encode2(MOV, 1, 2, 3).unwrap();
        chunk.encode2(MOV, 1, 2, 4).unwrap();
        chunk.encode2(MOV, 1, 2, 4).unwrap();
        chunk.encode2(MOV, 1, 2, 30).unwrap();
        chunk.encode2(MOV, 1, 2, 200).unwrap();
        assert!(chunk.offset_to_line(0).unwrap() == 1);
        assert!(chunk.offset_to_line(1).unwrap() == 1);
        assert!(chunk.offset_to_line(2).unwrap() == 1);

        assert!(chunk.offset_to_line(3).unwrap() == 2);
        assert!(chunk.offset_to_line(4).unwrap() == 2);
        assert!(chunk.offset_to_line(5).unwrap() == 2);

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

        assert!(chunk.offset_to_line(18).unwrap() == 200);
        assert!(chunk.offset_to_line(19).unwrap() == 200);
        assert!(chunk.offset_to_line(20).unwrap() == 200);

        assert!(chunk.line_to_offset(1).unwrap() == 0);
        assert!(chunk.line_to_offset(2).unwrap() == 3);
        assert!(chunk.line_to_offset(3).unwrap() == 6);
        assert!(chunk.line_to_offset(4).unwrap() == 9);
        assert!(chunk.line_to_offset(30).unwrap() == 15);
        assert!(chunk.line_to_offset(200).unwrap() == 18);
        assert!(chunk.line_to_offset(15).is_none());
        assert!(chunk.line_to_offset(0).is_none());
        assert!(chunk.line_to_offset(201).is_none());
        assert!(chunk.line_to_offset(101).is_none());
        assert!(chunk.encode0(RET, 1).is_err());
    }
}
