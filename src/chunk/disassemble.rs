use crate::opcodes::*;
use crate::{Chunk, VMError, VMResult, Value, Vm};

#[macro_export]
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

#[cfg(test)]
#[macro_export]
macro_rules! decode_chunk_u16 {
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

#[macro_export]
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

impl Chunk {
    fn disassemble_instruction<I>(chunk: I, op: OpCode, wide: bool) -> VMResult<bool>
    where
        I: IntoIterator<Item = (usize, u8)>,
    {
        let mut code = chunk.into_iter();
        match op {
            NOP => {
                println!("NOP");
                Ok(false)
            }
            HALT => {
                println!("HALT");
                Ok(false)
            }
            RET => {
                println!("RET");
                Ok(false)
            }
            SRET => {
                print!("SRET   \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            WIDE => {
                println!("WIDE");
                Ok(true)
            }
            MOV => {
                print!("MOV    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            SET => {
                print!("SET    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            CONST => {
                print!("CONST  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, false, wide);
                println!();
                Ok(false)
            }
            REF => {
                print!("REF    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                print!("G[");
                disassemble_operand!(code, true, wide);
                print!("]");
                println!();
                Ok(false)
            }
            DEF => {
                print!("DEF    \t");
                print!("G[");
                disassemble_operand!(code, true, wide);
                print!("]");
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            DEFV => {
                print!("DEFV   \t");
                print!("G[");
                disassemble_operand!(code, true, wide);
                print!("]");
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REFI => {
                print!("REFI   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                print!("G[");
                disassemble_immediate_big!(code, wide);
                print!("]");
                println!();
                Ok(false)
            }
            CLRREG => {
                print!("CLRREG \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGT => {
                print!("REGT   \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGF => {
                print!("REGF   \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGN => {
                print!("REGN   \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGC => {
                print!("REGC   \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGB => {
                print!("REGB   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            REGI => {
                print!("REGI   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            REGU => {
                print!("REGU   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            CLOSE => {
                print!("CLOSE  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            BMOV => {
                print!("BMOV   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            CALL => {
                print!("CALL   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
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
                Ok(false)
            }
            TCALL => {
                print!("TCALL  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            TCALLG => {
                print!("TCALLG \t");
                print!("G[");
                disassemble_immediate_big!(code, wide);
                print!("]");
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            CALLM => {
                print!("CALLM  \t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            TCALLM => {
                print!("TCALLM \t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            EQ => {
                print!("EQ     \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            EQUAL => {
                print!("EQUAL  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            NOT => {
                print!("NOT    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            ERR => {
                print!("ERR    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            CCC => {
                print!("CCC    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            DFR => {
                print!("DFR    \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            DFRPOP => {
                println!("DFRPOP");
                Ok(false)
            }
            ONERR => {
                print!("ONERR  \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            JMP => {
                print!("JMP    \t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMPF => {
                print!("JMPF   \t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMPB => {
                print!("JMPB   \t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMPFT => {
                print!("JMPFT  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMPBT => {
                print!("JMPBT  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMPFF => {
                print!("JMPFF  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMPBF => {
                print!("JMPBF  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMP_T => {
                print!("JMP_T  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMP_F => {
                print!("JMP_F  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMPEQ => {
                print!("JMPEQ  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMPLT => {
                print!("JMPLT  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMPGT => {
                print!("JMPGT  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMPFU => {
                print!("JMPFU  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMPBU => {
                print!("JMPBU  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMPFNU => {
                print!("JMPFNU \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            JMPBNU => {
                print!("JMPBNU \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            ADD => {
                print!("ADD    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            SUB => {
                print!("SUB    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            MUL => {
                print!("MUL    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            DIV => {
                print!("DIV    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            ADDM => {
                print!("ADDM   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            SUBM => {
                print!("SUBM   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            MULM => {
                print!("MULM   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            DIVM => {
                print!("DIVM   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            NUMEQ => {
                print!("NUMEQ  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            NUMNEQ => {
                print!("NUMNEQ \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            NUMLT => {
                print!("NUMLT  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            NUMGT => {
                print!("NUMGT  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            NUMLTE => {
                print!("NUMLTE \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            NUMGTE => {
                print!("NUMGTE \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            INC => {
                print!("INC    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            DEC => {
                print!("DEC    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
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
                Ok(false)
            }
            CAR => {
                println!("CAR");
                Ok(false)
            }
            CDR => {
                println!("CDR");
                Ok(false)
            }
            XAR => {
                println!("XAR");
                Ok(false)
            }
            XDR => {
                println!("XDR");
                Ok(false)
            }
            LIST => {
                print!("LIST    \t");
                //println!("{:#06x} ", decode_u16_enum!(code)?);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            APND => {
                print!("APND    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            VECMK => {
                print!("VECMK  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            VECELS => {
                print!("VECELS \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            VECPSH => {
                print!("VECPSH \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            VECPOP => {
                print!("VECPOP \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            VECNTH => {
                print!("VECNTH \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            VECSTH => {
                print!("VECSTH \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            VECMKD => {
                print!("VECMKD \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            VEC => {
                print!("VEC     \t");
                //println!("{:#06x} ", decode_u16_enum!(code)?);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            VECLEN => {
                print!("VECLEN  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            VECCLR => {
                print!("VECCLR  \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            STR => {
                print!("STR     \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            TYPE => {
                print!("TYPE    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            _ => Err(VMError::new_chunk(format!("ERROR: unknown opcode {}", op))),
        }
    }

    pub fn disassemble_chunk(&self, vm: &Vm, indent_level: u16) -> VMResult<()> {
        fn indent(indent_level: u16) {
            for _ in 0..indent_level {
                print!("\t");
            }
        }
        indent(indent_level);
        println!("CONSTANTS:");
        for (i, v) in self.constants.iter().enumerate() {
            indent(indent_level);
            println!("{}: {}", i, v.display_value(vm));
            match v {
                Value::Lambda(h) => vm.get_lambda(*h).disassemble_chunk(vm, indent_level + 1)?,
                Value::Closure(h) => vm.get_lambda(*h).disassemble_chunk(vm, indent_level + 1)?,
                _ => {}
            }
        }
        println!();
        if let Some(caps) = &self.captures {
            indent(indent_level);
            println!("Captures: {:?}", caps);
        }
        let mut code = self.code.iter().cloned().enumerate();
        let mut op = code.next();
        let mut last_line = 0;
        let mut wide = false;
        while let Some((idx, curr_op)) = op {
            indent(indent_level);
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
            wide = Chunk::disassemble_instruction(&mut code, curr_op, wide)?;
            op = code.next();
        }
        Ok(())
    }
}
