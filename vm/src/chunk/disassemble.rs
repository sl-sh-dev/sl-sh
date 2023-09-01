use crate::opcodes::*;
use crate::{Chunk, GVm, VMError, VMResult, Value};

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
        if let (Some(idx1), Some(idx2)) = ($code.next(), $code.next()) {
            Ok(((*idx1 as u16) << 8) | (*idx2 as u16))
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
        if let (Some((_, idx1)), Some((_, idx2))) = ($code.next(), $code.next()) {
            Ok(((idx1 as u16) << 8) | (idx2 as u16))
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
        if let (Some((_, idx1)), Some((_, idx2)), Some((_, idx3)), Some((_, idx4))) =
            ($code.next(), $code.next(), $code.next(), $code.next())
        {
            Ok(
                ((idx1 as u32) << 24)
                    | ((idx2 as u32) << 16)
                    | ((idx3 as u32) << 8)
                    | (idx4 as u32),
            )
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

macro_rules! disassemble_immediate_global {
    ($code:expr, $wide:expr, $vm:expr) => {{
        if $wide {
            let idx = decode_u32_enum!($code)?;
            print!("{idx:#010x}");
        } else {
            let idx = decode_u16_enum!($code)?;
            print!("{idx:#06x}");
        }
    }};
}

macro_rules! disassemble_jump_operand {
    ($chunk:expr, $code:expr, $wide:expr) => {{
        let idx = if $wide {
            let idx = decode_u16_enum!($code)?;
            print!("J({idx:#06x})\t");
            idx as usize
        } else {
            let idx = decode_u8_enum!($code)?;
            print!("J({idx:#04x})\t");
            idx as usize
        };
        print!("{:#010x}", $chunk.jump_table[idx]);
    }};
}

impl Chunk {
    fn disassemble_instruction<I, ENV>(
        &self,
        chunk: I,
        op: OpCode,
        wide: bool,
        _vm: &GVm<ENV>,
    ) -> VMResult<bool>
    where
        I: IntoIterator<Item = (usize, u8)>,
    {
        let mut code = chunk.into_iter();
        match op {
            NOP => {
                println!("NOP({NOP:#04x})");
                Ok(false)
            }
            HALT => {
                println!("HALT({HALT:#04x})");
                Ok(false)
            }
            RET => {
                println!("RET({RET:#04x})");
                Ok(false)
            }
            SRET => {
                print!("SRET({SRET:#04x})   \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            WIDE => {
                println!("WIDE({WIDE:#04x})");
                Ok(true)
            }
            MOV => {
                print!("MOV({MOV:#04x})    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            MOVI => {
                print!("MOVI({MOVI:#04x})   \t");
                print!("R[");
                disassemble_operand!(code, true, wide);
                print!("]");
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            MOVII => {
                print!("MOVII({MOVII:#04x})  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                print!("R[");
                disassemble_operand!(code, true, wide);
                print!("]");
                println!();
                Ok(false)
            }
            GET => {
                print!("GET({GET:#04x})  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            SET => {
                print!("SET({SET:#04x})    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            CONST => {
                print!("CONST({CONST:#04x})  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, false, wide);
                println!();
                Ok(false)
            }
            DEF => {
                print!("DEF({DEF:#04x})    \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                print!("G[");
                disassemble_immediate_global!(code, wide, _vm);
                println!("]");
                Ok(false)
            }
            DEFV => {
                print!("DEFV({DEFV:#04x})   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                print!("G[");
                disassemble_immediate_global!(code, wide, _vm);
                println!("]");
                Ok(false)
            }
            REFI => {
                print!("REFI({REFI:#04x})   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                print!("G[");
                disassemble_immediate_global!(code, wide, _vm);
                println!("]");
                Ok(false)
            }
            CLRREG => {
                print!("CLRREG({CLRREG:#04x}) \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGT => {
                print!("REGT({REGT:#04x})   \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGF => {
                print!("REGF({REGF:#04x})   \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGN => {
                print!("REGN({REGN:#04x})   \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGC => {
                print!("REGC({REGC:#04x})   \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGB => {
                print!("REGB({REGB:#04x})   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            REGI => {
                print!("REGI({REGI:#04x})   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            REGU => {
                print!("REGU({REGU:#04x})   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            CLOSE => {
                print!("CLOSE({CLOSE:#04x})  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            BMOV => {
                print!("BMOV({BMOV:#04x})   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            LDSC => {
                print!("LDSC({LDSC:#04x})   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            LDSCR => {
                print!("LDSCR({LDSCR:#04x})  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            MDSC => {
                print!("MDSC({MDSC:#04x})   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            COPY => {
                print!("COPY({COPY:#04x})   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            FRZ => {
                print!("FRZ({FRZ:#04x})    \t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            CALL => {
                print!("CALL({CALL:#04x})   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            CALLG => {
                print!("CALLG({CALLG:#04x})  \t");
                print!("G[");
                disassemble_immediate_global!(code, wide, _vm);
                print!("]");
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            TCALL => {
                print!("TCALL({TCALL:#04x})  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            TCALLG => {
                print!("TCALLG({TCALLG:#04x}) \t");
                print!("G[");
                disassemble_immediate_global!(code, wide, _vm);
                print!("]");
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            CALLM => {
                print!("CALLM({CALLM:#04x})  \t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            TCALLM => {
                print!("TCALLM({TCALLM:#04x}) \t");
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
            MKERR => {
                print!("MKERR  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
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
                print!("JMP({JMP:#04x})    \t");
                disassemble_jump_operand!(self, code, wide);
                println!();
                Ok(false)
            }
            JMPT => {
                print!("JMPT({JMPT:#04x})   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_jump_operand!(self, code, wide);
                println!();
                Ok(false)
            }
            JMPF => {
                print!("JMPF({JMPF:#04x})   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_jump_operand!(self, code, wide);
                println!();
                Ok(false)
            }
            JMPEQ => {
                print!("JMPEQ({JMPEQ:#04x})  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_jump_operand!(self, code, wide);
                println!();
                Ok(false)
            }
            JMPLT => {
                print!("JMPLT({JMPLT:#04x})  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_jump_operand!(self, code, wide);
                println!();
                Ok(false)
            }
            JMPGT => {
                print!("JMPGT({JMPGT:#04x})  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_jump_operand!(self, code, wide);
                println!();
                Ok(false)
            }
            JMPU => {
                print!("JMPU({JMPU:#04x})   \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_jump_operand!(self, code, wide);
                println!();
                Ok(false)
            }
            JMPNU => {
                print!("JMPNU({JMPNU:#04x})  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_jump_operand!(self, code, wide);
                println!();
                Ok(false)
            }
            JMPRU => {
                print!("JMPRU({JMPRU:#04x})  \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_jump_operand!(self, code, wide);
                println!();
                Ok(false)
            }
            JMPRNU => {
                print!("JMPRNU({JMPRNU:#04x}) \t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_jump_operand!(self, code, wide);
                println!();
                Ok(false)
            }
            ADD => {
                print!("ADD    \t");
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
                println!();
                Ok(false)
            }
            MUL => {
                print!("MUL    \t");
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
                print!("CAR    ");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            CDR => {
                print!("CDR    ");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            XAR => {
                print!("XAR    ");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            XDR => {
                print!("XDR    ");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
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
            _ => Err(VMError::new_chunk(format!("ERROR: unknown opcode {op}"))),
        }
    }

    pub fn disassemble_chunk<ENV>(&self, vm: &GVm<ENV>, indent_level: u16) -> VMResult<()> {
        fn indent(indent_level: u16) {
            for _ in 0..indent_level {
                print!("\t");
            }
        }
        indent(indent_level);
        println!(
            "INPUTS: {} args/optional/rest {}/{}/{}",
            self.input_regs, self.args, self.opt_args, self.rest
        );
        indent(indent_level);
        println!("EXTRA REGS: {}", self.extra_regs);
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
            println!("Captures: {caps:?}");
        }
        let mut code = self.code.iter().cloned().enumerate();
        let mut op = code.next();
        let mut last_line = 0;
        let mut wide = false;
        while let Some((idx, curr_op)) = op {
            indent(indent_level);
            print!("{idx:#010x} ");
            if let Some(line_number) = self.offset_to_line(idx) {
                if last_line != line_number {
                    print!("{line_number:>6} ");
                    last_line = line_number;
                } else {
                    print!("     | ");
                }
            } else {
                print!("     | ");
            }
            wide = self.disassemble_instruction(&mut code, curr_op, wide, vm)?;
            op = code.next();
        }
        Ok(())
    }
}
