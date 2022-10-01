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

macro_rules! decode_i24_enum {
    ($code:expr) => {{
        if let (Some((_, idx1)), Some((_, idx2)), Some((pnip, idx3))) =
            ($code.next(), $code.next(), $code.next())
        {
            let negative = (idx1 & 0x80) == 0x80;
            let num =
                ((((idx1 & 0x7f) as u32) << 16) | ((idx2 as u32) << 8) | (idx3 as u32)) as i32;
            if negative {
                Ok((-num, (pnip + 1) as i32))
            } else {
                Ok((num, (pnip + 1) as i32))
            }
        } else {
            Err(VMError::new_chunk(
                "Error decoding a i24 from chunk stream.",
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
            //print!("{:#010x}:{}", idx, $vm.global_name(idx as usize));
            print!("{:#010x}", idx);
        } else {
            let idx = decode_u16_enum!($code)?;
            //print!("{:#06x}:{}", idx, $vm.global_name(idx as usize));
            print!("{:#06x}", idx);
        }
    }};
}

macro_rules! disassemble_jump_offset {
    ($code:expr) => {{
        let (offset, nip) = decode_i24_enum!($code)?;
        print!("{} -> {:#010x}", offset, nip + offset);
    }};
}

impl Chunk {
    fn disassemble_instruction<I, ENV>(
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
                println!("NOP({:#04x})", NOP);
                Ok(false)
            }
            HALT => {
                println!("HALT({:#04x})", HALT);
                Ok(false)
            }
            RET => {
                println!("RET({:#04x})", RET);
                Ok(false)
            }
            SRET => {
                print!("SRET({:#04x})   \t", SRET);
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            WIDE => {
                println!("WIDE({:#04x})", WIDE);
                Ok(true)
            }
            MOV => {
                print!("MOV({:#04x})    \t", MOV);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            SET => {
                print!("SET({:#04x})    \t", SET);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            CONST => {
                print!("CONST({:#04x})  \t", CONST);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, false, wide);
                println!();
                Ok(false)
            }
            DEF => {
                print!("DEF({:#04x})    \t", DEF);
                disassemble_operand!(code, true, wide);
                print!("\t");
                print!("G[");
                disassemble_immediate_global!(code, wide, _vm);
                println!("]");
                Ok(false)
            }
            DEFV => {
                print!("DEFV({:#04x})   \t", DEFV);
                disassemble_operand!(code, true, wide);
                print!("\t");
                print!("G[");
                disassemble_immediate_global!(code, wide, _vm);
                println!("]");
                Ok(false)
            }
            REFI => {
                print!("REFI({:#04x})   \t", REFI);
                disassemble_operand!(code, true, wide);
                print!("\t");
                print!("G[");
                disassemble_immediate_global!(code, wide, _vm);
                println!("]");
                Ok(false)
            }
            CLRREG => {
                print!("CLRREG({:#04x}) \t", CLRREG);
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGT => {
                print!("REGT({:#04x})   \t", REGT);
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGF => {
                print!("REGF({:#04x})   \t", REGF);
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGN => {
                print!("REGN({:#04x})   \t", REGN);
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGC => {
                print!("REGC({:#04x})   \t", REGC);
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            REGB => {
                print!("REGB({:#04x})   \t", REGB);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            REGI => {
                print!("REGI({:#04x})   \t", REGI);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            REGU => {
                print!("REGU({:#04x})   \t", REGU);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            CLOSE => {
                print!("CLOSE({:#04x})  \t", CLOSE);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            BMOV => {
                print!("BMOV({:#04x})   \t", BMOV);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            LDSC => {
                print!("LDSC({:#04x})   \t", LDSC);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            LDSCR => {
                print!("LDSCR({:#04x})  \t", LDSCR);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            MDSC => {
                print!("MDSC({:#04x})   \t", MDSC);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            COPY => {
                print!("COPY({:#04x})   \t", COPY);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            FRZ => {
                print!("FRZ({:#04x})    \t", FRZ);
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            CALL => {
                print!("CALL({:#04x})   \t", CALL);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            CALLG => {
                print!("CALLG({:#04x})  \t", CALLG);
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
                print!("TCALL({:#04x})  \t", TCALL);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            TCALLG => {
                print!("TCALLG({:#04x}) \t", TCALLG);
                print!("G[");
                disassemble_immediate_global!(code, wide, _vm);
                print!("]");
                print!("\t");
                disassemble_immediate!(code, wide);
                println!();
                Ok(false)
            }
            CALLM => {
                print!("CALLM({:#04x})  \t", CALLM);
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                println!();
                Ok(false)
            }
            TCALLM => {
                print!("TCALLM({:#04x}) \t", TCALLM);
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
                print!("JMP({:#04x})    \t", JMP);
                disassemble_jump_offset!(code);
                println!();
                Ok(false)
            }
            JMPT => {
                print!("JMPT({:#04x})   \t", JMPT);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_jump_offset!(code);
                println!();
                Ok(false)
            }
            JMPF => {
                print!("JMPF({:#04x})   \t", JMPF);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_jump_offset!(code);
                println!();
                Ok(false)
            }
            JMPEQ => {
                print!("JMPEQ({:#04x})  \t", JMPEQ);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_jump_offset!(code);
                println!();
                Ok(false)
            }
            JMPLT => {
                print!("JMPLT({:#04x})  \t", JMPLT);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_jump_offset!(code);
                println!();
                Ok(false)
            }
            JMPGT => {
                print!("JMPGT({:#04x})  \t", JMPGT);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_jump_offset!(code);
                println!();
                Ok(false)
            }
            JMPU => {
                print!("JMPU({:#04x})   \t", JMPU);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_jump_offset!(code);
                println!();
                Ok(false)
            }
            JMPNU => {
                print!("JMPNU({:#04x})  \t", JMPNU);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_jump_offset!(code);
                println!();
                Ok(false)
            }
            JMPRU => {
                print!("JMPRU({:#04x})  \t", JMPRU);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_jump_offset!(code);
                println!();
                Ok(false)
            }
            JMPRNU => {
                print!("JMPRNU({:#04x}) \t", JMPRNU);
                disassemble_operand!(code, true, wide);
                print!("\t");
                disassemble_immediate!(code, wide);
                print!("\t");
                disassemble_jump_offset!(code);
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
            _ => Err(VMError::new_chunk(format!("ERROR: unknown opcode {}", op))),
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
            wide = Chunk::disassemble_instruction(&mut code, curr_op, wide, vm)?;
            op = code.next();
        }
        Ok(())
    }
}
