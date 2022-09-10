use crate::opcodes::*;
use crate::{CallFrame, Chunk, Continuation, F64Wrap, VMError, VMErrorObj, VMResult, Value, Vm};
use std::sync::Arc;

impl Vm {
    #[inline]
    fn map_destructure(
        &mut self,
        code: &[u8],
        registers: &mut [Value],
        wide: bool,
    ) -> VMResult<()> {
        let (dest, len, src) = decode3!(code, &mut self.ip, wide);
        if len > 0 {
            let len = len as usize;
            let dest = dest as usize;
            let val = get_reg_unref!(registers, src, self);
            match val {
                Value::Map(handle) => {
                    let map = self.get_map(handle);
                    for i in 0..len as usize {
                        let key = get_reg_unref!(registers, dest + i, self);
                        if let Some(item) = map.get(&key) {
                            registers[dest + i] = *item;
                        } else {
                            registers[dest + i] = Value::Undefined;
                        }
                    }
                }
                Value::Vector(handle) => {
                    let vector = self.get_vector(handle);
                    for i in 0..len as usize {
                        let key = get_reg_unref!(registers, dest + i, self);
                        if key.is_int() {
                            let key = key.get_int()?;
                            if key >= 0 && key < vector.len() as i64 {
                                registers[dest + i] = vector[key as usize];
                            } else {
                                return Err(VMError::new_vm("seq key out of bounds"));
                            }
                        } else {
                            let mut iter = vector.iter();
                            registers[dest + i] = Value::Undefined;
                            while let Some(v) = iter.next() {
                                if *v == key {
                                    if let Some(value) = iter.next() {
                                        registers[dest + i] = *value;
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }
                Value::List(handle, idx) => {
                    let vector = &self.get_vector(handle)[idx as usize..];
                    for i in 0..len as usize {
                        let key = get_reg_unref!(registers, dest + i, self);
                        if key.is_int() {
                            let key = key.get_int()?;
                            if key >= 0 && key < vector.len() as i64 {
                                registers[dest + i] = vector[key as usize];
                            } else {
                                return Err(VMError::new_vm("seq key out of bounds"));
                            }
                        } else {
                            let mut iter = vector.iter();
                            registers[dest + i] = Value::Undefined;
                            while let Some(v) = iter.next() {
                                if *v == key {
                                    if let Some(value) = iter.next() {
                                        registers[dest + i] = *value;
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }
                Value::Pair(_) => {
                    for i in 0..len as usize {
                        let key = get_reg_unref!(registers, dest + i, self);
                        if key.is_int() {
                            let key = key.get_int()?;
                            if key >= 0 {
                                let mut iter = val.iter(self);
                                if let Some(item) = iter.nth(key as usize) {
                                    registers[dest + i] = item;
                                } else {
                                    return Err(VMError::new_vm("seq key out of bounds"));
                                }
                            } else {
                                return Err(VMError::new_vm("seq key out of bounds"));
                            }
                        } else {
                            let mut iter = val.iter(self);
                            registers[dest + i] = Value::Undefined;
                            while let Some(v) = iter.next() {
                                if v == key {
                                    if let Some(value) = iter.next() {
                                        registers[dest + i] = value;
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }
                Value::Nil => {
                    for i in 0..len as usize {
                        registers[dest + i] = Value::Undefined;
                    }
                }
                _ => return Err(VMError::new_vm("not a map")),
            }
        }
        Ok(())
    }

    pub(super) fn exec_loop(&mut self, chunk: Arc<Chunk>) -> Result<(), (VMError, Arc<Chunk>)> {
        let mut registers = self.make_registers();
        let mut chunk = chunk;
        self.ip = 0;
        let mut wide = false;
        // Clean up the working regs we are about to use.
        if chunk.extra_regs > 0 {
            for r in chunk.input_regs..=chunk.input_regs + chunk.extra_regs {
                Vm::mov_register(registers, r, Value::Undefined);
            }
        }
        loop {
            let opcode = chunk.code[self.ip];
            self.current_ip = self.ip;
            self.ip += 1;
            match opcode {
                NOP => {}
                HALT => {
                    return Err((VMError::new_vm("HALT: VM halted and on fire!"), chunk));
                }
                RET => {
                    if let Some(defer) = self.defers.pop() {
                        let first_reg = (chunk.input_regs + chunk.extra_regs + 1) as u16;
                        self.ip = self.current_ip;
                        chunk = self.make_call(defer, chunk, registers, first_reg, 0, false)?;
                        registers = self.make_registers();
                    } else {
                        self.stack[self.stack_top] =
                            get_reg_unref!(registers, self.stack_top, self);
                        if let Some(frame) = self.call_frame_mut() {
                            // Need to break the call frame lifetime from self to avoid extra work.
                            // This is safe because the stack and heap are not touched so the reference is
                            // stable.  The unwrap() is OK because the frame can not be NULL.
                            let frame: &mut CallFrame =
                                unsafe { (frame as *mut CallFrame).as_mut().unwrap() };
                            self.stack_top = frame.stack_top;
                            registers = self.make_registers();
                            chunk = frame.chunk.clone();
                            self.stack_max = self.stack_top + chunk.input_regs + chunk.extra_regs;
                            self.ip = frame.ip;
                            self.current_ip = frame.current_ip;
                            self.this_fn = frame.this_fn;
                            self.on_error = frame.on_error;
                            std::mem::swap(&mut self.defers, &mut frame.defers);
                        } else {
                            return Ok(());
                        }
                    }
                }
                SRET => {
                    if let Some(defer) = self.defers.pop() {
                        let first_reg = (chunk.input_regs + chunk.extra_regs + 1) as u16;
                        self.ip = self.current_ip;
                        chunk = self.make_call(defer, chunk, registers, first_reg, 0, false)?;
                        registers = self.make_registers();
                    } else {
                        let src = decode1!(chunk.code, &mut self.ip, wide);
                        let val = get_reg_unref!(registers, src, self);
                        let old_top = self.stack_top;
                        if let Some(frame) = self.call_frame_mut() {
                            // Need to break the call frame lifetime from self to avoid extra work.
                            // This is safe because the stack and heap are not touched so the reference is
                            // stable.  The unwrap() is OK because the frame can not be NULL.
                            let frame: &mut CallFrame =
                                unsafe { (frame as *mut CallFrame).as_mut().unwrap() };
                            self.stack_top = frame.stack_top;
                            registers = self.make_registers();
                            chunk = frame.chunk.clone();
                            self.stack_max = self.stack_top + chunk.input_regs + chunk.extra_regs;
                            self.ip = frame.ip;
                            self.current_ip = frame.current_ip;
                            self.this_fn = frame.this_fn;
                            self.on_error = frame.on_error;
                            std::mem::swap(&mut self.defers, &mut frame.defers);
                        } else {
                            self.stack[old_top] = val;
                            return Ok(());
                        }
                        self.stack[old_top] = val;
                    }
                }
                WIDE => wide = true,
                MOV => {
                    let (dest, src) = decode2!(chunk.code, &mut self.ip, wide);
                    // XXX TODO- figure out proper mov symantics...
                    let val = get_reg_unref!(registers, src, self);
                    //let val = get_reg!(registers, src);
                    Self::mov_register(registers, dest as usize, val);
                }
                BMOV => {
                    let (dest, src, len) = decode3!(chunk.code, &mut self.ip, wide);
                    for i in 0..len as usize {
                        registers[dest as usize + i] = registers[src as usize + i];
                    }
                }
                LDSC => {
                    let (dest, len, src) = decode3!(chunk.code, &mut self.ip, wide);
                    if len > 0 {
                        let len = len as usize;
                        let dest = dest as usize;
                        let val = get_reg_unref!(registers, src, self);
                        match val {
                            Value::Vector(_) | Value::Pair(_) | Value::List(_, _) => {
                                let mut iter = val.iter(self);
                                for i in 0..len as usize {
                                    if let Some(item) = iter.next() {
                                        registers[dest + i] = item;
                                    } else {
                                        registers[dest + i] = Value::Undefined;
                                    }
                                }
                            }
                            _ => return Err((VMError::new_vm("not a sequence"), chunk)),
                        }
                    }
                }
                LDSCR => {
                    let (dest, len, src) = decode3!(chunk.code, &mut self.ip, wide);
                    if len > 0 {
                        let len = len as usize;
                        let dest = dest as usize;
                        let val = get_reg_unref!(registers, src, self);
                        match val {
                            Value::Vector(_) | Value::Pair(_) | Value::List(_, _) => {
                                let mut iter = val.iter(self);
                                for i in 0..len - 1 {
                                    if let Some(item) = iter.next() {
                                        registers[dest + i] = item;
                                    } else {
                                        registers[dest + i] = Value::Undefined;
                                    }
                                }
                                let rest: Vec<Value> = iter.collect();
                                if rest.is_empty() {
                                    registers[dest + (len - 1)] = Value::Nil;
                                } else {
                                    registers[dest + (len - 1)] = self.alloc_list_ro(rest);
                                }
                            }
                            _ => return Err((VMError::new_vm("not a sequence"), chunk)),
                        }
                    }
                }
                MDSC => {
                    self.map_destructure(&chunk.code[..], registers, wide)
                        .map_err(|e| (e, chunk.clone()))?;
                }
                COPY => {
                    let (_dest, _src) = decode2!(chunk.code, &mut self.ip, wide);
                    // XXX Deep copy src to dest
                }
                FRZ => {
                    let target = decode1!(chunk.code, &mut self.ip, wide);
                    let target = get_reg_unref!(registers, target, self);
                    if let Some(_handle) = target.get_handle() {
                        // XXX mark handle read only
                    }
                }
                SET => {
                    let (dest, src) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = get_reg_unref!(registers, src, self);
                    // self.set_register(registers, dest as usize, val);
                    match &get_reg!(registers, dest) {
                        Value::Value(handle) => {
                            *(self.get_value_mut(*handle)) = val;
                        }
                        Value::Global(dest) => self.globals.set(*dest, val),
                        _ => registers[dest as usize] = val,
                    }
                }
                CONST => {
                    let (dest, src) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = chunk.constants[src as usize];
                    self.set_register(registers, dest as usize, val);
                }
                REF => {
                    let (dest, src) = decode2!(chunk.code, &mut self.ip, wide);
                    let idx = match get_reg!(registers, src) {
                        Value::Symbol(s) => {
                            if let Some(i) = self.globals.interned_slot(s) {
                                i as u32
                            } else {
                                return Err((VMError::new_vm("REF: Symbol not global."), chunk));
                            }
                        }
                        Value::Global(i) => i,
                        _ => return Err((VMError::new_vm("REF: Not a symbol."), chunk)),
                    };
                    if let Value::Undefined = self.globals.get(idx as u32) {
                        return Err((VMError::new_vm("REF: Symbol is not defined."), chunk));
                    }
                    Self::mov_register(registers, dest as usize, Value::Global(idx));
                }
                DEF => {
                    let (dest, src) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = get_reg_unref!(registers, src, self);
                    if let Value::Global(i) = get_reg!(registers, dest) {
                        self.globals.set(i, val);
                    } else {
                        return Err((
                            VMError::new_vm(format!(
                                "DEF: Not a global, got: {:?}.",
                                get_reg!(registers, dest),
                            )),
                            chunk,
                        ));
                    }
                }
                DEFV => {
                    let (dest, src) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = get_reg_unref!(registers, src, self);
                    if let Value::Global(i) = get_reg!(registers, dest) {
                        if let Value::Undefined = self.globals.get(i) {
                            self.globals.set(i, val);
                        }
                    } else {
                        return Err((VMError::new_vm("DEFV: Not a global."), chunk));
                    }
                }
                REFI => {
                    let dest = decode1!(chunk.code, &mut self.ip, wide);
                    let idx = if wide {
                        decode_u32!(chunk.code, &mut self.ip)
                    } else {
                        decode_u16!(chunk.code, &mut self.ip) as u32
                    };
                    Self::mov_register(registers, dest as usize, Value::Global(idx));
                }
                CLRREG => {
                    let dest = decode1!(chunk.code, &mut self.ip, wide);
                    Self::mov_register(registers, dest as usize, Value::Undefined);
                }
                REGT => {
                    let dest = decode1!(chunk.code, &mut self.ip, wide);
                    self.set_register(registers, dest as usize, Value::True);
                }
                REGF => {
                    let dest = decode1!(chunk.code, &mut self.ip, wide);
                    self.set_register(registers, dest as usize, Value::False);
                }
                REGN => {
                    let dest = decode1!(chunk.code, &mut self.ip, wide);
                    self.set_register(registers, dest as usize, Value::Nil);
                }
                REGC => {
                    let dest = decode1!(chunk.code, &mut self.ip, wide);
                    self.set_register(registers, dest as usize, Value::Undefined);
                }
                REGB => {
                    let (dest, i) = decode2!(chunk.code, &mut self.ip, wide);
                    self.set_register(registers, dest as usize, Value::Byte(i as u8));
                }
                REGI => {
                    let (dest, i) = decode2!(chunk.code, &mut self.ip, wide);
                    self.set_register(registers, dest as usize, Value::Int(i as i64));
                }
                REGU => {
                    let (dest, i) = decode2!(chunk.code, &mut self.ip, wide);
                    self.set_register(registers, dest as usize, Value::UInt(i as u64));
                }
                CLOSE => {
                    let (dest, src) = decode2!(chunk.code, &mut self.ip, wide);
                    let lambda = get_reg_unref!(registers, src, self);
                    let (lambda, caps) = if let Value::Lambda(h) = lambda {
                        let l = self.heap.get_lambda(h);
                        let l = l.clone();
                        let mut caps = Vec::new();
                        if let Some(captures) = &l.captures {
                            for c in captures {
                                let r = get_reg!(registers, *c);
                                if let Value::Value(b) = r {
                                    caps.push(b);
                                } else {
                                    let val = self.new_upval(r);
                                    self.set_register(registers, *c as usize, val);
                                    caps.push(val.get_handle().unwrap());
                                }
                            }
                        }
                        (l.clone(), caps)
                    } else {
                        return Err((
                            VMError::new_vm(format!("CLOSE: requires a lambda, got {:?}.", lambda)),
                            chunk,
                        ));
                    };
                    let new_closure = self.alloc_closure(lambda, caps);
                    self.set_register(registers, dest as usize, new_closure);
                }
                CALL => {
                    let (lambda, num_args, first_reg) = decode3!(chunk.code, &mut self.ip, wide);
                    let lambda = get_reg_unref!(registers, lambda, self);
                    chunk = self.make_call(lambda, chunk, registers, first_reg, num_args, false)?;
                    registers = self.make_registers();
                }
                CALLG => {
                    let idx = if wide {
                        decode_u32!(chunk.code, &mut self.ip)
                    } else {
                        decode_u16!(chunk.code, &mut self.ip) as u32
                    };
                    let (num_args, first_reg) = decode2!(chunk.code, &mut self.ip, wide);
                    let lambda = self.get_global(idx);
                    chunk = self.make_call(lambda, chunk, registers, first_reg, num_args, false)?;
                    registers = self.make_registers();
                }
                TCALL => {
                    if let Some(defer) = self.defers.pop() {
                        // Tail call so do defers first.
                        let first_reg = (chunk.input_regs + chunk.extra_regs + 1) as u16;
                        self.ip = self.current_ip;
                        chunk = self.make_call(defer, chunk, registers, first_reg, 0, false)?;
                        registers = self.make_registers();
                    } else {
                        let (lambda, num_args) = decode2!(chunk.code, &mut self.ip, wide);
                        let lambda = get_reg_unref!(registers, lambda, self);
                        chunk = self.make_call(lambda, chunk, registers, 0, num_args, true)?;
                        registers = self.make_registers(); // In case of a builtin call
                    }
                }
                TCALLG => {
                    if let Some(defer) = self.defers.pop() {
                        // Tail call so do defers first.
                        let first_reg = (chunk.input_regs + chunk.extra_regs + 1) as u16;
                        self.ip = self.current_ip;
                        chunk = self.make_call(defer, chunk, registers, first_reg, 0, false)?;
                        registers = self.make_registers();
                    } else {
                        let idx = if wide {
                            decode_u32!(chunk.code, &mut self.ip)
                        } else {
                            decode_u16!(chunk.code, &mut self.ip) as u32
                        };
                        let num_args = decode1!(chunk.code, &mut self.ip, wide);
                        let lambda = self.get_global(idx);
                        chunk = self.make_call(lambda, chunk, registers, 0, num_args, true)?;
                        registers = self.make_registers(); // In case of a builtin call
                    }
                }
                CALLM => {
                    let (num_args, first_reg) = decode2!(chunk.code, &mut self.ip, wide);
                    if let Some(this_fn) = self.this_fn {
                        chunk =
                            self.make_call(this_fn, chunk, registers, first_reg, num_args, false)?;
                        registers = self.make_registers();
                    } else {
                        let line = if wide {
                            chunk.offset_to_line(self.ip - 4)
                        } else {
                            chunk.offset_to_line(self.ip - 2)
                        };
                        return Err((
                            VMError::new_vm(format!(
                                "CALLM: Not in an existing lambda call, line {}.",
                                line.unwrap_or(0)
                            )),
                            chunk,
                        ));
                    }
                }
                TCALLM => {
                    if let Some(defer) = self.defers.pop() {
                        // Tail call so do defers first.
                        let first_reg = (chunk.input_regs + chunk.extra_regs + 1) as u16;
                        self.ip = self.current_ip;
                        chunk = self.make_call(defer, chunk, registers, first_reg, 0, false)?;
                        registers = self.make_registers();
                    } else {
                        let num_args = decode1!(chunk.code, &mut self.ip, wide);
                        if let Some(this_fn) = self.this_fn {
                            chunk = self.make_call(this_fn, chunk, registers, 0, num_args, true)?;
                            registers = self.make_registers(); // In case of a builtin call
                        } else {
                            let line = if wide {
                                chunk.offset_to_line(self.ip - 4)
                            } else {
                                chunk.offset_to_line(self.ip - 2)
                            };
                            return Err((
                                VMError::new_vm(format!(
                                    "TCALLM: Not in an existing lambda call, line {}.",
                                    line.unwrap_or(0)
                                )),
                                chunk,
                            ));
                        }
                    }
                }
                JMP => {
                    let ipoff = decode_i24!(chunk.code, &mut self.ip);
                    self.ip = (self.ip as isize + ipoff) as usize;
                }
                JMPT => {
                    let test = decode1!(chunk.code, &mut self.ip, wide);
                    let ipoff = decode_i24!(chunk.code, &mut self.ip);
                    if get_reg_unref!(registers, test, self).is_truethy() {
                        self.ip = (self.ip as isize + ipoff) as usize;
                    }
                }
                JMPF => {
                    let test = decode1!(chunk.code, &mut self.ip, wide);
                    let ipoff = decode_i24!(chunk.code, &mut self.ip);
                    if get_reg_unref!(registers, test, self).is_falsey() {
                        self.ip = (self.ip as isize + ipoff) as usize;
                    }
                }
                JMPEQ => {
                    let (op1, op2) = decode2!(chunk.code, &mut self.ip, wide);
                    let ipoff = decode_i24!(chunk.code, &mut self.ip);
                    let op1 = get_reg_unref!(registers, op1, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    let op2 = get_reg_unref!(registers, op2, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    if op1 == op2 {
                        self.ip = (self.ip as isize + ipoff) as usize;
                    }
                }
                JMPLT => {
                    let (op1, op2) = decode2!(chunk.code, &mut self.ip, wide);
                    let ipoff = decode_i24!(chunk.code, &mut self.ip);
                    let op1 = get_reg_unref!(registers, op1, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    let op2 = get_reg_unref!(registers, op2, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    if op1 < op2 {
                        self.ip = (self.ip as isize + ipoff) as usize;
                    }
                }
                JMPGT => {
                    let (op1, op2) = decode2!(chunk.code, &mut self.ip, wide);
                    let ipoff = decode_i24!(chunk.code, &mut self.ip);
                    let op1 = get_reg_unref!(registers, op1, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    let op2 = get_reg_unref!(registers, op2, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    if op1 > op2 {
                        self.ip = (self.ip as isize + ipoff) as usize;
                    }
                }
                JMPU => {
                    let test = decode1!(chunk.code, &mut self.ip, wide);
                    let ipoff = decode_i24!(chunk.code, &mut self.ip);
                    if get_reg_unref!(registers, test, self).is_undef() {
                        self.ip = (self.ip as isize + ipoff) as usize;
                    }
                }
                JMPNU => {
                    let test = decode1!(chunk.code, &mut self.ip, wide);
                    let ipoff = decode_i24!(chunk.code, &mut self.ip) as isize;
                    if !get_reg_unref!(registers, test, self).is_undef() {
                        self.ip = (self.ip as isize + ipoff) as usize;
                    }
                }
                JMPRU => {
                    let (test, len) = decode2!(chunk.code, &mut self.ip, wide);
                    let ipoff = decode_i24!(chunk.code, &mut self.ip);
                    if len > 0 {
                        let test = test as usize;
                        let len = len as usize;
                        for i in 0..len as usize {
                            if get_reg_unref!(registers, test + i, self).is_undef() {
                                self.ip = (self.ip as isize + ipoff) as usize;
                                break;
                            }
                        }
                    }
                }
                JMPRNU => {
                    let (test, len) = decode2!(chunk.code, &mut self.ip, wide);
                    let ipoff = decode_i24!(chunk.code, &mut self.ip);
                    if len > 0 {
                        let len = len as usize;
                        let test = test as usize;
                        let mut jump = true;
                        for i in 0..len as usize {
                            if get_reg_unref!(registers, test + i, self).is_undef() {
                                jump = false;
                                break;
                            }
                        }
                        if jump {
                            self.ip = (self.ip as isize + ipoff) as usize;
                        }
                    }
                }
                EQ => {
                    let (dest, reg1, reg2) = decode3!(chunk.code, &mut self.ip, wide);
                    let val = self
                        .is_eq(registers, reg1, reg2)
                        .map_err(|e| (e, chunk.clone()))?;
                    self.set_register(registers, dest as usize, val);
                }
                EQUAL => {
                    let (dest, reg1, reg2) = decode3!(chunk.code, &mut self.ip, wide);
                    let val = self
                        .is_equal(registers, reg1, reg2)
                        .map_err(|e| (e, chunk.clone()))?;
                    self.set_register(registers, dest as usize, val);
                }
                NOT => {
                    let (dest, val) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = get_reg_unref!(registers, val, self);
                    let res = if val.is_falsey() {
                        Value::True
                    } else {
                        Value::False
                    };
                    self.set_register(registers, dest as usize, res);
                }
                ERR => {
                    let (key, val) = decode2!(chunk.code, &mut self.ip, wide);
                    let key = get_reg_unref!(registers, key, self);
                    let val = get_reg_unref!(registers, val, self);
                    let key_str = if let Value::Keyword(i) = key {
                        self.get_interned(i)
                    } else {
                        return Err((
                            VMError::new_vm(format!("ERR: key must be a keyword, got {:?}.", key)),
                            chunk,
                        ));
                    };
                    if let Value::StringConst(i) = val {
                        return Err((
                            VMError {
                                key: key_str,
                                obj: VMErrorObj::Message(self.get_interned(i).to_string()),
                            },
                            chunk,
                        ));
                    } else {
                        return Err((
                            VMError {
                                key: key_str,
                                obj: VMErrorObj::Object(val),
                            },
                            chunk,
                        ));
                    }
                }
                CCC => {
                    let (lambda, first_reg) = decode2!(chunk.code, &mut self.ip, wide);
                    let lambda = get_reg_unref!(registers, lambda, self);
                    let frame = CallFrame {
                        id: 0,
                        chunk: chunk.clone(),
                        ip: self.ip,
                        current_ip: self.current_ip,
                        stack_top: self.stack_top,
                        this_fn: self.this_fn,
                        defers: Vec::new(),
                        on_error: self.on_error,
                        called: Value::Undefined,
                    };
                    let mut stack = Vec::with_capacity(self.stack_max);
                    stack.resize(self.stack_max, Value::Undefined);
                    stack[..].copy_from_slice(&self.stack[0..self.stack_max]);
                    let k = Continuation {
                        frame,
                        arg_reg: self.stack_top + first_reg as usize, //stack_len,
                        stack,
                    };
                    let k_obj = self.alloc_continuation(k);
                    Self::mov_register(registers, (first_reg + 1) as usize, k_obj);
                    chunk = self.make_call(lambda, chunk, registers, first_reg as u16, 1, false)?;
                    registers = self.make_registers();
                }
                DFR => {
                    let lambda = decode1!(chunk.code, &mut self.ip, wide);
                    let lambda = get_reg_unref!(registers, lambda, self);
                    self.defers.push(lambda);
                }
                DFRPOP => {
                    if let Some(defer) = self.defers.pop() {
                        let first_reg = (chunk.input_regs + chunk.extra_regs + 1) as u16;
                        chunk = self.make_call(defer, chunk, registers, first_reg, 0, false)?;
                        registers = self.make_registers();
                    }
                }
                ONERR => {
                    let on_error_reg = decode1!(chunk.code, &mut self.ip, wide);
                    let on_error = get_reg_unref!(registers, on_error_reg, self);
                    if let Some(oe) = self.on_error {
                        Self::mov_register(registers, on_error_reg as usize, oe);
                    } else {
                        Self::mov_register(registers, on_error_reg as usize, Value::Nil);
                    }
                    if let Value::Nil = on_error {
                        self.on_error = None;
                    } else {
                        self.on_error = Some(on_error);
                    }
                }
                ADD => binary_math!(
                    self,
                    chunk,
                    &mut self.ip,
                    registers,
                    |a, b| a + b,
                    wide,
                    false
                ),
                SUB => binary_math!(
                    self,
                    chunk,
                    &mut self.ip,
                    registers,
                    |a, b| a - b,
                    wide,
                    false
                ),
                MUL => binary_math!(
                    self,
                    chunk,
                    &mut self.ip,
                    registers,
                    |a, b| a * b,
                    wide,
                    false
                ),
                DIV => div_math!(self, chunk, &mut self.ip, registers, wide, false),
                ADDM => binary_math!(
                    self,
                    chunk,
                    &mut self.ip,
                    registers,
                    |a, b| a + b,
                    wide,
                    true
                ),
                SUBM => binary_math!(
                    self,
                    chunk,
                    &mut self.ip,
                    registers,
                    |a, b| a - b,
                    wide,
                    true
                ),
                MULM => binary_math!(
                    self,
                    chunk,
                    &mut self.ip,
                    registers,
                    |a, b| a * b,
                    wide,
                    true
                ),
                DIVM => div_math!(self, chunk, &mut self.ip, registers, wide, true),
                NUMEQ => compare_int!(
                    self,
                    chunk,
                    &mut self.ip,
                    registers,
                    |a, b| a == b,
                    |a: f64, b: f64| (a - b).abs() < f64::EPSILON,
                    wide,
                    true,
                    false
                ),
                NUMNEQ => compare_int!(
                    self,
                    chunk,
                    &mut self.ip,
                    registers,
                    |a, b| a == b,
                    |a: f64, b: f64| (a - b).abs() < f64::EPSILON,
                    wide,
                    true,
                    true
                ),
                NUMLT => compare!(
                    self,
                    chunk,
                    &mut self.ip,
                    registers,
                    |a, b| a < b,
                    wide,
                    true
                ),
                NUMLTE => compare!(
                    self,
                    chunk,
                    &mut self.ip,
                    registers,
                    |a, b| a <= b,
                    wide,
                    true
                ),
                NUMGT => compare!(
                    self,
                    chunk,
                    &mut self.ip,
                    registers,
                    |a, b| a > b,
                    wide,
                    true
                ),
                NUMGTE => compare!(
                    self,
                    chunk,
                    &mut self.ip,
                    registers,
                    |a, b| a >= b,
                    wide,
                    true
                ),
                INC => {
                    let (dest, i) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = match get_reg_unref!(registers, dest, self) {
                        Value::Byte(v) => Value::Byte(v + i as u8),
                        Value::Int(v) => Value::Int(v + i as i64),
                        Value::UInt(v) => Value::UInt(v + i as u64),
                        _ => {
                            return Err((
                                VMError::new_vm(format!(
                                    "INC: Can only INC an integer type, got {:?}.",
                                    get_reg_unref!(registers, dest, self)
                                )),
                                chunk,
                            ))
                        }
                    };
                    self.set_register(registers, dest as usize, val);
                }
                DEC => {
                    let (dest, i) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = match get_reg_unref!(registers, dest, self) {
                        Value::Byte(v) => Value::Byte(v - i as u8),
                        Value::Int(v) => Value::Int(v - i as i64),
                        Value::UInt(v) => Value::UInt(v - i as u64),
                        _ => {
                            return Err((
                                VMError::new_vm(format!(
                                    "DEC: Can only DEC an integer type, got {:?}.",
                                    get_reg_unref!(registers, dest, self)
                                )),
                                chunk,
                            ))
                        }
                    };
                    self.set_register(registers, dest as usize, val);
                }
                CONS => {
                    let (dest, op2, op3) = decode3!(chunk.code, &mut self.ip, wide);
                    let car = get_reg_unref!(registers, op2, self);
                    let cdr = get_reg_unref!(registers, op3, self);
                    let pair = self.alloc_pair(car, cdr);
                    self.set_register(registers, dest as usize, pair);
                }
                CAR => {
                    let (dest, op) = decode2!(chunk.code, &mut self.ip, wide);
                    let op = get_reg_unref!(registers, op, self);
                    match op {
                        Value::Pair(_) | Value::List(_, _) => {
                            let (car, _) = op.get_pair(self).expect("Pair not a pair?");
                            self.set_register(registers, dest as usize, car);
                        }
                        Value::Nil => self.set_register(registers, dest as usize, Value::Nil),
                        _ => {
                            return Err((
                                VMError::new_vm(format!(
                                    "CAR: Not a pair/conscell. {}",
                                    op.display_value(self)
                                )),
                                chunk,
                            ))
                        }
                    }
                }
                CDR => {
                    let (dest, op) = decode2!(chunk.code, &mut self.ip, wide);
                    let op = get_reg_unref!(registers, op, self);
                    match op {
                        Value::Pair(_) | Value::List(_, _) => {
                            let (_, cdr) = op.get_pair(self).expect("Pair not a pair?");
                            self.set_register(registers, dest as usize, cdr);
                        }
                        Value::Nil => self.set_register(registers, dest as usize, Value::Nil),
                        _ => return Err((VMError::new_vm("CDR: Not a pair/conscell."), chunk)),
                    }
                }
                LIST => {
                    self.pause_gc();
                    let r = self
                        .list(&chunk.code[..], registers, wide)
                        .map_err(|e| (e, chunk.clone()));
                    self.unpause_gc();
                    r?
                }
                APND => {
                    self.pause_gc();
                    let r = self
                        .append(&chunk.code[..], registers, wide)
                        .map_err(|e| (e, chunk.clone()));
                    self.unpause_gc();
                    r?;
                }
                XAR => self
                    .xar(&chunk.code[..], registers, wide)
                    .map_err(|e| (e, chunk.clone()))?,
                XDR => self
                    .xdr(&chunk.code[..], registers, wide)
                    .map_err(|e| (e, chunk.clone()))?,
                VEC => {
                    let (dest, start, end) = decode3!(chunk.code, &mut self.ip, wide);
                    if end == start {
                        let vh = self.alloc_vector(Vec::new());
                        self.set_register(registers, dest as usize, vh);
                    } else {
                        let mut v = Vec::new();
                        for i in start..end {
                            v.push(get_reg_unref!(registers, i, self));
                        }
                        let vh = self.alloc_vector(v);
                        self.set_register(registers, dest as usize, vh);
                    }
                }
                VECMK => {
                    let (dest, op) = decode2!(chunk.code, &mut self.ip, wide);
                    let len = get_reg_unref!(registers, op, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    let val = self.alloc_vector(Vec::with_capacity(len as usize));
                    self.set_register(registers, dest as usize, val);
                }
                VECELS => {
                    let (dest, op) = decode2!(chunk.code, &mut self.ip, wide);
                    let len = get_reg_unref!(registers, op, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    if let Value::Vector(h) = get_reg_unref!(registers, dest, self) {
                        let v = self.get_vector_mut(h).map_err(|e| (e, chunk.clone()))?;
                        v.resize(len as usize, Value::Undefined);
                    }
                }
                VECPSH => {
                    let (dest, op) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = get_reg_unref!(registers, op, self);
                    if let Value::Vector(h) = get_reg_unref!(registers, dest, self) {
                        let v = self.get_vector_mut(h).map_err(|e| (e, chunk.clone()))?;
                        v.push(val);
                    }
                }
                VECPOP => {
                    let (vc, dest) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = if let Value::Vector(h) = get_reg_unref!(registers, vc, self) {
                        let v = self.get_vector_mut(h).map_err(|e| (e, chunk.clone()))?;
                        if let Some(val) = v.pop() {
                            val
                        } else {
                            return Err((VMError::new_vm("VECPOP: Vector is empty."), chunk));
                        }
                    } else {
                        return Err((VMError::new_vm("VECPOP: Not a vector."), chunk.clone()));
                    };
                    self.set_register(registers, dest as usize, val);
                }
                VECNTH => {
                    let (vc, dest, i) = decode3!(chunk.code, &mut self.ip, wide);
                    let i = get_reg_unref!(registers, i, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))? as usize;
                    let val = match get_reg_unref!(registers, vc, self) {
                        Value::Vector(h) => {
                            let v = self.get_vector(h);
                            if let Some(val) = v.get(i) {
                                *val
                            } else {
                                return Err((
                                    VMError::new_vm(format!(
                                        "VECNTH: index out of bounds, {}/{}.",
                                        i,
                                        v.len()
                                    )),
                                    chunk,
                                ));
                            }
                        }
                        Value::List(h, start) => {
                            let v = self.get_vector(h);
                            if let Some(val) = v.get(start as usize + i) {
                                *val
                            } else {
                                return Err((
                                    VMError::new_vm(format!(
                                        "VECNTH: index out of bounds, {}/{}.",
                                        i,
                                        v.len()
                                    )),
                                    chunk,
                                ));
                            }
                        }
                        _ => {
                            return Err((VMError::new_vm("VECNTH: Not a vector."), chunk));
                        }
                    };
                    self.set_register(registers, dest as usize, val);
                }
                VECSTH => {
                    let (vc, src, i) = decode3!(chunk.code, &mut self.ip, wide);
                    let i = get_reg_unref!(registers, i, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))? as usize;
                    let val = get_reg_unref!(registers, src, self);
                    if let Value::Vector(h) = get_reg_unref!(registers, vc, self) {
                        let v = self.get_vector_mut(h).map_err(|e| (e, chunk.clone()))?;
                        if i >= v.len() {
                            return Err((VMError::new_vm("VECSTH: Index out of range."), chunk));
                        }
                        v[i] = val;
                    } else {
                        return Err((VMError::new_vm("VECSTH: Not a vector."), chunk));
                    };
                }
                VECMKD => {
                    let (dest, len, dfn) = decode3!(chunk.code, &mut self.ip, wide);
                    let len = get_reg_unref!(registers, len, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    let dfn = get_reg_unref!(registers, dfn, self);
                    let mut v = Vec::with_capacity(len as usize);
                    for _ in 0..len {
                        v.push(dfn);
                    }
                    let val = self.alloc_vector(v);
                    self.set_register(registers, dest as usize, val);
                }
                VECLEN => {
                    let (dest, v) = decode2!(chunk.code, &mut self.ip, wide);
                    match get_reg_unref!(registers, v, self) {
                        Value::Vector(h) => {
                            let v = self.get_vector(h);
                            let len = Value::UInt(v.len() as u64);
                            self.set_register(registers, dest as usize, len);
                        }
                        Value::List(h, start) => {
                            let v = self.get_vector(h);
                            let len = Value::UInt((v.len() - start as usize) as u64);
                            self.set_register(registers, dest as usize, len);
                        }
                        _ => return Err((VMError::new_vm("VECLEN: Not a vector."), chunk)),
                    }
                }
                VECCLR => {
                    let v = decode1!(chunk.code, &mut self.ip, wide);
                    if let Value::Vector(h) = get_reg_unref!(registers, v, self) {
                        let v = self.get_vector_mut(h).map_err(|e| (e, chunk.clone()))?;
                        v.clear();
                    }
                }
                STR => {
                    let (dest, reg1, reg2) = decode3!(chunk.code, &mut self.ip, wide);
                    let val = self
                        .mk_str(registers, reg1, reg2)
                        .map_err(|e| (e, chunk.clone()))?;
                    self.set_register(registers, dest as usize, val);
                }
                TYPE => {
                    let (dest, val) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = get_reg_unref!(registers, val, self);
                    let t = Value::StringConst(self.intern_static(val.display_type(self)));
                    self.set_register(registers, dest as usize, t);
                }
                _ => {
                    return Err((VMError::new_vm(format!("Invalid opcode {}", opcode)), chunk));
                }
            }
            if wide && opcode != WIDE {
                wide = false;
            }
        }
    }
}
