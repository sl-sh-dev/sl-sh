use crate::opcodes::*;
use crate::{
    from_i56, CallFrame, Chunk, Continuation, Error, GVm, VMError, VMErrorObj, VMResult, Value,
    STACK_CAP,
};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Arc;
use unicode_segmentation::UnicodeSegmentation;

impl<ENV> GVm<ENV> {
    #[inline]
    fn map_destructure(
        &mut self,
        //code: T,
        decodes: (u16, u16, u16),
        //wide: bool,
    ) -> VMResult<()> {
        let (dest, len, src) = decodes; //decode3!(code, wide);
        if len > 0 {
            let len = len as usize;
            let dest = dest as usize;
            let val = self.register(src as usize);
            match val {
                Value::Map(handle) => {
                    let map = self.get_map(handle);
                    for i in 0..len {
                        let key = self.register(dest + i);
                        if let Some(item) = map.get(&key) {
                            *self.register_mut(dest + i) = *item;
                        } else {
                            *self.register_mut(dest + i) = Value::Undefined;
                        }
                    }
                }
                Value::Vector(handle) => {
                    let vector = self.get_vector(handle);
                    for i in 0..len {
                        let key = self.register(dest + i);
                        if key.is_int() {
                            let key = key.get_int(self)?;
                            if key >= 0 && key < vector.len() as i64 {
                                *self.register_mut(dest + i) = vector[key as usize];
                            } else {
                                return Err(VMError::new_vm("seq key out of bounds"));
                            }
                        } else {
                            let mut iter = vector.iter();
                            *self.register_mut(dest + i) = Value::Undefined;
                            while let Some(v) = iter.next() {
                                if *v == key {
                                    if let Some(value) = iter.next() {
                                        *self.register_mut(dest + i) = *value;
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }
                Value::List(handle, idx) => {
                    let vector = &self.get_vector(handle)[idx as usize..];
                    for i in 0..len {
                        let key = self.register(dest + i);
                        if key.is_int() {
                            let key = key.get_int(self)?;
                            if key >= 0 && key < vector.len() as i64 {
                                *self.register_mut(dest + i) = vector[key as usize];
                            } else {
                                return Err(VMError::new_vm("seq key out of bounds"));
                            }
                        } else {
                            let mut iter = vector.iter();
                            *self.register_mut(dest + i) = Value::Undefined;
                            while let Some(v) = iter.next() {
                                if *v == key {
                                    if let Some(value) = iter.next() {
                                        *self.register_mut(dest + i) = *value;
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }
                Value::Pair(_) => {
                    for i in 0..len {
                        let key = self.register(dest + i);
                        if key.is_int() {
                            let key = key.get_int(self)?;
                            if key >= 0 {
                                let mut iter = val.iter(self);
                                if let Some(item) = iter.nth(key as usize) {
                                    *self.register_mut(dest + i) = item;
                                } else {
                                    return Err(VMError::new_vm("seq key out of bounds"));
                                }
                            } else {
                                return Err(VMError::new_vm("seq key out of bounds"));
                            }
                        } else {
                            let mut iter = val.iter(self);
                            *self.register_mut(dest + i) = Value::Undefined;
                            while let Some(v) = iter.next() {
                                if v == key {
                                    if let Some(value) = iter.next() {
                                        *self.register_mut(dest + i) = value;
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }
                Value::Nil => {
                    for i in 0..len {
                        *self.register_mut(dest + i) = Value::Undefined;
                    }
                }
                _ => return Err(VMError::new_vm("not a map")),
            }
        }
        Ok(())
    }

    fn get_line(&self, wide: bool, chunk: &Chunk) -> Option<u32> {
        if wide {
            unsafe { chunk.offset_to_line(self.ip_ptr.offset_from(get_code!(chunk)) as usize - 4) }
        } else {
            unsafe { chunk.offset_to_line(self.ip_ptr.offset_from(get_code!(chunk)) as usize - 2) }
        }
    }

    fn get_string_idx(&mut self, val: Value, i: u16) -> VMResult<Value> {
        let s = match val {
            Value::StringConst(it) => self.get_interned(it),
            Value::String(h) => self.get_string(h),
            _ => panic!("Invalid call to get_string_idx!"),
        };
        let idx = self.register_int(i as usize)?;
        let mut iter = UnicodeSegmentation::graphemes(s, true);
        let ch = if idx >= 0 {
            iter.nth(idx as usize)
        } else {
            iter.rev().nth(((0 - idx) - 1) as usize)
        };
        let v = if let Some(ch) = ch {
            // borrow checker won;t let us use self.alloc_char(ch) here....
            let len = ch.len();
            if len <= 6 {
                let mut b = [0_u8; 6];
                b[0..len].copy_from_slice(ch.as_bytes());
                Value::CharCluster(len as u8, b)
            } else {
                let h = self.alloc_string(ch.to_string());
                Value::CharClusterLong(h.get_handle().expect("just allocated, missing handle!"))
            }
        } else {
            return Err(VMError::new_vm("GET: Index out of range."));
        };
        Ok(v)
    }

    fn get(&mut self, wide: bool) -> VMResult<()> {
        let (dest, data, i) = decode3!(self.ip_ptr, wide);
        let data = self.register(data as usize);
        let val = match data {
            Value::Vector(h) => {
                let v = self.get_vector(h);
                let idx = self.register_int(i as usize)?;
                let idx = if idx >= 0 { idx } else { v.len() as i64 + idx };
                if idx < 0 {
                    let iv = idx.into();
                    self.make_err("vm-missing", iv)
                } else if let Some(val) = v.get(idx as usize) {
                    *val
                } else {
                    let iv = idx.into();
                    self.make_err("vm-missing", iv)
                }
            }
            Value::List(h, start) => {
                let v = self.get_vector(h);
                let idx = self.register_int(i as usize)?;
                if idx < 0 {
                    let iv = idx.into();
                    self.make_err("vm-missing", iv)
                } else if let Some(val) = v.get(start as usize + idx as usize) {
                    *val
                } else {
                    let iv = idx.into();
                    self.make_err("vm-missing", iv)
                }
            }
            Value::Pair(_) => {
                let idx = self.register_int(i as usize)?;
                if idx >= 0 {
                    let idx = idx as usize;
                    if let Some((mut car_out, mut cdr)) = data.get_pair(self) {
                        for _ in 0..idx {
                            if let Some((car, cdr_in)) = cdr.get_pair(self) {
                                car_out = car;
                                cdr = cdr_in;
                            } else {
                                let iv = (idx as i64).into();
                                car_out = self.make_err("vm-missing", iv);
                            }
                        }
                        car_out
                    } else {
                        panic!("pair not a pair!")
                    }
                } else {
                    return Err(VMError::new_vm(format!(
                        "GET: list requires a positive index (pair), {}.",
                        i,
                    )));
                }
            }
            Value::Map(h) => {
                let map = self.get_map(h);
                let key = self.register(i as usize);
                if let Some(val) = map.get(&key) {
                    *val
                } else {
                    self.make_err("vm-missing", key)
                }
            }
            Value::StringConst(_) => self.get_string_idx(data, i)?,
            Value::String(_) => self.get_string_idx(data, i)?,
            Value::Error(_) => data, // Pass the error on (for stacked GETs).
            _ => {
                return Err(VMError::new_vm("GET: Not a compound data structure."));
            }
        };
        set_register!(self, dest as usize, val);
        Ok(())
    }

    fn set_data(&mut self, wide: bool) -> VMResult<()> {
        let (src, data, i) = decode3!(self.ip_ptr, wide);
        let data = self.register(data as usize);
        let src = self.register(src as usize);
        match data {
            Value::Vector(h) => {
                let idx = self.register_int(i as usize)?;
                let v = self.get_vector_mut(h)?;
                let idx = if idx >= 0 { idx } else { v.len() as i64 + idx };
                if idx < 0 {
                    return Err(VMError::new_vm(format!(
                        "index out of bounds, {}/{}.",
                        i,
                        v.len()
                    )));
                }
                if let Some(slot) = v.get_mut(idx as usize) {
                    *slot = src;
                } else {
                    v.resize(idx as usize + 1, Value::Nil);
                    if let Some(slot) = v.get_mut(idx as usize) {
                        *slot = src;
                    } else {
                        return Err(VMError::new_vm(
                            "Vector, index out of bounds (unable to grow vector).",
                        ));
                    }
                }
            }
            Value::List(h, start) => {
                let idx = self.register_int(i as usize)?;
                let v = self.get_vector_mut(h)?;
                if idx < 0 {
                    return Err(VMError::new_vm(format!(
                        "index out of bounds, {}/{}.",
                        i,
                        v.len() - start as usize,
                    )));
                }
                if let Some(slot) = v.get_mut(start as usize + idx as usize) {
                    *slot = src;
                } else {
                    return Err(VMError::new_vm("index out of bounds"));
                }
            }
            Value::Pair(_) => {
                let idx = self.register_int(i as usize)?;
                if idx >= 0 {
                    let idx = idx as usize;
                    let mut last_cdr = data;
                    for _ in 0..idx {
                        if let Some((_car, cdr_in)) = last_cdr.get_pair(self) {
                            last_cdr = cdr_in;
                        } else {
                            return Err(VMError::new_vm(format!(
                                "index out of bounds (pair), {}.",
                                i,
                            )));
                        }
                    }
                    match &last_cdr {
                        Value::Pair(handle) => {
                            let (car, _) = self.get_pair_mut(*handle)?;
                            *car = src;
                        }
                        Value::List(_, _) => return Err(VMError::new_vm("pair is read only")),
                        _ => return Err(VMError::new_vm("not a pair/conscell")),
                    }
                } else {
                    return Err(VMError::new_vm(format!(
                        "list requires a positive index (pair), {}.",
                        i,
                    )));
                }
            }
            Value::Map(h) => {
                let key = self.register(i as usize);
                let map = self.get_map_mut(h)?;
                let slot = map.entry(key);
                *slot.or_insert(Value::Undefined) = src;
            }
            _ => {
                return Err(VMError::new_vm(format!(
                    "Not a compound data structure: {}.",
                    data.display_type(self)
                )));
            }
        }
        Ok(())
    }

    // Some macro expansions trips this.
    #[allow(clippy::redundant_closure_call)]
    pub(super) fn exec_loop(&mut self, chunk: Arc<Chunk>) -> Result<(), (VMError, Arc<Chunk>)> {
        let _env: PhantomData<ENV>;
        self.make_registers();
        let mut chunk = chunk;
        self.ip_ptr = get_code!(chunk);
        let mut wide = false;
        // Clean up the working regs we are about to use.
        if chunk.extra_regs > 0 {
            let regs = unsafe {
                std::slice::from_raw_parts_mut(
                    self.stack.add(self.stack_top),
                    STACK_CAP - self.stack_top,
                )
            };
            for reg in regs
                .iter_mut()
                .skip(chunk.input_regs)
                .take(chunk.extra_regs + 1)
            {
                *reg = Value::Undefined;
            }
        }
        let mut opcode = NOP;
        loop {
            if wide && opcode != WIDE {
                wide = false;
            }
            self.current_ip_ptr = self.ip_ptr;
            opcode = decode_u8!(self.ip_ptr);
            match opcode {
                NOP => {}
                HALT => {
                    return Err((VMError::new_vm("HALT: VM halted and on fire!"), chunk));
                }
                RET => {
                    if let Some(defer) = self.defers.pop() {
                        let first_reg = (chunk.input_regs + chunk.extra_regs + 1) as u16;
                        self.ip_ptr = self.current_ip_ptr;
                        chunk = self.make_call(defer, chunk, first_reg, 0, false)?;
                        self.make_registers();
                    } else {
                        // Clear used regs to make sure no closures or globals get overwritten later.
                        for r in self.stack_top + 1..=self.stack_max {
                            *self.stack_mut(r) = Value::Undefined;
                        }
                        if let Some(frame) = self.call_frame() {
                            let stack_top = frame.stack_top;
                            let ip_ptr = frame.ip;
                            let current_ip = frame.current_ip;
                            let this_fn = frame.this_fn;
                            let on_error = frame.on_error;
                            chunk = frame.chunk.clone();
                            self.copy_frame_defers(); // Do this BEFORE we change stack_top...
                            self.stack_top = stack_top;
                            self.make_registers();
                            self.stack_max = self.stack_top + chunk.input_regs + chunk.extra_regs;
                            self.ip_ptr = ip_ptr;
                            self.current_ip_ptr = current_ip;
                            self.this_fn = this_fn;
                            self.on_error = on_error;
                        } else {
                            return Ok(());
                        }
                    }
                }
                SRET => {
                    if let Some(defer) = self.defers.pop() {
                        let first_reg = (chunk.input_regs + chunk.extra_regs + 1) as u16;
                        self.ip_ptr = self.current_ip_ptr;
                        chunk = self.make_call(defer, chunk, first_reg, 0, false)?;
                        self.make_registers();
                    } else {
                        let src = decode1!(self.ip_ptr, wide);
                        let val = self.register(src as usize);
                        let old_top = self.stack_top;
                        // Clear used regs to make sure no closures or globals get overwritten later.
                        for r in self.stack_top + 1..=self.stack_max {
                            *self.stack_mut(r) = Value::Undefined;
                        }
                        if let Some(frame) = self.call_frame() {
                            let stack_top = frame.stack_top;
                            let ip_ptr = frame.ip;
                            let current_ip = frame.current_ip;
                            let this_fn = frame.this_fn;
                            let on_error = frame.on_error;
                            chunk = frame.chunk.clone();
                            self.copy_frame_defers(); // Do this BEFORE we change stack_top...
                            self.stack_top = stack_top;
                            self.make_registers();
                            self.stack_max = self.stack_top + chunk.input_regs + chunk.extra_regs;
                            self.ip_ptr = ip_ptr;
                            self.current_ip_ptr = current_ip;
                            self.this_fn = this_fn;
                            self.on_error = on_error;
                        } else {
                            *self.stack_mut(old_top) = val;
                            return Ok(());
                        }
                        *self.stack_mut(old_top) = val;
                    }
                }
                WIDE => wide = true,
                MOV => {
                    let (dest, src) = decode2!(self.ip_ptr, wide);
                    // XXX TODO- figure out proper mov symantics...
                    let val = self.register_unref(src as usize);
                    //let val = self.register(src as usize);
                    mov_register!(self, dest as usize, val);
                }
                MOVI => {
                    let (dest, src) = decode2!(self.ip_ptr, wide);
                    let val = self.register_unref(src as usize);
                    let dest = self
                        .register_int(dest as usize)
                        .map_err(|e| (e, chunk.clone()))?;
                    mov_register!(self, dest as usize, val);
                }
                MOVII => {
                    let (dest, src) = decode2!(self.ip_ptr, wide);
                    let src = self
                        .register_int(src as usize)
                        .map_err(|e| (e, chunk.clone()))?;
                    let val = self.register_unref(src as usize);
                    mov_register!(self, dest as usize, val);
                }
                GET => self.get(wide).map_err(|e| (e, chunk.clone()))?,
                SETCOL => self.set_data(wide).map_err(|e| (e, chunk.clone()))?,
                BMOV => {
                    let (dest, src, len) = decode3!(self.ip_ptr, wide);
                    for i in 0..len as usize {
                        mov_register!(self, dest as usize + i, self.register(src as usize + i));
                    }
                }
                LDSC => {
                    let (dest, len, src) = decode3!(self.ip_ptr, wide);
                    if len > 0 {
                        let len = len as usize;
                        let dest = dest as usize;
                        let val = self.register(src as usize);
                        match val {
                            Value::Vector(_) | Value::Pair(_) | Value::List(_, _) => {
                                let mut iter = val.iter(self);
                                for i in 0..len {
                                    if let Some(item) = iter.next() {
                                        *self.register_mut(dest + i) = item;
                                    } else {
                                        *self.register_mut(dest + i) = Value::Undefined;
                                    }
                                }
                            }
                            _ => return Err((VMError::new_vm("not a sequence"), chunk)),
                        }
                    }
                }
                LDSCR => {
                    let (dest, len, src) = decode3!(self.ip_ptr, wide);
                    if len > 0 {
                        let len = len as usize;
                        let dest = dest as usize;
                        let val = self.register(src as usize);
                        match val {
                            Value::Vector(_) | Value::Pair(_) | Value::List(_, _) => {
                                let mut iter = val.iter(self);
                                for i in 0..len - 1 {
                                    if let Some(item) = iter.next() {
                                        *self.register_mut(dest + i) = item;
                                    } else {
                                        *self.register_mut(dest + i) = Value::Undefined;
                                    }
                                }
                                let rest: Vec<Value> = iter.collect();
                                if rest.is_empty() {
                                    *self.register_mut(dest + (len - 1)) = Value::Nil;
                                } else {
                                    *self.register_mut(dest + (len - 1)) = self.alloc_list_ro(rest);
                                }
                            }
                            _ => return Err((VMError::new_vm("not a sequence"), chunk)),
                        }
                    }
                }
                MDSC => {
                    let decodes = decode3!(self.ip_ptr, wide);
                    self.map_destructure(decodes)
                        .map_err(|e| (e, chunk.clone()))?;
                }
                COPY => {
                    let (_dest, _src) = decode2!(self.ip_ptr, wide);
                    // XXX Deep copy src to dest
                }
                FRZ => {
                    let target = decode1!(self.ip_ptr, wide);
                    let target = self.register(target as usize);
                    self.heap_mut().immutable(target);
                }
                SET => {
                    let (dest, src) = decode2!(self.ip_ptr, wide);
                    let val = self.register_unref(src as usize);
                    set_register!(self, dest as usize, val);
                }
                CONST => {
                    let (dest, src) = decode2!(self.ip_ptr, wide);
                    let val = chunk.constants[src as usize];
                    set_register!(self, dest as usize, val);
                }
                DEF => {
                    let src = decode1!(self.ip_ptr, wide);
                    let idx = if wide {
                        decode_u32!(self.ip_ptr)
                    } else {
                        decode_u16!(self.ip_ptr) as u32
                    };
                    let val = self.register(src as usize);
                    self.set_global(idx, val);
                }
                DEFV => {
                    let src = decode1!(self.ip_ptr, wide);
                    let idx = if wide {
                        decode_u32!(self.ip_ptr)
                    } else {
                        decode_u16!(self.ip_ptr) as u32
                    };
                    let val = self.register(src as usize);
                    if let Value::Undefined = self.globals.get(idx) {
                        self.set_global(idx, val);
                    }
                }
                REFI => {
                    let dest = decode1!(self.ip_ptr, wide);
                    let idx = if wide {
                        decode_u32!(self.ip_ptr)
                    } else {
                        decode_u16!(self.ip_ptr) as u32
                    };
                    mov_register!(self, dest as usize, self.globals.get(idx));
                }
                CLRREG => {
                    let dest = decode1!(self.ip_ptr, wide);
                    mov_register!(self, dest as usize, Value::Undefined);
                }
                REGT => {
                    let dest = decode1!(self.ip_ptr, wide);
                    set_register!(self, dest as usize, Value::True);
                }
                REGF => {
                    let dest = decode1!(self.ip_ptr, wide);
                    set_register!(self, dest as usize, Value::False);
                }
                REGN => {
                    let dest = decode1!(self.ip_ptr, wide);
                    set_register!(self, dest as usize, Value::Nil);
                }
                REGC => {
                    let dest = decode1!(self.ip_ptr, wide);
                    set_register!(self, dest as usize, Value::Undefined);
                }
                REGB => {
                    let (dest, i) = decode2!(self.ip_ptr, wide);
                    set_register!(self, dest as usize, Value::Byte(i as u8));
                }
                REGI => {
                    let (dest, i) = decode2!(self.ip_ptr, wide);
                    let i: Value = (i as i64).into();
                    set_register!(self, dest as usize, i);
                }
                CLOSE => {
                    let (dest, src) = decode2!(self.ip_ptr, wide);
                    //let lambda = self.register(src as usize);
                    let lambda = self.register_unref(src as usize);
                    let (lambda, caps) = if let Value::Lambda(h) = lambda {
                        let l = self.heap().get_lambda(h);
                        let mut caps = Vec::new();
                        if let Some(captures) = &l.captures {
                            for c in captures {
                                let r = self.register(*c as usize);
                                if let Value::Value(b) = r {
                                    caps.push(b);
                                } else {
                                    let val = self.new_upval(r);
                                    mov_register!(self, *c as usize, val);
                                    caps.push(val.get_handle().unwrap());
                                }
                            }
                        }
                        (l, caps)
                    } else {
                        return Err((
                            VMError::new_vm(format!("CLOSE: requires a lambda, got {lambda:?}.")),
                            chunk,
                        ));
                    };
                    let new_closure = self.alloc_closure(lambda, caps);
                    set_register!(self, dest as usize, new_closure);
                }
                CALL => {
                    let (lambda, num_args, first_reg) = decode3!(self.ip_ptr, wide);
                    let lambda = self.register(lambda as usize);
                    chunk = self.make_call(lambda, chunk, first_reg, num_args, false)?;
                    self.make_registers();
                }
                CALLG => {
                    let idx = if wide {
                        decode_u32!(self.ip_ptr)
                    } else {
                        decode_u16!(self.ip_ptr) as u32
                    };
                    let (num_args, first_reg) = decode2!(self.ip_ptr, wide);
                    let lambda = self.get_global(idx);
                    chunk = self.make_call(lambda, chunk, first_reg, num_args, false)?;
                    self.make_registers();
                }
                TCALL => {
                    let (lambda, num_args) = decode2!(self.ip_ptr, wide);
                    let lambda = self.register(lambda as usize);
                    chunk = self.make_call(lambda, chunk, 0, num_args, true)?;
                    self.make_registers(); // In case of a builtin call
                }
                TCALLG => {
                    let idx = if wide {
                        decode_u32!(self.ip_ptr)
                    } else {
                        decode_u16!(self.ip_ptr) as u32
                    };
                    let num_args = decode1!(self.ip_ptr, wide);
                    let lambda = self.get_global(idx);
                    chunk = self.make_call(lambda, chunk, 0, num_args, true)?;
                    self.make_registers(); // In case of a builtin call
                }
                CALLM => {
                    let (num_args, first_reg) = decode2!(self.ip_ptr, wide);
                    if let Some(this_fn) = self.this_fn {
                        chunk = self.make_call(this_fn, chunk, first_reg, num_args, false)?;
                        self.make_registers();
                    } else {
                        let line = self.get_line(wide, &chunk);
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
                    let num_args = decode1!(self.ip_ptr, wide);
                    if let Some(this_fn) = self.this_fn {
                        chunk = self.make_call(this_fn, chunk, 0, num_args, true)?;
                        self.make_registers(); // In case of a builtin call
                    } else {
                        let line = self.get_line(wide, &chunk);
                        return Err((
                            VMError::new_vm(format!(
                                "TCALLM: Not in an existing lambda call, line {}.",
                                line.unwrap_or(0)
                            )),
                            chunk,
                        ));
                    }
                }
                JMP => {
                    let jmp = decode1!(self.ip_ptr, wide);
                    self.ip_ptr = get_code_at!(chunk, chunk.jump_table[jmp as usize] as isize);
                }
                JMPT => {
                    let (test, jmp) = decode2!(self.ip_ptr, wide);
                    if self.register(test as usize).is_truethy() {
                        self.ip_ptr = get_code_at!(chunk, chunk.jump_table[jmp as usize] as isize);
                    }
                }
                JMPF => {
                    let (test, jmp) = decode2!(self.ip_ptr, wide);
                    if self.register(test as usize).is_falsey() {
                        self.ip_ptr = get_code_at!(chunk, chunk.jump_table[jmp as usize] as isize);
                    }
                }
                JMPEQ => {
                    let (op1, op2, jmp) = decode3!(self.ip_ptr, wide);
                    let op1 = self
                        .register(op1 as usize)
                        .get_int(self)
                        .map_err(|e| (e, chunk.clone()))?;
                    let op2 = self
                        .register(op2 as usize)
                        .get_int(self)
                        .map_err(|e| (e, chunk.clone()))?;
                    if op1 == op2 {
                        self.ip_ptr = get_code_at!(chunk, chunk.jump_table[jmp as usize] as isize);
                    }
                }
                JMPLT => {
                    let (op1, op2, jmp) = decode3!(self.ip_ptr, wide);
                    let op1 = self
                        .register_int(op1 as usize)
                        .map_err(|e| (e, chunk.clone()))?;
                    let op2 = self
                        .register_int(op2 as usize)
                        .map_err(|e| (e, chunk.clone()))?;
                    if op1 < op2 {
                        self.ip_ptr = get_code_at!(chunk, chunk.jump_table[jmp as usize] as isize);
                    }
                }
                JMPGT => {
                    let (op1, op2, jmp) = decode3!(self.ip_ptr, wide);
                    let op1 = self
                        .register_int(op1 as usize)
                        .map_err(|e| (e, chunk.clone()))?;
                    let op2 = self
                        .register_int(op2 as usize)
                        .map_err(|e| (e, chunk.clone()))?;
                    if op1 > op2 {
                        self.ip_ptr = get_code_at!(chunk, chunk.jump_table[jmp as usize] as isize);
                    }
                }
                JMPU => {
                    let (test, jmp) = decode2!(self.ip_ptr, wide);
                    if self.register(test as usize).is_undef() {
                        self.ip_ptr = get_code_at!(chunk, chunk.jump_table[jmp as usize] as isize);
                    }
                }
                JMPNU => {
                    let (test, jmp) = decode2!(self.ip_ptr, wide);
                    if !self.register(test as usize).is_undef() {
                        self.ip_ptr = get_code_at!(chunk, chunk.jump_table[jmp as usize] as isize);
                    }
                }
                JMPRU => {
                    let (test, len, jmp) = decode3!(self.ip_ptr, wide);
                    if len > 0 {
                        let test = test as usize;
                        let len = len as usize;
                        for i in 0..len {
                            if self.register(test + i).is_undef() {
                                self.ip_ptr =
                                    get_code_at!(chunk, chunk.jump_table[jmp as usize] as isize);
                                break;
                            }
                        }
                    }
                }
                JMPRNU => {
                    let (test, len, jmp) = decode3!(self.ip_ptr, wide);
                    if len > 0 {
                        let len = len as usize;
                        let test = test as usize;
                        let mut jump = true;
                        for i in 0..len {
                            if self.register(test + i).is_undef() {
                                jump = false;
                                break;
                            }
                        }
                        if jump {
                            self.ip_ptr =
                                get_code_at!(chunk, chunk.jump_table[jmp as usize] as isize);
                        }
                    }
                }
                EQ => {
                    let (dest, reg1, reg2) = decode3!(self.ip_ptr, wide);
                    let val = self.is_eq(reg1, reg2).map_err(|e| (e, chunk.clone()))?;
                    set_register!(self, dest as usize, val);
                }
                EQUAL => {
                    let (dest, reg1, reg2) = decode3!(self.ip_ptr, wide);
                    let val = self.is_equal(reg1, reg2).map_err(|e| (e, chunk.clone()))?;
                    set_register!(self, dest as usize, val);
                }
                ISERR => {
                    let (dest, testreg) = decode2!(self.ip_ptr, wide);
                    let test = self.register_unref(testreg as usize);
                    let val = if let Value::Error(_) = test {
                        Value::True
                    } else {
                        Value::False
                    };
                    set_register!(self, dest as usize, val);
                }
                ISOK => {
                    let (dest, testreg) = decode2!(self.ip_ptr, wide);
                    let test = self.register_unref(testreg as usize);
                    let val = if let Value::Error(_) = test {
                        Value::False
                    } else {
                        Value::True
                    };
                    set_register!(self, dest as usize, val);
                }
                NOT => {
                    let (dest, val) = decode2!(self.ip_ptr, wide);
                    let val = self.register(val as usize);
                    let res = if val.is_falsey() {
                        Value::True
                    } else {
                        Value::False
                    };
                    set_register!(self, dest as usize, res);
                }
                ERR => {
                    let (key, val) = decode2!(self.ip_ptr, wide);
                    let key = self.register(key as usize);
                    let val = self.register(val as usize);
                    let key_str = if let Value::Keyword(i) = key {
                        self.get_interned(i)
                    } else {
                        return Err((
                            VMError::new_vm(format!("ERR: key must be a keyword, got {key:?}.")),
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
                MKERR => {
                    let (dest, key, data) = decode3!(self.ip_ptr, wide);
                    let key = self.register(key as usize);
                    let data = self.register(data as usize);
                    let keyword = if let Value::Keyword(i) = key {
                        i
                    } else {
                        return Err((
                            VMError::new_vm(format!("MKERR: key must be a keyword, got {key:?}.")),
                            chunk,
                        ));
                    };
                    let err = Error { keyword, data };
                    let err = self.alloc_error(err);
                    set_register!(self, dest as usize, err);
                }
                CCC => {
                    let (lambda, first_reg) = decode2!(self.ip_ptr, wide);
                    let lambda = self.register(lambda as usize);
                    let mut defers = vec![Value::Undefined; self.defers.len()];
                    defers.copy_from_slice(&self.defers[..]);
                    let frame = CallFrame {
                        id: self.callframe_id, // This will duplicate the id for lambda ccc calls on purpose.
                        chunk: chunk.clone(),
                        ip: self.ip_ptr,
                        current_ip: self.current_ip_ptr,
                        stack_top: self.stack_top,
                        this_fn: self.this_fn,
                        defers,
                        on_error: self.on_error,
                        called: Value::Undefined,
                    };
                    let mut stack = Vec::with_capacity(self.stack_max);
                    stack.resize(self.stack_max, Value::Undefined);
                    stack[..].copy_from_slice(&self.stack_slice()[0..self.stack_max]);
                    let k = Continuation {
                        frame,
                        arg_reg: self.stack_top + first_reg as usize, //stack_len,
                        stack,
                    };
                    let k_obj = self.alloc_continuation(k);
                    mov_register!(self, (first_reg + 1) as usize, k_obj);
                    chunk = self.make_call(lambda, chunk, first_reg, 1, false)?;
                    self.make_registers();
                }
                DFR => {
                    let lambda = decode1!(self.ip_ptr, wide);
                    let lambda = self.register(lambda as usize);
                    self.defers.push(lambda);
                }
                DFRPOP => {
                    if let Some(defer) = self.defers.pop() {
                        let first_reg = (chunk.input_regs + chunk.extra_regs + 1) as u16;
                        chunk = self.make_call(defer, chunk, first_reg, 0, false)?;
                        self.make_registers();
                    }
                }
                ONERR => {
                    let on_error_reg = decode1!(self.ip_ptr, wide);
                    let on_error = self.register(on_error_reg as usize);
                    if let Some(oe) = self.on_error {
                        mov_register!(self, on_error_reg as usize, oe);
                    } else {
                        mov_register!(self, on_error_reg as usize, Value::Nil);
                    }
                    if let Value::Nil = on_error {
                        self.on_error = None;
                    } else {
                        self.on_error = Some(on_error);
                    }
                }
                ADD => binary_math!(self, chunk, self.ip_ptr, |a, b| a + b, wide),
                SUB => binary_math!(self, chunk, self.ip_ptr, |a, b| a - b, wide),
                MUL => binary_math!(self, chunk, self.ip_ptr, |a, b| a * b, wide),
                DIV => div_math!(self, chunk, self.ip_ptr, wide),
                NUMEQ => compare_int!(
                    self,
                    chunk,
                    self.ip_ptr,
                    |a, b| a == b,
                    |a: f64, b: f64| (a - b).abs() < f64::EPSILON,
                    wide,
                    true,
                    false
                ),
                NUMNEQ => compare_int!(
                    self,
                    chunk,
                    self.ip_ptr,
                    |a, b| a == b,
                    |a: f64, b: f64| (a - b).abs() < f64::EPSILON,
                    wide,
                    true,
                    true
                ),
                NUMLT => compare!(self, chunk, self.ip_ptr, |a, b| a < b, wide, true),
                NUMLTE => compare!(self, chunk, self.ip_ptr, |a, b| a <= b, wide, true),
                NUMGT => compare!(self, chunk, self.ip_ptr, |a, b| a > b, wide, true),
                NUMGTE => compare!(self, chunk, self.ip_ptr, |a, b| a >= b, wide, true),
                INC => {
                    let (dest, i) = decode2!(self.ip_ptr, wide);
                    match self.register(dest as usize) {
                        Value::Byte(v) => {
                            *self.register_mut(dest as usize) = Value::Byte(v + i as u8)
                        }
                        Value::Int(v) => {
                            let v = from_i56(&v);
                            *self.register_mut(dest as usize) = (v + i as i64).into()
                        }
                        _ => {
                            return Err((
                                VMError::new_vm(format!(
                                    "INC: Can only INC an integer type, got {:?}.",
                                    self.register(dest as usize)
                                )),
                                chunk,
                            ))
                        }
                    }
                }
                DEC => {
                    let (dest, i) = decode2!(self.ip_ptr, wide);
                    match self.register(dest as usize) {
                        Value::Byte(v) => {
                            *self.register_mut(dest as usize) = Value::Byte(v - i as u8)
                        }
                        Value::Int(v) => {
                            let v = from_i56(&v);
                            *self.register_mut(dest as usize) = (v - i as i64).into()
                        }
                        _ => {
                            return Err((
                                VMError::new_vm(format!(
                                    "DEC: Can only DEC an integer type, got {:?}.",
                                    self.register(dest as usize)
                                )),
                                chunk,
                            ))
                        }
                    }
                }
                CONS => {
                    let (dest, op2, op3) = decode3!(self.ip_ptr, wide);
                    let car = self.register(op2 as usize);
                    let cdr = self.register(op3 as usize);
                    let pair = self.alloc_pair(car, cdr);
                    set_register!(self, dest as usize, pair);
                }
                CAR => {
                    let (dest, op) = decode2!(self.ip_ptr, wide);
                    let op = self.register(op as usize);
                    match op {
                        Value::Pair(_) | Value::List(_, _) => {
                            let (car, _) = op.get_pair(self).expect("Pair not a pair?");
                            set_register!(self, dest as usize, car);
                        }
                        Value::Nil => set_register!(self, dest as usize, Value::Nil),
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
                    let (dest, op) = decode2!(self.ip_ptr, wide);
                    let op = self.register(op as usize);
                    match op {
                        Value::Pair(_) | Value::List(_, _) => {
                            let (_, cdr) = op.get_pair(self).expect("Pair not a pair?");
                            set_register!(self, dest as usize, cdr);
                        }
                        Value::Nil => set_register!(self, dest as usize, Value::Nil),
                        _ => return Err((VMError::new_vm("CDR: Not a pair/conscell."), chunk)),
                    }
                }
                LIST => {
                    self.pause_gc();
                    let r = self.list(wide).map_err(|e| (e, chunk.clone()));
                    self.unpause_gc();
                    r?
                }
                APND => {
                    self.pause_gc();
                    let r = self.append(wide).map_err(|e| (e, chunk.clone()));
                    self.unpause_gc();
                    r?;
                }
                XAR => self.xar(wide).map_err(|e| (e, chunk.clone()))?,
                XDR => self.xdr(wide).map_err(|e| (e, chunk.clone()))?,
                VEC => {
                    let (dest, start, end) = decode3!(self.ip_ptr, wide);
                    if end == start {
                        let vh = self.alloc_vector(Vec::new());
                        set_register!(self, dest as usize, vh);
                    } else {
                        let mut v = Vec::new();
                        for i in start..end {
                            v.push(self.register(i as usize));
                        }
                        let vh = self.alloc_vector(v);
                        set_register!(self, dest as usize, vh);
                    }
                }
                MAPMK => {
                    let (dest, start, end) = decode3!(self.ip_ptr, wide);
                    let map = if end == start {
                        HashMap::new()
                    } else if (end - start) % 2 != 0 {
                        return Err((
                            VMError::new_vm(
                                "make-hash: Invalid arguments (must be even, [key val]*)"
                                    .to_string(),
                            ),
                            chunk.clone(),
                        ));
                    } else {
                        let mut map = HashMap::new();
                        for i in (start..end).step_by(2) {
                            map.insert(self.register(i as usize), self.register(i as usize + 1));
                        }
                        map
                    };
                    let mh = self.alloc_map(map);
                    set_register!(self, dest as usize, mh);
                }
                VECMK => {
                    let (dest, op) = decode2!(self.ip_ptr, wide);
                    let len = self
                        .register(op as usize)
                        .get_int(self)
                        .map_err(|e| (e, chunk.clone()))?;
                    let val = self.alloc_vector(Vec::with_capacity(len as usize));
                    set_register!(self, dest as usize, val);
                }
                VECELS => {
                    let (dest, op) = decode2!(self.ip_ptr, wide);
                    let len = self
                        .register(op as usize)
                        .get_int(self)
                        .map_err(|e| (e, chunk.clone()))?;
                    if let Value::Vector(h) = self.register(dest as usize) {
                        let v = self
                            .heap_mut()
                            .get_vector_mut(h)
                            .map_err(|e| (e, chunk.clone()))?;
                        v.resize(len as usize, Value::Undefined);
                    }
                }
                VECPSH => {
                    let (dest, op) = decode2!(self.ip_ptr, wide);
                    let val = self.register(op as usize);
                    if let Value::Vector(h) = self.register(dest as usize) {
                        let v = self
                            .heap_mut()
                            .get_vector_mut(h)
                            .map_err(|e| (e, chunk.clone()))?;
                        v.push(val);
                    }
                }
                VECPOP => {
                    let (vc, dest) = decode2!(self.ip_ptr, wide);
                    let val = if let Value::Vector(h) = self.register(vc as usize) {
                        let v = self
                            .heap_mut()
                            .get_vector_mut(h)
                            .map_err(|e| (e, chunk.clone()))?;
                        if let Some(val) = v.pop() {
                            val
                        } else {
                            return Err((VMError::new_vm("VECPOP: Vector is empty."), chunk));
                        }
                    } else {
                        return Err((VMError::new_vm("VECPOP: Not a vector."), chunk));
                    };
                    set_register!(self, dest as usize, val);
                }
                VECMKD => {
                    let (dest, len, dfn) = decode3!(self.ip_ptr, wide);
                    let len = self
                        .register(len as usize)
                        .get_int(self)
                        .map_err(|e| (e, chunk.clone()))?;
                    let dfn = self.register(dfn as usize);
                    let mut v = Vec::with_capacity(len as usize);
                    for _ in 0..len {
                        v.push(dfn);
                    }
                    let val = self.alloc_vector(v);
                    set_register!(self, dest as usize, val);
                }
                LEN => {
                    let (dest, v) = decode2!(self.ip_ptr, wide);
                    let len = match self.register(v as usize) {
                        Value::String(h) => {
                            let mut len: i64 = 0;
                            for _ in UnicodeSegmentation::graphemes(self.get_string(h), true) {
                                len += 1;
                            }
                            len
                        }
                        Value::StringConst(i) => {
                            let mut len: i64 = 0;
                            for _ in UnicodeSegmentation::graphemes(self.get_interned(i), true) {
                                len += 1;
                            }
                            len
                        }
                        Value::Vector(h) => self.get_vector(h).len() as i64,
                        Value::List(h, i) => self.get_vector(h).len() as i64 - i as i64,
                        Value::Pair(h) => {
                            let mut len: i64 = 1;
                            let (_, mut cdr) = self.get_pair(h);
                            while let Value::Pair(h) = cdr {
                                let (_, c) = self.get_pair(h);
                                cdr = c;
                                len += 1;
                            }
                            len
                        }
                        Value::Map(h) => self.get_map(h).len() as i64,
                        Value::Nil | Value::False => 0,
                        _ => 1, /*Err(VMError::new_vm(format!(
                                    "len: net valid for value of type {}",
                                    val.display_type(vm)
                                ))),
                                */
                    };
                    set_register!(self, dest as usize, len.into());
                }
                CLR => {
                    let v = decode1!(self.ip_ptr, wide);
                    let val = self.register(v as usize);
                    match val {
                        Value::Vector(h) => {
                            let v = self
                                .heap_mut()
                                .get_vector_mut(h)
                                .map_err(|e| (e, chunk.clone()))?;
                            v.clear();
                        }
                        Value::Map(h) => {
                            self.get_map_mut(h).map_err(|e| (e, chunk.clone()))?.clear();
                        }
                        Value::String(h) => {
                            self.get_string_mut(h)
                                .map_err(|e| (e, chunk.clone()))?
                                .clear();
                        }
                        Value::StringConst(_) => {
                            return Err((
                                VMError::new_vm("can not clear a string const".to_string()),
                                chunk.clone(),
                            ));
                        }
                        _ => {
                            return Err((
                                VMError::new_vm(format!(
                                    "clr: net valid for value of type {}",
                                    val.display_type(self)
                                )),
                                chunk.clone(),
                            ))
                        }
                    }
                }
                STR => {
                    let (dest, reg1, reg2) = decode3!(self.ip_ptr, wide);
                    let val = self.mk_str(reg1, reg2).map_err(|e| (e, chunk.clone()))?;
                    set_register!(self, dest as usize, val);
                }
                TYPE => {
                    let (dest, val) = decode2!(self.ip_ptr, wide);
                    let val = self.register(val as usize);
                    let t = Value::Keyword(self.intern_static(val.display_type(self)));
                    set_register!(self, dest as usize, t);
                }
                _ => {
                    return Err((VMError::new_vm(format!("Invalid opcode {opcode}")), chunk));
                }
            }
            // Don't do stuff after match, do it at the top of the loop to avoid extra JMPs.
        }
    }
}
