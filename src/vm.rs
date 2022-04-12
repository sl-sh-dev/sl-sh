use std::sync::Arc;

use crate::chunk::*;
use crate::error::*;
use crate::heap::*;
use crate::interner::*;
use crate::opcodes::*;
use crate::value::*;

pub mod cons;
pub mod print;
pub mod storage;
#[macro_use]
pub mod macros;

const STACK_CAP: usize = 1024;

#[derive(Clone, Debug)]
pub struct Vm {
    interner: Interner,
    heap: Heap,
    stack: Vec<Value>,
    globals: Globals,
    this_fn: Option<Value>,
    on_error: Option<Value>,

    err_frame: Option<CallFrame>,
    stack_top: usize,
    stack_max: usize,
    ip: usize,
    current_ip: usize,
    callframe_id: usize,
    defers: Vec<Value>,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn new() -> Self {
        let globals = Globals::new();
        let mut stack = Vec::with_capacity(STACK_CAP);
        stack.resize(1024, Value::Undefined);
        Vm {
            interner: Interner::with_capacity(8192),
            heap: Heap::new(),
            stack,
            globals,
            this_fn: None,
            on_error: None,
            err_frame: None,
            stack_top: 0,
            stack_max: 0,
            ip: 0,
            current_ip: 0,
            callframe_id: 0,
            defers: Vec::new(),
        }
    }

    // Need to break the registers lifetime away from self or we can not do much...
    // The underlying stack should never be deleted or reallocated for the life
    // of Vm so this should be safe.
    fn make_registers(&mut self) -> &'static mut [Value] {
        unsafe { &mut *(&mut self.stack[self.stack_top..] as *mut [Value]) }
    }

    fn setup_rest(
        &mut self,
        chunk: &Arc<Chunk>,
        registers: &mut [Value],
        first_reg: u16,
        num_args: u16,
    ) -> (usize, Value) {
        let rest_reg = first_reg + chunk.args + chunk.opt_args;
        let v = if num_args < (chunk.args + chunk.opt_args) {
            Value::Nil
        } else {
            let mut last = Value::Nil;
            let rest_len = (num_args - (chunk.args + chunk.opt_args)) as usize + 1;
            for item in registers
                .iter()
                .take(rest_reg as usize + rest_len)
                .skip(rest_reg.into())
                .rev()
            {
                let old_last = last;
                last = self.alloc_pair(*item, old_last);
            }
            last
        };
        (rest_reg.into(), v)
    }

    fn k_unshared_stack(&self, stack_top: usize, k: &Continuation) -> Option<(usize, &Vec<Value>)> {
        if k.frame.stack_top >= stack_top {
            if let Value::CallFrame(h) = self.stack[stack_top] {
                let frame = self.heap.get_callframe(h);
                if let Value::CallFrame(k_h) = k.stack[stack_top] {
                    let k_frame = self.heap.get_callframe(k_h);
                    if frame.id != k_frame.id {
                        return Some((frame.stack_top, &frame.defers));
                    }
                } else {
                    return Some((frame.stack_top, &frame.defers));
                }
            }
        } else if let Value::CallFrame(h) = self.stack[stack_top] {
            let frame = self.heap.get_callframe(h);
            return Some((frame.stack_top, &frame.defers));
        }
        None
    }

    fn k_defers(&self, k: &Continuation) -> (bool, Option<usize>) {
        if !self.defers.is_empty() {
            return (true, None);
        }
        let mut stack_top = self.stack_top;
        while let Some((next_stack_top, defers)) = self.k_unshared_stack(stack_top, k) {
            if stack_top == next_stack_top {
                break;
            }
            if !defers.is_empty() {
                return if self.k_unshared_stack(next_stack_top, k).is_none() {
                    (false, Some(stack_top))
                } else {
                    (true, Some(stack_top))
                };
            }
            stack_top = next_stack_top;
        }
        (false, None)
    }

    fn make_call(
        &mut self,
        lambda: Value,
        chunk: Arc<Chunk>,
        registers: &mut [Value],
        first_reg: u16,
        num_args: u16,
        tail_call: bool,
    ) -> Result<Arc<Chunk>, (VMError, Arc<Chunk>)> {
        // Clear out the unused optional regs.
        // Will clear working set to avoid writing to globals or closures by accident.
        fn clear_opts(l: &Chunk, registers: &mut [Value], first_reg: u16, num_args: u16) {
            // First clear any optional arguments.
            if num_args < (l.args + l.opt_args) {
                for r in num_args..(l.args + l.opt_args) {
                    Vm::mov_register(
                        registers,
                        first_reg as usize + (r + 1) as usize,
                        Value::Undefined,
                    );
                }
            }
        }

        let mut do_cont = false;
        let result = match lambda {
            Value::Builtin(f) => {
                let last_reg = (first_reg + num_args + 1) as usize;
                let res = (f.func)(self, &registers[(first_reg + 1) as usize..last_reg])
                    .map_err(|e| (e, chunk.clone()))?;
                let res_reg = self.stack_top + first_reg as usize;
                if tail_call {
                    // Go to last call frame so SRET does not mess up the return of a builtin.
                    if let Some(frame) = self.call_frame_mut() {
                        // Need to break the call frame lifetime from self to avoid extra work.
                        // This is safe because the stack and heap are not touched so the reference is
                        // stable.  The unwrap() is OK because the frame can not be NULL.
                        let frame: &mut CallFrame =
                            unsafe { (frame as *mut CallFrame).as_mut().unwrap() };
                        self.stack_top = frame.stack_top;
                        self.stack_max =
                            self.stack_top + frame.chunk.input_regs + frame.chunk.extra_regs;
                        self.ip = frame.ip;
                        self.this_fn = frame.this_fn;
                        self.on_error = frame.on_error;
                        self.stack[res_reg] = res;
                        std::mem::swap(&mut self.defers, &mut frame.defers);
                        Ok(frame.chunk.clone())
                    } else {
                        self.stack[res_reg] = res;
                        Ok(chunk)
                    }
                } else {
                    self.stack[res_reg] = res;
                    Ok(chunk)
                }
            }
            Value::Lambda(handle) => {
                let l = self.heap.get_lambda(handle);
                if !tail_call {
                    let defers = std::mem::take(&mut self.defers);
                    let frame = CallFrame {
                        id: self.callframe_id,
                        chunk,
                        ip: self.ip,
                        current_ip: self.current_ip,
                        stack_top: self.stack_top,
                        this_fn: self.this_fn,
                        defers,
                        on_error: self.on_error,
                    };
                    self.callframe_id += 1;
                    Self::mov_register(registers, first_reg.into(), self.alloc_callframe(frame));
                    self.stack_top += first_reg as usize;
                }
                self.stack_max = self.stack_top + l.input_regs + l.extra_regs;
                self.this_fn = Some(lambda);
                self.ip = 0;
                if l.rest {
                    let (rest_reg, h) = self.setup_rest(&l, registers, first_reg, num_args);
                    Self::mov_register(registers, rest_reg, h);
                }
                // XXX TODO- double check num args.
                // XXX TODO- maybe test for stack overflow vs waiting for a panic.
                clear_opts(&l, registers, first_reg, num_args);
                Ok(l)
            }
            Value::Closure(handle) => {
                // Make a self (vm) with it's lifetime broken away so we can call setup_rest.
                // This is safe because it does not touch caps, it needs a &mut self so it
                // can heap allocate.
                let unsafe_vm: &mut Vm = unsafe { (self as *mut Vm).as_mut().unwrap() };
                let (l, caps) = self.heap.get_closure(handle);
                let frame = if !tail_call {
                    let defers = std::mem::take(&mut self.defers);
                    let frame = CallFrame {
                        id: self.callframe_id,
                        chunk,
                        ip: self.ip,
                        current_ip: self.current_ip,
                        stack_top: self.stack_top,
                        this_fn: self.this_fn,
                        defers,
                        on_error: self.on_error,
                    };
                    self.callframe_id += 1;
                    self.stack_top += first_reg as usize;
                    Some(frame)
                } else {
                    None
                };
                self.stack_max = self.stack_top + l.input_regs + l.extra_regs;
                self.this_fn = Some(lambda);
                self.ip = 0;
                let cap_first = (first_reg + l.args + l.opt_args + 1) as usize;
                if l.rest {
                    let (rest_reg, h) = unsafe_vm.setup_rest(&l, registers, first_reg, num_args);
                    for (i, c) in caps.iter().enumerate() {
                        Self::mov_register(registers, cap_first + i, Value::Value(*c));
                    }
                    Self::mov_register(registers, rest_reg, h);
                } else {
                    for (i, c) in caps.iter().enumerate() {
                        Self::mov_register(registers, cap_first + i, Value::Value(*c));
                    }
                }
                if let Some(frame) = frame {
                    Self::mov_register(registers, first_reg.into(), self.alloc_callframe(frame));
                }
                clear_opts(&l, registers, first_reg, num_args);
                Ok(l)
            }
            Value::Continuation(handle) => {
                let k = self.heap.get_continuation(handle);
                if num_args != 1 {
                    return Err((VMError::new_vm("Continuation takes one argument."), chunk));
                }
                let (defered, from) = self.k_defers(k);
                if let Some(from) = from {
                    // expect ok because this will be a call frame.
                    let frame = self.call_frame_mut_idx(from).expect("Invalid frame index!");
                    // Need to break the call frame lifetime from self to avoid extra work.
                    // This is safe because the stack and heap are not touched so the reference is
                    // stable.  The unwrap() is OK because the frame can not be NULL.
                    let frame: &mut CallFrame =
                        unsafe { (frame as *mut CallFrame).as_mut().unwrap() };
                    std::mem::swap(&mut self.defers, &mut frame.defers);
                }
                if defered {
                    if let Some(defer) = self.defers.pop() {
                        let first_reg = (chunk.input_regs + chunk.extra_regs + 1) as u16;
                        self.ip = self.current_ip;
                        self.make_call(defer, chunk, registers, first_reg, 0, false)
                    } else {
                        // If k_defers returns true than self.defers.pop() better
                        // return something.  Need this to make the borrow checker
                        // happy.
                        panic!("No defers but need a defer!");
                    }
                } else {
                    do_cont = true;
                    Ok(chunk)
                }
            }
            _ => Err((
                VMError::new_vm(format!("CALL: Not a callable {:?}.", lambda)),
                chunk,
            )),
        };
        if do_cont {
            // Had to break this out for continuations. Handling defers makes this necessary.
            match lambda {
                Value::Continuation(h) => {
                    let k = self.heap.get_continuation(h);
                    let arg = registers[first_reg as usize + 1];
                    self.stack[..k.stack.len()].copy_from_slice(&k.stack[..]);
                    self.stack[k.arg_reg] = arg;
                    self.stack_top = k.frame.stack_top;
                    self.stack_max =
                        self.stack_top + k.frame.chunk.input_regs + k.frame.chunk.extra_regs;
                    self.ip = k.frame.ip;
                    self.current_ip = k.frame.current_ip;
                    self.this_fn = k.frame.this_fn;
                    self.on_error = k.frame.on_error;
                    let chunk = k.frame.chunk.clone();
                    Ok(chunk)
                }
                _ => panic!("Must be a continuation!"),
            }
        } else {
            result
        }
    }

    fn mk_str(&mut self, registers: &mut [Value], reg1: u16, reg2: u16) -> VMResult<Value> {
        let mut val = String::new();
        for reg in reg1..=reg2 {
            let v = get_reg_unref!(registers, reg, self);
            val.push_str(&v.pretty_value(self));
        }
        let val = self.alloc_string(val.into());
        Ok(val)
    }

    fn is_eq(&self, registers: &mut [Value], reg1: u16, reg2: u16) -> VMResult<Value> {
        let mut val = Value::False;
        if reg1 == reg2 {
            val = Value::True;
        } else {
            //let mut val1 = get_reg!(registers, reg1);
            let mut val1 = get_reg_unref!(registers, reg1, self);
            for reg in reg1..reg2 {
                //let val2 = get_reg!(registers, reg + 1);
                let val2 = get_reg_unref!(registers, reg + 1, self);
                if val1 == val2 {
                    /*if unsafe {
                        std::mem::transmute::<Value, u128>(val1)
                            == std::mem::transmute::<Value, u128>(val2)
                    } {*/
                    val = Value::True;
                } else {
                    val = Value::False;
                    break;
                }
                val1 = val2;
            }
        };
        Ok(val)
    }

    fn is_equal_pair(&self, val1: Value, val2: Value) -> VMResult<Value> {
        let mut val = Value::False;
        if val1 == val2 {
            val = Value::True;
        } else if val1.is_int() && val2.is_int() {
            if val1.get_int()? == val2.get_int()? {
                val = Value::True;
            }
        } else if val1.is_number() && val2.is_number() {
            if (val1.get_float()? - val2.get_float()?).abs() < f64::EPSILON {
                val = Value::True;
            }
        } else {
            match val1 {
                Value::StringConst(s1) => {
                    if let Value::String(v2) = val2 {
                        let s2 = self.heap.get_string(v2);
                        if self.get_interned(s1) == s2 {
                            val = Value::True;
                        }
                    }
                }
                Value::String(h1) => {
                    let s1 = self.heap.get_string(h1);
                    if let Value::StringConst(s2) = val2 {
                        if s1 == self.get_interned(s2) {
                            val = Value::True;
                        }
                    }
                }
                Value::Vector(h1) => {
                    if let Value::Vector(h2) = val2 {
                        let v1 = self.heap.get_vector(h1);
                        let v2 = self.heap.get_vector(h2);
                        if v1.len() == v2.len() {
                            if v1.is_empty() {
                                val = Value::True;
                            } else {
                                for i in 0..v1.len() {
                                    val = self.is_equal_pair(v1[i], v2[i])?;
                                    if val == Value::False {
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
                Value::Bytes(h1) => {
                    if let Value::Bytes(h2) = val2 {
                        let b1 = self.heap.get_bytes(h1);
                        let b2 = self.heap.get_bytes(h2);
                        if b1.len() == b2.len() {
                            if b1.is_empty() {
                                val = Value::True;
                            } else {
                                for i in 0..b1.len() {
                                    if b1[i] == b2[i] {
                                        val = Value::True;
                                    } else {
                                        val = Value::False;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
                Value::Pair(h1) => {
                    // XXX use iterators to reduce recursion?
                    // Make sure pair iter will work for non-lists...
                    if let Value::Pair(h2) = val2 {
                        let (car1, cdr1) = self.heap.get_pair(h1);
                        let (car2, cdr2) = self.heap.get_pair(h2);
                        val = self.is_equal_pair(car1, car2)?;
                        if val == Value::True {
                            val = self.is_equal_pair(cdr1, cdr2)?;
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(val)
    }

    fn is_equal(&self, registers: &mut [Value], reg1: u16, reg2: u16) -> VMResult<Value> {
        let mut val = Value::False;
        if reg1 == reg2 {
            val = Value::True
        } else {
            let mut val1 = get_reg_unref!(registers, reg1, self);
            for reg in reg1..reg2 {
                let val2 = get_reg_unref!(registers, reg + 1, self);
                val = self.is_equal_pair(val1, val2)?;
                if val == Value::False {
                    break;
                }
                val1 = val2;
            }
        }
        Ok(val)
    }

    pub fn do_call(&mut self, chunk: Arc<Chunk>, params: &[Value]) -> VMResult<Value> {
        let stack_top = self.stack_top;
        let stack_max = self.stack_max;
        let ip = self.ip;
        let this_fn = self.this_fn;
        let on_error = self.on_error;
        self.this_fn = None;
        self.on_error = None;
        self.stack_top = self.stack_max;
        self.stack_max = self.stack_top + chunk.input_regs + chunk.extra_regs;
        self.stack[self.stack_top] = Value::UInt(params.len() as u64);
        if !params.is_empty() {
            self.stack[self.stack_top + 1..=params.len()].copy_from_slice(params);
        }
        if chunk.rest {
            let registers = self.make_registers();
            let (rest_reg, h) = self.setup_rest(&chunk, registers, 0, params.len() as u16);
            Self::mov_register(registers, rest_reg, h);
        }
        let res = if let Err(e) = self.execute2(chunk) {
            Err(e)
        } else {
            Ok(self.stack[0])
        };
        self.stack_top = stack_top;
        self.stack_max = stack_max;
        self.ip = ip;
        self.this_fn = this_fn;
        self.on_error = on_error;
        res
    }

    pub fn execute(&mut self, chunk: Arc<Chunk>) -> VMResult<()> {
        let stack_top = self.stack_top;
        let stack_max = self.stack_max;
        let ip = self.ip;
        let this_fn = self.this_fn;
        let on_error = self.on_error;
        self.this_fn = None;
        self.stack_top = self.stack_max;
        self.stack_max = self.stack_top + chunk.input_regs + chunk.extra_regs;

        let result = self.execute2(chunk);

        self.stack_top = stack_top;
        self.stack_max = stack_max;
        self.ip = ip;
        self.this_fn = this_fn;
        self.on_error = on_error;
        result
    }

    fn execute2(&mut self, chunk: Arc<Chunk>) -> VMResult<()> {
        let mut chunk = chunk;

        let mut done = false;
        let mut result = Ok(());
        while !done {
            result = if let Err((e, echunk)) = self.execute_internal(chunk.clone()) {
                self.err_frame = Some(CallFrame {
                    id: 0,
                    chunk: echunk,
                    stack_top: self.stack_top,
                    ip: self.ip,
                    current_ip: self.current_ip,
                    this_fn: self.this_fn,
                    defers: std::mem::take(&mut self.defers),
                    on_error: self.on_error,
                });
                if let Some(on_error) = self.on_error {
                    let registers = self.make_registers();
                    registers[1] = Value::Keyword(self.intern(e.key));
                    registers[2] = match &e.obj {
                        VMErrorObj::Message(msg) => Value::StringConst(self.intern(msg)),
                        VMErrorObj::Object(v) => *v,
                    };
                    self.on_error = None;
                    match self.make_call(on_error, chunk.clone(), registers, 0, 2, true) {
                        Ok(c) => {
                            chunk = c;
                            Err(e)
                        }
                        Err((ne, _c)) => {
                            done = true;
                            Err(ne)
                        }
                    }
                } else {
                    done = true;
                    Err(e)
                }
            } else {
                self.err_frame = None;
                done = true;
                Ok(())
            };
        }

        result
    }

    fn execute_internal(&mut self, chunk: Arc<Chunk>) -> Result<(), (VMError, Arc<Chunk>)> {
        let mut registers = self.make_registers();
        let mut chunk = chunk;
        self.ip = 0;
        let mut wide = false;
        // Clean up the working regs we are about to use.
        if chunk.extra_regs > 0 {
            for r in chunk.input_regs..chunk.input_regs + chunk.extra_regs {
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
                    } else if let Some(frame) = self.call_frame_mut() {
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
                            registers[old_top] = val;
                        } else {
                            registers[old_top] = val;
                            return Ok(());
                        }
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
                SET => {
                    let (dest, src) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = get_reg_unref!(registers, src, self);
                    //            self.set_register(registers, dest as usize, val);
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
                    Self::mov_register(registers, dest as usize, val);
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
                                get_reg!(registers, dest)
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
                                    Self::mov_register(registers, *c as usize, val);
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
                    Self::mov_register(registers, dest as usize, self.alloc_closure(lambda, caps));
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
                    let nip = decode1!(chunk.code, &mut self.ip, wide);
                    self.ip = nip as usize;
                }
                JMPF => {
                    let ipoff = decode1!(chunk.code, &mut self.ip, wide);
                    self.ip += ipoff as usize;
                }
                JMPB => {
                    let ipoff = decode1!(chunk.code, &mut self.ip, wide);
                    self.ip -= ipoff as usize;
                }
                JMPFT => {
                    let (test, ipoff) = decode2!(chunk.code, &mut self.ip, wide);
                    if get_reg_unref!(registers, test, self).is_truethy() {
                        self.ip += ipoff as usize;
                    }
                }
                JMPBT => {
                    let (test, ipoff) = decode2!(chunk.code, &mut self.ip, wide);
                    if get_reg_unref!(registers, test, self).is_truethy() {
                        self.ip -= ipoff as usize;
                    }
                }
                JMPFF => {
                    let (test, ipoff) = decode2!(chunk.code, &mut self.ip, wide);
                    if get_reg_unref!(registers, test, self).is_falsey() {
                        self.ip += ipoff as usize;
                    }
                }
                JMPBF => {
                    let (test, ipoff) = decode2!(chunk.code, &mut self.ip, wide);
                    if get_reg_unref!(registers, test, self).is_falsey() {
                        self.ip -= ipoff as usize;
                    }
                }
                JMP_T => {
                    let (test, nip) = decode2!(chunk.code, &mut self.ip, wide);
                    if get_reg_unref!(registers, test, self).is_truethy() {
                        self.ip = nip as usize;
                    }
                }
                JMP_F => {
                    let (test, nip) = decode2!(chunk.code, &mut self.ip, wide);
                    if get_reg_unref!(registers, test, self).is_falsey() {
                        self.ip = nip as usize;
                    }
                }
                JMPEQ => {
                    let (op1, op2, nip) = decode3!(chunk.code, &mut self.ip, wide);
                    let op1 = get_reg_unref!(registers, op1, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    let op2 = get_reg_unref!(registers, op2, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    if op1 == op2 {
                        self.ip = nip as usize;
                    }
                }
                JMPLT => {
                    let (op1, op2, nip) = decode3!(chunk.code, &mut self.ip, wide);
                    let op1 = get_reg_unref!(registers, op1, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    let op2 = get_reg_unref!(registers, op2, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    if op1 < op2 {
                        self.ip = nip as usize;
                    }
                }
                JMPGT => {
                    let (op1, op2, nip) = decode3!(chunk.code, &mut self.ip, wide);
                    let op1 = get_reg_unref!(registers, op1, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    let op2 = get_reg_unref!(registers, op2, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    if op1 > op2 {
                        self.ip = nip as usize;
                    }
                }
                JMPFU => {
                    let (test, ipoff) = decode2!(chunk.code, &mut self.ip, wide);
                    if get_reg_unref!(registers, test, self).is_undef() {
                        self.ip += ipoff as usize;
                    }
                }
                JMPBU => {
                    let (test, ipoff) = decode2!(chunk.code, &mut self.ip, wide);
                    if get_reg_unref!(registers, test, self).is_undef() {
                        self.ip -= ipoff as usize;
                    }
                }
                JMPFNU => {
                    let (test, ipoff) = decode2!(chunk.code, &mut self.ip, wide);
                    if !get_reg_unref!(registers, test, self).is_undef() {
                        self.ip += ipoff as usize;
                    }
                }
                JMPBNU => {
                    let (test, ipoff) = decode2!(chunk.code, &mut self.ip, wide);
                    if !get_reg_unref!(registers, test, self).is_undef() {
                        self.ip -= ipoff as usize;
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
                    let key = if let Value::Keyword(i) = key {
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
                                key,
                                obj: VMErrorObj::Message(self.get_interned(i).to_string()),
                            },
                            chunk,
                        ));
                    } else {
                        return Err((
                            VMError {
                                key,
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
                        Value::Pair(handle) => {
                            let (car, _) = self.heap.get_pair(handle);
                            self.set_register(registers, dest as usize, car);
                        }
                        Value::Nil => self.set_register(registers, dest as usize, Value::Nil),
                        _ => return Err((VMError::new_vm("CAR: Not a pair/conscell."), chunk)),
                    }
                }
                CDR => {
                    let (dest, op) = decode2!(chunk.code, &mut self.ip, wide);
                    let op = get_reg_unref!(registers, op, self);
                    match op {
                        Value::Pair(handle) => {
                            let (_, cdr) = self.heap.get_pair(handle);
                            self.set_register(registers, dest as usize, cdr);
                        }
                        Value::Nil => self.set_register(registers, dest as usize, Value::Nil),
                        _ => return Err((VMError::new_vm("CDR: Not a pair/conscell."), chunk)),
                    }
                }
                LIST => self
                    .list(&chunk.code[..], registers, wide)
                    .map_err(|e| (e, chunk.clone()))?,
                APND => self
                    .append(&chunk.code[..], registers, wide)
                    .map_err(|e| (e, chunk.clone()))?,
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
                        let v = self.get_vector_mut(h);
                        v.resize(len as usize, Value::Undefined);
                    }
                }
                VECPSH => {
                    let (dest, op) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = get_reg_unref!(registers, op, self);
                    if let Value::Vector(h) = get_reg_unref!(registers, dest, self) {
                        let v = self.get_vector_mut(h);
                        v.push(val);
                    }
                }
                VECPOP => {
                    let (vc, dest) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = if let Value::Vector(h) = get_reg_unref!(registers, vc, self) {
                        let v = self.get_vector_mut(h);
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
                    let val = if let Value::Vector(h) = get_reg_unref!(registers, vc, self) {
                        let v = self.get_vector_mut(h);
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
                    } else {
                        return Err((VMError::new_vm("VECNTH: Not a vector."), chunk));
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
                        let v = self.get_vector_mut(h);
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
                    if let Value::Vector(h) = get_reg_unref!(registers, v, self) {
                        let v = self.get_vector_mut(h);
                        let len = Value::UInt(v.len() as u64);
                        self.set_register(registers, dest as usize, len);
                    }
                }
                VECCLR => {
                    let v = decode1!(chunk.code, &mut self.ip, wide);
                    if let Value::Vector(h) = get_reg_unref!(registers, v, self) {
                        let v = self.get_vector_mut(h);
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

#[cfg(test)]
mod tests {
    use super::*;

    fn get_int(_vm: &Vm, val: &Value) -> VMResult<i64> {
        if let Value::Int(i) = val {
            Ok(*i)
        } else {
            Err(VMError::new_vm("Not an int"))
        }
    }

    fn is_nil(_vm: &Vm, val: &Value) -> VMResult<bool> {
        if let Value::Nil = val {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    #[test]
    fn test_list() -> VMResult<()> {
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        chunk.add_constant(Value::Int(1));
        chunk.add_constant(Value::Int(2));
        chunk.add_constant(Value::Int(3));
        chunk.add_constant(Value::Int(4));
        chunk.add_constant(Value::Nil);
        chunk.encode2(CONST, 0, 0, line).unwrap();
        chunk.encode2(CONST, 1, 1, line).unwrap();
        chunk.encode3(CONS, 1, 0, 1, line).unwrap();
        chunk.encode2(CDR, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        chunk.add_constant(Value::Nil);
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 2);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CAR, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 1);

        // car with nil
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, line).unwrap();
        chunk.encode2(CAR, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].is_nil());

        // car with nil on heap
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, line).unwrap();
        chunk.encode2(CAR, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].is_nil());

        // cdr with nil
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CDR, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].is_nil());

        // cdr with nil on heap
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, line).unwrap();
        chunk.encode2(CDR, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].is_nil());

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 2, line).unwrap();
        chunk.encode2(XAR, 1, 2, line).unwrap();
        chunk.encode2(CAR, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 3);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 3, line).unwrap();
        chunk.encode2(XDR, 1, 2, line).unwrap();
        chunk.encode2(CDR, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 4);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, line).unwrap();
        chunk.encode2(CONST, 3, 2, line).unwrap();
        chunk.encode2(XAR, 2, 3, line).unwrap();
        chunk.encode2(CAR, 0, 2, line).unwrap();
        chunk.encode2(CDR, 3, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 3);
        assert!(vm.stack[3].is_nil());

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, line).unwrap();
        chunk.encode2(CONST, 3, 3, line).unwrap();
        chunk.encode2(XDR, 2, 3, line).unwrap();
        chunk.encode2(CDR, 0, 2, line).unwrap();
        chunk.encode2(CAR, 3, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 4);
        assert!(vm.stack[3].is_nil());

        // Test a list with elements.
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 0, 0, line).unwrap();
        chunk.encode2(CONST, 1, 1, line).unwrap();
        chunk.encode2(CONST, 2, 2, line).unwrap();
        chunk.encode3(LIST, 0, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack.get(0).unwrap();
        if let Value::Pair(h) = result {
            let (car, cdr) = vm.heap.get_pair(*h);
            assert!(get_int(&vm, &car)? == 1);
            if let Value::Pair(h2) = cdr {
                let (car, cdr) = vm.heap.get_pair(h2);
                assert!(get_int(&vm, &car)? == 2);
                if let Value::Pair(h3) = cdr {
                    let (car, cdr) = vm.heap.get_pair(h3);
                    assert!(get_int(&vm, &car)? == 3);
                    assert!(is_nil(&vm, &cdr)?);
                } else {
                    assert!(false);
                }
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode3(LIST, 0, 1, 0, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack.get(0).unwrap();
        assert!(result.is_nil());
        Ok(())
    }

    #[test]
    fn test_store() -> VMResult<()> {
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        for i in 0..257 {
            chunk.add_constant(Value::Int(i as i64));
        }
        chunk.encode2(CONST, 0, 0, line).unwrap();
        chunk.encode2(CONST, 1, 255, line).unwrap();
        chunk.encode3(ADD, 0, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;

        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 255);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 1, 256, line).unwrap();
        chunk.encode3(ADD, 0, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;

        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 255 + 256);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(MOV, 1, 0, line).unwrap();
        chunk.encode0(RET, line)?;
        let result = vm.stack[1].get_int()?;
        assert!(result == 256);
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[1].get_int()?;
        assert!(result == 255 + 256);

        let mut vm = Vm::new();
        vm.stack[0] = vm.new_upval(Value::Int(1));
        vm.stack[1] = Value::Int(10);
        vm.stack[2] = Value::Int(1);
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(MOV, 1, 0, line).unwrap();
        chunk.encode3(ADD, 1, 1, 2, line).unwrap();
        chunk.encode3(ADD, 1, 1, 2, line).unwrap();
        chunk.encode3(ADD, 1, 1, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].unref(&vm).get_int()?;
        assert!(result == 1);
        let result = vm.stack[1].unref(&vm).get_int()?;
        assert!(result == 4);

        Ok(())
    }

    #[test]
    fn test_global() -> VMResult<()> {
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let mut vm = Vm::new();
        let sym = vm.intern("test_sym");
        let sym2 = vm.intern("test_symTWO");
        let slot = vm.globals.reserve(sym);
        let slot2 = vm.globals.reserve(sym2);
        let _const0 = chunk.add_constant(Value::Global(slot)) as u16;
        let const1 = chunk.add_constant(Value::Symbol(sym)) as u16;
        let const2 = chunk.add_constant(Value::Int(42)) as u16;
        let const3 = chunk.add_constant(Value::Global(slot2)) as u16;
        chunk.encode2(CONST, 0, const1, line)?;
        chunk.encode2(REF, 1, 0, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        assert!(vm.execute(chunk.clone()).is_err());

        vm.clear_err_frame();
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        vm.globals.set(slot, Value::Int(11));
        chunk.encode2(CONST, 0, const1, line)?;
        chunk.encode2(REF, 1, 0, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[1].unref(&vm).get_int()? == 11);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 0, const3, line)?;
        chunk.encode2(CONST, 1, const2, line)?;
        chunk.encode2(DEF, 0, 1, line)?;
        chunk.encode2(REF, 2, 0, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[2].unref(&vm).get_int()? == 42);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        vm.globals.set(slot, Value::Int(11));
        let slot = vm.globals.interned_slot(sym2).unwrap() as u32;
        let const1 = chunk.add_constant(Value::Global(slot)) as u16;
        let const2 = chunk.add_constant(Value::Int(43)) as u16;
        let const3 = chunk.add_constant(Value::Int(53)) as u16;
        let const4 = chunk.add_constant(Value::Symbol(sym2)) as u16;
        chunk.encode2(CONST, 0, const1, line)?;
        chunk.encode2(CONST, 1, const2, line)?;
        chunk.encode2(CONST, 3, const3, line)?;
        chunk.encode2(CONST, 4, const4, line)?;
        chunk.encode2(DEF, 0, 1, line)?;
        chunk.encode2(DEFV, 0, 3, line)?;
        chunk.encode2(REF, 2, 4, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[2].unref(&vm).get_int()? == 43);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        let slot = vm.globals.interned_slot(sym2).unwrap() as u32;
        vm.globals.set(slot, Value::Int(11));
        assert!(vm.globals.get(slot).get_int()? == 11);
        let const1 = chunk.add_constant(Value::Global(slot)) as u16;
        let const2 = chunk.add_constant(Value::Int(43)) as u16;
        let const3 = chunk.add_constant(Value::Int(53)) as u16;
        let const4 = chunk.add_constant(Value::Symbol(sym2)) as u16;
        chunk.encode2(CONST, 0, const1, line)?;
        chunk.encode2(CONST, 1, const2, line)?;
        chunk.encode2(CONST, 3, const3, line)?;
        chunk.encode2(CONST, 4, const4, line)?;
        chunk.encode2(DEF, 0, 1, line)?;
        chunk.encode2(DEFV, 0, 3, line)?;
        chunk.encode2(REF, 2, 4, line)?;
        chunk.encode2(REF, 5, 4, line)?;
        chunk.encode2(SET, 5, 3, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[2].unref(&vm).get_int()? == 53);
        assert!(vm.stack[5].unref(&vm).get_int()? == 53);
        assert!(vm.globals.get(slot).get_int()? == 53);

        let mut vm = Vm::new();
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        let slot = vm.globals.reserve(sym2);
        let const1 = chunk.add_constant(Value::Global(slot)) as u16;
        let const2 = chunk.add_constant(Value::Int(44)) as u16;
        let const3 = chunk.add_constant(Value::Int(53)) as u16;
        let const4 = chunk.add_constant(Value::Symbol(sym2)) as u16;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode2(CONST, 2, const2, line)?;
        chunk.encode2(CONST, 3, const3, line)?;
        chunk.encode2(CONST, 4, const4, line)?;
        chunk.encode2(DEFV, 1, 2, line)?;
        chunk.encode2(DEFV, 1, 3, line)?;
        chunk.encode2(REF, 0, 4, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].unref(&vm).get_int()? == 44);

        let mut vm = Vm::new();
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        let slot = vm.globals.reserve(sym2);
        let const1 = chunk.add_constant(Value::Global(slot)) as u16;
        let const2 = chunk.add_constant(Value::Int(45)) as u16;
        let const3 = chunk.add_constant(Value::Int(55)) as u16;
        let const4 = chunk.add_constant(Value::Symbol(sym2)) as u16;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode2(CONST, 2, const2, line)?;
        chunk.encode2(CONST, 3, const3, line)?;
        chunk.encode2(CONST, 4, const4, line)?;
        chunk.encode2(DEFV, 1, 2, line)?;
        chunk.encode2(DEF, 1, 3, line)?;
        chunk.encode2(REF, 0, 4, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].unref(&vm).get_int()? == 55);

        let mut vm = Vm::new();
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        let slot = vm.globals.reserve(sym2);
        let const1 = chunk.add_constant(Value::Global(slot)) as u16;
        let const2 = chunk.add_constant(Value::Int(45)) as u16;
        let const3 = chunk.add_constant(Value::Int(1)) as u16;
        let const4 = chunk.add_constant(Value::Symbol(sym2)) as u16;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode2(CONST, 2, const2, line)?;
        chunk.encode2(CONST, 3, const3, line)?;
        chunk.encode2(CONST, 4, const4, line)?;
        chunk.encode2(DEFV, 1, 2, line)?;
        chunk.encode2(DEF, 1, 3, line)?;
        chunk.encode2(REF, 0, 4, line)?;
        chunk.encode2(MOV, 5, 0, line)?;
        chunk.encode2(SET, 5, 3, line)?;
        chunk.encode3(ADD, 5, 5, 3, line)?;
        chunk.encode3(ADD, 5, 5, 3, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].unref(&vm).get_int()? == 1);
        assert!(vm.stack[5].unref(&vm).get_int()? == 3);
        assert!(vm.globals.get(slot).get_int()? == 1);

        Ok(())
    }

    #[test]
    fn test_pol() -> VMResult<()> {
        // algorithm from http://dan.corlan.net/bench.html
        // Do a lot of loops and simple math.
        /*
        (defn eval-pol (n x)
          (let ((su 0.0) (mu 10.0) (pu 0.0)
                (pol (make-vec 100 0.0)))
            (dotimes-i i n
              (do
                (set! su 0.0)
                (dotimes-i j 100
                   (do
                     (set! mu (/ (+ mu 2.0) 2.0))
                     (vec-set! pol j mu)))
                (dotimes-i j 100
                  (set! su (+ (vec-nth pol j) (* su x))))
                (set! pu (+ pu su))))
            (println pu)))
                 */
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("no_file", 1);
        let n = chunk.add_constant(Value::Int(5000)) as u16;
        let x = chunk.add_constant(Value::float(0.2)) as u16;
        let su = chunk.add_constant(Value::float(0.0)) as u16;
        let mu = chunk.add_constant(Value::float(10.0)) as u16;
        let pu = chunk.add_constant(Value::float(0.0)) as u16;
        let zero = chunk.add_constant(Value::Int(0)) as u16;
        let zerof = chunk.add_constant(Value::float(0.0)) as u16;
        let twof = chunk.add_constant(Value::float(2.0)) as u16;
        let hundred = chunk.add_constant(Value::Int(100)) as u16;
        let one = chunk.add_constant(Value::Int(1)) as u16;
        let line = 1;
        chunk.encode2(CONST, 1, n, line)?;
        chunk.encode2(CONST, 2, x, line)?;
        chunk.encode2(CONST, 3, su, line)?;
        chunk.encode2(CONST, 4, mu, line)?;
        chunk.encode2(CONST, 5, pu, line)?;
        chunk.encode2(CONST, 6, zero, line)?; // i
        chunk.encode2(CONST, 7, zero, line)?; // j
        chunk.encode2(CONST, 8, twof, line)?; // 2.0
        chunk.encode2(CONST, 100, hundred, line)?;
        chunk.encode2(CONST, 101, one, line)?;
        chunk.encode2(CONST, 103, zerof, line)?;
        chunk.encode3(VECMKD, 10, 100, 103, line)?; // pols
                                                    //chunk.encode2(VECELS, 10, 100, line)?;
                                                    // loop i .. n
        chunk.encode2(CONST, 3, zerof, line)?;
        chunk.encode2(CONST, 7, zero, line)?; // j
                                              // loop j .. 100
                                              // (set! mu (/ (+ mu 2.0) 2.0))
        chunk.encode3(ADD, 4, 4, 8, line)?;
        chunk.encode3(DIV, 4, 4, 8, line)?;
        // (vec-set! pol j mu)))
        chunk.encode3(VECSTH, 10, 4, 7, line)?;

        chunk.encode2(INC, 7, 1, line)?;
        chunk.encode3(JMPLT, 7, 100, 0x2b, line)?;

        chunk.encode2(CONST, 7, zero, line)?; // j
                                              // (dotimes-i j 100 (j2)
                                              //   (set! su (+ (vec-nth pol j) (* su x))))
        chunk.encode3(MUL, 50, 3, 2, line)?;
        chunk.encode3(VECNTH, 10, 51, 7, line)?;
        chunk.encode3(ADD, 3, 50, 51, line)?;

        chunk.encode2(INC, 7, 1, line)?;
        chunk.encode3(JMPLT, 7, 100, 0x41, line)?;
        // (set! pu (+ pu su))))
        chunk.encode3(ADD, 5, 5, 3, line)?;

        chunk.encode2(INC, 6, 1, line)?;
        chunk.encode3(JMPLT, 6, 1, 0x25, line)?;

        chunk.encode0(RET, line)?;
        //chunk.disassemble_chunk(&vm)?;
        //assert!(false);

        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[5].get_float()?;
        assert!(result == 12500.0);

        Ok(())
    }

    #[test]
    fn test_lambda() -> VMResult<()> {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        chunk.encode3(ADD, 3, 1, 2, line).unwrap();
        chunk.encode1(SRET, 3, line)?;
        let add = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Int(10)) as u16;
        chunk.encode2(CONST, 2, const1, line).unwrap();
        chunk.encode3(ADD, 3, 1, 2, line).unwrap();
        chunk.encode1(SRET, 3, line)?;
        let add_ten = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        vm.stack[0] = add;
        vm.stack[1] = add_ten;
        vm.stack[3] = Value::Int(5);
        vm.stack[4] = Value::Int(2);
        vm.stack[6] = Value::Int(2);
        chunk.encode3(CALL, 0, 2, 2, line).unwrap();
        chunk.encode3(CALL, 1, 1, 5, line).unwrap();
        chunk.encode0(RET, line)?;

        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[2].get_int()?;
        assert!(result == 7);
        let result = vm.stack[5].get_int()?;
        assert!(result == 12);
        let result = vm.stack[7].get_int()?;
        assert!(result == 10);

        Ok(())
    }

    #[test]
    fn test_tcall() -> VMResult<()> {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        chunk.encode3(ADD, 3, 1, 2, line).unwrap();
        chunk.encode1(SRET, 3, line)?;
        let add = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Int(10)) as u16;
        let const2 = chunk.add_constant(add) as u16;
        chunk.encode2(CONST, 2, const1, line).unwrap();
        chunk.encode2(CONST, 3, const2, line).unwrap();
        chunk.encode2(TCALL, 3, 2, line).unwrap();
        // The TCALL will keep HALT from executing.
        chunk.encode0(HALT, line)?;
        let add_ten = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        vm.stack[1] = Value::Int(5);
        vm.stack[2] = Value::Int(2);
        vm.stack[4] = Value::Int(2);
        vm.stack[50] = add;
        vm.stack[60] = add_ten;
        chunk.encode3(CALL, 60, 1, 3, line).unwrap();
        // tail call at the top level does not make sense
        //chunk.encode2(TCALL, 50, 2, line).unwrap();
        chunk.encode0(RET, line)?;

        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        //let result = vm.stack[0].get_int()?;
        //assert!(result == 7);
        let result = vm.stack[3].get_int()?;
        assert!(result == 12);

        Ok(())
    }

    #[test]
    fn test_builtin() -> VMResult<()> {
        fn add_b(_vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
            if registers.len() != 2 {
                return Err(VMError::new_vm("test add: wrong number of args."));
            }
            Ok(Value::Int(
                registers[0].get_int()? + registers[1].get_int()?,
            ))
        }
        fn add_10(_vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
            if registers.len() != 1 {
                return Err(VMError::new_vm("test add_10: wrong number of args."));
            }
            Ok(Value::Int(registers[0].get_int()? + 10))
        }
        fn make_str(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
            if registers.len() != 0 {
                return Err(VMError::new_vm("test make_str: wrong number of args."));
            }
            let s = vm.alloc_string("builtin hello".into());
            Ok(s)
        }
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Builtin(CallFunc { func: add_b })) as u16;
        chunk.encode2(CONST, 10, const1, line).unwrap();
        chunk.encode2(MOV, 4, 1, line).unwrap();
        chunk.encode2(MOV, 5, 2, line).unwrap();
        chunk.encode3(CALL, 10, 2, 3, line).unwrap();
        chunk.encode1(SRET, 3, line)?;
        let add = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Builtin(CallFunc { func: add_b })) as u16;
        chunk.encode2(CONST, 10, const1, line).unwrap();
        chunk.encode2(TCALL, 10, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let tadd = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Builtin(CallFunc { func: add_10 })) as u16;
        chunk.encode2(CONST, 4, const1, line).unwrap();
        chunk.encode2(MOV, 3, 1, line).unwrap();
        chunk.encode3(CALL, 4, 1, 2, line).unwrap();
        chunk.encode1(SRET, 2, line)?;
        let add_ten = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        vm.stack[0] = add;
        vm.stack[1] = add_ten;
        vm.stack[3] = Value::Int(6);
        vm.stack[4] = Value::Int(3);
        vm.stack[8] = Value::Int(12);
        let const1 = chunk.add_constant(Value::Builtin(CallFunc { func: make_str })) as u16;
        chunk.encode3(CALL, 0, 2, 2, line).unwrap();
        chunk.encode3(CALL, 1, 1, 7, line).unwrap();
        chunk.encode2(CONST, 15, const1, line).unwrap();
        chunk.encode3(CALL, 15, 0, 15, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[2].get_int()?;
        assert!(result == 9);
        let result = vm.stack[7].get_int()?;
        assert!(result == 22);
        match vm.stack[15] {
            Value::String(h) => assert!(vm.heap.get_string(h) == "builtin hello"),
            _ => panic!("bad make_str call"),
        }

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        for i in 0..100 {
            vm.stack[i] = Value::Undefined;
        }
        vm.stack[0] = tadd;
        vm.stack[1] = add_ten;
        vm.stack[3] = Value::Int(6);
        vm.stack[4] = Value::Int(3);
        vm.stack[6] = Value::Int(12);
        let const1 = chunk.add_constant(Value::Builtin(CallFunc { func: make_str })) as u16;
        chunk.encode3(CALL, 0, 2, 2, line).unwrap();
        chunk.encode3(CALL, 1, 1, 5, line).unwrap();
        chunk.encode2(CONST, 10, const1, line).unwrap();
        chunk.encode3(CALL, 10, 0, 11, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[2].get_int()?;
        assert!(result == 9);
        let result = vm.stack[5].get_int()?;
        assert!(result == 22);
        match vm.stack[11] {
            Value::String(h) => assert!(vm.heap.get_string(h) == "builtin hello"),
            _ => panic!("bad make_str call"),
        }

        Ok(())
    }

    #[test]
    fn test_jumps() -> VMResult<()> {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::Int(2 as i64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3 as i64)) as u16;
        vm.stack[0] = Value::True;
        vm.stack[1] = Value::False;
        vm.stack[2] = Value::Nil;
        vm.stack[3] = Value::Int(0);
        let line = 1;
        chunk.encode2(CONST, 4, const0, line)?;
        chunk.encode1(JMP, 8, line)?;
        chunk.encode2(CONST, 4, const1, line)?;
        chunk.encode2(CONST, 5, const1, line)?;

        chunk.encode2(CONST, 6, const0, line)?;
        chunk.encode1(JMPF, 3, line)?;
        chunk.encode2(CONST, 6, const1, line)?;
        chunk.encode2(CONST, 7, const1, line)?;

        chunk.encode1(JMPF, 5, line)?;
        chunk.encode2(CONST, 8, const0, line)?;
        chunk.encode1(JMPF, 2, line)?;
        chunk.encode1(JMPB, 7, line)?;
        chunk.encode2(CONST, 9, const1, line)?;

        chunk.encode2(CONST, 10, const0, line)?;
        chunk.encode2(JMPFT, 0, 3, line)?;
        chunk.encode2(CONST, 10, const1, line)?;
        chunk.encode2(CONST, 11, const1, line)?;

        chunk.encode2(CONST, 12, const0, line)?;
        chunk.encode2(JMPFT, 3, 3, line)?;
        chunk.encode2(CONST, 12, const1, line)?;
        chunk.encode2(CONST, 13, const1, line)?;

        chunk.encode2(CONST, 14, const0, line)?;
        chunk.encode2(JMPFF, 1, 3, line)?;
        chunk.encode2(CONST, 14, const1, line)?;
        chunk.encode2(CONST, 15, const1, line)?;

        chunk.encode2(CONST, 16, const0, line)?;
        chunk.encode2(JMPFF, 2, 3, line)?;
        chunk.encode2(CONST, 16, const1, line)?;
        chunk.encode2(CONST, 17, const1, line)?;

        chunk.encode2(CONST, 18, const0, line)?;
        chunk.encode2(JMPFT, 1, 3, line)?;
        chunk.encode2(CONST, 18, const1, line)?;
        chunk.encode2(CONST, 19, const1, line)?;

        chunk.encode2(CONST, 20, const0, line)?;
        chunk.encode2(JMPFT, 2, 3, line)?;
        chunk.encode2(CONST, 20, const1, line)?;
        chunk.encode2(CONST, 21, const1, line)?;

        chunk.encode2(CONST, 22, const0, line)?;
        chunk.encode2(JMPFF, 0, 3, line)?;
        chunk.encode2(CONST, 22, const1, line)?;
        chunk.encode2(CONST, 23, const1, line)?;

        chunk.encode2(CONST, 24, const0, line)?;
        chunk.encode2(JMPFF, 3, 3, line)?;
        chunk.encode2(CONST, 24, const1, line)?;
        chunk.encode2(CONST, 25, const1, line)?;

        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[4].get_int()? == 2);
        assert!(vm.stack[5].get_int()? == 3);
        assert!(vm.stack[6].get_int()? == 2);
        assert!(vm.stack[7].get_int()? == 3);
        assert!(vm.stack[8].get_int()? == 2);
        assert!(vm.stack[9].get_int()? == 3);
        assert!(vm.stack[10].get_int()? == 2);
        assert!(vm.stack[11].get_int()? == 3);
        assert!(vm.stack[12].get_int()? == 2);
        assert!(vm.stack[13].get_int()? == 3);
        assert!(vm.stack[14].get_int()? == 2);
        assert!(vm.stack[15].get_int()? == 3);
        assert!(vm.stack[16].get_int()? == 2);
        assert!(vm.stack[17].get_int()? == 3);
        assert!(vm.stack[18].get_int()? == 3);
        assert!(vm.stack[19].get_int()? == 3);
        assert!(vm.stack[20].get_int()? == 3);
        assert!(vm.stack[21].get_int()? == 3);
        assert!(vm.stack[22].get_int()? == 3);
        assert!(vm.stack[23].get_int()? == 3);
        assert!(vm.stack[24].get_int()? == 3);
        assert!(vm.stack[25].get_int()? == 3);
        Ok(())
    }

    #[test]
    fn test_vecs() -> VMResult<()> {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("no_file", 1);
        let zero = chunk.add_constant(Value::Int(0)) as u16;
        let hundred = chunk.add_constant(Value::Int(100)) as u16;
        let one = chunk.add_constant(Value::Int(1)) as u16;
        let line = 1;
        chunk.encode2(CONST, 2, hundred, line)?;
        chunk.encode2(CONST, 3, zero, line)?;
        chunk.encode2(CONST, 4, one, line)?;
        chunk.encode3(VECMKD, 1, 2, 3, line)?;
        chunk.encode2(VECPSH, 1, 4, line)?;
        chunk.encode2(VECPOP, 1, 5, line)?;
        chunk.encode2(VECPOP, 1, 6, line)?;
        chunk.encode2(VECPSH, 1, 4, line)?;
        chunk.encode2(VECPSH, 1, 4, line)?;
        chunk.encode3(VECNTH, 1, 7, 2, line)?;
        chunk.encode3(VECSTH, 1, 3, 2, line)?;
        chunk.encode3(VECNTH, 1, 8, 2, line)?;
        chunk.encode2(VECMK, 10, 2, line)?;
        chunk.encode2(VECPSH, 10, 4, line)?;
        chunk.encode2(VECPSH, 10, 3, line)?;
        chunk.encode2(VECPOP, 10, 15, line)?;
        chunk.encode2(VECPOP, 10, 16, line)?;
        chunk.encode2(VECPSH, 10, 4, line)?;
        chunk.encode2(VECPSH, 10, 4, line)?;
        chunk.encode3(VECNTH, 10, 17, 3, line)?;
        chunk.encode3(VECSTH, 10, 3, 3, line)?;
        chunk.encode3(VECNTH, 10, 18, 3, line)?;
        chunk.encode2(VECMK, 20, 2, line)?;
        chunk.encode2(VECELS, 20, 2, line)?;
        chunk.encode3(VECSTH, 20, 3, 3, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[5].get_int()? == 1);
        assert!(vm.stack[6].get_int()? == 0);
        assert!(vm.stack[7].get_int()? == 1);
        assert!(vm.stack[8].get_int()? == 0);

        assert!(vm.stack[15].get_int()? == 0);
        assert!(vm.stack[16].get_int()? == 1);
        assert!(vm.stack[17].get_int()? == 1);
        assert!(vm.stack[18].get_int()? == 0);
        let vc = vm.stack[1];
        if let Value::Vector(h) = vc {
            let v = vm.get_vector_mut(h);
            assert!(v.len() == 101);
            assert!(v[0].get_int()? == 0);
        }
        let vc = vm.stack[10];
        if let Value::Vector(h) = vc {
            let v = vm.get_vector_mut(h);
            assert!(v.len() == 2);
            assert!(v[0].get_int()? == 0);
            assert!(v[1].get_int()? == 1);
        }
        let vc = vm.stack[20];
        if let Value::Vector(h) = vc {
            let v = vm.get_vector_mut(h);
            assert!(v.len() == 100);
            assert!(v[0].get_int()? == 0);
            assert!(v[1].is_undef());
            assert!(v[99].is_undef());
        }
        Ok(())
    }

    #[test]
    fn test_add() -> VMResult<()> {
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const0 = chunk.add_constant(Value::Int(2 as i64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3 as i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode2(CONST, 2, const2, line)?;
        chunk.encode3(ADD, 0, 0, 1, line).unwrap();
        chunk.encode3(ADD, 0, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].get_int()? == 6);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::float(2 as f64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3 as i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode2(CONST, 2, const2, line)?;
        chunk.encode3(ADD, 0, 0, 1, line).unwrap();
        chunk.encode3(ADD, 0, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let item = vm.stack[0];
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 6.0);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..501 {
            chunk.add_constant(Value::Int(i as i64));
        }
        chunk.encode2(CONST, 1, 1, line)?;
        chunk.encode2(CONST, 2, 2, line)?;
        chunk.encode2(CONST, 5, 5, line)?;
        chunk.encode2(CONST, 500, 500, line)?;
        chunk.encode3(ADD, 0, 1, 2, line).unwrap();
        chunk.encode3(ADD, 0, 5, 0, line).unwrap();
        chunk.encode3(ADD, 1, 500, 0, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let item = vm.stack[0];
        let item2 = vm.stack[1];
        assert!(item.is_int());
        assert!(item.get_int()? == 8);
        assert!(item2.get_int()? == 508);
        Ok(())
    }

    #[test]
    fn test_sub() -> VMResult<()> {
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const0 = chunk.add_constant(Value::Int(2 as i64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3 as i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode2(CONST, 2, const2, line)?;
        chunk.encode3(SUB, 0, 0, 1, line).unwrap();
        chunk.encode3(SUB, 0, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].get_int()? == -2);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::float(5 as f64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3 as i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode2(CONST, 2, const2, line)?;
        chunk.encode3(SUB, 0, 0, 1, line).unwrap();
        chunk.encode3(SUB, 0, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let item = vm.stack[0];
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 1.0);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..501 {
            chunk.add_constant(Value::Int(i as i64));
        }
        chunk.encode2(CONST, 1, 1, line)?;
        chunk.encode2(CONST, 2, 2, line)?;
        chunk.encode2(CONST, 5, 5, line)?;
        chunk.encode2(CONST, 500, 500, line)?;
        chunk.encode3(SUB, 0, 1, 2, line).unwrap();
        chunk.encode3(SUB, 0, 5, 0, line).unwrap();
        chunk.encode3(SUB, 1, 500, 0, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let item = vm.stack[0];
        let item2 = vm.stack[1];
        assert!(item.is_int());
        assert!(item.get_int()? == 6);
        assert!(item2.get_int()? == 494);
        Ok(())
    }

    #[test]
    fn test_mul() -> VMResult<()> {
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const0 = chunk.add_constant(Value::Int(2 as i64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3 as i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode2(CONST, 2, const2, line)?;
        chunk.encode3(MUL, 0, 0, 1, line).unwrap();
        chunk.encode3(MUL, 0, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].get_int()? == 6);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::float(5 as f64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3 as i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(2)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode2(CONST, 2, const2, line)?;
        chunk.encode3(MUL, 0, 0, 1, line).unwrap();
        chunk.encode3(MUL, 0, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let item = vm.stack[0];
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 30.0);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..501 {
            chunk.add_constant(Value::Int(i as i64));
        }
        chunk.encode2(CONST, 1, 1, line)?;
        chunk.encode2(CONST, 2, 2, line)?;
        chunk.encode2(CONST, 5, 5, line)?;
        chunk.encode2(CONST, 500, 500, line)?;
        chunk.encode3(MUL, 0, 1, 2, line).unwrap();
        chunk.encode3(MUL, 0, 5, 0, line).unwrap();
        chunk.encode3(MUL, 1, 500, 0, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let item = vm.stack[0];
        let item2 = vm.stack[1];
        assert!(item.is_int());
        assert!(item.get_int()? == 10);
        assert!(item2.get_int()? == 5000);
        Ok(())
    }

    #[test]
    fn test_div() -> VMResult<()> {
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const0 = chunk.add_constant(Value::Int(18 as i64)) as u16;
        let const1 = chunk.add_constant(Value::Int(2 as i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(3)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode2(CONST, 2, const2, line)?;
        chunk.encode3(DIV, 0, 0, 1, line).unwrap();
        chunk.encode3(DIV, 0, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].get_int()? == 3);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::float(10 as f64)) as u16;
        let const1 = chunk.add_constant(Value::Int(2 as i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(2)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode2(CONST, 2, const2, line)?;
        chunk.encode3(DIV, 0, 0, 1, line).unwrap();
        chunk.encode3(DIV, 0, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let item = vm.stack[0];
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 2.5);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..501 {
            chunk.add_constant(Value::Int(i as i64));
        }
        chunk.encode2(CONST, 1, 1, line)?;
        chunk.encode2(CONST, 2, 2, line)?;
        chunk.encode2(CONST, 10, 10, line)?;
        chunk.encode2(CONST, 500, 500, line)?;
        chunk.encode3(DIV, 0, 2, 1, line).unwrap();
        chunk.encode3(DIV, 0, 10, 0, line).unwrap();
        chunk.encode3(DIV, 1, 500, 0, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let item = vm.stack[0];
        let item2 = vm.stack[1];
        assert!(item.is_int());
        assert!(item.get_int()? == 5);
        assert!(item2.get_int()? == 100);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::Int(10 as i64)) as u16;
        let const1 = chunk.add_constant(Value::Int(0 as i64)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode3(DIV, 0, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        let res = vm.execute(chunk.clone());
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[rt]: Divide by zero error.");

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::float(10 as f64)) as u16;
        let const1 = chunk.add_constant(Value::float(0 as f64)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode3(DIV, 0, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        let res = vm.execute(chunk.clone());
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[rt]: Divide by zero error.");

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::float(10 as f64)) as u16;
        let const1 = chunk.add_constant(Value::Byte(0)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode3(DIV, 0, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        let res = vm.execute(chunk.clone());
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[rt]: Divide by zero error.");
        Ok(())
    }
}
