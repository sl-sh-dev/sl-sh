use std::sync::Arc;

use crate::chunk::*;
use crate::error::*;
use crate::heap::*;
use crate::interner::*;
use crate::value::*;

pub mod cons;
pub mod print;
pub mod storage;
#[macro_use]
pub mod macros;
pub mod exec_loop;

const STACK_CAP: usize = 1024;

#[derive(Debug)]
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
            let rest_len = (num_args - (chunk.args + chunk.opt_args)) as usize + 1;
            let mut r = vec![Value::Undefined; rest_len];
            r.copy_from_slice(&registers[rest_reg as usize..(rest_reg as usize + rest_len)]);
            self.alloc_list_ro(r)
        };
        (rest_reg.into(), v)
    }

    fn k_unshared_stack(&self, stack_top: usize, k: &Continuation) -> Option<(usize, &Vec<Value>)> {
        if !k.stack.is_empty() {
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

    fn make_call_frame(
        &mut self,
        chunk: Arc<Chunk>,
        called: Value,
        with_defers: bool,
    ) -> CallFrame {
        let defers = if with_defers {
            std::mem::take(&mut self.defers)
        } else {
            Vec::new()
        };
        let frame = CallFrame {
            id: self.callframe_id,
            chunk,
            ip: self.ip,
            current_ip: self.current_ip,
            stack_top: self.stack_top,
            this_fn: self.this_fn,
            defers,
            on_error: self.on_error,
            called,
        };
        self.callframe_id += 1;
        frame
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
            let num_args = if l.rest && num_args == 0 {
                // Always have at least 1 arg if we have a rest argument.
                1
            } else {
                num_args
            };
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
                // Useful if the builtin runs bytecode that errors otherwise a waste...
                let frame = self.make_call_frame(chunk.clone(), lambda, false);
                let res = (f.func)(self, &registers[(first_reg + 1) as usize..last_reg]).map_err(
                    |e| {
                        if self.err_frame().is_some() {
                            Self::mov_register(
                                registers,
                                first_reg.into(),
                                self.alloc_callframe(frame),
                            );
                            self.stack_top += first_reg as usize;
                        }
                        (e, chunk.clone())
                    },
                )?;
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
                    let frame = self.make_call_frame(chunk, lambda, true);
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
                let frame = if !tail_call {
                    let frame = self.make_call_frame(chunk, lambda, true);
                    self.stack_top += first_reg as usize;
                    Some(frame)
                } else {
                    None
                };
                let (l, caps) = self.heap.get_closure(handle);
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
        let val = self.alloc_string(val);
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

    pub fn is_equal_pair(&self, val1: Value, val2: Value) -> VMResult<Value> {
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
                Value::Pair(_) | Value::List(_, _) => {
                    // XXX use iterators to reduce recursion?
                    // Make sure pair iter will work for non-lists...
                    if matches!(val2, Value::Pair(_) | Value::List(_, _)) {
                        let (car1, cdr1) = val1.get_pair(self).expect("Must be a pair or list!");
                        let (car2, cdr2) = val2.get_pair(self).expect("Must be a pair or list!");
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

    pub fn do_call(
        &mut self,
        chunk: Arc<Chunk>,
        params: &[Value],
        caps: Option<&[Handle]>,
    ) -> VMResult<Value> {
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
            if let Some(caps) = caps {
                let cap_first = (chunk.args + chunk.opt_args + 1) as usize;
                for (i, c) in caps.iter().enumerate() {
                    Self::mov_register(registers, cap_first + i, Value::Value(*c));
                }
            }
            Self::mov_register(registers, rest_reg, h);
        } else if let Some(caps) = caps {
            let registers = self.make_registers();
            let cap_first = (chunk.args + chunk.opt_args + 1) as usize;
            for (i, c) in caps.iter().enumerate() {
                Self::mov_register(registers, cap_first + i, Value::Value(*c));
            }
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

        self.execute2(chunk)?;

        self.stack_top = stack_top;
        self.stack_max = stack_max;
        self.ip = ip;
        self.this_fn = this_fn;
        self.on_error = on_error;
        Ok(())
    }

    fn execute2(&mut self, chunk: Arc<Chunk>) -> VMResult<()> {
        let mut chunk = chunk;

        let mut done = false;
        let mut result = Ok(());
        while !done {
            result = if let Err((e, echunk)) = self.exec_loop(chunk.clone()) {
                if self.err_frame.is_none() {
                    self.err_frame = Some(CallFrame {
                        id: 0,
                        chunk: echunk,
                        stack_top: self.stack_top,
                        ip: self.ip,
                        current_ip: self.current_ip,
                        this_fn: self.this_fn,
                        defers: std::mem::take(&mut self.defers),
                        on_error: self.on_error,
                        called: Value::Undefined,
                    });
                }
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcodes::*;

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
        chunk.encode2(CONST, 0, 0, Some(line)).unwrap();
        chunk.encode2(CONST, 1, 1, Some(line)).unwrap();
        chunk.encode3(CONS, 1, 0, 1, Some(line)).unwrap();
        chunk.encode2(CDR, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        chunk.add_constant(Value::Nil);
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 2);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CAR, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 1);

        // car with nil
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, Some(line)).unwrap();
        chunk.encode2(CAR, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].is_nil());

        // car with nil on heap
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, Some(line)).unwrap();
        chunk.encode2(CAR, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].is_nil());

        // cdr with nil
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CDR, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].is_nil());

        // cdr with nil on heap
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, Some(line)).unwrap();
        chunk.encode2(CDR, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].is_nil());

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 2, Some(line)).unwrap();
        chunk.encode2(XAR, 1, 2, Some(line)).unwrap();
        chunk.encode2(CAR, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 3);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 3, Some(line)).unwrap();
        chunk.encode2(XDR, 1, 2, Some(line)).unwrap();
        chunk.encode2(CDR, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 4);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, Some(line)).unwrap();
        chunk.encode2(CONST, 3, 2, Some(line)).unwrap();
        chunk.encode2(XAR, 2, 3, Some(line)).unwrap();
        chunk.encode2(CAR, 0, 2, Some(line)).unwrap();
        chunk.encode2(CDR, 3, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 3);
        assert!(vm.stack[3].is_nil());

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, Some(line)).unwrap();
        chunk.encode2(CONST, 3, 3, Some(line)).unwrap();
        chunk.encode2(XDR, 2, 3, Some(line)).unwrap();
        chunk.encode2(CDR, 0, 2, Some(line)).unwrap();
        chunk.encode2(CAR, 3, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 4);
        assert!(vm.stack[3].is_nil());

        // Test a list with elements.
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 0, 0, Some(line)).unwrap();
        chunk.encode2(CONST, 1, 1, Some(line)).unwrap();
        chunk.encode2(CONST, 2, 2, Some(line)).unwrap();
        chunk.encode3(LIST, 0, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
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
                    panic!();
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode3(LIST, 0, 1, 0, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
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
        chunk.encode2(CONST, 0, 0, Some(line)).unwrap();
        chunk.encode2(CONST, 1, 255, Some(line)).unwrap();
        chunk.encode3(ADD, 0, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;

        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 255);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 1, 256, Some(line)).unwrap();
        chunk.encode3(ADD, 0, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;

        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 255 + 256);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(MOV, 1, 0, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
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
        chunk.encode2(MOV, 1, 0, Some(line)).unwrap();
        chunk.encode3(ADD, 1, 1, 2, Some(line)).unwrap();
        chunk.encode3(ADD, 1, 1, 2, Some(line)).unwrap();
        chunk.encode3(ADD, 1, 1, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
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
        chunk.encode2(CONST, 0, const1, Some(line))?;
        chunk.encode2(REF, 1, 0, Some(line))?;
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        assert!(vm.execute(chunk.clone()).is_err());

        vm.clear_err_frame();
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        vm.globals.set(slot, Value::Int(11));
        chunk.encode2(CONST, 0, const1, Some(line))?;
        chunk.encode2(REF, 1, 0, Some(line))?;
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[1].unref(&vm).get_int()? == 11);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 0, const3, Some(line))?;
        chunk.encode2(CONST, 1, const2, Some(line))?;
        chunk.encode2(DEF, 0, 1, Some(line))?;
        chunk.encode2(REF, 2, 0, Some(line))?;
        chunk.encode0(RET, Some(line))?;
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
        chunk.encode2(CONST, 0, const1, Some(line))?;
        chunk.encode2(CONST, 1, const2, Some(line))?;
        chunk.encode2(CONST, 3, const3, Some(line))?;
        chunk.encode2(CONST, 4, const4, Some(line))?;
        chunk.encode2(DEF, 0, 1, Some(line))?;
        chunk.encode2(DEFV, 0, 3, Some(line))?;
        chunk.encode2(REF, 2, 4, Some(line))?;
        chunk.encode0(RET, Some(line))?;
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
        chunk.encode2(CONST, 0, const1, Some(line))?;
        chunk.encode2(CONST, 1, const2, Some(line))?;
        chunk.encode2(CONST, 3, const3, Some(line))?;
        chunk.encode2(CONST, 4, const4, Some(line))?;
        chunk.encode2(DEF, 0, 1, Some(line))?;
        chunk.encode2(DEFV, 0, 3, Some(line))?;
        chunk.encode2(REF, 2, 4, Some(line))?;
        chunk.encode2(REF, 5, 4, Some(line))?;
        chunk.encode2(SET, 5, 3, Some(line))?;
        chunk.encode0(RET, Some(line))?;
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
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(CONST, 3, const3, Some(line))?;
        chunk.encode2(CONST, 4, const4, Some(line))?;
        chunk.encode2(DEFV, 1, 2, Some(line))?;
        chunk.encode2(DEFV, 1, 3, Some(line))?;
        chunk.encode2(REF, 0, 4, Some(line))?;
        chunk.encode0(RET, Some(line))?;
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
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(CONST, 3, const3, Some(line))?;
        chunk.encode2(CONST, 4, const4, Some(line))?;
        chunk.encode2(DEFV, 1, 2, Some(line))?;
        chunk.encode2(DEF, 1, 3, Some(line))?;
        chunk.encode2(REF, 0, 4, Some(line))?;
        chunk.encode0(RET, Some(line))?;
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
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(CONST, 3, const3, Some(line))?;
        chunk.encode2(CONST, 4, const4, Some(line))?;
        chunk.encode2(DEFV, 1, 2, Some(line))?;
        chunk.encode2(DEF, 1, 3, Some(line))?;
        chunk.encode2(REF, 0, 4, Some(line))?;
        chunk.encode2(MOV, 5, 0, Some(line))?;
        chunk.encode2(SET, 5, 3, Some(line))?;
        chunk.encode3(ADD, 5, 5, 3, Some(line))?;
        chunk.encode3(ADD, 5, 5, 3, Some(line))?;
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
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
        chunk.encode2(CONST, 1, n, Some(line))?;
        chunk.encode2(CONST, 2, x, Some(line))?;
        chunk.encode2(CONST, 3, su, Some(line))?;
        chunk.encode2(CONST, 4, mu, Some(line))?;
        chunk.encode2(CONST, 5, pu, Some(line))?;
        chunk.encode2(CONST, 6, zero, Some(line))?; // i
        chunk.encode2(CONST, 7, zero, Some(line))?; // j
        chunk.encode2(CONST, 8, twof, Some(line))?; // 2.0
        chunk.encode2(CONST, 100, hundred, Some(line))?;
        chunk.encode2(CONST, 101, one, Some(line))?;
        chunk.encode2(CONST, 103, zerof, Some(line))?;
        chunk.encode3(VECMKD, 10, 100, 103, Some(line))?; // pols
                                                          //chunk.encode2(VECELS, 10, 100, Some(line))?;
                                                          // loop i .. n
        chunk.encode2(CONST, 3, zerof, Some(line))?;
        chunk.encode2(CONST, 7, zero, Some(line))?; // j
                                                    // loop j .. 100
                                                    // (set! mu (/ (+ mu 2.0) 2.0))
        chunk.encode3(ADD, 4, 4, 8, Some(line))?;
        chunk.encode3(DIV, 4, 4, 8, Some(line))?;
        // (vec-set! pol j mu)))
        chunk.encode3(VECSTH, 10, 4, 7, Some(line))?;

        chunk.encode2(INC, 7, 1, Some(line))?;
        chunk.encode2(JMPLT, 7, 100, Some(line))?;
        chunk.encode_jump_offset(-21)?;

        chunk.encode2(CONST, 7, zero, Some(line))?; // j
                                                    // (dotimes-i j 100 (j2)
                                                    //   (set! su (+ (vec-nth pol j) (* su x))))
        chunk.encode3(MUL, 50, 3, 2, Some(line))?;
        chunk.encode3(VECNTH, 10, 51, 7, Some(line))?;
        chunk.encode3(ADD, 3, 50, 51, Some(line))?;

        chunk.encode2(INC, 7, 1, Some(line))?;
        chunk.encode2(JMPLT, 7, 100, Some(line))?;
        chunk.encode_jump_offset(-21)?;
        // (set! pu (+ pu su))))
        chunk.encode3(ADD, 5, 5, 3, Some(line))?;

        chunk.encode2(INC, 6, 1, Some(line))?;
        chunk.encode2(JMPLT, 6, 1, Some(line))?;
        chunk.encode_jump_offset(-64)?;

        chunk.encode0(RET, Some(line))?;
        //chunk.disassemble_chunk(&vm)?;
        //assert!(false);

        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let result = vm.stack[5].get_float()?;
        assert!(result == 12500.0);

        Ok(())
    }

    #[test]
    fn test_lambda() -> VMResult<()> {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        chunk.encode3(ADD, 3, 1, 2, Some(line)).unwrap();
        chunk.encode1(SRET, 3, Some(line))?;
        let add = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Int(10)) as u16;
        chunk.encode2(CONST, 2, const1, Some(line)).unwrap();
        chunk.encode3(ADD, 3, 1, 2, Some(line)).unwrap();
        chunk.encode1(SRET, 3, Some(line))?;
        let add_ten = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        vm.stack[0] = add;
        vm.stack[1] = add_ten;
        vm.stack[3] = Value::Int(5);
        vm.stack[4] = Value::Int(2);
        vm.stack[6] = Value::Int(2);
        chunk.encode3(CALL, 0, 2, 2, Some(line)).unwrap();
        chunk.encode3(CALL, 1, 1, 5, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;

        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
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
        chunk.encode3(ADD, 3, 1, 2, Some(line)).unwrap();
        chunk.encode1(SRET, 3, Some(line))?;
        let add = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Int(10)) as u16;
        let const2 = chunk.add_constant(add) as u16;
        chunk.encode2(CONST, 2, const1, Some(line)).unwrap();
        chunk.encode2(CONST, 3, const2, Some(line)).unwrap();
        chunk.encode2(TCALL, 3, 2, Some(line)).unwrap();
        // The TCALL will keep HALT from executing.
        chunk.encode0(HALT, Some(line))?;
        let add_ten = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        vm.stack[1] = Value::Int(5);
        vm.stack[2] = Value::Int(2);
        vm.stack[4] = Value::Int(2);
        vm.stack[50] = add;
        vm.stack[60] = add_ten;
        chunk.encode3(CALL, 60, 1, 3, Some(line)).unwrap();
        // tail call at the top level does not make sense
        //chunk.encode2(TCALL, 50, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;

        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
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
            if !registers.is_empty() {
                return Err(VMError::new_vm("test make_str: wrong number of args."));
            }
            let s = vm.alloc_string("builtin hello".into());
            Ok(s)
        }
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Builtin(CallFunc { func: add_b })) as u16;
        chunk.encode2(CONST, 10, const1, Some(line)).unwrap();
        chunk.encode2(MOV, 4, 1, Some(line)).unwrap();
        chunk.encode2(MOV, 5, 2, Some(line)).unwrap();
        chunk.encode3(CALL, 10, 2, 3, Some(line)).unwrap();
        chunk.encode1(SRET, 3, Some(line))?;
        let add = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Builtin(CallFunc { func: add_b })) as u16;
        chunk.encode2(CONST, 10, const1, Some(line)).unwrap();
        chunk.encode2(TCALL, 10, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let tadd = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Builtin(CallFunc { func: add_10 })) as u16;
        chunk.encode2(CONST, 4, const1, Some(line)).unwrap();
        chunk.encode2(MOV, 3, 1, Some(line)).unwrap();
        chunk.encode3(CALL, 4, 1, 2, Some(line)).unwrap();
        chunk.encode1(SRET, 2, Some(line))?;
        let add_ten = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        vm.stack[0] = add;
        vm.stack[1] = add_ten;
        vm.stack[3] = Value::Int(6);
        vm.stack[4] = Value::Int(3);
        vm.stack[8] = Value::Int(12);
        let const1 = chunk.add_constant(Value::Builtin(CallFunc { func: make_str })) as u16;
        chunk.encode3(CALL, 0, 2, 2, Some(line)).unwrap();
        chunk.encode3(CALL, 1, 1, 7, Some(line)).unwrap();
        chunk.encode2(CONST, 15, const1, Some(line)).unwrap();
        chunk.encode3(CALL, 15, 0, 15, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
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
        chunk.encode3(CALL, 0, 2, 2, Some(line)).unwrap();
        chunk.encode3(CALL, 1, 1, 5, Some(line)).unwrap();
        chunk.encode2(CONST, 10, const1, Some(line)).unwrap();
        chunk.encode3(CALL, 10, 0, 11, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
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
        let const0 = chunk.add_constant(Value::Int(2_i64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3_i64)) as u16;
        vm.stack[0] = Value::True;
        vm.stack[1] = Value::False;
        vm.stack[2] = Value::Nil;
        vm.stack[3] = Value::Int(0);
        let line = 1;
        chunk.encode2(CONST, 4, const0, Some(line))?;
        chunk.encode0(JMP, Some(line))?;
        chunk.encode_jump_offset(3)?;
        chunk.encode2(CONST, 4, const1, Some(line))?;
        chunk.encode2(CONST, 5, const1, Some(line))?;

        chunk.encode2(CONST, 6, const0, Some(line))?;
        chunk.encode0(JMP, Some(line))?;
        chunk.encode_jump_offset(3)?;
        chunk.encode2(CONST, 6, const1, Some(line))?;
        chunk.encode2(CONST, 7, const1, Some(line))?;

        chunk.encode0(JMP, Some(line))?;
        chunk.encode_jump_offset(7)?;
        chunk.encode2(CONST, 8, const0, Some(line))?;
        chunk.encode0(JMP, Some(line))?;
        chunk.encode_jump_offset(4)?;
        chunk.encode0(JMP, Some(line))?;
        chunk.encode_jump_offset(-11)?;
        chunk.encode2(CONST, 9, const1, Some(line))?;

        chunk.encode2(CONST, 10, const0, Some(line))?;
        chunk.encode1(JMPT, 0, Some(line))?;
        chunk.encode_jump_offset(3)?;
        chunk.encode2(CONST, 10, const1, Some(line))?;
        chunk.encode2(CONST, 11, const1, Some(line))?;

        chunk.encode2(CONST, 12, const0, Some(line))?;
        chunk.encode1(JMPT, 3, Some(line))?;
        chunk.encode_jump_offset(3)?;
        chunk.encode2(CONST, 12, const1, Some(line))?;
        chunk.encode2(CONST, 13, const1, Some(line))?;

        chunk.encode2(CONST, 14, const0, Some(line))?;
        chunk.encode1(JMPF, 1, Some(line))?;
        chunk.encode_jump_offset(3)?;
        chunk.encode2(CONST, 14, const1, Some(line))?;
        chunk.encode2(CONST, 15, const1, Some(line))?;

        chunk.encode2(CONST, 16, const0, Some(line))?;
        chunk.encode1(JMPF, 2, Some(line))?;
        chunk.encode_jump_offset(3)?;
        chunk.encode2(CONST, 16, const1, Some(line))?;
        chunk.encode2(CONST, 17, const1, Some(line))?;

        chunk.encode2(CONST, 18, const0, Some(line))?;
        chunk.encode1(JMPT, 1, Some(line))?;
        chunk.encode_jump_offset(3)?;
        chunk.encode2(CONST, 18, const1, Some(line))?;
        chunk.encode2(CONST, 19, const1, Some(line))?;

        chunk.encode2(CONST, 20, const0, Some(line))?;
        chunk.encode1(JMPT, 2, Some(line))?;
        chunk.encode_jump_offset(3)?;
        chunk.encode2(CONST, 20, const1, Some(line))?;
        chunk.encode2(CONST, 21, const1, Some(line))?;

        chunk.encode2(CONST, 22, const0, Some(line))?;
        chunk.encode1(JMPF, 0, Some(line))?;
        chunk.encode_jump_offset(3)?;
        chunk.encode2(CONST, 22, const1, Some(line))?;
        chunk.encode2(CONST, 23, const1, Some(line))?;

        chunk.encode2(CONST, 24, const0, Some(line))?;
        chunk.encode1(JMPF, 3, Some(line))?;
        chunk.encode_jump_offset(3)?;
        chunk.encode2(CONST, 24, const1, Some(line))?;
        chunk.encode2(CONST, 25, const1, Some(line))?;

        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
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
        chunk.encode2(CONST, 2, hundred, Some(line))?;
        chunk.encode2(CONST, 3, zero, Some(line))?;
        chunk.encode2(CONST, 4, one, Some(line))?;
        chunk.encode3(VECMKD, 1, 2, 3, Some(line))?;
        chunk.encode2(VECPSH, 1, 4, Some(line))?;
        chunk.encode2(VECPOP, 1, 5, Some(line))?;
        chunk.encode2(VECPOP, 1, 6, Some(line))?;
        chunk.encode2(VECPSH, 1, 4, Some(line))?;
        chunk.encode2(VECPSH, 1, 4, Some(line))?;
        chunk.encode3(VECNTH, 1, 7, 2, Some(line))?;
        chunk.encode3(VECSTH, 1, 3, 2, Some(line))?;
        chunk.encode3(VECNTH, 1, 8, 2, Some(line))?;
        chunk.encode2(VECMK, 10, 2, Some(line))?;
        chunk.encode2(VECPSH, 10, 4, Some(line))?;
        chunk.encode2(VECPSH, 10, 3, Some(line))?;
        chunk.encode2(VECPOP, 10, 15, Some(line))?;
        chunk.encode2(VECPOP, 10, 16, Some(line))?;
        chunk.encode2(VECPSH, 10, 4, Some(line))?;
        chunk.encode2(VECPSH, 10, 4, Some(line))?;
        chunk.encode3(VECNTH, 10, 17, 3, Some(line))?;
        chunk.encode3(VECSTH, 10, 3, 3, Some(line))?;
        chunk.encode3(VECNTH, 10, 18, 3, Some(line))?;
        chunk.encode2(VECMK, 20, 2, Some(line))?;
        chunk.encode2(VECELS, 20, 2, Some(line))?;
        chunk.encode3(VECSTH, 20, 3, 3, Some(line))?;
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
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
            let v = vm.get_vector_mut(h).unwrap();
            assert!(v.len() == 101);
            assert!(v[0].get_int()? == 0);
        }
        let vc = vm.stack[10];
        if let Value::Vector(h) = vc {
            let v = vm.get_vector_mut(h).unwrap();
            assert!(v.len() == 2);
            assert!(v[0].get_int()? == 0);
            assert!(v[1].get_int()? == 1);
        }
        let vc = vm.stack[20];
        if let Value::Vector(h) = vc {
            let v = vm.get_vector_mut(h).unwrap();
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
        let const0 = chunk.add_constant(Value::Int(2_i64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3_i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode3(ADD, 0, 0, 1, Some(line)).unwrap();
        chunk.encode3(ADD, 0, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        assert!(vm.stack[0].get_int()? == 6);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::float(2_f64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3_i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode3(ADD, 0, 0, 1, Some(line)).unwrap();
        chunk.encode3(ADD, 0, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let item = vm.stack[0];
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 6.0);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..501 {
            chunk.add_constant(Value::Int(i as i64));
        }
        chunk.encode2(CONST, 1, 1, Some(line))?;
        chunk.encode2(CONST, 2, 2, Some(line))?;
        chunk.encode2(CONST, 5, 5, Some(line))?;
        chunk.encode2(CONST, 500, 500, Some(line))?;
        chunk.encode3(ADD, 0, 1, 2, Some(line)).unwrap();
        chunk.encode3(ADD, 0, 5, 0, Some(line)).unwrap();
        chunk.encode3(ADD, 1, 500, 0, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
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
        let const0 = chunk.add_constant(Value::Int(2_i64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3_i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode3(SUB, 0, 0, 1, Some(line)).unwrap();
        chunk.encode3(SUB, 0, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        assert!(vm.stack[0].get_int()? == -2);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::float(5_f64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3_i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode3(SUB, 0, 0, 1, Some(line)).unwrap();
        chunk.encode3(SUB, 0, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let item = vm.stack[0];
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 1.0);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..501 {
            chunk.add_constant(Value::Int(i as i64));
        }
        chunk.encode2(CONST, 1, 1, Some(line))?;
        chunk.encode2(CONST, 2, 2, Some(line))?;
        chunk.encode2(CONST, 5, 5, Some(line))?;
        chunk.encode2(CONST, 500, 500, Some(line))?;
        chunk.encode3(SUB, 0, 1, 2, Some(line)).unwrap();
        chunk.encode3(SUB, 0, 5, 0, Some(line)).unwrap();
        chunk.encode3(SUB, 1, 500, 0, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
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
        let const0 = chunk.add_constant(Value::Int(2_i64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3_i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode3(MUL, 0, 0, 1, Some(line)).unwrap();
        chunk.encode3(MUL, 0, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        assert!(vm.stack[0].get_int()? == 6);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::float(5_f64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3_i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(2)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode3(MUL, 0, 0, 1, Some(line)).unwrap();
        chunk.encode3(MUL, 0, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let item = vm.stack[0];
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 30.0);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..501 {
            chunk.add_constant(Value::Int(i as i64));
        }
        chunk.encode2(CONST, 1, 1, Some(line))?;
        chunk.encode2(CONST, 2, 2, Some(line))?;
        chunk.encode2(CONST, 5, 5, Some(line))?;
        chunk.encode2(CONST, 500, 500, Some(line))?;
        chunk.encode3(MUL, 0, 1, 2, Some(line)).unwrap();
        chunk.encode3(MUL, 0, 5, 0, Some(line)).unwrap();
        chunk.encode3(MUL, 1, 500, 0, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
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
        let const0 = chunk.add_constant(Value::Int(18_i64)) as u16;
        let const1 = chunk.add_constant(Value::Int(2_i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(3)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode3(DIV, 0, 0, 1, Some(line)).unwrap();
        chunk.encode3(DIV, 0, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        assert!(vm.stack[0].get_int()? == 3);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::float(10_f64)) as u16;
        let const1 = chunk.add_constant(Value::Int(2_i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(2)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode3(DIV, 0, 0, 1, Some(line)).unwrap();
        chunk.encode3(DIV, 0, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let item = vm.stack[0];
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 2.5);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..501 {
            chunk.add_constant(Value::Int(i as i64));
        }
        chunk.encode2(CONST, 1, 1, Some(line))?;
        chunk.encode2(CONST, 2, 2, Some(line))?;
        chunk.encode2(CONST, 10, 10, Some(line))?;
        chunk.encode2(CONST, 500, 500, Some(line))?;
        chunk.encode3(DIV, 0, 2, 1, Some(line)).unwrap();
        chunk.encode3(DIV, 0, 10, 0, Some(line)).unwrap();
        chunk.encode3(DIV, 1, 500, 0, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let item = vm.stack[0];
        let item2 = vm.stack[1];
        assert!(item.is_int());
        assert!(item.get_int()? == 5);
        assert!(item2.get_int()? == 100);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::Int(10_i64)) as u16;
        let const1 = chunk.add_constant(Value::Int(0_i64)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode3(DIV, 0, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        let res = vm.execute(chunk);
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[rt]: Divide by zero error.");

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::float(10_f64)) as u16;
        let const1 = chunk.add_constant(Value::float(0_f64)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode3(DIV, 0, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        let res = vm.execute(chunk);
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[rt]: Divide by zero error.");

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::float(10_f64)) as u16;
        let const1 = chunk.add_constant(Value::Byte(0)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode3(DIV, 0, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        let res = vm.execute(chunk);
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[rt]: Divide by zero error.");
        Ok(())
    }
}
