use std::alloc;
use std::alloc::Layout;
use std::sync::Arc;

use crate::chunk::*;
use crate::error::*;
use crate::heap::*;
use crate::interner::*;
use crate::value::*;
use crate::HALT;

mod cons;
mod storage;
#[macro_use]
pub mod macros;
mod call;
mod call_collection;
mod exec_loop;

/// Size (in elements/Values) of the stack.
pub const STACK_CAP: usize = 1024;

const DEAD_CODE: [u8; 3] = [HALT, HALT, HALT];

pub struct GVm<ENV> {
    interner: Interner,
    heap: Option<Heap>,
    //stack: Vec<Value>,
    numbers: [Numeric64; STACK_CAP],
    //stack: [Value; STACK_CAP],
    stack: *mut Value,
    registers: *mut Value,
    globals: Globals,
    buitins: Vec<CallFunc<ENV>>,
    this_fn: Option<Value>,
    on_error: Option<Value>,

    err_frame: Option<CallFrame>,
    stack_top: usize,
    stack_max: usize,
    ip_ptr: *const u8,
    current_ip_ptr: *const u8,
    callframe_id: usize,
    defers: Vec<Value>,
    env: ENV,
}

pub type Vm = GVm<()>;

impl Default for GVm<()> {
    fn default() -> Self {
        Self::new()
    }
}

impl GVm<()> {
    pub fn new() -> Self {
        GVm::new_with_env(())
    }
}

impl<ENV> GVm<ENV> {
    pub fn new_with_env(env: ENV) -> Self {
        let globals = Globals::new();
        let stack_layout =
            Layout::array::<Value>(STACK_CAP).expect("Failed to get memory layout for stack!");
        let stack = unsafe { alloc::alloc(stack_layout) } as *mut Value;
        if stack.is_null() {
            // Out of memory...
            panic!("Unable to allocate a stack!");
        }
        // Initialize the stack else it will contain garbage.
        for i in 0..STACK_CAP {
            let val = unsafe { stack.add(i).as_mut().expect("cant be null!") };
            *val = Value::Undefined;
        }
        Self {
            interner: Interner::with_capacity(8192),
            heap: Some(Heap::new()),
            numbers: [Numeric64 { int: 0 }; STACK_CAP],
            stack, //: [Value::Undefined; STACK_CAP],
            registers: stack,
            globals,
            buitins: Vec::new(),
            this_fn: None,
            on_error: None,
            err_frame: None,
            stack_top: 0,
            stack_max: 0,
            ip_ptr: DEAD_CODE.as_ptr(),
            current_ip_ptr: DEAD_CODE.as_ptr(),
            callframe_id: 0,
            defers: Vec::new(),
            env,
        }
    }

    pub fn stack(&self, idx: usize) -> Value {
        unsafe { *self.stack.add(idx) }
    }

    pub fn stack_mut(&mut self, idx: usize) -> &mut Value {
        unsafe { self.stack.add(idx).as_mut().expect("cant be null!") }
    }

    pub fn stack_slice(&self) -> &[Value] {
        unsafe { std::slice::from_raw_parts(self.stack, STACK_CAP) }
    }

    pub fn stack_slice_mut(&mut self) -> &mut [Value] {
        unsafe { std::slice::from_raw_parts_mut(self.stack, STACK_CAP) }
    }

    /// Return the register for idx.
    pub fn register(&self, idx: usize) -> Value {
        unsafe { *self.registers.add(idx).as_mut().expect("cant be null!") }
    }

    pub fn register_slice<'b>(&self) -> &'b [Value] {
        unsafe {
            std::slice::from_raw_parts(self.stack.add(self.stack_top), STACK_CAP - self.stack_top)
        }
    }

    /// Return the int representation of register idx or error if not an integral type.
    pub fn register_int(&self, idx: usize) -> VMResult<i64> {
        let reg = self.register(idx);
        match reg {
            Value::Byte(b) => Ok(b as i64),
            Value::Int32(i) => Ok(i as i64),
            Value::UInt32(i) => Ok(i as i64),
            Value::Int64(handle) => Ok(self.get_int(handle)),
            Value::UInt64(handle) => Ok(self.get_uint(handle) as i64), // XXX TODO- overflow.
            _ => Err(VMError::new_value(format!("Not an integer: {:?}", reg))),
        }
    }

    /// Return the current register for idx, if it is stored on heap then dereference it first.
    pub fn register_unref(&self, idx: usize) -> Value {
        let reg = self.register(idx);
        match reg {
            Value::Value(handle) => self.heap().get_value(handle),
            _ => reg,
        }
    }

    pub fn register_mut<'b>(&self, idx: usize) -> &'b mut Value {
        unsafe { self.registers.add(idx).as_mut().expect("cant be null!") }
    }

    pub fn env(&self) -> &ENV {
        &self.env
    }

    pub fn env_mut(&mut self) -> &mut ENV {
        &mut self.env
    }

    fn heap(&self) -> &Heap {
        self.heap.as_ref().expect("VM must have a Heap!")
    }

    fn heap_mut(&mut self) -> &mut Heap {
        self.heap.as_mut().expect("VM must have a Heap!")
    }

    /// Set the internal registers pointer, do this when the registers start position changes.
    fn make_registers(&mut self) {
        unsafe {
            self.registers = self.stack.add(self.stack_top);
        }
    }

    fn mk_str(&mut self, reg1: u16, reg2: u16) -> VMResult<Value> {
        let mut val = String::new();
        for reg in reg1..=reg2 {
            let v = self.register_unref(reg as usize);
            val.push_str(&v.pretty_value(self));
        }
        let val = self.alloc_string(val);
        Ok(val)
    }

    fn is_eq(&self, reg1: u16, reg2: u16) -> VMResult<Value> {
        let mut val = Value::False;
        if reg1 == reg2 {
            val = Value::True;
        } else {
            let mut val1 = self.register_unref(reg1 as usize);
            for reg in reg1..reg2 {
                let val2 = self.register_unref(reg as usize + 1);
                if val1 == val2 {
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

    pub fn add_builtin(&mut self, func: CallFuncSig<ENV>) -> Value {
        let result = self.buitins.len();
        self.buitins.push(CallFunc { func });
        Value::Builtin(result as u32)
    }

    /// Return the builtin function at idx.
    /// Note, will panic if idx is not a valid builtin index.
    pub fn get_builtin(&self, idx: u32) -> &CallFuncSig<ENV> {
        &self.buitins[idx as usize].func
    }

    pub fn is_equal_pair(&self, val1: Value, val2: Value) -> VMResult<Value> {
        let mut val = Value::False;
        if val1 == val2 {
            val = Value::True;
        } else if val1.is_int() && val2.is_int() {
            if val1.get_int(self)? == val2.get_int(self)? {
                val = Value::True;
            }
        } else if val1.is_number() && val2.is_number() {
            if (val1.get_float(self)? - val2.get_float(self)?).abs() < f64::EPSILON {
                val = Value::True;
            }
        } else {
            match val1 {
                Value::StringConst(s1) => {
                    if let Value::String(v2) = val2 {
                        let s2 = self.heap().get_string(v2);
                        if self.get_interned(s1) == s2 {
                            val = Value::True;
                        }
                    }
                }
                Value::String(h1) => {
                    let s1 = self.heap().get_string(h1);
                    if let Value::StringConst(s2) = val2 {
                        if s1 == self.get_interned(s2) {
                            val = Value::True;
                        }
                    }
                }
                Value::Vector(h1) => {
                    if let Value::Vector(h2) = val2 {
                        let v1 = self.heap().get_vector(h1);
                        let v2 = self.heap().get_vector(h2);
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
                        let b1 = self.heap().get_bytes(h1);
                        let b2 = self.heap().get_bytes(h2);
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

    fn is_equal(&self, reg1: u16, reg2: u16) -> VMResult<Value> {
        let mut val = Value::False;
        if reg1 == reg2 {
            val = Value::True
        } else {
            let mut val1 = self.register_unref(reg1 as usize);
            for reg in reg1..reg2 {
                let val2 = self.register_unref(reg as usize + 1);
                val = self.is_equal_pair(val1, val2)?;
                if val == Value::False {
                    break;
                }
                val1 = val2;
            }
        }
        Ok(val)
    }

    /// Runs a lambda.  Will save and restore the VM state even on error, chunk is expected to be a
    /// callable with params and any captures (closure) in caps.
    /// This is useful for macro expansion, eval and things like that.  It can be safely used while
    /// the VM is currently executing bytecode.
    pub fn do_call(
        &mut self,
        chunk: Arc<Chunk>,
        params: &[Value],
        caps: Option<&[Handle]>,
    ) -> VMResult<Value> {
        let stack_top = self.stack_top;
        let stack_max = self.stack_max;
        let ip = self.ip_ptr;
        let this_fn = self.this_fn;
        let on_error = self.on_error;
        self.this_fn = None;
        self.on_error = None;
        self.stack_top = self.stack_max;
        self.stack_max = self.stack_top + chunk.input_regs + chunk.extra_regs;
        *self.stack_mut(self.stack_top) = Value::UInt32(params.len() as u32);
        if !params.is_empty() {
            let r = self.stack_top + 1..self.stack_top + 1 + params.len();
            self.stack_slice_mut()[r].copy_from_slice(params);
        }
        if chunk.rest {
            self.make_registers();
            let (rest_reg, h) = self.setup_rest(&chunk, 0, params.len() as u16);
            if let Some(caps) = caps {
                let cap_first = (chunk.args + chunk.opt_args + 1) as usize;
                for (i, c) in caps.iter().enumerate() {
                    mov_register!(self, cap_first + i, Value::Value(*c));
                }
            }
            mov_register!(self, rest_reg, h);
        } else if let Some(caps) = caps {
            let cap_first = (chunk.args + chunk.opt_args + 1) as usize;
            for (i, c) in caps.iter().enumerate() {
                mov_register!(self, cap_first + i, Value::Value(*c));
            }
        }
        let res = self.execute2(chunk).map(|_| self.stack(self.stack_top));
        self.stack_top = stack_top;
        self.stack_max = stack_max;
        self.ip_ptr = ip;
        self.this_fn = this_fn;
        self.on_error = on_error;
        res
    }

    /// Executes chunk.  Will save the current VM state and restore on success or leave it on error.
    /// This allows a debugger to work with the "broken" image.
    pub fn execute(&mut self, chunk: Arc<Chunk>) -> VMResult<Value> {
        let stack_top = self.stack_top;
        let stack_max = self.stack_max;
        let ip = self.ip_ptr;
        let this_fn = self.this_fn;
        let on_error = self.on_error;
        self.this_fn = None;
        self.stack_top = self.stack_max;
        self.stack_max = self.stack_top + chunk.input_regs + chunk.extra_regs;

        // Return on error without resetting the VM.
        // This is to allow debugging a live image/vm.
        self.execute2(chunk)?;
        let res = self.stack(self.stack_top);

        self.stack_top = stack_top;
        self.stack_max = stack_max;
        self.ip_ptr = ip;
        self.this_fn = this_fn;
        self.on_error = on_error;
        Ok(res)
    }

    /// Reset the VM to default settings.  Useful for cleaning up if you want to abort an execute()
    /// that errored out.
    pub fn reset(&mut self) {
        self.this_fn = None;
        self.on_error = None;
        self.err_frame = None;
        self.stack_top = 0;
        self.stack_max = 0;
        self.ip_ptr = DEAD_CODE.as_ptr();
        self.current_ip_ptr = DEAD_CODE.as_ptr();
        self.callframe_id = 0;
        // XXX TODO- should probably run any defers before the reset.
        self.defers = Vec::new();
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
                        ip: self.ip_ptr,
                        current_ip: self.current_ip_ptr,
                        this_fn: self.this_fn,
                        defers: std::mem::take(&mut self.defers),
                        on_error: self.on_error,
                        called: Value::Undefined,
                    });
                }
                if let Some(on_error) = self.on_error {
                    self.make_registers();
                    *self.register_mut(1) = Value::Keyword(self.intern(e.key));
                    *self.register_mut(2) = match &e.obj {
                        VMErrorObj::Message(msg) => Value::StringConst(self.intern(msg)),
                        VMErrorObj::Object(v) => *v,
                    };
                    self.on_error = None;
                    match self.make_call(on_error, chunk.clone(), 0, 2, true) {
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
        if let Value::Int32(i) = val {
            Ok(*i as i64)
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
        chunk.add_constant(Value::Int32(1));
        chunk.add_constant(Value::Int32(2));
        chunk.add_constant(Value::Int32(3));
        chunk.add_constant(Value::Int32(4));
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
        let result = vm.stack(0).get_int(&vm)?;
        assert!(result == 2);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CAR, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack(0).get_int(&vm)?;
        assert!(result == 1);

        // car with nil
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, Some(line)).unwrap();
        chunk.encode2(CAR, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack(0).is_nil());

        // car with nil on heap
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, Some(line)).unwrap();
        chunk.encode2(CAR, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack(0).is_nil());

        // cdr with nil
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CDR, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack(0).is_nil());

        // cdr with nil on heap
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, Some(line)).unwrap();
        chunk.encode2(CDR, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack(0).is_nil());

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 2, Some(line)).unwrap();
        chunk.encode2(XAR, 1, 2, Some(line)).unwrap();
        chunk.encode2(CAR, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack(0).get_int(&vm)?;
        assert!(result == 3);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 3, Some(line)).unwrap();
        chunk.encode2(XDR, 1, 2, Some(line)).unwrap();
        chunk.encode2(CDR, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack(0).get_int(&vm)?;
        assert!(result == 4);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, Some(line)).unwrap();
        chunk.encode2(CONST, 3, 2, Some(line)).unwrap();
        chunk.encode2(XAR, 2, 3, Some(line)).unwrap();
        chunk.encode2(CAR, 0, 2, Some(line)).unwrap();
        chunk.encode2(CDR, 3, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut chunk = Arc::new(chunk);
        assert!(
            vm.execute(chunk.clone()).is_err(),
            "XAR on Nil is an error."
        );

        // Previous error stashed a clone of chunk so make a new one.
        Arc::make_mut(&mut chunk);
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, Some(line)).unwrap();
        chunk.encode2(CONST, 3, 3, Some(line)).unwrap();
        chunk.encode2(XDR, 2, 3, Some(line)).unwrap();
        chunk.encode2(CDR, 0, 2, Some(line)).unwrap();
        chunk.encode2(CAR, 3, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut chunk = Arc::new(chunk);
        assert!(vm.execute(chunk.clone()).is_err(), "Can not XDR Nil");

        // Previous error stashed a clone of chunk so make a new one.
        Arc::make_mut(&mut chunk);
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
        let result = vm.stack(0);
        if let Value::Pair(h) = result {
            let (car, cdr) = vm.heap().get_pair(h);
            assert!(get_int(&vm, &car)? == 1);
            if let Value::Pair(h2) = cdr {
                let (car, cdr) = vm.heap().get_pair(h2);
                assert!(get_int(&vm, &car)? == 2);
                if let Value::Pair(h3) = cdr {
                    let (car, cdr) = vm.heap().get_pair(h3);
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
        let result = vm.stack(0);
        assert!(result.is_nil());
        Ok(())
    }

    #[test]
    fn test_store() -> VMResult<()> {
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        for i in 0..257 {
            chunk.add_constant(Value::Int32(i));
        }
        chunk.encode2(CONST, 0, 0, Some(line)).unwrap();
        chunk.encode2(CONST, 1, 255, Some(line)).unwrap();
        chunk.encode2(ADD, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;

        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack(0).get_int(&vm)?;
        assert!(result == 255);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 1, 256, Some(line)).unwrap();
        chunk.encode2(ADD, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;

        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack(0).get_int(&vm)?;
        assert!(result == 255 + 256);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(MOV, 1, 0, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let result = vm.stack(1).get_int(&vm)?;
        assert!(result == 256);
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack(1).get_int(&vm)?;
        assert!(result == 255 + 256);

        let mut vm = Vm::new();
        *vm.stack_mut(0) = vm.new_upval(Value::Int32(1));
        *vm.stack_mut(1) = Value::Int32(10);
        *vm.stack_mut(2) = Value::Int32(1);
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(MOV, 1, 0, Some(line)).unwrap();
        chunk.encode2(ADD, 1, 2, Some(line)).unwrap();
        chunk.encode2(ADD, 1, 2, Some(line)).unwrap();
        chunk.encode2(ADD, 1, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let result = vm.stack(0).unref(&vm).get_int(&vm)?;
        assert!(result == 1);
        let result = vm.stack(1).unref(&vm).get_int(&vm)?;
        assert!(result == 4);

        Ok(())
    }

    #[test]
    fn test_global() -> VMResult<()> {
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let mut vm = Vm::new();
        let slot = vm.globals.reserve();
        let slot2 = vm.globals.reserve();
        let const2 = chunk.add_constant(Value::Int32(42)) as u16;
        vm.globals.set(slot, Value::Int32(11));
        chunk.encode_refi(1, slot, Some(line))?;
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack(1).unref(&vm).get_int(&vm)? == 11);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 1, const2, Some(line))?;
        chunk.encode_def(1, slot2, Some(line), false)?;
        chunk.encode_refi(2, slot2, Some(line))?;
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack(2).unref(&vm).get_int(&vm)? == 42);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        vm.globals.set(slot, Value::Int32(11));
        let const2 = chunk.add_constant(Value::Int32(43)) as u16;
        let const3 = chunk.add_constant(Value::Int32(53)) as u16;
        chunk.encode2(CONST, 1, const2, Some(line))?;
        chunk.encode2(CONST, 3, const3, Some(line))?;
        chunk.encode_def(1, slot2, Some(line), false)?;
        chunk.encode_def(3, slot2, Some(line), true)?;
        chunk.encode_refi(2, slot2, Some(line))?;
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack(2).unref(&vm).get_int(&vm)? == 43);

        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        vm.globals.set(slot2, Value::Int32(11));
        assert!(vm.globals.get(slot2).get_int(&vm)? == 11);
        let const2 = chunk.add_constant(Value::Int32(43)) as u16;
        let const3 = chunk.add_constant(Value::Int32(53)) as u16;
        chunk.encode2(CONST, 1, const2, Some(line))?;
        chunk.encode2(CONST, 3, const3, Some(line))?;
        chunk.encode_def(1, slot, Some(line), false)?;
        chunk.encode_def(3, slot, Some(line), true)?;
        chunk.encode_refi(2, slot, Some(line))?;
        chunk.encode_refi(5, slot, Some(line))?;
        chunk.encode2(SET, 5, 3, Some(line))?;
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack(2).unref(&vm).get_int(&vm)? == 43);
        assert!(vm.stack(5).unref(&vm).get_int(&vm)? == 53);
        assert_eq!(vm.globals.get(slot).get_int(&vm)?, 43);

        let mut vm = Vm::new();
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        let slot = vm.globals.reserve();
        let const2 = chunk.add_constant(Value::Int32(44)) as u16;
        let const3 = chunk.add_constant(Value::Int32(53)) as u16;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(CONST, 3, const3, Some(line))?;
        chunk.encode_def(2, slot, Some(line), true)?;
        chunk.encode_def(3, slot, Some(line), true)?;
        chunk.encode_refi(0, slot, Some(line))?;
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack(0).unref(&vm).get_int(&vm)? == 44);

        let mut vm = Vm::new();
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        let slot = vm.globals.reserve();
        let const2 = chunk.add_constant(Value::Int32(45)) as u16;
        let const3 = chunk.add_constant(Value::Int32(55)) as u16;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(CONST, 3, const3, Some(line))?;
        chunk.encode_def(2, slot, Some(line), true)?;
        chunk.encode_def(3, slot, Some(line), false)?;
        chunk.encode_refi(0, slot, Some(line))?;
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack(0).unref(&vm).get_int(&vm)? == 55);

        let mut vm = Vm::new();
        let mut chunk = Arc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        let slot = vm.globals.reserve();
        let const2 = chunk.add_constant(Value::Int32(45)) as u16;
        let const3 = chunk.add_constant(Value::Int32(1)) as u16;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(CONST, 3, const3, Some(line))?;
        chunk.encode_def(2, slot, Some(line), true)?;
        chunk.encode_def(3, slot, Some(line), false)?;
        chunk.encode_refi(0, slot, Some(line))?;
        chunk.encode2(MOV, 5, 0, Some(line))?;
        chunk.encode2(SET, 5, 3, Some(line))?;
        chunk.encode2(ADD, 5, 3, Some(line))?;
        chunk.encode2(ADD, 5, 3, Some(line))?;
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        assert!(vm.stack(0).unref(&vm).get_int(&vm)? == 1);
        assert!(vm.stack(0).unref(&vm).get_int(&vm)? == 1);
        assert!(vm.stack(0).unref(&vm).get_int(&vm)? == 1);
        assert!(vm.stack(0).unref(&vm).get_int(&vm)? == 1);
        assert!(vm.stack(5).unref(&vm).get_int(&vm)? == 3);
        assert!(vm.globals.get(slot).get_int(&vm)? == 1);

        Ok(())
    }

    #[test]
    //#[ignore]
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
        // XXX TODO- get rid of this pause when gc fixed.
        vm.pause_gc();
        let mut chunk = Chunk::new("no_file", 1);
        let n = chunk.add_constant(Value::Int32(5000)) as u16;
        let x = chunk.add_constant(vm.alloc_f64(0.2)) as u16;
        let su = chunk.add_constant(vm.alloc_f64(0.0)) as u16;
        let mu = chunk.add_constant(vm.alloc_f64(10.0)) as u16;
        let pu = chunk.add_constant(vm.alloc_f64(0.0)) as u16;
        let zero = chunk.add_constant(Value::Int32(0)) as u16;
        let zerof = chunk.add_constant(vm.alloc_f64(0.0)) as u16;
        let twof = chunk.add_constant(vm.alloc_f64(2.0)) as u16;
        let hundred = chunk.add_constant(Value::Int32(100)) as u16;
        let one = chunk.add_constant(Value::Int32(1)) as u16;
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
        let jmp0_idx = chunk.add_jump(chunk.code.len() as u32);
        chunk.encode2(CONST, 3, zerof, Some(line))?;
        chunk.encode2(CONST, 7, zero, Some(line))?; // j
        let jmp1_idx = chunk.add_jump(chunk.code.len() as u32);
        // loop j .. 100
        // (set! mu (/ (+ mu 2.0) 2.0))
        chunk.encode2(ADD, 4, 8, Some(line))?;
        chunk.encode2(DIV, 4, 8, Some(line))?;
        // (vec-set! pol j mu)))
        chunk.encode3(VECSTH, 10, 4, 7, Some(line))?;

        chunk.encode2(INC, 7, 1, Some(line))?;
        chunk.encode3(JMPLT, 7, 100, jmp1_idx as u16, Some(line))?;

        chunk.encode2(CONST, 7, zero, Some(line))?; // j
        let jmp2_idx = chunk.add_jump(chunk.code.len() as u32);
        // (dotimes-i j 100 (j2)
        //   (set! su (+ (vec-nth pol j) (* su x))))
        chunk.encode2(MUL, 3, 2, Some(line))?; // FROM 3
        chunk.encode3(VECNTH, 10, 51, 7, Some(line))?;
        chunk.encode2(ADD, 3, 51, Some(line))?;

        chunk.encode2(INC, 7, 1, Some(line))?;
        chunk.encode3(JMPLT, 7, 100, jmp2_idx as u16, Some(line))?;
        // (set! pu (+ pu su))))
        chunk.encode2(ADD, 5, 3, Some(line))?;

        chunk.encode2(INC, 6, 1, Some(line))?;
        chunk.encode3(JMPLT, 6, 1, jmp0_idx as u16, Some(line))?;

        chunk.encode0(RET, Some(line))?;
        //chunk.disassemble_chunk(&vm)?;
        //assert!(false);

        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let result = vm.stack(5).get_float(&vm)?;
        assert!(result == 12500.0);

        Ok(())
    }

    #[test]
    fn test_lambda() -> VMResult<()> {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        chunk.encode2(ADD, 1, 2, Some(line)).unwrap();
        chunk.encode2(MOV, 3, 1, Some(line)).unwrap();
        chunk.encode1(SRET, 3, Some(line))?;
        chunk.args = 2;
        let add = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Int32(10)) as u16;
        chunk.encode2(CONST, 2, const1, Some(line)).unwrap();
        chunk.encode2(ADD, 1, 2, Some(line)).unwrap();
        chunk.encode2(MOV, 3, 1, Some(line)).unwrap();
        chunk.encode1(SRET, 3, Some(line))?;
        chunk.args = 1;
        let add_ten = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        *vm.stack_mut(0) = add;
        *vm.stack_mut(1) = add_ten;
        *vm.stack_mut(3) = Value::Int32(5);
        *vm.stack_mut(4) = Value::Int32(2);
        *vm.stack_mut(6) = Value::Int32(2);
        chunk.encode3(CALL, 0, 2, 2, Some(line)).unwrap();
        chunk.encode3(CALL, 1, 1, 5, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;

        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let result = vm.stack(2).get_int(&vm)?;
        assert!(result == 7);
        let result = vm.stack(5).get_int(&vm)?;
        assert!(result == 12);
        let result = vm.stack(7).get_int(&vm)?;
        assert!(result == 10);

        Ok(())
    }

    #[test]
    fn test_tcall() -> VMResult<()> {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        chunk.encode2(MOV, 3, 2, Some(line)).unwrap();
        chunk.encode2(ADD, 3, 1, Some(line)).unwrap();
        chunk.encode1(SRET, 3, Some(line))?;
        chunk.args = 2;
        let add = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Int32(10)) as u16;
        let const2 = chunk.add_constant(add) as u16;
        chunk.encode2(CONST, 2, const1, Some(line)).unwrap();
        chunk.encode2(CONST, 3, const2, Some(line)).unwrap();
        chunk.encode2(TCALL, 3, 2, Some(line)).unwrap();
        // The TCALL will keep HALT from executing.
        chunk.encode0(HALT, Some(line))?;
        chunk.args = 1;
        let add_ten = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        *vm.stack_mut(1) = Value::Int32(5);
        *vm.stack_mut(2) = Value::Int32(2);
        *vm.stack_mut(4) = Value::Int32(2);
        *vm.stack_mut(50) = add;
        *vm.stack_mut(60) = add_ten;
        chunk.encode3(CALL, 60, 1, 3, Some(line)).unwrap();
        // tail call at the top level does not make sense
        //chunk.encode2(TCALL, 50, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;

        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        //let result = vm.stack(0).get_int(&vm)?;
        //assert!(result == 7);
        let result = vm.stack(3).get_int(&vm)?;
        assert!(result == 12);

        Ok(())
    }

    #[test]
    fn test_builtin() -> VMResult<()> {
        fn add_b(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
            if registers.len() != 2 {
                return Err(VMError::new_vm("test add: wrong number of args."));
            }
            Ok(Value::Int32(
                (registers[0].get_int(vm)? + registers[1].get_int(vm)?) as i32,
            ))
        }
        fn add_10(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
            if registers.len() != 1 {
                return Err(VMError::new_vm("test add_10: wrong number of args."));
            }
            Ok(Value::Int32(registers[0].get_int(vm)? as i32 + 10))
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
        let const1 = chunk.add_constant(vm.add_builtin(add_b)) as u16;
        chunk.encode2(CONST, 10, const1, Some(line)).unwrap();
        chunk.encode2(MOV, 4, 1, Some(line)).unwrap();
        chunk.encode2(MOV, 5, 2, Some(line)).unwrap();
        chunk.encode3(CALL, 10, 2, 3, Some(line)).unwrap();
        chunk.encode1(SRET, 3, Some(line))?;
        chunk.args = 2;
        let add = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(vm.add_builtin(add_b)) as u16;
        chunk.encode2(CONST, 10, const1, Some(line)).unwrap();
        chunk.encode2(TCALL, 10, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        chunk.args = 2;
        let tadd = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(vm.add_builtin(add_10)) as u16;
        chunk.encode2(CONST, 4, const1, Some(line)).unwrap();
        chunk.encode2(MOV, 3, 1, Some(line)).unwrap();
        chunk.encode3(CALL, 4, 1, 2, Some(line)).unwrap();
        chunk.encode1(SRET, 2, Some(line))?;
        chunk.args = 1;
        let add_ten = vm.alloc_lambda(Arc::new(chunk));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        *vm.stack_mut(0) = add;
        *vm.stack_mut(1) = add_ten;
        *vm.stack_mut(3) = Value::Int32(6);
        *vm.stack_mut(4) = Value::Int32(3);
        *vm.stack_mut(8) = Value::Int32(12);
        let const1 = chunk.add_constant(vm.add_builtin(make_str)) as u16;
        chunk.encode3(CALL, 0, 2, 2, Some(line)).unwrap();
        chunk.encode3(CALL, 1, 1, 7, Some(line)).unwrap();
        chunk.encode2(CONST, 15, const1, Some(line)).unwrap();
        chunk.encode3(CALL, 15, 0, 15, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let result = vm.stack(2).get_int(&vm)?;
        assert!(result == 9);
        let result = vm.stack(7).get_int(&vm)?;
        assert!(result == 22);
        match vm.stack(15) {
            Value::String(h) => assert!(vm.heap().get_string(h) == "builtin hello"),
            _ => panic!("bad make_str call"),
        }

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        for i in 0..100 {
            *vm.stack_mut(i) = Value::Undefined;
        }
        *vm.stack_mut(0) = tadd;
        *vm.stack_mut(1) = add_ten;
        *vm.stack_mut(3) = Value::Int32(6);
        *vm.stack_mut(4) = Value::Int32(3);
        *vm.stack_mut(6) = Value::Int32(12);
        let const1 = chunk.add_constant(vm.add_builtin(make_str)) as u16;
        chunk.encode3(CALL, 0, 2, 2, Some(line)).unwrap();
        chunk.encode3(CALL, 1, 1, 5, Some(line)).unwrap();
        chunk.encode2(CONST, 10, const1, Some(line)).unwrap();
        chunk.encode3(CALL, 10, 0, 11, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let result = vm.stack(2).get_int(&vm)?;
        assert!(result == 9);
        let result = vm.stack(5).get_int(&vm)?;
        assert!(result == 22);
        match vm.stack(11) {
            Value::String(h) => assert!(vm.heap().get_string(h) == "builtin hello"),
            _ => panic!("bad make_str call"),
        }

        Ok(())
    }

    #[test]
    fn test_jumps() -> VMResult<()> {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::Int32(2_i32)) as u16;
        let const1 = chunk.add_constant(Value::Int32(3_i32)) as u16;
        *vm.stack_mut(0) = Value::True;
        *vm.stack_mut(1) = Value::False;
        *vm.stack_mut(2) = Value::Nil;
        *vm.stack_mut(3) = Value::Int32(0);
        let line = 1;
        chunk.encode2(CONST, 4, const0, Some(line))?;
        let jmp = chunk.add_jump(chunk.code.len() as u32 + 5);
        chunk.encode1(JMP, jmp as u16, Some(line))?;
        chunk.encode2(CONST, 4, const1, Some(line))?;
        chunk.encode2(CONST, 5, const1, Some(line))?;

        chunk.encode2(CONST, 6, const0, Some(line))?;
        let jmp = chunk.add_jump(chunk.code.len() as u32 + 5);
        chunk.encode1(JMP, jmp as u16, Some(line))?;
        chunk.encode2(CONST, 6, const1, Some(line))?;
        chunk.encode2(CONST, 7, const1, Some(line))?;

        let jmp = chunk.add_jump(chunk.code.len() as u32 + 7);
        chunk.encode1(JMP, jmp as u16, Some(line))?;
        let jmp_back = chunk.add_jump(chunk.code.len() as u32);
        chunk.encode2(CONST, 8, const0, Some(line))?;
        let jmp = chunk.add_jump(chunk.code.len() as u32 + 4);
        chunk.encode1(JMP, jmp as u16, Some(line))?;
        chunk.encode1(JMP, jmp_back as u16, Some(line))?;
        chunk.encode2(CONST, 9, const1, Some(line))?;

        chunk.encode2(CONST, 10, const0, Some(line))?;
        let jmp = chunk.add_jump(chunk.code.len() as u32 + 6);
        chunk.encode2(JMPT, 0, jmp as u16, Some(line))?;
        chunk.encode2(CONST, 10, const1, Some(line))?;
        chunk.encode2(CONST, 11, const1, Some(line))?;

        chunk.encode2(CONST, 12, const0, Some(line))?;
        let jmp = chunk.add_jump(chunk.code.len() as u32 + 6);
        chunk.encode2(JMPT, 3, jmp as u16, Some(line))?;
        chunk.encode2(CONST, 12, const1, Some(line))?;
        chunk.encode2(CONST, 13, const1, Some(line))?;

        chunk.encode2(CONST, 14, const0, Some(line))?;
        let jmp = chunk.add_jump(chunk.code.len() as u32 + 6);
        chunk.encode2(JMPF, 1, jmp as u16, Some(line))?;
        chunk.encode2(CONST, 14, const1, Some(line))?;
        chunk.encode2(CONST, 15, const1, Some(line))?;

        chunk.encode2(CONST, 16, const0, Some(line))?;
        let jmp = chunk.add_jump(chunk.code.len() as u32 + 6);
        chunk.encode2(JMPF, 2, jmp as u16, Some(line))?;
        chunk.encode2(CONST, 16, const1, Some(line))?;
        chunk.encode2(CONST, 17, const1, Some(line))?;

        chunk.encode2(CONST, 18, const0, Some(line))?;
        let jmp = chunk.add_jump(chunk.code.len() as u32 + 6);
        chunk.encode2(JMPT, 1, jmp as u16, Some(line))?;
        chunk.encode2(CONST, 18, const1, Some(line))?;
        chunk.encode2(CONST, 19, const1, Some(line))?;

        chunk.encode2(CONST, 20, const0, Some(line))?;
        let jmp = chunk.add_jump(chunk.code.len() as u32 + 6);
        chunk.encode2(JMPT, 2, jmp as u16, Some(line))?;
        chunk.encode2(CONST, 20, const1, Some(line))?;
        chunk.encode2(CONST, 21, const1, Some(line))?;

        chunk.encode2(CONST, 22, const0, Some(line))?;
        let jmp = chunk.add_jump(chunk.code.len() as u32 + 6);
        chunk.encode2(JMPF, 0, jmp as u16, Some(line))?;
        chunk.encode2(CONST, 22, const1, Some(line))?;
        chunk.encode2(CONST, 23, const1, Some(line))?;

        chunk.encode2(CONST, 24, const0, Some(line))?;
        let jmp = chunk.add_jump(chunk.code.len() as u32 + 6);
        chunk.encode2(JMPF, 3, jmp as u16, Some(line))?;
        chunk.encode2(CONST, 24, const1, Some(line))?;
        chunk.encode2(CONST, 25, const1, Some(line))?;

        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        assert!(vm.stack(4).get_int(&vm)? == 2);
        assert!(vm.stack(5).get_int(&vm)? == 3);
        assert!(vm.stack(6).get_int(&vm)? == 2);
        assert!(vm.stack(7).get_int(&vm)? == 3);
        assert!(vm.stack(8).get_int(&vm)? == 2);
        assert!(vm.stack(9).get_int(&vm)? == 3);
        assert!(vm.stack(10).get_int(&vm)? == 2);
        assert!(vm.stack(11).get_int(&vm)? == 3);
        assert!(vm.stack(12).get_int(&vm)? == 2);
        assert!(vm.stack(13).get_int(&vm)? == 3);
        assert!(vm.stack(14).get_int(&vm)? == 2);
        assert!(vm.stack(15).get_int(&vm)? == 3);
        assert!(vm.stack(16).get_int(&vm)? == 2);
        assert!(vm.stack(17).get_int(&vm)? == 3);
        assert!(vm.stack(18).get_int(&vm)? == 3);
        assert!(vm.stack(19).get_int(&vm)? == 3);
        assert!(vm.stack(20).get_int(&vm)? == 3);
        assert!(vm.stack(21).get_int(&vm)? == 3);
        assert!(vm.stack(22).get_int(&vm)? == 3);
        assert!(vm.stack(23).get_int(&vm)? == 3);
        assert!(vm.stack(24).get_int(&vm)? == 3);
        assert!(vm.stack(25).get_int(&vm)? == 3);
        Ok(())
    }

    #[test]
    fn test_vecs() -> VMResult<()> {
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("no_file", 1);
        let zero = chunk.add_constant(Value::Int32(0)) as u16;
        let hundred = chunk.add_constant(Value::Int32(100)) as u16;
        let one = chunk.add_constant(Value::Int32(1)) as u16;
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
        assert!(vm.stack(5).get_int(&vm)? == 1);
        assert!(vm.stack(6).get_int(&vm)? == 0);
        assert!(vm.stack(7).get_int(&vm)? == 1);
        assert!(vm.stack(8).get_int(&vm)? == 0);

        assert!(vm.stack(15).get_int(&vm)? == 0);
        assert!(vm.stack(16).get_int(&vm)? == 1);
        assert!(vm.stack(17).get_int(&vm)? == 1);
        assert!(vm.stack(18).get_int(&vm)? == 0);
        let vc = vm.stack(1);
        if let Value::Vector(h) = vc {
            let v = vm.get_vector(h);
            assert!(v.len() == 101);
            assert!(v[0].get_int(&vm)? == 0);
        }
        let vc = vm.stack(10);
        if let Value::Vector(h) = vc {
            let v = vm.get_vector(h);
            assert!(v.len() == 2);
            assert!(v[0].get_int(&vm)? == 0);
            assert!(v[1].get_int(&vm)? == 1);
        }
        let vc = vm.stack(20);
        if let Value::Vector(h) = vc {
            let v = vm.get_vector(h);
            assert!(v.len() == 100);
            assert!(v[0].get_int(&vm)? == 0);
            assert!(v[1].is_undef());
            assert!(v[99].is_undef());
        }
        Ok(())
    }

    #[test]
    fn test_add() -> VMResult<()> {
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const0 = chunk.add_constant(Value::Int32(2_i32)) as u16;
        let const1 = chunk.add_constant(Value::Int32(3_i32)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(ADD, 0, 1, Some(line)).unwrap();
        chunk.encode2(ADD, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        assert!(vm.stack(0).get_int(&vm)? == 6);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(vm.alloc_f64(2_f64)) as u16;
        let const1 = chunk.add_constant(Value::Int32(3_i32)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(ADD, 0, 1, Some(line)).unwrap();
        chunk.encode2(ADD, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let item = vm.stack(0);
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float(&vm)? == 6.0);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..501 {
            chunk.add_constant(Value::Int32(i));
        }
        chunk.encode2(CONST, 1, 1, Some(line))?;
        chunk.encode2(CONST, 2, 2, Some(line))?;
        chunk.encode2(CONST, 5, 5, Some(line))?;
        chunk.encode2(CONST, 500, 500, Some(line))?;
        chunk.encode2(MOV, 0, 1, Some(line)).unwrap();
        chunk.encode2(ADD, 0, 2, Some(line)).unwrap();
        chunk.encode2(ADD, 0, 5, Some(line)).unwrap();
        chunk.encode2(ADD, 500, 0, Some(line)).unwrap();
        chunk.encode2(MOV, 1, 500, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let item = vm.stack(0);
        let item2 = vm.stack(1);
        assert!(item.is_int());
        assert!(item.get_int(&vm)? == 8);
        assert!(item2.get_int(&vm)? == 508);
        Ok(())
    }

    #[test]
    fn test_sub() -> VMResult<()> {
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const0 = chunk.add_constant(Value::Int32(2_i32)) as u16;
        let const1 = chunk.add_constant(Value::Int32(3_i32)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(SUB, 0, 1, Some(line)).unwrap();
        chunk.encode2(SUB, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        assert_eq!(vm.stack(0).get_int(&vm)?, -2);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(vm.alloc_f64(5_f64)) as u16;
        let const1 = chunk.add_constant(Value::Int32(3_i32)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(SUB, 0, 1, Some(line)).unwrap();
        chunk.encode2(SUB, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let item = vm.stack(0);
        assert!(!item.is_int(), "Item an int");
        assert!(item.is_number(), "Item not a number");
        assert_eq!(item.get_float(&vm)?, 1.0);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..501 {
            chunk.add_constant(Value::Int32(i));
        }
        chunk.encode2(CONST, 1, 1, Some(line))?;
        chunk.encode2(CONST, 2, 2, Some(line))?;
        chunk.encode2(CONST, 5, 5, Some(line))?;
        chunk.encode2(CONST, 500, 500, Some(line))?;
        chunk.encode2(SUB, 1, 2, Some(line)).unwrap();
        chunk.encode2(SUB, 5, 1, Some(line)).unwrap();
        chunk.encode2(MOV, 0, 5, Some(line)).unwrap();
        chunk.encode2(SUB, 500, 5, Some(line)).unwrap();
        chunk.encode2(MOV, 1, 500, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let item = vm.stack(0);
        let item2 = vm.stack(1);
        assert!(item.is_int(), "Item not an int");
        assert_eq!(item.get_int(&vm)?, 6);
        assert_eq!(item2.get_int(&vm)?, 494);
        Ok(())
    }

    #[test]
    fn test_mul() -> VMResult<()> {
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const0 = chunk.add_constant(Value::Int32(2_i32)) as u16;
        let const1 = chunk.add_constant(Value::Int32(3_i32)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(MUL, 0, 1, Some(line)).unwrap();
        chunk.encode2(MUL, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        assert!(vm.stack(0).get_int(&vm)? == 6);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(vm.alloc_f64(5_f64)) as u16;
        let const1 = chunk.add_constant(Value::Int32(3_i32)) as u16;
        let const2 = chunk.add_constant(Value::Byte(2)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(MUL, 0, 1, Some(line)).unwrap();
        chunk.encode2(MUL, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let item = vm.stack(0);
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float(&vm)? == 30.0);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..501 {
            chunk.add_constant(Value::Int32(i));
        }
        chunk.encode2(CONST, 1, 1, Some(line))?;
        chunk.encode2(CONST, 2, 2, Some(line))?;
        chunk.encode2(CONST, 5, 5, Some(line))?;
        chunk.encode2(CONST, 500, 500, Some(line))?;
        chunk.encode2(MOV, 0, 2, Some(line)).unwrap();
        chunk.encode2(MUL, 0, 1, Some(line)).unwrap();
        chunk.encode2(MUL, 0, 5, Some(line)).unwrap();
        chunk.encode2(MUL, 500, 0, Some(line)).unwrap();
        chunk.encode2(MOV, 1, 500, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let item = vm.stack(0);
        let item2 = vm.stack(1);
        assert!(item.is_int());
        assert!(item.get_int(&vm)? == 10);
        assert!(item2.get_int(&vm)? == 5000);
        Ok(())
    }

    #[test]
    fn test_div() -> VMResult<()> {
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const0 = chunk.add_constant(Value::Int32(18_i32)) as u16;
        let const1 = chunk.add_constant(Value::Int32(2_i32)) as u16;
        let const2 = chunk.add_constant(Value::Byte(3)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(DIV, 0, 1, Some(line)).unwrap();
        chunk.encode2(DIV, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let val10 = vm.alloc_f64(10_f64);
        let val0 = vm.alloc_f64(0_f64);
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        assert!(vm.stack(0).get_int(&vm)? == 3);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(val10) as u16;
        let const1 = chunk.add_constant(Value::Int32(2_i32)) as u16;
        let const2 = chunk.add_constant(Value::Byte(2)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(DIV, 0, 1, Some(line)).unwrap();
        chunk.encode2(DIV, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let item = vm.stack(0);
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float(&vm)? == 2.5);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..501 {
            chunk.add_constant(Value::Int32(i));
        }
        chunk.encode2(CONST, 1, 1, Some(line))?;
        chunk.encode2(CONST, 2, 2, Some(line))?;
        chunk.encode2(CONST, 10, 10, Some(line))?;
        chunk.encode2(CONST, 500, 500, Some(line))?;
        chunk.encode2(MOV, 0, 2, Some(line)).unwrap();
        chunk.encode2(DIV, 0, 1, Some(line)).unwrap();
        chunk.encode2(DIV, 10, 0, Some(line)).unwrap();
        chunk.encode2(MOV, 0, 10, Some(line)).unwrap();
        chunk.encode2(DIV, 500, 0, Some(line)).unwrap();
        chunk.encode2(MOV, 1, 500, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        let item = vm.stack(0);
        let item2 = vm.stack(1);
        assert!(item.is_int());
        assert!(item.get_int(&vm)? == 5);
        assert!(item2.get_int(&vm)? == 100);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::Int32(10_i32)) as u16;
        let const1 = chunk.add_constant(Value::Int32(0_i32)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(DIV, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        let res = vm.execute(chunk);
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[rt]: Divide by zero error.");

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(val10) as u16;
        let const1 = chunk.add_constant(val0) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(DIV, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        let res = vm.execute(chunk);
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[rt]: Divide by zero error.");

        let mut chunk = Chunk::new("no_file", 1);
        let const1 = chunk.add_constant(Value::Byte(0)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(DIV, 0, 1, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let chunk = Arc::new(chunk);
        let res = vm.execute(chunk);
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[rt]: Divide by zero error.");
        Ok(())
    }
}
