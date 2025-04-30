use std::alloc;
use std::alloc::Layout;
use std::sync::Arc;

use crate::{
    from_i56, CallFrame, CallFunc, CallFuncSig, Chunk, Globals, Handle, Heap, Interner, VMError,
    VMErrorObj, VMResult, Value, HALT,
};

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

/// Hold state of the VM, this is for making re-entrant calls on the VM.
struct VmState {
    stack_top: usize,
    stack_max: usize,
    ip: *const u8,
    current_ip: *const u8,
    this_fn: Option<Value>,
    on_error: Option<Value>,
    defers: Vec<Value>,
}

pub struct GVm<ENV> {
    interner: Interner,
    heap: Option<Heap>,
    //stack: [Value; STACK_CAP],
    stack: *mut Value,
    registers: *mut Value,
    globals: Globals,
    builtins: Vec<CallFunc<ENV>>,
    this_fn: Option<Value>,
    on_error: Option<Value>,

    err_frame: Option<CallFrame>,
    stack_top: usize,
    k_stack_top: Option<usize>, // Used for continuation/defer interaction.
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
            stack, //: [Value::Undefined; STACK_CAP],
            registers: stack,
            globals,
            builtins: Vec::new(),
            this_fn: None,
            on_error: None,
            err_frame: None,
            stack_top: 0,
            k_stack_top: None,
            stack_max: 0,
            ip_ptr: DEAD_CODE.as_ptr(),
            current_ip_ptr: DEAD_CODE.as_ptr(),
            callframe_id: 0,
            defers: Vec::new(),
            env,
        }
    }

    pub fn this_fn(&self) -> Option<Value> {
        self.this_fn
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
            Value::Int(i) => Ok(from_i56(&i)),
            _ => Err(VMError::new_value(format!(
                "Not an integer: {}",
                reg.display_value(self)
            ))),
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

    fn is_identical(&self, reg1: u16, reg2: u16) -> VMResult<Value> {
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
        let result = self.builtins.len();
        self.builtins.push(CallFunc { func });
        Value::Builtin(result as u32)
    }

    /// Return the builtin function at idx.
    /// Note, will panic if idx is not a valid builtin index.
    pub fn get_builtin(&self, idx: u32) -> &CallFuncSig<ENV> {
        &self.builtins[idx as usize].func
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
            // compare two floats by converting to f64 and using native equality check (IEEE)
            if val1.is_float() && val2.is_float() {
                if val1.get_float(self)? == val2.get_float(self)? {
                    val = Value::True;
                }
            } else {
                // we are comparing two numbers but they aren't both ints or both floats
                val = Value::False;
            }
        } else {
            match (val1, val2) {
                (Value::StringConst(s1), Value::CharCluster(l, c)) => {
                    let s2 = format!("{}", String::from_utf8_lossy(&c[0..l as usize]));
                    let s1 = self.get_interned(s1);
                    if s1 == s2 {
                        val = Value::True;
                    }
                }
                (Value::StringConst(s1), Value::CodePoint(c)) => {
                    let s2 = format!("{c}");
                    let s1 = self.get_interned(s1);
                    if s1 == s2 {
                        val = Value::True;
                    }
                }
                (Value::StringConst(s1), Value::StringConst(s2)) => {
                    let s1 = self.get_interned(s1);
                    if s1 == self.get_interned(s2) {
                        val = Value::True;
                    }
                }
                (Value::StringConst(s1), Value::String(h2) | Value::CharClusterLong(h2)) => {
                    let s1 = self.get_interned(s1);
                    if s1 == self.get_string(h2) {
                        val = Value::True;
                    }
                }
                (Value::String(h1) | Value::CharClusterLong(h1), Value::StringConst(s2)) => {
                    let s1 = self.get_string(h1);
                    if s1 == self.get_interned(s2) {
                        val = Value::True;
                    }
                }
                (Value::String(h1) | Value::CharClusterLong(h1), Value::CodePoint(c)) => {
                    let s1 = self.get_string(h1);
                    let s2 = format!("{c}");
                    if s1 == s2 {
                        val = Value::True;
                    }
                }
                (Value::String(h1) | Value::CharClusterLong(h1), Value::CharCluster(l, c)) => {
                    let s1 = self.get_string(h1);
                    let s2 = format!("{}", String::from_utf8_lossy(&c[0..l as usize]));
                    if s1 == s2 {
                        val = Value::True;
                    }
                }
                (
                    Value::String(h1) | Value::CharClusterLong(h1),
                    Value::CharClusterLong(h2) | Value::String(h2),
                ) => {
                    let s1 = self.get_string(h1);
                    if s1 == self.get_string(h2) {
                        val = Value::True;
                    }
                }
                (Value::Vector(h1), Value::Vector(h2)) => {
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
                (Value::Bytes(h1), Value::Bytes(h2)) => {
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
                (Value::Pair(_) | Value::List(_, _), Value::Pair(_) | Value::List(_, _)) => {
                    // XXX use iterators to reduce recursion?
                    // Make sure pair iter will work for non-lists...
                    let (car1, cdr1) = val1.get_pair(self).expect("Must be a pair or list!");
                    let (car2, cdr2) = val2.get_pair(self).expect("Must be a pair or list!");
                    val = self.is_equal_pair(car1, car2)?;
                    if val == Value::True {
                        val = self.is_equal_pair(cdr1, cdr2)?;
                    }
                }
                (Value::Map(m1), Value::Map(m2)) => {
                    let m1 = self.heap().get_map(m1);
                    let m2 = self.heap().get_map(m2);
                    if m1.len() == m2.len() {
                        if m1.is_empty() {
                            val = Value::True;
                        } else {
                            // must set val to false in two instances because
                            // its possible a previous iteration set val to true.
                            for (k, v) in m1.iter() {
                                if let Some(v2) = m2.get(self, k) {
                                    if self.is_equal_pair(v, v2)? == Value::False {
                                        val = Value::False;
                                        break;
                                    } else {
                                        val = Value::True;
                                    }
                                } else {
                                    val = Value::False;
                                    break;
                                }
                            }
                        }
                    }
                }
                (Value::Value(h1), Value::Value(h2)) => {
                    let v1 = self.get_value(h1);
                    let v2 = self.get_value(h2);
                    val = self.is_equal_pair(v1, v2)?;
                }
                (val1, Value::Value(h2)) => {
                    let v2 = self.get_value(h2);
                    val = self.is_equal_pair(val1, v2)?;
                }
                (Value::Value(v1), val2) => {
                    let v1 = self.get_value(v1);
                    val = self.is_equal_pair(v1, val2)?;
                }
                (Value::Nil | Value::Undefined, Value::True) => val = Value::False,
                (Value::Nil | Value::Undefined, Value::False) => val = Value::True,
                (Value::True, Value::Nil | Value::Undefined) => val = Value::False,
                (Value::False, Value::Nil | Value::Undefined) => val = Value::True,
                (Value::Nil | Value::Undefined, Value::Nil | Value::Undefined) => val = Value::True,
                (Value::Error(e1), Value::Error(e2)) => {
                    let err1 = self.get_error(e1);
                    let err2 = self.get_error(e2);
                    if self.get_interned(err1.keyword) == self.get_interned(err2.keyword) {
                        val = self.is_equal_pair(err1.data, err2.data)?;
                    }
                }
                (_, _) => {}
            }
        }
        Ok(val)
    }

    /// test if the operands are = (more lenient than identical)
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

    /// Return the current VM state (for re-entrant VM calls).
    fn save_state(&mut self) -> VmState {
        VmState {
            stack_top: self.stack_top,
            stack_max: self.stack_max,
            ip: self.ip_ptr,
            current_ip: self.current_ip_ptr,
            this_fn: self.this_fn,
            on_error: self.on_error,
            defers: std::mem::take(&mut self.defers),
        }
    }

    /// Restore saved VM state (for cleaning up after re-entrant VM calls).
    fn restore_state(&mut self, state: &mut VmState) {
        self.stack_top = state.stack_top;
        self.stack_max = state.stack_max;
        self.ip_ptr = state.ip;
        self.current_ip_ptr = state.current_ip;
        self.this_fn = state.this_fn;
        self.on_error = state.on_error;
        self.defers = std::mem::take(&mut state.defers);
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
        let mut vm_state = self.save_state();
        self.this_fn = None;
        self.on_error = None;
        self.stack_top = self.stack_max + 1;

        self.stack_max = self.stack_top + chunk.input_regs + chunk.extra_regs;

        // We don't have a call frame, this will cause RET/SRET to return control back when called.
        *self.stack_mut(self.stack_top) = Value::Undefined;
        if !params.is_empty() {
            let r = self.stack_top + 1..self.stack_top + 1 + params.len();
            self.stack_slice_mut()[r].copy_from_slice(params);
        }
        self.make_registers();
        if chunk.rest {
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
        self.restore_state(&mut vm_state);
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
        let mut skip_init = false;
        while !done {
            result = if let Err((e, echunk)) = self.exec_loop(chunk.clone(), skip_init) {
                skip_init = false;
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
                    let keyword = self.intern(e.key);
                    let data = match &e.obj {
                        VMErrorObj::Message(msg) => Value::StringConst(self.intern(msg)),
                        VMErrorObj::Object(v) => *v,
                    };
                    *self.register_mut(1) = self.alloc_error(crate::Error { keyword, data });
                    self.on_error = None;
                    match self.make_call(on_error, chunk.clone(), 0, 1, true) {
                        Ok(c) => {
                            chunk = c;
                            if let Value::Continuation(_) = on_error {
                                // Make sure to leave the new stack and ip_ptr alone when calling exec_loop().
                                skip_init = true;
                            }
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
            Ok(from_i56(i))
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
        chunk.add_constant(1.into());
        chunk.add_constant(2.into());
        chunk.add_constant(3.into());
        chunk.add_constant(4.into());
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
            chunk.add_constant(i.into());
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
        *vm.stack_mut(0) = vm.new_upval(1.into());
        *vm.stack_mut(1) = 10.into();
        *vm.stack_mut(2) = 1.into();
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
        let const2 = chunk.add_constant(42.into()) as u16;
        vm.globals.set(slot, 11.into());
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
        vm.globals.set(slot, 11.into());
        let const2 = chunk.add_constant(43.into()) as u16;
        let const3 = chunk.add_constant(53.into()) as u16;
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
        vm.globals.set(slot2, 11.into());
        assert!(vm.globals.get(slot2).get_int(&vm)? == 11);
        let const2 = chunk.add_constant(43.into()) as u16;
        let const3 = chunk.add_constant(53.into()) as u16;
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
        let const2 = chunk.add_constant(44.into()) as u16;
        let const3 = chunk.add_constant(53.into()) as u16;
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
        let const2 = chunk.add_constant(45.into()) as u16;
        let const3 = chunk.add_constant(55.into()) as u16;
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
        let const2 = chunk.add_constant(45.into()) as u16;
        let const3 = chunk.add_constant(1.into()) as u16;
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
        let const1 = chunk.add_constant(10.into()) as u16;
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
        *vm.stack_mut(3) = 5.into();
        *vm.stack_mut(4) = 2.into();
        *vm.stack_mut(6) = 2.into();
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
        let const1 = chunk.add_constant(10.into()) as u16;
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
        *vm.stack_mut(1) = 5.into();
        *vm.stack_mut(2) = 2.into();
        *vm.stack_mut(4) = 2.into();
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
            Ok((registers[0].get_int(vm)? + registers[1].get_int(vm)?).into())
        }
        fn add_10(vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
            if registers.len() != 1 {
                return Err(VMError::new_vm("test add_10: wrong number of args."));
            }
            Ok((registers[0].get_int(vm)? + 10).into())
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
        *vm.stack_mut(3) = 6.into();
        *vm.stack_mut(4) = 3.into();
        *vm.stack_mut(8) = 12.into();
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
        *vm.stack_mut(3) = 6.into();
        *vm.stack_mut(4) = 3.into();
        *vm.stack_mut(6) = 12.into();
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
        let const0 = chunk.add_constant(2.into()) as u16;
        let const1 = chunk.add_constant(3.into()) as u16;
        *vm.stack_mut(0) = Value::True;
        *vm.stack_mut(1) = Value::False;
        *vm.stack_mut(2) = Value::Nil;
        *vm.stack_mut(3) = 0.into();
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
        let zero = chunk.add_constant(0.into()) as u16;
        let hundred = chunk.add_constant(100.into()) as u16;
        let one = chunk.add_constant(1.into()) as u16;
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
        chunk.encode3(GET, 7, 1, 2, Some(line))?;
        chunk.encode3(SETCOL, 3, 1, 2, Some(line))?;
        chunk.encode3(GET, 8, 1, 2, Some(line))?;
        chunk.encode2(VECMK, 10, 2, Some(line))?;
        chunk.encode2(VECPSH, 10, 4, Some(line))?;
        chunk.encode2(VECPSH, 10, 3, Some(line))?;
        chunk.encode2(VECPOP, 10, 15, Some(line))?;
        chunk.encode2(VECPOP, 10, 16, Some(line))?;
        chunk.encode2(VECPSH, 10, 4, Some(line))?;
        chunk.encode2(VECPSH, 10, 4, Some(line))?;
        chunk.encode3(GET, 17, 10, 3, Some(line))?;
        chunk.encode3(SETCOL, 3, 10, 3, Some(line))?;
        chunk.encode3(GET, 18, 10, 3, Some(line))?;
        chunk.encode2(VECMK, 20, 2, Some(line))?;
        chunk.encode2(VECELS, 20, 2, Some(line))?;
        chunk.encode3(SETCOL, 3, 20, 3, Some(line))?;
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
        let const0 = chunk.add_constant(2.into()) as u16;
        let const1 = chunk.add_constant(3.into()) as u16;
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
        let const0 = chunk.add_constant(2_f32.into()) as u16;
        let const1 = chunk.add_constant(3.into()) as u16;
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
            chunk.add_constant(i.into());
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
        let const0 = chunk.add_constant(2.into()) as u16;
        let const1 = chunk.add_constant(3.into()) as u16;
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
        let const0 = chunk.add_constant(5_f32.into()) as u16;
        let const1 = chunk.add_constant(3.into()) as u16;
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
            chunk.add_constant(i.into());
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
        let const0 = chunk.add_constant(2.into()) as u16;
        let const1 = chunk.add_constant(3.into()) as u16;
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
        let const0 = chunk.add_constant(5_f32.into()) as u16;
        let const1 = chunk.add_constant(3.into()) as u16;
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
            chunk.add_constant(i.into());
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
        let const0 = chunk.add_constant(18.into()) as u16;
        let const1 = chunk.add_constant(2.into()) as u16;
        let const2 = chunk.add_constant(Value::Byte(3)) as u16;
        chunk.encode2(CONST, 0, const0, Some(line))?;
        chunk.encode2(CONST, 1, const1, Some(line))?;
        chunk.encode2(CONST, 2, const2, Some(line))?;
        chunk.encode2(DIV, 0, 1, Some(line)).unwrap();
        chunk.encode2(DIV, 0, 2, Some(line)).unwrap();
        chunk.encode0(RET, Some(line))?;
        let mut vm = Vm::new();
        let val10 = 10_f32.into();
        let val0 = 0_f32.into();
        let chunk = Arc::new(chunk);
        vm.execute(chunk)?;
        assert!(vm.stack(0).get_int(&vm)? == 3);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(val10) as u16;
        let const1 = chunk.add_constant(2.into()) as u16;
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
            chunk.add_constant(i.into());
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
        let const0 = chunk.add_constant(10.into()) as u16;
        let const1 = chunk.add_constant(0.into()) as u16;
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

    #[test]
    fn test_equality() {
        let vm = Vm::new();

        let byte = Value::Byte(2);
        let another_byte = Value::Byte(2);
        let int = Value::Int([0, 0, 0, 0, 0, 0, 2]);
        let another_int = Value::Int([0, 0, 0, 0, 0, 0, 2]);
        let float = Value::Float(2.0.into());
        let another_float = Value::Float(2.0000000000000000000000000000000000000000001.into());

        // testing `=`
        assert!(vm.is_equal_pair(byte, int).unwrap().is_true());
        assert!(vm.is_equal_pair(byte, float).unwrap().is_false());
        assert!(vm.is_equal_pair(int, float).unwrap().is_false());

        // testing `identical?`
        assert!(byte == another_byte);
        assert!(int == another_int);
        assert!(float == another_float);
        assert!(byte != int);
        assert!(byte != float);
        assert!(int != float);
    }
}
