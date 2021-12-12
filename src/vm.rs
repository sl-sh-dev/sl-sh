use std::rc::Rc;

use crate::chunk::*;
use crate::error::*;
use crate::heap::*;
use crate::interner::*;
use crate::opcodes::*;
use crate::value::*;

macro_rules! decode_u16 {
    ($code:expr, $ip:expr) => {{
        let idx1 = $code[*$ip];
        let idx2 = $code[*$ip + 1];
        *$ip += 2;
        ((idx1 as u16) << 8) | (idx2 as u16)
    }};
}

macro_rules! decode_u32 {
    ($code:expr, $ip:expr) => {{
        let idx1 = $code[*$ip];
        let idx2 = $code[*$ip + 1];
        let idx3 = $code[*$ip + 2];
        let idx4 = $code[*$ip + 3];
        *$ip += 4;
        ((idx1 as u32) << 24) | ((idx2 as u32) << 16) | ((idx3 as u32) << 8) | (idx4 as u32)
    }};
}

macro_rules! decode1 {
    ($code:expr, $ip:expr, $wide:expr) => {{
        if $wide {
            decode_u16!($code, $ip)
        } else {
            let oip = *$ip;
            *$ip += 1;
            $code[oip] as u16
        }
    }};
}

macro_rules! decode2 {
    ($code:expr, $ip:expr, $wide:expr) => {{
        if $wide {
            (decode_u16!($code, $ip), decode_u16!($code, $ip))
        } else {
            let oip = *$ip;
            *$ip += 2;
            ($code[oip] as u16, $code[oip + 1] as u16)
        }
    }};
}

macro_rules! decode3 {
    ($code:expr, $ip:expr, $wide:expr) => {{
        if $wide {
            (
                decode_u16!($code, $ip),
                decode_u16!($code, $ip),
                decode_u16!($code, $ip),
            )
        } else {
            let oip = *$ip;
            *$ip += 3;
            (
                $code[oip] as u16,
                $code[oip + 1] as u16,
                $code[oip + 2] as u16,
            )
        }
    }};
}

macro_rules! get_reg_unref {
    ($regs:expr, $idx:expr, $vm:expr) => {{
        $regs[$idx as usize].unref($vm)
    }};
}

macro_rules! get_reg {
    ($regs:expr, $idx:expr) => {{
        $regs[$idx as usize]
    }};
}

macro_rules! compare_int {
    ($vm:expr, $chunk:expr, $ip:expr, $registers:expr, $comp_fn:expr,
     $compf_fn:expr, $wide:expr, $move:expr, $not:expr) => {{
        let (dest, reg1, reg2) = decode3!($chunk.code, $ip, $wide);
        let mut val = false;
        for reg in reg1..reg2 {
            let op1 = get_reg_unref!($registers, reg, $vm);
            let op2 = get_reg_unref!($registers, reg + 1, $vm);
            val = if op1.is_int() && op2.is_int() {
                $comp_fn(
                    op1.get_int().map_err(|e| (e, $chunk.clone()))?,
                    op2.get_int().map_err(|e| (e, $chunk.clone()))?,
                )
            } else {
                $compf_fn(
                    op1.get_float().map_err(|e| (e, $chunk.clone()))?,
                    op2.get_float().map_err(|e| (e, $chunk.clone()))?,
                )
            };
            if !val {
                break;
            }
        }
        if $not {
            val = !val;
        }
        let val = if val { Value::True } else { Value::False };
        if $move {
            Vm::mov_register($registers, dest as usize, val);
        } else {
            $vm.set_register($registers, dest as usize, val);
        }
    }};
}

macro_rules! compare {
    ($vm:expr, $chunk:expr, $ip:expr, $registers:expr, $comp_fn:expr, $wide:expr, $move:expr) => {{
        compare_int!($vm, $chunk, $ip, $registers, $comp_fn, $comp_fn, $wide, $move, false)
    }};
}

macro_rules! binary_math {
    ($vm:expr, $chunk:expr, $ip:expr, $registers:expr, $bin_fn:expr, $wide:expr, $move:expr) => {{
        let (dest, op2, op3) = decode3!($chunk.code, $ip, $wide);
        let op2 = get_reg_unref!($registers, op2, $vm);
        let op3 = get_reg_unref!($registers, op3, $vm);
        let val = if op2.is_int() && op3.is_int() {
            Value::Int($bin_fn(
                op2.get_int().map_err(|e| (e, $chunk.clone()))?,
                op3.get_int().map_err(|e| (e, $chunk.clone()))?,
            ))
        } else {
            Value::Float(F64Wrap($bin_fn(
                op2.get_float().map_err(|e| (e, $chunk.clone()))?,
                op3.get_float().map_err(|e| (e, $chunk.clone()))?,
            )))
        };
        if $move {
            Vm::mov_register($registers, dest as usize, val);
        } else {
            $vm.set_register($registers, dest as usize, val);
        }
    }};
}

macro_rules! div_math {
    ($vm:expr, $chunk:expr, $ip:expr, $registers:expr, $wide:expr, $move:expr) => {{
        let (dest, op2, op3) = decode3!($chunk.code, $ip, $wide);
        let op2 = get_reg_unref!($registers, op2, $vm);
        let op3 = get_reg_unref!($registers, op3, $vm);
        let val = if op2.is_int() && op3.is_int() {
            let op3 = op3.get_int().map_err(|e| (e, $chunk.clone()))?;
            if op3 == 0 {
                return Err((VMError::new_vm("Divide by zero error."), $chunk));
            }
            Value::Int(op2.get_int().map_err(|e| (e, $chunk.clone()))? / op3)
        } else {
            let op3 = op3.get_float().map_err(|e| (e, $chunk.clone()))?;
            if op3 == 0.0 {
                return Err((VMError::new_vm("Divide by zero error."), $chunk));
            }
            Value::Float(F64Wrap(
                op2.get_float().map_err(|e| (e, $chunk.clone()))? / op3,
            ))
        };
        if $move {
            Vm::mov_register($registers, dest as usize, val);
        } else {
            $vm.set_register($registers, dest as usize, val);
        }
    }};
}

/*macro_rules! set_register {
    ($registers:expr, $idx:expr, $val:expr) => {{
        $registers[$idx as usize] = $val;
        /*unsafe {
            let r = $registers.get_unchecked_mut($idx as usize);
            *r = $val;
        }*/
    }};
}*/

pub struct CallFrame {
    pub chunk: Rc<Chunk>,
    pub ip: usize,
    pub current_ip: usize,
    pub stack_top: usize,
    pub this_fn: Option<Value>,
}

pub struct Vm {
    interner: Interner,
    heap: Heap,
    stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    globals: Globals,
    upvals: Vec<Value>, // XXX these need to be GCed...
    this_fn: Option<Value>,

    err_frame: Option<CallFrame>,
    stack_top: usize,
    ip: usize,
    current_ip: usize,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn new() -> Self {
        let globals = Globals::new();
        let mut stack = Vec::with_capacity(1024);
        stack.resize(1024, Value::Undefined);
        Vm {
            interner: Interner::with_capacity(8192),
            heap: Heap::new(),
            stack,
            call_stack: Vec::new(),
            globals,
            upvals: Vec::new(),
            this_fn: None,
            err_frame: None,
            stack_top: 0,
            ip: 0,
            current_ip: 0,
        }
    }

    pub fn clear_err_frame(&mut self) {
        self.err_frame = None;
    }

    pub fn err_frame(&self) -> &Option<CallFrame> {
        &self.err_frame
    }

    pub fn get_call_stack(&self) -> &[CallFrame] {
        &self.call_stack[..]
    }

    pub fn get_registers(&self, start: usize, end: usize) -> &[Value] {
        &self.stack[start..end]
    }

    pub fn alloc(&mut self, obj: Object) -> Handle {
        self.heap.alloc(obj, |_heap| Ok(()))
    }

    pub fn get(&self, handle: Handle) -> HandleRef<'_> {
        self.heap.get(handle)
    }

    pub fn get_mut(&mut self, handle: Handle) -> HandleRefMut<'_> {
        self.heap.get_mut(handle)
    }

    pub fn get_global(&self, idx: u32) -> Value {
        self.globals.get(idx)
    }

    pub fn get_upval(&self, idx: usize) -> Value {
        self.upvals[idx]
    }

    pub fn new_upval(&mut self, val: Value) -> usize {
        let r = self.upvals.len();
        self.upvals.push(val);
        r
    }

    pub fn get_stack(&self, idx: usize) -> Value {
        self.stack[idx]
    }

    pub fn get_interned(&self, i: Interned) -> &'static str {
        self.interner.get_string(i).expect("Invalid interned value")
    }

    pub fn intern(&mut self, string: &str) -> Interned {
        self.interner.intern(string)
    }

    pub fn reserve_symbol(&mut self, string: &str) -> Value {
        let sym = self.interner.intern(string);
        Value::Global(self.globals.reserve(sym))
    }

    pub fn reserve_interned(&mut self, i: Interned) -> Value {
        Value::Global(self.globals.reserve(i))
    }

    pub fn reserve_index(&mut self, i: Interned) -> u32 {
        self.globals.reserve(i)
    }

    pub fn set_global(&mut self, string: &str, value: Value) -> Value {
        let sym = self.interner.intern(string);
        let slot = self.globals.reserve(sym);
        self.globals.set(slot, value);
        Value::Global(slot)
    }

    pub fn intern_to_global(&self, i: Interned) -> Option<Value> {
        self.globals.get_if_interned(i)
    }

    pub fn global_intern_slot(&self, i: Interned) -> Option<usize> {
        self.globals.interned_slot(i)
    }

    pub fn dump_globals(&self) {
        println!("GLOBALS:");
        self.globals.dump(self);
        println!();
    }

    #[inline]
    fn set_register(&mut self, registers: &mut [Value], idx: usize, val: Value) {
        match &get_reg!(registers, idx) {
            Value::Binding(idx) => {
                self.upvals[*idx] = val;
            }
            Value::Global(idx) => self.globals.set(*idx, val),
            _ => registers[idx] = val,
        }
    }

    #[inline]
    fn mov_register(registers: &mut [Value], idx: usize, val: Value) {
        registers[idx] = val;
    }

    fn list(&mut self, code: &[u8], registers: &mut [Value], wide: bool) -> VMResult<()> {
        let (dest, start, end) = decode3!(code, &mut self.ip, wide);
        if end == start {
            self.set_register(registers, dest as usize, Value::Nil);
        } else {
            let mut last_cdr = Value::Nil;
            for i in (start..end).rev() {
                let car = get_reg_unref!(registers, i, self);
                let cdr = last_cdr;
                last_cdr = Value::Reference(self.alloc(Object::Pair(car, cdr, None)));
            }
            self.set_register(registers, dest as usize, last_cdr);
        }
        Ok(())
    }

    fn append(
        &mut self,
        code: &[u8],
        registers: &mut [Value],
        wide: bool,
        allow_singles: bool,
    ) -> VMResult<()> {
        let (dest, start, end) = decode3!(code, &mut self.ip, wide);
        if end == start {
            self.set_register(registers, dest as usize, Value::Nil);
        } else {
            let mut last_cdr = Value::Nil;
            let mut head = Value::Nil;
            let mut holder = Vec::new();
            for i in start..end {
                let lst = get_reg_unref!(registers, i, self);
                match lst {
                    Value::Nil => {}
                    Value::Reference(h) => {
                        match self.get(h) {
                            Object::Pair(_car, _cdr, _) => {
                                holder.clear();
                                // Make the borrow checker happy, at least reuse
                                // the vector to cut down on allocations...
                                for l in lst.iter(self) {
                                    holder.push(l);
                                }
                                for l in &holder {
                                    let cdr = last_cdr;
                                    last_cdr = Value::Reference(self.alloc(Object::Pair(
                                        *l,
                                        Value::Nil,
                                        None,
                                    )));
                                    match cdr {
                                        Value::Nil => head = last_cdr,
                                        Value::Reference(h) => {
                                            let cons_d = self.heap.get(h);
                                            if let Object::Pair(car, _cdr, meta) = &*cons_d {
                                                let car = *car;
                                                let meta = *meta;
                                                self.heap
                                                    .replace(h, Object::Pair(car, last_cdr, meta));
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {
                                if allow_singles {
                                    let cdr = last_cdr;
                                    last_cdr = Value::Reference(self.alloc(Object::Pair(
                                        lst,
                                        Value::Nil,
                                        None,
                                    )));
                                    match cdr {
                                        Value::Nil => head = last_cdr,
                                        Value::Reference(h) => {
                                            let cons_d = self.heap.get(h);
                                            if let Object::Pair(car, _cdr, meta) = &*cons_d {
                                                let car = *car;
                                                let meta = *meta;
                                                self.heap
                                                    .replace(h, Object::Pair(car, last_cdr, meta));
                                            }
                                        }
                                        _ => {}
                                    }
                                } else {
                                    return Err(VMError::new_vm("APND: Param not a list."));
                                }
                            }
                        }
                    }
                    _ => {
                        if allow_singles {
                            let cdr = last_cdr;
                            last_cdr =
                                Value::Reference(self.alloc(Object::Pair(lst, Value::Nil, None)));
                            match cdr {
                                Value::Nil => head = last_cdr,
                                Value::Reference(h) => {
                                    let cons_d = self.heap.get(h);
                                    if let Object::Pair(car, _cdr, meta) = &*cons_d {
                                        let car = *car;
                                        let meta = *meta;
                                        self.heap.replace(h, Object::Pair(car, last_cdr, meta));
                                    }
                                }
                                _ => {}
                            }
                        } else {
                            return Err(VMError::new_vm("APND: Param not a list."));
                        }
                    }
                }
            }
            self.set_register(registers, dest as usize, head);
        }
        Ok(())
    }

    fn xar(&mut self, code: &[u8], registers: &mut [Value], wide: bool) -> VMResult<()> {
        let (pair_reg, val) = decode2!(code, &mut self.ip, wide);
        let pair = get_reg_unref!(registers, pair_reg, self);
        let val = get_reg_unref!(registers, val, self);
        match &pair {
            Value::Reference(cons_handle) => {
                let cons_d = self.heap.get(*cons_handle);
                if let Object::Pair(_car, cdr, _) = &*cons_d {
                    let cdr = *cdr;
                    self.heap
                        .replace(*cons_handle, Object::Pair(val, cdr, None));
                } else {
                    return Err(VMError::new_vm("XAR: Not a pair/conscell."));
                }
            }
            Value::Nil => {
                let pair = Value::Reference(self.alloc(Object::Pair(val, Value::Nil, None)));
                self.set_register(registers, pair_reg as usize, pair);
            }
            _ => {
                return Err(VMError::new_vm("XAR: Not a pair/conscell."));
            }
        }
        Ok(())
    }

    fn xdr(&mut self, code: &[u8], registers: &mut [Value], wide: bool) -> VMResult<()> {
        let (pair_reg, val) = decode2!(code, &mut self.ip, wide);
        let pair = get_reg_unref!(registers, pair_reg, self);
        let val = get_reg_unref!(registers, val, self);
        match &pair {
            Value::Reference(cons_handle) => {
                let cons_d = self.heap.get(*cons_handle);
                if let Object::Pair(car, _cdr, _) = &*cons_d {
                    let car = *car;
                    self.heap
                        .replace(*cons_handle, Object::Pair(car, val, None));
                } else {
                    return Err(VMError::new_vm("XDR: Not a pair/conscell."));
                }
            }
            Value::Nil => {
                let pair = Value::Reference(self.alloc(Object::Pair(Value::Nil, val, None)));
                self.set_register(registers, pair_reg as usize, pair);
            }
            _ => {
                return Err(VMError::new_vm("XDR: Not a pair/conscell."));
            }
        }
        Ok(())
    }

    // Need to break the registers lifetime away from self or we can not do much...
    // The underlying stack should never be deleted or reallocated for the life
    // of Vm so this should be safe.
    fn make_registers(&mut self, start: usize) -> &'static mut [Value] {
        unsafe { &mut *(&mut self.stack[start..] as *mut [Value]) }
    }

    fn setup_rest(
        chunk: &Rc<Chunk>,
        registers: &mut [Value],
        first_reg: u16,
        num_args: u16,
    ) -> (usize, Vec<Value>) {
        let rest_reg = first_reg + chunk.args + chunk.opt_args;
        let v = if num_args < (chunk.args + chunk.opt_args) {
            Vec::with_capacity(0)
        } else {
            let rest_len = (num_args - (chunk.args + chunk.opt_args)) as usize + 1;
            let mut v = Vec::with_capacity(rest_len);
            for item in registers
                .iter()
                .take(rest_reg as usize + rest_len)
                .skip(rest_reg.into())
            {
                v.push(*item);
            }
            v
        };
        (rest_reg.into(), v)
    }

    fn make_call(
        &mut self,
        lambda: Value,
        chunk: Rc<Chunk>,
        registers: &mut [Value],
        first_reg: u16,
        num_args: u16,
        tail_call: bool,
    ) -> Result<Rc<Chunk>, (VMError, Rc<Chunk>)> {
        // Clear out the extra regs to avoid writing to globals or closures by
        // accident.
        fn clear_regs(l: &Chunk, registers: &mut [Value], first_reg: u16, num_args: u16) {
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
            if l.extra_regs > 0 {
                for r in l.input_regs..l.input_regs + l.extra_regs {
                    Vm::mov_register(registers, first_reg as usize + r, Value::Undefined);
                }
            }
        }

        match lambda {
            Value::Builtin(f) => {
                let last_reg = (first_reg + num_args + 1) as usize;
                let res = (f.func)(self, &registers[(first_reg + 1) as usize..last_reg])
                    .map_err(|e| (e, chunk.clone()))?;
                Self::mov_register(registers, first_reg as usize, res);
                if tail_call {
                    // Go to last call frame so the SRET does not mess up the return of a builtin.
                    if let Some(frame) = self.call_stack.pop() {
                        self.stack_top = frame.stack_top;
                        self.ip = frame.ip;
                        self.this_fn = frame.this_fn;
                        Ok(frame.chunk)
                    } else {
                        Ok(chunk)
                    }
                } else {
                    Ok(chunk)
                }
            }
            Value::Reference(h) => {
                let rf = self.heap.get(h);
                match rf {
                    Object::Lambda(l) => {
                        let l = l.clone();
                        if !tail_call {
                            let frame = CallFrame {
                                chunk,
                                ip: self.ip,
                                current_ip: self.current_ip,
                                stack_top: self.stack_top,
                                this_fn: self.this_fn,
                            };
                            self.call_stack.push(frame);
                            self.stack_top += first_reg as usize;
                        }
                        self.this_fn = Some(lambda);
                        self.ip = 0;
                        Self::mov_register(
                            registers,
                            first_reg.into(),
                            Value::UInt(num_args as u64),
                        );
                        if l.rest {
                            let (rest_reg, v) =
                                Self::setup_rest(&l, registers, first_reg, num_args);
                            let h = self.alloc(Object::Vector(v));
                            Self::mov_register(registers, rest_reg, Value::Reference(h));
                        }
                        // XXX TODO- double check num args.
                        // XXX TODO- maybe test for stack overflow vs waiting for a panic.
                        clear_regs(&l, registers, first_reg, num_args);
                        Ok(l)
                    }
                    Object::Closure(l, caps) => {
                        let l = l.clone();
                        if !tail_call {
                            let frame = CallFrame {
                                chunk,
                                ip: self.ip,
                                current_ip: self.current_ip,
                                stack_top: self.stack_top,
                                this_fn: self.this_fn,
                            };
                            self.call_stack.push(frame);
                            self.stack_top += first_reg as usize;
                        }
                        self.this_fn = Some(lambda);
                        self.ip = 0;
                        let cap_first = (first_reg + l.args + l.opt_args + 1) as usize;
                        if l.rest {
                            let (rest_reg, v) =
                                Self::setup_rest(&l, registers, first_reg, num_args);
                            for (i, c) in caps.iter().enumerate() {
                                Self::mov_register(registers, cap_first + i, Value::Binding(*c));
                            }
                            let h = self.alloc(Object::Vector(v));
                            Self::mov_register(registers, rest_reg, Value::Reference(h));
                        } else {
                            for (i, c) in caps.iter().enumerate() {
                                Self::mov_register(registers, cap_first + i, Value::Binding(*c));
                            }
                        }
                        Self::mov_register(
                            registers,
                            first_reg.into(),
                            Value::UInt(num_args as u64),
                        );
                        clear_regs(&l, registers, first_reg, num_args);
                        Ok(l)
                    }
                    _ => Err((VMError::new_vm("CALL: Not a callable."), chunk)),
                }
            }
            _ => Err((VMError::new_vm("CALL: Not a callable."), chunk)),
        }
    }

    fn is_eq(&mut self, registers: &mut [Value], reg1: u16, reg2: u16) -> VMResult<Value> {
        let mut val = Value::False;
        if reg1 == reg2 {
            val = Value::True;
        } else {
            //let mut val1 = get_reg!(registers, reg1);
            let mut val1 = get_reg_unref!(registers, reg1, self);
            for reg in reg1..reg2 {
                //let val2 = get_reg!(registers, reg + 1);
                let val2 = get_reg_unref!(registers, reg + 1, self);
                if unsafe {
                    std::mem::transmute::<Value, u128>(val1)
                        == std::mem::transmute::<Value, u128>(val2)
                } {
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
        if unsafe {
            std::mem::transmute::<Value, u128>(val1) == std::mem::transmute::<Value, u128>(val2)
        } {
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
                    if let Value::Reference(v2) = val2 {
                        if let Object::String(s2) = self.get(v2) {
                            if self.get_interned(s1) == s2 {
                                val = Value::True;
                            }
                        }
                    }
                }
                Value::Reference(h1) => {
                    if let Value::StringConst(s2) = val2 {
                        if let Object::String(s1) = self.get(h1) {
                            if s1 == self.get_interned(s2) {
                                val = Value::True;
                            }
                        }
                    } else if let Value::Reference(h2) = val2 {
                        match self.get(h1) {
                            Object::String(s1) => {
                                if let Object::String(s2) = self.get(h2) {
                                    if s1 == s2 {
                                        val = Value::True;
                                    }
                                }
                            }
                            Object::Vector(v1) => {
                                if let Object::Vector(v2) = self.get(h2) {
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
                            Object::Bytes(b1) => {
                                if let Object::Bytes(b2) = self.get(h2) {
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
                            Object::Pair(car1, cdr1, _) => {
                                // XXX use iterators to reduce recursion?
                                // Make sure pair iter will work for non-lists...
                                if let Object::Pair(car2, cdr2, _) = self.get(h2) {
                                    val = self.is_equal_pair(*car1, *car2)?;
                                    if val == Value::True {
                                        val = self.is_equal_pair(*cdr1, *cdr2)?;
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(val)
    }

    fn is_equal(&mut self, registers: &mut [Value], reg1: u16, reg2: u16) -> VMResult<Value> {
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

    pub fn do_call(&mut self, chunk: Rc<Chunk>, params: &[Value]) -> VMResult<Value> {
        self.stack[0] = Value::UInt(params.len() as u64);
        if !params.is_empty() {
            self.stack[1..=params.len()].copy_from_slice(params);
        }
        self.execute(chunk)?;
        Ok(self.stack[0])
    }

    pub fn execute(&mut self, chunk: Rc<Chunk>) -> VMResult<()> {
        let stack_top = self.stack_top;
        let ip = self.ip;
        let this_fn = self.this_fn;
        self.this_fn = None;
        let result = self.execute_internal(chunk);
        let result = if let Err((e, chunk)) = result {
            self.err_frame = Some(CallFrame {
                chunk,
                stack_top: self.stack_top,
                ip: self.ip,
                current_ip: self.current_ip,
                this_fn: self.this_fn,
            });
            Err(e)
        } else {
            self.err_frame = None;
            Ok(())
        };
        self.stack_top = stack_top;
        self.ip = ip;
        self.this_fn = this_fn;
        result
    }

    fn execute_internal(&mut self, chunk: Rc<Chunk>) -> Result<(), (VMError, Rc<Chunk>)> {
        self.stack_top = 0;
        let mut registers = self.make_registers(self.stack_top);
        let mut chunk = chunk;
        self.ip = 0;
        let mut wide = false;
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
                    if let Some(frame) = self.call_stack.pop() {
                        self.stack_top = frame.stack_top;
                        registers = self.make_registers(self.stack_top);
                        chunk = frame.chunk.clone();
                        self.ip = frame.ip;
                        self.current_ip = frame.current_ip;
                        self.this_fn = frame.this_fn;
                    } else {
                        return Ok(());
                    }
                }
                SRET => {
                    let src = decode1!(chunk.code, &mut self.ip, wide);
                    let val = get_reg_unref!(registers, src, self);
                    self.set_register(registers, 0, val);
                    if let Some(frame) = self.call_stack.pop() {
                        self.stack_top = frame.stack_top;
                        registers = self.make_registers(self.stack_top);
                        chunk = frame.chunk.clone();
                        self.ip = frame.ip;
                        self.current_ip = frame.current_ip;
                        self.this_fn = frame.this_fn;
                    } else {
                        return Ok(());
                    }
                }
                WIDE => wide = true,
                MOV => {
                    let (dest, src) = decode2!(chunk.code, &mut self.ip, wide);
                    // XXX TODO- figure out proper mov symantics...
                    //let val = get_reg_unref!(registers, src, self);
                    let val = get_reg!(registers, src);
                    Self::mov_register(registers, dest as usize, val);
                }
                SET => {
                    let (dest, src) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = get_reg_unref!(registers, src, self);
                    self.set_register(registers, dest as usize, val);
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
                SREGT => {
                    let dest = decode1!(chunk.code, &mut self.ip, wide);
                    self.set_register(registers, dest as usize, Value::True);
                }
                SREGF => {
                    let dest = decode1!(chunk.code, &mut self.ip, wide);
                    self.set_register(registers, dest as usize, Value::False);
                }
                SREGN => {
                    let dest = decode1!(chunk.code, &mut self.ip, wide);
                    self.set_register(registers, dest as usize, Value::Nil);
                }
                SREGC => {
                    let dest = decode1!(chunk.code, &mut self.ip, wide);
                    self.set_register(registers, dest as usize, Value::Undefined);
                }
                SREGB => {
                    let (dest, i) = decode2!(chunk.code, &mut self.ip, wide);
                    self.set_register(registers, dest as usize, Value::Byte(i as u8));
                }
                SREGI => {
                    let (dest, i) = decode2!(chunk.code, &mut self.ip, wide);
                    self.set_register(registers, dest as usize, Value::Int(i as i64));
                }
                SREGU => {
                    let (dest, i) = decode2!(chunk.code, &mut self.ip, wide);
                    self.set_register(registers, dest as usize, Value::UInt(i as u64));
                }
                MREGT => {
                    let dest = decode1!(chunk.code, &mut self.ip, wide);
                    Self::mov_register(registers, dest as usize, Value::True);
                }
                MREGF => {
                    let dest = decode1!(chunk.code, &mut self.ip, wide);
                    Self::mov_register(registers, dest as usize, Value::False);
                }
                MREGN => {
                    let dest = decode1!(chunk.code, &mut self.ip, wide);
                    Self::mov_register(registers, dest as usize, Value::Nil);
                }
                MREGC => {
                    let dest = decode1!(chunk.code, &mut self.ip, wide);
                    Self::mov_register(registers, dest as usize, Value::Undefined);
                }
                MREGB => {
                    let (dest, i) = decode2!(chunk.code, &mut self.ip, wide);
                    Self::mov_register(registers, dest as usize, Value::Byte(i as u8));
                }
                MREGI => {
                    let (dest, i) = decode2!(chunk.code, &mut self.ip, wide);
                    Self::mov_register(registers, dest as usize, Value::Int(i as i64));
                }
                MREGU => {
                    let (dest, i) = decode2!(chunk.code, &mut self.ip, wide);
                    Self::mov_register(registers, dest as usize, Value::UInt(i as u64));
                }
                CLOSE => {
                    let (dest, src) = decode2!(chunk.code, &mut self.ip, wide);
                    let lambda = get_reg_unref!(registers, src, self);
                    let (lambda, caps) = if let Value::Reference(h) = lambda {
                        let lr = self.get(h);
                        if let Object::Lambda(l) = lr {
                            let l = l.clone();
                            let mut caps = Vec::new();
                            if let Some(captures) = &l.captures {
                                for c in captures {
                                    let r = get_reg!(registers, *c);
                                    if let Value::Binding(b) = r {
                                        caps.push(b);
                                    } else {
                                        let slot = self.new_upval(r);
                                        Self::mov_register(
                                            registers,
                                            *c as usize,
                                            Value::Binding(slot),
                                        );
                                        caps.push(slot);
                                    }
                                }
                            }
                            (l.clone(), caps)
                        } else {
                            return Err((
                                VMError::new_vm(format!(
                                    "CLOSE: requires a lambda, got {:?}.",
                                    lambda
                                )),
                                chunk,
                            ));
                        }
                    } else {
                        return Err((
                            VMError::new_vm(format!("CLOSE: requires a lambda, got {:?}.", lambda)),
                            chunk,
                        ));
                    };
                    Self::mov_register(
                        registers,
                        dest as usize,
                        Value::Reference(self.alloc(Object::Closure(lambda, caps))),
                    );
                }
                CALL => {
                    let (lambda, num_args, first_reg) = decode3!(chunk.code, &mut self.ip, wide);
                    let lambda = get_reg_unref!(registers, lambda, self);
                    chunk = self.make_call(lambda, chunk, registers, first_reg, num_args, false)?;
                    registers = self.make_registers(self.stack_top);
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
                    registers = self.make_registers(self.stack_top);
                }
                TCALL => {
                    let (lambda, num_args) = decode2!(chunk.code, &mut self.ip, wide);
                    let lambda = get_reg_unref!(registers, lambda, self);
                    chunk = self.make_call(lambda, chunk, registers, 0, num_args, true)?;
                    registers = self.make_registers(self.stack_top); // In case of a builtin call
                }
                TCALLG => {
                    let idx = if wide {
                        decode_u32!(chunk.code, &mut self.ip)
                    } else {
                        decode_u16!(chunk.code, &mut self.ip) as u32
                    };
                    let num_args = decode1!(chunk.code, &mut self.ip, wide);
                    let lambda = self.get_global(idx);
                    chunk = self.make_call(lambda, chunk, registers, 0, num_args, true)?;
                    registers = self.make_registers(self.stack_top); // In case of a builtin call
                }
                CALLM => {
                    let (num_args, first_reg) = decode2!(chunk.code, &mut self.ip, wide);
                    if let Some(this_fn) = self.this_fn {
                        chunk =
                            self.make_call(this_fn, chunk, registers, first_reg, num_args, false)?;
                        registers = self.make_registers(self.stack_top);
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
                    let num_args = decode1!(chunk.code, &mut self.ip, wide);
                    if let Some(this_fn) = self.this_fn {
                        chunk = self.make_call(this_fn, chunk, registers, 0, num_args, true)?;
                        registers = self.make_registers(self.stack_top); // In case of a builtin call
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
                    let pair = Value::Reference(self.alloc(Object::Pair(car, cdr, None)));
                    self.set_register(registers, dest as usize, pair);
                }
                CAR => {
                    let (dest, op) = decode2!(chunk.code, &mut self.ip, wide);
                    let op = get_reg_unref!(registers, op, self);
                    match op {
                        Value::Reference(handle) => {
                            let handle_d = self.heap.get(handle);
                            if let Object::Pair(car, _, _) = &*handle_d {
                                let car = *car;
                                self.set_register(registers, dest as usize, car);
                            } else {
                                return Err((VMError::new_vm("CAR: Not a pair/conscell."), chunk));
                            }
                        }
                        Value::Nil => self.set_register(registers, dest as usize, Value::Nil),
                        _ => return Err((VMError::new_vm("CAR: Not a pair/conscell."), chunk)),
                    }
                }
                CDR => {
                    let (dest, op) = decode2!(chunk.code, &mut self.ip, wide);
                    let op = get_reg_unref!(registers, op, self);
                    match op {
                        Value::Reference(handle) => {
                            let handle_d = self.heap.get(handle);
                            if let Object::Pair(_, cdr, _) = &*handle_d {
                                let cdr = *cdr;
                                self.set_register(registers, dest as usize, cdr);
                            } else {
                                return Err((VMError::new_vm("CDR: Not a pair/conscell."), chunk));
                            }
                        }
                        Value::Nil => self.set_register(registers, dest as usize, Value::Nil),
                        _ => return Err((VMError::new_vm("CDR: Not a pair/conscell."), chunk)),
                    }
                }
                LIST => self
                    .list(&chunk.code[..], registers, wide)
                    .map_err(|e| (e, chunk.clone()))?,
                APND => self
                    .append(&chunk.code[..], registers, wide, false)
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
                        let vh = self.alloc(Object::Vector(Vec::new()));
                        self.set_register(registers, dest as usize, Value::Reference(vh));
                    } else {
                        let mut v = Vec::new();
                        for i in start..end {
                            v.push(get_reg_unref!(registers, i, self));
                        }
                        let vh = self.alloc(Object::Vector(v));
                        self.set_register(registers, dest as usize, Value::Reference(vh));
                    }
                }
                VECMK => {
                    let (dest, op) = decode2!(chunk.code, &mut self.ip, wide);
                    let len = get_reg_unref!(registers, op, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    let val = Value::Reference(
                        self.alloc(Object::Vector(Vec::with_capacity(len as usize))),
                    );
                    self.set_register(registers, dest as usize, val);
                }
                VECELS => {
                    let (dest, op) = decode2!(chunk.code, &mut self.ip, wide);
                    let len = get_reg_unref!(registers, op, self)
                        .get_int()
                        .map_err(|e| (e, chunk.clone()))?;
                    if let Object::Vector(v) = get_reg_unref!(registers, dest, self)
                        .get_object(self)
                        .map_err(|e| (e, chunk.clone()))?
                    {
                        v.resize(len as usize, Value::Undefined);
                    }
                }
                VECPSH => {
                    let (dest, op) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = get_reg_unref!(registers, op, self);
                    if let Object::Vector(v) = get_reg_unref!(registers, dest, self)
                        .get_object(self)
                        .map_err(|e| (e, chunk.clone()))?
                    {
                        v.push(val);
                    }
                }
                VECPOP => {
                    let (vc, dest) = decode2!(chunk.code, &mut self.ip, wide);
                    let val = if let Object::Vector(v) = get_reg_unref!(registers, vc, self)
                        .get_object(self)
                        .map_err(|e| (e, chunk.clone()))?
                    {
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
                    let val = if let Object::Vector(v) = get_reg_unref!(registers, vc, self)
                        .get_object(self)
                        .map_err(|e| (e, chunk.clone()))?
                    {
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
                    if let Object::Vector(v) = get_reg_unref!(registers, vc, self)
                        .get_object(self)
                        .map_err(|e| (e, chunk.clone()))?
                    {
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
                    let val = Value::Reference(self.alloc(Object::Vector(v)));
                    self.set_register(registers, dest as usize, val);
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
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 2);

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CAR, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 1);

        // car with nil
        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, line).unwrap();
        chunk.encode2(CAR, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].is_nil());

        // car with nil on heap
        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, line).unwrap();
        chunk.encode2(CAR, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].is_nil());

        // cdr with nil
        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CDR, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].is_nil());

        // cdr with nil on heap
        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, line).unwrap();
        chunk.encode2(CDR, 0, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].is_nil());

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 2, line).unwrap();
        chunk.encode2(XAR, 1, 2, line).unwrap();
        chunk.encode2(CAR, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 3);

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 3, line).unwrap();
        chunk.encode2(XDR, 1, 2, line).unwrap();
        chunk.encode2(CDR, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 4);

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, line).unwrap();
        chunk.encode2(CONST, 3, 2, line).unwrap();
        chunk.encode2(XAR, 2, 3, line).unwrap();
        chunk.encode2(CAR, 0, 2, line).unwrap();
        chunk.encode2(CDR, 3, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 3);
        assert!(vm.stack[3].is_nil());

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 2, 4, line).unwrap();
        chunk.encode2(CONST, 3, 3, line).unwrap();
        chunk.encode2(XDR, 2, 3, line).unwrap();
        chunk.encode2(CDR, 0, 2, line).unwrap();
        chunk.encode2(CAR, 3, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 4);
        assert!(vm.stack[3].is_nil());

        // Test a list with elements.
        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 0, 0, line).unwrap();
        chunk.encode2(CONST, 1, 1, line).unwrap();
        chunk.encode2(CONST, 2, 2, line).unwrap();
        chunk.encode3(LIST, 0, 0, 3, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack.get(0).unwrap();
        if let Value::Reference(h) = result {
            if let Object::Pair(car, cdr, _) = &*vm.heap.get(*h) {
                assert!(get_int(&vm, car)? == 1);
                if let Value::Reference(cdr) = cdr {
                    if let Object::Pair(car, cdr, _) = &*vm.heap.get(*cdr) {
                        assert!(get_int(&vm, car)? == 2);
                        if let Value::Reference(cdr) = cdr {
                            if let Object::Pair(car, cdr, _) = &*vm.heap.get(*cdr) {
                                assert!(get_int(&vm, car)? == 3);
                                assert!(is_nil(&vm, cdr)?);
                            } else {
                                assert!(false);
                            }
                        } else {
                            assert!(false);
                        }
                    } else {
                        assert!(false);
                    }
                } else {
                    assert!(false);
                }
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode3(LIST, 0, 0, 0, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack.get(0).unwrap();
        assert!(result.is_nil());

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode3(LIST, 0, 1, 1, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 255);

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 1, 256, line).unwrap();
        chunk.encode3(ADD, 0, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;

        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 255 + 256);

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(MOV, 1, 0, line).unwrap();
        chunk.encode0(RET, line)?;
        let result = vm.stack[1].get_int()?;
        assert!(result == 256);
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[1].get_int()?;
        assert!(result == 255 + 256);

        let mut vm = Vm::new();
        vm.stack[0] = Value::Binding(vm.new_upval(Value::Int(1)));
        vm.stack[1] = Value::Int(10);
        vm.stack[2] = Value::Int(1);
        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(MOV, 1, 0, line).unwrap();
        chunk.encode3(ADD, 1, 1, 2, line).unwrap();
        chunk.encode3(ADD, 1, 1, 2, line).unwrap();
        chunk.encode3(ADD, 1, 1, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].unref(&vm).get_int()?;
        println!("XXXX res: {:?}", result);
        assert!(result == 4);
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
        let chunk = Rc::new(chunk);
        assert!(vm.execute(chunk.clone()).is_err());

        vm.clear_err_frame();
        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        vm.globals.set(slot, Value::Int(11));
        chunk.encode2(CONST, 0, const1, line)?;
        chunk.encode2(REF, 1, 0, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[1].unref(&vm).get_int()? == 11);

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 0, const3, line)?;
        chunk.encode2(CONST, 1, const2, line)?;
        chunk.encode2(DEF, 0, 1, line)?;
        chunk.encode2(REF, 2, 0, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[2].unref(&vm).get_int()? == 42);

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
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
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[2].unref(&vm).get_int()? == 43);

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
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
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[2].unref(&vm).get_int()? == 53);
        assert!(vm.stack[5].unref(&vm).get_int()? == 53);
        assert!(vm.globals.get(slot).get_int()? == 53);

        let mut vm = Vm::new();
        let mut chunk = Rc::try_unwrap(chunk).unwrap();
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
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].unref(&vm).get_int()? == 44);

        let mut vm = Vm::new();
        let mut chunk = Rc::try_unwrap(chunk).unwrap();
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
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].unref(&vm).get_int()? == 55);

        let mut vm = Vm::new();
        let mut chunk = Rc::try_unwrap(chunk).unwrap();
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
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].unref(&vm).get_int()? == 3);
        assert!(vm.globals.get(slot).get_int()? == 3);

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

        let chunk = Rc::new(chunk);
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
        chunk.encode3(ADD, 0, 1, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let add = Value::Reference(vm.alloc(Object::Lambda(Rc::new(chunk))));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Int(10)) as u16;
        chunk.encode2(CONST, 2, const1, line).unwrap();
        chunk.encode3(ADD, 0, 1, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let add_ten = Value::Reference(vm.alloc(Object::Lambda(Rc::new(chunk))));

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

        let chunk = Rc::new(chunk);
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
        chunk.encode3(ADD, 0, 1, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let add = Value::Reference(vm.alloc(Object::Lambda(Rc::new(chunk))));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Int(10)) as u16;
        let const2 = chunk.add_constant(add) as u16;
        chunk.encode2(CONST, 2, const1, line).unwrap();
        chunk.encode2(CONST, 3, const2, line).unwrap();
        chunk.encode2(TCALL, 3, 2, line).unwrap();
        // The TCALL will keep HALT from executing.
        chunk.encode0(HALT, line)?;
        let add_ten = Value::Reference(vm.alloc(Object::Lambda(Rc::new(chunk))));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        vm.stack[1] = Value::Int(5);
        vm.stack[2] = Value::Int(2);
        vm.stack[4] = Value::Int(2);
        vm.stack[50] = add;
        vm.stack[60] = add_ten;
        chunk.encode3(CALL, 60, 1, 3, line).unwrap();
        chunk.encode2(TCALL, 50, 2, line).unwrap();
        // The TCALL will keep HALT from executing.
        chunk.encode0(HALT, line)?;

        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[0].get_int()?;
        assert!(result == 7);
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
            let s = Value::Reference(vm.alloc(Object::String("builtin hello".into())));
            Ok(s)
        }
        let mut vm = Vm::new();
        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Builtin(CallFunc { func: add_b })) as u16;
        chunk.encode2(CONST, 10, const1, line).unwrap();
        chunk.encode3(CALL, 10, 2, 0, line).unwrap();
        chunk.encode0(RET, line)?;
        let add = Value::Reference(vm.alloc(Object::Lambda(Rc::new(chunk))));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Builtin(CallFunc { func: add_b })) as u16;
        chunk.encode2(CONST, 10, const1, line).unwrap();
        chunk.encode2(TCALL, 10, 2, line).unwrap();
        chunk.encode0(RET, line)?;
        let tadd = Value::Reference(vm.alloc(Object::Lambda(Rc::new(chunk))));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Builtin(CallFunc { func: add_10 })) as u16;
        chunk.encode2(CONST, 3, const1, line).unwrap();
        chunk.encode3(CALL, 3, 1, 0, line).unwrap();
        chunk.encode0(RET, line)?;
        let add_ten = Value::Reference(vm.alloc(Object::Lambda(Rc::new(chunk))));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        vm.stack[0] = add;
        vm.stack[1] = add_ten;
        vm.stack[3] = Value::Int(6);
        vm.stack[4] = Value::Int(3);
        vm.stack[6] = Value::Int(12);
        let const1 = chunk.add_constant(Value::Builtin(CallFunc { func: make_str })) as u16;
        chunk.encode3(CALL, 0, 2, 2, line).unwrap();
        chunk.encode3(CALL, 1, 1, 5, line).unwrap();
        chunk.encode2(CONST, 8, const1, line).unwrap();
        chunk.encode3(CALL, 8, 0, 9, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[2].get_int()?;
        assert!(result == 9);
        let result = vm.stack[5].get_int()?;
        assert!(result == 22);
        match vm.stack[9] {
            Value::Reference(h) => match vm.heap.get(h) {
                Object::String(s) => assert!(s == "builtin hello"),
                _ => panic!("bad make_str call."),
            },
            _ => panic!("bad make_str call"),
        }
        assert!(vm.call_stack.is_empty());

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
        chunk.encode2(CONST, 8, const1, line).unwrap();
        chunk.encode3(CALL, 8, 0, 9, line).unwrap();
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let result = vm.stack[2].get_int()?;
        assert!(result == 9);
        let result = vm.stack[5].get_int()?;
        assert!(result == 22);
        match vm.stack[9] {
            Value::Reference(h) => match vm.heap.get(h) {
                Object::String(s) => assert!(s == "builtin hello"),
                _ => panic!("bad make_str call."),
            },
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
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
        if let Object::Vector(v) = vc.get_object(&mut vm)? {
            assert!(v.len() == 101);
            assert!(v[0].get_int()? == 0);
        }
        let vc = vm.stack[10];
        if let Object::Vector(v) = vc.get_object(&mut vm)? {
            assert!(v.len() == 2);
            assert!(v[0].get_int()? == 0);
            assert!(v[1].get_int()? == 1);
        }
        let vc = vm.stack[20];
        if let Object::Vector(v) = vc.get_object(&mut vm)? {
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
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
        let chunk = Rc::new(chunk);
        let res = vm.execute(chunk.clone());
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[rt]: Divide by zero error.");
        Ok(())
    }
}
