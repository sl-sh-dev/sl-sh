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

macro_rules! decode2 {
    ($code:expr, $ip:expr, $wide:expr) => {{
        let op1 = if $wide {
            decode_u16!($code, $ip)
        } else {
            let ret = $code[*$ip] as u16;
            *$ip += 1;
            ret
        };
        let op2 = if $wide {
            decode_u16!($code, $ip)
        } else {
            let ret = $code[*$ip] as u16;
            *$ip += 1;
            ret
        };
        (op1, op2)
    }};
}

macro_rules! decode3 {
    ($code:expr, $ip:expr, $wide:expr) => {{
        let op1 = if $wide {
            decode_u16!($code, $ip)
        } else {
            let ret = $code[*$ip] as u16;
            *$ip += 1;
            ret
        };
        let op2 = if $wide {
            decode_u16!($code, $ip)
        } else {
            let ret = $code[*$ip] as u16;
            *$ip += 1;
            ret
        };
        let op3 = if $wide {
            decode_u16!($code, $ip)
        } else {
            let ret = $code[*$ip] as u16;
            *$ip += 1;
            ret
        };
        (op1, op2, op3)
    }};
}

macro_rules! binary_math {
    ($chunk:expr, $ip:expr, $registers:expr, $bin_fn:expr, $wide:expr, $op2_reg:expr, $op3_reg:expr) => {{
        let (dest, op2, op3) = decode3!($chunk.code, $ip, $wide);
        let op2 = if $op2_reg {
            $registers[op2 as usize]
        } else {
            $chunk.constants[op2 as usize]
        };
        let op3 = if $op3_reg {
            $registers[op3 as usize]
        } else {
            $chunk.constants[op3 as usize]
        };
        let val = if op2.is_int() && op3.is_int() {
            Value::Int($bin_fn(op2.get_int()?, op3.get_int()?))
        } else {
            Value::Float($bin_fn(op2.get_float()?, op3.get_float()?))
        };
        set_register!($registers, dest, val);
    }};
}

macro_rules! div_math {
    ($chunk:expr, $ip:expr, $registers:expr, $wide:expr, $op2_reg:expr, $op3_reg:expr) => {{
        let (dest, op2, op3) = decode3!($chunk.code, $ip, $wide);
        let op2 = if $op2_reg {
            $registers[op2 as usize]
        } else {
            $chunk.constants[op2 as usize]
        };
        let op3 = if $op3_reg {
            $registers[op3 as usize]
        } else {
            $chunk.constants[op3 as usize]
        };
        let val = if op2.is_int() && op3.is_int() {
            let op3 = op3.get_int()?;
            if op3 == 0 {
                return Err(VMError::new_vm("Divide by zero error."));
            }
            Value::Int(op2.get_int()? / op3)
        } else {
            let op3 = op3.get_float()?;
            if op3 == 0.0 {
                return Err(VMError::new_vm("Divide by zero error."));
            }
            Value::Float(op2.get_float()? / op3)
        };
        set_register!($registers, dest, val);
        // Ok(())
    }};
}

macro_rules! set_register {
    ($registers:expr, $idx:expr, $val:expr) => {{
        $registers[$idx as usize] = $val;
        /*unsafe {
            let r = $registers.get_unchecked_mut($idx as usize);
            *r = $val;
        }*/
    }};
}

pub struct CallFrame {
    chunk: Rc<Chunk>,
    ip: usize,
    stack_top: usize,
}

pub struct Vm {
    interner: Interner,
    heap: Heap,
    stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    globals: Globals,
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
        }
    }

    pub fn alloc(&mut self, obj: Object) -> Handle {
        self.heap.alloc(obj, |_heap| Ok(()))
    }

    pub fn get(&self, handle: Handle) -> VMResult<HandleRef<'_>> {
        self.heap.get(handle)
    }

    pub fn intern(&mut self, string: &str) -> Interned {
        self.interner.intern(string)
    }

    pub fn reserve_symbol(&mut self, string: &str) -> Value {
        let sym = self.interner.intern(string);
        Value::Symbol(sym, Some(self.globals.reserve(sym)))
    }

    pub fn def_symbol(&mut self, string: &str, value: Value) -> Value {
        let sym = self.interner.intern(string);
        Value::Symbol(sym, Some(self.globals.def(sym, value)))
    }

    fn list(
        &mut self,
        code: &[u8],
        ip: &mut usize,
        registers: &mut [Value],
        wide: bool,
    ) -> VMResult<()> {
        let (dest, start, end) = decode3!(code, ip, wide);
        if end == start {
            set_register!(registers, dest, Value::Nil);
        } else {
            let mut last_cdr = Value::Nil;
            for i in (start..end).rev() {
                let car = if let Some(op) = registers.get(i as usize) {
                    op
                } else {
                    return Err(VMError::new_vm("List: Not enough elements."));
                };
                let cdr = last_cdr;
                last_cdr = Value::Reference(self.alloc(Object::Pair(*car, cdr)));
            }
            set_register!(registers, dest, last_cdr);
        }
        Ok(())
    }

    fn xar(
        &mut self,
        code: &[u8],
        ip: &mut usize,
        registers: &mut [Value],
        wide: bool,
    ) -> VMResult<()> {
        let (pair_reg, val) = decode2!(code, ip, wide);
        let pair = registers[pair_reg as usize];
        let val = registers[val as usize];
        match &pair {
            Value::Reference(cons_handle) => {
                let cons_d = self.heap.get(*cons_handle)?;
                if let Object::Pair(_car, cdr) = &*cons_d {
                    let cdr = *cdr;
                    self.heap.replace(*cons_handle, Object::Pair(val, cdr))?;
                } else if cons_d.is_nil() {
                    let pair = Object::Pair(val, Value::Nil);
                    self.heap.replace(*cons_handle, pair)?;
                } else {
                    return Err(VMError::new_vm("XAR: Not a pair/conscell."));
                }
            }
            Value::Nil => {
                let pair = Value::Reference(self.alloc(Object::Pair(val, Value::Nil)));
                set_register!(registers, pair_reg, pair);
            }
            _ => {
                return Err(VMError::new_vm("XAR: Not a pair/conscell."));
            }
        }
        Ok(())
    }

    fn xdr(
        &mut self,
        code: &[u8],
        ip: &mut usize,
        registers: &mut [Value],
        wide: bool,
    ) -> VMResult<()> {
        let (pair_reg, val) = decode2!(code, ip, wide);
        let pair = registers[pair_reg as usize];
        let val = registers[val as usize];
        match &pair {
            Value::Reference(cons_handle) => {
                let cons_d = self.heap.get(*cons_handle)?;
                if let Object::Pair(car, _cdr) = &*cons_d {
                    let car = *car;
                    self.heap.replace(*cons_handle, Object::Pair(car, val))?;
                } else if cons_d.is_nil() {
                    let pair = Object::Pair(Value::Nil, val);
                    self.heap.replace(*cons_handle, pair)?;
                } else {
                    return Err(VMError::new_vm("XAR: Not a pair/conscell."));
                }
            }
            Value::Nil => {
                let pair = Value::Reference(self.alloc(Object::Pair(Value::Nil, val)));
                set_register!(registers, pair_reg, pair);
            }
            _ => {
                return Err(VMError::new_vm("XAR: Not a pair/conscell."));
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

    pub fn execute(&mut self, chunk: Rc<Chunk>) -> VMResult<()> {
        let mut stack_top = 0;
        let mut registers = self.make_registers(stack_top);
        let mut chunk = chunk;
        let mut ip = 0;
        loop {
            let opcode = chunk.code[ip];
            ip += 1;
            let wide = (opcode & 0x80) != 0;
            match opcode & 0x7F {
                RET => {
                    if let Some(frame) = self.call_stack.pop() {
                        stack_top = frame.stack_top;
                        registers = self.make_registers(stack_top);
                        chunk = frame.chunk.clone();
                        ip = frame.ip;
                    } else {
                        return Ok(());
                    }
                }
                MOV => {
                    let (dest, src) = decode2!(chunk.code, &mut ip, wide);
                    let val = registers[src as usize];
                    set_register!(registers, dest, val);
                }
                CONST => {
                    let (dest, src) = decode2!(chunk.code, &mut ip, wide);
                    let val = chunk.constants[src as usize];
                    set_register!(registers, dest, val);
                }
                REF => {
                    let (dest, src) = decode2!(chunk.code, &mut ip, wide);
                    let val = if let Value::Symbol(s, i) = registers[src as usize] {
                        if let Some(i) = i {
                            self.globals.get(i)
                        } else {
                            self.globals.get_interned(s)
                        }
                    } else {
                        return Err(VMError::new_vm("REF: Not a symbol."));
                    };
                    if let Value::Undefined = val {
                        return Err(VMError::new_vm("REF: Symbol is not defined."));
                    }
                    set_register!(registers, dest, val);
                }
                DEF => {
                    let (dest, src) = decode2!(chunk.code, &mut ip, wide);
                    let val = registers[src as usize];
                    if let Value::Symbol(s, i) = registers[dest as usize] {
                        if let Some(i) = i {
                            self.globals.set(i, val);
                        } else {
                            self.globals.def(s, val);
                        }
                    } else {
                        return Err(VMError::new_vm("DEF: Not a symbol."));
                    }
                }
                DEFV => {
                    let (dest, src) = decode2!(chunk.code, &mut ip, wide);
                    let val = registers[src as usize];
                    if let Value::Symbol(s, i) = registers[dest as usize] {
                        if let Some(i) = i {
                            if let Value::Undefined = self.globals.get(i) {
                                self.globals.set(i, val);
                            }
                        } else {
                            self.globals.defvar(s, val);
                        }
                    } else {
                        return Err(VMError::new_vm("DEFV: Not a symbol."));
                    }
                }
                CALL => {
                    let (lambda, num_args, first_reg) = decode3!(chunk.code, &mut ip, wide);
                    let lambda = registers[lambda as usize];
                    match lambda {
                        Value::Builtin(f) => {
                            let last_reg = (first_reg + num_args + 1) as usize;
                            let res = f(self, &registers[(first_reg + 1) as usize..last_reg])?;
                            set_register!(registers, first_reg, res);
                        }
                        Value::Reference(h) => match self.heap.get(h)? {
                            Object::Lambda(l) => {
                                let frame = CallFrame {
                                    chunk: chunk.clone(),
                                    ip,
                                    stack_top,
                                };
                                self.call_stack.push(frame);
                                stack_top = first_reg as usize;
                                chunk = l.clone();
                                ip = 0;
                                registers = self.make_registers(stack_top);
                            }
                            _ => return Err(VMError::new_vm("CALL: Not a callable.")),
                        },
                        _ => return Err(VMError::new_vm("CALL: Not a callable.")),
                    }
                }
                ADD => binary_math!(chunk, &mut ip, registers, |a, b| a + b, wide, true, true),
                ADD_RK => binary_math!(chunk, &mut ip, registers, |a, b| a + b, wide, true, false),
                ADD_KR => binary_math!(chunk, &mut ip, registers, |a, b| a + b, wide, false, true),
                SUB => binary_math!(chunk, &mut ip, registers, |a, b| a - b, wide, true, true),
                SUB_RK => binary_math!(chunk, &mut ip, registers, |a, b| a - b, wide, true, false),
                SUB_KR => binary_math!(chunk, &mut ip, registers, |a, b| a - b, wide, false, true),
                MUL => binary_math!(chunk, &mut ip, registers, |a, b| a * b, wide, true, true),
                MUL_RK => binary_math!(chunk, &mut ip, registers, |a, b| a * b, wide, true, false),
                MUL_KR => binary_math!(chunk, &mut ip, registers, |a, b| a * b, wide, false, true),
                DIV => div_math!(chunk, &mut ip, registers, wide, true, true),
                DIV_RK => div_math!(chunk, &mut ip, registers, wide, true, false),
                DIV_KR => div_math!(chunk, &mut ip, registers, wide, false, true),
                CONS => {
                    let (dest, op2, op3) = decode3!(chunk.code, &mut ip, wide);
                    let car = registers[op2 as usize];
                    let cdr = registers[op3 as usize];
                    set_register!(
                        registers,
                        dest,
                        Value::Reference(self.alloc(Object::Pair(car, cdr)))
                    );
                }
                CAR => {
                    let (dest, op) = decode2!(chunk.code, &mut ip, wide);
                    let op = registers[op as usize];
                    match op.unref(self)? {
                        Value::Reference(handle) => {
                            let handle_d = self.heap.get(handle)?;
                            if let Object::Pair(car, _) = &*handle_d {
                                set_register!(registers, dest, *car);
                            } else {
                                return Err(VMError::new_vm("CAR: Not a pair/conscell."));
                            }
                        }
                        Value::Nil => set_register!(registers, dest, Value::Nil),
                        _ => return Err(VMError::new_vm("CAR: Not a pair/conscell.")),
                    }
                }
                CDR => {
                    let (dest, op) = decode2!(chunk.code, &mut ip, wide);
                    let op = registers[op as usize];
                    match op.unref(self)? {
                        Value::Reference(handle) => {
                            let handle_d = self.heap.get(handle)?;
                            if let Object::Pair(_, cdr) = &*handle_d {
                                set_register!(registers, dest, *cdr);
                            } else {
                                return Err(VMError::new_vm("CDR: Not a pair/conscell."));
                            }
                        }
                        Value::Nil => set_register!(registers, dest, Value::Nil),
                        _ => return Err(VMError::new_vm("CDR: Not a pair/conscell.")),
                    }
                }
                LIST => self.list(&chunk.code[..], &mut ip, registers, wide)?,
                XAR => self.xar(&chunk.code[..], &mut ip, registers, wide)?,
                XDR => self.xdr(&chunk.code[..], &mut ip, registers, wide)?,
                _ => {
                    return Err(VMError::new_vm(format!("Invalid opcode {}", opcode)));
                }
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
        let const_handle = vm.alloc(Object::Value(Value::Nil));
        chunk.add_constant(Value::Reference(const_handle));
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
        chunk.encode2(CONST, 2, 5, line).unwrap();
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
        chunk.encode2(CONST, 2, 5, line).unwrap();
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
            if let Object::Pair(car, cdr) = &*vm.heap.get(*h)? {
                assert!(get_int(&vm, car)? == 1);
                if let Value::Reference(cdr) = cdr {
                    if let Object::Pair(car, cdr) = &*vm.heap.get(*cdr)? {
                        assert!(get_int(&vm, car)? == 2);
                        if let Value::Reference(cdr) = cdr {
                            if let Object::Pair(car, cdr) = &*vm.heap.get(*cdr)? {
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
        for i in 0..u16::MAX {
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
        let const0 = chunk.add_constant(Value::Symbol(sym, Some(slot))) as u16;
        let const1 = chunk.add_constant(Value::Symbol(sym2, None)) as u16;
        let const2 = chunk.add_constant(Value::Int(42)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(REF, 1, 0, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        assert!(vm.execute(chunk.clone()).is_err());

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        vm.globals.set(slot, Value::Int(11));
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(REF, 1, 0, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[1].get_int()? == 11);

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        chunk.encode2(CONST, 0, const1, line)?;
        chunk.encode2(CONST, 1, const2, line)?;
        chunk.encode2(DEF, 0, 1, line)?;
        chunk.encode2(REF, 2, 0, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[2].get_int()? == 42);

        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        vm.globals.set(slot, Value::Int(11));
        let slot = vm.globals.interned_slot(sym2).unwrap() as u32;
        let const1 = chunk.add_constant(Value::Symbol(sym2, Some(slot))) as u16;
        let const2 = chunk.add_constant(Value::Int(43)) as u16;
        let const3 = chunk.add_constant(Value::Int(53)) as u16;
        chunk.encode2(CONST, 0, const1, line)?;
        chunk.encode2(CONST, 1, const2, line)?;
        chunk.encode2(CONST, 3, const3, line)?;
        chunk.encode2(DEF, 0, 1, line)?;
        chunk.encode2(DEFV, 0, 3, line)?;
        chunk.encode2(REF, 2, 0, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[2].get_int()? == 43);

        let mut vm = Vm::new();
        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        let slot = vm.globals.reserve(sym2);
        let const1 = chunk.add_constant(Value::Symbol(sym2, Some(slot))) as u16;
        let const2 = chunk.add_constant(Value::Int(44)) as u16;
        let const3 = chunk.add_constant(Value::Int(53)) as u16;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode2(CONST, 2, const2, line)?;
        chunk.encode2(CONST, 3, const3, line)?;
        chunk.encode2(DEFV, 1, 2, line)?;
        chunk.encode2(DEFV, 1, 3, line)?;
        chunk.encode2(REF, 0, 1, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].get_int()? == 44);

        let mut vm = Vm::new();
        let mut chunk = Rc::try_unwrap(chunk).unwrap();
        chunk.code.clear();
        let slot = vm.globals.reserve(sym2);
        let const1 = chunk.add_constant(Value::Symbol(sym2, Some(slot))) as u16;
        let const2 = chunk.add_constant(Value::Int(45)) as u16;
        let const3 = chunk.add_constant(Value::Int(55)) as u16;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode2(CONST, 2, const2, line)?;
        chunk.encode2(CONST, 3, const3, line)?;
        chunk.encode2(DEFV, 1, 2, line)?;
        chunk.encode2(DEF, 1, 3, line)?;
        chunk.encode2(REF, 0, 1, line)?;
        chunk.encode0(RET, line)?;
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].get_int()? == 55);

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
    fn test_builtin() -> VMResult<()> {
        fn add(_vm: &mut Vm, registers: &[Value]) -> VMResult<Value> {
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
        let const1 = chunk.add_constant(Value::Builtin(add)) as u16;
        chunk.encode2(CONST, 10, const1, line).unwrap();
        chunk.encode3(CALL, 10, 2, 0, line).unwrap();
        chunk.encode0(RET, line)?;
        let add = Value::Reference(vm.alloc(Object::Lambda(Rc::new(chunk))));

        let mut chunk = Chunk::new("no_file", 1);
        let line = 1;
        let const1 = chunk.add_constant(Value::Builtin(add_10)) as u16;
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
        let const1 = chunk.add_constant(Value::Builtin(make_str)) as u16;
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
            Value::Reference(h) => match vm.heap.get(h)? {
                Object::String(s) => assert!(s == "builtin hello"),
                _ => panic!("bad make_str call."),
            },
            _ => panic!("bad make_str call"),
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
        chunk.encode3(ADD_RK, 0, 0, const1, line).unwrap();
        chunk.encode3(ADD_RK, 0, 0, const2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].get_int()? == 6);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::Float(2 as f64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3 as i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode3(ADD_RK, 0, 0, const1, line).unwrap();
        chunk.encode3(ADD_RK, 0, 0, const2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let item = vm.stack[0];
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 6.0);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..u16::MAX {
            chunk.add_constant(Value::Int(i as i64));
        }
        chunk.encode2(CONST, 1, 1, line)?;
        chunk.encode2(CONST, 2, 2, line)?;
        chunk.encode3(ADD, 0, 1, 2, line).unwrap();
        chunk.encode3(ADD_KR, 0, 5, 0, line).unwrap();
        chunk.encode3(ADD_KR, 1, 500, 0, line).unwrap();
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
        chunk.encode3(SUB_RK, 0, 0, const1, line).unwrap();
        chunk.encode3(SUB_RK, 0, 0, const2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].get_int()? == -2);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::Float(5 as f64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3 as i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(1)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode3(SUB_RK, 0, 0, const1, line).unwrap();
        chunk.encode3(SUB_RK, 0, 0, const2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let item = vm.stack[0];
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 1.0);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..u16::MAX {
            chunk.add_constant(Value::Int(i as i64));
        }
        chunk.encode2(CONST, 1, 1, line)?;
        chunk.encode2(CONST, 2, 2, line)?;
        chunk.encode3(SUB, 0, 1, 2, line).unwrap();
        chunk.encode3(SUB_KR, 0, 5, 0, line).unwrap();
        chunk.encode3(SUB_KR, 1, 500, 0, line).unwrap();
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
        chunk.encode3(MUL_RK, 0, 0, const1, line).unwrap();
        chunk.encode3(MUL_RK, 0, 0, const2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].get_int()? == 6);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::Float(5 as f64)) as u16;
        let const1 = chunk.add_constant(Value::Int(3 as i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(2)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode3(MUL_RK, 0, 0, const1, line).unwrap();
        chunk.encode3(MUL_RK, 0, 0, const2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let item = vm.stack[0];
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 30.0);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..u16::MAX {
            chunk.add_constant(Value::Int(i as i64));
        }
        chunk.encode2(CONST, 1, 1, line)?;
        chunk.encode2(CONST, 2, 2, line)?;
        chunk.encode3(MUL, 0, 1, 2, line).unwrap();
        chunk.encode3(MUL_KR, 0, 5, 0, line).unwrap();
        chunk.encode3(MUL_KR, 1, 500, 0, line).unwrap();
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
        chunk.encode3(DIV_RK, 0, 0, const1, line).unwrap();
        chunk.encode3(DIV_RK, 0, 0, const2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        assert!(vm.stack[0].get_int()? == 3);

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::Float(10 as f64)) as u16;
        let const1 = chunk.add_constant(Value::Int(2 as i64)) as u16;
        let const2 = chunk.add_constant(Value::Byte(2)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode3(DIV_RK, 0, 0, const1, line).unwrap();
        chunk.encode3(DIV_RK, 0, 0, const2, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Rc::new(chunk);
        vm.execute(chunk.clone())?;
        let item = vm.stack[0];
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 2.5);

        let mut chunk = Chunk::new("no_file", 1);
        for i in 0..u16::MAX {
            chunk.add_constant(Value::Int(i as i64));
        }
        chunk.encode2(CONST, 1, 1, line)?;
        chunk.encode2(CONST, 2, 2, line)?;
        chunk.encode3(DIV, 0, 2, 1, line).unwrap();
        chunk.encode3(DIV_KR, 0, 10, 0, line).unwrap();
        chunk.encode3(DIV_KR, 1, 500, 0, line).unwrap();
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
        assert!(res.unwrap_err().to_string() == "[VM]: Divide by zero error.");

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::Float(10 as f64)) as u16;
        let const1 = chunk.add_constant(Value::Float(0 as f64)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode3(DIV, 0, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Rc::new(chunk);
        let res = vm.execute(chunk.clone());
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[VM]: Divide by zero error.");

        let mut chunk = Chunk::new("no_file", 1);
        let const0 = chunk.add_constant(Value::Float(10 as f64)) as u16;
        let const1 = chunk.add_constant(Value::Byte(0)) as u16;
        chunk.encode2(CONST, 0, const0, line)?;
        chunk.encode2(CONST, 1, const1, line)?;
        chunk.encode3(DIV, 0, 0, 1, line).unwrap();
        chunk.encode0(RET, line)?;
        let mut vm = Vm::new();
        let chunk = Rc::new(chunk);
        let res = vm.execute(chunk.clone());
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[VM]: Divide by zero error.");
        Ok(())
    }
}
