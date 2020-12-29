use crate::chunk::*;
use crate::decode_u16;
use crate::decode_u32;
use crate::error::*;
use crate::heap::*;
use crate::interner::*;
use crate::opcodes::*;
use crate::value::*;

pub struct Vm {
    interner: Interner,
    heap: Heap,
    chunk: Chunk,
    stack: Vec<Value>,
    ip: usize,
}

macro_rules! one_val {
    ($vm:expr) => {{
        if let Some(op) = $vm.stack.pop() {
            Ok(op)
        } else {
            Err(VMError::new_vm("Not enough values on stack."))
        }
    }};
}

macro_rules! two_vals {
    ($vm:expr) => {{
        if let Some(op2) = $vm.stack.pop() {
            if let Some(op1) = $vm.stack.pop() {
                Ok((op1, op2))
            } else {
                Err(VMError::new_vm("Not enough values on stack."))
            }
        } else {
            Err(VMError::new_vm("Not enough values on stack."))
        }
    }};
}

macro_rules! binary_math {
    ($vm:expr, $bin_fn:expr) => {{
        let (op1, op2) = two_vals!($vm)?;
        if op1.is_int() && op2.is_int() {
            $vm.stack
                .push(Value::Int($bin_fn(op1.get_int()?, op2.get_int()?)));
        } else {
            $vm.stack
                .push(Value::Float($bin_fn(op1.get_float()?, op2.get_float()?)));
        }
    }};
}

impl Vm {
    pub fn new(chunk: Chunk) -> Self {
        Vm {
            interner: Interner::with_capacity(8192),
            heap: Heap::new(),
            chunk,
            stack: Vec::with_capacity(256),
            ip: 0,
        }
    }

    pub fn intern(&mut self, string: &str) -> &'static str {
        self.interner.intern(string)
    }

    fn exec_cons(&mut self, opcode: OpCode) -> VMResult<()> {
        match opcode {
            CONS => {
                let (op1, op2) = two_vals!(self)?;
                let car = if let Value::Reference(handle) = op1 {
                    handle
                } else {
                    self.heap.alloc(Object::Value(op1))
                };
                let cdr = if let Value::Reference(handle) = op2 {
                    handle
                } else {
                    self.heap.alloc(Object::Value(op2))
                };
                self.stack
                    .push(Value::Reference(self.heap.alloc(Object::Pair(car, cdr))));
            }
            CAR => {
                let op = one_val!(self)?;
                if let Value::Reference(handle) = op {
                    let handle_d = self.heap.get(&handle)?;
                    if let Object::Pair(car, _) = &*handle_d {
                        if let Object::Value(val) = &*self.heap.get(car)? {
                            self.stack.push(val.clone());
                        } else {
                            self.stack.push(Value::Reference(car.clone()));
                        }
                    } else if let Object::Value(Value::Nil) = &*handle_d {
                        self.stack.push(Value::Nil);
                    } else {
                        return Err(VMError::new_vm("CAR: Not a pair/conscell."));
                    }
                } else if let Value::Nil = op {
                    self.stack.push(Value::Nil);
                } else {
                    return Err(VMError::new_vm("CAR: Not a pair/conscell."));
                }
            }
            CDR => {
                let op = one_val!(self)?;
                if let Value::Reference(handle) = op {
                    let handle_d = self.heap.get(&handle)?;
                    if let Object::Pair(_, cdr) = &*handle_d {
                        if let Object::Value(val) = &*self.heap.get(cdr)? {
                            self.stack.push(val.clone());
                        } else {
                            self.stack.push(Value::Reference(cdr.clone()));
                        }
                    } else if let Object::Value(Value::Nil) = &*handle_d {
                        self.stack.push(Value::Nil);
                    } else {
                        return Err(VMError::new_vm("CDR: Not a pair/conscell."));
                    }
                } else if let Value::Nil = op {
                    self.stack.push(Value::Nil);
                } else {
                    return Err(VMError::new_vm("CDR: Not a pair/conscell."));
                }
            }
            XAR => {
                let (pair, val) = two_vals!(self)?;
                match &pair {
                    Value::Reference(cons_handle) => {
                        let cons_d = self.heap.get(&cons_handle)?;
                        if let Object::Pair(_car, cdr) = &*cons_d {
                            if let Value::Reference(val_handle) = val {
                                let cdr = cdr.clone();
                                drop(cons_d);
                                self.heap
                                    .replace(&cons_handle, Object::Pair(val_handle, cdr))?;
                            } else {
                                let cdr = cdr.clone();
                                drop(cons_d);
                                let new_car = self.heap.alloc(Object::Value(val));
                                self.heap
                                    .replace(&cons_handle, Object::Pair(new_car, cdr))?;
                            }
                            self.stack.push(pair.clone());
                        } else if let Object::Value(Value::Nil) = &*cons_d {
                            let car = if let Value::Reference(handle) = val {
                                handle
                            } else {
                                self.heap.alloc(Object::Value(val))
                            };
                            let cdr = self.heap.alloc(Object::Value(Value::Nil));
                            let pair = Object::Pair(car, cdr);
                            drop(cons_d);
                            self.heap.replace(&cons_handle, pair)?;
                            self.stack.push(Value::Reference(cons_handle.clone()));
                        } else {
                            return Err(VMError::new_vm("XAR: Not a pair/conscell."));
                        }
                    }
                    Value::Nil => {
                        let car = if let Value::Reference(handle) = val {
                            handle
                        } else {
                            self.heap.alloc(Object::Value(val))
                        };
                        let cdr = self.heap.alloc(Object::Value(Value::Nil));
                        let pair = self.heap.alloc(Object::Pair(car, cdr));
                        self.stack.push(Value::Reference(pair));
                    }
                    _ => {
                        return Err(VMError::new_vm("XAR: Not a pair/conscell."));
                    }
                }
            }
            XDR => {
                let (pair, val) = two_vals!(self)?;
                match &pair {
                    Value::Reference(cons_handle) => {
                        let cons_d = self.heap.get(&cons_handle)?;
                        if let Object::Pair(car, _cdr) = &*cons_d {
                            if let Value::Reference(val_handle) = val {
                                let car = car.clone();
                                drop(cons_d);
                                self.heap
                                    .replace(&cons_handle, Object::Pair(car, val_handle))?;
                            } else {
                                let car = car.clone();
                                drop(cons_d);
                                let new_cdr = self.heap.alloc(Object::Value(val));
                                self.heap
                                    .replace(&cons_handle, Object::Pair(car, new_cdr))?;
                            }
                            self.stack.push(pair.clone());
                        } else if let Object::Value(Value::Nil) = &*cons_d {
                            let car = self.heap.alloc(Object::Value(Value::Nil));
                            let cdr = if let Value::Reference(handle) = val {
                                handle
                            } else {
                                self.heap.alloc(Object::Value(val))
                            };
                            let pair = Object::Pair(car, cdr);
                            drop(cons_d);
                            self.heap.replace(&cons_handle, pair)?;
                            self.stack.push(Value::Reference(cons_handle.clone()));
                        } else {
                            return Err(VMError::new_vm("XDR: Not a pair/conscell."));
                        }
                    }
                    Value::Nil => {
                        let car = self.heap.alloc(Object::Value(Value::Nil));
                        let cdr = if let Value::Reference(handle) = val {
                            handle
                        } else {
                            self.heap.alloc(Object::Value(val))
                        };
                        let pair = self.heap.alloc(Object::Pair(car, cdr));
                        self.stack.push(Value::Reference(pair));
                    }
                    _ => {
                        return Err(VMError::new_vm("XDR: Not a pair/conscell."));
                    }
                }
            }
            LIST => {
                let mut decode_iter = self.chunk.code[self.ip..=self.ip + 1].iter();
                let num_elements = decode_u16!(decode_iter)?;
                if num_elements > 0 {
                    let mut last_cdr = self.heap.alloc(Object::Value(Value::Nil));
                    for _ in 0..num_elements {
                        let car = if let Some(op) = self.stack.pop() {
                            if let Value::Reference(handle) = op {
                                handle
                            } else {
                                self.heap.alloc(Object::Value(op))
                            }
                        } else {
                            return Err(VMError::new_vm("List: Not enough elements."));
                        };
                        let cdr = last_cdr;
                        last_cdr = self.heap.alloc(Object::Pair(car, cdr));
                    }
                    self.stack.push(Value::Reference(last_cdr));
                } else {
                    self.stack
                        .push(Value::Reference(self.heap.alloc(Object::Value(Value::Nil))));
                }
                self.ip += 2;
            }
            _ => {
                //This should be impossible (due to how it is called) so could just panic?
                return Err(VMError::new_vm(format!("Invalid opcode {}", opcode)));
            }
        }
        Ok(())
    }

    pub fn execute(&mut self) -> VMResult<()> {
        loop {
            let opcode = self.chunk.code[self.ip];
            self.ip += 1;
            match opcode {
                RET => {
                    return Ok(());
                }
                CONST => {
                    let slot = self.chunk.code[self.ip] as usize;
                    self.ip += 1;
                    if let Some(v) = self.chunk.constants.get(slot) {
                        self.stack.push(v.clone());
                    }
                }
                CONST2 => {
                    let mut decode_iter = self.chunk.code[self.ip..=self.ip + 1].iter();
                    let slot = decode_u16!(decode_iter)?;
                    self.ip += 2;
                    if let Some(v) = self.chunk.constants.get(slot as usize) {
                        self.stack.push(v.clone());
                    }
                }
                CONST4 => {
                    let mut decode_iter = self.chunk.code[self.ip..=self.ip + 3].iter();
                    let slot = decode_u32!(decode_iter)?;
                    self.ip += 4;
                    if let Some(v) = self.chunk.constants.get(slot as usize) {
                        self.stack.push(v.clone());
                    }
                }
                LOAD => {
                    let slot = self.chunk.code[self.ip] as usize;
                    self.ip += 1;
                    let val = self.stack.get(slot);
                    if let Some(v) = val {
                        let v = v.clone();
                        self.stack.push(v);
                    } else {
                        return Err(VMError::new_vm("LOAD: Invalid stack access."));
                    }
                }
                LOAD2 => {
                    let mut decode_iter = self.chunk.code[self.ip..=self.ip + 1].iter();
                    let slot = decode_u16!(decode_iter)? as usize;
                    self.ip += 2;
                    let val = self.stack.get(slot);
                    if let Some(v) = val {
                        let v = v.clone();
                        self.stack.push(v);
                    } else {
                        return Err(VMError::new_vm("LOAD2: Invalid stack access."));
                    }
                }
                POP => {
                    self.stack.pop();
                }
                STORE => {
                    let slot = self.chunk.code[self.ip] as usize;
                    self.ip += 1;
                    if let Some(v) = self.stack.pop() {
                        self.stack[slot] = v;
                    } else {
                        return Err(VMError::new_vm("STORE: Invalid stack access."));
                    }
                }
                STORE2 => {
                    let mut decode_iter = self.chunk.code[self.ip..=self.ip + 1].iter();
                    let slot = decode_u16!(decode_iter)? as usize;
                    self.ip += 2;
                    if let Some(v) = self.stack.pop() {
                        self.stack[slot] = v;
                    } else {
                        return Err(VMError::new_vm("STORE2: Invalid stack access."));
                    }
                }
                ADD => {
                    binary_math!(self, |a, b| a + b);
                }
                SUB => {
                    binary_math!(self, |a, b| a - b);
                }
                MUL => {
                    binary_math!(self, |a, b| a * b);
                }
                DIV => {
                    let (op1, op2) = two_vals!(self)?;
                    if op1.is_int() && op2.is_int() {
                        let op2 = op2.get_int()?;
                        if op2 == 0 {
                            return Err(VMError::new_vm("Divide by zero error."));
                        }
                        self.stack.push(Value::Int(op1.get_int()? / op2));
                    } else {
                        let op2 = op2.get_float()?;
                        if op2 == 0.0 {
                            return Err(VMError::new_vm("Divide by zero error."));
                        }
                        self.stack.push(Value::Float(op1.get_float()? / op2));
                    }
                }
                CONS..=LIST => self.exec_cons(opcode)?,
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

    fn get_int(vm: &Vm, handle: &Handle) -> VMResult<i64> {
        if let Object::Value(Value::Int(i)) = &*vm.heap.get(handle)? {
            Ok(*i)
        } else {
            Err(VMError::new_vm("Not an int"))
        }
    }

    fn is_nil(vm: &Vm, handle: &Handle) -> VMResult<bool> {
        if let Object::Value(Value::Nil) = &*vm.heap.get(handle)? {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    #[test]
    fn test_list() -> VMResult<()> {
        let mut chunk = Chunk::with_file("no_file", 1);
        let line = 1;
        chunk.add_constant(Value::Int(1));
        chunk.add_constant(Value::Int(2));
        chunk.add_constant(Value::Int(3));
        chunk.add_constant(Value::Int(4));
        chunk.add_constant(Value::Nil);
        chunk.push_const(0, line).unwrap();
        chunk.push_const(1, line).unwrap();
        chunk.push_simple(CONS, line)?;
        chunk.push_simple(CAR, line)?;
        chunk.push_simple(RET, line)?;
        let mut vm = Vm::new(chunk);
        vm.chunk
            .add_constant(Value::Reference(vm.heap.alloc(Object::Value(Value::Nil))));
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let result = vm.stack.last().unwrap().get_int()?;
        assert!(result == 1);

        vm.chunk.push_const(0, line).unwrap();
        vm.chunk.push_const(1, line).unwrap();
        vm.chunk.push_simple(CONS, line)?;
        vm.chunk.push_simple(CDR, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 2);
        let result = vm.stack.last().unwrap().get_int()?;
        assert!(result == 2);

        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_const(0, line).unwrap();
        vm.chunk.push_const(1, line).unwrap();
        vm.chunk.push_simple(CONS, line)?;
        vm.chunk.push_load(0, line).unwrap();
        vm.chunk.push_const(2, line).unwrap();
        vm.chunk.push_simple(XAR, line)?;
        vm.chunk.push_simple(CAR, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 2);
        let result = vm.stack.last().unwrap().get_int()?;
        assert!(result == 3);

        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_simple(CAR, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let result = vm.stack.last().unwrap().get_int()?;
        assert!(result == 3);

        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_const(0, line).unwrap();
        vm.chunk.push_const(1, line).unwrap();
        vm.chunk.push_simple(CONS, line)?;
        vm.chunk.push_load(0, line).unwrap();
        vm.chunk.push_const(2, line).unwrap();
        vm.chunk.push_simple(XDR, line)?;
        vm.chunk.push_simple(CDR, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 2);
        let result = vm.stack.last().unwrap().get_int()?;
        assert!(result == 3);

        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_simple(CDR, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let result = vm.stack.last().unwrap().get_int()?;
        assert!(result == 3);

        // Test nil with CAR
        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_const(4, line).unwrap();
        vm.chunk.push_simple(CAR, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let result = vm.stack.last().unwrap();
        if let Value::Nil = result {
        } else {
            assert!(false);
        }

        // Test nil stored on the heap with CAR
        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_const(5, line).unwrap();
        vm.chunk.push_simple(CAR, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let result = vm.stack.last().unwrap();
        if let Value::Nil = result {
        } else {
            assert!(false);
        }

        // Test nil with CDR
        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_const(4, line).unwrap();
        vm.chunk.push_simple(CDR, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let result = vm.stack.last().unwrap();
        if let Value::Nil = result {
        } else {
            assert!(false);
        }

        // Test nil stored on the heap with CDR
        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_const(5, line).unwrap();
        vm.chunk.push_simple(CDR, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let result = vm.stack.last().unwrap();
        if let Value::Nil = result {
        } else {
            assert!(false);
        }

        // Test a list with elements.
        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_const(0, line).unwrap();
        vm.chunk.push_const(1, line).unwrap();
        vm.chunk.push_const(2, line).unwrap();
        vm.chunk.push_u16(LIST, 3, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let result = vm.stack.last().unwrap();
        if let Value::Reference(h) = result {
            if let Object::Pair(car, cdr) = &*vm.heap.get(h)? {
                assert!(get_int(&vm, car)? == 1);
                if let Object::Pair(car, cdr) = &*vm.heap.get(cdr)? {
                    assert!(get_int(&vm, car)? == 2);
                    if let Object::Pair(car, cdr) = &*vm.heap.get(cdr)? {
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

        // Test that an empty list produces nil (stored on the heap).
        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_u16(LIST, 0, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let result = vm.stack.last().unwrap();
        if let Value::Reference(h) = result {
            assert!(is_nil(&vm, &h)?);
        } else {
            assert!(false);
        }
        vm.chunk.push_const(2, line).unwrap();
        vm.chunk.push_simple(XDR, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        if let Value::Reference(handle) = vm.stack.last().unwrap() {
            if let Object::Pair(car, cdr) = &*vm.heap.get(handle)? {
                assert!(get_int(&vm, cdr)? == 3);
                assert!(is_nil(&vm, car)?);
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }
        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_u16(LIST, 0, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let result = vm.stack.last().unwrap();
        if let Value::Reference(h) = result {
            assert!(is_nil(&vm, &h)?);
        } else {
            assert!(false);
        }
        vm.chunk.push_const(2, line).unwrap();
        vm.chunk.push_simple(XAR, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        if let Value::Reference(handle) = vm.stack.last().unwrap() {
            if let Object::Pair(car, cdr) = &*vm.heap.get(handle)? {
                assert!(get_int(&vm, car)? == 3);
                assert!(is_nil(&vm, cdr)?);
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }

        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_const(4, line).unwrap();
        vm.chunk.push_const(2, line).unwrap();
        vm.chunk.push_simple(XAR, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        if let Value::Reference(handle) = vm.stack.last().unwrap() {
            if let Object::Pair(car, cdr) = &*vm.heap.get(handle)? {
                assert!(get_int(&vm, car)? == 3);
                assert!(is_nil(&vm, cdr)?);
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }

        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_const(4, line).unwrap();
        vm.chunk.push_const(2, line).unwrap();
        vm.chunk.push_simple(XDR, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        if let Value::Reference(handle) = vm.stack.last().unwrap() {
            if let Object::Pair(car, cdr) = &*vm.heap.get(handle)? {
                assert!(get_int(&vm, cdr)? == 3);
                assert!(is_nil(&vm, car)?);
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }

        Ok(())
    }

    #[test]
    fn test_stack() -> VMResult<()> {
        let mut chunk = Chunk::with_file("no_file", 1);
        let line = 1;
        for i in 0..(u16::MAX as usize + 10) {
            chunk.add_constant(Value::Int(i as i64));
        }
        chunk.push_const(0, line).unwrap();
        chunk.push_const(255, line).unwrap();
        chunk.push_simple(ADD, line)?;
        chunk.push_simple(RET, line)?;

        let mut vm = Vm::new(chunk);
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let result = vm.stack.last().unwrap().get_int()?;
        assert!(result == 255);

        vm.chunk.push_const(256, line).unwrap();
        vm.chunk.push_simple(ADD, line)?;
        vm.chunk.push_simple(RET, line)?;

        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let result = vm.stack.last().unwrap().get_int()?;
        assert!(result == 255 + 256);

        vm.chunk.push_const(u16::MAX as usize, line).unwrap();
        vm.chunk.push_simple(ADD, line)?;
        vm.chunk.push_const(u16::MAX as usize + 9, line).unwrap();
        vm.chunk.push_simple(ADD, line)?;
        vm.chunk.push_simple(RET, line)?;
        //let mut vm = Vm::new(chunk);
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let result = vm.stack.pop().unwrap().get_int()?;
        assert!(result == (0 + 255 + 256 + u16::MAX as i64 + (u16::MAX as i64 + 9)) as i64);

        vm.chunk.push_const(0, line)?;
        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 0);

        vm.chunk.push_const(1, line)?;
        vm.chunk.push_const(2, line)?;
        vm.chunk.push_const(3, line)?;
        vm.chunk.push_load(0, line)?;
        vm.chunk.push_load(1, line)?;
        vm.chunk.push_simple(ADD, line)?;
        vm.chunk.push_store(0, line)?;
        vm.chunk.push_load(1, line)?;
        vm.chunk.push_load(2, line)?;
        vm.chunk.push_simple(ADD, line)?;
        vm.chunk.push_store(1, line)?;
        vm.chunk.push_const(0, line)?;
        vm.chunk.push_store(2, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 3);
        assert!(vm.stack.pop().unwrap().get_int()? == 0);
        assert!(vm.stack.pop().unwrap().get_int()? == 5);
        assert!(vm.stack.pop().unwrap().get_int()? == 3);

        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_simple(POP, line)?;
        vm.chunk.push_simple(POP, line)?;
        // test the "long" versions of load/store
        for _i in 0..255 {
            vm.chunk.push_const(0, line)?;
        }
        vm.chunk.push_const(1, line)?;
        vm.chunk.push_const(2, line)?;
        vm.chunk.push_const(3, line)?;
        vm.chunk.push_load(255, line)?;
        vm.chunk.push_load(256, line)?;
        vm.chunk.push_simple(ADD, line)?;
        vm.chunk.push_store(255, line)?;
        vm.chunk.push_load(256, line)?;
        vm.chunk.push_load(257, line)?;
        vm.chunk.push_simple(ADD, line)?;
        vm.chunk.push_store(256, line)?;
        vm.chunk.push_const(0, line)?;
        vm.chunk.push_store(257, line)?;
        vm.chunk.push_simple(RET, line)?;
        vm.execute()?;
        assert!(vm.stack.len() == 258);
        assert!(vm.stack.pop().unwrap().get_int()? == 0);
        assert!(vm.stack.pop().unwrap().get_int()? == 5);
        assert!(vm.stack.pop().unwrap().get_int()? == 3);

        for _i in 0..258 {
            vm.chunk.push_simple(POP, line)?;
        }
        vm.chunk.push_load(3, line)?;
        vm.chunk.push_simple(RET, line)?;
        assert!(vm.execute().is_err());

        // Currently trying to write to an invalid stack location will panic
        // which is probably ok but may revisit to make a vm error instead.
        //vm.chunk.push_store(3, line)?;
        //vm.chunk.push_simple(RET, line)?;
        //assert!(vm.execute().is_err());

        Ok(())
    }

    #[test]
    fn test_add() -> VMResult<()> {
        let mut chunk = Chunk::with_file("no_file", 1);
        let line = 1;
        let mut off = chunk.add_constant(Value::Int(2 as i64));
        chunk.push_const(off, line).unwrap();
        off = chunk.add_constant(Value::Int(3 as i64));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(ADD, line)?;
        off = chunk.add_constant(Value::Byte(1));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(ADD, line)?;
        chunk.push_simple(RET, line)?;
        let mut vm = Vm::new(chunk);
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        assert!(vm.stack.pop().unwrap().get_int()? == 6);

        chunk = Chunk::with_file("no_file", 1);
        off = chunk.add_constant(Value::Float(2 as f64));
        chunk.push_const(off, line).unwrap();
        off = chunk.add_constant(Value::Int(3 as i64));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(ADD, line)?;
        off = chunk.add_constant(Value::Byte(1));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(ADD, line)?;
        chunk.push_simple(RET, line)?;
        let mut vm = Vm::new(chunk);
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let item = vm.stack.pop().unwrap();
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 6.0);
        Ok(())
    }

    #[test]
    fn test_sub() -> VMResult<()> {
        let mut chunk = Chunk::with_file("no_file", 1);
        let line = 1;
        let mut off = chunk.add_constant(Value::Int(2 as i64));
        chunk.push_const(off, line).unwrap();
        off = chunk.add_constant(Value::Int(3 as i64));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(SUB, line)?;
        off = chunk.add_constant(Value::Byte(1));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(SUB, line)?;
        chunk.push_simple(RET, line)?;
        let mut vm = Vm::new(chunk);
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        assert!(vm.stack.pop().unwrap().get_int()? == -2);

        chunk = Chunk::with_file("no_file", 1);
        off = chunk.add_constant(Value::Float(5 as f64));
        chunk.push_const(off, line).unwrap();
        off = chunk.add_constant(Value::Int(3 as i64));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(SUB, line)?;
        off = chunk.add_constant(Value::Byte(1));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(SUB, line)?;
        chunk.push_simple(RET, line)?;
        let mut vm = Vm::new(chunk);
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let item = vm.stack.pop().unwrap();
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 1.0);
        Ok(())
    }

    #[test]
    fn test_mul() -> VMResult<()> {
        let mut chunk = Chunk::with_file("no_file", 1);
        let line = 1;
        let mut off = chunk.add_constant(Value::Int(2 as i64));
        chunk.push_const(off, line).unwrap();
        off = chunk.add_constant(Value::Int(3 as i64));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(MUL, line)?;
        off = chunk.add_constant(Value::Byte(1));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(MUL, line)?;
        chunk.push_simple(RET, line)?;
        let mut vm = Vm::new(chunk);
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        assert!(vm.stack.pop().unwrap().get_int()? == 6);

        chunk = Chunk::with_file("no_file", 1);
        off = chunk.add_constant(Value::Float(5 as f64));
        chunk.push_const(off, line).unwrap();
        off = chunk.add_constant(Value::Int(3 as i64));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(MUL, line)?;
        off = chunk.add_constant(Value::Byte(2));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(MUL, line)?;
        chunk.push_simple(RET, line)?;
        let mut vm = Vm::new(chunk);
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let item = vm.stack.pop().unwrap();
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 30.0);
        Ok(())
    }

    #[test]
    fn test_div() -> VMResult<()> {
        let mut chunk = Chunk::with_file("no_file", 1);
        let line = 1;
        let mut off = chunk.add_constant(Value::Int(18 as i64));
        chunk.push_const(off, line).unwrap();
        off = chunk.add_constant(Value::Int(2 as i64));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(DIV, line)?;
        off = chunk.add_constant(Value::Byte(3));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(DIV, line)?;
        chunk.push_simple(RET, line)?;
        let mut vm = Vm::new(chunk);
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        assert!(vm.stack.pop().unwrap().get_int()? == 3);

        chunk = Chunk::with_file("no_file", 1);
        off = chunk.add_constant(Value::Float(10 as f64));
        chunk.push_const(off, line).unwrap();
        off = chunk.add_constant(Value::Int(2 as i64));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(DIV, line)?;
        off = chunk.add_constant(Value::Byte(2));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(DIV, line)?;
        chunk.push_simple(RET, line)?;
        vm = Vm::new(chunk);
        vm.execute()?;
        assert!(vm.stack.len() == 1);
        let item = vm.stack.pop().unwrap();
        assert!(!item.is_int());
        assert!(item.is_number());
        assert!(item.get_float()? == 2.5);

        chunk = Chunk::with_file("no_file", 1);
        off = chunk.add_constant(Value::Int(10 as i64));
        chunk.push_const(off, line).unwrap();
        off = chunk.add_constant(Value::Int(0 as i64));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(DIV, line)?;
        chunk.push_simple(RET, line)?;
        vm = Vm::new(chunk);
        let res = vm.execute();
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[VM]: Divide by zero error.");

        chunk = Chunk::with_file("no_file", 1);
        off = chunk.add_constant(Value::Float(10 as f64));
        chunk.push_const(off, line).unwrap();
        off = chunk.add_constant(Value::Float(0 as f64));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(DIV, line)?;
        chunk.push_simple(RET, line)?;
        vm = Vm::new(chunk);
        let res = vm.execute();
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[VM]: Divide by zero error.");

        chunk = Chunk::with_file("no_file", 1);
        off = chunk.add_constant(Value::Float(10 as f64));
        chunk.push_const(off, line).unwrap();
        off = chunk.add_constant(Value::Byte(0));
        chunk.push_const(off, line).unwrap();
        chunk.push_simple(DIV, line)?;
        chunk.push_simple(RET, line)?;
        vm = Vm::new(chunk);
        let res = vm.execute();
        assert!(res.is_err());
        assert!(res.unwrap_err().to_string() == "[VM]: Divide by zero error.");
        Ok(())
    }
}
