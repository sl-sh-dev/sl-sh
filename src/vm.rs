use crate::chunk::*;
use crate::decode_u16;
use crate::decode_u32;
use crate::error::*;
use crate::interner::*;
use crate::opcodes::*;
use crate::value::*;

pub struct Vm {
    interner: Interner,
    chunk: Chunk,
    stack: Vec<Value>,
    ip: usize,
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

macro_rules! binary_op {
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
            chunk,
            stack: Vec::with_capacity(256),
            ip: 0,
        }
    }

    pub fn intern(&mut self, string: &str) -> &'static str {
        self.interner.intern(string)
    }

    pub fn execute(&mut self) -> VMResult<()> {
        loop {
            match self.chunk.code[self.ip] {
                RET => {
                    self.ip += 1;
                    return Ok(());
                }
                CONST => {
                    let slot = self.chunk.code[self.ip + 1] as usize;
                    self.ip += 2;
                    if let Some(v) = self.chunk.constants.get(slot) {
                        self.stack.push(v.clone());
                    }
                }
                CONST2 => {
                    let mut decode_iter = self.chunk.code[self.ip + 1..=self.ip + 2].iter();
                    let slot = decode_u16!(decode_iter)?;
                    self.ip += 3;
                    if let Some(v) = self.chunk.constants.get(slot as usize) {
                        self.stack.push(v.clone());
                    }
                }
                CONST4 => {
                    let mut decode_iter = self.chunk.code[self.ip + 1..=self.ip + 4].iter();
                    let slot = decode_u32!(decode_iter)?;
                    self.ip += 5;
                    if let Some(v) = self.chunk.constants.get(slot as usize) {
                        self.stack.push(v.clone());
                    }
                }
                ADD => {
                    self.ip += 1;
                    binary_op!(self, |a, b| a + b);
                }
                SUB => {
                    self.ip += 1;
                    binary_op!(self, |a, b| a - b);
                }
                MUL => {
                    self.ip += 1;
                    binary_op!(self, |a, b| a * b);
                }
                DIV => {
                    self.ip += 1;
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
                _ => {
                    return Err(VMError::new_vm(format!(
                        "Invalid opcode {}",
                        self.chunk.code[self.ip]
                    )));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_const() -> VMResult<()> {
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
