use crate::error::*;
use crate::heap::Handle;

// Ideally Value would implement Copy but if Handle is a RC wrapper it can not
// be Copy.  Then intent is to support both an RC based heap and GC based heap
// so sticking with Clone for now but may need to revist this.
// Clone needs to be CHEAP for Value.
#[derive(Clone, Debug)]
pub enum Value {
    Byte(u8),
    Int(i64),
    UInt(u64),
    Float(f64),
    Reference(Handle),
    Nil,
    Undefined,
}

impl Default for Value {
    fn default() -> Self {
        Self::new()
    }
}

impl Value {
    pub fn new() -> Self {
        Value::Undefined
    }

    pub fn is_int(&self) -> bool {
        match &self {
            Value::Byte(_) => true,
            Value::Int(_) => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        match &self {
            Value::Byte(_) => true,
            Value::Int(_) => true,
            Value::Float(_) => true,
            _ => false,
        }
    }

    pub fn get_int(&self) -> VMResult<i64> {
        match &self {
            Value::Byte(b) => Ok(*b as i64),
            Value::Int(i) => Ok(*i),
            _ => Err(VMError::new_value("Not an integer")),
        }
    }

    pub fn get_float(&self) -> VMResult<f64> {
        match &self {
            Value::Byte(b) => Ok(*b as f64),
            Value::Int(i) => Ok(*i as f64),
            Value::Float(f) => Ok(*f),
            _ => Err(VMError::new_value("Not a float")),
        }
    }
}
