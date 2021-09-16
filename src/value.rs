use std::collections::HashMap;
use std::fmt;

use crate::error::*;
use crate::heap::*;
use crate::interner::*;
use crate::vm::Vm;

type CallFunc = fn(vm: &mut Vm, registers: &[Value]) -> VMResult<Value>;

#[derive(Copy, Clone)]
pub enum Value {
    Byte(u8),
    Int(i64),
    UInt(u64),
    Float(f64),
    Symbol(Interned, Option<u32>),
    StringConst(Interned),
    Reference(Handle),
    Builtin(CallFunc),
    True,
    False,
    Nil,
    Undefined,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Byte(b) => write!(f, "Byte({})", b),
            Self::Int(i) => write!(f, "Int({})", i),
            Self::UInt(i) => write!(f, "UInt({})", i),
            Self::Float(v) => write!(f, "Float({})", v),
            Self::Symbol(s, _) => write!(f, "Symbol({:?})", s),
            Self::StringConst(s) => write!(f, "StringConst({:?})", s),
            Self::Reference(r) => write!(f, "Reference({:?})", r),
            Self::Builtin(_) => write!(f, "Builtin(...)"),
            Self::True => write!(f, "True"),
            Self::False => write!(f, "False"),
            Self::Nil => write!(f, "Nil"),
            Self::Undefined => write!(f, "Undefined"),
        }
    }
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

    pub fn unref(self, vm: &Vm) -> VMResult<Value> {
        if let Value::Reference(handle) = &self {
            if let Object::Value(value) = &*vm.get(*handle)? {
                return Ok(*value);
            }
        }
        Ok(self)
    }

    pub fn handle(self, vm: &mut Vm) -> VMResult<Handle> {
        if let Value::Reference(handle) = self {
            Ok(handle)
        } else {
            Ok(vm.alloc(Object::Value(self)))
        }
    }

    pub fn is_ref(&self) -> bool {
        matches!(self, Value::Reference(_))
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn is_undef(&self) -> bool {
        matches!(self, Value::Undefined)
    }

    pub fn is_true(&self) -> bool {
        matches!(self, Value::True)
    }

    pub fn is_false(&self) -> bool {
        matches!(self, Value::False)
    }

    pub fn is_int(&self) -> bool {
        matches!(&self, Value::Byte(_) | Value::Int(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(&self, Value::Byte(_) | Value::Int(_) | Value::Float(_))
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

#[derive(Clone, Debug)]
pub struct Globals {
    objects: Vec<Value>,
    objects_map: HashMap<Interned, usize, BuildInternedHasher>,
    doc_strings: HashMap<Interned, String, BuildInternedHasher>,
}

impl Default for Globals {
    fn default() -> Self {
        Self::new()
    }
}

impl Globals {
    pub fn new() -> Self {
        Globals {
            objects: Vec::new(),
            objects_map: HashMap::with_hasher(BuildInternedHasher::new()),
            doc_strings: HashMap::with_hasher(BuildInternedHasher::new()),
        }
    }

    pub fn interned_slot(&self, symbol: Interned) -> Option<usize> {
        self.objects_map.get(&symbol).copied()
    }

    pub fn reserve(&mut self, symbol: Interned) -> u32 {
        if let Some(idx) = self.objects_map.get(&symbol) {
            *idx as u32
        } else {
            let index = self.objects.len();
            self.objects.push(Value::Undefined);
            self.objects_map.insert(symbol, index);
            index as u32
        }
    }

    pub fn def(&mut self, symbol: Interned, val: Value) -> u32 {
        if let Some(idx) = self.objects_map.get(&symbol) {
            self.objects[*idx] = val;
            *idx as u32
        } else {
            let index = self.objects.len();
            self.objects.push(val);
            self.objects_map.insert(symbol, index);
            index as u32
        }
    }

    pub fn defvar(&mut self, symbol: Interned, val: Value) -> Option<u32> {
        if let Some(idx) = self.objects_map.get(&symbol) {
            if let Value::Undefined = self.objects[*idx] {
                self.objects[*idx] = val;
                Some(*idx as u32)
            } else {
                None
            }
        } else {
            let index = self.objects.len();
            self.objects.push(val);
            self.objects_map.insert(symbol, index);
            Some(index as u32)
        }
    }

    pub fn set(&mut self, idx: u32, val: Value) {
        self.objects[idx as usize] = val;
    }

    pub fn get_interned(&self, symbol: Interned) -> Value {
        if let Some(idx) = self.objects_map.get(&symbol) {
            self.objects
                .get(*idx)
                .map_or_else(|| Value::Undefined, |v| *v)
        } else {
            Value::Undefined
        }
    }

    pub fn get(&self, idx: u32) -> Value {
        self.objects
            .get(idx as usize)
            .map_or_else(|| Value::Undefined, |v| *v)
    }
}
