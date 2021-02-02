use std::collections::HashMap;

use crate::error::*;
use crate::heap::*;
use crate::interner::*;
use crate::vm::Vm;

#[derive(Copy, Clone, Debug)]
pub enum Value {
    Byte(u8),
    Int(i64),
    UInt(u64),
    Float(f64),
    Symbol(Interned),
    StringConst(Interned),
    Reference(Handle),
    True,
    False,
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
    objects: Vec<Handle>,
    doc_strings: HashMap<Interned, String, BuildInternedHasher>,
    interner: Interner,
}

impl Globals {
    pub fn new() -> Self {
        Globals {
            objects: Vec::new(),
            doc_strings: HashMap::with_hasher(BuildInternedHasher::new()),
            interner: Interner::with_capacity(8192),
        }
    }

    pub fn intern_symbol(&mut self, string: &str, val: Handle) -> Interned {
        let sym = self.interner.intern(string);
        // Note that the index wrapped in sym must equal the index that is being pushed to objects.
        // ie the interner vector and objects need to stay in lock step.
        self.objects.push(val);
        sym
    }
}

impl Default for Globals {
    fn default() -> Self {
        Self::new()
    }
}
