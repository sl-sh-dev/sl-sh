use std::collections::HashMap;
use std::fmt;
use std::iter;

use crate::error::*;
use crate::heap::*;
use crate::interner::*;
use crate::vm::Vm;

type CallFunc = fn(vm: &mut Vm, registers: &[Value]) -> VMResult<Value>;

pub struct PairIter<'vm> {
    vm: &'vm Vm,
    current: Option<Value>,
    dotted: bool,
}

impl<'vm> PairIter<'vm> {
    pub fn new(vm: &'vm Vm, exp: Value) -> PairIter {
        PairIter {
            vm,
            current: Some(exp),
            dotted: false,
        }
    }

    pub fn is_dotted(&self) -> bool {
        self.dotted
    }
}

impl<'vm> Iterator for PairIter<'vm> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.current {
            match current {
                Value::Reference(h) => match self.vm.get(h) {
                    Object::Pair(car, cdr) => {
                        self.current = Some(*cdr);
                        Some(*car)
                    }
                    _ => {
                        let cur = Some(current);
                        self.current = None;
                        self.dotted = true;
                        cur
                    }
                },
                Value::Nil => None,
                _ => {
                    let cur = Some(current);
                    self.current = None;
                    self.dotted = true;
                    cur
                }
            }
        } else {
            None
        }
    }
}

#[derive(Copy, Clone)]
pub enum Value {
    Byte(u8),
    Int(i64),
    UInt(u64),
    Float(f64),
    CodePoint(char),
    CharCluster(u8, [u8; 14]),
    CharClusterLong(Handle), // XXX TODO- move to Object?
    Symbol(Interned, Option<u32>),
    StringConst(Interned),
    Reference(Handle),
    Binding(Handle),
    Global(u32),
    Builtin(CallFunc), // XXX TODO, special form?
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
            Self::CodePoint(c) => write!(f, "CodePoint({})", c),
            Self::CharCluster(_, c) => write!(f, "CharCluster({:?})", c),
            Self::CharClusterLong(c) => write!(f, "CharClusterLong({:?})", c),
            Self::Symbol(s, _) => write!(f, "Symbol({:?})", s),
            Self::StringConst(s) => write!(f, "StringConst({:?})", s),
            Self::Reference(r) => write!(f, "Reference({:?})", r),
            Self::Binding(r) => write!(f, "Binding({:?})", r),
            Self::Global(g) => write!(f, "Global({})", g),
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

    // This is used a LOT in a tight loop and this inline seems to help.
    #[inline(always)]
    pub fn unref(self, vm: &Vm) -> Value {
        // This is pointless from a logic standpoint but it makes the vm loop
        // faster...
        if !self.is_indirect() {
            return self;
        }
        match &self {
            Value::Reference(handle) => {
                if let Object::Value(value) = &*vm.get(*handle) {
                    *value
                } else {
                    self
                }
            }
            Value::Binding(handle) => {
                if let Object::Value(value) = &*vm.get(*handle) {
                    match value {
                        Value::Reference(handle) => {
                            if let Object::Value(value2) = vm.get(*handle) {
                                *value2
                            } else {
                                *value
                            }
                        }
                        _ => *value,
                    }
                } else {
                    self
                }
            }
            Value::Global(idx) => {
                let val = vm.get_global(*idx);
                match val {
                    Value::Reference(handle) => {
                        if let Object::Value(value) = vm.get(handle) {
                            *value
                        } else {
                            val
                        }
                    }
                    _ => val,
                }
            }
            _ => self,
        }
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

    pub fn is_indirect(&self) -> bool {
        matches!(
            self,
            Value::Reference(_) | Value::Binding(_) | Value::Global(_)
        )
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

    pub fn is_truethy(&self) -> bool {
        !matches!(self, Value::False | Value::Nil)
    }

    pub fn is_false(&self) -> bool {
        matches!(self, Value::False)
    }

    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::False | Value::Nil)
    }

    pub fn is_int(&self) -> bool {
        matches!(&self, Value::Byte(_) | Value::Int(_) | Value::UInt(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(
            &self,
            Value::Byte(_) | Value::Int(_) | Value::UInt(_) | Value::Float(_)
        )
    }

    pub fn get_int(&self) -> VMResult<i64> {
        match &self {
            Value::Byte(b) => Ok(*b as i64),
            Value::Int(i) => Ok(*i),
            Value::UInt(i) => Ok(*i as i64),
            _ => Err(VMError::new_value(format!("Not an integer: {:?}", self))),
        }
    }

    pub fn get_float(&self) -> VMResult<f64> {
        match &self {
            Value::Byte(b) => Ok(*b as f64),
            Value::Int(i) => Ok(*i as f64),
            Value::UInt(i) => Ok(*i as f64),
            Value::Float(f) => Ok(*f),
            _ => Err(VMError::new_value(format!("Not a float: {:?}", self))),
        }
    }

    pub fn get_object<'vm>(&self, vm: &'vm mut Vm) -> VMResult<HandleRefMut<'vm>> {
        match &self {
            Value::Reference(h) => Ok(vm.get_mut(*h)),
            _ => Err(VMError::new_value("Not an object")),
        }
    }

    pub fn iter<'vm>(&self, vm: &'vm Vm) -> Box<dyn Iterator<Item = Value> + 'vm> {
        match &self.unref(vm) {
            Value::Reference(h) => match vm.get(*h) {
                Object::Pair(_, _) => Box::new(PairIter::new(vm, *self)),
                Object::Vector(v) => Box::new(v.iter().copied()),
                _ => Box::new(iter::empty()),
            },
            _ => Box::new(iter::empty()),
        }
    }

    pub fn display_value(&self, vm: &Vm) -> String {
        fn list_out(vm: &Vm, res: &mut String, itr: &mut dyn Iterator<Item = Value>) {
            let mut first = true;
            for p in itr {
                if !first {
                    res.push(' ');
                } else {
                    first = false;
                }
                res.push_str(&p.display_value(vm));
            }
        }
        match self {
            Value::True => "true".to_string(),
            Value::False => "false".to_string(),
            Value::Float(f) => format!("{}", f),
            Value::Int(i) => format!("{}", i),
            Value::UInt(i) => format!("{}", i),
            Value::Byte(b) => format!("{}", b),
            Value::Symbol(i, _) => vm.get_interned(*i).to_string(),
            Value::StringConst(i) => format!("\"{}\"", vm.get_interned(*i).to_string()),
            Value::CodePoint(ch) => format!("#\\{}", ch),
            Value::CharCluster(l, c) => {
                format!("#\\{}", String::from_utf8_lossy(&c[0..*l as usize]))
            }
            Value::CharClusterLong(_) => "Char".to_string(), // XXX TODO- move this to Object?
            Value::Builtin(_) => "#<Function>".to_string(),
            Value::Binding(_) => self.unref(vm).display_type(vm),
            Value::Global(_) => self.unref(vm).display_type(vm),
            Value::Nil => "nil".to_string(),
            Value::Undefined => "#<Undefined>".to_string(), //panic!("Tried to get type for undefined!"),
            Value::Reference(h) => match vm.get(*h) {
                Object::Lambda(_) => "#<Lambda>".to_string(),
                //Object::Macro(_) => "Macro".to_string(),
                //Object::Process(_) => "Process".to_string(),
                Object::Vector(v) => {
                    let mut res = String::new();
                    res.push_str("#(");
                    list_out(vm, &mut res, &mut v.iter().copied());
                    res.push(')');
                    res
                }
                /*Object::Values(v) => {
                    if v.is_empty() {
                        "Nil".to_string()
                    } else {
                        let v: Expression = (&v[0]).clone();
                        v.display_type()
                    }
                }*/
                Object::Pair(_, _) => {
                    let mut res = String::new();
                    res.push('(');
                    list_out(vm, &mut res, &mut self.iter(vm));
                    res.push(')');
                    res
                }
                Object::Value(v) => v.display_value(vm),
                Object::String(s) => format!("\"{}\"", s),
                Object::Bytes(_) => "Bytes".to_string(), // XXX TODO
                                                         //Object::HashMap(_) => "HashMap".to_string(),
                                                         //Object::File(_) => "File".to_string(),
            },
        }
    }

    pub fn display_type(&self, vm: &Vm) -> String {
        match self {
            Value::True => "True".to_string(),
            Value::False => "False".to_string(),
            Value::Float(_) => "Float".to_string(),
            Value::Int(_) => "Int".to_string(),
            Value::UInt(_) => "UInt".to_string(),
            Value::Symbol(_, _) => "Symbol".to_string(),
            Value::StringConst(_) => "String".to_string(),
            Value::CodePoint(_) => "Char".to_string(),
            Value::CharCluster(_, _) => "Char".to_string(),
            Value::CharClusterLong(_) => "Char".to_string(),
            Value::Builtin(_) => "Builtin".to_string(),
            Value::Byte(_) => "Byte".to_string(),
            Value::Binding(_) => self.unref(vm).display_type(vm),
            Value::Global(_) => self.unref(vm).display_type(vm),
            Value::Nil => "Nil".to_string(),
            Value::Undefined => "Undefined".to_string(), //panic!("Tried to get type for undefined!"),
            Value::Reference(h) => match vm.get(*h) {
                Object::Lambda(_) => "Lambda".to_string(),
                //Object::Macro(_) => "Macro".to_string(),
                //Object::Process(_) => "Process".to_string(),
                /*Object::Function(f) => {
                    if f.is_special_form {
                        "SpecialForm".to_string()
                    } else {
                        "Function".to_string()
                    }
                }*/
                Object::Vector(_) => "Vector".to_string(),
                /*Object::Values(v) => {
                    if v.is_empty() {
                        "Nil".to_string()
                    } else {
                        let v: Expression = (&v[0]).clone();
                        v.display_type()
                    }
                }*/
                Object::Pair(_, _) => "Pair".to_string(),
                Object::Value(v) => v.display_type(vm),
                Object::String(_) => "String".to_string(),
                Object::Bytes(_) => "Bytes".to_string(),
                //Object::HashMap(_) => "HashMap".to_string(),
                //Object::File(_) => "File".to_string(),
            },
        }
    }

    pub fn is_proper_list(&self, vm: &Vm) -> bool {
        // does not detect empty (nil) lists on purpose.
        if let Value::Reference(h) = self {
            if let Object::Pair(_car, cdr) = vm.get(*h) {
                if cdr.is_nil() {
                    true
                } else {
                    cdr.is_proper_list(vm)
                }
            } else {
                false
            }
        } else {
            false
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
