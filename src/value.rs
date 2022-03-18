use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter;

use crate::error::*;
use crate::heap::*;
use crate::interner::*;
use crate::vm::Vm;

pub type CallFuncSig = fn(vm: &mut Vm, registers: &[Value]) -> VMResult<Value>;
#[derive(Copy, Clone)]
pub struct CallFunc {
    pub func: CallFuncSig,
}

impl PartialEq for CallFunc {
    fn eq(&self, other: &CallFunc) -> bool {
        std::ptr::eq(
            self.func as *const CallFuncSig,
            other.func as *const CallFuncSig,
        )
    }
}

impl Eq for CallFunc {}

impl Hash for CallFunc {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.func as usize);
    }
}

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
                    Object::Pair(car, cdr, _) => {
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

// Do this wrap nonsense so that Value is hashable...
#[derive(Copy, Clone)]
pub struct F64Wrap(pub f64);

impl PartialEq for F64Wrap {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}

impl Eq for F64Wrap {}

impl Hash for F64Wrap {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.0.to_bits());
    }
}

#[derive(Copy, Clone)]
pub enum Value {
    Byte(u8),
    Int(i64),
    UInt(u64),
    Float(F64Wrap),
    CodePoint(char),
    CharCluster(u8, [u8; 14]),
    CharClusterLong(Handle), // XXX TODO- move to Object?
    Symbol(Interned),
    Keyword(Interned),
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

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Byte(v1) => {
                if let Self::Byte(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            Self::Int(v1) => {
                if let Self::Int(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            Self::UInt(v1) => {
                if let Self::UInt(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            Self::Float(v1) => {
                if let Self::Float(v2) = other {
                    v1.0.to_bits() == v2.0.to_bits()
                } else {
                    false
                }
            }
            Self::CodePoint(v1) => {
                if let Self::CodePoint(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            Self::CharCluster(l1, v1) => {
                if let Self::CharCluster(l2, v2) = other {
                    l1 == l2 && v1 == v2
                } else {
                    false
                }
            }
            Self::CharClusterLong(v1) => {
                if let Self::CharClusterLong(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            Self::Symbol(v1) => {
                if let Self::Symbol(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            Self::Keyword(v1) => {
                if let Self::Keyword(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            Self::StringConst(v1) => {
                if let Self::StringConst(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            Self::Reference(v1) => {
                if let Self::Reference(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            Self::Binding(v1) => {
                if let Self::Binding(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            Self::Global(v1) => {
                if let Self::Global(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            Self::Builtin(v1) => {
                if let Self::Builtin(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            Self::True => matches!(other, Self::True),
            Self::False => matches!(other, Self::False),
            Self::Nil => matches!(other, Self::Nil),
            Self::Undefined => matches!(other, Self::Undefined),
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Float(f) => {
                f.0.to_bits().hash(state);
            }
            Value::Byte(b) => {
                1.hash(state);
                b.hash(state);
            }
            Value::Int(i) => {
                (1 << 1).hash(state);
                i.hash(state);
            }
            Value::UInt(i) => {
                (1 << 2).hash(state);
                i.hash(state);
            }
            Value::CodePoint(c) => {
                (1 << 3).hash(state);
                c.hash(state);
            }
            Value::CharCluster(l, a) => {
                (1 << 4).hash(state);
                l.hash(state);
                a.hash(state);
            }
            Value::CharClusterLong(l) => {
                (1 << 5).hash(state);
                l.hash(state);
            }
            Value::Symbol(i) => {
                (1 << 6).hash(state);
                i.hash(state);
            }
            Value::StringConst(i) => {
                (1 << 7).hash(state);
                i.hash(state);
            }
            Value::Reference(r) => {
                (1 << 8).hash(state);
                r.hash(state);
            }
            Value::Binding(b) => {
                (1 << 9).hash(state);
                b.hash(state);
            }
            Value::Global(g) => {
                (1 << 10).hash(state);
                g.hash(state);
            }
            Value::Builtin(c) => {
                (1 << 11).hash(state);
                c.hash(state);
            }
            Value::True => (1 << 12).hash(state),
            Value::False => (1 << 13).hash(state),
            Value::Nil => (1 << 14).hash(state),
            Value::Undefined => (1 << 15).hash(state),
            Value::Keyword(i) => {
                (1 << 16).hash(state);
                i.hash(state);
            }
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Byte(b) => write!(f, "Byte({})", b),
            Self::Int(i) => write!(f, "Int({})", i),
            Self::UInt(i) => write!(f, "UInt({})", i),
            Self::Float(v) => write!(f, "Float({})", v.0),
            Self::CodePoint(c) => write!(f, "CodePoint({})", c),
            Self::CharCluster(_, c) => write!(f, "CharCluster({:?})", c),
            Self::CharClusterLong(c) => write!(f, "CharClusterLong({:?})", c),
            Self::Symbol(s) => write!(f, "Symbol({:?})", s),
            Self::Keyword(s) => write!(f, "Keyword({:?})", s),
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

    pub fn float(f: f64) -> Self {
        Value::Float(F64Wrap(f))
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
            Value::Binding(handle) => vm.get_upval(*handle),
            Value::Global(idx) => vm.get_global(*idx),
            _ => self,
        }
    }

    pub fn get_symbol(&self) -> Option<Interned> {
        if let Value::Symbol(i) = self {
            Some(*i)
        } else {
            None
        }
    }

    pub fn is_symbol(&self, sym: Interned) -> bool {
        if let Value::Symbol(i) = self {
            *i == sym
        } else {
            false
        }
    }

    pub fn is_ref(&self) -> bool {
        matches!(self, Value::Reference(_))
    }

    pub fn is_indirect(&self) -> bool {
        matches!(
            self,
            //Value::Reference(_) | Value::Binding(_) | Value::Global(_)
            Value::Binding(_) | Value::Global(_)
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
            Value::Float(f) => Ok(f.0),
            _ => Err(VMError::new_value(format!("Not a float: {:?}", self))),
        }
    }

    pub fn get_object<'vm>(&self, vm: &'vm mut Vm) -> VMResult<HandleRefMut<'vm>> {
        match &self {
            Value::Reference(h) => Ok(vm.get_mut(*h)),
            _ => Err(VMError::new_value("Not an object")),
        }
    }

    pub fn get_pair(&self, vm: &Vm) -> Option<(Value, Value)> {
        match &self {
            Value::Reference(h) => match vm.get(*h) {
                Object::Pair(car, cdr, _) => Some((*car, *cdr)),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn iter<'vm>(&self, vm: &'vm Vm) -> Box<dyn Iterator<Item = Value> + 'vm> {
        match &self.unref(vm) {
            Value::Reference(h) => match vm.get(*h) {
                Object::Pair(_, _, _) => Box::new(PairIter::new(vm, *self)),
                Object::Vector(v) => Box::new(v.iter().copied()),
                _ => Box::new(iter::empty()),
            },
            _ => Box::new(iter::empty()),
        }
    }

    pub fn display_value(&self, vm: &Vm) -> String {
        fn list_out_iter(vm: &Vm, res: &mut String, itr: &mut dyn Iterator<Item = Value>) {
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
        fn list_out(vm: &Vm, res: &mut String, lst: Value) {
            let mut first = true;
            let mut cdr = lst;
            loop {
                if let Value::Nil = cdr {
                    break;
                }
                if !first {
                    res.push(' ');
                } else {
                    first = false;
                }
                match cdr {
                    Value::Reference(h) => match vm.get(h) {
                        Object::Pair(car, ncdr, _) => {
                            res.push_str(&car.display_value(vm));
                            cdr = *ncdr;
                        }
                        _ => {
                            res.push_str(". ");
                            res.push_str(&cdr.display_value(vm));
                            break;
                        }
                    },
                    _ => {
                        res.push_str(". ");
                        res.push_str(&cdr.display_value(vm));
                        break;
                    }
                }
            }
        }
        match self {
            Value::True => "true".to_string(),
            Value::False => "false".to_string(),
            Value::Float(f) => format!("{}", f.0),
            Value::Int(i) => format!("{}", i),
            Value::UInt(i) => format!("{}", i),
            Value::Byte(b) => format!("{}", b),
            Value::Symbol(i) => vm.get_interned(*i).to_string(),
            Value::Keyword(i) => format!(":{}", vm.get_interned(*i).to_string()),
            Value::StringConst(i) => format!("\"{}\"", vm.get_interned(*i).to_string()),
            Value::CodePoint(ch) => format!("#\\{}", ch),
            Value::CharCluster(l, c) => {
                format!("#\\{}", String::from_utf8_lossy(&c[0..*l as usize]))
            }
            Value::CharClusterLong(_) => "Char".to_string(), // XXX TODO- move this to Object?
            Value::Builtin(_) => "#<Function>".to_string(),
            Value::Binding(_) => self.unref(vm).display_value(vm),
            Value::Global(_) => self.unref(vm).display_value(vm),
            Value::Nil => "nil".to_string(),
            Value::Undefined => "#<Undefined>".to_string(), //panic!("Tried to get type for undefined!"),
            Value::Reference(h) => match vm.get(*h) {
                Object::Lambda(_) => "#<Lambda>".to_string(),
                Object::Macro(_) => "#<Macro>".to_string(),
                Object::Closure(_, _) => "#<Lambda>".to_string(),
                Object::Continuation(_) => "#<Continuation>".to_string(),
                Object::CallFrame(_) => "#<CallFrame>".to_string(),
                //Object::Macro(_) => "Macro".to_string(),
                //Object::Process(_) => "Process".to_string(),
                Object::Vector(v) => {
                    let mut res = String::new();
                    res.push_str("#(");
                    list_out_iter(vm, &mut res, &mut v.iter().copied());
                    res.push(')');
                    res
                }
                Object::Pair(_, _, _) => {
                    let mut res = String::new();
                    res.push('(');
                    list_out(vm, &mut res, *self);
                    res.push(')');
                    res
                }
                Object::String(s) => format!("\"{}\"", s),
                Object::Bytes(_) => "Bytes".to_string(), // XXX TODO
                Object::Upval(val) => val.display_value(vm),
                //Object::HashMap(_) => "HashMap".to_string(),
                //Object::File(_) => "File".to_string(),
            },
        }
    }

    pub fn pretty_value(&self, vm: &Vm) -> String {
        match self {
            Value::StringConst(i) => vm.get_interned(*i).to_string(),
            Value::CodePoint(ch) => format!("{}", ch),
            Value::CharCluster(l, c) => {
                format!("{}", String::from_utf8_lossy(&c[0..*l as usize]))
            }
            Value::CharClusterLong(_) => "Char".to_string(), // XXX TODO- move this to Object?
            Value::Reference(h) => match vm.get(*h) {
                Object::String(s) => format!("{}", s),
                _ => self.display_value(vm),
            },
            _ => self.display_value(vm),
        }
    }

    pub fn display_type(&self, vm: &Vm) -> &'static str {
        match self {
            Value::True => "True",
            Value::False => "False",
            Value::Float(_) => "Float",
            Value::Int(_) => "Int",
            Value::UInt(_) => "UInt",
            Value::Symbol(_) => "Symbol",
            Value::Keyword(_) => "Keyword",
            Value::StringConst(_) => "String",
            Value::CodePoint(_) => "Char",
            Value::CharCluster(_, _) => "Char",
            Value::CharClusterLong(_) => "Char",
            Value::Builtin(_) => "Builtin",
            Value::Byte(_) => "Byte",
            Value::Binding(_) => self.unref(vm).display_type(vm),
            Value::Global(_) => self.unref(vm).display_type(vm),
            Value::Nil => "Nil",
            Value::Undefined => "Undefined", //panic!("Tried to get type for undefined!"),
            Value::Reference(h) => match vm.get(*h) {
                Object::Lambda(_) => "Lambda",
                Object::Macro(_) => "Macro",
                Object::Closure(_, _) => "Lambda",
                Object::Continuation(_) => "Continuation",
                Object::CallFrame(_) => "CallFrame",
                //Object::Macro(_) => "Macro".to_string(),
                //Object::Process(_) => "Process".to_string(),
                /*Object::Function(f) => {
                    if f.is_special_form {
                        "SpecialForm".to_string()
                    } else {
                        "Function".to_string()
                    }
                }*/
                Object::Vector(_) => "Vector",
                Object::Pair(_, _, _) => "Pair",
                Object::String(_) => "String",
                Object::Bytes(_) => "Bytes",
                Object::Upval(val) => val.display_type(vm),
                //Object::HashMap(_) => "HashMap".to_string(),
                //Object::File(_) => "File".to_string(),
            },
        }
    }

    pub fn is_proper_list(&self, vm: &Vm) -> bool {
        // does not detect empty (nil) lists on purpose.
        if let Value::Reference(h) = self {
            if let Object::Pair(_car, cdr, _) = vm.get(*h) {
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
    objects_map: HashMap<Interned, usize>,
    _doc_strings: HashMap<Interned, String>,
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
            objects_map: HashMap::new(),
            _doc_strings: HashMap::new(),
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

    pub fn get_if_interned(&self, symbol: Interned) -> Option<Value> {
        if let Some(idx) = self.objects_map.get(&symbol) {
            self.objects.get(*idx).copied()
        } else {
            None
        }
    }

    pub fn get(&self, idx: u32) -> Value {
        self.objects
            .get(idx as usize)
            .map_or_else(|| Value::Undefined, |v| *v)
    }

    pub fn mark(&self, heap: &mut Heap) {
        for obj in &self.objects {
            if let Value::Reference(handle) = obj {
                heap.mark(*handle);
            }
        }
    }

    pub fn dump(&self, vm: &Vm) {
        for (k, v) in self.objects_map.iter() {
            println!(
                "{} ({}): {}",
                vm.get_interned(*k),
                *v,
                self.objects[*v].display_value(vm)
            );
        }
    }
}
