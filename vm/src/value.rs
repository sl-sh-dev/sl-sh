use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter;
use std::sync::Arc;

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

impl fmt::Debug for CallFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "...")
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
                Value::Pair(h) => {
                    let (car, cdr) = self.vm.get_pair(h);
                    self.current = Some(cdr);
                    Some(car)
                }
                // TODO: Handle List?
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
#[derive(Copy, Clone, Debug)]
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

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
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
    Global(u32),
    Builtin(CallFunc), // XXX TODO, special form?
    True,
    False,
    Nil,
    Undefined,

    String(Handle),
    Vector(Handle),
    Bytes(Handle),
    Pair(Handle),
    List(Handle, u32),
    Lambda(Handle),
    Closure(Handle),
    Continuation(Handle),
    CallFrame(Handle),
    Value(Handle),
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
            Value::Value(handle) => vm.get_value(*handle),
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

    pub fn is_indirect(&self) -> bool {
        matches!(
            self,
            //Value::Reference(_) | Value::Binding(_) | Value::Global(_)
            Value::Value(_) | Value::Global(_)
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

    pub fn get_handle(&self) -> Option<Handle> {
        match &self {
            Value::CharClusterLong(handle) => Some(*handle),
            Value::String(handle) => Some(*handle),
            Value::Vector(handle) => Some(*handle),
            Value::Bytes(handle) => Some(*handle),
            Value::Pair(handle) => Some(*handle),
            Value::List(handle, _) => Some(*handle),
            Value::Lambda(handle) => Some(*handle),
            Value::Closure(handle) => Some(*handle),
            Value::Continuation(handle) => Some(*handle),
            Value::CallFrame(handle) => Some(*handle),
            Value::Value(handle) => Some(*handle),

            Value::Byte(_) => None,
            Value::Int(_) => None,
            Value::UInt(_) => None,
            Value::Float(_) => None,
            Value::CodePoint(_) => None,
            Value::CharCluster(_, _) => None,
            Value::Symbol(_) => None,
            Value::Keyword(_) => None,
            Value::StringConst(_) => None,
            Value::Global(_) => None,
            Value::Builtin(_) => None,
            Value::True => None,
            Value::False => None,
            Value::Nil => None,
            Value::Undefined => None,
        }
    }

    pub fn get_pair(&self, vm: &Vm) -> Option<(Value, Value)> {
        match &self {
            Value::Pair(handle) => {
                let (car, cdr) = vm.get_pair(*handle);
                Some((car, cdr))
            }
            Value::List(handle, start_u32) => {
                let start = *start_u32 as usize;
                let v = vm.get_vector(*handle);
                let car = if start < v.len() {
                    v[start]
                } else {
                    Value::Nil
                };
                let cdr = if start + 1 < v.len() {
                    Value::List(*handle, start_u32 + 1)
                } else {
                    Value::Nil
                };
                Some((car, cdr))
            }
            _ => None,
        }
    }

    pub fn iter<'vm>(&self, vm: &'vm Vm) -> Box<dyn Iterator<Item = Value> + 'vm> {
        match &self.unref(vm) {
            Value::Pair(_) => Box::new(PairIter::new(vm, *self)),
            Value::List(handle, start) => {
                Box::new(vm.get_vector(*handle)[*start as usize..].iter().copied())
            }
            Value::Vector(handle) => Box::new(vm.get_vector(*handle).iter().copied()),
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
                    Value::Pair(handle) => {
                        let (car, ncdr) = vm.get_pair(handle);
                        res.push_str(&car.display_value(vm));
                        cdr = ncdr;
                    }
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
            Value::Keyword(i) => format!(":{}", vm.get_interned(*i)),
            Value::StringConst(i) => format!("\"{}\"", vm.get_interned(*i)),
            Value::CodePoint(ch) => format!("#\\{}", ch),
            Value::CharCluster(l, c) => {
                format!("#\\{}", String::from_utf8_lossy(&c[0..*l as usize]))
            }
            Value::CharClusterLong(_) => "Char".to_string(), // XXX TODO- move this to Object?
            Value::Builtin(_) => "#<Function>".to_string(),
            Value::Global(_) => self.unref(vm).display_value(vm),
            Value::Nil => "nil".to_string(),
            Value::Undefined => "#<Undefined>".to_string(), //panic!("Tried to get type for undefined!"),
            Value::Lambda(_) => "#<Lambda>".to_string(),
            Value::Closure(_) => "#<Lambda>".to_string(),
            Value::Continuation(_) => "#<Continuation>".to_string(),
            Value::CallFrame(_) => "#<CallFrame>".to_string(),
            Value::Vector(handle) => {
                let v = vm.get_vector(*handle);
                let mut res = String::new();
                res.push_str("#(");
                list_out_iter(vm, &mut res, &mut v.iter().copied());
                res.push(')');
                res
            }
            Value::Pair(_) => {
                let mut res = String::new();
                res.push('(');
                list_out(vm, &mut res, *self);
                res.push(')');
                res
            }
            Value::List(handle, start) => {
                let v = vm.get_vector(*handle);
                let mut res = String::new();
                res.push('(');
                list_out_iter(vm, &mut res, &mut v[*start as usize..].iter().copied());
                res.push(')');
                res
            }
            Value::String(handle) => format!("\"{}\"", vm.get_string(*handle)),
            Value::Bytes(_) => "Bytes".to_string(), // XXX TODO
            Value::Value(handle) => vm.get_value(*handle).display_value(vm),
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
            Value::String(handle) => vm.get_string(*handle).to_string(),
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
            Value::Global(_) => self.unref(vm).display_type(vm),
            Value::Nil => "Nil",
            Value::Undefined => "Undefined", //panic!("Tried to get type for undefined!"),
            Value::Lambda(_) => "Lambda",
            Value::Closure(_) => "Lambda",
            Value::Continuation(_) => "Continuation",
            Value::CallFrame(_) => "CallFrame",
            Value::Vector(_) => "Vector",
            Value::Pair(_) => "Pair",
            Value::List(_, _) => "Pair",
            Value::String(_) => "String",
            Value::Bytes(_) => "Bytes",
            Value::Value(handle) => vm.get_value(*handle).display_type(vm),
        }
    }

    pub fn is_proper_list(&self, vm: &Vm) -> bool {
        // does not detect empty (nil) lists on purpose.
        if let Value::Pair(handle) = self {
            let (_car, cdr) = vm.get_pair(*handle);
            if cdr.is_nil() {
                true
            } else {
                cdr.is_proper_list(vm)
            }
        } else {
            matches!(self, Value::List(_, _))
        }
    }
}

#[derive(Clone, Debug)]
pub struct Globals {
    objects: Vec<Value>,
    objects_map: HashMap<Interned, usize>,
    props: HashMap<u32, Arc<HashMap<Interned, Value>>>,
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
            props: HashMap::new(),
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
        self.objects.iter().for_each(|obj| {
            if let Some(handle) = obj.get_handle() {
                heap.mark(handle);
            }
        });
    }

    pub fn dump(&self, vm: &Vm) {
        let mut ordered_keys = Vec::with_capacity(self.objects_map.len());
        ordered_keys.resize(self.objects_map.len(), "");
        for (k, v) in self.objects_map.iter() {
            ordered_keys[*v] = vm.get_interned(*k);
        }
        for (i, k) in ordered_keys.iter().enumerate() {
            println!(
                "({:#010x})/{}: {}",
                i,
                *k,
                self.objects[i].display_value(vm)
            );
        }
    }

    pub fn index_to_name(&self, vm: &Vm, idx: usize) -> &'static str {
        for (k, v) in self.objects_map.iter() {
            if *v == idx {
                return vm.get_interned(*k);
            }
        }
        "#<N/A>"
    }

    pub fn get_property(&self, global: u32, prop: Interned) -> Option<Value> {
        if let Some(map) = self.props.get(&global) {
            if let Some(val) = map.get(&prop) {
                return Some(*val);
            }
        }
        None
    }

    pub fn set_property(&mut self, global: u32, prop: Interned, value: Value) {
        if let Some(map) = self.props.get_mut(&global) {
            let map = Arc::make_mut(map);
            map.insert(prop, value);
        } else {
            let mut map = HashMap::new();
            map.insert(prop, value);
            self.props.insert(global, Arc::new(map));
        }
    }
}
