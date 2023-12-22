use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter;
use std::sync::Arc;

use crate::error::*;
use crate::handle::Numeric64Handle;
use crate::heap::*;
use crate::interner::*;
use crate::persistent_vec::PersistentVecIter;
use crate::vm::GVm;

pub type CallFuncSig<ENV> = fn(vm: &mut GVm<ENV>, registers: &[Value]) -> VMResult<Value>;
#[derive(Copy, Clone)]
pub struct CallFunc<ENV> {
    pub func: CallFuncSig<ENV>,
}

impl<ENV> PartialEq for CallFunc<ENV> {
    fn eq(&self, other: &CallFunc<ENV>) -> bool {
        std::ptr::eq(
            self.func as *const CallFuncSig<ENV>,
            other.func as *const CallFuncSig<ENV>,
        )
    }
}

impl<ENV> Eq for CallFunc<ENV> {}

impl<ENV> Hash for CallFunc<ENV> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.func as usize);
    }
}

impl<ENV> fmt::Debug for CallFunc<ENV> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "...")
    }
}

pub struct PairIter<'vm, ENV> {
    vm: &'vm GVm<ENV>,
    current: Option<Value>,
    dotted: bool,
}

impl<'vm, ENV> PairIter<'vm, ENV> {
    pub fn new(vm: &'vm GVm<ENV>, exp: Value) -> Self {
        Self {
            vm,
            current: Some(exp),
            dotted: false,
        }
    }

    pub fn is_dotted(&self) -> bool {
        self.dotted
    }
}

impl<'vm, ENV> Iterator for PairIter<'vm, ENV> {
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
pub enum Numeric {
    Local(u16),
    Heap(Numeric64Handle),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Value {
    Byte(u8),
    Int32(i32),
    UInt32(u32),
    Int64(Numeric),
    UInt64(Numeric),
    Float64(Numeric),
    CodePoint(char),
    CharCluster(u8, [u8; 6]),
    CharClusterLong(Handle), // Handle points to a String on the heap.
    Symbol(Interned),
    Keyword(Interned),
    StringConst(Interned),
    Special(Interned), // Intended for symbols that are compiled.
    Builtin(u32),
    True,
    False,
    Nil,
    Undefined,

    String(Handle),
    Vector(Handle),
    PersistentVec(Handle),
    VecNode(Handle),
    PersistentMap(Handle),
    MapNode(Handle),
    Map(Handle),
    Bytes(Handle),
    Pair(Handle),
    List(Handle, u16),
    Lambda(Handle),
    Closure(Handle),
    Continuation(Handle),
    CallFrame(Handle),
    Value(Handle),
    Error(Handle),
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

    #[inline(always)]
    pub fn unref<ENV>(self, vm: &GVm<ENV>) -> Value {
        match &self {
            Value::Value(handle) => vm.get_value(*handle),
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
        matches!(self, Value::Value(_))
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
        matches!(
            &self,
            Value::Byte(_)
                | Value::Int32(_)
                | Value::UInt32(_)
                | Value::Int64(_)
                | Value::UInt64(_)
        )
    }

    pub fn is_number(&self) -> bool {
        matches!(
            &self,
            Value::Byte(_)
                | Value::Int32(_)
                | Value::UInt32(_)
                | Value::Int64(_)
                | Value::UInt64(_)
                | Value::Float64(_)
        )
    }

    pub fn get_int<ENV>(&self, vm: &GVm<ENV>) -> VMResult<i64> {
        match &self {
            Value::Byte(b) => Ok(*b as i64),
            Value::Int32(i) => Ok(*i as i64),
            Value::UInt32(i) => Ok(*i as i64),
            Value::Int64(handle) => Ok(vm.get_int(*handle)),
            Value::UInt64(handle) => Ok(vm.get_uint(*handle) as i64), // XXX TODO- overflow.
            _ => Err(VMError::new_value(format!("Not an integer: {self:?}"))),
        }
    }

    pub fn get_float<ENV>(&self, vm: &GVm<ENV>) -> VMResult<f64> {
        match &self {
            Value::Byte(b) => Ok(*b as f64),
            Value::Int32(i) => Ok(*i as f64),
            Value::UInt32(i) => Ok(*i as f64),
            Value::Float64(handle) => Ok(vm.get_float(*handle)),
            Value::Int64(handle) => Ok(vm.get_int(*handle) as f64),
            Value::UInt64(handle) => Ok(vm.get_uint(*handle) as f64),
            _ => Err(VMError::new_value(format!("Not a float: {self:?}"))),
        }
    }

    pub fn get_string<'vm, ENV>(&self, vm: &'vm GVm<ENV>) -> VMResult<&'vm str> {
        match &self {
            Value::String(h) => Ok(vm.get_string(*h)),
            Value::StringConst(i) => Ok(vm.get_interned(*i)),
            // TODO- handle chars/codepoints...
            _ => Err(VMError::new_value(format!("Not a string: {self:?}"))),
        }
    }

    pub fn get_handle(&self) -> Option<Handle> {
        match &self {
            Value::CharClusterLong(handle) => Some(*handle),
            Value::String(handle) => Some(*handle),
            Value::Vector(handle) => Some(*handle),
            Value::PersistentVec(handle) => Some(*handle),
            Value::VecNode(handle) => Some(*handle),
            Value::PersistentMap(handle) => Some(*handle),
            Value::MapNode(handle) => Some(*handle),
            Value::Map(handle) => Some(*handle),
            Value::Bytes(handle) => Some(*handle),
            Value::Pair(handle) => Some(*handle),
            Value::List(handle, _) => Some(*handle),
            Value::Lambda(handle) => Some(*handle),
            Value::Closure(handle) => Some(*handle),
            Value::Continuation(handle) => Some(*handle),
            Value::CallFrame(handle) => Some(*handle),
            Value::Value(handle) => Some(*handle),
            Value::Error(handle) => Some(*handle),

            Value::Byte(_) => None,
            Value::Int32(_) => None,
            Value::UInt32(_) => None,
            Value::Int64(_) => None,
            Value::UInt64(_) => None,
            Value::Float64(_) => None,
            Value::CodePoint(_) => None,
            Value::CharCluster(_, _) => None,
            Value::Symbol(_) => None,
            Value::Keyword(_) => None,
            Value::Special(_) => None,
            Value::StringConst(_) => None,
            Value::Builtin(_) => None,
            Value::True => None,
            Value::False => None,
            Value::Nil => None,
            Value::Undefined => None,
        }
    }

    pub fn get_pair<ENV>(&self, vm: &GVm<ENV>) -> Option<(Value, Value)> {
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

    pub fn iter<'vm, ENV>(&self, vm: &'vm GVm<ENV>) -> Box<dyn Iterator<Item = Value> + 'vm> {
        match &self.unref(vm) {
            Value::Pair(_) => Box::new(PairIter::new(vm, *self)),
            Value::List(handle, start) => {
                Box::new(vm.get_vector(*handle)[*start as usize..].iter().copied())
            }
            Value::Vector(handle) => Box::new(vm.get_vector(*handle).iter().copied()),
            Value::PersistentVec(handle) => Box::new(PersistentVecIter::new(
                vm,
                *vm.get_persistent_vector(*handle),
            )),
            _ => Box::new(iter::empty()),
        }
    }

    pub fn display_value<ENV>(&self, vm: &GVm<ENV>) -> String {
        fn list_out_iter<ENV>(
            vm: &GVm<ENV>,
            res: &mut String,
            itr: &mut dyn Iterator<Item = Value>,
        ) {
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
        fn list_out<ENV>(vm: &GVm<ENV>, res: &mut String, lst: Value) {
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
            Value::Int32(i) => format!("{i}"),
            Value::UInt32(i) => format!("{i}"),
            Value::Float64(handle) => format!("{}", vm.get_float(*handle)),
            Value::Int64(handle) => format!("{}", vm.get_int(*handle)),
            Value::UInt64(handle) => format!("{}", vm.get_uint(*handle)),
            Value::Byte(b) => format!("{b}"),
            Value::Symbol(i) => vm.get_interned(*i).to_string(),
            Value::Keyword(i) => format!(":{}", vm.get_interned(*i)),
            Value::StringConst(i) => format!("\"{}\"", vm.get_interned(*i)),
            Value::Special(i) => format!("#<SpecialFn({})>", vm.get_interned(*i)),
            Value::CodePoint(ch) => format!("\\{ch}"),
            Value::CharCluster(l, c) => {
                format!("\\{}", String::from_utf8_lossy(&c[0..*l as usize]))
            }
            Value::CharClusterLong(h) => format!("\\{}", vm.get_string(*h)),
            Value::Builtin(_) => "#<Function>".to_string(),
            Value::Nil => "nil".to_string(),
            Value::Undefined => "#<Undefined>".to_string(), //panic!("Tried to get type for undefined!"),
            Value::Lambda(_) => "#<Lambda>".to_string(),
            Value::Closure(_) => "#<Lambda>".to_string(),
            Value::Continuation(_) => "#<Continuation>".to_string(),
            Value::CallFrame(_) => "#<CallFrame>".to_string(),
            Value::Vector(handle) => {
                let v = vm.get_vector(*handle);
                let mut res = String::new();
                res.push('[');
                list_out_iter(vm, &mut res, &mut v.iter().copied());
                res.push(']');
                res
            }
            Value::PersistentVec(_) => {
                let mut res = String::new();
                res.push_str("#[");
                list_out_iter(vm, &mut res, &mut self.iter(vm));
                res.push(']');
                res
            }
            Value::VecNode(_) => {
                // TODO- implement.
                "IMPLEMENT".to_string()
            }
            Value::PersistentMap(_) => {
                // TODO- implement.
                "IMPLEMENT".to_string()
            }
            Value::MapNode(_) => {
                // TODO- implement.
                "IMPLEMENT".to_string()
            }
            Value::Map(handle) => {
                let mut res = String::new();
                res.push('{');
                for (key, val) in vm.get_map(*handle).iter() {
                    res.push_str(&format!(
                        "{} {}\n",
                        key.display_value(vm),
                        val.display_value(vm)
                    ));
                }
                res.push('}');
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
            Value::Error(handle) => {
                let err = vm.get_error(*handle);
                let key = vm.get_interned(err.keyword);
                format!("error [{key}]: {}", err.data.display_value(vm))
            }
        }
    }

    pub fn pretty_value<ENV>(&self, vm: &GVm<ENV>) -> String {
        match self {
            Value::StringConst(i) => vm.get_interned(*i).to_string(),
            Value::CodePoint(ch) => format!("{ch}"),
            Value::CharCluster(l, c) => {
                format!("{}", String::from_utf8_lossy(&c[0..*l as usize]))
            }
            Value::CharClusterLong(h) => vm.get_string(*h).to_string(),
            Value::String(handle) => vm.get_string(*handle).to_string(),
            _ => self.display_value(vm),
        }
    }

    pub fn display_type<ENV>(&self, vm: &GVm<ENV>) -> &'static str {
        match self {
            Value::True => "True",
            Value::False => "False",
            Value::Int32(_) => "Int",
            Value::UInt32(_) => "UInt",
            Value::Float64(_) => "Float",
            Value::Int64(_) => "Int",
            Value::UInt64(_) => "UInt",
            Value::Symbol(_) => "Symbol",
            Value::Keyword(_) => "Keyword",
            Value::StringConst(_) => "String",
            Value::Special(_) => "Special",
            Value::CodePoint(_) => "Char",
            Value::CharCluster(_, _) => "Char",
            Value::CharClusterLong(_) => "Char",
            Value::Builtin(_) => "Builtin",
            Value::Byte(_) => "Byte",
            Value::Nil => "Nil",
            Value::Undefined => "Undefined", //panic!("Tried to get type for undefined!"),
            Value::Lambda(_) => "Lambda",
            Value::Closure(_) => "Lambda",
            Value::Continuation(_) => "Continuation",
            Value::CallFrame(_) => "CallFrame",
            Value::Vector(_) => "Vector",
            Value::PersistentVec(_) => "PersistentVector",
            Value::VecNode(_) => "PersistentVectorNode",
            Value::PersistentMap(_) => "PersistentMap",
            Value::MapNode(_) => "PersistentMapNode",
            Value::Map(_) => "Map",
            Value::Pair(_) => "Pair",
            Value::List(_, _) => "Pair",
            Value::String(_) => "String",
            Value::Bytes(_) => "Bytes",
            Value::Value(handle) => vm.get_value(*handle).display_type(vm),
            Value::Error(_) => "Error",
        }
    }

    pub fn is_proper_list<ENV>(&self, vm: &GVm<ENV>) -> bool {
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
            props: HashMap::new(),
        }
    }

    pub fn reserve(&mut self) -> u32 {
        let index = self.objects.len();
        self.objects.push(Value::Undefined);
        index as u32
    }

    /// Sets a global to val.  The value needs have local numbers promoted to the heap before
    /// setting it.
    pub fn set(&mut self, idx: u32, val: Value) {
        self.objects[idx as usize] = val;
    }

    pub fn get(&self, idx: u32) -> Value {
        self.objects
            .get(idx as usize)
            .map_or_else(|| Value::Undefined, |v| *v)
    }

    pub fn mark(&self, heap: &mut Heap) {
        self.objects.iter().for_each(|obj| {
            heap.mark(*obj);
        });
        self.props.iter().for_each(|(_, map)| {
            for val in map.values() {
                heap.mark(*val);
            }
        });
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
