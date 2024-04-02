use crate::{float, Handle, Heap, Interned, VMError, VMResult};
use bridge_types::BridgedType;
use std::collections::{BTreeSet, HashMap};
use std::fmt;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::iter;
use std::ops::Deref;
use std::sync::Arc;

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

pub const INT_BITS: u8 = 56;
pub const INT_MAX: i64 = 2_i64.pow(INT_BITS as u32 - 1) - 1;
pub const INT_MIN: i64 = -(2_i64.pow(INT_BITS as u32 - 1));

pub fn from_i56(arr: &[u8; 7]) -> i64 {
    let mut bytes = [0x00, arr[0], arr[1], arr[2], arr[3], arr[4], arr[5], arr[6]];
    if (arr[0] & 0x80) > 0 {
        bytes[0] = 0xff;
        i64::from_be_bytes(bytes)
    } else {
        i64::from_be_bytes(bytes)
    }
}

pub fn to_i56(i: i64) -> Value {
    let bytes = i.to_be_bytes();
    let bytes7 = [
        bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
    ];
    Value::Int(bytes7)
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Value {
    Byte(u8),
    Int([u8; 7]),      // Store a 7 byte int (i56...).
    Float(float::F56), // Replace with float::F32Wrap if desired
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

impl From<f32> for Value {
    fn from(value: f32) -> Self {
        Self::Float(value.into())
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Float(value.into())
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        to_i56(value)
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        to_i56(value as i64)
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        to_i56(value as i64)
    }
}

impl BridgedType for Value {}
impl BridgedType for &Value {}
impl BridgedType for &mut Value {}

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
        matches!(&self, Value::Byte(_) | Value::Int(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(&self, Value::Byte(_) | Value::Int(_) | Value::Float(_))
    }

    pub fn get_int<ENV>(&self, _vm: &GVm<ENV>) -> VMResult<i64> {
        match &self {
            Value::Byte(b) => Ok(*b as i64),
            Value::Int(i) => Ok(from_i56(i)),
            _ => Err(VMError::new_value(format!("Not an integer: {self:?}"))),
        }
    }

    pub fn get_float<ENV>(&self, _vm: &GVm<ENV>) -> VMResult<f64> {
        match &self {
            Value::Byte(b) => Ok(*b as f64),
            Value::Int(i) => Ok(from_i56(i) as f64),
            Value::Float(f) => Ok(f64::from(*f)),
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
            Value::Int(_) => None,
            Value::Float(_) => None,
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

    /// Returns an iterator over all values in the list, vector, pair.
    /// If the item is not one of the above it returns an empty iterator.
    pub fn iter<'vm, ENV>(&self, vm: &'vm GVm<ENV>) -> Box<dyn Iterator<Item = Value> + 'vm> {
        match &self.unref(vm) {
            Value::Pair(_) => Box::new(PairIter::new(vm, *self)),
            Value::List(handle, start) => {
                Box::new(vm.get_vector(*handle)[*start as usize..].iter().copied())
            }
            Value::Vector(handle) => Box::new(vm.get_vector(*handle).iter().copied()),
            _ => Box::new(iter::empty()),
        }
    }

    /// Returns an iterator over all values in the list, vector, pair.
    /// Returns an empty iterator if the value is nil.
    /// If the item is not one of the above it returns a once iter of the value.
    ///
    /// Particularly useful  if iterating over a Vec<Value> that contains a heterogeneous
    /// mix of List(s), Vector(s), Pair(s), and potentially other values, rather than
    /// returning an empty list for any non iterable thing, return a once iter so
    /// the item is not missed.
    pub fn iter_all<'vm, ENV>(&self, vm: &'vm GVm<ENV>) -> Box<dyn Iterator<Item = Value> + 'vm> {
        match &self.unref(vm) {
            Value::Pair(_) => Box::new(PairIter::new(vm, *self)),
            Value::List(handle, start) => {
                Box::new(vm.get_vector(*handle)[*start as usize..].iter().copied())
            }
            Value::Vector(handle) => Box::new(vm.get_vector(*handle).iter().copied()),
            Value::Nil => Box::new(iter::empty()),
            v => Box::new(iter::once(*v)),
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
            Value::Int(i) => format!("{}", from_i56(i)),
            Value::Float(f) => format!("{}", f),
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

    /// Map a [`Value`] to a [`ValueType`] which can be written to a debug string that refers to the
    /// Slosh types and does not require passing in a [`GVm`] to do so.
    pub fn value_type<ENV>(&self, vm: &GVm<ENV>) -> ValueType {
        match self {
            Value::Byte(_) => ValueType::Byte,
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::CodePoint(_) => ValueType::CodePoint,
            Value::CharCluster(_, _) => ValueType::CharCluster,
            Value::CharClusterLong(_) => ValueType::CharClusterLong,
            Value::Symbol(_) => ValueType::Symbol,
            Value::Keyword(_) => ValueType::Keyword,
            Value::StringConst(_) => ValueType::StringConst,
            Value::Special(_) => ValueType::Special,
            Value::Builtin(_) => ValueType::Builtin,
            Value::True => ValueType::True,
            Value::False => ValueType::False,
            Value::Nil => ValueType::Nil,
            Value::Undefined => ValueType::Undefined,
            Value::String(_) => ValueType::String,
            Value::Vector(_) => ValueType::Vector,
            Value::Map(_) => ValueType::Map,
            Value::Bytes(_) => ValueType::Bytes,
            Value::Pair(_) => ValueType::Pair,
            Value::List(_, _) => ValueType::List,
            Value::Lambda(_) => ValueType::Lambda,
            Value::Closure(_) => ValueType::Closure,
            Value::Continuation(_) => ValueType::Continuation,
            Value::CallFrame(_) => ValueType::CallFrame,
            Value::Error(_) => ValueType::Error,
            Value::Value(handle) => vm.get_value(*handle).value_type(vm),
        }
    }

    pub fn display_type<ENV>(&self, vm: &GVm<ENV>) -> &'static str {
        self.value_type(vm).into()
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

pub const SLOSH_CHAR: &str = "Char";
pub const SLOSH_STRING: &str = "String";
pub const SLOSH_INT: &str = "Int";
pub const SLOSH_FLOAT: &str = "Float";
pub const SLOSH_BOOL_TRUE: &str = "True";
pub const SLOSH_BOOL_FALSE: &str = "False";
pub const SLOSH_SYMBOL: &str = "Symbol";
pub const SLOSH_KEYWORD: &str = "Keyword";
pub const SLOSH_SPECIAL: &str = "Special";
pub const SLOSH_BUILTIN: &str = "Builtin";
pub const SLOSH_BYTE: &str = "Byte";
pub const SLOSH_BYTES: &str = "Bytes";
pub const SLOSH_NIL: &str = "Nil";
pub const SLOSH_UNDEFINED: &str = "Undefined";
pub const SLOSH_LAMBDA: &str = "Lambda";
pub const SLOSH_CLOSURE: &str = "Lambda";
pub const SLOSH_CONTINUATION: &str = "Continuation";
pub const SLOSH_CALLFRAME: &str = "CallFrame";
pub const SLOSH_VECTOR: &str = "Vector";
pub const SLOSH_MAP: &str = "Map";
pub const SLOSH_PAIR: &str = "Pair";
pub const SLOSH_ERROR: &str = "Error";

/// Enum representing the various types of values in Slosh.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum ValueType {
    Byte,
    Int,
    Float,
    CodePoint,
    CharCluster,
    CharClusterLong,
    Symbol,
    Keyword,
    StringConst,
    Special,
    Builtin,
    True,
    False,
    Nil,
    Undefined,
    String,
    Vector,
    Map,
    Bytes,
    Pair,
    List,
    Lambda,
    Closure,
    Continuation,
    CallFrame,
    Error,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", <ValueType as Into<&'static str>>::into(*self))
    }
}

impl From<ValueType> for &'static str {
    fn from(value: ValueType) -> Self {
        match value {
            ValueType::True => SLOSH_BOOL_TRUE,
            ValueType::False => SLOSH_BOOL_FALSE,
            ValueType::Int => SLOSH_INT,
            ValueType::Float => SLOSH_FLOAT,
            ValueType::Symbol => SLOSH_SYMBOL,
            ValueType::Keyword => SLOSH_KEYWORD,
            ValueType::StringConst => SLOSH_STRING,
            ValueType::Special => SLOSH_SPECIAL,
            ValueType::CodePoint => SLOSH_CHAR,
            ValueType::CharCluster => SLOSH_CHAR,
            ValueType::CharClusterLong => SLOSH_CHAR,
            ValueType::Builtin => SLOSH_BUILTIN,
            ValueType::Byte => SLOSH_BYTE,
            ValueType::Bytes => SLOSH_BYTES,
            ValueType::Nil => SLOSH_NIL,
            ValueType::Undefined => SLOSH_UNDEFINED,
            ValueType::Lambda => SLOSH_LAMBDA,
            ValueType::Closure => SLOSH_LAMBDA,
            ValueType::Continuation => SLOSH_CONTINUATION,
            ValueType::CallFrame => SLOSH_CALLFRAME,
            ValueType::Vector => SLOSH_VECTOR,
            ValueType::Map => SLOSH_MAP,
            ValueType::Pair => SLOSH_PAIR,
            ValueType::List => SLOSH_PAIR,
            ValueType::String => SLOSH_STRING,
            ValueType::Error => SLOSH_ERROR,
        }
    }
}

pub struct ValueTypes<const N: usize> {
    values: [ValueType; N],
}

impl<const N: usize> From<[ValueType; N]> for ValueTypes<N> {
    fn from(values: [ValueType; N]) -> Self {
        Self { values }
    }
}

impl<const N: usize> Deref for ValueTypes<N> {
    type Target = [ValueType];

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

impl<const N: usize> From<ValueTypes<N>> for String {
    fn from(value: ValueTypes<N>) -> Self {
        let mut res = String::new();
        let set: BTreeSet<&str> = BTreeSet::from_iter(
            value
                .deref()
                .iter()
                .map(|v| <ValueType as Into<&'static str>>::into(*v)),
        );
        for (i, v) in set.iter().enumerate() {
            if i > 0 {
                res.push_str(", ");
            }
            res.push_str(v);
        }
        res
    }
}
