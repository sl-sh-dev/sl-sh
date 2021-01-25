use std::borrow::Cow;

use crate::error::*;
use crate::value::*;

//const FLAG_MARK: u8 = 0x01;
const FLAG_TRACE: u8 = 0x02;
const FLAG_NEW: u8 = 0x04;
//const FLAG_8: u8 = 0x08;

const TYPE_VALUE: u8 = 0x00;
const TYPE_STRING: u8 = 0x10;
const TYPE_VECTOR: u8 = 0x20;
const TYPE_BYTES: u8 = 0x30;
const TYPE_PAIR: u8 = 0x40;

// This is anything that can live on the heap.  Values normally live on the
// stack or as constants but can be stored in the heap as well
// (for instance closed over values or globals).
#[derive(Clone, Debug)]
pub enum Object {
    Value(Value),
    String(Cow<'static, str>),
    Vector(Vec<Value>),
    Bytes(Vec<u8>),
    Pair(Value, Value),
}

impl Object {
    pub fn is_nil(&self) -> bool {
        matches!(self, Object::Value(Value::Nil))
    }
}

pub type HandleRef<'a> = &'a Object;
pub type HandleRefMut<'a> = &'a mut Object;

#[derive(Copy, Clone, Debug)]
pub struct Handle {
    idx: usize,
}

type MarkRootFunc = fn(&mut Heap) -> VMResult<()>;

pub struct Heap {
    flags: Vec<u8>,
    objects: Vec<Object>,
    mark_roots: Option<MarkRootFunc>,
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            flags: vec![],
            objects: vec![],
            mark_roots: None,
        }
    }

    pub fn set_marker(&mut self, mark_roots: MarkRootFunc) {
        self.mark_roots = Some(mark_roots);
    }

    fn type_flag(obj: &Object) -> u8 {
        match obj {
            Object::Value(Value::Reference(_)) => TYPE_VALUE | FLAG_TRACE,
            Object::Value(_) => TYPE_VALUE,
            Object::String(_) => TYPE_STRING,
            Object::Vector(_) => TYPE_VECTOR | FLAG_TRACE,
            Object::Bytes(_) => TYPE_BYTES,
            Object::Pair(_, _) => TYPE_PAIR | FLAG_TRACE,
        }
    }

    pub fn alloc(&mut self, obj: Object) -> Handle {
        let type_flag = Self::type_flag(&obj);
        let idx = self.objects.len();
        self.objects.push(obj);
        self.flags.push(type_flag | FLAG_NEW);
        Handle { idx }
    }

    pub fn is_value(&self, handle: Handle) -> bool {
        if let Some(flag) = self.flags.get(handle.idx) {
            (flag & 0xf0) == TYPE_VALUE
        } else {
            false
        }
    }

    pub fn is_string(&self, handle: Handle) -> bool {
        if let Some(flag) = self.flags.get(handle.idx) {
            (flag & 0xf0) == TYPE_STRING
        } else {
            false
        }
    }

    pub fn is_vector(&self, handle: Handle) -> bool {
        if let Some(flag) = self.flags.get(handle.idx) {
            (flag & 0xf0) == TYPE_VECTOR
        } else {
            false
        }
    }

    pub fn is_bytes(&self, handle: Handle) -> bool {
        if let Some(flag) = self.flags.get(handle.idx) {
            (flag & 0xf0) == TYPE_BYTES
        } else {
            false
        }
    }

    pub fn is_pair(&self, handle: Handle) -> bool {
        if let Some(flag) = self.flags.get(handle.idx) {
            (flag & 0xf0) == TYPE_PAIR
        } else {
            false
        }
    }

    pub fn get(&self, handle: Handle) -> VMResult<HandleRef<'_>> {
        if let Some(data) = self.objects.get(handle.idx) {
            Ok(data)
        } else {
            Err(VMError::new_heap("Invalid object handle!"))
        }
    }

    pub fn get_mut(&mut self, handle: Handle) -> VMResult<HandleRefMut<'_>> {
        if let Some(data) = self.objects.get_mut(handle.idx) {
            Ok(data)
        } else {
            Err(VMError::new_heap("Invalid object handle!"))
        }
    }

    pub fn replace(&mut self, handle: Handle, obj: Object) -> VMResult<Object> {
        let type_flag = Self::type_flag(&obj);
        self.objects.push(obj);
        let old = self.objects.swap_remove(handle.idx);
        self.flags[handle.idx] = type_flag | (self.flags[handle.idx] & 0x0f);
        Ok(old)
    }

    pub fn get_value(&self, handle: Handle) -> VMResult<Value> {
        if let Object::Value(val) = self.get(handle)? {
            Ok(*val)
        } else {
            Ok(Value::Reference(handle))
        }
    }
}
