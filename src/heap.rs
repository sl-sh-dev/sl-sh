use std::borrow::Cow;
use std::cell::{Ref, RefCell, RefMut};

use crate::error::*;
use crate::value::*;

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

pub type HandleRef<'a> = Ref<'a, Object>;
pub type HandleRefMut<'a> = RefMut<'a, Object>;

struct RefData {
    idx: usize,
}

struct HeapData {
    obj: RefCell<Object>,
}

#[derive(Copy, Clone, Debug)]
pub struct Handle {
    idx: usize,
}

pub struct Heap {
    refs: Vec<RefData>,
    objects: Vec<HeapData>,
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            refs: vec![],
            objects: vec![],
        }
    }

    pub fn alloc(&mut self, obj: Object) -> Handle {
        let idx = self.objects.len();
        self.objects.push(HeapData {
            obj: RefCell::new(obj),
        });
        let ref_idx = self.refs.len();
        self.refs.push(RefData { idx });
        Handle { idx: ref_idx }
    }

    pub fn get(&self, handle: Handle) -> VMResult<HandleRef<'_>> {
        if let Some(ref_data) = self.refs.get(handle.idx) {
            if let Some(data) = self.objects.get(ref_data.idx) {
                Ok(data.obj.borrow())
            } else {
                Err(VMError::new_heap("Invalid object handle!"))
            }
        } else {
            Err(VMError::new_heap("Invalid object reference!"))
        }
    }

    pub fn get_mut(&self, handle: Handle) -> VMResult<HandleRefMut<'_>> {
        if let Some(ref_data) = self.refs.get(handle.idx) {
            if let Some(data) = self.objects.get(ref_data.idx) {
                Ok(data.obj.borrow_mut())
            } else {
                Err(VMError::new_heap("Invalid object handle!"))
            }
        } else {
            Err(VMError::new_heap("Invalid object reference!"))
        }
    }

    pub fn replace(&mut self, handle: Handle, obj: Object) -> VMResult<Object> {
        if let Some(ref_data) = self.refs.get(handle.idx) {
            if let Some(data) = self.objects.get_mut(ref_data.idx) {
                let old = data.obj.replace(obj);
                Ok(old)
            } else {
                Err(VMError::new_heap("Invalid object handle!"))
            }
        } else {
            Err(VMError::new_heap("Invalid object reference!"))
        }
    }

    pub fn normal_val(&self, handle: Handle) -> VMResult<Value> {
        if let Object::Value(val) = &*self.get(handle)? {
            Ok(*val)
        } else {
            Ok(Value::Reference(handle))
        }
    }
}
