use std::cell::{Ref, RefCell, RefMut};

use crate::error::*;
use crate::heap::Object;

pub type GCHandleRef<'a> = Ref<'a, Object>;
pub type GCHandleRefMut<'a> = RefMut<'a, Object>;

struct HeapData {
    obj: RefCell<Object>,
}

#[derive(Clone, Debug)]
pub struct GCHandle {
    idx: usize,
}

pub struct GCHeap {
    objects: Vec<HeapData>,
}

impl Default for GCHeap {
    fn default() -> Self {
        Self::new()
    }
}

impl GCHeap {
    pub fn new() -> Self {
        GCHeap { objects: vec![] }
    }

    pub fn alloc(&mut self, obj: Object) -> GCHandle {
        let idx = self.objects.len();
        self.objects.push(HeapData {
            obj: RefCell::new(obj),
        });
        GCHandle { idx }
    }

    pub fn get<'a>(&'a self, handle: &GCHandle) -> VMResult<GCHandleRef<'a>> {
        if let Some(data) = self.objects.get(handle.idx) {
            Ok(data.obj.borrow())
        } else {
            Err(VMError::new_heap("Invalid object handle!"))
        }
    }

    pub fn get_mut<'a>(&'a self, handle: &GCHandle) -> VMResult<GCHandleRefMut<'a>> {
        if let Some(data) = self.objects.get(handle.idx) {
            Ok(data.obj.borrow_mut())
        } else {
            Err(VMError::new_heap("Invalid object handle!"))
        }
    }
}
