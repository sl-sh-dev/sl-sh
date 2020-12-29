use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;

use crate::error::*;
use crate::heap::Object;

pub type RCHandleRef<'a> = Ref<'a, Object>;
pub type RCHandleRefMut<'a> = RefMut<'a, Object>;

#[derive(Clone, Debug)]
pub struct RCHandle {
    object: Rc<RefCell<Object>>,
}

impl RCHandle {
    fn borrow(&self) -> RCHandleRef {
        self.object.borrow()
    }

    fn borrow_mut(&self) -> RCHandleRefMut {
        self.object.borrow_mut()
    }
}

pub struct RCHeap {}

impl Default for RCHeap {
    fn default() -> Self {
        Self::new()
    }
}

impl RCHeap {
    pub fn new() -> Self {
        RCHeap {}
    }

    pub fn alloc(&mut self, obj: Object) -> RCHandle {
        RCHandle {
            object: Rc::new(RefCell::new(obj)),
        }
    }

    pub fn get<'a>(&self, handle: &'a RCHandle) -> VMResult<RCHandleRef<'a>> {
        Ok(handle.borrow())
    }

    pub fn get_mut<'a>(&self, handle: &'a RCHandle) -> VMResult<RCHandleRefMut<'a>> {
        Ok(handle.borrow_mut())
    }

    pub fn replace(&mut self, handle: &RCHandle, obj: Object) -> VMResult<Object> {
        let old = handle.object.replace(obj);
        Ok(old)
    }
}
