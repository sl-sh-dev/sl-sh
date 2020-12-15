///
/// A stub for a garbage collector.
/// Not really a stub anymore, just a RC wrapper around an object.
///
use crate::types::*;

use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

/// A handle to an object.
#[derive(Debug)]
pub struct Handle {
    data: Rc<RefCell<ExpObj>>,
}

impl Handle {
    pub fn new(data: ExpObj) -> Handle {
        Handle {
            data: Rc::new(RefCell::new(data)),
        }
    }

    /// Get a reference to a heap object if it exists on this heap.
    pub fn get(&self) -> Ref<ExpObj> {
        self.data.borrow()
    }

    /// Will a get succeed on this object?
    pub fn can_get(&self) -> bool {
        self.data.try_borrow().is_ok()
    }

    /// Get a mutable reference to a heap object
    pub fn get_mut(&self) -> RefMut<ExpObj> {
        self.data.borrow_mut()
    }

    pub fn try_unwrap(self) -> Result<ExpObj, Handle> {
        match Rc::try_unwrap(self.data) {
            Ok(data) => Ok(data.into_inner()),
            Err(handle) => Err(Handle { data: handle }),
        }
    }
}

impl Clone for Handle {
    fn clone(&self) -> Handle {
        Handle {
            data: self.data.clone(),
        }
    }
}

impl AsRef<Handle> for Handle {
    fn as_ref(&self) -> &Handle {
        self
    }
}
