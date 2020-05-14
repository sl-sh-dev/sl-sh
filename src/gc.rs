///
/// A stub for a garbage collector.
///
use crate::types::*;

use std::{
    cell::{Ref, RefCell, RefMut},
    ops::{Deref, DerefMut},
    rc::Rc,
};

static mut STATIC_GC: Option<Rc<RefCell<Heap>>> = None;

pub struct GcWrapRef<'a> {
    read: Ref<'a, Heap>,
}

impl<'a> Deref for GcWrapRef<'a> {
    type Target = Heap;

    fn deref(&self) -> &Self::Target {
        &*self.read
    }
}

pub struct GcWrapRefMut<'a> {
    write: RefMut<'a, Heap>,
}

impl<'a> Deref for GcWrapRefMut<'a> {
    type Target = Heap;

    fn deref(&self) -> &Self::Target {
        &*self.write
    }
}

impl<'a> DerefMut for GcWrapRefMut<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.write
    }
}

pub fn init_gc() {
    unsafe {
        if STATIC_GC.is_none() {
            STATIC_GC = Some(Rc::new(RefCell::new(Heap::new())));
        }
    }
}

pub fn get_gc() -> Rc<RefCell<Heap>> {
    unsafe { STATIC_GC.as_ref().unwrap().clone() }
}

pub fn gc<'a>() -> GcWrapRef<'a> {
    GcWrapRef {
        read: unsafe { STATIC_GC.as_ref().unwrap().borrow() },
    }
}

pub fn gc_mut<'a>() -> GcWrapRefMut<'a> {
    GcWrapRefMut {
        write: unsafe { STATIC_GC.as_ref().unwrap().borrow_mut() },
    }
}

/// A heap for storing objects.
///
/// [`Heap`] is the centre of `broom`'s universe. It's the singleton through with manipulation of
/// objects occurs. It can be used to create, access, mutate and garbage-collect objects.
///
/// Note that heaps, and the objects associated with them, are *not* compatible: this means that
/// you may not create trace routes (see [`Trace`]) that cross the boundary between different heaps.
pub struct Heap {}

impl Heap {
    /// Create an empty heap.
    fn new() -> Self {
        Self {}
    }

    pub fn insert(&mut self, object: ExpObj) -> Handle {
        Handle::new(object)
    }

    pub fn down_root(&mut self, _handle: impl AsRef<Handle>) {}

    pub fn prune_nursery(&mut self) -> Vec<ExpObj> {
        Vec::new()
    }

    /// Clean orphaned objects from the heap.
    pub fn clean(&mut self) {}
}

/// A handle to a heap object.
///
/// [`Handle`] may be cheaply copied as is necessary to serve your needs. It's even legal for it
/// to outlive the object it refers to, provided it is no longer used to access it afterwards.
#[derive(Debug)]
pub struct Handle {
    data: Rc<RefCell<ExpObj>>,
}

impl Handle {
    fn new(data: ExpObj) -> Handle {
        Handle {
            data: Rc::new(RefCell::new(data)),
        }
    }

    /// Get a reference to a heap object if it exists on this heap.
    pub fn get(&self) -> Ref<ExpObj> {
        self.data.borrow()
    }

    /// Get a mutable reference to a heap object
    pub fn get_mut(&self) -> RefMut<ExpObj> {
        self.data.borrow_mut()
    }

    pub fn clone_root(&self) -> Handle {
        Handle {
            data: self.data.clone(),
        }
    }

    pub fn clone_no_root(&self) -> Handle {
        Handle {
            data: self.data.clone(),
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

pub trait Trace {
    /// Trace *all* child objects of this type.
    ///
    /// Note that although failing to trace all children is not undefined behaviour on its own, it
    /// will mean that objects may be accidentally garbage-collected, and hence that the
    /// `_unchecked` methods in this crate will produce undefined behaviour when used to access
    /// those objects.
    ///
    /// In addition, you must ensure that this function does not result in the tracing of objects
    /// associated with other heaps: to do so is undefined behaviour.
    fn trace(&self, tracer: &mut Tracer);
}

/// A type used to perform a heap trace. Largely an implementation detail: To implement heap
/// tracing, look at the [`Trace`] trait instead.
pub struct Tracer {}

impl Trace for Handle {
    fn trace(&self, _tracer: &mut Tracer) {}
}
