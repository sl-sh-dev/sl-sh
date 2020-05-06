//! # Broom
//!
//! An ergonomic tracing garbage collector that supports mark 'n sweep garbage collection.
//!
//! ## Example
//!
//! ```
//! use broom::prelude::*;
//!
//! // The type you want the heap to contain
//! pub enum GcRefect {
//!     Num(f64),
//!     List(Vec<Handle<Self>>),
//! }
//!
//! // Tell the garbage collector how to explore a graph of this object
//! impl Trace<Self> for GcRefect {
//!     fn trace(&self, tracer: &mut Tracer<Self>) {
//!         match self {
//!             GcRefect::Num(_) => {},
//!             GcRefect::List(objects) => objects.trace(tracer),
//!         }
//!     }
//! }
//!
//! // Create a new heap
//! //let mut heap = Heap::default();
//! let gc = create_gc::<GcRefect>();
//! let mut heap = gc.get_mut();
//!
//! // Temporary objects are cheaper than rooted objects, but don't survive heap cleans
//! let a = heap.insert_temp(GcRefect::Num(42.0));
//! let b = heap.insert_temp(GcRefect::Num(1337.0));
//!
//! // Turn the numbers into a rooted list
//! let c = heap.insert(GcRefect::List(vec![a.clone(), b.clone()]));
//!
//! // Change one of the numbers - this is safe, even if the object is self-referential!
//! *heap.get_mut(&a).unwrap() = GcRefect::Num(256.0);
//!
//! // Create another number object
//! let d = heap.insert_temp(GcRefect::Num(0.0));
//!
//! // Clean up unused heap objects
//! heap.clean();
//!
//! // a, b and c are all kept alive because c is rooted and a and b are its children
//! assert!(heap.contains(a));
//! assert!(heap.contains(b));
//! assert!(heap.contains(c));
//!
//! // Because `d` was temporary and unused, it did not survive the heap clean
//! assert!(!heap.contains(d));
//!
//! ```

use crate::trace::*;
use crate::types::*;

//use hashbrown::{HashMap, HashSet};
use std::collections::HashMap;
use std::{
    //cell::{Ref, RefCell, RefMut},
    cell::{Ref, RefMut},
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
    //rc::Rc,
    sync::atomic::{AtomicU32, Ordering},
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

/// Common items that you'll probably need often.
/*pub mod prelude {
    pub use super::{
        create_gc,
        {Trace, Tracer},
        GcRef, GcRefMut, GcWrap, GcWrapRef, GcWrapRefMut, Handle, Heap,
    };
}*/

pub(crate) type HandleIdx = u32;
pub(crate) type HandleIdxAtomic = AtomicU32;

static mut STATIC_GC: Option<RwLock<Heap>> = None;

pub fn init_gc() {
    unsafe {
        if STATIC_GC.is_none() {
            STATIC_GC = Some(RwLock::new(Heap::new()));
        }
    }
}

pub fn gc<'a>() -> &'a Heap {
    unsafe { &*(&*STATIC_GC.as_ref().unwrap().read().unwrap() as *const Heap) }
}

pub fn gc_mut<'a>() -> &'a mut Heap {
    unsafe { &mut *(&mut *STATIC_GC.as_ref().unwrap().write().unwrap() as *mut Heap) }
}

struct Nursery {
    objects: Vec<Option<Arc<RwLock<ExpObj>>>>,
    objects_handle: Vec<HandleIdx>,
    roots: HashMap<HandleIdx, HandleIdxAtomic>,
    excludes: Vec<Handle>,
}

impl Nursery {
    fn new() -> Nursery {
        Nursery {
            objects: Vec::new(),
            objects_handle: Vec::new(),
            roots: HashMap::default(),
            excludes: Vec::new(),
        }
    }

    fn clear(&mut self) {
        self.objects.clear();
        self.objects_handle.clear();
        self.roots.clear();
        self.excludes.clear();
    }
}

/// A heap for storing objects.
///
/// [`Heap`] is the centre of `broom`'s universe. It's the singleton through with manipulation of
/// objects occurs. It can be used to create, access, mutate and garbage-collect objects.
///
/// Note that heaps, and the objects associated with them, are *not* compatible: this means that
/// you may not create trace routes (see [`Trace`]) that cross the boundary between different heaps.
pub struct Heap {
    last_sweep: usize,
    object_sweeps: HashMap<usize, usize>,
    objects: Vec<Option<Arc<RwLock<ExpObj>>>>,
    objects_handle: Vec<HandleIdx>,
    freed: Vec<usize>,
    //rooted: HashMap<Handle, AtomicU32>,
    roots: HashMap<HandleIdx, HandleIdxAtomic>,
    refs: HashMap<HandleIdx, HandleRef>,
    next_handle: HandleIdxAtomic,

    nursery: Nursery,
}

impl Heap {
    /// Create an empty heap.
    fn new() -> Self {
        Self {
            last_sweep: 0,
            object_sweeps: HashMap::default(),
            objects: Vec::default(),
            objects_handle: Vec::default(),
            freed: Vec::default(),
            roots: HashMap::default(),
            refs: HashMap::default(),
            next_handle: HandleIdxAtomic::new(0),
            nursery: Nursery::new(),
        }
    }

    pub fn print_stats(&self) {
        println!("last_sweep: {}, obj sweeps: {}, objects: {}, object handles: {}, freed len: {}, rooted: {}, refs: {}, next handle: {}",
                 self.last_sweep, self.object_sweeps.len(), self.objects.len(), self.objects_handle.len(), self.freed.len(), self.roots.len(), self.refs.len(), self.next_handle.load(Ordering::Relaxed));
    }
    pub fn objects(&self) -> usize {
        self.objects.len()
    }
    pub fn free_objects(&self) -> usize {
        self.freed.len()
    }
    pub fn used_objects(&self) -> usize {
        self.objects() - self.free_objects()
    }
    pub fn nursery_objects(&self) -> usize {
        self.nursery.objects.len()
    }

    fn update_handle(&mut self, handle: impl AsRef<Handle>, object: Arc<RwLock<ExpObj>>) -> Handle {
        let handle = handle.as_ref();
        let idx = handle.idx;
        //let refs = if let Some(rc) = self.roots.remove(&idx) {
        if let Some(rc) = self.roots.remove(&idx) {
            self.nursery.roots.insert(idx, rc);
            /*  self.nursery
                    .roots
                    .entry(idx)
                    .or_insert_with(|| HandleIdxAtomic::new(0))
            } else {
                self.nursery
                    .roots
                    .entry(idx)
                    .or_insert_with(|| HandleIdxAtomic::new(0))*/
        }
        //refs.fetch_add(1, Ordering::Relaxed);
        //let handle = Handle { root: true, idx };
        let handle = Handle { root: false, idx };
        self.nursery.objects.push(Some(object));
        self.nursery.objects_handle.push(handle.idx);
        let href = HandleRef {
            gen: RefGen::Nursery,
            handle: handle.clone(),
            idx: self.nursery.objects.len() - 1,
        };
        self.refs.insert(handle.idx, href);
        handle
    }

    fn insert_int(&mut self, object: Arc<RwLock<ExpObj>>) -> Handle {
        let idx = self.next_handle.fetch_add(1, Ordering::Relaxed);
        let refs = self
            .nursery
            .roots
            .entry(idx)
            .or_insert_with(|| HandleIdxAtomic::new(0));
        refs.fetch_add(1, Ordering::Relaxed);
        let handle = Handle { root: true, idx };
        self.nursery.objects.push(Some(object));
        self.nursery.objects_handle.push(handle.idx);
        let href = HandleRef {
            gen: RefGen::Nursery,
            handle: handle.clone(),
            idx: self.nursery.objects.len() - 1,
        };
        self.refs.insert(handle.idx, href);
        handle
    }

    pub fn insert(&mut self, object: ExpObj) -> Handle {
        self.insert_int(Arc::new(RwLock::new(object)))
    }

    pub fn down_root(&mut self, handle: impl AsRef<Handle>) {
        let handle = handle.as_ref();

        let mut object = None;
        if let Some(href) = self.refs.get(&handle.idx) {
            if let RefGen::GenX = href.gen {
                let obj = self.objects.get_mut(href.idx);
                if let Some(obj) = obj {
                    if let Some(obj) = obj {
                        object = Some(obj.clone());
                    }
                    if obj.is_some() {
                        self.freed.push(href.idx);
                        *obj = None;
                    }
                }
            }
        }
        if let Some(obj) = object {
            let handle = self.update_handle(handle, obj.clone());
            self.nursery.excludes.push(handle);
        }
    }

    /// Count the number of heap-allocated objects in this heap
    pub fn len(&self) -> usize {
        self.objects.len()
    }

    pub fn is_empty(&self) -> bool {
        self.objects.is_empty()
    }

    /// Return true if the heap contains the specified handle
    pub fn contains(&self, handle: impl AsRef<Handle>) -> bool {
        let handle = handle.as_ref();
        if let Some(href) = self.refs.get(&handle.idx) {
            let objects = match href.gen {
                RefGen::Nursery => &self.nursery.objects,
                RefGen::GenX => &self.objects,
            };
            objects.get(href.idx).expect("Invalid handle!").is_some()
        } else {
            false
        }
    }

    /// Get a reference to a heap object if it exists on this heap.
    pub fn get(&self, handle: impl AsRef<Handle>) -> Option<GcRef> {
        let handle = handle.as_ref();
        if let Some(href) = self.refs.get(&handle.idx) {
            let objects = match href.gen {
                RefGen::Nursery => &self.nursery.objects,
                RefGen::GenX => &self.objects,
            };
            if let Some(obj) = objects.get(href.idx) {
                if let Some(obj) = obj {
                    return Some(GcRef::new_arc(
                        self,
                        handle.clone_root(),
                        obj.read().unwrap(),
                    ));
                }
            }
        }
        None
    }

    /// Get a mutable reference to a heap object
    pub fn get_mut(&self, handle: impl AsRef<Handle>) -> Option<GcRefMut> {
        let handle = handle.as_ref();
        if let Some(href) = self.refs.get(&handle.idx) {
            let objects = match href.gen {
                RefGen::Nursery => &self.nursery.objects,
                RefGen::GenX => &self.objects,
            };
            if let Some(obj) = objects.get(href.idx) {
                if let Some(obj) = obj {
                    return Some(GcRefMut::new_arc(
                        self,
                        handle.clone_root(),
                        obj.write().unwrap(),
                    ));
                }
            }
        }
        None
    }

    pub fn prune_nursery(&mut self) {
        let mut new_nursery = Nursery::new();
        let new_sweep = 1;
        let mut object_sweeps: HashMap<usize, usize> = HashMap::default();
        {
            let mut tracer = Tracer {
                new_sweep,
                object_sweeps: &mut object_sweeps,
                objects: &self.nursery.objects,
                refs: &self.refs,
                ref_gen: RefGen::Nursery,
            };

            // Mark
            self.nursery.roots.retain(|handle_idx, rc| {
                if rc.load(Ordering::Relaxed) > 0 {
                    tracer.mark_handle(*handle_idx);
                    true
                } else {
                    false
                }
            });
            for handle in &self.nursery.excludes {
                tracer.mark_handle(handle.idx);
            }
            // drop the tracer now to avoid deadlock
        }

        for (i, obj) in self.nursery.objects.drain(..).enumerate() {
            if object_sweeps
                .get(&i)
                .map(|sweep| *sweep == new_sweep)
                .unwrap_or(false)
            {
                let handle_idx = self.nursery.objects_handle[i];
                new_nursery.objects.push(obj);
                new_nursery.objects_handle.push(handle_idx);
                let href = HandleRef {
                    gen: RefGen::Nursery,
                    handle: Handle::new(handle_idx),
                    idx: new_nursery.objects.len() - 1,
                };
                self.refs.insert(handle_idx, href);
            } else {
                let handle_idx = self.nursery.objects_handle[i];
                self.refs.remove(&handle_idx);
            }
        }
        for (idx, rc) in self.nursery.roots.drain() {
            new_nursery.roots.insert(idx, rc);
        }
        self.nursery = new_nursery;
    }

    fn promote_nursery(&mut self) {
        let new_sweep = 1;
        let mut object_sweeps: HashMap<usize, usize> = HashMap::default();
        {
            let mut tracer = Tracer {
                new_sweep,
                object_sweeps: &mut object_sweeps,
                objects: &self.nursery.objects,
                refs: &self.refs,
                ref_gen: RefGen::Nursery,
            };

            // Mark
            self.nursery.roots.retain(|idx, rc| {
                if rc.load(Ordering::Relaxed) > 0 {
                    tracer.mark_handle(*idx);
                    true
                } else {
                    false
                }
            });
            for handle in &self.nursery.excludes {
                tracer.mark_handle(handle.idx);
            }
            // drop the tracer now to avoid deadlock
        }

        // Promote referenced objects to the heap.
        for (i, obj) in self.nursery.objects.iter().enumerate() {
            if object_sweeps
                .get(&i)
                .map(|sweep| *sweep == new_sweep)
                .unwrap_or(false)
            {
                if let Some(obj) = obj {
                    let handle_idx = self.nursery.objects_handle[i];

                    let idx = if let Some(idx) = self.freed.pop() {
                        self.objects.push(Some(obj.clone()));
                        self.objects.swap_remove(idx);
                        self.objects_handle.push(handle_idx);
                        self.objects_handle.swap_remove(idx);
                        idx
                    } else {
                        self.objects.push(Some(obj.clone()));
                        self.objects_handle.push(handle_idx);
                        self.objects.len() - 1
                    };
                    let href = HandleRef {
                        gen: RefGen::GenX,
                        handle: Handle::new(handle_idx),
                        idx,
                    };
                    self.refs.insert(handle_idx, href);
                }
            } else {
                let handle_idx = self.nursery.objects_handle[i];
                self.refs.remove(&handle_idx);
            }
        }
        for (idx, rc) in self.nursery.roots.drain() {
            self.roots.insert(idx, rc);
        }
        self.nursery.clear();
    }

    /// Clean orphaned objects from the heap.
    pub fn clean(&mut self) {
        self.promote_nursery();
        let new_sweep = self.last_sweep + 1;
        {
            let mut tracer = Tracer {
                new_sweep,
                object_sweeps: &mut self.object_sweeps,
                objects: &self.objects,
                refs: &self.refs,
                ref_gen: RefGen::GenX,
            };

            // Mark
            self.roots.retain(|handle_idx, rc| {
                if rc.load(Ordering::Relaxed) > 0 {
                    tracer.mark_handle(*handle_idx);
                    true
                } else {
                    false
                }
            });
            // drop the tracer now to avoid deadlock
        }

        // Sweep
        for (i, obj) in self.objects.iter_mut().enumerate() {
            if !self
                .object_sweeps
                .get(&i)
                .map(|sweep| *sweep == new_sweep)
                .unwrap_or(false)
            {
                self.object_sweeps.remove(&i);
                if obj.is_some() {
                    self.freed.push(i);
                    *obj = None;
                    self.refs.remove(
                        self.objects_handle
                            .get(i)
                            .expect("GcRefect to handle mapping broken!"),
                    );
                }
            }
        }

        self.last_sweep = new_sweep;
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum RefGen {
    Nursery,
    GenX,
}

pub struct HandleRef {
    gen: RefGen,
    handle: Handle,
    idx: usize,
}

impl HandleRef {
    pub fn handle(&self) -> Handle {
        self.handle.clone()
    }

    pub fn index(&self) -> usize {
        self.idx
    }

    pub fn gen(&self) -> RefGen {
        self.gen
    }
}

impl Clone for HandleRef {
    fn clone(&self) -> HandleRef {
        HandleRef {
            gen: self.gen,
            handle: self.handle.clone(),
            idx: self.idx,
        }
    }
}

/// A handle to a heap object.
///
/// [`Handle`] may be cheaply copied as is necessary to serve your needs. It's even legal for it
/// to outlive the object it refers to, provided it is no longer used to access it afterwards.
#[derive(Debug)]
pub struct Handle {
    root: bool,
    idx: HandleIdx,
}

impl Handle {
    fn new(idx: HandleIdx) -> Handle {
        Handle { root: false, idx }
        //Handle::new_root(idx)
    }

    fn new_root(idx: HandleIdx) -> Handle {
        let refs = if gc().roots.contains_key(&idx) {
            gc_mut()
                .roots
                .entry(idx)
                .or_insert_with(|| HandleIdxAtomic::new(0))
        } else {
            gc_mut()
                .nursery
                .roots
                .entry(idx)
                .or_insert_with(|| HandleIdxAtomic::new(0))
        };
        refs.fetch_add(1, Ordering::Relaxed);
        Handle { root: true, idx }
    }

    /// Get a reference to a heap object if it exists on this heap.
    pub fn get(&self) -> Option<GcRef> {
        gc().get(self)
    }

    /// Get a mutable reference to a heap object
    pub fn get_mut(&self) -> Option<GcRefMut> {
        gc().get_mut(self)
    }

    pub fn clone_root(&self) -> Handle {
        Handle::new_root(self.idx)
    }

    pub fn clone_no_root(&self) -> Handle {
        Handle {
            root: false,
            idx: self.idx,
        }
        //Handle::new_root(self.idx)
    }

    pub fn id(&self) -> HandleIdx {
        self.idx
    }
}

impl Clone for Handle {
    fn clone(&self) -> Handle {
        if self.root {
            self.clone_root()
        } else {
            self.clone_no_root()
        }
    }
}

impl PartialEq<Self> for Handle {
    fn eq(&self, other: &Self) -> bool {
        //self.gen == other.gen && self.ptr == other.ptr
        self.idx == other.idx
    }
}
impl Eq for Handle {}

impl Hash for Handle {
    fn hash<H: Hasher>(&self, state: &mut H) {
        //self.gen.hash(state);
        //self.ptr.hash(state);
        self.idx.hash(state);
    }
}

impl AsRef<Handle> for Handle {
    fn as_ref(&self) -> &Handle {
        self
    }
}

impl Drop for Handle {
    fn drop(&mut self) {
        if self.root {
            let refs = if gc().roots.contains_key(&self.idx) {
                gc_mut()
                    .roots
                    .entry(self.idx)
                    .or_insert_with(|| panic!("Dropped invalid Handle!"))
            } else {
                gc_mut()
                    .nursery
                    .roots
                    .entry(self.idx)
                    .or_insert_with(|| panic!("Dropped invalid Handle!"))
                //.or_insert_with(|| HandleIdxAtomic::new(1))//panic!("Dropped invalid Handle!"))
            };
            refs.fetch_sub(1, Ordering::Relaxed);
        }
    }
}

pub enum GcRef<'a> {
    Arc {
        //obj: Arc<RwLock>,
        handle: Handle,
        heap: &'a Heap,
        read: RwLockReadGuard<'a, ExpObj>,
    },
    Rc {
        read: Ref<'a, ExpObj>,
    },
}

impl<'a> GcRef<'a> {
    /*fn new_lifetime_arc(obj: &Arc<RwLock>) -> &'a Arc<RwLock> {
        unsafe { &*(obj as *const Arc<RwLock>) }
    }*/

    /*fn new_lifetime_rc(obj: &Rc<RefCell>) -> &'a Rc<RefCell> {
        unsafe { &*(obj as *const Rc<RefCell>) }
    }*/

    fn new_arc(
        heap: &'a Heap,
        handle: Handle,
        read: RwLockReadGuard<'a, ExpObj>, /*obj: Arc<RwLock>*/
    ) -> GcRef<'a> {
        //let read = GcRef::new_lifetime_arc(&obj)
        //    .read()
        //    .expect("Heap has poisoned data, done!");
        //GcRef::Arc { obj, read }
        GcRef::Arc { heap, handle, read }
    }

    /*fn new_rc(obj: Rc<RefCell>) -> GcRef<'a> {
        let read = GcRef::new_lifetime_rc(&obj).borrow();
        GcRef::Rc { _obj: obj, read }
    }*/

    pub fn data<'b>(&'b self) -> &'b ExpObj
    where
        'a: 'b,
    {
        match self {
            //Self::Arc { obj: _, read } => &*read,
            Self::Arc {
                heap: _,
                handle: _,
                read,
            } => &*read,
            Self::Rc { read } => &*read,
        }
    }
}

impl<'a> Deref for GcRef<'a> {
    type Target = ExpObj;

    fn deref(&self) -> &Self::Target {
        self.data()
    }
}

/*impl<'a> Drop for GcRef<'a> {
    fn drop(&mut self) {
        match self {
            Self::Arc { heap, handle, read } => {
                drop(read)
            }
            Self::Rc { read } => drop(read),
        }
    }
}*/

pub enum GcRefMut<'a> {
    Arc {
        handle: Handle,
        heap: &'a Heap,
        write: RwLockWriteGuard<'a, ExpObj>,
    },
    Rc {
        //_obj: Rc<RefCell>,
        write: RefMut<'a, ExpObj>,
    },
}

impl<'a> GcRefMut<'a> {
    /*fn new_lifetime_arc(obj: &Arc<RwLock>) -> &'a Arc<RwLock> {
        unsafe { &*(obj as *const Arc<RwLock>) }
    }*/

    /*fn new_lifetime_rc(obj: &Rc<RefCell>) -> &'a Rc<RefCell> {
        unsafe { &*(obj as *const Rc<RefCell>) }
    }*/

    fn new_arc(
        heap: &'a Heap,
        handle: Handle,
        write: RwLockWriteGuard<'a, ExpObj>, /*obj: Arc<RwLock>*/
    ) -> GcRefMut<'a> {
        //let write = GcRefMut::new_lifetime_arc(&obj)
        //    .try_write()
        //    .expect("Heap has poisoned data, done!");
        //GcRefMut::Arc { _obj: obj, write }
        GcRefMut::Arc {
            heap,
            handle,
            write,
        }
    }

    /*fn new_rc(obj: Rc<RefCell>) -> GcRefMut<'a> {
        let write = GcRefMut::new_lifetime_rc(&obj).borrow_mut();
        GcRefMut::Rc { _obj: obj, write }
    }*/

    pub fn data<'b>(&'b self) -> &'b ExpObj
    where
        'a: 'b,
    {
        match self {
            Self::Arc {
                heap: _,
                handle: _,
                write,
            } => &*write,
            Self::Rc { write } => &*write,
        }
    }

    pub fn data_mut<'b>(&'b mut self) -> &'b mut ExpObj
    where
        'a: 'b,
    {
        match self {
            Self::Arc {
                heap: _,
                handle: _,
                write,
            } => &mut *write,
            Self::Rc { write } => &mut *write,
        }
    }
}

impl<'a> Deref for GcRefMut<'a> {
    type Target = ExpObj;

    fn deref(&self) -> &Self::Target {
        self.data()
    }
}

impl<'a> DerefMut for GcRefMut<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.data_mut()
    }
}

/*impl<'a> Drop for GcRefMut<'a> {
    fn drop(&mut self) {
        match self {
            Self::Arc {
                heap,
                handle,
                write,
            } => {
                drop(write)
            }
            Self::Rc { write } => drop(write),
        }
    }
}*/
/*
#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    enum Value<'a> {
        Base(&'a AtomicUsize),
        Refs(&'a AtomicUsize, Handle<Value<'a>>, Handle<Value<'a>>),
    }

    impl<'a> Trace<Self> for Value<'a> {
        fn trace(&self, tracer: &mut Tracer<Self>) {
            match self {
                Value::Base(_) => {}
                Value::Refs(_, a, b) => {
                    a.trace(tracer);
                    b.trace(tracer);
                }
            }
        }
    }

    impl<'a> Drop for Value<'a> {
        fn drop(&mut self) {
            match self {
                Value::Base(count) | Value::Refs(count, _, _) => {
                    count.fetch_sub(1, Ordering::Relaxed)
                }
            };
        }
    }

    #[test]
    fn basic() {
        let count: AtomicUsize = AtomicUsize::new(0);

        let new_count = || {
            count.fetch_add(1, Ordering::Relaxed);
            &count
        };

        let mut heap = Heap::new();

        let a = heap.insert(Value::Base(new_count()));

        heap.clean();

        assert_eq!(heap.contains(&a), true);

        let a = a.clone();

        heap.clean();

        assert_eq!(heap.contains(&a), false);

        drop(heap);
        assert_eq!(count.load(Ordering::Acquire), 0);
    }

    #[test]
    fn ownership() {
        let count: AtomicUsize = AtomicUsize::new(0);

        let new_count = || {
            count.fetch_add(1, Ordering::Relaxed);
            &count
        };

        let mut heap = Heap::new();

        let a = heap.insert(Value::Base(new_count())).clone();
        let b = heap.insert(Value::Base(new_count())).clone();
        let c = heap.insert(Value::Base(new_count())).clone();
        let d = heap.insert(Value::Refs(new_count(), a.clone(), c.clone()));
        let e = heap.insert(Value::Base(new_count())).clone();

        heap.clean();

        assert_eq!(heap.contains(&a), true);
        assert_eq!(heap.contains(&b), false);
        assert_eq!(heap.contains(&c), true);
        assert_eq!(heap.contains(&d), true);
        assert_eq!(heap.contains(&e), false);

        let a = heap.insert(Value::Base(new_count())).clone();

        heap.clean();
        assert_eq!(heap.contains(&a), false);

        let a = heap.insert(Value::Base(new_count())).clone();
        let a = a.clone_root();

        heap.clean();

        assert_eq!(heap.contains(&a), true);

        drop(heap);
        assert_eq!(count.load(Ordering::Acquire), 0);
    }

    #[test]
    fn recursive() {
        let count: AtomicUsize = AtomicUsize::new(0);

        let new_count = || {
            count.fetch_add(1, Ordering::Relaxed);
            &count
        };

        let mut heap = Heap::new();

        let a = heap.insert(Value::Base(new_count()));
        let b = heap.insert(Value::Base(new_count()));

        *heap.get_mut(&a).unwrap() = Value::Refs(new_count(), a.clone(), b.clone());

        heap.clean();

        assert_eq!(heap.contains(&a), true);
        assert_eq!(heap.contains(&b), true);

        //let a = a.into_handle();
        let a = a.clone();

        heap.clean();

        assert_eq!(heap.contains(&a), false);
        assert_eq!(heap.contains(&b), true);

        drop(heap);
        assert_eq!(count.load(Ordering::Acquire), 0);
    }

    #[test]
    fn temporary() {
        let count: AtomicUsize = AtomicUsize::new(0);

        let new_count = || {
            count.fetch_add(1, Ordering::Relaxed);
            &count
        };

        let mut heap = Heap::new();

        let a = heap.insert(Value::Base(new_count())).clone();

        heap.clean();

        assert_eq!(heap.contains(&a), false);

        let a = heap.insert(Value::Base(new_count())).clone();
        let b = heap.insert(Value::Refs(new_count(), a.clone(), a.clone()));

        heap.clean();

        assert_eq!(heap.contains(&a), true);
        assert_eq!(heap.contains(&b), true);

        /*let a = heap.insert_temp(Value::Base(new_count()));

        heap.clean_excluding(Some(a));

        assert_eq!(heap.contains(&a), true);*/

        drop(heap);
        assert_eq!(count.load(Ordering::Acquire), 0);
    }
}
*/
