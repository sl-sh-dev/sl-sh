/// # Broom
///
/// An ergonomic tracing garbage collector that supports mark 'n sweep garbage collection.
///
/// ## Example
///
/// ```
/// //use broom::prelude::*;
/// use sl_sh::gc::*;
/// use sl_sh::trace::*;
///
/// // The type you want the heap to contain
/// pub enum GcRefect {
///     Num(f64),
///     List(Vec<Handle<Self>>),
/// }
///
/// // Tell the garbage collector how to explore a graph of this object
/// impl Trace<Self> for GcRefect {
///     fn trace(&self, tracer: &mut Tracer<Self>) {
///         match self {
///             GcRefect::Num(_) => {},
///             GcRefect::List(objects) => objects.trace(tracer),
///         }
///     }
/// }
///
/// // Create a new heap
/// //let mut heap = Heap::default();
/// let gc = create_gc::<GcRefect>();
/// let mut heap = gc.get_mut();
///
/// // Temporary objects are cheaper than rooted objects, but don't survive heap cleans
/// let a = heap.insert_temp(GcRefect::Num(42.0));
/// let b = heap.insert_temp(GcRefect::Num(1337.0));
///
/// // Turn the numbers into a rooted list
/// let c = heap.insert(GcRefect::List(vec![a.clone(), b.clone()]));
///
/// // Change one of the numbers - this is safe, even if the object is self-referential!
/// *heap.get_mut(&a).unwrap() = GcRefect::Num(256.0);
///
/// // Create another number object
/// let d = heap.insert_temp(GcRefect::Num(0.0));
///
/// // Clean up unused heap objects
/// heap.clean();
///
/// // a, b and c are all kept alive because c is rooted and a and b are its children
/// assert!(heap.contains(a));
/// assert!(heap.contains(b));
/// assert!(heap.contains(c));
///
/// // Because `d` was temporary and unused, it did not survive the heap clean
/// assert!(!heap.contains(d));
///
/// ```
use crate::trace::*;
use crate::types::*;

//use hashbrown::{HashMap, HashSet};
use std::collections::HashMap;
use std::{
    //cell::{Ref, RefCell, RefMut},
    cell::{Ref, RefCell, RefMut},
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
static mut NURSERY_ROOTS: Option<RwLock<HashMap<HandleIdx, HandleIdxAtomic>>> = None;
static mut ROOTS: Option<RwLock<HashMap<HandleIdx, HandleIdxAtomic>>> = None;

pub struct GcWrapRef<'a> {
    read: RwLockReadGuard<'a, Heap>,
}

impl<'a> Deref for GcWrapRef<'a> {
    type Target = Heap;

    fn deref(&self) -> &Self::Target {
        &*self.read
    }
}

pub struct GcWrapRefMut<'a> {
    write: RwLockWriteGuard<'a, Heap>,
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

struct RootsRef<'a> {
    read: RwLockReadGuard<'a, HashMap<HandleIdx, HandleIdxAtomic>>,
}

impl<'a> Deref for RootsRef<'a> {
    type Target = HashMap<HandleIdx, HandleIdxAtomic>;

    fn deref(&self) -> &Self::Target {
        &*self.read
    }
}

struct RootsRefMut<'a> {
    write: RwLockWriteGuard<'a, HashMap<HandleIdx, HandleIdxAtomic>>,
}

impl<'a> Deref for RootsRefMut<'a> {
    type Target = HashMap<HandleIdx, HandleIdxAtomic>;

    fn deref(&self) -> &Self::Target {
        &*self.write
    }
}

impl<'a> DerefMut for RootsRefMut<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.write
    }
}

pub fn init_gc() {
    unsafe {
        if STATIC_GC.is_none() {
            STATIC_GC = Some(RwLock::new(Heap::new()));
        }
        if NURSERY_ROOTS.is_none() {
            NURSERY_ROOTS = Some(RwLock::new(HashMap::default()));
        }
        if ROOTS.is_none() {
            ROOTS = Some(RwLock::new(HashMap::default()));
        }
    }
}

//pub fn gc<'a>() -> &'a Heap {
pub fn gc<'a>() -> GcWrapRef<'a> {
    //unsafe { &*(&*STATIC_GC.as_ref().unwrap().read().unwrap() as *const Heap) }
    GcWrapRef {
        read: unsafe { STATIC_GC.as_ref().unwrap().read().unwrap() },
    }
}

//pub fn gc_mut<'a>() -> &'a mut Heap {
pub fn gc_mut<'a>() -> GcWrapRefMut<'a> {
    //unsafe { &mut *(&mut *STATIC_GC.as_ref().unwrap().write().unwrap() as *mut Heap) }
    GcWrapRefMut {
        write: unsafe { STATIC_GC.as_ref().unwrap().write().unwrap() },
    }
}

fn nursery_roots<'a>() -> RootsRef<'a> {
    RootsRef {
        read: unsafe { NURSERY_ROOTS.as_ref().unwrap().read().unwrap() },
    }
}

fn nursery_roots_mut<'a>() -> RootsRefMut<'a> {
    RootsRefMut {
        write: unsafe { NURSERY_ROOTS.as_ref().unwrap().write().unwrap() },
    }
}

fn roots<'a>() -> RootsRef<'a> {
    RootsRef {
        read: unsafe { ROOTS.as_ref().unwrap().read().unwrap() },
    }
}

fn roots_mut<'a>() -> RootsRefMut<'a> {
    RootsRefMut {
        write: unsafe { ROOTS.as_ref().unwrap().write().unwrap() },
    }
}

struct Nursery {
    objects: RwLock<Vec<Option<ExpObj>>>,
    objects_handle: Vec<HandleIdx>,
    //roots: RwLock<HashMap<HandleIdx, HandleIdxAtomic>>,
    excludes: Vec<Handle>,
}

impl Nursery {
    fn new() -> Nursery {
        Nursery {
            objects: RwLock::new(Vec::with_capacity(1024)),
            objects_handle: Vec::new(),
            //roots: RwLock::new(HashMap::default()),
            excludes: Vec::new(),
        }
    }

    fn clear(&mut self) {
        if let Ok(mut objects) = self.objects.write() {
            objects.clear();
        }
        self.objects_handle.clear();
        nursery_roots_mut().clear();
        //self.roots.write().unwrap().clear();
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
    objects: RwLock<Vec<Option<ExpObj>>>,
    objects_handle: Vec<HandleIdx>,
    freed: Vec<usize>,
    //rooted: HashMap<Handle, AtomicU32>,
    //roots: RwLock<HashMap<HandleIdx, HandleIdxAtomic>>,
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
            objects: RwLock::new(Vec::default()),
            objects_handle: Vec::default(),
            freed: Vec::default(),
            //roots: RwLock::new(HashMap::default()),
            refs: HashMap::default(),
            next_handle: HandleIdxAtomic::new(0),
            nursery: Nursery::new(),
        }
    }

    pub fn print_stats(&self) {
        println!("last_sweep: {}, obj sweeps: {}, objects: {}, object handles: {}, freed len: {}, rooted: {}, refs: {}, next handle: {}",
                 self.last_sweep, self.object_sweeps.len(), self.objects.read().unwrap().len(), self.objects_handle.len(), self.freed.len(), roots().len(), self.refs.len(), self.next_handle.load(Ordering::Relaxed));
    }
    pub fn objects(&self) -> usize {
        self.objects.read().unwrap().len()
    }
    pub fn free_objects(&self) -> usize {
        self.freed.len()
    }
    pub fn used_objects(&self) -> usize {
        self.objects() - self.free_objects()
    }
    pub fn nursery_objects(&self) -> usize {
        self.nursery.objects.read().unwrap().len()
    }

    fn update_handle(&mut self, handle: impl AsRef<Handle>, object: ExpObj) -> Handle {
        let handle = handle.as_ref();
        let idx = handle.idx;
        if let Some(rc) = roots_mut().remove(&idx) {
            nursery_roots_mut().insert(idx, rc);
        }
        let handle = Handle { root: false, idx };
        self.nursery.objects.write().unwrap().push(Some(object));
        self.nursery.objects_handle.push(handle.idx);
        let href = HandleRef {
            gen: RefGen::Nursery,
            handle: handle.clone_no_root(),
            idx: self.nursery.objects.read().unwrap().len() - 1,
        };
        self.refs.insert(handle.idx, href);
        handle
    }

    fn insert_int(&mut self, object: ExpObj) -> Handle {
        let idx = self.next_handle.fetch_add(1, Ordering::Relaxed);
        {
            let mut roots = nursery_roots_mut();
            let refs = roots.entry(idx).or_insert_with(|| HandleIdxAtomic::new(0));
            refs.fetch_add(1, Ordering::Relaxed);
        }
        let handle = Handle { root: true, idx };
        let idx;
        if let Ok(mut objects) = self.nursery.objects.write() {
            objects.push(Some(object));
            self.nursery.objects_handle.push(handle.idx);
            idx = objects.len() - 1;
        } else {
            panic!("Failed to get write lock on nursery objects.");
        }
        let href = HandleRef {
            gen: RefGen::Nursery,
            handle: handle.clone_no_root(),
            idx,
        };
        self.refs.insert(handle.idx, href);
        handle
    }

    pub fn insert(&mut self, object: ExpObj) -> Handle {
        self.insert_int(object)
    }

    pub fn down_root(&mut self, handle: impl AsRef<Handle>) {
        let handle = handle.as_ref();

        let mut object = None;
        if let Some(href) = self.refs.get(&handle.idx) {
            if let RefGen::GenX = href.gen {
                if let Ok(mut objects) = self.objects.write() {
                    objects.push(None);
                    let obj = objects.swap_remove(href.idx);
                    self.objects_handle.push(0);
                    self.objects_handle.swap_remove(href.idx);
                    self.freed.push(href.idx);
                    object = obj;
                    if object.is_none() {
                        panic!("down_root: Not a valid expression (does something have a mutable copy?)!");
                    }
                }
            }
        }
        if let Some(obj) = object {
            let handle = self.update_handle(handle, obj);
            self.nursery.excludes.push(handle);
        }
    }

    /// Count the number of heap-allocated objects in this heap
    /*pub fn len(&self) -> usize {
        self.objects.len()
    }

    pub fn is_empty(&self) -> bool {
        self.objects.is_empty()
    }*/

    /// Return true if the heap contains the specified handle
    pub fn contains(&self, handle: impl AsRef<Handle>) -> bool {
        let handle = handle.as_ref();
        if let Some(href) = self.refs.get(&handle.idx) {
            match href.gen {
                RefGen::Nursery => self
                    .nursery
                    .objects
                    .read()
                    .unwrap()
                    .get(href.idx)
                    .expect("Invalid handle!")
                    .is_some(),
                RefGen::GenX => self
                    .objects
                    .read()
                    .unwrap()
                    .get(href.idx)
                    .expect("Invalid handle!")
                    .is_some(),
            }
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
            if let Ok(objects) = objects.read() {
                //objects.push(None);
                //let data = objects.swap_remove(href.idx);
                if let Some(data) = objects.get(href.idx) {
                    if let Some(data) = data {
                        return Some(GcRef::new(handle.clone_root(), data.clone()));
                    }
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
            if let Ok(mut objects) = objects.write() {
                objects.push(None);
                let data = objects.swap_remove(href.idx);
                if let Some(data) = data {
                    return Some(GcRefMut::new(handle.clone_root(), data));
                }
                /*if let Some(data) = objects.get(href.idx) {
                    if let Some(data) = data {
                        return Some(GcRefMut::new(handle.clone_root(), data.clone()));
                    }
                }*/
            }
        }
        None
    }

    pub fn prune_nursery(&mut self) -> Vec<Option<ExpObj>> {
        let mut new_nursery = Nursery::new();
        let new_sweep = 1;
        let mut object_sweeps: HashMap<usize, usize> = HashMap::default();
        {
            let mut tracer = Tracer {
                new_sweep,
                object_sweeps: &mut object_sweeps,
                objects: &self.nursery.objects.read().unwrap(),
                refs: &self.refs,
                ref_gen: RefGen::Nursery,
            };

            // Mark
            nursery_roots_mut().retain(|handle_idx, rc| {
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

        let mut corpses = Vec::new();
        for (i, obj) in self.nursery.objects.write().unwrap().drain(..).enumerate() {
            if object_sweeps
                .get(&i)
                .map(|sweep| *sweep == new_sweep)
                .unwrap_or(false)
            {
                let handle_idx = self.nursery.objects_handle[i];
                if let Ok(mut objects) = new_nursery.objects.write() {
                    objects.push(obj);
                    new_nursery.objects_handle.push(handle_idx);
                    let href = HandleRef {
                        gen: RefGen::Nursery,
                        handle: Handle::new(handle_idx),
                        idx: objects.len() - 1,
                    };
                    self.refs.insert(handle_idx, href);
                } else {
                    panic!("Invalid new nursery.");
                }
            } else {
                let handle_idx = self.nursery.objects_handle[i];
                self.refs.remove(&handle_idx);
                corpses.push(obj);
            }
        }
        //for (idx, rc) in nursery_roots_mut().drain() {
        //    new_nursery.roots.write().unwrap().insert(idx, rc);
        //}
        self.nursery = new_nursery;
        corpses
    }

    fn promote_nursery(&mut self) {
        let new_sweep = 1;
        let mut object_sweeps: HashMap<usize, usize> = HashMap::default();
        {
            let mut tracer = Tracer {
                new_sweep,
                object_sweeps: &mut object_sweeps,
                objects: &self.nursery.objects.read().unwrap(),
                refs: &self.refs,
                ref_gen: RefGen::Nursery,
            };

            // Mark
            nursery_roots_mut().retain(|idx, rc| {
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
        for (i, obj) in self.nursery.objects.write().unwrap().drain(..).enumerate() {
            if object_sweeps
                .get(&i)
                .map(|sweep| *sweep == new_sweep)
                .unwrap_or(false)
            {
                if let Some(obj) = obj {
                    let handle_idx = self.nursery.objects_handle[i];

                    if let Ok(mut objects) = self.objects.write() {
                        let idx = if let Some(idx) = self.freed.pop() {
                            objects.push(Some(obj));
                            objects.swap_remove(idx);
                            self.objects_handle.push(handle_idx);
                            self.objects_handle.swap_remove(idx);
                            idx
                        } else {
                            objects.push(Some(obj));
                            self.objects_handle.push(handle_idx);
                            objects.len() - 1
                        };
                        let href = HandleRef {
                            gen: RefGen::GenX,
                            handle: Handle::new(handle_idx),
                            idx,
                        };
                        self.refs.insert(handle_idx, href);
                    } else {
                        panic!("Failed to get write lock on objects!");
                    }
                }
            } else {
                let handle_idx = self.nursery.objects_handle[i];
                self.refs.remove(&handle_idx);
            }
        }
        for (idx, rc) in nursery_roots_mut().drain() {
            roots_mut().insert(idx, rc);
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
                objects: &self.objects.read().unwrap(),
                refs: &self.refs,
                ref_gen: RefGen::GenX,
            };

            // Mark
            roots_mut().retain(|handle_idx, rc| {
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
        for (i, obj) in self.objects.write().unwrap().iter_mut().enumerate() {
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
            handle: self.handle.clone_no_root(),
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
        if roots().contains_key(&idx) {
            let mut roots = roots_mut();
            let refs = roots.entry(idx).or_insert_with(|| HandleIdxAtomic::new(0));
            refs.fetch_add(1, Ordering::Relaxed);
        } else {
            let mut roots = nursery_roots_mut();
            let refs = roots.entry(idx).or_insert_with(|| HandleIdxAtomic::new(0));
            refs.fetch_add(1, Ordering::Relaxed);
        };
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
            if roots().contains_key(&self.idx) {
                let mut roots = roots_mut();
                let refs = roots
                    .entry(self.idx)
                    .or_insert_with(|| panic!("Dropped invalid Handle!"));
                refs.fetch_sub(1, Ordering::Relaxed);
            } else {
                let mut roots = nursery_roots_mut();
                let refs = roots
                    .entry(self.idx)
                    .or_insert_with(|| panic!("Dropped invalid Handle!"));
                //.or_insert_with(|| HandleIdxAtomic::new(1))//panic!("Dropped invalid Handle!"))
                refs.fetch_sub(1, Ordering::Relaxed);
            };
        }
    }
}

pub struct GcRef {
    handle: Handle,
    data: ExpObj,
}

impl GcRef {
    fn new(handle: Handle, data: ExpObj) -> GcRef {
        GcRef { handle, data }
    }
}

impl Deref for GcRef {
    type Target = ExpObj;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
/*
impl Drop for GcRef {
    fn drop(&mut self) {
        let gc = gc();
        if let Some(href) = gc.refs.get(&self.handle.idx) {
            match href.gen {
                RefGen::Nursery => {
                    let mut objects = gc.nursery.objects.write().unwrap();
                    objects.push(Some(self.data.clone()));
                    objects.swap_remove(href.idx);
                }
                RefGen::GenX => {
                    let mut objects = gc.objects.write().unwrap();
                    objects.push(Some(self.data.clone()));
                    objects.swap_remove(href.idx);
                }
            };
        }
    }
}
*/
pub struct GcRefMut {
    handle: Handle,
    data: ExpObj,
}

impl GcRefMut {
    fn new(handle: Handle, data: ExpObj) -> GcRefMut {
        //GcRefMut { handle, data: Some(data) }
        GcRefMut { handle, data }
    }
}

impl Deref for GcRefMut {
    type Target = ExpObj;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl DerefMut for GcRefMut {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl Drop for GcRefMut {
    fn drop(&mut self) {
        let gc = gc();
        if let Some(href) = gc.refs.get(&self.handle.idx) {
            let mut data = ExpObj {
                data: ExpEnum::Nil,
                meta: None,
            };
            std::mem::swap(&mut self.data, &mut data);
            match href.gen {
                RefGen::Nursery => {
                    let mut objects = gc.nursery.objects.write().unwrap();
                    objects.push(Some(data));
                    objects.swap_remove(href.idx);
                }
                RefGen::GenX => {
                    let mut objects = gc.objects.write().unwrap();
                    objects.push(Some(data));
                    objects.swap_remove(href.idx);
                }
            };
        } else {
            panic!("Dropping an invalid GcRefMut!");
        }
    }
}

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
