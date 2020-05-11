//use hashbrown::{HashMap, HashSet};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};

use crate::gc::*;
use crate::types::*;

/// A trait used to tell the garbage collector how it may explore an object graph composed of
/// values of type `T`.
///
/// To implement this, simply call `foo.trace(tracer)` on all traceable children
/// of the type. Note that this trait has default implementations for a variety of common types.
///
/// # Example
/// ```
/// //use broom::prelude::*;
/// use sl_sh::gc::*;
/// use sl_sh::trace::*;
///
/// pub enum Object {
///     Num(f64),
///     List(Vec<Handle<Self>>),
/// }
///
/// impl Trace<Self> for Object {
///     fn trace(&self, tracer: &mut Tracer<Self>) {
///         match self {
///             Object::Num(_) => {},
///             Object::List(objects) => objects.trace(tracer),
///         }
///     }
/// }
/// ```
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
pub struct Tracer<'a> {
    pub(crate) new_sweep: usize,
    pub(crate) object_sweeps: &'a mut HashMap<usize, usize>,
    pub(crate) objects: &'a Vec<Option<ExpObj>>,
    pub(crate) refs: &'a HashMap<HandleIdx, HandleRef>,
    pub(crate) ref_gen: RefGen,
}

impl<'a> Tracer<'a> {
    pub(crate) fn mark(&mut self, href: &HandleRef) {
        if href.gen() == self.ref_gen {
            let sweep = self
                .object_sweeps
                .entry(href.index())
                .or_insert(self.new_sweep - 1);
            if *sweep != self.new_sweep {
                *sweep = self.new_sweep;
                if let Some(obj) = &self.objects[href.index()] {
                    //XXX FIX ME
                    obj.trace(self);
                }
            }
        }
    }

    pub(crate) fn mark_handle(&mut self, handle: HandleIdx) {
        if let Some(href) = self.refs.get(&handle) {
            self.mark(&href);
        }
    }
}

impl Trace for Handle {
    fn trace(&self, tracer: &mut Tracer) {
        if let Some(href) = tracer.refs.get(&self.id()) {
            tracer.mark(&href);
        }
    }
}

/*
impl<O: Trace<O>> Trace<O> for Rooted<O> {
    fn trace(&self, tracer: &mut Tracer<O>) {
        self.handle().trace(tracer);
    }
}
*/
/*
impl Trace for [ExpObj] {
    fn trace(&self, tracer: &mut Tracer) {
        self.iter().for_each(|object| object.trace(tracer));
    }
}

impl<K> Trace for HashMap<K, ExpObj> {
    fn trace(&self, tracer: &mut Tracer) {
        self.values().for_each(|object| object.trace(tracer));
    }
}

impl<S: ::std::hash::BuildHasher> Trace for HashSet<ExpObj, S> {
    fn trace(&self, tracer: &mut Tracer) {
        self.iter().for_each(|object| object.trace(tracer));
    }
}

impl<K> Trace for HashMap<K, Handle> {
    fn trace(&self, tracer: &mut Tracer) {
        self.values().for_each(|object| object.trace(tracer));
    }
}

impl<S: ::std::hash::BuildHasher> Trace for HashSet<Handle, S> {
    fn trace(&self, tracer: &mut Tracer) {
        self.iter().for_each(|object| object.trace(tracer));
    }
}
*/
