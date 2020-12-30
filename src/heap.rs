use std::borrow::Cow;

#[cfg(not(feature = "gc"))]
use crate::rc_heap::*;

#[cfg(feature = "gc")]
use crate::gc_heap::*;
use crate::value::*;

// This is anything that can live on the heap.  Values normally live on the
// stack or as constants but can be stored in the heap as well
// (for instance closed over values).
#[derive(Clone, Debug)]
pub enum Object {
    Value(Value),
    String(Cow<'static, str>),
    Vector(Vec<Value>),
    Bytes(Vec<u8>),
    Pair(Handle, Handle),
}

impl Object {
    pub fn is_nil(&self) -> bool {
        matches!(self, Object::Value(Value::Nil))
    }
}

// Can swap heap strategies by replacing these at compile time.
#[cfg(not(feature = "gc"))]
pub type HandleRef<'a> = RCHandleRef<'a>;
#[cfg(not(feature = "gc"))]
pub type HandleRefMut<'a> = RCHandleRefMut<'a>;
#[cfg(not(feature = "gc"))]
pub type Handle = RCHandle;
#[cfg(not(feature = "gc"))]
pub type Heap = RCHeap;


#[cfg(feature = "gc")]
pub type HandleRef<'a> = GCHandleRef<'a>;
#[cfg(feature = "gc")]
pub type HandleRefMut<'a> = GCHandleRefMut<'a>;
#[cfg(feature = "gc")]
pub type Handle = GCHandle;
#[cfg(feature = "gc")]
pub type Heap = GCHeap;
