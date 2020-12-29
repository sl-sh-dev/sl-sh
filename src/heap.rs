use std::borrow::Cow;

use crate::rc_heap::*;
//use crate::gc_heap::*;
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

// Can swap heap strategies by replacing these at compile time.
pub type HandleRef<'a> = RCHandleRef<'a>;
pub type HandleRefMut<'a> = RCHandleRefMut<'a>;
pub type Handle = RCHandle;
pub type Heap = RCHeap;
//pub type HandleRef<'a> = GCHandleRef<'a>;
//pub type HandleRefMut<'a> = GCHandleRefMut<'a>;
//pub type Handle = GCHandle;
//pub type Heap = GCHeap;
