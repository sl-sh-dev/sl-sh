use std::borrow::Cow;

use crate::rc_heap::*;
use crate::value::*;

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
