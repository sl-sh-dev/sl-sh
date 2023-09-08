use std::collections::HashMap;
use std::sync::Arc;

use crate::bits::FLAG_MUT;
use crate::chunk::*;
use crate::error::*;
use crate::value::*;
use crate::{get_code, FxHashMap, Interned};

pub mod handle;
pub use crate::handle::Handle;
use crate::handle::Numeric64Handle;
use crate::heap::storage::Storage;
use crate::persistent_map::{MapNode, PersistentMap};
use crate::persistent_vec::{PersistentVec, VecNode};

pub mod bits;
pub mod persistent_map;
pub mod persistent_vec;
mod storage;

#[derive(Clone, Debug)]
pub struct CallFrame {
    pub id: usize,
    pub chunk: Arc<Chunk>,
    pub ip: *const u8,
    pub current_ip: *const u8,
    pub stack_top: usize,
    pub this_fn: Option<Value>,
    pub defers: Vec<Value>,
    pub on_error: Option<Value>,
    pub called: Value,
}

impl CallFrame {
    /// Return the line number that corresponds to the current_ip if available.
    pub fn current_line(&self) -> Option<u32> {
        let offset = unsafe { self.current_ip.offset_from(get_code!(self.chunk)) as usize };
        self.chunk.offset_to_line(offset)
    }

    /// Return the current offset (IP) for the frame using current_ip.
    pub fn current_offset(&self) -> usize {
        unsafe { self.current_ip.offset_from(get_code!(self.chunk)) as usize }
    }
}

#[derive(Clone, Debug)]
pub struct Continuation {
    pub frame: CallFrame,
    pub arg_reg: usize,
    pub stack: Vec<Value>,
}

// This is anything that can live on the heap.  Values normally live on the
// stack or as constants.
#[derive(Clone, Debug)]
enum Object {
    String(Arc<String>),
    Vector(Arc<Vec<Value>>),
    //Vector(Vec<Value>),
    Map(Arc<HashMap<Value, Value>>),
    Bytes(Arc<Vec<u8>>),
    Pair(Arc<(Value, Value)>),
    Value(Value),

    PersistentVec(Arc<PersistentVec>),
    VecNode(Arc<VecNode>),
    PersistentMap(Arc<PersistentMap>),
    MapNode(Arc<MapNode>),

    CallFrame(Arc<CallFrame>),
    // Everything below here is always read only.
    Lambda(Arc<Chunk>),
    Closure(Arc<Chunk>, Arc<Vec<Handle>>),
    Continuation(Arc<Continuation>),
    // Place holder for an empty object slot.
    Empty,
}

#[derive(Clone, Copy)]
pub union Numeric64 {
    pub int: i64,
    pub uint: u64,
    pub float: f64,
}

#[derive(Clone, Copy)]
pub struct Error {
    pub keyword: Interned,
    pub data: Value,
}

pub enum MutState {
    Mutable,
    Immutable,
}

impl MutState {
    fn flag(&self) -> u8 {
        match self {
            Self::Mutable => FLAG_MUT,
            Self::Immutable => 0,
        }
    }
}

//#[derive(Debug)]
pub struct Heap {
    objects: Storage<Object>,
    numerics: Storage<Numeric64>,
    errors: Storage<Error>,
    props: Option<FxHashMap<Value, Arc<FxHashMap<Interned, Value>>>>,
    greys: Vec<Value>,
    paused: u32,
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

macro_rules! value_op {
    ($heap:expr, $val:expr, $op:ident, $default:expr) => {{
        match $val {
            Value::CharClusterLong(handle) => $heap.objects.$op(handle.idx()),
            Value::String(handle) => $heap.objects.$op(handle.idx()),
            Value::Vector(handle) => $heap.objects.$op(handle.idx()),
            Value::PersistentVec(handle) => $heap.objects.$op(handle.idx()),
            Value::PersistentMap(handle) => $heap.objects.$op(handle.idx()),
            Value::VecNode(handle) => $heap.objects.$op(handle.idx()),
            Value::MapNode(handle) => $heap.objects.$op(handle.idx()),
            Value::Map(handle) => $heap.objects.$op(handle.idx()),
            Value::Bytes(handle) => $heap.objects.$op(handle.idx()),
            Value::Pair(handle) => $heap.objects.$op(handle.idx()),
            Value::List(handle, _) => $heap.objects.$op(handle.idx()),
            Value::Lambda(handle) => $heap.objects.$op(handle.idx()),
            Value::Closure(handle) => $heap.objects.$op(handle.idx()),
            Value::Continuation(handle) => $heap.objects.$op(handle.idx()),
            Value::CallFrame(handle) => $heap.objects.$op(handle.idx()),
            Value::Value(handle) => $heap.objects.$op(handle.idx()),

            Value::Int64(handle) => match handle {
                Numeric::Local(_) => $default,
                Numeric::Heap(handle) => $heap.numerics.$op(handle.into()),
            },
            Value::UInt64(handle) => match handle {
                Numeric::Local(_) => $default,
                Numeric::Heap(handle) => $heap.numerics.$op(handle.into()),
            },
            Value::Float64(handle) => match handle {
                Numeric::Local(_) => $default,
                Numeric::Heap(handle) => $heap.numerics.$op(handle.into()),
            },
            Value::Error(handle) => $heap.errors.$op(handle.idx()),

            Value::Byte(_)
            | Value::Int32(_)
            | Value::UInt32(_)
            | Value::CodePoint(_)
            | Value::CharCluster(_, _)
            | Value::Symbol(_)
            | Value::Keyword(_)
            | Value::StringConst(_)
            | Value::Special(_)
            | Value::Builtin(_)
            | Value::True
            | Value::False
            | Value::Nil
            | Value::Undefined => $default,
        }
    }};
}

macro_rules! mark {
    ($heap:expr, $val:expr) => {{
        value_op!($heap, $val, mark, ());
    }};
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            objects: Storage::default(),
            numerics: Storage::default(),
            errors: Storage::default(),
            props: Some(FxHashMap::default()),
            greys: vec![],
            paused: 0,
        }
    }

    fn props(&self) -> &FxHashMap<Value, Arc<FxHashMap<Interned, Value>>> {
        self.props.as_ref().expect("missing heap properties")
    }

    fn props_mut(&mut self) -> &mut FxHashMap<Value, Arc<FxHashMap<Interned, Value>>> {
        self.props.as_mut().expect("missing heap properties")
    }

    pub fn sizeof_object() -> usize {
        std::mem::size_of::<Object>()
    }

    /// Pause garbage collection.
    /// Each pause_gc must have an unpause_gc before GC resumes (it is a counter that must be 0).
    pub fn pause_gc(&mut self) {
        self.paused += 1;
    }

    /// UnPause garbage collection.
    /// Each pause_gc must have an unpause_gc before GC resumes (it is a counter that must be 0).
    pub fn unpause_gc(&mut self) {
        self.paused -= 1;
    }

    pub fn set_grow_factor(&mut self, grow_factor: f64) {
        self.objects.set_grow_factor(grow_factor);
    }

    fn alloc<MarkFunc>(&mut self, obj: Object, flags: u8, mark_roots: MarkFunc) -> Handle
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        if self.objects.live_objects() >= self.objects.capacity() && self.paused == 0 {
            self.collect(mark_roots);
        }
        Handle::new32(self.objects.alloc(obj, flags))
    }

    pub fn alloc_u64<MarkFunc>(
        &mut self,
        num: u64,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        if self.numerics.live_objects() >= self.numerics.capacity() && self.paused == 0 {
            self.collect(mark_roots);
        }
        let num = Numeric64 { uint: num };
        Value::Int64(Numeric::Heap(
            self.numerics.alloc(num, mutable.flag()).into(),
        ))
    }

    pub fn alloc_i64<MarkFunc>(
        &mut self,
        num: i64,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        if self.numerics.live_objects() >= self.numerics.capacity() && self.paused == 0 {
            self.collect(mark_roots);
        }
        let num = Numeric64 { int: num };
        Value::UInt64(Numeric::Heap(
            self.numerics.alloc(num, mutable.flag()).into(),
        ))
    }

    pub fn alloc_f64<MarkFunc>(
        &mut self,
        num: f64,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        if self.numerics.live_objects() >= self.numerics.capacity() && self.paused == 0 {
            self.collect(mark_roots);
        }
        let num = Numeric64 { float: num };
        Value::Float64(Numeric::Heap(
            self.numerics.alloc(num, mutable.flag()).into(),
        ))
    }

    pub fn alloc_pair<MarkFunc>(
        &mut self,
        car: Value,
        cdr: Value,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::Pair(self.alloc(
            Object::Pair(Arc::new((car, cdr))),
            mutable.flag(),
            mark_roots,
        ))
    }

    pub fn alloc_string<MarkFunc>(
        &mut self,
        s: String,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::String(self.alloc(Object::String(Arc::new(s)), mutable.flag(), mark_roots))
    }

    pub fn alloc_vector<MarkFunc>(
        &mut self,
        v: Vec<Value>,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::Vector(self.alloc(Object::Vector(Arc::new(v)), mutable.flag(), mark_roots))
        //Value::Vector(self.alloc(Object::Vector(v), mutable.flag(), mark_roots))
    }

    pub fn alloc_persistent_vector<MarkFunc>(
        &mut self,
        v: PersistentVec,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::PersistentVec(self.alloc(
            Object::PersistentVec(Arc::new(v)),
            mutable.flag(),
            mark_roots,
        ))
    }

    pub(crate) fn alloc_vecnode<MarkFunc>(&mut self, node: VecNode, mark_roots: MarkFunc) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::VecNode(self.alloc(Object::VecNode(Arc::new(node)), FLAG_MUT, mark_roots))
    }

    pub fn alloc_persistent_map<MarkFunc>(
        &mut self,
        v: PersistentMap,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::PersistentMap(self.alloc(
            Object::PersistentMap(Arc::new(v)),
            mutable.flag(),
            mark_roots,
        ))
    }

    pub(crate) fn alloc_mapnode<MarkFunc>(&mut self, node: MapNode, mark_roots: MarkFunc) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::MapNode(self.alloc(Object::MapNode(Arc::new(node)), FLAG_MUT, mark_roots))
    }

    pub fn alloc_map<MarkFunc>(
        &mut self,
        map: HashMap<Value, Value>,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::Map(self.alloc(Object::Map(Arc::new(map)), mutable.flag(), mark_roots))
    }

    pub fn alloc_bytes<MarkFunc>(
        &mut self,
        v: Vec<u8>,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::Bytes(self.alloc(Object::Bytes(Arc::new(v)), mutable.flag(), mark_roots))
    }

    pub fn alloc_lambda<MarkFunc>(&mut self, l: Arc<Chunk>, mark_roots: MarkFunc) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::Lambda(self.alloc(Object::Lambda(l), 0, mark_roots))
    }

    pub fn alloc_closure<MarkFunc>(
        &mut self,
        l: Arc<Chunk>,
        v: Vec<Handle>,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::Closure(self.alloc(Object::Closure(l, Arc::new(v)), 0, mark_roots))
    }

    pub fn alloc_continuation<MarkFunc>(&mut self, k: Continuation, mark_roots: MarkFunc) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::Continuation(self.alloc(Object::Continuation(Arc::new(k)), 0, mark_roots))
    }

    pub fn alloc_callframe<MarkFunc>(&mut self, frame: CallFrame, mark_roots: MarkFunc) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::CallFrame(self.alloc(Object::CallFrame(Arc::new(frame)), 0, mark_roots))
    }

    pub fn alloc_value<MarkFunc>(
        &mut self,
        val: Value,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::Value(self.alloc(Object::Value(val), mutable.flag(), mark_roots))
    }

    pub fn alloc_error<MarkFunc>(
        &mut self,
        error: Error,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        if self.errors.live_objects() >= self.errors.capacity() && self.paused == 0 {
            self.collect(mark_roots);
        }
        Value::Error(self.errors.alloc(error, mutable.flag()).into())
    }

    pub fn get_int(&self, handle: Numeric64Handle) -> i64 {
        unsafe {
            if let Some(Numeric64 { int }) = self.numerics.get(handle.as_usize()) {
                *int
            } else {
                panic!("Handle {handle} is not a valid int!");
            }
        }
    }

    pub fn get_int_mut(&mut self, handle: Numeric64Handle) -> &mut i64 {
        unsafe {
            if let Some(Numeric64 { int }) = self.numerics.get_mut(handle.as_usize()) {
                int
            } else {
                panic!("Handle {handle} is not a valid int!");
            }
        }
    }

    pub fn get_uint(&self, handle: Numeric64Handle) -> u64 {
        unsafe {
            if let Some(Numeric64 { uint }) = self.numerics.get(handle.as_usize()) {
                *uint
            } else {
                panic!("Handle {handle} is not a valid uint!");
            }
        }
    }

    pub fn get_uint_mut(&mut self, handle: Numeric64Handle) -> &mut u64 {
        unsafe {
            if let Some(Numeric64 { uint }) = self.numerics.get_mut(handle.as_usize()) {
                uint
            } else {
                panic!("Handle {handle} is not a valid uint!");
            }
        }
    }

    pub fn get_float(&self, handle: Numeric64Handle) -> f64 {
        unsafe {
            if let Some(Numeric64 { float }) = self.numerics.get(handle.as_usize()) {
                *float
            } else {
                panic!("Handle {handle} is not a valid float!");
            }
        }
    }

    pub fn get_float_mut(&mut self, handle: Numeric64Handle) -> &mut f64 {
        unsafe {
            if let Some(Numeric64 { float }) = self.numerics.get_mut(handle.as_usize()) {
                float
            } else {
                panic!("Handle {handle} is not a valid float!");
            }
        }
    }

    pub fn get_string(&self, handle: Handle) -> &str {
        if let Some(Object::String(ptr)) = self.objects.get(handle.idx()) {
            ptr
        } else {
            panic!("Handle {} is not a string!", handle.idx());
        }
    }

    pub fn get_string_mut(&mut self, handle: Handle) -> &mut String {
        if let Some(Object::String(ptr)) = self.objects.get_mut(handle.idx()) {
            Arc::make_mut(ptr)
        } else {
            panic!("Handle {} is not a string!", handle.idx());
        }
    }

    pub fn get_vector(&self, handle: Handle) -> &[Value] {
        if let Some(Object::Vector(v)) = self.objects.get(handle.idx()) {
            v
        } else {
            panic!("Handle {} is not a vector!", handle.idx());
        }
    }

    pub fn get_vector_mut(&mut self, handle: Handle) -> VMResult<&mut Vec<Value>> {
        if !self.objects.is_mutable(handle.idx()) {
            return Err(VMError::new_heap("Vector is not mutable!"));
        }
        if let Some(Object::Vector(v)) = self.objects.get_mut(handle.idx()) {
            Ok(Arc::make_mut(v))
        } else {
            panic!("Handle {} is not a vector!", handle.idx());
        }
    }

    pub(crate) fn get_persistent_vector(&self, handle: Handle) -> &PersistentVec {
        if let Some(Object::PersistentVec(vec)) = self.objects.get(handle.idx()) {
            vec
        } else {
            panic!("Handle {} is not a persistent vector!", handle.idx());
        }
    }

    pub(crate) fn get_vecnode(&self, handle: Handle) -> &VecNode {
        if let Some(Object::VecNode(node)) = self.objects.get(handle.idx()) {
            node
        } else {
            panic!("Handle {} is not a vector node!", handle.idx());
        }
    }

    /*pub(crate) fn _get_vecnode_mut(&mut self, handle: Handle) -> VMResult<&mut VecNode> {
        if !self.is_mutable(handle) {
            return Err(VMError::new_heap("VecNode is not mutable!"));
        }
        if let Some(Object::VecNode(node)) = self.objects.get_mut(handle.idx()) {
            Ok(node)
        } else {
            panic!("Handle {} is not a vector node!", handle.idx());
        }
    }*/

    pub(crate) fn _get_persistent_map(&self, handle: Handle) -> &PersistentMap {
        if let Some(Object::PersistentMap(map)) = self.objects.get(handle.idx()) {
            map
        } else {
            panic!("Handle {} is not a persistent map!", handle.idx());
        }
    }

    pub(crate) fn get_mapnode(&self, handle: Handle) -> &MapNode {
        if let Some(Object::MapNode(node)) = self.objects.get(handle.idx()) {
            node
        } else {
            panic!("Handle {} is not a map node!", handle.idx());
        }
    }

    pub fn get_map(&self, handle: Handle) -> &HashMap<Value, Value> {
        if let Some(Object::Map(map)) = self.objects.get(handle.idx()) {
            map
        } else {
            panic!("Handle {} is not a map!", handle.idx());
        }
    }

    pub fn get_map_mut(&mut self, handle: Handle) -> VMResult<&mut HashMap<Value, Value>> {
        if !self.objects.is_mutable(handle.idx()) {
            return Err(VMError::new_heap("Map is not mutable!"));
        }
        if let Some(Object::Map(map)) = self.objects.get_mut(handle.idx()) {
            Ok(Arc::make_mut(map))
        } else {
            panic!("Handle {} is not a map!", handle.idx());
        }
    }

    pub fn get_bytes(&self, handle: Handle) -> &[u8] {
        if let Some(Object::Bytes(v)) = self.objects.get(handle.idx()) {
            v
        } else {
            panic!("Handle {} is not bytes!", handle.idx());
        }
    }

    pub fn get_pair(&self, handle: Handle) -> (Value, Value) {
        if let Some(Object::Pair(ptr)) = self.objects.get(handle.idx()) {
            (ptr.0, ptr.1)
        } else {
            panic!("Handle {} is not a pair!", handle.idx());
        }
    }

    pub fn get_pair_mut(&mut self, handle: Handle) -> VMResult<(&mut Value, &mut Value)> {
        if !self.objects.is_mutable(handle.idx()) {
            return Err(VMError::new_heap("Pair is not mutable!"));
        }
        if let Some(Object::Pair(ptr)) = self.objects.get_mut(handle.idx()) {
            let data = Arc::make_mut(ptr);
            Ok((&mut data.0, &mut data.1))
        } else {
            panic!("Handle {} is not a pair!", handle.idx());
        }
    }

    pub fn get_pair_mut_override(&mut self, handle: Handle) -> (&mut Value, &mut Value) {
        if let Some(Object::Pair(ptr)) = self.objects.get_mut(handle.idx()) {
            let data = Arc::make_mut(ptr);
            (&mut data.0, &mut data.1)
        } else {
            panic!("Handle {} is not a pair!", handle.idx());
        }
    }

    pub fn get_lambda(&self, handle: Handle) -> Arc<Chunk> {
        if let Some(Object::Lambda(lambda)) = self.objects.get(handle.idx()) {
            lambda.clone()
        } else {
            panic!("Handle {} is not a lambda!", handle.idx());
        }
    }

    pub fn get_closure(&self, handle: Handle) -> (Arc<Chunk>, &[Handle]) {
        if let Some(Object::Closure(lambda, captures)) = self.objects.get(handle.idx()) {
            (lambda.clone(), captures)
        } else {
            panic!("Handle {} is not a closure!", handle.idx());
        }
    }

    pub fn get_closure_captures(&self, handle: Handle) -> &[Handle] {
        if let Some(Object::Closure(_, captures)) = self.objects.get(handle.idx()) {
            captures
        } else {
            panic!("Handle {} is not a closure!", handle.idx());
        }
    }

    pub fn get_continuation(&self, handle: Handle) -> &Continuation {
        if let Some(Object::Continuation(cont)) = self.objects.get(handle.idx()) {
            cont
        } else {
            panic!("Handle {} is not a continuation!", handle.idx());
        }
    }

    pub fn get_callframe(&self, handle: Handle) -> &CallFrame {
        if let Some(Object::CallFrame(call_frame)) = self.objects.get(handle.idx()) {
            call_frame
        } else {
            panic!("Handle {} is not a continuation!", handle.idx());
        }
    }

    pub fn get_value(&self, handle: Handle) -> Value {
        if let Some(Object::Value(value)) = self.objects.get(handle.idx()) {
            //if let Object::Value(value) = self.objects[handle.idx()] {
            *value
        } else {
            panic!("Handle {} is not a value!", handle.idx());
        }
    }

    pub fn get_value_mut(&mut self, handle: Handle) -> &mut Value {
        if let Some(Object::Value(value)) = self.objects.get_mut(handle.idx()) {
            value
        } else {
            panic!("Handle {} is not a value!", handle.idx());
        }
    }

    pub fn get_error(&self, handle: Handle) -> Error {
        if let Some(error) = self.errors.get(handle.idx()) {
            *error
        } else {
            panic!("Handle {} is not an error!", handle.idx());
        }
    }

    /// If val is o the heap is it still alive after GC
    /// Return true if val is not a heap object.
    pub fn is_live(&self, val: Value) -> bool {
        value_op!(self, val, is_live, true)
    }

    pub fn immutable(&mut self, val: Value) {
        value_op!(self, val, immutable, ());
    }

    pub fn sticky(&mut self, val: Value) {
        value_op!(self, val, sticky, ());
    }

    pub fn unsticky(&mut self, val: Value) {
        value_op!(self, val, unsticky, ());
    }

    pub fn is_traced_and_set(&mut self, val: Value) -> bool {
        value_op!(self, val, is_traced_and_set, true)
    }

    pub fn mark(&mut self, value: Value) {
        mark!(self, value);
    }

    fn mark_trace(&mut self, val: Value) {
        mark!(self, val);
        self.greys.push(val);
    }

    fn mark_chunk(&mut self, chunk: &Chunk) {
        for constant in &chunk.constants {
            self.mark_trace(*constant);
        }
    }

    fn mark_call_frame(&mut self, call_frame: &CallFrame) {
        self.mark_chunk(&call_frame.chunk);
        if let Some(this_fn) = call_frame.this_fn {
            self.mark_trace(this_fn);
        }
        for defer in &call_frame.defers {
            self.mark_trace(*defer);
        }
        if let Some(on_error) = call_frame.on_error {
            self.mark_trace(on_error);
        }
        self.mark_trace(call_frame.called);
    }

    fn trace_object(&mut self, obj: &Object) {
        match obj {
            Object::String(_) => {}
            Object::Vector(vec) => {
                for v in vec.iter() {
                    self.mark_trace(*v);
                }
            }
            Object::Map(map) => {
                for (key, val) in map.iter() {
                    self.mark_trace(*key);
                    self.mark_trace(*val);
                }
            }
            Object::Bytes(_) => {}
            Object::Pair(data) => {
                self.mark_trace(data.0);
                self.mark_trace(data.1);
            }
            Object::Lambda(chunk) => self.mark_chunk(chunk),
            Object::Closure(chunk, closures) => {
                self.mark_chunk(chunk);
                for close in closures.iter() {
                    self.mark_trace(Value::Value(*close));
                }
            }
            Object::Continuation(continuation) => {
                self.mark_call_frame(&continuation.frame);
                for obj in &continuation.stack {
                    self.mark_trace(*obj);
                }
            }
            Object::CallFrame(call_frame) => self.mark_call_frame(call_frame),
            Object::Value(val) => {
                self.mark_trace(*val);
            }
            Object::PersistentVec(pvec) => {
                if let Some(root) = pvec.root() {
                    if let Some(nodes) = root.nodes() {
                        nodes
                            .iter()
                            .filter(|n| !n.is_undef())
                            .for_each(|val| self.mark_trace(*val));
                    }
                    if let Some(leaf) = root.leaf() {
                        leaf.iter().for_each(|val| self.mark_trace(*val));
                    }
                    pvec.tail().iter().for_each(|val| self.mark_trace(*val));
                }
            }
            Object::VecNode(node) => {
                if let Some(nodes) = node.nodes() {
                    nodes
                        .iter()
                        .filter(|n| !n.is_undef())
                        .for_each(|val| self.mark_trace(*val));
                }
                if let Some(leaf) = node.leaf() {
                    leaf.iter().for_each(|val| self.mark_trace(*val));
                }
            }
            Object::PersistentMap(_pmap) => {} // TODO- trace me!
            Object::MapNode(_node) => {}       // TODO- trace me!
            Object::Empty => panic!("An empty object can not be live!"),
        }
    }

    fn trace(&mut self, val: Value) {
        let props = self.props.take().expect("missing heap props");
        if let Some(props) = props.get(&val) {
            // Make sure we don't do anything that can access self.props here since that will panic...
            // trace any properties for val.
            for val in props.values() {
                self.mark_trace(*val);
            }
        }
        self.props = Some(props);
        match val {
            Value::CharClusterLong(handle)
            | Value::String(handle)
            | Value::Vector(handle)
            | Value::PersistentVec(handle)
            | Value::PersistentMap(handle)
            | Value::VecNode(handle)
            | Value::MapNode(handle)
            | Value::Map(handle)
            | Value::Bytes(handle)
            | Value::Pair(handle)
            | Value::List(handle, _)
            | Value::Lambda(handle)
            | Value::Closure(handle)
            | Value::Continuation(handle)
            | Value::CallFrame(handle)
            | Value::Value(handle) => {
                let obj = self
                    .objects
                    .get(handle.idx())
                    .expect("Invalid object handle!")
                    .clone();
                self.trace_object(&obj);
            }

            Value::Error(handle) => {
                let err = self
                    .errors
                    .get(handle.idx())
                    .expect("Invalid error handle!");
                self.mark_trace(err.data);
            }

            Value::Int64(_)
            | Value::UInt64(_)
            | Value::Float64(_)
            | Value::Byte(_)
            | Value::Int32(_)
            | Value::UInt32(_)
            | Value::CodePoint(_)
            | Value::CharCluster(_, _)
            | Value::Symbol(_)
            | Value::Keyword(_)
            | Value::StringConst(_)
            | Value::Special(_)
            | Value::Builtin(_)
            | Value::True
            | Value::False
            | Value::Nil
            | Value::Undefined => {}
        }
    }

    fn collect<MarkFunc>(&mut self, mut mark_roots: MarkFunc)
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        self.objects.clear_marks();
        self.numerics.clear_marks();
        self.errors.clear_marks();
        mark_roots(self).expect("Failed to mark the roots!");
        let mut objs = Vec::new();
        self.objects.trace_all_live(|obj| {
            // this cloning is not great...
            objs.push(obj.clone());
        });
        for obj in &objs {
            self.trace_object(obj);
        }
        while let Some(val) = self.greys.pop() {
            if !self.is_traced_and_set(val) {
                self.trace(val);
            }
        }
        // Sweep out collected properties.
        let mut props = self.props.take().expect("missing heap props");
        props.retain(|key, _val| self.is_live(*key));
        self.props = Some(props);
        self.objects.set_all_dead(Object::Empty);
    }

    pub fn capacity(&self) -> usize {
        self.objects.capacity()
    }

    pub fn live_objects(&self) -> usize {
        self.objects.live_objects() + self.numerics.live_objects()
    }

    pub fn get_property(&self, value: Value, prop: Interned) -> Option<Value> {
        if let Some(map) = self.props().get(&value) {
            if let Some(val) = map.get(&prop) {
                return Some(*val);
            }
        }
        None
    }

    pub fn set_property(&mut self, key_value: Value, prop: Interned, value: Value) {
        if let Some(map) = self.props_mut().get_mut(&key_value) {
            let map = Arc::make_mut(map);
            map.insert(prop, value);
        } else {
            let mut map = FxHashMap::default();
            map.insert(prop, value);
            self.props_mut().insert(key_value, Arc::new(map));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn _test_send_sync<T>(_t: T)
    where
        T: Send + Sync,
    {
    }

    //#[test]
    //fn test_obj_send_sync() {
    //    test_send_sync(Object::Value(Value::Nil));
    //}

    #[test]
    fn test_basic() -> VMResult<()> {
        let mut heap = Heap::default();
        let mark_roots = |_heap: &mut Heap| -> VMResult<()> { Ok(()) };
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 0);
        for x in 0..512 {
            heap.alloc_pair(Value::Int32(x), Value::Nil, MutState::Mutable, mark_roots);
        }
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 512);
        for x in 0..512 {
            if let (Value::Int32(v), Value::Nil) = heap.get_pair(Handle::new(x)) {
                assert!(x == v as usize);
            } else {
                panic!();
            }
        }
        heap.alloc_pair(Value::Int32(512), Value::Nil, MutState::Mutable, mark_roots);
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 1);
        if let (Value::Int32(v), Value::Nil) = heap.get_pair(Handle::new(0)) {
            assert!(512 == v);
        } else {
            panic!();
        }
        let mark_roots = |heap: &mut Heap| -> VMResult<()> {
            for idx in 0..512 {
                heap.mark(Value::Pair(Handle::new(idx)));
            }
            Ok(())
        };
        for x in 0..512 {
            heap.alloc_pair(Value::Int32(x), Value::Nil, MutState::Mutable, mark_roots);
        }
        assert!(heap.capacity() == 1024);
        assert!(heap.live_objects() == 513);
        for x in 0..513 {
            if let (Value::Int32(v), Value::Nil) = heap.get_pair(Handle::new(x)) {
                if x == 0 {
                    assert!(512 == v);
                } else {
                    assert!(x - 1 == v as usize);
                }
            } else {
                panic!();
            }
        }
        let mark_roots = |heap: &mut Heap| -> VMResult<()> {
            for idx in 0..513 {
                if idx % 2 == 0 {
                    heap.mark(Value::Pair(Handle::new(idx)));
                }
            }
            Ok(())
        };
        heap.collect(mark_roots);
        assert!(heap.capacity() == 1024);
        assert!(heap.live_objects() == 257);
        let mark_roots = |_heap: &mut Heap| -> VMResult<()> { Ok(()) };
        heap.collect(mark_roots);
        assert!(heap.capacity() == 1024);
        assert!(heap.live_objects() == 0);
        for x in 0..512 {
            let h = heap.alloc_pair(Value::Int32(x), Value::Nil, MutState::Mutable, mark_roots);
            heap.sticky(h);
        }
        heap.collect(mark_roots);
        assert!(heap.capacity() == 1024);
        assert!(heap.live_objects() == 512);
        for x in 512..1024 {
            let _h = heap.alloc_pair(Value::Int32(x), Value::Nil, MutState::Mutable, mark_roots);
        }
        let mark_roots = |heap: &mut Heap| -> VMResult<()> {
            for idx in 0..1024 {
                heap.mark(Value::Pair(Handle::new(idx)));
            }
            Ok(())
        };
        heap.collect(mark_roots);
        assert!(heap.capacity() == 1024);
        assert!(heap.live_objects() == 1024);
        heap.alloc_string("steve".into(), MutState::Mutable, mark_roots);
        assert!(heap.capacity() == 2048);
        assert!(heap.live_objects() == 1025);
        Ok(())
    }

    #[test]
    fn test_trace_val() -> VMResult<()> {
        let mut heap = Heap::default();

        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 0);
        let outers = std::rc::Rc::new(std::cell::RefCell::new(vec![]));
        let outers_mark = outers.clone();
        let mark_roots = |heap: &mut Heap| -> VMResult<()> {
            for h in outers_mark.borrow().iter() {
                heap.mark(*h);
            }
            Ok(())
        };
        for x in 0..256 {
            let inner = heap.alloc_pair(Value::Int32(x), Value::Nil, MutState::Mutable, mark_roots);
            outers.borrow_mut().push(heap.alloc_pair(
                inner,
                Value::Nil,
                MutState::Mutable,
                mark_roots,
            ));
        }
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 512);
        for (i, h) in outers.borrow().iter().enumerate() {
            if let (Value::Pair(inner), Value::Nil) = heap.get_pair(h.get_handle().unwrap()) {
                if let (Value::Int32(v), Value::Nil) = heap.get_pair(inner) {
                    assert!(i == v as usize);
                } else {
                    panic!();
                }
            } else {
                panic!();
            }
        }
        heap.collect(mark_roots);
        assert_eq!(heap.capacity(), 512);
        assert_eq!(heap.live_objects(), 512);
        /* XXXSLS
        for h in outers.borrow().iter() {
            //let data = Box::new("bloop".to_string());
            //let d = Box::into_raw(data) as usize;
            heap.replace(h.get_handle().unwrap(), Object::String(Arc::new("bloop".into())));
        }
        heap.collect(mark_roots);
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 256);
        for h in outers.borrow().iter() {
            let sstr = heap.get_string(h.get_handle().unwrap());
            assert!(sstr == "bloop");
            i += 1;
        }
        */
        Ok(())
    }

    #[test]
    fn test_trace_vec() -> VMResult<()> {
        let mut heap = Heap::default();

        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 0);
        let outers = std::rc::Rc::new(std::cell::RefCell::new(vec![]));
        let outers_mark = outers.clone();
        let mark_roots = |heap: &mut Heap| -> VMResult<()> {
            for h in outers_mark.borrow().iter() {
                heap.mark(*h);
            }
            Ok(())
        };
        let mut v = vec![];
        for x in 0..256 {
            let inner = heap.alloc_pair(Value::Int32(x), Value::Nil, MutState::Mutable, mark_roots);
            v.push(inner);
        }
        outers.borrow_mut().push(Value::Vector(heap.alloc(
            Object::Vector(Arc::new(v)),
            MutState::Mutable.flag(),
            mark_roots,
        )));
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 257);
        for h in outers.borrow().iter() {
            let v = heap.get_vector(h.get_handle().unwrap());
            for (i, hv) in v.iter().enumerate() {
                if let Value::Pair(hv) = hv {
                    if let (Value::Int32(v), Value::Nil) = heap.get_pair(*hv) {
                        assert!(i == v as usize);
                    } else {
                        panic!();
                    }
                } else {
                    panic!();
                }
            }
        }
        heap.collect(mark_roots);
        assert_eq!(heap.capacity(), 512);
        assert_eq!(heap.live_objects(), 257);
        for h in outers.borrow().iter() {
            let v = heap.get_vector(h.get_handle().unwrap());
            for (i, hv) in v.iter().enumerate() {
                if let Value::Pair(hv) = hv {
                    if let (Value::Int32(v), Value::Nil) = heap.get_pair(*hv) {
                        assert!(i == v as usize);
                    } else {
                        panic!();
                    }
                } else {
                    panic!();
                }
            }
        }
        /* XXXSLS
        for h in outers.borrow().iter() {
            //let data = Box::new("bloop".to_string());
            //let d = Box::into_raw(data) as usize;
            //heap.replace(*h, Object::StringMut(d));
            heap.replace(h.get_handle().unwrap(), Object::String(Arc::new("bloop".into())));
        }
        heap.collect(mark_roots);
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 1);
        for h in outers.borrow().iter() {
            let sstr = heap.get_string(h.get_handle().unwrap());
            assert!(sstr == "bloop");
        }
        */
        Ok(())
    }

    #[test]
    fn test_trace_pair() -> VMResult<()> {
        let mut heap = Heap::default();
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 0);
        let outers = std::rc::Rc::new(std::cell::RefCell::new(vec![]));
        let outers_mark = outers.clone();
        let mark_roots = |heap: &mut Heap| -> VMResult<()> {
            for h in outers_mark.borrow().iter() {
                heap.mark(*h);
            }
            Ok(())
        };
        outers.borrow_mut().push(heap.alloc_pair(
            Value::Int32(1),
            Value::Int32(2),
            MutState::Mutable,
            mark_roots,
        ));
        let car_h = heap.alloc_pair(Value::Int32(3), Value::Nil, MutState::Mutable, mark_roots);
        let cdr_h = heap.alloc_pair(Value::Int32(4), Value::Nil, MutState::Mutable, mark_roots);
        outers.borrow_mut().push(heap.alloc_pair(
            car_h,
            Value::Int32(2),
            MutState::Mutable,
            mark_roots,
        ));
        outers.borrow_mut().push(heap.alloc_pair(
            Value::Int32(1),
            cdr_h,
            MutState::Mutable,
            mark_roots,
        ));
        outers
            .borrow_mut()
            .push(heap.alloc_pair(car_h, cdr_h, MutState::Mutable, mark_roots));
        assert_eq!(heap.capacity(), 512);
        assert_eq!(heap.live_objects(), 6);
        heap.collect(mark_roots);
        assert_eq!(heap.capacity(), 512);
        assert_eq!(heap.live_objects(), 6);
        for (i, h) in outers.borrow().iter().enumerate() {
            let (car, cdr) = heap.get_pair(h.get_handle().unwrap());
            if i == 0 {
                let (car, cdr) = if let Value::Int32(car) = car {
                    if let Value::Int32(cdr) = cdr {
                        (car, cdr)
                    } else {
                        (car, 0)
                    }
                } else {
                    (0, 0)
                };
                assert!(car == 1);
                assert!(cdr == 2);
            } else if i == 1 {
                let (car, cdr) = if let Value::Pair(car_h) = car {
                    if let (Value::Int32(car), Value::Nil) = heap.get_pair(car_h) {
                        if let Value::Int32(cdr) = cdr {
                            (car, cdr)
                        } else {
                            (car, 0)
                        }
                    } else {
                        (0, 0)
                    }
                } else {
                    (0, 0)
                };
                assert_eq!(car, 3);
                assert_eq!(cdr, 2);
            } else if i == 2 {
                let (car, cdr) = if let Value::Pair(cdr_h) = cdr {
                    if let (Value::Int32(cdr), Value::Nil) = heap.get_pair(cdr_h) {
                        if let Value::Int32(car) = car {
                            (car, cdr)
                        } else {
                            (0, cdr)
                        }
                    } else {
                        (0, 0)
                    }
                } else {
                    (0, 0)
                };
                assert!(car == 1);
                assert!(cdr == 4);
            } else if i == 3 {
                let (car, cdr) = if let Value::Pair(car_h) = car {
                    if let (Value::Int32(car), Value::Nil) = heap.get_pair(car_h) {
                        if let Value::Pair(cdr_h) = cdr {
                            if let (Value::Int32(cdr), Value::Nil) = heap.get_pair(cdr_h) {
                                (car, cdr)
                            } else {
                                (car, 0)
                            }
                        } else {
                            (0, 0)
                        }
                    } else {
                        (0, 0)
                    }
                } else {
                    (0, 0)
                };
                assert!(car == 3);
                assert!(cdr == 4);
            } else {
                panic!()
            }
        }

        Ok(())
    }
}
