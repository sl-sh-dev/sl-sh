use std::collections::HashMap;
use std::sync::Arc;

use crate::chunk::*;
use crate::error::*;
use crate::value::*;

const FLAG_MARK: u8 = 0x01;
const FLAG_TRACE: u8 = 0x02;
const FLAG_STICKY: u8 = 0x04;
const FLAG_MUT: u8 = 0x08;

macro_rules! is_bit_set {
    ($val:expr, $bit:expr) => {{
        ($val & $bit) != 0
    }};
}

macro_rules! set_bit {
    ($val:expr, $bit:expr) => {{
        $val |= $bit;
    }};
}

macro_rules! clear_bit {
    ($val:expr, $bit:expr) => {{
        if is_bit_set!($val, $bit) {
            $val ^= $bit;
        }
    }};
}

fn is_marked(flag: u8) -> bool {
    is_bit_set!(flag, FLAG_MARK | FLAG_STICKY)
}

fn need_trace(flag: u8) -> bool {
    is_marked(flag) && is_bit_set!(flag, FLAG_TRACE)
}

#[derive(Clone, Debug)]
pub struct CallFrame {
    pub id: usize,
    pub chunk: Arc<Chunk>,
    pub ip: usize,
    pub current_ip: usize,
    pub stack_top: usize,
    pub this_fn: Option<Value>,
    pub defers: Vec<Value>,
    pub on_error: Option<Value>,
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
    Bytes(Arc<Vec<u8>>),
    Pair(Arc<(Value, Value)>),
    Value(Value),
    // CallFrame can be mutable for internal purposes.
    CallFrame(Arc<CallFrame>),
    // Everything below here is always read only.
    Lambda(Arc<Chunk>),
    Macro(Arc<Chunk>),
    Closure(Arc<Chunk>, Arc<Vec<Handle>>),
    Continuation(Arc<Continuation>),
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Handle {
    idx: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct HeapStats {
    live_objects: usize,
    sticky_objects: usize,
    //string_bytes: usize,
    //vec_bytes: usize,
    //byte_bytes: usize,
}

impl HeapStats {
    pub fn new() -> Self {
        HeapStats {
            live_objects: 0,
            sticky_objects: 0,
            //string_bytes: 0,
            //vec_bytes: 0,
            //byte_bytes: 0,
        }
    }

    pub fn live_objects(&self) -> usize {
        self.live_objects
    }
}

impl Default for HeapStats {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
pub struct Heap {
    flags: Vec<u8>,
    objects: Vec<Object>,
    props: HashMap<Handle, Arc<HashMap<&'static str, Value>>>,
    greys: Vec<usize>,
    grow_factor: f64,
    capacity: usize,
    stats: HeapStats,
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            flags: Vec::with_capacity(512),
            // Keep one extra slot to do swap on replace.
            objects: Vec::with_capacity(512 + 1),
            props: HashMap::new(),
            greys: vec![],
            grow_factor: 2.0,
            capacity: 512,
            stats: HeapStats::default(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Heap {
            flags: Vec::with_capacity(capacity),
            // Keep one extra slot to do sway on replace.
            objects: Vec::with_capacity(capacity + 1),
            props: HashMap::new(),
            greys: vec![],
            grow_factor: 2.0,
            capacity,
            stats: HeapStats::default(),
        }
    }

    pub fn set_grow_factor(&mut self, grow_factor: f64) {
        self.grow_factor = grow_factor;
    }

    fn alloc<MarkFunc>(&mut self, obj: Object, flags: u8, mark_roots: MarkFunc) -> Handle
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        if self.stats.live_objects() >= self.capacity() {
            self.collect(mark_roots);
            let new_min = (self.stats.live_objects() as f64 * self.grow_factor) as usize;
            if new_min > self.capacity() {
                self.capacity = new_min;
                self.flags.reserve(new_min - self.flags.len());
                self.objects.reserve((new_min - self.objects.len()) + 1);
            }
        }
        if self.objects.len() < self.capacity() {
            let idx = self.objects.len();
            self.objects.push(obj);
            self.flags.push(flags | FLAG_MARK);
            self.stats.live_objects += 1;
            Handle { idx }
        } else {
            for (idx, flag) in self.flags.iter_mut().enumerate() {
                if !is_marked(*flag) {
                    self.stats.live_objects += 1;
                    *flag = flags | FLAG_MARK;
                    self.objects.push(obj);
                    self.objects.swap_remove(idx);
                    return Handle { idx };
                }
            }
            panic!("Failed to allocate to heap- no free objects and no capacity!");
        }
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
            mutable.flag() | FLAG_TRACE,
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
        Value::Vector(self.alloc(
            Object::Vector(Arc::new(v)),
            mutable.flag() | FLAG_TRACE,
            mark_roots,
        ))
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

    pub fn alloc_lambda<MarkFunc>(
        &mut self,
        l: Arc<Chunk>,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::Lambda(self.alloc(Object::Lambda(l), mutable.flag() | FLAG_TRACE, mark_roots))
    }

    pub fn alloc_macro<MarkFunc>(
        &mut self,
        l: Arc<Chunk>,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::Macro(self.alloc(Object::Macro(l), mutable.flag() | FLAG_TRACE, mark_roots))
    }

    pub fn alloc_closure<MarkFunc>(
        &mut self,
        l: Arc<Chunk>,
        v: Vec<Handle>,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::Closure(self.alloc(
            Object::Closure(l, Arc::new(v)),
            mutable.flag() | FLAG_TRACE,
            mark_roots,
        ))
    }

    pub fn alloc_continuation<MarkFunc>(
        &mut self,
        k: Continuation,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::Continuation(self.alloc(
            Object::Continuation(Arc::new(k)),
            mutable.flag() | FLAG_TRACE,
            mark_roots,
        ))
    }

    pub fn alloc_callframe<MarkFunc>(
        &mut self,
        frame: CallFrame,
        mutable: MutState,
        mark_roots: MarkFunc,
    ) -> Value
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        Value::CallFrame(self.alloc(
            Object::CallFrame(Arc::new(frame)),
            mutable.flag() | FLAG_TRACE,
            mark_roots,
        ))
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
        Value::Value(self.alloc(Object::Value(val), mutable.flag() | FLAG_TRACE, mark_roots))
    }

    pub fn get_string(&self, handle: Handle) -> &str {
        if let Some(Object::String(ptr)) = self.objects.get(handle.idx) {
            &*ptr
        } else {
            panic!("Handle {} is not a string!", handle.idx);
        }
    }

    pub fn get_string_mut(&mut self, handle: Handle) -> &mut String {
        if let Some(Object::String(ptr)) = self.objects.get_mut(handle.idx) {
            Arc::make_mut(ptr)
        } else {
            panic!("Handle {} is not a string!", handle.idx);
        }
    }

    pub fn get_vector(&self, handle: Handle) -> &[Value] {
        if let Some(Object::Vector(v)) = self.objects.get(handle.idx) {
            v
        } else {
            panic!("Handle {} is not a vector!", handle.idx);
        }
    }

    pub fn get_vector_mut(&mut self, handle: Handle) -> &mut Vec<Value> {
        if let Some(Object::Vector(v)) = self.objects.get_mut(handle.idx) {
            Arc::make_mut(v)
        } else {
            panic!("Handle {} is not a vector!", handle.idx);
        }
    }

    pub fn get_bytes(&self, handle: Handle) -> &[u8] {
        if let Some(Object::Bytes(v)) = self.objects.get(handle.idx) {
            v
        } else {
            panic!("Handle {} is not bytes!", handle.idx);
        }
    }

    pub fn get_pair(&self, handle: Handle) -> (Value, Value) {
        if let Some(Object::Pair(ptr)) = self.objects.get(handle.idx) {
            (ptr.0, ptr.1)
        } else {
            panic!("Handle {} is not a pair!", handle.idx);
        }
    }

    pub fn get_pair_mut(&mut self, handle: Handle) -> (&mut Value, &mut Value) {
        if let Some(Object::Pair(ptr)) = self.objects.get_mut(handle.idx) {
            let data = Arc::make_mut(ptr);
            (&mut data.0, &mut data.1)
        } else {
            panic!("Handle {} is not a pair!", handle.idx);
        }
    }

    pub fn get_lambda(&self, handle: Handle) -> Arc<Chunk> {
        if let Some(Object::Lambda(lambda)) = self.objects.get(handle.idx) {
            lambda.clone()
        } else {
            panic!("Handle {} is not a lambda!", handle.idx);
        }
    }

    pub fn get_macro(&self, handle: Handle) -> Arc<Chunk> {
        if let Some(Object::Macro(lambda)) = self.objects.get(handle.idx) {
            lambda.clone()
        } else {
            panic!("Handle {} is not a macro!", handle.idx);
        }
    }

    pub fn get_closure(&self, handle: Handle) -> (Arc<Chunk>, &[Handle]) {
        if let Some(Object::Closure(lambda, captures)) = self.objects.get(handle.idx) {
            (lambda.clone(), captures)
        } else {
            panic!("Handle {} is not a closure!", handle.idx);
        }
    }

    pub fn get_continuation(&self, handle: Handle) -> &Continuation {
        if let Some(Object::Continuation(cont)) = self.objects.get(handle.idx) {
            cont
        } else {
            panic!("Handle {} is not a continuation!", handle.idx);
        }
    }

    pub fn get_callframe(&self, handle: Handle) -> &CallFrame {
        if let Some(Object::CallFrame(call_frame)) = self.objects.get(handle.idx) {
            call_frame
        } else {
            panic!("Handle {} is not a continuation!", handle.idx);
        }
    }

    pub fn get_callframe_mut(&mut self, handle: Handle) -> &mut CallFrame {
        if let Some(Object::CallFrame(call_frame)) = self.objects.get_mut(handle.idx) {
            Arc::make_mut(call_frame)
        } else {
            panic!("Handle {} is not a continuation!", handle.idx);
        }
    }

    pub fn get_value(&self, handle: Handle) -> Value {
        if let Some(Object::Value(value)) = self.objects.get(handle.idx) {
            *value
        } else {
            panic!("Handle {} is not a value!", handle.idx);
        }
    }

    pub fn get_value_mut(&mut self, handle: Handle) -> &mut Value {
        if let Some(Object::Value(value)) = self.objects.get_mut(handle.idx) {
            value
        } else {
            panic!("Handle {} is not a value!", handle.idx);
        }
    }

    // Used for a couple of tests, DO NOT try this on real code you WILL break the heap.
    #[cfg(test)]
    fn replace(&mut self, handle: Handle, obj: Object) -> Object {
        self.objects.push(obj);
        let old = self.objects.swap_remove(handle.idx);
        self.flags[handle.idx] = self.flags[handle.idx] & 0x0f;
        old
    }

    pub fn is_marked(&self, handle: Handle) -> bool {
        if let Some(flag) = self.flags.get(handle.idx) {
            is_marked(*flag)
        } else {
            false
        }
    }

    pub fn mark(&mut self, handle: Handle) {
        if let Some(flag) = self.flags.get_mut(handle.idx) {
            if !is_marked(*flag) {
                self.stats.live_objects += 1;
                set_bit!(*flag, FLAG_MARK);
            }
        } else {
            panic!("Invalid object handle in mark!")
        }
    }

    pub fn sticky(&mut self, handle: Handle) {
        if let Some(flag) = self.flags.get_mut(handle.idx) {
            if !is_bit_set!(*flag, FLAG_STICKY) {
                self.stats.sticky_objects += 1;
                self.stats.live_objects += 1;
                set_bit!(*flag, FLAG_STICKY);
            }
        } else {
            panic!("Invalid object handle in sticky!")
        }
    }

    pub fn unsticky(&mut self, handle: Handle) {
        if let Some(flag) = self.flags.get_mut(handle.idx) {
            if is_bit_set!(*flag, FLAG_STICKY) {
                self.stats.sticky_objects -= 1;
                self.stats.live_objects -= 1;
                clear_bit!(*flag, FLAG_STICKY);
            }
        } else {
            panic!("Invalid object handle in unsticky!")
        }
    }

    // mark_trace has an invariant to maintain, do not touch objects (see unsafe in
    // trace below).
    fn mark_trace(&mut self, handle: Handle, current: usize) {
        if !self.is_marked(handle) {
            self.mark(handle);
            if handle.idx < current {
                self.greys.push(handle.idx);
            }
        }
    }

    fn mark_chunk(&mut self, chunk: &Chunk, current: usize) {
        for constant in &chunk.constants {
            if let Some(handle) = constant.get_handle() {
                self.mark_trace(handle, current);
            }
        }
    }

    fn mark_call_frame(&mut self, call_frame: &CallFrame, current: usize) {
        self.mark_chunk(&call_frame.chunk, current);
        if let Some(this_fn) = call_frame.this_fn {
            if let Some(handle) = this_fn.get_handle() {
                self.mark_trace(handle, current);
            }
        }
        for defer in &call_frame.defers {
            if let Some(handle) = defer.get_handle() {
                self.mark_trace(handle, current);
            }
        }
        if let Some(on_error) = call_frame.on_error {
            if let Some(handle) = on_error.get_handle() {
                self.mark_trace(handle, current);
            }
        }
    }

    fn trace(&mut self, idx: usize, current: usize) {
        // This unsafe avoids cloning the object to avoid having a mutable and immutable self.
        // This should be fine because we are not touching objects in a mark, only flags.
        // idx should also have been validated before it gets here (by mark if nothing else).
        let obj = unsafe { &*(self.objects.get_unchecked(idx) as *const Object) };
        match obj {
            Object::String(_) => {}
            Object::Vector(vec) => {
                for v in vec.iter() {
                    if let Some(h) = v.get_handle() {
                        let h = h;
                        self.mark_trace(h, current);
                    }
                }
            }
            Object::Bytes(_) => {}
            Object::Pair(data) => match (data.0.get_handle(), data.1.get_handle()) {
                (Some(car), Some(cdr)) => {
                    self.mark_trace(car, current);
                    self.mark_trace(cdr, current);
                }
                (Some(car), None) => self.mark_trace(car, current),
                (None, Some(cdr)) => self.mark_trace(cdr, current),
                (None, None) => {}
            },
            Object::Lambda(chunk) => self.mark_chunk(chunk, current),
            Object::Macro(chunk) => self.mark_chunk(chunk, current),
            Object::Closure(chunk, closures) => {
                self.mark_chunk(chunk, current);
                for close in closures.iter() {
                    self.mark_trace(*close, current);
                }
            }
            Object::Continuation(continuation) => {
                self.mark_call_frame(&continuation.frame, current);
                for obj in &continuation.stack {
                    if let Some(handle) = obj.get_handle() {
                        self.mark_trace(handle, current);
                    }
                }
            }
            Object::CallFrame(call_frame) => self.mark_call_frame(call_frame, current),
            Object::Value(val) => {
                if let Some(handle) = val.get_handle() {
                    self.mark_trace(handle, current);
                }
            }
        }
    }

    fn collect<MarkFunc>(&mut self, mut mark_roots: MarkFunc)
    where
        MarkFunc: FnMut(&mut Heap) -> VMResult<()>,
    {
        self.stats.live_objects = self.stats.sticky_objects;
        for flag in self.flags.iter_mut() {
            clear_bit!(*flag, FLAG_MARK);
        }
        mark_roots(self).expect("Failed to mark the roots!");
        let mut cur = 0;
        //for (cur, flag) in self.flags.iter().enumerate() {
        let mut val = self.flags.get(cur);
        while let Some(flag) = val {
            if need_trace(*flag) {
                self.trace(cur, cur);
                while let Some(idx) = self.greys.pop() {
                    self.trace(idx, cur);
                }
            }
            cur += 1;
            val = self.flags.get(cur);
        }
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    pub fn live_objects(&self) -> usize {
        self.stats.live_objects()
    }

    pub fn get_property(&self, handle: Handle, prop: &str) -> Option<Value> {
        if let Some(map) = self.props.get(&handle) {
            if let Some(val) = map.get(prop) {
                return Some(*val);
            }
        }
        None
    }

    pub fn set_property(&mut self, handle: Handle, prop: &'static str, value: Value) {
        if let Some(map) = self.props.get_mut(&handle) {
            let map = Arc::make_mut(map);
            map.insert(prop, value);
        } else {
            let mut map = HashMap::new();
            map.insert(prop, value);
            self.props.insert(handle, Arc::new(map));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_send_sync<T>(_t: T)
    where
        T: Send + Sync,
    {
    }

    #[test]
    fn test_obj_send_sync() {
        test_send_sync(Object::Value(Value::Nil));
    }

    #[test]
    fn test_basic() -> VMResult<()> {
        let mut heap = Heap::default();
        let mark_roots = |_heap: &mut Heap| -> VMResult<()> { Ok(()) };
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 0);
        for x in 0..512 {
            heap.alloc_pair(Value::Int(x), Value::Nil, MutState::Mutable, mark_roots);
        }
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 512);
        for x in 0..512 {
            if let (Value::Int(v), Value::Nil) = heap.get_pair(Handle { idx: x }) {
                assert!(x == v as usize);
            } else {
                assert!(false);
            }
        }
        heap.alloc_pair(Value::Int(512), Value::Nil, MutState::Mutable, mark_roots);
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 1);
        if let (Value::Int(v), Value::Nil) = heap.get_pair(Handle { idx: 0 }) {
            assert!(512 == v);
        } else {
            assert!(false);
        }
        let mark_roots = |heap: &mut Heap| -> VMResult<()> {
            for idx in 0..512 {
                heap.mark(Handle { idx });
            }
            Ok(())
        };
        for x in 0..512 {
            heap.alloc_pair(Value::Int(x), Value::Nil, MutState::Mutable, mark_roots);
        }
        assert!(heap.capacity() == 1024);
        assert!(heap.live_objects() == 513);
        for x in 0..513 {
            if let (Value::Int(v), Value::Nil) = heap.get_pair(Handle { idx: x }) {
                if x == 0 {
                    assert!(512 == v);
                } else {
                    assert!(x - 1 == v as usize);
                }
            } else {
                assert!(false);
            }
        }
        let mark_roots = |heap: &mut Heap| -> VMResult<()> {
            for idx in 0..513 {
                if idx % 2 == 0 {
                    heap.mark(Handle { idx });
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
            let h = heap.alloc_pair(Value::Int(x), Value::Nil, MutState::Mutable, mark_roots);
            heap.sticky(h.get_handle().unwrap());
        }
        heap.collect(mark_roots);
        assert!(heap.capacity() == 1024);
        assert!(heap.live_objects() == 512);
        for x in 512..1024 {
            let _h = heap.alloc_pair(Value::Int(x), Value::Nil, MutState::Mutable, mark_roots);
        }
        let mark_roots = |heap: &mut Heap| -> VMResult<()> {
            for idx in 0..1024 {
                heap.mark(Handle { idx });
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
            let inner = heap.alloc_pair(Value::Int(x), Value::Nil, MutState::Mutable, mark_roots);
            outers.borrow_mut().push(
                heap.alloc_pair(inner, Value::Nil, MutState::Mutable, mark_roots)
                    .get_handle()
                    .unwrap(),
            );
        }
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 512);
        let mut i = 0;
        for h in outers.borrow().iter() {
            if let (Value::Pair(inner), Value::Nil) = heap.get_pair(*h) {
                if let (Value::Int(v), Value::Nil) = heap.get_pair(inner) {
                    assert!(i == v as usize);
                } else {
                    assert!(false);
                }
            } else {
                assert!(false);
            }
            i += 1;
        }
        heap.collect(mark_roots);
        assert_eq!(heap.capacity(), 512);
        assert_eq!(heap.live_objects(), 512);
        for h in outers.borrow().iter() {
            //let data = Box::new("bloop".to_string());
            //let d = Box::into_raw(data) as usize;
            heap.replace(*h, Object::String(Arc::new("bloop".into())));
        }
        heap.collect(mark_roots);
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 256);
        for h in outers.borrow().iter() {
            let sstr = heap.get_string(*h);
            assert!(sstr == "bloop");
            i += 1;
        }
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
            let inner = heap.alloc_pair(Value::Int(x), Value::Nil, MutState::Mutable, mark_roots);
            v.push(inner);
        }
        outers.borrow_mut().push(heap.alloc(
            Object::Vector(Arc::new(v)),
            MutState::Mutable.flag() | FLAG_TRACE,
            mark_roots,
        ));
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 257);
        for h in outers.borrow().iter() {
            let v = heap.get_vector(*h);
            let mut i = 0;
            for hv in v {
                if let Value::Pair(hv) = hv {
                    if let (Value::Int(v), Value::Nil) = heap.get_pair(*hv) {
                        assert!(i == v as usize);
                    } else {
                        assert!(false);
                    }
                } else {
                    assert!(false);
                }
                i += 1;
            }
        }
        heap.collect(mark_roots);
        assert_eq!(heap.capacity(), 512);
        assert_eq!(heap.live_objects(), 257);
        for h in outers.borrow().iter() {
            let v = heap.get_vector(*h);
            let mut i = 0;
            for hv in v {
                if let Value::Pair(hv) = hv {
                    if let (Value::Int(v), Value::Nil) = heap.get_pair(*hv) {
                        assert!(i == v as usize);
                    } else {
                        assert!(false);
                    }
                } else {
                    assert!(false);
                }
                i += 1;
            }
        }
        for h in outers.borrow().iter() {
            //let data = Box::new("bloop".to_string());
            //let d = Box::into_raw(data) as usize;
            //heap.replace(*h, Object::StringMut(d));
            heap.replace(*h, Object::String(Arc::new("bloop".into())));
        }
        heap.collect(mark_roots);
        assert!(heap.capacity() == 512);
        assert!(heap.live_objects() == 1);
        for h in outers.borrow().iter() {
            let sstr = heap.get_string(*h);
            assert!(sstr == "bloop");
        }
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
        outers.borrow_mut().push(
            heap.alloc_pair(Value::Int(1), Value::Int(2), MutState::Mutable, mark_roots)
                .get_handle()
                .unwrap(),
        );
        let car_h = heap.alloc_pair(Value::Int(3), Value::Nil, MutState::Mutable, mark_roots);
        let cdr_h = heap.alloc_pair(Value::Int(4), Value::Nil, MutState::Mutable, mark_roots);
        outers.borrow_mut().push(
            heap.alloc_pair(car_h, Value::Int(2), MutState::Mutable, mark_roots)
                .get_handle()
                .unwrap(),
        );
        outers.borrow_mut().push(
            heap.alloc_pair(Value::Int(1), cdr_h, MutState::Mutable, mark_roots)
                .get_handle()
                .unwrap(),
        );
        outers.borrow_mut().push(
            heap.alloc_pair(car_h, cdr_h, MutState::Mutable, mark_roots)
                .get_handle()
                .unwrap(),
        );
        assert_eq!(heap.capacity(), 512);
        assert_eq!(heap.live_objects(), 6);
        heap.collect(mark_roots);
        assert_eq!(heap.capacity(), 512);
        assert_eq!(heap.live_objects(), 6);
        let mut i = 0;
        for h in outers.borrow().iter() {
            let (car, cdr) = heap.get_pair(*h);
            if i == 0 {
                let (car, cdr) = if let Value::Int(car) = car {
                    if let Value::Int(cdr) = cdr {
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
                    if let (Value::Int(car), Value::Nil) = heap.get_pair(car_h) {
                        if let Value::Int(cdr) = cdr {
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
                    if let (Value::Int(cdr), Value::Nil) = heap.get_pair(cdr_h) {
                        if let Value::Int(car) = car {
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
                    if let (Value::Int(car), Value::Nil) = heap.get_pair(car_h) {
                        if let Value::Pair(cdr_h) = cdr {
                            if let (Value::Int(cdr), Value::Nil) = heap.get_pair(cdr_h) {
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
                assert!(false);
            }
            i += 1;
        }

        Ok(())
    }
}
