use std::collections::HashMap;
use std::sync::Arc;
use crate::interner::Interned;

use crate::chunk::*;
use crate::error::*;
use crate::heap::*;
use crate::value::*;
use crate::GVm;

/// Vm code to access storage, heap, stack, globals, etc.

pub struct CallStackIter<'vm, ENV> {
    vm: &'vm GVm<ENV>,
    current: usize,
    last_current: usize,
}

impl<'vm, ENV> CallStackIter<'vm, ENV> {
    pub fn new(vm: &'vm GVm<ENV>) -> Self {
        CallStackIter {
            vm,
            current: vm.stack_top,
            last_current: 1,
        }
    }
}

impl<'vm, ENV> Iterator for CallStackIter<'vm, ENV> {
    type Item = &'vm CallFrame;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(frame) = self.vm.call_frame_idx(self.current) {
            if self.last_current == 0 {
                None
            } else {
                self.last_current = self.current;
                self.current = frame.stack_top;
                Some(frame)
            }
        } else {
            None
        }
    }
}

impl<ENV> GVm<ENV> {
    pub fn clear_err_frame(&mut self) {
        self.err_frame = None;
    }

    pub fn err_frame(&self) -> &Option<CallFrame> {
        &self.err_frame
    }

    pub fn get_registers(&self, start: usize, end: usize) -> &[Value] {
        &self.stack_slice()[start..end]
    }

    pub fn get_current_registers(&self) -> &[Value] {
        let start = self.stack_top;
        let end = self.stack_max;
        &self.stack_slice()[start..=end]
    }

    pub fn get_stack(&self, idx: usize) -> Value {
        self.stack(idx)
    }

    pub fn stack_max(&self) -> usize {
        self.stack_max
    }

    pub fn get_interned(&self, i: Interned) -> &'static str {
        self.interner.get_string(i).expect("Invalid interned value")
    }

    pub fn intern_static(&mut self, string: &'static str) -> Interned {
        self.interner.intern_static(string)
    }

    pub fn intern(&mut self, string: &str) -> Interned {
        self.interner.intern(string)
    }

    pub fn get_if_interned(&self, string: &str) -> Option<Interned> {
        self.interner.get_if_interned(string)
    }

    pub fn set_global(&mut self, slot: u32, value: Value) {
        self.globals.set(slot, value);
    }

    pub fn reserve_global(&mut self) -> u32 {
        self.globals.reserve()
    }

    pub fn get_call_stack(&self) -> CallStackIter<ENV> {
        CallStackIter::new(self)
    }

    pub fn sizeof_heap_object() -> usize {
        Heap::sizeof_object()
    }

    pub fn alloc_pair(&mut self, car: Value, cdr: Value) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_pair(car, cdr, MutState::Mutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_pair_ro(&mut self, car: Value, cdr: Value) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_pair(car, cdr, MutState::Immutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_string(&mut self, s: String) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_string(s, MutState::Mutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_string_ro(&mut self, s: String) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_string(s, MutState::Immutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_char(&mut self, ch: &str) -> Value {
        if ch.len() < 7 {
            let mut v: [u8; 6] = [0; 6];
            for (i, c) in ch.bytes().enumerate() {
                v[i] = c;
            }
            Value::CharCluster(ch.len() as u8, v)
        } else if let Value::String(handle) = self.alloc_string_ro(ch.to_string()) {
            Value::CharClusterLong(handle)
        } else {
            panic!("Invalid alloc_string!");
        }
    }

    pub fn alloc_vector(&mut self, v: Vec<Value>) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_vector(v, MutState::Mutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_vector_ro(&mut self, v: Vec<Value>) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_vector(v, MutState::Immutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_map(&mut self, map: HashMap<Value, Value>) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_map(map, MutState::Mutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_map_ro(&mut self, map: HashMap<Value, Value>) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_map(map, MutState::Immutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_list_ro(&mut self, v: Vec<Value>) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = Value::List(
            heap.alloc_vector(v, MutState::Immutable, |heap| self.mark_roots(heap))
                .get_handle()
                .expect("Allocated vector not a vector?"),
            0,
        );
        self.heap = Some(heap);
        res
    }

    pub fn alloc_bytes(&mut self, v: Vec<u8>) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        let res = heap.alloc_bytes(v, MutState::Mutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_lambda(&mut self, l: Arc<Chunk>) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        let res = heap.alloc_lambda(l, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_closure(&mut self, l: Arc<Chunk>, v: Vec<Handle>) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        let res = heap.alloc_closure(l, v, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_continuation(&mut self, k: Continuation) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        let res = heap.alloc_continuation(k, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_callframe(&mut self, frame: CallFrame) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        let res = heap.alloc_callframe(frame, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    /// Allocate a Value on the heap.  Moving a value to the heap is useful for captured variable
    /// for instance.
    pub fn alloc_value(&mut self, val: Value) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_value(val, MutState::Mutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    /// Allocate an Error on the heap.
    pub fn alloc_error(&mut self, err: Error) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_error(err, MutState::Mutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn heap_immutable(&mut self, val: Value) {
        self.heap_mut().immutable(val);
    }

    pub fn heap_sticky(&mut self, val: Value) {
        self.heap_mut().sticky(val);
    }

    pub fn heap_unsticky(&mut self, val: Value) {
        self.heap_mut().unsticky(val);
    }

    /// Pause garbage collection.
    /// Each pause_gc must have an unpause_gc before GC resumes (it is a counter that must be 0).
    pub fn pause_gc(&mut self) {
        self.heap_mut().pause_gc();
    }

    /// UnPause garbage collection.
    /// Each pause_gc must have an unpause_gc before GC resumes (it is a counter that must be 0).
    pub fn unpause_gc(&mut self) {
        self.heap_mut().unpause_gc();
    }

    pub fn get_heap_property(&self, key_val: Value, prop: &str) -> Option<Value> {
        if let Some(interned) = self.get_if_interned(prop) {
            self.heap().get_property(key_val, interned)
        } else {
            None
        }
    }

    pub fn set_heap_property(&mut self, key_val: Value, prop: &str, value: Value) {
        let str_ref = self.intern(prop);
        self.heap_mut().set_property(key_val, str_ref, value)
    }

    pub fn get_heap_property_interned(&self, key_val: Value, prop: Interned) -> Option<Value> {
        self.heap().get_property(key_val, prop)
    }

    pub fn set_heap_property_interned(&mut self, key_val: Value, prop: Interned, value: Value) {
        self.heap_mut().set_property(key_val, prop, value)
    }

    pub fn get_global_property(&self, global: u32, prop: Interned) -> Option<Value> {
        self.globals.get_property(global, prop)
    }

    pub fn set_global_property(&mut self, global: u32, prop: Interned, value: Value) {
        self.globals.set_property(global, prop, value)
    }

    pub fn get_global(&self, idx: u32) -> Value {
        self.globals.get(idx)
    }

    pub fn get_string(&self, handle: Handle) -> &str {
        self.heap().get_string(handle)
    }

    pub fn get_string_mut(&mut self, handle: Handle) -> VMResult<&mut String> {
        self.heap_mut().get_string_mut(handle)
    }

    pub fn get_vector(&self, handle: Handle) -> &[Value] {
        self.heap().get_vector(handle)
    }

    pub fn get_vector_mut(&mut self, handle: Handle) -> VMResult<&mut Vec<Value>> {
        self.heap_mut().get_vector_mut(handle)
    }

    pub fn get_map(&self, handle: Handle) -> &HashMap<Value, Value> {
        self.heap().get_map(handle)
    }

    pub fn get_map_mut(&mut self, handle: Handle) -> VMResult<&mut HashMap<Value, Value>> {
        self.heap_mut().get_map_mut(handle)
    }

    pub fn get_bytes(&self, handle: Handle) -> &[u8] {
        self.heap().get_bytes(handle)
    }

    pub fn get_pair(&self, handle: Handle) -> (Value, Value) {
        self.heap().get_pair(handle)
    }

    pub fn get_pair_mut(&mut self, handle: Handle) -> VMResult<(&mut Value, &mut Value)> {
        self.heap_mut().get_pair_mut(handle)
    }

    pub fn get_pair_mut_override(&mut self, handle: Handle) -> (&mut Value, &mut Value) {
        self.heap_mut().get_pair_mut_override(handle)
    }

    pub fn get_lambda(&self, handle: Handle) -> Arc<Chunk> {
        self.heap().get_lambda(handle)
    }

    pub fn get_closure(&self, handle: Handle) -> (Arc<Chunk>, &[Handle]) {
        self.heap().get_closure(handle)
    }

    pub fn get_continuation(&self, handle: Handle) -> &Continuation {
        self.heap().get_continuation(handle)
    }

    pub fn get_callframe(&self, handle: Handle) -> &CallFrame {
        self.heap().get_callframe(handle)
    }

    pub fn get_value(&self, handle: Handle) -> Value {
        self.heap().get_value(handle)
    }

    pub fn get_value_mut(&mut self, handle: Handle) -> &mut Value {
        self.heap_mut().get_value_mut(handle)
    }

    pub fn get_error(&self, handle: Handle) -> Error {
        self.heap().get_error(handle)
    }

    pub fn new_upval(&mut self, val: Value) -> Value {
        self.alloc_value(val)
    }

    pub fn call_frame(&self) -> Option<&CallFrame> {
        self.call_frame_idx(self.stack_top)
    }

    pub fn make_err(&mut self, key: &'static str, data: Value) -> Value {
        let keyword = self.intern_static(key);
        let err = Error { keyword, data };
        self.alloc_error(err)
    }

    pub(super) fn call_frame_idx(&self, idx: usize) -> Option<&CallFrame> {
        match self.stack(idx) {
            Value::CallFrame(handle) => Some(self.get_callframe(handle)),
            _ => None,
        }
    }

    pub(super) fn copy_frame_defers(&mut self) {
        if let Some(frame) = self.call_frame() {
            // Need to break the call frame lifetime from self to avoid extra work (allocations).
            // This should safe because the stack and heap are not touched so the reference is
            // stable.  The unwrap() is OK because the frame can not be NULL.
            let frame: &CallFrame = unsafe { (frame as *const CallFrame).as_ref().unwrap() };
            self.defers.resize(frame.defers.len(), Value::Undefined);
            // Generally self.defers will be empty but if not don't loose them!
            self.defers.copy_from_slice(&frame.defers[..]);
        }
    }

    fn mark_roots(&mut self, heap: &mut Heap) -> VMResult<()> {
        self.globals.mark(heap);
        // TODO- add a bound to ENV so we can call a mark_roots?  I think we need this for the
        // temporarily held doc_string for instance but also generally useful?
        for i in 0..self.stack_max {
            heap.mark(self.stack(i));
        }
        if let Some(this_fn) = self.this_fn {
            heap.mark(this_fn);
        }
        if let Some(on_error) = self.on_error {
            heap.mark(on_error);
        }
        // TODO: XXX do we need this?  Probably but maybe not.
        //if let Some(err_frame) = &self.err_frame {
        //}
        for defer in &self.defers {
            heap.mark(*defer);
        }
        Ok(())
    }
}
