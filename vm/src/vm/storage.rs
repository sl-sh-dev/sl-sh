use std::collections::HashMap;
use std::sync::Arc;

use crate::chunk::*;
use crate::error::*;
use crate::heap::*;
use crate::interner::*;
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
        &self.stack[start..end]
    }

    pub fn get_stack(&self, idx: usize) -> Value {
        self.stack[idx]
    }

    pub fn stack_max(&self) -> usize {
        self.stack_max
    }

    pub fn stack(&self) -> &[Value] {
        &self.stack
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
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_pair(car, cdr, MutState::Mutable, |heap| self.mark_roots(heap))
    }

    pub fn alloc_pair_ro(&mut self, car: Value, cdr: Value) -> Value {
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_pair(car, cdr, MutState::Immutable, |heap| self.mark_roots(heap))
    }

    pub fn alloc_string(&mut self, s: String) -> Value {
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_string(s, MutState::Mutable, |heap| self.mark_roots(heap))
    }

    pub fn alloc_string_ro(&mut self, s: String) -> Value {
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_string(s, MutState::Immutable, |heap| self.mark_roots(heap))
    }

    pub fn alloc_vector(&mut self, v: Vec<Value>) -> Value {
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_vector(v, MutState::Mutable, |heap| self.mark_roots(heap))
    }

    pub fn alloc_vector_ro(&mut self, v: Vec<Value>) -> Value {
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_vector(v, MutState::Immutable, |heap| self.mark_roots(heap))
    }

    pub fn alloc_map(&mut self, map: HashMap<Value, Value>) -> Value {
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_map(map, MutState::Mutable, |heap| self.mark_roots(heap))
    }

    pub fn alloc_map_ro(&mut self, map: HashMap<Value, Value>) -> Value {
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_map(map, MutState::Immutable, |heap| self.mark_roots(heap))
    }

    pub fn alloc_list_ro(&mut self, v: Vec<Value>) -> Value {
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        Value::List(
            heap.alloc_vector(v, MutState::Immutable, |heap| self.mark_roots(heap))
                .get_handle()
                .expect("Allocated vector not a vector?"),
            0,
        )
    }

    pub fn alloc_bytes(&mut self, v: Vec<u8>) -> Value {
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_bytes(v, MutState::Mutable, |heap| self.mark_roots(heap))
    }

    pub fn alloc_lambda(&mut self, l: Arc<Chunk>) -> Value {
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_lambda(l, |heap| self.mark_roots(heap))
    }

    pub fn alloc_closure(&mut self, l: Arc<Chunk>, v: Vec<Handle>) -> Value {
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_closure(l, v, |heap| self.mark_roots(heap))
    }

    pub fn alloc_continuation(&mut self, k: Continuation) -> Value {
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_continuation(k, |heap| self.mark_roots(heap))
    }

    pub fn alloc_callframe(&mut self, frame: CallFrame) -> Value {
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_callframe(frame, |heap| self.mark_roots(heap))
    }

    pub fn alloc_value(&mut self, val: Value) -> Value {
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (&mut self.heap as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_value(val, MutState::Mutable, |heap| self.mark_roots(heap))
    }

    pub fn heap_sticky(&mut self, handle: Handle) {
        self.heap.sticky(handle);
    }

    pub fn heap_unsticky(&mut self, handle: Handle) {
        self.heap.unsticky(handle);
    }

    /// Pause garbage collection.
    /// Each pause_gc must have an unpause_gc before GC resumes (it is a counter that must be 0).
    pub fn pause_gc(&mut self) {
        self.heap.pause_gc();
    }

    /// UnPause garbage collection.
    /// Each pause_gc must have an unpause_gc before GC resumes (it is a counter that must be 0).
    pub fn unpause_gc(&mut self) {
        self.heap.unpause_gc();
    }

    pub fn get_heap_property(&self, handle: Handle, prop: &str) -> Option<Value> {
        if let Some(interned) = self.get_if_interned(prop) {
            self.heap.get_property(handle, interned)
        } else {
            None
        }
    }

    pub fn set_heap_property(&mut self, handle: Handle, prop: &str, value: Value) {
        let str_ref = self.intern(prop);
        self.heap.set_property(handle, str_ref, value)
    }

    pub fn get_heap_property_interned(&self, handle: Handle, prop: Interned) -> Option<Value> {
        self.heap.get_property(handle, prop)
    }

    pub fn set_heap_property_interned(&mut self, handle: Handle, prop: Interned, value: Value) {
        self.heap.set_property(handle, prop, value)
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
        self.heap.get_string(handle)
    }

    pub fn get_string_mut(&mut self, handle: Handle) -> &mut String {
        self.heap.get_string_mut(handle)
    }

    pub fn get_vector(&self, handle: Handle) -> &[Value] {
        self.heap.get_vector(handle)
    }

    pub fn get_vector_mut(&mut self, handle: Handle) -> VMResult<&mut Vec<Value>> {
        self.heap.get_vector_mut(handle)
    }

    pub fn get_map(&self, handle: Handle) -> &HashMap<Value, Value> {
        self.heap.get_map(handle)
    }

    pub fn get_map_mut(&mut self, handle: Handle) -> VMResult<&mut HashMap<Value, Value>> {
        self.heap.get_map_mut(handle)
    }

    pub fn get_bytes(&self, handle: Handle) -> &[u8] {
        self.heap.get_bytes(handle)
    }

    pub fn get_pair(&self, handle: Handle) -> (Value, Value) {
        self.heap.get_pair(handle)
    }

    pub fn get_pair_mut(&mut self, handle: Handle) -> VMResult<(&mut Value, &mut Value)> {
        self.heap.get_pair_mut(handle)
    }

    pub fn get_pair_mut_override(&mut self, handle: Handle) -> (&mut Value, &mut Value) {
        self.heap.get_pair_mut_override(handle)
    }

    pub fn get_lambda(&self, handle: Handle) -> Arc<Chunk> {
        self.heap.get_lambda(handle)
    }

    pub fn get_closure(&self, handle: Handle) -> (Arc<Chunk>, &[Handle]) {
        self.heap.get_closure(handle)
    }

    pub fn get_continuation(&self, handle: Handle) -> &Continuation {
        self.heap.get_continuation(handle)
    }

    pub fn get_callframe(&self, handle: Handle) -> &CallFrame {
        self.heap.get_callframe(handle)
    }

    pub fn get_callframe_mut(&mut self, handle: Handle) -> &mut CallFrame {
        self.heap.get_callframe_mut(handle)
    }

    pub fn get_value(&self, handle: Handle) -> Value {
        self.heap.get_value(handle)
    }

    pub fn get_value_mut(&mut self, handle: Handle) -> &mut Value {
        self.heap.get_value_mut(handle)
    }

    pub fn new_upval(&mut self, val: Value) -> Value {
        self.alloc_value(val)
    }

    pub(super) fn _call_frame(&self) -> Option<&CallFrame> {
        self.call_frame_idx(self.stack_top)
    }

    pub(super) fn call_frame_idx(&self, idx: usize) -> Option<&CallFrame> {
        match self.stack[idx] {
            Value::CallFrame(handle) => {
                let frame = self.get_callframe(handle);
                Some(frame)
            }
            _ => None,
            //_ => panic!("Invalid stack, not a call frame."),
        }
    }

    pub(super) fn call_frame_mut(&mut self) -> Option<&mut CallFrame> {
        self.call_frame_mut_idx(self.stack_top)
    }

    pub(super) fn call_frame_mut_idx(&mut self, idx: usize) -> Option<&mut CallFrame> {
        match self.stack[idx] {
            Value::CallFrame(handle) => Some(self.get_callframe_mut(handle)),
            _ => None,
            //_ => panic!("Invalid stack, not a call frame."),
        }
    }

    fn mark_roots(&mut self, heap: &mut Heap) -> VMResult<()> {
        self.globals.mark(heap);
        for i in 0..self.stack_max {
            if let Some(handle) = self.stack[i].get_handle() {
                heap.mark(handle);
            }
        }
        if let Some(this_fn) = self.this_fn {
            if let Some(handle) = this_fn.get_handle() {
                heap.mark(handle);
            }
        }
        if let Some(on_error) = self.on_error {
            if let Some(handle) = on_error.get_handle() {
                heap.mark(handle);
            }
        }
        // TODO: XXX do we need this?  Probably but maybe not.
        //if let Some(err_frame) = &self.err_frame {
        //}
        for defer in &self.defers {
            if let Some(handle) = defer.get_handle() {
                heap.mark(handle);
            }
        }
        Ok(())
    }
}
