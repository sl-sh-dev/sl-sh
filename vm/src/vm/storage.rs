use std::collections::HashMap;
use std::sync::Arc;

use crate::chunk::*;
use crate::error::*;
use crate::heap::*;
use crate::interner::*;
use crate::persistent_map::{MapNode, PersistentMap};
use crate::persistent_vec::{PersistentVec, VecNode};
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
        let value = self.promote_number(value);
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

    pub fn alloc_int(&mut self, num: i64) -> Value {
        if num >= 0 && num < u32::MAX as i64 {
            Value::UInt32(num as u32)
        } else if num > i32::MIN as i64 && num < i32::MAX as i64 {
            Value::Int32(num as i32)
        } else {
            let mut heap = self.heap.take().expect("VM must have a Heap!");
            let res = heap.alloc_i64(num, MutState::Mutable, |heap| self.mark_roots(heap));
            self.heap = Some(heap);
            res
        }
    }

    pub fn local_i64(&mut self, reg: usize, num: i64) -> Value {
        self.numbers[reg].int = num;
        Value::Int64(Numeric::Local(reg as u16))
    }

    pub fn alloc_i64(&mut self, num: i64) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_i64(num, MutState::Mutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn local_u64(&mut self, reg: usize, num: u64) -> Value {
        self.numbers[reg].uint = num;
        Value::UInt64(Numeric::Local(reg as u16))
    }

    pub fn alloc_u64(&mut self, num: u64) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_u64(num, MutState::Mutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn local_f64(&mut self, reg: usize, num: f64) -> Value {
        self.numbers[reg].float = num;
        Value::Float64(Numeric::Local(reg as u16))
    }

    pub fn alloc_f64(&mut self, num: f64) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_f64(num, MutState::Mutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
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

    pub fn alloc_persistent_vector(&mut self, vec: PersistentVec) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_persistent_vector(vec, MutState::Immutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_vecnode(&mut self, node: VecNode) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_vecnode(node, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_persistent_map(&mut self, map: PersistentMap) -> Value {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        let res = heap.alloc_persistent_map(map, MutState::Immutable, |heap| self.mark_roots(heap));
        self.heap = Some(heap);
        res
    }

    pub fn alloc_mapnode(&mut self, node: MapNode) -> Handle {
        let mut heap = self.heap.take().expect("VM must have a Heap!");
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        let res = heap.alloc_mapnode(node, |heap| self.mark_roots(heap))
            .get_handle()
            .unwrap();
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

    /// If val is a 64 bit number stored on the number stack then promote to the heap.  Otherwise
    /// just return val.
    fn promote_number(&mut self, mut val: Value) -> Value {
        // If we have a number stored locally then put it on the heap as well as the value that references it.
        if let Value::Int64(Numeric::Local(idx)) = val {
            val = self.alloc_i64(unsafe { self.numbers[idx as usize].int });
        }
        if let Value::UInt64(Numeric::Local(idx)) = val {
            val = self.alloc_u64(unsafe { self.numbers[idx as usize].uint });
        }
        if let Value::Float64(Numeric::Local(idx)) = val {
            val = self.alloc_f64(unsafe { self.numbers[idx as usize].float });
        }
        val
    }

    /// Allocate a Value on the heap.  Moving a value to the heap is useful for captured variable
    /// for instance.
    pub fn alloc_value(&mut self, val: Value) -> Value {
        let val = self.promote_number(val);
        // Break the lifetime of heap away from self for this call so we can mark_roots if needed.
        let heap: &mut Heap = unsafe { (self.heap_mut() as *mut Heap).as_mut().unwrap() };
        // alloc must not save mark_roots (it does not) since we broke heap away from self.
        heap.alloc_value(val, MutState::Mutable, |heap| self.mark_roots(heap))
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

    pub fn get_int(&self, handle: Numeric) -> i64 {
        match handle {
            Numeric::Local(idx) => unsafe { self.numbers[idx as usize].int },
            Numeric::Heap(handle) => self.heap().get_int(handle),
        }
    }

    pub fn get_int_mut(&mut self, handle: Numeric) -> &mut i64 {
        match handle {
            Numeric::Local(idx) => unsafe { &mut self.numbers[idx as usize].int },
            Numeric::Heap(handle) => self.heap_mut().get_int_mut(handle),
        }
    }

    pub fn get_uint(&self, handle: Numeric) -> u64 {
        match handle {
            Numeric::Local(idx) => unsafe { self.numbers[idx as usize].uint },
            Numeric::Heap(handle) => self.heap().get_uint(handle),
        }
    }

    pub fn get_uint_mut(&mut self, handle: Numeric) -> &mut u64 {
        match handle {
            Numeric::Local(idx) => unsafe { &mut self.numbers[idx as usize].uint },
            Numeric::Heap(handle) => self.heap_mut().get_uint_mut(handle),
        }
    }

    pub fn get_float(&self, handle: Numeric) -> f64 {
        match handle {
            Numeric::Local(idx) => unsafe { self.numbers[idx as usize].float },
            Numeric::Heap(handle) => self.heap().get_float(handle),
        }
    }

    pub fn get_float_mut(&mut self, handle: Numeric) -> &mut f64 {
        match handle {
            Numeric::Local(idx) => unsafe { &mut self.numbers[idx as usize].float },
            Numeric::Heap(handle) => self.heap_mut().get_float_mut(handle),
        }
    }

    pub fn get_string(&self, handle: Handle) -> &str {
        self.heap().get_string(handle)
    }

    pub fn get_string_mut(&mut self, handle: Handle) -> &mut String {
        self.heap_mut().get_string_mut(handle)
    }

    pub fn get_vector(&self, handle: Handle) -> &[Value] {
        self.heap().get_vector(handle)
    }

    pub fn get_vector_mut(&mut self, handle: Handle) -> VMResult<&mut Vec<Value>> {
        self.heap_mut().get_vector_mut(handle)
    }

    pub(crate) fn get_persistent_vector(&self, handle: Handle) -> &PersistentVec {
        self.heap().get_persistent_vector(handle)
    }

    pub fn get_vecnode(&self, handle: Handle) -> &VecNode {
        self.heap().get_vecnode(handle)
    }

    pub(crate) fn _get_persistent_map(&self, handle: Handle) -> &PersistentMap {
        self.heap()._get_persistent_map(handle)
    }

    pub fn get_mapnode(&self, handle: Handle) -> &MapNode {
        self.heap().get_mapnode(handle)
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

    pub fn get_callframe_mut(&mut self, handle: Handle) -> &mut CallFrame {
        self.heap_mut().get_callframe_mut(handle)
    }

    pub fn get_value(&self, handle: Handle) -> Value {
        self.heap().get_value(handle)
    }

    pub fn get_value_mut(&mut self, handle: Handle) -> &mut Value {
        self.heap_mut().get_value_mut(handle)
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
            heap.mark(self.stack[i]);
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
