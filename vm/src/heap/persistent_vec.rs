//! Implements a persistent vector against the slosh heap.  This is like the Clojure PersistentVector.
//! See the series of blog post starting at https://hypirion.com/musings/understanding-persistent-vector-pt-1
//! for more details.

use crate::{GVm, Value};

const BITS: usize = 5;
const WIDTH: usize = 1 << BITS; // 2^5 = 32
const MASK: usize = WIDTH - 1; // 31, or 0x1f

const PUSH_STACK_SIZE: usize = 12;

/// Simple little stack based "stack" for tracking paths through a tree.
/// This saves allocations or for some operations.
/// 12 levels deep at a width of 32 provides a huge index range (32^12), not quite 64^2 but close enough..
struct PathStack {
    path: [Option<(VecNode, usize)>; PUSH_STACK_SIZE],
    idx: usize,
}

impl PathStack {
    fn new() -> PathStack {
        PathStack {
            path: [None; PUSH_STACK_SIZE],
            idx: 0,
        }
    }

    fn push(&mut self, val: (VecNode, usize)) {
        self.idx += 1;
        if self.idx >= PUSH_STACK_SIZE {
            panic!("depth of vector tree to deep!");
        }
        self.path[self.idx] = Some(val);
    }

    fn pop(&mut self) -> Option<(VecNode, usize)> {
        if self.idx > 0 {
            let r = self.path[self.idx].take();
            self.idx -= 1;
            r
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct PersistentVec {
    // How many elements are in the vec.
    length: usize,
    // How many elements are in the tail.
    tail_length: usize,
    // number of bits to shift to get the first level index.  Will be a multiple of BITS.
    shift: usize,
    // Need to keep PersistentVec Copy but maybe this should be a handle to a root on the heap.
    root: VecNode,
    // Tail array for the vector.  Elements above tail_length are not cleared of old values.
    tail: [Value; WIDTH],
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum NodeType {
    Node([Value; WIDTH]),
    Leaf([Value; WIDTH]),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct VecNode {
    id: u32,
    data: NodeType,
}

impl VecNode {
    fn new_node(id: u32) -> Self {
        Self {
            id,
            data: NodeType::Node([Value::Undefined; WIDTH]),
        }
    }

    fn new_leaf(id: u32) -> Self {
        Self {
            id,
            data: NodeType::Leaf([Value::Undefined; WIDTH]),
        }
    }

    /// Return the nodes if this is a node and not a leaf.
    /// Invalid handles are not in use.
    pub fn nodes(&self) -> Option<&[Value]> {
        if let NodeType::Node(nodes) = &self.data {
            Some(nodes)
        } else {
            None
        }
    }

    /// Return the leaves if this is a leaf and not a node.
    /// Values or Undefined are not in use.
    pub fn leaf(&self) -> Option<&[Value]> {
        if let NodeType::Leaf(leaf) = &self.data {
            Some(leaf)
        } else {
            None
        }
    }
}

fn new_path<ENV>(id: u32, shift: usize, node_handle: Value, vm: &mut GVm<ENV>) -> Value {
    if shift == 0 {
        node_handle
    } else {
        let mut ret = VecNode {
            id,
            data: NodeType::Node([Value::Undefined; WIDTH]),
        };
        if let NodeType::Node(handles) = &mut ret.data {
            handles[0] = new_path(id, shift - BITS, node_handle, vm);
        }
        vm.alloc_vecnode(ret)
    }
}

impl Default for PersistentVec {
    fn default() -> Self {
        Self::new()
    }
}

impl PersistentVec {
    /// Create a new empty persistent vector.
    pub fn new() -> Self {
        Self {
            length: 0,
            tail_length: 0,
            shift: 0,
            root: VecNode::new_leaf(0),
            tail: [Value::Undefined; WIDTH],
        }
    }

    fn push_tail<ENV>(
        &self,
        shift: usize,
        parent: &VecNode,
        tailnode_handle: Value,
        vm: &mut GVm<ENV>,
    ) -> VecNode {
        let subidx = ((self.length - 1) >> shift) & MASK;
        let mut ret = *parent;
        let node_to_insert = if shift == BITS {
            tailnode_handle
        } else {
            let child_handle = if let NodeType::Node(handles) = parent.data {
                handles[subidx]
            } else {
                Value::Undefined
            };
            if !child_handle.is_undef() {
                let child = *vm.get_vecnode(child_handle.get_handle().expect("Not a vecnode!"));
                let tail = self.push_tail(shift - BITS, &child, tailnode_handle, vm);
                vm.alloc_vecnode(tail)
            } else {
                new_path(parent.id, shift - BITS, tailnode_handle, vm)
            }
        };
        if let NodeType::Node(handles) = &mut ret.data {
            handles[subidx] = node_to_insert;
        }
        ret
    }

    /// Number of items in the vec.
    pub fn len(&self) -> usize {
        self.length
    }

    /// Is the vec empty?
    pub fn is_empty(&self) -> bool {
        self.length == 0
    }

    /// First element (0) in list if not empty.
    pub fn first<ENV>(&self, vm: &GVm<ENV>) -> Option<Value> {
        self.get(0, vm)
    }

    /// Last element (len() - 1) if not empty.
    pub fn last<ENV>(&self, vm: &GVm<ENV>) -> Option<Value> {
        self.get(self.length - 1, vm)
    }

    /// Return the current root node if it is use.
    /// Primarily for GC.
    pub fn root(&self) -> Option<&VecNode> {
        if self.length > self.tail_length {
            Some(&self.root)
        } else {
            None
        }
    }

    /// Return the current tail, will have a length from 0 to WIDTH.
    /// Primarily for GC.
    pub fn tail(&self) -> &[Value] {
        &self.tail[0..self.tail_length]
    }

    /// Push value on vec and return the new vector.
    pub fn push<ENV>(&self, value: Value, vm: &mut GVm<ENV>) -> PersistentVec {
        let mut new_vec = *self;
        if self.tail_length < WIDTH {
            // Fits in the tail.
            new_vec.tail[self.tail_length] = value;
            new_vec.length += 1;
            new_vec.tail_length += 1;
            return new_vec;
        }
        let new_tail = VecNode {
            id: new_vec.root.id,
            data: NodeType::Leaf(self.tail),
        };
        let new_root = if (self.length >> BITS) > (1 << self.shift) {
            // root overflow
            let mut new_root = VecNode::new_node(new_vec.root.id);
            if let NodeType::Node(handles) = &mut new_root.data {
                handles[0] = vm.alloc_vecnode(new_vec.root);
                handles[1] = new_path(
                    new_vec.root.id,
                    new_vec.shift,
                    vm.alloc_vecnode(new_tail),
                    vm,
                );
            }
            new_vec.shift += BITS;
            new_root
        } else {
            // push the tail
            if matches!(new_vec.root.data, NodeType::Leaf(_)) {
                // If root is a leaf and this did not overflow then just copy tail to root.
                VecNode {
                    id: new_vec.root.id,
                    data: NodeType::Leaf(new_vec.tail),
                }
            } else {
                let new_tail_handle = vm.alloc_vecnode(new_tail);
                self.push_tail(new_vec.shift, &new_vec.root, new_tail_handle, vm)
            }
        };
        new_vec.root = new_root;
        new_vec.tail[0] = value;
        new_vec.length += 1;
        new_vec.tail_length = 1;
        new_vec
    }

    /// Pop the last value from vec and return the new vector.
    pub fn pop<ENV>(&self, vm: &mut GVm<ENV>) -> PersistentVec {
        let mut new_vec = *self;
        if self.length == 0 {
            return new_vec;
        }
        if new_vec.tail_length > 0 {
            new_vec.length -= 1;
            new_vec.tail_length -= 1;
        } else {
            let idx = self.length - 1;
            let mut path = PathStack::new();
            let mut node = new_vec.root;
            let mut level = self.shift;
            loop {
                match &mut node.data {
                    NodeType::Node(handles) => {
                        let next_node = *vm.get_vecnode(
                            handles[(idx >> level) & MASK]
                                .get_handle()
                                .expect("Not a vecnode!"),
                        );
                        path.push((node, (idx >> level) & MASK));
                        node = next_node;
                    }
                    NodeType::Leaf(values) => {
                        new_vec.tail = *values;
                        new_vec.tail_length = WIDTH - 1;
                        new_vec.length -= 1;
                        let mut first = true;
                        while let Some((mut prev_node, idx)) = path.pop() {
                            if let NodeType::Node(handles) = &mut prev_node.data {
                                if first && idx == 0 {
                                    // This node is removed, stay on first to try next node.
                                } else if first {
                                    handles[idx] = Value::Undefined;
                                    first = false;
                                } else {
                                    handles[idx] = vm.alloc_vecnode(node);
                                }
                                node = prev_node;
                            }
                        }
                        if first {
                            new_vec.root = VecNode::new_leaf(self.root.id);
                        } else {
                            new_vec.root = node;
                        }
                        return new_vec;
                    }
                }
                if level < BITS {
                    // Rut-Row, can't be here- should have found the leaf above when done.
                    panic!("Invalid Persistent Tree");
                }
                level -= BITS;
            }
        }

        new_vec
    }

    /// Get the value at idx, return None if index is invalid.
    pub fn get<ENV>(&self, idx: usize, vm: &GVm<ENV>) -> Option<Value> {
        if idx >= self.length {
            return None;
        }
        // Get it from the tail.
        let tail_offset = self.length - self.tail_length;
        if idx >= tail_offset {
            return self.tail.get(idx - tail_offset).copied();
        }

        let mut level = self.shift;
        let mut node = self.root;
        loop {
            match node.data {
                NodeType::Node(handles) => {
                    node = *vm.get_vecnode(
                        handles[(idx >> level) & MASK]
                            .get_handle()
                            .expect("Not a vecnode!"),
                    );
                }
                NodeType::Leaf(values) => return Some(values[idx & MASK]),
            }
            if level < BITS {
                // Rut-Row, can't be here- should have found the leaf above when done.
                panic!("Invalid Persistent Tree");
            }
            level -= BITS;
        }
    }

    /// Replace the value at idx with new_value and return a new vec.
    pub fn replace<ENV>(
        &self,
        idx: usize,
        new_value: Value,
        vm: &mut GVm<ENV>,
    ) -> Option<PersistentVec> {
        if idx >= self.length {
            return None;
        }
        let mut new_vec = *self;
        // Set it in the tail.
        let tail_offset = self.length - self.tail_length;
        if idx >= tail_offset {
            new_vec.tail[idx - tail_offset] = new_value;
            return Some(new_vec);
        }

        let mut path = PathStack::new();
        let mut level = self.shift;
        let mut node = new_vec.root;
        loop {
            match &mut node.data {
                NodeType::Node(handles) => {
                    let next_node = *vm.get_vecnode(
                        handles[(idx >> level) & MASK]
                            .get_handle()
                            .expect("Not a vecnode!"),
                    );
                    path.push((node, (idx >> level) & MASK));
                    node = next_node;
                }
                NodeType::Leaf(values) => {
                    values[idx & MASK] = new_value;
                    while let Some((mut prev_node, idx)) = path.pop() {
                        if let NodeType::Node(handles) = &mut prev_node.data {
                            handles[idx] = vm.alloc_vecnode(node);
                            node = prev_node;
                        }
                    }
                    new_vec.root = node;
                    return Some(new_vec);
                }
            }
            if level == 0 {
                break;
            }
            level -= BITS;
        }
        None
    }
}

pub struct PersistentVecIter<'vm, ENV> {
    vm: &'vm GVm<ENV>,
    vec: PersistentVec,
    idx: usize,
}

impl<'vm, ENV> PersistentVecIter<'vm, ENV> {
    pub fn new(vm: &'vm GVm<ENV>, vec: PersistentVec) -> Self {
        Self { vm, vec, idx: 0 }
    }
}

impl<'vm, ENV> Iterator for PersistentVecIter<'vm, ENV> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(res) = self.vec.get(self.idx, self.vm) {
            self.idx += 1;
            Some(res)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{VMResult, Value, Vm};

    #[test]
    fn test_pvec() -> VMResult<()> {
        let mut vm = Vm::new();
        vm.pause_gc();
        let pvec = PersistentVec::new();
        let global = vm.reserve_global();
        let pvec_val = vm.alloc_persistent_vector(pvec);
        vm.set_global(global, pvec_val);
        assert_eq!(pvec.len(), 0);
        assert!(pvec.is_empty());
        let pvec2 = pvec.push(Value::Int32(0), &mut vm);
        assert_eq!(pvec.len(), 0);
        assert!(pvec.is_empty());
        assert_eq!(pvec2.len(), 1);
        assert!(!pvec2.is_empty());
        assert_eq!(pvec2.get(0, &vm), Some(Value::Int32(0)));
        let mut pvec3 = pvec2;
        for i in 1..33 {
            pvec3 = pvec3.push(Value::Int32(i), &mut vm);
        }
        assert_eq!(pvec3.len(), 33);
        assert!(!pvec3.is_empty());
        assert_eq!(pvec3.get(0, &vm), Some(Value::Int32(0)));
        assert_eq!(pvec3.get(16, &vm), Some(Value::Int32(16)));
        assert_eq!(pvec3.get(31, &vm), Some(Value::Int32(31)));
        assert_eq!(pvec3.get(32, &vm), Some(Value::Int32(32)));
        let old_pvec3 = pvec3;

        for i in 33..1000 {
            pvec3 = pvec3.push(Value::Int32(i), &mut vm);
        }
        assert_eq!(old_pvec3.len(), 33);
        assert_eq!(pvec3.len(), 1000);
        assert!(!pvec3.is_empty());
        assert_eq!(pvec3.get(0, &vm), Some(Value::Int32(0)));
        assert_eq!(pvec3.get(16, &vm), Some(Value::Int32(16)));
        assert_eq!(pvec3.get(31, &vm), Some(Value::Int32(31)));
        assert_eq!(pvec3.get(32, &vm), Some(Value::Int32(32)));
        assert_eq!(pvec3.get(48, &vm), Some(Value::Int32(48)));
        assert_eq!(pvec3.get(63, &vm), Some(Value::Int32(63)));
        assert_eq!(pvec3.get(64, &vm), Some(Value::Int32(64)));
        assert_eq!(pvec3.get(512, &vm), Some(Value::Int32(512)));
        assert_eq!(pvec3.get(888, &vm), Some(Value::Int32(888)));
        assert_eq!(pvec3.get(999, &vm), Some(Value::Int32(999)));

        for i in 1000..5000 {
            pvec3 = pvec3.push(Value::Int32(i), &mut vm);
        }
        assert_eq!(pvec3.len(), 5000);
        assert!(!pvec3.is_empty());
        assert_eq!(pvec3.get(0, &vm), Some(Value::Int32(0)));
        assert_eq!(pvec3.get(16, &vm), Some(Value::Int32(16)));
        assert_eq!(pvec3.get(31, &vm), Some(Value::Int32(31)));
        assert_eq!(pvec3.get(32, &vm), Some(Value::Int32(32)));
        assert_eq!(pvec3.get(48, &vm), Some(Value::Int32(48)));
        assert_eq!(pvec3.get(63, &vm), Some(Value::Int32(63)));
        assert_eq!(pvec3.get(64, &vm), Some(Value::Int32(64)));
        assert_eq!(pvec3.get(512, &vm), Some(Value::Int32(512)));
        assert_eq!(pvec3.get(888, &vm), Some(Value::Int32(888)));
        assert_eq!(pvec3.get(999, &vm), Some(Value::Int32(999)));
        assert_eq!(pvec3.get(3999, &vm), Some(Value::Int32(3999)));
        assert_eq!(pvec3.get(4999, &vm), Some(Value::Int32(4999)));
        Ok(())
    }

    #[test]
    fn test_pvec_replace() -> VMResult<()> {
        let mut vm = Vm::new();
        vm.pause_gc();
        let mut pvec = PersistentVec::new();
        assert_eq!(pvec.len(), 0);
        assert!(pvec.is_empty());
        assert!(pvec.replace(0, Value::Nil, &mut vm).is_none());
        assert!(pvec.replace(32, Value::Nil, &mut vm).is_none());
        for i in 0..5000 {
            pvec = pvec.push(Value::Int32(i), &mut vm);
        }
        assert_eq!(pvec.len(), 5000);
        assert!(!pvec.is_empty());
        assert_eq!(pvec.get(0, &vm), Some(Value::Int32(0)));
        pvec = pvec.replace(0, Value::Nil, &mut vm).unwrap();
        assert_eq!(pvec.get(0, &vm), Some(Value::Nil));
        assert_eq!(pvec.get(31, &vm), Some(Value::Int32(31)));
        pvec = pvec.replace(31, Value::Nil, &mut vm).unwrap();
        assert_eq!(pvec.get(31, &vm), Some(Value::Nil));
        assert_eq!(pvec.get(32, &vm), Some(Value::Int32(32)));
        pvec = pvec.replace(32, Value::Nil, &mut vm).unwrap();
        assert_eq!(pvec.get(32, &vm), Some(Value::Nil));
        assert_eq!(pvec.get(1024, &vm), Some(Value::Int32(1024)));
        pvec = pvec.replace(1024, Value::Nil, &mut vm).unwrap();
        assert_eq!(pvec.get(1024, &vm), Some(Value::Nil));
        assert_eq!(pvec.get(2000, &vm), Some(Value::Int32(2000)));
        pvec = pvec.replace(2000, Value::Nil, &mut vm).unwrap();
        assert_eq!(pvec.get(2000, &vm), Some(Value::Nil));
        assert_eq!(pvec.get(4999, &vm), Some(Value::Int32(4999)));
        pvec = pvec.replace(4999, Value::Nil, &mut vm).unwrap();
        assert_eq!(pvec.get(4999, &vm), Some(Value::Nil));
        Ok(())
    }

    #[test]
    fn test_pvec_iter() -> VMResult<()> {
        let mut vm = Vm::new();
        vm.pause_gc();
        let mut pvec = PersistentVec::new();
        assert_eq!(pvec.len(), 0);
        assert!(pvec.is_empty());
        assert!(pvec.replace(0, Value::Nil, &mut vm).is_none());
        assert!(pvec.replace(32, Value::Nil, &mut vm).is_none());
        for i in 0..5000 {
            pvec = pvec.push(Value::Int32(i), &mut vm);
        }
        assert_eq!(pvec.len(), 5000);
        assert!(!pvec.is_empty());
        let iter = vm.alloc_persistent_vector(pvec).iter(&vm);
        let mut max = 0;
        for (i, val) in iter.enumerate() {
            assert_eq!(val, Value::Int32(i as i32));
            max = i;
        }
        assert_eq!(max, 4999);
        Ok(())
    }

    #[test]
    fn test_pvec_pop() -> VMResult<()> {
        let mut vm = Vm::new();
        vm.pause_gc();
        let mut pvec = PersistentVec::new();
        assert_eq!(pvec.len(), 0);
        assert!(pvec.is_empty());
        for i in 0..5000 {
            pvec = pvec.push(Value::Int32(i), &mut vm);
        }
        assert_eq!(pvec.len(), 5000);
        assert!(!pvec.is_empty());
        for _i in 0..2000 {
            pvec = pvec.pop(&mut vm);
        }
        assert_eq!(pvec.len(), 3000);
        assert!(!pvec.is_empty());
        let iter = vm.alloc_persistent_vector(pvec).iter(&vm);
        let mut max = 0;
        for (i, val) in iter.enumerate() {
            assert_eq!(val, Value::Int32(i as i32));
            max = i;
        }
        assert_eq!(max, 2999);

        for _i in 16..3000 {
            pvec = pvec.pop(&mut vm);
        }
        assert_eq!(pvec.len(), 16);
        assert!(!pvec.is_empty());
        let iter = vm.alloc_persistent_vector(pvec).iter(&vm);
        let mut max = 0;
        for (i, val) in iter.enumerate() {
            assert_eq!(val, Value::Int32(i as i32));
            max = i;
        }
        assert_eq!(max, 15);
        for _i in 0..16 {
            pvec = pvec.pop(&mut vm);
        }
        assert_eq!(pvec.len(), 0);
        assert!(pvec.is_empty());
        Ok(())
    }

    #[test]
    fn test_pvec_pop_tail_only() -> VMResult<()> {
        let mut vm = Vm::new();
        vm.pause_gc();
        let mut pvec = PersistentVec::new();
        assert_eq!(pvec.len(), 0);
        assert!(pvec.is_empty());
        for i in 0..16 {
            pvec = pvec.push(Value::Int32(i), &mut vm);
        }
        assert_eq!(pvec.len(), 16);
        assert!(!pvec.is_empty());
        for _i in 0..8 {
            pvec = pvec.pop(&mut vm);
        }
        assert_eq!(pvec.len(), 8);
        assert!(!pvec.is_empty());
        let iter = vm.alloc_persistent_vector(pvec).iter(&vm);
        let mut max = 0;
        for (i, val) in iter.enumerate() {
            assert_eq!(val, Value::Int32(i as i32));
            max = i;
        }
        assert_eq!(max, 7);
        for _i in 0..8 {
            pvec = pvec.pop(&mut vm);
        }
        assert_eq!(pvec.len(), 0);
        assert!(pvec.is_empty());
        let iter = vm.alloc_persistent_vector(pvec).iter(&vm);
        let mut max = 0;
        for (i, val) in iter.enumerate() {
            assert_eq!(val, Value::Int32(i as i32));
            max = i;
        }
        assert_eq!(max, 0);
        let pvec2 = pvec;
        pvec.pop(&mut vm);
        assert_eq!(pvec, pvec2);
        Ok(())
    }
}
