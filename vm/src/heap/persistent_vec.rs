//! Implements a persistent vector against the slosh heap.  This is like the Clojure PersistentVector.
//! See the series of blog post starting at https://hypirion.com/musings/understanding-persistent-vector-pt-1
//! for more details.

use crate::{GVm, Handle, Value};

const BITS: usize = 5;
const WIDTH: usize = 1 << BITS; // 2^5 = 32
const MASK: usize = WIDTH - 1; // 31, or 0x1f

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct PersistentVec {
    id: u32,
    length: usize,
    tail_length: usize,
    shift: usize,
    root: Option<VecNode>,
    tail: [Value; WIDTH],
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum NodeType {
    Node([Handle; WIDTH]),
    Leaf([Value; WIDTH]),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct VecNode {
    id: u32,
    data: NodeType,
}

fn new_path<ENV>(id: u32, shift: usize, node_handle: Handle, vm: &mut GVm<ENV>) -> Handle {
    if shift == 0 {
        node_handle
    } else {
        let mut ret = VecNode {
            id,
            data: NodeType::Node([Handle::invalid(); WIDTH]),
        };
        if let NodeType::Node(handles) = &mut ret.data {
            handles[0] = new_path(id, shift - 5, node_handle, vm);
        }
        vm.alloc_vecnode(ret)
    }
}

impl PersistentVec {
    /// Create a new empty persistent vector.
    pub fn new() -> Self {
        Self {
            id: 0,
            length: 0,
            tail_length: 0,
            shift: 0,
            root: None,
            tail: [Value::Undefined; WIDTH],
        }
    }

    fn push_tail<ENV>(
        &self,
        level: usize,
        parent: &VecNode,
        tailnode_handle: Handle,
        vm: &mut GVm<ENV>,
    ) -> VecNode {
        let subidx = ((self.length - 1) >> level) & MASK;
        let mut ret = *parent;
        let node_to_insert = if level == 5 {
            tailnode_handle
        } else {
            let child_handle = if let NodeType::Node(handles) = parent.data {
                handles[subidx]
            } else {
                Handle::invalid()
            };
            if child_handle.valid() {
                let child = *vm.get_vecnode(child_handle);
                let tail = self.push_tail(level - BITS, &child, tailnode_handle, vm);
                vm.alloc_vecnode(tail)
            } else {
                new_path(parent.id, level - BITS, tailnode_handle, vm)
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
            id: new_vec.id,
            data: NodeType::Leaf(self.tail),
        };
        let new_root = if (self.length >> BITS) > (1 << self.shift) {
            // root overflow
            let new_root = if let Some(root_node) = self.root {
                let mut new_root = VecNode {
                    id: new_vec.id,
                    data: NodeType::Node([Handle::invalid(); WIDTH]),
                };
                if let NodeType::Node(handles) = &mut new_root.data {
                    handles[0] = vm.alloc_vecnode(root_node);
                    handles[1] =
                        new_path(new_vec.id, new_vec.shift, vm.alloc_vecnode(new_tail), vm);
                }
                new_root
            } else {
                VecNode {
                    id: new_vec.id,
                    data: NodeType::Leaf(self.tail),
                }
            };
            new_vec.shift += BITS;
            new_root
        } else {
            // push the tail
            let new_root = if let Some(root) = &self.root {
                let new_tail_handle = vm.alloc_vecnode(new_tail);
                self.push_tail(self.shift, root, new_tail_handle, vm)
            } else {
                new_tail
            };
            new_root
        };
        new_vec.root = Some(new_root);
        new_vec.tail[0] = value;
        new_vec.length += 1;
        new_vec.tail_length = 1;
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

        match self.root {
            None => return None,
            Some(mut node) => {
                let mut level = self.shift;
                loop {
                    match node.data {
                        NodeType::Node(handles) => {
                            node = *vm.get_vecnode(handles[(idx >> level) & MASK]);
                        }
                        NodeType::Leaf(values) => return Some(values[idx & MASK]),
                    }
                    if level == 0 {
                        break;
                    }
                    level -= BITS;
                }
            }
        }
        None
    }

    /// Replace the value ad idx with new_value and return a new vec.
    pub fn replace<ENV>(&self, idx: usize, new_value: Value, vm: &mut GVm<ENV>) -> Option<PersistentVec> {
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

        let mut path = Vec::new();
        match self.root {
            None => return None,
            Some(mut node) => {
                let mut level = self.shift;
                loop {
                    match &mut node.data {
                        NodeType::Node(handles) => {
                            let next_node = *vm.get_vecnode(handles[(idx >> level) & MASK]);
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
                            new_vec.root = Some(node);
                            return Some(new_vec);
                        },
                    }
                    if level == 0 {
                        break;
                    }
                    level -= BITS;
                }
            }
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
        Self {
            vm,
            vec,
            idx: 0,
        }
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
        let pvec = PersistentVec::new();
        assert_eq!(pvec.len(), 0);
        assert!(pvec.is_empty());
        let pvec2 = pvec.push(Value::Int(0), &mut vm);
        assert_eq!(pvec.len(), 0);
        assert!(pvec.is_empty());
        assert_eq!(pvec2.len(), 1);
        assert!(!pvec2.is_empty());
        assert_eq!(pvec2.get(0, &vm), Some(Value::Int(0)));
        let mut pvec3 = pvec2;
        for i in 1..33 {
            pvec3 = pvec3.push(Value::Int(i), &mut vm);
        }
        assert_eq!(pvec3.len(), 33);
        assert!(!pvec3.is_empty());
        assert_eq!(pvec3.get(0, &vm), Some(Value::Int(0)));
        assert_eq!(pvec3.get(16, &vm), Some(Value::Int(16)));
        assert_eq!(pvec3.get(31, &vm), Some(Value::Int(31)));
        assert_eq!(pvec3.get(32, &vm), Some(Value::Int(32)));
        let old_pvec3 = pvec3;

        for i in 33..1000 {
            pvec3 = pvec3.push(Value::Int(i), &mut vm);
        }
        assert_eq!(old_pvec3.len(), 33);
        assert_eq!(pvec3.len(), 1000);
        assert!(!pvec3.is_empty());
        assert_eq!(pvec3.get(0, &vm), Some(Value::Int(0)));
        assert_eq!(pvec3.get(16, &vm), Some(Value::Int(16)));
        assert_eq!(pvec3.get(31, &vm), Some(Value::Int(31)));
        assert_eq!(pvec3.get(32, &vm), Some(Value::Int(32)));
        assert_eq!(pvec3.get(48, &vm), Some(Value::Int(48)));
        assert_eq!(pvec3.get(63, &vm), Some(Value::Int(63)));
        assert_eq!(pvec3.get(64, &vm), Some(Value::Int(64)));
        assert_eq!(pvec3.get(512, &vm), Some(Value::Int(512)));
        assert_eq!(pvec3.get(888, &vm), Some(Value::Int(888)));
        assert_eq!(pvec3.get(999, &vm), Some(Value::Int(999)));

        for i in 1000..5000 {
            pvec3 = pvec3.push(Value::Int(i), &mut vm);
        }
        assert_eq!(pvec3.len(), 5000);
        assert!(!pvec3.is_empty());
        assert_eq!(pvec3.get(0, &vm), Some(Value::Int(0)));
        assert_eq!(pvec3.get(16, &vm), Some(Value::Int(16)));
        assert_eq!(pvec3.get(31, &vm), Some(Value::Int(31)));
        assert_eq!(pvec3.get(32, &vm), Some(Value::Int(32)));
        assert_eq!(pvec3.get(48, &vm), Some(Value::Int(48)));
        assert_eq!(pvec3.get(63, &vm), Some(Value::Int(63)));
        assert_eq!(pvec3.get(64, &vm), Some(Value::Int(64)));
        assert_eq!(pvec3.get(512, &vm), Some(Value::Int(512)));
        assert_eq!(pvec3.get(888, &vm), Some(Value::Int(888)));
        assert_eq!(pvec3.get(999, &vm), Some(Value::Int(999)));
        assert_eq!(pvec3.get(3999, &vm), Some(Value::Int(3999)));
        assert_eq!(pvec3.get(4999, &vm), Some(Value::Int(4999)));
        Ok(())
    }

    #[test]
    fn test_pvec_replace() -> VMResult<()> {
        let mut vm = Vm::new();
        let mut pvec = PersistentVec::new();
        assert_eq!(pvec.len(), 0);
        assert!(pvec.is_empty());
        assert!(pvec.replace(0, Value::Nil, &mut vm).is_none());
        assert!(pvec.replace(32, Value::Nil, &mut vm).is_none());
        for i in 0..5000 {
            pvec = pvec.push(Value::Int(i), &mut vm);
        }
        assert_eq!(pvec.len(), 5000);
        assert!(!pvec.is_empty());
        assert_eq!(pvec.get(0, &vm), Some(Value::Int(0)));
        pvec = pvec.replace(0, Value::Nil, &mut vm).unwrap();
        assert_eq!(pvec.get(0, &vm), Some(Value::Nil));
        assert_eq!(pvec.get(31, &vm), Some(Value::Int(31)));
        pvec = pvec.replace(31, Value::Nil, &mut vm).unwrap();
        assert_eq!(pvec.get(31, &vm), Some(Value::Nil));
        assert_eq!(pvec.get(32, &vm), Some(Value::Int(32)));
        pvec = pvec.replace(32, Value::Nil, &mut vm).unwrap();
        assert_eq!(pvec.get(32, &vm), Some(Value::Nil));
        assert_eq!(pvec.get(1024, &vm), Some(Value::Int(1024)));
        pvec = pvec.replace(1024, Value::Nil, &mut vm).unwrap();
        assert_eq!(pvec.get(1024, &vm), Some(Value::Nil));
        assert_eq!(pvec.get(2000, &vm), Some(Value::Int(2000)));
        pvec = pvec.replace(2000, Value::Nil, &mut vm).unwrap();
        assert_eq!(pvec.get(2000, &vm), Some(Value::Nil));
        assert_eq!(pvec.get(4999, &vm), Some(Value::Int(4999)));
        pvec = pvec.replace(4999, Value::Nil, &mut vm).unwrap();
        assert_eq!(pvec.get(4999, &vm), Some(Value::Nil));
        Ok(())
    }

    #[test]
    fn test_pvec_iter() -> VMResult<()> {
        let mut vm = Vm::new();
        let mut pvec = PersistentVec::new();
        assert_eq!(pvec.len(), 0);
        assert!(pvec.is_empty());
        assert!(pvec.replace(0, Value::Nil, &mut vm).is_none());
        assert!(pvec.replace(32, Value::Nil, &mut vm).is_none());
        for i in 0..5000 {
            pvec = pvec.push(Value::Int(i), &mut vm);
        }
        assert_eq!(pvec.len(), 5000);
        assert!(!pvec.is_empty());
        let iter = vm.alloc_persistent_vector(pvec).iter(&vm);
        let mut max = 0;
        for (i, val) in iter.enumerate() {
            assert_eq!(val, Value::Int(i as i64));
            max = i;
        }
        assert_eq!(max, 4999);
        Ok(())
    }
}
