use crate::{FxHasher, GVm, Handle, Value};
use std::hash::{Hash, Hasher};

const BITS: u64 = 5;
const WIDTH: usize = 1 << BITS; // 2^5 = 32
const MASK: u64 = (WIDTH as u64) - 1; // 31, or 0x1f

const PUSH_STACK_SIZE: usize = 13;

/// Simple little stack based "stack" for tracking paths through a tree.
/// This saves allocations or for some operations.
/// 13 levels deep at a width of 5 bits covers 64 bit hashes.
struct PathStack {
    path: [Option<(MapNode, usize)>; PUSH_STACK_SIZE],
    idx: usize,
}

impl PathStack {
    fn new() -> PathStack {
        PathStack {
            path: [None; PUSH_STACK_SIZE],
            idx: 0,
        }
    }

    fn push(&mut self, val: (MapNode, usize)) {
        self.idx += 1;
        if self.idx >= PUSH_STACK_SIZE {
            panic!("depth of vector tree to deep!");
        }
        self.path[self.idx] = Some(val);
    }

    fn pop(&mut self) -> Option<(MapNode, usize)> {
        if self.idx > 0 {
            let r = self.path[self.idx].take();
            self.idx -= 1;
            r
        } else {
            None
        }
    }
}

/// A persistent map data structure, based on Bagwell's ideal Hash Tree.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct PersistentMap {
    // How many elements are in the vec.
    length: usize,
    // Need to keep PersistentVec Copy but maybe this should be a handle to a root on the heap.
    root: MapNode,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum NodeType {
    None,
    Ref(Handle),
    Value((Value, Value)),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct MapNode {
    id: u32,
    data: [NodeType; WIDTH],
}

impl MapNode {
    fn new(id: u32) -> Self {
        Self {
            id,
            data: [NodeType::None; WIDTH],
        }
    }
}

impl PersistentMap {
    /// Create a new empty PersistentMap.
    pub fn new() -> Self {
        Self {
            length: 0,
            root: MapNode::new(0),
        }
    }

    /// Number of items in the map.
    pub fn len(&self) -> usize {
        self.length
    }

    /// Is the map empty?
    pub fn is_empty(&self) -> bool {
        self.length == 0
    }

    /// Lookup key and return its value if found, None otherwise.
    pub fn get<ENV>(&self, key: Value, vm: &GVm<ENV>) -> Option<Value> {
        let mut hasher = FxHasher::default();
        key.hash(&mut hasher);
        let hash = hasher.finish();
        let mut shift = 64 - BITS;
        let mut node = self.root;
        loop {
            let branch = ((hash >> shift) & MASK) as usize;
            match node.data[branch] {
                NodeType::None => return None,
                NodeType::Ref(handle) => {
                    node = *vm.get_mapnode(handle);
                }
                NodeType::Value((key_val, val)) => {
                    return if key == key_val { Some(val) } else { None };
                }
            }
            if shift < BITS {
                if shift > 1 {
                    shift = 1;
                } else {
                    return None;
                }
            } else {
                shift -= BITS;
            }
        }
    }

    /// Lookup key and return true if it exists in map.
    pub fn contains<ENV>(&self, key: Value, vm: &GVm<ENV>) -> bool {
        self.get(key, vm).is_some()
    }

    fn dec_shift(&self, shift: u64) -> u64 {
        if shift < BITS {
            1
        } else {
            shift - BITS
        }
    }

    /// Insert a new key, value and return the new map.
    pub fn insert<ENV>(&self, key: Value, val: Value, vm: &mut GVm<ENV>) -> Self {
        let mut hasher = FxHasher::default();
        key.hash(&mut hasher);
        let hash = hasher.finish();
        let mut shift = 64 - BITS;
        let mut new_map = *self;
        let mut node = new_map.root;
        let mut path = PathStack::new();
        loop {
            let branch = ((hash >> shift) & MASK) as usize;
            match node.data[branch] {
                NodeType::None => {
                    node.data[branch] = NodeType::Value((key, val));
                    while let Some((mut path_node, path_branch)) = path.pop() {
                        path_node.data[path_branch] = NodeType::Ref(vm.alloc_mapnode(node));
                        node = path_node;
                    }
                    new_map.length += 1;
                    new_map.root = node;
                    return new_map;
                }
                NodeType::Ref(handle) => {
                    path.push((node, branch));
                    node = *vm.get_mapnode(handle);
                }
                NodeType::Value((key2, val2)) => {
                    if key == key2 {
                        if val == val2 {
                            return new_map;
                        } else {
                            new_map.length -= 1;
                            node.data[branch] = NodeType::None;
                            continue;
                        }
                    }
                    let mut hasher = FxHasher::default();
                    key2.hash(&mut hasher);
                    let hash2 = hasher.finish();
                    let shift2 = self.dec_shift(shift);
                    let branch2 = ((hash2 >> shift2) & MASK) as usize;
                    let mut new_node = MapNode::new(new_map.root.id);
                    new_node.data[branch2] = NodeType::Value((key2, val2));
                    node.data[branch] = NodeType::Ref(vm.alloc_mapnode(new_node));
                    continue;
                }
            }
            if shift == 1 {
                break;
            }
            shift = self.dec_shift(shift);
        }
        new_map
    }

    /// Remove a key, returns the updated map if the key is found and removed.
    pub fn remove<ENV>(&self, key: Value, vm: &mut GVm<ENV>) -> Option<Self> {
        let mut hasher = FxHasher::default();
        key.hash(&mut hasher);
        let hash = hasher.finish();
        let mut shift = 64 - BITS;
        let mut node = self.root;
        let mut path = PathStack::new();
        loop {
            let branch = ((hash >> shift) & MASK) as usize;
            match node.data[branch] {
                NodeType::None => {
                    break;
                }
                NodeType::Ref(handle) => {
                    path.push((node, branch));
                    node = *vm.get_mapnode(handle);
                }
                NodeType::Value((key2, _)) => {
                    if key == key2 {
                        let mut new_map = *self;
                        new_map.length -= 1;
                        node.data[branch] = NodeType::None;
                        while let Some((mut path_node, path_branch)) = path.pop() {
                            path_node.data[path_branch] = NodeType::Ref(vm.alloc_mapnode(node));
                            node = path_node;
                        }
                        new_map.root = node;
                        return Some(new_map);
                    }
                    break;
                }
            }
            if shift == 1 {
                break;
            }
            shift = self.dec_shift(shift);
        }
        None
    }
}

impl Default for PersistentMap {
    fn default() -> Self {
        Self::new()
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{VMResult, Vm};

    #[test]
    fn test_pmap() -> VMResult<()> {
        let mut vm = Vm::new();
        vm.pause_gc();
        let mut pmap = PersistentMap::new();
        let pmap_orig = pmap;

        assert_eq!(pmap.len(), 0);
        assert!(pmap.is_empty());

        pmap = pmap.insert(1.into(), 1.into(), &mut vm);
        pmap = pmap.insert(2.into(), 2.into(), &mut vm);
        pmap = pmap.insert(3.into(), 3.into(), &mut vm);
        assert_eq!(pmap.len(), 3);
        assert!(!pmap.is_empty());
        assert_eq!(pmap.get(1.into(), &vm).unwrap(), 1.into());
        assert_eq!(pmap.get(2.into(), &vm).unwrap(), 2.into());
        assert_eq!(pmap.get(3.into(), &vm).unwrap(), 3.into());
        let pmap2 = pmap;

        pmap = pmap.insert(3.into(), 30.into(), &mut vm);
        assert_eq!(pmap.len(), 3);
        assert!(!pmap.is_empty());
        assert_eq!(pmap.get(1.into(), &vm).unwrap(), 1.into());
        assert_eq!(pmap.get(2.into(), &vm).unwrap(), 2.into());
        assert_eq!(pmap.get(3.into(), &vm).unwrap(), 30.into());

        assert_eq!(pmap2.len(), 3);
        assert!(!pmap2.is_empty());
        assert_eq!(pmap2.get(1.into(), &vm).unwrap(), 1.into());
        assert_eq!(pmap2.get(2.into(), &vm).unwrap(), 2.into());
        assert_eq!(pmap2.get(3.into(), &vm).unwrap(), 3.into());

        assert_eq!(pmap_orig.len(), 0);
        assert!(pmap_orig.is_empty());

        let pmap3 = pmap.remove(2.into(), &mut vm).unwrap();
        assert_eq!(pmap3.len(), 2);
        assert!(!pmap3.is_empty());
        assert_eq!(pmap3.get(1.into(), &vm).unwrap(), 1.into());
        assert_eq!(pmap3.get(2.into(), &vm), None);
        assert_eq!(pmap3.get(3.into(), &vm).unwrap(), 30.into());
        assert_eq!(pmap3.remove(2.into(), &mut vm), None);

        assert_eq!(pmap.len(), 3);
        assert!(!pmap.is_empty());
        assert_eq!(pmap.get(1.into(), &vm).unwrap(), 1.into());
        assert_eq!(pmap.get(2.into(), &vm).unwrap(), 2.into());
        assert_eq!(pmap.get(3.into(), &vm).unwrap(), 30.into());

        Ok(())
    }
}
