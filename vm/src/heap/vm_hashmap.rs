use crate::{GVm, Value};
use bridge_types::BridgedType;
use std::collections::HashMap;
use std::collections::hash_map::Keys;
use std::hash::{BuildHasher, Hash, Hasher};

/**
 * Provides a wrapper to allow us to build a hashmap with Value keys that hashes String and StringConst
 * to the same hash.
 * Note, this is public only to allow use of insert_id and remove_id which we need to work around
 * borrowing issues.
*/
#[derive(Copy, Clone, Debug)]
pub struct ValHash {
    val: Value,
    hash: u64,
}

impl ValHash {
    /** Make a ValHash from a Value. */
    pub fn from_value<ENV>(vm: &GVm<ENV>, val: Value) -> Self {
        ValHash {
            val,
            hash: val.get_hash(vm),
        }
    }
}

impl PartialEq for ValHash {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
    }
}

impl Eq for ValHash {}

impl Hash for ValHash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}

#[derive(Copy, Clone, Debug, Default)]
struct IdHasher {
    hash: u64,
}

impl BuildHasher for IdHasher {
    type Hasher = IdHasher;

    fn build_hasher(&self) -> Self::Hasher {
        *self
    }
}

impl Hasher for IdHasher {
    fn finish(&self) -> u64 {
        self.hash
    }

    fn write(&mut self, _bytes: &[u8]) {
        panic!("Invalid use of IdHasher!");
    }

    fn write_u64(&mut self, val: u64) {
        self.hash = val;
    }
}

/**
 * Wrapper class for a HashMap<Value, Value>.  We need this because we want String and StringConst
 * to hash to the same value (what a script user will expect) and that requires a VM so can not just
 * implement Hash on Value (will not have access to a VM).
 */
#[derive(Clone, Debug)]
pub struct VMHashMap {
    map: HashMap<ValHash, Value, IdHasher>,
}

impl VMHashMap {
    /** Create a new empty HashMap. */
    pub fn new() -> Self {
        VMHashMap {
            map: HashMap::default(),
        }
    }

    /** Create a new empty HashMap with an initial capacity. */
    pub fn with_capacity(cap: usize) -> Self {
        VMHashMap {
            map: HashMap::with_capacity_and_hasher(cap, IdHasher::default()),
        }
    }

    /** Get the value at key, requires the current VM for hashing. */
    pub fn get<ENV>(&self, vm: &GVm<ENV>, key: Value) -> Option<Value> {
        let id = ValHash::from_value(vm, key);
        self.map.get(&id).copied()
    }

    /** Insert the value at key, requires the current VM for hashing.
     * Returns the old value at key if it exists (None otherwise).
     */
    pub fn insert<ENV>(&mut self, vm: &GVm<ENV>, key: Value, val: Value) -> Option<Value> {
        let id = ValHash::from_value(vm, key);
        self.map.insert(id, val)
    }

    /** Insert val at the key id provided.  This allows calling code to pre-generate the ValHash.
     * This is a borrow checker workaround.
     */
    pub fn insert_id(&mut self, id: ValHash, val: Value) -> Option<Value> {
        self.map.insert(id, val)
    }

    /** Number of items in the HashMap. */
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /** Is this HashMap empty? */
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /** Does this HashMap contain key? */
    pub fn contains_key<ENV>(&self, vm: &GVm<ENV>, key: Value) -> bool {
        let id = ValHash::from_value(vm, key);
        self.map.contains_key(&id)
    }

    /** Clear (remove all key/values) from the HashMap. */
    pub fn clear(&mut self) {
        self.map.clear();
    }

    /** Remove key from the HashMap.  Return the old value if it existed (None otherwise). */
    pub fn remove<ENV>(&mut self, vm: &GVm<ENV>, key: Value) -> Option<Value> {
        let id = ValHash::from_value(vm, key);
        self.map.remove(&id)
    }

    /** Remove the key from HashMap (like remove) except caller pre-generates the ValHash.  Used to
     * work around the borrow checker. */
    pub fn remove_id(&mut self, id: ValHash) -> Option<Value> {
        self.map.remove(&id)
    }

    /** Returns an iterator over all the keys in the HashMap. */
    pub fn keys(&self) -> VMMapKeys<'_> {
        VMMapKeys {
            keys: self.map.keys(),
        }
    }

    /** Return an iterator over all the (key, value) pairs in the HashMap. */
    pub fn iter(&self) -> VMHashMapIter<'_> {
        VMHashMapIter {
            iter: self.map.iter(),
        }
    }
}

/// A [`VMHashMap`] that contains a [`BridgedType`] can be represented as a rust value.
impl BridgedType for VMHashMap {}

impl Default for VMHashMap {
    fn default() -> Self {
        Self::new()
    }
}

/** Iterator over the key vals in a HashMap. */
pub struct VMHashMapIter<'a> {
    iter: std::collections::hash_map::Iter<'a, ValHash, Value>,
}

impl Iterator for VMHashMapIter<'_> {
    type Item = (Value, Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(k, v)| (k.val, *v))
    }
}

/** Iterator over the keys in a HashMap. */
pub struct VMMapKeys<'a> {
    keys: Keys<'a, ValHash, Value>,
}

impl Iterator for VMMapKeys<'_> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.keys.next().map(|v| v.val)
    }
}

#[cfg(test)]
mod tests {
    use crate::vm_hashmap::VMHashMap;
    use crate::{Value, Vm};

    #[test]
    fn test_map_str() {
        let mut vm = Vm::new();
        let mut m = VMHashMap::default();
        let cs = Value::StringConst(vm.intern("Test String"));
        let ds = vm.alloc_string("Test String".to_string());
        let i: Value = 1.into();
        m.insert(&mut vm, cs, i);
        assert_eq!(m.get(&vm, cs).unwrap(), i);
        assert_eq!(m.get(&vm, ds).unwrap(), i);
        let i: Value = 10.into();
        m.insert(&mut vm, ds, i);
        assert_eq!(m.get(&vm, cs).unwrap(), i);
        assert_eq!(m.get(&vm, ds).unwrap(), i);
        let old = m.remove(&mut vm, cs).unwrap();
        assert_eq!(old, i);
        assert!(m.get(&vm, cs).is_none());
        assert!(m.get(&vm, ds).is_none());
    }

    #[test]
    fn test_map_sym_key_sanity() {
        let mut vm = Vm::new();
        let mut m = VMHashMap::default();
        let sym = Value::Symbol(vm.intern("Test String"));
        let key = Value::Keyword(vm.intern("Test String"));
        let i: Value = 1.into();
        m.insert(&mut vm, sym, i);
        assert_eq!(m.get(&vm, sym).unwrap(), i);
        assert!(m.get(&vm, key).is_none());
        let i2: Value = 10.into();
        m.insert(&mut vm, key, i2);
        assert_eq!(m.get(&vm, sym).unwrap(), i);
        assert_eq!(m.get(&vm, key).unwrap(), i2);
        let old = m.remove(&mut vm, sym).unwrap();
        assert_eq!(old, i);
        assert!(m.get(&vm, sym).is_none());
        assert_eq!(m.get(&vm, key).unwrap(), i2);
    }

    #[test]
    fn test_map_key_iter_sanity() {
        let mut vm = Vm::new();
        let mut m = VMHashMap::default();
        let key1 = Value::Keyword(vm.intern("one"));
        let key2 = Value::Keyword(vm.intern("two"));
        let key3 = Value::Keyword(vm.intern("three"));
        assert_eq!(m.keys().count(), 0);
        let i1: Value = 1.into();
        m.insert(&mut vm, key1, i1);
        let i2: Value = 1.into();
        m.insert(&mut vm, key2, i2);
        let i3: Value = 1.into();
        m.insert(&mut vm, key3, i3);
        assert_eq!(m.keys().count(), 3);
        m.remove(&mut vm, key1);
        assert_eq!(m.keys().count(), 2);
    }

    #[test]
    fn test_map_iter_sanity() {
        let mut vm = Vm::new();
        let mut m = VMHashMap::default();
        let key1 = Value::Keyword(vm.intern("one"));
        let key2 = Value::Keyword(vm.intern("two"));
        let key3 = Value::Keyword(vm.intern("three"));
        assert_eq!(m.iter().count(), 0);
        let i1: Value = 1.into();
        m.insert(&mut vm, key1, i1);
        let i2: Value = 1.into();
        m.insert(&mut vm, key2, i2);
        let i3: Value = 1.into();
        m.insert(&mut vm, key3, i3);
        assert_eq!(m.iter().count(), 3);
        m.remove(&mut vm, key1);
        assert_eq!(m.iter().count(), 2);
    }
}
