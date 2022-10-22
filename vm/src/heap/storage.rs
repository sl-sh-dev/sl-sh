use crate::bits::{is_live, is_marked, is_mutable, is_traced, FLAG_MARK, FLAG_STICKY, FLAG_TRACED};
use crate::{clear_bit, is_bit_set, set_bit, Handle};

#[derive(Debug)]
pub(super) struct Storage<T: Clone> {
    flags: Vec<u8>,
    vals: Vec<T>,
    capacity: usize,
    live_objects: usize,
    sticky_objects: usize,
    grow_factor: f64,
}

impl<T: Clone> Storage<T> {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            flags: Vec::with_capacity(capacity),
            // Keep one extra slot to do sway on replace.
            vals: Vec::with_capacity(capacity + 1),
            capacity,
            live_objects: 0,
            sticky_objects: 0,
            grow_factor: 2.0,
        }
    }

    pub fn vals(&self) -> &[T] {
        &self.vals
    }

    pub fn vals_mut(&mut self) -> &mut [T] {
        &mut self.vals
    }

    pub fn get_mut(&mut self, idx: usize) -> Option<&mut T> {
        self.vals.get_mut(idx)
    }

    pub fn flags(&self) -> &[u8] {
        &self.flags
    }

    pub fn flags_mut(&mut self) -> &mut [u8] {
        &mut self.flags
    }

    pub fn set_grow_factor(&mut self, grow_factor: f64) {
        self.grow_factor = grow_factor;
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    pub fn live_objects(&self) -> usize {
        self.live_objects
    }

    pub fn alloc<CollectFunc>(&mut self, obj: T, flags: u8, collect: CollectFunc) -> Handle
    where
        CollectFunc: FnOnce() -> (),
    {
        if self.live_objects >= self.capacity {
            collect();
            let new_min = (self.live_objects as f64 * self.grow_factor) as usize;
            if new_min > self.capacity {
                self.capacity = new_min;
                self.flags.reserve(new_min - self.flags.len());
                self.vals.reserve((new_min - self.vals.len()) + 1);
            }
        }
        if self.vals.len() < self.capacity {
            let idx = self.vals.len();
            self.vals.push(obj);
            self.flags.push(flags | FLAG_MARK);
            self.live_objects += 1;
            Handle::new(idx)
        } else {
            for (idx, flag) in self.flags.iter_mut().enumerate() {
                if !is_live(*flag) {
                    self.live_objects += 1;
                    *flag = flags | FLAG_MARK;
                    self.vals.push(obj);
                    self.vals.swap_remove(idx);
                    let handle = Handle::new(idx);
                    return handle;
                }
            }
            panic!("Failed to allocate to heap- no free objects and no capacity!");
        }
    }

    pub fn clear_marks(&mut self) {
        self.live_objects = 0;
        self.live_objects = 0;
        for flag in self.flags.iter_mut() {
            clear_bit!(*flag, FLAG_MARK);
            clear_bit!(*flag, FLAG_TRACED);
            // if it is sticky mark it
            if is_bit_set!(*flag, FLAG_STICKY) {
                self.live_objects += 1;
                set_bit!(*flag, FLAG_MARK);
            }
        }
    }

    pub fn is_live(&self, idx: usize) -> bool {
        if let Some(flag) = self.flags.get(idx) {
            is_live(*flag)
        } else {
            false
        }
    }

    pub fn is_marked(&self, idx: usize) -> bool {
        if let Some(flag) = self.flags.get(idx) {
            is_marked(*flag)
        } else {
            false
        }
    }

    pub fn is_traced(&self, idx: usize) -> bool {
        if let Some(flag) = self.flags.get(idx) {
            is_traced(*flag)
        } else {
            false
        }
    }

    pub fn is_mutable(&self, idx: usize) -> bool {
        if let Some(flag) = self.flags.get(idx) {
            is_mutable(*flag)
        } else {
            false
        }
    }

    pub fn mark(&mut self, idx: usize) {
        if let Some(flag) = self.flags.get_mut(idx) {
            if !is_marked(*flag) {
                self.live_objects += 1;
                set_bit!(*flag, FLAG_MARK);
            }
        } else {
            panic!("Invalid object handle in mark!")
        }
    }

    pub fn traced(&mut self, idx: usize) {
        if let Some(flag) = self.flags.get_mut(idx) {
            set_bit!(*flag, FLAG_TRACED);
        } else {
            panic!("Invalid object handle in traced!")
        }
    }

    pub fn is_traced_and_set(&mut self, idx: usize) -> bool {
        if let Some(flag) = self.flags.get_mut(idx) {
            let ret = is_traced(*flag);
            set_bit!(*flag, FLAG_TRACED);
            ret
        } else {
            panic!("Invalid object handle in traced!")
        }
    }

    pub fn sticky(&mut self, idx: usize) {
        if let Some(flag) = self.flags.get_mut(idx) {
            if !is_bit_set!(*flag, FLAG_STICKY) {
                self.sticky_objects += 1;
                set_bit!(*flag, FLAG_STICKY);
            }
        } else {
            panic!("Invalid object handle in sticky!")
        }
    }

    pub fn unsticky(&mut self, idx: usize) {
        if let Some(flag) = self.flags.get_mut(idx) {
            if is_bit_set!(*flag, FLAG_STICKY) {
                self.sticky_objects -= 1;
                clear_bit!(*flag, FLAG_STICKY);
            }
        } else {
            panic!("Invalid object handle in unsticky!")
        }
    }

    /// For any dead, live bit not set, objects in heap set them to val.
    pub fn set_all_dead(&mut self, val: T) {
        let mut cur = 0;
        let mut flags_iter = self.flags.iter();
        while let Some(flag) = flags_iter.next() {
            if !is_live(*flag) {
                self.vals.push(val.clone());
                self.vals.swap_remove(cur);
            }
            cur += 1;
        }
    }

    pub fn trace_all_live<FN: FnMut(&T)>(&mut self, mut trace: FN) {
        for (flag, value) in self.flags.iter_mut().zip(self.vals.iter()) {
            if is_live(*flag) {
                set_bit!(*flag, FLAG_TRACED);
                trace(value);
            }
        }
    }
}

impl<T: Clone> Default for Storage<T> {
    fn default() -> Self {
        Self::with_capacity(512)
    }
}
