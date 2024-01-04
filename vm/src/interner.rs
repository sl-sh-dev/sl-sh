use std::collections::HashMap;
use std::mem;
use std::hash::{Hash, Hasher};

// This interner is initially based on this: https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html
// Inspiration also from: https://github.com/CAD97/simple-interner/blob/master/src/interner.rs
// See https://www.reddit.com/r/rust/comments/fn1jxf/blog_post_fast_and_simple_rust_interner/
// This is a simple string interner.  It hands out &'static str and it WILL leak memory
// to keep them valid.  Intended to live for the programs lifetime.
#[derive(Clone, Debug)]
pub struct Interner {
    map: HashMap<&'static str, Interned>,
    vals: Vec<&'static str>,
    // Leak buffers to keep the static lifetimes we hand out valid.
    buf: mem::ManuallyDrop<String>,
    capacity: usize,
    used: usize,
}

impl Interner {
    /// Create an interner with capacity cap (to the next power of two).
    pub fn with_capacity(cap: usize) -> Interner {
        let cap = cap.next_power_of_two();
        Interner {
            map: HashMap::default(),
            vals: Vec::new(),
            buf: mem::ManuallyDrop::new(String::with_capacity(cap)),
            capacity: cap,
            used: 0,
        }
    }

    /// True if name is an interned symbol.
    pub fn contains(&self, name: &str) -> bool {
        self.map.contains_key(name)
    }

    fn intern_final(&mut self, name: &'static str) -> Interned {
        let id = self.vals.len() as u32;
        self.vals.push(name);
        let interned = Interned { id };
        self.map.insert(name, interned);
        interned
    }

    /// If name is interned then return it, otherwise None.
    pub fn get_if_interned(&self, name: &str) -> Option<Interned> {
        self.map.get(name).copied()
    }

    /// Intern name in this interner.  Will return the existing symbol if it
    /// exists or add it and and return it if not.  Use this if you already have
    /// a static str reference to avoid wasting space on making another.
    pub fn intern_static(&mut self, name: &'static str) -> Interned {
        if let Some(&id) = self.map.get(name) {
            return id;
        }
        self.intern_final(name)
    }

    /// Intern name in this interner.  Will return the existing symbol if it
    /// exists or add it and and return it if not.
    pub fn intern(&mut self, name: &str) -> Interned {
        if let Some(&id) = self.map.get(name) {
            return id;
        }
        let name = {
            let cap = self.buf.capacity();
            if cap < self.buf.len() + name.len() {
                let new_cap = (cap.max(name.len()) + 1).next_power_of_two();
                let new_buf = mem::ManuallyDrop::new(String::with_capacity(new_cap));
                self.capacity += new_cap;
                // Leak memory to keep the static lifetimes valid.
                let _old_buf = mem::replace(&mut self.buf, new_buf);
            }

            let start = self.buf.len();
            self.buf.push_str(name);
            self.used += name.len();
            unsafe { &*(&self.buf[start..] as *const str) }
        };
        self.intern_final(name)
    }

    pub fn get_string(&self, interned: Interned) -> Option<&'static str> {
        if let Some(s) = self.vals.get(interned.id as usize) {
            Some(s)
        } else {
            None
        }
    }

    /// Return the amount of memory allocated by the interner.
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Return the amount of memory used to store symbols in the interner.
    pub fn used(&self) -> usize {
        self.used
    }

    /// Return the number of symbols in the interner.
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Are there no symbols in this interner?
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic() {
        let mut i = Interner::with_capacity(7);
        assert!(i.capacity() == 8);
        assert!(i.used() == 0);
        assert!(i.is_empty());
        let one = i.intern("one");
        assert!(i.get_string(one).unwrap() == "one");
        assert!(i.used() == 3);
        assert!(i.len() == 1);
        let fives = i.intern("fives");
        assert!(i.get_string(fives).unwrap() == "fives");
        assert!(i.capacity() == 8);
        assert!(i.used() == 8);
        assert!(i.len() == 2);
        let v1 = i.intern("xxx");
        assert!(i.get_string(one).unwrap() == "one");
        assert!(i.get_string(fives).unwrap() == "fives");
        assert!(i.get_string(v1).unwrap() == "xxx");
        assert!(i.capacity() == 24);
        assert!(i.used() == 11);
        assert!(i.len() == 3);

        let one2 = i.intern("one");
        assert!(i.get_string(one).unwrap() == "one");
        assert!(i.get_string(one2).unwrap() == "one");
        assert!(one == one2);
        assert!(std::ptr::eq(
            i.get_string(one).unwrap(),
            i.get_string(one2).unwrap()
        ));
        assert!(i.capacity() == 24);
        assert!(i.used() == 11);
        assert!(i.len() == 3);

        let v2 = i.intern("1234567890");
        assert!(i.get_string(one).unwrap() == "one");
        assert!(i.get_string(fives).unwrap() == "fives");
        assert!(i.get_string(v1).unwrap() == "xxx");
        assert!(i.get_string(v2).unwrap() == "1234567890");
        assert!(i.capacity() == 24);
        assert!(i.used() == 21);
        assert!(i.len() == 4);

        let v3 = i.intern("1234");
        assert!(i.get_string(one).unwrap() == "one");
        assert!(i.get_string(fives).unwrap() == "fives");
        assert!(i.get_string(v1).unwrap() == "xxx");
        assert!(i.get_string(v2).unwrap() == "1234567890");
        assert!(i.get_string(v3).unwrap() == "1234");
        assert!(i.capacity() == 56);
        assert!(i.used() == 25);
        assert!(i.len() == 5);

        let v2_2 = i.intern("1234567890");
        assert!(i.get_string(v2).unwrap() == "1234567890");
        assert!(i.get_string(v2_2).unwrap() == "1234567890");
        assert!(v2 == v2_2);
        assert!(std::ptr::eq(
            i.get_string(v2).unwrap(),
            i.get_string(v2_2).unwrap()
        ));
        assert!(i.capacity() == 56);
        assert!(i.used() == 25);
        assert!(i.len() == 5);
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Interned {
    pub id: u32,
}

impl PartialEq for Interned {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Interned {}

impl Hash for Interned {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u32(self.id);
    }
}
