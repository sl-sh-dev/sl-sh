use std::collections::HashSet;
use std::mem;

// This interner is initially based on this: https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html
// Inspiration also from: https://github.com/CAD97/simple-interner/blob/master/src/interner.rs
// See https://www.reddit.com/r/rust/comments/fn1jxf/blog_post_fast_and_simple_rust_interner/
// This is a simple string interner.  It hands out &'static str and it WILL leak memory
// to keep them valid.  Intended to live for the programs lifetime.
#[derive(Clone, Debug)]
pub struct Interner {
    map: HashSet<&'static str>,
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
            map: HashSet::default(),
            buf: mem::ManuallyDrop::new(String::with_capacity(cap)),
            capacity: cap,
            used: 0,
        }
    }

    /// True if name is an interned symbol.
    pub fn contains(&self, name: &str) -> bool {
        self.map.contains(name)
    }

    /// Intern name in this interner.  Will return the existing symbol if it
    /// exists or add it and and return it if not.
    pub fn intern(&mut self, name: &str) -> &'static str {
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
        self.map.insert(name);
        name
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
    fn test_one() {
        let mut i = Interner::with_capacity(7);
        assert!(i.capacity() == 8);
        assert!(i.used() == 0);
        assert!(i.len() == 0);
        let one = i.intern("one");
        assert!(one == "one");
        assert!(i.used() == 3);
        assert!(i.len() == 1);
        let fives = i.intern("fives");
        assert!(fives == "fives");
        assert!(i.capacity() == 8);
        assert!(i.used() == 8);
        assert!(i.len() == 2);
        let v1 = i.intern("xxx");
        assert!(one == "one");
        assert!(fives == "fives");
        assert!(v1 == "xxx");
        assert!(i.capacity() == 24);
        assert!(i.used() == 11);
        assert!(i.len() == 3);

        let one2 = i.intern("one");
        assert!(one == "one");
        assert!(one2 == "one");
        assert!(one == one2);
        assert!(std::ptr::eq(one, one2));
        assert!(i.capacity() == 24);
        assert!(i.used() == 11);
        assert!(i.len() == 3);

        let v2 = i.intern("1234567890");
        assert!(one == "one");
        assert!(fives == "fives");
        assert!(v1 == "xxx");
        assert!(v2 == "1234567890");
        assert!(i.capacity() == 24);
        assert!(i.used() == 21);
        assert!(i.len() == 4);

        let v3 = i.intern("1234");
        assert!(one == "one");
        assert!(fives == "fives");
        assert!(v1 == "xxx");
        assert!(v2 == "1234567890");
        assert!(v3 == "1234");
        assert!(i.capacity() == 56);
        assert!(i.used() == 25);
        assert!(i.len() == 5);

        let v2_2 = i.intern("1234567890");
        assert!(v2 == "1234567890");
        assert!(v2_2 == "1234567890");
        assert!(v2 == v2_2);
        assert!(std::ptr::eq(v2, v2_2));
        assert!(i.capacity() == 56);
        assert!(i.used() == 25);
        assert!(i.len() == 5);
    }
}
