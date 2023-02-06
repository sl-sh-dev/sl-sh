use std::fmt::{Display, Formatter};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Handle {
    idx: u32,
}

impl Handle {
    pub fn new(idx: usize) -> Self {
        Self { idx: idx as u32 }
    }

    pub fn new32(idx: u32) -> Self {
        Self { idx }
    }

    pub fn idx(&self) -> usize {
        self.idx as usize
    }
}

impl From<u32> for Handle {
    fn from(idx: u32) -> Self {
        Self { idx }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Numeric64Handle(u32);

impl From<u32> for Numeric64Handle {
    fn from(idx: u32) -> Self {
        Self(idx)
    }
}

impl From<Numeric64Handle> for u32 {
    fn from(handle: Numeric64Handle) -> Self {
        handle.0
    }
}

impl From<Numeric64Handle> for usize {
    fn from(handle: Numeric64Handle) -> Self {
        handle.0 as usize
    }
}

impl Display for Numeric64Handle {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Numeric64Handle {
    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}
