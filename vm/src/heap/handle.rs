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
