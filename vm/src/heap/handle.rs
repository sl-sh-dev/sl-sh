#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Handle {
    valid: bool,
    idx: u32,
}

impl Handle {
    pub fn invalid() -> Self {
        Self {
            valid: false,
            idx: 0,
        }
    }

    pub fn new(idx: usize) -> Self {
        Self {
            valid: true,
            idx: idx as u32,
        }
    }

    pub fn idx(&self) -> usize {
        self.idx as usize
    }

    pub fn valid(&self) -> bool {
        self.valid
    }
}
