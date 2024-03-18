use std::fmt;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Copy, Clone, Debug)]
pub struct F32Wrap(pub f32);
impl Display for F32Wrap {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl From<F32Wrap> for f32 {
    fn from(f: F32Wrap) -> Self {
        f.0
    }
}
impl From<f32> for F32Wrap {
    fn from(f: f32) -> Self {
        F32Wrap(f)
    }
}
impl From<f64> for F32Wrap {
    fn from(f: f64) -> Self {
        F32Wrap(f as f32)
    }
}
impl From<F32Wrap> for f64 {
    fn from(f: F32Wrap) -> Self {
        f.0 as f64
    }
}
impl PartialEq for F32Wrap {
    fn eq(&self, other: &Self) -> bool {
        (self.0 - other.0).abs() < f32::EPSILON
        //self.0.to_bits() == other.0.to_bits()
    }
}

impl Eq for F32Wrap {}

impl Hash for F32Wrap {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u32(self.0.to_bits());
    }
}
