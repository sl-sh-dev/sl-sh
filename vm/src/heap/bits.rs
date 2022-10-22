pub const FLAG_MARK: u8 = 0x01;
pub const FLAG_STICKY: u8 = 0x02;
pub const FLAG_MUT: u8 = 0x04;
pub const FLAG_TRACED: u8 = 0x08;

#[macro_export]
macro_rules! is_bit_set {
    ($val:expr, $bit:expr) => {{
        ($val & $bit) != 0
    }};
}

#[macro_export]
macro_rules! set_bit {
    ($val:expr, $bit:expr) => {{
        $val |= $bit;
    }};
}

#[macro_export]
macro_rules! clear_bit {
    ($val:expr, $bit:expr) => {{
        if is_bit_set!($val, $bit) {
            $val ^= $bit;
        }
    }};
}

pub fn is_live(flag: u8) -> bool {
    is_bit_set!(flag, FLAG_MARK | FLAG_STICKY)
}

pub fn is_marked(flag: u8) -> bool {
    is_bit_set!(flag, FLAG_MARK)
}

pub fn is_mutable(flag: u8) -> bool {
    is_bit_set!(flag, FLAG_MUT)
}

pub fn is_traced(flag: u8) -> bool {
    is_bit_set!(flag, FLAG_TRACED)
}
