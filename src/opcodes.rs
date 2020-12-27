pub type OpCode = u8;

pub const RET: OpCode = 0x01;
pub const CONST: OpCode = 0x02;
pub const CONST2: OpCode = 0x03;
pub const CONST4: OpCode = 0x04;
pub const ADD: OpCode = 0x05;
pub const SUB: OpCode = 0x06;
pub const MUL: OpCode = 0x07;
pub const DIV: OpCode = 0x08;
