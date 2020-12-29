pub type OpCode = u8;

pub const RET: OpCode = 0x01;
// Load a constant onto the stack
const STACK_BASE: OpCode = 0x02;
pub const CONST: OpCode = STACK_BASE;
pub const CONST2: OpCode = STACK_BASE + 1;
pub const CONST4: OpCode = STACK_BASE + 2;
pub const LOAD: OpCode = STACK_BASE + 3;
pub const LOAD2: OpCode = STACK_BASE + 4;
pub const POP: OpCode = STACK_BASE + 5;
pub const STORE: OpCode = STACK_BASE + 6;
pub const STORE2: OpCode = STACK_BASE + 7;

// Basic math
const MATH_BASE: OpCode = STACK_BASE + 8;
pub const ADD: OpCode = MATH_BASE;
pub const SUB: OpCode = MATH_BASE + 1;
pub const MUL: OpCode = MATH_BASE + 2;
pub const DIV: OpCode = MATH_BASE + 3;

// Cons cells
const CONS_BASE: OpCode = MATH_BASE + 4;
pub const CONS: OpCode = CONS_BASE;
pub const CAR: OpCode = CONS_BASE + 1;
pub const CDR: OpCode = CONS_BASE + 2;
pub const XAR: OpCode = CONS_BASE + 3;
pub const XDR: OpCode = CONS_BASE + 4;
pub const LIST: OpCode = CONS_BASE + 5;
