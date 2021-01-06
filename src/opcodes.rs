pub type OpCode = u8;

pub const NOP: OpCode = 0x00;
pub const HALT: OpCode = 0x01;
pub const RET: OpCode = 0x02;
// Load a constant onto the stack
const STACK_BASE: OpCode = 0x03;
pub const STORE: OpCode = STACK_BASE + 1; // STORE A B - R(A) = R(B)
pub const STORE_K: OpCode = STACK_BASE + 2; // STORE_K A B - R(A) = K(B)
pub const REF: OpCode = STACK_BASE + 3; // REF A B - R(A) = G[default](R(B))
pub const REF_K: OpCode = STACK_BASE + 4; // REF_K A B - R(A) = G[default](K(B))
pub const REFNS: OpCode = STACK_BASE + 5; // REFNS A B C - R(A) = G[R(B)](R(C))
pub const REFNS_K: OpCode = STACK_BASE + 6; // REFNS_K A B C - R(A) = G[K(B)](K(C))
pub const BIND: OpCode = STACK_BASE + 7; // BIND A B - G[default](R(A)) = R(B)
pub const BIND_K: OpCode = STACK_BASE + 8; // BIND A B - G[default](K(A)) = R(B)
pub const BINDNS: OpCode = STACK_BASE + 9; // BINDNS A B C - G[R(A)](R(B)) = R(C)
pub const BINDNS_K: OpCode = STACK_BASE + 10; // BINDNS_K A B C - G[K(A)](K(B)) = R(C)

// Basic math
const MATH_BASE: OpCode = STACK_BASE + 11;
pub const ADD: OpCode = MATH_BASE;
pub const ADD_RK: OpCode = MATH_BASE + 1;
pub const ADD_KR: OpCode = MATH_BASE + 2;
pub const SUB: OpCode = MATH_BASE + 3;
pub const SUB_RK: OpCode = MATH_BASE + 4;
pub const SUB_KR: OpCode = MATH_BASE + 5;
pub const MUL: OpCode = MATH_BASE + 6;
pub const MUL_RK: OpCode = MATH_BASE + 7;
pub const MUL_KR: OpCode = MATH_BASE + 8;
pub const DIV: OpCode = MATH_BASE + 9;
pub const DIV_RK: OpCode = MATH_BASE + 10;
pub const DIV_KR: OpCode = MATH_BASE + 11;

// Cons cells
const CONS_BASE: OpCode = MATH_BASE + 12;
pub const CONS: OpCode = CONS_BASE; // CONS A B C - R(A) = conscell(R(B), R(C))
pub const CAR: OpCode = CONS_BASE + 1; // CAR A B - R(A) = car(R(B))
pub const CDR: OpCode = CONS_BASE + 2; // CDR A B - R(A) = cdr(R(B))
pub const XAR: OpCode = CONS_BASE + 3; // XAR A B - car(R(A)) = R(B)
pub const XDR: OpCode = CONS_BASE + 4; // XDR A B - cdr(R(A)) = R(B)
pub const LIST: OpCode = CONS_BASE + 5;
