pub type OpCode = u8;

pub const NOP: OpCode = 0x00;
pub const HALT: OpCode = 0x01;
pub const RET: OpCode = 0x02;
// Load a constant onto the stack
const STACK_BASE: OpCode = 0x03;
pub const MOV: OpCode = STACK_BASE + 1; // MOV A B - R(A) = R(B)
pub const CONST: OpCode = STACK_BASE + 2; // CONST A B - R(A) = K(B)
pub const REF: OpCode = STACK_BASE + 3; // REF A B - R(A) = G[R(B)]
pub const DEF: OpCode = STACK_BASE + 4; // DEF A B - G[R(A)] = R(B)
pub const DEFV: OpCode = STACK_BASE + 5; // DEFV A B - G[R(A)] = R(B) if G[R(A)] is undefined

// Basic math
const MATH_BASE: OpCode = STACK_BASE + 6;
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
pub const LIST: OpCode = CONS_BASE + 5; // LIST A B C - R(A) = list(elements R(B)..R(C)) (R(B) inclusive, R(C) exclusive)
