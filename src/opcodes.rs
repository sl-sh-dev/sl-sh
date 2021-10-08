pub type OpCode = u8;

pub const NOP: OpCode = 0x00;
pub const HALT: OpCode = 0x01;
pub const RET: OpCode = 0x02;
pub const SRET: OpCode = 0x03; // SRET A - R(0) = R(A) and then RET
pub const WIDE: OpCode = 0x04;
// Load a constant onto the stack
const STACK_BASE: OpCode = 0x04;
pub const MOV: OpCode = STACK_BASE + 1; // MOV A B - R(A) = R(B)
pub const SET: OpCode = STACK_BASE + 2; // SET A B respecting global and local bindings - R(A) = R(B)
pub const CONST: OpCode = STACK_BASE + 3; // CONST A B - R(A) = K(B)
pub const REF: OpCode = STACK_BASE + 4; // REF A B - R(A) = G[R(B)]
pub const DEF: OpCode = STACK_BASE + 5; // DEF A B - G[R(A)] = R(B)
pub const DEFV: OpCode = STACK_BASE + 6; // DEFV A B - G[R(A)] = R(B) if G[R(A)] is undefined
pub const REFI: OpCode = STACK_BASE + 7; // REFI A B - R(A) = G[B]

// Flow control
const FLOW_BASE: OpCode = STACK_BASE + 8;
// CALL A B C - Call fn R(A) with B args with R(C) as first reg/param
pub const CALL: OpCode = FLOW_BASE;
// TCALL A B - Tail Call fn R(A) with B args with existing stack/regs
pub const TCALL: OpCode = FLOW_BASE + 1;
// JMP A - Jump to IP A
pub const JMP: OpCode = FLOW_BASE + 2;
// JMPF A - Jump to current IP + A
pub const JMPF: OpCode = FLOW_BASE + 3;
// JMPB A - Jump to current IP - A
pub const JMPB: OpCode = FLOW_BASE + 4;
// JMPFT A B - Jump to current IP + B if R(A) is truthy (not nil or false)
pub const JMPFT: OpCode = FLOW_BASE + 5;
// JMPBT A B - Jump to current IP - B if R(A) is truthy (not nil or false)
pub const JMPBT: OpCode = FLOW_BASE + 6;
// JMPFF A B - Jump to current IP + B if R(A) is falsy (nil or false)
pub const JMPFF: OpCode = FLOW_BASE + 7;
// JMPBF A B - Jump to current IP - B if R(A) is falsy (nil or false)
pub const JMPBF: OpCode = FLOW_BASE + 8;
// JMP_T A B - Jump to B if R(A) is truthy (not nil or false)
pub const JMP_T: OpCode = FLOW_BASE + 9;
// JMP_F A B - Jump to B if R(A) is falsy (nil or false)
pub const JMP_F: OpCode = FLOW_BASE + 10;

// JMPEQ A B C - compare B and C and jump to IP C if they are equal
pub const JMPEQ: OpCode = FLOW_BASE + 11;
// JMPLT A B C - compare B and C and jump to IP C if R(A) < R(B)
pub const JMPLT: OpCode = FLOW_BASE + 12;
// JMPGT A B C - compare B and C and jump to IP C if R(A) > R(B)
pub const JMPGT: OpCode = FLOW_BASE + 13;

// Basic math
const MATH_BASE: OpCode = FLOW_BASE + 14;
pub const ADD: OpCode = MATH_BASE;
pub const SUB: OpCode = MATH_BASE + 1;
pub const MUL: OpCode = MATH_BASE + 2;
pub const DIV: OpCode = MATH_BASE + 3;
// INC A B - Increment the integer in R(A) by B
pub const INC: OpCode = MATH_BASE + 4;
// DEC A B - Decrement the integer in R(A) by B
pub const DEC: OpCode = MATH_BASE + 5;

// Cons cells
const CONS_BASE: OpCode = MATH_BASE + 6;
pub const CONS: OpCode = CONS_BASE; // CONS A B C - R(A) = conscell(R(B), R(C))
pub const CAR: OpCode = CONS_BASE + 1; // CAR A B - R(A) = car(R(B))
pub const CDR: OpCode = CONS_BASE + 2; // CDR A B - R(A) = cdr(R(B))
pub const XAR: OpCode = CONS_BASE + 3; // XAR A B - car(R(A)) = R(B)
pub const XDR: OpCode = CONS_BASE + 4; // XDR A B - cdr(R(A)) = R(B)
pub const LIST: OpCode = CONS_BASE + 5; // LIST A B C - R(A) = list(elements R(B)..R(C)) (R(B) inclusive, R(C) exclusive)

// Vectors
const VEC_BASE: OpCode = CONS_BASE + 6;
// VECMK A B - make a vector with R(B) elements and put it in R(A)
pub const VECMK: OpCode = VEC_BASE;
// VECELS A B - make the length of vec in R(A) R(B)
pub const VECELS: OpCode = VEC_BASE + 1;
// VECPSH A B - push R(B) into vec in R(A)
pub const VECPSH: OpCode = VEC_BASE + 2;
// VECPOP A B - pop from vec in R(A) to R(B)
pub const VECPOP: OpCode = VEC_BASE + 3;
// VECNTH A B C - get nth R(C) item from vec in R(A) to R(B)
pub const VECNTH: OpCode = VEC_BASE + 4;
// VECSTH A B C - set nth R(C) item in vec R(A) from R(B)
pub const VECSTH: OpCode = VEC_BASE + 5;
// VECMKD A B C - make a vector with R(B) elements and put it in R(A), each element defauted to R(C)
pub const VECMKD: OpCode = VEC_BASE + 6;
