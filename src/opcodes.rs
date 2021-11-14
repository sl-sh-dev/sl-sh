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
pub const SREGT: OpCode = STACK_BASE + 8; // SREGT A - R(A) = TRUE, set semantics
pub const SREGF: OpCode = STACK_BASE + 9; // SREGF A - R(A) = FALSE, set semantics
pub const SREGN: OpCode = STACK_BASE + 10; // SREGN A - R(A) = NIL, set semantics
pub const SREGC: OpCode = STACK_BASE + 11; // SREGC A - R(A) = UNDEFINED, set semantics
pub const SREGB: OpCode = STACK_BASE + 12; // SREGB A B - R(A) = Byte(B), set semantics
pub const SREGI: OpCode = STACK_BASE + 13; // SREGI A B - R(A) = Int(B), set semantics
pub const SREGU: OpCode = STACK_BASE + 14; // SREGU A B - R(A) = UInt(B), set semantics
pub const MREGT: OpCode = STACK_BASE + 15; // MREGT A - R(A) = TRUE, mov semantics
pub const MREGF: OpCode = STACK_BASE + 16; // MREGF A - R(A) = FALSE, mov semantics
pub const MREGN: OpCode = STACK_BASE + 17; // MREGN A - R(A) = NIL, mov semantics
pub const MREGC: OpCode = STACK_BASE + 18; // MREGC A - R(A) = UNDEFINED, mov semantics
pub const MREGB: OpCode = STACK_BASE + 19; // MREGB A B - R(A) = Byte(B), mov semantics
pub const MREGI: OpCode = STACK_BASE + 20; // MREGI A B - R(A) = Int(B), mov semantics
pub const MREGU: OpCode = STACK_BASE + 21; // MREGU A B - R(A) = UInt(B), mov semantics
pub const CLOSE: OpCode = STACK_BASE + 22; // CLOSE A B - R(A) = closure derived from lambda in R(B)

// Flow control
const FLOW_BASE: OpCode = STACK_BASE + 23;
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

// CALLG A B C - Call fn G[A] with B args with R(C) as first reg/param
pub const CALLG: OpCode = FLOW_BASE + 14;
// TCALLG A B - Tail Call fn G[A] with B args with existing stack/regs
pub const TCALLG: OpCode = FLOW_BASE + 15;
// CALLM A B - Call current fn with B args with R(C) as first reg/param
pub const CALLM: OpCode = FLOW_BASE + 16;
// TCALLM B - Tail Call current fn with B args with existing stack/regs
pub const TCALLM: OpCode = FLOW_BASE + 17;

// EQ A B C - R[A] is #t if objects in R[B] - R[C] (inclusive) are the same objects
pub const EQ: OpCode = FLOW_BASE + 18;
// EQUAL A B C - R[A] is #t if objects in R[B] - R[C] (inclusive) are the same objects, values or
// containers with equal values (must be the same container type)
pub const EQUAL: OpCode = FLOW_BASE + 20;

// Basic math
const MATH_BASE: OpCode = FLOW_BASE + 21;
// ADD A B C - set R(A) = R(B) + R(C)
pub const ADD: OpCode = MATH_BASE;
// SUB A B C - set R(A) = R(B) - R(C)
pub const SUB: OpCode = MATH_BASE + 1;
// MUL A B C - set R(A) = R(B) * R(C)
pub const MUL: OpCode = MATH_BASE + 2;
// DIV A B C - set R(A) = R(B) / R(C)
pub const DIV: OpCode = MATH_BASE + 3;
// INC A B - Increment the integer in R(A) by B
pub const INC: OpCode = MATH_BASE + 4;
// DEC A B - Decrement the integer in R(A) by B
pub const DEC: OpCode = MATH_BASE + 5;
// ADDM A B C - mov R(A) = R(B) + R(C) NOTE, mov overwrites global or closures.
pub const ADDM: OpCode = MATH_BASE + 6;
// SUBM A B C - mov R(A) = R(B) - R(C) NOTE, mov overwrites global or closures.
pub const SUBM: OpCode = MATH_BASE + 7;
// MULM A B C - mov R(A) = R(B) * R(C) NOTE, mov overwrites global or closures.
pub const MULM: OpCode = MATH_BASE + 8;
// DIVM A B C - mov R(A) = R(B) / R(C) NOTE, mov overwrites global or closures.
pub const DIVM: OpCode = MATH_BASE + 9;
// NUMEQ A B C - compare (=) in register B (inclusive) to C (inclusive) and set R[A] to the
// result.
pub const NUMEQ: OpCode = MATH_BASE + 10;
// NUMNEQ A B C - compare (!=) in register B (inclusive) to C (inclusive) and set R[A] to the
// result.
pub const NUMNEQ: OpCode = MATH_BASE + 11;
// NUMLT A B C - compare (<) in register B (inclusive) to C (inclusive) and set R[A] to the
// result.
pub const NUMLT: OpCode = MATH_BASE + 12;
// NUMGT A B C - compare (>) in register B (inclusive) to C (inclusive) and set R[A] to the
// result.
pub const NUMGT: OpCode = MATH_BASE + 13;
// NUMLTE A B C - compare (<=) in register B (inclusive) to C (inclusive) and set R[A] to the
// result.
pub const NUMLTE: OpCode = MATH_BASE + 14;
// NUMGTE A B C - compare (>=) in register B (inclusive) to C (inclusive) and set R[A] to the
// result.
pub const NUMGTE: OpCode = MATH_BASE + 15;

// Cons cells
const CONS_BASE: OpCode = MATH_BASE + 16;
pub const CONS: OpCode = CONS_BASE; // CONS A B C - R(A) = conscell(R(B), R(C))
pub const CAR: OpCode = CONS_BASE + 1; // CAR A B - R(A) = car(R(B))
pub const CDR: OpCode = CONS_BASE + 2; // CDR A B - R(A) = cdr(R(B))
pub const XAR: OpCode = CONS_BASE + 3; // XAR A B - car(R(A)) = R(B)
pub const XDR: OpCode = CONS_BASE + 4; // XDR A B - cdr(R(A)) = R(B)
pub const LIST: OpCode = CONS_BASE + 5; // LIST A B C - R(A) = list(elements R(B)..R(C)) (R(B) inclusive, R(C) exclusive)
pub const APND: OpCode = CONS_BASE + 6; // APND A B C - R(A) = append lists R(B)..R(C) (R(B) inclusive, R(C) exclusive)

// Vectors
const VEC_BASE: OpCode = CONS_BASE + 7;
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
