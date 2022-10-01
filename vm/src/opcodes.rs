pub type OpCode = u8;

pub const NOP: OpCode = 0x00;
pub const HALT: OpCode = 0x01;
pub const RET: OpCode = 0x02;
pub const SRET: OpCode = 0x03; // SRET A - R(0) = R(A) and then RET
pub const WIDE: OpCode = 0x04;

const STACK_BASE: OpCode = 0x04;
pub const MOV: OpCode = STACK_BASE + 1; // MOV A B - R(A) = R(B) does not respect closed over values
pub const SET: OpCode = STACK_BASE + 2; // SET A B - R(A) = R(B) respecting local closed over values
pub const CONST: OpCode = STACK_BASE + 3; // CONST A B - R(A) = K(B)
pub const DEF: OpCode = STACK_BASE + 4; // DEF A B - G(B) = R(A)
pub const DEFV: OpCode = STACK_BASE + 5; // DEFV A B - G(B) = R(A) if G(B) is undefined
pub const REFI: OpCode = STACK_BASE + 6; // REFI A B - R(A) = G[B]
pub const CLRREG: OpCode = STACK_BASE + 7; // CLRREG A - R(A) = UNDEFINED (ignores a closed over value)
pub const REGT: OpCode = STACK_BASE + 8; // REGT A - R(A) = TRUE
pub const REGF: OpCode = STACK_BASE + 9; // REGF A - R(A) = FALSE
pub const REGN: OpCode = STACK_BASE + 10; // REGN A - R(A) = NIL
pub const REGC: OpCode = STACK_BASE + 11; // REGC A - R(A) = UNDEFINED
pub const REGB: OpCode = STACK_BASE + 12; // REGB A B - R(A) = Byte(B)
pub const REGI: OpCode = STACK_BASE + 13; // REGI A B - R(A) = Int(B)
pub const REGU: OpCode = STACK_BASE + 14; // REGU A B - R(A) = UInt(B)
pub const CLOSE: OpCode = STACK_BASE + 15; // CLOSE A B - R(A) = closure derived from lambda in R(B)
pub const BMOV: OpCode = STACK_BASE + 16; // BMOV A B C - R(A)..R(A+C) = R(B)..R(B+C) does not respect closed over values
pub const LDSC: OpCode = STACK_BASE + 17; // LDSC A B C - R(A)..R(A+B) = destructured list or vec in R(C) (ignore leftover values)
pub const LDSCR: OpCode = STACK_BASE + 18; // LDSCR A B C - R(A)..R(A+B) = destructured list or vec in R(C) (R(A+B) gets all leftover values)
pub const MDSC: OpCode = STACK_BASE + 19; // MDSC A B C - R(A)..R(A+B) = destructured map in R(C) (ignore leftover values), R(A..) start with keys
pub const COPY: OpCode = STACK_BASE + 20; // COPY A B - R(A) = deep copy of R(B)
pub const FRZ: OpCode = STACK_BASE + 21; // FRZ A - R(A) if a heap object will be made read only

// Flow control
const FLOW_BASE: OpCode = STACK_BASE + 22;
// CALL A B C - Call fn R(A) with B args with R(C) as first reg/param
pub const CALL: OpCode = FLOW_BASE;
// TCALL A B - Tail Call fn R(A) with B args with existing stack/regs
pub const TCALL: OpCode = FLOW_BASE + 1;
// CALLG A B C - Call fn G[A] with B args with R(C) as first reg/param
pub const CALLG: OpCode = FLOW_BASE + 2;
// TCALLG A B - Tail Call fn G[A] with B args with existing stack/regs
pub const TCALLG: OpCode = FLOW_BASE + 3;
// CALLM A B - Call current fn with B args with R(C) as first reg/param
pub const CALLM: OpCode = FLOW_BASE + 4;
// TCALLM B - Tail Call current fn with B args with existing stack/regs
pub const TCALLM: OpCode = FLOW_BASE + 5;

// Jumps, all jumps use a signed 24 bit OFFSET (high bit is sign and next 23 are integer).
// This means all jumps are forward or back (negative target).
// JMP OFFSET - Jump to IP + OFFSET
pub const JMP: OpCode = FLOW_BASE + 6;
// JMPT A OFFSET - Jump to current IP + OFFSET if R(A) is truthy (not (nil or false))
pub const JMPT: OpCode = FLOW_BASE + 7;
// JMPF A OFFSET - Jump to current IP + OFFSET if R(A) is falsy (nil or false)
pub const JMPF: OpCode = FLOW_BASE + 8;

// JMPEQ A B OFFSET - compare A and B and jump to IP + OFFSET if they are equal
pub const JMPEQ: OpCode = FLOW_BASE + 9;
// JMPLT A B OFFSET - compare A and B and jump to IP + OFFSET if R(A) < R(B)
pub const JMPLT: OpCode = FLOW_BASE + 10;
// JMPGT A B OFFSET - compare A and B and jump to IP + OFFSET if R(A) > R(B)
pub const JMPGT: OpCode = FLOW_BASE + 11;

// JMPU A OFFSET - Jump to current IP + OFFSET if R(A) is undefined
pub const JMPU: OpCode = FLOW_BASE + 12;
// JMPNU A OFFSET - Jump to current IP + OFFSET if R(A) is NOT undefined
pub const JMPNU: OpCode = FLOW_BASE + 13;

// EQ A B C - R[A] is #t if objects in R[B] - R[C] (inclusive) are the same objects
pub const EQ: OpCode = FLOW_BASE + 14;
// EQUAL A B C - R[A] is #t if objects in R[B] - R[C] (inclusive) are the same objects, values or
// containers with equal values (must be the same container type)
pub const EQUAL: OpCode = FLOW_BASE + 15;
// NOT A B - R[A] is #t if R[B] is falsey and #f otherwise
pub const NOT: OpCode = FLOW_BASE + 16;
// ERR A B - raise error with key R(A) (must be keyword) and value R(B)
pub const ERR: OpCode = FLOW_BASE + 17;
// CCC A B - call with continuation, R(A) must be a lambda that takes one arg (the continuation)
// R(B) is the first reg for the call
pub const CCC: OpCode = FLOW_BASE + 18;
// DFR A - Add a lambda, R(A) to the deferred list.
pub const DFR: OpCode = FLOW_BASE + 19;
// DFRPOP - Pop and call the last deferred lambda.
pub const DFRPOP: OpCode = FLOW_BASE + 20;
// ONERR A - Make R(A) the current error handler and put the previous error handler in R(A).
// If R(A) is nil then remove error handler.
pub const ONERR: OpCode = FLOW_BASE + 21;

// JMPRU A B OFFSET - Jump to current IP + OFFSET if any in R(A)..R(A+B) is undefined
pub const JMPRU: OpCode = FLOW_BASE + 22;
// JMPRNU A B OFFSET - Jump to current IP + OFFSET if any in R(A)..R(A+B) is NOT undefined
pub const JMPRNU: OpCode = FLOW_BASE + 23;

// Basic math
const MATH_BASE: OpCode = FLOW_BASE + 24;
// ADD A B - set R(A) = R(A) + R(B)
pub const ADD: OpCode = MATH_BASE;
// SUB A B - set R(A) = R(A) - R(B)
pub const SUB: OpCode = MATH_BASE + 1;
// MUL A B - set R(A) = R(A) * R(B)
pub const MUL: OpCode = MATH_BASE + 2;
// DIV A B - set R(A) = R(A) / R(B)
pub const DIV: OpCode = MATH_BASE + 3;
// INC A B - Increment the integer in R(A) by B
pub const INC: OpCode = MATH_BASE + 4;
// DEC A B - Decrement the integer in R(A) by B
pub const DEC: OpCode = MATH_BASE + 5;
// NUMEQ A B C - compare (=) in register B (inclusive) to C (inclusive) and set R[A] to the
// result.
pub const NUMEQ: OpCode = MATH_BASE + 6;
// NUMNEQ A B C - compare (!=) in register B (inclusive) to C (inclusive) and set R[A] to the
// result.
pub const NUMNEQ: OpCode = MATH_BASE + 7;
// NUMLT A B C - compare (<) in register B (inclusive) to C (inclusive) and set R[A] to the
// result.
pub const NUMLT: OpCode = MATH_BASE + 8;
// NUMGT A B C - compare (>) in register B (inclusive) to C (inclusive) and set R[A] to the
// result.
pub const NUMGT: OpCode = MATH_BASE + 9;
// NUMLTE A B C - compare (<=) in register B (inclusive) to C (inclusive) and set R[A] to the
// result.
pub const NUMLTE: OpCode = MATH_BASE + 10;
// NUMGTE A B C - compare (>=) in register B (inclusive) to C (inclusive) and set R[A] to the
// result.
pub const NUMGTE: OpCode = MATH_BASE + 11;

// Cons cells
const CONS_BASE: OpCode = MATH_BASE + 12;
pub const CONS: OpCode = CONS_BASE; // CONS A B C - R(A) = conscell(R(B), R(C))
pub const CAR: OpCode = CONS_BASE + 1; // CAR A B - R(A) = car(R(B))
pub const CDR: OpCode = CONS_BASE + 2; // CDR A B - R(A) = cdr(R(B))
pub const XAR: OpCode = CONS_BASE + 3; // XAR A B - car(R(A)) = R(B)
pub const XDR: OpCode = CONS_BASE + 4; // XDR A B - cdr(R(A)) = R(B)
pub const LIST: OpCode = CONS_BASE + 5; // LIST A B C - R(A) = list(elements R(B)..R(C)) (R(B) and R(C) are inclusive)
pub const APND: OpCode = CONS_BASE + 6; // APND A B C - R(A) = append lists R(B)..R(C) (R(B) and R(C) are inclusive)

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
// VEC A B C - R(A) = vec(elements R(B)..R(C)) (R(B) inclusive, R(C) exclusive)
pub const VEC: OpCode = VEC_BASE + 7;
// VECLEN A B - R(A) = vector length of vector in R(B)
pub const VECLEN: OpCode = VEC_BASE + 8;
// VECCLR A - Clear the vector in R(A)
pub const VECCLR: OpCode = VEC_BASE + 9;

// Strings
const STRING_BASE: OpCode = VEC_BASE + 10;
// STR A B C - R(A) = string concatinated from objects in R(A) - R(B) (inclusive)
pub const STR: OpCode = STRING_BASE;

// Types
const TYPE_BASE: OpCode = STRING_BASE + 1;
// TYPE A B - R(A) = type(R(B)) as a StringConst
pub const TYPE: OpCode = TYPE_BASE;

pub const MAX_OP_CODE: OpCode = TYPE_BASE;
