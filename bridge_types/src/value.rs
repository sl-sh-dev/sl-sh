use std::fmt::{Display, Formatter};

pub const SLOSH_CHAR: &'static str = "Char";
pub const SLOSH_STRING: &'static str = "String";
pub const SLOSH_INT: &'static str = "Int";
pub const SLOSH_FLOAT: &'static str = "Float";
pub const SLOSH_BOOL_TRUE: &'static str = "True";
pub const SLOSH_BOOL_FALSE: &'static str = "False";
pub const SLOSH_SYMBOL: &'static str = "Symbol";
pub const SLOSH_KEYWORD: &'static str = "Keyword";
pub const SLOSH_SPECIAL: &'static str = "Special";
pub const SLOSH_BUILTIN: &'static str = "Builtin";
pub const SLOSH_BYTE: &'static str = "Byte";
pub const SLOSH_BYTES: &'static str = "Bytes";
pub const SLOSH_NIL: &'static str = "Nil";
pub const SLOSH_UNDEFINED: &'static str = "Undefined";
pub const SLOSH_LAMBDA: &'static str = "Lambda";
pub const SLOSH_CLOSURE: &'static str = "Lambda";
pub const SLOSH_CONTINUATION: &'static str = "Continuation";
pub const SLOSH_CALLFRAME: &'static str = "CallFrame";
pub const SLOSH_VECTOR: &'static str = "Vector";
pub const SLOSH_MAP: &'static str = "Map";
pub const SLOSH_PAIR: &'static str = "Pair";
pub const SLOSH_ERROR: &'static str = "Error";

/// Enum representing the various types of values in Slosh.
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ValueType {
    Byte,
    Int,
    Float,
    CodePoint,
    CharCluster,
    CharClusterLong,
    Symbol,
    Keyword,
    StringConst,
    Special,
    Builtin,
    True,
    False,
    Nil,
    Undefined,
    String,
    Vector,
    Map,
    Bytes,
    Pair,
    List,
    Lambda,
    Closure,
    Continuation,
    CallFrame,
    Error,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", <ValueType as Into<&'static str>>::into(*self))
    }
}

impl From<ValueType> for &'static str {
    fn from(value: ValueType) -> Self {
        match value {
            ValueType::True => SLOSH_BOOL_TRUE,
            ValueType::False => SLOSH_BOOL_FALSE,
            ValueType::Int => SLOSH_INT,
            ValueType::Float => SLOSH_FLOAT,
            ValueType::Symbol => SLOSH_SYMBOL,
            ValueType::Keyword => SLOSH_KEYWORD,
            ValueType::StringConst => SLOSH_STRING,
            ValueType::Special => SLOSH_SPECIAL,
            ValueType::CodePoint => SLOSH_CHAR,
            ValueType::CharCluster => SLOSH_CHAR,
            ValueType::CharClusterLong => SLOSH_CHAR,
            ValueType::Builtin => SLOSH_BUILTIN,
            ValueType::Byte => SLOSH_BYTE,
            ValueType::Bytes => SLOSH_BYTES,
            ValueType::Nil => SLOSH_NIL,
            ValueType::Undefined => SLOSH_UNDEFINED,
            ValueType::Lambda => SLOSH_LAMBDA,
            ValueType::Closure => SLOSH_LAMBDA,
            ValueType::Continuation => SLOSH_CONTINUATION,
            ValueType::CallFrame => SLOSH_CALLFRAME,
            ValueType::Vector => SLOSH_VECTOR,
            ValueType::Map => SLOSH_MAP,
            ValueType::Pair => SLOSH_PAIR,
            ValueType::List => SLOSH_PAIR,
            ValueType::String => SLOSH_STRING,
            ValueType::Error => SLOSH_ERROR,
        }
    }
}
