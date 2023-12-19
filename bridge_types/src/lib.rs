use std::borrow::Cow;

/// Public type used by rust native -> slosh bridge macro to represent
/// arguments in slosh that correspond to variadic functions in rust
///
/// Type is useful so it is possible to write rust native functions
/// that appear in slosh as functions that can receive any number of arguments.
pub type VarArgs<T> = Vec<T>;

/// [Value](../slvm/value/enum.Value.html)
/// Type to hold anything in Slosh that can be represented as a string.
///
/// Public type used by rust native -> slosh bridge macro to represent
/// arguments that can be loosely cast to strings. Unlike the [`String`]
/// and [`&str`] types, in slosh there are various types that can be
/// represented as strings. When the rust native function doesn't
/// require *strict* type checking on whether or not the [`Value`]`::String`
/// type is passed in use this function.
pub type LooseString<'a, T> = Cow<'a, T>;

/// [Value](../slvm/value/enum.Value.html)
/// Type to hold Slosh's notion of a char.
///
/// In slosh a character can either be an actual char, e.g. a [`Value`]`::CodePoint`
/// or a [`Value`]`::CharCluster`/[`Value`]`::CharClusterLong` in which case it will
/// be stored in an &str.
pub enum SloshChar<'a> {
    Char(char),
    String(Cow<'a, str>),
}

/// Used by sl_sh_fn macro to embed information at runtime about the parameters of
/// the rust native function, specifically whether it is a normal Type, or some
/// supported wrapped type, e.g. Optional.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TypeHandle {
    Direct,
    Optional,
    VarArgs,
}

/// Used by sl_sh_fn macro to embed information at runtime about the parameters of
/// the rust native function, specifically whether it is going to pass the value (a move),
/// a reference, or mutable reference.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PassingStyle {
    Value,
    Reference,
    MutReference,
}

/// Struct used by sl_sh_fn macro to embed information in an array at runtime about each of
/// the parameters of the rust native function.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Param {
    pub handle: TypeHandle,
    pub passing_style: PassingStyle,
}

//TODO PC use this! need common error messages but want a more comprehensive approach?
// ... talk to sstanfield.
pub struct ErrorStrings {}

impl ErrorStrings {
    pub fn mismatched_type(fn_name: &str, expected: &str, got: &str) -> String {
        format!("{fn_name}: mismatched type input, expected {expected}, got {got}.")
    }
}
