use std::borrow::Cow;
use std::fmt::Display;

/// A slosh [`Value`](../slvm/value/enum.Value.html) that can potentially be represented as a rust value.
/// Marker traits
pub trait BridgedType {}

/// An [`Option`] value that contains a [`BridgedType`] can be represented as a rust value.
impl<T> BridgedType for Option<T> where T: BridgedType {}

/// A [`Result`] value that contains a [`BridgedType`] can be represented as a rust value.
impl<T, U> BridgedType for Result<T, U> where T: BridgedType {}

/// A [`Vec`] that contains a [`BridgedType`] can be represented as a rust value.
impl<T> BridgedType for Vec<T> where T: BridgedType {}

/// Public type used by rust native -> slosh bridge macro to represent
/// arguments in slosh that correspond to variadic functions in rust
///
/// Type is useful so it is possible to write rust native functions
/// that appear in slosh as functions that can receive any number of arguments.
pub type VarArgs<T> = Vec<T>;

/// [`Value`]: ../slvm/value/enum.Value.html
/// [`Value::String`]: ../slvm/value/enum.Value.html#variant.String
/// Type to hold anything in Slosh that can be represented as a [`String`].
///
/// Public type used by rust native -> slosh bridge macro to represent
/// arguments that can be loosely cast to strings. Unlike the [`String`]
/// and [`&str`] types, in slosh there are various types that can be
/// represented as strings. When the rust native function doesn't
/// require *strict* type checking on whether or not the [`Value::String`]
/// type is passed in use this function.
///
/// Can represent SlRefInto [`Value`] types:
/// - String
/// - CodePoint
/// - CharCluster
/// - CharClusterLong
/// - Symbol
/// - Keyword
/// - StringConst
///
/// Always does an allocation and returns a [`Value::String`] type.
pub type LooseString<'a> = Cow<'a, str>;

/// [`Value`]: ../slvm/value/enum.Value.html
/// [`Value::Float`]: ../slvm/value/enum.Value.html#variant.Float
/// Type to hold anything in Slosh that can be represented as a slosh float value: `[u8; 7]`.
///
/// Public type used by rust native -> slosh bridge macro to represent
/// arguments that can be loosely cast to an int.
///
/// Can represent SlRefInto [`Value`] types:
/// - Byte
/// - Int
/// - Float
/// - String
/// - CodePoint
/// - CharCluster
/// - CharClusterLong
/// - Symbol
/// - Keyword
/// - StringConst
///
/// Always returns a [`Value::Float`] type.
pub struct LooseFloat(pub [u8; 7]);

impl From<LooseFloat> for [u8; 7] {
    fn from(value: LooseFloat) -> Self {
        value.0
    }
}

impl From<[u8; 7]> for LooseFloat {
    fn from(value: [u8; 7]) -> Self {
        LooseFloat(value)
    }
}

/// [`Value`]: ../slvm/value/enum.Value.html
/// [`Value::Symbol`]: ../slvm/value/enum.Value.html#variant.Symbol
/// Type to hold anything in Slosh that can be represented as a slosh symbol: u32.
///
/// Public type used by rust native -> slosh bridge macro to represent
/// arguments that reference a valid symbol.
///
/// Can represent SlRefInto [`Value`] types:
/// - Symbol
///
/// Always returns a [`Value::Symbol`] type.
pub struct Symbol(u32);

impl From<u32> for Symbol {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<Symbol> for u32 {
    fn from(value: Symbol) -> Self {
        value.0
    }
}

/// [`Value`]: ../slvm/value/enum.Value.html
/// [`Value::Symbol`]: ../slvm/value/enum.Value.html#variant.Symbol
/// Type to hold anything in Slosh that can be represented as a slosh symbol, however, this
/// type, in contrast to [`Symbol`] does need to do some allocations, specifically when
/// using [`SlFrom`]. Not a huge penalty but worth mentioning if unnecessary.
///
/// Public type used by rust native -> slosh bridge macro to represent
/// arguments that reference a valid symbol.
///
/// Can represent SlRefInto [`Value`] types:
/// - Symbol
///
/// Always returns a [`Value::Symbol`] type.
pub struct SymbolAsString(String);

impl AsRef<str> for SymbolAsString {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl From<String> for SymbolAsString {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<SymbolAsString> for String {
    fn from(value: SymbolAsString) -> Self {
        value.0
    }
}

/// [`Value`]: ../slvm/value/enum.Value.html
/// Type to hold anything in Slosh that can be represented as slosh int value: `[u8; 7]`.
///
/// Public type used by rust native -> slosh bridge macro to represent
/// arguments that can be loosely cast to an int.
///
/// Can represent SlRefInto [`Value`] types:
/// - Byte
/// - Int
/// - Float
/// - String
/// - CodePoint
/// - CharCluster
/// - CharClusterLong
/// - Symbol
/// - Keyword
/// - StringConst
///
/// Always returns a [`Value`]`::Int` type.
pub struct LooseInt(pub [u8; 7]);

impl From<LooseInt> for [u8; 7] {
    fn from(value: LooseInt) -> Self {
        value.0
    }
}

impl From<[u8; 7]> for LooseInt {
    fn from(value: [u8; 7]) -> Self {
        LooseInt(value)
    }
}

/// [`Value`]: ../slvm/value/enum.Value.html
/// [`Value::CodePoint`]: ../slvm/value/enum.Value.html#variant.CodePoint
/// [`Value::CharCluster`]: ../slvm/value/enum.Value.html#variant.CharCluster
/// [`Value::CharClusterLong`]: ../slvm/value/enum.Value.html#variant.CharClusterLong
/// Type to hold Slosh's notion of a char.
///
/// In slosh a character can either be an actual char, e.g. a [`Value::CodePoint`]
/// or a [`Value::CharCluster`]/[`Value::CharClusterLong`] in which case it will
/// be stored in an &str.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SloshChar<'a> {
    Char(char),
    String(Cow<'a, str>),
}

impl Display for SloshChar<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            SloshChar::Char(c) => {
                format!("{}", c)
            }
            SloshChar::String(c) => c.to_string(),
        };
        write!(f, "{}", str)
    }
}

/// [`Value`]: ../slvm/value/enum.Value.html
/// [`Value::Keyword`]: ../slvm/value/enum.Value.html#variant.Keyword
/// Type to hold anything in Slosh that can be represented as a slosh Keyword: u32.
///
/// Public type used by rust native -> slosh bridge macro to represent
/// arguments that reference a valid keyword.
///
/// Can represent SlRefInto [`Value`] types:
/// - Keyword
///
/// Always returns a [`Value::Symbol`] type.
pub struct Keyword(u32);

impl From<u32> for Keyword {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<Keyword> for u32 {
    fn from(value: Keyword) -> Self {
        value.0
    }
}

/// [`Value`]: ../slvm/value/enum.Value.html
/// [`Value::Keyword`]: ../slvm/value/enum.Value.html#variant.Keyword
/// Type to hold anything in Slosh that can be represented as a slosh symbol, however, this
/// type, in contrast to [`Keyword`] does need to do some allocations, specifically when
/// using [`SlFrom`]. Not a huge penalty but worth mentioning if unnecessary.
///
/// Public type used by rust native -> slosh bridge macro to represent
/// arguments that reference a valid symbol.
///
/// Can represent SlRefInto [`Value`] types:
/// - Keyword
///
/// Always returns a [`Value::Keyword`] type.
pub struct KeywordAsString(String);

impl AsRef<str> for KeywordAsString {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl From<String> for KeywordAsString {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<KeywordAsString> for String {
    fn from(value: KeywordAsString) -> Self {
        value.0
    }
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

//TODO #224 use this but create a module there is a need for common error messages but there should be a consistent comprehensive approach.
pub struct ErrorStrings {}

impl ErrorStrings {
    pub fn mismatched_type(
        fn_name: impl AsRef<str>,
        expected: impl AsRef<str>,
        got: impl AsRef<str>,
        additional: impl AsRef<str>,
    ) -> String {
        if additional.as_ref().is_empty() {
            format!(
                "{}: mismatched type input, expected value of type {}, got {}.",
                fn_name.as_ref(),
                expected.as_ref(),
                got.as_ref(),
            )
        } else {
            format!(
                "{}: mismatched type input, expected value of type {}, got {}. {}",
                fn_name.as_ref(),
                expected.as_ref(),
                got.as_ref(),
                additional.as_ref(),
            )
        }
    }

    pub fn fix_me_invalid_string(expected: impl AsRef<str>, got: impl AsRef<str>) -> String {
        Self::mismatched_type("fixme_invalid_string", expected, got, "")
    }

    pub fn fix_me_mismatched_type(expected: impl AsRef<str>, got: impl AsRef<str>) -> String {
        Self::mismatched_type("fixme", expected, got, "")
    }

    pub fn fix_me_mismatched_type_with_context(
        expected: impl AsRef<str>,
        got: impl AsRef<str>,
        additional: impl AsRef<str>,
    ) -> String {
        Self::mismatched_type("fixme", expected, got, additional)
    }
}
