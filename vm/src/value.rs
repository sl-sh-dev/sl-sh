use crate::{Handle, Heap, Interned, VMError, VMResult};
use std::collections::{BTreeSet, HashMap};
use std::fmt;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::iter;
use std::ops::Deref;
use std::sync::Arc;

use crate::vm::GVm;

pub type CallFuncSig<ENV> = fn(vm: &mut GVm<ENV>, registers: &[Value]) -> VMResult<Value>;
#[derive(Copy, Clone)]
pub struct CallFunc<ENV> {
    pub func: CallFuncSig<ENV>,
}

impl<ENV> PartialEq for CallFunc<ENV> {
    fn eq(&self, other: &CallFunc<ENV>) -> bool {
        std::ptr::eq(
            self.func as *const CallFuncSig<ENV>,
            other.func as *const CallFuncSig<ENV>,
        )
    }
}

impl<ENV> Eq for CallFunc<ENV> {}

impl<ENV> Hash for CallFunc<ENV> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.func as usize);
    }
}

impl<ENV> fmt::Debug for CallFunc<ENV> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "...")
    }
}

pub struct PairIter<'vm, ENV> {
    vm: &'vm GVm<ENV>,
    current: Option<Value>,
    dotted: bool,
}

impl<'vm, ENV> PairIter<'vm, ENV> {
    pub fn new(vm: &'vm GVm<ENV>, exp: Value) -> Self {
        Self {
            vm,
            current: Some(exp),
            dotted: false,
        }
    }

    pub fn is_dotted(&self) -> bool {
        self.dotted
    }
}

impl<'vm, ENV> Iterator for PairIter<'vm, ENV> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.current {
            match current {
                Value::Pair(h) => {
                    let (car, cdr) = self.vm.get_pair(h);
                    self.current = Some(cdr);
                    Some(car)
                }
                // TODO: Handle List?
                Value::Nil => None,
                _ => {
                    let cur = Some(current);
                    self.current = None;
                    self.dotted = true;
                    cur
                }
            }
        } else {
            None
        }
    }
}

/// This struct uses 7 bytes to represent a floating point number.
/// Most operations are done by converting to f64, performing the operation, and then converting back to F56
///
/// F56 uses 1 bit for the sign, 10 bits for the exponent, and 45 bits for the mantissa.
/// Compared to f32, it has +2 exponent bits and +22 mantissa bits.
/// Compared to f64, it has -1 exponent bit and -7 mantissa bits.
///   Byte 0    Byte 1    Byte 2    Byte 3    Byte 4    Byte 5    Byte 6
/// [sEEEEEEE][EEEmmmmm][mmmmmmmm][mmmmmmmm][mmmmmmmm][mmmmmmmm][mmmmmmmm]
///
/// Exponent bits range from 0 to 1023
/// they represent -511 to +512 but are stored biased by +511
/// the exponent of -511 is reserved for the number 0 and subnormal numbers
/// the exponent of +512 is reserved for infinity and NaN
/// so normal exponents range from -510 to +511
///
/// smallest positive subnormal value is 8.48e-168 (2^-555)
/// smallest positive normal value is 2.98e-154 (2^-510)
/// maximum finite value is 1.34e154
#[derive(Copy, Clone)]
pub struct F56(pub [u8; 7]);
impl Eq for F56 {}
impl PartialEq for F56 {
    fn eq(&self, other: &Self) -> bool {
        f64::from(*self) == f64::from(*other)
    }
}
impl Hash for F56 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(f64::from(*self).to_bits())
    }
}
impl std::fmt::Debug for F56 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "F56({:?})", f64::from(*self))
    }
}
impl Display for F56 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Converting the F56 to f64 to print it almost works
        // But the f64 is slightly more precise and the internal implementation of f64->string
        // knows that the f64 has 15 decimal digits that are guaranteed accurate.
        // F56 only has 12, meaning that .0023 appears as .0022999999999999687
        // if F56 knew to only print 12 digits then it would be fine,
        // but when going through f64, it thinks it has 15 perfect digits.

        // We can set the precision to 12 but that increases numbers like 1.0 to 1.0000000000000
        // So let's do that, and remove the trailing zeros and decimal points added on
        let first_pass = format!("{:.*}", F56::DIGITS, f64::from(*self));
        // remove trailing zeros and trailing decimal point
        let second_pass = if first_pass.contains('.') {
            first_pass.trim_end_matches('0').trim_end_matches('.')
        } else {
            &first_pass
        };
        write!(f, "{}", second_pass)
    }
}
impl From<f64> for F56 {
    fn from(f: f64) -> F56 {
        let f64_bytes = f.to_be_bytes();
        let f64_word = u64::from_be_bytes(f64_bytes);
        let f64_sign: u8 = (f64_word >> 63) as u8; // first bit
        let f64_biased_exponent: u16 = ((f64_word >> 52) & 0b111_1111_1111) as u16; // first 11 bits after the sign bit
        let true_exponent: i16 = f64_biased_exponent as i16 - 1023i16; // remove the bias of 2^10-1
        let f64_mantissa = f64_word & 0x000f_ffff_ffff_ffff; // everything after first 12 bits
        let mut f56_mantissa = f64_mantissa >> 7; // we lose 7 bits in mantissa
        if F56::ROUNDUP_ENABLED {
            let round_up_bit = f64_mantissa & 0b0100_0000u64 > 0; // the highest bit we lost (7th bit from the end)
            f56_mantissa += if round_up_bit { 1 } else { 0 }; // round up if the 7th bit is 1
        }

        let f56_biased_exponent = match f64_biased_exponent {
            0b111_1111_1111 => {
                // Special case meaning the f64 is NaN or Infinity
                // NaN's mantissa has at least one [1]
                // Infinity's mantissa is all [0]s
                // We need to make sure that the lost bits from the f64 mantissa don't change it from NaN to Infinity
                if f64_mantissa == 0u64 {
                    f56_mantissa = 0u64; // mantissa must be all [0]s to represent Infinity
                } else {
                    f56_mantissa = 0b11_1111_1111u64; // mantissa must be all [1]s to represent NaN
                }
                0b11_1111_1111u64 // 10 bits of all 1s
            }
            0b000_0000_0000 => {
                // Special case meaning the f64 is 0 or subnormal
                // in both cases the f56 will be 0
                // F56 cannot represent any numbers that are subnormal in F64
                // The smallest positive F56 number is 8e-168 and F64 subnormals start at 1e-308
                f56_mantissa = 0u64;
                0b00_0000_0000u64
            }
            _ if true_exponent > 511 => {
                // TOO LARGE TO PROPERLY REPRESENT
                // standard behavior converting from f64 to f32 is to represent this as Infinity rather than panicking
                f56_mantissa = 0u64; // mantissa must be all [0]s to represent Infinity
                0b11_1111_1111u64 // exponent for Infinity
            }
            _ if true_exponent < -510 => {
                // This will be either a subnormal or 0
                // Requires a subnormal f56 which will lose precision as we near 8.48e-168

                // to calculate a 45 bit subnormal mantissa as 0.fraction,
                // take the 45 bits and treat them like an unsigned int and then divide by 2^45

                // value of subnormal f56 = value of f64
                // value of subnormal f56 = 2^-510 * 0.fraction
                // value of f64 = 2^-510 * (u45 / 2^45)
                // value of f64 * 2^555 = u45

                // multiplying the f64 by 2^555 can be done by adding 555 to the exponent
                // we can do this safely because the max biased exponent is 2047
                // and we know that the current biased exponent is < 513 (corresponding to true exponent of -510)
                let new_f64_exponent = (f64_biased_exponent + 555) as u64;
                let new_f64_word = (f64_sign as u64) << 63 | new_f64_exponent << 52 | f64_mantissa;
                let new_f64 = f64::from_bits(new_f64_word);
                let u45 = new_f64 as u64; // we only care about the integer part
                f56_mantissa = u45; // mantissa is set to u45

                0b00_0000_0000u64 // exponent is set to 0
            }
            _ => {
                // Generic case
                (true_exponent + 511) as u64 // add in the bias for F56
            }
        };

        let f56_sign: u64 = f64_sign as u64;
        let word = f56_sign << 55 | f56_biased_exponent << 45 | f56_mantissa;
        let f56_bytes = word.to_be_bytes();
        F56([
            f56_bytes[1],
            f56_bytes[2],
            f56_bytes[3],
            f56_bytes[4],
            f56_bytes[5],
            f56_bytes[6],
            f56_bytes[7],
        ])
    }
}
impl From<f32> for F56 {
    fn from(f: f32) -> F56 {
        f64::from(f).into()
    }
}
impl From<F56> for f64 {
    fn from(f: F56) -> f64 {
        // f64 has 1 sign bit, 11 exponent bits, and 52 mantissa bits
        // f56 has 1 sign bit, 10 exponent bits, and 45 mantissa bits
        let bytes7 = f.0;
        let f56_word = u64::from_be_bytes([
            0, bytes7[0], bytes7[1], bytes7[2], bytes7[3], bytes7[4], bytes7[5], bytes7[6],
        ]);
        let f56_sign: u8 = (f56_word >> 55) as u8; // first bit
        let f56_biased_exponent: u16 = (f56_word >> 45) as u16 & 0x3FF; // first 10 bits after the sign bit
        let f56_mantissa: u64 = f56_word & 0x1FFF_FFFF_FFFF; // the rightmost 45 bits
        let true_exponent = f56_biased_exponent as i16 - 511; // remove the bias of 2^9-1

        let f64_biased_exponent: u64 = match f56_biased_exponent {
            0b11_1111_1111 => {
                // Special case of all [1]s meaning NaN or Infinity
                0b111_1111_1111_u64
            }
            0b00_0000_0000 => {
                // Special case of all [0]s meaning 0 or subnormal
                if f56_mantissa == 0u64 {
                    // the f56 was 0 so the f64 should be 0
                    0b000_0000_0000_u64
                } else {
                    // the f56 was subnormal so the f64 should interpret the exponent as -512
                    // note the slightly different addition of 1022 instead of 1023
                    // when the exponents field falls from 0x1 to 0x0 it conceptually stays at 2^-510
                    // so even though the biased exponent looks 1 lower it is actually not
                    // so we don't need to add quite as much to get it where it needs to be
                    (true_exponent + 1022) as u64
                }
            }
            _ => {
                // Generic case
                (true_exponent + 1023) as u64 // add in the bias for F64
            }
        };

        let f64_sign = f56_sign as u64;
        let f64_mantissa = f56_mantissa << 7_u64; // we add 7 bits in mantissa, but they're all zeros
        let word: u64 = f64_sign << 63 | f64_biased_exponent << 52 | f64_mantissa;
        f64::from_be_bytes(word.to_be_bytes())
    }
}
impl From<F56> for f32 {
    fn from(f: F56) -> f32 {
        f64::from(f) as f32
    }
}
impl F56 {
    // Largest finite F56, roughly 1.34e154
    pub const MAX: F56 = F56([0x7F, 0xDF, 0xff, 0xff, 0xff, 0xff, 0xff]);
    // Smallest positive normal F56, roughly 2.98e-154
    pub const MIN_POSITIVE: F56 = F56([0x00, 0b0010_0000, 0x00, 0x00, 0x00, 0x00, 0x00]);
    // Smallest positive subnormal F56, roughly 8.48e-168
    pub const MIN_POSITIVE_SUBNORMAL: F56 = F56([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);
    // Minimum numer of decimal digits of precision (experimentally derived)
    pub const DIGITS: usize = 12;
    // Cutoff for relative difference between an f64 and the F56's approximation
    pub const EPSILON: f64 = 1e-13;
    // When converting from f64 to F56 we truncate 7 bits of the mantissa
    // We could round up if the 7th bit is 1, but this is might cause issues.
    // Mantissas like 0xFFFF_FFFF_... can catastrophically round to 0x0000_0000_...
    pub const ROUNDUP_ENABLED: bool = false;
}

pub const INT_BITS: u8 = 56;
pub const INT_MAX: i64 = 2_i64.pow(INT_BITS as u32 - 1) - 1;
pub const INT_MIN: i64 = -(2_i64.pow(INT_BITS as u32 - 1));

pub fn from_i56(arr: &[u8; 7]) -> i64 {
    let mut bytes = [0x00, arr[0], arr[1], arr[2], arr[3], arr[4], arr[5], arr[6]];
    if (arr[0] & 0x80) > 0 {
        bytes[0] = 0xff;
        i64::from_be_bytes(bytes)
    } else {
        i64::from_be_bytes(bytes)
    }
}

pub fn to_i56(i: i64) -> Value {
    let bytes = i.to_be_bytes();
    let bytes7 = [
        bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
    ];
    Value::Int(bytes7)
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Value {
    Byte(u8),
    Int([u8; 7]), // Store a 7 byte int (i56...).
    Float(F56),
    CodePoint(char),
    CharCluster(u8, [u8; 6]),
    CharClusterLong(Handle), // Handle points to a String on the heap.
    Symbol(Interned),
    Keyword(Interned),
    StringConst(Interned),
    Special(Interned), // Intended for symbols that are compiled.
    Builtin(u32),
    True,
    False,
    Nil,
    Undefined,

    String(Handle),
    Vector(Handle),
    Map(Handle),
    Bytes(Handle),
    Pair(Handle),
    List(Handle, u16),
    Lambda(Handle),
    Closure(Handle),
    Continuation(Handle),
    CallFrame(Handle),
    Value(Handle),
    Error(Handle),
}

impl Default for Value {
    fn default() -> Self {
        Self::new()
    }
}

impl From<f32> for Value {
    fn from(value: f32) -> Self {
        Self::Float(F56::from(value))
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Float(F56::from(value))
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        to_i56(value)
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        to_i56(value as i64)
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        to_i56(value as i64)
    }
}

impl Value {
    pub fn new() -> Self {
        Value::Undefined
    }

    #[inline(always)]
    pub fn unref<ENV>(self, vm: &GVm<ENV>) -> Value {
        match &self {
            Value::Value(handle) => vm.get_value(*handle),
            _ => self,
        }
    }

    pub fn get_symbol(&self) -> Option<Interned> {
        if let Value::Symbol(i) = self {
            Some(*i)
        } else {
            None
        }
    }

    pub fn is_symbol(&self, sym: Interned) -> bool {
        if let Value::Symbol(i) = self {
            *i == sym
        } else {
            false
        }
    }

    pub fn is_indirect(&self) -> bool {
        matches!(self, Value::Value(_))
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn is_undef(&self) -> bool {
        matches!(self, Value::Undefined)
    }

    pub fn is_true(&self) -> bool {
        matches!(self, Value::True)
    }

    pub fn is_truethy(&self) -> bool {
        !matches!(self, Value::False | Value::Nil)
    }

    pub fn is_false(&self) -> bool {
        matches!(self, Value::False)
    }

    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::False | Value::Nil)
    }

    pub fn is_int(&self) -> bool {
        matches!(&self, Value::Byte(_) | Value::Int(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(&self, Value::Byte(_) | Value::Int(_) | Value::Float(_))
    }

    pub fn get_int<ENV>(&self, _vm: &GVm<ENV>) -> VMResult<i64> {
        match &self {
            Value::Byte(b) => Ok(*b as i64),
            Value::Int(i) => Ok(from_i56(i)),
            _ => Err(VMError::new_value(format!("Not an integer: {self:?}"))),
        }
    }

    pub fn get_float<ENV>(&self, _vm: &GVm<ENV>) -> VMResult<f32> {
        match &self {
            Value::Byte(b) => Ok(*b as f32),
            Value::Int(i) => Ok(from_i56(i) as f32),
            Value::Float(f) => Ok(f32::from(*f)),
            _ => Err(VMError::new_value(format!("Not a float: {self:?}"))),
        }
    }

    pub fn get_string<'vm, ENV>(&self, vm: &'vm GVm<ENV>) -> VMResult<&'vm str> {
        match &self {
            Value::String(h) => Ok(vm.get_string(*h)),
            Value::StringConst(i) => Ok(vm.get_interned(*i)),
            // TODO- handle chars/codepoints...
            _ => Err(VMError::new_value(format!("Not a string: {self:?}"))),
        }
    }

    pub fn get_handle(&self) -> Option<Handle> {
        match &self {
            Value::CharClusterLong(handle) => Some(*handle),
            Value::String(handle) => Some(*handle),
            Value::Vector(handle) => Some(*handle),
            Value::Map(handle) => Some(*handle),
            Value::Bytes(handle) => Some(*handle),
            Value::Pair(handle) => Some(*handle),
            Value::List(handle, _) => Some(*handle),
            Value::Lambda(handle) => Some(*handle),
            Value::Closure(handle) => Some(*handle),
            Value::Continuation(handle) => Some(*handle),
            Value::CallFrame(handle) => Some(*handle),
            Value::Value(handle) => Some(*handle),
            Value::Error(handle) => Some(*handle),

            Value::Byte(_) => None,
            Value::Int(_) => None,
            Value::Float(_) => None,
            Value::CodePoint(_) => None,
            Value::CharCluster(_, _) => None,
            Value::Symbol(_) => None,
            Value::Keyword(_) => None,
            Value::Special(_) => None,
            Value::StringConst(_) => None,
            Value::Builtin(_) => None,
            Value::True => None,
            Value::False => None,
            Value::Nil => None,
            Value::Undefined => None,
        }
    }

    pub fn get_pair<ENV>(&self, vm: &GVm<ENV>) -> Option<(Value, Value)> {
        match &self {
            Value::Pair(handle) => {
                let (car, cdr) = vm.get_pair(*handle);
                Some((car, cdr))
            }
            Value::List(handle, start_u32) => {
                let start = *start_u32 as usize;
                let v = vm.get_vector(*handle);
                let car = if start < v.len() {
                    v[start]
                } else {
                    Value::Nil
                };
                let cdr = if start + 1 < v.len() {
                    Value::List(*handle, start_u32 + 1)
                } else {
                    Value::Nil
                };
                Some((car, cdr))
            }
            _ => None,
        }
    }

    pub fn iter<'vm, ENV>(&self, vm: &'vm GVm<ENV>) -> Box<dyn Iterator<Item = Value> + 'vm> {
        match &self.unref(vm) {
            Value::Pair(_) => Box::new(PairIter::new(vm, *self)),
            Value::List(handle, start) => {
                Box::new(vm.get_vector(*handle)[*start as usize..].iter().copied())
            }
            Value::Vector(handle) => Box::new(vm.get_vector(*handle).iter().copied()),
            _ => Box::new(iter::empty()),
        }
    }

    pub fn display_value<ENV>(&self, vm: &GVm<ENV>) -> String {
        fn list_out_iter<ENV>(
            vm: &GVm<ENV>,
            res: &mut String,
            itr: &mut dyn Iterator<Item = Value>,
        ) {
            let mut first = true;
            for p in itr {
                if !first {
                    res.push(' ');
                } else {
                    first = false;
                }
                res.push_str(&p.display_value(vm));
            }
        }
        fn list_out<ENV>(vm: &GVm<ENV>, res: &mut String, lst: Value) {
            let mut first = true;
            let mut cdr = lst;
            loop {
                if let Value::Nil = cdr {
                    break;
                }
                if !first {
                    res.push(' ');
                } else {
                    first = false;
                }
                match cdr {
                    Value::Pair(handle) => {
                        let (car, ncdr) = vm.get_pair(handle);
                        res.push_str(&car.display_value(vm));
                        cdr = ncdr;
                    }
                    _ => {
                        res.push_str(". ");
                        res.push_str(&cdr.display_value(vm));
                        break;
                    }
                }
            }
        }
        match self {
            Value::True => "true".to_string(),
            Value::False => "false".to_string(),
            Value::Int(i) => format!("{}", from_i56(i)),
            Value::Float(f) => format!("{}", f),
            Value::Byte(b) => format!("{b}"),
            Value::Symbol(i) => vm.get_interned(*i).to_string(),
            Value::Keyword(i) => format!(":{}", vm.get_interned(*i)),
            Value::StringConst(i) => format!("\"{}\"", vm.get_interned(*i)),
            Value::Special(i) => format!("#<SpecialFn({})>", vm.get_interned(*i)),
            Value::CodePoint(ch) => format!("\\{ch}"),
            Value::CharCluster(l, c) => {
                format!("\\{}", String::from_utf8_lossy(&c[0..*l as usize]))
            }
            Value::CharClusterLong(h) => format!("\\{}", vm.get_string(*h)),
            Value::Builtin(_) => "#<Function>".to_string(),
            Value::Nil => "nil".to_string(),
            Value::Undefined => "#<Undefined>".to_string(), //panic!("Tried to get type for undefined!"),
            Value::Lambda(_) => "#<Lambda>".to_string(),
            Value::Closure(_) => "#<Lambda>".to_string(),
            Value::Continuation(_) => "#<Continuation>".to_string(),
            Value::CallFrame(_) => "#<CallFrame>".to_string(),
            Value::Vector(handle) => {
                let v = vm.get_vector(*handle);
                let mut res = String::new();
                res.push('[');
                list_out_iter(vm, &mut res, &mut v.iter().copied());
                res.push(']');
                res
            }
            Value::Map(handle) => {
                let mut res = String::new();
                res.push('{');
                for (key, val) in vm.get_map(*handle).iter() {
                    res.push_str(&format!(
                        "{} {}\n",
                        key.display_value(vm),
                        val.display_value(vm)
                    ));
                }
                res.push('}');
                res
            }
            Value::Pair(_) => {
                let mut res = String::new();
                res.push('(');
                list_out(vm, &mut res, *self);
                res.push(')');
                res
            }
            Value::List(handle, start) => {
                let v = vm.get_vector(*handle);
                let mut res = String::new();
                res.push('(');
                list_out_iter(vm, &mut res, &mut v[*start as usize..].iter().copied());
                res.push(')');
                res
            }
            Value::String(handle) => format!("\"{}\"", vm.get_string(*handle)),
            Value::Bytes(_) => "Bytes".to_string(), // XXX TODO
            Value::Value(handle) => vm.get_value(*handle).display_value(vm),
            Value::Error(handle) => {
                let err = vm.get_error(*handle);
                let key = vm.get_interned(err.keyword);
                format!("error [{key}]: {}", err.data.display_value(vm))
            }
        }
    }

    pub fn pretty_value<ENV>(&self, vm: &GVm<ENV>) -> String {
        match self {
            Value::StringConst(i) => vm.get_interned(*i).to_string(),
            Value::CodePoint(ch) => format!("{ch}"),
            Value::CharCluster(l, c) => {
                format!("{}", String::from_utf8_lossy(&c[0..*l as usize]))
            }
            Value::CharClusterLong(h) => vm.get_string(*h).to_string(),
            Value::String(handle) => vm.get_string(*handle).to_string(),
            _ => self.display_value(vm),
        }
    }

    /// Map a [`Value`] to a [`ValueType`] which can be written to a debug string that refers to the
    /// Slosh types and does not require passing in a [`GVm`] to do so.
    pub fn value_type<ENV>(&self, vm: &GVm<ENV>) -> ValueType {
        match self {
            Value::Byte(_) => ValueType::Byte,
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::CodePoint(_) => ValueType::CodePoint,
            Value::CharCluster(_, _) => ValueType::CharCluster,
            Value::CharClusterLong(_) => ValueType::CharClusterLong,
            Value::Symbol(_) => ValueType::Symbol,
            Value::Keyword(_) => ValueType::Keyword,
            Value::StringConst(_) => ValueType::StringConst,
            Value::Special(_) => ValueType::Special,
            Value::Builtin(_) => ValueType::Builtin,
            Value::True => ValueType::True,
            Value::False => ValueType::False,
            Value::Nil => ValueType::Nil,
            Value::Undefined => ValueType::Undefined,
            Value::String(_) => ValueType::String,
            Value::Vector(_) => ValueType::Vector,
            Value::Map(_) => ValueType::Map,
            Value::Bytes(_) => ValueType::Bytes,
            Value::Pair(_) => ValueType::Pair,
            Value::List(_, _) => ValueType::List,
            Value::Lambda(_) => ValueType::Lambda,
            Value::Closure(_) => ValueType::Closure,
            Value::Continuation(_) => ValueType::Continuation,
            Value::CallFrame(_) => ValueType::CallFrame,
            Value::Error(_) => ValueType::Error,
            Value::Value(handle) => vm.get_value(*handle).value_type(vm),
        }
    }

    pub fn display_type<ENV>(&self, vm: &GVm<ENV>) -> &'static str {
        self.value_type(vm).into()
    }

    pub fn is_proper_list<ENV>(&self, vm: &GVm<ENV>) -> bool {
        // does not detect empty (nil) lists on purpose.
        if let Value::Pair(handle) = self {
            let (_car, cdr) = vm.get_pair(*handle);
            if cdr.is_nil() {
                true
            } else {
                cdr.is_proper_list(vm)
            }
        } else {
            matches!(self, Value::List(_, _))
        }
    }
}

#[derive(Clone, Debug)]
pub struct Globals {
    objects: Vec<Value>,
    props: HashMap<u32, Arc<HashMap<Interned, Value>>>,
}

impl Default for Globals {
    fn default() -> Self {
        Self::new()
    }
}

impl Globals {
    pub fn new() -> Self {
        Globals {
            objects: Vec::new(),
            props: HashMap::new(),
        }
    }

    pub fn reserve(&mut self) -> u32 {
        let index = self.objects.len();
        self.objects.push(Value::Undefined);
        index as u32
    }

    /// Sets a global to val.  The value needs have local numbers promoted to the heap before
    /// setting it.
    pub fn set(&mut self, idx: u32, val: Value) {
        self.objects[idx as usize] = val;
    }

    pub fn get(&self, idx: u32) -> Value {
        self.objects
            .get(idx as usize)
            .map_or_else(|| Value::Undefined, |v| *v)
    }

    pub fn mark(&self, heap: &mut Heap) {
        self.objects.iter().for_each(|obj| {
            heap.mark(*obj);
        });
        self.props.iter().for_each(|(_, map)| {
            for val in map.values() {
                heap.mark(*val);
            }
        });
    }

    pub fn get_property(&self, global: u32, prop: Interned) -> Option<Value> {
        if let Some(map) = self.props.get(&global) {
            if let Some(val) = map.get(&prop) {
                return Some(*val);
            }
        }
        None
    }

    pub fn set_property(&mut self, global: u32, prop: Interned, value: Value) {
        if let Some(map) = self.props.get_mut(&global) {
            let map = Arc::make_mut(map);
            map.insert(prop, value);
        } else {
            let mut map = HashMap::new();
            map.insert(prop, value);
            self.props.insert(global, Arc::new(map));
        }
    }
}

pub const SLOSH_CHAR: &str = "Char";
pub const SLOSH_STRING: &str = "String";
pub const SLOSH_INT: &str = "Int";
pub const SLOSH_FLOAT: &str = "Float";
pub const SLOSH_BOOL_TRUE: &str = "True";
pub const SLOSH_BOOL_FALSE: &str = "False";
pub const SLOSH_SYMBOL: &str = "Symbol";
pub const SLOSH_KEYWORD: &str = "Keyword";
pub const SLOSH_SPECIAL: &str = "Special";
pub const SLOSH_BUILTIN: &str = "Builtin";
pub const SLOSH_BYTE: &str = "Byte";
pub const SLOSH_BYTES: &str = "Bytes";
pub const SLOSH_NIL: &str = "Nil";
pub const SLOSH_UNDEFINED: &str = "Undefined";
pub const SLOSH_LAMBDA: &str = "Lambda";
pub const SLOSH_CLOSURE: &str = "Lambda";
pub const SLOSH_CONTINUATION: &str = "Continuation";
pub const SLOSH_CALLFRAME: &str = "CallFrame";
pub const SLOSH_VECTOR: &str = "Vector";
pub const SLOSH_MAP: &str = "Map";
pub const SLOSH_PAIR: &str = "Pair";
pub const SLOSH_ERROR: &str = "Error";

/// Enum representing the various types of values in Slosh.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
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

pub struct ValueTypes<const N: usize> {
    values: [ValueType; N],
}

impl<const N: usize> From<[ValueType; N]> for ValueTypes<N> {
    fn from(values: [ValueType; N]) -> Self {
        Self { values }
    }
}

impl<const N: usize> Deref for ValueTypes<N> {
    type Target = [ValueType];

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

impl<const N: usize> From<ValueTypes<N>> for String {
    fn from(value: ValueTypes<N>) -> Self {
        let mut res = String::new();
        let set: BTreeSet<&str> = BTreeSet::from_iter(
            value
                .deref()
                .iter()
                .map(|v| <ValueType as Into<&'static str>>::into(*v)),
        );
        for (i, v) in set.iter().enumerate() {
            if i > 0 {
                res.push_str(", ");
            }
            res.push_str(v);
        }
        res
    }
}
