use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

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
        let self_as_f64 = f64::from(*self);
        let other_as_f64 = f64::from(*other);
        // Allow NaN == NaN so equality is reflexive and we can impl Eq to use F56 as a hash key
        if self_as_f64.is_nan() && other_as_f64.is_nan() {
            return true;
        };
        // Round to nearest multiple of F56::EPSILON for equality test
        // Note how this is different from testing that the difference between the two is less than F56::EPSILON
        // But this is necessary to guarantee that a == b => hash(a) == hash(b)
        let precision = 1.0 / F56::EPSILON;
        // since we are just comparing the values, we don't actually need to calculate the rounded value
        // so we can omit the last step to divide by precision from both sides
        let self_scaled = (self_as_f64 * precision).round();
        let other_scaled = (other_as_f64 * precision).round();
        self_scaled == other_scaled
    }
}
impl Hash for F56 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Make sure NaN hashes to the same value
        if f64::from(*self).is_nan() {
            state.write_u64(0x7FF8000000000000u64);
            return;
        }
        // round to the nearest multiple of F56::EPSILON
        // this way, two equal F56s will always hash to the same value
        let precision = 1.0 / F56::EPSILON;
        let value = f64::from(*self);
        let rounded = (value * precision).round() / precision;
        state.write_u64(rounded.to_bits())
    }
}
impl std::fmt::Debug for F56 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "F56({:?})", f64::from(*self))
    }
}
impl Display for F56 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", F56::round_f64_to_f56_precision(f64::from(*self)))
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
            // NaN or Infinity
            // Either way the f64 will also have an exponent of all [1]s
            0b11_1111_1111 => 0b111_1111_1111_u64,

            // Zero
            _ if f56_biased_exponent == 0b00_0000_0000 && f56_mantissa == 0u64 => {
                0b000_0000_0000_u64
            }
            // Subnormal
            _ if f56_biased_exponent == 0b00_0000_0000 && f56_mantissa > 0u64 => {
                // the f56's exponent is actually representing -510 instead of 0
                // note that -510 exponent would also represented by 0x1
                // which is why we only need to add 1022 instead of 1023 to bias this for f64
                (true_exponent + 1022) as u64
            }

            // Generic case
            _ => {
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
    pub const MIN_POSITIVE_SUBNORMAL: F56 = F56([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]);
    // Minimum numer of decimal digits of precision (experimentally derived)
    // for comparison, f32 has 6-9 decimal digits of precision and f64 has 15-17. I believe F56 has 12-14
    pub const DIGITS: usize = 12;
    // Cutoff for relative difference between an f64 and the F56's approximation
    pub const EPSILON: f64 = 1e-13;
    // When converting from f64 to F56 we truncate 7 bits of the mantissa
    // We could round up if the 7th bit is 1, but this is might cause issues.
    // Mantissas like 0xFFFF_FFFF_... can catastrophically round to 0x0000_0000_...
    pub const ROUNDUP_ENABLED: bool = false;
}

impl F56 {
    pub fn round_f64_to_f56_precision(raw_f64: f64) -> f64 {
        if raw_f64.is_nan() || raw_f64.is_infinite() || raw_f64 == 0.0 {
            return raw_f64;
        }
        // round to a max of F56::DIGITS sig figs
        let orig_exponent_value = raw_f64.abs().log10().floor() as i32; // the number after 'e' in scientific notation
        let target_exponent_value = F56::DIGITS as i32 - 1; // exponent that we will shift this number to
        let scale_factor = 10f64.powi(target_exponent_value - orig_exponent_value);
        let scaled_and_rounded = (raw_f64 * scale_factor).round();

        scaled_and_rounded / scale_factor
    }
}

#[cfg(test)]
mod tests {

    use crate::f56::F56;

    const MAXIMUM_ACCEPTABLE_RELATIVE_DIFFERENCE: f64 = 1e-10;

    pub fn log_f32(f: f32) -> String {
        let bytes = f.to_be_bytes();
        let word = u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
        let f32_biased_exponent: u8 = (word >> 23) as u8; // first 8 bits after the sign bit
        let mut true_exponent: i16 = (f32_biased_exponent as i16) - 127; // remove the bias of 2^7-1
        if f32_biased_exponent == 0 {
            true_exponent = 0;
        }
        let f32_mantissa = word & 0x007f_ffff; // everything after first 9 bits
                                               // print the f32 in scientific notation, the true exponent in decimal, and the mantissa in decimal, and the hex word
        format!(
            "f32: {:.5e}, true exponent: {}, mantissa: {:016x}, word: {:016x}",
            f, true_exponent, f32_mantissa, word
        )
    }
    pub fn log_f56(f: F56) -> String {
        let bytes7 = f.0;
        let word = u64::from_be_bytes([
            0, bytes7[0], bytes7[1], bytes7[2], bytes7[3], bytes7[4], bytes7[5], bytes7[6],
        ]);
        let f56_biased_exponent: u16 = ((word >> 45) as u16) & 0x3FF; // first 10 bits after the sign bit
        let f56_mantissa = word & 0x1FFFF_FFFF_FFFF; // everything after first 10 bits
        let mut true_exponent = f56_biased_exponent as i16 - 511; // remove the bias of 2^9-1
        if f56_biased_exponent == 0 {
            true_exponent = 0;
        }
        format!(
            "f56: {:.5e}, true exponent: {}, mantissa: {:016x}, word: {:016x}",
            f64::from(f),
            true_exponent,
            f56_mantissa,
            word
        )
    }
    pub fn log_f64(f: f64) -> String {
        let bytes = f.to_be_bytes();
        let word = u64::from_be_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ]);
        let f64_biased_exponent: i16 = ((word & 0x7ff0_0000_0000_0000) >> 52) as i16; // first 11 bits after the sign bit
        let mut true_exponent: i16 = f64_biased_exponent - 1023; // remove the bias of 2^10-1
        if f64_biased_exponent == 0 {
            true_exponent = 0;
        }
        let f64_mantissa = word & 0x001f_ffff_ffff_ffff; // everything after first 11 bits
        format!(
            "f64: {:.5e}, true exponent: {}, mantissa: {:016x}, word: {:016x}",
            f, true_exponent, f64_mantissa, word
        )
    }
    fn debug(orig_f64: f64, index: usize) {
        println!("index: {}", index);
        println!("original f64      : {}", log_f64(orig_f64));
        println!("f64 -> f32        : {}", log_f32(orig_f64 as f32));
        println!("f64 -> f32 -> f64 : {}", log_f64((orig_f64 as f32) as f64));
        println!("f64 -> f56        : {}", log_f56(F56::from(orig_f64)));
        println!(
            "f64 -> f56 -> f64 : {}",
            log_f64(f64::from(F56::from(orig_f64)))
        );
        let f32_diff = (f64::from(orig_f64 as f32) - orig_f64).abs();
        let f32_relative_difference = f32_diff / orig_f64.abs();
        if f32_relative_difference > 0.0 {
            println!("f32 relative difference {:.5e}", f32_relative_difference);
        }
        let f56_diff: f64 = (f64::from(F56::from(orig_f64)) - orig_f64).abs();
        let f56_relative_difference = f56_diff / orig_f64.abs();
        if f56_relative_difference > 0.0 {
            println!("f56 relative difference {:.5e}", f56_relative_difference);
        }
        println!("")
    }

    fn get_regular_f64_values() -> [f64; 15] {
        [
            0_f64,
            0.0,
            1.0,
            2.0,
            2.5123,
            3.0,
            4.0,
            5.0,
            6.0,
            7.0,
            8.0,
            9.0,
            10.0,
            -1.0,
            -0.33333333333333333333333333333,
        ]
    }

    fn get_variety_f64_values() -> [f64; 133] {
        [
            399.999_999_999_58527_f64,
            399.999_999_999_585_f64,
            399.999_999_999_58_f64,
            399.999_999_999_6_f64,
            399.999_999_999_f64,
            0.000_000_012_312_312_412_412_312_3_f64,
            0.000_000_012_312_312_452_57_f64,
            399_999_999.999_58_f64,
            399_999_999_999_600_000_000_000_000_000_000_0_f64,
            399_999_999_999_600_000_000_000_000_000_000_0_f64,
            0x0000_0000_0000_0000u64 as f64,
            0x0000_0000_0000_0001u64 as f64,
            0x8000_0000_0000_0000u64 as f64,
            0x7FFF_FFFF_FFFF_FFFFu64 as f64,
            0x7FFF_FFFF_FFFF_FFFEu64 as f64,
            0xFFFF_FFFF_FFFF_FFFFu64 as f64,
            0x7000_0000_0000_0000u64 as f64,
            0xDEAD_BEEF_DEAD_BEEFu64 as f64,
            0x1234_5678_9ABC_DEF0u64 as f64,
            0x1111_1111_1111_1111u64 as f64,
            0x2222_2222_2222_2222u64 as f64,
            0x3333_3333_3333_3333u64 as f64,
            0x4444_4444_4444_4444u64 as f64,
            0x5555_5555_5555_5555u64 as f64,
            0x6666_6666_6666_6666u64 as f64,
            0x7777_7777_7777_7777u64 as f64,
            0x8888_8888_8888_8888u64 as f64,
            0x9999_9999_9999_9999u64 as f64,
            0xAAAA_AAAA_AAAA_AAAAu64 as f64,
            0xBBBB_BBBB_BBBB_BBBBu64 as f64,
            0xCCCC_CCCC_CCCC_CCCCu64 as f64,
            0xDDDD_DDDD_DDDD_DDDDu64 as f64,
            0xEEEE_EEEE_EEEE_EEEEu64 as f64,
            0xFFFF_FFFF_FFFF_FFFEu64 as f64,
            0xFFF0_0000_0000_0001u64 as f64,
            0xDEAD_BEEF_DEAD_BEEFu64 as f64,
            0x1234_5678_9ABC_DEF0u64 as f64,
            0x1111_1111_1111_1111u64 as f64,
            0x2222_2222_2222_2222u64 as f64,
            0x3333_3333_3333_3333u64 as f64,
            0x4444_4444_4444_4444u64 as f64,
            0x5555_5555_5555_5555u64 as f64,
            0x6666_6666_6666_6666u64 as f64,
            0x7777_7777_7777_7777u64 as f64,
            0x8888_8888_8888_8888u64 as f64,
            0x9999_9999_9999_9999u64 as f64,
            0xAAAA_AAAA_AAAA_AAAAu64 as f64,
            0xBBBB_BBBB_BBBB_BBBBu64 as f64,
            0xCCCC_CCCC_CCCC_CCCCu64 as f64,
            0xDDDD_DDDD_DDDD_DDDDu64 as f64,
            0xEEEE_EEEE_EEEE_EEEEu64 as f64,
            2.3,
            23.0,
            230.0,
            2300.0,
            23000.0,
            230000.0,
            23e5,
            23e6,
            2.3e5,
            0.23,
            0.023,
            0.0023,
            0.00023,
            0.000023,
            0.0000023,
            0.23e-5,
            -1234567890123456789012345678901.0,
            -1.1412314e108,
            -3.33e55,
            -1e44,
            -1337.1337,
            -222.2,
            -0.0,
            -0.1,
            0.0,
            0.1,
            0.01,
            0.001,
            0.249,
            0.999,
            1.0,
            1.001,
            1.01,
            1.1,
            1.999,
            2.0,
            2.2345,
            3.0,
            3.33333333333333333333333333333333,
            4.0,
            4.44,
            5.0,
            5.1,
            6.0,
            6.2,
            7.0,
            8.0,
            9.0,
            10.0,
            100.0,
            234.432,
            420.69,
            1234.0,
            12345.0,
            123456.0,
            1234567.0,
            12345678.0,
            123456789.0,
            1234567890.0,
            12345678901.0,
            123456789012.0,
            1234567890123.0,
            12345678901234.0,
            123456789012345.0,
            1234567890123456.0,
            12345678901234567.0,
            123456789012345678.0,
            1234567890123456789.0,
            12345678901234567890.0,
            123456789012345678901.0,
            1234567890123456789012.0,
            12345678901234567890123.0,
            123456789012345678901234.0,
            1234567890123456789012345.0,
            12345678901234567890123456.0,
            123456789012345678901234567.0,
            1234567890123456789012345678.0,
            12345678901234567890123456789.0,
            123456789012345678901234567890.0,
            123456789012345678901234567890.1,
            999.999e99,
            1e100,
        ]
    }

    fn get_edge_case_f64_values() -> [f64; 39] {
        [
            f64::MIN,
            f64::MAX,
            f64::MIN_POSITIVE,
            f64::INFINITY,
            f64::NEG_INFINITY,
            f64::NAN,
            -0.0,
            f32::MIN_POSITIVE as f64, // 42
            f32::MIN_POSITIVE as f64 / 3.0,
            f32::MIN_POSITIVE as f64 / 7e5,
            f32::MIN_POSITIVE as f64 / 7e6,
            f32::MIN_POSITIVE as f64 / 7e7,
            f32::MIN_POSITIVE as f64 / 7e8,
            f32::MIN_POSITIVE as f64 / 4.123e14,
            f32::MAX as f64,
            f32::MAX as f64 * 3.0,
            f32::MAX as f64 + 99.0,
            f64::MIN_POSITIVE,
            f64::MIN_POSITIVE / 2.0,
            f64::MIN_POSITIVE / 10.0,
            f64::MIN,
            f64::MAX,
            f64::INFINITY,
            f64::NEG_INFINITY,
            f64::NAN,
            F56::EPSILON,
            F56::EPSILON / 3.0,
            8.4e-168,
            8.4e-169,
            8.4e-170,
            0xFFFF_FFFF_FFFF_FFFEu64 as f64,
            0xFFF0_0000_0000_0001u64 as f64,
            0x0000_0000_0000_0000u64 as f64,
            0x0000_0000_0000_0001u64 as f64,
            0x8000_0000_0000_0000u64 as f64,
            0x7FFF_FFFF_FFFF_FFFFu64 as f64,
            0x7FFF_FFFF_FFFF_FFFEu64 as f64,
            0xFFFF_FFFF_FFFF_FFFFu64 as f64,
            0x7000_0000_0000_0000u64 as f64,
        ]
    }

    fn relative_difference(a: f64, b: f64) -> f64 {
        (a - b).abs() / b.abs()
    }

    // TODO: test the F56::MAX, F56::MIN_POSITIVE, F56::MIN_POSITIVE_SUBNORMAL cases

    #[test]
    fn f56_strings_match_f64_strings() {
        let string_test_closure = |f64_value: &f64| {
            let f64_string = format!("{}", f64_value);
            let f56_value = F56::from(*f64_value);
            let f56_string = format!("{}", f56_value);
            if f56_string == f64_string {
                return;
            }
            println!("f64: {} not quite equal\nF56: {}", f64_string, f56_string);
            // let abs: f64 = f64_value.abs();
            if f64_value.abs() > f64::from(F56::MAX) && f56_string.contains("inf") {
                println!("But F56 is expected to be infinite if f64 is outside of its range. {:.0e} > {:.0e}\n", f64_value, f64::from(F56::MAX));
                return;
            }
            if f64_value.abs() < F56::MIN_POSITIVE_SUBNORMAL.into() && f64::from(f56_value) == 0.0 {
                println!(
                    "But F56 is expected to be 0 if f64 is outside of its range. {:.0e} < {:.0e}\n",
                    f64_value,
                    f64::from(F56::MIN_POSITIVE_SUBNORMAL)
                );
                return;
            }

            let f56_string_to_f64_value = f56_string.parse::<f64>().unwrap();
            let f64_string_to_f64_value = f64_string.parse::<f64>().unwrap();
            let relative_difference =
                relative_difference(f56_string_to_f64_value, f64_string_to_f64_value);
            if relative_difference < MAXIMUM_ACCEPTABLE_RELATIVE_DIFFERENCE {
                println!(
                    "But the relative difference of {} is acceptably below the maximum {}\n",
                    relative_difference, MAXIMUM_ACCEPTABLE_RELATIVE_DIFFERENCE,
                );
                return;
            }
            // Failing this test case
            debug(*f64_value, 0);
            assert_eq!(
                f64_string, f56_string,
                "f64(left) and f56(right) string values must be equal"
            );
        };
        println!("\n\n\n\nRegular f64 values");
        for f in get_regular_f64_values().iter() {
            string_test_closure(f);
        }
        println!("\n\n\n\nVariety f64 values");
        for f in get_variety_f64_values().iter() {
            string_test_closure(f);
        }
        println!("\n\n\n\nEdge case f64 values");
        for f in get_edge_case_f64_values().iter() {
            string_test_closure(f);
        }
    }
}
