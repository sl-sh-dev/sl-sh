use crate::types::SlFrom;
use bridge_types::ErrorStrings;
use compile_state::state::SloshVm;
use slvm::value::ValueType;
use slvm::{from_i56, to_i56, VMError, VMResult, Value};

impl SlFrom<i32> for Value {
    fn sl_from(value: i32, _vm: &mut SloshVm) -> VMResult<Self> {
        Ok(to_i56(value as i64))
    }
}
impl SlFrom<u32> for Value {
    fn sl_from(value: u32, _vm: &mut SloshVm) -> VMResult<Self> {
        Ok(to_i56(value as i64))
    }
}

impl SlFrom<&Value> for i32 {
    fn sl_from(value: &Value, vm: &mut SloshVm) -> VMResult<i32> {
        match value {
            Value::Int(num) => {
                let num = from_i56(num);
                num.try_into().map_err(|_| {
                    VMError::new_conversion(
                        "Provided slosh value too small to fit desired type.".to_string(),
                    )
                })
            }
            _ => Err(VMError::new_conversion(
                ErrorStrings::fix_me_mismatched_type(
                    <&'static str>::from(ValueType::Int),
                    value.display_type(vm),
                ),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::types::SlFrom;
    use crate::types::SlInto;
    use compile_state::state::new_slosh_vm;
    use core::panic;
    use slvm::F56;
    use slvm::{to_i56, Value};

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

    #[test]
    fn test_f56_formatting() {
        // F56 should have a maximum of 12 decimal digits displayed
        // And the 12th digit should be rounded using the 13th digit
        // If we input an f64 with the 13th digit of 5
        // The nearest F56 representation might have the 13th digit become 4 or 6
        // So be wary that rounding may not occur as expected
        // 3.999_999_999_995 rounds down instead of rounding up to 4
        let map_from_value_to_expected_string = [
            // (3.999_999_999_995, "4"), // bad rounding
            (399.999_999_999_58527_f64, "400"),
            (399.999_999_999_585_f64, "400"),
            (399.999_999_999_58_f64, "400"),
            (399.999_999_999_6_f64, "400"),
            (399.999_999_999_f64, "399.999999999"),
            (
                0.000_000_012_312_312_412_412_312_3_f64,
                "0.0000000123123124124",
            ),
            (0.000_000_012_312_312_452_57_f64, "0.0000000123123124526"),
            (399_999_999.999_58_f64, "400000000"),
            (
                399_999_999_999_600_000_000_000_000_000_000_0_f64,
                "4000000000000000000000000000000000",
            ),
            (
                399_999_999_999_600_000_000_000_000_000_000_0_f64,
                "4000000000000000000000000000000000",
            ),
            // special values
            (f64::NAN, "NaN"),
            (f64::INFINITY, "inf"),
            (f64::NEG_INFINITY, "-inf"),
            (0.0, "0"),
            (-0.0, "-0"),
            (f64::MIN_POSITIVE, "0"),
            // (F56::MIN_POSITIVE.into(), "0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000298333629248"),
        ];
        for (f, expected) in map_from_value_to_expected_string.iter() {
            // println!("{:?}", f);
            let f56 = F56::from(*f);
            let formatted = format!("{:}", f56);
            assert_eq!(formatted, *expected);
        }
    }

    #[test]
    fn test_i32_conversions_rust_to_value() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;
        let test_vals = vec![0_i32, 1_i32, -1_i32, i32::MIN, i32::MAX];
        for val in test_vals {
            let val: Value = val.sl_into(vm).expect("i32 can be converted to Value");
            assert!(matches!(val, Value::Int(_)));
        }
    }

    #[test]
    fn test_i32_conversions_value_to_rust() {
        let mut vm = new_slosh_vm();
        let vm = &mut vm;
        let val = to_i56(7_i32 as i64);
        let _val: i32 = i32::sl_from(&val, vm).expect("Value can be converted to i32");
    }

    #[test]
    /// Declares a bunch of f64s meant to represent a wide range of values of various edge cases
    /// Converts each f64 to f56 and back to f64 to see how it changes
    /// If they are equal, then that conversion was successful
    /// If they are unequal, then it may still be successful if the f64 was outside of the range that the F56 could represent
    /// A debug function is provided to visually inspect the bytes of the f64, F56, and corresponding f32

    fn test_f56() {
        let numbers_to_test = [
            f64::from_bits(0x0000_0000_0000_0000u64),
            f64::from_bits(0x0000_0000_0000_0001u64),
            f64::from_bits(0x8000_0000_0000_0000u64),
            f64::from_bits(0x7FFF_FFFF_FFFF_FFFFu64),
            f64::from_bits(0x7FFF_FFFF_FFFF_FFFEu64),
            f64::from_bits(0xFFFF_FFFF_FFFF_FFFFu64),
            f64::from_bits(0x7000_0000_0000_0000u64),
            f64::from_bits(0xDEAD_BEEF_DEAD_BEEFu64),
            f64::from_bits(0x1234_5678_9ABC_DEF0u64),
            f64::from_bits(0x1111_1111_1111_1111u64),
            f64::from_bits(0x2222_2222_2222_2222u64),
            f64::from_bits(0x3333_3333_3333_3333u64),
            f64::from_bits(0x4444_4444_4444_4444u64),
            f64::from_bits(0x5555_5555_5555_5555u64),
            f64::from_bits(0x6666_6666_6666_6666u64),
            f64::from_bits(0x7777_7777_7777_7777u64),
            f64::from_bits(0x8888_8888_8888_8888u64),
            f64::from_bits(0x9999_9999_9999_9999u64),
            f64::from_bits(0xAAAA_AAAA_AAAA_AAAAu64),
            f64::from_bits(0xBBBB_BBBB_BBBB_BBBBu64),
            f64::from_bits(0xCCCC_CCCC_CCCC_CCCCu64),
            f64::from_bits(0xDDDD_DDDD_DDDD_DDDDu64),
            f64::from_bits(0xEEEE_EEEE_EEEE_EEEEu64),
            f64::from_bits(0xFFFF_FFFF_FFFF_FFFEu64),
            f64::from_bits(0xFFF0_0000_0000_0001u64),
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
            0.0,
            -0.0,
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
            f64::from(F56::MAX),
            f64::from(F56::MAX) + 1.0,
            f64::from(F56::MIN_POSITIVE),
            f64::from(F56::MIN_POSITIVE) / 3.0, // #55
            f64::from(F56::MIN_POSITIVE_SUBNORMAL),
            f64::from(F56::MIN_POSITIVE_SUBNORMAL) / 3.0,
            -f64::from(F56::MAX),
            -f64::from(F56::MAX) - 1.0,
            -f64::from(F56::MAX) * 3.3,
            8.4e-168,
            8.4e-169,
            8.4e-170,
        ];

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

        for index in 0..numbers_to_test.len() {
            let orig_f64 = numbers_to_test[index];
            // Calculate the values
            let f32 = orig_f64 as f32;
            let f56 = F56::from(orig_f64);
            let back_to_f64: f64 = f56.into();
            let f32_diff = ((f32 as f64) - orig_f64).abs();
            let f56_diff = (back_to_f64 - orig_f64).abs();

            // If the converted value equals the original, we passed the test
            if back_to_f64 == orig_f64 {
                continue;
            }

            // Signs must match
            if orig_f64.is_sign_positive() != back_to_f64.is_sign_positive() {
                debug(orig_f64, index);
                panic!("Signs don't match");
            }

            // NaNs must match
            if orig_f64.is_nan() != back_to_f64.is_nan() {
                debug(orig_f64, index);
                panic!("original f64 and f56 should either both be NaN or neither be NaN");
            }

            // Both must be finite or infinite
            // Unless the f64 is very large and the F56 is infinite
            if orig_f64.is_infinite() != back_to_f64.is_infinite() {
                if orig_f64.is_infinite() {
                    debug(orig_f64, index);
                    panic!("f56 should be infinite when original f64 is infinite");
                } else if back_to_f64.is_infinite() {
                    if orig_f64.abs() < f64::from(F56::MAX) {
                        debug(orig_f64, index);
                        println!("F56::MAX: {:.2e}", f64::from(F56::MAX));
                        panic!("f56 should be finite if original f64 is between -F56::MAX and +F56::MAX");
                    }
                }
                continue; // Don't check magnitude of difference if one is infinite
            }
            // Both must be zero or nonzero
            // Unless the f64 is very small and the F56 is zero
            if (orig_f64 == 0.0) != (back_to_f64 == 0.0) {
                if orig_f64 == 0.0 {
                    debug(orig_f64, index);
                    panic!("f56 should be zero when original f64 is zero");
                } else if orig_f64.abs() > f64::from(F56::MIN_POSITIVE) {
                    debug(orig_f64, index);
                    panic!(
                        "f56 should be zero when abs(original f64) < F56::MIN_POSITIVE_SUBNORMAL ({})",
                        f64::from(F56::MIN_POSITIVE_SUBNORMAL)
                    );
                }
                continue;
            }

            // The absolute difference must be small
            // smaller than the difference between an f32 and the f64
            if f56_diff > f32_diff {
                debug(orig_f64, index);
                panic!("f56 is less accurate than the f32 conversion");
            }

            // The relative difference must be less than F56's EPSILON
            let relative_difference = f56_diff / orig_f64.abs();
            if relative_difference > F56::EPSILON {
                debug(orig_f64, index);
                panic!(
                    "f56 is less accurate than it should be. relative difference: {}, max: {}",
                    relative_difference,
                    F56::EPSILON
                );
            }
        }
    }
}
