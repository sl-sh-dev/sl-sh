#[cfg(not(feature = "nohelmet"))]
#[macro_export]
macro_rules! get_code {
    ($chunk:expr) => {{
        &$chunk.code[..]
    }};
}

#[cfg(feature = "nohelmet")]
#[macro_export]
macro_rules! get_code {
    ($chunk:expr) => {{
        $chunk.code.as_ptr()
    }};
}

#[cfg(not(feature = "nohelmet"))]
#[macro_export]
macro_rules! decode_u8 {
    ($code:expr, $ip:expr) => {{
        let idx1 = $code[*$ip];
        *$ip += 1;
        idx1
    }};
}

#[cfg(feature = "nohelmet")]
#[macro_export]
macro_rules! decode_u8 {
    ($code:expr, $ip:expr) => {{
        unsafe {
            let idx1 = *$code.add(*$ip);
            *$ip += 1;
            idx1
        }
    }};
}

#[cfg(not(feature = "nohelmet"))]
#[macro_export]
macro_rules! decode_u16 {
    ($code:expr, $ip:expr) => {{
        let idx1 = $code[*$ip];
        let idx2 = $code[*$ip + 1];
        *$ip += 2;
        ((idx1 as u16) << 8) | (idx2 as u16)
    }};
}

#[cfg(feature = "nohelmet")]
#[macro_export]
macro_rules! decode_u16 {
    ($code:expr, $ip:expr) => {{
        unsafe {
            let idx1 = *$code.add(*$ip);
            let idx2 = *$code.add(*$ip + 1);
            *$ip += 2;
            ((idx1 as u16) << 8) | (idx2 as u16)
        }
    }};
}

#[cfg(not(feature = "nohelmet"))]
#[macro_export]
macro_rules! decode_u32 {
    ($code:expr, $ip:expr) => {{
        let idx1 = $code[*$ip];
        let idx2 = $code[*$ip + 1];
        let idx3 = $code[*$ip + 2];
        let idx4 = $code[*$ip + 3];
        *$ip += 4;
        ((idx1 as u32) << 24) | ((idx2 as u32) << 16) | ((idx3 as u32) << 8) | (idx4 as u32)
    }};
}

#[cfg(feature = "nohelmet")]
#[macro_export]
macro_rules! decode_u32 {
    ($code:expr, $ip:expr) => {{
        unsafe {
            let idx1 = *$code.add(*$ip);
            let idx2 = *$code.add(*$ip + 1);
            let idx3 = *$code.add(*$ip + 2);
            let idx4 = *$code.add(*$ip + 3);
            *$ip += 4;
            ((idx1 as u32) << 24) | ((idx2 as u32) << 16) | ((idx3 as u32) << 8) | (idx4 as u32)
        }
    }};
}

#[cfg(not(feature = "nohelmet"))]
#[macro_export]
macro_rules! decode1 {
    ($code:expr, $ip:expr, $wide:expr) => {{
        if $wide {
            decode_u16!($code, $ip)
        } else {
            let oip = *$ip;
            *$ip += 1;
            $code[oip] as u16
        }
    }};
}

#[cfg(feature = "nohelmet")]
#[macro_export]
macro_rules! decode1 {
    ($code:expr, $ip:expr, $wide:expr) => {{
        if $wide {
            decode_u16!($code, $ip)
        } else {
            let oip = *$ip;
            *$ip += 1;
            unsafe { *$code.add(oip) as u16 }
        }
    }};
}

#[cfg(not(feature = "nohelmet"))]
#[macro_export]
macro_rules! decode2 {
    ($code:expr, $ip:expr, $wide:expr) => {{
        if $wide {
            (decode_u16!($code, $ip), decode_u16!($code, $ip))
        } else {
            let oip = *$ip;
            *$ip += 2;
            ($code[oip] as u16, $code[oip + 1] as u16)
        }
    }};
}

#[cfg(feature = "nohelmet")]
#[macro_export]
macro_rules! decode2 {
    ($code:expr, $ip:expr, $wide:expr) => {{
        if $wide {
            (decode_u16!($code, $ip), decode_u16!($code, $ip))
        } else {
            let oip = *$ip;
            *$ip += 2;
            unsafe { (*$code.add(oip) as u16, *$code.add(oip + 1) as u16) }
        }
    }};
}

#[cfg(not(feature = "nohelmet"))]
#[macro_export]
macro_rules! decode3 {
    ($code:expr, $ip:expr, $wide:expr) => {{
        if $wide {
            (
                decode_u16!($code, $ip),
                decode_u16!($code, $ip),
                decode_u16!($code, $ip),
            )
        } else {
            let oip = *$ip;
            *$ip += 3;
            (
                $code[oip] as u16,
                $code[oip + 1] as u16,
                $code[oip + 2] as u16,
            )
        }
    }};
}

#[cfg(feature = "nohelmet")]
#[macro_export]
macro_rules! decode3 {
    ($code:expr, $ip:expr, $wide:expr) => {{
        if $wide {
            (
                decode_u16!($code, $ip),
                decode_u16!($code, $ip),
                decode_u16!($code, $ip),
            )
        } else {
            let oip = *$ip;
            *$ip += 3;
            unsafe {
                (
                    *$code.add(oip) as u16,
                    *$code.add(oip + 1) as u16,
                    *$code.add(oip + 2) as u16,
                )
            }
        }
    }};
}

#[macro_export]
macro_rules! get_reg_unref {
    ($regs:expr, $idx:expr, $vm:expr) => {{
        let reg = $regs[$idx as usize];
        match &reg {
            Value::Value(handle) => $vm.heap.get_value(*handle),
            _ => reg,
        }
    }};
}

#[macro_export]
macro_rules! get_reg_unref_int {
    ($regs:expr, $idx:expr, $vm:expr) => {{
        let reg = $regs[$idx as usize];
        match match &reg {
            Value::Value(handle) => $vm.heap.get_value(*handle),
            _ => reg,
        } {
            Value::Byte(b) => Ok(b as i64),
            Value::Int(i) => Ok(i),
            Value::UInt(i) => Ok(i as i64),
            // XXX TODO int 64s
            _ => Err(VMError::new_value(format!("Not an integer: {:?}", reg))),
        }
    }};
}

#[macro_export]
macro_rules! get_reg_int {
    ($vm:expr, $regs:expr, $idx:expr) => {{
        let reg = $regs[$idx as usize];
        match reg {
            Value::Byte(b) => Ok(b as i64),
            Value::Int32(i) => Ok(i as i64),
            Value::UInt32(i) => Ok(i as i64),
            Value::Int64(handle) => Ok($vm.get_int(handle)),
            Value::UInt64(handle) => Ok($vm.get_uint(handle) as i64), // XXX TODO- overflow.
            _ => Err(VMError::new_value(format!("Not an integer: {:?}", reg))),
        }
    }};
}

#[macro_export]
macro_rules! get_reg {
    ($regs:expr, $idx:expr) => {{
        $regs[$idx as usize]
    }};
}

macro_rules! compare_int {
    ($vm:expr, $chunk:expr, $code:expr, $ip:expr, $registers:expr, $comp_fn:expr,
     $compf_fn:expr, $wide:expr, $move:expr, $not:expr) => {{
        let (dest, reg1, reg2) = decode3!($code, $ip, $wide);
        let mut val = false;
        for reg in reg1..reg2 {
            let op1 = get_reg_unref!($registers, reg, $vm);
            let op2 = get_reg_unref!($registers, reg + 1, $vm);
            val = if matches!(op1, Value::Float64(_)) || matches!(op2, Value::Float64(_)) {
                $comp_fn(
                    get_float!($vm, op1).map_err(|e| (e, $chunk.clone()))?,
                    get_float!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
                )
            } else {
                $comp_fn(
                    get_int!($vm, op1).map_err(|e| (e, $chunk.clone()))?,
                    get_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
                )
            };
            if !val {
                break;
            }
        }
        if $not {
            val = !val;
        }
        let val = if val { Value::True } else { Value::False };
        if $move {
            $registers[dest as usize] = val;
        } else {
            set_register!($vm, $registers, dest as usize, val);
        }
    }};
}

macro_rules! compare {
    ($vm:expr, $chunk:expr, $code:expr, $ip:expr, $registers:expr, $comp_fn:expr, $wide:expr, $move:expr) => {{
        compare_int!($vm, $chunk, $code, $ip, $registers, $comp_fn, $comp_fn, $wide, $move, false)
    }};
}

macro_rules! get_int {
    ($vm:expr, $val:expr) => {{
        match $val {
            Value::Byte(b) => Ok(b as i64),
            Value::Int32(i) => Ok(i as i64),
            Value::UInt32(i) => Ok(i as i64),
            Value::Int64(handle) => Ok($vm.get_int(handle)),
            Value::UInt64(handle) => Ok($vm.get_uint(handle) as i64), // XXX TODO- overflow.
            _ => Err(VMError::new_value(format!("Not an integer: {:?}", $val))),
        }
    }};
}

macro_rules! get_float {
    ($vm:expr, $val:expr) => {{
        match $val {
            Value::Byte(b) => Ok(b as f64),
            Value::Int32(i) => Ok(i as f64),
            Value::UInt32(i) => Ok(i as f64),
            Value::Int64(handle) => Ok($vm.get_int(handle) as f64),
            Value::UInt64(handle) => Ok($vm.get_uint(handle) as f64),
            Value::Float64(handle) => Ok($vm.get_float(handle)),
            _ => Err(VMError::new_value(format!("Not a float: {:?}", $val))),
        }
    }};
}

macro_rules! binary_math {
    ($vm:expr, $chunk:expr, $code:expr, $ip:expr, $registers:expr, $bin_fn:expr, $wide:expr) => {{
        let (dest, op2) = decode2!($code, $ip, $wide);
        let op1 = get_reg!($registers, dest);
        let op2 = get_reg!($registers, op2);
        match (op1, op2) {
            (Value::Float64(op1_handle), Value::Float64(op2_handle)) => {
                *$vm.get_float_mut(op1_handle) =
                    $bin_fn($vm.get_float(op1_handle), $vm.get_float(op2_handle));
            }
            (Value::Float64(op1_handle), _) => {
                *$vm.get_float_mut(op1_handle) = $bin_fn(
                    $vm.get_float(op1_handle),
                    get_float!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
                );
            }
            (_, Value::Float64(op2_handle)) => {
                $registers[dest as usize] = $vm.local_f64(
                    dest as usize,
                    $bin_fn(
                        get_float!($vm, op1).map_err(|e| (e, $chunk.clone()))?,
                        $vm.get_float(op2_handle),
                    ),
                );
            }
            (Value::Int64(op1_handle), Value::Int64(op2_handle)) => {
                *$vm.get_int_mut(op1_handle) =
                    $bin_fn($vm.get_int(op1_handle), $vm.get_int(op2_handle));
            }
            (Value::Int64(op1_handle), _) => {
                *$vm.get_int_mut(op1_handle) = $bin_fn(
                    $vm.get_int(op1_handle),
                    get_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
                );
            }
            (Value::UInt64(op1_handle), Value::UInt64(op2_handle)) => {
                *$vm.get_uint_mut(op1_handle) =
                    $bin_fn($vm.get_uint(op1_handle), $vm.get_uint(op2_handle));
            }
            (Value::UInt64(op1_handle), _) => {
                let val = $bin_fn(
                    $vm.get_uint(op1_handle) as i64,
                    get_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
                );
                if val >= 0 {
                    *$vm.get_uint_mut(op1_handle) = val as u64;
                } else {
                    $registers[dest as usize] = $vm.local_i64(dest as usize, val);
                }
            }
            (Value::Int32(op1_val), _) => {
                let val = $bin_fn(
                    op1_val as i64,
                    get_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
                );
                if val < i32::MAX as i64 {
                    $registers[dest as usize] = Value::Int32(val as i32);
                } else {
                    $registers[dest as usize] = $vm.local_i64(dest as usize, val);
                }
            }
            (Value::UInt32(op1_val), _) => {
                let val = $bin_fn(
                    op1_val as i64,
                    get_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
                );
                if val >= 0 && val < u32::MAX as i64 {
                    $registers[dest as usize] = Value::UInt32(val as u32);
                } else {
                    $registers[dest as usize] = $vm.local_i64(dest as usize, val);
                }
            }
            (_, _) => {
                $registers[dest as usize] = $vm.local_i64(
                    dest as usize,
                    $bin_fn(
                        get_int!($vm, op1).map_err(|e| (e, $chunk.clone()))?,
                        get_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
                    ),
                );
            }
        }
    }};
}

macro_rules! div_math {
    ($vm:expr, $chunk:expr, $code:expr, $ip:expr, $registers:expr, $wide:expr) => {{
        let (dest, op2) = decode2!($code, $ip, $wide);
        let op1 = get_reg!($registers, dest);
        let op2 = get_reg!($registers, op2);
        match (op1, op2) {
            (Value::Float64(op1_handle), Value::Float64(op2_handle)) => {
                let op1 = $vm.get_float(op1_handle);
                let op2 = $vm.get_float(op2_handle);
                if op2 == 0.0 {
                    return Err((VMError::new_vm("Divide by zero error."), $chunk));
                }
                *$vm.get_float_mut(op1_handle) = op1 / op2;
            }
            (Value::Float64(op1_handle), _) => {
                let op1 = $vm.get_float(op1_handle);
                let op2 = get_float!($vm, op2).map_err(|e| (e, $chunk.clone()))?;
                if op2 == 0.0 {
                    return Err((VMError::new_vm("Divide by zero error."), $chunk));
                }
                *$vm.get_float_mut(op1_handle) = op1 / op2;
            }
            (_, Value::Float64(op2_handle)) => {
                let op1 = get_float!($vm, op1).map_err(|e| (e, $chunk.clone()))?;
                let op2 = $vm.get_float(op2_handle);
                if op2 == 0.0 {
                    return Err((VMError::new_vm("Divide by zero error."), $chunk));
                }
                $registers[dest as usize] = $vm.local_f64(dest as usize, op1 / op2);
            }
            (Value::Int64(op1_handle), Value::Int64(op2_handle)) => {
                let op1 = $vm.get_int(op1_handle);
                let op2 = $vm.get_int(op2_handle);
                if op2 == 0 {
                    return Err((VMError::new_vm("Divide by zero error."), $chunk));
                }
                *$vm.get_int_mut(op1_handle) = op1 / op2;
            }
            (Value::Int64(op1_handle), _) => {
                let op1 = $vm.get_int(op1_handle);
                let op2 = get_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?;
                if op2 == 0 {
                    return Err((VMError::new_vm("Divide by zero error."), $chunk));
                }
                *$vm.get_int_mut(op1_handle) = op1 / op2;
            }
            (Value::UInt64(op1_handle), Value::UInt64(op2_handle)) => {
                let op1 = $vm.get_uint(op1_handle);
                let op2 = $vm.get_uint(op2_handle);
                if op2 == 0 {
                    return Err((VMError::new_vm("Divide by zero error."), $chunk));
                }
                *$vm.get_uint_mut(op1_handle) = op1 / op2;
            }
            (Value::UInt64(op1_handle), _) => {
                let op1 = $vm.get_uint(op1_handle);
                let op2 = get_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?;
                if op2 == 0 {
                    return Err((VMError::new_vm("Divide by zero error."), $chunk));
                }
                if op2 > 0 {
                    *$vm.get_uint_mut(op1_handle) = op1 / op2 as u64;
                } else {
                    $registers[dest as usize] = $vm.local_i64(dest as usize, op1 as i64 / op2);
                }
            }
            (Value::Int32(op1_val), _) => {
                let op1 = op1_val as i64;
                let op2 = get_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?;
                if op2 == 0 {
                    return Err((VMError::new_vm("Divide by zero error."), $chunk));
                }
                let val = op1 / op2;
                if val < i32::MAX as i64 {
                    $registers[dest as usize] = Value::Int32(val as i32);
                } else {
                    $registers[dest as usize] = $vm.local_i64(dest as usize, val);
                }
            }
            (Value::UInt32(op1_val), _) => {
                let op1 = op1_val as i64;
                let op2 = get_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?;
                if op2 == 0 {
                    return Err((VMError::new_vm("Divide by zero error."), $chunk));
                }
                let val = op1 / op2;
                if val >= 0 && val < u32::MAX as i64 {
                    $registers[dest as usize] = Value::UInt32(val as u32);
                } else {
                    $registers[dest as usize] = $vm.local_i64(dest as usize, val);
                }
            }
            (_, _) => {
                let op1 = get_int!($vm, op1).map_err(|e| (e, $chunk.clone()))?;
                let op2 = get_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?;
                if op2 == 0 {
                    return Err((VMError::new_vm("Divide by zero error."), $chunk));
                }
                let val = op1 / op2;
                if val >= 0 && val < u32::MAX as i64 {
                    $registers[dest as usize] = Value::UInt32(val as u32);
                } else if val > i32::MIN as i64 && val < 0 {
                    $registers[dest as usize] = Value::Int32(val as i32);
                } else {
                    $registers[dest as usize] = $vm.local_i64(dest as usize, val);
                }
            }
        }
    }};
}

#[macro_export]
macro_rules! set_register {
    ($vm:expr, $registers:expr, $idx:expr, $val:expr) => {{
        match (&get_reg!($registers, $idx), $val) {
            (Value::Value(handle), _) => {
                *($vm.heap.get_value_mut(*handle)) = $val;
            }
            (Value::Float64(handle_to), Value::Float64(handle_from)) => {
                *$vm.get_float_mut(*handle_to) = $vm.get_float(handle_from);
            }
            (Value::Int64(handle_to), Value::Int64(handle_from)) => {
                *$vm.get_int_mut(*handle_to) = $vm.get_int(handle_from);
            }
            (Value::UInt64(handle_to), Value::UInt64(handle_from)) => {
                *$vm.get_uint_mut(*handle_to) = $vm.get_uint(handle_from);
            }
            (_, Value::Float64(handle_from)) => {
                let f = $vm.get_float(handle_from);
                $registers[$idx] = $vm.local_f64($idx, f);
            }
            (_, Value::Int64(handle_from)) => {
                let f = $vm.get_int(handle_from);
                $registers[$idx] = $vm.local_i64($idx, f);
            }
            (_, Value::UInt64(handle_from)) => {
                let f = $vm.get_uint(handle_from);
                $registers[$idx] = $vm.local_u64($idx, f);
            }
            _ => $registers[$idx] = $val,
        }
    }};
}

#[macro_export]
macro_rules! mov_register {
    ($registers:expr, $idx:expr, $val:expr) => {{
        $registers[$idx] = $val;
    }};
}

#[macro_export]
macro_rules! mov_register_num {
    ($vm:expr, $registers:expr, $idx:expr, $val:expr) => {{
        match (&get_reg!($registers, $idx), $val) {
            (Value::Float64(handle_to), Value::Float64(handle_from)) => {
                *$vm.get_float_mut(*handle_to) = $vm.get_float(handle_from);
            }
            (Value::Int64(handle_to), Value::Int64(handle_from)) => {
                *$vm.get_int_mut(*handle_to) = $vm.get_int(handle_from);
            }
            (Value::UInt64(handle_to), Value::UInt64(handle_from)) => {
                *$vm.get_uint_mut(*handle_to) = $vm.get_uint(handle_from);
            }
            (_, Value::Float64(handle_from)) => {
                let f = $vm.get_float(handle_from);
                $registers[$idx] = $vm.local_f64($idx, f);
            }
            (_, Value::Int64(handle_from)) => {
                let f = $vm.get_int(handle_from);
                $registers[$idx] = $vm.local_i64($idx, f);
            }
            (_, Value::UInt64(handle_from)) => {
                let f = $vm.get_uint(handle_from);
                $registers[$idx] = $vm.local_u64($idx, f);
            }
            _ => $registers[$idx] = $val,
        }
    }};
}

#[macro_export]
macro_rules! set_value {
    ($vm:expr, $left:expr, $right:expr) => {{
        match ($left, $right) {
            (Value::Float64(handle_to), Value::Float64(handle_from)) => {
                *$vm.get_float_mut(handle_to) = $vm.get_float(handle_from);
            }
            (Value::Int64(handle_to), Value::Int64(handle_from)) => {
                *$vm.get_int_mut(handle_to) = $vm.get_int(handle_from);
            }
            (Value::UInt64(handle_to), Value::UInt64(handle_from)) => {
                *$vm.get_uint_mut(handle_to) = $vm.get_uint(handle_from);
            }
            (_, Value::Float64(handle_from)) => {
                let f = $vm.get_float(handle_from);
                $left = $vm.alloc_f64(f);
            }
            (_, Value::Int64(handle_from)) => {
                let f = $vm.get_int(handle_from);
                $left = $vm.alloc_i64(f);
            }
            (_, Value::UInt64(handle_from)) => {
                let f = $vm.get_uint(handle_from);
                $left = $vm.alloc_u64(f);
            }
            _ => $left = $right,
        }
    }};
}
