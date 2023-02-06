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
macro_rules! decode_i24 {
    ($code:expr, $ip:expr) => {{
        let idx1 = $code[*$ip];
        let idx2 = $code[*$ip + 1];
        let idx3 = $code[*$ip + 2];
        *$ip += 3;
        let negative = (idx1 & 0x80) == 0x80;
        let num = ((((idx1 & 0x7f) as u32) << 16) | ((idx2 as u32) << 8) | (idx3 as u32)) as i32;
        if negative {
            -num as isize
        } else {
            num as isize
        }
    }};
}

#[cfg(feature = "nohelmet")]
#[macro_export]
macro_rules! decode_i24 {
    ($code:expr, $ip:expr) => {{
        unsafe {
            let idx1 = *$code.add(*$ip);
            let idx2 = *$code.add(*$ip + 1);
            let idx3 = *$code.add(*$ip + 2);
            *$ip += 3;
            let negative = (idx1 & 0x80) == 0x80;
            let num =
                ((((idx1 & 0x7f) as u32) << 16) | ((idx2 as u32) << 8) | (idx3 as u32)) as i32;
            if negative {
                -num as isize
            } else {
                num as isize
            }
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
        let val = if matches!(op1, Value::Float64(_)) || matches!(op2, Value::Float64(_)) {
            if let Value::Float64(handle) = op1 {
                *$vm.get_float_mut(handle) = $bin_fn(
                    get_float!($vm, op1).map_err(|e| (e, $chunk.clone()))?,
                    get_float!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
                );
                op1
            } else {
                $vm.alloc_f64($bin_fn(
                    get_float!($vm, op1).map_err(|e| (e, $chunk.clone()))?,
                    get_float!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
                ))
            }
        } else {
            // XXX TODO- overflow
            Value::Int32($bin_fn(
                get_int!($vm, op1).map_err(|e| (e, $chunk.clone()))?,
                get_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
            ) as i32)
        };
        mov_register!($registers, dest as usize, val);
    }};
}

macro_rules! div_math {
    ($vm:expr, $chunk:expr, $code:expr, $ip:expr, $registers:expr, $wide:expr) => {{
        let (dest, op2) = decode2!($code, $ip, $wide);
        let op1 = get_reg!($registers, dest);
        let op2 = get_reg!($registers, op2);
        let val = if matches!(op1, Value::Float64(_)) || matches!(op2, Value::Float64(_)) {
            let op2 = get_float!($vm, op2).map_err(|e| (e, $chunk.clone()))?;
            if op2 == 0.0 {
                return Err((VMError::new_vm("Divide by zero error."), $chunk));
            }
            if let Value::Float64(handle) = op1 {
                *$vm.get_float_mut(handle) =
                    get_float!($vm, op1).map_err(|e| (e, $chunk.clone()))? / op2;
                op1
            } else {
                $vm.alloc_f64(get_float!($vm, op1).map_err(|e| (e, $chunk.clone()))? / op2)
            }
        } else {
            // XXX TODO- overflow
            let op2 = get_int!($vm, op2).map_err(|e| (e, $chunk.clone()))? as i32;
            if op2 == 0 {
                return Err((VMError::new_vm("Divide by zero error."), $chunk));
            }
            Value::Int32(get_int!($vm, op1).map_err(|e| (e, $chunk.clone()))? as i32 / op2)
        };
        mov_register!($registers, dest as usize, val);
    }};
}

/*#[macro_export]
macro_rules! set_register {
    ($vm:expr, $registers:expr, $idx:expr, $val:expr) => {{
        match &get_reg!($registers, $idx) {
            Value::Value(handle) => {
                *($vm.heap.get_value_mut(*handle)) = $val;
            }
            _ => $registers[$idx] = $val,
        }
    }};
}*/

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
            (_, Value::Float64(handle_from)) => {
                let f = $vm.get_float(handle_from);
                $registers[$idx] = $vm.alloc_f64(f);
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
            (_, Value::Float64(handle_from)) => {
                let f = $vm.get_float(handle_from);
                $registers[$idx] = $vm.alloc_f64(f);
            }
            _ => $registers[$idx] = $val,
        }
        /*match $val {
            Value::Float64(handle_from) => {
                let f = $vm.get_float(handle_from);
                $registers[$idx] = $vm.alloc_f64(f);
            }
            _ => $registers[$idx] = $val,
        }*/
    }};
}

#[macro_export]
macro_rules! set_value {
    ($vm:expr, $left:expr, $right:expr) => {{
        match ($left, $right) {
            (Value::Float64(handle_to), Value::Float64(handle_from)) => {
                *$vm.get_float_mut(handle_to) = $vm.get_float(handle_from);
            }
            (_, Value::Float64(handle_from)) => {
                let f = $vm.get_float(handle_from);
                $left = $vm.alloc_f64(f);
            }
            _ => $left = $right,
        }
    }};
}
