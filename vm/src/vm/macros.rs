#[macro_export]
macro_rules! decode_u16 {
    ($code:expr, $ip:expr) => {{
        let idx1 = $code[*$ip];
        let idx2 = $code[*$ip + 1];
        *$ip += 2;
        ((idx1 as u16) << 8) | (idx2 as u16)
    }};
}

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

#[macro_export]
macro_rules! get_reg_unref {
    ($regs:expr, $idx:expr, $vm:expr) => {{
        $regs[$idx as usize].unref($vm)
    }};
}

#[macro_export]
macro_rules! get_reg {
    ($regs:expr, $idx:expr) => {{
        $regs[$idx as usize]
    }};
}

macro_rules! compare_int {
    ($vm:expr, $chunk:expr, $ip:expr, $registers:expr, $comp_fn:expr,
     $compf_fn:expr, $wide:expr, $move:expr, $not:expr) => {{
        let (dest, reg1, reg2) = decode3!($chunk.code, $ip, $wide);
        let mut val = false;
        for reg in reg1..reg2 {
            let op1 = get_reg_unref!($registers, reg, $vm);
            let op2 = get_reg_unref!($registers, reg + 1, $vm);
            val = if op1.is_int() && op2.is_int() {
                $comp_fn(
                    op1.get_int().map_err(|e| (e, $chunk.clone()))?,
                    op2.get_int().map_err(|e| (e, $chunk.clone()))?,
                )
            } else {
                $compf_fn(
                    op1.get_float().map_err(|e| (e, $chunk.clone()))?,
                    op2.get_float().map_err(|e| (e, $chunk.clone()))?,
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
            GVm::<ENV>::mov_register($registers, dest as usize, val);
        } else {
            $vm.set_register($registers, dest as usize, val);
        }
    }};
}

macro_rules! compare {
    ($vm:expr, $chunk:expr, $ip:expr, $registers:expr, $comp_fn:expr, $wide:expr, $move:expr) => {{
        compare_int!($vm, $chunk, $ip, $registers, $comp_fn, $comp_fn, $wide, $move, false)
    }};
}

macro_rules! binary_math {
    ($vm:expr, $chunk:expr, $ip:expr, $registers:expr, $bin_fn:expr, $wide:expr, $move:expr) => {{
        let (dest, op2, op3) = decode3!($chunk.code, $ip, $wide);
        let op2 = get_reg_unref!($registers, op2, $vm);
        let op3 = get_reg_unref!($registers, op3, $vm);
        let val = if op2.is_int() && op3.is_int() {
            Value::Int($bin_fn(
                op2.get_int().map_err(|e| (e, $chunk.clone()))?,
                op3.get_int().map_err(|e| (e, $chunk.clone()))?,
            ))
        } else {
            Value::Float(F64Wrap($bin_fn(
                op2.get_float().map_err(|e| (e, $chunk.clone()))?,
                op3.get_float().map_err(|e| (e, $chunk.clone()))?,
            )))
        };
        if $move {
            GVm::<ENV>::mov_register($registers, dest as usize, val);
        } else {
            $vm.set_register($registers, dest as usize, val);
        }
    }};
}

macro_rules! div_math {
    ($vm:expr, $chunk:expr, $ip:expr, $registers:expr, $wide:expr, $move:expr) => {{
        let (dest, op2, op3) = decode3!($chunk.code, $ip, $wide);
        let op2 = get_reg_unref!($registers, op2, $vm);
        let op3 = get_reg_unref!($registers, op3, $vm);
        let val = if op2.is_int() && op3.is_int() {
            let op3 = op3.get_int().map_err(|e| (e, $chunk.clone()))?;
            if op3 == 0 {
                return Err((VMError::new_vm("Divide by zero error."), $chunk));
            }
            Value::Int(op2.get_int().map_err(|e| (e, $chunk.clone()))? / op3)
        } else {
            let op3 = op3.get_float().map_err(|e| (e, $chunk.clone()))?;
            if op3 == 0.0 {
                return Err((VMError::new_vm("Divide by zero error."), $chunk));
            }
            Value::Float(F64Wrap(
                op2.get_float().map_err(|e| (e, $chunk.clone()))? / op3,
            ))
        };
        if $move {
            GVm::<ENV>::mov_register($registers, dest as usize, val);
        } else {
            $vm.set_register($registers, dest as usize, val);
        }
    }};
}

/*macro_rules! set_register {
    ($registers:expr, $idx:expr, $val:expr) => {{
        $registers[$idx as usize] = $val;
        /*unsafe {
            let r = $registers.get_unchecked_mut($idx as usize);
            *r = $val;
        }*/
    }};
}*/
