#[macro_export]
macro_rules! inc_ip {
    ($code_ip:expr) => {{
        #[allow(clippy::macro_metavars_in_unsafe)]
        unsafe {
            // SAFETY: `$code_ip` must be a valid pointer to at least 1 byte of memory.
            let r = *$code_ip;
            $code_ip = $code_ip.offset(1);
            r
        }
    }};
}

#[macro_export]
macro_rules! get_code {
    ($chunk:expr) => {{
        $chunk.code.as_ptr()
    }};
}

#[macro_export]
macro_rules! get_code_at {
    ($chunk:expr, $idx:expr) => {{
        let code = $chunk.code.as_ptr();
        let idx: isize = $idx as isize;
        unsafe { code.offset(idx) }
    }};
}

#[macro_export]
macro_rules! decode_u8 {
    ($code:expr) => {{
        $crate::inc_ip!($code)
    }};
}

#[macro_export]
macro_rules! decode_u16 {
    ($code:expr) => {{
        #[allow(clippy::macro_metavars_in_unsafe)]
        unsafe {
            // SAFETY: `$code` must be a valid pointer to at least 2 bytes of memory.
            let idx1 = *$code;
            let idx2 = *$code.offset(1);
            $code = $code.offset(2);
            ((idx1 as u16) << 8) | (idx2 as u16)
        }
    }};
}

#[macro_export]
macro_rules! decode_u32 {
    ($code:expr) => {{
        #[allow(clippy::macro_metavars_in_unsafe)]
        unsafe {
            // SAFETY: `$code` must be a valid pointer to at least 4 bytes of memory.
            let idx1 = *$code;
            let idx2 = *$code.offset(1);
            let idx3 = *$code.offset(2);
            let idx4 = *$code.offset(3);
            $code = $code.offset(4);
            ((idx1 as u32) << 24) | ((idx2 as u32) << 16) | ((idx3 as u32) << 8) | (idx4 as u32)
        }
    }};
}

#[macro_export]
macro_rules! decode1 {
    ($code:expr, $wide:expr) => {{
        if $wide {
            decode_u16!($code)
        } else {
            $crate::inc_ip!($code) as u16
        }
    }};
}

#[macro_export]
macro_rules! decode2 {
    ($code:expr, $wide:expr) => {{
        if $wide {
            (decode_u16!($code), decode_u16!($code))
        } else {
            //(crate::inc_ip!($code) as u16, crate::inc_ip!($code) as u16)
            #[allow(clippy::macro_metavars_in_unsafe)]
            unsafe {
                // SAFETY: $code must be a valid pointer to at least 2 bytes of memory.
                let r = (*$code as u16, *$code.offset(1) as u16);
                $code = $code.offset(2);
                r
            }
        }
    }};
}

#[macro_export]
macro_rules! decode3 {
    ($code:expr, $wide:expr) => {{
        if $wide {
            (decode_u16!($code), decode_u16!($code), decode_u16!($code))
        } else {
            #[allow(clippy::macro_metavars_in_unsafe)]
            unsafe {
                // SAFETY: $code must be a valid pointer to at least 3 bytes of memory.
                let r = (
                    *$code as u16,
                    *$code.offset(1) as u16,
                    *$code.offset(2) as u16,
                );
                $code = $code.offset(3);
                r
            }
        }
    }};
}

macro_rules! compare_numeric {
    ($vm:expr, $chunk:expr, $code:expr, $comp_fn:expr, $wide:expr) => {{
        let (dest, reg1, reg2) = decode3!($code, $wide);
        let mut val = false;
        for reg in reg1..reg2 {
            let op1 = $vm.register_unref(reg as usize);
            let op2 = $vm.register_unref(reg as usize + 1);
            val = if matches!(op1, $crate::Value::Float(_))
                || matches!(op2, $crate::Value::Float(_))
            {
                // Both operands are floats.
                // The macro expansion trips this.
                #[allow(clippy::redundant_closure_call)]
                $comp_fn(
                    get_primitive_float!($vm, op1).map_err(|e| (e, $chunk.clone()))?,
                    get_primitive_float!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
                )
            } else {
                // Both operands are treated as integers.
                // The macro expansion trips this.
                #[allow(clippy::redundant_closure_call)]
                $comp_fn(
                    get_primitive_int!($vm, op1).map_err(|e| (e, $chunk.clone()))?,
                    get_primitive_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
                )
            };
            if !val {
                break;
            }
        }
        let val = if val {
            $crate::Value::True
        } else {
            $crate::Value::False
        };
        *$vm.register_mut(dest as usize) = val;
    }};
}

/// Convert a Value into an i64 integer primitive if possible
macro_rules! get_primitive_int {
    ($vm:expr, $val:expr) => {{
        match $val {
            $crate::Value::Byte(b) => Ok(b as i64),
            $crate::Value::Int(i) => Ok($crate::from_i56(&i)),
            _ => Err($crate::VMError::new_value(format!(
                "Not an integer: {}",
                $val.display_value($vm)
            ))),
        }
    }};
}

/// Convert a numeric Value into an f64 float primitive
macro_rules! get_primitive_float {
    ($vm:expr, $val:expr) => {{
        match $val {
            $crate::Value::Byte(b) => Ok(b as f64),
            $crate::Value::Int(i) => Ok(crate::from_i56(&i) as f64),
            $crate::Value::Float(f) => Ok(f64::from(f)),
            _ => Err($crate::VMError::new_value(format!(
                "Not a float: {:?}",
                $val
            ))),
        }
    }};
}

macro_rules! binary_math {
    ($vm:expr, $chunk:expr, $code:expr, $bin_fn:expr, $wide:expr) => {{
        let (dest, op2) = decode2!($code, $wide);
        let op1 = $vm.register(dest as usize);
        let op2 = $vm.register(op2 as usize);
        match (op1, op2) {
            ($crate::Value::Float(op1_f), $crate::Value::Float(op2_f)) => {
                *$vm.register_mut(dest as usize) =
                    $bin_fn(f64::from(op1_f), f64::from(op2_f)).into();
            }
            ($crate::Value::Float(op1_f), _) => {
                *$vm.register_mut(dest as usize) = $bin_fn(
                    f64::from(op1_f),
                    get_primitive_float!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
                )
                .into();
            }
            (_, $crate::Value::Float(op2_f)) => {
                *$vm.register_mut(dest as usize) = $bin_fn(
                    get_primitive_float!($vm, op1).map_err(|e| (e, $chunk.clone()))?,
                    f64::from(op2_f),
                )
                .into();
            }
            (_, _) => {
                *$vm.register_mut(dest as usize) = $bin_fn(
                    get_primitive_int!($vm, op1).map_err(|e| (e, $chunk.clone()))?,
                    get_primitive_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?,
                )
                .into();
            }
        }
    }};
}

macro_rules! div_math {
    ($vm:expr, $chunk:expr, $code:expr, $wide:expr) => {{
        let (dest, op2) = decode2!($code, $wide);
        let op1 = $vm.register(dest as usize);
        let op2 = $vm.register(op2 as usize);
        match (op1, op2) {
            ($crate::Value::Float(op1_f), $crate::Value::Float(op2_f)) => {
                let op1 = f64::from(op1_f);
                let op2 = f64::from(op2_f);
                if op2 == 0.0 {
                    return Err(($crate::VMError::new_vm("Divide by zero error."), $chunk));
                }
                *$vm.register_mut(dest as usize) = (op1 / op2).into();
            }
            ($crate::Value::Float(op1_f), _) => {
                let op1 = f64::from(op1_f);
                let op2 = get_primitive_float!($vm, op2).map_err(|e| (e, $chunk.clone()))? as f64;
                if op2 == 0.0 {
                    return Err(($crate::VMError::new_vm("Divide by zero error."), $chunk));
                }
                *$vm.register_mut(dest as usize) = (op1 / op2).into();
            }
            (_, $crate::Value::Float(op2_f)) => {
                let op1 = get_primitive_float!($vm, op1).map_err(|e| (e, $chunk.clone()))? as f64;
                let op2 = f64::from(op2_f);
                if op2 == 0.0 {
                    return Err(($crate::VMError::new_vm("Divide by zero error."), $chunk));
                }
                *$vm.register_mut(dest as usize) = (op1 / op2).into();
            }
            (_, _) => {
                let op1 = get_primitive_int!($vm, op1).map_err(|e| (e, $chunk.clone()))?;
                let op2 = get_primitive_int!($vm, op2).map_err(|e| (e, $chunk.clone()))?;
                if op2 == 0 {
                    return Err(($crate::VMError::new_vm("Divide by zero error."), $chunk));
                }
                let val = op1 / op2;
                *$vm.register_mut(dest as usize) = val.into();
            }
        }
    }};
}

#[macro_export]
macro_rules! set_register {
    ($vm:expr, $idx:expr, $val:expr) => {{
        match (&$vm.register($idx as usize), $val) {
            ($crate::Value::Value(_), $crate::Value::Value(_)) => {
                panic!("Do not set recursive Values...")
            }
            ($crate::Value::Value(handle), _) => {
                *($vm.heap_mut().get_value_mut(*handle)) = $val;
            }
            _ => *$vm.register_mut($idx) = $val,
        }
    }};
}

#[macro_export]
macro_rules! mov_register {
    ($vm:expr, $idx:expr, $val:expr) => {{
        *$vm.register_mut($idx) = $val;
    }};
}
