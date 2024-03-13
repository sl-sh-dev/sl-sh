use bridge_macros::sl_sh_fn;
use bridge_types::VarArgs;
use slvm::{VMResult, Value};

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_option")]
pub fn accept_option(_opt: Option<char>) -> VMResult<()> {
    Ok(())
}

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_mult_option")]
pub fn accept_mult_option(_opt1: Option<char>, _opt2: Option<char>) -> VMResult<()> {
    Ok(())
}
