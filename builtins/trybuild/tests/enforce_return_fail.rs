use bridge_macros::sl_sh_fn;

pub fn main() {}

/// obligatory doc
#[sl_sh_fn(fn_name = "invalid_return")]
pub fn invalid_return() -> Result<(), ()> {
    Ok(())
}
