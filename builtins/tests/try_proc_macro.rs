use bridge_macros::sl_sh_fn;
use slvm::VMResult;
pub fn main() {}

/// obligatory doc
#[sl_sh_fn(fn_name = "triy")]
pub fn triy() -> VMResult<()> {
    Ok(())
}
