pub mod reader;

use crate::load_eval::add_load_builtins;
pub use crate::reader::*;
use builtins::add_misc_builtins;
use builtins::collections::setup_collection_builtins;
use builtins::conversions::add_conv_builtins;
use builtins::fs_meta::add_fs_meta_builtins;
use builtins::fs_temp::add_fs_temp_builtins;
use builtins::io::add_io_builtins;
use builtins::print::add_print_builtins;
use builtins::string::add_str_builtins;

pub use compile_state::state::*;
use slvm::{INT_BITS, INT_MAX, INT_MIN};

pub mod config;
pub use crate::config::*;

pub mod backquote;
pub use crate::backquote::*;

pub mod compile;
pub mod pass1;

pub mod load_eval;
#[cfg(test)]
pub mod test_utils;

pub use crate::compile::*;

pub fn set_builtins(env: &mut SloshVm) {
    setup_collection_builtins(env);
    add_print_builtins(env);
    add_load_builtins(env);
    add_str_builtins(env);
    add_misc_builtins(env);
    add_io_builtins(env);
    add_fs_meta_builtins(env);
    add_fs_temp_builtins(env);
    add_conv_builtins(env);

    env.set_named_global("*int-bits*", (INT_BITS as i64).into());
    env.set_named_global("*int-max*", INT_MAX.into());
    env.set_named_global("*int-min*", INT_MIN.into());
}

pub fn new_slosh_vm_with_builtins() -> SloshVm {
    let mut env = new_slosh_vm();
    set_builtins(&mut env);
    env
}
