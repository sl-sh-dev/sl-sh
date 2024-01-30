pub mod utils;

// to keep the actual utility file clear of any dependencies that use
// paths that start with crate:: (because test_utils/utils.rs can be
// imported in any crate in the workspace and therefore should not
// have any contextual use statements).
use compile_state::state::{CompileState, SloshVm, SloshVmTrait};
use slvm::{RET, Value};
use std::sync::Arc;
use crate::{compile, Reader, ReadError};
use crate::pass1::pass1;
