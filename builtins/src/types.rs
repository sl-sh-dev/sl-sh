use compile_state::state::SloshVm;
use slvm::{Value, VMResult};

/// Type wrapper to use in [`RustProcedure`] and [`RustProcedureRefMut`] declarations for
/// partial application.
pub trait VmToRustType<'a, T, F>
    where
        Self: Sized,
        T: 'a,
        F: FnOnce(&'a mut SloshVm) -> T,
{
    fn apply(&self, fun: F) -> VMResult<Value>;
}
pub trait TryFromSlosh<T> {
    fn try_from_slosh(&self, vm: &mut SloshVm, val: &Value) -> VMResult<T>;
}

pub trait TryIntoSlosh {
    fn try_into_slosh(self, vm: &mut SloshVm) -> VMResult<Value>;
}

impl TryIntoSlosh for String {
    fn try_into_slosh(self, vm: &mut SloshVm) -> VMResult<Value> {
        Ok(vm.alloc_string(self))
    }
}