use crate::SloshVm;
use slvm::{VMError, VMResult, Value};

pub(crate) fn get_args_iter<'vm>(
    env: &'vm SloshVm,
    args: Value,
    name: &str,
) -> VMResult<Box<dyn Iterator<Item = Value> + 'vm>> {
    match args {
        Value::Pair(_) | Value::List(_, _) | Value::Nil => Ok(args.iter(env)),
        _ => Err(VMError::new_compile(format!("{name}, invalid args"))),
    }
}
