use crate::CompileEnvironment;
use slvm::{VMError, VMResult, Value};

pub(crate) fn get_args_iter<'vm>(
    env: &'vm CompileEnvironment,
    args: Value,
    name: &str,
) -> VMResult<Box<dyn Iterator<Item = Value> + 'vm>> {
    match args {
        Value::Pair(_) | Value::List(_, _) | Value::Nil => Ok(args.iter(env.vm())),
        _ => {
            return Err(VMError::new_compile(format!("{}, invalid args", name)));
        }
    }
}
