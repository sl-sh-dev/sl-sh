use bridge_adapters::add_builtin;
use compile_state::state::SloshVm;
use sl_compiler::load_eval::{run_reader, CORE_LISP_NAME, TEST_LISP_NAME};
use sl_compiler::Reader;
use slosh_lib::{load_builtins_lisp_less_sloshrc, run};
use slosh_test_lib::docs;
use slvm::{VMError, VMResult, Value};
use std::process;

pub const VERSION_STRING: &str = env!("VERSION_STRING");

fn version(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_compile("version: requires no argument"));
    }
    Ok(vm.alloc_string(VERSION_STRING.to_string()))
}

fn modify_vm(vm: &mut SloshVm) {
    docs::add_builtins(vm);

    add_builtin(
        vm,
        "version",
        version,
        "Return the software version string.",
    );

    let code = format!(
        "(do {})",
        [CORE_LISP_NAME, TEST_LISP_NAME]
            .iter()
            .map(|x| format!("(load \"{x}\")"))
            .collect::<Vec<String>>()
            .join(" ")
    );
    let mut reader = Reader::from_string(code, vm, "", 1, 0);
    _ = run_reader(&mut reader);
    _ = load_builtins_lisp_less_sloshrc(vm);
}

fn main() {
    let exit_code = run(modify_vm);
    process::exit(exit_code)
}

#[cfg(test)]
mod tests {
    use compiler_test_utils::exec;
    use slosh_lib::{ENV, set_builtins_and_shell_builtins, set_initial_load_path};
    use slvm::{Value, from_i56};
    use std::fs::{File, create_dir_all};
    use std::io::Write;
    use std::ops::DerefMut;
    use temp_env;
    use tempfile::TempDir;

    #[test]
    fn test_load_path_no_home() {
        // create home dir
        let tmp_dir = TempDir::with_prefix("test_load_path").unwrap();
        let home_dir = tmp_dir.path().to_str();
        let home_path = home_dir.unwrap().to_string();

        let tmp_0 = tmp_dir.path().join("tmp_0");
        let tmp_1 = tmp_dir.path().join("tmp_1");
        {
            // create a dir with an add fcn that adds 1 in  add.slosh
            create_dir_all(tmp_0.clone()).unwrap();
            let file_0 = tmp_0.as_path().join("add.slosh");
            let mut file_0 = File::create(file_0).unwrap();
            writeln!(file_0, "(def add (fn (x) (+ 1 x)))").unwrap();
            File::flush(&mut file_0).unwrap();

            // create a dir with an add fcn that adds 2 in add.slosh
            create_dir_all(tmp_1.clone()).unwrap();
            let file_1 = tmp_1.as_path().join("add.slosh");
            let mut file_1 = File::create(file_1).unwrap();
            writeln!(file_1, "(def add (fn (x) (+ 2 x)))").unwrap();
            File::flush(&mut file_1).unwrap();
        }

        let v = temp_env::with_var("HOME", home_dir, || {
            ENV.with(|env| {
                let mut vm = env.borrow_mut();
                set_builtins_and_shell_builtins(vm.deref_mut());
                set_initial_load_path(
                    vm.deref_mut(),
                    vec![
                        &home_path,
                        tmp_0.to_str().unwrap().as_ref(),
                        tmp_1.to_str().unwrap().as_ref(),
                    ],
                );
                _ = exec(vm.deref_mut(), "(load \"add.slosh\")");
                let v = exec(vm.deref_mut(), "(add 1)");
                match v {
                    Value::Int(i) => from_i56(&i),
                    _ => {
                        panic!("Value should be an integer");
                    }
                }
            })
        });
        assert_eq!(v, 2i64);
    }
}
