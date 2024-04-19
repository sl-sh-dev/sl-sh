pub mod config;

use crate::config::VERSION_STRING;
use bridge_adapters::add_builtin;
use compile_state::state::SloshVm;
use slosh_lib::run;
use slvm::{VMError, VMResult, Value};
use std::process;

fn modify_vm(vm: &mut SloshVm) {
    add_builtin(
        vm,
        "version",
        version,
        "Return the software version string.",
    );
}

fn version(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_compile("version: requires no argument"));
    }
    Ok(vm.alloc_string(VERSION_STRING.to_string()))
}

fn main() {
    let exit_code = run(modify_vm);
    process::exit(exit_code)
}

#[cfg(test)]
mod tests {
    extern crate tempdir;

    use compiler_test_utils::exec;
    use slosh_lib::{set_builtins_shell, set_initial_load_path, ENV};
    use slvm::{from_i56, Value};
    use std::fs::{create_dir_all, File};
    use std::io::Write;
    use std::ops::DerefMut;
    use temp_env;
    use tempdir::TempDir;

    #[test]
    fn test_load_path_no_home() {
        // create home dir
        let tmp_dir = TempDir::new("test_load_path").unwrap();
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
                set_builtins_shell(vm.deref_mut());
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
