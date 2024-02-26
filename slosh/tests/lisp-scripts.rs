mod common;

use std::collections::HashSet;
use std::path::PathBuf;
use std::process::Command;

#[test]
/// To run all tests in this executable with printouts.
///
/// ```
/// cargo test --package slosh --test lisp-scripts -- --nocapture
///```
///
/// All slosh scripts in the ./slosh/tests/ directory will be run by this integration test. If
/// the suffix of the file is _fail.slosh, then the test script is expected to fail. All other
/// slosh scripts should return a 0 exit code or the integration test will fail.
fn lisp_scripts() {
    let scripts = common::get_globs_from_test_directory("*.slosh");
    let fails = common::get_globs_from_test_directory("*_fail.slosh")
        .into_iter()
        .collect::<HashSet<PathBuf>>();
    let slosh_path = common::get_slosh_exe().into_os_string();
    let slosh_path = slosh_path
        .to_str()
        .expect("path to CARGO_BIN_EXE_slosh should be a valid utf-8 string");
    for test_script in scripts {
        println!(
            "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
        );
        let ret = !fails.contains(&test_script);
        println!("Script: {test_script:?}");
        let basename = test_script.file_name().unwrap_or(test_script.as_os_str());
        if ret {
            println!("Verify run succeeds: {basename:?}");
        } else {
            println!("Verify run fails: {basename:?}");
        }
        let output = Command::new(slosh_path)
            .arg(test_script)
            .output()
            .expect("Failed to execute command");

        let s = String::from_utf8_lossy(output.stdout.as_slice());
        println!("stdout:\n{}", s);
        println!("status: {}", output.status);
        assert_eq!(output.status.success(), ret);
        println!(
            "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
        );
    }
}
