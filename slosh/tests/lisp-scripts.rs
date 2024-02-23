use shell::glob::{expand_glob, GlobOutput};
use std::collections::HashSet;
use std::path::PathBuf;
use std::process::Command;

// To run all tests in this executable with printouts
//
// cargo test --package slosh --test lisp-scripts -- --nocapture
//
// This integration test exists to test all the functions that slosh has by default.
// This differs from the unit tests that test the docs in the docs module
fn glob_to_vec(pat: impl Into<PathBuf>) -> Vec<PathBuf> {
    match expand_glob(pat) {
        GlobOutput::Arg(p) => {
            vec![p]
        }
        GlobOutput::Args(ps) => ps,
    }
}

#[test]
/// ALL slosh scripts in the ./slosh/tests/ directory will be run by this integreation test. If
/// the suffix of the file is _fail.slosh, then the test script is expected to fail. All other
/// slosh scripts should return a 0 exit code or the integration test will fail.
///
/// [toggle appropriately](https://stackoverflow.com/questions/48583049/run-additional-tests-by-using-a-feature-flag-to-cargo-test)
/// look at all these amazing [environment variables](https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-crates)!
/// EXE only works [with integration tests](https://doc.rust-lang.org/rust-by-example/testing/integration_testing.html)
/// To Execute:
///     cargo test --features regex --package slosh --test slosh-docs test_slosh_doc_string_parsing_in_slosh -- --exact
///
fn lisp_scripts() {
    let tests = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
    let test_glob = tests.join("*.slosh");
    let fail_tests = tests.join("*_fail.slosh");
    let scripts = glob_to_vec(test_glob);
    let binding = glob_to_vec(fail_tests);
    let fails = binding.into_iter().collect::<HashSet<PathBuf>>();
    let slosh_path = PathBuf::from(env!("CARGO_BIN_EXE_slosh")).into_os_string();
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
