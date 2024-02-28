use std::collections::HashSet;
use std::path::PathBuf;
use std::process::Command;

use shell::glob::{expand_glob, GlobOutput};

/// returns ./slosh/tests/ PathBuf
pub fn get_tests_directory() -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path
}

pub fn get_slosh_exe() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_slosh"))
}

fn glob_to_vec(pat: impl Into<PathBuf>) -> Vec<PathBuf> {
    match expand_glob(pat) {
        GlobOutput::Arg(p) => {
            vec![p]
        }
        GlobOutput::Args(ps) => ps,
    }
}

pub fn get_glob_from_dir(dir: PathBuf, glob: &str) -> Vec<PathBuf> {
    let test_glob = dir.join(glob);
    glob_to_vec(test_glob)
}

pub fn get_globs_from_test_directory(glob: &str) -> Vec<PathBuf> {
    let tests = get_tests_directory();
    get_glob_from_dir(tests, glob)
}

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
    let scripts = get_globs_from_test_directory("*.slosh");
    let fails = get_globs_from_test_directory("*_fail.slosh")
        .into_iter()
        .collect::<HashSet<PathBuf>>();
    let slosh_path = get_slosh_exe().into_os_string();
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
