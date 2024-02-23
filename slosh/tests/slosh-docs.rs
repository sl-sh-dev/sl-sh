use std::path::PathBuf;
use std::process::Command;

// To run all tests in this executable
//
// cargo test --features regex --package slosh --test slosh-docs
//
// This integration test exists to test all the functions that slosh has by default.
// This differs from the unit tests that test the docs in the docs module

// TODO PC slosh -c broken?

#[test]
/// [toggle appropriately](https://stackoverflow.com/questions/48583049/run-additional-tests-by-using-a-feature-flag-to-cargo-test)
#[cfg_attr(not(feature = "lisp-test"), ignore)]
/// look at all these amazing [environment variables](https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-crates)!
/// EXE only works [with integration tests](https://doc.rust-lang.org/rust-by-example/testing/integration_testing.html)
/// To Execute:
///     cargo test --features lisp-test --package slosh --test slosh-docs test_slosh_doc_string_parsing_in_slosh -- --exact
///
fn test_slosh_doc_string_parsing_in_slosh() {
    let test_script = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("run-tests.slosh");
    println!("Slosh doc test script: {test_script:?}");

    let slosh_path = PathBuf::from(env!("CARGO_BIN_EXE_slosh")).into_os_string();
    let slosh_path = slosh_path.to_str();
    let output = Command::new(slosh_path.unwrap())
        .arg(test_script)
        .output()
        .expect("Failed to execute command");

    let s = String::from_utf8_lossy(output.stdout.as_slice());
    println!("stdout:\n{}", s);
    println!("status: {}", output.status);

    assert!(
        output.status.success(),
        "Integration test script failed!: stderr:\n{}",
        String::from_utf8_lossy(output.stderr.as_slice())
    );
}
