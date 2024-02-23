use std::path::PathBuf;
use std::process::Command;

// To run all tests in this executable
//
// cargo test --features regex --package slosh --test slosh-docs
//
// This integration test exists to test all the functions that slosh has by default.
// This differs from the unit tests that test the docs in the docs module

// TODO PC ISSUE #111 I believe... an integration test could just run through every
//  slosh file it finds in a directory, run it, and report if any errors occurred?
//  issue is making sure any error in the slosh file actually reports a non zero exit
//  code. Would be a good way to easily add slosh level tests with slosh scripts BUT
//  have theam easily caught in CI as per issue.

// TODO PC slosh -c broken?

#[test]
/// [toggle appropriately](https://stackoverflow.com/questions/48583049/run-additional-tests-by-using-a-feature-flag-to-cargo-test)
#[cfg_attr(not(feature = "regex"), ignore)]
/// look at all these amazing [environment variables](https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-crates)!
/// EXE only works [with integration tests](https://doc.rust-lang.org/rust-by-example/testing/integration_testing.html)
/// To Execute:
///     cargo test --features regex --package slosh --test slosh-docs test_slosh_doc_string_parsing_in_slosh -- --exact
///
fn test_slosh_doc_string_parsing_in_slosh() {
    let test_script = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("run-tests.slosh");
    println!("test_script: {test_script:?}");

    let slosh_path = PathBuf::from(env!("CARGO_BIN_EXE_slosh")).into_os_string();
    let slosh_path = slosh_path.to_str();
    println!("curr_path: {slosh_path:?}");
    println!("slosh_path: {slosh_path:?}");

    let output = Command::new(slosh_path.unwrap())
        .arg(test_script)
        .output()
        .expect("Failed to execute command");

    let s = String::from_utf8_lossy(output.stdout.as_slice());
    println!("status {}", output.status);
    println!("stdout {}", s);

    assert!(
        output.status.success(),
        "Integration test script failed!: \nstdout:\n{}\n\nstderr:\n{}",
        String::from_utf8_lossy(output.stdout.as_slice()),
        String::from_utf8_lossy(output.stderr.as_slice())
    );
}
