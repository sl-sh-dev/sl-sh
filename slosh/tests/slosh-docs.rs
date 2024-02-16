use std::path::PathBuf;
use std::process::Command;

// To run all tests
// cargo test --features regex --package slosh --test slosh-docs

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

    assert!(output.status.success());
}
