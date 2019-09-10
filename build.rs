use std::env::consts::{ARCH, OS};
use std::process::Command;

use chrono::prelude::Utc;

#[cfg(debug_assertions)]
const BUILD_TYPE: &'static str = "debug";
#[cfg(not(debug_assertions))]
const BUILD_TYPE: &'static str = "release";

fn main() {
    let version_string = format!(
        "{} {} ({}:{}{}, {} build, {} [{}], {} UTC)",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION"),
        get_branch_name(),
        get_commit_hash(),
        if is_working_tree_clean() { "" } else { "+" },
        BUILD_TYPE,
        OS,
        ARCH,
        Utc::now().format("%b %d %Y, %T")
    );

    println!("cargo:rustc-env=VERSION_STRING={}", version_string);
}

fn get_commit_hash() -> String {
    let output = Command::new("git")
        .arg("log")
        .arg("-1")
        .arg("--pretty=format:%h") // Abbreviated commit hash
        // .arg("--pretty=format:%H") // Full commit hash
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .output()
        .unwrap();

    assert!(output.status.success());

    String::from_utf8_lossy(&output.stdout).to_string()
}

fn get_branch_name() -> String {
    let output = Command::new("git")
        .arg("rev-parse")
        .arg("--abbrev-ref")
        .arg("HEAD")
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .output()
        .unwrap();

    assert!(output.status.success());

    String::from_utf8_lossy(&output.stdout)
        .trim_end()
        .to_string()
}

fn is_working_tree_clean() -> bool {
    let status = Command::new("git")
        .arg("diff")
        .arg("--quiet")
        .arg("--exit-code")
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .status()
        .unwrap();

    status.code().unwrap() == 0
}
