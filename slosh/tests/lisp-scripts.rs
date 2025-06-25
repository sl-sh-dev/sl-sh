use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
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

/// Recursively find all .slosh files in a directory
fn find_slosh_files_recursive(dir: &Path) -> Vec<PathBuf> {
    let mut results = Vec::new();

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();

            if path.is_dir() {
                // Recursively search subdirectories
                results.extend(find_slosh_files_recursive(&path));
            } else if path.is_file() {
                // Check if it's a .slosh file and not README.md
                if let Some(ext) = path.extension() {
                    if ext == "slosh" {
                        if let Some(file_name) = path.file_name() {
                            let file_name_str = file_name.to_string_lossy();
                            if !file_name_str.eq_ignore_ascii_case("README.md") {
                                results.push(path);
                            }
                        }
                    }
                }
            }
        }
    }

    results
}

/// Get test files based on command line argument or default behavior
fn get_test_files() -> Vec<PathBuf> {
    // Check for command line argument
    let args: Vec<String> = env::args().collect();

    // With harness = false, arguments come directly to us
    // Skip the first argument (the program name) and look for a path
    let target_path = if args.len() > 1 && !args[1].starts_with("--") {
        Some(&args[1])
    } else {
        None
    };

    if let Some(path_str) = target_path {
        let path = PathBuf::from(path_str);

        if path.is_file() && path.extension().map(|ext| ext == "slosh").unwrap_or(false) {
            // Single file specified
            vec![path]
        } else if path.is_dir() {
            // Directory specified - recursively find all .slosh files
            find_slosh_files_recursive(&path)
        } else {
            // Try as a pattern relative to tests directory
            let tests_dir = get_tests_directory();
            let full_path = tests_dir.join(&path);

            if full_path.is_file()
                && full_path
                    .extension()
                    .map(|ext| ext == "slosh")
                    .unwrap_or(false)
            {
                vec![full_path]
            } else if full_path.is_dir() {
                find_slosh_files_recursive(&full_path)
            } else {
                // Fall back to glob pattern
                get_globs_from_test_directory(path_str)
            }
        }
    } else {
        // No argument - run all tests recursively
        find_slosh_files_recursive(&get_tests_directory())
    }
}

/// To run all tests in this executable:
///
/// ```
/// # Run all tests
/// cargo test --package slosh --test lisp-scripts
///
/// # Run just string tests
/// cargo test --package slosh --test lisp-scripts string/
///
/// # Run just math tests
/// cargo test --package slosh --test lisp-scripts math/
///
/// # Run a specific test file
/// cargo test --package slosh --test lisp-scripts string/str-append.slosh
/// ```
///
/// All slosh scripts found will be run by this integration test. If
/// the suffix of the file is _fail.slosh, then the test script is expected to fail. All other
/// slosh scripts should return a 0 exit code or the integration test will fail.
fn lisp_scripts() {
    let scripts = get_test_files();

    if scripts.is_empty() {
        println!("No test files found!");
        return;
    }

    // Build set of expected-to-fail tests
    let fails: HashSet<PathBuf> = scripts
        .iter()
        .filter(|p| {
            p.file_name()
                .and_then(|n| n.to_str())
                .map(|n| n.ends_with("_fail.slosh"))
                .unwrap_or(false)
        })
        .cloned()
        .collect();

    let slosh_path = get_slosh_exe();
    let tests_dir = get_tests_directory();

    println!("Running {} test file(s)...", scripts.len());
    if scripts.len() < 10 {
        for script in &scripts {
            println!("  - {}", script.display());
        }
    }
    println!();

    let mut passed = 0;
    let mut failed = 0;

    for (idx, test_script) in scripts.iter().enumerate() {
        // Calculate relative path for cleaner output
        let display_path = test_script.strip_prefix(&tests_dir).unwrap_or(test_script);

        let should_fail = fails.contains(test_script);

        println!(
            "════════════════════════════════════════════════════════════════════════════════"
        );
        println!(
            "[{}/{}] Testing: {}",
            idx + 1,
            scripts.len(),
            display_path.display()
        );

        if should_fail {
            println!("      Expected: FAIL");
        } else {
            println!("      Expected: PASS");
        }

        let output = Command::new(&slosh_path)
            .arg(test_script)
            .output()
            .unwrap_or_else(|e| {
                panic!(
                    "Failed to execute slosh for {}: {}",
                    test_script.display(),
                    e
                )
            });

        let success = output.status.success();
        let expected = !should_fail;
        let test_passed = success == expected;

        println!(
            "      Status:   {} (exit code: {})",
            if success { "SUCCESS" } else { "FAILED" },
            output.status.code().unwrap_or(-1)
        );

        // Only print output if there is any or if the test failed
        if !output.stdout.is_empty() {
            println!("\n--- stdout ---");
            print!("{}", String::from_utf8_lossy(&output.stdout));
        }

        if !output.stderr.is_empty() {
            println!("\n--- stderr ---");
            print!("{}", String::from_utf8_lossy(&output.stderr));
        }

        if test_passed {
            println!("\n✓ Test PASSED");
            passed += 1;
        } else {
            println!(
                "\n✗ Test FAILED - {}",
                if success {
                    "expected to fail but succeeded"
                } else {
                    "expected to succeed but failed"
                }
            );
            failed += 1;
        }

        // Don't assert here since we handle the exit code in main()
    }

    println!("\n════════════════════════════════════════════════════════════════════════════════");
    println!("Test Summary: {} passed, {} failed", passed, failed);
    println!("════════════════════════════════════════════════════════════════════════════════");

    if failed > 0 {
        std::process::exit(1);
    }
}

fn main() {
    lisp_scripts();
}
