use shell::glob::{expand_glob, GlobOutput};
use std::path::PathBuf;

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
