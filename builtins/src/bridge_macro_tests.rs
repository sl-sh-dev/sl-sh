#[cfg(test)]
mod tests {
    #[test]
    fn macro_passing_tests() {
        let t = trybuild::TestCases::new();
        t.pass("tests/*.rs");
    }

    #[test]
    fn macro_failing_tests() {
        let t = trybuild::TestCases::new();
        t.compile_fail("trybuild/tests/*fail.rs");
    }
}
