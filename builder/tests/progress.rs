
#[test]
fn tests() {
    let test_cases = trybuild::TestCases::new();

    test_cases.pass("tests/01-macro-exists.rs");
}
