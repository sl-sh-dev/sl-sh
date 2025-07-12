# Slosh Test Suite Organization

This directory contains the organized test suite for Slosh. Tests are categorized by functionality to make them easy to find, maintain, and extend.


Each directory can house any number of slosh files. Any slosh file in this hierarchy will be run by the test runner or on demand with the command:

```
cargo test --package slosh --test lisp-scripts
```

## Directory Structure

Any direct child directory within the tests directory can run only the slosh files that are it's children if specific tests should be run, e.g.:
```
# Run just string tests
cargo test --package slosh --test lisp-scripts string/

# Run just math tests
cargo test --package slosh --test lisp-scripts math/
```

Files can also be run directly:

```
# Run a specific test file
cargo test --package slosh --test lisp-scripts string/str-append.slosh
```


## Test File Naming Convention

Test files should be named descriptively using kebab-case and end with `.slosh`:
- `basic-arithmetic.slosh`
- `string-concatenation.slosh`
- `hash-table-operations.slosh`

## Running Tests

Tests can be run using the slosh test runner from the project root.
