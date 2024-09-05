# Justfile
# 'just' is a command runner, like a modern 'make'
# Install instructions at https://github.com/casey/just?tab=readme-ov-file#installation
# Usage is as simple as 'just test' to run all the tests we care about.

# The default recipe that runs if you type 'just' with no arguments
# List available commands
default:
    @just --list --unsorted

# For all the recipes in this file, don't echo the commands to the terminal
set quiet

# Run the most recently built slosh executable
slosh:
    @if [ -f ./target/debug/slosh ] && [ -f ./target/release/slosh ]; then \
        if [ ./target/debug/slosh -nt ./target/release/slosh ]; then \
            ./target/debug/slosh; \
        else \
            ./target/release/slosh; \
        fi; \
    elif [ -f ./target/debug/slosh ]; then \
        ./target/debug/slosh; \
    elif [ -f ./target/release/slosh ]; then \
        ./target/release/slosh; \
    else \
        cargo build && ./target/debug/slosh; \
    fi

# Run all the necessary tests: clippy, unit tests, and integration tests
test:
    sh scripts/check-clippy-version.sh
    cargo fmt
    cargo clippy
    cargo test --workspace

# These tests are a subset of 'cargo test --workspace'
# It runs ./slosh_test/tests/slosh-tests.rs which loads /slosh_test/run-tests.slosh which loads up all the globals and executes the Example block of each docstring
# Test docstring examples of slosh globals defined with add_builtin or add_special
test-slosh-globals:
     cargo test --package slosh_test --test slosh-tests run_slosh_tests -- --exact --nocapture

# These tests are a subset of 'cargo test --workspace'
# Test slosh tests defined in /slosh/tests/*.slosh
test-slosh-tests:
    cargo test --package slosh --test lisp-scripts -- --nocapture

# Use cspell to spellcheck the codebase
spellcheck:
    npx cspell .
