To get macro expansion output for a given test

    `cargo expand --test <name_of_test>`

    e.g.
    `cargo expand --test special_args_pass`

If macro expansion for the test/trybuild/ is needed it is a little more complicated.

Why? well, if you put failing tests in `/tests/` then cargo tests fails so you
can't do that. But `cargo expand --test` relies on finding modules in a test directory
so, you can move a test from trybuild up one directory but move it back b/c
otherwise `cargo test` will fail and CI won't pass.

because `/trybuild`
