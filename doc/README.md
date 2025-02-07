### Generated documentation

The [documentation site](https://sl-sh-dev.github.io/sl-sh/) is served from the `doc` directory
based on the `doc/mk-site.sh` script referenced in the github action `.github/workflows/gh_pages.yml`
the `doc/book/all.html` file and directory hierarchy it makes can be blown away after each invocation of `doc/mk-site.sh`

## Build locally

```bash
# in `./doc` subdirectory
rm -rf ./book/*
./mk-site.sh
# generated site entrypoint at `./book/index.html`
```

TODO PC need md files in a specific folder named after a section to be inlined within the section documentation

## Important information

### Adding a new doc
0. Create a md file somewhere in ./doc/src/
1. Make sure the SUMMARY.md file in ./doc/ references the new md file.

### How does this work?

0. Must be compiled in lisp-test mode for appropriate documentation functions to be present (`cargo build --features lisp-test`).
1. To create new documentation files add them in markdown format to the `doc/src/` directory and
   then reference them in `doc/src/SUMMARY.md` when slosh builds the documentation it will create
   an html file in the format specified in the SUMMARY.md table of contents at the provided relative URL.
2. Docs from functions are automatically added to the chapter corresponding to their section.
3. md files in `./src/section-docs/` named after a given section are automatically included in the paragraph
   immediately below the section's header for that chapter when docs are generated.
4. All code blocks marked in html with the `slosh` tag (three backticks indicating code block start
   immediately followed by the string `slosh`) will be evaluated by the slosh preprocessor
   and the return value will be added to the bottom of the code block, e.g.
   some-file.md

```slosh
(def x 42)
(fn () x)
```

will be rewritten in the generated html site as

```
(def x 42)
(fn () x)

=> 42
```

Troubleshooting:

1. Check the slosh `build-docs` function implementation in slosh_test/docs.rs to see how the docs are generated.
2. For more information about the library used to generated the docs see [mdBook](https://rust-lang.github.io/mdBook/index.html)
   crate and related documentation.
3. For more information about the preprocessor used to execute slosh code `mdbook-slosh-eval` see:
    - https://rust-lang.github.io/mdBook/for_developers/preprocessors.html
    - https://github.com/rust-lang/mdBook/blob/master/examples/nop-preprocessor.rs
    - https://github.com/Byron/termbook/blob/8af7230ec7b9d5e72f43214dfa7540f90a2e6da9/lib/pulldown-cmark-to-cmark/examples/stupicat.rs
4. You CANNOT log to stdout during the mdbook-slosh-eval preprocessing phase. Use
   the log macros instead (e.g. log::warn!) as they do not log to stdout directly
   and won't confuse the mdbook build process by writing to stdout. The `mk-site.sh`
   script sets the RUST_LOG environment variable to debug by default.

For more information about how the docs are generated see [mdBook](https://rust-lang.github.io/mdBook/index.html)
crate and related documentation.

### Running a REPL with the test build (access to mdbook features)
```
cargo run --features lisp-test --bin slosh_test
```
