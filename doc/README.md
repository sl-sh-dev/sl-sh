### Generated documentation

The [documentation site](https://sl-sh-dev.github.io/sl-sh/) is served from the `doc` directory
based on the `doc/mk-site.sh` script referenced in the github action `.github/workflows/gh_pages.yml`
the `doc/book/all.html` file and directory hierarchy it makes can be blown away after each invocation of `doc/mk-site.sh`

# Configure
- edit book.toml
```toml
# section specifying how mdbook run the `mdbook-slosh-eval` mdbook plugin in this repo.
[preprocessor.slosh-eval]
after = ["links"]
# whether or not legacy sl-sh docs, rust docs, and other supplemental html is built.
doc-supplementary = true
# the keyword for the triple backticks in md files that indicate code the vm
# should interpret.
code-block-label = "slosh"

[preprocessor.slosh-eval.doc-forms]
# whether or not to build docs for rust std lib.
std-lib = true
# whether or not to build user docs by loading files referenced in
#`user-doc-files`, or accessible by the load path in `user-doc-load-paths`.
user = true
# list of file to load, this is the default setting if none is provided.
user-doc-files = [
"~/.config/slosh/init.slosh",
]
# list of load paths to use, this is the default setting if non isprovided.
user-doc-load-paths = [
"~/.config/slosh/",
]
```

## Build locally
```bash
# in `./doc` subdirectory
rm -rf ./book/*
# generated site entrypoint at `./book/index.html`
./mk-site.sh
```
## Serving for continuous updating

It is possible to re-generate the html files every time a change is made to the
documentation by setting the env variable `SERVE=TRUE`.
```bash
# in `./doc` subdirectory
rm -rf ./book/*
# generated site entrypoint at `./book/index.html` automatically rebuilds when
# documentation source changes.
# C-c/C-d to exit the process but it will not return as it is invoking a
# process that listens for file changes and regenerates html, does not return.
SERVE=TRUE ./mk-site.sh
```

## Skipping larger possibly unnecessary html files in development
the slosh docs have supplemental material, namely all the rust documentation,
built when `doc-supplementary` is set to true in the book.toml. In iterative
documentation developemnt (i.e. when using mdbook serve) it is advised to
set `doc-supplementary` to false to avoid the overhead of re-processing unnecessary
bulky html.

It is possible to skip building those docs at all when running  the `mk-site.sh`
script by setting the env variable `SKIP_SUPPLEMENTAL`.
```bash
export SKIP_SUPPLEMENTAL=SKIP_SUPPLEMENTAL
```

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
