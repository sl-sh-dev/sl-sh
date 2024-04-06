#!/bin/bash
#
export RUST_LOG="DEBUG"

# clone in the legacy docs
git clone -b gh-pages-legacy-html https://github.com/sl-sh-dev/sl-sh src/legacy/

# don't want git submodules
rm -rf src/legacy/.git
mkdir src/rust-docs

cargo doc --features lisp-test --target-dir src/rust-docs

# make the symlinks work
pushd "mdbook-nop"
cargo build
popd

cargo build --features "lisp-test"

export PATH="$PATH:./mdbook-nop/target/debug"

../target/debug/slosh -c "(build-doc \"${PWD}\")"
