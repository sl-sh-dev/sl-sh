#!/bin/bash

export RUST_LOG="DEBUG"
echo "Building docs."

if [ "${SKIP_SUPPLEMENTAL}" != "SKIP_SUPPLEMENTAL" ]; then
    # don't want git submodules
    rm -rf legacy/.git || true
    mkdir slosh-rust-docs || true
    mkdir all-rust-docs || true

    cargo doc --features lisp-test --target-dir all-rust-docs
    cargo doc --no-deps --document-private-items --features lisp-test --target-dir slosh-rust-docs

    # clone in the legacy docs
    git clone -b gh-pages-legacy-html https://github.com/sl-sh-dev/sl-sh legacy/ || true
else
    echo "Skipped supplemental doc building"
fi

# make the symlinks work
pushd "mdbook-slosh-eval"
cargo build
popd

cargo build --workspace

export PATH="$PATH:./mdbook-slosh-eval/target/debug"

mdbook build

./search-hack-patch.sh
