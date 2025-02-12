#!/bin/bash

export RUST_LOG="DEBUG"

# clone in the legacy docs
git clone -b gh-pages-legacy-html https://github.com/sl-sh-dev/sl-sh legacy/ || true

# don't want git submodules
rm -rf legacy/.git || true
mkdir slosh-rust-docs || true
mkdir all-rust-docs || true

if [ "${SKIP_DOCS}" != "SKIP_DOCS" ]; then
    echo "Building docs."
    cargo doc --features lisp-test --target-dir all-rust-docs
    cargo doc --no-deps --document-private-items --features lisp-test --target-dir slosh-rust-docs
else
    echo "Skipped doc building"
fi

# make the symlinks work
pushd "mdbook-slosh-eval"
cargo build
popd

cargo build --workspace

export PATH="$PATH:./mdbook-slosh-eval/target/debug"

# make the slosh-forms.csv file
#./list-slosh-forms.sh | tail -n +4 > src/slosh-forms.csv
#./list-slosh-forms.sh | tail -n +4 > slosh-forms.csv
#./list-slosh-doc-exemptions.sh | tail -n +4 > src/slosh-doc-exemptions.csv
#./list-slosh-doc-exemptions.sh | tail -n +4 > slosh-doc-exemptions.csvAk
../target/debug/slosh_test -c "(build-doc \"${PWD}\")"
# from old pr, might actually be what I want?
#
# how come "the new way" doesn't need the lisp-test feature?
#cargo build --features "lisp-test"
#
#../target/debug/slosh -c "(build-doc \"${PWD}\")"
#
##rm -rf $PWD/autogenerated-*.md
#pushd "mdbook-nop"
#cargo build
#popd
#
#cargo build --features "lisp-test"
#
#export PATH="$PATH:./mdbook-nop/target/debug"
#
#../target/debug/slosh -c "(build-doc \"${PWD}\")"

# doing this because im tryingto use the mk-site.slosh script but don't
# see a need to re-write this shell script in slosh at this moment too.
./search-hack-patch.sh
