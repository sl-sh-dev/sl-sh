#!/bin/bash
#
export RUST_LOG="DEBUG"

# clone in the legacy docs
git clone -b gh-pages-legacy-html https://github.com/sl-sh-dev/sl-sh src/legacy/

# don't want git submodules
rm -rf src/legacy/.git
mkdir src/rust-docs

cargo doc --target-dir src/rust-docs

# make the symlinks work
pushd "mdbook-slosh-eval"
cargo build
popd

cargo build --workspace

export PATH="$PATH:./mdbook-slosh-eval/target/debug"

# make the slosh-forms.csv file
./list-slosh-forms.sh | tail -n +4 > src/slosh-forms.csv
./list-slosh-forms.sh | tail -n +4 > slosh-forms.csv
./list-slosh-doc-exemptions.sh | tail -n +4 > src/slosh-doc-exemptions.csv
./list-slosh-doc-exemptions.sh | tail -n +4 > slosh-doc-exemptions.csv
../target/debug/slosh_test -c "(build-doc \"${PWD}\")"
