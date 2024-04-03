#!/bin/bash

# clone in the legacy docs
git clone -b gh-pages-legacy-html https://github.com/sl-sh-dev/sl-sh src/legacy/

# don't want git submodules
rm -rf src/legacy/.git

cargo build --features "lisp-test"

../target/debug/slosh -c "(build-doc \"${PWD}\")"
