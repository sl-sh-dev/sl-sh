#!/bin/bash
#
export RUST_LOG="DEBUG"

# clone in the legacy docs
git clone -b gh-pages-legacy-html https://github.com/sl-sh-dev/sl-sh src/legacy/

# don't want git submodules
rm -rf src/legacy/.git
mkdir src/rust-docs

cargo doc --features lisp-test --target-dir src/all-rust-docs
cargo doc --no-deps --document-private-items --features lisp-test --target-dir src/slosh-rust-docs

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

##############################################################
# Hack to get mdbook to allow searching for symbols like `%` #
##############################################################
# Path to the searcher.js file
SEARCHER_JS="./book/searcher.js"
# Line to find
SEARCH_LINE='        searchindex = elasticlunr.Index.load(config.index);'
# Lines to insert after the found line
INSERT_LINES='\
        // Hack to allow searching for symbols like `%` \
        // Related discussion at https://github.com/weixsong/elasticlunr.js/issues/53 \
        // and https://github.com/rust-lang/mdBook/issues/2393 \
        // where people are trying to get mdbook and elasticlunr.js to handle Chinese and its non-English characters \
        // Removing the trimmer makes sense, and perhaps there is another place to remove it more elegantly \
        searchindex.pipeline.remove(elasticlunr.trimmer); \
        // Perhaps because we are removing the trimmer at the wrong spot \
        // We also have to re-add everything to the index \
        // This might double the memory usage \
        let temp = Object.values(searchindex.documentStore.docs); \
        temp.forEach((d) => searchindex.addDoc(d)); \
        // End of Hack to allow searching for symbols like `%` \
'
# Use awk to find the line and insert the new lines after it
awk -v search="$SEARCH_LINE" -v insert="$INSERT_LINES" '
{
    print
    if ($0 == search) {
        print insert
    }
}' "$SEARCHER_JS" > "${SEARCHER_JS}.tmp" && mv "${SEARCHER_JS}.tmp" "$SEARCHER_JS"
