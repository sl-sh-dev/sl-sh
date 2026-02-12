##############################################################
# Hack to get mdbook to allow searching for symbols like `%` #
##############################################################
# Path to the searcher.js file (mdbook 0.5+ uses hashed filenames)
SEARCHER_JS=$(ls ./book/searcher-*.js 2>/dev/null || echo "./book/searcher.js")
if [ ! -f "$SEARCHER_JS" ]; then
    echo "Warning: searcher.js not found, skipping search hack patch"
    exit 0
fi
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
