use bridge_adapters::add_builtin;
use bridge_adapters::lisp_adapters::SlFrom;
use bridge_adapters::{BridgeError, BridgeResult};
use compile_state::state::{SloshVm, SloshVmTrait};
use regex::{Regex, RegexBuilder};
use slvm::vm_hashmap::VMHashMap;
use slvm::{Interned, SLOSH_NIL, VMError, VMErrorObj, VMResult, Value};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::LazyLock;

pub const GLOBAL_NAMESPACE: &str = "root";
pub const USAGE: &str = "usage";
pub const DESCRIPTION: &str = "description";
pub const SECTION: &str = "section";
pub const EXAMPLE: &str = "example";

pub static DOC_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    RegexBuilder::new(
        r#"(\s*?Usage:(.+?)$\n\n|\s*?)(\S{1}.*)\n\n\s*Section:(.+?)$(\n\n\s*Example:\n(.*)|\s*)"#,
    )
    .multi_line(true)
    .dot_matches_new_line(true)
    .crlf(true)
    .build()
    .unwrap()
});

pub static EXEMPTIONS: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
    let mut exemption_set = HashSet::new();
    exemption_set.insert("version");
    exemption_set.insert("this-fn");
    exemption_set.insert("identical?");
    exemption_set.insert("type");
    exemption_set.insert("call/cc");
    exemption_set.insert("defer");
    exemption_set.insert("while");
    exemption_set.insert("doc-string");
    exemption_set.insert("get");
    exemption_set.insert("return");
    exemption_set.insert("*int-min*");
    exemption_set.insert("*int-max*");
    exemption_set.insert("prn");
    exemption_set.insert("pr");
    exemption_set.insert("fprn");
    exemption_set.insert("fpr");
    exemption_set.insert("eprn");
    exemption_set.insert("epr");
    exemption_set.insert("dump-regs");
    exemption_set.insert("dasm");
    exemption_set.insert("*int-bits*");
    exemption_set.insert("*stdout*");
    exemption_set.insert("*prn*");

    // slosh specific colors
    exemption_set.insert("get-rgb-seq");
    exemption_set.insert("tok-slsh-form-color");
    exemption_set.insert("tok-slsh-fcn-color");
    exemption_set.insert("tok-default-color");
    exemption_set.insert("tok-sys-command-color");
    exemption_set.insert("tok-sys-alias-color");
    exemption_set.insert("tok-string-color");
    exemption_set.insert("tok-invalid-color");

    exemption_set.insert("*fg-default*");
    exemption_set.insert("*fg-black*");
    exemption_set.insert("*fg-red*");
    exemption_set.insert("*fg-green*");
    exemption_set.insert("*fg-yellow*");
    exemption_set.insert("*fg-blue*");
    exemption_set.insert("*fg-magenta*");
    exemption_set.insert("*fg-cyan*");
    exemption_set.insert("*fg-white*");

    exemption_set.insert("*bg-default*");
    exemption_set.insert("*bg-black*");
    exemption_set.insert("*bg-red*");
    exemption_set.insert("*bg-green*");
    exemption_set.insert("*bg-yellow*");
    exemption_set.insert("*bg-blue*");
    exemption_set.insert("*bg-magenta*");
    exemption_set.insert("*bg-cyan*");
    exemption_set.insert("*bg-white*");

    // default init.slosh
    exemption_set.insert("*ns*");
    exemption_set.insert("__prompt");
    exemption_set.insert("__line_handler");
    exemption_set.insert("get-pwd");
    exemption_set.insert("set-prompt-tail");
    exemption_set.insert("parse-git-branch");

    // in runtime
    exemption_set.insert("#<remember-me>");

    exemption_set
});

/// Build a usage string for a symbol from its lambda/closure debug args.
/// This is a local version to avoid circular dependency on slosh_lib.
fn usage(vm: &mut SloshVm, slot: u32, sym: &Value) -> String {
    let name = sym.display_value(vm);
    let mut doc_str = String::new();
    let sym = vm.get_global(slot);
    let args = match sym {
        Value::Lambda(h) => {
            let l = vm.get_lambda(h);
            l.dbg_args.clone()
        }
        Value::Closure(h) => {
            let (l, _h) = vm.get_closure(h);
            l.dbg_args.clone()
        }
        _ => {
            return doc_str;
        }
    };
    if let Some(args) = args {
        doc_str.push('(');
        doc_str.push_str(&name);
        for a in args {
            let arg = vm.get_interned(a);
            doc_str.push(' ');
            doc_str.push_str(arg);
        }
        doc_str.push(')');
    }
    doc_str
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Namespace {
    Global,
    Other(Interned),
}

impl Namespace {
    pub fn display(&self, vm: &mut SloshVm) -> String {
        match self {
            Namespace::Global => GLOBAL_NAMESPACE.to_string(),
            Namespace::Other(s) => {
                let s = vm.get_interned(*s);
                s.to_string()
            }
        }
    }

    fn get_doc(
        &self,
        interned: &Interned,
        docs: &mut Vec<SloshDoc>,
        vm: &mut SloshVm,
        require_proper_format: bool,
    ) -> DocResult<()> {
        let slosh_doc = SloshDoc::new(*interned, vm, self.clone());
        match slosh_doc {
            Ok(slosh_doc) => {
                docs.push(slosh_doc);
            }
            Err(e) => match e {
                _ if !require_proper_format => {
                    let incomplete_doc = SloshDoc::new_incomplete(*interned, vm, self.clone())?;
                    docs.push(incomplete_doc);
                }
                _ => {
                    return Err(e);
                }
            },
        }
        Ok(())
    }

    pub fn add_docs(
        &self,
        docs: &mut Vec<SloshDoc>,
        vm: &mut SloshVm,
        require_proper_format: bool,
    ) -> DocResult<()> {
        match self {
            Namespace::Global => {
                for g in vm.globals().clone().keys() {
                    self.get_doc(g, docs, vm, require_proper_format)?;
                }
            }
            Namespace::Other(i) => {
                let value = crate::retrieve_in_namespace(vm, i);
                for v in value {
                    if let Value::Symbol(sym) = v {
                        self.get_doc(&sym, docs, vm, require_proper_format)?;
                    }
                }
            }
        }
        docs.sort();
        docs.dedup();
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct DocStringSection {
    pub usage: Option<String>,
    pub description: String,
    pub section: String,
    pub example: Option<String>,
}

impl Display for DocStringSection {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let usage = self
            .usage
            .clone()
            .map(|usage| format!("Usage:{}\n\n", usage))
            .unwrap_or_default();
        let example = self
            .example
            .clone()
            .map(|example| format!("Example:\n{}", example))
            .unwrap_or_default();
        write!(
            f,
            "{usage}\n{description}\n\nSection: {section}\n\n{example}",
            usage = usage,
            description = self.description,
            section = self.section,
            example = example,
        )
    }
}

impl DocStringSection {
    pub fn from_symbol(slot: u32, sym: Value, vm: &mut SloshVm) -> DocResult<DocStringSection> {
        let sym_str = sym.display_value(vm);
        let raw_doc_string = Self::raw_docstring(slot, vm);
        let backup_usage = usage(vm, slot, &sym);
        Self::parse_doc_string(Cow::Owned(sym_str), raw_doc_string, backup_usage)
    }

    pub fn raw_docstring(slot: u32, vm: &mut SloshVm) -> String {
        let docstring_key = vm.intern_static("doc-string");
        vm.get_global_property(slot, docstring_key)
            .and_then(|x| match x {
                Value::String(h) => Some(vm.get_string(h).to_string()),
                Value::StringConst(i) => Some(vm.get_interned(i).to_string()),
                _ => None,
            })
            // return default empty string and have parse_doc_string handle error if no doc provided.
            .unwrap_or_default()
    }

    /// Given the rules for parsing slosh docstrings, parse one! See [`static@DOC_REGEX`]
    /// for the specification.
    pub fn parse_doc_string(
        symbol: Cow<'_, str>,
        raw_doc_string: String,
        backup_usage: String,
    ) -> DocResult<DocStringSection> {
        let cap =
            DOC_REGEX
                .captures(raw_doc_string.as_str())
                .ok_or_else(|| DocError::NoDocString {
                    symbol: symbol.to_string(),
                });
        if EXEMPTIONS.contains(symbol.as_ref()) && cap.is_err() {
            let usage = Some("unknown".to_string());
            let description = "unknown".to_string();
            let section = "undocumented".to_string();
            let example = None;
            return Ok(DocStringSection {
                usage,
                description,
                section,
                example,
            });
        }
        let cap = cap?;
        let mut usage = cap.get(2).map(|x| x.as_str().trim().to_string());
        if usage.is_none() && !backup_usage.trim().is_empty() {
            usage = Some(backup_usage);
        }
        let description = cap
            .get(3)
            .ok_or_else(|| DocError::DocStringMissingSection {
                symbol: symbol.to_string(),
                section: "Description".to_string(),
            })
            .map(|x| x.as_str().to_string())?;
        let section = cap
            .get(4)
            .ok_or_else(|| DocError::DocStringMissingSection {
                symbol: symbol.to_string(),
                section: "Section".to_string(),
            })
            .map(|x| x.as_str().trim().to_string())?;
        let example = cap.get(6).map(|x| x.as_str().trim().to_string());
        if EXEMPTIONS.contains(symbol.as_ref()) {
            Err(DocError::RemoveExemption {
                symbol: symbol.to_string(),
            })
        } else {
            Ok(DocStringSection {
                usage,
                description,
                section,
                example,
            })
        }
    }

    /// Just write everything that was in the doc section to the description section.
    pub fn new_incomplete(slot: u32, sym: &Value, vm: &mut SloshVm) -> Self {
        let description = Self::raw_docstring(slot, vm);
        let usage_str = Some(usage(vm, slot, sym));
        DocStringSection {
            usage: usage_str,
            description,
            section: SLOSH_NIL.to_string(),
            example: None,
        }
    }

    fn into_styled_output(mut self, query: &DocSearchQuery, style: &StyleOptions) -> Self {
        (self.usage, self.description, self.section, self.example) = {
            let Self {
                usage,
                description,
                section,
                example,
            } = &self;
            (
                usage.as_ref().map(|x| highlight_matches(x, query, style)),
                highlight_matches(description, query, style),
                highlight_matches(section, query, style),
                example.as_ref().map(|x| highlight_matches(x, query, style)),
            )
        };
        self
    }
}

pub trait AsMd {
    fn as_md(&self) -> String;
}

impl AsMd for SloshDoc {
    fn as_md(&self) -> String {
        let mut content = format!(" ### {}\n", self.symbol);
        if let Some(usage) = &self.doc_string.usage {
            content += &format!("**Usage:** {}\n\n", usage);
        }
        content = content + &format!("**Namespace:** {}\n\n", self.namespace);
        content = content + &format!("{}\n", self.doc_string.description);
        if let Some(example) = &self.doc_string.example {
            content += "\n\nExample:\n```\n";
            content += example;
            content += "\n``` \n";
        } else {
            //content += "\n\nNo Examples\n";
        }
        content
    }
}

#[derive(Eq, Debug, Clone)]
pub struct SloshDoc {
    pub symbol: String,
    pub symbol_type: String,
    pub namespace: String,
    pub doc_string: DocStringSection,
}

impl Display for SloshDoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{symbol}\nType: {symbol_type}\nNamespace: {namespace}\n\n{doc_string}",
            symbol = self.symbol,
            symbol_type = self.symbol_type,
            namespace = self.namespace,
            doc_string = self.doc_string
        )
    }
}

impl PartialEq for SloshDoc {
    fn eq(&self, other: &Self) -> bool {
        self.symbol == other.symbol && self.namespace == other.namespace
    }
}

impl Hash for SloshDoc {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(self.symbol.as_bytes());
        state.write(self.namespace.as_bytes());
    }
}

impl PartialOrd for SloshDoc {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SloshDoc {
    fn cmp(&self, other: &Self) -> Ordering {
        self.symbol
            .cmp(&other.symbol)
            .then_with(|| self.namespace.cmp(&other.namespace))
    }
}

impl SloshDoc {
    pub fn new(g: Interned, vm: &mut SloshVm, namespace: Namespace) -> DocResult<SloshDoc> {
        let sym = Value::Symbol(g);
        let slot = vm.global_intern_slot(g);
        if let Some(slot) = slot {
            let doc_string = DocStringSection::from_symbol(slot, sym, vm)?;
            let symbol = sym.display_value(vm);
            let mut full_name: Vec<_> = symbol.split("::").collect();
            let symbol = full_name
                .pop()
                .expect("Symbol should never be an empty.")
                .to_string();
            let symbol_type = sym.display_type(vm).to_string();
            let namespace = namespace.display(vm);
            Ok(SloshDoc {
                symbol,
                symbol_type,
                namespace,
                doc_string,
            })
        } else {
            Err(DocError::NoSymbol {
                symbol: sym.display_value(vm).to_string(),
            })
        }
    }

    pub fn new_incomplete(
        g: Interned,
        vm: &mut SloshVm,
        namespace: Namespace,
    ) -> DocResult<SloshDoc> {
        let sym = Value::Symbol(g);
        let slot = vm.global_intern_slot(g);
        if let Some(slot) = slot {
            let doc_string = DocStringSection::new_incomplete(slot, &sym, vm);
            let symbol = sym.display_value(vm);
            let symbol_type = sym.display_type(vm).to_string();
            let namespace = namespace.display(vm);
            Ok(SloshDoc {
                symbol,
                symbol_type,
                namespace,
                doc_string,
            })
        } else {
            Err(DocError::NoSymbol {
                symbol: sym.display_value(vm).to_string(),
            })
        }
    }

    pub fn fully_qualified_name(&self) -> String {
        self.namespace.to_string() + "::" + self.symbol.as_ref()
    }

    fn into_styled_output(mut self, query: &DocSearchQuery, style: &StyleOptions) -> Self {
        (
            self.symbol,
            self.namespace,
            self.symbol_type,
            self.doc_string,
        ) = {
            let Self {
                symbol,
                namespace,
                symbol_type,
                doc_string,
            } = &self;
            (
                highlight_matches(symbol, query, style),
                highlight_matches(namespace, query, style),
                highlight_matches(symbol_type, query, style),
                doc_string.clone().into_styled_output(query, style),
            )
        };
        self
    }
}

pub enum DocError {
    NoSymbol { symbol: String },
    NoDocString { symbol: String },
    RemoveExemption { symbol: String },
    DocStringMissingSection { symbol: String, section: String },
}

impl Debug for DocError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for DocError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            DocError::NoSymbol { symbol } => {
                format!(
                    "No documentation string provided for symbol {symbol}."
                )
            }
            DocError::NoDocString { symbol } => {
                format!(
                    "Either documentation provided does not conform to conventional layout or no documentation string provided for symbol {symbol} all slosh functions with documentation must have a string that conforms to the conventional layout."
                )
            }
            DocError::DocStringMissingSection { symbol, section } => {
                format!("Invalid documentation string for symbol {symbol}, missing required section {section:?}")
            }
            DocError::RemoveExemption { symbol } => {
                format!("Documentation has been added for {symbol}, remove it from EXEMPTIONS list in builtins::doc_search::EXEMPTIONS.")
            }
        }
            .to_string();
        write!(f, "{}", str)
    }
}

impl Error for DocError {}

impl From<DocError> for VMError {
    fn from(value: DocError) -> Self {
        VMError {
            key: "doc",
            obj: VMErrorObj::Message(value.to_string()),
        }
    }
}

pub type DocResult<T> = Result<T, DocError>;

#[derive(Debug, Clone)]
struct StyleOptions {
    highlight_color: String,
    default_color: String,
    use_background: bool,
}

impl Default for StyleOptions {
    fn default() -> Self {
        StyleOptions {
            highlight_color: "\x1b[43m\x1b[30m".to_string(), // yellow background with black text
            default_color: "\x1b[0m".to_string(),            // reset
            use_background: true,
        }
    }
}

impl StyleOptions {
    fn from_handle(vm: &mut SloshVm, handle: slvm::Handle) -> Self {
        let mut options = Self::default();

        let highlight_color_key = Value::Keyword(vm.intern("highlight-color"));
        let default_color_key = Value::Keyword(vm.intern("default-color"));
        let use_background_key = Value::Keyword(vm.intern("use-background"));

        let map = vm.get_map(handle);

        if let Some(val) = map.get(vm, highlight_color_key) {
            match val {
                Value::String(s) => options.highlight_color = vm.get_string(s).to_string(),
                Value::StringConst(i) => options.highlight_color = vm.get_interned(i).to_string(),
                _ => {}
            }
        }

        if let Some(val) = map.get(vm, default_color_key) {
            match val {
                Value::String(s) => options.default_color = vm.get_string(s).to_string(),
                Value::StringConst(i) => options.default_color = vm.get_interned(i).to_string(),
                _ => {}
            }
        }

        if let Some(Value::False) = map.get(vm, use_background_key) {
            options.use_background = false;
        }

        options
    }
}

fn highlight_matches(text: &str, query: &DocSearchQuery, style: &StyleOptions) -> String {
    match &query.search_mode {
        SearchMode::Regex => highlight_regex_matches(text, &query.query, style),
        SearchMode::Fuzzy => highlight_fuzzy_matches(text, &query.query, style),
        SearchMode::Regular => highlight_substring_matches(text, &query.query, style),
    }
}

fn highlight_substring_matches(text: &str, query: &str, style: &StyleOptions) -> String {
    let text_lower = text.to_lowercase();
    let query_lower = query.to_lowercase();
    let mut result = String::new();
    let mut last_end = 0;

    for (start, _) in text_lower.match_indices(&query_lower) {
        result.push_str(&text[last_end..start]);
        result.push_str(&style.highlight_color);
        result.push_str(&text[start..start + query.len()]);
        result.push_str(&style.default_color);
        last_end = start + query.len();
    }

    result.push_str(&text[last_end..]);
    result
}

fn highlight_regex_matches(text: &str, pattern: &str, style: &StyleOptions) -> String {
    if let Ok(re) = Regex::new(pattern) {
        let mut result = String::new();
        let mut last_end = 0;

        for mat in re.find_iter(text) {
            result.push_str(&text[last_end..mat.start()]);
            result.push_str(&style.highlight_color);
            result.push_str(mat.as_str());
            result.push_str(&style.default_color);
            last_end = mat.end();
        }

        result.push_str(&text[last_end..]);
        result
    } else {
        text.to_string()
    }
}

fn highlight_fuzzy_matches(text: &str, pattern: &str, style: &StyleOptions) -> String {
    let pattern_lower = pattern.to_lowercase();
    let mut result = String::new();
    let mut pattern_chars = pattern_lower.chars().peekable();
    let mut in_match = false;

    for ch in text.chars() {
        if let Some(&pattern_ch) = pattern_chars.peek() {
            if ch.to_lowercase().next() == Some(pattern_ch) {
                if !in_match {
                    result.push_str(&style.highlight_color);
                    in_match = true;
                }
                result.push(ch);
                pattern_chars.next();
            } else {
                if in_match {
                    result.push_str(&style.default_color);
                    in_match = false;
                }
                result.push(ch);
            }
        } else {
            if in_match {
                result.push_str(&style.default_color);
                in_match = false;
            }
            result.push(ch);
        }
    }

    if in_match {
        result.push_str(&style.default_color);
    }

    result
}

fn generate_styled_output(
    _vm: &mut SloshVm,
    results: &BTreeSet<SloshDoc>,
    query: &DocSearchQuery,
    style: &StyleOptions,
) -> String {
    let mut output = String::new();
    let len = results.len();

    for (i, doc) in results.iter().enumerate() {
        if i > 0 {
            output.push_str("\n────────────────────────────────────────\n\n");
        }

        output.push_str(&format!("Result {}: ", i + 1));
        let mut doc = doc.clone();
        doc = doc.into_styled_output(query, style);
        output.push_str(&format!("{doc}"));
    }

    output.push_str(&format!(
        "\nFound {} result{}.",
        len,
        if len == 1 { "" } else { "s" }
    ));

    output
}

fn insert_section(map: &mut VMHashMap, key: &'static str, value: String, vm: &mut SloshVm) {
    let key_const = Value::Keyword(vm.intern_static(key));
    let value_text = vm.alloc_string(value);
    map.insert(vm, key_const, value_text);
}

fn insert_nil_section(map: &mut VMHashMap, key: &'static str, vm: &mut SloshVm) {
    let key_const = Value::Keyword(vm.intern_static(key));
    map.insert(vm, key_const, Value::Nil);
}

impl SlFrom<SloshDoc> for VMHashMap {
    fn sl_from(value: SloshDoc, vm: &mut SloshVm) -> BridgeResult<Self> {
        let mut map = Self::with_capacity(4);
        match (value.doc_string.usage, value.doc_string.example) {
            (Some(usage), Some(example)) => {
                insert_section(&mut map, USAGE, usage, vm);
                insert_section(&mut map, EXAMPLE, example, vm);
            }
            (Some(usage), None) => {
                insert_section(&mut map, USAGE, usage, vm);
                insert_nil_section(&mut map, EXAMPLE, vm);
            }
            (None, Some(example)) => {
                insert_section(&mut map, EXAMPLE, example, vm);
                insert_nil_section(&mut map, USAGE, vm);
            }
            (None, None) => {
                insert_nil_section(&mut map, EXAMPLE, vm);
                insert_nil_section(&mut map, USAGE, vm);
            }
        }
        insert_section(&mut map, SECTION, value.doc_string.section, vm);
        insert_section(&mut map, DESCRIPTION, value.doc_string.description, vm);
        Ok(map)
    }
}

impl SlFrom<SloshDoc> for Value {
    fn sl_from(value: SloshDoc, vm: &mut SloshVm) -> BridgeResult<Self> {
        let map = VMHashMap::sl_from(value, vm)?;
        Ok(vm.alloc_map(map))
    }
}

#[derive(Debug, Clone, PartialEq)]
enum SearchMode {
    /// Default substring matching (case-insensitive).
    Regular,
    /// Fuzzy per-word subsequence matching with span limits.
    Fuzzy,
    /// Full regex pattern matching.
    Regex,
}

#[derive(Debug, Clone)]
struct DocSearchQuery {
    query: String,
    search_mode: SearchMode,
    fields: HashSet<String>,
    namespace_filter: Option<String>,
    section_filter: Option<String>,
}

impl DocSearchQuery {
    fn new(query: String) -> Self {
        DocSearchQuery {
            query,
            search_mode: SearchMode::Regular,
            fields: HashSet::new(),
            namespace_filter: None,
            section_filter: None,
        }
    }

    fn with_search_mode(mut self, mode: SearchMode) -> Self {
        self.search_mode = mode;
        self
    }

    fn with_fields(mut self, fields: Vec<String>) -> Self {
        self.fields = fields.into_iter().collect();
        self
    }

    fn with_namespace(mut self, namespace: String) -> Self {
        self.namespace_filter = Some(namespace);
        self
    }

    fn with_section(mut self, section: String) -> Self {
        self.section_filter = Some(section);
        self
    }

    fn should_search_field(&self, field: &str) -> bool {
        self.fields.is_empty() || self.fields.contains(field)
    }

    fn matches_text(&self, text: &str) -> bool {
        match &self.search_mode {
            SearchMode::Regex => {
                if let Ok(re) = Regex::new(&self.query) {
                    re.is_match(text)
                } else {
                    false
                }
            }
            SearchMode::Fuzzy => fuzzy_match_query(text, &self.query),
            SearchMode::Regular => text.to_lowercase().contains(&self.query.to_lowercase()),
        }
    }
}

/// Fuzzy-matches a multi-word query against text.
///
/// Each query word is matched against text words using subsequence
/// matching within word boundaries — e.g., "gntoo" matches "gentoo"
/// because g-n-t-o-o appears in order within the single word "gentoo",
/// but would NOT match across multiple words like "gate noun toad".
///
/// All query words must match some text word, but they may appear in
/// any order and need not be adjacent.
///
/// Examples:
///   query: "gntoo"       text: "install gentoo"       → true
///   query: "gntoo intal" text: "gentoo install"       → true
///   query: "intal gntoo" text: "gentoo install"       → true  (any order)
///   query: "gntoo intal" text: "gentoo blah install"  → true  (gaps ok)
///   query: "gntoo"       text: "gate noun toad"       → false (per-word only)
///   query: "gntoo intal" text: "gentoo"               → false (both must match)
fn fuzzy_match_query(text: &str, pattern: &str) -> bool {
    let query_words: Vec<String> = pattern
        .split_whitespace()
        .map(|w| w.to_lowercase())
        .collect();
    let text_words: Vec<String> = text.split_whitespace().map(|w| w.to_lowercase()).collect();

    query_words
        .iter()
        .all(|qw| text_words.iter().any(|tw| is_subsequence(qw, tw)))
}

/// Returns true if every character in `pattern` appears in `text`
/// in order, and the matched characters span at most twice the
/// pattern length. This prevents spurious matches across long words
/// where characters happen to appear in order but are widely scattered.
fn is_subsequence(pattern: &str, text: &str) -> bool {
    let pattern_len = pattern.chars().count();
    if pattern_len == 0 {
        return true;
    }
    let max_span = pattern_len * 2;

    let mut pi = pattern.chars();
    let mut cur = pi.next();
    let mut first_match: Option<usize> = None;
    let mut last_match: usize = 0;

    for (i, tc) in text.chars().enumerate() {
        if let Some(pc) = cur {
            if tc == pc {
                if first_match.is_none() {
                    first_match = Some(i);
                }
                last_match = i;
                cur = pi.next();
            }
        } else {
            break;
        }
    }

    if cur.is_some() {
        return false;
    }

    let span = last_match - first_match.unwrap() + 1;
    span <= max_span
}

/// Each doc has a tag in its `Section:` definition by convention that logically groups functions.
/// Using a HashMap store the section tags as keys and add all slosh docs to a vector as a value
/// corresponding to its section.
pub fn get_docs_by_section(
    vm: &mut SloshVm,
    require_proper_format: bool,
) -> HashMap<String, BTreeSet<SloshDoc>> {
    let mut docs_by_section: HashMap<String, BTreeSet<SloshDoc>> = HashMap::new();
    let mut docs: Vec<SloshDoc> = vec![];
    Namespace::Global
        .add_docs(&mut docs, vm, require_proper_format)
        .unwrap();
    let namespaces = crate::get_namespaces_interned(vm);
    for i in namespaces {
        let namespace = Namespace::Other(i);
        namespace
            .add_docs(&mut docs, vm, require_proper_format)
            .unwrap();
    }
    for d in docs {
        let d = d.clone();
        let section = d.doc_string.section.clone();
        docs_by_section.entry(section).or_default().insert(d);
    }
    docs_by_section
}

fn search_docs(
    vm: &mut SloshVm,
    query: &DocSearchQuery,
    require_proper_format: bool,
) -> BTreeSet<SloshDoc> {
    let docs_by_section = get_docs_by_section(vm, require_proper_format);
    let mut results = BTreeSet::new();

    for (section, docs) in docs_by_section {
        // Check section filter
        if let Some(ref section_filter) = query.section_filter {
            if section != *section_filter {
                continue;
            }
        }

        for doc in docs {
            // Check namespace filter
            if let Some(ref ns_filter) = query.namespace_filter {
                if doc.namespace != *ns_filter {
                    continue;
                }
            }

            let mut matched = false;

            // Search in symbol name
            if (query.should_search_field("name") || query.should_search_field("symbol"))
                && query.matches_text(&doc.symbol)
            {
                matched = true;
            }

            // Search in usage
            if !matched
                && query.should_search_field(USAGE)
                && let Some(ref usage) = doc.doc_string.usage
                && query.matches_text(usage)
            {
                matched = true;
            }

            // Search in description
            if !matched
                && query.should_search_field(DESCRIPTION)
                && query.matches_text(&doc.doc_string.description)
            {
                matched = true;
            }

            // Search in section
            if !matched
                && query.should_search_field(SECTION)
                && query.matches_text(&doc.doc_string.section)
            {
                matched = true;
            }

            // Search in example
            if !matched
                && query.should_search_field(EXAMPLE)
                && let Some(ref example) = doc.doc_string.example
                && query.matches_text(example)
            {
                matched = true;
            }

            // If no specific fields specified, search all fields
            if !matched && query.fields.is_empty() {
                matched = query.matches_text(&doc.symbol)
                    || doc
                        .doc_string
                        .usage
                        .as_ref()
                        .map(|u| query.matches_text(u))
                        .unwrap_or(false)
                    || query.matches_text(&doc.doc_string.description)
                    || query.matches_text(&doc.doc_string.section)
                    || doc
                        .doc_string
                        .example
                        .as_ref()
                        .map(|e| query.matches_text(e))
                        .unwrap_or(false);
            }

            if matched {
                results.insert(doc);
            }
        }
    }

    results
}

fn doc_map(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    match (i.next(), i.next()) {
        (Some(Value::Symbol(g)), None) => {
            vm.pause_gc();

            let res = match SloshDoc::new(*g, vm, Namespace::Global) {
                Ok(slosh_doc) => BridgeError::with_fn(Value::sl_from(slosh_doc, vm), "doc-map"),
                Err(e) => Err(VMError::from(e)),
            };
            vm.unpause_gc();
            res
        }
        _ => Err(VMError::new_vm("takes one argument (symbol)".to_string())),
    }
}

fn doc_search(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if registers.is_empty() {
        return Err(VMError::new_vm(
            "doc-search requires at least a query string".to_string(),
        ));
    }

    let query_str = match &registers[0] {
        Value::String(s) => vm.get_string(*s).to_string(),
        Value::StringConst(i) => vm.get_interned(*i).to_string(),
        _ => {
            return Err(VMError::new_vm(
                "First argument must be a query string".to_string(),
            ));
        }
    };

    let mut query = DocSearchQuery::new(query_str);

    let mut use_styled = true;
    let mut style_options = StyleOptions::default();
    let mut style_map_handle_opt = None;
    if registers.len() > 1 {
        match &registers[1] {
            Value::Map(handle) => {
                let search_key = Value::Keyword(vm.intern_static("search"));
                let fields_key = Value::Keyword(vm.intern_static("fields"));
                let ns_key = Value::Keyword(vm.intern_static("namespace"));
                let section_key = Value::Keyword(vm.intern_static("section"));
                let styled_key = Value::Keyword(vm.intern_static("styled"));
                let style_key = Value::Keyword(vm.intern_static("style"));

                let map = vm.get_map(*handle);

                if let Some(Value::Keyword(mode)) = map.get(vm, search_key) {
                    let mode_str = vm.get_interned(mode);
                    match mode_str {
                        "regular" => query = query.with_search_mode(SearchMode::Regular),
                        "fuzzy" => query = query.with_search_mode(SearchMode::Fuzzy),
                        "regex" => query = query.with_search_mode(SearchMode::Regex),
                        _ => {
                            return Err(VMError::new_vm(format!(
                                ":search must be :regular, :fuzzy, or :regex, got :{mode_str}"
                            )));
                        }
                    }
                }

                if let Some(Value::Vector(v_handle)) = map.get(vm, fields_key) {
                    let vec = vm.get_vector(v_handle);
                    let mut fields = Vec::new();
                    for val in vec.iter() {
                        match val {
                            Value::String(s) => fields.push(vm.get_string(*s).to_string()),
                            Value::StringConst(i) => fields.push(vm.get_interned(*i).to_string()),
                            Value::Keyword(i) => fields.push(vm.get_interned(*i).to_string()),
                            _ => {}
                        }
                    }
                    query = query.with_fields(fields);
                }

                if let Some(ns_val) = map.get(vm, ns_key) {
                    match ns_val {
                        Value::String(s) => {
                            query = query.with_namespace(vm.get_string(s).to_string())
                        }
                        Value::StringConst(i) => {
                            query = query.with_namespace(vm.get_interned(i).to_string())
                        }
                        _ => {}
                    }
                }

                if let Some(section_val) = map.get(vm, section_key) {
                    match section_val {
                        Value::String(s) => {
                            query = query.with_section(vm.get_string(s).to_string())
                        }
                        Value::StringConst(i) => {
                            query = query.with_section(vm.get_interned(i).to_string())
                        }
                        _ => {}
                    }
                }

                if let Some(Value::False) = map.get(vm, styled_key) {
                    use_styled = false;
                }

                if let Some(Value::Map(style_handle)) = map.get(vm, style_key) {
                    use_styled = true;
                    style_map_handle_opt = Some(style_handle);
                }
            }
            _ => {
                return Err(VMError::new_vm(
                    "Second argument must be a map of options".to_string(),
                ));
            }
        }
    }

    if let Some(style_handle) = style_map_handle_opt {
        style_options = StyleOptions::from_handle(vm, style_handle);
    }

    let mut use_markdown = false;
    if registers.len() > 1 {
        if let Value::Map(handle) = &registers[1] {
            let markdown_key = Value::Keyword(vm.intern_static("markdown"));
            let map = vm.get_map(*handle);
            if let Some(Value::True) = map.get(vm, markdown_key) {
                use_markdown = true;
            }
        }
    }

    let results = search_docs(vm, &query, false);

    let result = if use_markdown {
        let mut markdown_output = String::new();
        let len = results.len();
        for (i, doc) in results.iter().enumerate() {
            if i > 0 {
                markdown_output.push_str("\n---\n\n");
            }
            markdown_output.push_str(&format!(
                "**Result {}: `{}::{}`**\n\n",
                i + 1,
                doc.namespace,
                doc.symbol
            ));
            markdown_output.push_str(&doc.as_md());
        }
        markdown_output.push_str(&format!(
            "\nFound {len} result{}.",
            if len == 1 { "" } else { "s" }
        ));
        vm.alloc_string(markdown_output)
    } else if use_styled {
        let styled_output = generate_styled_output(vm, &results, &query, &style_options);
        vm.alloc_string(styled_output)
    } else {
        let mut result_values = Vec::new();
        for doc in results {
            match BridgeError::with_fn(Value::sl_from(doc, vm), "doc-search") {
                Ok(val) => result_values.push(val),
                Err(e) => {
                    vm.unpause_gc();
                    return Err(e);
                }
            }
        }
        vm.alloc_vector(result_values)
    };

    Ok(result)
}

pub fn add_doc_search_builtins(env: &mut SloshVm) {
    add_builtin(
        env,
        "doc-map",
        doc_map,
        "Usage: (doc-map symbol)

Returns documentation for given symbol as map. Keyword is a documentation fragment
(usage, section, description, example) and value is text describing given fragment.

Section: doc

Example:
#t
",
    );

    add_builtin(
        env,
        "doc-search",
        doc_search,
        "Usage: (doc-search query-string [options-map])

Search through all documentation for functions matching the query.
The query string is searched across all documentation fields by default.

Options map can contain:
- :search :regular | :fuzzy | :regex - Search mode (default :regular)
  :regular - Case-insensitive substring matching
  :fuzzy   - Per-word subsequence matching with span limits, good for typos
  :regex   - Full regular expression pattern matching
- :fields [\"usage\" \"description\"] - Limit search to specific fields
- :namespace \"namespace-name\" - Filter by namespace
- :section \"section-name\" - Filter by section
- :markdown #t - Return results as a markdown-formatted string instead of vector
- :styled #t - Return results with highlighted search matches
- :style {:highlight-color \"\\x1b[43m\\x1b[30m\" ...} - Customize highlight style

Style options (when :style map is provided):
- :highlight-color - Color for highlighting matches (default: yellow background, black text)
- :default-color - Color to reset after highlight (default: \\x1b[0m)
- :use-background #f - Use foreground color instead of background

When :styled is true, matching text in results is highlighted to show why
each result matched the search query.

Returns a vector of documentation maps, markdown string, or styled string.

Section: doc

Example:
(doc-search \"file\")
(doc-search \"^str-\" {:search :regex})
(doc-search \"core\" {:fields [\"section\"]})
(doc-search \"map\" {:search :fuzzy :section \"core\"})
(doc-search \"string\" {:markdown #t})
(doc-search \"string\" {:styled #t})
(doc-search \"map\" {:style {:highlight-color *fg-red* :use-background #f}})
",
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use compile_state::state::new_slosh_vm;

    mod fuzzy_match {
        use super::*;
        #[test]
        fn fuzzy_match_basic() {
            assert!(fuzzy_match_query("str-append", "sta"));
            assert!(fuzzy_match_query("str-append", "str"));
            assert!(fuzzy_match_query("str-append", "sap"));
        }

        #[test]
        fn fuzzy_match_case_insensitive() {
            assert!(fuzzy_match_query("Str-Append", "sta"));
            assert!(fuzzy_match_query("str-append", "STA"));
        }

        #[test]
        fn fuzzy_match_full_string() {
            assert!(fuzzy_match_query("abc", "abc"));
        }

        #[test]
        fn fuzzy_match_empty_pattern() {
            assert!(fuzzy_match_query("anything", ""));
        }

        #[test]
        fn fuzzy_match_no_match() {
            assert!(!fuzzy_match_query("abc", "z"));
            assert!(!fuzzy_match_query("abc", "abcd"));
            assert!(!fuzzy_match_query("abc", "ba"));
        }

        #[test]
        fn fuzzy_match_empty_text() {
            assert!(!fuzzy_match_query("", "a"));
            assert!(fuzzy_match_query("", ""));
        }
    }

    mod doc_search_query {
        use super::*;
        #[test]
        fn query_default_substring_match() {
            let q = DocSearchQuery::new("file".to_string());
            assert!(q.matches_text("open a file"));
            assert!(q.matches_text("FILE stuff"));
            assert!(!q.matches_text("no match"));
        }

        #[test]
        fn query_regex_match() {
            let q = DocSearchQuery::new("^str-".to_string()).with_search_mode(SearchMode::Regex);
            assert!(q.matches_text("str-append"));
            assert!(!q.matches_text("my-str-append"));
        }

        #[test]
        fn query_fuzzy_match() {
            let q = DocSearchQuery::new("sta".to_string()).with_search_mode(SearchMode::Fuzzy);
            assert!(q.matches_text("str-append"));
            assert!(!q.matches_text("zoo"));
        }

        #[test]
        fn query_last_search_mode_wins() {
            let q = DocSearchQuery::new("abc".to_string())
                .with_search_mode(SearchMode::Fuzzy)
                .with_search_mode(SearchMode::Regex);
            assert_eq!(q.search_mode, SearchMode::Regex);
        }

        #[test]
        fn query_should_search_field_empty_matches_all() {
            let q = DocSearchQuery::new("x".to_string());
            assert!(q.should_search_field("usage"));
            assert!(q.should_search_field("description"));
            assert!(q.should_search_field("anything"));
        }

        #[test]
        fn query_should_search_field_respects_filter() {
            let q = DocSearchQuery::new("x".to_string()).with_fields(vec!["usage".to_string()]);
            assert!(q.should_search_field("usage"));
            assert!(!q.should_search_field("description"));
            assert!(!q.should_search_field("section"));
        }

        #[test]
        fn query_invalid_regex_returns_false() {
            let q = DocSearchQuery::new("[invalid".to_string()).with_search_mode(SearchMode::Regex);
            assert!(!q.matches_text("anything"));
        }
    }

    fn test_style() -> StyleOptions {
        StyleOptions {
            highlight_color: "<H>".to_string(),
            default_color: "</H>".to_string(),
            use_background: true,
        }
    }

    mod highlighting {
        use super::*;
        #[test]
        fn highlight_substring_single_match() {
            let result = highlight_substring_matches("open a file", "file", &test_style());
            assert_eq!(result, "open a <H>file</H>");
        }

        #[test]
        fn highlight_substring_multiple_matches() {
            let result = highlight_substring_matches("file and file", "file", &test_style());
            assert_eq!(result, "<H>file</H> and <H>file</H>");
        }

        #[test]
        fn highlight_substring_case_insensitive() {
            let result = highlight_substring_matches("Open a FILE", "file", &test_style());
            assert_eq!(result, "Open a <H>FILE</H>");
        }

        #[test]
        fn highlight_substring_no_match() {
            let result = highlight_substring_matches("nothing here", "xyz", &test_style());
            assert_eq!(result, "nothing here");
        }

        #[test]
        fn highlight_regex_match() {
            let result = highlight_regex_matches("str-append", "^str-", &test_style());
            assert_eq!(result, "<H>str-</H>append");
        }

        #[test]
        fn highlight_regex_invalid_returns_original() {
            let result = highlight_regex_matches("some text", "[invalid", &test_style());
            assert_eq!(result, "some text");
        }

        #[test]
        fn highlight_fuzzy_basic() {
            let result = highlight_fuzzy_matches("str-append", "sap", &test_style());
            assert_eq!(result, "<H>s</H>tr-<H>ap</H>pend");
        }

        #[test]
        fn highlight_fuzzy_no_match_chars_left() {
            // When pattern is longer than what matches, remaining chars aren't highlighted
            let result = highlight_fuzzy_matches("ab", "abc", &test_style());
            assert_eq!(result, "<H>ab</H>");
        }
    }

    mod doc_string_section_parsing {
        use super::*;
        #[test]
        fn parse_full_doc_string() {
            let doc = DocStringSection::parse_doc_string(
                Cow::Borrowed("test-fn"),
                "Usage: (test-fn arg)\n\nDoes a thing.\n\nSection: core\n\nExample:\n#t\n"
                    .to_string(),
                "".to_string(),
            )
            .unwrap();
            assert_eq!(doc.usage.as_deref(), Some("(test-fn arg)"));
            assert_eq!(doc.description, "Does a thing.");
            assert_eq!(doc.section, "core");
            assert_eq!(doc.example.as_deref(), Some("#t"));
        }

        #[test]
        fn parse_doc_string_no_usage_with_backup() {
            let doc = DocStringSection::parse_doc_string(
                Cow::Borrowed("test-fn"),
                "Does a thing.\n\nSection: core\n".to_string(),
                "(test-fn x y)".to_string(),
            )
            .unwrap();
            assert_eq!(doc.usage.as_deref(), Some("(test-fn x y)"));
            assert_eq!(doc.description, "Does a thing.");
            assert_eq!(doc.section, "core");
            assert!(doc.example.is_none());
        }

        #[test]
        fn parse_doc_string_no_section_fails() {
            let result = DocStringSection::parse_doc_string(
                Cow::Borrowed("test-fn"),
                "Just a description with no section.".to_string(),
                "".to_string(),
            );
            assert!(result.is_err());
        }

        #[test]
        fn parse_doc_string_exempted_symbol_no_doc() {
            let result = DocStringSection::parse_doc_string(
                Cow::Borrowed("version"),
                "".to_string(),
                "".to_string(),
            );
            let doc = result.unwrap();
            assert_eq!(doc.section, "undocumented");
        }
    }

    fn setup_vm() -> SloshVm {
        let mut vm = new_slosh_vm();
        crate::add_misc_builtins(&mut vm);
        add_doc_search_builtins(&mut vm);
        vm
    }

    mod search_docs_with_vm {
        use super::*;

        #[test]
        fn search_docs_substring_finds_results() {
            let mut vm = setup_vm();
            let query = DocSearchQuery::new("doc-search".to_string());
            let results = search_docs(&mut vm, &query, false);
            assert!(
                results.iter().any(|d| d.symbol == "doc-search"),
                "Should find doc-search itself"
            );
        }

        #[test]
        fn search_docs_regex_caret_filters() {
            let mut vm = setup_vm();
            let query =
                DocSearchQuery::new("^doc-".to_string()).with_search_mode(SearchMode::Regex);
            let results = search_docs(&mut vm, &query, false);
            for doc in &results {
                assert!(
                    doc.symbol.starts_with("doc-")
                        || doc
                            .doc_string
                            .usage
                            .as_ref()
                            .is_some_and(|u| u.contains("doc-"))
                        || doc.doc_string.description.contains("doc-")
                        || doc.doc_string.section.starts_with("doc-")
                        || doc
                            .doc_string
                            .example
                            .as_ref()
                            .is_some_and(|e| e.contains("doc-")),
                    "Result '{}' should match ^doc- in some field",
                    doc.symbol
                );
            }
        }

        #[test]
        fn search_docs_section_filter() {
            let mut vm = setup_vm();
            // Section filter passes sections that either match the filter exactly
            // OR where the query text matches the section name as a substring.
            // So searching for "doc" with section "doc" also includes "undocumented"
            // since "doc" is a substring of "undocumented".
            let query =
                DocSearchQuery::new("doc-search".to_string()).with_section("doc".to_string());
            let results = search_docs(&mut vm, &query, false);
            assert!(!results.is_empty(), "Should find results in doc section");
            for doc in &results {
                assert_eq!(
                    doc.doc_string.section, "doc",
                    "'{}' (section: '{}') should be in section 'doc'",
                    doc.symbol, doc.doc_string.section
                );
            }
        }

        #[test]
        fn search_docs_field_filter_limits_search() {
            let mut vm = setup_vm();
            // Search for "doc" only in section field
            let query =
                DocSearchQuery::new("doc".to_string()).with_fields(vec!["section".to_string()]);
            let results = search_docs(&mut vm, &query, false);
            assert!(!results.is_empty());
            for doc in &results {
                assert!(
                    doc.doc_string.section.to_lowercase().contains("doc"),
                    "'{}' section '{}' should contain 'doc'",
                    doc.symbol,
                    doc.doc_string.section
                );
            }
        }

        #[test]
        fn search_docs_no_results() {
            let mut vm = setup_vm();
            let query = DocSearchQuery::new("zzzzz_absolutely_no_match_99999".to_string());
            let results = search_docs(&mut vm, &query, false);
            assert!(results.is_empty());
        }

        #[test]
        fn search_docs_fuzzy() {
            let mut vm = setup_vm();
            let query = DocSearchQuery::new("dsr".to_string()).with_search_mode(SearchMode::Fuzzy);
            let results = search_docs(&mut vm, &query, false);
            assert!(
                results.iter().any(|d| d.symbol == "doc-search"),
                "Fuzzy 'dsr' should match 'doc-search' (d-oc-s-ea-r-ch)"
            );
        }
    }

    mod doc_search_builtin_register_level {
        use super::*;
        #[test]
        fn doc_search_builtin_returns_styled_string() {
            let mut vm = setup_vm();
            let query_val = vm.alloc_string("doc-search".to_string());
            let result = doc_search(&mut vm, &[query_val]).unwrap();
            match result {
                Value::String(h) => {
                    let s = vm.get_string(h).to_string();
                    assert!(
                        s.contains("doc-search"),
                        "Styled output should contain 'doc-search'"
                    );
                    assert!(
                        s.contains("Found"),
                        "Styled output should contain result count"
                    );
                }
                _ => panic!("Expected string result from styled doc-search"),
            }
        }

        #[test]
        fn doc_search_builtin_no_args_errors() {
            let mut vm = setup_vm();
            let result = doc_search(&mut vm, &[]);
            assert!(result.is_err());
        }

        #[test]
        fn doc_search_builtin_wrong_type_errors() {
            let mut vm = setup_vm();
            let result = doc_search(&mut vm, &[Value::from(42_i64)]);
            assert!(result.is_err());
        }
    }

    mod doc_map_builtin {
        use super::*;

        #[test]
        fn doc_map_returns_map_for_known_symbol() {
            let mut vm = setup_vm();
            let sym = vm.intern("doc-search");
            let result = doc_map(&mut vm, &[Value::Symbol(sym)]).unwrap();
            assert!(
                matches!(result, Value::Map(_)),
                "doc-map should return a map"
            );
        }

        #[test]
        fn doc_map_no_args_errors() {
            let mut vm = setup_vm();
            let result = doc_map(&mut vm, &[]);
            assert!(result.is_err());
        }

        #[test]
        fn doc_map_wrong_type_errors() {
            let mut vm = setup_vm();
            let s = vm.alloc_string("not-a-symbol".to_string());
            let result = doc_map(&mut vm, &[s]);
            assert!(result.is_err());
        }
    }

    mod namespace_slosh_doc {
        use super::*;

        #[test]
        fn namespace_display_global() {
            let mut vm = new_slosh_vm();
            assert_eq!(Namespace::Global.display(&mut vm), "root");
        }

        #[test]
        fn slosh_doc_ordering() {
            let make_doc = |sym: &str, ns: &str| SloshDoc {
                symbol: sym.to_string(),
                symbol_type: "Symbol".to_string(),
                namespace: ns.to_string(),
                doc_string: DocStringSection {
                    usage: None,
                    description: "test".to_string(),
                    section: "test".to_string(),
                    example: None,
                },
            };
            let a = make_doc("aaa", "root");
            let b = make_doc("bbb", "root");
            let a2 = make_doc("aaa", "other");
            assert!(a < b);
            assert!(a2 < b);
            // Same symbol, different namespace
            assert!(a2 < a); // "other" < "root"
        }

        #[test]
        fn slosh_doc_equality_ignores_type() {
            let d1 = SloshDoc {
                symbol: "foo".to_string(),
                symbol_type: "Symbol".to_string(),
                namespace: "root".to_string(),
                doc_string: DocStringSection {
                    usage: Some("(foo)".to_string()),
                    description: "one".to_string(),
                    section: "core".to_string(),
                    example: None,
                },
            };
            let d2 = SloshDoc {
                symbol: "foo".to_string(),
                symbol_type: "Lambda".to_string(),
                namespace: "root".to_string(),
                doc_string: DocStringSection {
                    usage: None,
                    description: "two".to_string(),
                    section: "other".to_string(),
                    example: Some("example".to_string()),
                },
            };
            assert_eq!(d1, d2, "Equality is based on symbol + namespace only");
        }

        #[test]
        fn slosh_doc_as_md() {
            let doc = SloshDoc {
                symbol: "my-fn".to_string(),
                symbol_type: "Symbol".to_string(),
                namespace: "root".to_string(),
                doc_string: DocStringSection {
                    usage: Some("(my-fn x)".to_string()),
                    description: "Does stuff.".to_string(),
                    section: "core".to_string(),
                    example: Some("#t".to_string()),
                },
            };
            let md = doc.as_md();
            assert!(md.contains("### my-fn"));
            assert!(md.contains("**Usage:** (my-fn x)"));
            assert!(md.contains("**Namespace:** root"));
            assert!(md.contains("Does stuff."));
            assert!(md.contains("#t"));
        }

        /// get_docs_by_section with VM
        #[test]
        fn get_docs_by_section_returns_sections() {
            let mut vm = setup_vm();
            let sections = get_docs_by_section(&mut vm, false);
            assert!(
                sections.contains_key("doc"),
                "Should have a 'doc' section from doc-search/doc-map"
            );
        }
    }

    mod fuzzy_match_query {
        use super::*;

        #[test]
        fn subsequence_exact_match() {
            assert!(is_subsequence("gentoo", "gentoo"));
        }

        #[test]
        fn subsequence_missing_char() {
            assert!(is_subsequence("gntoo", "gentoo"));
        }

        #[test]
        fn subsequence_single_char() {
            assert!(is_subsequence("g", "gentoo"));
        }

        #[test]
        fn subsequence_empty_pattern() {
            assert!(is_subsequence("", "gentoo"));
        }

        #[test]
        fn subsequence_no_match() {
            assert!(!is_subsequence("xyz", "gentoo"));
        }

        #[test]
        fn subsequence_pattern_longer_than_text() {
            assert!(!is_subsequence("gentooo", "gentoo"));
        }

        #[test]
        fn subsequence_rejects_wide_span() {
            // "act" (3 chars) in "abcdefghijklmnopqrst": a=0, c=2, t=19
            // span=20, limit=6 -> rejected
            assert!(!is_subsequence("act", "abcdefghijklmnopqrst"));
        }

        #[test]
        fn subsequence_accepts_tight_span() {
            // "ac" (2 chars) in "abc": a=0, c=2, span=3, limit=4 -> OK
            assert!(is_subsequence("ac", "abc"));
        }

        #[test]
        fn single_word_match() {
            assert!(fuzzy_match_query("install gentoo", "gntoo"));
        }

        #[test]
        fn multi_word_match() {
            assert!(fuzzy_match_query("gentoo install", "gntoo intal"));
        }

        #[test]
        fn multi_word_any_order() {
            assert!(fuzzy_match_query("gentoo install", "intal gntoo"));
        }

        #[test]
        fn multi_word_with_gaps() {
            assert!(fuzzy_match_query("gentoo blah blah install", "gntoo intal"));
        }

        #[test]
        fn no_cross_word_smearing() {
            assert!(!fuzzy_match_query("gate noun toad", "gntoo"));
        }

        #[test]
        fn all_query_words_must_match() {
            assert!(!fuzzy_match_query("gentoo", "gntoo intal"));
        }

        #[test]
        fn case_insensitive() {
            assert!(fuzzy_match_query("Gentoo Install", "GNTOO INTAL"));
        }

        #[test]
        fn empty_query_matches_anything() {
            assert!(fuzzy_match_query("whatever", ""));
        }

        #[test]
        fn duplicate_query_words_match_same_text_word() {
            // documented behavior: both "gntoo"s match the single "gentoo"
            assert!(fuzzy_match_query("gentoo", "gntoo gntoo"));
        }
    }
}
