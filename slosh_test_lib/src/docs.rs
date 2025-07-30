use crate::docs::legacy as legacy_docs;
use bridge_adapters::add_builtin;
use bridge_adapters::lisp_adapters::SlFrom;
use bridge_adapters::{BridgeError, BridgeResult};
use compile_state::state::{SloshVm, SloshVmTrait};
use lazy_static::lazy_static;
use mdbook::book::{Book, Chapter};
use mdbook::{BookItem, MDBook};
use regex::{Regex, RegexBuilder};
use slvm::vm_hashmap::VMHashMap;
use slvm::{Interned, SLOSH_NIL, VMError, VMErrorObj, VMResult, Value};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::fs::{self, File};
use std::hash::{Hash, Hasher};
use std::io::{self, Write};
use std::path::Path;
use std::path::PathBuf;
use std::string::ToString;

pub mod legacy;

const USER_FORMS: &str = "User Forms";
const GLOBAL_NAMESPACE: &str = "root";
const USAGE: &str = "usage";
const DESCRIPTION: &str = "description";
const SECTION: &str = "section";
const EXAMPLE: &str = "example";

lazy_static! {
    static ref DOC_REGEX: Regex =
        RegexBuilder::new(r#"(\s*?Usage:(.+?)$\n\n|\s*?)(\S{1}.*)\n\n\s*Section:(.+?)$(\n\n\s*Example:\n(.*)|\s*)"#)
            .multi_line(true)
            .dot_matches_new_line(true)
            .crlf(true)
            .build()
            .unwrap();
    // TODO #229 save off list of EXEMPTIONS triggered and write to a md file for docs.
    pub static ref EXEMPTIONS: HashSet<&'static str> = {
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
    };
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
enum Namespace {
    Global,
    Other(Interned),
}

impl Namespace {
    fn display(&self, vm: &mut SloshVm) -> String {
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

    fn add_docs(
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
                let value = builtins::retrieve_in_namespace(vm, i);
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
struct DocStringSection {
    usage: Option<String>,
    description: String,
    section: String,
    example: Option<String>,
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
        let backup_usage = slosh_lib::usage(vm, slot, &sym);
        Self::parse_doc_string(Cow::Owned(sym_str), raw_doc_string, backup_usage)
    }

    fn raw_docstring(slot: u32, vm: &mut SloshVm) -> String {
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
    /// TODO #229 track incomplete documenntation in a page.
    pub fn new_incomplete(slot: u32, sym: &Value, vm: &mut SloshVm) -> Self {
        let description = Self::raw_docstring(slot, vm);
        let usage = Some(slosh_lib::usage(vm, slot, sym));
        DocStringSection {
            usage,
            description,
            section: SLOSH_NIL.to_string(),
            example: None,
        }
    }

    fn into_styled_output(mut self, query: &DocSearchQuery, style: &StyleOptions) -> Self {
        (self.usage, self.description, self.section, self.example) = {
            // use destructing to force compiler error if structure changes.
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
        //content = content + &format!("- type: {}\n", docs.symbol_type);
        //content = content + &format!("- namespace: {}\n", docs.namespace);
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
            content += "\n\nNo Examples\n";
        }
        content
    }
}

#[derive(Eq, Debug, Clone)]
pub struct SloshDoc {
    symbol: String,
    symbol_type: String,
    namespace: String,
    doc_string: DocStringSection,
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
    fn new(g: Interned, vm: &mut SloshVm, namespace: Namespace) -> DocResult<SloshDoc> {
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

    fn new_incomplete(g: Interned, vm: &mut SloshVm, namespace: Namespace) -> DocResult<SloshDoc> {
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

    /// Provide the fully
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
            // use destructing to force compiler error if structure changes.
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

enum DocError {
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
            DocError::RemoveExemption { symbol} => {
                format!("Documentation has been added for {symbol}, remove it from EXEMPTIONS list in slosh_test::docs::EXEMPTIONS.")
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

type DocResult<T> = Result<T, DocError>;

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

        // Intern all keys upfront
        let highlight_color_key = Value::Keyword(vm.intern("highlight-color"));
        let default_color_key = Value::Keyword(vm.intern("default-color"));
        let use_background_key = Value::Keyword(vm.intern("use-background"));

        let map = vm.get_map(handle);

        // Extract colors from map
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
    if query.use_regex {
        highlight_regex_matches(text, &query.query, style)
    } else if query.basic {
        highlight_basic_matches(text, &query.query, style)
    } else {
        highlight_substring_matches(text, &query.query, style)
    }
}

fn highlight_substring_matches(text: &str, query: &str, style: &StyleOptions) -> String {
    let text_lower = text.to_lowercase();
    let query_lower = query.to_lowercase();
    let mut result = String::new();
    let mut last_end = 0;

    for (start, _) in text_lower.match_indices(&query_lower) {
        // Add text before match
        result.push_str(&text[last_end..start]);
        // Add highlighted match
        result.push_str(&style.highlight_color);
        result.push_str(&text[start..start + query.len()]);
        result.push_str(&style.default_color);
        last_end = start + query.len();
    }

    // Add remaining text
    result.push_str(&text[last_end..]);
    result
}

fn highlight_regex_matches(text: &str, pattern: &str, style: &StyleOptions) -> String {
    if let Ok(re) = Regex::new(pattern) {
        let mut result = String::new();
        let mut last_end = 0;

        for mat in re.find_iter(text) {
            // Add text before match
            result.push_str(&text[last_end..mat.start()]);
            // Add highlighted match
            result.push_str(&style.highlight_color);
            result.push_str(mat.as_str());
            result.push_str(&style.default_color);
            last_end = mat.end();
        }

        // Add remaining text
        result.push_str(&text[last_end..]);
        result
    } else {
        text.to_string()
    }
}

fn highlight_basic_matches(text: &str, pattern: &str, style: &StyleOptions) -> String {
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

        // Result header with highlighted symbol
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

fn doc_map(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    match (i.next(), i.next()) {
        (Some(Value::Symbol(g)), None) => {
            // Pause GC so that we don't wind up collecting any strings used to build the doc map
            // before they get rooted via the map.
            vm.pause_gc();

            let res = match SloshDoc::new(*g, vm, Namespace::Global) {
                Ok(slosh_doc) => BridgeError::with_fn(Value::sl_from(slosh_doc, vm), "doc-map"),
                Err(e) => Err(VMError::from(e)),
            };
            // Unpause GC, this MUST happen so no early returns (looking at you ?).
            vm.unpause_gc();
            res
        }
        _ => Err(VMError::new_vm("takes one argument (symbol)".to_string())),
    }
}

/// Each doc has a tag in its `Section:` definition by convention that logically groups functions.
/// Using a HashMap store the section tags as keys and add all slosh docs from to a vector as a value
/// corresponding to its section.
fn get_docs_by_section(
    vm: &mut SloshVm,
    require_proper_format: bool,
) -> HashMap<String, BTreeSet<SloshDoc>> {
    let mut docs_by_section: HashMap<String, BTreeSet<SloshDoc>> = HashMap::new();
    let mut docs: Vec<SloshDoc> = vec![];
    Namespace::Global
        .add_docs(&mut docs, vm, require_proper_format)
        .unwrap();
    let namespaces = builtins::get_namespaces_interned(vm);
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

fn build_symbols_list(
    docs_by_section: &BTreeMap<String, BTreeSet<SloshDoc>>,
    namer: fn(&str, &SloshDoc) -> String,
) -> BTreeMap<String, String> {
    let mut map = BTreeMap::new();
    for (section, v) in docs_by_section.iter() {
        let mut list = "".to_string();
        let len = v.len();
        for (i, docs) in v.iter().enumerate() {
            let name = namer(section.as_ref(), docs);
            list += &name;

            if i + 1 != len {
                list += ", ";
            }
        }
        list += "\n";
        map.insert(section.to_string(), list);
    }
    map
}

fn symbol_and_capitalized_symbol(doc: &SloshDoc) -> (String, String) {
    let sym = doc.symbol.clone();
    let cap: String = sym
        .chars()
        .filter(|c| c.is_alphabetic() || *c == '-')
        .collect();
    (sym, cap)
}

fn name_for_all_page(section: &str, doc: &SloshDoc) -> String {
    let (s, t) = symbol_and_capitalized_symbol(doc);
    format!("[{}]({section}.html#{})", s, t)
}

fn name_for_section_page(_section: &str, doc: &SloshDoc) -> String {
    let (s, t) = symbol_and_capitalized_symbol(doc);
    format!("[{}](#{})", s, t)
}

fn build_all_slosh_forms_listing_chapter(
    name: &str,
    docs_by_section: &BTreeMap<String, BTreeSet<SloshDoc>>,
) -> VMResult<Chapter> {
    let mut all_content = format!("# {}\n\n", name);
    if name == USER_FORMS {
        all_content += r#"
When built locally (see doc/README.md) the mdbook generator has access to the user's
slosh environment. This enables creating documentation in md form of all user defined
functions. The user's init.slosh is automatically loaded, so any additional files it
loads are also imported. If the user needs to alter the slosh load path or add
additional files to be loaded (presumably because they aren't imported in init.slosh
or any slosh file it imports) it can be done by adding the paths in the `user-doc-files`
and `user-doc-load-paths` string arrays in doc/book.toml. The default is to use
`~/.config/slosh/` as the load path and `~/.config/slosh/init.slosh` for the rc file.

"#;
    }
    let sections_len = docs_by_section.keys().len();
    let mut list = "List of sections: \n\n".to_string();
    for (i, section) in docs_by_section.keys().enumerate() {
        list = list + &format!("[{}](#section-{})", section, section);
        if i + 1 != sections_len {
            list += ", ";
        }
    }
    list += "\n\n";
    all_content += &list;

    let list = build_symbols_list(docs_by_section, name_for_all_page);
    for (section, content) in list {
        let header = format!("## Section: {} \n\n", section);
        all_content += &(header + &content);
    }

    let p = make_file(format!("all-{}", name.to_ascii_lowercase()), &all_content)
        .map_err(|e| VMError::new_vm(format!("Failed to write to file: {e}.")))?;

    Ok(Chapter::new("All", all_content, p, vec![]))
}

fn build_each_docs_section_chapter(
    docs_by_section: &BTreeMap<String, BTreeSet<SloshDoc>>,
) -> VMResult<Vec<Chapter>> {
    let mut sections_as_md_text: BTreeMap<String, String> = BTreeMap::new();
    let lists = build_symbols_list(docs_by_section, name_for_section_page);
    for (section, v) in docs_by_section {
        let mut content = lists.get(section).unwrap().to_owned() + "\n";
        for docs in v {
            content = content + &docs.as_md();
        }
        content += "\n";

        sections_as_md_text.insert(section.to_string(), content.clone());
    }

    let mut chapters = vec![];
    for (section, list) in sections_as_md_text.iter() {
        let mut content = format!("## {}\n\n", section);

        let file = format!("src/section-docs/{}.md", section);
        // If there is a section file header include it for preprocessing.
        if let Ok(file_str) = fs::read_to_string(&file) {
            content = content + &file_str;
            // could never get it to work right w/ the #include directive...
            //content = content + &format!("{{{{#include section-docs/{}.md}}}}\n\n\n", section);
        } else if fs::metadata(&file).is_ok() {
            eprintln!("Error processing file: {}", file);
        }

        let header = "\n\nList of symbols: \n\n".to_string();
        content = content + &header + list;

        let path = make_file(section, &content)
            .map_err(|e| VMError::new_vm(format!("Failed to write to file: {e}.")))?;
        let capped = capitalize_first(section);
        let section_chapter = Chapter::new(&capped, content.clone(), &path, vec![]);
        chapters.push(section_chapter);
    }
    Ok(chapters)
}

fn build_sl_sh_transition_chapter(vm: &mut SloshVm) -> VMResult<Chapter> {
    let report = legacy::build_report(vm)?;
    let name = "sl-sh -> slosh port";
    let path = make_file(name, &report)
        .map_err(|e| VMError::new_vm(format!("Failed to write to file: {e}.")))?;

    let section_chapter = Chapter::new(name, report, &path, vec![]);
    Ok(section_chapter)
}

/// Convention is that the section that enumerates all of the forms by section
/// ([`get_slosh_docs`]) is added to [`MDBook`] and then the supplementary materials
/// ([`link_supplementary_docs`]).
pub fn add_forms_and_supplementary_docs(vm: &mut SloshVm, md_book: &mut Book) -> VMResult<()> {
    get_slosh_docs(vm, md_book, true)?;
    link_supplementary_docs(vm, md_book)?;
    Ok(())
}

/// Supplementary materials include things like, the sl-sh -> slosh port tracker, actual Rust documentation,
/// and legacy sl-sh documentation.
pub fn link_supplementary_docs(vm: &mut SloshVm, md_book: &mut Book) -> VMResult<()> {
    // Add a separator and a title for the new autogenerated section.
    md_book.push_item(BookItem::Separator);
    md_book.push_item(BookItem::PartTitle("Supplemental Material".to_string()));

    // Transition section!
    md_book.push_item(BookItem::Chapter(build_sl_sh_transition_chapter(vm)?));
    let sections = vec![
        ("Slosh Rust Docs", "slosh-rust-docs/doc/slosh/index.html"),
        ("All Rust Docs", "all-rust-docs/doc/slosh/index.html"),
        ("Legacy sl-sh Documentation", "legacy/index.html"),
    ];
    for (section_name, section_link) in sections.into_iter() {
        let content = format!("[{}] ", section_name);
        let section_chapter = Chapter::new(section_name, content, section_link, vec![]);
        md_book.push_item(BookItem::Chapter(section_chapter));
    }
    Ok(())
}

/// Retrieve the docs for each section. Optional side-effects write docs to provided [`Book`] if toggled
/// with bool).
pub fn get_slosh_docs(
    vm: &mut SloshVm,
    md_book: &mut Book,
    write_to_book: bool,
) -> VMResult<BTreeMap<String, BTreeSet<SloshDoc>>> {
    // get docs by section and then make sure the docs are in alphabetical order
    let docs_by_section_unsorted = get_docs_by_section(vm, true);
    // use a BTreeMap so the sections are in alphabetical order as well as the SloshDoc vec.
    let mut docs_by_section = BTreeMap::new();
    for (s, v) in docs_by_section_unsorted {
        docs_by_section.insert(s, v);
    }

    if write_to_book {
        add_forms_to_md_book_part("Slosh Forms".to_string(), md_book, &docs_by_section)?;
    }

    Ok(docs_by_section)
}

/// allows user specific forms to be identified. everything gathered recently. not in provided sections
///
/// provided sections will have a lot of information that the return of get_docs_by_section will have
/// but the new invocation should have more, MUST call a version of the vm that includes init.slosh
///
/// this method is very specific and should be provided a list of sections gathered without init.slosh
/// loaded and then called after its loaded.
pub fn add_user_docs_to_mdbook_less_provided_sections(
    vm: &mut SloshVm,
    md_book: &mut Book,
    provided_sections: BTreeMap<String, BTreeSet<SloshDoc>>,
) -> VMResult<()> {
    let docs_by_section_unsorted = get_docs_by_section(vm, false);
    let mut docs_by_section = BTreeMap::new();
    for (s, all_docs) in docs_by_section_unsorted {
        if !provided_sections.contains_key(&s) {
            let mut set = BTreeSet::new();
            // to set
            for d in all_docs.into_iter() {
                let namespace = GLOBAL_NAMESPACE.to_string();
                if d.namespace != namespace {
                    set.insert(d);
                }
            }

            if !set.is_empty() {
                docs_by_section.insert(s, set);
            }
        }
    }

    add_forms_to_md_book_part(USER_FORMS.to_string(), md_book, &docs_by_section)?;
    Ok(())
}

fn add_forms_to_md_book_part(
    part_title: String,
    md_book: &mut Book,
    docs_by_section: &BTreeMap<String, BTreeSet<SloshDoc>>,
) -> VMResult<()> {
    // First chapter introduces each section and lists all the symbols in that section.
    let all_chapter = build_all_slosh_forms_listing_chapter(&part_title, docs_by_section)?;

    // Add a separator and a title for the new autogenerated section.
    md_book.push_item(BookItem::Separator);
    md_book.push_item(BookItem::PartTitle(part_title));

    md_book.push_item(BookItem::Chapter(all_chapter));

    // Each subsequent chapter is a section with a list of all symbols in that section
    // followed by a complete list of the documentation for each symbol in that section.
    let chapters = build_each_docs_section_chapter(docs_by_section)?;
    for chapter in chapters {
        md_book.push_item(BookItem::Chapter(chapter));
    }
    Ok(())
}

/// load md book and attach doc to it. can also attach pre-processors.
///
/// defaults here for illustrative purposes.
/// ```ignore
///  let renderer = HtmlHandlebars::new();
///  let l = LinkPreprocessor::new();
///  md_book.with_preprocessor(l);
///  let i = IndexPreprocessor::new();
///  md_book.with_preprocessor(i);
///  //md_book.with_renderer(renderer);
///  md_book.preprocess_book(&renderer).unwrap();
/// ```
fn build_doc(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    let next = i.next();
    let path = match next {
        None => Err(VMError::new_vm("takes one argument (filepath)".to_string())),
        Some(v) => match v {
            Value::String(s) => Ok(vm.get_string(*s).to_string()),
            Value::StringConst(i) => Ok(vm.get_interned(*i).to_string()),
            _ => Err(VMError::new_vm(
                "argument must be a string to a valid filepath".to_string(),
            )),
        },
    }?;

    slosh_lib::load_builtins_lisp_less_sloshrc(vm)?;

    let mut md_book = MDBook::load(PathBuf::from(path))
        .map_err(|_e| VMError::new("mdbook", "Unable to load the book at provided path."))?;
    add_forms_and_supplementary_docs(vm, &mut md_book.book)?;
    md_book
        .build()
        .map_err(|e| VMError::new("mdbook", format!("Failed to build book: {e}")))?;
    Ok(Value::Nil)
}

fn make_file(name: impl AsRef<Path>, content: &str) -> io::Result<PathBuf> {
    let filename = format!("src/generated-sections/{}.md", name.as_ref().display()).into();
    let mut file_0 = File::create(&filename)?;
    writeln!(file_0, "{}", content)?;
    File::flush(&mut file_0)?;
    Ok(filename)
}

fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn get_exemptions(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm(
            "get-exemptions: takes no arguments".to_string(),
        ));
    }
    let mut exemptions = EXEMPTIONS.iter().copied().collect::<Vec<&str>>();
    exemptions.sort();
    let exemptions = exemptions
        .iter()
        .map(|x| Value::Symbol(vm.intern(x)))
        .collect::<Vec<Value>>();
    Ok(vm.alloc_vector(exemptions))
}

fn get_globals_sorted(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm(
            "get-globals-sorted: takes no arguments".to_string(),
        ));
    }
    let mut result = BTreeMap::new();
    for g in vm.globals().keys() {
        let sym = Value::Symbol(*g);
        let val: String = sym.display_value(vm);
        result.insert(val, sym);
    }
    let v = result.values().cloned().collect();
    Ok(vm.alloc_vector(v))
}

#[derive(Debug, Clone)]
struct DocSearchQuery {
    query: String,
    use_regex: bool,
    basic: bool,
    fields: HashSet<String>,
    namespace_filter: Option<String>,
    section_filter: Option<String>,
}

impl DocSearchQuery {
    fn new(query: String) -> Self {
        DocSearchQuery {
            query,
            use_regex: false,
            basic: false,
            fields: HashSet::new(),
            namespace_filter: None,
            section_filter: None,
        }
    }

    fn with_regex(mut self) -> Self {
        self.use_regex = true;
        self.basic = false;
        self
    }

    fn with_basic(mut self) -> Self {
        self.basic = true;
        self.use_regex = false;
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
        if self.use_regex {
            if let Ok(re) = Regex::new(&self.query) {
                re.is_match(text)
            } else {
                false
            }
        } else if self.basic {
            basic_match(text, &self.query)
        } else {
            text.to_lowercase().contains(&self.query.to_lowercase())
        }
    }
}

fn basic_match(text: &str, pattern: &str) -> bool {
    let text_lower = text.to_lowercase();
    let pattern_lower = pattern.to_lowercase();
    let mut pattern_chars = pattern_lower.chars();
    let mut current_char = pattern_chars.next();

    for text_char in text_lower.chars() {
        if let Some(p_char) = current_char {
            if text_char == p_char {
                current_char = pattern_chars.next();
            }
        } else {
            return true;
        }
    }

    current_char.is_none()
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
            if !query.matches_text(&section) && section != *section_filter {
                continue;
            }
        }

        for doc in docs {
            // Check namespace filter
            if let Some(ref ns_filter) = query.namespace_filter {
                if !doc.namespace.contains(ns_filter) && doc.namespace != *ns_filter {
                    continue;
                }
            }

            let mut matched = false;

            // Search in symbol name
            if query.should_search_field("name")
                || query.should_search_field("symbol") && query.matches_text(&doc.symbol)
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

    // Parse options if provided as a hash map
    let mut use_styled = true;
    let mut style_options = StyleOptions::default();
    let mut style_map_handle_opt = None;
    if registers.len() > 1 {
        match &registers[1] {
            Value::Map(handle) => {
                // Intern all keys first to avoid borrowing issues
                let regex_key = Value::Keyword(vm.intern_static("regex"));
                let basic_search_key = Value::Keyword(vm.intern_static("basic"));
                let fields_key = Value::Keyword(vm.intern_static("fields"));
                let ns_key = Value::Keyword(vm.intern_static("namespace"));
                let section_key = Value::Keyword(vm.intern_static("section"));
                let styled_key = Value::Keyword(vm.intern_static("styled"));
                let style_key = Value::Keyword(vm.intern_static("style"));

                let map = vm.get_map(*handle);

                // Check for regex option
                if let Some(Value::True) = map.get(vm, regex_key) {
                    query = query.with_regex();
                }

                // Check for basic option
                if let Some(Value::True) = map.get(vm, basic_search_key) {
                    query = query.with_basic();
                }

                // Check for fields option
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

                // Check for namespace filter
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

                // Check for section filter
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

                // Check for styled option
                if let Some(Value::False) = map.get(vm, styled_key) {
                    use_styled = false;
                }

                // Check for style customization
                if let Some(Value::Map(style_handle)) = map.get(vm, style_key) {
                    use_styled = true; // Enable styling if style options are provided
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

    // Process style options after we're done with the initial map
    if let Some(style_handle) = style_map_handle_opt {
        style_options = StyleOptions::from_handle(vm, style_handle);
    }

    // Check if markdown output is requested
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
        // Generate markdown output
        let mut markdown_output = String::new();
        let len = results.len();
        for (i, doc) in results.iter().enumerate() {
            // Don't print to stdout when generating markdown - the numbered list is included in the markdown output instead
            if i > 0 {
                markdown_output.push_str("\n---\n\n");
            }
            // Add a header showing which result this is
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
        // Generate styled string output
        let styled_output = generate_styled_output(vm, &results, &query, &style_options);
        vm.alloc_string(styled_output)
    } else {
        // Generate vector of doc maps
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

fn legacy_report(vm: &mut SloshVm, _registers: &[Value]) -> VMResult<Value> {
    let report = legacy::build_report(vm)?;
    Ok(vm.alloc_string(report))
}

pub fn add_builtins(env: &mut SloshVm) {
    add_builtin(
        env,
        "legacy-report",
        legacy_report,
        "Usage: (legacy-report)

Output as a string the current legacy report, detailing how much of the former sl-sh project has been covered by the new slosh.

Section: doc

Example:
#t
",
    );
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
- :regex #t - Use regular expressions for matching
- :basic #t - Use basic matching (characters must appear in order)
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
(doc-search \"^str-\" {:regex #t})
(doc-search \"core\" {:fields [\"section\"]})
(doc-search \"map\" {:basic #t :section \"core\"})
(doc-search \"string\" {:markdown #t})
(doc-search \"string\" {:styled #t})
(doc-search \"map\" {:style {:highlight-color *fg-red* :use-background #f}})
",
    );

    add_builtin(
        env,
        "legacy_forms",
        legacy_docs::get_legacy_sl_sh_form_syms,
        "Usage: (legacy_forms)

Gets list of all forms that were used in the previous version of sl_sh.

Section: doc

Example:
#t
",
    );

    add_builtin(
        env,
        "get-globals-sorted",
        get_globals_sorted,
        "Usage: (get-globals-sorted)

Return a vector containing all the symbols currently defined globally in sorted order (alphanumerically).

Section: doc
",
    );

    add_builtin(
        env,
        "get-exemptions",
        get_exemptions,
        "Usage: (get-exemptions)

Return a vector containing all the symbols currently exempted from docs
(so the build passes), Ideally this will be 0.

Section: doc
",
    );

    add_builtin(
        env,
        "build-doc",
        build_doc,
        "Usage: (build-doc valid-filepath)

Uses mdbook to build the documentation for the given book.

Section: doc

Example:
#t
",
    );
}

#[cfg(test)]
mod test {
    use super::*;
    use compile_state::state::new_slosh_vm;
    use sl_compiler::Reader;
    use slosh_lib::{
        ENV, load_builtins_lisp_less_sloshrc, run_reader, set_builtins_and_shell_builtins,
        set_initial_load_path,
    };
    use std::collections::BTreeMap;
    use std::ops::DerefMut;
    use tempfile::TempDir;

    #[test]
    fn exec_global_slosh_tests_in_rust() {
        // create home dir
        let tmp_dir = TempDir::with_prefix("test_load_path").unwrap();
        let home_dir = tmp_dir.path().to_str();
        let home_path = home_dir.unwrap().to_string();

        // Need to mask signals in case any tests shell out (use 'sh' or '$sh') otherwise test will hang but appear to finish...
        shell::signals::mask_signals();
        temp_env::with_var("HOME", home_dir, || {
            ENV.with(|env| {
                let mut vm = env.borrow_mut();
                set_builtins_and_shell_builtins(vm.deref_mut());
                set_initial_load_path(vm.deref_mut(), vec![&home_path]);
                load_builtins_lisp_less_sloshrc(vm.deref_mut()).unwrap();

                let mut docs: Vec<SloshDoc> = vec![];
                Namespace::Global
                    .add_docs(&mut docs, &mut vm, true)
                    .unwrap();
                docs.sort();
                for d in docs {
                    if let Some(example) = d.doc_string.example {
                        let symbol = d.symbol;
                        println!("{} ===============================", symbol);
                        println!("Should Run test for: {}", symbol);
                        if symbol == "fs-exists?" {
                            continue;
                        }
                        println!("Code:\n{}", example);
                        let mut reader = Reader::from_string(example, &mut vm, "", 1, 0);
                        let val = run_reader(&mut reader).unwrap();
                        println!("{}:\n{:?}", symbol, val);
                        assert!(!matches!(val, Value::Error(_)));
                        println!("{} ===============================", symbol);
                    }
                }
            })
        });
    }

    #[test]
    fn list_slosh_functions() {
        let mut vm = new_slosh_vm();
        set_builtins_and_shell_builtins(&mut vm);
        for (g, _) in vm.globals() {
            let sym = Value::Symbol(*g);
            let symbol = sym.display_value(&vm);
            let symbol_type = sym.display_type(&vm).to_string();
            println!("{}: {}", symbol, symbol_type);
        }
    }

    #[test]
    fn test_global_slosh_docs_formatted_properly() {
        let mut env = new_slosh_vm();
        set_builtins_and_shell_builtins(&mut env);

        let mut docs: Vec<SloshDoc> = vec![];
        Namespace::Global
            .add_docs(&mut docs, &mut env, true)
            .unwrap();
        for d in docs {
            assert!(
                d.doc_string.usage.is_some(),
                "All global builtins must have a usage section, because it can NOT be inferred from the environment: {}.",
                d.symbol
            );
        }
    }

    lazy_static! {
        static ref REGEX_TEST_CASES: BTreeMap<(bool, &'static str), &'static str> = {
            let mut set = BTreeMap::new();

            set.insert(
                (true, "no whitespace around content"),
                "Usage: (defmacro name doc_string? argument_list body)

Create a macro and bind it to a symbol in the current scope.

Section: core

Example:
(def test-mac-x 2)",
            );

            set.insert(
                (true, "newlines at beginning and end"),
                "
Usage: (defmacro name doc_string? argument_list body)

Create a macro and bind it to a symbol in the current scope.

Section: core

Example:
(def test-mac-x 2)
",
            );

            set.insert(
                (true, "mixed whitespace at beginning and end"),
                "

  Usage: (defmacro name doc_string? argument_list body)

Create a macro and bind it to a symbol in the current scope.

Section: core

Example:
(def test-mac-x 2)


    ",
            );

            set.insert(
                (true, "no usage, no whitespace around content"),
                "Create a macro and bind it to a symbol in the current scope.

Section: core

Example:
(def test-mac-x 2)",
            );

            set.insert(
                (true, "no usage, newlines at beginning and end"),
                "
Create a macro and bind it to a symbol in the current scope.

Section: core

Example:
(def test-mac-x 2)
",
            );

            set.insert(
                (true, "no usage, mixed whitespace at beginning and end"),
                "

   Create a macro and bind it to a symbol in the current scope.

Section: core

Example:
(def test-mac-x 2)


    ",
            );

            set.insert(
                (true, "no whitespace around content"),
                "Usage: (defmacro name doc_string? argument_list body)

    Create a macro and bind it to a symbol in the current scope.

    Section: core

    Example:
    (def test-mac-x 2)",
            );

            set.insert(
                (true, "newlines at beginning and end"),
                "
 Usage: (defmacro name doc_string? argument_list body)

Create a macro and bind it to a symbol in the current scope.

Section:
core

Example:
(def test-mac-x 2)
",
            );

            set.insert(
                (true, "mixed whitespace at beginning and end"),
                "

  Usage: (defmacro name doc_string? argument_list body)

Create a macro and bind it to a symbol in the current scope.

Section: core


    Example:
(def test-mac-x 2)


    ",
            );

            set.insert(
                (true, "no usage, extra whitespace around content"),
                "Create a macro and bind it to a symbol in the current scope.

    Section: core

    Example:
    (def test-mac-x 2)",
            );

            set.insert(
                (
                    false,
                    "no description, no usage, no example, newlines at beginning and end",
                ),
                "

Section:
core
",
            );

            set.insert(
                (true, "no usage, no example, newlines at beginning and end, extra whitespace around content."),
                "
    Create a macro and bind it to a symbol in the current scope.

    Section:
    core

",
            );

            set.insert(
                (true, "no usage, mixed whitespace at beginning and end, extra whitespace around content."),
                "

   Create a macro and bind it to a symbol in the current scope.

Section: core

    Example:
(def test-mac-x 2)


    ",
            );

            set.insert(
                (
                    false,
                    "no usage, no section, mixed whitespace at beginning and end, extra whitespace around content.",
                ),
                "

   Create a macro and bind it to a symbol in the current scope.

core

    Example:
(def test-mac-x 2)


    ",
            );

            set.insert(
                (true, "str-sub test"),
                " Usage: (str-sub string start [length]) -> string\n\n Return a substring from a string given start (0 based) and optional length.\n If length is 0 or not provided produces the r
est of the string from start to\n string end.\n\n Section: string\n\n Example:\n (test::assert-equal \"string\" (str-sub \"stringxxxyyyxxxsome\" 0 6))\n (test::assert-equal \"some\" (str-sub
 \"stringxxxyyyxxxsome\" 15 4))\n (test::assert-equal \"yyy\" (str-sub \"stringxxxyyyxxxsome\" 9 3))\n (test::assert-equal \"some\" (str-sub \"stringxxxyyyxxxsome\" 15))\n",
            );

            set
        };
    }

    #[test]
    fn test_doc_string_regex() {
        for ((result, label), test_case) in REGEX_TEST_CASES.iter() {
            let fake_symbol = Cow::Owned("fake-symbol".to_string());
            match DocStringSection::parse_doc_string(
                fake_symbol,
                test_case.to_string(),
                "".to_string(),
            ) {
                ok @ Ok(_) => {
                    assert_eq!(
                        ok.is_ok(),
                        *result,
                        "Case: {},  Regex succeeded but should not have with test case: {}!",
                        label,
                        test_case
                    );
                }
                ref err @ Err(_) => {
                    assert_ne!(
                        err.is_err(),
                        *result,
                        "Case: {}, Regex failed but should not have, got err ({:?}) with tests case: {}!",
                        label,
                        err,
                        test_case
                    );
                }
            }
        }
    }
}
