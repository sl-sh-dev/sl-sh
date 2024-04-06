use bridge_adapters::add_builtin;
use bridge_adapters::lisp_adapters::SlFrom;
use compile_state::state::{SloshVm, SloshVmTrait};
use lazy_static::lazy_static;
use mdbook::book::Chapter;
use mdbook::{BookItem, MDBook};
use regex::{Regex, RegexBuilder};
use sl_compiler::load_eval::run_reader;
use sl_compiler::Reader;
use slvm::VMErrorObj::Message;
use slvm::{Interned, VMError, VMResult, Value};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::string::ToString;
use std::{fs, io};

const USAGE: &str = "usage";
const DESCRIPTION: &str = "description";
const SECTION: &str = "section";
const EXAMPLE: &str = "example";

lazy_static! {
    static ref DOC_REGEX: Regex =
    //TODO PC optional Usage section OR must be auto generated?
    // legacy/builtins.rs L#937
        RegexBuilder::new(r#"(\s*?Usage:(.+?)$\n\n|\s*?)(\S{1}.*)\n\n\s*Section:(.+?)$(\n\n\s*Example:\n(.*)|\s*)"#)
            .multi_line(true)
            .dot_matches_new_line(true)
            .crlf(true)
            .build()
            .unwrap();
    static ref EXEMPTIONS: HashSet<&'static str> = {
        let mut exemption_set = HashSet::new();
        exemption_set.insert("version");
        exemption_set.insert("env");
        exemption_set.insert("sh");
        exemption_set.insert("$sh");
        exemption_set.insert("this-fn");
        exemption_set.insert("cons");
        exemption_set.insert("list-append");
        exemption_set.insert("/=");
        exemption_set.insert("eq?");
        exemption_set.insert("equal?");
        exemption_set.insert("type");
        exemption_set.insert("err");
        exemption_set.insert("call/cc");
        exemption_set.insert("defer");
        exemption_set.insert("on-error");
        exemption_set.insert("while");
        exemption_set.insert("doc-string");
        exemption_set.insert("get");
        exemption_set.insert("mk-err");
        exemption_set.insert("err?");
        exemption_set.insert("ok?");
        exemption_set.insert("return");
        exemption_set.insert("*euid*");
        exemption_set.insert("*last-status*");
        exemption_set.insert("set-prop");
        exemption_set.insert("sizeof-heap-object");
        exemption_set.insert("*int-min*");
        exemption_set.insert("gensym");
        exemption_set.insert("*uid*");
        exemption_set.insert("*int-max*");
        exemption_set.insert("prn");
        exemption_set.insert("pr");
        exemption_set.insert("sizeof-value");
        exemption_set.insert("dump-regs");
        exemption_set.insert("dasm");
        exemption_set.insert("load");
        exemption_set.insert("read-all");
        exemption_set.insert("eval");
        exemption_set.insert("*int-bits*");
        exemption_set.insert("get-prop");
        exemption_set.insert("expand-macro");

        // slosh specific colors
        exemption_set.insert("get-rgb-seq");
        exemption_set.insert("bg-color-rgb");
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
        exemption_set.insert("block");

        // in runtime
        exemption_set.insert("#<remember-me>");

        exemption_set
    };
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
enum Namespace {
    Global,
    // Can be adapted when namespaces are added.
    // Other(String),
}

impl Display for Namespace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Namespace::Global => "global".to_string(),
            }
        )
    }
}

#[cfg(any(test, feature = "lisp-test"))]
impl Namespace {
    fn add_docs(&self, docs: &mut Vec<SloshDoc>, vm: &mut SloshVm) -> DocResult<()> {
        match self {
            Namespace::Global => {
                for g in vm.globals().clone().keys() {
                    let slosh_doc = SloshDoc::new(*g, vm, self.clone());
                    match slosh_doc {
                        Ok(slosh_doc) => {
                            docs.push(slosh_doc);
                        }
                        Err(e) => match e {
                            DocError::ExemptFromProperDocString { symbol } => {
                                eprintln!("Exempt from proper doc string: {symbol}");
                            }
                            _ => {
                                return Err(e);
                            }
                        },
                    }
                }
            }
        }
        docs.sort();
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
            "{usage}{description}Section: {section}\n\n{example}",
            usage = usage,
            description = self.description,
            section = self.section,
            example = example,
        )
    }
}

impl DocStringSection {
    pub fn from_symbol(slot: u32, sym: Value, vm: &mut SloshVm) -> DocResult<DocStringSection> {
        let docstring_key = vm.intern_static("doc-string");
        let sym_str = sym.display_value(vm);
        let raw_doc_string = vm
            .get_global_property(slot, docstring_key)
            .and_then(|x| match x {
                Value::String(h) => Some(vm.get_string(h).to_string()),
                Value::StringConst(i) => Some(vm.get_interned(i).to_string()),
                _ => None,
            })
            // return default empty string and have parse_doc_string handle error if no doc provided.
            .unwrap_or_default();
        let backup_usage = slosh_lib::usage(vm, slot, &sym);
        Self::parse_doc_string(Cow::Owned(sym_str), raw_doc_string, backup_usage)
    }

    /// Given the rules for parsing slosh docstrings, parse one! See [`DOC_REGEX`]
    /// for the specification.
    pub fn parse_doc_string(
        symbol: Cow<'_, String>,
        raw_doc_string: String,
        backup_usage: String,
    ) -> DocResult<DocStringSection> {
        let sym_text = symbol.as_str().to_string();
        let cap = DOC_REGEX.captures(raw_doc_string.as_str()).ok_or_else(|| {
            if EXEMPTIONS.contains(symbol.as_str()) {
                DocError::ExemptFromProperDocString {
                    symbol: sym_text.clone(),
                }
            } else {
                DocError::NoDocString {
                    symbol: sym_text.clone(),
                }
            }
        })?;
        let mut usage = cap.get(2).map(|x| x.as_str().trim().to_string());
        if usage.is_none() && !backup_usage.trim().is_empty() {
            usage = Some(backup_usage);
        }
        let description = cap
            .get(3)
            .ok_or_else(|| DocError::DocStringMissingSection {
                symbol: sym_text.clone(),
                section: "Description".to_string(),
            })
            .map(|x| x.as_str().to_string())?;
        let section = cap
            .get(4)
            .ok_or_else(|| DocError::DocStringMissingSection {
                symbol: sym_text,
                section: "Section".to_string(),
            })
            .map(|x| x.as_str().trim().to_string())?;
        let example = cap.get(6).map(|x| x.as_str().trim().to_string());

        Ok(DocStringSection {
            usage,
            description,
            section,
            example,
        })
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
            content = content + &format!("**Usage:** {}\n\n", usage);
        }
        //content = content + &format!("section: {}\n", docs.doc_string.section);
        content = content + &format!("{}\n", self.doc_string.description);
        if let Some(example) = &self.doc_string.example {
            content = content + &format!("Example: \n ```");
            content = content + &format!("{}", example);
            content = content + &format!("\n``` \n");
        } else {
            content = content + "No Examples\n";
        }
        content
    }
}

#[derive(Eq, Debug, Clone)]
struct SloshDoc {
    symbol: String,
    symbol_type: String,
    namespace: Namespace,
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
        self.fully_qualified_name()
            .eq_ignore_ascii_case(&other.fully_qualified_name())
    }
}

impl PartialOrd for SloshDoc {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SloshDoc {
    fn cmp(&self, other: &Self) -> Ordering {
        self.fully_qualified_name()
            .cmp(&other.fully_qualified_name())
    }
}

impl SloshDoc {
    fn new(g: Interned, vm: &mut SloshVm, namespace: Namespace) -> DocResult<SloshDoc> {
        let sym = Value::Symbol(g);
        let slot = vm.global_intern_slot(g);
        if let Some(slot) = slot {
            let doc_string = DocStringSection::from_symbol(slot, sym, vm)?;
            let symbol = sym.display_value(vm);
            let symbol_type = sym.display_type(vm).to_string();
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

    /// Return an empty documentation map.
    fn nil_doc_map(vm: &mut SloshVm) -> HashMap<Value, Value> {
        let mut map = HashMap::with_capacity(4);
        insert_nil_section(&mut map, USAGE, vm);
        insert_nil_section(&mut map, SECTION, vm);
        insert_nil_section(&mut map, DESCRIPTION, vm);
        insert_nil_section(&mut map, EXAMPLE, vm);
        map
    }
}

enum DocError {
    NoSymbol { symbol: String },
    NoDocString { symbol: String },
    DocStringMissingSection { symbol: String, section: String },
    ExemptFromProperDocString { symbol: String },
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
            DocError::ExemptFromProperDocString { symbol } => {
                format!(
                    "No documentation exists for provided symbol {symbol}, this should be rectified."
                )
            }
            DocError::DocStringMissingSection { symbol, section } => {
                format!("Invalid documentation string for symbol {symbol}, missing required section {section:?}")
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
            obj: Message(value.to_string()),
        }
    }
}

type DocResult<T> = Result<T, DocError>;

fn insert_section(
    map: &mut HashMap<Value, Value>,
    key: &'static str,
    value: String,
    vm: &mut SloshVm,
) {
    let key_const = Value::Keyword(vm.intern_static(key));
    let value_text = vm.alloc_string(value);
    map.insert(key_const, value_text);
}

fn insert_nil_section(map: &mut HashMap<Value, Value>, key: &'static str, vm: &mut SloshVm) {
    let key_const = Value::Keyword(vm.intern_static(key));
    map.insert(key_const, Value::Nil);
}

impl SlFrom<SloshDoc> for HashMap<Value, Value> {
    fn sl_from(value: SloshDoc, vm: &mut SloshVm) -> VMResult<Self> {
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
    fn sl_from(value: SloshDoc, vm: &mut SloshVm) -> VMResult<Self> {
        let map = HashMap::sl_from(value, vm)?;
        Ok(vm.alloc_map(map))
    }
}

fn doc_map(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    match (i.next(), i.next()) {
        (Some(Value::Symbol(g)), None) => match SloshDoc::new(*g, vm, Namespace::Global) {
            Ok(slosh_doc) => Value::sl_from(slosh_doc, vm),
            Err(DocError::ExemptFromProperDocString { symbol: _ }) => {
                let map = SloshDoc::nil_doc_map(vm);
                Ok(vm.alloc_map(map))
            }
            Err(e) => Err(VMError::from(e)),
        },
        _ => Err(VMError::new_vm("takes one argument (symbol)".to_string())),
    }
}

/// Each doc has a tag in its `Section:` definition by convention that logically groups functions.
/// Using a HashMap store the section tags as keys and add all slosh docs from to a vector as a value
/// corresponding to its section.
fn get_docs_by_section(vm: &mut SloshVm) -> HashMap<String, Vec<SloshDoc>> {
    let mut docs_by_section: HashMap<String, Vec<SloshDoc>> = HashMap::new();
    let mut docs: Vec<SloshDoc> = vec![];
    Namespace::Global.add_docs(&mut docs, vm).unwrap();
    docs.sort();
    for d in docs {
        let d = d.clone();
        let section = d.doc_string.section.clone();
        docs_by_section
            .entry(section)
            .or_insert_with(|| vec![])
            .push(d);
    }
    docs_by_section
}

fn build_symbols_list(
    docs_by_section: &BTreeMap<String, Vec<SloshDoc>>,
    namer: fn(&String, &SloshDoc) -> String,
) -> BTreeMap<String, String> {
    let mut map = BTreeMap::new();
    for (section, v) in docs_by_section.iter() {
        let mut list = "".to_string();
        let len = v.len();
        for (i, docs) in v.iter().enumerate() {
            let name = namer(section, docs);
            list = list + &name;

            if i + 1 != len {
                list = list + ", ";
            }
        }
        list = list + "\n";
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

fn name_for_all_page(section: &String, doc: &SloshDoc) -> String {
    let (s, t) = symbol_and_capitalized_symbol(doc);
    format!("[{}]({section}.html#{})", s, t)
}

fn name_for_section_page(_section: &String, doc: &SloshDoc) -> String {
    let (s, t) = symbol_and_capitalized_symbol(doc);
    format!("[{}](#{})", s, t)
}

fn build_all_slosh_forms_listing_chapter(
    docs_by_section: &BTreeMap<String, Vec<SloshDoc>>,
) -> VMResult<Chapter> {
    let mut all_content = "# Slosh Forms\n\n".to_string();

    let sections_len = docs_by_section.keys().len();
    let mut list = "List of sections: \n\n".to_string();
    for (i, section) in docs_by_section.keys().enumerate() {
        list = list + &format!("[{}](#section-{})", section, section);
        if i + 1 != sections_len {
            list = list + ", ";
        }
    }
    list = list + "\n\n";
    all_content = all_content + &list;

    let list = build_symbols_list(docs_by_section, name_for_all_page);
    for (section, content) in list {
        let header = format!("## Section: {} \n\n", section);
        all_content = all_content + &header + &content;
    }

    let p = make_file("all", &all_content)
        .map_err(|e| VMError::new_vm(&format!("Failed to write to file: {e}.")))?;

    Ok(Chapter::new("All", all_content, p, vec![]))
}

fn build_each_docs_section_chapter(
    docs_by_section: &BTreeMap<String, Vec<SloshDoc>>,
) -> VMResult<Vec<Chapter>> {
    let mut sections_as_md_text: BTreeMap<String, String> = BTreeMap::new();
    let lists = build_symbols_list(docs_by_section, name_for_section_page);
    for (section, v) in docs_by_section {
        let mut content = lists.get(section).unwrap().to_owned() + "\n";
        for docs in v {
            content = content + &docs.as_md();
        }
        content = content + "\n";

        sections_as_md_text.insert(section.to_string(), content.clone());
    }

    let mut chapters = vec![];
    for (section, list) in sections_as_md_text.iter() {
        let mut content = format!("## {}\n\n", section);

        let file = format!("src/section-docs/{}.md", section);
        // If there is a section file header include it for preprocessing.
        if fs::metadata(&file).is_ok() {
            content = content + &format!("{{{{ #include section-docs/{}.md }}}}\n\n\n", section);
        }

        let header = "List of symbols: \n".to_string();
        content = content + &header + &list;

        let path = make_file(section, &content)
            .map_err(|e| VMError::new_vm(&format!("Failed to write to file: {e}.")))?;
        let capped = capitalize_first(section);
        let section_chapter = Chapter::new(&capped, content.clone(), &path, vec![]);
        chapters.push(section_chapter);
    }
    Ok(chapters)
}

fn build_doc(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut i = registers.iter();
    let next = i.next();
    let path = match next {
        None => Err(VMError::new_vm("takes one argument (filepath)".to_string())),
        Some(v) => match v {
            Value::String(s) => Ok(vm.get_string(*s)),
            Value::StringConst(i) => Ok(vm.get_interned(*i)),
            _ => Err(VMError::new_vm(
                "argument must be a string to a valid filepath".to_string(),
            )),
        },
    }?;

    let path_buf: PathBuf = path.into();
    let mut md = MDBook::load(path_buf.clone()).expect("Unable to load the book");

    let mut reader = Reader::from_string(r#"(load "core.slosh")"#.to_string(), vm, "", 1, 0);
    _ = run_reader(&mut reader).unwrap();

    // Add a separator and a title for the new autogenerated section.
    md.book.push_item(BookItem::Separator);
    md.book
        .push_item(BookItem::PartTitle("Slosh Forms".to_string()));

    // get docs by section and then make sure the docs are in alphabetical order
    let docs_by_section_unsorted = get_docs_by_section(vm);
    // use a BTreeMap so the sections are in alphabetical order as well as the SloshDoc vec.
    let mut docs_by_section = BTreeMap::new();
    for (s, mut v) in docs_by_section_unsorted {
        v.sort();
        docs_by_section.insert(s, v);
    }

    // First chapter introduces each section and lists all the symbols in that section.
    let all_chapter = build_all_slosh_forms_listing_chapter(&docs_by_section)?;
    md.book.push_item(BookItem::Chapter(all_chapter));

    // Each subsequent chapter is a section with a list of all of the symbols in that section
    // followed by a complete list of the documentation for each symbol in that section.
    let chapters = build_each_docs_section_chapter(&docs_by_section)?;
    for chapter in chapters {
        md.book.push_item(BookItem::Chapter(chapter));
    }

    md.build().expect("Building failed");
    Ok(Value::Nil)
}

fn make_file(name: impl AsRef<Path>, content: &str) -> io::Result<PathBuf> {
    //TODO I do not understand why have to both pass the file contents and have a md file
    // and if I do not have the md file i get a no parents error?
    let filename = format!("{}.md", name.as_ref().display()).into();
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

fn get_globals_sorted(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_vm(
            "get_globals_sorted: takes no arguments".to_string(),
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

pub fn add_builtins(env: &mut SloshVm) {
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
        "build-doc",
        build_doc,
        "Usage: (build-doc valid-filepath)

Uses mdbook to build the documentation for the given book.

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
}

#[cfg(test)]
mod test {
    use super::*;
    use compile_state::state::new_slosh_vm;
    use sl_compiler::Reader;
    use slosh_lib::{run_reader, set_builtins, set_initial_load_path, ENV};
    use std::collections::BTreeMap;
    use std::ops::DerefMut;
    use tempdir::TempDir;

    #[test]
    fn exec_global_slosh_tests_in_rust() {
        // create home dir
        let tmp_dir = TempDir::new("test_load_path").unwrap();
        let home_dir = tmp_dir.path().to_str();
        let home_path = home_dir.unwrap().to_string();

        temp_env::with_var("HOME", home_dir, || {
            ENV.with(|env| {
                let mut vm = env.borrow_mut();
                set_builtins(vm.deref_mut());
                set_initial_load_path(vm.deref_mut(), vec![&home_path]);
                let mut reader =
                    Reader::from_string(r#"(load "core.slosh")"#.to_string(), &mut vm, "", 1, 0);
                _ = run_reader(&mut reader).unwrap();

                let mut docs: Vec<SloshDoc> = vec![];
                Namespace::Global.add_docs(&mut docs, &mut vm).unwrap();
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
        set_builtins(&mut vm);
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
        set_builtins(&mut env);

        let mut docs: Vec<SloshDoc> = vec![];
        Namespace::Global.add_docs(&mut docs, &mut env).unwrap();
        for d in docs {
            assert!(d.doc_string.usage.is_some(), "All global builtins must have a usage section, because it can NOT be inferred from the environment: {}.", d.symbol);
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
