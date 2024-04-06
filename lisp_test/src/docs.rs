use bridge_adapters::add_builtin;
use bridge_adapters::lisp_adapters::SlFrom;
use compile_state::state::{SloshVm, SloshVmTrait};
use lazy_static::lazy_static;
use regex::{Regex, RegexBuilder};
use slvm::VMErrorObj::Message;
use slvm::{Interned, VMError, VMResult, Value};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::string::ToString;

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

#[cfg(test)]
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
        let sym_str = sym.display_value(&vm);
        let raw_doc_string = vm
            .get_global_property(slot, docstring_key)
            .map_or(None, |x| match x {
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
        let cap = DOC_REGEX.captures(raw_doc_string.as_str()).ok_or_else(|| {
            if EXEMPTIONS.contains(symbol.as_str()) {
                DocError::ExemptFromProperDocString {
                    symbol: symbol.to_owned().to_string(),
                }
            } else {
                DocError::NoDocString {
                    symbol: symbol.to_owned().to_string(),
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
                symbol: symbol.to_owned().to_string(),
                section: "Description".to_string(),
            })
            .map(|x| x.as_str().to_string())?;
        let section = cap
            .get(4)
            .ok_or_else(|| DocError::DocStringMissingSection {
                symbol: symbol.to_owned().to_string(),
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

#[derive(Eq, Debug)]
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
        self.fully_qualified_name()
            .partial_cmp(&other.fully_qualified_name())
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
            let symbol = sym.display_value(&vm);
            let symbol_type = sym.display_type(&vm).to_string();
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
        let map = HashMap::sl_from(value, vm)?.into();
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

Section: core

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

Section: core
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
