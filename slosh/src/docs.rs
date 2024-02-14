use builtins::add_builtin;
use builtins::types::SlFrom;
use compile_state::state::{SloshVm, SloshVmTrait};
use lazy_static::lazy_static;
use regex::{Regex, RegexBuilder};
use slvm::VMErrorObj::Message;
use slvm::{Interned, VMError, VMResult, Value};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
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
        RegexBuilder::new(r#"(Usage:(.+?)$\n\n|\s*)(.*)\n\n^Section:(.+?)$(\n\n^Example:\n(.*)|\s*)"#)
        //RegexBuilder::new(r#"Usage:(.+?)$\n\n(.*)\n\n^Section:(.+?)$(\n\n^Example:\n(.*)|\s*)"#)
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
        exemption_set.insert("eval");
        exemption_set.insert("*int-bits*");
        exemption_set.insert("get-prop");
        exemption_set.insert("expand-macro");

        // slosh specific colors
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
                for (g, slot) in vm.globals().clone() {
                    let slosh_doc = SloshDoc::new(g, slot as u32, vm, self.clone());
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
    usage: String,
    description: String,
    section: String,
    example: Option<String>,
}

impl Display for DocStringSection {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let example = self
            .example
            .clone()
            .map(|example| format!("Example:\n{}", example))
            .unwrap_or_default();
        write!(
            f,
            "Usage: {usage}\n\n{description}Section: {section}\n\n{example}",
            usage = self.usage,
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
            .map_or(None, |x| {
                if let Value::String(h) = x {
                    Some(vm.get_string(h).to_string())
                } else {
                    None
                }
            })
            .unwrap_or_default();
        Self::parse_doc_string(Cow::Owned(sym_str), raw_doc_string)
    }

    /// Given the rules for parsing slosh docstrings, parse one! See [`DOC_REGEX`]
    /// for the specification.
    pub fn parse_doc_string(
        symbol: Cow<'_, String>,
        raw_doc_string: String,
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
        let usage = cap
            .get(1)
            .ok_or_else(|| DocError::DocStringMustStartWithUsage {
                symbol: symbol.to_owned().to_string(),
            })
            .map(|x| x.as_str().trim().to_string())?;
        let description = cap
            .get(2)
            .ok_or_else(|| DocError::DocStringMissingSection {
                symbol: symbol.to_owned().to_string(),
                section: "Description".to_string(),
            })
            .map(|x| x.as_str().to_string())?;
        let section = cap
            .get(3)
            .ok_or_else(|| DocError::DocStringMissingSection {
                symbol: symbol.to_owned().to_string(),
                section: "Section".to_string(),
            })
            .map(|x| x.as_str().trim().to_string())?;
        let example = cap.get(5).map(|x| x.as_str().trim().to_string());

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
    fn new(g: Interned, slot: u32, vm: &mut SloshVm, namespace: Namespace) -> DocResult<SloshDoc> {
        let sym = Value::Symbol(g);
        let doc_string = DocStringSection::from_symbol(slot, sym, vm)?;
        let symbol = sym.display_value(&vm);
        let symbol_type = sym.display_type(&vm).to_string();
        Ok(SloshDoc {
            symbol,
            symbol_type,
            namespace,
            doc_string,
        })
    }

    /// Provide the fully
    pub fn fully_qualified_name(&self) -> String {
        self.namespace.to_string() + "::" + self.symbol.as_ref()
    }
}

enum DocError {
    NoDocString { symbol: String },
    DocStringMissingSection { symbol: String, section: String },
    DocStringMustStartWithUsage { symbol: String },
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
            DocError::NoDocString { symbol } => {
                format!(
                    "Either documentation provided does not conform to conventional layout or no documentation string provided for symbol {symbol} all slosh functions written in Rust must have a valid documentation string."
                )
            }
            DocError::ExemptFromProperDocString { symbol } => {
                format!(
                    "No documentation needed for provided symbol {symbol}."
                )
            }
            DocError::DocStringMissingSection { symbol, section } => {
                format!("Invalid documentation string for symbol {symbol}, missing required section {section:?}")
            }
            DocError::DocStringMustStartWithUsage { symbol } => {
                format!(
                    "Invalid documentation string for symbol {symbol}, first line must start with \"Usage:\""
                )
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

impl SlFrom<SloshDoc> for HashMap<Value, Value> {
    fn sl_from(value: SloshDoc, vm: &mut SloshVm) -> VMResult<Self> {
        let mut map;
        if let Some(example) = value.doc_string.example {
            map = Self::with_capacity(4);
            let example_const = Value::StringConst(vm.intern_static(EXAMPLE));
            let example_text = vm.alloc_string(example);
            map.insert(example_const, example_text);
        } else {
            map = Self::with_capacity(3)
        }
        let usage_const = Value::StringConst(vm.intern_static(USAGE));
        let usage_text = vm.alloc_string(value.doc_string.usage);
        map.insert(usage_const, usage_text);

        let section_const = Value::StringConst(vm.intern_static(SECTION));
        let section_text = vm.alloc_string(value.doc_string.section);
        map.insert(section_const, section_text);

        let desc_const = Value::StringConst(vm.intern_static(DESCRIPTION));
        let desc_text = vm.alloc_string(value.doc_string.description);
        map.insert(desc_const, desc_text);
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
        (Some(Value::Symbol(g)), None) => {
            let slot = vm.global_intern_slot(*g);
            if let Some(slot) = slot {
                match SloshDoc::new(*g, slot, vm, Namespace::Global) {
                    Ok(slosh_doc) => Value::sl_from(slosh_doc, vm),
                    Err(DocError::ExemptFromProperDocString { symbol: _ }) => {
                        Ok(vm.alloc_map(HashMap::new()))
                    }
                    Err(e) => Err(VMError::from(e)),
                }
            } else {
                Err(VMError::new_vm(
                    "first form must evaluate to a symbol".to_string(),
                ))
            }
        }
        _ => Err(VMError::new_vm("takes one argument (symbol)".to_string())),
    }
}

pub fn add_builtins(env: &mut SloshVm) {
    add_builtin(
        env,
        "doc-map",
        doc_map,
        "Usage: (doc-map symbol)

Returns documentation for given symbol as map. Keyword is a documentation fragment
(usage, section, description, example) and value is text describing given fragment.

Section: global

Example:
#t
",
    );
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::set_builtins;
    use compile_state::state::new_slosh_vm;

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
            println!("{}", d);
        }
    }
}
