use bridge_adapters::add_builtin;
use compile_state::state::SloshVm;
use mdbook::book::{Book, Chapter};
use mdbook::{BookItem, MDBook};
use slvm::{VMError, VMResult, Value};
use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::Path;
use std::path::PathBuf;

pub mod legacy;

// Re-export core doc types from slosh_lib for consumers
pub use slosh_lib::docs::{
    AsMd, DocError, DocResult, DocStringSection, EXEMPTIONS, GLOBAL_NAMESPACE, Namespace,
    SloshDoc, USER_FORMS, get_docs_by_section,
};

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
        "legacy_forms",
        legacy::get_legacy_sl_sh_form_syms,
        "Usage: (legacy_forms)

Gets list of all forms that were used in the previous version of sl_sh.

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
}

#[cfg(test)]
mod test {
    use super::*;
    use compile_state::state::new_slosh_vm;
    use compile_state::state::SloshVmTrait;
    use sl_compiler::Reader;
    use slosh_lib::{
        ENV, load_builtins_lisp_less_sloshrc, run_reader, set_builtins_and_shell_builtins,
        set_initial_load_path,
    };
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
}
