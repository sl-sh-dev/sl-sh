extern crate pulldown_cmark;
extern crate pulldown_cmark_to_cmark;

use crate::slosh_eval_lib::SloshArtifacts;
use clap::{Arg, ArgMatches, Command};
use compile_state::state;
use compile_state::state::SloshVm;
use mdbook::book::{Book, BookItem};
use mdbook::errors::Error;
use mdbook::preprocess::{CmdPreprocessor, Preprocessor, PreprocessorContext};
use pulldown_cmark::{Event, Parser, Tag, TagEnd};
use pulldown_cmark_to_cmark::cmark;
use semver::{Version, VersionReq};
use sl_compiler::load_eval;
use slosh_test_lib::docs;
use slvm::{VMError, VMResult, Value};
use std::io;
use std::process;

pub const VERSION_STRING: &str = env!("VERSION_STRING");

fn version(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    if !registers.is_empty() {
        return Err(VMError::new_compile("version: requires no argument"));
    }
    Ok(vm.alloc_string(VERSION_STRING.to_string()))
}

pub fn make_app() -> Command {
    Command::new("mdbook-slosh-eval")
        .about(
            "A mdbook preprocessor which evaluates blocks of code in md fenced as ```slosh blocks",
        )
        .subcommand(
            Command::new("supports")
                .arg(Arg::new("renderer").required(true))
                .about("Check whether a renderer is supported by this preprocessor"),
        )
}

fn main() {
    env_logger::init();
    let matches = make_app().get_matches();

    let preprocessor = SloshArtifacts::new();

    if let Some(sub_args) = matches.subcommand_matches("supports") {
        handle_supports(&preprocessor, sub_args);
    } else if let Err(e) = handle_preprocessing(&preprocessor) {
        log::error!("{}", e);
        process::exit(1);
    }
}

fn handle_preprocessing(pre: &dyn Preprocessor) -> Result<(), Error> {
    let (ctx, book) = CmdPreprocessor::parse_input(io::stdin())?;

    let book_version = Version::parse(&ctx.mdbook_version)?;
    let version_req = VersionReq::parse(mdbook::MDBOOK_VERSION)?;

    if !version_req.matches(&book_version) {
        log::debug!(
            "Warning: The {} plugin was built against version {} of mdbook, \
             but we're being called from version {}",
            pre.name(),
            mdbook::MDBOOK_VERSION,
            ctx.mdbook_version
        );
    }

    let processed_book = pre.run(&ctx, book)?;
    serde_json::to_writer(io::stdout(), &processed_book)?;

    Ok(())
}

fn handle_supports(pre: &dyn Preprocessor, sub_args: &ArgMatches) -> ! {
    let renderer = sub_args
        .get_one::<String>("renderer")
        .expect("Required argument");
    let supported = pre.supports_renderer(renderer);

    // Signal whether the renderer is supported by exiting with 1 or 0.
    if supported {
        process::exit(0);
    } else {
        process::exit(1);
    }
}

mod slosh_eval_lib {
    use super::*;
    use pulldown_cmark::CodeBlockKind;
    use slosh_lib::Reader;
    use toml::Value;

    /// Preprocessor to create slosh artifacts
    ///     - eval slosh code
    ///     - build std lib docs
    ///     - build user docs
    ///     - build supplemental docs
    ///         - legacy documentation
    ///         - rust docs
    pub struct SloshArtifacts;

    impl SloshArtifacts {
        pub fn new() -> SloshArtifacts {
            SloshArtifacts
        }
    }

    fn eval_code_blocks(book: &mut Book, code_block_label: &str) {
        book.for_each_mut(|bi: &mut BookItem| match bi {
            BookItem::Separator | BookItem::PartTitle(_) => {}
            BookItem::Chapter(chapter) => {
                let mut tracking = false;
                let mut buf = String::new();
                let mut events = vec![];
                let mut slosh_code_block_num = 0;
                for event in Parser::new(&chapter.content) {
                    match event {
                        Event::Start(Tag::CodeBlock(ref kind)) => match kind {
                            CodeBlockKind::Fenced(name) if !tracking => {
                                if name.starts_with(code_block_label)
                                    && !name.contains("no-execute")
                                    && !name.contains("ignore")
                                    && !name.contains("skip")
                                {
                                    slosh_code_block_num += 1;
                                    log::debug!(
                                        "File: {}: block #{}",
                                        chapter.name,
                                        slosh_code_block_num,
                                    );
                                    tracking = true;
                                }
                            }
                            CodeBlockKind::Fenced(_) | CodeBlockKind::Indented => {}
                        },
                        Event::Text(ref c) if tracking => {
                            buf += c.as_ref();
                        }
                        Event::End(TagEnd::CodeBlock) if tracking => {
                            let mut vm = state::new_slosh_vm();
                            vm.pause_gc();
                            slosh_test_lib::vm_with_builtins_and_core(&mut vm, false);
                            vm.unpause_gc();
                            let eval = exec_code(&mut vm, buf.clone());

                            let mut first = true;
                            buf += "\n";
                            for line in eval.lines() {
                                if first {
                                    buf += ";; => ";
                                    first = false;
                                } else {
                                    buf += ";;    ";
                                }
                                buf += line;
                                buf += "\n";
                            }
                            tracking = false;
                            log::debug!("```{}\n{}", code_block_label, buf);
                            log::debug!("```");
                            events.push(Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(
                                code_block_label.into(),
                            ))));
                            events.push(Event::Text(buf.clone().into()));
                            events.push(Event::End(TagEnd::CodeBlock));
                            buf.clear();
                        }
                        _ => {}
                    }
                    // If we are tracking some slosh code block we do not want to add the original
                    // code block to the event stream because we are going copy that code block and
                    // append its output to the end and write that to events.
                    if !tracking {
                        events.push(event);
                    }
                }
                let mut buf = String::new();
                let ret = cmark(events.iter(), &mut buf);
                // write new content back to chapter.content.
                match ret {
                    Ok(_) => {
                        chapter.content = buf;
                    }
                    Err(e) => {
                        chapter.content = format!(
                            "# Error in slosh eval pre processor {}.\n\n{}",
                            e, &chapter.content
                        );
                    }
                }
            }
        });
    }

    fn exec_code(vm: &mut SloshVm, code: String) -> String {
        let code = format!(
            r#"(import test)
                (def *prn* "")
                (def *stdout* "")
                (dyn
                    prn
                    (fn (&rest) (set! *prn* (str *prn* &rest)))
                    (do  {}))"#,
            code
        );

        let mut reader = Reader::from_string(code, vm, "", 1, 0);
        let s = load_eval::run_reader(&mut reader)
            .map(|x| x.display_value(vm))
            .map_err(|e| format!("Encountered error: {}", e));
        match s {
            Ok(s) | Err(s) => s,
        }
    }

    fn modify_vm(vm: &mut SloshVm) {
        docs::add_builtins(vm);

        bridge_adapters::add_builtin(
            vm,
            "version",
            version,
            "Return the software version string.",
        );

        let mut reader = Reader::from_string(r#"(load "core.slosh")"#.to_string(), vm, "", 1, 0);
        _ = load_eval::run_reader(&mut reader);
    }

    impl Preprocessor for SloshArtifacts {
        fn name(&self) -> &str {
            "slosh-eval"
        }

        fn run(&self, ctx: &PreprocessorContext, mut book: Book) -> Result<Book, Error> {
            if let Some(slosh_eval_cfg) = ctx.config.get_preprocessor(self.name()) {
                let mut gen_std_lib_docs = false;
                let mut gen_user_docs = false;
                let mut user_files = vec![];
                let mut user_load_paths = vec![];
                let key = "doc-forms";
                if let Some(Value::Table(table)) = slosh_eval_cfg.get(key) {
                    for (k, v) in table {
                        match k.as_str() {
                            "std-lib" => {
                                if let Value::Boolean(b) = v {
                                    gen_std_lib_docs = *b;
                                }
                            }
                            "user" => {
                                if let Value::Boolean(b) = v {
                                    gen_user_docs = *b;
                                }
                            }
                            "user-doc-files" => {
                                if let Value::Array(arr) = v {
                                    for s in arr {
                                        if let Value::String(s) = s {
                                            user_files.push(s.to_string());
                                        }
                                    }
                                }
                            }
                            "user-doc-load-paths" => {
                                if let Value::Array(arr) = v {
                                    for s in arr {
                                        if let Value::String(s) = s {
                                            user_load_paths.push(s.to_string());
                                        }
                                    }
                                }
                            }
                            _ => {
                                panic!("Invalid config key,value pair: {}, {:?}", k, v.as_str())
                            }
                        }
                    }
                }

                if gen_std_lib_docs {
                    log::info!("Evaluate docs.");
                    let mut vm = state::new_slosh_vm();
                    vm.pause_gc();
                    slosh_test_lib::vm_with_builtins_and_core(&mut vm, false);
                    vm.unpause_gc();

                    log::debug!("Add key {}.", key);
                    docs::add_slosh_docs_to_mdbook(&mut vm, &mut book, true)?;
                    log::info!("Docs evaluated.");
                }

                if gen_user_docs {
                    let modify_vm_local = |vm: &mut SloshVm| {
                        modify_vm(vm);

                        if user_files.is_empty() {
                            user_files = vec!["~/.config/slosh/init.slosh".to_string()];
                        }
                        if user_load_paths.is_empty() {
                            user_load_paths = vec!["~/.config/slosh/".to_string()];
                        }

                        let vm = &mut state::new_slosh_vm();
                        vm.pause_gc();
                        slosh_test_lib::vm_with_builtins_and_core(vm, true);
                        vm.unpause_gc();

                        // first get provided sections
                        if let Ok(provided_sections) =
                            docs::add_slosh_docs_to_mdbook(vm, &mut book, false)
                        {
                            vm.pause_gc();
                            // then load init.slosh
                            slosh_test_lib::add_user_builtins(
                                vm,
                                user_load_paths.as_slice(),
                                user_files.as_slice(),
                            );
                            vm.unpause_gc();
                            let _ = docs::add_user_docs_to_mdbook_less_provided_sections(
                                vm,
                                &mut book,
                                provided_sections,
                            );
                        }
                    };
                    let _vm = slosh_lib::run_slosh_print_noop(modify_vm_local, true);
                }

                let key = "code-block-label";
                if let Some(Value::String(block)) = slosh_eval_cfg.get(key) {
                    log::debug!("Use key {}, evaluate code blocks labeled: {}", key, block);
                    eval_code_blocks(&mut book, block);
                }

                let key = "doc-supplementary";
                if let Some(Value::Boolean(b)) = slosh_eval_cfg.get(key) {
                    if *b {
                        let mut vm = state::new_slosh_vm();
                        vm.pause_gc();
                        slosh_test_lib::vm_with_builtins_and_core(&mut vm, false);
                        vm.unpause_gc();

                        log::debug!("Add key {}.", key);
                        _ = docs::link_supplementary_docs(&mut vm, &mut book);
                    }
                }
            }

            Ok(book)
        }

        fn supports_renderer(&self, renderer: &str) -> bool {
            renderer != "not-supported"
        }
    }
}
