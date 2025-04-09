extern crate pulldown_cmark;
extern crate pulldown_cmark_to_cmark;

use crate::slosh_eval_lib::EvalSlosh;
use clap::{Arg, ArgMatches, Command};
use compile_state::state::new_slosh_vm;
use compile_state::state::SloshVm;
use mdbook::book::{Book, BookItem};
use mdbook::errors::Error;
use mdbook::preprocess::{CmdPreprocessor, Preprocessor, PreprocessorContext};
use pulldown_cmark::{Event, Parser, Tag, TagEnd};
use pulldown_cmark_to_cmark::cmark;
use semver::{Version, VersionReq};
use slosh_test_lib::docs;
use std::cell::RefCell;
use std::io;
use std::process;

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

    let preprocessor = EvalSlosh::new();

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

thread_local! {
    /// Env (job control status, etc) for the shell.
    pub static ENV: RefCell<SloshVm> = RefCell::new(new_slosh_vm());
}

mod slosh_eval_lib {
    use super::*;
    use pulldown_cmark::CodeBlockKind;
    use slosh_lib::Reader;
    use toml::Value;

    /// Preprocessor to evaluate slosh code
    pub struct EvalSlosh;

    impl EvalSlosh {
        pub fn new() -> EvalSlosh {
            EvalSlosh
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
                            let mut vm = slosh_test_lib::new_slosh_vm_with_builtins_and_core();
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
        let s = slosh_test_lib::run_reader(&mut reader)
            .map(|x| x.display_value(&vm))
            .map_err(|e| format!("Encountered error: {}", e));
        match s {
            Ok(s) | Err(s) => s,
        }
    }

    impl Preprocessor for EvalSlosh {
        fn name(&self) -> &str {
            "slosh-eval"
        }

        fn run(&self, ctx: &PreprocessorContext, mut book: Book) -> Result<Book, Error> {
            if let Some(slosh_eval_cfg) = ctx.config.get_preprocessor(self.name()) {
                let key = "code-block-label";
                if let Some(Value::String(block)) = slosh_eval_cfg.get(key) {
                    log::debug!("Use key {}, evaluate code blocks labeled: {}", key, block);
                    eval_code_blocks(&mut book, &block);
                }

                let key = "doc-forms";
                if let Some(Value::Boolean(b)) = slosh_eval_cfg.get(key) {
                    if *b {
                        log::info!("Evaluate docs.");
                        let mut vm = slosh_test_lib::new_slosh_vm_with_builtins_and_core();
                        docs::add_builtins(&mut vm);

                        log::debug!("Add key {}.", key);
                        docs::add_slosh_docs_to_mdbook(&mut vm, &mut book, true)?;
                        log::info!("Docs evaluated.");
                    }
                }

                let key = "user-doc-forms";
                if let Some(Value::Boolean(b)) = slosh_eval_cfg.get(key) {
                    if *b {
                        let mut vm = slosh_test_lib::new_slosh_vm_with_builtins_and_core();
                        docs::add_builtins(&mut vm);

                        // first get provided sections
                        let provided_sections = docs::add_slosh_docs_to_mdbook(&mut vm, &mut book, false)?;

                        // then load init.slosh
                        slosh_test_lib::add_user_builtins(&mut vm);
                        docs::add_user_docs_to_mdbook_less_provided_sections(
                            &mut vm,
                            &mut book,
                            provided_sections,
                        )?;
                    }
                }

                let key = "doc-supplementary";
                if let Some(Value::Boolean(b)) = slosh_eval_cfg.get(key) {
                    if *b {
                        let mut env = slosh_test_lib::new_slosh_vm_with_builtins_and_core();
                        let vm = &mut env;
                        docs::add_builtins(vm);

                        log::debug!("Add key {}.", key);
                        _ = docs::link_supplementary_docs(vm, &mut book);
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
