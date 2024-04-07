extern crate pulldown_cmark;
extern crate pulldown_cmark_to_cmark;

use crate::nop_lib::Nop;
use clap::{Arg, ArgMatches, Command};
use mdbook::book::{Book, BookItem};
use mdbook::errors::Error;
use mdbook::preprocess::{CmdPreprocessor, Preprocessor, PreprocessorContext};
use pulldown_cmark::{Event, Parser, Tag, TagEnd};
use pulldown_cmark_to_cmark::cmark;
use semver::{Version, VersionReq};
use std::io;
use std::process;

pub fn make_app() -> Command {
    Command::new("mdbook-nop")
        .about("A mdbook preprocessor which does precisely nothing")
        .subcommand(
            Command::new("supports")
                .arg(Arg::new("renderer").required(true))
                .about("Check whether a renderer is supported by this preprocessor"),
        )
}

fn main() {
    env_logger::init();
    let matches = make_app().get_matches();

    // Users will want to construct their own preprocessor here
    let preprocessor = Nop::new();

    if let Some(sub_args) = matches.subcommand_matches("supports") {
        handle_supports(&preprocessor, sub_args);
    } else if let Err(e) = handle_preprocessing(&preprocessor) {
        eprintln!("{}", e);
        process::exit(1);
    }
}

fn handle_preprocessing(pre: &dyn Preprocessor) -> Result<(), Error> {
    let (ctx, book) = CmdPreprocessor::parse_input(io::stdin())?;

    let book_version = Version::parse(&ctx.mdbook_version)?;
    let version_req = VersionReq::parse(mdbook::MDBOOK_VERSION)?;

    if !version_req.matches(&book_version) {
        eprintln!(
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

/// The actual implementation of the `Nop` preprocessor. This would usually go
/// in your main `lib.rs` file.
mod nop_lib {
    use super::*;
    use pulldown_cmark::CodeBlockKind;
    use slosh_lib::{new_slosh_vm_with_builtins, run_reader, Reader};

    /// A no-op preprocessor.
    pub struct Nop;

    impl Nop {
        pub fn new() -> Nop {
            Nop
        }
    }

    impl Preprocessor for Nop {
        fn name(&self) -> &str {
            "nop-preprocessor"
        }

        fn run(&self, ctx: &PreprocessorContext, mut book: Book) -> Result<Book, Error> {
            // In testing we want to tell the preprocessor to blow up by setting a
            // particular config value
            if let Some(nop_cfg) = ctx.config.get_preprocessor(self.name()) {
                if nop_cfg.contains_key("blow-up") {
                    anyhow::bail!("Boom!!1!");
                }
            }

            book.for_each_mut(|bi: &mut BookItem| match bi {
                BookItem::Separator | BookItem::PartTitle(_) => {}
                BookItem::Chapter(chapter) => {
                    let mut tracking = false;
                    let mut buf = String::new();
                    let mut events = vec![];
                    for event in Parser::new(&chapter.content) {
                        match event {
                            Event::Start(Tag::CodeBlock(ref kind)) => match kind {
                                CodeBlockKind::Fenced(name) if !tracking => {
                                    if name.starts_with("slosh") && !name.contains("no-execute") {
                                        log::debug!(
                                            "Evaluate slosh code in chapter: {}",
                                            chapter.name
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
                                let eval = exec_code(buf.clone());
                                buf += "\n=> ";
                                buf += &eval;
                                buf += "\n";
                                tracking = false;
                                log::debug!("New Code Block: {}", buf);
                                events.push(Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(
                                    "slosh".into(),
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
                    cmark(events.iter(), &mut buf).unwrap();
                    chapter.content = buf;
                }
            });

            Ok(book)
        }

        fn supports_renderer(&self, renderer: &str) -> bool {
            renderer != "not-supported"
        }
    }

    fn exec_code(code: String) -> String {
        let mut vm = new_slosh_vm_with_builtins();

        //TODO use dyn to make prn do something else?
        // write to a buffer or something?
        let mut reader =
            Reader::from_string(r#"(load "core.slosh")"#.to_string(), &mut vm, "", 1, 0);
        let code = format!(
            r#"(def *prn* "")
               (dyn prn (fn (&rest) (set! *prn* (str *prn* &rest))) (do {}))"#,
            code
        );
        _ = run_reader(&mut reader).unwrap();
        let mut reader = Reader::from_string(code, &mut vm, "", 1, 0);
        let s = run_reader(&mut reader)
            .map(|x| x.display_value(&mut vm))
            .map_err(|e| format!("Encountered error: {}", e));
        match s {
            Ok(s) | Err(s) => s,
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn nop_preprocessor_run() {
            let input_json = r##"[
                {
                    "root": "/path/to/book",
                    "config": {
                        "book": {
                            "authors": ["AUTHOR"],
                            "language": "en",
                            "multilingual": false,
                            "src": "src",
                            "title": "TITLE"
                        },
                        "preprocessor": {
                            "nop": {}
                        }
                    },
                    "renderer": "html",
                    "mdbook_version": "0.4.21"
                },
                {
                    "sections": [
                        {
                            "Chapter": {
                                "name": "Chapter 1",
                                "content": "# Chapter 1\n",
                                "number": [1],
                                "sub_items": [],
                                "path": "chapter_1.md",
                                "source_path": "chapter_1.md",
                                "parent_names": []
                            }
                        }
                    ],
                    "__non_exhaustive": null
                }
            ]"##;
            let input_json = input_json.as_bytes();

            let (ctx, book) = mdbook::preprocess::CmdPreprocessor::parse_input(input_json).unwrap();
            let expected_book = book.clone();
            let result = Nop::new().run(&ctx, book);
            assert!(result.is_ok());

            // The nop-preprocessor should not have made any changes to the book content.
            let actual_book = result.unwrap();
            assert_eq!(actual_book, expected_book);
        }
    }
}
