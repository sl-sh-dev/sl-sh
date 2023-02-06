use std::env;
use std::ffi::OsString;

pub struct Config {
    pub script: String,
    pub args: Vec<String>,
    pub dump: bool,
    pub run: bool,
    pub globals_pre: bool,
    pub globals_post: bool,
}

pub const VERSION_STRING: &str = env!("VERSION_STRING");

const HELP: &str = r#"sl-compiler - Simple Lisp Compiler
Run sl-compiler.

USAGE:
    sl-compiler [FLAGS] [OPTIONS] [args]

FLAGS:
    -v, --version  Print the version, platform and revision of sl-compiler then exit.
    -h, --help     Print help (this) and exit.

OPTIONS:
    -r, --run          Compile and run the script.
    -d, --dump         Compile and dump the bytecode for the script.
    -g1, --global_pre  Compile and dump the globals before running.
    -g2, --global_post Compile and dump the globals before running.

ARGS:
    <args>...      Script to run with arguments."#;

fn help(_name: &str) {
    println!("{HELP}");
}

fn version() {
    println!("{VERSION_STRING}");
}

fn get_arg(exe_name: &str, args: &mut Vec<OsString>) -> Option<String> {
    if let Some(argument) = args.pop() {
        if let Ok(arg) = argument.into_string() {
            return Some(arg);
        }
    }
    help(exe_name);
    None
}

pub fn get_config() -> Option<Config> {
    let mut script: Option<String> = None;
    let mut command_args: Vec<String> = Vec::new();

    let mut args: Vec<OsString> = env::args_os().collect();

    args.reverse();
    let exe_name = get_arg("unknown", &mut args)?; // Pop off the executable name.

    let mut run = true;
    let mut dump = false;
    let mut globals_pre = false;
    let mut globals_post = false;
    while !args.is_empty() {
        if let Some(argument) = args.pop() {
            if let Ok(arg) = argument.into_string() {
                match &arg[..] {
                    "-r" | "--run" => run = true,
                    "-d" | "--dump" => {
                        run = false;
                        dump = true;
                    }
                    "-g1" | "--globals_pre" => globals_pre = true,
                    "-g2" | "--globals_post" => {
                        globals_post = true;
                        run = true;
                    }
                    "-v" | "--version" => {
                        version();
                        return None;
                    }
                    "-h" | "--help" => {
                        help(&exe_name);
                        return None;
                    }
                    _ => {
                        if script.is_none() {
                            script = Some(arg);
                        } else {
                            command_args.push(arg);
                        }
                    }
                }
            } else {
                help(&exe_name);
                return None;
            }
        }
    }
    if script.is_none() {
        println!("Must provide a script to compile.");
        help(&exe_name);
        return None;
    }
    Some(Config {
        run,
        dump,
        globals_pre,
        globals_post,
        script: script.unwrap(),
        args: command_args,
    })
}
