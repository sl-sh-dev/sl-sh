use std::env;
use std::ffi::OsString;

pub struct Config {
    pub command: Option<String>,
    pub script: Option<String>,
    pub args: Vec<String>,
}

pub const VERSION_STRING: &str = env!("VERSION_STRING");

const HELP: &str = r#"slosh - Experimental Lisp REPL

USAGE:
    slosh [FLAGS] [OPTIONS] [args]

FLAGS:
    -v, --version  Print the version, platform and revision of sl-sh then exit.
    -h, --help     Print help (this) and exit.

OPTIONS:
    -c             Command to run instead of entering the REPL.

ARGS:
    <args>...      Script to run with arguments."#;

fn help(_name: &str) {
    println!("{}", HELP);
}

fn version() {
    println!("{}", VERSION_STRING);
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
    let mut command: Option<String> = None;
    let mut script: Option<String> = None;
    let mut command_args: Vec<String> = Vec::new();

    let mut args: Vec<OsString> = env::args_os().collect();

    args.reverse();
    let exe_name = get_arg("unknown", &mut args)?; // Pop off the executable name.

    while !args.is_empty() {
        if let Some(argument) = args.pop() {
            if let Ok(arg) = argument.into_string() {
                match &arg[..] {
                    "-c" => {
                        if command.is_some() {
                            help(&exe_name);
                            return None;
                        }
                        command = Some(get_arg(&exe_name, &mut args)?);
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
                        if command.is_none() && script.is_none() {
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
    Some(Config {
        command,
        script,
        args: command_args,
    })
}
