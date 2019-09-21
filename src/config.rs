use std::env;
use std::ffi::OsString;

pub struct Config {
    pub command: Option<String>,
    pub args: Vec<String>,
}

pub const VERSION_STRING: &str = env!("VERSION_STRING");

const HELP: &str = r#"slsh - Simple Lisp Shell
Start the slsh shell.

USAGE:
    slsh [FLAGS] [OPTIONS] [args]

FLAGS:
    -v, --version  Print the version, platform and revision of server then exit.
    -h, --help     Print help (this) and exit.

OPTIONS:
    -c             Command to run instead of entering the REPL.

ARGS:
    <args>...  Script arguments. If the -c option is not specified the
               first parameter is the filename to execute."#;

fn help(_name: &str) {
    println!("{}", HELP);
}

fn version() {
    println!("{}", VERSION_STRING);
}

fn get_arg(exe_name: &str, args: &mut Vec<OsString>) -> Result<String, ()> {
    if let Some(argument) = args.pop() {
        if let Ok(arg) = argument.into_string() {
            return Ok(arg);
        }
    }
    help(exe_name);
    Err(())
}

pub fn get_config() -> Result<Config, ()> {
    let mut command: Option<String> = None;
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
                            return Err(());
                        }
                        command = Some(get_arg(&exe_name, &mut args)?);
                    }
                    "-v" | "--version" => {
                        version();
                        return Err(());
                    }
                    "-h" | "--help" => {
                        help(&exe_name);
                        return Err(());
                    }
                    _ => {
                        if command.is_none() {
                            command = Some(arg);
                        } else {
                            command_args.push(arg);
                        }
                    }
                }
            } else {
                help(&exe_name);
                return Err(());
            }
        }
    }
    Ok(Config {
        command,
        args: command_args,
    })
}
