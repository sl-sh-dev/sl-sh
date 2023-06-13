use crate::command_data::{Arg, CommandWithArgs, Redirects, Run};
use crate::platform::{FileDesc, STDERR_FILENO, STDIN_FILENO, STDOUT_FILENO};
use std::fmt::{Display, Formatter};
use std::io;
use std::io::ErrorKind;
use std::iter::Peekable;
use std::str::{Chars, FromStr};

/// Type of the current sequence being parsed.
#[derive(Copy, Clone, Debug)]
enum SeqType {
    Command,
    Pipe,
    Sequence,
    And,
    Or,
}

/// A list of commands to run in a pipe as a job.
#[derive(Clone, Debug)]
pub struct ParsedJob {
    // Only wrapped in an optional to use take() in push methods, must always be Some.
    commands: Option<Run>,
}

impl Display for ParsedJob {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(run) = &self.commands {
            write!(f, "{run}")?;
        }
        Ok(())
    }
}

impl ParsedJob {
    fn new() -> Self {
        Self {
            commands: Some(Run::Empty),
        }
    }

    fn push_command(&mut self, new_run: Run) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_run(new_run));
        }
    }

    fn push_pipe(&mut self, new_run: Run) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_pipe(new_run));
        }
    }

    fn push_sequence(&mut self, new_run: Run) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_sequence(new_run));
        }
    }

    fn push_and(&mut self, new_run: Run) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_and(new_run));
        }
    }

    fn push_or(&mut self, new_run: Run) {
        if let Some(run) = self.commands.take() {
            self.commands = Some(run.push_or(new_run));
        }
    }

    /// Slice of the individual commands in pipe order.
    pub fn commands(&self) -> &Run {
        self.commands.as_ref().expect("missing command")
    }

    /// Consume and produce the inner ['Run'].
    pub fn into_run(mut self) -> Run {
        self.commands.take().expect("invalid empty command")
    }
}

fn push_next_seq_run(job: &mut ParsedJob, new_run: Run, seq_type: SeqType) {
    match seq_type {
        SeqType::Command => job.push_command(new_run),
        SeqType::Pipe => job.push_pipe(new_run),
        SeqType::Sequence => job.push_sequence(new_run),
        SeqType::And => job.push_and(new_run),
        SeqType::Or => job.push_or(new_run),
    }
}

fn push_next_seq_item(
    job: &mut ParsedJob,
    command: CommandWithArgs,
    seq_type: SeqType,
    background: bool,
) {
    let new_run = if background {
        Run::BackgroundCommand(command)
    } else {
        Run::Command(command)
    };
    match seq_type {
        SeqType::Command => job.push_command(new_run),
        SeqType::Pipe => job.push_pipe(new_run),
        SeqType::Sequence => job.push_sequence(new_run),
        SeqType::And => job.push_and(new_run),
        SeqType::Or => job.push_or(new_run),
    }
}

#[derive(Debug)]
struct ParseState {
    ret: ParsedJob,
    // Should not be None, is Option to allow ownership to change.
    command: Option<CommandWithArgs>,
    stdio: Redirects,
    // Should not be None, is Option to allow ownership to change.
    token: Option<String>,
    last_ch: char,
    current_seq: SeqType,
}

impl ParseState {
    fn new() -> Self {
        let ret = ParsedJob::new();
        let command = Some(CommandWithArgs::new());
        let token = Some(String::new());
        let last_ch = ' ';
        let current_seq = SeqType::Command;
        Self {
            ret,
            command,
            stdio: Redirects::default(),
            token,
            last_ch,
            current_seq,
        }
    }

    fn command(&mut self) -> &mut CommandWithArgs {
        self.command.as_mut().expect("invalid empty command")
    }

    fn token(&mut self) -> &mut String {
        self.token.as_mut().expect("invalid empty token")
    }

    fn take_token(&mut self) -> String {
        let result = self.token.take().expect("invalid empty token");
        self.token = Some(String::new());
        result
    }

    fn proc_token(&mut self) {
        let token = self.take_token();
        if !token.is_empty() {
            self.command().push_arg(Arg::Str(token.into()));
            self.command().stop_compound_arg();
        }
    }

    fn end_command(&mut self, background: bool) {
        if !self.command().is_empty() {
            if let Some(mut command) = self.command.take() {
                command.set_stdios(self.stdio.clone());
                self.stdio.clear();
                push_next_seq_item(&mut self.ret, command, self.current_seq, background);
            }
            self.command = Some(CommandWithArgs::new());
        }
    }

    fn pipe_or(&mut self, ch: char, next_char: char) {
        if self.last_ch == '\\' {
            self.token().push('|');
            self.last_ch = ' ';
        } else if self.last_ch == '|' {
            self.proc_token();
            self.end_command(false);
            self.current_seq = SeqType::Or;
            self.last_ch = ' ';
        } else if next_char == '|' {
            // If the next char is not a '|' then we have a pipe, else will loop and become an OR.
            self.last_ch = ch;
        } else {
            self.proc_token();
            self.end_command(false);
            self.current_seq = SeqType::Pipe;
            self.last_ch = ' ';
        }
    }

    fn seq(&mut self) {
        self.proc_token();
        self.end_command(false);
        self.current_seq = SeqType::Sequence;
    }

    fn and(&mut self) {
        self.proc_token();
        self.end_command(false);
        self.current_seq = SeqType::And;
        self.last_ch = ' ';
    }

    fn redir_out(
        &mut self,
        chars: &mut Peekable<Chars>,
        end_char: Option<char>,
    ) -> Result<(), io::Error> {
        if self.last_ch == '\\' {
            self.token().push('>');
            self.last_ch = ' ';
            return Ok(());
        }
        let mut amp = false;
        let out_fd = if let Some(token) = &self.token {
            if token == "&" {
                self.take_token();
                amp = true;
                STDOUT_FILENO
            } else if let Ok(fd) = FileDesc::from_str(token) {
                if fd >= STDIN_FILENO {
                    self.take_token();
                    fd
                } else {
                    self.proc_token();
                    STDOUT_FILENO
                }
            } else {
                self.proc_token();
                STDOUT_FILENO
            }
        } else {
            self.proc_token();
            STDOUT_FILENO
        };
        let next_char = *chars.peek().unwrap_or(&' ');
        if next_char == '>' {
            chars.next();
            consume_whitespace(chars);
            let fd_arg = read_arg(chars, end_char)?;
            self.stdio.set_out_path(out_fd, fd_arg, false);
            self.last_ch = ' ';
        } else {
            if next_char == '&' {
                chars.next(); // Consume the &
            }
            consume_whitespace(chars);
            let fd_arg = read_arg(chars, end_char)?;
            if next_char == '&' {
                self.stdio.set_out_fd(out_fd, fd_arg, true);
            } else {
                self.stdio.set_out_path(out_fd, fd_arg, true);
            }
            self.last_ch = ' ';
        }
        if amp {
            self.stdio.set_out_internal_fd(STDERR_FILENO, out_fd, true);
        }
        Ok(())
    }

    fn redir_in(
        &mut self,
        chars: &mut Peekable<Chars>,
        end_char: Option<char>,
    ) -> Result<(), io::Error> {
        if self.last_ch == '\\' {
            self.token().push('<');
            self.last_ch = ' ';
            return Ok(());
        }
        let in_fd = if let Some(token) = &self.token {
            if let Ok(fd) = FileDesc::from_str(token) {
                if fd >= STDIN_FILENO {
                    self.take_token();
                    fd
                } else {
                    self.proc_token();
                    STDIN_FILENO
                }
            } else {
                self.proc_token();
                STDIN_FILENO
            }
        } else {
            self.proc_token();
            STDIN_FILENO
        };
        let next_char = *chars.peek().unwrap_or(&' ');
        if next_char == '<' {
            chars.next();
            consume_whitespace(chars);
            let fd_arg = read_arg(chars, end_char)?;
            self.last_ch = ' ';
            self.stdio.set_in_direct(in_fd, fd_arg);
        } else if next_char == '>' {
            // <> bidirectional fd.
            chars.next();
            let next_char = *chars.peek().unwrap_or(&' ');
            if next_char == '&' {
                chars.next(); // Consume the &
            }
            consume_whitespace(chars);
            let fd_arg = read_arg(chars, end_char)?;
            self.last_ch = ' ';
            if next_char == '&' {
                self.stdio.set_in_out_fd(in_fd, fd_arg);
            } else {
                self.stdio.set_in_out_path(in_fd, fd_arg);
            }
        } else {
            if next_char == '&' {
                chars.next(); // Consume the &
            }
            consume_whitespace(chars);
            let fd_arg = read_arg(chars, end_char)?;
            self.last_ch = ' ';
            if next_char == '&' {
                self.stdio.set_in_fd(in_fd, fd_arg, true);
            } else {
                self.stdio.set_in_path(in_fd, fd_arg);
            }
        }
        Ok(())
    }

    fn special_arg(
        &mut self,
        chars: &mut Peekable<Chars>,
        end_char: Option<char>,
    ) -> Result<(), io::Error> {
        if let Some('(') = chars.peek() {
            // Subshell to capture
            chars.next();
            self.proc_token();
            let mut sub = parse_line_inner(chars, Some(')'))?;
            if let Some(sub) = sub.commands.take() {
                self.command().push_run_arg(sub);
            }
            if chars.peek().unwrap_or(&' ').is_whitespace() {
                self.command().stop_compound_arg();
            } else {
                self.command().start_compound_arg();
            }
        } else {
            // Env var
            self.proc_token();
            let name = if let Some('{') = chars.peek() {
                chars.next();
                let r = read_token(chars, Some('}'));
                if let Some('}') = chars.peek() {
                    chars.next();
                    r
                } else {
                    return Err(io::Error::new(ErrorKind::Other, "bad substitution"));
                }
            } else {
                read_token(chars, end_char)
            };
            if !name.is_empty() {
                self.command().push_env_var_arg(name.into());
            }
            if chars.peek().unwrap_or(&' ').is_whitespace() {
                self.command().stop_compound_arg();
            } else {
                self.command().start_compound_arg();
            }
        }
        Ok(())
    }
}

impl From<ParseState> for ParsedJob {
    fn from(value: ParseState) -> Self {
        value.ret
    }
}

fn consume_whitespace(chars: &mut Peekable<Chars>) {
    while let Some(ch) = chars.peek() {
        if ch.is_whitespace() {
            chars.next();
        } else {
            break;
        }
    }
}

/// Read string surrounded by single quote (').  Assumes chars is on the open quote and
/// consumes the end quote.
/// This simply reads the chars until the next ' and puts them in a String Arg.
/// Note, can not produce a string containing a ' character.
fn read_simple_string(chars: &mut Peekable<Chars>) -> Result<Arg, io::Error> {
    let mut res = String::new();
    let mut next_ch = chars.peek().copied();
    while let Some(ch) = next_ch {
        if ch == '\'' {
            chars.next();
            return Ok(Arg::QuotedStr(res.into()));
        }
        chars.next();
        res.push(ch);
        next_ch = chars.peek().copied();
    }
    Err(io::Error::new(ErrorKind::Other, "unclosed string"))
}

fn char_to_hex_num(ch: char) -> Result<u8, io::Error> {
    if ch.is_ascii_digit() {
        Ok(ch as u8 - b'0')
    } else {
        match ch {
            'a' | 'A' => Ok(10),
            'b' | 'B' => Ok(11),
            'c' | 'C' => Ok(12),
            'd' | 'D' => Ok(13),
            'e' | 'E' => Ok(14),
            'f' | 'F' => Ok(15),
            _ => Err(io::Error::new(
                ErrorKind::Other,
                format!("Invalid hex digit {ch}, expected 0-9 or A-F."),
            )),
        }
    }
}

/// Read an ascii char from 0x00-0x7F endowed in a string as '\xXX' where X is single hex digit.
fn escape_to_char(chars: &mut Peekable<Chars>) -> Result<char, io::Error> {
    if let (Some(ch1), Some(ch2)) = (chars.next(), chars.peek()) {
        let ch_n: u8 = (char_to_hex_num(ch1)? * 16) + (char_to_hex_num(*ch2)?);
        if ch_n > 0x7f {
            Err(io::Error::new(
                ErrorKind::Other,
                "Invalid hex ascii code, must be less then \\x7f.".to_string(),
            ))
        } else {
            Ok(ch_n as char)
        }
    } else {
        Err(io::Error::new(
            ErrorKind::Other,
            "Invalid hex ascii code, expected two digits.".to_string(),
        ))
    }
}

/// Read a UTF8 codepoint encoded in a string with '\uXXXXXXXX' or \u{XXXXXXXX}' where X is a
/// single hex digit.  There can be 1-8 hex values (X- a nibble) in the encoding (Up to 4 bytes).
fn read_utf_scalar(chars: &mut Peekable<Chars>) -> Result<char, io::Error> {
    fn finish(char_u32: u32) -> Result<char, io::Error> {
        if let Some(val) = std::char::from_u32(char_u32) {
            Ok(val)
        } else {
            Err(io::Error::new(
                ErrorKind::Other,
                format!("Invalid unicode scalar, {char_u32:x} not a valid utf scalar."),
            ))
        }
    }
    let mut first = true;
    let mut has_bracket = false;
    let mut char_u32 = 0;
    let mut nibbles = 0;
    let mut next_ch = chars.peek().copied();
    while let Some(ch) = next_ch {
        if ch == '\n' {
            break;
        }
        if ch == '"' {
            break;
        }
        if !has_bracket && ch.is_whitespace() {
            return finish(char_u32);
        }
        if first && ch == '{' {
            has_bracket = true;
            first = false;
            chars.next();
            next_ch = chars.peek().copied();
            continue;
        }
        first = false;
        if has_bracket && ch == '}' {
            return finish(char_u32);
        }
        if nibbles >= 8 {
            return Err(io::Error::new(
                ErrorKind::Other,
                "Invalid unicode scalar, too many bytes (4 max).".to_string(),
            ));
        }
        nibbles += 1;
        let nib = char_to_hex_num(ch)?;
        char_u32 = (char_u32 << 4) | nib as u32;
        chars.next();
        next_ch = chars.peek().copied();
    }
    if has_bracket {
        Err(io::Error::new(
            ErrorKind::Other,
            "Invalid unicode scalar, failed to parse.".to_string(),
        ))
    } else {
        finish(char_u32)
    }
}

/// Read string surrounded by quote (").  Assumes chars is on the open quote and
/// consumes the end quote.
/// This version will handle interpolation and escape chars.
fn read_string(chars: &mut Peekable<Chars>) -> Result<Arg, io::Error> {
    let mut res = String::new();
    let mut arg = None;
    let mut next_ch = chars.peek().copied();
    while let Some(ch) = next_ch {
        if ch == '"' {
            chars.next();
            if let Some(Arg::Compound(mut args)) = arg {
                if !res.is_empty() {
                    args.push(Arg::QuotedStr(res.into()));
                }
                return Ok(Arg::Compound(args));
            } else {
                return Ok(Arg::QuotedStr(res.into()));
            }
        } else if ch == '$' {
            chars.next();
            let spec_arg = read_special_arg(chars, Some('"'))?;
            if let Some(Arg::Compound(mut args)) = arg {
                if !res.is_empty() {
                    args.push(Arg::Str(res.into()));
                }
                args.push(spec_arg);
                res = String::new();
                arg = Some(Arg::Compound(args));
            } else {
                let args = if !res.is_empty() {
                    vec![Arg::QuotedStr(res.into()), spec_arg]
                } else {
                    vec![spec_arg]
                };
                res = String::new();
                arg = Some(Arg::Compound(args));
            }
            next_ch = chars.peek().copied();
        } else if ch == '\\' {
            chars.next();
            next_ch = chars.peek().copied();
            match next_ch {
                Some('\n') => {
                    // Consume \newline.
                }
                Some('e') | Some('E') => {
                    // Escape
                    res.push('\x1B');
                }
                Some('n') => {
                    // Linefeed
                    res.push('\n');
                }
                Some('r') => {
                    // Carriage Return
                    res.push('\r');
                }
                Some('t') => {
                    // Tab
                    res.push('\t');
                }
                Some('a') => {
                    // Bell
                    res.push('\x07');
                }
                Some('b') => {
                    // Backspace
                    res.push('\x08');
                }
                Some('f') => {
                    // Formfeed
                    res.push('\x0C');
                }
                Some('v') => {
                    // Vertical tab
                    res.push('\x0B');
                }
                Some('\"') => {
                    res.push('"');
                }
                Some('x') => {
                    chars.next();
                    let xch = escape_to_char(chars)?;
                    res.push(xch);
                }
                Some('\\') => {
                    res.push('\\');
                }
                Some('u') => {
                    chars.next();
                    let uch = read_utf_scalar(chars)?;
                    res.push(uch);
                    if let Some(ch) = chars.peek().copied() {
                        // If a \u ends in whitespace need to keep it vs swallow it.
                        if ch.is_whitespace() {
                            res.push(ch);
                        }
                        // Need to see this quote to finish...
                        if ch == '"' {
                            next_ch = chars.peek().copied();
                            continue;
                        }
                    }
                }
                Some(nch) => {
                    res.push(ch);
                    res.push(nch);
                }
                _ => {
                    res.push('\\');
                }
            }
            chars.next();
            next_ch = chars.peek().copied();
        } else {
            chars.next();
            res.push(ch);
            next_ch = chars.peek().copied();
        }
    }
    Err(io::Error::new(ErrorKind::Other, "unclosed string"))
}

fn read_token(chars: &mut Peekable<Chars>, end_char: Option<char>) -> String {
    let end_char = end_char.unwrap_or(' ');
    let end_set = ['"', '\'', '$', '|', ';', '&', '<', '>', '(', end_char];
    let mut res = String::new();
    let mut next_ch = chars.peek();
    while let Some(ch) = next_ch {
        let ch = *ch;
        if !ch.is_whitespace() && !end_set.contains(&ch) {
            chars.next();
            res.push(ch);
            next_ch = chars.peek();
        } else {
            next_ch = None;
        }
    }
    res
}

fn read_arg(chars: &mut Peekable<Chars>, end_char_in: Option<char>) -> Result<Arg, io::Error> {
    let mut args = vec![];
    let end_char = end_char_in.unwrap_or(' ');
    let end_set = ['$', '|', ';', '&', '<', '>', '(', end_char];
    let mut res = String::new();
    let mut next_ch = chars.peek().copied();
    while let Some(ch) = next_ch {
        let ch = ch;
        if ch == '$' {
            args.push(Arg::Str(res.clone().into()));
            res.clear();
            chars.next();
            let next_arg = read_special_arg(chars, end_char_in)?;
            if let Arg::Compound(mut nargs) = next_arg {
                args.append(&mut nargs);
            } else {
                args.push(next_arg);
            }
            next_ch = chars.peek().copied();
        } else if ch == '\'' && ch != end_char {
            chars.next(); // Advance to opening quote.
            args.push(read_simple_string(chars)?);
            next_ch = chars.peek().copied();
        } else if ch == '"' && ch != end_char {
            chars.next(); // Advance to opening quote.
            args.push(read_string(chars)?);
            next_ch = chars.peek().copied();
        } else if !ch.is_whitespace() && !end_set.contains(&ch) {
            chars.next();
            res.push(ch);
            next_ch = chars.peek().copied();
        } else {
            next_ch = None;
        }
    }
    args.push(Arg::Str(res.into()));
    Ok(if args.len() == 1 {
        args.pop().expect("we had one element...")
    } else {
        Arg::Compound(args)
    })
}

fn read_special_arg(chars: &mut Peekable<Chars>, end_char: Option<char>) -> Result<Arg, io::Error> {
    let mut args = vec![];
    if let Some('(') = chars.peek() {
        // Subshell to capture
        chars.next();
        let mut sub = parse_line_inner(chars, Some(')'))?;
        if let Some(sub) = sub.commands.take() {
            args.push(Arg::Command(sub));
        }
    } else {
        // Env var
        let name = if let Some('{') = chars.peek() {
            chars.next();
            let r = read_token(chars, Some('}'));
            if let Some('}') = chars.peek() {
                chars.next();
                r
            } else {
                return Err(io::Error::new(ErrorKind::Other, "bad substitution"));
            }
        } else {
            read_token(chars, end_char)
        };
        if !name.is_empty() {
            args.push(Arg::Var(name.into()));
        }
    }
    Ok(if args.len() == 1 {
        args.pop().expect("we had one element...")
    } else {
        Arg::Compound(args)
    })
}

fn parse_line_inner(
    chars: &mut Peekable<Chars>,
    end_char: Option<char>,
) -> Result<ParsedJob, io::Error> {
    let mut state = ParseState::new();
    while let Some(ch) = chars.next() {
        if let Some(end_ch) = end_char {
            if ch == end_ch {
                break;
            }
        }
        let next_char = *chars.peek().unwrap_or(&' ');
        if ch.is_whitespace() {
            state.proc_token();
        } else {
            match ch {
                '\'' if state.last_ch != '\\' => {
                    state.proc_token();
                    let str_arg = read_simple_string(chars)?;
                    state.command().push_arg(str_arg);
                    state.command().start_compound_arg();
                }
                '"' if state.last_ch != '\\' => {
                    state.proc_token();
                    let str_arg = read_string(chars)?;
                    state.command().push_arg(str_arg);
                    state.command().start_compound_arg();
                }
                '|' => {
                    state.pipe_or(ch, next_char);
                }
                ';' if state.last_ch != '\\' => {
                    state.seq();
                }
                '>' => {
                    state.redir_out(chars, end_char)?;
                }
                '<' => {
                    state.redir_in(chars, end_char)?;
                }
                '&' if state.last_ch == '\\' => {
                    state.token().push('&');
                    state.last_ch = ' ';
                }
                '&' if next_char == '>' || next_char == '&' => {
                    state.last_ch = '&';
                }
                '&' if state.last_ch == '&' => {
                    state.and();
                }
                '&' => {
                    state.proc_token();
                    state.end_command(true);
                    state.last_ch = ' ';
                }
                '\\' => {
                    if state.last_ch == '\\' {
                        state.token().push('\\');
                        state.last_ch = ' ';
                    } else {
                        state.last_ch = ch;
                    }
                }
                '(' => {
                    state.proc_token();
                    state.end_command(false);
                    let mut sub = parse_line_inner(chars, Some(')'))?;
                    if let Some(sub) = sub.commands.take() {
                        push_next_seq_run(
                            &mut state.ret,
                            Run::Subshell(Box::new(sub)),
                            state.current_seq,
                        );
                    }
                }
                '\n' if state.last_ch == '\\' => {
                    state.last_ch = ' ';
                }
                '$' => state.special_arg(chars, end_char)?,
                _ => {
                    if state.last_ch == '\\' {
                        // Last char was a backslash and seems unremarkable so put it in the token.
                        state.token().push('\\');
                    }
                    state.token().push(ch);
                    state.last_ch = ch;
                }
            }
        }
    }
    state.proc_token();
    state.end_command(false);
    Ok(state.into())
}

pub fn parse_line(input: &str) -> Result<ParsedJob, io::Error> {
    let mut chars = input.chars().peekable();
    parse_line_inner(&mut chars, None)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_parse(input: &str, expected: &str) {
        let pj = parse_line(input).unwrap();
        let pj_str = pj.to_string();
        assert_eq!(&pj_str, expected);
        let pj = parse_line(&pj_str).unwrap();
        assert_eq!(&pj.to_string(), expected);
    }

    fn test_parse_once(input: &str, expected: &str) {
        let pj = parse_line(input).unwrap();
        let pj_str = pj.to_string();
        assert_eq!(&pj_str, expected);
    }

    #[test]
    fn test_basic_parse() {
        test_parse("ls", "ls");
        test_parse("ls -al", "ls -al");
        test_parse("ls -al|grep lisp", "ls -al | grep lisp");
        test_parse("<in_file ls -al|grep lisp", "ls -al 0<in_file | grep lisp");

        test_parse("ls -al;grep lisp", "ls -al ; grep lisp");

        test_parse("ls -al&&grep lisp", "ls -al && grep lisp");

        test_parse("ls -al||grep lisp", "ls -al || grep lisp");

        test_parse(
            "</some/file grep test|grep lisp>/out_file",
            "grep test 0</some/file | grep lisp 1>/out_file",
        );

        test_parse(
            "</some/file > out_file grep test;<in_file grep lisp>/out_file;ls",
            "grep test 0</some/file 1>out_file ; grep lisp 0<in_file 1>/out_file ; ls",
        );

        test_parse(
            "</some/file 2>&1 > out_file grep test;<in_file grep lisp 1>&2 >/out_file;ls",
            "grep test 0</some/file 2>&1 1>out_file ; grep lisp 0<in_file 1>&2 1>/out_file ; ls",
        );

        test_parse(
            "</some/file 2>&1 > out_file grep test;ls|<in_file grep lisp 1>&2 >/out_file;ls",
            "grep test 0</some/file 2>&1 1>out_file ; ls | grep lisp 0<in_file 1>&2 1>/out_file ; ls",
        );
    }

    #[test]
    fn test_subshell_parse() {
        test_parse("(ls -al)", "(ls -al)");
        test_parse("(ls -al)|grep May", "(ls -al) | grep May");
        test_parse(
            "(ls -al)|(grep May|grep 7)|grep 00",
            "(ls -al) | (grep May | grep 7) | grep 00",
        )
    }

    #[test]
    fn test_strings() {
        test_parse_once("\"one\\ntwo\"", "one\x0Atwo");
        test_parse_once("'one\\ntwo'", "one\\ntwo");
        test_parse_once("'one\ntwo'", "one\x0Atwo");
        test_parse_once("\"one\\x0atwo\"", "one\x0Atwo");
        test_parse_once("\"one\\x0Atwo\"", "one\x0Atwo");
        test_parse_once("\"one\\u0a two\"", "one\x0A two");
        test_parse_once("\"one\\u0A two\"", "one\x0A two");
        test_parse_once("\"one\\u{0a}two\"", "one\x0Atwo");
        test_parse_once("\"one\\u{0A}two\"", "one\x0Atwo");
        test_parse_once("\"one\\vtwo\"", "one\x0Btwo");
        test_parse_once("\"one\\ftwo\"", "one\x0Ctwo");
        test_parse_once("\"one\\etwo\"", "one\x1Btwo");
        test_parse_once("\"one\\Etwo\"", "one\x1Btwo");
        test_parse_once("\"one\\atwo\"", "one\x07two");
        test_parse_once("\"one\\btwo\"", "one\x08two");
        test_parse_once("\"one\\ttwo\"", "one\x09two");
        test_parse_once("\"one\\rtwo\"", "one\x0Dtwo");
        test_parse_once("\"one\\\\rtwo\"", "one\\rtwo");
        test_parse_once("\"one\\\"two\"", "one\"two");
        test_parse_once("\"one\\x0a\"", "one\x0A");
        test_parse_once("\"one\\u0a\"", "one\x0A");
        test_parse_once("\"one\\u0a \"", "one\x0A ");
        test_parse_once("\"one\\u{0a}\"", "one\x0A");
        test_parse_once("\"one\\u{0a}\ntwo\"", "one\x0A\ntwo");
        test_parse_once("\"one\\u0a\ntwo\"", "one\x0A\ntwo");
        test_parse_once("\"one\\u0a\n\"", "one\x0A\n");
    }
}
