use std::cell::RefCell;
use std::num::{ParseFloatError, ParseIntError};

use crate::types::*;

fn is_whitespace(ch: char) -> bool {
    match ch {
        ' ' => true,
        '\t' => true,
        _ => false,
    }
}

macro_rules! save_token {
    ($tokens:expr, $token:expr) => {{
        let t_token = $token.trim();
        if !t_token.is_empty() {
            $tokens.push(t_token.to_string());
            $token = String::new();
        }
    }};
}

fn char_to_hex_num(ch: char) -> u8 {
    if ch > '0' && ch < '9' {
        ch as u8 - b'0'
    } else {
        match ch {
            'a' => 10,
            'b' => 11,
            'c' => 12,
            'd' => 13,
            'e' => 14,
            'f' => 15,
            _ => 0,
        }
    }
}

fn escape_to_char(escape_code: &[char]) -> char {
    let mut ch_n: u8 = 0;
    if escape_code.len() > 1 {
        ch_n = (char_to_hex_num(escape_code[0]) * 16) + (char_to_hex_num(escape_code[1]));
    } else if escape_code.len() == 1 {
        ch_n = char_to_hex_num(escape_code[0]);
    }
    ch_n as char
}

fn do_in_string(
    mut token: String,
    ch: char,
    last_ch: &mut char,
    in_escape_code: &mut bool,
    escape_code: &mut Vec<char>,
) -> String {
    let mut set_last_char = false;
    if !(ch == '\\' && *last_ch != '\\') {
        // skip a standalone \ for now
        if *in_escape_code {
            escape_code.push(ch);
            if escape_code.len() == 2 {
                token.push(escape_to_char(escape_code));
                escape_code.clear();
                *in_escape_code = false;
            }
        } else if *last_ch == '\\' {
            match ch {
                'n' => token.push('\n'),
                'r' => token.push('\r'),
                't' => token.push('\t'),
                '"' => token.push('"'),
                'x' => {
                    *in_escape_code = true;
                }
                '\\' => {
                    // These \ are consumed so do not use again.
                    *last_ch = ' ';
                    set_last_char = true;
                    token.push('\\');
                }
                _ => {
                    token.push('\\');
                    token.push(ch);
                }
            }
        } else {
            token.push(ch);
        }
    }
    if !set_last_char {
        *last_ch = ch;
    }
    token
}

fn handle_char(
    tokens: &mut Vec<String>,
    mut token: String,
    ch: char,
    last_ch: char,
    last_comma: &mut bool,
) -> String {
    if ch == '(' {
        save_token!(tokens, token);
        tokens.push("(".to_string());
    } else if ch == ')' {
        save_token!(tokens, token);
        tokens.push(")".to_string());
    } else if ch == '\'' && (last_ch == ' ' || last_ch == '(' || last_ch == '\'' || last_ch == '`')
    {
        save_token!(tokens, token);
        tokens.push("'".to_string());
    } else if ch == '`' && (last_ch == ' ' || last_ch == '(' || last_ch == '\'' || last_ch == '`') {
        save_token!(tokens, token);
        tokens.push("`".to_string());
    } else if ch == ',' && (last_ch == ' ' || last_ch == '(') {
        *last_comma = true;
    } else if is_whitespace(ch) {
        save_token!(tokens, token);
    } else if ch == '\\' && last_ch != '\\' {
        // Do nothing...
    } else {
        token.push(ch);
    }
    token
}

fn tokenize(text: &str) -> Vec<String> {
    let mut tokens: Vec<String> = Vec::new();
    let mut in_string = false;
    let mut token = String::new();
    let mut last_ch = ' ';
    let mut in_comment = false;
    let mut last_comma = false;
    let mut escape_code: Vec<char> = Vec::with_capacity(2);
    let mut in_escape_code = false;
    if text.starts_with("#!") {
        // Work with shebanged scripts.
        in_comment = true;
    }
    for ch in text.chars() {
        if last_comma {
            last_comma = false;
            save_token!(tokens, token);
            if ch == '@' {
                tokens.push(",@".to_string());
                last_ch = ch;
                continue;
            } else {
                tokens.push(",".to_string());
            }
        }
        if in_comment {
            if ch == '\n' {
                in_comment = false;
            }
            continue;
        }
        if ch == '\n' && last_ch == '\\' {
            // Line ended on \ so combine with next line.
            token.push('\n');
            last_ch = ch;
            continue;
        }
        if ch == '\"' && last_ch != '\\' {
            // Kakoune bug "
            in_string = !in_string;
            token.push(ch);
            if !in_string {
                tokens.push(token);
                token = String::new();
            } else {
                in_escape_code = false;
                escape_code.clear();
            }
            last_ch = ch;
            continue;
        }
        if in_string {
            token = do_in_string(
                token,
                ch,
                &mut last_ch,
                &mut in_escape_code,
                &mut escape_code,
            );
        } else {
            if ch == ';' {
                // Comment, ignore the rest of the line.
                in_comment = true;
                continue;
            }
            token = handle_char(&mut tokens, token, ch, last_ch, &mut last_comma);
            last_ch = ch;
        }
    }
    let token = token.trim();
    if !token.is_empty() {
        tokens.push(token.to_string());
    }
    tokens
}

fn parse_atom(token: &str) -> Expression {
    if token.is_empty() {
        return Expression::Atom(Atom::Nil);
    }
    if token.len() > 1 && token.starts_with('\"') && token.ends_with('\"') {
        // Kakoune bug "
        let string = token[1..token.len() - 1].to_string();
        return Expression::Atom(Atom::String(string));
    }

    if token == "t" {
        Expression::Atom(Atom::True)
    } else if token == "nil" {
        Expression::Atom(Atom::Nil)
    } else {
        let potential_int: Result<i64, ParseIntError> = token.parse();
        match potential_int {
            Ok(v) => Expression::Atom(Atom::Int(v)),
            Err(_) => {
                let potential_float: Result<f64, ParseFloatError> = token.parse();
                match potential_float {
                    Ok(v) => Expression::Atom(Atom::Float(v)),
                    Err(_) => Expression::Atom(Atom::Symbol(token.to_string().clone())),
                }
            }
        }
    }
}

fn close_list(level: i32, stack: &mut Vec<Vec<Expression>>) -> Result<(), ParseError> {
    if level < 0 {
        return Err(ParseError {
            reason: "Unexpected `)`".to_string(),
        });
    }
    if level > 0 {
        match stack.pop() {
            Some(v) => match stack.pop() {
                Some(mut v2) => {
                    v2.push(Expression::List(RefCell::new(v)));
                    stack.push(v2);
                }
                None => {
                    stack.push(v);
                }
            },
            None => {
                return Err(ParseError {
                    reason: "Unexpected `)`".to_string(),
                });
            }
        }
    }
    Ok(())
}

fn parse(tokens: &[String]) -> Result<Expression, ParseError> {
    if tokens.is_empty() {
        return Err(ParseError {
            reason: "No tokens".to_string(),
        });
    }
    if tokens[0] != "(" && tokens[0] != "'" && tokens[0] != "`" {
        return Err(ParseError {
            reason: "Not a list".to_string(),
        });
    }
    let mut stack: Vec<Vec<Expression>> = Vec::new();
    let mut level = 0;
    let mut qexits: Vec<i32> = Vec::new();
    let mut backtick_level = 0;
    for token in tokens {
        match &token[..] {
            "'" => {
                level += 1;
                qexits.push(level);
                let mut quoted = Vec::<Expression>::new();
                quoted.push(Expression::Atom(Atom::Symbol("quote".to_string())));
                stack.push(quoted);
            }
            "`" => {
                level += 1;
                qexits.push(level);
                let mut quoted = Vec::<Expression>::new();
                if backtick_level > 0 {
                    quoted.push(Expression::Atom(Atom::Symbol("quote".to_string())));
                } else {
                    quoted.push(Expression::Atom(Atom::Symbol("bquote".to_string())));
                    backtick_level = level;
                }
                stack.push(quoted);
            }
            "(" => {
                level += 1;
                stack.push(Vec::<Expression>::new());
            }
            ")" => {
                level -= 1;
                close_list(level, &mut stack)?;
                while let Some(quote_exit_level) = qexits.pop() {
                    if level == quote_exit_level {
                        if level == backtick_level {
                            backtick_level = 0;
                        }
                        level -= 1;
                        close_list(level, &mut stack)?;
                    } else {
                        qexits.push(quote_exit_level);
                        break;
                    }
                }
            }
            _ => match stack.pop() {
                Some(mut v) => {
                    v.push(parse_atom(token));
                    stack.push(v);
                    if let Some(quote_exit_level) = qexits.pop() {
                        if level == quote_exit_level {
                            if level == backtick_level {
                                backtick_level = 0;
                            }
                            level -= 1;
                            close_list(level, &mut stack)?;
                        } else {
                            qexits.push(quote_exit_level);
                        }
                    }
                }
                None => {
                    return Err(ParseError {
                        reason: "Found symbol without containing list".to_string(),
                    });
                }
            },
        }
    }
    if !qexits.is_empty() {
        qexits.reverse();
        for quote_exit_level in qexits.drain(..) {
            if level == quote_exit_level {
                level -= 1;
                close_list(level, &mut stack)?;
            }
        }
    }
    if level != 0 {
        return Err(ParseError {
            reason: "Unclosed list(s)".to_string(),
        });
    }
    if stack.len() > 1 {
        let mut v: Vec<Expression> = Vec::new();
        for s in stack {
            v.push(Expression::List(RefCell::new(s)));
        }
        Ok(Expression::List(RefCell::new(v)))
    } else {
        match stack.pop() {
            Some(v) => Ok(Expression::List(RefCell::new(v))),
            None => Err(ParseError {
                reason: "Empty results".to_string(),
            }),
        }
    }
}

pub fn read(text: &str) -> Result<Expression, ParseError> {
    let tokens = tokenize(text);
    parse(&tokens)
}
