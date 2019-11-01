use std::cell::RefCell;
use std::num::{ParseFloatError, ParseIntError};
use std::rc::Rc;

use crate::types::*;

enum ListType {
    Vector,
    List,
}

struct List {
    list_type: ListType,
    vec: Vec<Expression>,
}

struct Token {
    token: String,
    line: usize,
    column: usize,
}

fn is_whitespace(ch: char) -> bool {
    match ch {
        ' ' => true,
        '\t' => true,
        _ => false,
    }
}

macro_rules! save_token {
    ($tokens:expr, $token:expr, $line:expr, $column:expr) => {{
        let t_token = $token.trim();
        if !t_token.is_empty() {
            $tokens.push(Token {
                token: t_token.to_string(),
                line: $line,
                column: $column,
            });
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
    tokens: &mut Vec<Token>,
    mut token: String,
    ch: char,
    last_ch: char,
    last_comma: &mut bool,
    line: usize,
    column: usize,
) -> String {
    if last_ch == '#' && ch == '(' {
        save_token!(tokens, token, line, column);
        tokens.push(Token {
            token: "#(".to_string(),
            line,
            column,
        });
    } else if last_ch == '#' && ch == '<' {
        save_token!(tokens, token, line, column);
        tokens.push(Token {
            token: "#<".to_string(),
            line,
            column,
        });
    } else if ch == '(' && last_ch == '\\' {
        token.push(ch);
    } else if ch == '(' {
        save_token!(tokens, token, line, column);
        tokens.push(Token {
            token: "(".to_string(),
            line,
            column,
        });
    } else if ch == ')' && last_ch == '\\' {
        token.push(ch);
    } else if ch == ')' {
        save_token!(tokens, token, line, column);
        tokens.push(Token {
            token: ")".to_string(),
            line,
            column,
        });
    } else if ch == '\'' && (last_ch == ' ' || last_ch == '(' || last_ch == '\'' || last_ch == '`')
    {
        save_token!(tokens, token, line, column);
        tokens.push(Token {
            token: "'".to_string(),
            line,
            column,
        });
    } else if ch == '`' && (last_ch == ' ' || last_ch == '(' || last_ch == '\'' || last_ch == '`') {
        save_token!(tokens, token, line, column);
        tokens.push(Token {
            token: "`".to_string(),
            line,
            column,
        });
    } else if ch == ',' && (last_ch == ' ' || last_ch == '(') {
        *last_comma = true;
    } else if last_ch == '\\' && ch == ' ' {
        // Keep an escaped space in token since this is a shell...
        token.push(ch);
    } else if is_whitespace(ch) {
        save_token!(tokens, token, line, column);
    } else if ch == '\\' || ch == '#' || ch == '\n' {
        // Do nothing...
        // # is reader macro char, do not save in tokens.
    } else {
        if last_ch == '\\' {
            token.push(last_ch);
        }
        token.push(ch);
    }
    token
}

fn tokenize(text: &str, add_parens: bool) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut in_string = false;
    let mut token = String::new();
    let mut last_ch = ' ';
    let mut in_comment = false;
    let mut comment_depth = 0;
    let mut last_comma = false;
    let mut escape_code: Vec<char> = Vec::with_capacity(2);
    let mut in_escape_code = false;
    let mut line = 1;
    let mut column = 0;
    if add_parens {
        tokens.push(Token {
            token: "(".to_string(),
            line,
            column,
        });
    }
    if text.starts_with("#!") {
        // Work with shebanged scripts.
        in_comment = true;
    }
    for ch in text.chars() {
        if ch == '\n' {
            line += 1;
            column = 0;
        } else {
            column += 1;
        }
        if last_comma {
            last_comma = false;
            save_token!(tokens, token, line, column);
            if ch == '@' {
                tokens.push(Token {
                    token: ",@".to_string(),
                    line,
                    column,
                });
                last_ch = ch;
                continue;
            } else {
                tokens.push(Token {
                    token: ",".to_string(),
                    line,
                    column,
                });
            }
        }
        if in_comment {
            if ch == '\n' && comment_depth == 0 {
                in_comment = false;
            } else if last_ch == '|' && ch == '#' {
                comment_depth -= 1;
                if comment_depth == 0 {
                    in_comment = false;
                }
            } else if last_ch == '#' && ch == '|' {
                comment_depth += 1;
            }
            last_ch = ch;
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
                tokens.push(Token {
                    token,
                    line,
                    column,
                });
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
            } else if last_ch == '#' && ch == '|' {
                comment_depth += 1;
                in_comment = true;
                continue;
            }
            token = handle_char(
                &mut tokens,
                token,
                ch,
                last_ch,
                &mut last_comma,
                line,
                column,
            );
            last_ch = ch;
        }
    }
    let token = token.trim();
    if !token.is_empty() {
        tokens.push(Token {
            token: token.to_string(),
            line,
            column,
        });
    }
    if add_parens {
        tokens.push(Token {
            token: ")".to_string(),
            line,
            column,
        });
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

fn to_cons(v: &mut Vec<Expression>) -> Expression {
    let mut last_pair = Expression::Atom(Atom::Nil);
    if !v.is_empty() {
        let mut i = v.len() - 1;
        loop {
            last_pair = Expression::Pair(
                Rc::new(RefCell::new(v.remove(i))),
                Rc::new(RefCell::new(last_pair.clone())),
            );
            if i == 0 {
                break;
            }
            i -= 1;
        }
    }
    last_pair
}

fn close_list(level: i32, stack: &mut Vec<List>) -> Result<(), ParseError> {
    if level < 0 {
        return Err(ParseError {
            reason: "Unexpected `)`".to_string(),
        });
    }
    if level > 0 {
        match stack.pop() {
            Some(mut v) => match stack.pop() {
                Some(mut v2) => {
                    match v.list_type {
                        ListType::Vector => {
                            v2.vec.push(Expression::with_list(v.vec));
                        }
                        ListType::List => {
                            if v.vec.len() == 3 && v.vec[1].to_string() == "." {
                                v2.vec.push(Expression::Pair(
                                    Rc::new(RefCell::new(v.vec[0].clone())),
                                    Rc::new(RefCell::new(v.vec[2].clone())),
                                ));
                            } else {
                                let cons_list = to_cons(&mut v.vec);
                                v2.vec.push(cons_list);
                            }
                        }
                    }
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

fn parse(tokens: &[Token]) -> Result<Expression, ParseError> {
    if tokens.is_empty() {
        return Err(ParseError {
            reason: "No tokens".to_string(),
        });
    }
    if tokens[0].token != "("
        && tokens[0].token != "#("
        && tokens[0].token != "'"
        && tokens[0].token != "`"
    {
        return Err(ParseError {
            reason: "Not a list".to_string(),
        });
    }
    let mut stack: Vec<List> = Vec::new();
    let mut level = 0;
    let mut qexits: Vec<i32> = Vec::new();
    let mut backtick_level = 0;
    for token_full in tokens {
        let token = &token_full.token;
        match &token[..] {
            "'" => {
                level += 1;
                qexits.push(level);
                let mut quoted = Vec::<Expression>::new();
                quoted.push(Expression::Atom(Atom::Symbol("quote".to_string())));
                stack.push(List {
                    list_type: ListType::List,
                    vec: quoted,
                });
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
                stack.push(List {
                    list_type: ListType::List,
                    vec: quoted,
                });
            }
            "#(" => {
                level += 1;
                stack.push(List {
                    list_type: ListType::Vector,
                    vec: Vec::<Expression>::new(),
                });
            }
            "(" => {
                level += 1;
                stack.push(List {
                    list_type: ListType::List,
                    vec: Vec::<Expression>::new(),
                });
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
            "#<" => {
                let reason = format!(
                    "Found an unreadable token: line {}, col: {}",
                    token_full.line, token_full.column
                );
                return Err(ParseError { reason });
            }
            _ => match stack.pop() {
                Some(mut v) => {
                    v.vec.push(parse_atom(&token));
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
                    let reason = format!(
                        "Found symbol without containing list: line {}, col: {}",
                        token_full.line, token_full.column
                    );
                    return Err(ParseError { reason });
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
        for s in stack.iter_mut() {
            match s.list_type {
                ListType::Vector => {
                    // XXX do something about this stupid clone...
                    v.push(Expression::with_list(s.vec.clone()));
                }
                ListType::List => {
                    v.push(to_cons(&mut s.vec));
                }
            }
        }
        Ok(Expression::with_list(v))
    } else {
        match stack.pop() {
            Some(mut v) => match v.list_type {
                ListType::Vector => Ok(Expression::with_list(v.vec)),
                ListType::List => Ok(to_cons(&mut v.vec)),
            },
            None => Err(ParseError {
                reason: "Empty results".to_string(),
            }),
        }
    }
}

pub fn read(text: &str, add_parens: bool) -> Result<Expression, ParseError> {
    let tokens = tokenize(text, add_parens);
    parse(&tokens)
}
