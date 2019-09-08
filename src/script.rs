use std::num::{ParseFloatError, ParseIntError};

use crate::types::*;

pub fn is_whitespace(ch: char) -> bool {
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

pub fn tokenize(text: &str) -> Vec<String> {
    let mut tokens: Vec<String> = Vec::new();
    let mut in_string = false;
    let mut token = String::new();
    let mut last_ch = ' ';
    let mut in_comment = false;
    for ch in text.chars() {
        if in_comment {
            if ch == '\n' {
                in_comment = false;
            }
            continue;
        }
        if ch == '\"' && last_ch != '\\' {
            // Kakoune bug "
            in_string = !in_string;
            token.push(ch);
            if !in_string {
                tokens.push(token);
                token = String::new();
            }
            last_ch = ch;
            continue;
        }
        if in_string {
            token.push(ch);
        } else {
            if ch == ';' {
                // Comment, ignore the rest of the line.
                in_comment = true;
                continue;
            }
            if ch == '(' {
                save_token!(tokens, token);
                tokens.push("(".to_string());
            } else if ch == ')' {
                save_token!(tokens, token);
                tokens.push(")".to_string());
            } else if ch == '\''
                && (last_ch == ' ' || last_ch == '(' || last_ch == '\'' || last_ch == '`')
            {
                save_token!(tokens, token);
                tokens.push("'".to_string());
            } else if ch == '`'
                && (last_ch == ' ' || last_ch == '(' || last_ch == '\'' || last_ch == '`')
            {
                save_token!(tokens, token);
                tokens.push("`".to_string());
            } else if ch == ',' && (last_ch == ' ' || last_ch == '(') {
                save_token!(tokens, token);
                tokens.push(",".to_string());
            } else if is_whitespace(ch) {
                save_token!(tokens, token);
            } else {
                token.push(ch);
            }
        }
        last_ch = ch;
    }
    tokens
}

fn parse_atom(token: &str) -> Expression {
    if token.is_empty() {
        return Expression::Atom(Atom::Nil);
    }
    if token.len() > 1 && token.starts_with('\"') && token.ends_with('\"') {
        // Kakoune bug "
        return Expression::Atom(Atom::String(token[1..token.len() - 1].to_string()));
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
                    v2.push(Expression::List(v));
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

pub fn parse(tokens: &[String]) -> Result<Expression, ParseError> {
    if tokens.is_empty() {
        return Err(ParseError {
            reason: "No tokens".to_string(),
        });
    }
    if tokens[0] != "(" {
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
    if level != 0 {
        return Err(ParseError {
            reason: "Unclosed list(s)".to_string(),
        });
    }
    if stack.len() > 1 {
        let mut v: Vec<Expression> = Vec::new();
        for s in stack {
            v.push(Expression::List(s));
        }
        Ok(Expression::List(v))
    } else {
        match stack.pop() {
            Some(v) => Ok(Expression::List(v)),
            None => Err(ParseError {
                reason: "Empty results".to_string(),
            }),
        }
    }
}
