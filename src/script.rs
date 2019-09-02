use std::num::{ParseFloatError, ParseIntError};

#[derive(Clone, Copy, Debug)]
pub enum Number {
    Float(f64),
    Int(i64),
}

#[derive(Clone, Debug)]
pub enum Expression {
    Nil,
    Symbol(String),
    Number(Number),
    String(String),
    List(Vec<Expression>),
}

#[derive(Clone, Debug)]
pub struct ParseError {
    reason: String,
}

pub fn is_whitespace(ch: char) -> bool {
    match ch {
        ' ' => true,
        '\t' => true,
        _ => false,
    }
}

pub fn tokenize(text: &str) -> Vec<String> {
    let mut tokens: Vec<String> = Vec::new();
    let mut in_string = false;
    let mut token = String::new();
    let mut last_ch = ' ';
    for ch in text.chars() {
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
                return tokens;
            }
            if ch == '(' {
                if !token.is_empty() {
                    tokens.push(token);
                    token = String::new();
                }
                tokens.push("(".to_string());
            } else if ch == ')' {
                if !token.is_empty() {
                    tokens.push(token);
                    token = String::new();
                }
                tokens.push(")".to_string());
            } else if is_whitespace(ch) {
                if !token.is_empty() {
                    tokens.push(token);
                    token = String::new();
                }
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
        return Expression::Nil;
    }
    if token.len() > 1 && token.starts_with('\"') && token.ends_with('\"') {
        // Kakoune bug "
        return Expression::String(token[1..token.len() - 1].to_string());
    }

    let potential_int: Result<i64, ParseIntError> = token.parse();
    match potential_int {
        Ok(v) => Expression::Number(Number::Int(v)),
        Err(_) => {
            let potential_float: Result<f64, ParseFloatError> = token.parse();
            match potential_float {
                Ok(v) => Expression::Number(Number::Float(v)),
                Err(_) => Expression::Symbol(token.to_string().clone()),
            }
        }
    }
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
    for token in tokens {
        match &token[..] {
            "(" => {
                level += 1;
                stack.push(Vec::<Expression>::new());
            }
            ")" => {
                level -= 1;
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
            }
            _ => match stack.pop() {
                Some(mut v) => {
                    v.push(parse_atom(token));
                    stack.push(v);
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
