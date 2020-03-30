use std::cell::RefCell;
use std::cmp::Ordering;
use std::num::{ParseFloatError, ParseIntError};
use std::rc::Rc;

use crate::environment::*;
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
        if !$token.is_empty() {
            $tokens.push(Token {
                token: $token,
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
            'A' => 10,
            'b' => 11,
            'B' => 11,
            'c' => 12,
            'C' => 12,
            'd' => 13,
            'D' => 13,
            'e' => 14,
            'E' => 14,
            'f' => 15,
            'F' => 15,
            _ => 0,
        }
    }
}

fn escape_to_char(escape_code: &[char]) -> char {
    let mut ch_n: u8 = 0;
    match escape_code.len().cmp(&1) {
        Ordering::Greater => {
            ch_n = (char_to_hex_num(escape_code[0]) * 16) + (char_to_hex_num(escape_code[1]))
        }
        Ordering::Equal => ch_n = char_to_hex_num(escape_code[0]),
        Ordering::Less => {}
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
    mut ch: char,
    last_ch: &mut char,
    last_comma: &mut bool,
    expect_char: &mut bool,
    line_column: (usize, usize),
) -> String {
    let (line, column) = line_column;
    if *last_ch == '#' && ch == '(' {
        save_token!(tokens, token, line, column);
        tokens.push(Token {
            token: "#(".to_string(),
            line,
            column,
        });
    } else if *last_ch == '#' && ch == '\\' {
        save_token!(tokens, token, line, column);
        tokens.push(Token {
            token: "#\\".to_string(),
            line,
            column,
        });
        *expect_char = true;
    } else if *last_ch == '#' && ch == '<' {
        save_token!(tokens, token, line, column);
        tokens.push(Token {
            token: "#<".to_string(),
            line,
            column,
        });
    } else if ch == '(' && *last_ch == '\\' {
        token.push(ch);
    } else if ch == '(' {
        save_token!(tokens, token, line, column);
        tokens.push(Token {
            token: "(".to_string(),
            line,
            column,
        });
    } else if ch == ')' && *last_ch == '\\' {
        token.push(ch);
    } else if ch == ')' {
        save_token!(tokens, token, line, column);
        tokens.push(Token {
            token: ")".to_string(),
            line,
            column,
        });
    } else if ch == '\''
        && (*last_ch == ' ' || *last_ch == '(' || *last_ch == '\'' || *last_ch == '`')
    {
        save_token!(tokens, token, line, column);
        tokens.push(Token {
            token: "'".to_string(),
            line,
            column,
        });
    } else if ch == '`'
        && (*last_ch == ' ' || *last_ch == '(' || *last_ch == '\'' || *last_ch == '`')
    {
        save_token!(tokens, token, line, column);
        tokens.push(Token {
            token: "`".to_string(),
            line,
            column,
        });
    } else if ch == ','
        && (is_whitespace(*last_ch) || *last_ch == '(' || *last_ch == '\'' || *last_ch == '`')
    {
        *last_comma = true;
    } else if *last_ch == '\\' && is_whitespace(ch) {
        // Keep an escaped whitespace in token since this is a shell...
        token.push(ch);
    } else if is_whitespace(ch) {
        save_token!(tokens, token, line, column);
    } else if *expect_char && ch == '\\' {
        *expect_char = false;
        token.push(ch);
        // Get rid of the \ or there will be trouble...
        ch = ' ';
    } else if ch == '\\' || ch == '#' || ch == '\n' {
        // Do nothing...
        // # is reader macro char, do not save in tokens.
    } else if *last_ch == '\\' && !*expect_char {
        token.push(*last_ch);
        token.push(ch);
    } else {
        *expect_char = false;
        token.push(ch);
    }
    *last_ch = ch;
    token
}

fn tokenize(text: &str) -> Vec<Token> {
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
    let mut expect_char = false;
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
            if !in_string {
                save_token!(tokens, token, line, column);
            }
            in_string = !in_string;
            token.push(ch);
            if !in_string {
                save_token!(tokens, token, line, column);
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
                &mut last_ch,
                &mut last_comma,
                &mut expect_char,
                (line, column),
            );
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
    tokens
}

fn parse_char(token_full: &Token) -> Result<Expression, ParseError> {
    match &token_full.token.to_lowercase()[..] {
        "space" => return Ok(Expression::Atom(Atom::Char(' '))),
        "tab" => return Ok(Expression::Atom(Atom::Char('\t'))),
        // newline should be the platform line end.
        "newline" => return Ok(Expression::Atom(Atom::Char('\n'))),
        "linefeed" => return Ok(Expression::Atom(Atom::Char('\n'))),
        "return" => return Ok(Expression::Atom(Atom::Char('\r'))),
        "backspace" => return Ok(Expression::Atom(Atom::Char('\u{0008}'))),
        _ => {}
    }
    if token_full.token.len() != 1 {
        let reason = format!(
            "Not a valid char [{}]: line {}, col: {}",
            token_full.token, token_full.line, token_full.column
        );
        return Err(ParseError { reason });
    }
    Ok(Expression::Atom(Atom::Char(
        token_full.token.chars().next().unwrap(),
    )))
}

fn parse_atom(environment: &mut Environment, token: &str) -> Expression {
    if token.is_empty() {
        return Expression::nil();
    }
    if token.len() > 1 && token.starts_with('\"') && token.ends_with('\"') {
        let string = token[1..token.len() - 1].to_string();
        return Expression::Atom(Atom::String(string));
    }

    if token == "t" {
        Expression::Atom(Atom::True)
    } else if token == "nil" {
        Expression::nil()
    } else {
        let potential_int: Result<i64, ParseIntError> = token.parse();
        match potential_int {
            Ok(v) => Expression::Atom(Atom::Int(v)),
            Err(_) => {
                let potential_float: Result<f64, ParseFloatError> = token.parse();
                match potential_float {
                    Ok(v) => Expression::Atom(Atom::Float(v)),
                    Err(_) => Expression::Atom(Atom::Symbol(environment.interner.intern(token))),
                }
            }
        }
    }
}

fn close_list(
    level: i32,
    stack: &mut Vec<List>,
    exp_meta: Option<ExpMeta>,
) -> Result<(), ParseError> {
    if level < 0 {
        return Err(ParseError {
            reason: "Unexpected `)`".to_string(),
        });
    }
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
                                Rc::new(RefCell::new(Some((v.vec[0].clone(), v.vec[2].clone())))),
                                exp_meta,
                            ));
                        } else {
                            v2.vec.push(Expression::cons_from_vec(&mut v.vec, exp_meta));
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
    Ok(())
}

fn get_meta(name: Option<String>, line: usize, col: usize) -> Option<ExpMeta> {
    if let Some(file) = name {
        Some(ExpMeta { file, line, col })
    } else {
        None
    }
}

fn parse(
    environment: &mut Environment,
    tokens: &[Token],
    name: Option<String>,
    always_wrap: bool,
) -> Result<Expression, ParseError> {
    if tokens.is_empty() {
        return Ok(Expression::nil());
    }
    let mut stack: Vec<List> = Vec::new();
    stack.push(List {
        list_type: ListType::Vector,
        vec: Vec::<Expression>::new(),
    });
    let mut level = 0;
    let mut qexits: Vec<i32> = Vec::new();
    let mut backtick_level = 0;
    let mut is_char = false;
    for token_full in tokens {
        let token = &token_full.token;
        match &token[..] {
            "'" if !is_char => {
                level += 1;
                qexits.push(level);
                let mut quoted = Vec::<Expression>::new();
                quoted.push(Expression::Atom(Atom::Symbol(
                    environment.interner.intern("quote"),
                )));
                stack.push(List {
                    list_type: ListType::List,
                    vec: quoted,
                });
            }
            "`" if !is_char => {
                level += 1;
                qexits.push(level);
                let mut quoted = Vec::<Expression>::new();
                if backtick_level > 0 {
                    quoted.push(Expression::Atom(Atom::Symbol(
                        environment.interner.intern("quote"),
                    )));
                } else {
                    quoted.push(Expression::Atom(Atom::Symbol(
                        environment.interner.intern("bquote"),
                    )));
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
            "(" if !is_char => {
                level += 1;
                stack.push(List {
                    list_type: ListType::List,
                    vec: Vec::<Expression>::new(),
                });
            }
            ")" if !is_char => {
                level -= 1;
                close_list(
                    level,
                    &mut stack,
                    get_meta(name.clone(), token_full.line, token_full.column),
                )?;
                while let Some(quote_exit_level) = qexits.pop() {
                    if level == quote_exit_level {
                        if level == backtick_level {
                            backtick_level = 0;
                        }
                        level -= 1;
                        close_list(
                            level,
                            &mut stack,
                            get_meta(name.clone(), token_full.line, token_full.column),
                        )?;
                    } else {
                        qexits.push(quote_exit_level);
                        break;
                    }
                }
            }
            "#\\" => {
                is_char = true;
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
                    let mut is_comma = false;
                    if is_char {
                        v.vec.push(parse_char(&token_full)?);
                        is_char = false;
                    } else {
                        let token = token.trim();
                        if !token.is_empty() {
                            if token == "," {
                                is_comma = true;
                            }
                            v.vec.push(parse_atom(environment, &token));
                        }
                    }
                    stack.push(v);
                    if !is_comma {
                        if let Some(quote_exit_level) = qexits.pop() {
                            if level == quote_exit_level {
                                if level == backtick_level {
                                    backtick_level = 0;
                                }
                                level -= 1;
                                close_list(
                                    level,
                                    &mut stack,
                                    get_meta(name.clone(), token_full.line, token_full.column),
                                )?;
                            } else {
                                qexits.push(quote_exit_level);
                            }
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
                close_list(level, &mut stack, get_meta(name.clone(), 0, 0))?;
            }
        }
    }
    if level != 0 {
        return Err(ParseError {
            reason: "Unclosed list(s)".to_string(),
        });
    }
    let exp_meta = get_meta(name, 0, 0);
    close_list(level, &mut stack, exp_meta.clone())?;
    if stack.len() > 1 {
        Err(ParseError {
            reason: "WTF?".to_string(),
        })
    } else {
        match stack.pop() {
            Some(mut v) => {
                if v.vec.is_empty() {
                    Err(ParseError {
                        reason: "Empty results".to_string(),
                    })
                } else if v.vec.len() == 1 && !always_wrap {
                    // If fe only have one thing and it is a vector or list then
                    // remove the outer list that was added (unless always_wrap
                    // is set).
                    let exp = v.vec.pop().unwrap();
                    match exp {
                        Expression::Vector(_, _) => Ok(exp),
                        Expression::Pair(_, _) => Ok(exp),
                        _ => {
                            v.vec.push(exp);
                            Ok(Expression::with_list_meta(v.vec, exp_meta))
                        }
                    }
                } else {
                    Ok(Expression::with_list_meta(v.vec, exp_meta))
                }
            }
            None => Err(ParseError {
                reason: "WTF, Empty results".to_string(),
            }),
        }
    }
}

pub fn read(
    environment: &mut Environment,
    text: &str,
    name: Option<String>,
) -> Result<Expression, ParseError> {
    let tokens = tokenize(text);
    parse(environment, &tokens, name, false)
}

// Read the text but always wrap in an outer list even if text is one list.
// Useful for loading scripts.
pub fn read_list_wrap(
    environment: &mut Environment,
    text: &str,
    name: Option<String>,
) -> Result<Expression, ParseError> {
    let tokens = tokenize(text);
    parse(environment, &tokens, name, true)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {
        let tokens = tokenize("one two three \"four\" 5 6");
        assert!(tokens[0].token == "one");
        assert!(tokens[1].token == "two");
        assert!(tokens[2].token == "three");
        assert!(tokens[3].token == "\"four\"");
        assert!(tokens[4].token == "5");
        assert!(tokens[5].token == "6");
        let tokens = tokenize("(1 2 3)");
        assert!(tokens[0].token == "(");
        assert!(tokens[1].token == "1");
        assert!(tokens[2].token == "2");
        assert!(tokens[3].token == "3");
        assert!(tokens[4].token == ")");
        let tokens = tokenize("'(1 2 3)");
        assert!(tokens[0].token == "'");
        assert!(tokens[1].token == "(");
        assert!(tokens[2].token == "1");
        assert!(tokens[3].token == "2");
        assert!(tokens[4].token == "3");
        assert!(tokens[5].token == ")");
        let tokens = tokenize("`(1 2 ,3)");
        assert!(tokens[0].token == "`");
        assert!(tokens[1].token == "(");
        assert!(tokens[2].token == "1");
        assert!(tokens[3].token == "2");
        assert!(tokens[4].token == ",");
        assert!(tokens[5].token == "3");
        assert!(tokens[6].token == ")");
        let tokens = tokenize("`(1 2 ,@3)");
        assert!(tokens[0].token == "`");
        assert!(tokens[1].token == "(");
        assert!(tokens[2].token == "1");
        assert!(tokens[3].token == "2");
        assert!(tokens[4].token == ",@");
        assert!(tokens[5].token == "3");
        assert!(tokens[6].token == ")");
        let tokens = tokenize("  (  1    2\t3   )  ");
        assert!(tokens[0].token == "(");
        assert!(tokens[1].token == "1");
        assert!(tokens[2].token == "2");
        assert!(tokens[3].token == "3");
        assert!(tokens[4].token == ")");
        let tokens = tokenize("#(#\\A 2 3)");
        assert!(tokens[0].token == "#(");
        assert!(tokens[1].token == "#\\");
        assert!(tokens[2].token == "A");
        assert!(tokens[3].token == "2");
        assert!(tokens[4].token == "3");
        assert!(tokens[5].token == ")");
    }

    #[test]
    fn test_tok_strings() {
        let tokens = tokenize(
            "\"on\\te\\ntwo\" two \"th\\rree\" \"fo\\\"u\\\\r\" 5 6 \"slash\\x2fx\\x2F\\x3a\\x3b\"",
        );
        assert!(tokens[0].token == "\"on\te\ntwo\"");
        assert!(tokens[1].token == "two");
        assert!(tokens[2].token == "\"th\rree\"");
        assert!(tokens[3].token == "\"fo\"u\\r\"");
        assert!(tokens[4].token == "5");
        assert!(tokens[5].token == "6");
        println!("XXXX {}", tokens[6].token);
        assert!(tokens[6].token == "\"slash/x/:;\"");
    }
}
