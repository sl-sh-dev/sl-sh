use std::cell::RefCell;
use std::cmp::Ordering;
use std::num::{ParseFloatError, ParseIntError};
use std::rc::Rc;

use crate::environment::*;
use crate::types::*;

pub trait PeekableIterator: std::iter::Iterator {
    fn peek(&mut self) -> Option<&Self::Item>;
}

impl<I: std::iter::Iterator> PeekableIterator for std::iter::Peekable<I> {
    fn peek(&mut self) -> Option<&Self::Item> {
        std::iter::Peekable::peek(self)
    }
}

enum ListType {
    Vector,
    List,
}

struct List {
    list_type: ListType,
    vec: Vec<Expression>,
}

fn is_whitespace(ch: char) -> bool {
    match ch {
        ' ' => true,
        '\t' => true,
        '\n' => true,
        _ => false,
    }
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

fn close_list(stack: &mut Vec<List>, exp_meta: Option<ExpMeta>) -> Result<(), ParseError> {
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

fn get_meta(name: Option<&'static str>, line: usize, col: usize) -> Option<ExpMeta> {
    if let Some(file) = name {
        Some(ExpMeta { file, line, col })
    } else {
        None
    }
}

fn consume_line_comment<P>(chars: &mut P, line: &mut usize, column: &mut usize)
where
    P: PeekableIterator<Item = char>,
{
    for ch in chars {
        if ch == '\n' {
            *line += 1;
            *column = 0;
            return;
        }
    }
}

fn consume_block_comment<P>(chars: &mut P, line: &mut usize, column: &mut usize)
where
    P: PeekableIterator<Item = char>,
{
    let mut depth = 1;
    let mut last_ch = ' ';
    for ch in chars {
        if ch == '\n' {
            *line += 1;
            *column = 0;
        } else {
            *column += 1;
        }
        if last_ch == '|' && ch == '#' {
            depth -= 1;
        }
        if last_ch == '#' && ch == '|' {
            depth += 1;
        }
        last_ch = ch;
        if depth == 0 {
            break;
        }
    }
}

fn end_symbol(ch: char) -> bool {
    if is_whitespace(ch) {
        true
    } else {
        match ch {
            '(' => true,
            ')' => true,
            '#' => true,
            '"' => true,
            ',' => true,
            '\'' => true,
            '`' => true,
            _ => false,
        }
    }
}

fn do_char(symbol: &str, line: usize, column: usize) -> Result<Expression, ParseError> {
    match &symbol.to_lowercase()[..] {
        "space" => return Ok(Expression::Atom(Atom::Char(' '))),
        "tab" => return Ok(Expression::Atom(Atom::Char('\t'))),
        // newline should be the platform line end.
        "newline" => return Ok(Expression::Atom(Atom::Char('\n'))),
        "linefeed" => return Ok(Expression::Atom(Atom::Char('\n'))),
        "return" => return Ok(Expression::Atom(Atom::Char('\r'))),
        "backspace" => return Ok(Expression::Atom(Atom::Char('\u{0008}'))),
        _ => {}
    }
    if symbol.len() != 1 {
        let reason = format!(
            "Not a valid char [{}]: line {}, col: {}",
            symbol, line, column
        );
        return Err(ParseError { reason });
    }
    Ok(Expression::Atom(Atom::Char(symbol.chars().next().unwrap())))
}

fn read_string<P>(
    chars: &mut P,
    symbol: &mut String,
    line: &mut usize,
    column: &mut usize,
) -> Result<Expression, ParseError>
where
    P: PeekableIterator<Item = char>,
{
    symbol.clear();
    let mut escape_code: Vec<char> = Vec::with_capacity(2);
    let mut in_escape_code = false;
    let mut last_ch = ' ';
    let mut skip_last_ch = false;

    for ch in chars {
        if ch == '\n' {
            *line += 1;
            *column = 0;
        } else {
            *column += 1;
        }
        if in_escape_code {
            escape_code.push(ch);
            if escape_code.len() == 2 {
                symbol.push(escape_to_char(&escape_code));
                escape_code.clear();
                in_escape_code = false;
            }
        } else if last_ch == '\\' {
            match ch {
                'n' => symbol.push('\n'),
                'r' => symbol.push('\r'),
                't' => symbol.push('\t'),
                '"' => symbol.push('"'),
                'x' => {
                    in_escape_code = true;
                }
                '\\' => {
                    skip_last_ch = true;
                    symbol.push('\\');
                }
                _ => {
                    symbol.push('\\');
                    symbol.push(ch);
                }
            }
        } else {
            if ch == '"' {
                break;
            }
            if ch != '\\' {
                symbol.push(ch);
            }
        }

        last_ch = if skip_last_ch {
            skip_last_ch = false;
            ' '
        } else {
            ch
        }
    }
    Ok(Expression::Atom(Atom::String(symbol.clone())))
}

fn do_atom(environment: &mut Environment, symbol: &str) -> Expression {
    if symbol.is_empty() {
        return Expression::nil();
    }
    if symbol == "t" {
        Expression::Atom(Atom::True)
    } else if symbol == "nil" {
        Expression::nil()
    } else {
        let potential_int: Result<i64, ParseIntError> = symbol.parse();
        match potential_int {
            Ok(v) => Expression::Atom(Atom::Int(v)),
            Err(_) => {
                let potential_float: Result<f64, ParseFloatError> = symbol.parse();
                match potential_float {
                    Ok(v) => Expression::Atom(Atom::Float(v)),
                    Err(_) => Expression::Atom(Atom::Symbol(environment.interner.intern(symbol))),
                }
            }
        }
    }
}

fn push_stack(
    stack: &mut Vec<List>,
    expression: Expression,
    line: usize,
    column: usize,
) -> Result<(), ParseError> {
    match stack.pop() {
        Some(mut v) => {
            v.vec.push(expression);
            stack.push(v);
            Ok(())
        }
        None => {
            let reason = format!(
                "Found symbol without containing list: line {}, col: {}",
                line, column
            );
            Err(ParseError { reason })
        }
    }
}

fn read_symbol<P>(
    buffer: &mut String,
    chars: &mut P,
    line: &mut usize,
    column: &mut usize,
    for_ch: bool,
) where
    P: PeekableIterator<Item = char>,
{
    let mut has_peek;
    let mut push_next = false;
    if let Some(ch) = chars.peek() {
        if end_symbol(*ch) && !for_ch {
            return;
        }
    };
    let mut next_ch = chars.next();
    while next_ch.is_some() {
        let ch = next_ch.unwrap();
        let peek_ch = if let Some(pch) = chars.peek() {
            has_peek = true;
            *pch
        } else {
            has_peek = false;
            ' '
        };
        if ch == '\n' {
            *line += 1;
            *column = 0;
        } else {
            *column += 1;
        }
        if ch == '\\' && has_peek && !for_ch {
            push_next = true;
        } else {
            buffer.push(ch);
        }
        if push_next {
            buffer.push(chars.next().unwrap());
            push_next = false;
        } else if end_symbol(peek_ch) {
            break;
        }
        next_ch = chars.next();
    }
}

fn next2<P>(chars: &mut P) -> Option<(char, char)>
where
    P: PeekableIterator<Item = char>,
{
    let next_ch = chars.next();
    if let Some(ch) = next_ch {
        let peek_ch = if let Some(pch) = chars.peek() {
            *pch
        } else {
            ' '
        };
        Some((ch, peek_ch))
    } else {
        None
    }
}

fn read_inner<P>(
    environment: &mut Environment,
    chars: &mut P,
    stack: &mut Vec<List>,
    buffer: &mut String,
    line: &mut usize,
    column: &mut usize,
    name: Option<&'static str>,
    in_bquote: bool,
) -> Result<bool, ParseError>
where
    P: PeekableIterator<Item = char>,
{
    let mut level = 0;
    let mut next_chars = next2(chars);
    let mut read_next = false;
    while next_chars.is_some() {
        let (mut ch, mut peek_ch) = next_chars.unwrap();

        // Consume leading whitespace.
        while is_whitespace(ch) {
            if ch == '\n' {
                *line += 1;
                *column = 0;
            } else {
                *column += 1;
            }
            if let Some((tch, pch)) = next2(chars) {
                ch = tch;
                peek_ch = pch;
            } else {
                return Ok(false);
            };
        }

        if ch == '\n' {
            *line += 1;
            *column = 0;
        } else {
            *column += 1;
        }
        match ch {
            '"' => {
                push_stack(
                    stack,
                    read_string(chars, buffer, line, column)?,
                    *line,
                    *column,
                )?;
            }
            '\'' => {
                let mut quoted = Vec::<Expression>::new();
                quoted.push(Expression::Atom(Atom::Symbol(
                    environment.interner.intern("quote"),
                )));
                stack.push(List {
                    list_type: ListType::List,
                    vec: quoted,
                });
                read_inner(
                    environment,
                    chars,
                    stack,
                    buffer,
                    line,
                    column,
                    name,
                    in_bquote,
                )?;
                close_list(stack, get_meta(name, *line, *column))?;
            }
            '`' => {
                let mut quoted = Vec::<Expression>::new();
                if in_bquote {
                    quoted.push(Expression::Atom(Atom::Symbol(
                        environment.interner.intern("quote"),
                    )));
                } else {
                    quoted.push(Expression::Atom(Atom::Symbol(
                        environment.interner.intern("bquote"),
                    )));
                }
                stack.push(List {
                    list_type: ListType::List,
                    vec: quoted,
                });
                read_inner(environment, chars, stack, buffer, line, column, name, true)?;
                close_list(stack, get_meta(name, *line, *column))?;
            }
            ',' => {
                read_next = true; // , always needs the symbol after
                if peek_ch == '@' {
                    chars.next();
                    push_stack(
                        stack,
                        Expression::Atom(Atom::Symbol(environment.interner.intern(",@"))),
                        *line,
                        *column,
                    )?;
                } else {
                    push_stack(
                        stack,
                        Expression::Atom(Atom::Symbol(environment.interner.intern(","))),
                        *line,
                        *column,
                    )?;
                }
            }
            '#' => {
                chars.next();
                match peek_ch {
                    '|' => consume_block_comment(chars, line, column),
                    '\\' => {
                        buffer.clear();
                        read_symbol(buffer, chars, line, column, true);
                        push_stack(stack, do_char(buffer, *line, *column)?, *line, *column)?;
                    }
                    '<' => {
                        let reason =
                            format!("Found an unreadable token: line {}, col: {}", line, column);
                        return Err(ParseError { reason });
                    }
                    '(' => {
                        level += 1;
                        stack.push(List {
                            list_type: ListType::Vector,
                            vec: Vec::<Expression>::new(),
                        });
                    }
                    't' => push_stack(stack, Expression::Atom(Atom::True), *line, *column)?,
                    _ => {
                        let reason = format!(
                            "Found # with invalid char {}: line {}, col: {}",
                            peek_ch, line, column
                        );
                        return Err(ParseError { reason });
                    }
                }
            }
            '(' => {
                level += 1;
                stack.push(List {
                    list_type: ListType::List,
                    vec: Vec::<Expression>::new(),
                });
            }
            ')' => {
                if level <= 0 {
                    return Err(ParseError {
                        reason: "Unexpected `)`".to_string(),
                    });
                }
                level -= 1;
                close_list(stack, get_meta(name, *line, *column))?;
            }
            ';' => {
                consume_line_comment(chars, line, column);
            }
            _ => {
                buffer.clear();
                buffer.push(ch);
                read_symbol(buffer, chars, line, column, false);
                push_stack(stack, do_atom(environment, buffer), *line, *column)?;
            }
        }
        if level == 0 && !read_next {
            return Ok(true);
        }
        read_next = false;
        next_chars = next2(chars);
    }
    if level != 0 {
        Err(ParseError {
            reason: "Unclosed list(s)".to_string(),
        })
    } else {
        Ok(false)
    }
}

fn read2(
    environment: &mut Environment,
    text: &str,
    name: Option<&'static str>,
    always_wrap: bool,
) -> Result<Expression, ParseError> {
    let mut buffer = String::new();
    let mut line = 1;
    let mut column = 0;

    let mut stack: Vec<List> = Vec::new();
    stack.push(List {
        list_type: ListType::Vector,
        vec: Vec::<Expression>::new(),
    });
    let mut chars = text.chars().peekable();
    if text.starts_with("#!") {
        // Work with shebanged scripts.
        consume_line_comment(&mut chars, &mut line, &mut column);
    }
    while read_inner(
        environment,
        &mut chars,
        &mut stack,
        &mut buffer,
        &mut line,
        &mut column,
        name,
        false,
    )? {}
    if chars.next().is_some() {
        let reason = format!(
            "Premature end (to many ')'?) line: {}, column: {}",
            line, column
        );
        return Err(ParseError { reason });
    }
    let exp_meta = get_meta(name, 0, 0);
    close_list(&mut stack, exp_meta.clone())?;
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
                    // If we only have one thing and it is a vector or list then
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
    name: Option<&'static str>,
) -> Result<Expression, ParseError> {
    read2(environment, text, name, false)
}

// Read the text but always wrap in an outer list even if text is one list.
// Useful for loading scripts.
pub fn read_list_wrap(
    environment: &mut Environment,
    text: &str,
    name: Option<&'static str>,
) -> Result<Expression, ParseError> {
    read2(environment, text, name, true)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicBool;
    use std::sync::Arc;

    use crate::builtins_util::is_proper_list;

    fn to_strs(output: &mut Vec<String>, exp: &Expression) {
        match exp {
            Expression::Vector(list, _) => {
                output.push("#(".to_string());
                for exp in list.borrow().iter() {
                    to_strs(output, exp);
                }
                output.push(")".to_string());
            }
            Expression::Pair(p, _) => {
                if let Some((e1, e2)) = &*p.borrow() {
                    if is_proper_list(exp) {
                        output.push("(".to_string());
                        for p in exp.iter() {
                            to_strs(output, p);
                        }
                        output.push(")".to_string());
                    } else {
                        output.push("(".to_string());
                        to_strs(output, e1);
                        output.push(".".to_string());
                        to_strs(output, e2);
                        output.push(")".to_string());
                    }
                } else {
                    output.push("nil".to_string());
                }
            }
            _ => {
                output.push(format!("{}:{}", exp.display_type(), exp.to_string()));
            }
        }
    }

    fn tokenize(
        environment: &mut Environment,
        input: &str,
        name: Option<&'static str>,
    ) -> Vec<String> {
        let exp = read(environment, input, name);
        let mut tokens = Vec::new();
        if let Ok(exp) = exp {
            to_strs(&mut tokens, &exp);
        } else {
            assert!(false);
        }
        tokens
    }

    #[test]
    fn test_tokenize() {
        let mut environment = build_default_environment(Arc::new(AtomicBool::new(false)));
        let tokens = tokenize(&mut environment, "one two three \"four\" 5 6", None);
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Symbol:one");
        assert!(tokens[2] == "Symbol:two");
        assert!(tokens[3] == "Symbol:three");
        assert!(tokens[4] == "String:\"four\"");
        assert!(tokens[5] == "Int:5");
        assert!(tokens[6] == "Int:6");
        assert!(tokens[7] == ")");
        let tokens = tokenize(&mut environment, "(1 2 3)", None);
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize(&mut environment, "'(1 2 3)", None);
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "Int:1");
        assert!(tokens[4] == "Int:2");
        assert!(tokens[5] == "Int:3");
        assert!(tokens[6] == ")");
        assert!(tokens[7] == ")");
        let tokens = tokenize(&mut environment, "`(1 2 ,3)", None);
        assert!(tokens.len() == 9);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:bquote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "Int:1");
        assert!(tokens[4] == "Int:2");
        assert!(tokens[5] == "Symbol:,");
        assert!(tokens[6] == "Int:3");
        assert!(tokens[7] == ")");
        assert!(tokens[8] == ")");
        let tokens = tokenize(&mut environment, "`(1 2 ,@3)", None);
        assert!(tokens.len() == 9);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:bquote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "Int:1");
        assert!(tokens[4] == "Int:2");
        assert!(tokens[5] == "Symbol:,@");
        assert!(tokens[6] == "Int:3");
        assert!(tokens[7] == ")");
        assert!(tokens[8] == ")");
        let tokens = tokenize(&mut environment, "  (  1    2\t3   )  ", None);
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize(&mut environment, "#(#\\A 2 3)", None);
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Char:#\\A");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize(&mut environment, "#(#\\  2 3)", None);
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Char:#\\ ");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize(&mut environment, "'((1 2 (3)))", None);
        assert!(tokens.len() == 12);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "(");
        assert!(tokens[4] == "Int:1");
        assert!(tokens[5] == "Int:2");
        assert!(tokens[6] == "(");
        assert!(tokens[7] == "Int:3");
        assert!(tokens[8] == ")");
        assert!(tokens[9] == ")");
        assert!(tokens[10] == ")");
        assert!(tokens[11] == ")");
        let tokens = tokenize(&mut environment, "(length \"12345\")", None);
        assert!(tokens.len() == 4);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:length");
        assert!(tokens[2] == "String:\"12345\"");
        assert!(tokens[3] == ")");
        let tokens = tokenize(&mut environment, "(length \"12345Σ\")", None);
        assert!(tokens.len() == 4);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:length");
        assert!(tokens[2] == "String:\"12345Σ\"");
        assert!(tokens[3] == ")");
    }

    #[test]
    fn test_tok_strings() {
        let mut environment = build_default_environment(Arc::new(AtomicBool::new(false)));
        let input =
            "\"on\\te\\ntwo\" two \"th\\rree\" \"fo\\\"u\\\\r\" 5 6 \"slash\\x2fx\\x2F\\x3a\\x3b\"";
        let tokens = tokenize(&mut environment, input, None);
        println!("XXX input: {}", input);
        for s in &tokens {
            println!("XXX token {}", s);
        }
        assert!(tokens.len() == 9);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "String:\"on\te\ntwo\"");
        assert!(tokens[2] == "Symbol:two");
        assert!(tokens[3] == "String:\"th\rree\"");
        assert!(tokens[4] == "String:\"fo\"u\\r\"");
        assert!(tokens[5] == "Int:5");
        assert!(tokens[6] == "Int:6");
        assert!(tokens[7] == "String:\"slash/x/:;\"");
        assert!(tokens[8] == ")");
    }
}
