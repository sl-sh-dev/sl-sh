use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;
use std::num::{ParseFloatError, ParseIntError};

use unicode_segmentation::UnicodeSegmentation;

use crate::builtins_hashmap::cow_to_ref;
use crate::environment::*;
use crate::eval::eval;
use crate::types::*;

#[derive(Clone, Debug)]
pub struct ReadError {
    pub reason: String,
}

impl Error for ReadError {}

impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.reason)
    }
}

fn is_whitespace(ch: &str) -> bool {
    matches!(ch, " " | "\t" | "\n")
}

fn char_to_hex_num(ch: &str) -> Result<u8, ReadError> {
    if ("0"..="9").contains(&ch) {
        Ok(ch.chars().next().unwrap() as u8 - b'0')
    } else {
        match ch {
            "a" => Ok(10),
            "A" => Ok(10),
            "b" => Ok(11),
            "B" => Ok(11),
            "c" => Ok(12),
            "C" => Ok(12),
            "d" => Ok(13),
            "D" => Ok(13),
            "e" => Ok(14),
            "E" => Ok(14),
            "f" => Ok(15),
            "F" => Ok(15),
            _ => Err(ReadError {
                reason: format!("Invalid hex digit {}, expected 0-9 or A-F.", ch),
            }),
        }
    }
}

fn escape_to_char(chars: &mut CharIter, reader_state: &mut ReaderState) -> Result<char, ReadError> {
    if let (Some(ch1), Some(ch2)) = (chars.next(), chars.next()) {
        reader_state.column += 1;
        let ch_n: u8 = (char_to_hex_num(&*ch1)? * 16) + (char_to_hex_num(&*ch2)?);
        if ch_n > 0x7f {
            Err(ReadError {
                reason: "Invalid hex ascii code, must be less then \\x7f.".to_string(),
            })
        } else {
            Ok(ch_n as char)
        }
    } else {
        Err(ReadError {
            reason: "Invalid hex ascii code, expected two digits.".to_string(),
        })
    }
}

fn make_exp(data: ExpEnum, meta: Option<ExpMeta>) -> Expression {
    Expression::alloc(ExpObj {
        data,
        meta,
        meta_tags: None,
        analyzed: RefCell::new(false),
    })
}

fn get_meta(name: Option<&'static str>, line: usize, col: usize) -> Option<ExpMeta> {
    if let Some(file) = name {
        Some(ExpMeta { file, line, col })
    } else {
        None
    }
}

fn consume_line_comment(chars: &mut CharIter, reader_state: &mut ReaderState) {
    for ch in chars {
        if ch == "\n" {
            reader_state.line += 1;
            reader_state.column = 0;
            return;
        }
    }
}

fn consume_block_comment(chars: &mut CharIter, reader_state: &mut ReaderState) {
    let mut depth = 1;
    let mut last_ch = Cow::Borrowed(" ");
    for ch in chars {
        if ch == "\n" {
            reader_state.line += 1;
            reader_state.column = 0;
        } else {
            reader_state.column += 1;
        }
        if last_ch == "|" && ch == "#" {
            depth -= 1;
        }
        if last_ch == "#" && ch == "|" {
            depth += 1;
        }
        last_ch = ch;
        if depth == 0 {
            break;
        }
    }
}

fn end_symbol(ch: &str, reader_state: &mut ReaderState) -> bool {
    if is_whitespace(ch) || (reader_state.end_ch.is_some() && ch == reader_state.end_ch.unwrap()) {
        true
    } else {
        matches!(ch, "(" | ")" | "#" | "\"" | "," | "'" | "`")
    }
}

fn is_digit(ch: &str) -> bool {
    matches!(
        ch,
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    )
}

fn do_char(
    environment: &mut Environment,
    symbol: &str,
    meta: Option<ExpMeta>,
) -> Result<Expression, ReadError> {
    match &symbol.to_lowercase()[..] {
        "space" => return Ok(Expression::alloc_data(ExpEnum::Char(" ".into()))),
        "tab" => return Ok(Expression::alloc_data(ExpEnum::Char("\t".into()))),
        // newline should be the platform line end.
        "newline" => return Ok(Expression::alloc_data(ExpEnum::Char("\n".into()))),
        "linefeed" => return Ok(Expression::alloc_data(ExpEnum::Char("\n".into()))),
        "return" => return Ok(Expression::alloc_data(ExpEnum::Char("\r".into()))),
        "backspace" => return Ok(Expression::alloc_data(ExpEnum::Char("\u{0008}".into()))),
        _ => {}
    }
    // Do this so the chars iterator has a static lifetime.  Should be ok since
    // iterator dies at the end of the function and symbol does not.
    // Note: interning the chars below keeps from using the temp buffer.
    let ntext = unsafe { &*(symbol as *const str) };
    let mut chars: CharIter = Box::new(
        UnicodeSegmentation::graphemes(ntext, true)
            .map(|s| Cow::Borrowed(s))
            .peekable(),
    );
    if let Some(ch) = chars.next() {
        if chars.peek().is_some() {
            match &*ch {
                "u" => {
                    let reader_state = &mut environment.reader_state.as_mut().unwrap();
                    let char_str = format!("{}", read_utf_scalar(&mut chars, reader_state,)?);
                    return Ok(Expression::alloc_data(ExpEnum::Char(char_str.into())));
                }
                "x" => {
                    let reader_state = &mut environment.reader_state.as_mut().unwrap();
                    let char_str = format!("{}", escape_to_char(&mut chars, reader_state,)?);
                    return Ok(Expression::alloc_data(ExpEnum::Char(char_str.into())));
                }
                _ => {
                    let reader_state = &mut environment.reader_state.as_mut().unwrap();
                    let reason = format!(
                        "Not a valid char [{}]: line {}, col: {}",
                        symbol, reader_state.line, reader_state.column
                    );
                    return Err(ReadError { reason });
                }
            }
        }
        Ok(make_exp(
            ExpEnum::Char(environment.interner.intern(&*ch).into()),
            meta,
        ))
    } else {
        let reader_state = &mut environment.reader_state.as_mut().unwrap();
        let reason = format!(
            "Not a valid char [{}]: line {}, col: {}",
            symbol, reader_state.line, reader_state.column
        );
        Err(ReadError { reason })
    }
}

fn read_utf_scalar(
    chars: &mut CharIter,
    reader_state: &mut ReaderState,
) -> Result<char, ReadError> {
    fn finish(char_u32: u32) -> Result<char, ReadError> {
        if let Some(val) = std::char::from_u32(char_u32) {
            Ok(val)
        } else {
            Err(ReadError {
                reason: format!(
                    "Invalid unicode scalar, {:x} not a valid utf scalar.",
                    char_u32
                ),
            })
        }
    }
    let mut first = true;
    let mut has_bracket = false;
    let mut char_u32 = 0;
    let mut nibbles = 0;
    let mut out_ch = chars.next();
    while let Some(ch) = out_ch {
        if ch == "\n" {
            reader_state.line += 1;
            reader_state.column = 0;
            if has_bracket {
                return Err(ReadError {
                    reason: "Invalid unicode scalar, unexpected newline.".to_string(),
                });
            } else {
                return finish(char_u32);
            }
        } else {
            reader_state.column += 1;
        }
        if first && ch == "{" {
            has_bracket = true;
            out_ch = chars.next();
            first = false;
            continue;
        }
        first = false;
        if has_bracket && ch == "}" {
            return finish(char_u32);
        }
        if nibbles >= 8 {
            return Err(ReadError {
                reason: "Invalid unicode scalar, too many bytes (4 max).".to_string(),
            });
        }
        nibbles += 1;
        let nib = char_to_hex_num(&ch)?;
        char_u32 = (char_u32 << 4) | nib as u32;
        if let Some(pch) = chars.peek() {
            if !has_bracket && is_whitespace(&*pch) {
                return finish(char_u32);
            }
        }
        out_ch = chars.next();
    }
    if has_bracket {
        Err(ReadError {
            reason: "Invalid unicode scalar, failed to parse.".to_string(),
        })
    } else {
        finish(char_u32)
    }
}

fn read_string(
    chars: &mut CharIter,
    symbol: &mut String,
    reader_state: &mut ReaderState,
) -> Result<Expression, ReadError> {
    symbol.clear();
    let mut last_ch = Cow::Borrowed(" ");
    let mut skip_last_ch = false;
    let meta = get_meta(
        reader_state.file_name,
        reader_state.line,
        reader_state.column,
    );

    let mut out_ch = chars.next();
    while let Some(ch) = out_ch {
        if ch == "\n" {
            reader_state.line += 1;
            reader_state.column = 0;
        } else {
            reader_state.column += 1;
        }
        if last_ch == "\\" {
            match &*ch {
                "n" => symbol.push('\n'),
                "r" => symbol.push('\r'),
                "t" => symbol.push('\t'),
                "\"" => symbol.push('"'),
                "x" => symbol.push(escape_to_char(chars, reader_state)?),
                "\\" => {
                    skip_last_ch = true;
                    symbol.push('\\');
                }
                "u" => symbol.push(read_utf_scalar(chars, reader_state)?),
                _ => {
                    symbol.push('\\');
                    symbol.push_str(&ch);
                }
            }
        } else {
            if ch == "\"" {
                break;
            }
            if ch != "\\" {
                symbol.push_str(&ch);
            }
        }

        last_ch = if skip_last_ch {
            skip_last_ch = false;
            Cow::Borrowed(" ")
        } else {
            ch
        };
        out_ch = chars.next();
    }
    Ok(make_exp(ExpEnum::String(symbol.clone().into(), None), meta))
}

fn do_atom(
    environment: &mut Environment,
    symbol: &str,
    is_number: bool,
    meta: Option<ExpMeta>,
) -> Expression {
    if is_number {
        let mut num_str = symbol.to_string();
        num_str.retain(|ch| ch != '_');
        let potential_int: Result<i64, ParseIntError> = num_str.parse();
        match potential_int {
            Ok(v) => Expression::alloc_data(ExpEnum::Int(v)),
            Err(_) => {
                let potential_float: Result<f64, ParseFloatError> = num_str.parse();
                match potential_float {
                    Ok(v) => make_exp(ExpEnum::Float(v), meta),
                    Err(_) => make_exp(
                        ExpEnum::Symbol(environment.interner.intern(symbol), SymLoc::None),
                        meta,
                    ),
                }
            }
        }
    } else {
        if symbol.is_empty() {
            return make_exp(ExpEnum::Nil, meta);
        }
        if symbol == "t" {
            make_exp(ExpEnum::True, meta)
        } else if symbol == "nil" {
            make_exp(ExpEnum::Nil, meta)
        } else {
            make_exp(
                ExpEnum::Symbol(environment.interner.intern(symbol), SymLoc::None),
                meta,
            )
        }
    }
}

fn read_symbol(
    buffer: &mut String,
    chars: &mut CharIter,
    reader_state: &mut ReaderState,
    for_ch: bool,
    skip_underscore: bool,
) -> bool {
    fn maybe_number(ch: &str, has_e: &mut bool, last_e: &mut bool, has_decimal: &mut bool) -> bool {
        if ch == "." {
            if *has_decimal {
                false
            } else {
                *has_decimal = true;
                true
            }
        } else if !*has_e && ch == "e" {
            *has_e = true;
            *last_e = true;
            true
        } else {
            is_digit(&ch) || ch == "." || ch == "_" || (*last_e && (ch == "+" || ch == "-"))
        }
    }

    let mut has_peek;
    let mut push_next = false;
    let mut is_number = buffer.is_empty()
        || (buffer.len() == 1
            && (is_digit(&buffer[..])
                || (&buffer[..] == "+")
                || (&buffer[..] == "-")
                || (&buffer[..] == ".")));
    let mut has_decimal = buffer.len() == 1 && &buffer[..] == ".";
    let mut has_e = false;
    let mut last_e = false;
    if let Some(ch) = chars.peek() {
        if end_symbol(&ch, reader_state) && !for_ch {
            return buffer.len() == 1 && is_digit(&buffer[..]);
        }
    };
    let mut next_ch = chars.next();
    while next_ch.is_some() {
        let ch = next_ch.unwrap();
        let peek_ch = if let Some(pch) = chars.peek() {
            has_peek = true;
            &pch
        } else {
            has_peek = false;
            " "
        };
        if ch == "\n" {
            reader_state.line += 1;
            reader_state.column = 0;
        } else {
            reader_state.column += 1;
        }
        if ch == "\\" && has_peek && !for_ch {
            push_next = true;
        } else if !skip_underscore || ch != "_" {
            if is_number {
                is_number = maybe_number(&ch, &mut has_e, &mut last_e, &mut has_decimal);
            }
            buffer.push_str(&ch);
        }
        if push_next {
            let next_ch = chars.next().unwrap();
            if is_number {
                is_number = maybe_number(&ch, &mut has_e, &mut last_e, &mut has_decimal);
            }
            buffer.push_str(&next_ch);
            push_next = false;
        } else if end_symbol(peek_ch, reader_state) {
            break;
        }
        next_ch = chars.next();
    }
    is_number
}

fn next2(chars: &mut CharIter) -> Option<(Cow<'static, str>, Cow<'static, str>)> {
    if let Some(ch) = chars.next() {
        let peek_ch = if let Some(pch) = chars.peek() {
            pch.clone()
        } else {
            Cow::Borrowed(" ")
        };
        Some((ch, peek_ch))
    } else {
        None
    }
}

fn call_reader_macro(
    environment: &mut Environment,
    name: &str,
    stream: Expression,
    ch: &str,
    end_ch: Option<&'static str>,
) -> Result<Expression, ReadError> {
    if let Some(exp) = lookup_expression(environment, name) {
        let exp = match &exp.get().data {
            ExpEnum::Lambda(_) => {
                let mut v = Vec::with_capacity(1);
                v.push(Expression::alloc_data(ExpEnum::Symbol(
                    environment.interner.intern(name),
                    SymLoc::None,
                )));
                v.push(stream);
                v.push(Expression::alloc_data(ExpEnum::Char(ch.to_string().into())));
                Expression::with_list(v)
            }
            _ => {
                let reason = format!(
                    "Error calling reader macro (not a lambda) {}, {} : line {}, col: {}",
                    name,
                    environment
                        .reader_state
                        .as_ref()
                        .unwrap()
                        .file_name
                        .unwrap_or(""),
                    environment.reader_state.as_ref().unwrap().line,
                    environment.reader_state.as_ref().unwrap().column
                );
                return Err(ReadError { reason });
            }
        };
        let old_end_ch = environment.reader_state.as_ref().unwrap().end_ch;
        environment.reader_state.as_mut().unwrap().end_ch = end_ch;
        let res = match eval(environment, exp) {
            Ok(exp) => {
                let meta = get_meta(
                    environment.reader_state.as_ref().unwrap().file_name,
                    environment.reader_state.as_ref().unwrap().line,
                    environment.reader_state.as_ref().unwrap().column,
                );
                exp.get_mut().meta = meta;
                Ok(exp)
            }
            Err(err) => {
                let reason = format!(
                    "Error in reader {}: {} ({} : line {}, col: {})",
                    name,
                    err,
                    environment
                        .reader_state
                        .as_ref()
                        .unwrap()
                        .file_name
                        .unwrap_or(""),
                    environment.reader_state.as_ref().unwrap().line,
                    environment.reader_state.as_ref().unwrap().column
                );
                Err(ReadError { reason })
            }
        };
        environment.reader_state.as_mut().unwrap().end_ch = old_end_ch;
        res
    } else {
        let reason = format!(
            "Error calling reader macro (not found) {}, {} : line {}, col: {}",
            name,
            environment
                .reader_state
                .as_ref()
                .unwrap()
                .file_name
                .unwrap_or(""),
            environment.reader_state.as_ref().unwrap().line,
            environment.reader_state.as_ref().unwrap().column
        );
        Err(ReadError { reason })
    }
}

fn prep_reader_macro(
    environment: &mut Environment,
    chars: CharIter, // Pass ownership in and out for reader macro support.
    name: &str,
    ch: &str,
    end_ch: Option<&'static str>,
) -> Result<(Option<Expression>, CharIter), (ReadError, CharIter)> {
    fn recover_chars(stream_exp: &Expression) -> CharIter {
        let mut exp_d = stream_exp.get_mut();
        if let ExpEnum::String(_, chars_iter) = &mut exp_d.data {
            chars_iter.take().unwrap()
        } else {
            panic!("read: something happened to char iterator in reader macro!");
        }
    }
    let stream_exp = Expression::alloc_data(ExpEnum::String("".into(), Some(chars)));
    {
        let mut exp_d = stream_exp.get_mut();
        if let Some(tags) = &mut exp_d.meta_tags {
            tags.insert("--reader-text-stream--");
        } else {
            let mut tags: HashSet<&'static str> = HashSet::new();
            tags.insert("--reader-text-stream--");
            exp_d.meta_tags = Some(tags);
        }
    }
    let rm = match call_reader_macro(environment, name, stream_exp.clone(), ch, end_ch) {
        Ok(rm) => rm,
        Err(e) => {
            let chars = recover_chars(&stream_exp);
            return Err((e, chars));
        }
    };
    let res = recover_chars(&stream_exp);
    // Clear the stream expression in case the reader macro saved it for some dumb reason.
    let mut exp_d = stream_exp.get_mut();
    exp_d.data.replace(ExpEnum::Nil);
    exp_d.meta_tags = None;
    Ok((Some(rm), res))
}

fn consume_whitespace(environment: &mut Environment, chars: &mut CharIter) {
    // Consume whitespace.
    let mut ch = chars.peek();
    while ch.is_some() && is_whitespace(ch.unwrap()) {
        if let Some(ch) = ch {
            if *ch == "\n" {
                environment.reader_state.as_mut().unwrap().line += 1;
                environment.reader_state.as_mut().unwrap().column = 0;
            } else {
                environment.reader_state.as_mut().unwrap().column += 1;
            }
            chars.next();
        }
        ch = chars.peek();
    }
}

fn read_num_radix(
    environment: &mut Environment,
    mut chars: CharIter, // Pass ownership in and out for reader macro support.
    buffer: &mut String,
    radix: u32,
    meta: Option<ExpMeta>,
) -> Result<(Expression, CharIter), (ReadError, CharIter)> {
    buffer.clear();
    read_symbol(
        buffer,
        &mut chars,
        &mut environment.reader_state.as_mut().unwrap(),
        true,
        true,
    );
    match i64::from_str_radix(buffer, radix) {
        Ok(n) => Ok((make_exp(ExpEnum::Int(n), meta), chars)),
        Err(e) => Err((
            ReadError {
                reason: e.to_string(),
            },
            chars,
        )),
    }
}

fn read_vector(
    environment: &mut Environment,
    mut chars: CharIter, // Pass ownership in and out for reader macro support.
    buffer: &mut String,
    in_back_quote: bool,
) -> Result<(Expression, CharIter), (ReadError, CharIter)> {
    let mut v: Vec<Expression> = Vec::new();
    let meta = get_meta(
        environment.reader_state.as_ref().unwrap().file_name,
        environment.reader_state.as_ref().unwrap().line,
        environment.reader_state.as_ref().unwrap().column,
    );
    let mut cont = true;

    while cont {
        let (exp, mut ichars) = match read_inner(environment, chars, buffer, in_back_quote, true) {
            Ok((exp, ichars)) => {
                if let Some(exp) = &exp {
                    if let ExpEnum::Symbol(")", _) = exp.get().data {
                        return Ok((make_exp(ExpEnum::Vector(v), meta), ichars));
                    }
                }
                (exp, ichars)
            }
            Err((err, ichars)) => {
                return Err((err, ichars));
            }
        };
        let pch = ichars.peek();
        if let Some(exp) = exp {
            v.push(exp);
        } else if pch.is_none() {
            cont = false;
        }
        chars = ichars;
    }
    Err((
        ReadError {
            reason: "Unclosed vector".to_string(),
        },
        chars,
    ))
}

fn get_unquote_lst(exp: &Expression) -> Option<Expression> {
    let exp_d = exp.get();
    if let ExpEnum::Pair(car, cdr) = &exp_d.data {
        if let ExpEnum::Symbol("unquote", _) = &car.get().data {
            return Some(cdr.clone());
        }
    }
    None
}

fn is_unquote_splice(exp: &Expression) -> bool {
    let exp_d = exp.get();
    match &exp_d.data {
        ExpEnum::Pair(car, _) => {
            if let ExpEnum::Symbol("unquote-splice", _) = &car.get().data {
                return true;
            }
        }
        ExpEnum::Vector(v) => {
            if let Some(car) = v.get(0) {
                if let ExpEnum::Symbol("unquote-splice", _) = &car.get().data {
                    return true;
                }
            }
        }
        _ => {}
    }
    false
}

fn read_list(
    environment: &mut Environment,
    mut chars: CharIter, // Pass ownership in and out for reader macro support.
    buffer: &mut String,
    in_back_quote: bool,
) -> Result<(Expression, CharIter), (ReadError, CharIter)> {
    let mut head = ExpEnum::Nil;
    let mut tail = ExpEnum::Nil;
    let meta = get_meta(
        environment.reader_state.as_ref().unwrap().file_name,
        environment.reader_state.as_ref().unwrap().line,
        environment.reader_state.as_ref().unwrap().column,
    );
    let mut cont = true;
    let mut dot = false;
    let mut dot_count = 0;

    while cont {
        let (exp, mut ichars) = match read_inner(environment, chars, buffer, in_back_quote, true) {
            Ok((exp, ichars)) => {
                if let Some(exp) = &exp {
                    if let ExpEnum::Symbol(")", _) = exp.get().data {
                        return Ok((make_exp(head, meta), ichars));
                    } else if let ExpEnum::Symbol(".", _) = exp.get().data {
                        dot = true;
                        chars = ichars;
                        continue;
                    }
                }
                (exp, ichars)
            }
            Err((err, ichars)) => {
                return Err((err, ichars));
            }
        };
        let pch = ichars.peek();
        if let Some(exp) = exp {
            if let ExpEnum::Nil = head {
                if dot {
                    return Err((
                        ReadError {
                            reason: "Invalid dotted pair syntax (nothing before dot).".to_string(),
                        },
                        ichars,
                    ));
                }
                head = ExpEnum::Pair(exp.clone(), make_exp(ExpEnum::Nil, None));
                tail = head.clone();
            } else if dot {
                if is_unquote_splice(&exp) {
                    return Err((
                        ReadError {
                            reason: "Invalid dotted pair syntax with unquote-splice.".to_string(),
                        },
                        ichars,
                    ));
                }
                let exp = if let Some(uqexp) = get_unquote_lst(&exp) {
                    // Do this so `(x y . ,z) works
                    let mut v = Vec::new();
                    v.push(ExpEnum::Symbol("unquote", SymLoc::None).into());
                    let mut i = 0;
                    for e in uqexp.iter() {
                        v.push(e);
                        i += 1;
                    }
                    if i != 1 {
                        return Err((
                            ReadError {
                                reason: "Invalid dotted pair syntax with unquote.".to_string(),
                            },
                            ichars,
                        ));
                    }
                    Expression::with_list(v)
                } else {
                    exp
                };
                if let ExpEnum::Pair(_, cdr) = &tail {
                    let mut cdr = cdr.get_mut();
                    cdr.data = exp.get().data.clone();
                    cdr.meta = exp.get().meta;
                }
            } else {
                let new_tail = ExpEnum::Pair(exp.clone(), make_exp(ExpEnum::Nil, None));
                if let ExpEnum::Pair(_, cdr) = &tail {
                    cdr.get_mut().data = new_tail.clone();
                }
                tail = new_tail;
            }
        } else if pch.is_none() {
            cont = false;
        }
        chars = ichars;
        if dot {
            dot_count += 1;
        }
        if dot_count > 1 {
            return Err((
                ReadError {
                    reason: "Invalid dotted pair syntax (more than object follows dot)."
                        .to_string(),
                },
                chars,
            ));
        }
    }
    Err((
        ReadError {
            reason: "Unclosed list".to_string(),
        },
        chars,
    ))
}

fn read_inner(
    environment: &mut Environment,
    mut chars: CharIter, // Pass ownership in and out for reader macro support.
    buffer: &mut String,
    in_back_quote: bool,
    return_close_paren: bool,
) -> Result<(Option<Expression>, CharIter), (ReadError, CharIter)> {
    if environment.reader_state.is_none() {
        panic!("tried to read with no state!");
    }
    let read_table = lookup_expression(&environment, "*read-table*");
    let mut read_table_chars: HashSet<&'static str> = HashSet::new();
    if let Some(read_table) = &read_table {
        if let ExpEnum::HashMap(map) = &read_table.get().data {
            for key in map.keys() {
                read_table_chars.insert(key);
            }
        }
    }
    let read_table_end_char = lookup_expression(&environment, "*read-table-end-char*");
    consume_whitespace(environment, &mut chars);

    while let Some((ch, peek_ch)) = next2(&mut chars) {
        environment.reader_state.as_mut().unwrap().column += 1;
        if read_table_chars.contains(&*ch) {
            let mut end_ch = None;
            if let Some(read_table_end_char) = &read_table_end_char {
                if let ExpEnum::HashMap(map) = &read_table_end_char.get().data {
                    if map.contains_key(&*ch) {
                        if let ExpEnum::Char(ch) = &map.get(&*ch).unwrap().get().data {
                            end_ch = Some(cow_to_ref(environment, &ch));
                        }
                    }
                }
            }
            if let Some(read_table) = &read_table {
                if let ExpEnum::HashMap(map) = &read_table.get().data {
                    if map.contains_key(&*ch) {
                        if let ExpEnum::Symbol(s, _) = map.get(&*ch).unwrap().get().data {
                            let res = prep_reader_macro(environment, chars, s, &ch, end_ch);
                            match res {
                                Ok((None, ichars)) => {
                                    chars = ichars;
                                    continue;
                                }
                                _ => return res,
                            }
                        }
                    }
                }
            }
        }
        let meta = get_meta(
            environment.reader_state.as_ref().unwrap().file_name,
            environment.reader_state.as_ref().unwrap().line,
            environment.reader_state.as_ref().unwrap().column,
        );
        match &*ch {
            "\"" => {
                match read_string(
                    &mut chars,
                    buffer,
                    &mut environment.reader_state.as_mut().unwrap(),
                ) {
                    Ok(s) => return Ok((Some(s), chars)),
                    Err(e) => return Err((e, chars)),
                };
            }
            "'" => match read_inner(environment, chars, buffer, in_back_quote, false) {
                Ok((Some(exp), ichars)) => {
                    let qlist = make_exp(
                        ExpEnum::Pair(
                            Expression::alloc_data(ExpEnum::Symbol(
                                environment.interner.intern("quote"),
                                SymLoc::None,
                            )),
                            make_exp(ExpEnum::Pair(exp, Expression::make_nil()), None),
                        ),
                        meta,
                    );
                    return Ok((Some(qlist), ichars));
                }
                Ok((None, ichars)) => {
                    return Err((
                        ReadError {
                            reason: "Invalid quote".to_string(),
                        },
                        ichars,
                    ));
                }
                Err((err, ichars)) => {
                    return Err((err, ichars));
                }
            },
            "`" => match read_inner(environment, chars, buffer, true, false) {
                Ok((Some(exp), ichars)) => {
                    let qlist = make_exp(
                        ExpEnum::Pair(
                            Expression::alloc_data(ExpEnum::Symbol(
                                environment.interner.intern("back-quote"),
                                SymLoc::None,
                            )),
                            make_exp(ExpEnum::Pair(exp, Expression::make_nil()), None),
                        ),
                        meta,
                    );
                    return Ok((Some(qlist), ichars));
                }
                Ok((None, ichars)) => {
                    return Err((
                        ReadError {
                            reason: "Invalid back-quote".to_string(),
                        },
                        ichars,
                    ));
                }
                Err((err, ichars)) => {
                    return Err((err, ichars));
                }
            },
            "," if in_back_quote => {
                let sym = if peek_ch == "@" {
                    chars.next();
                    Expression::alloc_data(ExpEnum::Symbol(
                        environment.interner.intern("unquote-splice"),
                        SymLoc::None,
                    ))
                } else {
                    Expression::alloc_data(ExpEnum::Symbol(
                        environment.interner.intern("unquote"),
                        SymLoc::None,
                    ))
                };
                match read_inner(environment, chars, buffer, in_back_quote, false) {
                    Ok((Some(exp), ichars)) => {
                        return Ok((
                            Some(make_exp(
                                ExpEnum::Pair(
                                    sym,
                                    make_exp(ExpEnum::Pair(exp, Expression::make_nil()), None),
                                ),
                                meta,
                            )),
                            ichars,
                        ));
                    }
                    Ok((None, ichars)) => {
                        return Err((
                            ReadError {
                                reason: "Invalid back-quote".to_string(),
                            },
                            ichars,
                        ));
                    }
                    Err((err, ichars)) => {
                        return Err((err, ichars));
                    }
                }
            }
            "," => {
                return Err((
                    ReadError {
                        reason: "Unquote outside of a back-quote".to_string(),
                    },
                    chars,
                ))
            }
            "#" => {
                chars.next();
                match &*peek_ch {
                    "|" => consume_block_comment(
                        &mut chars,
                        &mut environment.reader_state.as_mut().unwrap(),
                    ),
                    "\\" => {
                        buffer.clear();
                        read_symbol(
                            buffer,
                            &mut chars,
                            &mut environment.reader_state.as_mut().unwrap(),
                            true,
                            false,
                        );
                        match do_char(environment, buffer, meta) {
                            Ok(ch) => return Ok((Some(ch), chars)),
                            Err(e) => return Err((e, chars)),
                        };
                    }
                    "<" => {
                        let reason = format!(
                            "Found an unreadable token: line {}, col: {}",
                            environment.reader_state.as_ref().unwrap().line,
                            environment.reader_state.as_ref().unwrap().column
                        );
                        return Err((ReadError { reason }, chars));
                    }
                    "(" => {
                        let (exp, chars) = read_vector(environment, chars, buffer, in_back_quote)?;
                        return Ok((Some(exp), chars));
                    }
                    "t" => {
                        return Ok((Some(Expression::alloc_data(ExpEnum::True)), chars));
                    }
                    "." => {
                        return prep_reader_macro(
                            environment,
                            chars,
                            "reader-macro-dot",
                            ".",
                            None,
                        );
                    }
                    // Read an octal int
                    "o" => {
                        let (exp, chars) = read_num_radix(environment, chars, buffer, 8, meta)?;
                        return Ok((Some(exp), chars));
                    }
                    // Read a hex int
                    "x" => {
                        let (exp, chars) = read_num_radix(environment, chars, buffer, 16, meta)?;
                        return Ok((Some(exp), chars));
                    }
                    // Read a binary int
                    "b" => {
                        let (exp, chars) = read_num_radix(environment, chars, buffer, 2, meta)?;
                        return Ok((Some(exp), chars));
                    }
                    _ => {
                        let reason = format!(
                            "Found # with invalid char {}: line {}, col: {}",
                            peek_ch,
                            environment.reader_state.as_ref().unwrap().line,
                            environment.reader_state.as_ref().unwrap().column
                        );
                        return Err((ReadError { reason }, chars));
                    }
                }
            }
            "(" => {
                let (exp, chars) = read_list(environment, chars, buffer, in_back_quote)?;
                return Ok((Some(exp), chars));
            }
            ")" => {
                if return_close_paren {
                    return Ok((
                        Some(Expression::alloc_data(ExpEnum::Symbol(")", SymLoc::None))),
                        chars,
                    ));
                } else {
                    return Err((
                        ReadError {
                            reason: "Unexpected `)`".to_string(),
                        },
                        chars,
                    ));
                }
            }
            ";" => {
                consume_line_comment(&mut chars, &mut environment.reader_state.as_mut().unwrap());
            }
            _ => {
                buffer.clear();
                buffer.push_str(&ch);
                let is_number = read_symbol(
                    buffer,
                    &mut chars,
                    &mut environment.reader_state.as_mut().unwrap(),
                    false,
                    false,
                );
                return Ok((Some(do_atom(environment, buffer, is_number, meta)), chars));
            }
        }
        consume_whitespace(environment, &mut chars);
    }
    Ok((None, chars))
}

fn read2(
    environment: &mut Environment,
    text: &str,
    always_wrap: bool,
    file_name: Option<&'static str>,
    list_only: bool,
) -> Result<Expression, ReadError> {
    let clear_state = if environment.reader_state.is_none() {
        environment.reader_state = Some(ReaderState {
            file_name,
            column: 0,
            line: 1,
            end_ch: None,
        });
        true
    } else {
        false
    };
    let mut buffer = String::new();
    let mut exps = Vec::new();

    // Do this so the chars iterator has a static lifetime.  Should be ok since both the string
    // reference and iterator go away at the end of this function.
    let ntext = unsafe { &*(text as *const str) };
    let mut chars: CharIter = Box::new(
        UnicodeSegmentation::graphemes(ntext, true)
            .map(|s| Cow::Borrowed(s))
            .peekable(),
    );
    if text.starts_with("#!") {
        // Work with shebanged scripts.
        consume_line_comment(&mut chars, &mut environment.reader_state.as_mut().unwrap());
    }
    let mut cont = true;
    while cont {
        let (exp, ichars) = match read_inner(environment, chars, &mut buffer, false, false) {
            Ok(r) => r,
            Err((err, _)) => {
                if clear_state {
                    environment.reader_state = None;
                }
                return Err(err);
            }
        };
        if let Some(exp) = exp {
            exps.push(exp);
        } else {
            cont = false;
        }
        chars = ichars;
    }
    if chars.next().is_some() {
        if clear_state {
            environment.reader_state = None;
        }
        let reason = format!(
            "Premature end (to many ')'?) line: {}, column: {}",
            environment.reader_state.as_ref().unwrap().line,
            environment.reader_state.as_ref().unwrap().column
        );
        return Err(ReadError { reason });
    }
    let exp_meta = get_meta(environment.reader_state.as_ref().unwrap().file_name, 0, 0);
    if clear_state {
        environment.reader_state = None;
    }

    if always_wrap {
        Ok(Expression::with_list_meta(exps, exp_meta))
    } else if list_only {
        if exps.len() == 1 {
            let exp_d = exps[0].get();
            match &exp_d.data {
                ExpEnum::Pair(_, _) => Ok(exps[0].clone()),
                ExpEnum::Vector(_) => Ok(exps[0].clone()),
                ExpEnum::Nil => Ok(exps[0].clone()),
                _ => {
                    drop(exp_d);
                    Ok(Expression::with_list_meta(exps, exp_meta))
                }
            }
        } else if exps.is_empty() {
            Err(ReadError {
                reason: "Empty value".to_string(),
            })
        } else {
            Ok(Expression::with_list_meta(exps, exp_meta))
        }
    } else if exps.len() == 1 {
        Ok(exps[0].clone())
    } else {
        Ok(Expression::with_list_meta(exps, exp_meta))
    }
}

pub fn read_form(
    environment: &mut Environment,
    chars: CharIter,
) -> Result<(Expression, CharIter), (ReadError, CharIter)> {
    let clear_state = if environment.reader_state.is_none() {
        environment.reader_state = Some(ReaderState {
            file_name: None,
            column: 0,
            line: 1,
            end_ch: None,
        });
        true
    } else {
        false
    };
    let mut buffer = String::new();
    match read_inner(environment, chars, &mut buffer, false, false) {
        Ok((Some(exp), ichars)) => {
            if clear_state {
                environment.reader_state = None;
            }
            Ok((exp, ichars))
        }
        Ok((None, ichars)) => {
            if clear_state {
                environment.reader_state = None;
            }
            Err((
                ReadError {
                    reason: "Empty value".to_string(),
                },
                ichars,
            ))
        }
        Err((err, ichars)) => {
            if clear_state {
                environment.reader_state = None;
            }
            Err((err, ichars))
        }
    }
}

pub fn read(
    environment: &mut Environment,
    text: &str,
    name: Option<&'static str>,
    list_only: bool,
) -> Result<Expression, ReadError> {
    read2(environment, text, false, name, list_only)
}

// Read the text but always wrap in an outer list even if text is one list.
// Useful for loading scripts.
pub fn read_list_wrap(
    environment: &mut Environment,
    text: &str,
    name: Option<&'static str>,
) -> Result<Expression, ReadError> {
    read2(environment, text, true, name, false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicBool;
    use std::sync::Arc;

    use crate::builtins_util::is_proper_list;

    fn to_strs(output: &mut Vec<String>, exp: &Expression) {
        match &exp.get().data {
            ExpEnum::Vector(list) => {
                output.push("#(".to_string());
                for exp in list.iter() {
                    to_strs(output, &exp);
                }
                output.push(")".to_string());
            }
            ExpEnum::Pair(e1, e2) => {
                if is_proper_list(&exp) {
                    output.push("(".to_string());
                    for p in exp.iter() {
                        to_strs(output, &p);
                    }
                    output.push(")".to_string());
                } else {
                    output.push("(".to_string());
                    to_strs(output, &e1);
                    output.push(".".to_string());
                    to_strs(output, &e2);
                    output.push(")".to_string());
                }
            }
            ExpEnum::Nil => output.push("nil".to_string()),
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
        let exp = read(environment, input, name, false);
        let mut tokens = Vec::new();
        if let Ok(exp) = exp {
            to_strs(&mut tokens, &exp);
        } else {
            println!("{:?}", exp);
            assert!(false);
        }
        tokens
    }

    fn tokenize_err(
        environment: &mut Environment,
        input: &str,
        name: Option<&'static str>,
    ) -> ReadError {
        let exp = read(environment, input, name, false);
        if let Err(err) = exp {
            return err;
        } else {
            assert!(false);
        }
        ReadError {
            reason: "WTF".to_string(),
        }
    }

    fn tokenize_wrap(
        environment: &mut Environment,
        input: &str,
        name: Option<&'static str>,
    ) -> Vec<String> {
        let exp = read_list_wrap(environment, input, name);
        let mut tokens = Vec::new();
        if let Ok(exp) = exp {
            to_strs(&mut tokens, &exp);
        } else {
            assert!(false);
        }
        tokens
    }

    fn build_def_env() -> Environment {
        let mut environment = build_default_environment(Arc::new(AtomicBool::new(false)));
        environment.reader_state = Some(ReaderState {
            line: 0,
            column: 0,
            file_name: None,
            end_ch: None,
        });
        environment
    }

    #[test]
    fn test_tokenize() {
        let mut environment = build_def_env();
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
    fn test_quotes() {
        let mut environment = build_def_env();
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
        let tokens = tokenize(&mut environment, "'(1 2 ,3)", None);
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "Int:1");
        assert!(tokens[4] == "Int:2");
        assert!(tokens[5] == "Symbol:,3");
        assert!(tokens[6] == ")");
        assert!(tokens[7] == ")");
        let tokens = tokenize(&mut environment, "'(1 2 ,@3)", None);
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "Int:1");
        assert!(tokens[4] == "Int:2");
        assert!(tokens[5] == "Symbol:,@3");
        assert!(tokens[6] == ")");
        assert!(tokens[7] == ")");
        let tokens = tokenize(&mut environment, "`(1 2 ,3)", None);
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:back-quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "Int:1");
        assert!(tokens[4] == "Int:2");
        assert!(tokens[5] == "(");
        assert!(tokens[6] == "Symbol:unquote");
        assert!(tokens[7] == "Int:3");
        assert!(tokens[8] == ")");
        assert!(tokens[9] == ")");
        assert!(tokens[10] == ")");
        let tokens = tokenize(&mut environment, "`(1 2 ,@3)", None);
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:back-quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "Int:1");
        assert!(tokens[4] == "Int:2");
        assert!(tokens[5] == "(");
        assert!(tokens[6] == "Symbol:unquote-splice");
        assert!(tokens[7] == "Int:3");
        assert!(tokens[8] == ")");
        assert!(tokens[9] == ")");
        assert!(tokens[10] == ")");
        let tokens = tokenize(&mut environment, "`(1 `2 ,@3)", None);
        assert!(tokens.len() == 14);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:back-quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "Int:1");
        assert!(tokens[4] == "(");
        assert!(tokens[5] == "Symbol:back-quote");
        assert!(tokens[6] == "Int:2");
        assert!(tokens[7] == ")");
        assert!(tokens[8] == "(");
        assert!(tokens[9] == "Symbol:unquote-splice");
        assert!(tokens[10] == "Int:3");
        assert!(tokens[11] == ")");
        assert!(tokens[12] == ")");
        assert!(tokens[13] == ")");
        let tokens = tokenize(&mut environment, "`(1 `(2 ,x) ,@3)", None);
        assert!(tokens.len() == 20);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:back-quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "Int:1");
        assert!(tokens[4] == "(");
        assert!(tokens[5] == "Symbol:back-quote");
        assert!(tokens[6] == "(");
        assert!(tokens[7] == "Int:2");
        assert!(tokens[8] == "(");
        assert!(tokens[9] == "Symbol:unquote");
        assert!(tokens[10] == "Symbol:x");
        assert!(tokens[11] == ")");
        assert!(tokens[12] == ")");
        assert!(tokens[13] == ")");
        assert!(tokens[14] == "(");
        assert!(tokens[15] == "Symbol:unquote-splice");
        assert!(tokens[16] == "Int:3");
        assert!(tokens[17] == ")");
        assert!(tokens[18] == ")");
        assert!(tokens[19] == ")");
    }

    #[test]
    fn test_types() {
        let mut environment = build_def_env();
        let tokens = tokenize(
            &mut environment,
            "(one 2 3.0 \"four\" #\\B #t nil 3.5 ())",
            None,
        );
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:one");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Float:3");
        assert!(tokens[4] == "String:\"four\"");
        assert!(tokens[5] == "Char:#\\B");
        assert!(tokens[6] == "True:true");
        assert!(tokens[7] == "nil");
        assert!(tokens[8] == "Float:3.5");
        assert!(tokens[9] == "nil");
        assert!(tokens[10] == ")");

        let tokens = tokenize(
            &mut environment,
            "#(one 2 3.0 \"four\" #\\B #t nil 3.5 ())",
            None,
        );
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Symbol:one");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Float:3");
        assert!(tokens[4] == "String:\"four\"");
        assert!(tokens[5] == "Char:#\\B");
        assert!(tokens[6] == "True:true");
        assert!(tokens[7] == "nil");
        assert!(tokens[8] == "Float:3.5");
        assert!(tokens[9] == "nil");
        assert!(tokens[10] == ")");

        let tokens = tokenize(
            &mut environment,
            "one 2 3.0 \"four\" #\\B #t nil 3.5 ()",
            None,
        );
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Symbol:one");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Float:3");
        assert!(tokens[4] == "String:\"four\"");
        assert!(tokens[5] == "Char:#\\B");
        assert!(tokens[6] == "True:true");
        assert!(tokens[7] == "nil");
        assert!(tokens[8] == "Float:3.5");
        assert!(tokens[9] == "nil");
        assert!(tokens[10] == ")");
    }

    #[test]
    fn test_wrap() {
        let mut environment = build_def_env();
        let tokens = tokenize(&mut environment, "(1 2 3)", None);
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize_wrap(&mut environment, "(1 2 3)", None);
        assert!(tokens.len() == 7);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "(");
        assert!(tokens[2] == "Int:1");
        assert!(tokens[3] == "Int:2");
        assert!(tokens[4] == "Int:3");
        assert!(tokens[5] == ")");
        assert!(tokens[6] == ")");

        let tokens = tokenize(&mut environment, "1 2 3", None);
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize_wrap(&mut environment, "1 2 3", None);
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");

        let tokens = tokenize(&mut environment, "(1 2 3) (4 5 6)", None);
        assert!(tokens.len() == 12);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "(");
        assert!(tokens[2] == "Int:1");
        assert!(tokens[3] == "Int:2");
        assert!(tokens[4] == "Int:3");
        assert!(tokens[5] == ")");
        assert!(tokens[6] == "(");
        assert!(tokens[7] == "Int:4");
        assert!(tokens[8] == "Int:5");
        assert!(tokens[9] == "Int:6");
        assert!(tokens[10] == ")");
        assert!(tokens[11] == ")");
        let tokens = tokenize_wrap(&mut environment, "(1 2 3) (4 5 6)", None);
        assert!(tokens.len() == 12);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "(");
        assert!(tokens[2] == "Int:1");
        assert!(tokens[3] == "Int:2");
        assert!(tokens[4] == "Int:3");
        assert!(tokens[5] == ")");
        assert!(tokens[6] == "(");
        assert!(tokens[7] == "Int:4");
        assert!(tokens[8] == "Int:5");
        assert!(tokens[9] == "Int:6");
        assert!(tokens[10] == ")");
        assert!(tokens[11] == ")");

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
        let tokens = tokenize_wrap(&mut environment, "'(1 2 3)", None);
        assert!(tokens.len() == 10);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "(");
        assert!(tokens[2] == "Symbol:quote");
        assert!(tokens[3] == "(");
        assert!(tokens[4] == "Int:1");
        assert!(tokens[5] == "Int:2");
        assert!(tokens[6] == "Int:3");
        assert!(tokens[7] == ")");
        assert!(tokens[8] == ")");
        assert!(tokens[9] == ")");

        let tokens = tokenize(&mut environment, "nil", None);
        assert!(tokens.len() == 1);
        assert!(tokens[0] == "nil");
        let tokens = tokenize(&mut environment, "()", None);
        assert!(tokens.len() == 1);
        assert!(tokens[0] == "nil");
        let tokens = tokenize_wrap(&mut environment, "nil", None);
        assert!(tokens.len() == 3);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "nil");
        assert!(tokens[2] == ")");
        let tokens = tokenize_wrap(&mut environment, "()", None);
        assert!(tokens.len() == 3);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "nil");
        assert!(tokens[2] == ")");
    }

    #[test]
    fn test_tok_strings() {
        let mut environment = build_def_env();
        let input =
            "\"on\\te\\ntwo\" two \"th\\rree\" \"fo\\\"u\\\\r\" 5 6 \"slash\\x2fx\\x2F\\x3a\\x3b\"";
        let tokens = tokenize(&mut environment, input, None);
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

        let input =
            "\"\\u{03bb} two \" \"\\x20 \\u{03BB} end\" \"fo\\\"u\\\\r\" 5 6 \"slash\\x2fx\\x2F\\x3a\\x3b\"";
        let tokens = tokenize(&mut environment, input, None);
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "String:\"\u{03bb} two \"");
        assert!(tokens[2] == "String:\"  λ end\"");
        assert!(tokens[3] == "String:\"fo\"u\\r\"");
        assert!(tokens[4] == "Int:5");
        assert!(tokens[5] == "Int:6");
        assert!(tokens[6] == "String:\"slash/x/:;\"");
        assert!(tokens[7] == ")");

        let input =
            "\"\\u03bb two \" \"\\x20 \\u03BB \nend\" \"fo\\\"u\\\\r\" 5 6 \"slash\\x2fx\\x2F\\x3a\\x3b\"";
        let tokens = tokenize(&mut environment, input, None);
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "String:\"\u{03bb} two \"");
        assert!(tokens[2] == "String:\"  λ \nend\"");
        assert!(tokens[3] == "String:\"fo\"u\\r\"");
        assert!(tokens[4] == "Int:5");
        assert!(tokens[5] == "Int:6");
        assert!(tokens[6] == "String:\"slash/x/:;\"");
        assert!(tokens[7] == ")");
    }

    #[test]
    fn test_tok_chars() {
        let mut environment = build_def_env();
        let input = "#\\x #\\X #\\x20 #\\u03bb #\\u{03BB} #\\u03bb";
        let tokens = tokenize(&mut environment, input, None);
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Char:#\\x");
        assert!(tokens[2] == "Char:#\\X");
        assert!(tokens[3] == "Char:#\\ ");
        assert!(tokens[4] == "Char:#\\λ");
        assert!(tokens[5] == "Char:#\\\u{03bb}");
        assert!(tokens[6] == "Char:#\\λ");
        assert!(tokens[7] == ")");
    }

    #[test]
    fn test_tok_ints() {
        let mut environment = build_def_env();
        let input = "2300 23_000 #xFF #xff #x0f #xF #b0000_0000 #b1111_1111 #b11111111 #b11111111_11111111 #o07 #o17";
        let tokens = tokenize(&mut environment, input, None);
        assert!(tokens.len() == 14);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Int:2300");
        assert!(tokens[2] == "Int:23000");
        assert!(tokens[3] == "Int:255");
        assert!(tokens[4] == "Int:255");
        assert!(tokens[5] == "Int:15");
        assert!(tokens[6] == "Int:15");
        assert!(tokens[7] == "Int:0");
        assert!(tokens[8] == "Int:255");
        assert!(tokens[9] == "Int:255");
        assert!(tokens[10] == "Int:65535");
        assert!(tokens[11] == "Int:7");
        assert!(tokens[12] == "Int:15");
        assert!(tokens[13] == ")");
        let input = "#xFG";
        tokenize_err(&mut environment, input, None);
        let input = "#b1112";
        tokenize_err(&mut environment, input, None);
        let input = "#o80";
        tokenize_err(&mut environment, input, None);
    }

    #[test]
    fn test_tok_floats() {
        let mut environment = build_def_env();
        let input = "2300.0 23_000.0 23e10 23e+5 23e-4 23e-+5 23e-5e+4 23.123 0.23.123";
        let tokens = tokenize(&mut environment, input, None);
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Float:2300");
        assert!(tokens[2] == "Float:23000");
        assert!(tokens[3] == "Float:230000000000");
        assert!(tokens[4] == "Float:2300000");
        assert!(tokens[5] == "Float:0.0023");
        assert!(tokens[6] == "Symbol:23e-+5");
        assert!(tokens[7] == "Symbol:23e-5e+4");
        assert!(tokens[8] == "Float:23.123");
        assert!(tokens[9] == "Symbol:0.23.123");
        assert!(tokens[10] == ")");
    }
}
