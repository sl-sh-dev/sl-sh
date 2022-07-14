use std::borrow::Cow;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::{BufReader, Cursor, Read};
use std::num::{ParseFloatError, ParseIntError};

use slvm::value::*;
use slvm::vm::*;
use slvm::Chunk;
use unicode_reader::Graphemes;
use unicode_segmentation::UnicodeSegmentation;

pub trait PeekableIterator: std::iter::Iterator {
    fn peek(&mut self) -> Option<&Self::Item>;
}

impl<I: std::iter::Iterator> PeekableIterator for std::iter::Peekable<I> {
    fn peek(&mut self) -> Option<&Self::Item> {
        std::iter::Peekable::peek(self)
    }
}

pub type CharIter = Box<dyn PeekableIterator<Item = Cow<'static, str>>>;

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

#[derive(Clone, Debug)]
pub struct ReaderState {
    pub file_name: &'static str,
    pub line: usize,
    pub column: usize,
}

impl ReaderState {
    pub fn new() -> Self {
        ReaderState::default()
    }

    pub fn clear(&mut self) {
        self.column = 0;
        self.line = 1;
    }
}

impl Default for ReaderState {
    fn default() -> Self {
        ReaderState {
            file_name: "",
            column: 0,
            line: 1,
        }
    }
}

fn alloc_pair(vm: &mut Vm, car: Value, cdr: Value, reader_state: &ReaderState) -> Value {
    let result = vm.alloc_pair_ro(car, cdr);
    // Just allocated this so the unwrap is safe.
    let handle = result.get_handle().unwrap();
    let file_name = vm.intern_static(reader_state.file_name);
    vm.set_heap_property(handle, "dbg-file", Value::StringConst(file_name));
    vm.set_heap_property(handle, "dbg-line", Value::UInt(reader_state.line as u64));
    vm.set_heap_property(handle, "dbg-col", Value::UInt(reader_state.column as u64));
    result
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

fn end_symbol(ch: &str, read_table_term: &HashMap<&'static str, Value>) -> bool {
    if is_whitespace(ch) || read_table_term.contains_key(ch) {
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

fn do_char(vm: &mut Vm, reader_state: &mut ReaderState, symbol: &str) -> Result<Value, ReadError> {
    match &symbol.to_lowercase()[..] {
        "space" => return Ok(Value::CodePoint(' ')),
        "tab" => return Ok(Value::CodePoint('\t')),
        // newline should be the platform line end.
        "newline" => return Ok(Value::CodePoint('\n')),
        "linefeed" => return Ok(Value::CodePoint('\n')),
        "return" => return Ok(Value::CodePoint('\r')),
        "backspace" => return Ok(Value::CodePoint('\u{0008}')),
        _ => {}
    }
    // Do this so the chars iterator has a static lifetime.  Should be ok since
    // iterator dies at the end of the function and symbol does not.
    // Note: interning the chars below keeps from using the temp buffer.
    let ntext = unsafe { &*(symbol as *const str) };
    let mut chars: CharIter = Box::new(
        UnicodeSegmentation::graphemes(ntext, true)
            .map(Cow::Borrowed)
            .peekable(),
    );
    if let Some(ch) = chars.next() {
        if chars.peek().is_some() {
            match &*ch {
                "u" => {
                    let ch = read_utf_scalar(&mut chars, reader_state)?;
                    // XXX TODO- codepoint here?
                    return Ok(Value::CodePoint(ch));
                }
                "x" => {
                    let ch = escape_to_char(&mut chars, reader_state)?;
                    return Ok(Value::CodePoint(ch));
                }
                _ => {
                    let reader_state = reader_state;
                    let reason = format!(
                        "Not a valid char [{}]: line {}, col: {}",
                        symbol, reader_state.line, reader_state.column
                    );
                    return Err(ReadError { reason });
                }
            }
        }
        if ch.len() == 1 {
            Ok(Value::CodePoint(ch.chars().next().unwrap()))
        } else if ch.len() < 15 {
            let mut v: [u8; 14] = [0; 14];
            for (i, c) in ch.bytes().enumerate() {
                v[i] = c;
            }
            Ok(Value::CharCluster(ch.len() as u8, v))
        } else if let Value::String(handle) = vm.alloc_string_ro(ch.to_string()) {
            Ok(Value::CharClusterLong(handle))
        } else {
            panic!("Invalid alloc_string!");
        }
    } else {
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
    while let Some(ch) = chars.next() {
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
    }
    if has_bracket {
        Err(ReadError {
            reason: "Invalid unicode scalar, failed to parse.".to_string(),
        })
    } else {
        finish(char_u32)
    }
}

fn read_string<'sym>(
    _vm: &mut Vm,
    reader_state: &mut ReaderState,
    mut chars: CharIter,
    symbol: &'sym mut String,
    read_table: &HashMap<&'static str, Chunk>,
) -> Result<(&'sym mut String, CharIter), (ReadError, CharIter)> {
    symbol.clear();
    let mut last_ch_escape = false;

    while let Some(ch) = chars.next() {
        if ch == "\n" {
            reader_state.line += 1;
            reader_state.column = 0;
        } else {
            reader_state.column += 1;
        }
        if last_ch_escape {
            let mut do_match = true;
            if read_table.contains_key(&*ch) {
                do_match = false;
                symbol.push_str(&ch);
            }
            if do_match {
                match &*ch {
                    "n" => symbol.push('\n'),
                    "r" => symbol.push('\r'),
                    "t" => symbol.push('\t'),
                    "\"" => symbol.push('"'),
                    "x" => {
                        let res = escape_to_char(&mut chars, reader_state);
                        // ? seems to confuse the borrow checker here.
                        let res = if let Err(e) = res {
                            return Err((e, chars));
                        } else {
                            res.unwrap()
                        };
                        symbol.push(res);
                    }
                    "\\" => {
                        symbol.push('\\');
                    }
                    "u" => {
                        let res = read_utf_scalar(&mut chars, reader_state);
                        // ? seems to confuse the borrow checker here.
                        let res = if let Err(e) = res {
                            return Err((e, chars));
                        } else {
                            res.unwrap()
                        };
                        symbol.push(res);
                    }
                    _ => {
                        symbol.push('\\');
                        symbol.push_str(&ch);
                    }
                }
            }
            last_ch_escape = false;
        } else {
            if ch == "\"" {
                break;
            }
            let mut proc_ch = true;
            if read_table.contains_key(&*ch) {
                proc_ch = false;
                /*if let ExpEnum::Symbol(s) = read_table.get(&*ch).unwrap().get().data {
                    let res = prep_reader_macro(environment, chars, s, &ch);
                    match res {
                        Ok((None, ichars)) => {
                            chars = ichars;
                            continue;
                        }
                        Ok((Some(exp), ichars)) => {
                            chars = ichars;
                            let exp_d = exp.get();
                            match &exp_d.data {
                                ExpEnum::String(s, _) => symbol.push_str(s),
                                ExpEnum::Char(s) => symbol.push_str(s),
                                ExpEnum::CodePoint(c) => symbol.push(*c),
                                _ => {
                                    if let Some(l) = res_list.as_mut() {
                                        drop(exp_d);
                                        if !symbol.is_empty() {
                                            l.push(make_exp(
                                                ExpEnum::String(symbol.clone().into(), None),
                                                meta,
                                            ));
                                        }
                                        symbol.clear();
                                        l.push(wrap_trim(exp, meta));
                                    } else {
                                        drop(exp_d);
                                        let mut list = vec![make_exp(
                                            ExpEnum::Symbol("str"),
                                            meta,
                                        )];
                                        if !symbol.is_empty() {
                                            list.push(make_exp(
                                                ExpEnum::String(symbol.clone().into(), None),
                                                meta,
                                            ));
                                        }
                                        symbol.clear();
                                        list.push(wrap_trim(exp, meta));
                                        res_list = Some(list);
                                    }
                                }
                            }
                        }
                        Err((err, ichars)) => return Err((err, ichars)),
                    }
                }*/
            }
            if proc_ch {
                if ch != "\\" {
                    last_ch_escape = false;
                    symbol.push_str(&ch);
                } else {
                    last_ch_escape = true;
                }
            }
        }
    }
    /*if let Some(mut list) = res_list.take() {
        if !symbol.is_empty() {
            list.push(make_exp(ExpEnum::String(symbol.clone().into(), None), meta));
        }
        let fl = Expression::with_list_meta(list, meta);
        Ok((fl, chars))
    } else {*/
    Ok((
        symbol, //Value::Reference(vm.alloc(Object::String(symbol.clone().into()))),
        chars,
    ))
    //}
}

fn read_string_literal<'sym>(
    _vm: &mut Vm,
    reader_state: &mut ReaderState,
    mut chars: CharIter,
    symbol: &'sym mut String,
) -> Result<(&'sym mut String, CharIter), (ReadError, CharIter)> {
    symbol.clear();
    let end_ch = if let Some(ch) = chars.next() {
        reader_state.column += 1;
        ch
    } else {
        return Err((
            ReadError {
                reason: "Unexpected stream end on string literal".to_string(),
            },
            chars,
        ));
    };

    while let Some(ch) = chars.next() {
        if ch == "\n" {
            reader_state.line += 1;
            reader_state.column = 0;
        } else {
            reader_state.column += 1;
        }
        let peek = if let Some(pch) = chars.peek() {
            pch
        } else {
            ""
        };
        if ch == end_ch && peek == "\"" {
            chars.next();
            return Ok((
                symbol,
                //Value::Reference(vm.alloc(Object::String(symbol.clone().into()))),
                chars,
            ));
        }
        symbol.push_str(&ch);
    }
    Err((
        ReadError {
            reason: "Unexpected end of string literal".to_string(),
        },
        chars,
    ))
}

fn do_atom(vm: &mut Vm, symbol: &str, is_number: bool) -> Value {
    if is_number {
        let mut num_str = symbol.to_string();
        num_str.retain(|ch| ch != '_');
        let potential_int: Result<i64, ParseIntError> = num_str.parse();
        match potential_int {
            Ok(v) => Value::Int(v),
            Err(_) => {
                let potential_float: Result<f64, ParseFloatError> = num_str.parse();
                match potential_float {
                    Ok(v) => Value::float(v),
                    Err(_) => Value::Symbol(vm.intern(symbol)),
                }
            }
        }
    } else {
        if symbol.is_empty() {
            return Value::Nil;
        }
        if symbol == "nil" {
            Value::Nil
        } else if symbol.len() > 1 && symbol.starts_with(':') {
            Value::Keyword(vm.intern(&symbol[1..]))
        } else {
            Value::Symbol(vm.intern(symbol))
        }
    }
}

fn read_symbol(
    buffer: &mut String,
    chars: &mut CharIter,
    reader_state: &mut ReaderState,
    for_ch: bool,
    skip_underscore: bool,
    read_table_term: &HashMap<&'static str, Value>,
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
            is_digit(ch) || ch == "." || ch == "_" || (*last_e && (ch == "+" || ch == "-"))
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
        if end_symbol(ch, read_table_term) && !for_ch {
            return buffer.len() == 1 && is_digit(&buffer[..]);
        }
    };
    let mut next_ch = chars.next();
    while next_ch.is_some() {
        let ch = next_ch.unwrap();
        let peek_ch = if let Some(pch) = chars.peek() {
            has_peek = true;
            pch
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
        } else if end_symbol(peek_ch, read_table_term) {
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

fn consume_whitespace(reader_state: &mut ReaderState, chars: &mut CharIter) {
    // Consume whitespace.
    let mut ch = chars.peek();
    while ch.is_some() && is_whitespace(ch.unwrap()) {
        if let Some(ch) = ch {
            if *ch == "\n" {
                reader_state.line += 1;
                reader_state.column = 0;
            } else {
                reader_state.column += 1;
            }
            chars.next();
        }
        ch = chars.peek();
    }
}

fn read_num_radix(
    reader_state: &mut ReaderState,
    mut chars: CharIter, // Pass ownership in and out for reader macro support.
    buffer: &mut String,
    radix: u32,
    read_table_term: &HashMap<&'static str, Value>,
) -> Result<(i64, CharIter), (ReadError, CharIter)> {
    buffer.clear();
    read_symbol(
        buffer,
        &mut chars,
        reader_state,
        true,
        true,
        read_table_term,
    );
    match i64::from_str_radix(buffer, radix) {
        Ok(n) => Ok((n, chars)),
        Err(e) => Err((
            ReadError {
                reason: e.to_string(),
            },
            chars,
        )),
    }
}

fn read_vector(
    vm: &mut Vm,
    reader_state: &mut ReaderState,
    mut chars: CharIter, // Pass ownership in and out for reader macro support.
    buffer: &mut String,
    in_back_quote: bool,
) -> Result<(Vec<Value>, CharIter), (ReadError, CharIter)> {
    let mut v: Vec<Value> = Vec::new();
    let mut cont = true;

    let close_intern = vm.intern(")");
    while cont {
        let (exp, mut ichars) =
            match read_inner(vm, reader_state, chars, buffer, in_back_quote, true) {
                Ok((exp, ichars)) => {
                    if let Some(Value::Symbol(i)) = &exp {
                        if *i == close_intern {
                            return Ok((v, ichars));
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

fn get_unquote_lst(vm: &mut Vm, exp: Value) -> Option<Value> {
    if let Value::Pair(h) = exp {
        let uq = vm.intern("unquote");
        let (car, cdr) = vm.get_pair(h);
        if let Value::Symbol(si) = car {
            if si == uq {
                return Some(cdr);
            }
        }
    }
    None
}

fn is_unquote_splice(vm: &mut Vm, exp: Value) -> bool {
    fn is_splice(vm: &mut Vm, car: Value) -> bool {
        if let Value::Symbol(si) = car {
            if si == vm.intern("unquote-splice") {
                return true;
            }
        }
        if let Value::Symbol(si) = car {
            if si == vm.intern("unquote-splice!") {
                return true;
            }
        }
        false
    }
    if let Value::Pair(h) = exp {
        let (car, _) = vm.get_pair(h);
        is_splice(vm, car)
    } else if let Value::Vector(h) = exp {
        let v = vm.get_vector(h);
        let car = if let Some(car) = v.get(0) {
            *car
        } else {
            return false;
        };
        is_splice(vm, car)
    } else {
        false
    }
}

fn read_list(
    vm: &mut Vm,
    reader_state: &mut ReaderState,
    mut chars: CharIter, // Pass ownership in and out for reader macro support.
    buffer: &mut String,
    in_back_quote: bool,
) -> Result<(Value, CharIter), (ReadError, CharIter)> {
    let mut head = Value::Nil;
    let mut tail = Value::Nil;
    let mut cont = true;
    let mut dot = false;
    let mut dot_count = 0;
    let i_close = vm.intern(")");
    let i_dot = vm.intern(".");

    while cont {
        let (exp, mut ichars) =
            match read_inner(vm, reader_state, chars, buffer, in_back_quote, true) {
                Ok((exp, ichars)) => {
                    if let Some(Value::Symbol(si)) = exp {
                        if si == i_close {
                            return Ok((head, ichars));
                        } else if si == i_dot {
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
            if let Value::Nil = head {
                if dot {
                    return Err((
                        ReadError {
                            reason: "Invalid dotted pair syntax (nothing before dot).".to_string(),
                        },
                        ichars,
                    ));
                }
                head = alloc_pair(vm, exp, Value::Nil, reader_state);
                tail = head;
            } else if dot {
                if is_unquote_splice(vm, exp) {
                    return Err((
                        ReadError {
                            reason: "Invalid dotted pair syntax with unquote-splice (,@/,.)."
                                .to_string(),
                        },
                        ichars,
                    ));
                }
                let exp = if let Some(uqexp) = get_unquote_lst(vm, exp) {
                    // Do this so `(x y . ,z) works
                    let mut v = vec![Value::Symbol(vm.intern("unquote"))];
                    let mut i = 0;
                    for e in uqexp.iter(vm) {
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
                    vm.alloc_vector_ro(v)
                } else {
                    exp
                };
                if let Value::Pair(h) = tail {
                    let (_, cdr) = vm.get_pair_mut_override(h);
                    *cdr = exp;
                }
            } else {
                let new_tail = alloc_pair(vm, exp, Value::Nil, reader_state);
                if let Value::Pair(h) = tail {
                    let (_, cdr) = vm.get_pair_mut_override(h);
                    *cdr = new_tail;
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
    vm: &mut Vm,
    reader_state: &mut ReaderState,
    mut chars: CharIter, // Pass ownership in and out for reader macro support.
    buffer: &mut String,
    in_back_quote: bool,
    return_close_paren: bool,
) -> Result<(Option<Value>, CharIter), (ReadError, CharIter)> {
    /*let read_table_out;
    let read_table_d;
    let empty_read_table;
    let read_table = get_read_table!(
        environment,
        "*read-table*",
        read_table_out,
        read_table_d,
        empty_read_table
    );
    let str_read_table_out;
    let str_read_table_d;
    let str_empty_read_table;
    let str_read_table = get_read_table!(
        environment,
        "*string-read-table*",
        str_read_table_out,
        str_read_table_d,
        str_empty_read_table
    );
    let term_read_table_out;
    let term_read_table_d;
    let term_empty_read_table;
    let read_table_term = get_read_table!(
        environment,
        "*read-table-terminal*",
        term_read_table_out,
        term_read_table_d,
        term_empty_read_table
    );*/
    consume_whitespace(reader_state, &mut chars);
    let read_table_term: HashMap<&'static str, Value> = HashMap::new();
    let read_table: HashMap<&'static str, Chunk> = HashMap::new();

    let i_quote = vm.intern("quote");
    let i_backquote = vm.intern("back-quote");
    while let Some((ch, peek_ch)) = next2(&mut chars) {
        reader_state.column += 1;
        /*if read_table.contains_key(&*ch) {
            if let ExpEnum::Symbol(s) = read_table.get(&*ch).unwrap().get().data {
                let res = prep_reader_macro(environment, chars, s, &ch);
                match res {
                    Ok((None, ichars)) => {
                        chars = ichars;
                        continue;
                    }
                    _ => return res,
                }
            }
        } else if read_table_term.contains_key(&*ch) {
            if let ExpEnum::Symbol(s) = read_table_term.get(&*ch).unwrap().get().data {
                let res = prep_reader_macro(environment, chars, s, &ch);
                match res {
                    Ok((None, ichars)) => {
                        chars = ichars;
                        continue;
                    }
                    _ => return res,
                }
            }
        }*/
        match &*ch {
            "\"" => {
                match read_string(vm, reader_state, chars, buffer, /*str_*/ &read_table) {
                    Ok((s, ichars)) => return Ok((Some(Value::StringConst(vm.intern(s))), ichars)),
                    Err((e, ichars)) => return Err((e, ichars)),
                };
            }
            "'" => match read_inner(vm, reader_state, chars, buffer, in_back_quote, false) {
                Ok((Some(exp), ichars)) => {
                    let cdr = alloc_pair(vm, exp, Value::Nil, reader_state);
                    let qlist = alloc_pair(vm, Value::Symbol(i_quote), cdr, reader_state);
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
            "`" => match read_inner(vm, reader_state, chars, buffer, true, false) {
                Ok((Some(exp), ichars)) => {
                    let cdr = alloc_pair(vm, exp, Value::Nil, reader_state);
                    let qlist = alloc_pair(vm, Value::Symbol(i_backquote), cdr, reader_state);
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
                    Value::Symbol(vm.intern("unquote-splice"))
                } else if peek_ch == "." {
                    chars.next();
                    Value::Symbol(vm.intern("unquote-splice!"))
                } else {
                    Value::Symbol(vm.intern("unquote"))
                };
                match read_inner(vm, reader_state, chars, buffer, in_back_quote, false) {
                    Ok((Some(exp), ichars)) => {
                        let cdr = alloc_pair(vm, exp, Value::Nil, reader_state);
                        return Ok((Some(alloc_pair(vm, sym, cdr, reader_state)), ichars));
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
                    "|" => consume_block_comment(&mut chars, reader_state),
                    "\\" => {
                        buffer.clear();
                        read_symbol(
                            buffer,
                            &mut chars,
                            reader_state,
                            true,
                            false,
                            &read_table_term,
                        );
                        match do_char(vm, reader_state, buffer) {
                            Ok(ch) => return Ok((Some(ch), chars)),
                            Err(e) => return Err((e, chars)),
                        };
                    }
                    "<" => {
                        let reason = format!(
                            "Found an unreadable token: line {}, col: {}",
                            reader_state.line, reader_state.column
                        );
                        return Err((ReadError { reason }, chars));
                    }
                    "(" => {
                        let (exp, chars) =
                            read_vector(vm, reader_state, chars, buffer, in_back_quote)?;
                        return Ok((Some(vm.alloc_vector_ro(exp)), chars));
                    }
                    "t" => {
                        return Ok((Some(Value::True), chars));
                    }
                    "f" => {
                        return Ok((Some(Value::False), chars));
                    }
                    "\"" => match read_string_literal(vm, reader_state, chars, buffer) {
                        Ok((s, ichars)) => {
                            return Ok((Some(Value::StringConst(vm.intern(s))), ichars))
                        }
                        Err((e, ichars)) => return Err((e, ichars)),
                    },
                    //"." => {
                    //    return prep_reader_macro(environment, chars, "reader-macro-dot", ".");
                    //}
                    // Read an octal int
                    "o" => {
                        let (exp, chars) =
                            read_num_radix(reader_state, chars, buffer, 8, &read_table_term)?;
                        return Ok((Some(Value::Int(exp)), chars));
                    }
                    // Read a hex int
                    "x" => {
                        let (exp, chars) =
                            read_num_radix(reader_state, chars, buffer, 16, &read_table_term)?;
                        return Ok((Some(Value::Int(exp)), chars));
                    }
                    // Read a binary int
                    "b" => {
                        let (exp, chars) =
                            read_num_radix(reader_state, chars, buffer, 2, &read_table_term)?;
                        return Ok((Some(Value::Int(exp)), chars));
                    }
                    ";" => {
                        match read_inner(vm, reader_state, chars, buffer, in_back_quote, false) {
                            Ok((_, ichars)) => {
                                // XXX TODO- this is a bug, need to wrap this in a special form or something,
                                // this indicates the input is done...
                                return Ok((None, ichars));
                            }
                            Err((err, ichars)) => {
                                return Err((err, ichars));
                            }
                        }
                    }
                    _ => {
                        let reason = format!(
                            "Found # with invalid char {}: line {}, col: {}",
                            peek_ch, reader_state.line, reader_state.column
                        );
                        return Err((ReadError { reason }, chars));
                    }
                }
            }
            "(" => {
                let (exp, chars) = read_list(vm, reader_state, chars, buffer, in_back_quote)?;
                return Ok((Some(exp), chars));
            }
            ")" => {
                if return_close_paren {
                    return Ok((Some(Value::Symbol(vm.intern(")"))), chars));
                } else {
                    let reason = format!(
                        "Unexpected ')': line {} col {}",
                        reader_state.line, reader_state.column
                    );
                    return Err((ReadError { reason }, chars));
                }
            }
            ";" => {
                consume_line_comment(&mut chars, reader_state);
            }
            _ => {
                buffer.clear();
                buffer.push_str(&ch);
                let is_number = read_symbol(
                    buffer,
                    &mut chars,
                    reader_state,
                    false,
                    false,
                    &read_table_term,
                );
                return Ok((Some(do_atom(vm, buffer, is_number)), chars));
            }
        }
        consume_whitespace(reader_state, &mut chars);
    }
    Ok((None, chars))
}

fn read_form(
    vm: &mut Vm,
    reader_state: &mut ReaderState,
    chars: CharIter,
) -> Result<(Option<Value>, CharIter), (ReadError, CharIter)> {
    vm.pause_gc();
    let mut buffer = String::new();
    let res = read_inner(vm, reader_state, chars, &mut buffer, false, false);
    vm.unpause_gc();
    res
}

fn _read_all_inner(
    vm: &mut Vm,
    reader_state: &mut ReaderState,
    text: &'static str,
) -> Result<Vec<Value>, ReadError> {
    let mut buffer = String::new();
    let mut exps = Vec::new();

    // Do this so the chars iterator has a static lifetime.  Should be ok since both the string
    // reference and iterator go away at the end of this function.
    //let ntext = unsafe { &*(text as *const str) };
    let mut chars: CharIter = Box::new(
        UnicodeSegmentation::graphemes(text, true)
            .map(Cow::Borrowed)
            .peekable(),
    );
    if text.starts_with("#!") {
        // Work with shebanged scripts.
        consume_line_comment(&mut chars, reader_state);
    }
    let mut cont = true;
    while cont {
        let (exp, ichars) = read_inner(vm, reader_state, chars, &mut buffer, false, false)
            .map_err(|(err, _)| err)?;
        if let Some(exp) = exp {
            exps.push(exp);
        } else {
            cont = false;
        }
        chars = ichars;
    }
    if chars.next().is_some() {
        let reason = format!(
            "Premature end (to many ')'?) line: {}, column: {}",
            reader_state.line, reader_state.column
        );
        return Err(ReadError { reason });
    }

    Ok(exps)
}

pub fn read_all(
    vm: &mut Vm,
    reader_state: &mut ReaderState,
    text: &'static str,
) -> Result<Vec<Value>, ReadError> {
    /*vm.pause_gc();
    let res = read_all_inner(vm, reader_state, text);
    vm.unpause_gc();
    res*/
    ReadIter::from_string(text.to_string(), vm, reader_state.clone()).collect()
}

pub struct ReadIter<'vm> {
    vm: &'vm mut Vm,
    reader_state: ReaderState,
    char_iter: Option<CharIter>,
}

impl<'vm> Iterator for ReadIter<'vm> {
    type Item = Result<Value, ReadError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(mut char_iter) = self.char_iter.take() {
            if char_iter.peek().is_none() {
                None
            } else {
                let (result, chars) = match read_form(self.vm, &mut self.reader_state, char_iter) {
                    Ok((None, _chars)) => return None,
                    Ok((Some(val), chars)) => (Ok(val), chars),
                    Err((err, chars)) => (Err(err), chars),
                };
                self.char_iter = Some(chars);
                Some(result)
            }
        } else {
            None
        }
    }
}

impl<'vm> ReadIter<'vm> {
    pub fn from_read<R: Read + 'static>(
        src: R,
        vm: &'vm mut Vm,
        reader_state: ReaderState,
    ) -> Self {
        let char_iter: CharIter = Box::new(
            Graphemes::from(src)
                .map(|s| {
                    if let Ok(s) = s {
                        Cow::Owned(s)
                    } else {
                        Cow::Borrowed("")
                    }
                })
                .peekable(),
        );
        Self {
            vm,
            reader_state,
            char_iter: Some(char_iter),
        }
    }
    pub fn from_file(src: File, vm: &'vm mut Vm, reader_state: ReaderState) -> Self {
        let char_iter: CharIter = Box::new(
            Graphemes::from(BufReader::new(src))
                .map(|s| {
                    if let Ok(s) = s {
                        Cow::Owned(s)
                    } else {
                        Cow::Borrowed("")
                    }
                })
                .peekable(),
        );
        Self {
            vm,
            reader_state,
            char_iter: Some(char_iter),
        }
    }

    pub fn from_string(src: String, vm: &'vm mut Vm, reader_state: ReaderState) -> Self {
        let char_iter: CharIter = Box::new(
            Graphemes::from(BufReader::new(Cursor::new(src.into_bytes())))
                .map(|s| {
                    if let Ok(s) = s {
                        Cow::Owned(s)
                    } else {
                        Cow::Borrowed("")
                    }
                })
                .peekable(),
        );
        Self {
            vm,
            reader_state,
            char_iter: Some(char_iter),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn to_strs(vm: &mut Vm, output: &mut Vec<String>, exp: Value) {
        match exp {
            Value::Pair(h) => {
                let (e1, e2) = vm.get_pair(h);
                if exp.is_proper_list(vm) {
                    output.push("(".to_string());
                    let exps: Vec<Value> = exp.iter(vm).collect();
                    for p in exps {
                        to_strs(vm, output, p);
                    }
                    output.push(")".to_string());
                } else {
                    output.push("(".to_string());
                    to_strs(vm, output, e1);
                    output.push(".".to_string());
                    to_strs(vm, output, e2);
                    output.push(")".to_string());
                }
            }
            Value::Vector(h) => {
                let list = vm.get_vector(h).to_vec();
                output.push("#(".to_string());
                for exp in list.iter() {
                    to_strs(vm, output, *exp);
                }
                output.push(")".to_string());
            }
            Value::Nil => output.push("nil".to_string()),
            _ => {
                output.push(format!(
                    "{}:{}",
                    exp.display_type(vm),
                    exp.display_value(vm)
                ));
            }
        }
    }

    fn read_test(
        vm: &mut Vm,
        reader_state: &mut ReaderState,
        text: &'static str,
    ) -> Result<Value, ReadError> {
        let exps = read_all(vm, reader_state, text)?;
        // Don't exit early without unpausing....
        vm.pause_gc();
        let res = if exps.len() == 1 {
            Ok(exps[0])
        } else {
            Ok(vm.alloc_vector_ro(exps))
        };
        vm.unpause_gc();
        res
    }

    fn tokenize(
        vm: &mut Vm,
        reader_state: &mut ReaderState,
        input: &'static str,
        _name: Option<&'static str>,
    ) -> Vec<String> {
        let exp = read_test(vm, reader_state, input);
        let mut tokens = Vec::new();
        if let Ok(exp) = exp {
            to_strs(vm, &mut tokens, exp);
        } else {
            panic!("Got unexpected token error: {:?}", exp);
        }
        tokens
    }

    fn tokenize_err(
        vm: &mut Vm,
        reader_state: &mut ReaderState,
        input: &'static str,
        _name: Option<&'static str>,
    ) -> ReadError {
        let exp = read_test(vm, reader_state, input);
        if let Err(err) = exp {
            err
        } else {
            panic!("Not an error but must be an error!");
        }
    }

    fn tokenize_wrap(vm: &mut Vm, reader_state: ReaderState, input: &str) -> Vec<String> {
        let mut tokens = Vec::new();
        let mut token_exps = Vec::new();
        let read_iter = ReadIter::from_string(input.to_string(), vm, reader_state);
        for exp in read_iter {
            token_exps.push(exp.unwrap());
        }
        let val = vm.alloc_vector_ro(token_exps);
        to_strs(vm, &mut tokens, val);
        tokens
    }

    fn build_def_vm() -> Vm {
        Vm::new()
    }

    #[test]
    fn test_tokenize() {
        let mut vm = build_def_vm();
        let mut reader_state = ReaderState::new();
        let tokens = tokenize(
            &mut vm,
            &mut reader_state,
            "one two three \"four\" 5 6",
            None,
        );
        println!("XXX {:?}", tokens);
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Symbol:one");
        assert!(tokens[2] == "Symbol:two");
        assert!(tokens[3] == "Symbol:three");
        assert!(tokens[4] == "String:\"four\"");
        assert!(tokens[5] == "Int:5");
        assert!(tokens[6] == "Int:6");
        assert!(tokens[7] == ")");
        let tokens = tokenize(&mut vm, &mut reader_state, "(1 2 3)", None);
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize(&mut vm, &mut reader_state, "  (  1    2\t3   )  ", None);
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize(&mut vm, &mut reader_state, "#(#\\A 2 3)", None);
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Char:#\\A");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize(&mut vm, &mut reader_state, "#(#\\  2 3)", None);
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Char:#\\ ");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize(&mut vm, &mut reader_state, "'((1 2 (3)))", None);
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
        let tokens = tokenize(
            &mut vm,
            &mut reader_state,
            "'((1 2 #;4 #;\"gone\"(3#;(x)) #;'(1 2 3)))",
            None,
        );
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
        let tokens = tokenize(&mut vm, &mut reader_state, "(length \"12345\")", None);
        assert!(tokens.len() == 4);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:length");
        assert!(tokens[2] == "String:\"12345\"");
        assert!(tokens[3] == ")");
        let tokens = tokenize(&mut vm, &mut reader_state, "(length \"12345Σ\")", None);
        assert!(tokens.len() == 4);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:length");
        assert!(tokens[2] == "String:\"12345Σ\"");
        assert!(tokens[3] == ")");
    }

    #[test]
    fn test_quotes() {
        let mut vm = build_def_vm();
        let mut reader_state = ReaderState::new();
        let tokens = tokenize(&mut vm, &mut reader_state, "'(1 2 3)", None);
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "Int:1");
        assert!(tokens[4] == "Int:2");
        assert!(tokens[5] == "Int:3");
        assert!(tokens[6] == ")");
        assert!(tokens[7] == ")");
        tokenize_err(&mut vm, &mut reader_state, "'(1 2 ,3)", None);
        tokenize_err(&mut vm, &mut reader_state, "'(1 2 ,@3)", None);
        let tokens = tokenize(&mut vm, &mut reader_state, "`(1 2 ,3)", None);
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
        let tokens = tokenize(&mut vm, &mut reader_state, "`(1 2 ,@3)", None);
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
        let tokens = tokenize(&mut vm, &mut reader_state, "`(1 2 ,.3)", None);
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:back-quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "Int:1");
        assert!(tokens[4] == "Int:2");
        assert!(tokens[5] == "(");
        assert!(tokens[6] == "Symbol:unquote-splice!");
        assert!(tokens[7] == "Int:3");
        assert!(tokens[8] == ")");
        assert!(tokens[9] == ")");
        assert!(tokens[10] == ")");
        let tokens = tokenize(&mut vm, &mut reader_state, "`(1 `2 ,@3)", None);
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
        let tokens = tokenize(&mut vm, &mut reader_state, "`(1 `(2 ,x) ,@3)", None);
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
        let mut vm = build_def_vm();
        let mut reader_state = ReaderState::new();
        let tokens = tokenize(
            &mut vm,
            &mut reader_state,
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
            &mut vm,
            &mut reader_state,
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
            &mut vm,
            &mut reader_state,
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
        let mut vm = build_def_vm();
        let mut reader_state = ReaderState::new();
        let tokens = tokenize(&mut vm, &mut reader_state, "(1 2 3)", None);
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize_wrap(&mut vm, reader_state.clone(), "(1 2 3)");
        assert!(tokens.len() == 7);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "(");
        assert!(tokens[2] == "Int:1");
        assert!(tokens[3] == "Int:2");
        assert!(tokens[4] == "Int:3");
        assert!(tokens[5] == ")");
        assert!(tokens[6] == ")");

        let tokens = tokenize(&mut vm, &mut reader_state, "1 2 3", None);
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize_wrap(&mut vm, reader_state.clone(), "1 2 3");
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");

        let tokens = tokenize(&mut vm, &mut reader_state, "(1 2 3) (4 5 6)", None);
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
        let tokens = tokenize_wrap(&mut vm, reader_state.clone(), "(1 2 3) (4 5 6)");
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

        let tokens = tokenize(&mut vm, &mut reader_state, "'(1 2 3)", None);
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "Int:1");
        assert!(tokens[4] == "Int:2");
        assert!(tokens[5] == "Int:3");
        assert!(tokens[6] == ")");
        assert!(tokens[7] == ")");
        let tokens = tokenize_wrap(&mut vm, reader_state.clone(), "'(1 2 3)");
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

        let tokens = tokenize(&mut vm, &mut reader_state, "nil", None);
        assert!(tokens.len() == 1);
        assert!(tokens[0] == "nil");
        let tokens = tokenize(&mut vm, &mut reader_state, "()", None);
        assert!(tokens.len() == 1);
        assert!(tokens[0] == "nil");
        let tokens = tokenize_wrap(&mut vm, reader_state.clone(), "nil");
        assert!(tokens.len() == 3);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "nil");
        assert!(tokens[2] == ")");
        let tokens = tokenize_wrap(&mut vm, reader_state.clone(), "()");
        assert!(tokens.len() == 3);
        assert!(tokens[0] == "#(");
        assert!(tokens[1] == "nil");
        assert!(tokens[2] == ")");
    }

    #[test]
    fn test_tok_strings() {
        let mut vm = build_def_vm();
        let mut reader_state = ReaderState::new();
        let input = r#""on\te\ntwo" two "th\rree" "fo\"u\\r" 5 6 "slash\x2fx\x2F\x3a\x3b""#;
        let tokens = tokenize(&mut vm, &mut reader_state, input, None);
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

        let input = r##"#"_on	e
two_" two "th\rree" "fo\"u\\r" 5 6 #"_slash/x/:;_""##;
        let tokens = tokenize(&mut vm, &mut reader_state, input, None);
        assert!(tokens.len() == 9);
        assert!(tokens[0] == "#(");
        assert!(
            tokens[1]
                == r#"String:"on	e
two""#
        );
        assert!(tokens[2] == "Symbol:two");
        assert!(tokens[3] == "String:\"th\rree\"");
        assert!(tokens[4] == "String:\"fo\"u\\r\"");
        assert!(tokens[5] == "Int:5");
        assert!(tokens[6] == "Int:6");
        assert!(tokens[7] == "String:\"slash/x/:;\"");
        assert!(tokens[8] == ")");

        let input =
            "\"\\u{03bb} two \" \"\\x20 \\u{03BB} end\" \"fo\\\"u\\\\r\" 5 6 \"slash\\x2fx\\x2F\\x3a\\x3b\"";
        let tokens = tokenize(&mut vm, &mut reader_state, input, None);
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
        let tokens = tokenize(&mut vm, &mut reader_state, input, None);
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
        let mut vm = build_def_vm();
        let mut reader_state = ReaderState::new();
        let input = "#\\x #\\X #\\x20 #\\u03bb #\\u{03BB} #\\u03bb";
        let tokens = tokenize(&mut vm, &mut reader_state, input, None);
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
        let mut vm = build_def_vm();
        let mut reader_state = ReaderState::new();
        let input = "2300 23_000 #xFF #xff #x0f #xF #b0000_0000 #b1111_1111 #b11111111 #b11111111_11111111 #o07 #o17";
        let tokens = tokenize(&mut vm, &mut reader_state, input, None);
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
        tokenize_err(&mut vm, &mut reader_state, input, None);
        let input = "#b1112";
        tokenize_err(&mut vm, &mut reader_state, input, None);
        let input = "#o80";
        tokenize_err(&mut vm, &mut reader_state, input, None);
    }

    #[test]
    fn test_tok_floats() {
        let mut vm = build_def_vm();
        let mut reader_state = ReaderState::new();
        let input = "2300.0 23_000.0 23e10 23e+5 23e-4 23e-+5 23e-5e+4 23.123 0.23.123";
        let tokens = tokenize(&mut vm, &mut reader_state, input, None);
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
