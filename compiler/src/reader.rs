use std::borrow::Cow;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::{BufReader, Cursor};
use std::num::{ParseFloatError, ParseIntError};

use compile_state::state::SloshVm;
use slvm::value::*;
use slvm::Chunk;
use unicode_reader::Graphemes;

pub trait PeekableIterator: std::iter::Iterator {
    fn peek(&mut self) -> Option<&Self::Item>;
}

impl<I: std::iter::Iterator> PeekableIterator for std::iter::Peekable<I> {
    fn peek(&mut self) -> Option<&Self::Item> {
        std::iter::Peekable::peek(self)
    }
}

pub type CharIter = Box<dyn PeekableIterator<Item = Cow<'static, str>>>;

struct ReaderCharIter {
    inner: CharIter,
    line: usize,
    column: usize,
}

impl Iterator for ReaderCharIter {
    type Item = Cow<'static, str>;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.inner.next();
        if let Some(ch) = &ch {
            if ch == "\n" {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
        }
        ch
    }
}

impl PeekableIterator for ReaderCharIter {
    fn peek(&mut self) -> Option<&Self::Item> {
        self.inner.peek()
    }
}

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

//#[derive(Clone, Debug)]
pub struct Reader<'vm> {
    vm: &'vm mut SloshVm,
    char_iter: Option<Box<ReaderCharIter>>,
    pub file_name: &'static str,
}

impl<'vm> Iterator for Reader<'vm> {
    type Item = Result<Value, ReadError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.chars().peek().is_none() {
            None
        } else {
            let result = match self.read_form() {
                Ok(None) => return None,
                Ok(Some(val)) => Ok(val),
                Err(err) => Err(err),
            };
            Some(result)
        }
    }
}

fn is_whitespace(ch: &str) -> bool {
    matches!(ch, " " | "\t" | "\n" | ",")
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
                reason: format!("Invalid hex digit {ch}, expected 0-9 or A-F."),
            }),
        }
    }
}

fn end_symbol(ch: &str, read_table_term: &HashMap<&'static str, Value>) -> bool {
    if is_whitespace(ch) || read_table_term.contains_key(ch) {
        true
    } else {
        matches!(
            ch,
            "(" | ")" | "#" | "\"" | "~" | "'" | "`" | "[" | "]" | "{" | "}" | "\\"
        )
    }
}

fn is_digit(ch: &str) -> bool {
    matches!(
        ch,
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    )
}

enum ReadReturn {
    None,
    List,
    Vector,
    Map,
}

impl<'vm> Reader<'vm> {
    pub fn from_file(
        src: File,
        vm: &'vm mut SloshVm,
        file_name: &'static str,
        line: usize,
        column: usize,
    ) -> Self {
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
        let char_iter = Box::new(ReaderCharIter {
            inner: char_iter,
            line,
            column,
        });
        Self {
            vm,
            char_iter: Some(char_iter),
            file_name,
        }
    }

    pub fn from_string(
        src: String,
        vm: &'vm mut SloshVm,
        file_name: &'static str,
        line: usize,
        column: usize,
    ) -> Self {
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
        let char_iter = Box::new(ReaderCharIter {
            inner: char_iter,
            line,
            column,
        });
        Self {
            vm,
            char_iter: Some(char_iter),
            file_name,
        }
    }

    fn line(&self) -> usize {
        self.char_iter.as_ref().expect("Invalid Reader!").line
    }

    fn column(&self) -> usize {
        self.char_iter.as_ref().expect("Invalid Reader!").column
    }

    pub fn vm(&mut self) -> &mut SloshVm {
        self.vm
    }

    fn chars(&mut self) -> &mut dyn PeekableIterator<Item = Cow<'static, str>> {
        &mut **self.char_iter.as_mut().expect("Invalid Reader!")
    }

    fn alloc_pair(&mut self, car: Value, cdr: Value) -> Value {
        let result = self.vm.alloc_pair_ro(car, cdr);
        // Just allocated this so the unwrap is safe.
        let file_name = self.vm.intern_static(self.file_name);
        self.vm
            .set_heap_property(result, "dbg-file", Value::StringConst(file_name));
        self.vm
            .set_heap_property(result, "dbg-line", Value::UInt32(self.line() as u32));
        self.vm
            .set_heap_property(result, "dbg-col", Value::UInt32(self.column() as u32));
        result
    }

    fn alloc_list(&mut self, list: Vec<Value>) -> Value {
        let result = self.vm.alloc_list_ro(list);
        // Just allocated this so the unwrap is safe.
        let file_name = self.vm.intern_static(self.file_name);
        self.vm
            .set_heap_property(result, "dbg-file", Value::StringConst(file_name));
        self.vm
            .set_heap_property(result, "dbg-line", Value::UInt32(self.line() as u32));
        self.vm
            .set_heap_property(result, "dbg-col", Value::UInt32(self.column() as u32));
        result
    }

    fn escape_to_char(&mut self) -> Result<char, ReadError> {
        if let (Some(ch1), Some(ch2)) = (self.chars().next(), self.chars().next()) {
            let ch_n: u8 = (char_to_hex_num(&ch1)? * 16) + (char_to_hex_num(&ch2)?);
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

    fn read_doc_string<'sym>(
        &mut self,
        buffer: &'sym mut String,
        read_table: &HashMap<&'static str, Chunk>,
    ) -> Result<&'sym mut String, ReadError> {
        self.read_string(buffer, read_table, true)
    }

    fn consume_line_comment(&mut self) {
        for ch in self.chars() {
            if ch == "\n" {
                return;
            }
        }
    }

    fn consume_block_comment(&mut self) {
        let mut depth = 1;
        let mut last_ch = Cow::Borrowed(" ");
        while let Some(ch) = self.chars().next() {
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

    fn do_char(
        &mut self,
        buffer: &mut String,
        read_table_term: &HashMap<&'static str, Value>,
    ) -> Result<Value, ReadError> {
        if let Some(ch) = self.chars().next() {
            if let Some(pch) = self.chars().peek() {
                if !is_whitespace(pch) {
                    match &*ch {
                        "u" => {
                            let ch = self.read_utf_scalar()?;
                            // XXX TODO- codepoint here?
                            return Ok(Value::CodePoint(ch));
                        }
                        "x" => {
                            let ch = self.escape_to_char()?;
                            return Ok(Value::CodePoint(ch));
                        }
                        _ => {
                            buffer.clear();
                            buffer.push_str(&ch);
                            self.read_symbol(buffer, true, false, read_table_term);
                            match &buffer.to_lowercase()[..] {
                                "space" => return Ok(Value::CodePoint(' ')),
                                "tab" => return Ok(Value::CodePoint('\t')),
                                // newline should be the platform line end.
                                "newline" => return Ok(Value::CodePoint('\n')),
                                "linefeed" => return Ok(Value::CodePoint('\n')),
                                "return" => return Ok(Value::CodePoint('\r')),
                                "backspace" => return Ok(Value::CodePoint('\u{0008}')),
                                _ => {
                                    let reason = format!(
                                        "Not a valid char [{}]: line {}, col: {}",
                                        buffer,
                                        self.line(),
                                        self.column()
                                    );
                                    return Err(ReadError { reason });
                                }
                            }
                        }
                    }
                }
            }
            if ch.len() == 1 {
                Ok(Value::CodePoint(ch.chars().next().unwrap()))
            } else if ch.len() < 7 {
                let mut v: [u8; 6] = [0; 6];
                for (i, c) in ch.bytes().enumerate() {
                    v[i] = c;
                }
                Ok(Value::CharCluster(ch.len() as u8, v))
            } else if let Value::String(handle) = self.vm.alloc_string_ro(ch.to_string()) {
                Ok(Value::CharClusterLong(handle))
            } else {
                panic!("Invalid alloc_string!");
            }
        } else {
            let reason = format!(
                "Not a valid char, missing: line {}, col: {}",
                self.line(),
                self.column()
            );
            Err(ReadError { reason })
        }
    }

    fn read_utf_scalar(&mut self) -> Result<char, ReadError> {
        fn finish(char_u32: u32) -> Result<char, ReadError> {
            if let Some(val) = std::char::from_u32(char_u32) {
                Ok(val)
            } else {
                Err(ReadError {
                    reason: format!("Invalid unicode scalar, {char_u32:x} not a valid utf scalar.",),
                })
            }
        }
        let mut first = true;
        let mut has_bracket = false;
        let mut char_u32 = 0;
        let mut nibbles = 0;
        while let Some(ch) = self.chars().next() {
            if ch == "\n" {
                if has_bracket {
                    return Err(ReadError {
                        reason: "Invalid unicode scalar, unexpected newline.".to_string(),
                    });
                } else {
                    return finish(char_u32);
                }
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
            if let Some(pch) = self.chars().peek() {
                if !has_bracket && is_whitespace(pch) {
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
        &mut self,
        symbol: &'sym mut String,
        read_table: &HashMap<&'static str, Chunk>,
        doc_string: bool,
    ) -> Result<&'sym mut String, ReadError> {
        symbol.clear();
        let mut last_ch_escape = false;

        while let Some(ch) = self.chars().next() {
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
                            let res = self.escape_to_char()?;
                            symbol.push(res);
                        }
                        "\\" => {
                            symbol.push('\\');
                        }
                        "u" => {
                            let res = self.read_utf_scalar()?;
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
                if doc_string {
                    let peek = if let Some(pch) = self.chars().peek() {
                        pch
                    } else {
                        ""
                    };
                    if ch == "!" && peek == "#" {
                        self.chars().next();
                        break;
                    }
                } else if ch == "\"" {
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
        Ok(symbol)
        //}
    }

    fn read_string_literal<'sym>(
        &mut self,
        symbol: &'sym mut String,
    ) -> Result<&'sym mut String, ReadError> {
        symbol.clear();
        let end_ch = if let Some(ch) = self.chars().next() {
            ch
        } else {
            return Err(ReadError {
                reason: "Unexpected stream end on string literal".to_string(),
            });
        };

        while let Some(ch) = self.chars().next() {
            let peek = if let Some(pch) = self.chars().peek() {
                pch
            } else {
                ""
            };
            if ch == end_ch && peek == "\"" {
                self.chars().next();
                return Ok(symbol);
            }
            symbol.push_str(&ch);
        }
        Err(ReadError {
            reason: "Unexpected end of string literal".to_string(),
        })
    }

    fn do_atom(&mut self, symbol: &str, is_number: bool) -> Value {
        if is_number {
            let mut num_str = symbol.to_string();
            num_str.retain(|ch| ch != '_');
            let potential_int: Result<i64, ParseIntError> = num_str.parse();
            match potential_int {
                Ok(v) => self.vm.alloc_int(v),
                Err(_) => {
                    let potential_float: Result<f64, ParseFloatError> = num_str.parse();
                    match potential_float {
                        Ok(f) => self.vm.alloc_f64(f),
                        Err(_) => Value::Symbol(self.vm.intern(symbol)),
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
                Value::Keyword(self.vm.intern(&symbol[1..]))
            } else {
                Value::Symbol(self.vm.intern(symbol))
            }
        }
    }

    fn read_symbol(
        &mut self,
        buffer: &mut String,
        for_ch: bool,
        skip_underscore: bool,
        read_table_term: &HashMap<&'static str, Value>,
    ) -> bool {
        fn maybe_number(
            ch: &str,
            has_e: &mut bool,
            last_e: &mut bool,
            has_decimal: &mut bool,
        ) -> bool {
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
        if let Some(ch) = self.chars().peek() {
            if end_symbol(ch, read_table_term) && !for_ch {
                return buffer.len() == 1 && is_digit(&buffer[..]);
            }
        };
        let mut next_ch = self.chars().next();
        while let Some(ch) = next_ch {
            let pch = self.chars().peek();
            let peek_ch = if let Some(pch) = pch {
                has_peek = true;
                pch
            } else {
                has_peek = false;
                " "
            };
            if ch == "\\" && has_peek && !for_ch {
                push_next = true;
            } else if !skip_underscore || ch != "_" {
                if is_number {
                    is_number = maybe_number(&ch, &mut has_e, &mut last_e, &mut has_decimal);
                }
                buffer.push_str(&ch);
            }
            if push_next {
                let next_ch = self.chars().next().unwrap();
                if is_number {
                    is_number = maybe_number(&ch, &mut has_e, &mut last_e, &mut has_decimal);
                }
                buffer.push_str(&next_ch);
                push_next = false;
            } else if end_symbol(peek_ch, read_table_term) {
                break;
            }
            next_ch = self.chars().next();
        }
        is_number
    }

    fn next2(&mut self) -> Option<(Cow<'static, str>, Cow<'static, str>)> {
        if let Some(ch) = self.chars().next() {
            let peek_ch = if let Some(pch) = self.chars().peek() {
                pch.clone()
            } else {
                Cow::Borrowed(" ")
            };
            Some((ch, peek_ch))
        } else {
            None
        }
    }

    fn consume_whitespace(&mut self) {
        // Consume whitespace.
        let mut ch = self.chars().peek();
        while ch.is_some() && is_whitespace(ch.unwrap()) {
            self.chars().next();
            ch = self.chars().peek();
        }
    }

    fn read_num_radix(
        &mut self,
        buffer: &mut String,
        radix: u32,
        read_table_term: &HashMap<&'static str, Value>,
    ) -> Result<i64, ReadError> {
        buffer.clear();
        self.read_symbol(buffer, true, true, read_table_term);
        match i64::from_str_radix(buffer, radix) {
            Ok(n) => Ok(n),
            Err(e) => Err(ReadError {
                reason: e.to_string(),
            }),
        }
    }

    fn read_vector(
        &mut self,
        buffer: &mut String,
        in_back_quote: bool,
    ) -> Result<Vec<Value>, ReadError> {
        let mut v: Vec<Value> = Vec::new();
        let mut cont = true;

        let close_intern = self.vm.intern("]");
        while cont {
            let exp = match self.read_inner(buffer, in_back_quote, ReadReturn::Vector) {
                Ok(exp) => {
                    if let Some(Value::Symbol(i)) = &exp {
                        if *i == close_intern {
                            return Ok(v);
                        }
                    }
                    exp
                }
                Err(err) => {
                    return Err(err);
                }
            };
            let pch = self.chars().peek();
            if let Some(exp) = exp {
                v.push(exp);
            } else if pch.is_none() {
                cont = false;
            }
        }
        Err(ReadError {
            reason: "Unclosed vector".to_string(),
        })
    }

    fn read_map(
        &mut self,
        buffer: &mut String,
        in_back_quote: bool,
    ) -> Result<HashMap<Value, Value>, ReadError> {
        let mut map: HashMap<Value, Value> = HashMap::new();
        let mut cont = true;

        let close_intern = self.vm.intern("}");
        while cont {
            let key = match self.read_inner(buffer, in_back_quote, ReadReturn::Map) {
                Ok(exp) => {
                    if let Some(Value::Symbol(i)) = &exp {
                        if *i == close_intern {
                            return Ok(map);
                        }
                    }
                    exp
                }
                Err(err) => {
                    return Err(err);
                }
            };
            let val = match self.read_inner(buffer, in_back_quote, ReadReturn::Map) {
                Ok(exp) => {
                    if let Some(Value::Symbol(i)) = &exp {
                        if *i == close_intern {
                            return Err(ReadError {
                                reason: "Map missing value".to_string(),
                            });
                        }
                    }
                    exp
                }
                Err(err) => {
                    return Err(err);
                }
            };
            let pch = self.chars().peek();
            if let (Some(key), Some(val)) = (key, val) {
                map.insert(key, val);
            } else if pch.is_none() {
                cont = false;
            }
        }
        Err(ReadError {
            reason: "Unclosed map".to_string(),
        })
    }

    fn get_unquote_lst(&mut self, exp: Value) -> Option<Value> {
        if let Value::Pair(h) = exp {
            let uq = self.vm.intern("unquote");
            let (car, cdr) = self.vm.get_pair(h);
            if let Value::Symbol(si) = car {
                if si == uq {
                    return Some(cdr);
                }
            }
        }
        None
    }

    fn unquote_splice(&mut self, exp: Value) -> bool {
        fn is_splice(vm: &mut SloshVm, car: Value) -> bool {
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
            let (car, _) = self.vm.get_pair(h);
            is_splice(self.vm, car)
        } else if let Value::Vector(h) = exp {
            let v = self.vm.get_vector(h);
            let car = if let Some(car) = v.get(0) {
                *car
            } else {
                return false;
            };
            is_splice(self.vm, car)
        } else {
            false
        }
    }

    fn read_list(&mut self, buffer: &mut String, in_back_quote: bool) -> Result<Value, ReadError> {
        let mut cont = true;
        let mut dot = false;
        let mut closed = false;
        let mut dot_count = 0;
        let mut list = Vec::new();
        let i_close = self.vm.intern(")");
        let i_dot = self.vm.intern(".");

        while cont {
            let exp = match self.read_inner(buffer, in_back_quote, ReadReturn::List) {
                Ok(exp) => {
                    if let Some(Value::Symbol(si)) = exp {
                        if si == i_close {
                            closed = true;
                            cont = false;
                            continue;
                        } else if si == i_dot {
                            dot = true;
                            continue;
                        }
                    }
                    exp
                }
                Err(err) => {
                    return Err(err);
                }
            };
            let pch = self.chars().peek();
            if let Some(exp) = exp {
                if list.is_empty() && dot {
                    return Err(ReadError {
                        reason: "Invalid dotted pair syntax (nothing before dot).".to_string(),
                    });
                } else if dot {
                    if self.unquote_splice(exp) {
                        return Err(ReadError {
                            reason: "Invalid dotted pair syntax with unquote-splice (,@/,.)."
                                .to_string(),
                        });
                    }
                    let exp = if let Some(uqexp) = self.get_unquote_lst(exp) {
                        // Do this so `(x y . ,z) works
                        let mut v = vec![Value::Symbol(self.vm.intern("unquote"))];
                        let mut i = 0;
                        for e in uqexp.iter(self.vm) {
                            v.push(e);
                            i += 1;
                        }
                        if i != 1 {
                            return Err(ReadError {
                                reason: "Invalid dotted pair syntax with unquote.".to_string(),
                            });
                        }
                        self.vm.alloc_vector_ro(v)
                    } else {
                        exp
                    };
                    list.push(exp);
                } else {
                    list.push(exp);
                }
            } else if pch.is_none() {
                cont = false;
            }
            if dot {
                dot_count += 1;
            }
            if dot_count > 1 {
                return Err(ReadError {
                    reason: "Invalid dotted pair syntax (more than object follows dot)."
                        .to_string(),
                });
            }
        }
        if !closed {
            Err(ReadError {
                reason: "Unclosed list".to_string(),
            })
        } else if dot {
            let mut list_iter = list.iter().rev();
            if let Some(last) = list_iter.next() {
                let mut last = *last;
                for v in list_iter {
                    last = self.alloc_pair(*v, last);
                }
                Ok(last)
            } else {
                Ok(Value::Nil)
            }
        } else if list.is_empty() {
            Ok(Value::Nil)
        } else {
            Ok(self.alloc_list(list))
        }
    }

    fn read_inner(
        &mut self,
        buffer: &mut String,
        in_back_quote: bool,
        return_close: ReadReturn,
    ) -> Result<Option<Value>, ReadError> {
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
        self.consume_whitespace();
        let read_table_term: HashMap<&'static str, Value> = HashMap::new();
        let read_table: HashMap<&'static str, Chunk> = HashMap::new();

        let i_quote = self.vm.intern("quote");
        let i_backquote = self.vm.intern("back-quote");
        while let Some((ch, peek_ch)) = self.next2() {
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
                    match self.read_string(buffer, &read_table, false) {
                        Ok(s) => return Ok(Some(Value::StringConst(self.vm.intern(s)))),
                        Err(e) => return Err(e),
                    };
                }
                "'" => match self.read_inner(buffer, in_back_quote, ReadReturn::None) {
                    Ok(Some(exp)) => {
                        let cdr = self.alloc_pair(exp, Value::Nil);
                        let qlist = self.alloc_pair(Value::Symbol(i_quote), cdr);
                        return Ok(Some(qlist));
                    }
                    Ok(None) => {
                        return Err(ReadError {
                            reason: "Invalid quote".to_string(),
                        });
                    }
                    Err(err) => {
                        return Err(err);
                    }
                },
                "`" => match self.read_inner(buffer, true, ReadReturn::None) {
                    Ok(Some(exp)) => {
                        let cdr = self.alloc_pair(exp, Value::Nil);
                        let qlist = self.alloc_pair(Value::Symbol(i_backquote), cdr);
                        return Ok(Some(qlist));
                    }
                    Ok(None) => {
                        return Err(ReadError {
                            reason: "Invalid back-quote".to_string(),
                        });
                    }
                    Err(err) => {
                        return Err(err);
                    }
                },
                "~" if in_back_quote => {
                    let sym = if peek_ch == "@" {
                        self.chars().next();
                        Value::Symbol(self.vm.intern("unquote-splice"))
                    } else if peek_ch == "." {
                        self.chars().next();
                        Value::Symbol(self.vm.intern("unquote-splice!"))
                    } else {
                        Value::Symbol(self.vm.intern("unquote"))
                    };
                    match self.read_inner(buffer, in_back_quote, ReadReturn::None) {
                        Ok(Some(exp)) => {
                            let cdr = self.alloc_pair(exp, Value::Nil);
                            return Ok(Some(self.alloc_pair(sym, cdr)));
                        }
                        Ok(None) => {
                            return Err(ReadError {
                                reason: "Invalid back-quote".to_string(),
                            });
                        }
                        Err(err) => {
                            return Err(err);
                        }
                    }
                }
                "~" => {
                    return Err(ReadError {
                        reason: "Unquote outside of a back-quote".to_string(),
                    })
                }
                "\\" => {
                    return Ok(Some(self.do_char(buffer, &read_table_term)?));
                }
                "#" => {
                    self.chars().next();
                    match &*peek_ch {
                        "|" => self.consume_block_comment(),
                        "!" => {
                            match self.read_doc_string(buffer, &read_table) {
                                Ok(s) => {
                                    let doc_sym = Value::Symbol(self.vm.intern("doc-string"));
                                    let doc_string = Value::StringConst(self.vm.intern(s));
                                    let list = self.alloc_list(vec![doc_sym, doc_string]);
                                    return Ok(Some(list));
                                }
                                Err(e) => return Err(e),
                            };
                        }
                        "<" => {
                            let reason = format!(
                                "Found an unreadable token: line {}, col: {}",
                                self.line(),
                                self.column()
                            );
                            return Err(ReadError { reason });
                        }
                        "t" => {
                            return Ok(Some(Value::True));
                        }
                        "f" => {
                            return Ok(Some(Value::False));
                        }
                        "\"" => match self.read_string_literal(buffer) {
                            Ok(s) => return Ok(Some(Value::StringConst(self.vm.intern(s)))),
                            Err(e) => return Err(e),
                        },
                        //"." => {
                        //    return prep_reader_macro(environment, chars, "reader-macro-dot", ".");
                        //}
                        // Read an octal int
                        "o" => {
                            let exp = self.read_num_radix(buffer, 8, &read_table_term)?;
                            return Ok(Some(self.vm.alloc_int(exp)));
                        }
                        // Read a hex int
                        "x" => {
                            let exp = self.read_num_radix(buffer, 16, &read_table_term)?;
                            return Ok(Some(self.vm.alloc_int(exp)));
                        }
                        // Read a binary int
                        "b" => {
                            let exp = self.read_num_radix(buffer, 2, &read_table_term)?;
                            return Ok(Some(self.vm.alloc_int(exp)));
                        }
                        ";" => {
                            match self.read_inner(buffer, in_back_quote, ReadReturn::None) {
                                Ok(_) => {
                                    // Consumed and threw away one form so return the next.
                                    return self.read_inner(buffer, in_back_quote, return_close);
                                }
                                Err(err) => {
                                    return Err(err);
                                }
                            }
                        }
                        _ => {
                            let reason = format!(
                                "Found # with invalid char {}: line {}, col: {}",
                                peek_ch,
                                self.line(),
                                self.column()
                            );
                            return Err(ReadError { reason });
                        }
                    }
                }
                "(" => {
                    let exp = self.read_list(buffer, in_back_quote)?;
                    return Ok(Some(exp));
                }
                ")" if matches!(return_close, ReadReturn::List) => {
                    return Ok(Some(Value::Symbol(self.vm.intern(")"))));
                }
                ")" => {
                    let reason =
                        format!("Unexpected ')': line {} col {}", self.line(), self.column());
                    return Err(ReadError { reason });
                }
                "{" => {
                    let exp = self.read_map(buffer, in_back_quote)?;
                    return Ok(Some(self.vm.alloc_map_ro(exp)));
                }
                "}" if matches!(return_close, ReadReturn::Map) => {
                    return Ok(Some(Value::Symbol(self.vm.intern("}"))));
                }
                "}" => {
                    let reason = format!(
                        "Unexpected '}}': line {} col {}",
                        self.line(),
                        self.column()
                    );
                    return Err(ReadError { reason });
                }
                "[" => {
                    let exp = self.read_vector(buffer, in_back_quote)?;
                    return Ok(Some(self.vm.alloc_vector_ro(exp)));
                }
                "]" if matches!(return_close, ReadReturn::Vector) => {
                    return Ok(Some(Value::Symbol(self.vm.intern("]"))));
                }
                "]" => {
                    let reason =
                        format!("Unexpected ']': line {} col {}", self.line(), self.column());
                    return Err(ReadError { reason });
                }
                ";" => self.consume_line_comment(),
                _ => {
                    buffer.clear();
                    buffer.push_str(&ch);
                    let is_number = self.read_symbol(buffer, false, false, &read_table_term);
                    return Ok(Some(self.do_atom(buffer, is_number)));
                }
            }
            self.consume_whitespace();
        }
        Ok(None)
    }

    fn read_form(&mut self) -> Result<Option<Value>, ReadError> {
        self.vm.pause_gc();
        let mut buffer = String::new();
        let res = self.read_inner(&mut buffer, false, ReadReturn::None);
        self.vm.unpause_gc();
        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use compile_state::state::{new_slosh_vm, SloshVm};

    fn to_strs(vm: &mut SloshVm, output: &mut Vec<String>, exp: Value) {
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
            Value::List(h, _) => {
                let list = vm.get_vector(h).to_vec();
                output.push("(".to_string());
                for exp in list.iter() {
                    to_strs(vm, output, *exp);
                }
                output.push(")".to_string());
            }
            Value::Vector(h) => {
                let list = vm.get_vector(h).to_vec();
                output.push("[".to_string());
                for exp in list.iter() {
                    to_strs(vm, output, *exp);
                }
                output.push("]".to_string());
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

    fn read_test(vm: &mut SloshVm, text: &'static str) -> Result<Value, ReadError> {
        let reader = Reader::from_string(text.to_string(), vm, "", 1, 0);
        let exps: Vec<Value> = reader.collect::<Result<Vec<Value>, ReadError>>()?;
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

    fn tokenize(vm: &mut SloshVm, input: &'static str) -> Vec<String> {
        let exp = read_test(vm, input);
        let mut tokens = Vec::new();
        if let Ok(exp) = exp {
            to_strs(vm, &mut tokens, exp);
        } else {
            panic!("Got unexpected token error: {exp:?}");
        }
        tokens
    }

    fn tokenize_err(vm: &mut SloshVm, input: &'static str) -> ReadError {
        let exp = read_test(vm, input);
        if let Err(err) = exp {
            err
        } else {
            panic!("Not an error but must be an error!");
        }
    }

    fn tokenize_wrap(vm: &mut SloshVm, input: &str) -> Vec<String> {
        let mut tokens = Vec::new();
        let mut token_exps = Vec::new();
        let read_iter = Reader::from_string(input.to_string(), vm, "", 1, 0);
        for exp in read_iter {
            token_exps.push(exp.unwrap());
        }
        let val = vm.alloc_vector_ro(token_exps);
        to_strs(vm, &mut tokens, val);
        tokens
    }

    fn build_def_vm() -> SloshVm {
        new_slosh_vm()
    }

    #[test]
    fn test_tokenize() {
        let mut vm = build_def_vm();
        let tokens = tokenize(&mut vm, "one two three \"four\" 5 6");
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "Symbol:one");
        assert!(tokens[2] == "Symbol:two");
        assert!(tokens[3] == "Symbol:three");
        assert!(tokens[4] == "String:\"four\"");
        assert!(tokens[5] == "UInt:5");
        assert!(tokens[6] == "UInt:6");
        assert!(tokens[7] == "]");

        let tokens = tokenize(&mut vm, "one, two,three ,,,, \"four\" 5 , 6,");
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "Symbol:one");
        assert!(tokens[2] == "Symbol:two");
        assert!(tokens[3] == "Symbol:three");
        assert!(tokens[4] == "String:\"four\"");
        assert!(tokens[5] == "UInt:5");
        assert!(tokens[6] == "UInt:6");
        assert!(tokens[7] == "]");

        let tokens = tokenize(&mut vm, "(1 2 3)");
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "UInt:1");
        assert!(tokens[2] == "UInt:2");
        assert!(tokens[3] == "UInt:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize(&mut vm, "  (  1    2\t3   )  ");
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "UInt:1");
        assert!(tokens[2] == "UInt:2");
        assert!(tokens[3] == "UInt:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize(&mut vm, "[\\A 2 3]");
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "Char:\\A");
        assert!(tokens[2] == "UInt:2");
        assert!(tokens[3] == "UInt:3");
        assert!(tokens[4] == "]");
        let tokens = tokenize(&mut vm, "[\\  2 3]");
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "Char:\\ ");
        assert!(tokens[2] == "UInt:2");
        assert!(tokens[3] == "UInt:3");
        assert!(tokens[4] == "]");
        let tokens = tokenize(&mut vm, "'((1 2 (3)))");
        assert!(tokens.len() == 12);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "(");
        assert!(tokens[4] == "UInt:1");
        assert!(tokens[5] == "UInt:2");
        assert!(tokens[6] == "(");
        assert!(tokens[7] == "UInt:3");
        assert!(tokens[8] == ")");
        assert!(tokens[9] == ")");
        assert!(tokens[10] == ")");
        assert!(tokens[11] == ")");
        let tokens = tokenize(&mut vm, "'((1 2 #;4 #;\"gone\"(3#;(x)) #;'(1 2 3)))");
        assert!(tokens.len() == 12);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "(");
        assert!(tokens[4] == "UInt:1");
        assert!(tokens[5] == "UInt:2");
        assert!(tokens[6] == "(");
        assert!(tokens[7] == "UInt:3");
        assert!(tokens[8] == ")");
        assert!(tokens[9] == ")");
        assert!(tokens[10] == ")");
        assert!(tokens[11] == ")");
        let tokens = tokenize(&mut vm, "(length \"12345\")");
        assert!(tokens.len() == 4);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:length");
        assert!(tokens[2] == "String:\"12345\"");
        assert!(tokens[3] == ")");
        let tokens = tokenize(&mut vm, "(length \"12345Σ\")");
        assert!(tokens.len() == 4);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:length");
        assert!(tokens[2] == "String:\"12345Σ\"");
        assert!(tokens[3] == ")");
    }

    #[test]
    fn test_quotes() {
        let mut vm = build_def_vm();
        let tokens = tokenize(&mut vm, "'(1 2 3)");
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "UInt:1");
        assert!(tokens[4] == "UInt:2");
        assert!(tokens[5] == "UInt:3");
        assert!(tokens[6] == ")");
        assert!(tokens[7] == ")");
        tokenize_err(&mut vm, "'(1 2 ~3)");
        tokenize_err(&mut vm, "'(1 2 ~@3)");
        let tokens = tokenize(&mut vm, "`(1 2 ~3)");
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:back-quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "UInt:1");
        assert!(tokens[4] == "UInt:2");
        assert!(tokens[5] == "(");
        assert!(tokens[6] == "Symbol:unquote");
        assert!(tokens[7] == "UInt:3");
        assert!(tokens[8] == ")");
        assert!(tokens[9] == ")");
        assert!(tokens[10] == ")");
        let tokens = tokenize(&mut vm, "`(1 2 ~@3)");
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:back-quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "UInt:1");
        assert!(tokens[4] == "UInt:2");
        assert!(tokens[5] == "(");
        assert!(tokens[6] == "Symbol:unquote-splice");
        assert!(tokens[7] == "UInt:3");
        assert!(tokens[8] == ")");
        assert!(tokens[9] == ")");
        assert!(tokens[10] == ")");
        let tokens = tokenize(&mut vm, "`(1 2 ~.3)");
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:back-quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "UInt:1");
        assert!(tokens[4] == "UInt:2");
        assert!(tokens[5] == "(");
        assert!(tokens[6] == "Symbol:unquote-splice!");
        assert!(tokens[7] == "UInt:3");
        assert!(tokens[8] == ")");
        assert!(tokens[9] == ")");
        assert!(tokens[10] == ")");
        let tokens = tokenize(&mut vm, "`(1 `2 ~@3)");
        assert!(tokens.len() == 14);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:back-quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "UInt:1");
        assert!(tokens[4] == "(");
        assert!(tokens[5] == "Symbol:back-quote");
        assert!(tokens[6] == "UInt:2");
        assert!(tokens[7] == ")");
        assert!(tokens[8] == "(");
        assert!(tokens[9] == "Symbol:unquote-splice");
        assert!(tokens[10] == "UInt:3");
        assert!(tokens[11] == ")");
        assert!(tokens[12] == ")");
        assert!(tokens[13] == ")");
        let tokens = tokenize(&mut vm, "`(1 `(2 ~x) ~@3)");
        assert!(tokens.len() == 20);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:back-quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "UInt:1");
        assert!(tokens[4] == "(");
        assert!(tokens[5] == "Symbol:back-quote");
        assert!(tokens[6] == "(");
        assert!(tokens[7] == "UInt:2");
        assert!(tokens[8] == "(");
        assert!(tokens[9] == "Symbol:unquote");
        assert!(tokens[10] == "Symbol:x");
        assert!(tokens[11] == ")");
        assert!(tokens[12] == ")");
        assert!(tokens[13] == ")");
        assert!(tokens[14] == "(");
        assert!(tokens[15] == "Symbol:unquote-splice");
        assert!(tokens[16] == "UInt:3");
        assert!(tokens[17] == ")");
        assert!(tokens[18] == ")");
        assert!(tokens[19] == ")");
    }

    #[test]
    fn test_types() {
        let mut vm = build_def_vm();
        let tokens = tokenize(&mut vm, "(one 2 3.0 \"four\" \\B #t nil 3.5 ())");
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:one");
        assert!(tokens[2] == "UInt:2");
        assert!(tokens[3] == "Float:3");
        assert!(tokens[4] == "String:\"four\"");
        assert!(tokens[5] == "Char:\\B");
        assert!(tokens[6] == "True:true");
        assert!(tokens[7] == "nil");
        assert!(tokens[8] == "Float:3.5");
        assert!(tokens[9] == "nil");
        assert!(tokens[10] == ")");

        let tokens = tokenize(&mut vm, "[one 2 3.0 \"four\" \\B #t nil 3.5 ()]");
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "Symbol:one");
        assert!(tokens[2] == "UInt:2");
        assert!(tokens[3] == "Float:3");
        assert!(tokens[4] == "String:\"four\"");
        assert!(tokens[5] == "Char:\\B");
        assert!(tokens[6] == "True:true");
        assert!(tokens[7] == "nil");
        assert!(tokens[8] == "Float:3.5");
        assert!(tokens[9] == "nil");
        assert!(tokens[10] == "]");

        let tokens = tokenize(&mut vm, "one 2 3.0 \"four\" \\B #t nil 3.5 ()");
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "Symbol:one");
        assert!(tokens[2] == "UInt:2");
        assert!(tokens[3] == "Float:3");
        assert!(tokens[4] == "String:\"four\"");
        assert!(tokens[5] == "Char:\\B");
        assert!(tokens[6] == "True:true");
        assert!(tokens[7] == "nil");
        assert!(tokens[8] == "Float:3.5");
        assert!(tokens[9] == "nil");
        assert!(tokens[10] == "]");
    }

    #[test]
    fn test_wrap() {
        let mut vm = build_def_vm();
        let tokens = tokenize(&mut vm, "(1 2 3)");
        println!("XXXX toks {tokens:?}");
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "UInt:1");
        assert!(tokens[2] == "UInt:2");
        assert!(tokens[3] == "UInt:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize_wrap(&mut vm, "(1 2 3)");
        assert!(tokens.len() == 7);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "(");
        assert!(tokens[2] == "UInt:1");
        assert!(tokens[3] == "UInt:2");
        assert!(tokens[4] == "UInt:3");
        assert!(tokens[5] == ")");
        assert!(tokens[6] == "]");

        let tokens = tokenize(&mut vm, "1 2 3");
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "UInt:1");
        assert!(tokens[2] == "UInt:2");
        assert!(tokens[3] == "UInt:3");
        assert!(tokens[4] == "]");
        let tokens = tokenize_wrap(&mut vm, "1 2 3");
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "UInt:1");
        assert!(tokens[2] == "UInt:2");
        assert!(tokens[3] == "UInt:3");
        assert!(tokens[4] == "]");

        let tokens = tokenize(&mut vm, "(1 2 3) (4 5 6)");
        assert!(tokens.len() == 12);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "(");
        assert!(tokens[2] == "UInt:1");
        assert!(tokens[3] == "UInt:2");
        assert!(tokens[4] == "UInt:3");
        assert!(tokens[5] == ")");
        assert!(tokens[6] == "(");
        assert!(tokens[7] == "UInt:4");
        assert!(tokens[8] == "UInt:5");
        assert!(tokens[9] == "UInt:6");
        assert!(tokens[10] == ")");
        assert!(tokens[11] == "]");
        let tokens = tokenize_wrap(&mut vm, "(1 2 3) (4 5 6)");
        assert!(tokens.len() == 12);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "(");
        assert!(tokens[2] == "UInt:1");
        assert!(tokens[3] == "UInt:2");
        assert!(tokens[4] == "UInt:3");
        assert!(tokens[5] == ")");
        assert!(tokens[6] == "(");
        assert!(tokens[7] == "UInt:4");
        assert!(tokens[8] == "UInt:5");
        assert!(tokens[9] == "UInt:6");
        assert!(tokens[10] == ")");
        assert!(tokens[11] == "]");

        let tokens = tokenize(&mut vm, "'(1 2 3)");
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "UInt:1");
        assert!(tokens[4] == "UInt:2");
        assert!(tokens[5] == "UInt:3");
        assert!(tokens[6] == ")");
        assert!(tokens[7] == ")");
        let tokens = tokenize_wrap(&mut vm, "'(1 2 3)");
        assert!(tokens.len() == 10);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "(");
        assert!(tokens[2] == "Symbol:quote");
        assert!(tokens[3] == "(");
        assert!(tokens[4] == "UInt:1");
        assert!(tokens[5] == "UInt:2");
        assert!(tokens[6] == "UInt:3");
        assert!(tokens[7] == ")");
        assert!(tokens[8] == ")");
        assert!(tokens[9] == "]");

        let tokens = tokenize(&mut vm, "nil");
        assert!(tokens.len() == 1);
        assert!(tokens[0] == "nil");
        let tokens = tokenize(&mut vm, "()");
        assert!(tokens.len() == 1);
        assert!(tokens[0] == "nil");
        let tokens = tokenize_wrap(&mut vm, "nil");
        assert!(tokens.len() == 3);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "nil");
        assert!(tokens[2] == "]");
        let tokens = tokenize_wrap(&mut vm, "()");
        assert!(tokens.len() == 3);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "nil");
        assert!(tokens[2] == "]");
    }

    #[test]
    fn test_tok_strings() {
        let mut vm = build_def_vm();
        let input = r#""on\te\ntwo" two "th\rree" "fo\"u\\r" 5 6 "slash\x2fx\x2F\x3a\x3b""#;
        let tokens = tokenize(&mut vm, input);
        assert!(tokens.len() == 9);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "String:\"on\te\ntwo\"");
        assert!(tokens[2] == "Symbol:two");
        assert!(tokens[3] == "String:\"th\rree\"");
        assert!(tokens[4] == "String:\"fo\"u\\r\"");
        assert!(tokens[5] == "UInt:5");
        assert!(tokens[6] == "UInt:6");
        assert!(tokens[7] == "String:\"slash/x/:;\"");
        assert!(tokens[8] == "]");

        let input = r##"#"_on	e
two_" two "th\rree" "fo\"u\\r" 5 6 #"_slash/x/:;_""##;
        let tokens = tokenize(&mut vm, input);
        assert!(tokens.len() == 9);
        assert!(tokens[0] == "[");
        assert!(
            tokens[1]
                == r#"String:"on	e
two""#
        );
        assert!(tokens[2] == "Symbol:two");
        assert!(tokens[3] == "String:\"th\rree\"");
        assert!(tokens[4] == "String:\"fo\"u\\r\"");
        assert!(tokens[5] == "UInt:5");
        assert!(tokens[6] == "UInt:6");
        assert!(tokens[7] == "String:\"slash/x/:;\"");
        assert!(tokens[8] == "]");

        let input =
            "\"\\u{03bb} two \" \"\\x20 \\u{03BB} end\" \"fo\\\"u\\\\r\" 5 6 \"slash\\x2fx\\x2F\\x3a\\x3b\"";
        let tokens = tokenize(&mut vm, input);
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "String:\"\u{03bb} two \"");
        assert!(tokens[2] == "String:\"  λ end\"");
        assert!(tokens[3] == "String:\"fo\"u\\r\"");
        assert!(tokens[4] == "UInt:5");
        assert!(tokens[5] == "UInt:6");
        assert!(tokens[6] == "String:\"slash/x/:;\"");
        assert!(tokens[7] == "]");

        let input =
            "\"\\u03bb two \" \"\\x20 \\u03BB \nend\" \"fo\\\"u\\\\r\" 5 6 \"slash\\x2fx\\x2F\\x3a\\x3b\"";
        let tokens = tokenize(&mut vm, input);
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "String:\"\u{03bb} two \"");
        assert!(tokens[2] == "String:\"  λ \nend\"");
        assert!(tokens[3] == "String:\"fo\"u\\r\"");
        assert!(tokens[4] == "UInt:5");
        assert!(tokens[5] == "UInt:6");
        assert!(tokens[6] == "String:\"slash/x/:;\"");
        assert!(tokens[7] == "]");
    }

    #[test]
    fn test_tok_chars() {
        let mut vm = build_def_vm();
        let input = "\\x \\X \\x20 \\u03bb \\u{03BB} \\u03bb";
        let tokens = tokenize(&mut vm, input);
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "Char:\\x");
        assert!(tokens[2] == "Char:\\X");
        assert!(tokens[3] == "Char:\\ ");
        assert!(tokens[4] == "Char:\\λ");
        assert!(tokens[5] == "Char:\\\u{03bb}");
        assert!(tokens[6] == "Char:\\λ");
        assert!(tokens[7] == "]");
    }

    #[test]
    fn test_tok_ints() {
        let mut vm = build_def_vm();
        let input = "2300 23_000 #xFF #xff #x0f #xF #b0000_0000 #b1111_1111 #b11111111 #b11111111_11111111 #o07 #o17";
        let tokens = tokenize(&mut vm, input);
        assert!(tokens.len() == 14);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "UInt:2300");
        assert!(tokens[2] == "UInt:23000");
        assert!(tokens[3] == "UInt:255");
        assert!(tokens[4] == "UInt:255");
        assert!(tokens[5] == "UInt:15");
        assert!(tokens[6] == "UInt:15");
        assert!(tokens[7] == "UInt:0");
        assert!(tokens[8] == "UInt:255");
        assert!(tokens[9] == "UInt:255");
        assert!(tokens[10] == "UInt:65535");
        assert!(tokens[11] == "UInt:7");
        assert!(tokens[12] == "UInt:15");
        assert!(tokens[13] == "]");
        let input = "#xFG";
        tokenize_err(&mut vm, input);
        let input = "#b1112";
        tokenize_err(&mut vm, input);
        let input = "#o80";
        tokenize_err(&mut vm, input);
    }

    #[test]
    fn test_tok_floats() {
        let mut vm = build_def_vm();
        let input = "2300.0 23_000.0 23e10 23e+5 23e-4 23e-+5 23e-5e+4 23.123 0.23.123";
        let tokens = tokenize(&mut vm, input);
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "Float:2300");
        assert!(tokens[2] == "Float:23000");
        assert!(tokens[3] == "Float:230000000000");
        assert!(tokens[4] == "Float:2300000");
        assert!(tokens[5] == "Float:0.0023");
        assert!(tokens[6] == "Symbol:23e-+5");
        assert!(tokens[7] == "Symbol:23e-5e+4");
        assert!(tokens[8] == "Float:23.123");
        assert!(tokens[9] == "Symbol:0.23.123");
        assert!(tokens[10] == "]");
    }

    #[test]
    fn test_doc_string() {
        let mut vm = build_def_vm();
        let input = "#! Doc string! !#";
        let tokens = tokenize(&mut vm, input);
        assert!(tokens.len() == 4);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:doc-string");
        assert!(tokens[2] == "String:\" Doc string! \"");
        assert!(tokens[3] == ")");
        let input = "#!Doc string!!#";
        let tokens = tokenize(&mut vm, input);
        assert!(tokens.len() == 4);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:doc-string");
        assert!(tokens[2] == "String:\"Doc string!\"");
        assert!(tokens[3] == ")");
        let input = "#!\
         Doc string!\
         !#\
         (defn fnx () (prn x))";
        let tokens = tokenize(&mut vm, input);
        assert!(tokens.len() == 15);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "(");
        assert!(tokens[2] == "Symbol:doc-string");
        assert!(
            tokens[3]
                == "String:\"\
         Doc string!\
          \""
        );
        assert!(tokens[12] == ")");
        assert!(tokens[13] == ")");
        assert!(tokens[14] == "]");
    }
}
