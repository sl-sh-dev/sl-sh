use std::borrow::Cow;
use std::collections::HashMap;
use std::error::Error;
use std::fs::File;
use std::io::{BufReader, Cursor, Read};
use std::num::{ParseFloatError, ParseIntError};
use std::{fmt, io};

use compile_state::state::{SloshVm, SloshVmTrait};
use slvm::{Chunk, Value};
use unicode_reader::Graphemes;

pub trait PeekableIterator: std::iter::Iterator {
    fn peek(&mut self) -> Option<&Self::Item>;
}

impl<I: std::iter::Iterator> PeekableIterator for std::iter::Peekable<I> {
    fn peek(&mut self) -> Option<&Self::Item> {
        std::iter::Peekable::peek(self)
    }
}

pub type CharIter = Box<dyn PeekableIterator<Item = io::Result<Cow<'static, str>>>>;

pub struct ReaderCharIter {
    inner: CharIter,
    line: usize,
    column: usize,
}

impl Iterator for ReaderCharIter {
    type Item = io::Result<Cow<'static, str>>;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.inner.next();
        if let Some(Ok(ch)) = &ch {
            if &**ch == "\n" {
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

impl From<io::Error> for ReadError {
    fn from(err: io::Error) -> Self {
        Self {
            reason: format!("IO Error: {}", err),
        }
    }
}

impl From<&io::Error> for ReadError {
    fn from(err: &io::Error) -> Self {
        Self {
            reason: format!("IO Error: {}", err),
        }
    }
}

pub struct Reader<'vm> {
    vm: &'vm mut SloshVm,
    char_iter: Option<Box<ReaderCharIter>>,
    file_name: &'static str,
}

impl Iterator for Reader<'_> {
    type Item = Result<Value, ReadError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.chars().peek().is_none() {
            None
        } else {
            match self.read_form() {
                Ok(None) => None,
                Ok(Some(val)) => Some(Ok(val)),
                Err(err) => Some(Err(err)),
            }
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

/** Peek the next char and return Ok(true) if it signals an end symbol. */
fn end_symbol(
    ch: Option<&io::Result<Cow<'static, str>>>,
    read_table_term: &HashMap<&'static str, Value>,
) -> Result<bool, ReadError> {
    if let Some(ch) = ch {
        match ch {
            Ok(ch) => {
                let ch = &**ch;
                if is_whitespace(ch) || read_table_term.contains_key(ch) {
                    Ok(true)
                } else {
                    Ok(matches!(
                        ch,
                        "(" | ")"
                            | "#"
                            | "\""
                            | "~"
                            | "'"
                            | "`"
                            | "["
                            | "]"
                            | "{"
                            | "}"
                            | "\\"
                            | ";"
                    ))
                }
            }
            Err(e) => Err(e.into()),
        }
    } else {
        Ok(false)
    }
}

fn is_digit(ch: &str) -> bool {
    matches!(
        ch,
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    )
}

#[derive(Copy, Clone, Debug)]
enum ReadReturn {
    None,
    List,
    Vector,
    Map,
}

impl<'vm> Reader<'vm> {
    pub fn from_reader<R: Read + 'static>(
        src: R,
        vm: &'vm mut SloshVm,
        file_name: &'static str,
        line: usize,
        column: usize,
    ) -> Self {
        let char_iter: CharIter = Box::new(
            Graphemes::from(src)
                .map(|s| match s {
                    Ok(s) => Ok(Cow::Owned(s)),
                    Err(e) => Err(e),
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

    pub fn from_file(
        src: File,
        vm: &'vm mut SloshVm,
        file_name: &'static str,
        line: usize,
        column: usize,
    ) -> Self {
        Self::from_reader(BufReader::new(src), vm, file_name, line, column)
    }

    pub fn from_static_string(
        src: &'static str,
        vm: &'vm mut SloshVm,
        file_name: &'static str,
        line: usize,
        column: usize,
    ) -> Self {
        let char_iter: CharIter = Box::new(
            Graphemes::from(BufReader::new(Cursor::new(src.as_bytes())))
                .map(|s| match s {
                    Ok(s) => Ok(Cow::Owned(s)),
                    Err(e) => Err(e),
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
                .map(|s| match s {
                    Ok(s) => Ok(Cow::Owned(s)),
                    Err(e) => Err(e),
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

    /** Consume the Reader and return the internal Char iter. */
    pub fn into_char_iter(mut self) -> Box<ReaderCharIter> {
        self.char_iter.take().expect("invalid Reader")
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

    fn chars(&mut self) -> &mut dyn PeekableIterator<Item = io::Result<Cow<'static, str>>> {
        &mut **self.char_iter.as_mut().expect("Invalid Reader!")
    }

    fn alloc_pair(&mut self, car: Value, cdr: Value, line: u32, column: u32) -> Value {
        let result = self.vm.alloc_pair_ro(car, cdr);
        // Just allocated this so the unwrap is safe.
        let file_name = self.vm.intern_static(self.file_name);
        self.vm
            .set_heap_property(result, "dbg-file", Value::StringConst(file_name));
        self.vm.set_heap_property(result, "dbg-line", line.into());
        self.vm.set_heap_property(result, "dbg-col", column.into());
        result
    }

    fn alloc_list(&mut self, list: Vec<Value>, line: u32, column: u32) -> Value {
        let result = self.vm.alloc_list_ro(list);
        let file_name = self.vm.intern_static(self.file_name);
        self.vm
            .set_heap_property(result, "dbg-file", Value::StringConst(file_name));
        self.vm.set_heap_property(result, "dbg-line", line.into());
        self.vm.set_heap_property(result, "dbg-col", column.into());
        result
    }

    fn escape_to_char(&mut self) -> Result<char, ReadError> {
        if let (Some(ch1), Some(ch2)) = (self.chars().next(), self.chars().next()) {
            let ch_n: u8 = (char_to_hex_num(&ch1?)? * 16) + (char_to_hex_num(&ch2?)?);
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

    fn read_doc_string(&mut self, buffer: &mut String) -> Result<Value, ReadError> {
        self.read_string(buffer, true)
    }

    fn consume_line_comment(&mut self) -> Result<(), ReadError> {
        for ch in self.chars() {
            let ch = ch?;
            if ch == "\n" {
                return Ok(());
            }
        }
        Ok(())
    }

    fn consume_block_comment(&mut self) -> Result<(), ReadError> {
        let mut depth = 1;
        let mut last_ch = Cow::Borrowed(" ");
        while let Some(ch) = self.chars().next() {
            let ch = ch?;
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
        Ok(())
    }

    fn do_char(
        &mut self,
        buffer: &mut String,
        read_table_term: &HashMap<&'static str, Value>,
    ) -> Result<Value, ReadError> {
        if let Some(ch) = self.chars().next() {
            let ch = ch?;
            if self.peek_is("{")? || !end_symbol(self.chars().peek(), read_table_term)? {
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
                        self.read_symbol(buffer, true, false, read_table_term)?;
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
            Ok(self.vm.alloc_char(&ch))
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
            let ch = ch?;
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
                match pch {
                    Ok(pch) => {
                        if !has_bracket && is_whitespace(pch) {
                            return finish(char_u32);
                        }
                    }
                    Err(e) => return Err(e.into()),
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

    fn end_string(&mut self, ch: &str, doc_string: bool) -> Result<bool, ReadError> {
        if doc_string {
            if ch == "%" && self.peek_is("#")? {
                self.chars().next();
                return Ok(true);
            }
        } else if ch == "\"" {
            return Ok(true);
        }
        Ok(false)
    }

    fn read_string(&mut self, symbol: &mut String, doc_string: bool) -> Result<Value, ReadError> {
        symbol.clear();
        let mut last_ch_escape = false;
        let mut args = vec![];
        let line = self.line() as u32;
        let column = self.column() as u32;

        while let Some(ch) = self.chars().next() {
            let ch = ch?;
            if last_ch_escape {
                match &*ch {
                    "n" => symbol.push('\n'),
                    "r" => symbol.push('\r'),
                    "t" => symbol.push('\t'),
                    "x" => {
                        let res = self.escape_to_char()?;
                        symbol.push(res);
                    }
                    "u" => {
                        let res = self.read_utf_scalar()?;
                        symbol.push(res);
                    }
                    _ => {
                        symbol.push_str(&ch);
                    }
                }
                last_ch_escape = false;
            } else {
                if self.end_string(&ch, doc_string)? {
                    break;
                }
                if ch == "{" && !doc_string {
                    if args.is_empty() {
                        args.push(Value::Symbol(self.vm.intern("str")));
                    }
                    if !symbol.is_empty() {
                        args.push(Value::StringConst(self.vm.intern(symbol)));
                    }
                    if let Some(next_arg) = self.read_inner(symbol, false, ReadReturn::None)? {
                        args.push(next_arg);
                    }
                    symbol.clear();
                    let ch = self.chars().next();
                    if let Some(ch) = ch {
                        if ch? != "}" {
                            return Err(ReadError {
                                reason: "invalid str format, missing '}'".to_string(),
                            });
                        }
                    }
                } else if ch == "\\" {
                    last_ch_escape = true;
                } else {
                    symbol.push_str(&ch);
                }
            }
        }
        if args.is_empty() {
            Ok(Value::StringConst(self.vm.intern(symbol)))
        } else {
            if !symbol.is_empty() {
                args.push(Value::StringConst(self.vm.intern(symbol)));
                symbol.clear();
            }
            Ok(self.alloc_list(args, line, column))
        }
    }

    fn read_string_literal<'sym>(
        &mut self,
        symbol: &'sym mut String,
    ) -> Result<&'sym mut String, ReadError> {
        symbol.clear();
        let end_ch = if let Some(ch) = self.chars().next() {
            ch?
        } else {
            return Err(ReadError {
                reason: "Unexpected stream end on string literal".to_string(),
            });
        };

        while let Some(ch) = self.chars().next() {
            let ch = ch?;
            if ch == end_ch && self.peek_is("\"")? {
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
                Ok(v) => v.into(),
                Err(_) => {
                    let potential_float: Result<f64, ParseFloatError> = num_str.parse();
                    match potential_float {
                        Ok(f) => f.into(),
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
    ) -> Result<bool, ReadError> {
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
        if end_symbol(self.chars().peek(), read_table_term)? && !for_ch {
            return Ok(buffer.len() == 1 && is_digit(&buffer[..]));
        }
        let mut next_ch = self.chars().next();
        while let Some(ch) = next_ch {
            let ch = ch?;
            if ch == "\\" && self.chars().peek().is_some() && !for_ch {
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
                buffer.push_str(&next_ch?);
                push_next = false;
            } else if ch == "." && self.peek_is("~")? {
                buffer.push_str(&ch);
            } else if end_symbol(self.chars().peek(), read_table_term)? {
                break;
            }
            next_ch = self.chars().next();
        }
        Ok(is_number)
    }

    fn peek_is(&mut self, test: &str) -> Result<bool, ReadError> {
        if let Some(pch) = self.chars().peek() {
            match pch {
                Ok(pch) => {
                    if test == &**pch {
                        Ok(true)
                    } else {
                        Ok(false)
                    }
                }
                Err(e) => Err(e.into()),
            }
        } else {
            Ok(false)
        }
    }

    fn consume_whitespace(&mut self) -> Result<(), ReadError> {
        // Consume whitespace.
        loop {
            match self.chars().peek() {
                Some(Ok(ch)) => {
                    if is_whitespace(ch) {
                        self.chars().next();
                    } else {
                        break;
                    }
                }
                Some(Err(e)) => return Err(e.into()),
                None => break,
            }
        }
        Ok(())
    }

    fn read_num_radix(
        &mut self,
        buffer: &mut String,
        radix: u32,
        read_table_term: &HashMap<&'static str, Value>,
    ) -> Result<i64, ReadError> {
        buffer.clear();
        self.read_symbol(buffer, true, true, read_table_term)?;
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
    ) -> Result<Value, ReadError> {
        let mut v: Vec<Value> = Vec::new();
        let mut cont = true;
        let make_vec = self.vm.intern("vec");
        v.push(Value::Symbol(make_vec));
        let line = self.line() as u32;
        let column = self.column() as u32;

        let close_intern = self.vm.intern("]");
        while cont {
            let exp = match self.read_inner(buffer, in_back_quote, ReadReturn::Vector) {
                Ok(exp) => {
                    if let Some(Value::Symbol(i)) = &exp {
                        if *i == close_intern {
                            return Ok(self.alloc_list(v, line, column));
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

    fn read_map(&mut self, buffer: &mut String, in_back_quote: bool) -> Result<Value, ReadError> {
        //let mut map: HashMap<Value, Value> = HashMap::new();
        let mut cont = true;

        let close_intern = self.vm.intern("}");
        let make_hash = self.vm.specials().make_hash;
        let mut list = Vec::new();
        list.push(Value::Symbol(make_hash));
        let line = self.line() as u32;
        let column = self.column() as u32;
        while cont {
            let key = match self.read_inner(buffer, in_back_quote, ReadReturn::Map) {
                Ok(exp) => {
                    if let Some(Value::Symbol(i)) = &exp {
                        if *i == close_intern {
                            return Ok(self.alloc_list(list, line, column));
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
                list.push(key);
                list.push(val);
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
            let car = if let Some(car) = v.first() {
                *car
            } else {
                return false;
            };
            is_splice(self.vm, car)
        } else {
            false
        }
    }

    fn read_shell_list(
        &mut self,
        buffer: &mut String,
        in_back_quote: bool,
    ) -> Result<Value, ReadError> {
        let mut closed = false;
        let mut list = Vec::new();
        list.push(Value::Symbol(self.vm().intern_static("sh")));
        let line = self.line() as u32;
        let column = self.column() as u32;
        let mut open_parens: u32 = 0;
        buffer.clear();

        while let Some(ch) = self.chars().next() {
            let ch = ch?;
            match &*ch {
                ")" if open_parens == 0 => {
                    closed = true;
                    if !buffer.is_empty() {
                        list.push(self.vm().alloc_string(buffer.clone()));
                        buffer.clear();
                    }
                    break;
                }
                ")" => open_parens -= 1,
                "(" => open_parens += 1,
                ":" => {
                    if !buffer.is_empty() {
                        list.push(self.vm().alloc_string(buffer.clone()));
                    }
                    if let Some(Value::Symbol(i)) =
                        self.read_inner(buffer, in_back_quote, ReadReturn::List)?
                    {
                        list.push(Value::Keyword(i));
                        buffer.clear();
                    } else {
                        return Err(ReadError {
                            reason: "Invalid keyword".to_string(),
                        });
                    }
                }
                "~" => {
                    if !buffer.is_empty() {
                        list.push(self.vm().alloc_string(buffer.clone()));
                    }
                    if let Some(exp) = self.read_inner(buffer, in_back_quote, ReadReturn::List)? {
                        list.push(exp);
                        buffer.clear();
                    }
                }
                _ => buffer.push_str(&ch),
            }
        }
        if !closed {
            Err(ReadError {
                reason: "Unclosed list".to_string(),
            })
        } else if list.is_empty() {
            Err(ReadError {
                reason: "Empty shell command".to_string(),
            })
        } else {
            Ok(self.alloc_list(list, line, column))
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
        let line = self.line() as u32;
        let column = self.column() as u32;

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
                    last = self.alloc_pair(*v, last, line, column);
                }
                Ok(last)
            } else {
                Ok(Value::Nil)
            }
        } else if list.is_empty() {
            Ok(Value::Nil)
        } else {
            Ok(self.alloc_list(list, line, column))
        }
    }

    fn parse_get(&mut self, buffer: &str) -> Option<Value> {
        if buffer.contains('.') {
            let i_get = self.vm.intern("get");
            let mut vals = vec![Value::Symbol(i_get)];
            let line = self.line() as u32;
            let column = self.column() as u32;
            let parts = buffer.split('.');
            let mut i = 0;
            for p in parts {
                if !p.is_empty() {
                    if i != 0 && i % 2 == 0 {
                        let inner = self.alloc_list(vals, line, column);
                        vals = vec![Value::Symbol(i_get)];
                        vals.push(inner);
                    }
                    if p.starts_with(':') && p.len() > 1 {
                        let i_p = self.vm.intern(&p[1..]);
                        vals.push(Value::Keyword(i_p));
                    } else if p.starts_with('~') && p.len() > 1 {
                        let i_p = self.vm.intern(&p[1..]);
                        vals.push(Value::Symbol(i_p));
                    } else if let Ok(num) = p.parse::<i32>() {
                        vals.push(num.into());
                    } else if i % 2 == 0 {
                        let i_p = self.vm.intern(p);
                        vals.push(Value::Symbol(i_p));
                    } else {
                        let i_p = self.vm.intern(p);
                        let i_quote = self.vm.intern_static("quote");
                        vals.push(self.alloc_list(
                            vec![Value::Symbol(i_quote), Value::Symbol(i_p)],
                            line,
                            column,
                        ));
                    }
                    i += 1;
                }
            }
            if i > 1 {
                let res = self.alloc_list(vals, line, column);
                Some(res)
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Reader for the '#' builtin reader macro.
    ///
    /// If it returns Ok(Some(...)) then that value should be returned while Ok(None) should be ignored.
    fn hash_macro(
        &mut self,
        buffer: &mut String,
        in_back_quote: bool,
        return_close: ReadReturn,
        read_table_term: &HashMap<&'static str, Value>,
    ) -> Result<Option<Value>, ReadError> {
        let next = self.chars().next();
        let (line, column) = (self.line(), self.column());
        if let Some(Ok(peek_ch)) = &next {
            match &**peek_ch {
                "|" => {
                    self.consume_block_comment()?;
                    Ok(None)
                }
                "!" => {
                    // This is an alternate line comment for shebang in a script.
                    self.consume_line_comment()?;
                    Ok(None)
                }
                "%" => {
                    let line = self.line() as u32;
                    let column = self.column() as u32;
                    match self.read_doc_string(buffer) {
                        Ok(s) => {
                            let doc_sym = Value::Symbol(self.vm.intern("doc-string"));
                            let doc_string = s;
                            let list = self.alloc_list(vec![doc_sym, doc_string], line, column);
                            Ok(Some(list))
                        }
                        Err(e) => Err(e),
                    }
                }
                "<" => {
                    let reason =
                        format!("Found an unreadable token: line {}, col: {}", line, column);
                    Err(ReadError { reason })
                }
                "t" => Ok(Some(Value::True)),
                "f" => Ok(Some(Value::False)),
                "\"" => match self.read_string_literal(buffer) {
                    Ok(s) => Ok(Some(Value::StringConst(self.vm.intern(s)))),
                    Err(e) => Err(e),
                },
                //"." => {
                //    return prep_reader_macro(environment, chars, "reader-macro-dot", ".");
                //}
                // Read an octal int
                "o" => {
                    let exp = self.read_num_radix(buffer, 8, read_table_term)?;
                    Ok(Some(exp.into()))
                }
                // Read a hex int
                "x" => {
                    let exp = self.read_num_radix(buffer, 16, read_table_term)?;
                    Ok(Some(exp.into()))
                }
                // Read a binary int
                "b" => {
                    let exp = self.read_num_radix(buffer, 2, read_table_term)?;
                    Ok(Some(exp.into()))
                }
                ";" => {
                    match self.read_inner(buffer, in_back_quote, ReadReturn::None) {
                        Ok(_) => {
                            // Consumed and threw away one form so return the next.
                            self.read_inner(buffer, in_back_quote, return_close)
                        }
                        Err(err) => Err(err),
                    }
                }
                _ => {
                    let reason = format!(
                        "Found # with invalid char {}: line {}, col: {}",
                        peek_ch, line, column
                    );
                    Err(ReadError { reason })
                }
            }
        } else {
            Err(ReadError {
                reason: "Floating '#'".to_string(),
            })
        }
    }

    fn read_inner(
        &mut self,
        buffer: &mut String,
        in_back_quote: bool,
        return_close: ReadReturn,
    ) -> Result<Option<Value>, ReadError> {
        self.consume_whitespace()?;
        let read_table_term: HashMap<&'static str, Value> = HashMap::new();
        let _read_table: HashMap<&'static str, Chunk> = HashMap::new();

        let i_quote = self.vm.intern("quote");
        let i_backquote = self.vm.intern("back-quote");
        while let Some(ch) = self.chars().next() {
            let ch = ch?;
            let line = self.line() as u32;
            let column = self.column() as u32;
            match &*ch {
                "\"" => {
                    match self.read_string(buffer, false) {
                        Ok(s) => return Ok(Some(s)),
                        Err(e) => return Err(e),
                    };
                }
                "'" => match self.read_inner(buffer, in_back_quote, ReadReturn::None) {
                    Ok(Some(exp)) => {
                        let cdr = self.alloc_pair(exp, Value::Nil, line, column);
                        let qlist = self.alloc_pair(Value::Symbol(i_quote), cdr, line, column);
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
                        let cdr = self.alloc_pair(exp, Value::Nil, line, column);
                        let qlist = self.alloc_pair(Value::Symbol(i_backquote), cdr, line, column);
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
                    let sym = if self.peek_is("@")? {
                        self.chars().next();
                        Value::Symbol(self.vm.intern("unquote-splice"))
                    } else if self.peek_is(".")? {
                        self.chars().next();
                        Value::Symbol(self.vm.intern("unquote-splice!"))
                    } else {
                        Value::Symbol(self.vm.intern("unquote"))
                    };
                    match self.read_inner(buffer, in_back_quote, ReadReturn::None) {
                        Ok(Some(exp)) => {
                            let cdr = self.alloc_pair(exp, Value::Nil, line, column);
                            return Ok(Some(self.alloc_pair(sym, cdr, line, column)));
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
                    if let Some(v) =
                        self.hash_macro(buffer, in_back_quote, return_close, &read_table_term)?
                    {
                        return Ok(Some(v));
                    }
                }
                "$" if self.peek_is("(")? => {
                    self.chars().next();
                    let exp = self.read_shell_list(buffer, in_back_quote)?;
                    return Ok(Some(exp));
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
                    return Ok(Some(self.read_map(buffer, in_back_quote)?));
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
                    return Ok(Some(self.read_vector(buffer, in_back_quote)?));
                }
                "]" if matches!(return_close, ReadReturn::Vector) => {
                    return Ok(Some(Value::Symbol(self.vm.intern("]"))));
                }
                "]" => {
                    let reason =
                        format!("Unexpected ']': line {} col {}", self.line(), self.column());
                    return Err(ReadError { reason });
                }
                ";" => self.consume_line_comment()?,
                _ => {
                    buffer.clear();
                    buffer.push_str(&ch);
                    let is_number = self.read_symbol(buffer, false, false, &read_table_term)?;
                    if is_number {
                        return Ok(Some(self.do_atom(buffer, is_number)));
                    } else if let Some(get) = self.parse_get(buffer) {
                        return Ok(Some(get));
                    } else {
                        return Ok(Some(self.do_atom(buffer, is_number)));
                    }
                }
            }
            self.consume_whitespace()?;
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
        assert!(tokens[5] == "Int:5");
        assert!(tokens[6] == "Int:6");
        assert!(tokens[7] == "]");

        let tokens = tokenize(&mut vm, "one, two,three ,,,, \"four\" 5 , 6,");
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "Symbol:one");
        assert!(tokens[2] == "Symbol:two");
        assert!(tokens[3] == "Symbol:three");
        assert!(tokens[4] == "String:\"four\"");
        assert!(tokens[5] == "Int:5");
        assert!(tokens[6] == "Int:6");
        assert!(tokens[7] == "]");

        let tokens = tokenize(&mut vm, "(1 2 3)");
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize(&mut vm, "  (  1    2\t3   )  ");
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize(&mut vm, "[\\A 2 3]");
        assert!(tokens.len() == 6);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:vec");
        assert!(tokens[2] == "Char:\\A");
        assert!(tokens[3] == "Int:2");
        assert!(tokens[4] == "Int:3");
        assert!(tokens[5] == ")");
        let tokens = tokenize(&mut vm, "[\\  2 3]");
        assert!(tokens.len() == 6);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:vec");
        assert!(tokens[2] == "Char:\\ ");
        assert!(tokens[3] == "Int:2");
        assert!(tokens[4] == "Int:3");
        assert!(tokens[5] == ")");
        let tokens = tokenize(&mut vm, "'((1 2 (3)))");
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
        let tokens = tokenize(&mut vm, "'((1 2 #;4 #;\"gone\"(3#;(x)) #;'(1 2 3)))");
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
        assert!(tokens[3] == "Int:1");
        assert!(tokens[4] == "Int:2");
        assert!(tokens[5] == "Int:3");
        assert!(tokens[6] == ")");
        assert!(tokens[7] == ")");
        tokenize_err(&mut vm, "'(1 2 ~3)");
        tokenize_err(&mut vm, "'(1 2 ~@3)");
        let tokens = tokenize(&mut vm, "`(1 2 ~3)");
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
        let tokens = tokenize(&mut vm, "`(1 2 ~@3)");
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
        let tokens = tokenize(&mut vm, "`(1 2 ~.3)");
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
        let tokens = tokenize(&mut vm, "`(1 `2 ~@3)");
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
        let tokens = tokenize(&mut vm, "`(1 `(2 ~x) ~@3)");
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
        let tokens = tokenize(&mut vm, "(one 2 3.0 \"four\" \\B #t nil 3.5 ())");
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:one");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Float:3");
        assert!(tokens[4] == "String:\"four\"");
        assert!(tokens[5] == "Char:\\B");
        assert!(tokens[6] == "True:true");
        assert!(tokens[7] == "nil");
        assert!(tokens[8] == "Float:3.5");
        assert!(tokens[9] == "nil");
        assert!(tokens[10] == ")");

        let tokens = tokenize(&mut vm, "[one 2 3.0 \"four\" \\B #t nil 3.5 ()]");
        assert!(tokens.len() == 12);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:vec");
        assert!(tokens[2] == "Symbol:one");
        assert!(tokens[3] == "Int:2");
        assert!(tokens[4] == "Float:3");
        assert!(tokens[5] == "String:\"four\"");
        assert!(tokens[6] == "Char:\\B");
        assert!(tokens[7] == "True:true");
        assert!(tokens[8] == "nil");
        assert!(tokens[9] == "Float:3.5");
        assert!(tokens[10] == "nil");
        assert!(tokens[11] == ")");

        let tokens = tokenize(&mut vm, "one 2 3.0 \"four\" \\B #t nil 3.5 ()");
        assert!(tokens.len() == 11);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "Symbol:one");
        assert!(tokens[2] == "Int:2");
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
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == ")");
        let tokens = tokenize_wrap(&mut vm, "(1 2 3)");
        assert!(tokens.len() == 7);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "(");
        assert!(tokens[2] == "Int:1");
        assert!(tokens[3] == "Int:2");
        assert!(tokens[4] == "Int:3");
        assert!(tokens[5] == ")");
        assert!(tokens[6] == "]");

        let tokens = tokenize(&mut vm, "1 2 3");
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == "]");
        let tokens = tokenize_wrap(&mut vm, "1 2 3");
        assert!(tokens.len() == 5);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "Int:1");
        assert!(tokens[2] == "Int:2");
        assert!(tokens[3] == "Int:3");
        assert!(tokens[4] == "]");

        let tokens = tokenize(&mut vm, "(1 2 3) (4 5 6)");
        assert!(tokens.len() == 12);
        assert!(tokens[0] == "[");
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
        assert!(tokens[11] == "]");
        let tokens = tokenize_wrap(&mut vm, "(1 2 3) (4 5 6)");
        assert!(tokens.len() == 12);
        assert!(tokens[0] == "[");
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
        assert!(tokens[11] == "]");

        let tokens = tokenize(&mut vm, "'(1 2 3)");
        assert!(tokens.len() == 8);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:quote");
        assert!(tokens[2] == "(");
        assert!(tokens[3] == "Int:1");
        assert!(tokens[4] == "Int:2");
        assert!(tokens[5] == "Int:3");
        assert!(tokens[6] == ")");
        assert!(tokens[7] == ")");
        let tokens = tokenize_wrap(&mut vm, "'(1 2 3)");
        assert!(tokens.len() == 10);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "(");
        assert!(tokens[2] == "Symbol:quote");
        assert!(tokens[3] == "(");
        assert!(tokens[4] == "Int:1");
        assert!(tokens[5] == "Int:2");
        assert!(tokens[6] == "Int:3");
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
        assert!(tokens[5] == "Int:5");
        assert!(tokens[6] == "Int:6");
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
        assert!(tokens[5] == "Int:5");
        assert!(tokens[6] == "Int:6");
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
        assert!(tokens[4] == "Int:5");
        assert!(tokens[5] == "Int:6");
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
        assert!(tokens[4] == "Int:5");
        assert!(tokens[5] == "Int:6");
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
        let input = "2300.0 23_000.0 23e10 23e+5 23e-4 23e-+5 23e-5e+4 23.123";
        let tokens = tokenize(&mut vm, input);
        assert!(tokens.len() == 10);
        assert!(tokens[0] == "[");
        assert!(tokens[1] == "Float:2300");
        assert!(tokens[2] == "Float:23000");
        assert!(tokens[3] == "Float:230000000000");
        assert!(tokens[4] == "Float:2300000");
        assert!(tokens[5] == "Float:0.0023");
        assert!(tokens[6] == "Symbol:23e-+5");
        assert!(tokens[7] == "Symbol:23e-5e+4");
        assert!(tokens[8] == "Float:23.123");
        assert!(tokens[9] == "]");
    }

    #[test]
    fn test_doc_string() {
        let mut vm = build_def_vm();
        let input = "#% Doc string! %#";
        let tokens = tokenize(&mut vm, input);
        assert!(tokens.len() == 4);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:doc-string");
        assert!(tokens[2] == "String:\" Doc string! \"");
        assert!(tokens[3] == ")");
        let input = "#%Doc string!%#";
        let tokens = tokenize(&mut vm, input);
        assert!(tokens.len() == 4);
        assert!(tokens[0] == "(");
        assert!(tokens[1] == "Symbol:doc-string");
        assert!(tokens[2] == "String:\"Doc string!\"");
        assert!(tokens[3] == ")");
        let input = "#%\
         Doc string!\
         %#\
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
