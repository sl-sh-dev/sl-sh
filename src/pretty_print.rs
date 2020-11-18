use std::fmt;
use std::io::{self, Write};

use crate::builtins_util::is_proper_list;
use crate::environment::*;
use crate::types::*;

fn params_to_string(params: &[&'static str]) -> String {
    let mut pstr = "(".to_string();
    let mut first = true;
    for p in params {
        if first {
            first = false;
        } else {
            pstr.push(' ');
        }
        pstr.push_str(p);
    }
    pstr.push(')');
    pstr
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn list_out(res: &mut String, itr: &mut dyn Iterator<Item = Expression>) {
            let mut first = true;
            let mut last_exp: Expression = Expression::make_nil();
            for p in itr {
                if !first {
                    if let ExpEnum::Symbol(sym, _) = &last_exp.get().data {
                        if sym != &"," && sym != &",@" {
                            res.push_str(" ");
                        }
                    } else {
                        res.push_str(" ");
                    }
                } else {
                    first = false;
                }
                res.push_str(&p.to_string());
                last_exp = p;
            }
        }

        match &self.get().data {
            ExpEnum::True => write!(f, "true"),
            ExpEnum::Float(n) => write!(f, "{}", n),
            ExpEnum::Int(i) => write!(f, "{}", i),
            ExpEnum::Symbol(s, _) => write!(f, "{}", s),
            ExpEnum::String(s, _) => write!(f, "\"{}\"", s),
            ExpEnum::Char(c) => write!(f, "#\\{}", c),
            ExpEnum::CodePoint(c) => write!(f, "#\\{}", c),
            ExpEnum::Lambda(l) => {
                let body: Expression = l.body.clone().into();
                write!(
                    f,
                    "(fn {} {})",
                    params_to_string(&l.params),
                    body.to_string()
                )
            }
            ExpEnum::Macro(m) => {
                let body: Expression = m.body.clone().into();
                write!(
                    f,
                    "(macro {} {})",
                    params_to_string(&m.params),
                    body.to_string()
                )
            }
            ExpEnum::Process(ProcessState::Running(pid)) => write!(f, "#<PID: {} Running>", pid),
            ExpEnum::Process(ProcessState::Over(pid, exit_status)) => write!(
                f,
                "#<PID: {}, EXIT STATUS: {},  Complete>",
                pid, exit_status
            ),
            ExpEnum::Function(_) => write!(f, "#<Function>"),
            ExpEnum::Vector(list) => {
                let mut res = String::new();
                res.push_str("#(");
                let mut ib = Box::new(ListIter::new_list(&list));
                list_out(&mut res, &mut ib);
                res.push(')');
                write!(f, "{}", res)
            }
            ExpEnum::Values(v) => {
                if v.is_empty() {
                    f.write_str("nil")
                } else {
                    let v: Expression = (&v[0]).into();
                    v.fmt(f)
                }
            }
            ExpEnum::Pair(e1, e2) => {
                let e1: Expression = e1.into();
                let e2: Expression = e2.into();
                if is_proper_list(&self) {
                    match &e1.get().data {
                        ExpEnum::Symbol(sym, _) if sym == &"quote" => {
                            f.write_str("'")?;
                            // This will be a two element list or something is wrong...
                            if let ExpEnum::Pair(a2, _) = &e2.get().data {
                                let a2: Expression = a2.into();
                                f.write_str(&a2.to_string())
                            } else {
                                f.write_str(&e2.to_string())
                            }
                        }
                        ExpEnum::Symbol(sym, _) if sym == &"bquote" => {
                            f.write_str("`")?;
                            // This will be a two element list or something is wrong...
                            if let ExpEnum::Pair(a2, _) = &e2.get().data {
                                let a2: Expression = a2.into();
                                f.write_str(&a2.to_string())
                            } else {
                                f.write_str(&e2.to_string())
                            }
                        }
                        _ => {
                            let mut res = String::new();
                            res.push_str("(");
                            list_out(&mut res, &mut self.iter());
                            res.push(')');
                            write!(f, "{}", res)
                        }
                    }
                } else {
                    let e1: Expression = e1;
                    let e2: Expression = e2;
                    write!(f, "({} . {})", e1.to_string(), e2.to_string())
                }
            }
            ExpEnum::Nil => f.write_str("nil"),
            ExpEnum::HashMap(map) => {
                let mut res = String::new();
                res.push_str("(make-hash (");
                for (key, val) in map.iter() {
                    let val: Expression = val.into();
                    res.push_str(&format!("({} . {})", key, val));
                }
                res.push_str("))");
                write!(f, "{}", res)
            }
            ExpEnum::File(file) => match &*file.borrow() {
                FileState::Stdout => write!(f, "#<STDOUT>"),
                FileState::Stderr => write!(f, "#<STDERR>"),
                FileState::Stdin => write!(f, "#<STDIN>"),
                FileState::Closed => write!(f, "#<CLOSED FILE>"),
                FileState::Read(_file, _) => write!(f, "#<READ FILE>"),
                FileState::ReadBinary(_file) => write!(f, "#<READ (BIN) FILE>"),
                FileState::Write(_file) => write!(f, "#<WRITE FILE>"),
            },
            ExpEnum::LazyFn(_, args) => {
                let mut res = String::new();
                res.push_str("#<LAZYFN<");
                list_out(&mut res, &mut Box::new(ListIter::new_list(args)));
                res.push_str(">>");
                write!(f, "{}", res)
            }
            ExpEnum::Wrapper(exp) => {
                let exp: Expression = exp.into();
                exp.fmt(f)
            }
            ExpEnum::DeclareDef => write!(f, "#<Function>"),
            ExpEnum::DeclareVar => write!(f, "#<Function>"),
            ExpEnum::DeclareFn => write!(f, "#<Function>"),
            ExpEnum::Undefined => write!(f, "#<Undefined>"), // XXX maybe panic here instead?
        }
    }
}

fn pretty_print_int(
    expression: &Expression,
    environment: &mut Environment,
    indent: usize,
    writer: &mut dyn Write,
) -> Result<(), LispError> {
    fn init_space(indent: usize, writer: &mut dyn Write) -> Result<(), LispError> {
        let mut i = 0;
        if indent > 0 {
            writer.write_all(b"\n")?;
        }
        while i < indent {
            writer.write_all(b"    ")?;
            i += 1;
        }
        Ok(())
    }
    match &expression.get().data {
        ExpEnum::Vector(list) => {
            init_space(indent, writer)?;
            let a_str = expression.to_string();
            if a_str.len() < 40 || a_str.starts_with('\'') || a_str.starts_with('`') {
                writer.write_all(a_str.as_bytes())?;
            } else {
                writer.write_all(b"#(")?;
                let mut first = true;
                for exp in list.iter() {
                    let exp: Expression = exp.into();
                    if !first {
                        writer.write_all(b" ")?;
                    } else {
                        first = false;
                    }
                    pretty_print_int(&exp, environment, indent + 1, writer)?;
                }
                writer.write_all(b")")?;
            }
        }
        ExpEnum::Values(v) => {
            if v.is_empty() {
                write!(writer, "nil")?;
            } else {
                let v: Expression = (&v[0]).into();
                pretty_print_int(&v, environment, indent, writer)?;
            }
        }
        ExpEnum::Pair(e1, e2) => {
            init_space(indent, writer)?;
            let a_str = expression.to_string();
            if a_str.len() < 40 || a_str.starts_with('\'') || a_str.starts_with('`') {
                writer.write_all(a_str.as_bytes())?;
            } else if is_proper_list(&expression) {
                writer.write_all(b"(")?;
                let mut first = true;
                let mut last_p: Expression = Expression::make_nil();
                for p in expression.iter() {
                    if !first {
                        if let ExpEnum::Symbol(sym, _) = &last_p.get().data {
                            if sym != &"," && sym != &",@" {
                                writer.write_all(b" ")?;
                            }
                        } else {
                            writer.write_all(b" ")?;
                        }
                    } else {
                        first = false;
                    }
                    pretty_print_int(&p, environment, indent + 1, writer)?;
                    last_p = p; //&p.get().data;
                }
                writer.write_all(b")")?;
            } else {
                let e1: Expression = e1.into();
                let e2: Expression = e2.into();
                write!(writer, "({} . {})", e1.to_string(), e2.to_string())?;
            }
        }
        ExpEnum::Nil => write!(writer, "nil")?,
        ExpEnum::HashMap(map) => {
            init_space(indent, writer)?;
            let a_str = expression.to_string();
            if a_str.len() < 40 {
                writer.write_all(a_str.as_bytes())?;
            } else {
                writer.write_all(b"(make-hash (")?;
                for (key, val) in map.iter() {
                    let val: Expression = val.into();
                    init_space(indent + 1, writer)?;
                    write!(writer, "({} . {})", key, val)?;
                }
                write!(writer, "))")?;
            }
        }
        ExpEnum::String(_, _) => {
            write!(writer, "{}", expression.to_string())?;
        }
        ExpEnum::Char(_c) => {
            write!(writer, "{}", expression.to_string())?;
        }
        ExpEnum::CodePoint(_c) => {
            write!(writer, "{}", expression.to_string())?;
        }
        ExpEnum::Lambda(l) => {
            let body: Expression = l.body.clone().into();
            write!(writer, "(fn {}", params_to_string(&l.params))?;
            pretty_print_int(&body, environment, indent + 1, writer)?;
            writer.write_all(b")")?;
        }
        ExpEnum::Macro(m) => {
            let body: Expression = m.body.clone().into();
            write!(writer, "(macro {}", params_to_string(&m.params))?;
            pretty_print_int(&body, environment, indent + 1, writer)?;
            writer.write_all(b")")?;
        }
        ExpEnum::Wrapper(exp) => {
            let exp: Expression = exp.into();
            pretty_print_int(&exp, environment, indent, writer)?;
        }
        ExpEnum::True => expression.writef(environment, writer)?,
        ExpEnum::Float(_) => expression.writef(environment, writer)?,
        ExpEnum::Int(_) => expression.writef(environment, writer)?,
        ExpEnum::Symbol(_, _) => expression.writef(environment, writer)?,
        ExpEnum::Function(_) => expression.writef(environment, writer)?,
        ExpEnum::LazyFn(_, _) => expression.writef(environment, writer)?,
        ExpEnum::Process(_) => expression.writef(environment, writer)?,
        ExpEnum::File(_) => expression.writef(environment, writer)?,
        ExpEnum::DeclareDef => expression.writef(environment, writer)?,
        ExpEnum::DeclareVar => expression.writef(environment, writer)?,
        ExpEnum::DeclareFn => expression.writef(environment, writer)?,
        ExpEnum::Undefined => expression.writef(environment, writer)?,
    }
    Ok(())
}

pub fn pretty_printf(
    expression: &Expression,
    environment: &mut Environment,
    writer: &mut dyn Write,
) -> Result<(), LispError> {
    pretty_print_int(expression, environment, 0, writer)
}

pub fn pretty_print(
    expression: &Expression,
    environment: &mut Environment,
) -> Result<(), LispError> {
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    pretty_print_int(expression, environment, 0, &mut handle)
}
