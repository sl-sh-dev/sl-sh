use slvm::error::*;
use slvm::interner::Interned;
use slvm::value::*;

use crate::compile::*;
use crate::pass1::pass1;
use compile_state::state::*;

macro_rules! is_tag {
    ($vm:expr, $exp:expr, $form:expr) => {{
        match $exp {
            Value::Pair(_) | Value::List(_, _) => {
                let (car, _) = $exp.get_pair($vm).expect("List/Pair not a List/Pair?");
                if car.is_symbol($form) {
                    return true;
                }
            }
            Value::Vector(handle) => {
                let v = $vm.get_vector(handle);
                if let Some(car) = v.get(0) {
                    if car.is_symbol($form) {
                        return true;
                    }
                }
            }
            _ => {}
        }
        false
    }};
}

macro_rules! get_data {
    ($vm:expr, $exp:expr) => {{
        match $exp {
            Value::Pair(_) | Value::List(_, _) => {
                let (_, cdr) = $exp.get_pair($vm).expect("List/Pair not a List/Pair?");
                if let Value::Pair(handle) | Value::List(handle, _) = cdr {
                    let (ncar, ncdr) = cdr.get_pair($vm).expect("List/Pair not a List/Pair?");
                    if ncdr.is_nil() {
                        return Ok(ncar);
                    } else {
                        let (line, col) = (
                            $vm.get_heap_property(handle, "dbg-line"),
                            $vm.get_heap_property(handle, "dbg-col"),
                        );
                        if let (Some(Value::UInt(line)), Some(Value::UInt(col))) = (line, col) {
                            return Err(VMError::new_compile(format!(
                                "Invalid tag at {}:{}, takes one expression.",
                                line, col
                            )));
                        } else {
                            return Err(VMError::new_compile("Invalid tag, takes one expression."));
                        }
                    }
                }
            }
            Value::Vector(handle) => {
                let v = $vm.get_vector(handle);
                if v.len() != 2 {
                    return Err(VMError::new_compile("Invalid tag, takes one expression."));
                }
                return Ok(v[1]);
            }
            _ => {}
        }
        Err(VMError::new_compile("Invalid tag, takes one expression."))
    }};
}

pub struct Tag {
    backquote: Interned,
    unquote: Interned,
    splice: Interned,
    splice_bang: Interned,
}

impl Tag {
    pub fn new(vm: &mut SloshVm) -> Self {
        Tag {
            backquote: vm.intern("back-quote"),
            unquote: vm.intern("unquote"),
            splice: vm.intern("unquote-splice"),
            splice_bang: vm.intern("unquote-splice!"),
        }
    }

    pub fn is_backquote(&self, vm: &SloshVm, exp: Value) -> bool {
        is_tag!(vm, exp, self.backquote)
    }

    pub fn is_unquote(&self, vm: &SloshVm, exp: Value) -> bool {
        is_tag!(vm, exp, self.unquote)
    }

    pub fn is_splice(&self, vm: &SloshVm, exp: Value) -> bool {
        is_tag!(vm, exp, self.splice)
    }

    pub fn is_splice_bang(&self, vm: &SloshVm, exp: Value) -> bool {
        is_tag!(vm, exp, self.splice_bang)
    }

    pub fn data(vm: &SloshVm, exp: Value) -> VMResult<Value> {
        get_data!(vm, exp)
    }
}

fn quote(vm: &mut SloshVm, exp: Value) -> Value {
    let cdr = vm.alloc_pair_ro(exp, Value::Nil);
    let q_i = vm.intern_static("quote");
    vm.alloc_pair_ro(Value::Symbol(q_i), cdr)
}

fn list(vm: &mut SloshVm, exp: Value) -> Value {
    let cdr = vm.alloc_pair_ro(exp, Value::Nil);
    let q_i = vm.intern_static("list");
    vm.alloc_pair_ro(Value::Symbol(q_i), cdr)
}

fn vec(vm: &mut SloshVm, v: &[Value]) -> Value {
    let mut last_pair = Value::Nil;
    if !v.is_empty() {
        let mut i = v.len();
        while i > 0 {
            last_pair = vm.alloc_pair_ro(v[i - 1], last_pair);
            i -= 1;
        }
    }
    let q_i = vm.intern_static("vec");
    vm.alloc_pair_ro(Value::Symbol(q_i), last_pair)
}

fn list2(vm: &mut SloshVm, exp: Value) -> Value {
    let q_i = vm.intern_static("list");
    vm.alloc_pair_ro(Value::Symbol(q_i), exp)
}

fn append(vm: &mut SloshVm, exp1: Value, exp2: Value) -> Value {
    let cdr1 = vm.alloc_pair_ro(exp2, Value::Nil);
    let cdr2 = vm.alloc_pair_ro(exp1, cdr1);
    let q_i = vm.intern_static("list-append");
    vm.alloc_pair_ro(Value::Symbol(q_i), cdr2)
}

fn rewrap(vm: &mut SloshVm, exp: Value, sym: &'static str) -> Value {
    let cdr = vm.alloc_pair_ro(exp, Value::Nil);
    let q_i = vm.intern_static(sym);
    let car = quote(vm, Value::Symbol(q_i));
    let cdr = vm.alloc_pair_ro(car, cdr);
    list2(vm, cdr)
}

fn unquote(vm: &mut SloshVm, exp: Value) -> Value {
    rewrap(vm, exp, "unquote")
}

fn splice(vm: &mut SloshVm, exp: Value) -> Value {
    rewrap(vm, exp, "unquote-splice")
}

fn splice_bang(vm: &mut SloshVm, exp: Value) -> Value {
    rewrap(vm, exp, "unquote-splice!")
}

fn back_quote(vm: &mut SloshVm, exp: Value) -> Value {
    rewrap(vm, exp, "back-quote")
}

// Algorithm initially from
// https://3e8.org/pub/scheme/doc/Quasiquotation%20in%20Lisp%20(Bawden).pdf
fn qq_expand(vm: &mut SloshVm, exp: Value, line: u32, depth: u32) -> VMResult<Value> {
    let tag = Tag::new(vm);
    if tag.is_unquote(vm, exp) {
        if depth == 0 {
            Ok(Tag::data(vm, exp)?)
        } else {
            let expand = qq_expand(vm, Tag::data(vm, exp)?, line, depth - 1)?;
            Ok(unquote(vm, expand))
        }
    } else if tag.is_splice(vm, exp) {
        if depth == 0 {
            Err(VMError::new_compile(format!(
                ",@ not valid here: line {}",
                line
            )))
        } else {
            let expand = qq_expand(vm, Tag::data(vm, exp)?, line, depth - 1)?;
            Ok(splice(vm, expand))
        }
    } else if tag.is_splice_bang(vm, exp) {
        if depth == 0 {
            Err(VMError::new_compile(format!(
                ",. not valid here: line {}",
                line
            )))
        } else {
            let expand = qq_expand(vm, Tag::data(vm, exp)?, line, depth - 1)?;
            Ok(splice_bang(vm, expand))
        }
    } else if tag.is_backquote(vm, exp) {
        let inner = qq_expand(vm, Tag::data(vm, exp)?, line, depth + 1)?;
        Ok(back_quote(vm, inner))
    } else {
        match exp {
            Value::Pair(_) | Value::List(_, _) => {
                let (car, cdr) = exp.get_pair(vm).expect("Pair/List not a Pair or List?");
                let l1 = qq_expand_list(vm, car, line, depth)?;
                if cdr.is_nil() {
                    Ok(l1)
                } else {
                    let l2 = qq_expand(vm, cdr, line, depth)?;
                    Ok(append(vm, l1, l2))
                }
            }
            Value::Vector(handle) => {
                let vector = vm.get_vector(handle);
                let mut new_vec: Vec<Value> = vector.to_vec();
                for i in &mut new_vec {
                    *i = qq_expand(vm, *i, line, depth)?;
                }
                Ok(vec(vm, &new_vec[..]))
            }
            _ => Ok(quote(vm, exp)),
        }
    }
}

fn qq_expand_list(vm: &mut SloshVm, exp: Value, line: u32, depth: u32) -> VMResult<Value> {
    let tag = Tag::new(vm);
    if tag.is_unquote(vm, exp) {
        if depth == 0 {
            let data = Tag::data(vm, exp)?;
            Ok(list(vm, data))
        } else {
            let expand = qq_expand(vm, Tag::data(vm, exp)?, line, depth - 1)?;
            let inner = unquote(vm, expand);
            Ok(list(vm, inner))
        }
    } else if tag.is_splice(vm, exp) {
        if depth == 0 {
            Ok(Tag::data(vm, exp)?)
        } else {
            let expand = qq_expand(vm, Tag::data(vm, exp)?, line, depth - 1)?;
            let inner = splice(vm, expand);
            Ok(list(vm, inner))
        }
    } else if tag.is_splice_bang(vm, exp) {
        if depth == 0 {
            Ok(Tag::data(vm, exp)?)
        } else {
            let expand = qq_expand(vm, Tag::data(vm, exp)?, line, depth - 1)?;
            let inner = splice_bang(vm, expand);
            Ok(list(vm, inner))
        }
    } else if tag.is_backquote(vm, exp) {
        let inner = qq_expand(vm, Tag::data(vm, exp)?, line, depth + 1)?;
        let inner = back_quote(vm, inner);
        Ok(list(vm, inner))
    } else {
        match exp {
            Value::Pair(_) | Value::List(_, _) => {
                let (car, cdr) = exp.get_pair(vm).expect("List/Pair not a List/Pair?");
                let l1 = qq_expand_list(vm, car, line, depth)?;
                if cdr.is_nil() {
                    Ok(list(vm, l1))
                } else {
                    let l2 = qq_expand(vm, cdr, line, depth)?;
                    let app = append(vm, l1, l2);
                    Ok(list(vm, app))
                }
            }
            Value::Vector(handle) => {
                let vector = vm.get_vector(handle);
                let mut new_vec: Vec<Value> = vector.to_vec();
                for i in &mut new_vec {
                    *i = qq_expand(vm, *i, line, depth)?;
                }
                let vv = vec(vm, &new_vec[..]);
                Ok(list(vm, vv))
            }
            _ => {
                let q = quote(vm, exp);
                Ok(list(vm, q))
            }
        }
    }
}

pub fn backquote(
    env: &mut SloshVm,
    state: &mut CompileState,
    exp: Value,
    result: usize,
) -> VMResult<()> {
    env.pause_gc();
    let line = env.line_num();
    let result = qq_expand(env, exp, line, 0).and_then(|expand| {
        // XXX disable line numbering?
        pass1(env, state, expand).and_then(|_| compile(env, state, expand, result))
    });
    env.unpause_gc();
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ReadError, Reader};
    use slvm::RET;
    use std::sync::Arc;

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

    fn exec(vm: &mut SloshVm, input: &'static str) -> VMResult<Value> {
        let exp = read_test(vm, input);
        if let Ok(exp) = exp {
            let mut state = CompileState::new();
            compile(vm, &mut state, exp, 0)?;
            state.chunk.encode0(RET, Some(1))?;
            vm.execute(Arc::new(state.chunk))?;
            Ok(vm.stack()[0])
        } else {
            panic!("Got unexpected token error: {:?}", exp);
        }
    }

    #[test]
    fn test_quote() -> VMResult<()> {
        let mut vm = new_slosh_vm();
        let result = exec(&mut vm, "'(1 2 3)")?;
        let expected = read_test(&mut vm, "(1 2 3)").map_err(|e| VMError::new("read", e.reason))?;
        assert!(vm.is_equal_pair(expected, result)?.is_true());

        let result = exec(&mut vm, "'(x y z)")?;
        let expected = read_test(&mut vm, "(x y z)").map_err(|e| VMError::new("read", e.reason))?;
        assert!(vm.is_equal_pair(expected, result)?.is_true());

        let result = exec(&mut vm, "'x")?;
        let expected = read_test(&mut vm, "x").map_err(|e| VMError::new("read", e.reason))?;
        assert!(vm.is_equal_pair(expected, result)?.is_true());

        let result = exec(&mut vm, "'5")?;
        let expected = read_test(&mut vm, "5").map_err(|e| VMError::new("read", e.reason))?;
        assert!(vm.is_equal_pair(expected, result)?.is_true());

        Ok(())
    }

    #[test]
    fn test_backquote() -> VMResult<()> {
        let mut vm = new_slosh_vm();
        let result = exec(&mut vm, "(do (def x 3) `(1 2 ~x))")?;
        let expected = read_test(&mut vm, "(1 2 3)").map_err(|e| VMError::new("read", e.reason))?;
        assert!(vm.is_equal_pair(expected, result)?.is_true());

        let result = exec(&mut vm, "(let (x 3) `(1 2 ~x))")?;
        let expected = read_test(&mut vm, "(1 2 3)").map_err(|e| VMError::new("read", e.reason))?;
        assert!(vm.is_equal_pair(expected, result)?.is_true());

        let result = exec(&mut vm, "(let (x 3) `(1 2 (x y ~x) ~x))")?;
        let expected =
            read_test(&mut vm, "(1 2 (x y 3) 3)").map_err(|e| VMError::new("read", e.reason))?;
        assert!(vm.is_equal_pair(expected, result)?.is_true());

        let result = exec(&mut vm, "(let (x 3) `~x)")?;
        let expected = read_test(&mut vm, "3").map_err(|e| VMError::new("read", e.reason))?;
        assert!(vm.is_equal_pair(expected, result)?.is_true());

        let result = exec(&mut vm, "(let (z '(1 2 3)) `(~@z 4 5 6))")?;
        let expected =
            read_test(&mut vm, "(1 2 3 4 5 6)").map_err(|e| VMError::new("read", e.reason))?;
        assert!(vm.is_equal_pair(expected, result)?.is_true());

        let result = exec(&mut vm, "(let (y 'x) `~y)")?;
        let expected = read_test(&mut vm, "x").map_err(|e| VMError::new("read", e.reason))?;
        assert!(vm.is_equal_pair(expected, result)?.is_true());
        let result = exec(&mut vm, "(let (y 'x) ``~~y)")?;
        let expected = read_test(&mut vm, "`~x").map_err(|e| VMError::new("read", e.reason))?;
        assert!(vm.is_equal_pair(expected, result)?.is_true());

        let result = exec(&mut vm, "(let (x \"xxx\") `~x)")?;
        let expected = read_test(&mut vm, "\"xxx\"").map_err(|e| VMError::new("read", e.reason))?;
        assert!(vm.is_equal_pair(expected, result)?.is_true());

        let result = exec(&mut vm, "(do (def x \"xxx\") `~x)")?;
        let expected = read_test(&mut vm, "\"xxx\"").map_err(|e| VMError::new("read", e.reason))?;
        assert!(vm.is_equal_pair(expected, result)?.is_true());

        let result = exec(&mut vm, "`~5")?;
        let expected = read_test(&mut vm, "5").map_err(|e| VMError::new("read", e.reason))?;
        assert!(vm.is_equal_pair(expected, result)?.is_true());

        Ok(())
    }
}
