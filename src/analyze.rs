use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::builtins::expand_macro;
use crate::builtins_bind::{builtin_def, builtin_set};
use crate::environment::*;
use crate::eval::*;
use crate::gc::*;
use crate::types::*;

#[derive(Clone, Debug)]
struct SymbolsInt {
    syms: HashMap<&'static str, usize>,
    count: usize,
}

#[derive(Clone, Debug)]
pub struct Symbols {
    data: Rc<RefCell<SymbolsInt>>,
    lex_id: usize,
    lex_depth: u16,
}

impl Symbols {
    pub fn new() -> Symbols {
        let data = Rc::new(RefCell::new(SymbolsInt {
            syms: HashMap::new(),
            count: 0,
        }));
        Symbols {
            data,
            lex_id: 0,
            lex_depth: 0,
        }
    }

    pub fn lex_id(&self) -> usize {
        self.lex_id
    }

    pub fn lex_depth(&self) -> u16 {
        self.lex_depth
    }

    pub fn contains_symbol(&self, key: &str) -> bool {
        self.data.borrow().syms.contains_key(key)
    }

    pub fn get(&self, key: &str) -> Option<usize> {
        if let Some(idx) = self.data.borrow().syms.get(key) {
            Some(*idx)
        } else {
            None
        }
    }

    pub fn clear(&mut self) {
        self.data.borrow_mut().syms.clear();
    }

    pub fn insert(&mut self, key: &'static str) -> usize {
        let mut data = self.data.borrow_mut();
        let count = data.count;
        data.syms.insert(key, count);
        data.count += 1;
        count
    }

    pub fn set_lex(&mut self, environment: &mut Environment) {
        if let Some(lex_syms) = &environment.syms {
            self.lex_id = lex_syms.lex_id;
            self.lex_depth = lex_syms.lex_depth + 1;
        } else {
            self.lex_id = environment.next_lex_id;
            environment.next_lex_id += 1;
            self.lex_depth = 0;
        }
    }
}

impl Default for Symbols {
    fn default() -> Self {
        Self::new()
    }
}

fn make_fn(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<Expression, LispError> {
    if let Some(params) = args.next() {
        let (first, second) = (args.next(), args.next());
        let body = if let Some(first) = first {
            if let Some(second) = second {
                let mut body: Vec<Handle> = Vec::new();
                body.push(Expression::alloc_data_h(ExpEnum::Symbol(
                    "do",
                    SymLoc::None,
                )));
                body.push(first.into());
                body.push(second.into());
                for a in args {
                    body.push(a.into());
                }
                Expression::with_list(body)
            } else {
                first
            }
        } else {
            Expression::make_nil()
        };
        let params_d = params.get();
        let p_iter = if let ExpEnum::Vector(vec) = &params_d.data {
            Box::new(ListIter::new_list(&vec))
        } else {
            params.iter()
        };
        let mut params = Vec::new();
        let mut syms = Symbols::new();
        syms.set_lex(environment);
        for p in p_iter {
            if let ExpEnum::Symbol(s, _) = p.get().data {
                params.push(s);
                syms.insert(s);
            } else {
                return Err(LispError::new("fn: parameters must be symbols"));
            }
        }
        let body = body.handle_no_root();
        return Ok(Expression::alloc_data(ExpEnum::Lambda(Lambda {
            params,
            body,
            syms,
            namespace: environment.namespace.clone(),
        })));
    }
    Err(LispError::new("fn: needs at least one form"))
}

fn declare_var(
    environment: &mut Environment,
    args: &mut dyn Iterator<Item = Expression>,
) -> Result<(), LispError> {
    if let Some(syms) = &mut environment.syms {
        if let Some(key) = args.next() {
            let mut key_d = key.get_mut();
            match &mut key_d.data {
                ExpEnum::Symbol(s, location) => {
                    if syms.contains_symbol(s) {
                        Err(LispError::new(format!(
                            "var: Symbol {} already defined in scope.",
                            s
                        )))
                    } else {
                        let idx = syms.insert(*s);
                        location.replace(SymLoc::Stack(idx));
                        Ok(())
                    }
                }
                _ => Err(LispError::new(
                    "var: First form (binding key) must be a symbol",
                )),
            }
        } else {
            Err(LispError::new("var: Requires a symbol."))
        }
    } else {
        Err(LispError::new(
            "var: Using var outside a lambda not allowed.",
        ))
    }
}

pub fn analyze(
    environment: &mut Environment,
    expression_in: &Expression,
) -> Result<Expression, LispError> {
    let mut expression = expression_in.clone_root();
    // If we have a macro expand it and replace the expression with the expansion.
    if let Some(exp) = expand_macro(environment, &expression, false, 0)? {
        let mut nv: Vec<Handle> = Vec::new();
        let mut macro_replace = true;
        if let ExpEnum::Vector(list) = &exp.get().data {
            for item in list {
                let item: Expression = item.into();
                let item = item.resolve(environment)?;
                nv.push(item.into());
            }
        } else if let ExpEnum::Pair(_, _) = &exp.get().data {
            for item in exp.iter() {
                let item = item.resolve(environment)?;
                nv.push(item.into());
            }
        } else {
            expression = exp.clone();
            macro_replace = false;
        }
        if macro_replace {
            let mut exp_mut = expression.get_mut();
            match exp_mut.data {
                ExpEnum::Vector(_) => {
                    exp_mut.data.replace(ExpEnum::Vector(nv));
                    drop(exp_mut);
                    gc_mut().down_root(&expression);
                }
                ExpEnum::Pair(_, _) => {
                    exp_mut.data.replace(ExpEnum::cons_from_vec(&mut nv));
                    drop(exp_mut);
                    gc_mut().down_root(&expression);
                }
                _ => {}
            }
        }
    }
    let mut exp_a = expression.get_mut();
    let ret =
        match &mut exp_a.data {
            ExpEnum::Vector(v) => {
                if let Some((car, cdr)) = v.split_first() {
                    let car: Expression = car.into();
                    let car_d = car.get();
                    if let ExpEnum::Symbol(_, _) = &car_d.data {
                        let form = get_expression(environment, car.clone());
                        if let Some(form_exp) = form {
                            let exp_d = form_exp.get();
                            match &exp_d.data {
                                ExpEnum::DeclareFn => {
                                    let lambda = {
                                        let mut ib = box_slice_it(cdr);
                                        make_fn(environment, &mut ib)?
                                    };
                                    drop(exp_a);
                                    expression
                                        .get_mut()
                                        .data
                                        .replace(ExpEnum::Wrapper(lambda.into()));
                                }
                                ExpEnum::DeclareDef => {
                                    drop(exp_d);
                                    form_exp.get_mut().data.replace(ExpEnum::Function(
                                        Callable::new(builtin_def, true),
                                    ));
                                }
                                ExpEnum::DeclareVar => {
                                    {
                                        let mut ib = box_slice_it(cdr);
                                        declare_var(environment, &mut ib)?;
                                    }
                                    drop(exp_d);
                                    form_exp.get_mut().data.replace(ExpEnum::Function(
                                        Callable::new(builtin_set, true),
                                    ));
                                }
                                ExpEnum::Macro(_) => {
                                    panic!("Macros should have been expanded at this point!");
                                }
                                _ => {}
                            }
                        }
                    }
                }
                Ok(expression.clone())
            }
            ExpEnum::Pair(car, cdr) => {
                let car: Expression = car.into();
                if let ExpEnum::Symbol(_, _) = &car.get().data {
                    let form = get_expression(environment, car.clone());
                    if let Some(form_exp) = form {
                        let exp_d = form_exp.get();
                        match &exp_d.data {
                            ExpEnum::DeclareFn => {
                                let cdr: Expression = cdr.into();
                                let lambda = make_fn(environment, &mut cdr.iter())?;
                                drop(exp_a);
                                expression
                                    .get_mut()
                                    .data
                                    .replace(ExpEnum::Wrapper(lambda.into()));
                            }
                            ExpEnum::DeclareDef => {
                                drop(exp_d);
                                form_exp
                                    .get_mut()
                                    .data
                                    .replace(ExpEnum::Function(Callable::new(builtin_def, true)));
                            }
                            ExpEnum::DeclareVar => {
                                {
                                    let cdr: Expression = cdr.into();
                                    declare_var(environment, &mut cdr.iter())?;
                                }
                                drop(exp_d);
                                form_exp
                                    .get_mut()
                                    .data
                                    .replace(ExpEnum::Function(Callable::new(builtin_set, true)));
                            }
                            ExpEnum::Macro(_) => {
                                panic!("Macros should have been expanded at this point!");
                            }
                            _ => {}
                        }
                    }
                }
                Ok(expression.clone())
            }
            ExpEnum::Values(_v) => Ok(expression.clone()),
            ExpEnum::Nil => Ok(expression.clone()),
            ExpEnum::Symbol(s, location) => {
                if let SymLoc::None = location {
                    if let Some(syms) = &mut environment.syms {
                        if let Some(idx) = syms.get(s) {
                            location.replace(SymLoc::Stack(idx));
                        } else if let Some(r) = lookup_expression(environment, s) {
                            location.replace(SymLoc::Ref(r));
                        }
                    } else if let Some(r) = lookup_expression(environment, s) {
                        location.replace(SymLoc::Ref(r));
                    }
                }
                //SymLoc::None) => {
                /*if let Some(r) = lookup_expression(environment, s) {
                    let new_exp = ExpEnum::Symbol(s, SymLoc::Ref(r));
                    drop(exp_d);
                    drop(exp_a);
                    expression.get_mut().data.replace(new_exp);
                }*/
                Ok(expression.clone())
            }
            //ExpEnum::Symbol(_, _) => Ok(expression.clone()),
            ExpEnum::HashMap(_) => Ok(expression.clone()),
            ExpEnum::String(_, _) => Ok(expression.clone()),
            ExpEnum::True => Ok(expression.clone()),
            ExpEnum::Float(_) => Ok(expression.clone()),
            ExpEnum::Int(_) => Ok(expression.clone()),
            ExpEnum::Char(_) => Ok(expression.clone()),
            ExpEnum::CodePoint(_) => Ok(expression.clone()),
            ExpEnum::Lambda(_) => Ok(expression.clone()),
            ExpEnum::Macro(_) => panic!("Invalid macro in analyze!"),
            ExpEnum::Function(_) => Ok(expression.clone()),
            ExpEnum::Process(_) => Ok(expression.clone()),
            ExpEnum::File(_) => Ok(expression.clone()),
            ExpEnum::LazyFn(_, _) => Ok(expression.clone()),
            ExpEnum::Wrapper(_exp) => Ok(expression.clone()),
            ExpEnum::DeclareDef => panic!("Invalid def in analyze!"),
            ExpEnum::DeclareVar => panic!("Invalid var in analyze!"),
            ExpEnum::DeclareFn => panic!("Invalid fn in analyze!"),
            ExpEnum::Undefined => panic!("Invalid undefined in analyze!"),
        };
    match ret {
        Ok(ret) => Ok(ret.clone_root()),
        Err(err) => Err(err),
    }
}
