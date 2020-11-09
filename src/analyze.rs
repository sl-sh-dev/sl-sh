use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::builtins::builtin_fn;
use crate::builtins::expand_macro;
use crate::builtins_bind::{builtin_def, builtin_var};
use crate::environment::*;
use crate::eval::*;
use crate::gc::*;
use crate::types::*;

#[derive(Clone, Debug)]
pub struct Symbols {
    syms: Rc<RefCell<HashMap<&'static str, usize>>>,
    count: usize,
}

impl Symbols {
    pub fn new() -> Symbols {
        Symbols {
            syms: Rc::new(RefCell::new(HashMap::new())),
            count: 0,
        }
    }

    pub fn contains_symbol(&self, key: &str) -> bool {
        self.syms.borrow().contains_key(key)
    }

    pub fn get(&self, key: &str) -> Option<usize> {
        if let Some(idx) = self.syms.borrow().get(key) {
            Some(*idx)
        } else {
            None
        }
    }

    pub fn clear(&mut self) {
        self.syms.borrow_mut().clear();
    }

    pub fn insert(&mut self, key: &'static str) {
        self.syms.borrow_mut().insert(key, self.count);
        self.count += 1;
    }
}

impl Default for Symbols {
    fn default() -> Self {
        Self::new()
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
    let exp_a = expression.get();
    let exp_d = &exp_a.data;
    let ret =
        match exp_d {
            ExpEnum::Vector(v) => {
                if let Some((car, cdr)) = v.split_first() {
                    let car: Expression = car.into();
                    let car_d = car.get();
                    if let ExpEnum::Symbol(_, _) = &car_d.data {
                        let form = get_expression(environment, car.clone());
                        if let Some(exp) = form {
                            match &exp.exp.get().data {
                                ExpEnum::DeclareFn => {
                                    let lambda = {
                                        let mut ib = box_slice_it(cdr);
                                        builtin_fn(environment, &mut ib)?
                                    };
                                    drop(exp_a);
                                    expression
                                        .get_mut()
                                        .data
                                        .replace(ExpEnum::Wrapper(lambda.into()));
                                }
                                ExpEnum::DeclareDef => {
                                    drop(exp_a);
                                    expression.get_mut().data.replace(ExpEnum::Function(
                                        Callable::new(builtin_def, true),
                                    ));
                                }
                                ExpEnum::DeclareVar => {
                                    drop(exp_a);
                                    expression.get_mut().data.replace(ExpEnum::Function(
                                        Callable::new(builtin_var, true),
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
                        let exp_d = form_exp.exp.get();
                        match &exp_d.data {
                            ExpEnum::DeclareFn => {
                                let cdr: Expression = cdr.into();
                                let lambda = builtin_fn(environment, &mut cdr.iter())?;
                                drop(exp_a);
                                expression
                                    .get_mut()
                                    .data
                                    .replace(ExpEnum::Wrapper(lambda.into()));
                            }
                            ExpEnum::DeclareDef => {
                                drop(exp_d);
                                form_exp
                                    .exp
                                    .get_mut()
                                    .data
                                    .replace(ExpEnum::Function(Callable::new(builtin_def, true)));
                            }
                            ExpEnum::DeclareVar => {
                                drop(exp_d);
                                form_exp
                                    .exp
                                    .get_mut()
                                    .data
                                    .replace(ExpEnum::Function(Callable::new(builtin_var, true)));
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
            /*ExpEnum::Symbol(s, _) => {//SymLoc::None) => {
                if let Some(r) = lookup_expression(environment, s) {
                    let new_exp = ExpEnum::Symbol(s, SymLoc::Ref(r));
                    drop(exp_d);
                    drop(exp_a);
                    expression.get_mut().data.replace(new_exp);
                }
                Ok(expression.clone())
            }*/
            ExpEnum::Symbol(_, _) => Ok(expression.clone()),
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
        };
    match ret {
        Ok(ret) => Ok(ret.clone_root()),
        Err(err) => Err(err),
    }
}
