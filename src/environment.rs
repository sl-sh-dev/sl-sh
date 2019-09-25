use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::io;
use std::process::Child;
use std::rc::Rc;

use crate::builtins::add_builtins;
use crate::builtins_file::add_file_builtins;
use crate::builtins_list::add_list_builtins;
use crate::builtins_math::add_math_builtins;
use crate::builtins_str::add_str_builtins;
use crate::process::*;
use crate::types::*;

#[derive(Clone, Debug)]
pub enum IOState {
    FileAppend(String),
    FileOverwrite(String),
    Pipe,
    Inherit,
    Null,
}

#[derive(Clone, Debug)]
pub struct EnvState {
    pub recur_num_args: Option<usize>,
    pub gensym_count: u32,
    pub stdout_status: Option<IOState>,
    pub stderr_status: Option<IOState>,
    pub eval_level: u32,
    pub stopped_procs: Vec<u32>,
    pub is_spawn: bool,
    pub pipe_pgid: Option<u32>,
}

impl Default for EnvState {
    fn default() -> Self {
        EnvState {
            recur_num_args: None,
            gensym_count: 0,
            stdout_status: None,
            stderr_status: None,
            eval_level: 0,
            stopped_procs: Vec::new(),
            is_spawn: false,
            pipe_pgid: None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FormType {
    Any,
    FormOnly,
    ExternalOnly,
}

#[derive(Clone, Debug)]
pub struct Environment<'a> {
    pub state: Rc<RefCell<EnvState>>,
    pub in_pipe: bool,
    pub is_tty: bool,
    pub loose_symbols: bool,
    pub data: HashMap<String, Expression>,
    pub global: Rc<RefCell<HashMap<String, Expression>>>,
    pub procs: Rc<RefCell<HashMap<u32, Child>>>,
    pub outer: Option<&'a Environment<'a>>,
    pub data_in: Option<Expression>,
    pub form_type: FormType,
}

pub fn build_default_environment<'a>() -> Environment<'a> {
    let mut data: HashMap<String, Expression> = HashMap::new();
    let procs: Rc<RefCell<HashMap<u32, Child>>> = Rc::new(RefCell::new(HashMap::new()));
    let global: Rc<RefCell<HashMap<String, Expression>>> = Rc::new(RefCell::new(HashMap::new()));
    add_builtins(&mut data);
    add_math_builtins(&mut data);
    add_str_builtins(&mut data);
    add_list_builtins(&mut data);
    add_file_builtins(&mut data);
    Environment {
        state: Rc::new(RefCell::new(EnvState::default())),
        in_pipe: false,
        is_tty: true,
        loose_symbols: false,
        data,
        global,
        procs,
        outer: None,
        data_in: None,
        form_type: FormType::Any,
    }
}

pub fn build_new_scope_with_data<'a, S: ::std::hash::BuildHasher>(
    environment: &'a Environment<'a>,
    mut data_in: HashMap<String, Expression, S>,
) -> Environment<'a> {
    let mut data: HashMap<String, Expression> = HashMap::with_capacity(data_in.len());
    for (k, v) in data_in.drain() {
        data.insert(k, v);
    }
    Environment {
        state: environment.state.clone(),
        in_pipe: environment.in_pipe,
        is_tty: environment.is_tty,
        loose_symbols: false,
        data,
        global: environment.global.clone(),
        procs: environment.procs.clone(),
        outer: Some(environment),
        data_in: None,
        form_type: environment.form_type,
    }
}

pub fn build_new_spawn_scope<'a, S: ::std::hash::BuildHasher>(
    mut data_in: HashMap<String, Expression, S>,
    mut global_in: HashMap<String, Expression, S>,
) -> Environment<'a> {
    let procs: Rc<RefCell<HashMap<u32, Child>>> = Rc::new(RefCell::new(HashMap::new()));
    let state = Rc::new(RefCell::new(EnvState::default()));
    let mut global: HashMap<String, Expression> = HashMap::with_capacity(global_in.len());
    for (k, v) in global_in.drain() {
        global.insert(k, v);
    }
    let global: Rc<RefCell<HashMap<String, Expression>>> = Rc::new(RefCell::new(global));
    let mut data: HashMap<String, Expression> = HashMap::with_capacity(data_in.len());
    for (k, v) in data_in.drain() {
        data.insert(k, v);
    }
    state.borrow_mut().is_spawn = true;
    Environment {
        state,
        in_pipe: false,
        is_tty: false,
        loose_symbols: false,
        data,
        global,
        procs,
        outer: None,
        data_in: None,
        form_type: FormType::Any,
    }
}

pub fn build_new_scope<'a>(environment: &'a Environment<'a>) -> Environment<'a> {
    let data: HashMap<String, Expression> = HashMap::new();
    Environment {
        state: environment.state.clone(),
        in_pipe: environment.in_pipe,
        is_tty: environment.is_tty,
        loose_symbols: false,
        data,
        global: environment.global.clone(),
        procs: environment.procs.clone(),
        outer: Some(environment),
        data_in: None,
        form_type: environment.form_type,
    }
}

pub fn clone_symbols<S: ::std::hash::BuildHasher>(
    environment: &Environment,
    data_in: &mut HashMap<String, Expression, S>,
) {
    for (k, v) in &environment.data {
        data_in.insert(k.clone(), v.clone());
    }
    if let Some(outer) = environment.outer {
        clone_symbols(outer, data_in);
    }
}

pub fn get_expression(environment: &Environment, key: &str) -> Option<Expression> {
    if key.starts_with("#g") {
        match environment.global.borrow().get(&key[2..]) {
            Some(global) => Some(global.clone()),
            None => None,
        }
    } else {
        match environment.data.get(key) {
            Some(exp) => Some(exp.clone()),
            None => match environment.outer {
                Some(outer) => get_expression(outer, key),
                None => match environment.global.borrow().get(key) {
                    Some(global) => Some(global.clone()),
                    None => None,
                },
            },
        }
    }
}

pub fn set_expression(environment: &mut Environment, key: String, expression: Expression) {
    environment.data.insert(key, expression);
}

pub fn set_expression_global(environment: &mut Environment, key: String, expression: Expression) {
    environment.global.borrow_mut().insert(key, expression);
}

pub fn is_expression(environment: &Environment, key: &str) -> bool {
    if key.starts_with('$') {
        env::var(&key[1..]).is_ok()
    } else {
        match environment.data.get(key) {
            Some(_) => true,
            None => match environment.outer {
                Some(outer) => is_expression(outer, key),
                None => environment.global.borrow().get(key).is_some(),
            },
        }
    }
}

pub fn add_process(environment: &Environment, process: Child) -> u32 {
    let pid = process.id();
    environment.procs.borrow_mut().insert(pid, process);
    pid
}

pub fn reap_procs(environment: &Environment) -> io::Result<()> {
    let mut procs = environment.procs.borrow_mut();
    let keys: Vec<u32> = procs.keys().copied().collect();
    let mut pids: Vec<u32> = Vec::with_capacity(keys.len());
    for key in keys {
        if let Some(child) = procs.get_mut(&key) {
            pids.push(child.id());
        }
    }
    drop(procs);
    for pid in pids {
        try_wait_pid(environment, pid);
    }
    // XXX remove them or better replace pid with exit status
    Ok(())
}
