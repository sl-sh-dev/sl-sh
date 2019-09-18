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
        }
    }
}

#[derive(Clone, Debug)]
pub struct Environment<'a> {
    pub state: Rc<RefCell<EnvState>>,
    pub in_pipe: bool,
    pub data: HashMap<String, Expression>,
    pub procs: Rc<RefCell<HashMap<u32, Child>>>,
    pub outer: Option<&'a Environment<'a>>,
}

pub fn build_default_environment<'a>() -> Environment<'a> {
    let mut data: HashMap<String, Expression> = HashMap::new();
    let procs: Rc<RefCell<HashMap<u32, Child>>> = Rc::new(RefCell::new(HashMap::new()));
    add_builtins(&mut data);
    add_math_builtins(&mut data);
    add_str_builtins(&mut data);
    add_list_builtins(&mut data);
    add_file_builtins(&mut data);
    Environment {
        state: Rc::new(RefCell::new(EnvState::default())),
        in_pipe: false,
        data,
        procs,
        outer: None,
    }
}

pub fn build_new_scope_with_data<'a, S: ::std::hash::BuildHasher>(
    environment: &'a Environment<'a>,
    mut data_in: HashMap<String, Expression, S>,
) -> Environment<'a> {
    let mut data: HashMap<String, Expression> = HashMap::new();
    for (k, v) in data_in.drain() {
        data.insert(k, v);
    }
    Environment {
        state: environment.state.clone(),
        in_pipe: environment.in_pipe,
        data,
        procs: environment.procs.clone(),
        outer: Some(environment),
    }
}

pub fn build_new_scope<'a>(environment: &'a Environment<'a>) -> Environment<'a> {
    let data: HashMap<String, Expression> = HashMap::new();
    Environment {
        state: environment.state.clone(),
        in_pipe: environment.in_pipe,
        data,
        procs: environment.procs.clone(),
        outer: Some(environment),
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
    match environment.data.get(key) {
        Some(exp) => Some(exp.clone()),
        None => match environment.outer {
            Some(outer) => get_expression(outer, key),
            None => None,
        },
    }
}

pub fn is_expression(environment: &Environment, key: &str) -> bool {
    if key.starts_with('$') {
        env::var(&key[1..]).is_ok()
    } else {
        match environment.data.get(key) {
            Some(_) => true,
            None => match environment.outer {
                Some(outer) => is_expression(outer, key),
                None => false,
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
