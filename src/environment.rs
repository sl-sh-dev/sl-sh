use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::rc::Rc;

use sl_liner::Context;

use crate::interner::*;
use crate::process::*;
use crate::reader::ReaderState;
use crate::symbols::*;
use crate::types::*;
use crate::unix::cvt;
use crate::{add_math_builtins, add_stats_builtins};

const ROOT_NS: &str = "root";
const MATH_NS: &str = "math";
const STATS_NS: &str = "stats";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keys {
    Vi,
    Emacs,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReplSettings {
    pub key_bindings: Keys,
    pub max_history: usize,
    pub vi_esc_sequence: Option<(char, char, u32)>,
    pub vi_normal_prompt_prefix: Option<String>,
    pub vi_normal_prompt_suffix: Option<String>,
    pub vi_insert_prompt_prefix: Option<String>,
    pub vi_insert_prompt_suffix: Option<String>,
}

impl Default for ReplSettings {
    fn default() -> Self {
        ReplSettings {
            key_bindings: Keys::Emacs,
            max_history: 1000,
            vi_esc_sequence: None,
            vi_normal_prompt_prefix: None,
            vi_normal_prompt_suffix: None,
            vi_insert_prompt_prefix: None,
            vi_insert_prompt_suffix: None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum JobStatus {
    Running,
    Stopped,
}

impl fmt::Display for JobStatus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            JobStatus::Running => write!(f, "Running"),
            JobStatus::Stopped => write!(f, "Stopped"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Job {
    pub pids: Vec<u32>,
    pub names: Vec<String>,
    pub status: JobStatus,
}

#[derive(Clone, Debug)]
pub struct StackFrame {
    pub index: usize,
    pub symbols: Symbols,
}

pub struct GrabProcOutput<'a> {
    pub old_grab_proc_output: bool,
    pub environment: &'a mut Environment,
}

pub fn set_grab_proc_output(
    environment: &mut Environment,
    grab_proc_output: bool,
) -> GrabProcOutput {
    let old_grab_proc_output = environment.grab_proc_output;
    environment.grab_proc_output = grab_proc_output;
    GrabProcOutput {
        old_grab_proc_output,
        environment,
    }
}

impl<'a> Drop for GrabProcOutput<'a> {
    fn drop(&mut self) {
        self.environment.grab_proc_output = self.old_grab_proc_output;
    }
}

pub type ProcessMap = Rc<RefCell<HashMap<u32, (Expression, Option<i32>)>>>;

//#[derive(Clone, Debug)]
pub struct Environment {
    pub recur_num_args: Option<usize>,
    pub gensym_count: u32,
    pub eval_level: u32,
    pub pipe_pgid: Option<u32>,
    pub stack: Vec<Binding>,
    pub stack_frames: Vec<StackFrame>,
    pub stack_frame_base: usize,
    pub reader_state: ReaderState,
    pub stopped_procs: Rc<RefCell<Vec<u32>>>,
    pub jobs: Rc<RefCell<Vec<Job>>>,
    pub is_tty: bool,
    pub do_job_control: bool,
    // key is pid, val is (process expression, output fd)
    pub procs: ProcessMap,
    pub save_exit_status: bool,
    // If this is Some then need to unwind and exit with then provided code (exit was called).
    pub exit_code: Option<i32>,
    // This is the environment's root (global namespace), it will also be part of
    // higher level namespaces.
    // It's special so keep a reference here as well for handy access.
    pub root_scope: Rc<RefCell<Namespace>>,
    // This is the current namespace.
    pub namespace: Rc<RefCell<Namespace>>,
    // Map of all the created namespaces.
    pub namespaces: HashMap<&'static str, Rc<RefCell<Namespace>>>,
    // Allow lazy functions (i.e. enable TCO).
    pub allow_lazy_fn: bool,
    // Used for block/return-from
    pub return_val: Option<(Option<&'static str>, Expression)>,
    // Interner for symbols and some strings.
    pub interner: Interner,
    // Save the meta data for the last expression evalled.
    pub last_meta: Option<ExpMeta>,
    pub repl_settings: ReplSettings,
    pub liners: HashMap<&'static str, Context>,
    pub next_lex_id: usize,
    pub supress_eval: bool, // XXX Hack for apply...
    pub terminal_fd: i32,
    pub grab_proc_output: bool,
    pub in_fork: bool,
}

impl Environment {
    pub fn insert_into_root_scope(&mut self, symbol: &'static str, data: Expression) {
        self.root_scope.borrow_mut().insert(symbol, data);
    }
}

pub fn build_default_environment() -> Environment {
    let procs: ProcessMap = Rc::new(RefCell::new(HashMap::new()));
    let mut interner = Interner::with_capacity(8192);
    let root_scope = Rc::new(RefCell::new(Namespace::new_root(&mut interner)));
    let math_scope = Rc::new(RefCell::new(Namespace::new_ns(
        &mut interner,
        MATH_NS,
        add_math_builtins,
    )));
    let stats_scope = Rc::new(RefCell::new(Namespace::new_ns(
        &mut interner,
        STATS_NS,
        add_stats_builtins,
    )));
    let namespace = root_scope.clone();
    let mut namespaces = HashMap::new();
    let terminal_fd = unsafe {
        if let Ok(fd) = cvt(libc::dup(0)) {
            fd
        } else {
            0
        }
    };
    let reader_state = ReaderState::new();
    namespaces.insert(interner.intern(ROOT_NS), root_scope.clone());
    namespaces.insert(interner.intern(MATH_NS), math_scope);
    namespaces.insert(interner.intern(STATS_NS), stats_scope);
    Environment {
        recur_num_args: None,
        gensym_count: 0,
        eval_level: 0,
        pipe_pgid: None,
        stack: Vec::with_capacity(1024),
        stack_frames: Vec::with_capacity(500),
        stack_frame_base: 0,
        reader_state,
        stopped_procs: Rc::new(RefCell::new(Vec::new())),
        jobs: Rc::new(RefCell::new(Vec::new())),
        is_tty: true,
        do_job_control: true,
        procs,
        save_exit_status: true,
        exit_code: None,
        root_scope,
        namespace,
        namespaces,
        allow_lazy_fn: true,
        return_val: None,
        interner,
        last_meta: None,
        repl_settings: ReplSettings::default(),
        liners: HashMap::new(),
        next_lex_id: 1,
        supress_eval: false,
        terminal_fd,
        grab_proc_output: false,
        in_fork: false,
    }
}

pub fn build_new_namespace(
    environment: &mut Environment,
    name: &str,
) -> Result<Rc<RefCell<Namespace>>, LispError> {
    if environment.namespaces.contains_key(name) {
        let msg = format!("Namespace {} already exists!", name);
        Err(LispError::new(msg))
    } else {
        let name = environment.interner.intern(name);
        let mut namespace = Namespace::new_with_outer(name, Some(environment.root_scope.clone()));
        namespace.insert("*ns*", ExpEnum::String(name.into(), None).into());
        let namespace = Rc::new(RefCell::new(namespace));
        environment.namespaces.insert(name, namespace.clone());
        Ok(namespace)
    }
}

pub fn get_from_namespace(environment: &Environment, key: &str) -> Option<Expression> {
    let mut loop_scope = Some(environment.namespace.clone());
    while let Some(scope) = loop_scope {
        if let Some(exp) = scope.borrow().get(key) {
            return Some(exp);
        }
        loop_scope = scope.borrow().outer();
    }
    None
}

fn lookup_in_stack(environment: &Environment, key: &str) -> Option<Binding> {
    if let Some(current_frame) = environment.stack_frames.last() {
        let lex_id = current_frame.symbols.lex_id();
        let lex_depth = current_frame.symbols.lex_depth();
        if let Some(idx) = environment
            .stack_frames
            .iter()
            .rev()
            .map(|syms| {
                (
                    syms.symbols.get(key),
                    syms.index,
                    syms.symbols.lex_id(),
                    syms.symbols.lex_depth(),
                )
            })
            .find(|(v, _i, my_lex_id, my_lex_depth)| {
                v.is_some() && *my_lex_id == lex_id && *my_lex_depth <= lex_depth
            })
            .map(|(v, i, _, _)| v.unwrap() + i)
        {
            if let Some(r) = environment.stack.get(idx) {
                return Some(r.clone());
            }
        }
    }
    None
}

fn lookup_in_namespace(environment: &Environment, key: &str) -> Option<Binding> {
    // Check for namespaced symbols.
    if key.contains("::") {
        // namespace reference.
        let mut key_i = key.splitn(2, "::");
        if let Some(namespace) = key_i.next() {
            if let Some(scope) = environment.namespaces.get(namespace) {
                if let Some(key) = key_i.next() {
                    if let Some(reference) = scope.borrow().get_binding(key) {
                        return Some(reference);
                    }
                }
            }
        }
        None
    // If in a lambda then it's namespace should be current.
    } else if let Some(reference) = environment.namespace.borrow().get_binding(key) {
        Some(reference)
    // Finally check root (this might be a duplicate if in root but in that case about give up anyway).
    } else {
        environment.root_scope.borrow().get_binding(key)
    }
}

pub fn lookup_expression(environment: &Environment, key: &str) -> Option<Expression> {
    // First check any "local" lexical scopes.
    if let Some(exp) = lookup_in_stack(environment, key) {
        Some(exp.get())
    // Check for namespaced symbols.
    } else {
        lookup_in_namespace(environment, key).map(|binding| binding.get())
    }
}

pub fn capture_expression(environment: &Environment, key: &str) -> Option<Binding> {
    // First check any "local" lexical scopes.
    if let Some(exp) = lookup_in_stack(environment, key) {
        Some(exp)
    // Check for namespaced symbols.
    } else {
        lookup_in_namespace(environment, key)
    }
}

pub fn get_expression_stack(environment: &Environment, idx: usize) -> Option<Expression> {
    environment
        .stack
        .get(environment.stack_frame_base + idx)
        .map(|r| r.get())
}

pub fn get_expression_look(
    environment: &Environment,
    expression: Expression,
    allow_lookup: bool,
) -> Option<Expression> {
    match &expression.get().data {
        ExpEnum::Symbol(sym, location) => match location {
            SymLoc::None => {
                if allow_lookup {
                    // XXX TODO- only lookup in namespace not stack.
                    lookup_expression(environment, sym)
                } else {
                    None
                }
            }
            SymLoc::Ref(binding) => Some(binding.get()),
            SymLoc::Namespace(scope, idx) => scope.borrow().get_idx(*idx),
            SymLoc::Stack(idx) => get_expression_stack(environment, *idx),
        },
        _ => None, // XXX Maybe this should be an error?
    }
}

pub fn get_expression(environment: &Environment, expression: Expression) -> Option<Expression> {
    get_expression_look(environment, expression, true) // XXX TODO- this should work with false- or maybe not...
}

pub fn is_expression(environment: &Environment, key: &str) -> bool {
    lookup_expression(environment, key).is_some()
}

pub fn get_symbol_namespaces(environment: &Environment, key: &str) -> Vec<Rc<RefCell<Namespace>>> {
    let mut ret: Vec<Rc<RefCell<Namespace>>> = Vec::new();
    for namespace in environment.namespaces.values() {
        if namespace.borrow().contains_key(key) {
            ret.push(namespace.clone());
        }
    }
    ret
}

pub fn get_namespace(environment: &Environment, name: &str) -> Option<Rc<RefCell<Namespace>>> {
    if environment.namespaces.contains_key(name) {
        Some(environment.namespaces.get(name).unwrap().clone())
    } else {
        None
    }
}

pub fn mark_job_stopped(environment: &Environment, pid: u32) {
    'outer: for mut j in environment.jobs.borrow_mut().iter_mut() {
        for p in &j.pids {
            if *p == pid {
                j.status = JobStatus::Stopped;
                break 'outer;
            }
        }
    }
}

pub fn mark_job_running(environment: &Environment, pid: u32) {
    'outer: for mut j in environment.jobs.borrow_mut().iter_mut() {
        for p in &j.pids {
            if *p == pid {
                j.status = JobStatus::Running;
                break 'outer;
            }
        }
    }
}

pub fn remove_job(environment: &Environment, pid: u32) {
    let mut idx: Option<usize> = None;
    'outer: for (i, j) in environment.jobs.borrow_mut().iter_mut().enumerate() {
        for p in &j.pids {
            if *p == pid {
                idx = Some(i);
                break 'outer;
            }
        }
    }
    if let Some(i) = idx {
        environment.jobs.borrow_mut().remove(i);
    }
}

pub fn add_process(environment: &Environment, pid: u32, val: (Expression, Option<i32>)) {
    environment.procs.borrow_mut().insert(pid, val);
}

pub fn reap_procs(environment: &Environment) -> io::Result<()> {
    let procs = environment.procs.borrow_mut();
    if !procs.is_empty() {
        let keys: Vec<u32> = procs.keys().copied().collect();
        let mut pids: Vec<u32> = Vec::with_capacity(keys.len());
        for key in keys {
            pids.push(key);
        }
        drop(procs);
        for pid in pids {
            // try_wait_pid removes them and tracks exit status
            try_wait_pid(environment, pid);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_lookup(environment: &Environment, key: &str, val: i64) {
        let xxx_i = if let Some(exp) = lookup_expression(environment, key) {
            if let ExpEnum::Int(i) = exp.get().data {
                i
            } else {
                -2
            }
        } else {
            -1
        };
        assert!(xxx_i == val);
    }

    fn assert_capture(environment: &Environment, key: &str, val: i64) {
        let xxx_i = if let Some(exp) = capture_expression(environment, key) {
            if let ExpEnum::Int(i) = exp.get().get().data {
                i
            } else {
                -2
            }
        } else {
            -1
        };
        assert!(xxx_i == val);
    }

    fn assert_exp_lookup(environment: &Environment, exp: Expression, val: i64) {
        let xxx_i = if let Some(exp) = get_expression_look(environment, exp, true) {
            if let ExpEnum::Int(i) = exp.get().data {
                i
            } else {
                -2
            }
        } else {
            -1
        };
        assert!(xxx_i == val);
    }

    fn assert_exp(environment: &Environment, exp: Expression, val: i64) {
        let xxx_i = if let Some(exp) = get_expression(environment, exp) {
            if let ExpEnum::Int(i) = exp.get().data {
                i
            } else {
                -2
            }
        } else {
            -1
        };
        assert!(xxx_i == val);
    }

    #[test]
    fn test_lookup_expression() -> Result<(), LispError> {
        let mut environment = build_default_environment();
        assert!(lookup_expression(&mut environment, "XXX").is_none());
        environment
            .stack
            .push(Binding::with_expression(ExpEnum::Int(222).into()));
        let mut syms = Symbols::with_frame(&mut environment, &None);
        syms.insert("XXX");
        environment.stack_frames.push(StackFrame {
            index: 0,
            symbols: syms.clone(),
        });
        assert!(lookup_expression(&mut environment, "XXXX").is_none());
        assert_lookup(&environment, "XXX", 222);
        assert!(lookup_expression(&mut environment, "YYY").is_none());
        environment
            .stack
            .push(Binding::with_expression(ExpEnum::Int(322).into()));
        syms.insert("YYY");
        assert_lookup(&environment, "YYY", 322);

        let mut syms2 = Symbols::with_frame(&mut environment, &Some(syms.clone()));
        syms2.insert("XXX2");
        environment.stack_frames.push(StackFrame {
            index: syms.len(),
            symbols: syms2.clone(),
        });
        environment.stack_frame_base = syms.len();
        environment
            .stack
            .push(Binding::with_expression(ExpEnum::Int(2).into()));
        assert!(lookup_expression(&mut environment, "XXXX").is_none());
        assert_lookup(&environment, "XXX", 222);
        assert_lookup(&environment, "YYY", 322);
        assert_lookup(&environment, "XXX2", 2);

        let mut syms3 = Symbols::with_frame(&mut environment, &None);
        syms3.insert("XXX3");
        syms3.insert("XXX3-2");
        syms3.insert("XXX3-3");
        environment.stack_frames.push(StackFrame {
            index: (syms.len() + syms2.len()),
            symbols: syms3.clone(),
        });
        environment.stack_frame_base = syms.len() + syms2.len();
        environment
            .stack
            .push(Binding::with_expression(ExpEnum::Int(3).into()));
        environment
            .stack
            .push(Binding::with_expression(ExpEnum::Int(32).into()));
        environment
            .stack
            .push(Binding::with_expression(ExpEnum::Int(33).into()));
        assert!(lookup_expression(&mut environment, "XXX").is_none());
        assert!(lookup_expression(&mut environment, "YYY").is_none());
        assert!(lookup_expression(&mut environment, "XXX2").is_none());
        assert_lookup(&environment, "XXX3", 3);
        assert_lookup(&environment, "XXX3-2", 32);
        assert_lookup(&environment, "XXX3-3", 33);

        environment
            .stack
            .truncate(environment.stack.len() - syms3.len());
        environment
            .stack_frames
            .truncate(environment.stack_frames.len() - 1);
        environment.stack_frame_base = syms.len();
        assert!(lookup_expression(&mut environment, "XXX3").is_none());
        assert!(lookup_expression(&mut environment, "XXX3-2").is_none());
        assert!(lookup_expression(&mut environment, "XXX3-3").is_none());
        assert_lookup(&environment, "XXX", 222);
        assert_lookup(&environment, "YYY", 322);
        assert_lookup(&environment, "XXX2", 2);

        assert!(lookup_expression(&mut environment, "*ns*").is_some());

        let ns_a = build_new_namespace(&mut environment, "ns-a")?;
        ns_a.borrow_mut().insert("a1", ExpEnum::Int(11).into());
        let ns_b = build_new_namespace(&mut environment, "ns-b")?;
        ns_b.borrow_mut().insert("b1", ExpEnum::Int(21).into());
        let ns_c = build_new_namespace(&mut environment, "ns-c")?;
        ns_c.borrow_mut().insert("c1", ExpEnum::Int(31).into());
        assert!(lookup_expression(&mut environment, "a1").is_none());
        assert!(lookup_expression(&mut environment, "b1").is_none());
        assert!(lookup_expression(&mut environment, "c1").is_none());
        assert_lookup(&environment, "ns-a::a1", 11);
        assert_lookup(&environment, "ns-b::b1", 21);
        assert_lookup(&environment, "ns-c::c1", 31);
        environment.namespace = ns_a.clone();
        environment.stack.truncate(0);
        environment.stack_frames.truncate(0);
        environment.stack_frame_base = 0;
        assert_lookup(&environment, "ns-a::a1", 11);
        assert!(lookup_expression(&mut environment, "b1").is_none());
        assert!(lookup_expression(&mut environment, "c1").is_none());
        environment.namespace = ns_b.clone();
        assert!(lookup_expression(&mut environment, "a1").is_none());
        assert_lookup(&environment, "ns-b::b1", 21);
        assert!(lookup_expression(&mut environment, "c1").is_none());
        environment.namespace = ns_c.clone();
        assert!(lookup_expression(&mut environment, "a1").is_none());
        assert!(lookup_expression(&mut environment, "b1").is_none());
        assert_lookup(&environment, "ns-c::c1", 31);
        assert_capture(&environment, "ns-c::c1", 31);
        assert_capture(&environment, "c1", 31);
        environment.namespace = ns_b.clone();
        assert_lookup(&environment, "ns-b::b1", 21);
        assert_lookup(&environment, "b1", 21);
        assert_lookup(&environment, "ns-c::c1", 31);
        assert!(lookup_expression(&mut environment, "c1").is_none());

        environment.namespace = ns_b.clone();
        environment
            .stack
            .push(Binding::with_expression(ExpEnum::Int(20).into()));
        let mut syms4 = Symbols::with_frame(&mut environment, &None);
        syms4.insert("st-1");
        environment.stack_frames.push(StackFrame {
            index: 0,
            symbols: syms4.clone(),
        });
        environment.stack_frame_base = 0;
        assert!(lookup_expression(&mut environment, "a1").is_none());
        assert!(lookup_expression(&mut environment, "c1").is_none());
        assert_lookup(&environment, "b1", 21);
        assert_lookup(&environment, "st-1", 20);
        assert_lookup(&environment, "ns-c::c1", 31);
        environment.namespace = ns_a.clone();
        assert!(lookup_expression(&mut environment, "a1").is_some());
        assert!(lookup_expression(&mut environment, "c1").is_none());
        environment.namespace = ns_b.clone();
        assert_lookup(&environment, "b1", 21);
        assert_lookup(&environment, "st-1", 20);
        assert_lookup(&environment, "ns-c::c1", 31);
        Ok(())
    }

    #[test]
    fn test_get_expression() -> Result<(), LispError> {
        let mut environment = build_default_environment();
        assert!(
            get_expression(&mut environment, ExpEnum::Symbol("NA", SymLoc::None).into()).is_none()
        );
        assert!(get_expression_look(
            &mut environment,
            ExpEnum::Symbol("NA", SymLoc::None).into(),
            true
        )
        .is_none());
        environment
            .stack
            .push(Binding::with_expression(ExpEnum::Int(222).into()));
        let mut syms = Symbols::with_frame(&mut environment, &None);
        syms.insert("NA");
        environment.stack_frames.push(StackFrame {
            index: 0,
            symbols: syms.clone(),
        });
        //assert!(
        //    get_expression(&mut environment, ExpEnum::Symbol("NA", SymLoc::None).into()).is_none()
        //);
        assert_exp_lookup(
            &environment,
            ExpEnum::Symbol("NA", SymLoc::None).into(),
            222,
        );
        assert_exp(
            &environment,
            ExpEnum::Symbol("NA", SymLoc::Stack(0)).into(),
            222,
        );

        let mut syms2 = Symbols::with_frame(&mut environment, &Some(syms.clone()));
        syms2.insert("NA2");
        environment.stack_frames.push(StackFrame {
            index: syms.len(),
            symbols: syms2.clone(),
        });
        environment.stack_frame_base = syms.len();
        environment
            .stack
            .push(Binding::with_expression(ExpEnum::Int(2).into()));
        assert_exp_lookup(
            &environment,
            ExpEnum::Symbol("NA", SymLoc::None).into(),
            222,
        );
        assert_exp_lookup(&environment, ExpEnum::Symbol("NA2", SymLoc::None).into(), 2);
        assert_exp(
            &environment,
            ExpEnum::Symbol("NA2", SymLoc::Stack(0)).into(),
            2,
        );

        assert_exp(
            &environment,
            ExpEnum::Symbol(
                "NA2",
                SymLoc::Ref(Binding::with_expression(ExpEnum::Int(3).into())),
            )
            .into(),
            3,
        );
        let ns_a = build_new_namespace(&mut environment, "ns-a")?;
        ns_a.borrow_mut().insert("a1", ExpEnum::Int(11).into());
        assert_exp(
            &environment,
            ExpEnum::Symbol("NA2", SymLoc::Namespace(ns_a.clone(), 1)).into(),
            11,
        );

        Ok(())
    }
}
