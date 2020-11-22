use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::fmt;
use std::io;
use std::process::Child;
use std::rc::Rc;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use liner::Context;

use crate::analyze::*;
use crate::builtins::add_builtins;
use crate::builtins_bind::add_bind_builtins;
use crate::builtins_edit::add_edit_builtins;
use crate::builtins_file::add_file_builtins;
use crate::builtins_hashmap::add_hash_builtins;
use crate::builtins_io::add_io_builtins;
use crate::builtins_math::add_math_builtins;
use crate::builtins_namespace::add_namespace_builtins;
use crate::builtins_pair::add_pair_builtins;
use crate::builtins_str::add_str_builtins;
use crate::builtins_system::add_system_builtins;
use crate::builtins_types::add_type_builtins;
use crate::builtins_values::add_values_builtins;
use crate::builtins_vector::add_vec_builtins;
use crate::interner::*;
use crate::process::*;
use crate::types::*;

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
pub enum IOState {
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
            is_spawn: false,
            pipe_pgid: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ReaderState {
    pub line: usize,
    pub column: usize,
    pub file_name: Option<&'static str>,
    pub end_ch: Option<&'static str>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FormType {
    Any,
    FormOnly,
    ExternalOnly,
}

#[derive(Clone, Debug)]
pub struct Namespace {
    map: HashMap<&'static str, usize>,
    data: Vec<Expression>,
    doc_strings: Vec<Option<String>>,
    outer: Option<Rc<RefCell<Namespace>>>,
    name: &'static str,
    free_list: Vec<usize>,
}

impl Namespace {
    pub fn new_with_outer(name: &'static str, outer: Option<Rc<RefCell<Namespace>>>) -> Namespace {
        Namespace {
            map: HashMap::new(),
            data: Vec::new(),
            doc_strings: Vec::new(),
            outer,
            name,
            free_list: Vec::new(),
        }
    }

    fn new_root(interner: &mut Interner) -> Self {
        let mut data: HashMap<&'static str, (Expression, String)> = HashMap::new();
        add_builtins(interner, &mut data);
        add_system_builtins(interner, &mut data);
        add_math_builtins(interner, &mut data);
        add_str_builtins(interner, &mut data);
        add_vec_builtins(interner, &mut data);
        add_values_builtins(interner, &mut data);
        add_edit_builtins(interner, &mut data);
        add_file_builtins(interner, &mut data);
        add_io_builtins(interner, &mut data);
        add_pair_builtins(interner, &mut data);
        add_hash_builtins(interner, &mut data);
        add_type_builtins(interner, &mut data);
        add_namespace_builtins(interner, &mut data);
        add_bind_builtins(interner, &mut data);
        data.insert(
            interner.intern("*stdin*"),
            (
                ExpEnum::File(Rc::new(RefCell::new(FileState::Stdin))).into(),
                    "Usage: (read-line *stdin*)

File that connects to standard in by default.

Can be used in place of a read file object in any form that takes one.

Section: shell

Example:
(def stdin-test (open \"/tmp/sl-sh.stdin.test\" :create :truncate))
(write-line stdin-test \"Test line\")
(close stdin-test)
; Use a file for stdin for test.
(dyn *stdin* (open \"/tmp/sl-sh.stdin.test\" :read) (do (test::assert-equal \"Test line\n\" (read-line *stdin*)) (close *stdin*)))
".to_string()),
        );
        data.insert(
            interner.intern("*stdout*"),
            (
                ExpEnum::File(Rc::new(RefCell::new(FileState::Stdout))).into(),
                    "Usage: (write-line *stdout*)

File that connects to standard out by default.

Can be used in place of a write file object in any form that takes one.  Used
as the default for print and println.

Section: shell

Example:
; Use a file for stdout for test.
(dyn *stdout* (open \"/tmp/sl-sh.stdout.test\" :create :truncate) (do (write-line *stdout* \"Test out\") (close *stdout*)))
(test::assert-equal \"Test out\n\" (read-line (open \"/tmp/sl-sh.stdout.test\" :read)))
".to_string()),
        );
        data.insert(
            interner.intern("*stderr*"),
                (ExpEnum::File(Rc::new(RefCell::new(FileState::Stderr))).into(),
                    "Usage: (write-line *stderr*)

File that connects to standard error by default.

Can be used in place of a write file object in any form that takes one.  Used
as the default for eprint and eprintln.

Section: shell

Example:
; Use a file for stderr for test.
(dyn *stderr* (open \"/tmp/sl-sh.stderr.test\" :create :truncate) (do (write-line *stderr* \"Test Error\") (close *stderr*)))
(test::assert-equal \"Test Error\n\" (read-line (open \"/tmp/sl-sh.stderr.test\" :read)))
".to_string()),
        );
        data.insert(
            interner.intern("*ns*"),
            (
                ExpEnum::String(interner.intern("root").into(), None).into(),
                "Usage: (print *ns*)

Symbol that contains the name of the current namespace.

Can be used anywhere a symbol pointing to a string is valid.

Example:
(ns-push 'root)
(test::assert-equal \"root\" *ns*)
(ns-pop)
t
"
                .to_string(),
            ),
        );
        let mut vdata = Vec::with_capacity(data.len());
        let mut vdocs = Vec::with_capacity(data.len());
        let mut map = HashMap::new();
        for (k, (exp, doc_str)) in data.drain() {
            vdata.push(exp);
            if doc_str.is_empty() {
                vdocs.push(None);
            } else {
                vdocs.push(Some(doc_str));
            }
            map.insert(k, vdata.len() - 1);
        }
        Namespace {
            map,
            data: vdata,
            doc_strings: vdocs,
            outer: None,
            name: interner.intern("root"),
            free_list: Vec::new(),
        }
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.map.contains_key(key)
    }

    pub fn keys(&self) -> std::collections::hash_map::Keys<'_, &'static str, usize> {
        self.map.keys()
    }

    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn outer(&self) -> Option<Rc<RefCell<Namespace>>> {
        self.outer.clone()
    }

    pub fn get(&self, key: &str) -> Option<&Expression> {
        if let Some(idx) = self.map.get(key) {
            if let Some(reference) = self.data.get(*idx) {
                Some(reference)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn get_docs(&self, key: &str) -> Option<&str> {
        if let Some(idx) = self.map.get(key) {
            if let Some(docs) = self.doc_strings.get(*idx) {
                if let Some(docs) = docs {
                    Some(docs)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn get_idx(&self, idx: usize) -> Option<Expression> {
        if let Some(reference) = self.data.get(idx) {
            Some(reference.clone())
        } else {
            None
        }
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.data.clear();
    }

    pub fn remove(&mut self, key: &str) -> Option<Expression> {
        if let Some(idx) = self.map.remove(key) {
            self.free_list.push(idx);
            self.data.push(Expression::make_nil());
            self.doc_strings.push(None);
            self.doc_strings.swap_remove(idx);
            return Some(self.data.swap_remove(idx));
        }
        None
    }

    pub fn update_entry(
        &mut self,
        key: &str,
        val: Expression,
        doc_str: Option<String>,
    ) -> Result<Expression, LispError> {
        if let Some(idx) = self.map.get(key) {
            if let Some(docs) = self.doc_strings.get_mut(*idx) {
                if doc_str.is_some() {
                    *docs = doc_str;
                }
            }
            if let Some(entry) = self.data.get_mut(*idx) {
                *entry = val;
                return Ok(entry.clone());
            }
        }
        Err(LispError::new(format!("update, key not found {}", key)))
    }

    pub fn insert(&mut self, key: &'static str, exp: Expression) {
        self.insert_with_doc(key, exp, None)
    }

    pub fn insert_exp_data(&mut self, key: &'static str, data: ExpEnum) {
        let exp: Expression = data.into();
        // XXX verify this but is making a ref should have been analyzed.
        exp.get_mut().analyzed = true;
        self.insert(key, exp);
    }

    pub fn insert_with_doc(
        &mut self,
        key: &'static str,
        exp: Expression,
        doc_string: Option<String>,
    ) {
        if let Some(idx) = self.free_list.pop() {
            self.data.push(exp);
            self.data.swap_remove(idx);
            self.doc_strings.push(doc_string);
            self.doc_strings.swap_remove(idx);
            self.map.insert(key, idx);
        } else {
            self.data.push(exp);
            self.doc_strings.push(doc_string);
            self.map.insert(key, self.data.len() - 1);
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
    index: usize,
    symbols: Symbols,
}

//#[derive(Clone, Debug)]
pub struct Environment {
    // Set to true when a SIGINT (ctrl-c) was received, lets long running stuff die.
    pub sig_int: Arc<AtomicBool>,
    pub state: EnvState,
    pub stack: Vec<Expression>,
    pub stack_frames: Vec<StackFrame>,
    pub reader_state: Option<ReaderState>,
    pub stopped_procs: Rc<RefCell<Vec<u32>>>,
    pub jobs: Rc<RefCell<Vec<Job>>>,
    pub in_pipe: bool,
    pub run_background: bool,
    pub is_tty: bool,
    pub do_job_control: bool,
    pub loose_symbols: bool,
    pub str_ignore_expand: bool,
    pub procs: Rc<RefCell<HashMap<u32, Child>>>,
    pub data_in: Option<Expression>,
    pub form_type: FormType,
    pub save_exit_status: bool,
    // If this is Some then need to unwind and exit with then provided code (exit was called).
    pub exit_code: Option<i32>,
    // This is the dynamic bindings.  These take precidence over the other
    // bindings.
    pub dynamic_scope: HashMap<&'static str, Expression>,
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
    pub syms: Option<Symbols>,
    pub next_lex_id: usize,
}

impl Environment {
    pub fn insert_into_root_scope(&mut self, symbol: &'static str, data: Expression) {
        self.root_scope.borrow_mut().insert(symbol, data);
    }
}

pub fn build_default_environment(sig_int: Arc<AtomicBool>) -> Environment {
    let procs: Rc<RefCell<HashMap<u32, Child>>> = Rc::new(RefCell::new(HashMap::new()));
    let mut interner = Interner::with_capacity(8192);
    let root_scope = Rc::new(RefCell::new(Namespace::new_root(&mut interner)));
    let namespace = root_scope.clone();
    let mut namespaces = HashMap::new();
    namespaces.insert(interner.intern("root"), root_scope.clone());
    Environment {
        sig_int,
        state: EnvState::default(),
        stack: Vec::with_capacity(1024),
        stack_frames: Vec::with_capacity(500),
        reader_state: None,
        stopped_procs: Rc::new(RefCell::new(Vec::new())),
        jobs: Rc::new(RefCell::new(Vec::new())),
        in_pipe: false,
        run_background: false,
        is_tty: true,
        do_job_control: true,
        loose_symbols: false,
        str_ignore_expand: false,
        procs,
        data_in: None,
        form_type: FormType::Any,
        save_exit_status: true,
        exit_code: None,
        dynamic_scope: HashMap::new(),
        root_scope,
        namespace,
        namespaces,
        allow_lazy_fn: true,
        return_val: None,
        interner,
        last_meta: None,
        repl_settings: ReplSettings::default(),
        liners: HashMap::new(),
        syms: None,
        next_lex_id: 1,
    }
}

pub fn build_new_scope(
    name: &'static str,
    outer: Option<Rc<RefCell<Namespace>>>,
) -> Rc<RefCell<Namespace>> {
    let map = HashMap::new();
    let data = Vec::new();
    let doc_strings = Vec::new();
    Rc::new(RefCell::new(Namespace {
        map,
        data,
        doc_strings,
        outer,
        name,
        free_list: Vec::new(),
    }))
}

pub fn stack_push_data(environment: &mut Environment, data: ExpEnum) {
    environment.stack.push(data.into());
}

pub fn stack_push_exp(environment: &mut Environment, exp: Expression) {
    environment.stack.push(exp);
}

pub fn build_new_namespace(
    environment: &mut Environment,
    name: &str,
) -> Result<Rc<RefCell<Namespace>>, String> {
    if environment.namespaces.contains_key(name) {
        let msg = format!("Namespace {} already exists!", name);
        Err(msg)
    } else {
        let name = environment.interner.intern(name);
        let mut map = HashMap::new();
        let mut data = Vec::new();
        let mut doc_strings = Vec::new();
        data.push(ExpEnum::String(name.into(), None).into());
        doc_strings.push(None);
        map.insert(environment.interner.intern("*ns*"), 0);
        let scope = Namespace {
            map,
            data,
            doc_strings,
            outer: Some(environment.root_scope.clone()),
            name,
            free_list: Vec::new(),
        };
        let scope = Rc::new(RefCell::new(scope));
        environment.namespaces.insert(name, scope.clone());
        Ok(scope)
    }
}

pub fn get_from_namespace(environment: &Environment, key: &str) -> Option<Expression> {
    let mut loop_scope = Some(environment.namespace.clone());
    while let Some(scope) = loop_scope {
        if let Some(exp) = scope.borrow().get(key) {
            return Some(exp.clone());
        }
        loop_scope = scope.borrow().outer.clone();
    }
    None
}

pub fn lookup_expression(environment: &Environment, key: &str) -> Option<Expression> {
    // First check any "local" lexical scopes.
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
    /*let mut loop_scope = if !environment.scopes.is_empty() {
        Some(environment.scopes.last().unwrap().clone())
    } else {
        None
    };
    let mut namespace = None;
    // First check any "local" lexical scopes.
    while let Some(scope_outer) = loop_scope {
        let scope = scope_outer.borrow_mut();
        // Stop when we get to the underlying namespace- handle those afer dynamics.
        if scope.name.is_some() {
            namespace = Some(scope_outer.clone());
            break;
        }
        if let Some(reference) = scope.get(key) {
            return Some(reference.clone());
        }
        loop_scope = scope.outer.clone();
    }*/
    // Then check dynamic scope.
    if let Some(reference) = environment.dynamic_scope.get(key) {
        Some(reference.clone())
    // Check for namespaced symbols.
    } else if key.contains("::") {
        // namespace reference.
        let mut key_i = key.splitn(2, "::");
        if let Some(namespace) = key_i.next() {
            if let Some(scope) = environment.namespaces.get(namespace) {
                if let Some(key) = key_i.next() {
                    // Do not let it sneak past a dynamic binding!
                    if let Some(reference) = environment.dynamic_scope.get(key) {
                        return Some(reference.clone());
                    } else if let Some(reference) = scope.borrow().get(key) {
                        return Some(reference.clone());
                    }
                }
            }
        }
        None
    // Then check the namespace. Note, use the namespace from lexical scope if available.
    /*    } else if let Some(namespace) = namespace {
    if let Some(reference) = namespace.borrow().get(key) {
        Some(reference.clone())
    } else if let Some(reference) = environment.root_scope.borrow().get(key) {
        Some(reference.clone())
    } else {
        None
    }*/
    // If no namespace from lexical scope use the default.
    } else if let Some(reference) = environment.namespace.borrow().get(key) {
        Some(reference.clone())
    // Finally check root (this might be a duplicate if in root but in that case about give up anyway).
    } else if let Some(reference) = environment.root_scope.borrow().get(key) {
        Some(reference.clone())
    } else {
        None
    }
}

pub fn get_expression(environment: &Environment, expression: Expression) -> Option<Expression> {
    match &expression.get().data {
        ExpEnum::Symbol(sym, location) => match location {
            SymLoc::None => lookup_expression(environment, sym),
            SymLoc::Ref(r) => Some(r.clone()),
            SymLoc::Namespace(scope, idx) => scope.borrow().get_idx(*idx),
            SymLoc::Stack(idx) => {
                if let Some(r) = environment.stack.get(*idx) {
                    Some(r.clone())
                } else {
                    None
                }
            }
        },
        _ => None, // XXX Maybe this should be an error?
    }
}

pub fn is_expression(environment: &Environment, key: &str) -> bool {
    // XXX TODO- check where this is called, making key an Expression is probably better.
    if key.starts_with('$') {
        env::var(&key[1..]).is_ok()
    } else {
        lookup_expression(environment, key).is_some()
    }
}

pub fn get_symbol_namespaces(environment: &Environment, key: &str) -> Vec<Rc<RefCell<Namespace>>> {
    let mut ret: Vec<Rc<RefCell<Namespace>>> = Vec::new();
    for namespace in environment.namespaces.values() {
        if namespace.borrow().map.contains_key(key) {
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
