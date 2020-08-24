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
use crate::gc::*;
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
pub struct RefMetaData {
    pub namespace: Option<&'static str>,
    pub doc_string: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Reference {
    pub exp: Expression,
    pub meta: RefMetaData,
}

impl Reference {
    pub fn new(exp: ExpEnum, meta: RefMetaData) -> Reference {
        let root = gc_mut().insert(ExpObj {
            data: exp,
            meta: None,
            meta_tags: None,
        });
        Reference {
            exp: Expression::new(root),
            meta,
        }
    }

    pub fn new_rooted(exp: Expression, meta: RefMetaData) -> Reference {
        let exp = exp.clone_root();
        Reference { exp, meta }
    }
}

#[derive(Clone, Debug)]
pub struct Scope {
    pub data: HashMap<&'static str, Reference>,
    pub outer: Option<Rc<RefCell<Scope>>>,
    // If this scope is a namespace it will have a name otherwise it will be None.
    pub name: Option<&'static str>,
}

impl Scope {
    fn new_root(interner: &mut Interner) -> Self {
        let mut data: HashMap<&'static str, Reference> = HashMap::new();
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
        let root = interner.intern("root");
        data.insert(
            interner.intern("*stdin*"),
            Reference::new(
                ExpEnum::File(Rc::new(RefCell::new(FileState::Stdin))),
                RefMetaData {
                    namespace: Some(root),
                    doc_string: Some("Usage: (read-line *stdin*)

File that connects to standard in by default.

Can be used in place of a read file object in any form that takes one.

Section: shell

Example:
(def 'stdin-test (open \"/tmp/sl-sh.stdin.test\" :create :truncate))
(write-line stdin-test \"Test line\")
(close stdin-test)
; Use a file for stdin for test.
(dyn '*stdin* (open \"/tmp/sl-sh.stdin.test\" :read) (do (test::assert-equal \"Test line\n\" (read-line *stdin*)) (close *stdin*)))
".to_string()),
                },
            ),
        );
        data.insert(
            interner.intern("*stdout*"),
            Reference::new(
                ExpEnum::File(Rc::new(RefCell::new(FileState::Stdout))),
                RefMetaData {
                    namespace: Some(root),
                    doc_string: Some("Usage: (write-line *stdout*)

File that connects to standard out by default.

Can be used in place of a write file object in any form that takes one.  Used
as the default for print and println.

Section: shell

Example:
; Use a file for stdout for test.
(dyn '*stdout* (open \"/tmp/sl-sh.stdout.test\" :create :truncate) (do (write-line *stdout* \"Test out\") (close *stdout*)))
(test::assert-equal \"Test out\n\" (read-line (open \"/tmp/sl-sh.stdout.test\" :read)))
".to_string()),
                },
            ),
        );
        data.insert(
            interner.intern("*stderr*"),
            Reference::new(
                ExpEnum::File(Rc::new(RefCell::new(FileState::Stderr))),
                RefMetaData {
                    namespace: Some(root),
                    doc_string: Some("Usage: (write-line *stderr*)

File that connects to standard error by default.

Can be used in place of a write file object in any form that takes one.  Used
as the default for eprint and eprintln.

Section: shell

Example:
; Use a file for stderr for test.
(dyn '*stderr* (open \"/tmp/sl-sh.stderr.test\" :create :truncate) (do (write-line *stderr* \"Test Error\") (close *stderr*)))
(test::assert-equal \"Test Error\n\" (read-line (open \"/tmp/sl-sh.stderr.test\" :read)))
".to_string()),
                },
            ),
        );
        data.insert(
            interner.intern("*ns*"),
            Reference::new(
                ExpEnum::Atom(Atom::String(interner.intern("root").into(), None)),
                RefMetaData {
                    namespace: Some(root),
                    doc_string: Some(
                        "Usage: (print *ns*)

Symbol that contains the name of the current namespace.

Can be used anywhere a symbol pointing to a string is valid.

Example:
(ns-enter 'root)
(test::assert-equal \"root\" *ns*)
(ns-pop)
t
"
                        .to_string(),
                    ),
                },
            ),
        );
        Scope {
            data,
            outer: None,
            name: Some(interner.intern("root")),
        }
    }

    pub fn with_data<S: ::std::hash::BuildHasher>(
        environment: Option<&Environment>,
        mut data_in: HashMap<&'static str, Reference, S>,
    ) -> Scope {
        let mut data: HashMap<&'static str, Reference> = HashMap::with_capacity(data_in.len());
        for (k, v) in data_in.drain() {
            data.insert(k, v);
        }
        let outer = if let Some(environment) = environment {
            if let Some(scope) = environment.scopes.last() {
                Some(scope.clone())
            } else {
                Some(environment.namespace.clone())
            }
        } else {
            None
        };
        Scope {
            data,
            outer,
            name: None,
        }
    }

    pub fn insert_exp(&mut self, key: &'static str, exp: Expression) {
        let reference = Reference::new_rooted(
            exp,
            RefMetaData {
                namespace: self.name,
                doc_string: None,
            },
        );
        self.data.insert(key, reference);
    }

    pub fn insert_exp_data(&mut self, key: &'static str, data: ExpEnum) {
        let reference = Reference::new(
            data,
            RefMetaData {
                namespace: self.name,
                doc_string: None,
            },
        );
        self.data.insert(key, reference);
    }

    pub fn insert_exp_with_doc(
        &mut self,
        key: &'static str,
        exp: Expression,
        doc_string: Option<String>,
    ) {
        let reference = Reference::new_rooted(
            exp,
            RefMetaData {
                namespace: self.name,
                doc_string,
            },
        );
        self.data.insert(key, reference);
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

//#[derive(Clone, Debug)]
pub struct Environment {
    // Set to true when a SIGINT (ctrl-c) was received, lets long running stuff die.
    pub sig_int: Arc<AtomicBool>,
    pub state: EnvState,
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
    pub dynamic_scope: HashMap<&'static str, Reference>,
    // This is the environment's root (global scope), it will also be part of
    // higher level scopes and in the current_scope vector (the first item).
    // It's special so keep a reference here as well for handy access.
    pub root_scope: Rc<RefCell<Scope>>,
    // This is the current namespace's scope.  Namespace scopes are NOT pushed onto scopes.
    pub namespace: Rc<RefCell<Scope>>,
    // Use as a stack of scopes, entering a new pushes and it gets popped on exit
    // The actual lookups are done using the scope and it's outer chain NOT this stack.
    pub scopes: Vec<Rc<RefCell<Scope>>>,
    // Map of all the created namespaces.
    pub namespaces: HashMap<&'static str, Rc<RefCell<Scope>>>,
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
}

impl Environment {
    pub fn insert_into_root_scope(&mut self, symbol: &'static str, data: Expression) {
        self.root_scope.borrow_mut().insert_exp(symbol, data);
    }
}

pub fn build_default_environment(sig_int: Arc<AtomicBool>) -> Environment {
    let procs: Rc<RefCell<HashMap<u32, Child>>> = Rc::new(RefCell::new(HashMap::new()));
    let mut interner = Interner::with_capacity(8192);
    let root_scope = Rc::new(RefCell::new(Scope::new_root(&mut interner)));
    root_scope.borrow_mut().data.insert(
        interner.intern("*last-ns*"),
        Reference::new(
            ExpEnum::Atom(Atom::String("root".into(), None)),
            RefMetaData {
                namespace: None,
                doc_string: None,
            },
        ),
    );
    let namespace = root_scope.clone();
    let scopes = Vec::new();
    let mut namespaces = HashMap::new();
    namespaces.insert(interner.intern("root"), root_scope.clone());
    Environment {
        sig_int,
        state: EnvState::default(),
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
        scopes,
        namespaces,
        allow_lazy_fn: true,
        return_val: None,
        interner,
        last_meta: None,
        repl_settings: ReplSettings::default(),
        liners: HashMap::new(),
    }
}

pub fn build_new_scope(outer: Option<Rc<RefCell<Scope>>>) -> Rc<RefCell<Scope>> {
    let data: HashMap<&'static str, Reference> = HashMap::new();
    Rc::new(RefCell::new(Scope {
        data,
        outer,
        name: None,
    }))
}

pub fn build_new_namespace(
    environment: &mut Environment,
    name: &str,
) -> Result<Rc<RefCell<Scope>>, String> {
    if environment.namespaces.contains_key(name) {
        let msg = format!("Namespace {} already exists!", name);
        Err(msg)
    } else {
        let name = environment.interner.intern(name);
        let mut data: HashMap<&'static str, Reference> = HashMap::new();
        data.insert(
            environment.interner.intern("*ns*"),
            Reference::new(
                ExpEnum::Atom(Atom::String(name.into(), None)),
                RefMetaData {
                    namespace: Some(name),
                    doc_string: None,
                },
            ),
        );
        let scope = Scope {
            data,
            outer: Some(environment.root_scope.clone()),
            name: Some(name),
        };
        let scope = Rc::new(RefCell::new(scope));
        environment.namespaces.insert(name, scope.clone());
        Ok(scope)
    }
}

pub fn clone_symbols<S: ::std::hash::BuildHasher>(
    scope: &Scope,
    data_in: &mut HashMap<&'static str, Reference, S>,
) {
    for (k, v) in &scope.data {
        //let v = &**v;
        data_in.insert(k, v.clone());
    }
    if let Some(outer) = &scope.outer {
        clone_symbols(&outer.borrow(), data_in);
    }
}

pub fn get_from_namespace(environment: &Environment, key: &str) -> Option<Reference> {
    let mut loop_scope = Some(environment.namespace.clone());
    while let Some(scope) = loop_scope {
        if let Some(exp) = scope.borrow().data.get(key) {
            return Some(exp.clone());
        }
        loop_scope = scope.borrow().outer.clone();
    }
    None
}

pub fn get_expression(environment: &Environment, key: &str) -> Option<Reference> {
    if key.starts_with('$') || key.starts_with(':') {
        // Can not lookup env vars or keywords...
        None
    } else if let Some(reference) = environment.dynamic_scope.get(key) {
        Some(reference.clone())
    } else if key.contains("::") {
        // namespace reference.
        let mut key_i = key.splitn(2, "::");
        if let Some(namespace) = key_i.next() {
            if let Some(scope) = environment.namespaces.get(namespace) {
                if let Some(key) = key_i.next() {
                    if let Some(exp) = scope.borrow().data.get(key) {
                        return Some(exp.clone());
                    }
                }
            }
        }
        None
    } else {
        let mut loop_scope = if !environment.scopes.is_empty() {
            Some(environment.scopes.last().unwrap().clone())
        } else {
            Some(environment.namespace.clone())
        };
        while let Some(scope) = loop_scope {
            if let Some(exp) = scope.borrow().data.get(key) {
                return Some(exp.clone());
            }
            loop_scope = scope.borrow().outer.clone();
        }
        None
    }
}

pub fn get_current_scope(environment: &mut Environment) -> Rc<RefCell<Scope>> {
    if !environment.scopes.is_empty() {
        environment.scopes.last().unwrap().clone()
    } else {
        environment.namespace.clone()
    }
}

pub fn set_expression_current(
    environment: &mut Environment,
    key: &'static str,
    doc_str: Option<String>,
    expression: Expression,
) {
    let current_scope = get_current_scope(environment);
    let mut current_scope = current_scope.borrow_mut();
    let reference = Reference::new_rooted(
        expression,
        RefMetaData {
            namespace: current_scope.name,
            doc_string: doc_str,
        },
    );
    current_scope.data.insert(key, reference);
}

pub fn set_expression_current_data(
    environment: &mut Environment,
    key: &'static str,
    doc_str: Option<String>,
    data: ExpEnum,
) {
    let current_scope = get_current_scope(environment);
    let mut current_scope = current_scope.borrow_mut();
    let reference = Reference::new(
        data,
        RefMetaData {
            namespace: current_scope.name,
            doc_string: doc_str,
        },
    );
    current_scope.data.insert(key, reference);
}

pub fn set_expression_current_namespace(
    environment: &mut Environment,
    key: &'static str,
    reference: Reference,
) {
    environment
        .namespace
        .borrow_mut()
        .data
        .insert(key, reference);
}

pub fn set_expression_current_ref(
    environment: &mut Environment,
    key: &'static str,
    reference: Reference,
) {
    get_current_scope(environment)
        .borrow_mut()
        .data
        .insert(key, reference);
}

pub fn remove_expression_current(environment: &mut Environment, key: &str) -> Option<Reference> {
    get_current_scope(environment).borrow_mut().data.remove(key)
}

pub fn is_expression(environment: &Environment, key: &str) -> bool {
    if key.starts_with('$') {
        env::var(&key[1..]).is_ok()
    } else {
        get_expression(environment, key).is_some()
    }
}

pub fn get_symbols_scope(environment: &Environment, key: &str) -> Option<Rc<RefCell<Scope>>> {
    // DO NOT return a namespace for a namespace key otherwise set will work to
    // set symbols in other namespaces.
    if !key.contains("::") {
        let mut loop_scope = if !environment.scopes.is_empty() {
            Some(environment.scopes.last().unwrap().clone())
        } else {
            Some(environment.namespace.clone())
        };
        while loop_scope.is_some() {
            let scope = loop_scope.unwrap();
            if let Some(_exp) = scope.borrow().data.get(key) {
                return Some(scope.clone());
            }
            loop_scope = scope.borrow().outer.clone();
        }
    }
    None
}

pub fn get_namespace(environment: &Environment, name: &str) -> Option<Rc<RefCell<Scope>>> {
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
