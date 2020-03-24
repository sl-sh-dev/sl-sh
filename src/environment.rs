use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fmt;
use std::io;
use std::mem;
use std::process::Child;
use std::rc::Rc;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;

use crate::builtins::add_builtins;
use crate::builtins_file::add_file_builtins;
use crate::builtins_hashmap::add_hash_builtins;
use crate::builtins_io::add_io_builtins;
use crate::builtins_math::add_math_builtins;
use crate::builtins_namespace::add_namespace_builtins;
use crate::builtins_pair::add_pair_builtins;
use crate::builtins_str::add_str_builtins;
use crate::builtins_types::add_type_builtins;
use crate::builtins_vector::add_vec_builtins;
use crate::process::*;
use crate::types::*;

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

#[derive(Clone, Debug)]
pub struct Scope {
    pub data: HashMap<&'static str, Rc<Reference>>,
    pub outer: Option<Rc<RefCell<Scope>>>,
    // If this scope is a namespace it will have a name otherwise it will be None.
    pub name: Option<&'static str>,
}

impl Scope {
    fn new_root(interner: &mut Interner) -> Self {
        let mut data: HashMap<&'static str, Rc<Reference>> = HashMap::new();
        add_builtins(interner, &mut data);
        add_math_builtins(interner, &mut data);
        add_str_builtins(interner, &mut data);
        add_vec_builtins(interner, &mut data);
        add_file_builtins(interner, &mut data);
        add_io_builtins(interner, &mut data);
        add_pair_builtins(interner, &mut data);
        add_hash_builtins(interner, &mut data);
        add_type_builtins(interner, &mut data);
        add_namespace_builtins(interner, &mut data);
        let root = interner.intern("root");
        data.insert(
            interner.intern("*stdin*"),
            Rc::new(Reference {
                exp: Expression::File(Rc::new(RefCell::new(FileState::Stdin))),
                meta: RefMetaData {
                    namespace: Some(root),
                    doc_string: Some("Usage: (read-line *stdin*)

File that connects to standard in by default.

Can be used in place of a read file object in any form that takes one.

Example:
(def 'stdin-test (open \"/tmp/sl-sh.stdin.test\" :create :truncate))
(write-line stdin-test \"Test line\")
(close stdin-test)
; Use a file for stdin for test.
(dyn '*stdin* (open \"/tmp/sl-sh.stdin.test\" :read) (test::assert-equal \"Test line\n\" (read-line *stdin*)))
".to_string()),
                },
            }),
        );
        data.insert(
            interner.intern("*stdout*"),
            Rc::new(Reference {
                exp: Expression::File(Rc::new(RefCell::new(FileState::Stdout))),
                meta: RefMetaData {
                    namespace: Some(root),
                    doc_string: Some("Usage: (write-line *stdout*)

File that connects to standard out by default.

Can be used in place of a write file object in any form that takes one.  Used
as the default for print and println.

Example:
; Use a file for stdout for test.
(dyn '*stdout* (open \"/tmp/sl-sh.stdout.test\" :create :truncate) (write-line *stdout* \"Test out\"))
(test::assert-equal \"Test out\n\" (read-line (open \"/tmp/sl-sh.stdout.test\" :read)))
".to_string()),
                },
            }),
        );
        data.insert(
            interner.intern("*stderr*"),
            Rc::new(Reference {
                exp: Expression::File(Rc::new(RefCell::new(FileState::Stderr))),
                meta: RefMetaData {
                    namespace: Some(root),
                    doc_string: Some("Usage: (write-line *stderr*)

File that connects to standard error by default.

Can be used in place of a write file object in any form that takes one.  Used
as the default for eprint and eprintln.

Example:
; Use a file for stderr for test.
(dyn '*stderr* (open \"/tmp/sl-sh.stderr.test\" :create :truncate) (write-line *stderr* \"Test Error\"))
(test::assert-equal \"Test Error\n\" (read-line (open \"/tmp/sl-sh.stderr.test\" :read)))
".to_string()),
                },
            }),
        );
        data.insert(
            interner.intern("*ns*"),
            Rc::new(Reference {
                exp: Expression::Atom(Atom::StringRef(interner.intern("root"))),
                meta: RefMetaData {
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
            }),
        );
        Scope {
            data,
            outer: None,
            name: Some(interner.intern("root")),
        }
    }

    pub fn with_data<S: ::std::hash::BuildHasher>(
        environment: Option<&Environment>,
        mut data_in: HashMap<&'static str, Rc<Reference>, S>,
    ) -> Scope {
        let mut data: HashMap<&'static str, Rc<Reference>> = HashMap::with_capacity(data_in.len());
        for (k, v) in data_in.drain() {
            data.insert(k, v);
        }
        let outer = if let Some(environment) = environment {
            if let Some(scope) = environment.current_scope.last() {
                Some(scope.clone())
            } else {
                None
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
        let reference = Rc::new(Reference {
            exp,
            meta: RefMetaData {
                namespace: self.name,
                doc_string: None,
            },
        });
        self.data.insert(key, reference);
    }

    pub fn insert_exp_with_doc(
        &mut self,
        key: &'static str,
        exp: Expression,
        doc_string: Option<String>,
    ) {
        let reference = Rc::new(Reference {
            exp,
            meta: RefMetaData {
                namespace: self.name,
                doc_string,
            },
        });
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

// This interner is initially based on this: https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html
// Inspiration also from: https://github.com/CAD97/simple-interner/blob/master/src/interner.rs
// See https://www.reddit.com/r/rust/comments/fn1jxf/blog_post_fast_and_simple_rust_interner/
// This is a simple string interner.  It hands out &'static str and it WILL leak memory
// to keep them valid.  Intended to live for the programs lifetime.
#[derive(Clone, Debug)]
pub struct Interner {
    map: HashSet<&'static str>,
    // Leak buffers to keep the static lifetimes we hand out valid.
    buf: mem::ManuallyDrop<String>,
}

impl Interner {
    pub fn with_capacity(cap: usize) -> Interner {
        let cap = cap.next_power_of_two();
        Interner {
            map: HashSet::default(),
            buf: mem::ManuallyDrop::new(String::with_capacity(cap)),
        }
    }

    pub fn contains(&self, name: &str) -> bool {
        self.map.contains(name)
    }

    pub fn intern(&mut self, name: &str) -> &'static str {
        if let Some(&id) = self.map.get(name) {
            return id;
        }
        let name = {
            let cap = self.buf.capacity();
            if cap < self.buf.len() + name.len() {
                let new_cap = (cap.max(name.len()) + 1).next_power_of_two();
                let new_buf = mem::ManuallyDrop::new(String::with_capacity(new_cap));
                // Leak memory to keep the static lifetimes valid.
                let _old_buf = mem::replace(&mut self.buf, new_buf);
            }

            let start = self.buf.len();
            self.buf.push_str(name);
            unsafe { &*(&self.buf[start..] as *const str) }
        };
        self.map.insert(name);
        name
    }
}

#[derive(Clone, Debug)]
pub struct Job {
    pub pids: Vec<u32>,
    pub names: Vec<String>,
    pub status: JobStatus,
}

#[derive(Clone, Debug)]
pub struct Environment {
    // Set to true when a SIGINT (ctrl-c) was received, lets long running stuff die.
    pub sig_int: Arc<AtomicBool>,
    pub state: EnvState,
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
    pub stack_on_error: bool,
    pub error_expression: Option<Expression>,
    // If this is Some then need to unwind and exit with then provided code (exit was called).
    pub exit_code: Option<i32>,
    // This is the dynamic bindings.  These take precidence over the other
    // bindings.
    pub dynamic_scope: HashMap<&'static str, Rc<Expression>>,
    // This is the environment's root (global scope), it will also be part of
    // higher level scopes and in the current_scope vector (the first item).
    // It's special so keep a reference here as well for handy access.
    pub root_scope: Rc<RefCell<Scope>>,
    // Use as a stack of scopes, entering a new pushes and it gets popped on exit
    // The actual lookups are done using the scope and it's outer chain NOT this stack.
    pub current_scope: Vec<Rc<RefCell<Scope>>>,
    // Map of all the created namespaces.
    pub namespaces: HashMap<&'static str, Rc<RefCell<Scope>>>,
    // Allow lazy functions (i.e. enable TCO).
    pub allow_lazy_fn: bool,
    // Used for block/return-from
    pub return_val: Option<(Option<&'static str>, Expression)>,
    // Interner for symbols and some strings.
    pub interner: Interner,
}

pub fn build_default_environment(sig_int: Arc<AtomicBool>) -> Environment {
    let procs: Rc<RefCell<HashMap<u32, Child>>> = Rc::new(RefCell::new(HashMap::new()));
    let mut interner = Interner::with_capacity(102_400);
    let root_scope = Rc::new(RefCell::new(Scope::new_root(&mut interner)));
    let mut current_scope = Vec::new();
    current_scope.push(root_scope.clone());
    let mut namespaces = HashMap::new();
    namespaces.insert(interner.intern("root"), root_scope.clone());
    Environment {
        sig_int,
        state: EnvState::default(),
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
        stack_on_error: false,
        error_expression: None,
        exit_code: None,
        dynamic_scope: HashMap::new(),
        root_scope,
        current_scope,
        namespaces,
        allow_lazy_fn: true,
        return_val: None,
        interner,
    }
}

pub fn build_new_spawn_scope<S: ::std::hash::BuildHasher>(
    mut data_in: HashMap<&'static str, Reference, S>,
    sig_int: Arc<AtomicBool>,
) -> Environment {
    let procs: Rc<RefCell<HashMap<u32, Child>>> = Rc::new(RefCell::new(HashMap::new()));
    let mut state = EnvState::default();
    let mut data: HashMap<&'static str, Rc<Reference>> = HashMap::with_capacity(data_in.len());
    let mut interner = Interner::with_capacity(102_400);
    data.insert(
        interner.intern("*ns*"),
        Rc::new(Reference {
            exp: Expression::Atom(Atom::StringRef(interner.intern("root"))),
            meta: RefMetaData {
                namespace: Some(interner.intern("root")),
                doc_string: None,
            },
        }),
    );
    for (k, v) in data_in.drain() {
        data.insert(k, Rc::new(v));
    }
    state.is_spawn = true;
    let root_scope = Rc::new(RefCell::new(Scope::with_data(None, data)));
    let mut current_scope = Vec::new();
    current_scope.push(root_scope.clone());
    let mut namespaces = HashMap::new();
    namespaces.insert(interner.intern("root"), root_scope.clone());
    Environment {
        sig_int,
        state,
        stopped_procs: Rc::new(RefCell::new(Vec::new())),
        jobs: Rc::new(RefCell::new(Vec::new())),
        in_pipe: false,
        run_background: false,
        is_tty: false,
        do_job_control: false,
        loose_symbols: false,
        str_ignore_expand: false,
        procs,
        data_in: None,
        form_type: FormType::Any,
        save_exit_status: true,
        stack_on_error: false,
        error_expression: None,
        exit_code: None,
        dynamic_scope: HashMap::new(),
        root_scope,
        current_scope,
        namespaces,
        allow_lazy_fn: true,
        return_val: None,
        interner,
    }
}

pub fn build_new_scope(outer: Option<Rc<RefCell<Scope>>>) -> Rc<RefCell<Scope>> {
    let data: HashMap<&'static str, Rc<Reference>> = HashMap::new();
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
        let mut data: HashMap<&'static str, Rc<Reference>> = HashMap::new();
        data.insert(
            environment.interner.intern("*ns*"),
            Rc::new(Reference {
                exp: Expression::Atom(Atom::StringRef(name)),
                meta: RefMetaData {
                    namespace: Some(name),
                    doc_string: None,
                },
            }),
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
        let v = &**v;
        data_in.insert(k, v.clone());
    }
    if let Some(outer) = &scope.outer {
        clone_symbols(&outer.borrow(), data_in);
    }
}

pub fn get_expression(environment: &Environment, key: &str) -> Option<Rc<Reference>> {
    if key.starts_with('$') || key.starts_with(':') {
        // Can not lookup env vars or keywords...
        None
    } else if let Some(exp) = environment.dynamic_scope.get(key) {
        let new_exp = &*exp.clone();
        // WTF is going on here, should not need a clone of a clone...
        let reference = Rc::new(Reference {
            exp: new_exp.clone(),
            meta: RefMetaData {
                namespace: None,
                doc_string: None,
            },
        });
        Some(reference)
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
        let mut loop_scope = Some(environment.current_scope.last().unwrap().clone());
        while let Some(scope) = loop_scope {
            if let Some(exp) = scope.borrow().data.get(key) {
                return Some(exp.clone());
            }
            loop_scope = scope.borrow().outer.clone();
        }
        None
    }
}

pub fn set_expression_current(
    environment: &mut Environment,
    key: &'static str,
    doc_str: Option<String>,
    expression: Expression,
) {
    let mut current_scope = environment
        .current_scope
        .last()
        .unwrap() // Always has at least root scope unless horribly broken.
        .borrow_mut();
    let reference = Rc::new(Reference {
        exp: expression,
        meta: RefMetaData {
            namespace: current_scope.name,
            doc_string: doc_str,
        },
    });
    current_scope.data.insert(key, reference);
}

pub fn set_expression_current_ref(
    environment: &mut Environment,
    key: &'static str,
    reference: Rc<Reference>,
) {
    let mut current_scope = environment
        .current_scope
        .last()
        .unwrap() // Always has at least root scope unless horribly broken.
        .borrow_mut();
    current_scope.data.insert(key, reference);
}

pub fn remove_expression_current(environment: &mut Environment, key: &str) {
    environment
        .current_scope
        .last()
        .unwrap() // Always has at least root scope unless horribly broken.
        .borrow_mut()
        .data
        .remove(key);
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
        let mut loop_scope = Some(environment.current_scope.last().unwrap().clone());
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
