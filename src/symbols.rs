use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
use crate::environment::*;
use crate::interner::*;
use crate::types::*;

#[derive(Clone, Debug)]
pub struct Binding {
    expression: Rc<RefCell<Expression>>,
}

impl Binding {
    pub fn new() -> Binding {
        Binding::default()
    }

    pub fn with_expression(exp: Expression) -> Binding {
        Binding {
            expression: Rc::new(RefCell::new(exp)),
        }
    }

    pub fn get(&self) -> Expression {
        self.expression.borrow().clone()
    }

    pub fn replace(&self, exp: Expression) -> Expression {
        self.expression.replace(exp)
    }

    pub fn count(&self) -> usize {
        Rc::strong_count(&self.expression)
    }
}

impl Default for Binding {
    fn default() -> Self {
        Binding {
            expression: Rc::new(RefCell::new(ExpEnum::Undefined.into())),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SymbolsInt {
    pub syms: HashMap<&'static str, usize>,
    count: usize,
}

type Captures = Rc<RefCell<Vec<(&'static str, usize, Binding)>>>;

#[derive(Clone, Debug)]
pub struct Symbols {
    pub data: Rc<RefCell<SymbolsInt>>,
    lex_id: usize,
    lex_depth: u16,
    outer: Option<Rc<RefCell<Symbols>>>,
    namespace: Rc<RefCell<Namespace>>,
    captures: Captures,
}

impl Symbols {
    pub fn with_frame(environment: &mut Environment, syms: &Option<Symbols>) -> Symbols {
        let data = Rc::new(RefCell::new(SymbolsInt {
            syms: HashMap::new(),
            count: 0,
        }));
        let (lex_id, lex_depth, namespace, outer) = if let Some(lex_syms) = syms {
            (
                lex_syms.lex_id,
                lex_syms.lex_depth + 1,
                lex_syms.namespace.clone(),
                Some(Rc::new(RefCell::new(lex_syms.clone()))),
            )
        } else {
            let next_lex_id = environment.next_lex_id;
            environment.next_lex_id += 1;
            (next_lex_id, 0, environment.namespace.clone(), None)
        };
        Symbols {
            data,
            lex_id,
            lex_depth,
            outer,
            namespace,
            captures: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn dup(&self) -> Symbols {
        let self_captures = self.captures.borrow();
        let mut captures = Vec::with_capacity(self_captures.len());
        for c in &*self_captures {
            captures.push((c.0, c.1, Binding::new()));
        }
        Symbols {
            data: self.data.clone(),
            lex_id: self.lex_id,
            lex_depth: self.lex_depth,
            outer: self.outer.clone(),
            namespace: self.namespace.clone(),
            captures: Rc::new(RefCell::new(captures)),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.data.borrow().syms.is_empty()
    }

    pub fn len(&self) -> usize {
        self.data.borrow().syms.len()
    }

    pub fn lex_id(&self) -> usize {
        self.lex_id
    }

    pub fn lex_depth(&self) -> u16 {
        self.lex_depth
    }

    pub fn namespace(&self) -> &Rc<RefCell<Namespace>> {
        &self.namespace
    }

    pub fn contains_symbol(&self, key: &str) -> bool {
        self.data.borrow().syms.contains_key(key)
    }

    pub fn can_capture(&self, key: &'static str) -> bool {
        let mut loop_outer = self.outer.clone();
        while let Some(outer) = loop_outer {
            let outer = outer.borrow();
            if outer.contains_symbol(key) {
                return true;
            }
            loop_outer = outer.outer.clone();
        }
        false
    }

    pub fn get_capture_binding(&self, key: &str) -> Option<Binding> {
        for cap in &*self.captures.borrow() {
            if cap.0 == key {
                return Some(cap.2.clone());
            }
        }
        None
    }

    pub fn get(&self, key: &str) -> Option<usize> {
        self.data.borrow().syms.get(key).copied()
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

    pub fn insert_capture(&self, key: &'static str, environment: &mut Environment) -> usize {
        let mut data = self.data.borrow_mut();
        let count = data.count;
        data.syms.insert(key, count);
        data.count += 1;
        self.captures
            .borrow_mut()
            .push((key, count, Binding::new()));
        if let Some(outer) = &self.outer {
            // Also capture in outer lexical scope or bad things can happen.
            let outer = outer.borrow();
            if !outer.contains_symbol(key) {
                outer.insert_capture(key, environment);
            }
        }
        count
    }

    pub fn refresh_captures(&mut self, environment: &mut Environment) -> Result<(), LispError> {
        self.namespace = environment.namespace.clone();
        for cap in self.captures.borrow_mut().iter_mut() {
            if let Some(r) = capture_expression(environment, cap.0) {
                *cap = (cap.0, cap.1, r);
            } else {
                return Err(LispError::new(format!(
                    "captured variable {} not found",
                    cap.0
                )));
            }
        }
        Ok(())
    }

    pub fn len_captures(&self) -> usize {
        self.captures.borrow().len()
    }

    pub fn stack_captures(&self, environment: &mut Environment, base_idx: usize) {
        for (_s, idx, cap) in &*self.captures.borrow() {
            if let Some(c) = environment.stack.get_mut(base_idx + *idx) {
                *c = cap.clone();
            } else {
                panic!("Invalid stack, setting captures!");
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Namespace {
    map: HashMap<&'static str, usize>,
    data: Vec<Binding>,
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

    pub fn new_root(interner: &mut Interner) -> Self {
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

Section: root

Example:
(ns-push 'root)
;XXX TODO- analyzer breaks this test by patching *ns* to the namespace before the push... (test::assert-equal \"root\" *ns*)
(ns-pop)
t
"
                .to_string(),
            ),
        );
        data.insert(
            interner.intern("*read-table*"),
            (
                ExpEnum::HashMap(HashMap::new()).into(),
                "Usage: (print *read-table*)

Symbol that contains the current read table.

Section: root

Example:
;(hash-set! *read-table* #\\$ 'shell-read::shell-read)
t
"
                .to_string(),
            ),
        );
        data.insert(
            interner.intern("*read-table-terminal*"),
            (
                ExpEnum::HashMap(HashMap::new()).into(),
                "Usage: (print *read-table-terminal*)

Symbol that contains the current terminal read table.

Section: root

Example:
;(hash-set! *read-table-terminal* #\\] 'nop-read)
t
"
                .to_string(),
            ),
        );
        data.insert(
            interner.intern("*string-read-table*"),
            (
                ExpEnum::HashMap(HashMap::new()).into(),
                "Usage: (print *string-read-table*)

Symbol that contains the current string read table.

Section: root

Example:
;(hash-set! *string-read-table* #\\$ 'shell-read::shell-read)
t
"
                .to_string(),
            ),
        );
        let mut vdata = Vec::with_capacity(data.len());
        let mut vdocs = Vec::with_capacity(data.len());
        let mut map = HashMap::new();
        for (k, (exp, doc_str)) in data.drain() {
            vdata.push(Binding::with_expression(exp));
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

    pub fn get_with_outer(&self, key: &str) -> Option<Binding> {
        if let Some(binding) = self.get_binding(key) {
            return Some(binding);
        } else if let Some(out) = &self.outer {
            return out.borrow().get_with_outer(key);
        }
        None
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

    pub fn get(&self, key: &str) -> Option<Expression> {
        if let Some(idx) = self.map.get(key) {
            self.data.get(*idx).map(|binding| binding.get())
        } else {
            None
        }
    }

    pub fn get_binding(&self, key: &str) -> Option<Binding> {
        if let Some(idx) = self.map.get(key) {
            self.data.get(*idx).cloned()
        } else {
            None
        }
    }

    pub fn get_docs(&self, key: &str) -> Option<&str> {
        if let Some(idx) = self.map.get(key) {
            if let Some(Some(docs)) = self.doc_strings.get(*idx) {
                Some(docs)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn get_idx(&self, idx: usize) -> Option<Expression> {
        self.data.get(idx).map(|binding| binding.get())
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.data.clear();
    }

    pub fn remove(&mut self, key: &str) -> Option<Expression> {
        if let Some(idx) = self.map.remove(key) {
            self.free_list.push(idx);
            self.data.push(Binding::new());
            self.doc_strings.push(None);
            self.doc_strings.swap_remove(idx);
            return Some(self.data.swap_remove(idx).get());
        }
        None
    }

    fn update_entry_idx(
        &mut self,
        key: &str,
        idx: usize,
        val: Expression,
        doc_str: Option<String>,
    ) -> Result<Expression, LispError> {
        if let Some(docs) = self.doc_strings.get_mut(idx) {
            if doc_str.is_some() {
                *docs = doc_str;
            }
        }
        if let Some(binding) = self.data.get_mut(idx) {
            //let entry = entry.clone();
            //entry.get_mut().data.replace(val.into());
            Ok(binding.replace(val))
        } else {
            Err(LispError::new(format!(
                "update, key not found {} - invalid index",
                key
            )))
        }
    }

    pub fn update_entry(
        &mut self,
        key: &str,
        val: Expression,
        doc_str: Option<String>,
    ) -> Result<Expression, LispError> {
        if let Some(idx) = self.map.get(key) {
            let idx = *idx;
            self.update_entry_idx(key, idx, val, doc_str)
        } else {
            Err(LispError::new(format!("update, key not found {}", key)))
        }
    }

    pub fn insert(&mut self, key: &'static str, exp: Expression) {
        self.insert_with_doc(key, exp, None)
    }

    pub fn insert_exp_data(&mut self, key: &'static str, data: ExpEnum) {
        let exp: Expression = data.into();
        self.insert(key, exp);
    }

    pub fn insert_with_doc(
        &mut self,
        key: &'static str,
        exp: Expression,
        doc_string: Option<String>,
    ) {
        if let Some(idx) = self.map.get(key) {
            let idx = *idx;
            if self.update_entry_idx(key, idx, exp, doc_string).is_err() {
                panic!("Failed to insert with a verified index, this can not happen?");
            }
        } else if let Some(idx) = self.free_list.pop() {
            self.data.push(Binding::with_expression(exp));
            self.data.swap_remove(idx);
            self.doc_strings.push(doc_string);
            self.doc_strings.swap_remove(idx);
            self.map.insert(key, idx);
        } else {
            self.data.push(Binding::with_expression(exp));
            self.doc_strings.push(doc_string);
            self.map.insert(key, self.data.len() - 1);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_int_equal(exp1: &Expression, exp2: &Expression) {
        if let ExpEnum::Int(i1) = exp1.get().data {
            if let ExpEnum::Int(i2) = exp2.get().data {
                assert!(i1 == i2);
                return;
            }
        }
        assert!(false);
    }

    #[test]
    fn test_namespace() -> Result<(), LispError> {
        let mut environment = build_default_environment();
        let root = Rc::new(RefCell::new(Namespace::new_root(&mut environment.interner)));
        let ns_name = environment.interner.intern("namespace");
        let ns = Rc::new(RefCell::new(Namespace::new_with_outer(
            ns_name,
            Some(root.clone()),
        )));
        let tv0: Expression = ExpEnum::Int(1).into();
        ns.borrow_mut().insert("v1", tv0.clone()); //ExpEnum::Int(1).into());
        let tv1 = ns.borrow().get("v1").unwrap().clone();
        assert_int_equal(&tv1, &ExpEnum::Int(1).into());
        ns.borrow_mut().insert("v1", ExpEnum::Int(2).into());
        assert_int_equal(&tv1, &ExpEnum::Int(1).into());
        assert_int_equal(&tv0, &ExpEnum::Int(1).into());
        let tv1 = ns.borrow().get("v1").unwrap().clone();
        assert_int_equal(&tv1, &ExpEnum::Int(2).into());
        Ok(())
    }
}
