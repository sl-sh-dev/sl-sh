use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use slvm::chunk::*;
use slvm::interner::*;
use slvm::value::*;
use slvm::vm::*;

#[derive(Clone, Debug)]
pub struct SymbolsInt {
    pub syms: HashMap<Interned, usize>,
    count: usize,
}

impl SymbolsInt {
    pub fn add_sym(&mut self, sym: Interned) {
        self.syms.insert(sym, self.count);
        self.count += 1;
    }
}

// symbol name, idx/reg for scope, idx/reg for outer scope
type Captures = Rc<RefCell<Vec<(Interned, usize, usize)>>>;

#[derive(Clone, Debug)]
pub struct Symbols {
    pub data: Rc<RefCell<SymbolsInt>>,
    outer: Option<Rc<RefCell<Symbols>>>,
    pub captures: Captures,
}

impl Symbols {
    pub fn with_outer(outer: Option<Rc<RefCell<Symbols>>>) -> Symbols {
        let data = Rc::new(RefCell::new(SymbolsInt {
            syms: HashMap::new(),
            count: 1,
        }));
        Symbols {
            data,
            outer,
            captures: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn with_let(source: Rc<RefCell<Symbols>>) -> Symbols {
        let data = Rc::new(RefCell::new(SymbolsInt {
            syms: HashMap::new(),
            count: source.borrow().data.borrow().count,
        }));
        {
            let mut datad = data.borrow_mut();
            for (key, val) in source.borrow().data.borrow().syms.iter() {
                datad.syms.insert(*key, *val);
            }
        }
        Symbols {
            data,
            outer: source.borrow().outer.clone(),
            captures: source.borrow().captures.clone(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.data.borrow().syms.is_empty()
    }

    pub fn regs_count(&self) -> usize {
        self.data.borrow().count
    }

    pub fn contains_symbol(&self, key: Interned) -> bool {
        self.data.borrow().syms.contains_key(&key)
    }

    pub fn can_capture(&self, key: Interned) -> bool {
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

    pub fn get_capture_binding(&self, key: Interned) -> Option<usize> {
        for cap in &*self.captures.borrow() {
            if cap.0 == key {
                return Some(cap.2);
            }
        }
        None
    }

    pub fn get(&self, key: Interned) -> Option<usize> {
        self.data.borrow().syms.get(&key).copied()
    }

    pub fn clear(&mut self) {
        self.data.borrow_mut().syms.clear();
    }

    pub fn reserve_reg(&mut self) -> usize {
        let mut data = self.data.borrow_mut();
        let count = data.count;
        data.count += 1;
        count
    }

    pub fn insert(&mut self, key: Interned) -> usize {
        let mut data = self.data.borrow_mut();
        let count = data.count;
        data.syms.insert(key, count);
        data.count += 1;
        count
    }

    pub fn insert_reserved(&mut self, key: Interned, register: usize) {
        let mut data = self.data.borrow_mut();
        data.syms.insert(key, register);
    }

    pub fn insert_capture(&self, vm: &mut SloshVm, key: Interned) -> Option<usize> {
        let data_d = self.data.borrow();
        if let Some(idx) = data_d.syms.get(&key) {
            Some(*idx)
        } else {
            if let Some(outer) = &self.outer {
                drop(data_d);
                // Also capture in outer lexical scope or bad things can happen.
                if let Some(outer_idx) = outer.borrow().insert_capture(vm, key) {
                    let mut data = self.data.borrow_mut();
                    let count = data.count;
                    data.syms.insert(key, count);
                    data.count += 1;
                    self.captures.borrow_mut().push((key, count, outer_idx));
                    return Some(count);
                }
            }
            None
        }
    }

    pub fn len_captures(&self) -> usize {
        self.captures.borrow().len()
    }
}

pub struct Specials {
    pub def: Interned,
    pub set: Interned,
    pub do_: Interned,
    pub fn_: Interned,
    pub mac_: Interned,
    pub if_: Interned,
    pub add: Interned,
    pub sub: Interned,
    pub mul: Interned,
    pub div: Interned,
    pub inc: Interned,
    pub dec: Interned,
    pub list: Interned,
    pub list_append: Interned,
    pub cons: Interned,
    pub car: Interned,
    pub cdr: Interned,
    pub xar: Interned,
    pub xdr: Interned,
    pub vec: Interned,
    pub make_vec: Interned,
    pub vec_pop: Interned,
    pub vec_push: Interned,
    pub vec_nth: Interned,
    pub vec_set: Interned,
    pub quote: Interned,
    pub backquote: Interned,
    pub recur: Interned,
    pub this_fn: Interned,
    pub numeq: Interned,
    pub numneq: Interned,
    pub numlt: Interned,
    pub numlte: Interned,
    pub numgt: Interned,
    pub numgte: Interned,
    pub eq: Interned,
    pub equal: Interned,
    pub type_: Interned,
    pub not: Interned,
    pub and: Interned,
    pub or: Interned,
    pub err: Interned,
    pub vec_len: Interned,
    pub vec_clr: Interned,
    pub str_: Interned,
    pub let_: Interned,
    pub call_cc: Interned,
    pub defer: Interned,
    pub on_error: Interned,
    pub while_: Interned,
    pub doc_string: Interned,

    pub rest: Interned,
    pub optional: Interned,
    pub scratch: Interned,
}

impl Specials {
    pub fn new(vm: &mut SloshVm) -> Self {
        Self {
            def: vm.intern_static("def"),
            set: vm.intern_static("set!"),
            do_: vm.intern_static("do"),
            fn_: vm.intern_static("fn"),
            mac_: vm.intern_static("macro"),
            if_: vm.intern_static("if"),
            add: vm.intern_static("+"),
            sub: vm.intern_static("-"),
            mul: vm.intern_static("*"),
            div: vm.intern_static("/"),
            inc: vm.intern_static("inc!"),
            dec: vm.intern_static("dec!"),
            list: vm.intern_static("list"),
            list_append: vm.intern_static("list-append"),
            cons: vm.intern_static("cons"),
            car: vm.intern_static("car"),
            cdr: vm.intern_static("cdr"),
            xar: vm.intern_static("xar!"),
            xdr: vm.intern_static("xdr!"),
            vec: vm.intern_static("vec"),
            make_vec: vm.intern_static("make-vec"),
            vec_push: vm.intern_static("vec-push!"),
            vec_pop: vm.intern_static("vec-pop!"),
            vec_nth: vm.intern_static("vec-nth"),
            vec_set: vm.intern_static("vec-set!"),
            quote: vm.intern_static("quote"),
            backquote: vm.intern_static("back-quote"),
            recur: vm.intern_static("recur"),
            this_fn: vm.intern_static("this-fn"),
            numeq: vm.intern_static("="),
            numneq: vm.intern_static("/="),
            numlt: vm.intern_static("<"),
            numlte: vm.intern_static("<="),
            numgt: vm.intern_static(">"),
            numgte: vm.intern_static(">="),
            eq: vm.intern_static("eq?"),
            equal: vm.intern_static("equal?"),
            type_: vm.intern_static("type"),
            not: vm.intern_static("not"),
            and: vm.intern_static("and"),
            or: vm.intern_static("or"),
            err: vm.intern_static("err"),
            vec_len: vm.intern_static("vec-len"),
            vec_clr: vm.intern_static("vec-clear!"),
            str_: vm.intern_static("str"),
            let_: vm.intern_static("let"),
            call_cc: vm.intern_static("call/cc"),
            defer: vm.intern_static("defer"),
            on_error: vm.intern_static("on-error"),
            while_: vm.intern_static("while"),
            doc_string: vm.intern_static("doc-string"),

            rest: vm.intern_static("&"),
            optional: vm.intern_static("%"),
            scratch: vm.intern_static("[SCRATCH]"),
        }
    }
}

pub struct CompileState {
    pub symbols: Rc<RefCell<Symbols>>,
    pub constants: HashMap<Value, usize>,
    pub chunk: Chunk,
    pub max_regs: usize,
    pub tail: bool,
    pub defers: usize,
    pub doc_string: Option<Value>,
}

impl Default for CompileState {
    fn default() -> Self {
        Self::new()
    }
}

impl CompileState {
    pub fn new() -> Self {
        CompileState {
            symbols: Rc::new(RefCell::new(Symbols::with_outer(None))),
            constants: HashMap::new(),
            chunk: Chunk::new("no_file", 1),
            max_regs: 0,
            tail: false,
            defers: 0,
            doc_string: None,
        }
    }

    pub fn new_state(
        file_name: &'static str,
        first_line: u32,
        outer: Option<Rc<RefCell<Symbols>>>,
    ) -> Self {
        let symbols = Rc::new(RefCell::new(Symbols::with_outer(outer)));
        CompileState {
            symbols,
            constants: HashMap::new(),
            chunk: Chunk::new(file_name, first_line),
            max_regs: 0,
            tail: false,
            defers: 0,
            doc_string: None,
        }
    }

    pub fn reserved_regs(&self) -> usize {
        self.symbols.borrow().regs_count()
    }

    pub fn get_symbol(&self, sym: Interned) -> Option<usize> {
        self.symbols.borrow().data.borrow().syms.get(&sym).copied()
    }

    pub fn add_constant(&mut self, exp: Value) -> usize {
        if let Some(i) = self.constants.get(&exp) {
            *i
        } else {
            let const_i = self.chunk.add_constant(exp);
            self.constants.insert(exp, const_i);
            const_i
        }
    }
}

pub struct CompileEnvironment {
    use_line: bool,
    line: u32,
    specials: Option<Specials>,
    global_map: HashMap<Interned, usize>,
}

impl Default for CompileEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

impl CompileEnvironment {
    pub fn new() -> Self {
        Self {
            use_line: true,
            line: 1,
            specials: None,
            global_map: HashMap::new(),
        }
    }
}

pub type SloshVm = GVm<CompileEnvironment>;

pub trait SloshVmTrait {
    fn set_line_val(&mut self, state: &mut CompileState, val: Value);
    fn get_reserve_global(&mut self, symbol: Interned) -> u32;
    fn set_named_global(&mut self, string: &str, value: Value) -> u32;
    fn set_global_builtin(&mut self, string: &str, func: CallFuncSig<CompileEnvironment>) -> u32;
    fn dump_globals(&self);
    fn own_line(&self) -> Option<u32>;
    fn line_num(&self) -> u32;
    fn specials(&self) -> &Specials;
    fn global_intern_slot(&self, symbol: Interned) -> Option<u32>;
}

pub fn new_slosh_vm() -> SloshVm {
    let temp_env = CompileEnvironment::new();
    let mut vm = GVm::new_with_env(temp_env);
    let specials = Specials::new(&mut vm);
    vm.env_mut().specials = Some(specials);
    vm
}

impl SloshVmTrait for SloshVm {
    fn set_line_val(&mut self, state: &mut CompileState, val: Value) {
        if let (Some(Value::UInt32(dline)), Some(Value::StringConst(file_intern))) = (
            self.get_heap_property(val, "dbg-line"),
            self.get_heap_property(val, "dbg-file"),
        ) {
            let file_name = self.get_interned(file_intern);
            if file_name == state.chunk.file_name && dline as u32 > self.env().line {
                self.env_mut().line = dline as u32;
            }
        }
    }

    fn get_reserve_global(&mut self, symbol: Interned) -> u32 {
        if let Some(idx) = self.env().global_map.get(&symbol) {
            *idx as u32
        } else {
            let idx = self.reserve_global();
            self.env_mut().global_map.insert(symbol, idx as usize);
            idx
        }
    }

    fn set_named_global(&mut self, string: &str, value: Value) -> u32 {
        let sym = self.intern(string);
        let slot = self.get_reserve_global(sym);
        self.set_global(slot, value);
        slot
    }

    fn set_global_builtin(&mut self, string: &str, func: CallFuncSig<CompileEnvironment>) -> u32 {
        let f_val = self.add_builtin(func);
        self.set_named_global(string, f_val)
    }

    fn dump_globals(&self) {
        println!("GLOBALS:");
        let mut ordered_keys = Vec::with_capacity(self.env().global_map.len());
        ordered_keys.resize(self.env().global_map.len(), "");
        for (k, v) in self.env().global_map.iter() {
            ordered_keys[*v] = self.get_interned(*k);
        }
        for (i, k) in ordered_keys.iter().enumerate() {
            println!(
                "({:#010x})/{}: {}",
                i,
                *k,
                self.get_global(i as u32).display_value(self)
            );
        }
        println!();
    }

    fn own_line(&self) -> Option<u32> {
        if self.env().use_line {
            Some(self.env().line)
        } else {
            None
        }
    }

    fn line_num(&self) -> u32 {
        if self.env().use_line {
            self.env().line
        } else {
            0
        }
    }

    fn specials(&self) -> &Specials {
        if self.env().specials.is_none() {}
        self.env().specials.as_ref().unwrap()
    }

    fn global_intern_slot(&self, symbol: Interned) -> Option<u32> {
        self.env()
            .global_map
            .get(&symbol)
            .copied()
            .map(|i| i as u32)
    }
}
