use crate::Vm;

impl Vm {
    pub fn dump_globals(&self) {
        println!("GLOBALS:");
        self.globals.dump(self);
        println!();
    }

    pub fn global_name(&self, idx: usize) -> &'static str {
        self.globals.index_to_name(self, idx)
    }
}
