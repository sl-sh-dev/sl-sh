use crate::Vm;

impl Vm {
    pub fn dump_globals(&self) {
        println!("GLOBALS:");
        self.globals.dump(self);
        println!();
    }
}
