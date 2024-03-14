use bridge_adapters::lisp_adapters::{SlAsRef, SlInto};
use compile_state::state::new_slosh_vm;

pub fn main() {}
fn borrow_check() {
    let mut vm = new_slosh_vm();
    let dest = vm.alloc_string("XXX".to_string());
    let dest2 = vm.alloc_string("YYY".to_string());

    let y: &str = (&dest2).sl_as_ref(&vm).unwrap();
    let x: String = (&dest).sl_into(&mut vm).unwrap();
    println!("y: {}", y);
    println!("x: {}", x);
}
