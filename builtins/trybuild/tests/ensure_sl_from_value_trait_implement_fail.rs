use bridge_adapters::lisp_adapters::{SlAsRef, SlInto};
use compile_state::state::new_slosh_vm;

pub fn main() {}
fn do_not_implement_sl_from_to_convert_from_value_types_to_rust_types() {
    let mut vm = new_slosh_vm();
    let dest = vm.alloc_string("XXX".to_string());
    let dest2 = vm.alloc_string("YYY".to_string());

    let x: String = (&dest).sl_into(&mut vm).unwrap();
    let x: f64 = (&dest).sl_into(&mut vm).unwrap();
    let x: i64 = (&dest).sl_into(&mut vm).unwrap();
    let x: usize = (&dest).sl_into(&mut vm).unwrap();
    let x: f32 = (&dest).sl_into(&mut vm).unwrap();
    let x: &str = (&dest).sl_into(&mut vm).unwrap();
    let x: bool = (&dest).sl_into(&mut vm).unwrap();
}
