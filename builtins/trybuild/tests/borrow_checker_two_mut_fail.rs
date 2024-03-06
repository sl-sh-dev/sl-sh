use compile_state::state::new_slosh_vm;
use compile_state::types::{SlAsMut, SlAsRef, SlInto};
use slvm::Value;

pub fn main() {}
fn borrow_check() {
    let mut vm = new_slosh_vm();
    let dest = vm.alloc_string("XXX".to_string());
    let dest2 = vm.alloc_string("YYY".to_string());
    match dest {
        Value::String(d) => {
            let d = vm.get_string_mut(d).unwrap();
            match dest2 {
                Value::String(d2) => {
                    let d2 = vm.get_string_mut(d2).unwrap();
                    println!("x: {}", d);
                    println!("y: {}", d2);
                }
                _ => {
                    panic!("failed");
                }
            }
        }
        _ => {
            panic!("failed");
        }
    }
}
