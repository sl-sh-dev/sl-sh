use derive_sl_sh::sl_sh_fn;

#[sl_sh_fn]
fn sl_sh_me(fun: u8) {
}

fn main() {
    let x = sl_sh_me(1);
    assert_eq!(x, 2);
}
