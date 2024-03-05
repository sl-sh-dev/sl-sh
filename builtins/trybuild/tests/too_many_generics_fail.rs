use bridge_macros::sl_sh_fn;

pub fn main() {}

/// obligatory doc
#[sl_sh_fn(fn_name = "too_many_generics")]
pub fn too_many_generics<'a, 'b>(a: &'a str, b: &'b str) -> &'a str {
    a
}
