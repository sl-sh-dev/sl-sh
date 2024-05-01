use bridge_macros::sl_sh_fn;
use bridge_types::VarArgs;
use compile_state::state::new_slosh_vm;
use slvm::{VMResult, Value};

pub fn main() {
    let mut vm = new_slosh_vm();

    let no_args = vec![];
    // no arguments allowed for varargs
    assert!(parse_accept_varargs(&mut vm, no_args.as_slice()).is_ok());
    // no argument for vec
    assert!(parse_accept_vec(&mut vm, no_args.as_slice()).is_err());
    // no argument for vec or opt
    assert!(parse_accept_vec_opt(&mut vm, no_args.as_slice()).is_err());
    // fine, no input required
    assert!(parse_accept_opt_varargs(&mut vm, no_args.as_slice()).is_ok());

    let one_arg = vec![Value::CodePoint('a')];
    // varargs can be one
    assert!(parse_accept_varargs(&mut vm, one_arg.as_slice()).is_ok());
    // a value is not a vector
    assert!(parse_accept_vec(&mut vm, one_arg.as_slice()).is_err());
    // one value is optional, varargs allows none
    assert!(parse_accept_opt_varargs(&mut vm, one_arg.as_slice()).is_ok());
    // a value is not a vector, fine that there is no second argument
    assert!(parse_accept_vec_opt(&mut vm, one_arg.as_slice()).is_err());

    let two_args = vec![Value::CodePoint('a'), Value::CodePoint('a')];
    // varargs can be any length
    assert!(parse_accept_varargs(&mut vm, two_args.as_slice()).is_ok());
    // two values is not a vector
    assert!(parse_accept_vec(&mut vm, two_args.as_slice()).is_err());
    // one value for opt and the other for varargs
    assert!(parse_accept_opt_varargs(&mut vm, two_args.as_slice()).is_ok());
    // two values is not a vector
    assert!(parse_accept_vec_opt(&mut vm, two_args.as_slice()).is_err());

    let three_args = vec![
        Value::CodePoint('a'),
        Value::CodePoint('a'),
        Value::CodePoint('a'),
    ];
    // varargs can be any length
    assert!(parse_accept_varargs(&mut vm, three_args.as_slice()).is_ok());
    // three values is not a vector
    assert!(parse_accept_vec(&mut vm, three_args.as_slice()).is_err());
    // a value for opt and the rest for varargs
    assert!(parse_accept_opt_varargs(&mut vm, three_args.as_slice()).is_ok());
    // three args is not a vector
    assert!(parse_accept_vec_opt(&mut vm, three_args.as_slice()).is_err());
    let vec_and_arg = vec![
        vm.alloc_vector(vec![Value::CodePoint('a')]),
        Value::CodePoint('a'),
    ];
    // verify that a vector and some value can be accepted
    assert!(parse_accept_vec_opt(&mut vm, vec_and_arg.as_slice()).is_ok());
    // both varargs and vec can accept a vector
    let vals = vec![
        Value::CodePoint('a'),
        Value::CodePoint('a'),
        Value::CodePoint('a'),
    ];
    let vector = vec![vm.alloc_vector(vals.clone())];
    // a vector can be passed to varargs, coerced to a vector of the same type
    assert!(parse_accept_varargs(&mut vm, vector.as_slice()).is_ok());
    // a vector is a vector
    assert!(parse_accept_vec(&mut vm, vector.as_slice()).is_ok());
    // a vector is a vector and no second argument needed
    assert!(parse_accept_vec_opt(&mut vm, vector.as_slice()).is_ok());
    // a vector of chars is not an Optional<char>
    assert!(parse_accept_opt_varargs(&mut vm, vector.as_slice()).is_err());

    let val_and_vec = vec![Value::CodePoint('a'), vm.alloc_vector(vals.clone())];
    // verify that some value is accepted for opt and a vector can be passed in for varargs
    assert!(parse_accept_opt_varargs(&mut vm, val_and_vec.as_slice()).is_ok());

    // but varargs can take a variadic number of anything including two vectors
    let vectors = vec![vm.alloc_vector(vals.clone()), vm.alloc_vector(vals)];
    assert!(parse_accept_varargs(&mut vm, vectors.as_slice()).is_ok());
    // vec is only supposed to take one vector not two
    assert!(parse_accept_vec(&mut vm, vectors.as_slice()).is_err());
    // the second vector can not be passed to Optional<char>
    assert!(parse_accept_vec_opt(&mut vm, vectors.as_slice()).is_err());
    // the first vector can not be passed to Optional<char>
    assert!(parse_accept_opt_varargs(&mut vm, vectors.as_slice()).is_err());

    let nil = vec![Value::Nil];
    assert!(parse_accept_varargs(&mut vm, nil.as_slice()).is_ok());
    // vec is only supposed to take a vector
    assert!(parse_accept_vec(&mut vm, nil.as_slice()).is_err());
}

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_varargs")]
pub fn accept_varargs(_args: VarArgs<char>) -> VMResult<()> {
    Ok(())
}

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_vecvalues")]
pub fn accept_vecvalues(_args: Vec<Value>) -> VMResult<()> {
    Ok(())
}

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_vec")]
pub fn accept_vec(_args: Vec<char>) -> VMResult<()> {
    Ok(())
}

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_vec_opt")]
pub fn accept_vec_opt(_args: Vec<char>, _opt: Option<char>) -> VMResult<()> {
    Ok(())
}

/// obligatory doc
#[sl_sh_fn(fn_name = "accept_opt_varargs")]
pub fn accept_opt_varargs(_opt: Option<char>, _args: VarArgs<char>) -> VMResult<()> {
    Ok(())
}

/// obligatory doc
#[sl_sh_fn(fn_name = "return_vec")]
pub fn return_vec() -> VMResult<Vec<String>> {
    Ok(vec!["a".to_string(), "b".to_string(), "c".to_string()])
}
