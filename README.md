sl-sh proc macro attributes to eliminate need for boilerplate in creating builtin functions

Notes
-----
- must use the static_assertions crate and have the line

`extern crate static_assertions;`

in the main.rs file.

this package is used to guarantee at compile time that the necessary functions,
namely Into, TryInto, and TryIntoExpression are implemented for the necessary
Rust types so the builtin_ version of the function can be type checked properly.

- This also means that all types for a function using this proc_macro_attribute
must implement the traits Into, TryInto, and TryIntoExpression traits defined
in the sl-sh crate.

- All rust functions that use these macros *must* have documentation.
Documentation is structured in the following way:
/// Usage: (<name-of-sl-sh-fun> <fn_args>)
///
/// <description>
///
/// Section: <type>
///
/// Example:
/// <example sl-sh code>

- currently only functions are supported.

Macros
------
- The sl_sh_fn proc_macro_attribute
- this attribute is used on rust functions to generate code that is compatible
with the sl-sh runtime.
- the macro must have the name-value pair (fn_name = "<name-of-sl-sh-fun>") so
the macro can reference the name of the function in case of an error at runtime.

Attribute Name-Value Pairs
--------------------------
- macro must have attribute (fn_name = "<name-of-sl-sh-fun>"), e.g.

``` rust
#[sl_sh_fn(fn_name = "int->float")]
fn float_to_int(val: i64) -> f64 {
    ...
}
```

- macro supports optional name value attribute (eval_values = "<true|false>"), e.g.

``` rust
#[sl_sh_fn(fn_name = "int->float", eval_values = false)]
fn float_to_int(val: i64) -> f64 {
    ...
}
```

the default value is false, which means all sl-sh values objects will be treated
as equal to (values-nth 0 exp). In sl-sh objects of type values evaluate to their
first element unless the caller is the values?, values-nth, or values-length functions.
These three methods are examples of methods where `..., eval_values = true)` is required.

Limitations
-----------
1. If a Result return type is needed for simplicity only LispResult is supported.
2. TypePath for LispResult must be bare can not be qualified with any type path, i.e. sl_sh::LispResult
// TODO make a marker trait for sl_sh::LispResult, Vec, and Opt so those can be used to to do compile
// time checks instead of matching TypePath's

Example
-------
- for example the function:

```rust
/// Usage: (int->float int) -> float
///     Cast an int as a float.
/// Section: type
/// Example:
///     (test::assert-equal 0 (int->float 0))
///     (test::assert-equal 10 (int->float 10))
///     (test::assert-equal -101 (int->float -101))
///     (test::assert-error (int->float "not int"))
#[sl_sh_fn(fn_name = "int->float")]
fn int_to_float(int: i64) -> f64 {
	int as f64
}
```

will trigger the generation of three new functions, `parse_int_to_float`,
`builtin_int_to_float`, and `intern_int_to_float`.
- parse_int_to_float expands to something like:

```rust
fn parse_int_to_float(
    environment: &mut crate::environment::Environment,
    args: &mut dyn Iterator<Item = crate::types::Expression>,
) -> crate::LispResult<crate::types::Expression> {
    use std::convert::TryInto;
    use crate::builtins_util::ExpandVecToArgs;
    let args = crate::builtins_util::make_args(environment, args)?;
    let fn_name = "int->float";
    const args_len: usize = 1usize;
    if args.len() == args_len {
        match args.try_into() {
            Ok(params) => {
                let params: [crate::types::Expression; args_len] = params;
                builtin_int_to_float.call_expand_args(params)
            }
            Err(e) => Err(LispError::new({
                let res = ::alloc::fmt::format(::core::fmt::Arguments::new_v1(
                    &["", " is broken and can\'t parse its arguments.."],
                    &[::core::fmt::ArgumentV1::new_display(&fn_name)],
                ));
                res
            })),
        }
    } else if args.len() > args_len {
        Err(LispError::new({
            let res = ::alloc::fmt::format(::core::fmt::Arguments::new_v1(
                &["", " given too many arguments, expected ", ", got ", "."],
                &[
                    ::core::fmt::ArgumentV1::new_display(&fn_name),
                    ::core::fmt::ArgumentV1::new_display(&args_len),
                    ::core::fmt::ArgumentV1::new_display(&args.len()),
                ],
            ));
            res
        }))
    } else {
        Err(LispError::new({
            let res = ::alloc::fmt::format(::core::fmt::Arguments::new_v1(
                &["", " not given enough arguments, expected ", ", got ", "."],
                &[
                    ::core::fmt::ArgumentV1::new_display(&fn_name),
                    ::core::fmt::ArgumentV1::new_display(&args_len),
                    ::core::fmt::ArgumentV1::new_display(&args.len()),
                ],
            ));
            res
        }))
    }
}
```

- `parse_int_to_float`'s job is to make sure that the signatures match, and, if they do
it calls the `builtin_int_to_float` function, otherwise it returns a LispError at runtime.
- the builtin_ function expands to something like this:

```rust
fn builtin_int_to_float(arg_0: crate::Expression) -> crate::LispResult<crate::types::Expression> {
    use std::convert::TryInto;
    use std::convert::Into;
    use crate::builtins_util::TryIntoExpression;
    let fn_name = "int->float";
    const _: fn() = || {
        fn assert_impl_all<T: ?Sized + std::convert::TryInto<i64>>() {}
        assert_impl_all::<crate::Expression>();
    };
    const _: fn() = || {
        fn assert_impl_all<T: ?Sized + crate::builtins_util::TryIntoExpression<i64>>() {}
        assert_impl_all::<crate::Expression>();
    };
    const _: fn() = || {
        fn assert_impl_all<T: ?Sized + std::convert::Into<crate::Expression>>() {}
        assert_impl_all::<f64>();
    };
    let result = int_to_float(arg_0.try_into_for(fn_name)?);
    Ok(result.into())
}
```

- `builtin_int_to_float`'s job is to (at compile time) ensure the proper traits are
implemented and then coerce the rust types into sl-sh Expressions with the
appropriate errors and call the original function, `int_to_float` with rust
types.
- once `builtin_int_to_float` exists, `intern_int_to_float` must be manually
called in the add_buitlins function like:

```rust
pub fn add_type_builtins<S: BuildHasher>(
	interner: &mut Interner,
	data: &mut HashMap<&'static str, (Expression, String), S>,
) {
	intern_int_to_float(interner, data);
}
```

- this is a bit ugly but necessary given the current architecture. This function
tells sl-sh's plumbing to use the `builtin_int_to_float`
method when the `int->float` function is called.

Errors
------
- great pains were taken to make sure that the errors in development are as nice as
possible, if the errors are wrong/confusing and/or using the wrong syn::Span (manifested
as an error message potentially pointing at the wrong code) when compile errors occur
look at the usage of the MacroResult type to ensure correctness.
It's very possible it also isn't being used where it should or is being used
where it shouldn't, development is iterative!

```rust
  type MacroResult<T> = Result<T, syn::Error>;
```

- `syn:Error` is used with an accompanying span to pass invalid, or unsupported syntax
back to the generated code to report errors at compile time using the syn::Error::to_compile_error
method.


happy sl-shing!
