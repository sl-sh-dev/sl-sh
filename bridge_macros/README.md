TODO PC ISSUE #8 update! for slosh



sl-sh proc macro attributes to eliminate need for boilerplate in creating builtin functions

Notes
-----
TODO PC relax this requirement. is static assertions really needed?
- must use the static_assertions crate and have the line

`extern crate static_assertions;`

in the main.rs file.

This package is used to guarantee at compile time that the necessary functions,
namely SlFrom and SlFromRef are implemented for the necessary
Rust types so the annotations rust function signatures types can be converted
into slvm::Value types into their corresponding rust types (`impl SlFromRef<Value> for Foo { ... }`) and back again
as a slvm::Value (`impl SlFrom<Foo> for Value { ... }`) from a rust type.

- All rust functions that use these macros *must* have documentation.

```
Documentation is structured in the following way:
/// Usage: (<name-of-sl-sh-fun> <fn_args>)
///
/// <description>
///
/// Section: <type>
///
/// Example:
/// <example sl-sh code>
```

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

- macro supports optional name value attribute (takes_env = "<true|false>"), e.g.

``` rust
#[sl_sh_fn(fn_name = "int->float", takes_env = true)]
fn float_to_int(&mut Environment, val: i64) -> f64 {
    ...
}
```

the default value is false, which means the macro will not expect the first argument
of your function is `&mut Environment`.


Limitations
-----------
1. If a function returns a type, T, T must implement `Slrom<T> for slvm::Value`.
2. All types, T, U, .., in Vec<T>, VarArgs<(T, U, ..)>, and (T, U, ...) must implement `SlFrom<Value> for Value`
3. All types, T, &U, &mut V, input to fn expressions with the sl_sh_fn macro:
    ```
    #[sl_sh_fn(fn_name = "foo")]
    fn foo(t: T, u: &U, v: &mut V) -> LispResult<slvm::Value> {
        ...
    }
     ```
    must implement
    `impl SlFromRef<&Value> for T`
    for T,
   `impl SlFromRef<&Value> for F`
    for U, and,
    `impl SlAsMut<&Value> for V`
    for V.
4. If a `Result` return type is needed (for simplicity) use `VMResult`, as it's supported instead.
5. TypePath for LispResult (and other types recognized by this macro, e.g. VarArgs or Vec) must be bare
    and can not be qualified with any type path, i.e. `slvm::VMResult` is invalid as a return type
    but `VMResult` is fine.
6. `VarArgs` support requires use of `crate::VarArgs` which is a type alias for Vec<T>,
    this tells the macro that the function can receive zero to N more arguments.
7. `VarArgs<T>` must be last argument if used, but using it allows the function to accept N
   more arguments.
8. `VarArgs<T>` and `Vec<T>` are supported but in both cases `T` must implement `SlFromRef`
    because a clone must occur to pass the inner `ExpEnum` in `Expression` to a Vec.
9. Using a `Vec<T>` as a parameter corresponds to receiving an `Expression` that evaluates to
    `ExpEnum::Nil`/`Pair`/`Vector`.
10`Option<T>` types are supported but those arguments must be last (but can be before one `VarArgs<T>`).
11. Tuples are supported but if they are in a `Vec<(U, T, ...)>` or `VarArgs<(U, T, ..)>` but
    all tuple members must implement `TryIntoExpression` in order to turn each `Expression` into a `Vec`.

Example
-------
- TOOD need examples of ALL types of function signatures supported
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
#[sl_sh_fn(fn_name = "int-to-float")]
fn int_to_float(int: i64) -> f64 {
	int as f64
}
```

will trigger the generation of three new functions, `parse_int_to_float`,
`builtin_int_to_float`, and `intern_int_to_float`.
- parse_int_to_float expands to something like:

```rust
fn parse_int_to_float(
    environment: &mut sl_sh::environment::Environment,
    args: &mut dyn Iterator<Item = sl_sh::types::Expression>,
) -> sl_sh::LispResult<sl_sh::types::Expression> {
    use sl_sh::builtins_util::ExpandVecToArgs;
    use std::convert::TryInto;
    let args = sl_sh::builtins_util::make_args_eval_no_values(environment, args)?;
    let fn_name = "int-to-float";
    const args_len: usize = 1usize;
    // this arg_types variable is generated by the macro for use at runtime.
    let arg_types = vec![sl_sh::builtins_util::Arg {
        val: sl_sh::builtins_util::ArgVal::Value,
        passing_style: sl_sh::builtins_util::ArgPassingStyle::Move,
    }];

    let args = crate::builtins_util::make_args_eval_no_values(environment, args)?;
    let args = sl_sh::get_arg_types(fn_name, arg_types, args)?;
    if args.len() == args_len {
        match args.try_into() {
            Ok(params) => {
                // use const generics and blanket implementation of ExpandvecToArgs over
                // function calls to map vector to function call.
                let params: [sl_sh::ArgType; args_len] = params;
                builtin_int_to_float.call_expand_args(params)
            }
            Err(e) => Err(sl_sh::types::LispError::new(format!(
                "{} is broken and can't parse its arguments.",
                fn_name
            ))),
        }
    } else if args.len() > args_len {
        Err(sl_sh::types::LispError::new(format!(
            "{}  given too many arguments, expected {}, got, {}.",
            fn_name,
            args_len,
            args.len()
        )))
    } else {
        Err(sl_sh::types::LispError::new(format!(
            "{}  not given enough arguments, expected {}, got, {}.",
            fn_name,
            args_len,
            args.len()
        )))
    }
}
```

- `parse_int_to_float`'s job is to make sure that the signatures match, and, if they do
it calls the `builtin_int_to_float` function, otherwise it returns a LispError at runtime.
- the builtin_ function expands to something like this:

```rust
fn builtin_one_int_to_float(arg_0: sl_sh::ArgType) -> sl_sh::LispResult<sl_sh::types::Expression> {
   const _: fn() = || {
       fn assert_impl_all<T: ?Sized + std::convert::Into<crate::Expression>>() {}
       assert_impl_all::<f64>();
   };
   let fn_name = "one-int-to-float";
   match arg_0 {
       sl_sh::ArgType::Exp(arg_0) => {
           use sl_sh::types::RustProcedure;
           let mut typed_data: sl_sh::types::TypedExpression<i64> =
               sl_sh::types::TypedExpression::new(arg_0);
           let callback = |arg_0: i64| -> sl_sh::LispResult<sl_sh::types::Expression> {
               one_int_to_float(arg_0).map(Into::into)
           };
           typed_data.apply(fn_name, callback)
       }
       _ => {
           return Err(LispError::new(
               "sl_sh_fn macro is broken. ArgType::Exp can't be parsed as ArgType::Exp",
           ));
       }
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
method when the `int-to-float` function is called.

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
