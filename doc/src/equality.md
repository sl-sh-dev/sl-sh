# Equality

When testing for equality in Slosh, there are several functions that can be used.
Consider invocations like
`(= 1 2)`,
`(eq? 1 2)`,
`(equal? 1 2)`, and
`(assert-equal 1 2)`
these are translated to different opcodes and equality implementations as listed below

```rust
let x = 42i64;
```

```slosh
(prn "42")
```

-   `equal?`

    -   lenient
    -   `state.rs` defines special forms object with key `equal` mapped to the name `equal?`.
    -   `compile.rs` matches on `env.specials().equal` and generates opcode `EQUAL`
    -   `exec_loop.rs` maps opcode `EQUAL` to function `is_equal`
    -   `vm.rs` `is_equal` converts each arg to a `Value` (in case it needs to be dereferenced) and calls `is_equal_pair`
    -   `vm.rs` `is_equal_pair` does a complex test for equality
        -   check if both args are Byte or Int and if so, converts both to `i64` with `Value::get_int` and compares with rust native `==`
        -   check if both args are Byte or Int or Float and if so, converts both to `f64` with `Value::get_float`
            -   then subtracts the second from the first and takes the absolute value and tests `diff == 0.0`

-   `assert-equal`

    -   based on `equal?`
    -   is a macro defined in core.slosh which checks if the arguments are `equal?` and throws an error if they are not

-   `eq?`

    -   uses `Value::PartialEq` implementation
    -   `state.rs` defines special forms object with key `eq` mapped to the name `eq?`.
    -   `compile.rs` matches on `env.specials().eq` and generates opcode `EQ`
    -   `exec_loop.rs` maps opcode `EQ` to function `is_eq`
    -   `vm.rs` `is_eq` converts each arg to a `Value` and compares `val1 == val2` which uses `Value::PartialEq` implementation

-   `=`
    -   numeric equality after converting to `i64` or `f64`
    -   `state.rs` defines special forms object with key `numeq` mapped to the name `=`.
    -   `compile.rs` calls `compile_list` which calls `compile_special` which calls `compile_math` in `compile_math.rs`
    -   `compile_math.rs` `compile_math` matches on `env.specials().numeq` and generates opcode `NUMEQ`
    -   `exec_loop.rs` maps opcode `NUMEQ` to function `compare_int` and passes two comparators
        -   an integer comparator: `|a,b| a == b`
        -   a float comparator: `|a: f64, b: f64| (a - b).abs() < f64::EPSILON`
    -   `macros.rs` `compare_int`
        -   checks if either argument is a `Float` and if so, converts both to `f64` with `get_float` macro and uses the float comparator
        -   checks if either argument is a `Int` and if so, converts both to `i64` with `get_int` macro and uses the integer comparator
    -   ultimately, ints are compared as i64 and floats are compared as f64 with a tolerance of `f64::EPSILON`
