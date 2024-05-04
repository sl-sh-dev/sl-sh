# Equality

The most common way to test equality is with `=`
For numeric equality (IEEE) use `==`
For bytewise equality use `identical?`

The behavior and names are based on Clojure's implementation. Read their docs here: https://clojure.org/guides/equality

-   `=`
    -   `(= 2 0x2)` is `true` (comparing an int to a byte)
    -   `(= 2 2.0)` is `false` (comparing an int to a float)
    -   `(= NaN NaN)` is `false`
    -   `state.rs` defines special forms object with key `equal` mapped to the name `=`.
    -   `compile.rs` matches on `env.specials().equal` and generates opcode `EQUAL`
    -   `exec_loop.rs` maps opcode `EQUAL` to function `is_equal`
    -   `vm.rs` `is_equal` converts each arg to a `Value` (in case it needs to be dereferenced) and calls `is_equal_pair`
    -   `vm.rs` `is_equal_pair` does a complex test for equality
        -   check if both args are Byte or Int and if so, converts both to `i64` with `Value::get_int` and compares with rust native `==`
        -   check if both args are Byte or Int or Float and if so, converts both to `f64` with `Value::get_float`
            -   then subtracts the second from the first and takes the absolute value and tests `diff == 0.0`
-   `==`

    -   `(== 1 1.0)` is `true` (comparing an int to a float)
    -   `(== NaN NaN)` is `false`
    -   returns true whenever `=` does, but also returns true for numbers that are numerically equal

    -   when comparing two floats, converts two both to `f64` and compares with native f64 `==`
    -   does not use F56::PartialEq implementation

    -   `state.rs` defines special forms object with key `numeq` mapped to the name `==`.
    -   `compile.rs` calls `compile_list` which calls `compile_special` which calls `compile_math` in `compile_math.rs`
    -   `compile_math.rs` `compile_math` matches on `env.specials().numeq` and generates opcode `NUMEQ`
    -   `exec_loop.rs` maps opcode `NUMEQ` to function `compare_numeric` and passes a comparator `|a,b| a == b`
    -   `macros.rs` `compare_numeric`
        -   checks if either argument is a `Float` and if so, converts both to `f64` with `get_primitive_float` macro and uses the comparator
        -   checks if either argument is a `Int` and if so, converts both to `i64` with `get_primitive_int` macro and uses the comparator

-   `identical?`

    -   uses `Value::PartialEq` implementation
    -   `state.rs` defines special forms object with key `eq` mapped to the name `identical?`.
    -   `compile.rs` matches on `env.specials().eq` and generates opcode `EQ`
    -   `exec_loop.rs` maps opcode `EQ` to function `is_identical`
    -   `vm.rs` `is_identical` converts each arg to a `Value` (in case it needs to be dereferenced) and compares `val1 == val2` which uses `Value::PartialEq` implementation

-   `assert-equal`

    -   based on `equal?`
    -   is a macro defined in core.slosh which checks if the arguments are `equal?` and throws an error if they are not

-   `not=`
    -   `(not= 1 2)` is equivalent to `(not (= 1 2))`
    -   `(not= 1 1.0)` is `true` (comparing an int to a float)
    -   `state.rs` defines special forms object with key `numneq` mapped to the name `not=`.
    -   `compile_math.rs` converts `numneq` to opcode `NUMNEQ`
    -   `exec_loop.rs` maps opcode `NUMNEQ` to a negation of the implementation of opcode `EQUAL` AKA `=`

### Moving Forward

-   = should be converted to =?
-   =? should just be an alias for equal? which is the slower more intuitive equality check
-   eq? is the faster more strict equality check but it does spend a little extra time making sure to dereference captured heap values
-   eq? on floats should do an IEEE comparison
-   eq? should consider 1.0 equal to 1
-   equal? on floats should do a fuzzy comparison
-   if we want a bitwise comparison of floats we will implement it later

eq? does not do type coercion
just do what clojure does
numeq should be a bit fuzzy
NaN != NaN is false and NaN == NaN is false

may 1 2024
= (equal) loosy goosey (still use IEEE comparison)
== (=?) (for numbers) (IEEE comparison)
identical? (eq?) (bytewise equality)
users who want rough epsilon equality can defn their own fn
