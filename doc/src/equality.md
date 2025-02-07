# Equality

The most common way to test equality is with `=`
For numeric equality (IEEE) use `==`
For bytewise equality use `identical?`

The behavior and names are based on Clojure's implementation. Read their docs here: https://clojure.org/guides/equality
Also check out `slosh/tests/equality.slosh` for some examples.

- `=`

    - `(= 2 0x2)` is `true` (comparing an int to a byte)
    - `(= 2 2.0)` is `false` (comparing an int to a float)
    - `(= 0.0 -0.0)` is `true`
    - `(= NaN NaN)` is `false`

    - `state.rs` defines special forms object with key `equal` mapped to the name `=`.
    - `compile.rs` matches on `env.specials().equal` and generates opcode `EQUAL`
    - `exec_loop.rs` maps opcode `EQUAL` to function `is_equal`
    - `vm.rs` `is_equal` converts each arg to a `Value` (in case it needs to be dereferenced) and calls `is_equal_pair`
    - `vm.rs` `is_equal_pair` does a complex test for equality
        - check if both args are Byte or Int and if so, converts both to `i64` with `Value::get_int` and compares with
          rust native `==`
        - check if both args are numbers (Byte, Int, or Float) and if so, converts both to `f64` with `Value::get_float`
          and compares with rust native `==`

- `==`

    - `(== 1 1.0)` is `true` (comparing an int to a float)
    - `(== 0.0 -0.0)` is `true`
    - `(== NaN NaN)` is `false`
    - returns true whenever `=` does, but also returns true for numbers that are numerically equal

    - when comparing two floats, converts two both to `f64` and compares with native f64 `==`
    - does not use F56::PartialEq implementation

    - `state.rs` defines special forms object with key `numeq` mapped to the name `==`.
    - `compile.rs` calls `compile_list` which calls `compile_special` which calls `compile_math` in `compile_math.rs`
    - `compile_math.rs` `compile_math` matches on `env.specials().numeq` and generates opcode `NUMEQ`
    - `exec_loop.rs` maps opcode `NUMEQ` to function `compare_numeric` and passes a comparator `|a,b| a == b`
    - `macros.rs` `compare_numeric`
        - checks if either argument is a `Float` and if so, converts both to `f64` with `get_primitive_float` macro and
          uses the comparator
        - checks if either argument is a `Int` and if so, converts both to `i64` with `get_primitive_int` macro and uses
          the comparator

- `identical?`

    - `(identical? 1 1)` is `true`
    - `(identical? 1 1.0)` is `false` (different types)
    - `(identical? 0.0 -0.0)` is `false` (comparing floats with different bit patterns)
    - `(identical? NaN NaN)` might be `true` or `false`. There are trillions of different bit patterns that represent
      NaN in IEEE 754

    - is the only equality comparison that uses `Value::PartialEq` implementation which is always false for different
      types of Values
    - using identical equality for floats causes problems with hashing.
      [#125](https://github.com/sl-sh-dev/sl-sh/issues/125)
      identical equality is 'too strict' in that you probably expect that +0 and -0 should hash to the same thing, but
      they don't
      rendering hash tables

    - `state.rs` defines special forms object with key `eq` mapped to the name `identical?`.
    - `compile.rs` matches on `env.specials().eq` and generates opcode `EQ`
    - `exec_loop.rs` maps opcode `EQ` to function `is_identical`
    - `vm.rs` `is_identical` converts each arg to a `Value` (in case it needs to be dereferenced) and
      compares `val1 == val2` which uses `Value::PartialEq` implementation

- `assert-equal`

    - based on `=`
    - is a macro defined in core.slosh which checks if the arguments are `=` and throws an error if they are not

- `not=`

    - defined in `vm/core.slosh` as the negation of `=`

- `not==`

    - defined in `vm/core.slosh` as the negation of `==`
