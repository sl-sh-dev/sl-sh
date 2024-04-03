# End to End example of Slosh execution

1. Start with the lisp program `(1.1)`
2. In `reader.rs`

-   The `read_inner` function controls most parsing.
    -   Numeric parsing happens at the end in the catch-all of the match statement.
-   The `do_atom` function attempts to parse 1.1 as an i64 and fails, so it parses it as f64 and then calls `.into()` to convert to a `Value`

3. In `main.rs`

-   The `exec_expression` function calls `pass1`
-   And then it calls `compile`

4. In `pass1.rs`

-   The `pass1` function initially operates on (1.1) as a pair or list
-   It iterates over each element of the list and recursively calls `pass1` on each element
-   So then `pass1` is called on 1.1 which is then handled in the catch-all of the match statement
-   and this is where we add 1.1 to the heap and add it is a constant to the vm

5. In `state.rs`

-   `add_constant` is called which inserts the `Value` of 1.1 into `pub struct CompileState`'s `pub constants: HashMap<Value, usize>`
-   Since constants are stored in a hashmap, if two different numeric constants hash to the same thing, they will be stored as the same constant

6. In `float_56.rs`

-   `F56` impl's the `Hash` trait and has a custom implementation of `hash` that converts the `F56` to a `u64` and then hashes the result

7. In `compile.rs`

-   Recall that `exec_expression` called `compile` after `pass1`
-   calls to `compile` trickle down into `compile_list`, `compile_special`, `compile_math`, and others.
-   `compile_list` handles a single value like this and the match statement catch-all prints Boo and the value itself
