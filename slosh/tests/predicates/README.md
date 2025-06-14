# Predicate Tests

This directory contains tests for predicate functions (functions that return true/false) in Slosh.

## Test Coverage

### Type Predicates
- nil? - test for nil
- string? - test for strings
- number? - test for numeric types
- int? - test for integers
- float? - test for floating-point numbers
- symbol? - test for symbols
- list? - test for lists
- vector? - test for vectors
- hash? - test for hash tables
- fn? - test for functions
- macro? - test for macros

### Comparison Predicates
- = (equality)
- eq? (identity equality)
- < (less than)
- > (greater than)
- <= (less than or equal)
- >= (greater than or equal)
- zero? - test for zero
- positive? - test for positive numbers
- negative? - test for negative numbers

### Collection Predicates
- empty? - test for empty collections
- contains? - test for element membership
- every? - test if all elements satisfy predicate
- some? - test if any element satisfies predicate

### String Predicates
- starts-with? - test string prefix
- ends-with? - test string suffix
- matches? - test regex match

### Special Predicates
- even? - test for even numbers
- odd? - test for odd numbers
- true? - test for boolean true
- false? - test for boolean false