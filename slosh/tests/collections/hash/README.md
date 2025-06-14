# Hash Table Tests

This directory contains tests for hash table (associative array/map) operations in Slosh.

## Test Coverage

### Creation and Initialization
- Empty hash table creation
- Hash table literals
- From association list
- From key-value pairs

### Basic Operations
- get - retrieve value by key
- set - insert/update key-value pair
- contains? - check key existence
- remove - delete key-value pair
- clear - remove all entries

### Iteration
- keys - get all keys
- values - get all values
- entries - get key-value pairs
- for-each iteration
- map/filter operations on hash tables

### Advanced Operations
- merge - combine hash tables
- select-keys - create subset with specific keys
- dissoc - create new hash without specific keys
- update - modify value for key
- get-in - nested hash access

### Edge Cases
- nil as key or value
- Hash collision handling
- Very large hash tables
- Circular references
- Different key types (strings, symbols, numbers)
- Hash table equality testing