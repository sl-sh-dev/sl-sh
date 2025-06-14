# Vector Tests

This directory contains tests for vector (dynamic array) operations in Slosh.

## Test Coverage

### Creation and Initialization
- Empty vector creation
- Vector literals
- Vector from list
- Vector with repeated elements
- Range vectors

### Access Operations
- nth - element access by index
- first/last - endpoint access
- Bounds checking
- Negative indexing (if supported)

### Modification Operations
- conj - add element
- assoc - update at index
- pop - remove last element
- subvec - extract subvector
- insert-at - insert at position
- remove-at - remove at position

### Sequence Operations
- map - transform elements
- filter - select elements
- reduce - aggregate values
- reverse - reverse order
- sort - sort elements
- distinct - unique elements

### Vector Arithmetic
- Vector addition/concatenation
- Vector slicing
- Vector rotation
- Vector partitioning

### Edge Cases
- Empty vector operations
- Single element vectors
- Very large vectors
- Out of bounds access
- Type heterogeneity
- Vector equality testing