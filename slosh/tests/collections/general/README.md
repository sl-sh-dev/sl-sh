# General Collection Tests

This directory contains tests for operations that work across multiple collection types in Slosh.

## Test Coverage

### Universal Operations
- count/length - collection size
- empty? - emptiness check
- seq - sequence abstraction
- into - collection type conversion
- Collection equality across types

### Iteration Protocols
- for-each - side-effecting iteration
- map - transformation
- filter - selection
- reduce/fold - aggregation
- take/drop - subsequences
- partition - splitting

### Collection Transformations
- flatten - nested collection flattening
- group-by - grouping by key function
- frequencies - count occurrences
- distinct - remove duplicates
- interleave - combine collections
- zip - pair up elements

### Set Operations
- union - combine collections
- intersection - common elements
- difference - unique elements
- symmetric-difference

### Performance Tests
- Large collection handling
- Memory efficiency
- Operation complexity
- Lazy vs eager evaluation

### Cross-Type Operations
- Converting between lists, vectors, and sets
- Preserving order where applicable
- Type-specific optimizations
- Generic collection algorithms