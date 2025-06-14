# Type Conversion Tests

This directory contains tests for converting between different data types in Slosh.

## Test Coverage

### String Conversions
- String to integer
- String to float
- Number to string
- Symbol to string
- String to symbol

### Numeric Conversions
- Integer to float
- Float to integer (various rounding behaviors)
- Numeric parsing from strings
- Base conversions (hex, octal, binary)

### Collection Conversions
- List to vector
- Vector to list
- String to list of characters
- List of characters to string

### Boolean Conversions
- Truthy/falsy value testing
- Explicit boolean conversion

### Special Cases
- nil conversions
- Error handling for invalid conversions
- Overflow/underflow handling
- Precision loss in conversions