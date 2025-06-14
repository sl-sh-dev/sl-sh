# Slosh Test Suite Organization

This directory contains the organized test suite for Slosh. Tests are categorized by functionality to make them easy to find, maintain, and extend.

## Directory Structure

### math/
Mathematical operation tests including arithmetic, trigonometry, and numerical functions.
- **basic/** - Basic arithmetic operations (+, -, *, /, modulo)
- **trigonometry/** - Trigonometric functions (sin, cos, tan, etc.)
- **rounding/** - Rounding and precision functions (round, floor, ceiling, truncate)

### type-conversions/
Tests for converting between different data types (string to number, etc.)

### strings/
String manipulation and processing tests including concatenation, splitting, searching, and formatting.

### collections/
Tests for collection data structures and their operations.
- **hash/** - Hash table/map operations
- **vector/** - Vector/list operations
- **general/** - General collection utilities and common operations

### predicates/
Tests for predicate functions (type checking, comparisons, boolean operations).

### system-io/
System interaction and I/O operation tests including file operations, environment variables, and process management.

## Test File Naming Convention

Test files should be named descriptively using kebab-case and end with `.slosh`:
- `basic-arithmetic.slosh`
- `string-concatenation.slosh`
- `hash-table-operations.slosh`

## Running Tests

Tests can be run using the slosh test runner from the project root.