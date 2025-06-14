# Rounding Tests

This directory contains tests for rounding and precision functions in Slosh.

## Test Coverage

### Round Function
- Round to nearest integer
- Round with precision (decimal places)
- Halfway cases (0.5, 1.5, 2.5)
- Negative number rounding
- Different rounding modes (if supported)

### Floor Function
- Floor for positive numbers
- Floor for negative numbers
- Already integer inputs
- Very large numbers
- Very small numbers

### Ceiling Function
- Ceiling for positive numbers
- Ceiling for negative numbers
- Already integer inputs
- Very large numbers
- Very small numbers

### Truncate Function
- Truncate positive numbers
- Truncate negative numbers
- Comparison with floor/ceiling
- Zero handling

### Precision Control
- Rounding to specific decimal places
- Significant figures
- Scientific notation handling
- Floating-point precision limits

### Edge Cases
- NaN and infinity
- Numbers near integer boundaries
- Very small decimals (< 1e-10)
- Very large numbers (> 1e10)
- Negative zero handling