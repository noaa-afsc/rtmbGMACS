# Evaluate strings in a character vector as digits

Function to evaluate strings in a character vector as digits.

## Usage

``` r
evalTextAsDigits(x)
```

## Arguments

- x:

  - character vector to parse/evaluate

## Value

integer vector

## Details

For each element in `x`, everything except for digits is removed and the
resulting character value is converted to an integer.

## Examples

``` r
evalTextAsDigits(c("ab23","cd42","a42 b23"))
#> [1]   23   42 4223
```
