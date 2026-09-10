# Concatenate text

Function to concatenate text

## Usage

``` r
concatText(..., concat = "+")
```

## Arguments

- ...:

  \- text values to concatenate

- concat:

  \- string to use as concatenator

## Value

string

## Details

The elements in ... are concateated into a single string using the
\`concat\` string separated by spaces.

## Examples

``` r
# example code
concatText(1,2,3)
#> Error in concatText(1, 2, 3): could not find function "concatText"
concatText(1,NA,3)
#> Error in concatText(1, NA, 3): could not find function "concatText"
concatText(1,2,NULL,4)
#> Error in concatText(1, 2, NULL, 4): could not find function "concatText"
export
#> Error: object 'export' not found
```
