# Extract text between first and last parentheses

Function to extract text between first and last parentheses

## Usage

``` r
extractTextBetweenParens(txt)
```

## Arguments

- txt:

  \- character vector from which to extract strings

## Value

character vector of extracted strings

## Details

For each element in the vector, this extracts the string between the
\*first\* "(" and \*last\* ")" from each element of the input character
vector, or \`NA\` if no matching parentheses.

## Examples

``` r
# example code
extractTextBetweenParens("fred(x,y)=0");
#> [1] "x,y"
extractTextBetweenParens(c("fred(x,y)=0","b(z0)"));
#> [1] "x,y" "z0" 
extractTextBetweenParens("fred(x,y(z))=0");
#> [1] "x,y(z)"
```
