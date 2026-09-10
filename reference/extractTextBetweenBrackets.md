# Extract text between first and last brackets

Function to extract text between first and last brackets

## Usage

``` r
extractTextBetweenBrackets(txt)
```

## Arguments

- txt:

  \- character vector from which to extract strings

## Value

character vector of extracted strings

## Details

For each element in the vector, this extracts the string between the
\*first\* "\\" and \*last\* "\\" from each element of the input
character vector, or \`NA\` if no matching brackets.

## Examples

``` r
# example code
extractTextBetweenBrackets("fred[x,y]=0");
#> [1] "x,y"
extractTextBetweenParens(c("fred[x,y]=0","b[z0]"));
#> [1] NA NA
extractTextBetweenParens("fred[x,y[z]]=0");
#> [1] NA
```
