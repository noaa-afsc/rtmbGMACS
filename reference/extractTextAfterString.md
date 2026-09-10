# Extract text after a string

Function to extract text after a string

## Usage

``` r
extractTextAfterString(txt, str)
```

## Arguments

- txt:

  \- character vector from which to extract strings

- str:

  \- string to extract after (see NOTE under details)

## Value

character vector of extracted strings

## Details

For each element in the vector, this extracts the string following the
\`after\` string from each element of the input character vector, or
\`NA\` if no \`after\` string found.

NOTE: Characters with special regex meanings (e.g., ")","\]") need to be
double-escaped to use in \`str\`.

## Examples

``` r
# example code
extractTextAfterString("test:fred[x,y]==0","==");
#> [1] "0"
extractTextAfterString(c("fred[x,y]==0: FALSE","b[z0]: 1"),":");
#> [1] " FALSE" " 1"    
extractTextAfterString("fred[x,y[z]]=0","=");
#> [1] "0"
extractTextAfterString("fred[x,y[z]]=0","\\[");
#> [1] "x,y[z]]=0"
extractTextAfterString("fred[x,y[z]]=0","fred\\[");
#> [1] "x,y[z]]=0"
```
