# Extract a numeric value from a line of text

Function to extract a numeric value from a line of text.

## Usage

``` r
extractNumericValue(txt, comment = "#")
```

## Arguments

- txt:

  \- input text string (1-element character vector)

- comment:

  \- string used to split text into value and comments (default="#")

## Value

a 1-element numeric vector

## Details

The 1-line input text is split into a character vector using
\[stringr::str_split_1()\] and the first element of the result is
retained. Whitespace preceding and following the retained text is also
removed (but "interior" whitespace is retained). The result is converted
to numeric using \[as.numeric()\].

## Examples

``` r
# example code
txt = "  45.27  #--input value of interest";
extractNumericValue(txt)
#> [1] 45.27
txt = "'hows this'  #--input text of interest";
extractTextValue(txt);  #--returns NA
#> [1] "'hows this'"
```
