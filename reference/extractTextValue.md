# Extract a (character) value from a line of text

Function to extract a (character) value from a line of text.

## Usage

``` r
extractTextValue(txt, comment = "#")
```

## Arguments

- txt:

  \- input text string (1-element character vector)

- comment:

  \- string used to split text into value and comments (default="#")

## Value

a 1-element character vector

## Details

The 1-line input text is split into a character vector using
\[stringr::str_split_1()\] and the first element of the result is
retained. Whitespace preceding and following the retained text is also
removed (but "interior" whitespace is retained).

## Examples

``` r
# example code
txt = "'hows this'  #--input text of interest";
extractTextValue(txt)
#> [1] "'hows this'"
```
