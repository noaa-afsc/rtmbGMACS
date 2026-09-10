# Split text string into a character vector

Function to split text string to a character vector.

## Usage

``` r
splitText(txt, split = "\\n")
```

## Arguments

- txt:

  - text string (1-elenent character vector) to split

- split:

  - string used to split text (default="\n)

## Value

a character vector

## Details

The text is plit into a character vector using
[`stringr::str_split_1()`](https://stringr.tidyverse.org/reference/str_split.html).
