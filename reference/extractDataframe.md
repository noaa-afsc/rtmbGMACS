# Extract a dataframe (a [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)) from a character vector

Function to a dataframe (a
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html))
from a character vector.

## Usage

``` r
extractDataframe(txt, n, start, comment = "#")
```

## Arguments

- txt:

  - character vector to search

- n:

  - number of non-comment lines to extract

- start:

  - index into `txt` to regard as the start of the search

- comment:

  - character indicating the following text in the string is a comment

## Value

list with elements `txt` and `end` (see details)

## Details

Each element in `txt` is regarded as a line of text (as in a text file).
The dataframe structure and column types are determined by
[`readr::read_table()`](https://readr.tidyverse.org/reference/read_table.html).

- txt - character vector of length `n` with comment lines removed (i.e.,
  a subset of `txt`)

- end - index of the last element extracted in the input `txt` vector

This function differs from
[`evalTextAsDataframe()`](https://noaa-afsc.github.io/rtmbGMACS/reference/evalTextAsDataframe.md)
in that the input character vector may be a much larger section of text
than what defines the dataframe to be extracted, but the number of rows
(`n`) to extract is known, as is the index of the element to begin with
(`start`). In
[`evalTextAsDataframe()`](https://noaa-afsc.github.io/rtmbGMACS/reference/evalTextAsDataframe.md),
after comments are removed are removed from the input character vector
`char_vec`, the resulting character vector is regarded as defining the
dataframe.
