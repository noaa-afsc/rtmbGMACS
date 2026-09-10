# Extract a text section with only non-comment lines from a character vector

Function to extract a text section with only non-comment lines from a
character vector.

## Usage

``` r
extractTextSection(txt, n = length(txt), start = 1, comment = "#")
```

## Arguments

- txt:

  - character vector to search

- n:

  - number of non-comment lines to extract (default=length(txt))

- start:

  - index into `txt` to regard as the start of the search (default=1)

- comment:

  - character indicating the following text in the string is a comment

## Value

list with elements `txt` and `end` (see details)

## Details

Each element in `txt` is regarded as a line of text (as in a text file).
The function searches for the first `n` non-comment lines and extracts
them as a new vector. The returned list has elements

- txt - character vector of length `n` with comment lines removed (i.e.,
  a subset of `txt`)

- end - index of the last element extracted in the input `txt` vector
