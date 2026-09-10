# Extract character vector from section of longer vector

Function to extract character vector from section of longer vector.

## Usage

``` r
extractLines(txt, start, end)
```

## Arguments

- txt:

  - long character vector

- start:

  - string to regard as marking the start of section (not included in
    the section)

- end:

  - string to regard as marking the end of the section (not included in
    the section)

## Value

a character vector of the "enclosed" section

## Details

The text lines at which the `start` and `end` are found are not included
in the "section" returned by this function.
