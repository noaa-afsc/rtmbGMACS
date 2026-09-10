# Identify first non-comment line in a character vector

Function to identify the index of the first non-comment line in a
character vector.

## Usage

``` r
skipCommentLines(txt, start, comment = "#")
```

## Arguments

- txt:

  - character vector to search

- start:

  - index into `txt` to regard as the start of the search

- comment:

  - character indicating the following text in the string is a comment

## Value

the index of the first line in `txt` that does not start with a comment
character

## Details

Each element in `txt` is regarded as a line of text (as in a text file).
The function searches for the first non-comment line and returns its
index.

## Examples

``` r
txt = c("#this is a comment",
        "  #so is this,even thought the line starts with blank space",
        "this is not a comment",
        "this is not either, but the first is line 3.");
extractLines(txt,1); #--should be 3: txt[3] is first non-comment line
#> Error in extractLines(txt, 1): argument "end" is missing, with no default
```
