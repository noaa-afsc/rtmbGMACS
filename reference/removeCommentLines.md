# Remove strings starting with a comment character

Function to remove strings starting with a comment character.

## Usage

``` r
removeCommentLines(strv, comment = "#")
```

## Arguments

- strv:

  \- character vector to remove comment lines from

- comment:

  \- character(s) to regard as indicating a comment

## Value

a character vector with elements that don't start with a comment
character

## Examples

``` r
txt = c("#this is a comment",
        "  #so is this,even thought the line starts with blank space",
        "this is not a comment",
        "this is not either, but the first is line 3.");
removeCommentLines(txt); #--should be \code{txt[3:4]}
#> [1] "this is not a comment"                       
#> [2] "this is not either, but the first is line 3."
```
