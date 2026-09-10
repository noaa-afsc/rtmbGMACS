# Project Instructions

## R code style

1.  Always use spaces in R code. Do not use tabs.
2.  Qualify functions with their package names using
    `package::function()`, except for functions from the `base` and
    `stats` packages.
3.  Use two spaces for each indentation level.
4.  When defining a function, place the opening curly brace on the same
    line as the function signature. Place the closing curly brace on a
    new line after the function body unless the entire function body is
    written on one line.
5.  Functions that return values must use an explicit
    [`return()`](https://rdrr.io/r/base/function.html) statement.
6.  For `for`, `while`, and `if` statements, place the opening curly
    brace at the end of the loop or test declaration. Place the closing
    curly brace on a new line after the enclosed code.
7.  Indent code enclosed by loops and conditional statements two spaces
    beyond the statement declaration.
8.  in a markdown cell, start code indented once as in Example

Example:

``` r

  calculate_mean <- function(x) {
    if (length(x) == 0) {
      return(NA_real_)
    }

    values <- dplyr::filter(x, !is.na(value))

    for (i in seq_len(nrow(values))) {
      values$value[i] <- as.numeric(values$value[i])
    }

    return(mean(values$value))
  }
```
