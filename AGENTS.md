# Project Instructions

## R code style

1. Always use spaces in R code. Do not use tabs.
2. Qualify functions with their package names using `package::function()`, except for functions from the `base` and `stats` packages.
3. Use two spaces for each indentation level.
4. When defining a function, place the opening curly brace on the same line as the function signature. Place the closing curly brace on a new line after the function body unless the entire function body is written on one line.
5. Functions that return values must use an explicit `return()` statement.
6. For `for`, `while`, and `if` statements, place the opening curly brace at the end of the loop or test declaration. Place the closing curly brace on a new line after the enclosed code.
7. Indent code enclosed by loops and conditional statements two spaces beyond the statement declaration.
8. Always end a code statement with ";".

## RTMB functions

Functions that implement autodifferention functionality found in the RTMB package should:

1. have "rtmbFunction" as a roxygen keyword
2. indicate which function variables are "data" and which are "param"'s in roxygen @param declaration
3. include the following code lines as the first executable lines in the function
    - "[<-"  <- ADoverload("[<-");
    - "c"    <- ADoverload("[c");
    - "diag<-" <- ADoverload("[diag<-");
4. preallocate (if possible) AD vectors using 'AD(vector("numeric",n)) where "n" is the vector length
5. use the Matrix package to create and manipulate matrix objects
6. always use Matrix::expm rather than expm::expm for matrix exponentiation.

Tests for each function (say "fcn(x,y,z,a)") should create a scaffold that: 

1. starts the testing function by calling "library(RTMB)"
2. assigns values to input variables (e.g., x,y,z)
3. calculates fcn based on selected values for inputs in "normal" R, e.g.

   valsR = fcn(x,y,z,a);
   
4. identifies which function variables are "data" from the associated man page 
   and creates a list named "data" with elements named for each variable identified as "data", 
   e.g. with x, y the "data" variables
   
   data = list(x=x,y=y);
   
5. identifies which function variables are "param"s from the associated man page 
   and creates a named list "params" with elements named for each variable identified as "param",
   e.g. with z and a as param variables
   
   params = list(z=z,a=a);
   
6. creates a modified function "fcn1" (using fcn(x,y,z,a) as an example, 
   where x and y are marked as "data" and z and a are marked as "param"s) as 
   
  fcn1 <- function(data,params){
    val = fcn(data$x,data$y,params$z,params$a);
    return(val);
  }
7. creates an "enclosing"" function "encf" as 
  
  encf <- function(f,data){function(params){f(data,params)}}
  
8. calculates the number of parameters N as 

  N <-sum(sapply(params,length))
  
9. creates an RTMB "tape" of the modified function

  F <- MakeTape(encf(fcn1, data), numeric(N));
  
10. evaluates F

  p = as.numeric(unlist(params));
  
  valRTMB <- F(p)
  
11. asserts that valR = valRTMB

## Markdown/Quarto style

1. in a markdown cell, start code indented once as in Example

Example:

```r
  calculate_mean <- function(x) {
    if (length(x) == 0) {
      return(NA_real_);
    }

    values <- dplyr::filter(x, !is.na(value));

    for (i in seq_len(nrow(values))) {
      values$value[i] <- as.numeric(values$value[i]);
    }

    return(mean(values$value));
  }
```

## Repository website

1. Always use quarto/_quarto.yml to create repository website on gh-pages branch
