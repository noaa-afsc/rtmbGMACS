# Evaluate a list from a character vector

Function to evaluate a list from a character vector.

## Usage

``` r
evalTextAsList(strv, split = "<-", verbose = FALSE)
```

## Arguments

- strv:

  - character vector to parse/evaluate

- split:

  - character(s) to use to split lines into name and value
    (default="\<-")

- verbose:

  - flag to print diagnostic info

## Value

list with named elements

## Details

Each quantity assigned using `<-` in `strv` is interpreted as an
equation defining an element of the returned list. Quantities assigned
using `=` outside a list structure are evaluated locally for use in
constructing a list element of the returned list. Attributes for a list
element of the returned list must be assigned (see example code 2)
immediately after the list element.

## Examples

``` r
if (FALSE) {
# example code 1
str=paste(
  'MODEL_DIMS
  y <- 2020:2024;                           #--years
  s <- 1;                                   #--seasons
  r <- "EBS";                               #--regions
  x <- c("male","female");                  #--sex classes
  m <- c("immature","mature");              #--maturity state classes
  p <- c("new_shell","old_shell");          #--post-molt ages
  zc <- seq(55.5,104.5,5);                  #--size bin cutpoints
  f <- c("TCF","SCF","NMFS");               #--fleets
  END');
strv = str |> splitText() |> extractLines("MODEL_DIMS","END");
lstDims = evalTextAsList(strv);
# example code 2
  str=paste(
  'MODEL_DIMS
    r <- "EBS";
    miz = as.character(seq(25,75,5)); #--size bin midpoints for immature males
    mmz = as.character(seq(50,75,5)); #--size bin midpoints for mature males
    fiz = as.character(seq(25,65,5)); #--size bin midpoints for immature females
    fmz = as.character(seq(30,65,5)); #--size bin midpoints for mature females
    x<-list(male=list(imm=list(`new`=miz,
                                 `old`=miz),
                        mat=list(`new`=mmz,
                                 `old`=mmz,
                                 `very`=mmz)
                      ),
              female=list(imm=list(`new`=fiz,
                                   `old`=fiz),
                          mat=list(`new`=fmz,
                                   `old`=fmz)
                         )
             );
    attr(x,"dmnms") = c("x","m","p","z");#--define order of dimensions in nested list
  END'
  )
  strv = str |> splitText() |> extractLines("MODEL_DIMS","END");
  lstDms = evalTextAsList(strv);
}
```
