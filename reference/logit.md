# Convert values to the logit scale

Function to convert values to the logit scale.

## Usage

``` r
logit(x)
```

## Arguments

- x:

  \- numeric object

## Value

log(x/(1-x)), with same names and class as \`x\`

## Details

inverse of logistic function. Values of \`x\` outside the (0,1) range
generate NaNs.
