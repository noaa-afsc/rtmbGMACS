# Calculate a 0-symmetric logistic function between -1 and 1

Function to calculate a logistic function centered on 0

## Usage

``` r
symlogistic(x)
```

## Arguments

- x:

  \- numeric vector at which to calculate the symmetric logistic
  function

## Value

a numeric vector with the same size and names (if any) as \`x\`

## Details

This is the inverse to \`symlogit\` (see examples)

## Examples

``` r
if (FALSE) {
##--R context
x = seq(-5,5,0.1);
y = symlogistic(x);
head(y);
xp = symlogit(y) - x
##--RTMB context
x = RTMB::AD(seq(-5,5,0.1),force=TRUE);
y = symlogistic(x);
head(y)
}
```
