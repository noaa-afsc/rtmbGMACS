# Calculate a 0-symmetric logit function

Function to calculate a logit function centered on 0

## Usage

``` r
symlogit(x)
```

## Arguments

- x:

  \- numeric vector with values from -1.0 to 1.0 at which to calculate
  the symmetric logit function

## Value

a numeric vector with the same size and names (if any) as \`x\`

## Details

This is the inverse to \`symlogistic\` (see examples)

## Examples

``` r
if (FALSE) {
##--R context
x = seq(-0.99,0.99,0.01);
y = symlogit(x);
head(y);
xp = symlogistic(y) - x
##--RTMB context
x = RTMB::AD(seq(-0.49,0.49,0.01),force=TRUE);
y = symlogit(x);
head(y)
}
```
