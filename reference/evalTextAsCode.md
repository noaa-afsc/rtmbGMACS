# Evaluate text as lines of code

Function to evaluate text as lines of code.

## Usage

``` r
evalTextAsCode(strv, frame = 0)
```

## Arguments

- strv:

  \- character vector with lines of code to evaluate.

## Value

nothing

## Details

The lines of \`code\` in strv are evaluated in the frame specified by
\`frame\`. If \`frame\` is non-negative, it specifies the parent frame
relative to the caller (so \`frame=0\`, the deault, is evaluated in the
caller's frame). If \`frame\` is negative, the lines of code are
evaluated in the environment specified by its absolute value.

## Examples
