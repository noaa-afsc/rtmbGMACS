# Evaluate a character vector as a dataframe (a `tbl_df`)

Function to evaluate a character vector as a dataframe (a `tbl_df`).

## Usage

``` r
evalTextAsDataframe(char_vec, comment = "#")
```

## Arguments

- char_vec:

  - character vector to parse/evaluate as a dataframe

- comment:

  - string indicating that what follows is a comment

## Value

a dataframe (actually a `tbl_df`, see
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html))

## Details

Each element in `char_vec` is regarded as a line of text (as in a text
file). The dataframe structure and column types are determined by
[`readr::read_table()`](https://readr.tidyverse.org/reference/read_table.html).

This function differs from
[`extractDataframe()`](https://noaa-afsc.github.io/rtmbGMACS/reference/extractDataframe.md)
in that, after comments are removed are removed from the input character
vector `char_vec`, the resulting character vector is regarded as
defining the dataframe. In
[`extractDataframe()`](https://noaa-afsc.github.io/rtmbGMACS/reference/extractDataframe.md),
the input character vector may be a much larger section of text than
what defines the dataframe to be extracted, but the number of rows (`n`)
to extract is known as, is the index of the element to begin with
(`start`).

## Examples

``` r
if (FALSE) {
# example code
str<-paste0('
  id        function  frame   params              description
  pwrLaw1   pwrLaw1   mfALL   pA,pB               w(z)_=_pA_*_(z^pB)
  pwrLaw2   pwrLaw2   mf2024  pLnA,pLnS,pB,pZ0    w(z)_=_exp(pLnA+pLnS+pB*ln(z/pZ0))
')
dfr = txt |> splitText() |> evalTextAsDataframe();
}
```
