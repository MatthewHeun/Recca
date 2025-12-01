# Tell if a string starts with any of a vector of strings

This function returns `TRUE` if `x` starts with any of the strings in
`target` and `FALSE` otherwise.

## Usage

``` r
startsWith_any_of(x, prefixes)
```

## Arguments

- x:

  a string (or vector or list of strings)

- prefixes:

  a vector or list of strings

## Value

`TRUE` if `x` starts with any of the strings in `target`, `FALSE`
otherwise. If `x` is a vector or list of strings, the return value is
the same length as `x` and contains the result of applying the test to
each item in `x`.

## Details

This function is vectorized. If `x` is a vector or list of strings, the
return value has the same length as `x` and contains the result of
applying the test (does `x` start with any of `target`) for each item in
`x`.

## Examples

``` r
startsWith_any_of(x = "prefix - suffix", prefixes = c("a", "b", "prefix"))
#> [1] TRUE
startsWith_any_of(x = "prefix - suffix", prefixes = c("a", "b", "c"))
#> [1] FALSE
startsWith_any_of(x = "prefix - suffix", prefixes = "suffix")
#> [1] FALSE
startsWith_any_of(x = c("Production - Crude", "Production - NG",
                         "Exports - Oil", "Exports - Crude"),
                   prefixes = c("Production", "Imports"))
#> [1]  TRUE  TRUE FALSE FALSE
```
