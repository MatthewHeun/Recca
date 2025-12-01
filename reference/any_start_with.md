# Tell if any of a vector of strings starts with another string

This function returns `TRUE` if any of the strings in `x` starts with
the string in `target` and `FALSE` otherwise.

## Usage

``` r
any_start_with(x, target)
```

## Arguments

- x:

  a vector or list of strings

- target:

  a string (or a vector or list of strings)

## Value

`TRUE` if any of `x` starts with `target`, `FALSE` otherwise. If
`target` is a vector or list, the return value is the same length as
`target` and contains the result of applying the test to each item in
`target`.

## Details

This function is vectorized. If `target` is a vector or list of strings,
the return value is the same length as `target` and contains the result
of applying the same test (do any of `x` start with `target`?) to each
item in `target`.

## Examples

``` r
# TRUE, because one of the x string ("bd") starts with "b"
any_start_with(x = c("ad", "bd", "cd"), target = "b")
#> [1] TRUE
# TRUE, because two of the x strings starts with "Production"
any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Production")
#> [1] TRUE
# FALSE, because none of the x strings starts with "Offshore"
any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Offshore")
#> [1] FALSE
# TRUE FALSE, because the x strings start with "Production" but not "Offshore"
any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"),
               target = c("Production", "Offshore"))
#> [1]  TRUE FALSE
```
