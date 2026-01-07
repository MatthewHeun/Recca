# Columns in a data frame that contain final-to-useful allocations

A string list containing named names of columns in PSUT data frames.
Items in the list provide default values for column name function
arguments throughout the `Recca` package.

## Usage

``` r
alloc_cols
```

## Format

A string list with 2 entries.

- C_Y:

  The name of a column in a wide-by-matrices data frame containing
  allocations to final-to-useful machines used in final demand. "C_Y"

- C_eiou:

  The name of a column in a wide-by-matrices data frame containing
  allocations to final-to-useful machines used in final demand. "C_EIOU"

## Examples

``` r
alloc_cols
#> $C_Y
#> [1] "C_Y"
#> 
#> $C_eiou
#> [1] "C_EIOU"
#> 
```
