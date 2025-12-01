# Primary industry column names

A string list containing named names of columns in SUT data frames.
Items in the list provide default values for column name function
arguments throughout the `Recca` package.

## Usage

``` r
industry_cols
```

## Format

A string list with 2 entries.

- p_industries_prefixes:

  The name of a column in a wide-by-matrices data frame containing
  prefixes for names of primary industries.

- p_industries_complete:

  The name of a column in a wide-by-matrices data frame containing
  complete names of primary industries.

## Examples

``` r
industry_cols
#> $p_industry_prefixes
#> [1] "p_industry_prefixes"
#> 
#> $p_industries_complete
#> [1] "p_industries_complete"
#> 
```
