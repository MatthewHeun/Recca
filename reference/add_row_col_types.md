# Infer and set row and column types

Given a list of matrices and their names, this function sets the row and
column types according to the following algorithm:

- **R** and **V**: Row types are `industry_type`; Column types are
  `product_type`.

- **U**, **Y**, **U_feed**, **U_EIOU**, **r_EIOU**: Row types are
  `product_type`; Column types are `industry_type`.

- **S_units**: Row types are `product_type`; Column types are
  `unit_type`.

## Usage

``` r
add_row_col_types(
  .df = NULL,
  matvals = Recca::psut_cols$matvals,
  matnames = Recca::psut_cols$matnames,
  with_row_col_types_colname = "WithRCTypes",
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  U_feed = Recca::psut_cols$U_feed,
  U_eiou = Recca::psut_cols$U_eiou,
  r_eiou = Recca::psut_cols$r_eiou,
  S_units = Recca::psut_cols$S_units,
  industry_type = Recca::row_col_types$industry_type,
  product_type = Recca::row_col_types$product_type,
  unit_type = Recca::row_col_types$unit_type
)
```

## Arguments

- .df:

  A data frame containing columns of matrix names and matrices. Default
  is `NULL`.

- matvals:

  A list of matrices to have their row and column types set or the name
  of a column of matrices in `.df` whose row and column types are to be
  set. Default is `psut_cols$matvals`.

- matnames:

  A list of names of the matrices in `matvals` or the name of a column
  of matrix names in `.df`. Default is `psut_cols$matnames`.

- with_row_col_types_colname:

  The name of a column in the output data frame whose matrices have row
  and column types set. Default is "WithRCTypes".

- R, U, V, Y, U_feed, U_eiou, r_eiou, S_units:

  Names of matrices. Defaults are from
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md).

- industry_type, product_type, unit_type:

  String names of row and column types. Defaults are from
  [`Recca::row_col_types`](https://matthewheun.github.io/Recca/reference/row_col_types.md).

## Value

`.mats` with row and product types set.

## Details

Note that matrix names are matched via
[`startsWith_any_of()`](https://matthewheun.github.io/Recca/reference/startsWith_any_of.md).

This function uses
[`matsindf::matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.html)
internally, so `matnames` and `matvals` can be lists (`.df` should be
`NULL`) or string names of columns in `.df`.

## Examples

``` r
mats <- list(R = matrix(1), U = matrix(2),
             V = matrix(3), Y = matrix(4),
             U_feed = matrix(5), U_EIOU = matrix(6),
             r_EIOU = matrix(7), S_units = matrix(8))
add_row_col_types(matnames = names(mats),
                  matvals = mats)
#> $WithRCTypes
#> $WithRCTypes[[1]]
#>      [,1]
#> [1,]    1
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
#> $WithRCTypes[[2]]
#>      [,1]
#> [1,]    2
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> $WithRCTypes[[3]]
#>      [,1]
#> [1,]    3
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
#> $WithRCTypes[[4]]
#>      [,1]
#> [1,]    4
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> $WithRCTypes[[5]]
#>      [,1]
#> [1,]    5
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> $WithRCTypes[[6]]
#>      [,1]
#> [1,]    6
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> $WithRCTypes[[7]]
#>      [,1]
#> [1,]    7
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> $WithRCTypes[[8]]
#>      [,1]
#> [1,]    8
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Unit"
#> 
#> 
df <- tibble::tibble(matnames = c("R", "S_units"),
                     matvals = list(matrix(1), matrix(2)))
df
#> # A tibble: 2 × 2
#>   matnames matvals      
#>   <chr>    <list>       
#> 1 R        <dbl [1 × 1]>
#> 2 S_units  <dbl [1 × 1]>
res <- df |>
  add_row_col_types(matnames = df$matnames,
                    matvals = df$matvals)
res
#> # A tibble: 2 × 3
#>   this_mat      this_name WithRCTypes  
#>   <list>        <chr>     <list>       
#> 1 <dbl [1 × 1]> R         <dbl [1 × 1]>
#> 2 <dbl [1 × 1]> S_units   <dbl [1 × 1]>
res$WithRCTypes[[1]]
#>      [,1]
#> [1,]    1
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
res$WithRCTypes[[2]]
#>      [,1]
#> [1,]    2
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Unit"
```
