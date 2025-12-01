# Tell whether each industry's inputs are unit-homogeneous

Returns `TRUE` if each industry's inputs are unit-homogeneous.

## Usage

``` r
inputs_unit_homogeneous(
  .sutmats = NULL,
  U = "U",
  S_units = "S_units",
  keep_details = FALSE,
  ins_unit_homogeneous = ".inputs_unit_homogeneous"
)
```

## Arguments

- .sutmats:

  a data frame of supply-use table matrices with matrices arranged in
  columns.

- U:

  a use (`U`) matrix or name of the column in `.sutmats` that contains
  same. Default is "`U`".

- S_units:

  an `S_units` matrix or name of a column in `.sutmats` that contains
  same. Default is "`S_units`".

- keep_details:

  if `TRUE`, per-product results are returned; if `FALSE`, per-ECC
  results are returned. Default is `FALSE`.

- ins_unit_homogeneous:

  name of the output boolean that tells whether each industry's inputs
  are unit-homogeneous. Default is "`.inputs_unit_homogeneous`".

## Value

a list or data frame containing `TRUE` if inputs to each energy
conversion industry are unit-homogeneous, `FALSE` otherwise.

## Details

The `U_bar` matrix is queried for the number of non-zero entries in each
column. If the number of non-zero entries in each column is exactly 1,
industry inputs are unit-homogeneous. Note that
`U_bar = `[`matrixproduct_byname`](https://matthewheun.github.io/matsbyname/reference/matrixproduct_byname.html)`(`[`transpose_byname`](https://matthewheun.github.io/matsbyname/reference/transpose_byname.html)`(S_units), U)`.

## Examples

``` r
library(tidyr)
UKEnergy2000mats %>%
  spread(key = "matrix.name", value = "matrix") %>%
  inputs_unit_homogeneous()
#> # A tibble: 4 × 13
#>   Country  Year EnergyType LastStage R             S_units  U        U_EIOU  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 5 more variables: U_feed <list>, V <list>, Y <list>, r_EIOU <list>,
#> #   .inputs_unit_homogeneous <lgl>
```
