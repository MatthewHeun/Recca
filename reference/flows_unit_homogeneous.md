# Tell whether industry flows (inputs and outputs) are unit-homogeneous

Returns `TRUE` if each industry's flows (all inputs and outputs) are
unit-homogeneous.

## Usage

``` r
flows_unit_homogeneous(
  .sutmats = NULL,
  U = "U",
  V = "V",
  S_units = "S_units",
  keep_details = FALSE,
  flows_unit_homogeneous = ".flows_unit_homogeneous"
)
```

## Arguments

- .sutmats:

  a data frame of supply-use table matrices with matrices arranged in
  columns.

- U:

  a use (`U`) matrix or name of the column in `.sutmats` that contains
  same. Default is "`U`".

- V:

  a make (`V`) matrix or name of the column in `.sutmats` that contains
  same. Default is "`V`".

- S_units:

  an `S_units` matrix or name of a column in `.sutmats` that contains
  same. Default is "`S_units`".

- keep_details:

  if `TRUE`, per-industry results are returned; if `FALSE`, per-ECC
  results are returned. Default is "`FALSE`".

- flows_unit_homogeneous:

  the name of the output column that tells whether each industry's
  outputs are unit-homogeneous. Default is "`.flows_unit_homogeneous`".

## Value

`.sutdata` with additional column "`flows_unit_homogeneous`" containing
`TRUE` if each industry's flows are unit-homogeneous, `FALSE` if each
industry's flows are unit-heterogeneous.

## Details

The `V_bar` matrix is queried for the number of non-zero entries in each
row. If the number of non-zero entries in each row is exactly 1,
industry outputs are unit-homogeneous. Note that
`V_bar = `[`matrixproduct_byname`](https://matthewheun.github.io/matsbyname/reference/matrixproduct_byname.html)`(V, S_units)`.

## Examples

``` r
library(tidyr)
UKEnergy2000mats %>%
  spread(key = "matrix.name", value = "matrix") %>%
  flows_unit_homogeneous()
#> # A tibble: 4 × 13
#>   Country  Year EnergyType LastStage R             S_units  U        U_EIOU  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 5 more variables: U_feed <list>, V <list>, Y <list>, r_EIOU <list>,
#> #   .flows_unit_homogeneous <lgl>
```
