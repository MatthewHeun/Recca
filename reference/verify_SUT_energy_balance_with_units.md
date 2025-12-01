# Confirm that an SUT-style data frame conserves energy.

**\[deprecated\]**

## Usage

``` r
verify_SUT_energy_balance_with_units(
  .sutmats = NULL,
  R = "R",
  U = "U",
  U_feed = "U_feed",
  U_eiou = "U_EIOU",
  r_eiou = "r_EIOU",
  V = "V",
  Y = "Y",
  S_units = "S_units",
  tol = 1e-06,
  matnames = "matnames",
  matvals = "matvals",
  rowtypes = "rowtypes",
  coltypes = "coltypes",
  rownames = "rownames",
  colnames = "colnames",
  prod_diff = ".prod_diff",
  ind_diff = ".ind_diff",
  SUT_prod_energy_balanced = ".SUT_prod_energy_balanced",
  SUT_ind_energy_balanced = ".SUT_ind_energy_balanced",
  ebal_error = "ebal_error",
  product = "Product"
)
```

## Arguments

- .sutmats:

  An SUT-style data frame containing columns `R` (optionally), `U`, `V`,
  `Y`, and `S_units`.

- R:

  Resource (`R`) matrix or name of the column in `.sutmats` that
  contains same. Default is "R".

- U:

  Use (`U`) matrix or name of the column in `.sutmats` that contains
  same. Default is "U".

- U_feed, U_eiou, r_eiou:

  Optional matrices or columns in `.sutmats`.

- V:

  Make (`V`) matrix or name of the column in `.sutmats`that contains
  same. Default is "V".

- Y:

  Final demand (`Y`) matrix or name of the column in `.sutmats` that
  contains same. Default is "Y".

- S_units:

  `S_units` A matrix or name of the column in `.sutmats` that contains
  same. Default is "S_units".

- tol:

  The maximum amount by which energy can be out of balance. Default is
  `1e-6`.

- matnames, matvals, rowtypes, coltypes, rownames, colnames:

  Column names used internally.

- prod_diff, ind_diff, ebal_error, product:

  Column names for product and industry energy balance errors.

- SUT_prod_energy_balanced:

  The name for booleans telling if product energy is in balance. Default
  is ".SUT_prod_energy_balance".

- SUT_ind_energy_balanced:

  The name for booleans telling if product energy is in balance. Default
  is ".SUT_inds_energy_balance".

## Value

`.sutmats` with additional columns.

## Details

If energy is in balance for every row, `.sutmats` is returned with two
additional columns, and execution returns to the caller. If energy
balance is not observed for one or more rows, a warning is emitted, and
columns named `SUT_prod_energy_blance` and `SUT_ind_energy_blance` are
added to `.sutmats`. `FALSE` indicates energy is not in balance.

This function should be called for its side-effect of testing whether
energy is in balance in `.sutmats`.

Both product and industry energy balance are verified. Units (as
supplied by the `S_units` matrix) are respected.

## Examples

``` r
library(tidyr)
verify_SUT_energy_balance_with_units(UKEnergy2000mats %>%
                                       tidyr::spread(key = matrix.name, value = matrix),
                                       tol = 1e-3)
#> Warning: `verify_SUT_energy_balance_with_units()` was deprecated in Recca 0.1.65.
#> ℹ verify_SUT_energy_balance_with_units() seemed like a good idea at the time,
#>   but we never use it.
#> # A tibble: 4 × 14
#>   Country  Year EnergyType LastStage R             S_units  U        U_EIOU  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 6 more variables: U_feed <list>, V <list>, Y <list>, r_EIOU <list>,
#> #   .SUT_prod_energy_balanced <lgl>, .SUT_ind_energy_balanced <lgl>
```
