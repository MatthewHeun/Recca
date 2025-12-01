# Verifies SUT inter-industry energy balances

**\[deprecated\]**

## Usage

``` r
verify_SUT_energy_balance(
  .sutmats = NULL,
  R = "R",
  U = "U",
  V = "V",
  Y = "Y",
  tol = 1e-06,
  SUT_energy_balance = ".SUT_energy_balance"
)
```

## Arguments

- .sutmats:

  an SUT-style data frame with columns of matrices, including `U`, `V`,
  and `Y` columns.

- R:

  resources (`R`) matrix or name of the column in `.sutmats` that
  contains same. Default is "R".

- U:

  use (`U`) matrix or name of the column in `.sutmats` that contains
  same. Default is "U".

- V:

  make (`V`) matrix or name of the column in `.sutmats`that contains
  same. Default is "V".

- Y:

  final demand (`Y`) matrix or name of the column in `.sutmats` that
  contains same. Default is "Y".

- tol:

  the maximum amount by which Supply and Consumption can be out of
  balance. Default is `1e-6`.

- SUT_energy_balance:

  The name of the column in output containing logical values
  representing the balance status of that row.

## Value

a list or data frame saying whether `.sutmats` are in balance.

## Details

`verify_SUT_energy_balance()` is deprecated in favor of the combination
of
[`calc_inter_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md)
and
[`verify_inter_industry_balance()`](https://matthewheun.github.io/Recca/reference/verify-balances.md).
Pipe the first into the second.

Energy balances are confirmed by Product (within `tol`) for every row in
`.sutmats`.

If energy is in balance for every row, `.sutmats` is returned with an
additional column, and execution returns to the caller. If energy
balance is not observed for one or more of the rows, a warning is
emitted, and the additional column (`SUT_energy_blance`) indicates where
the problem occurred, with `FALSE` showing where energy is not in
balance.

## Examples

``` r
library(dplyr)
library(tidyr)
# This function is deprecated.
# Instead of this:
UKEnergy2000mats |>
  dplyr::filter(LastStage %in% c("Final", "Useful")) |>
  tidyr::spread(key = matrix.name, value = matrix) |>
  verify_SUT_energy_balance(tol = 1e-4)
#> Warning: `verify_SUT_energy_balance()` was deprecated in Recca 0.1.65.
#> ℹ Please use `verify_inter_industry_balance()` instead.
#> # A tibble: 2 × 13
#>   Country  Year EnergyType LastStage R             S_units  U        U_EIOU  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 5 more variables: U_feed <list>, V <list>, Y <list>, r_EIOU <list>,
#> #   .SUT_energy_balance <lgl>
# Do this:
UKEnergy2000mats |>
  dplyr::filter(LastStage %in% c("Final", "Useful")) |>
  tidyr::spread(key = matrix.name, value = matrix) |>
  calc_inter_industry_balance() |>
  verify_inter_industry_balance(tol = 1e-4)
#> # A tibble: 2 × 14
#>   Country  Year EnergyType LastStage R             S_units  U        U_EIOU  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 6 more variables: U_feed <list>, V <list>, Y <list>, r_EIOU <list>,
#> #   SUTInterIndustryBalance <list>, SUTInterIndustryBalanced <lgl>
```
