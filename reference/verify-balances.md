# Verify inter- and intra-industry balances for PSUT (**RUVY**) matrices

Balances can be verified in two ways, inter-industry and intra-industry.
Inter-industry balances are between industries and performed by product.
Intra-industry balances are across industries and performed per
industry.

## Usage

``` r
verify_inter_industry_balance(
  .sutmats = NULL,
  balances = Recca::balance_cols$inter_industry_balance_colname,
  tol = 1e-06,
  balanced_colname = Recca::balance_cols$inter_industry_balanced_colname
)

verify_intra_industry_balance(
  .sutmats = NULL,
  balances = Recca::balance_cols$intra_industry_balance_colname,
  tol = 1e-06,
  balanced_colname = Recca::balance_cols$intra_industry_balanced_colname
)
```

## Arguments

- .sutmats:

  An SUT-style data frame with a column named `balances`.

- balances:

  The name of a column that contains balance vectors. For
  `verify_inter_industry_balances()`, the default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$inter_industry_balance_colname`
  or "SUTInterIndustryBalance". For `verify_intra_industry_balances()`,
  the default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$intra_industry_balance_colname`
  or "SUTIntraIndustryBalance".

- tol:

  The maximum amount by which products can be out of balance and still
  be considered balanced. Default is `1e-6`.

- balanced_colname:

  The name for booleans telling if balance is present. For
  `verify_inter_industry_balances()`, the default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$inter_industry_balanced_colname`
  or "SUTInterIndustryBalanced". For `verify_intra_industry_balances()`,
  the default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$intra_industry_balanced_colname`
  or "SUTIntraIndustryBalanced".

## Value

A list or data frame with an additional value or column saying whether
`.sutmats` are in balance.

## Details

### Inter-industry balances (by product)

In a PSUT description of an economy, all of every product leaving one
industry must arrive at another industry. Inter-industry
(between-industry) balances are verified by product (within `tol`) for
every row in `.sutmats`. Inter-industry balances should be calculated
via `calc_inter_industry_balances()` before calling
`verify_inter_industry_balances()`. See examples.

### Intra-industry balances (by industry)

Intra-industry (across-industry) balances are verified by industry
(within `tol`) for every row in `.sutmats`. Intra-industry balances
should be calculated via `calc_intra_industry_balances()` before calling
`verify_intra_industry_balances()`. See examples.

The calculation of inter-industry balances will give non-zero vectors
for quantities that are not conserved (such as exergy) and for
conversion chains that do not include wastes or losses.

### Outputs

If every conversion chain in `.sutmats` is balanced, `.sutmats` is
returned with an additional column (called `balanced_colname`), and
execution returns to the caller. If balance is not observed for one or
more of the rows in `.sutmats`, a warning is emitted, and the additional
column (`balanced_colname`) indicates where the problem occurred, with
`FALSE` showing where products are not balanced to within `tol`.

Typically, one would call `calc_int*_industry_balance()` before calling
`verify_int*_industry_balance()`. See the examples.

## See also

[`calc_inter_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md),
[`calc_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md)

## Examples

``` r
library(dplyr)
library(tidyr)
result_inter <- UKEnergy2000mats |>
  dplyr::filter(LastStage %in% c("Final", "Useful")) |>
  tidyr::pivot_wider(names_from = matrix.name,
                     values_from = matrix) |>
  calc_inter_industry_balance() |>
  verify_inter_industry_balance(tol = 1e-4)
result_inter
#> # A tibble: 2 × 14
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 6 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>,
#> #   SUTInterIndustryBalance <list>, SUTInterIndustryBalanced <lgl>
result_inter[[Recca::balance_cols$inter_industry_balanced_colname]]
#> [1] TRUE TRUE
```
