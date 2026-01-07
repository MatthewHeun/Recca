# Verify inter- and intra-industry balances for PSUT (**RUVY**) matrices

Balances can be verified in two ways, inter-industry and intra-industry.
Inter-industry balances are between industries and performed by product.
Intra-industry balances are across industries and performed per
industry.

## Usage

``` r
verify_inter_industry_balance(
  .sutmats = NULL,
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  balances = Recca::balance_cols$inter_industry_balance_colname,
  tol = 1e-06,
  balanced = Recca::balance_cols$inter_industry_balanced_colname,
  delete_balance_if_verified = FALSE
)

verify_intra_industry_balance(
  .sutmats = NULL,
  U = Recca::psut_cols$U,
  V = Recca::psut_cols$V,
  balances = Recca::balance_cols$intra_industry_balance_colname,
  tol = 1e-06,
  balanced = Recca::balance_cols$intra_industry_balanced_colname,
  delete_balance_if_verified = FALSE
)
```

## Arguments

- .sutmats:

  An SUT-style data frame with a column named `balances`.

- R:

  Resources (**R**) matrix or name of the column in `.sutmats` that
  contains same. Default is "R".

- U:

  Use (**U**) matrix or name of the column in `.sutmats` that contains
  same. Default is "U".

- V:

  Make (**V**) matrix or name of the column in `.sutmats` that contains
  same. Default is "V".

- Y:

  Final demand (**Y**) matrix or name of the column in `.sutmats` that
  contains same. Default is "Y".

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

- balanced:

  The name for booleans telling if balance is present. For
  `verify_inter_industry_balances()`, the default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$inter_industry_balanced_colname`
  or "SUTInterIndustryBalanced". For `verify_intra_industry_balances()`,
  the default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$intra_industry_balanced_colname`
  or "SUTIntraIndustryBalanced".

- delete_balance_if_verified:

  A boolean that tells whether to delete the `balances` and
  `balanced_colname` columns if `.sutmats` is a data frame and if
  balances are verified. Default is `FALSE`. If individual matrices are
  specified in the `R`, `U`, `V`, and `Y` arguments, no deletion is
  performed.

## Value

A list or data frame with an additional value or column saying whether
`.sutmats` are in balance.

## Details

### Inter-industry balances (by product)

In a PSUT description of an economy, all of every product leaving one
industry must arrive at another industry. Inter-industry
(between-industry) balances are verified by product (within `tol`) for
every row in `.sutmats`. Inter-industry balances can be calculated via
`calc_inter_industry_balances()` before calling
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
df <- UKEnergy2000mats |>
  dplyr::filter(LastStage %in% c("Final", "Useful")) |>
  tidyr::pivot_wider(names_from = matrix.name,
                     values_from = matrix)
df |>
  calc_inter_industry_balance() |>
  verify_inter_industry_balance(tol = 1e-4) |>
  dplyr::glimpse()
#> Rows: 2
#> Columns: 14
#> $ Country                  <chr> "GBR", "GBR"
#> $ Year                     <dbl> 2000, 2000
#> $ EnergyType               <chr> "E", "E"
#> $ LastStage                <chr> "Final", "Useful"
#> $ R                        <list> <<matrix[2 x 2]>>, <<matrix[2 x 2]>>
#> $ U                        <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ U_EIOU                   <list> <<matrix[7 x 8]>>, <<matrix[3 x 8]>>
#> $ U_feed                   <list> <<matrix[9 x 9]>>, <<matrix[12 x 13]>>
#> $ V                        <list> <<matrix[9 x 10]>>, <<matrix[13 x 14]>>
#> $ Y                        <list> <<matrix[4 x 2]>>, <<matrix[4 x 2]>>
#> $ r_EIOU                   <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ S_units                  <list> <<matrix[12 x 1]>>, <<matrix[16 x 1]>>
#> $ SUTInterIndustryBalance  <list> <<matrix[12 x 1]>>, <<matrix[16 x 1]>>
#> $ SUTInterIndustryBalanced <lgl> TRUE, TRUE
# Also works without first calculating the balances
df |>
  verify_inter_industry_balance(tol = 1e-4) |>
  glimpse()
#> Rows: 2
#> Columns: 13
#> $ Country                  <chr> "GBR", "GBR"
#> $ Year                     <dbl> 2000, 2000
#> $ EnergyType               <chr> "E", "E"
#> $ LastStage                <chr> "Final", "Useful"
#> $ R                        <list> <<matrix[2 x 2]>>, <<matrix[2 x 2]>>
#> $ U                        <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ U_EIOU                   <list> <<matrix[7 x 8]>>, <<matrix[3 x 8]>>
#> $ U_feed                   <list> <<matrix[9 x 9]>>, <<matrix[12 x 13]>>
#> $ V                        <list> <<matrix[9 x 10]>>, <<matrix[13 x 14]>>
#> $ Y                        <list> <<matrix[4 x 2]>>, <<matrix[4 x 2]>>
#> $ r_EIOU                   <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ S_units                  <list> <<matrix[12 x 1]>>, <<matrix[16 x 1]>>
#> $ SUTInterIndustryBalanced <lgl> TRUE, TRUE
df |>
  calc_intra_industry_balance() |>
  glimpse()
#> Rows: 2
#> Columns: 13
#> $ Country                 <chr> "GBR", "GBR"
#> $ Year                    <dbl> 2000, 2000
#> $ EnergyType              <chr> "E", "E"
#> $ LastStage               <chr> "Final", "Useful"
#> $ R                       <list> <<matrix[2 x 2]>>, <<matrix[2 x 2]>>
#> $ U                       <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ U_EIOU                  <list> <<matrix[7 x 8]>>, <<matrix[3 x 8]>>
#> $ U_feed                  <list> <<matrix[9 x 9]>>, <<matrix[12 x 13]>>
#> $ V                       <list> <<matrix[9 x 10]>>, <<matrix[13 x 14]>>
#> $ Y                       <list> <<matrix[4 x 2]>>, <<matrix[4 x 2]>>
#> $ r_EIOU                  <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ S_units                 <list> <<matrix[12 x 1]>>, <<matrix[16 x 1]>>
#> $ SUTIntraIndustryBalance <list> <<matrix[9 x 1]>>, <<matrix[13 x 1]>>
# Not run, because it fails and emits a warning
if (FALSE) { # \dontrun{
df |>
  verify_intra_industry_balance()
} # }
```
