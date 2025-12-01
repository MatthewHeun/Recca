# Calculate inter- and intra-industry balances for PSUT (**RUVY**) matrices2

Balances are an important aspect of analyzing mass and energy conversion
chains in the PSUT framework with the **RUVY** matrices. Often, balances
are calculated on energy or exergy flows. Balances can be calculated for
mass flows or monetary flows, too.

## Usage

``` r
calc_inter_industry_balance(
  .sutmats = NULL,
  R = "R",
  U = "U",
  V = "V",
  Y = "Y",
  balance_colname = Recca::balance_cols$inter_industry_balance_colname
)

calc_intra_industry_balance(
  .sutmats = NULL,
  U = "U",
  V = "V",
  balance_colname = Recca::balance_cols$intra_industry_balance_colname
)
```

## Arguments

- .sutmats:

  A named list of matrices or an SUT-style, wide-by-matrices data frame
  with columns of matrices, including `R`, `U`, `V`, and `Y`.

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

- balance_colname:

  The name of the column containing energy balance vectors. Defaults are
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$inter_industry_balance_colname`
  or "SUTInterIndustryBalance" for `calc_inter_industry_balances()` and
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$intra_industry_balance_colname`
  or "SUTIntraIndustryBalance" for `calc_intra_industry_balances()`.

## Value

A list or data frame containing balances.

## Details

### Inter-industry balances

In a PSUT description of a mass or energy conversion chain, all of every
product leaving one industry must arrive at another industry.
`calc_inter_industry_balances()` calculates these between-industry
balances via ((**R** + **V**)^T^ - (**U** + **Y**))**i**. Inter-industry
balances are calculated for products (not industries).

### Intra-industry balances

`calc_intra_industry_balances()` calculates across-industry balances via
(**U**^T^ - **V**)**i**. Inter-industry balances are calculated for
industries (not products), and the result is a column vector of industry
balances.

### Outputs

The argument `balance_colname` specifies the name of the result. By
default, the output of `calc_inter_industry_balances()` is
[balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$inter_industry_balance_colname`
or "SUTInterIndustryBalance". By default, the output of
`calc_intra_industry_balances()` is
[balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$intra_industry_balance_colname`
or "SUTIntraIndustryBalance".

For inter-industry (between-industry) balances, the result is a column
vector of product balances. The result should **always** be the **0**
vector.

In a PSUT description of a mass or energy conversion chain, the meaning
of intra-industry (across-industry) balances depends on the construction
of the matrices. When all losses are accounted in the matrices
themselves, the calculation of intra-industry balances for mass and
energy conversion chains should give the **0** vector with industries in
rows, because mass and energy are conserved. When losses are **not**
accounted in the matrices, the calculation of intra-industry balances
gives losses.

For exergy conversion chains (mass, energy, or both), when losses are
accounted in the matrices, the intra-industry balance gives exergy
destruction. When losses are **not** accounted in the matrices, the
intra-industry balance gives the sum of exergy destruction and exergy
losses.

## See also

[`verify_inter_industry_balance()`](https://matthewheun.github.io/Recca/reference/verify-balances.md)
and
[`verify_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/verify-balances.md)

## Examples

``` r
# Inter-industry balances
inter <- UKEnergy2000mats |>
  dplyr::filter(LastStage %in% c("Final", "Useful")) |>
  tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
  calc_inter_industry_balance()
inter[[Recca::balance_cols$inter_industry_balance_colname]][[1]]
#>                     Industry
#> Crude                      0
#> Crude [from Dist.]         0
#> Crude [from Fields]        0
#> Diesel                     0
#> Diesel [from Dist.]        0
#> Elect                      0
#> Elect [from Grid]          0
#> NG                         0
#> NG [from Dist.]            0
#> NG [from Wells]            0
#> Petrol                     0
#> Petrol [from Dist.]        0
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
inter[[Recca::balance_cols$inter_industry_balance_colname]][[2]]
#>                             Industry
#> Crude                   0.000000e+00
#> Crude [from Dist.]      0.000000e+00
#> Crude [from Fields]     0.000000e+00
#> Diesel                  0.000000e+00
#> Diesel [from Dist.]     0.000000e+00
#> Elect                   0.000000e+00
#> Elect [from Grid]       0.000000e+00
#> LTH                     0.000000e+00
#> Light                   0.000000e+00
#> MD [from Car engines]   0.000000e+00
#> MD [from Truck engines] 2.273737e-13
#> NG                      0.000000e+00
#> NG [from Dist.]         0.000000e+00
#> NG [from Wells]         0.000000e+00
#> Petrol                  0.000000e+00
#> Petrol [from Dist.]     0.000000e+00
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
# Intra-industry balances
intra <- UKEnergy2000mats |>
  dplyr::filter(LastStage %in% c("Final", "Useful")) |>
  tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
  calc_intra_industry_balance()
intra[[Recca::balance_cols$intra_industry_balance_colname]][[1]]
#>                   Product
#> Crude dist.           550
#> Diesel dist.          350
#> Elect. grid           125
#> Gas wells & proc.    2075
#> NG dist.               50
#> Oil fields           2575
#> Oil refineries       5075
#> Petrol dist.          750
#> Power plants         9700
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
intra[[Recca::balance_cols$intra_industry_balance_colname]][[2]]
#>                      Product
#> Car engines       22999.6000
#> Crude dist.         545.0000
#> Diesel dist.        367.9998
#> Elect. grid         125.0000
#> Furnaces           5000.0000
#> Gas wells & proc.  2075.0000
#> Light fixtures     4800.0000
#> NG dist.             45.0000
#> Oil fields         2575.0000
#> Oil refineries     5075.0000
#> Petrol dist.        526.9997
#> Power plants       9700.0000
#> Truck engines     13250.0200
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
```
