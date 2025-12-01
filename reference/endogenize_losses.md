# Endogenize losses into PSUT matrices

When a conversion chain does *not* include losses in the **RUVY**
matrices of the PSUT framework, it may be helpful to endogenize the
losses. This function performs the endogenization.

## Usage

``` r
endogenize_losses(
  .sutmats = NULL,
  loss_product = Recca::balance_cols$waste_heat,
  loss_sector = Recca::balance_cols$losses_sector,
  V = "V",
  Y = "Y",
  balance_colname = Recca::balance_cols$intra_industry_balance_colname,
  V_prime = "V_prime",
  Y_prime = "Y_prime"
)
```

## Arguments

- .sutmats:

  A `matsindf` data frame, wide by matrices.

- loss_product:

  The string name of the loss product. Default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$waste_heat`
  or "Waste heat".

- loss_sector:

  The string name of the sector that will absorb losses. Default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$losses_sector`
  or "Losses".

- V:

  Make (**V**) matrix or name of the column in `.sutmats` that contains
  same. Default is "V".

- Y:

  Final demand (**Y**) matrix or name of the column in `.sutmats` that
  contains same. Default is "Y".

- balance_colname:

  The name of the column containing energy balance vectors. Default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$intra_industry_balance_colname`
  or "SUTIntraIndustryBalance".

- V_prime:

  The name of the **V** matrix with endogenized losses.

- Y_prime:

  The name of the **Y** matrix with endogenized losses.

## Value

A version of `.sutmats` with losses endogenized.

## Details

Intra-industry balances need to be calculated (most easily via
[`calc_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md))
prior to calling this function. See the example.

## Examples

``` r
UKEnergy2000mats |>
  tidyr::pivot_wider(names_from = matrix.name,
                     values_from = matrix) |>
  dplyr::filter(.data[[IEATools::iea_cols$last_stage]] %in%
                  c(IEATools::last_stages$final, IEATools::last_stages$useful)) |>
  calc_intra_industry_balance() |>
  endogenize_losses() |>
  # Use the endogenized matrices
  dplyr::mutate(
    V = V_prime,
    Y = Y_prime,
    V_prime = NULL,
    Y_prime = NULL,
    "{Recca::balance_cols$intra_industry_balance_colname}" := NULL
  )
#> # A tibble: 2 × 12
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 4 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>
```
