# Calculate embodied EIOU per industry output

This function calculates the EIOU per industry output given a `.iomats`
data frame input-output matrices for an energy conversion chain. The
returned `E_EIOU` matrix contains the by-product EIOU used by unit of
each industry output, while the `e_EIOU` vector contains the total EIOU
by unit of industry output.

## Usage

``` r
calc_E_EIOU(
  .iomats = NULL,
  g = "g",
  U_EIOU = "U_EIOU",
  E_EIOU = "E_EIOU",
  e_EIOU = "e_EIOU"
)
```

## Arguments

- .iomats:

  a data frame containing matrices that describe the Input-Output
  structure of an Energy Conversion Chain. `.iomats` will likely have
  been obtained from the
  [`calc_io_mats()`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md)
  function.

- g:

  name of the `g` vector on output. Default is "`g`".

- U_EIOU:

  name of the `U_EIOU` matrices on output. Default is "`U_EIOU`".

- E_EIOU:

  name for the `E_EIOU` matrix on output. Default is "`E_EIOU`".

- e_EIOU:

  name for the `e_EIOU` vector on output. Default is "`e_EIOU`".

## Value

List or data frame containing the `E_EIOU` matrix and `e_EIOU` vector.

## Details

The output `E_EIOU` matrix is calculated as `U_EIOU %*% g_hat_inv`.

The output `e_EIOU` vector is sum of `E_EIOU` columns:
`transpose(i) %*% E_EIOU`.

## Examples

``` r
library(IEATools)
#> 
#> Attaching package: ‘IEATools’
#> The following objects are masked from ‘package:Recca’:
#> 
#>     aggregate_cols, all_stages, energy_types, finaldemand_aggregates,
#>     primary_aggregates, psut_cols, row_col_types
UKEnergy2000mats %>%
   dplyr::filter(LastStage == "Final", EnergyType == "E") %>%
   tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
   calc_io_mats() %>%
   calc_E_EIOU()
#> # A tibble: 1 × 34
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 26 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>,
#> #   y <list>, q <list>, f <list>, g <list>, h <list>, r <list>, W <list>,
#> #   Z <list>, K <list>, C <list>, D <list>, A <list>, O <list>, L_pxp <list>,
#> #   L_ixp <list>, Z_feed <list>, K_feed <list>, A_feed <list>,
#> #   L_pxp_feed <list>, L_ixp_feed <list>, E_EIOU <list>, e_EIOU <list>
```
