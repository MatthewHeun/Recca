# Calculate energy return ratios for the gamma system boundary.

Calculates energy return ratios for the gamma system boundary.
Calculations are performed as shown in Equations 8, 9, and 10 in Heun,
Owen, and Brockway. 2018. A physical supply-use table framework for
energy analysis on the energy conversion chain. Applied Energy, vol 226,
pp. 1134-1162.

## Usage

``` r
calc_ERRs_gamma(
  .sutmats,
  U = "U",
  r_EIOU = "r_EIOU",
  V = "V",
  g = "g",
  S_units = "S_units",
  ger_gamma = "ger_gamma",
  ner_gamma = "ner_gamma",
  r_gamma = "r_gamma"
)
```

## Arguments

- .sutmats:

  a data frame containing columns for `U` and `r_EIOU` matrices and a
  `g` vector.

- U:

  a string for the name of a column of `U` matrices in `.sutmats`.
  (Default is "`U`".)

- r_EIOU:

  a string for the name of a column of `r_EIOU` matrices in `.sutmats`.
  (Default is "`r_EIOU`".)

- V:

  a string for the name of a column of `V` matrices in `.sutmats`.
  (Default is "`V`".)

- g:

  a string for the name of a column of `g` vector in `.sutmats`.
  (Default is "`g`".)

- S_units:

  a string for the name of a column of `S_units` matrices in `.sutmats`.
  (Default is "`S_units`".)

- ger_gamma:

  the name of the gross energy ratio column in output. (Default is
  "`ger_gamma`",

- ner_gamma:

  the name of the net energy ratio column in output. (Default is
  "`ner_gamma`",)

- r_gamma:

  the name of the ratio of energy ratios in output. (Default is
  "`r_gamma`",

## Value

`.sutmats` with additional columns "`ner_gamma`", "`ger_gamma`", and
"`r_gamma`".

## Details

The energy return ratios for a given industry are calculated iff the
units for inputs and outputs for that industry are unit-homogeneous. If
units for inputs or outputs are heterogeneous for an industry, `NA` is
the result.

## Examples

``` r
library(tidyr)
library(Recca)
UKEnergy2000mats %>%
  spread(key = "matrix.name", value = "matrix") %>%
  calc_io_mats() %>%
  calc_ERRs_gamma()
#> # A tibble: 4 × 35
#>   Country  Year EnergyType LastStage R             S_units  U        U_EIOU  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 27 more variables: U_feed <list>, V <list>, Y <list>, r_EIOU <list>,
#> #   y <list>, q <list>, f <list>, g <list>, h <list>, r <list>, W <list>,
#> #   Z <list>, K <list>, C <list>, D <list>, A <list>, O <list>, L_pxp <list>,
#> #   L_ixp <list>, Z_feed <list>, K_feed <list>, A_feed <list>,
#> #   L_pxp_feed <list>, L_ixp_feed <list>, ger_gamma <list>, ner_gamma <list>,
#> #   r_gamma <list>
```
