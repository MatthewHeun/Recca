# Combine resource (`R`) and make (`V`) matrices into a make plus resource (`R_plus_V`) matrix

`combine_RV` is the inverse of
[`separate_RV`](https://matthewheun.github.io/Recca/reference/separate_RV.md).

## Usage

``` r
combine_RV(.sutmats = NULL, R = "R", V = "V", R_plus_V = "R_plus_V")
```

## Arguments

- .sutmats:

  a list or data frame containing use matrix(ces) and make matrix(ces)

- R:

  an `R` matrix or name of a column in `.sutmats` that contains same.
  Default is "`R`".

- V:

  a make (`V`) matrix or name of a column in `.sutmats` that contains
  same. Default is "`V`".

- R_plus_V:

  name for `R_plus_V` matrix on output. Default is "`R_plus_V`".

## Value

a list or data frame containing `R_plus_V`

## Examples

``` r
library(dplyr)
library(tidyr)
UKEnergy2000mats %>%
  spread(key = "matrix.name", value = "matrix") %>%
  combine_RV()
#> # A tibble: 4 × 13
#>   Country  Year EnergyType LastStage R             S_units  U        U_EIOU  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 5 more variables: U_feed <list>, V <list>, Y <list>, r_EIOU <list>,
#> #   R_plus_V <list>
```
