# Separate resource (`R`) and make (`V`) matrices from make plus resource (`R_plus_V`) matrices

Resource industries are industries that make a product without using any
products. Resource industries are identified by interrogating the use
(`U`) and make (`R_plus_V`) matrices. Resource industries have all
zeroes in their column of the use matrix (`U`) and at least one non-zero
value in their row of the make (`R_plus_V`) matrix.

## Usage

``` r
separate_RV(.sutmats = NULL, U = "U", R_plus_V = "R_plus_V", R = "R", V = "V")
```

## Arguments

- .sutmats:

  a list or data frame containing use matrix(ces) and make matrix(ces)

- U:

  a use (`U`) matrix or name of the column in `.sutmats` that contains
  same. Default is "`U`".

- R_plus_V:

  an `R_plus_V` matrix or name of the column in `.sutmats` that contains
  same. Default is "`R_plus_V`".

- R:

  name for resource (`R`) matrix on output. Default is "`R`".

- V:

  name for make (`V`) matrix on output. Default is "`V`".

## Value

a list or data frame containing `R` and `V` matrices

## Details

A resource matrix (`R`) has industries in rows and products in columns.
The elements of of `R` indicate extraction of resources from the
biosphere. The industries of `R` are the reserves of the extracted
products.

This function uses the
[`resource_industries`](https://matthewheun.github.io/Recca/reference/resource_industries.md)
function to identify the resource industries in the `R_plus_V` matrix.
Thereafter, the function extracts the resource industries from the
`R_plus_V` matrix to form the `R` matrix. Finally, the `R` matrix is
subtracted from the `R_plus_V` matrix and saved as the `V` matrix. If
there are no resource industries in the `R_plus_V` matrix, a warning is
emitted, no `R` matrix is created, and no changes are made to the
`R_plus_V` matrix.

`separate_RV` is the inverse of
[`combine_RV`](https://matthewheun.github.io/Recca/reference/combine_RV.md).

## Examples

``` r
library(dplyr)
library(tidyr)
UKEnergy2000mats %>%
  spread(key = "matrix.name", value = "matrix") %>%
  # Rename the V matrix, because it includes the R matrix.
  rename(
    R_plus_V = V
  ) %>%
  separate_RV()
#> Warning: No R created in separate_RV
#> Warning: No R created in separate_RV
#> Warning: No R created in separate_RV
#> Warning: No R created in separate_RV
#> Warning: Name collision in matsindf::matsindf_apply(). The following arguments appear both in .dat and in the output of `FUN`: R
#> # A tibble: 4 × 14
#>   Country  Year EnergyType LastStage R             S_units  U        U_EIOU  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 6 more variables: U_feed <list>, R_plus_V <list>, Y <list>, r_EIOU <list>,
#> #   R <list>, V <list>
```
