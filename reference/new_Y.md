# Reconstruct an economy given a new final demand matrix

When the final demand matrix changes from **Y** to **Y_prime**, this
function calculates new resource (**R_prime**), use (**U_prime**), feed
(**U_feed**), energy industry own use (**U_eiou**), ratio (**r_eiou**),
and make (**V_prime**) matrices that would be required to meet the new
final demand (**Y_prime**).

## Usage

``` r
new_Y(
  .sutmats = NULL,
  Y_prime = "Y_prime",
  L_ixp = "L_ixp",
  L_pxp = "L_pxp",
  Z = "Z",
  Z_feed = "Z_feed",
  D = "D",
  O = "O",
  R_prime = "R_prime",
  U_prime = "U_prime",
  U_feed_prime = "U_feed_prime",
  U_eiou_prime = "U_EIOU_prime",
  r_eiou_prime = "r_EIOU_prime",
  V_prime = "V_prime"
)
```

## Arguments

- .sutmats:

  A data frame of supply-use table matrices with matrices arranged in
  columns.

- Y_prime:

  A new final demand matrix or name of a column in `.sutmats` containing
  same. Default is "Y_prime".

- L_ixp, L_pxp, Z, Z_feed, D, O:

  Input matrices that describe the structure of the energy conversion
  chain. Values can be string names (the default) for columns in a data
  frame `.sutmats` or names of items in a list `.sutmats`

- R_prime, U_prime, U_feed_prime, U_eiou_prime, r_eiou_prime, V_prime:

  The new names for new matrices. Defaults are each argument name as a
  string.

## Value

A list or data frame containing **R_prime**, **U_prime**, **U_feed**,
**U_eiou**, **r_eiou**, and **V_prime** matrices.

## Details

Note that inputs **L_ixp**, **L_pxp**, **Z**, and **D** can be
conveniently calculated by the function
[`calc_io_mats()`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md).

Internally, this function uses
[`matsindf::matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.html),
and documentation assumes that `.sutmats` is not `NULL` and is a data
frame. But `.sutmats` can also be a named list of matrices. Or matrices
can be supplied individually to the `Y_prime`, `L_ixp`, `L_pxp`, `Z`,
`Z_feed`, `D`, and `O` arguments. If `.sutmats` is present, output is a
data frame with columns named by string values of output arguments, and
input arguments should be character strings that name columns in
`.sutmats`. If `.sutmats` is `NULL` (the default), output is a list with
items named by output strings, and input arguments should be single
matrices or vectors.

## Examples

``` r
library(dplyr)
library(matsbyname)
library(tidyr)
UKEnergy2000mats %>%
  spread(key = matrix.name, value = matrix) %>%
  select(Country, Year, EnergyType, LastStage, R, U, U_feed, V, Y, r_EIOU, S_units) %>%
  calc_io_mats() %>%
  mutate(
    # Give new Y matrices that are double the existing Y matrices
    Y_prime = matsbyname::hadamardproduct_byname(2, Y)
  ) %>%
  # Should give U_prime and V_prime matrices that are double the existing U and V matrices
  new_Y()
#> # A tibble: 4 × 38
#>   Country  Year EnergyType LastStage R             U        U_feed   V       
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 30 more variables: Y <list>, r_EIOU <list>, S_units <list>, y <list>,
#> #   q <list>, f <list>, g <list>, h <list>, r <list>, W <list>, Z <list>,
#> #   K <list>, C <list>, D <list>, A <list>, O <list>, L_pxp <list>,
#> #   L_ixp <list>, Z_feed <list>, K_feed <list>, A_feed <list>,
#> #   L_pxp_feed <list>, L_ixp_feed <list>, Y_prime <list>, R_prime <list>,
#> #   U_prime <list>, U_feed_prime <list>, U_EIOU_prime <list>,
#> #   r_EIOU_prime <list>, V_prime <list>
```
