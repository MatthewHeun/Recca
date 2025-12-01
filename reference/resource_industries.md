# Resource industries

Identifies resource industries.

## Usage

``` r
resource_industries(
  .sutdata = NULL,
  R = "R",
  U = "U",
  V = "V",
  r_industries = "r_industries"
)
```

## Arguments

- .sutdata:

  a list or data frame containing use matrix(ces) and make matrix(ces)

- R:

  resource (`R`) matrix or name of the column in `.sutmats` that
  contains same. Default is "R".

- U:

  use (`U`) matrix or name of the column in `.sutmats` that contains
  same. Default is "U".

- V:

  make (`V`) matrix or name of the column in `.sutmats` that contains
  same. Default is "V".

- r_industries:

  name for the resource industry vector on output. Default is
  "r_industries".

## Value

a list or data frame with `.sutdata` with an additional column (named
with the value of the `p_industries` argument) containing the resource
industries for each row

## Details

Resource industries are industries that make a product without using any
products. If `R` is given, its industries are automatically included in
the output. Additional resource industries are identified by
interrogating the resources (`R`), use (`U`) and make (`V`) matrices.
Resource industries are, by definition, present in the `R` matrix, or
they have all zeroes in their column of the use matrix (`U`) and at
least one non-zero value in their row of the make (`V`) matrix.

Argument and value descriptions are written assuming that `.sutdata` is
a data frame. Alternatively, `.sutdata` can be unspecified, and `U` and
`V` can be matrices. In that case, the return value is a list with a
single item (`r_industries`) which contains a vector of names of
resource industries for the `U` and `V` matrices.

## Examples

``` r
library(tidyr)
UKEnergy2000mats %>%
  spread(key = matrix.name, value = matrix) %>%
  resource_industries()
#> # A tibble: 4 × 13
#>   Country  Year EnergyType LastStage R             S_units  U        U_EIOU  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 5 more variables: U_feed <list>, V <list>, Y <list>, r_EIOU <list>,
#> #   r_industries <list>
```
