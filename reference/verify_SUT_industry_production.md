# Confirm that all Industries in an SUT-style data frame produce energy.

If a transformation process industry consumes energy (in the `U` matrix)
but does not make energy (in the `V` matrix), it is most certainly an
error. (In contrast, there can be Industries that make energy but do not
consume it, such as Industries involved in Production. And final demand
sectors consume energy but do not produce any.) This function emits a
warning if an Industry in the `U` matrix is found to consume energy but
not make energy. Look at the `industry_production_OK` column of the
output to see which rows of `.sutmats` exhibit the problem. Look at the
`problem_industries` column of the output to see which industries
exhibit this problem.

## Usage

``` r
verify_SUT_industry_production(
  .sutmats = NULL,
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  V = Recca::psut_cols$V,
  industry_production_OK = ".industry_production_OK",
  problem_industries = ".problem_industries"
)
```

## Arguments

- .sutmats:

  an SUT-style data frame containing metadata columns (typically
  `Country`, `Year`, `LedgerSide`, `Product`, etc.) and columns of SUT
  matrices, including `U` and `V`.

- R:

  resources (**R**) matrix or name of the column in `.sutmats` that
  contains same. Default is "R".

- U:

  use (**U**) matrix or name of the column in `.sutmats` that contains
  same. Default is "U".

- V:

  make (**V**) matrix or name of the column in `.sutmats`that contains
  same. Default is "V".

- industry_production_OK:

  the name of the column in the output that tells whether all industries
  produce something. Default is ".industry_production_OK".

- problem_industries:

  the name of the column in the output that tells which transformation
  processes consume energy but do not produce anything.

## Value

`.sutmats` with added column named with the value of
`industry_production_OK`.

## Examples

``` r
library(tidyr)
verify_SUT_industry_production(UKEnergy2000mats %>%
                                 spread(key = matrix.name, value = matrix))
#> # A tibble: 4 × 14
#>   Country  Year EnergyType LastStage R             S_units  U        U_EIOU  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 6 more variables: U_feed <list>, V <list>, Y <list>, r_EIOU <list>,
#> #   .industry_production_OK <lgl>, .problem_industries <list>
```
