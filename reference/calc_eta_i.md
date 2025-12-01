# Calculate industry efficiencies

Calculates industry efficiencies for all energy conversion industries in
the ECC. Calculations are performed as shown in Equation 11 in Heun,
Owen, and Brockway. 2018. A physical supply-use table framework for
energy analysis on the energy conversion chain. Applied Energy, vol 226,
pp. 1134-1162.

## Usage

``` r
calc_eta_i(
  .sutmats,
  U = Recca::psut_cols$U,
  V = Recca::psut_cols$V,
  S_units = Recca::psut_cols$S_units,
  eta_i = Recca::efficiency_cols$eta_i
)
```

## Arguments

- .sutmats:

  A data frame containing columns for **U**, **V**, and **S_units**
  matrices.

- U:

  A string for the name of a column of **U** matrices in `.sutmats`.
  Default is `Recca::psut_cols$U`.

- V:

  A string for the name of a column of **V** matrices in `.sutmats`.
  Default is `Recca::psut_cols$V`.

- S_units:

  A string for the name of a column of **S_units** matrices in
  `.sutmats`. Default is `Recca::psut_cols$S_units`.)

- eta_i:

  The name of the industry efficiency column in output. Default is
  `Recca::efficiency_cols$eta_i`.

## Value

`.sutmats` with an additional column `eta_i`

## Details

The efficiency for a given industry is calculated iff the units for
inputs and outputs for that industry are unit-homogeneous. If units for
inputs and outputs are heterogeneous for an industry, `NA` is the
result.

Note that these efficiencies (`eta`) are different from final demand
sector and product efficiencies (`eta_s` and `eta_p`, respectively).
Both final demand sector and product efficiencies (`eta_s` and `eta_p`)
are based on embodied energy, whereas industry efficiencies (`eta`) is
based on direct inputs consumed and outputs produced by the energy
conversion industry.

To calculate energy conversion final demand sector and product
efficiencies, use the
[`calc_embodied_etas()`](https://matthewheun.github.io/Recca/reference/calc_embodied_etas.md)
function.

## Examples

``` r
library(tidyr)
UKEnergy2000mats %>%
  tidyr::spread(key = "matrix.name", value = "matrix") %>%
  calc_eta_i()
#> # A tibble: 4 × 13
#>   Country  Year EnergyType LastStage R             S_units  U        U_EIOU  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 5 more variables: U_feed <list>, V <list>, Y <list>, r_EIOU <list>,
#> #   eta_i <list>
```
