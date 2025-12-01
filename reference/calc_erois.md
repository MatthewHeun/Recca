# Calculate EROIs

This function calculates energy return on investment (EROI) values given
a data frame of input-output matrices in the physical supply-use table
(PSUT) format for an energy conversion chain.

## Usage

``` r
calc_erois(
  .iomats = NULL,
  e_EIOU = "e_EIOU",
  L_ixp = "L_ixp",
  L_ixp_feed = "L_ixp_feed",
  D = "D",
  C = "C",
  eroi_g_p = "eroi_g_p",
  eroi_g_i = "eroi_g_i",
  eroi_g_p_feed = "eroi_g_p_feed",
  eroi_g_i_feed = "eroi_g_i_feed"
)
```

## Arguments

- .iomats:

  A wide-by-matrices data frame containing matrices that describe the
  Input-Output structure (using the supply-use table format) of an
  Energy Conversion Chain. `.iomats` will likely have been obtained
  combining the
  [`calc_io_mats()`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md)
  and
  [`calc_E_EIOU()`](https://matthewheun.github.io/Recca/reference/calc_E_EIOU.md)
  functions. See the example.

- e_EIOU:

  The name of the column containing `e_EIOU` vectors in `.iomats`.
  Default is "e_EIOU".

- L_ixp:

  The name of the column containing `L_ixp` matrices in `.iomats`.
  Default is "L_ixp".

- L_ixp_feed:

  The name of the column containing `L_ixp_feed` matrix in `.iomats`.
  Default is "L_ixp_feed".

- D:

  The name of the column containing `D` matrices in `.iomats`. Default
  is "D".

- C:

  The name of the column containing `C` matrix in `.iomats`. Default is
  "C".

- eroi_g_p:

  The name of the output column containing vectors of product-level
  gross EROIs, including both energy use for feedstock and EIOU
  production. Default is "eroi_g_p".

- eroi_g_i:

  The name of the output column containing vectors of industry-level
  gross EROIs, including both energy use for feedstock and EIOU
  production. Default is "eroi_g_i".

- eroi_g_p_feed:

  The name of the output column containing vectors of product-level
  gross EROIs, including only energy use for feedstock production.
  Default is "eroi_g_p_feed".

- eroi_g_i_feed:

  The name of the output column containing vectors of industry-level
  gross EROIs, including only energy use for feedstock production.
  Default is "eroi_g_i_feed".

## Value

A data frame that includes several additional EROIs vectors in added
columns. See description for details.

## Details

The argument `.iomats` should be a wide-by-matrices data frame.

Other input columns are named by the matrices they contain.

This function adds many additional columns to `.iomats`, each one
containing a particular type of EROI. The default column names use the
following naming convention:

- names of EROIs calculated for products include the string "\_p";

- names of EROIs calculated for industries include the string "\_i";

- names of gross EROIs include the string "\_g";

- names of net EROIs include the string "\_n".

In addition, calculations are made based on inclusion of either

- only EIOU required for feedstock inputs production (in which case
  "\_feed" appears in the name);

- both EIOU required for feedstock and EIOU inputs production (no
  additional string in the name).

Output columns include:

- `eroi_g_p`: vector of product-level gross EROIs, including both EIOU
  required for feedstock and EIOU inputs production. The inverse of
  `eroi_g_p` is calculated by `transpose(i) %*% e_EIOU_hat %*% L_ixp`.

- `eroi_g_i`: vector of industry-level gross EROIs, including both EIOU
  required for feedstock and EIOU inputs production. The inverse of
  `eroi_i_p` is calculated by `transpose(C) * eroi_g_p_inv`.

- `eroi_g_p_feed`: vector of product-level gross EROIs, including only
  EIOU required for feedstock inputs production. The inverse of
  `eroi_g_p_feed` is calculated by
  `transpose(i) %*% e_EIOU_hat %*% L_ixp_feed`.

- `eroi_g_i_feed`: vector of industry-level gross EROIs, including only
  EIOU required for feedstock inputs production. The inverse of
  `eroi_i_p` is calculated by `transpose(C) %*% eroi_g_p_feed_inv`.

Note: All matrix multiplication (`%*%`) is performed "by name" using
[`matsbyname::matrixproduct_byname()`](https://matthewheun.github.io/matsbyname/reference/matrixproduct_byname.html).

## Examples

``` r
library(IEATools)
UKEnergy2000mats %>%
  dplyr::filter(LastStage == "Final", EnergyType == "E") %>%
  tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
  calc_io_mats() %>%
  calc_E_EIOU() %>%
  calc_erois()
#> # A tibble: 1 × 38
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 30 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>,
#> #   y <list>, q <list>, f <list>, g <list>, h <list>, r <list>, W <list>,
#> #   Z <list>, K <list>, C <list>, D <list>, A <list>, O <list>, L_pxp <list>,
#> #   L_ixp <list>, Z_feed <list>, K_feed <list>, A_feed <list>,
#> #   L_pxp_feed <list>, L_ixp_feed <list>, E_EIOU <list>, e_EIOU <list>,
#> #   eroi_g_p <list>, eroi_g_i <list>, eroi_g_p_feed <list>,
#> #   eroi_g_i_feed <list>
```
