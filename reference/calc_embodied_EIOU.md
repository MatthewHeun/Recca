# Calculate various embodied EIOU matrices

This function calculates different embodied Energy Industry Own Use
(EIOU) matrices (see details) for a given energy conversion chain and
final demand, using a data frame of input-output matrices in the
physical supply-use table (PSUT) format.

## Usage

``` r
calc_embodied_EIOU(
  .iomats = NULL,
  e_EIOU = "e_EIOU",
  Y = "Y",
  y = "y",
  L_ixp = "L_ixp",
  L_ixp_feed = "L_ixp_feed",
  Q_EIOU_s = "Q_EIOU_s",
  Q_EIOU_p = "Q_EIOU_p",
  Q_EIOU_feed_s = "Q_EIOU_feed_s",
  Q_EIOU_feed_p = "Q_EIOU_feed_p"
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
  functions. See the examples.

- e_EIOU:

  A direct energy use extension vector or name of column in `.iomats`
  containing same. Default is "e_EIOU".

- Y:

  A final demand matrix (**Y**) or name of column in `.iomats`
  containing same. Default is "Y".

- y:

  A **y** vector or name of column in `.iomats` containing same. Default
  is "y".

- L_ixp:

  An **L_ixp** matrix or name of column in `.iomats` containing same.
  Default is "L_ixp".

- L_ixp_feed:

  An **L_ixp_feed** matrix or name of column in `.iomats` containing
  same. Default is "L_ixp_feed".

- Q_EIOU_s:

  The name of the output column containing the EIOU embodied by final
  demand sectors, including both energy use for feedstock and EIOU
  production. Default is "Q_EIOU_s".

- Q_EIOU_p:

  The name of the output column containing the EIOU embodied by final
  demand products, including both energy use for feedstock and EIOU
  production. Default is "Q_EIOU_p".

- Q_EIOU_feed_s:

  The name of the output column containing the EIOU embodied by final
  demand sectors, including only energy use for feedstock production.
  Default is "Q_EIOU_feed_s".

- Q_EIOU_feed_p:

  The name of the output column containing the EIOU embodied by final
  demand products, including only energy use for feedstock production.
  Default is "Q_EIOU_feed_p".

## Value

A data frame that contains several embodied EIOU matrices in added
columns. See description for details.

## Details

The argument `.iomats` should be a wide-by-matrices data frame, obtained
combining the `calc_iomats()` and
[`calc_E_EIOU()`](https://matthewheun.github.io/Recca/reference/calc_E_EIOU.md)
functions as described in the example.

This function adds many additional columns to `.iomats`, each one
containing particular embodied EIOU matrices.

The embodied EIOU matrices are calculated either:

- by final demand sector (subscript "\_s" appears in the name);

- by final demand products (subscript "\_p" appears in the name);

- including only EIOU required for feedstock inputs production
  (subscript "\_feed" appears in the name);

- including both EIOU required for feedstock and EIOU inputs production
  (no additional subscript).

Note: All matrix multiplication (`%*%`) is performed "by name" using
[`matsbyname::matrixproduct_byname()`](https://matthewheun.github.io/matsbyname/reference/matrixproduct_byname.html).

Output columns include:

- `Q_EIOU_s`: matrix of embodied EIOU by final demand sectors, including
  both energy use for feedstock and EIOU production. `Q_EIOU_s` is
  calculated by `e_EIOU_hat %*% L_ixp %*% Y`.

- `Q_EIOU_p`: matrix of embodied EIOU by final demand products,
  including both energy use for feedstock and EIOU production.
  `Q_EIOU_p` is calculated by `e_EIOU_hat %*% L_ixp %*% y_hat`.

- `Q_EIOU_feed_s`: matrix of embodied EIOU by final demand sectors,
  including only energy use for feedstock production. `Q_EIOU_feed_s` is
  calculated by `e_EIOU_hat %*% L_ixp_feed %*% Y`.

- `Q_EIOU_feed_p`: matrix of embodied EIOU by final demand products,
  including only energy use for feedstock production. `Q_EIOU_feed_p` is
  calculated by `e_EIOU_hat %*% L_ixp_feed %*% y_hat`.

## Examples

``` r
library(IEATools)
UKEnergy2000mats %>%
  tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
  calc_io_mats() %>%
  calc_E_EIOU() %>%
  calc_embodied_EIOU()
#> # A tibble: 4 × 38
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 30 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>,
#> #   y <list>, q <list>, f <list>, g <list>, h <list>, r <list>, W <list>,
#> #   Z <list>, K <list>, C <list>, D <list>, A <list>, O <list>, L_pxp <list>,
#> #   L_ixp <list>, Z_feed <list>, K_feed <list>, A_feed <list>,
#> #   L_pxp_feed <list>, L_ixp_feed <list>, E_EIOU <list>, e_EIOU <list>,
#> #   Q_EIOU_p <list>, Q_EIOU_s <list>, Q_EIOU_feed_p <list>,
#> #   Q_EIOU_feed_s <list>
```
