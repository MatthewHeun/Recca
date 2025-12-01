# Calculate several input-output matrices

This function bundles several others and calculates matrices that
describe the structure of an energy conversion chain.

## Usage

``` r
calc_io_mats(
  .sutdata = NULL,
  direction = c("upstream", "demand", "Leontief", "downstream", "supply", "Ghosh"),
  method = c("solve", "QR", "SVD"),
  tol = .Machine$double.eps,
  method_q_calculation = c("sum_U_Y_rows", "sum_R_V_cols"),
  R = "R",
  U = "U",
  U_feed = "U_feed",
  V = "V",
  Y = "Y",
  S_units = "S_units",
  y = "y",
  q = "q",
  f = "f",
  g = "g",
  h = "h",
  r = "r",
  W = "W",
  K = "K",
  Z = "Z",
  C = "C",
  D = "D",
  A = "A",
  L_ixp = "L_ixp",
  L_pxp = "L_pxp",
  O = "O",
  Z_feed = "Z_feed",
  K_feed = "K_feed",
  A_feed = "A_feed",
  L_ixp_feed = "L_ixp_feed",
  L_pxp_feed = "L_pxp_feed",
  Z_s = "Z_s",
  C_s = "C_s",
  D_s = "D_s",
  D_feed_s = "D_feed_s",
  B = "B",
  G_ixp = "G_ixp",
  G_pxp = "G_pxp",
  O_s = "O_s"
)
```

## Arguments

- .sutdata:

  A data frame of supply-use table matrices with matrices arranged in
  columns.

- direction:

  A string that identifies the directionality of the IO matrices. See
  details. Default is "upstream".

- method:

  One of "solve", "QR", or "SVD". Default is "solve". See details.

- tol:

  The tolerance for detecting linear dependencies during matrix
  inversion. Default is `.Machine$double.eps`.

- method_q_calculation:

  Specifies the method with which the q vector should be calculated. See
  details.

- R:

  The resources (**R**) matrix or name of the column in `.sutmats` that
  contains same. Default is "R".

- U:

  The use (**U**) matrix or name of the column in `.sutmats` that
  contains same. Default is "U".

- U_feed:

  The feed portion of the use matrix (**U_feed**) or name of the column
  in `.sutmats` that contains same. Default is "U_feed".

- V:

  The make (**V**) matrix or name of the column in `.sutmats`that
  contains same. Default is "V".

- Y:

  The final demand (**Y**) matrix or name of the column in `.sutmats`
  that contains same. Default is "Y".

- S_units:

  The unit summation matrix (**S_units**) or name of the column in
  `.sutmats` that contains same. Default is "S_units".

- y:

  The name for the **y** vector on output. Default is "y". **y** is
  calculated by `rowsums(Y)`.

- q:

  The name for the **q** vector on output. Default is "q". **q** is
  calculated by `rowsums(U) + y`.

- f:

  The name for the **f** vector on output. Default is "f". **f** is
  calculated by `colsums(U)`.

- g:

  The name for the **g** vector on output. Default is "g". **g** is
  calculated by `rowsums(V)`.

- h:

  The name for the **h** vector on output. Default is "h". **h** is
  calculated by `colsums(transpose(R))`.

- r:

  The name for the **r** vector on output. Default is "r". **r** is
  calculated by `rowsums(R)`.

- W:

  The name for the **W** matrix on output. Default is "W". **W** is
  calculated by `transpose(V) - U`.

- K:

  The name for the **K** matrix on output. Default is "K". **K** is
  calculated by `U * f_hat_inv`.

- Z:

  The name fort the **Z** matrix on output. Default is "Z". **Z** is
  calculated by `U * g_hat_inv`.

- C:

  The name for the **C** matrix on output. Default is "C". **C** is
  calculated by `transpose(V) * g_hat_inv`.

- D:

  The name for the **D** matrix on output. Default is "D". **D** is
  calculated by `V * q_hat_inv`.

- A:

  The name for the **A** matrix on output. Default is "A". **A** is
  calculated by `Z * D`.

- L_ixp:

  The name for the **L_ixp** matrix on output. Default is "L_ixp".
  **L_ixp** is calculated by `D * L_pxp`.

- L_pxp:

  The name for the **L_pxp_feed** matrix on output. Default is
  "L_pxp_feed". **L_pxp** is calculated by `(I - Z*D)^-1`.

- O:

  name for the **O** matrix on output. Default is "O". **O** is
  calculated by `R * h_hat_inv`.

- Z_feed:

  The name for the **Z_feed** matrix on output. Default is "Z_feed".
  **Z_feed** is calculated by `U_feed * g_hat_inv`.

- K_feed:

  The name for the **K_feed** matrix on output. Default is "K_feed".
  **K_feed** is calculated by `U_feed * f_hat_inv`.

- A_feed:

  The name for the **A_feed** matrix on output. Default is "A_feed".
  **A_feed** is calculated by `Z_feed * D_feed`.

- L_ixp_feed:

  The name for the **L_ixp_feed** matrix on output. Default is
  "L_ixp_feed". **L_ixp_feed** is calculated by `D_feed * L_pxp_feed`.

- L_pxp_feed:

  The name for the **L_pxp_feed** matrix on output. Default is
  "L_pxp_feed". **L_pxp_feed** is calculated by `(I - Z_feed*D)^-1`.

- Z_s:

  The name for the **Z_s** matrix on output. Default is "Z_s". **Z_s**
  is calculated by `transpose(V) * f_hat_inv`.

- C_s:

  The name for the **C_s** matrix on output. Default is "C_s". **C_s**
  is calculated by `U * f_hat_inv`.

- D_s:

  The name for the **D_s** matrix on output. Default is "D_s". **D_s**
  is calculated by `transpose(U) * q_hat_inv`.

- D_feed_s:

  The name for the **D_feed_s** matrix on output. Default is "D_feed_s".
  **D_s** is calculated by `transpose(U_feed) * q_hat_inv`.

- B:

  The name for the **B** matrix on output. Default is "B". **B** is
  calculated by `Z_s * D_s`.

- G_ixp:

  The name for the **G_ixp** matrix on output. Default is "G_ixp".
  **G_ixp** is calculated by `D_s * G_pxp`.

- G_pxp:

  The name for the **G_pxp** matrix on output. Default is "G_pxp".
  `G_pxp` is calculated by `inverse(I - A_s)`.

- O_s:

  The name for the **O_s** matrix on output. Default is "O_s". **O** is
  calculated by `q_hat_inv * Y`.

## Value

A list or data frame containing input-output matrices.

## Details

Some calculations involve a matrix inversion step. The `method` argument
specifies which method should be used for calculating the inverse. See
[`matsbyname::invert_byname()`](https://matthewheun.github.io/matsbyname/reference/invert_byname.html).

`method_q_calculation` specifies the method with which the q vector
should be calculated. Default is "sum_U_Y_rows", corresponding to a
demand-sided view of **q**. Alternatively, an analyst can choose to use
the "sum_R_V_cols" method, corresponding to a supply-sided view of
**q**. In the case of a balanced ECC, the method does not matter.

Input-output matrices can be calculated for either an upstream swim
(demand-sided as Leontief) or a downstream swim (supply-sided as Ghosh).
The `direction` argument defines the direction. Different IO matrices
are calculated based on direction. The default is "upstream", meaning
that an upstream swim is desired. Note that "upstream", "demand", and
"Leontief" are synonyms. "downstream", "supply", and "Ghosh" are
synonyms.

## Examples

``` r
library(dplyr)
library(tidyr)
UKEnergy2000mats %>%
  spread(key = matrix.name, value = matrix) %>%
  select(Country, Year, EnergyType, LastStage, U, U_feed, V, Y, r_EIOU, S_units) %>%
  calc_io_mats()
#> # A tibble: 4 × 30
#>   Country  Year EnergyType LastStage U               U_feed   V        Y       
#>   <chr>   <dbl> <chr>      <chr>     <list>          <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [12 × 9]>  <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [17 × 17]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [13 × 13]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [17 × 17]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 22 more variables: r_EIOU <list>, S_units <list>, y <list>, q <list>,
#> #   f <list>, g <list>, h <list>, r <list>, W <list>, Z <list>, K <list>,
#> #   C <list>, D <list>, A <list>, O <list>, L_pxp <list>, L_ixp <list>,
#> #   Z_feed <list>, K_feed <list>, A_feed <list>, L_pxp_feed <list>,
#> #   L_ixp_feed <list>
```
