# Calculates downstream effects of a new level of extracted resources

This function calculates the effect of changing the resources available
to an ECC, i.e. of a new resources matrix **R_prime** on the rest of the
ECC matrices (**U**, **V**, **W**, and **Y**). New versions of the
**U**, **V**, **W**, and **Y** matrices are returned, and respectively
called `U_prime`, `V_prime`, `W_prime`, and `Y_prime`. This function
assumes that each industry's inputs are perfectly substitutable (ps).

## Usage

``` r
new_R_ps(
  .sutmats = NULL,
  method = c("solve", "QR", "SVD"),
  tol = .Machine$double.eps,
  R_prime = "R_prime",
  U = "U",
  U_feed = "U_feed",
  V = "V",
  Y = "Y",
  q = "q",
  f = "f",
  G_pxp = "G_pxp",
  G_ixp = "G_ixp",
  O_s = "O_s",
  D_s = "D_s",
  D_feed_s = "D_feed_s",
  Z_s = "Z_s",
  U_prime = "U_prime",
  U_feed_prime = "U_feed_prime",
  U_eiou_prime = "U_EIOU_prime",
  r_eiou_prime = "r_EIOU_prime",
  V_prime = "V_prime",
  Y_prime = "Y_prime"
)
```

## Arguments

- .sutmats:

  a data frame of supply-use table matrices with matrices arranged in
  columns.

- method:

  One of "solve", "QR", or "SVD". Default is "solve". See details.

- tol:

  The tolerance for detecting linear dependencies during matrix
  inversion. Default is `.Machine$double.eps`.

- R_prime:

  The name of the new **R** matrix column in the input data frame, for
  which the new ECC must be assessed. Default is "R_prime".

- U:

  The name of the **U** matrix column in the input data frame. Default
  is "U".

- U_feed:

  The name of the **U_feed** matrix column in the input data frame.
  Default is "U_feed".

- V:

  The name of the **V** matrix column in the input data frame. Default
  is "V".

- Y:

  The name of the **Y** matrix column in the input data frame. Default
  is "Y".

- q:

  The name of the **q** vector column in the input data frame. Default
  is "q".

- f:

  The name of the **f** vector column in the input data frame. Default
  is "f".

- G_pxp:

  The name of the **G_pxp** matrix column in the input data frame.
  Default is "G_pxp".

- G_ixp:

  The name of the **G_ixp** matrix column in the input data frame.
  Default is "G_ixp".

- O_s:

  The name of the **O_s** matrix column in the input data frame. Default
  is "O_s", where "\_s" indicates supply-sided.

- D_s:

  The name of the **D_s** matrix column in the input data frame. Default
  is "D_s", where "\_s" indicates supply-sided.

- D_feed_s:

  The name of the **D_feed_s** matrix column in the input data frame.
  Default is "D_feed_s", where "\_s" indicates supply-sided.

- Z_s:

  The name of the **Z_s** matrix column in the input data frame. Default
  is "Z_s", where "\_s" indicates supply-sided.

- U_prime:

  The name of the output column containing the new **U** matrices.
  Default is "U_prime".

- U_feed_prime:

  The name of the output column containing the new **U_feed** matrices.
  Default is "U_feed_prime".

- U_eiou_prime:

  The name of the output column containing the new **U_EIOU** matrices.
  Default is "U_EIOU_prime".

- r_eiou_prime:

  The name of the output column containing the new **r_EIOU** matrices.
  Default is "r_EIOU_prime".

- V_prime:

  The name of the output column containing the new **V** matrices.
  Default is "V_prime".

- Y_prime:

  The name of the output column containing the new **Y** matrices.
  Default is "Y_prime".

## Value

A data frame with added columns representing each of the new
**U_prime**, **U_feed_prime**, **U_EIOU_prime**, **r_EIOU_prime**,
**V_prime**, and **Y_prime** matrices.

## Details

Each industry must be unit-homogeneous on its inputs. If not, a matrix
populated with `NA` is returned as the result for **U_prime**,
**V_prime**, and **Y_prime**.

Calculating the new matrices requires a matrix inversion operation. The
`method` argument specifies which method should be used for calculating
the inverse. "solve" uses
[`base::solve()`](https://rdrr.io/r/base/solve.html) and the value of
`tol`. "QR" uses [`base::solve.qr()`](https://rdrr.io/r/base/qr.html)
and the value of `tol`. "SVD" uses
[`matrixcalc::svd.inverse()`](https://rdrr.io/pkg/matrixcalc/man/svd.inverse.html),
ignoring the `tol` argument.

Both `tol` and `method` should be a single values and apply to all
matrices in `a`.

## Examples

``` r
UKEnergy2000mats %>%
  tidyr::spread(key = "matrix.name", value = "matrix") %>%
  # When LastStage is "services", we get units problems.
  # Avoid by using only ECCs with "Final" and "Useful" as the LastStage.
  dplyr::filter(LastStage != IEATools::last_stages$services) %>%
  # Calculate the input-output matrices which are inputs to the new_R function.
  calc_io_mats(direction = "downstream") %>%
  # Make an R_prime matrix that gives twice the resource inputs to the economy.
  dplyr::mutate(
    R_prime = matsbyname::hadamardproduct_byname(2, R)
  ) %>%
  # Now call new_R_ps() which will calculate
  # updated U, V, and Y matrices (U_prime, V_prime, and Y_prime)
  # given R_prime.
  # Each of the *_prime matrices should be 2x their originals,
  # because R_prime is 2x relative to R.
  new_R_ps()
#> # A tibble: 2 × 34
#>   Country  Year EnergyType LastStage R             S_units  U        U_EIOU  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 26 more variables: U_feed <list>, V <list>, Y <list>, r_EIOU <list>,
#> #   y <list>, q <list>, f <list>, g <list>, h <list>, r <list>, W <list>,
#> #   Z_s <list>, C_s <list>, D_s <list>, D_feed_s <list>, O_s <list>, B <list>,
#> #   G_pxp <list>, G_ixp <list>, R_prime <list>, U_prime <list>,
#> #   U_feed_prime <list>, U_EIOU_prime <list>, r_EIOU_prime <list>,
#> #   V_prime <list>, Y_prime <list>
```
