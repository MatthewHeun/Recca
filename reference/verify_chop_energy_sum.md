# Verify energy sum after chop calculations

**R** and **Y** chop calculations involve isolating rows or columns of
the **R** and **Y** matrices, performing downstream swims (with
[`new_R_ps()`](https://matthewheun.github.io/Recca/reference/new_R_ps.md))
and upstream swims (with
[`new_Y()`](https://matthewheun.github.io/Recca/reference/new_Y.md)),
and creating the ECC portions that follow from the row or column of
**R** or support the creation of the row or column of **Y**. After
performing that downstream or upstream swim, the sum of the isolated
(chopped) ECCs should equal the original ECC. This function performs
that energy balance verification.

## Usage

``` r
verify_chop_energy_sum(
  .sut_data = NULL,
  tol = 1e-04,
  R_mat,
  U_mat,
  U_feed_mat,
  V_mat,
  Y_mat,
  R_chop_list,
  U_chop_list,
  U_feed_chop_list,
  V_chop_list,
  Y_chop_list
)
```

## Arguments

- .sut_data:

  An optional data frame of energy conversion chain matrices.

- tol:

  The tolerance within which energy balance is assumed to be OK. Default
  is `1e-4`.

- R_mat, U_mat, U_feed_mat, V_mat, Y_mat:

  The matrices of the original ECC.

- R_chop_list, U_chop_list, U_feed_chop_list, V_chop_list, Y_chop_list:

  Lists of matrices from different upstream swims corresponding to
  different rows or columns of **Y**.

## Value

`TRUE` if energy balance is observed, `FALSE` otherwise.

## Details

The various `*_chop_list` arguments should be lists of matrices formed
by isolating (chopping) different parts of **R** or **Y**. The matrices
in `R_chop_list`, `U_chop_list`, `U_feed_chop_list` `V_chop_list`, and
`Y_chop_list` should sum to `R`, `U`, `U_feed`, `V`, and `Y`,
respectively.

This is not a public function. It is an internal helper function for
[`chop_R()`](https://matthewheun.github.io/Recca/reference/chop-doc.md)
and
[`chop_Y()`](https://matthewheun.github.io/Recca/reference/chop-doc.md).
