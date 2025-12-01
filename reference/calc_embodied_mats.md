# Calculate various embodied energy matrices

Calculate various embodied energy matrices

## Usage

``` r
calc_embodied_mats(
  .iomats = NULL,
  q = "q",
  g = "g",
  r = "r",
  L_ixp = "L_ixp",
  A = "A",
  R = "R",
  V = "V",
  U_feed = "U_feed",
  Y = "Y",
  G = "G",
  H = "H",
  E = "E",
  M_p = "M_p",
  M_s = "M_s",
  F_footprint_p = "F_footprint_p",
  F_effects_p = "F_effects_p",
  F_footprint_s = "F_footprint_s",
  F_effects_s = "F_effects_s"
)
```

## Arguments

- .iomats:

  a data frame containing matrices that describe the Input-Output
  structure (using the supply-use table format) of an Energy Conversion
  Chain. `.iomats` will likely have been obtained from the
  [`calc_io_mats()`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md)
  function.

- q:

  Final demand (**q**) vector or name of the column in `.iomats`
  containing same. Default is "q".

- g:

  A **g** vector or name of the column in `.iomats` containing same.
  Default is "g".

- r:

  An **r** vector or name of the column in `.iomats` containing same.
  Default is "r".

- L_ixp:

  Industry-by-product Leontief (**L_ixp**) matrix or name of the column
  in `.iomats` containing same. Default is "L_ixp".

- A:

  An **A** matrix or name of the column in `.iomats` containing same.
  Default is "A".

- R:

  A resources (**R**) matrix or name of the column in `.iomats`
  containing same. Default is "R".

- V:

  A make (**V**) matrix or name of the column in `.iomats` containing
  same. Default is "V".

- U_feed:

  A feedstock use (**U_feed**) matrix or name of the column in `.iomats`
  containing same. Default is "U_feed".

- Y:

  A final demand (**Y**) matrix or name of the column in `.iomats`
  containing same. Default is "Y".

- G:

  The name of the **G** matrix on output. **G** is calculated by
  `L_ixp %*% y_hat`. Default is "G".

- H:

  The name of the **H** matrix on output. **H** is calculated by
  `L_ixp %*% Y`. Default is "H".

- E:

  The name of the **E** matrix on output. **E** is calculated by
  `W %*% g_hat_inv`. Default is "E".

- M_p:

  The name of the **M_p** matrix on output. **M_p** is formed from
  column sums of positive entries in the various **Q**\_x matrices.
  Default is "M_p".

- M_s:

  The name of the **M_s** matrix on output. **M_s** is calculated by
  `M_p %*% q_hat_inv %*% Y`. Default is "M_s".

- F_footprint_p:

  The name of the **F_footprint_p** matrix on output. **F_footprint_p**
  is calculated by `M_p %*% (M_p^T %*% i)_hat_inv`. Default is
  "F_footprint_p".

- F_effects_p:

  The name of the **F_effects_p** matrix on output. **F_effects_p** is
  calculated by `(M_p %*% i)_hat_inv %*% M_p`. Default is "F_effects_p".

- F_footprint_s:

  The name of the **F_footprint_s** matrix on output. **F_footprint_s**
  is calculated by `M_s %*% (M_s^T %*% i)_hat_inv`. Default is
  "F_footprint_s".

- F_effects_s:

  The name of the **F_effects_s** matrix on output. **F_effects_s** is
  calculated by `(M_s %*% i)_hat_inv %*% M_s`. Default is "F_effects_s".

## Value

A list or data frame containing embodied energy matrices.
