# Calculate the `G` and `H` matrices for embodied energy calculations

Calculate the `G` and `H` matrices for embodied energy calculations

## Usage

``` r
calc_GH(
  .iomats = NULL,
  Y = "Y",
  L_ixp = "L_ixp",
  R = "R",
  A = "A",
  q = "q",
  G_V = "G_V",
  G_R = "G_R",
  G = "G",
  H_V = "H_V",
  H_R = "H_R",
  H = "H"
)
```

## Arguments

- .iomats:

  a data frame containing matrices that describe the Input-Output
  structure of an Energy Conversion Chain. `.iomats` will likely have
  been obtained from the
  [`calc_io_mats`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md)
  function.

- Y:

  final demand (`Y`) matrix or name of the column in `.iodata`
  containing same. Default is "`Y`".

- L_ixp:

  industry-by-product Leontief (`L_ixp`) matrix or name of the column in
  `.iodata` containing same. Default is "`L_ixp`".

- R:

  Resources (`R`) matrix or name of the column in `.iodata` containing
  same. Default is "`R`".

- A:

  The name of the `A` matrix column in the `.iomats` data frame. Default
  is "A".

- q:

  The name of the `q` vector in the `.iomats` data frame. Default is
  "q".

- G_V:

  name for the `G_V` matrix on output. Default is "`G_V`". `G_V` is
  calculated by `L_ixp * y_hat`.

- G_R:

  name for the `G_R` matrix on output. Default is "`G_R`". `G_R` is
  calculated by `R * q_hat_inv * L_pxp * y_hat`.

- G:

  name for the `G` matrix on output. Default is "`G`". `G` is calculated
  by `G_R + G_V`.

- H_V:

  name for the `H_V` matrix on output. Default is "`H_V`". `H_V` is
  calculated by `L_ixp * Y`.

- H_R:

  name for the `H_R` matrix on output. Default is "`H_R`". `H_R` is
  calculated by `R * q_hat_inv * L_pxp * Y`.

- H:

  name for the `H` matrix on output. Default is "`H`". `H` is calculated
  by `H_V + H_R`.

## Value

a list or data frame containing `G` and `H` matrices.
