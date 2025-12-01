# Calculate the `E` matrix for embodied energy calculations

Calculate the `E` matrix for embodied energy calculations

## Usage

``` r
calc_E(
  .iomats = NULL,
  R = "R",
  V = "V",
  U_feed = "U_feed",
  g = "g",
  r = "r",
  E = "E"
)
```

## Arguments

- .iomats:

  a data frame containing matrices that describe the Input-Output
  structure of an Energy Conversion Chain. `.iomats` will likely have
  been obtained from the
  [`calc_io_mats`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md)
  function.

- R:

  A resources (**R**) matrix or name of column in `.iomats` containing
  same. Default is "R".

- V:

  A make (**V**) matrix or name of column in `.iomats` containing same.
  Default is "V".

- U_feed:

  A feedstock use (**U_feed**) matrix or name of column in `.iomats`
  containing same. Default is "U_feed".

- g:

  A **g** vector or name of column in `.iomats` containing same. Default
  is "g".

- r:

  An **r** vector or name of column in `.iomats` containing same.
  Default is "r".

- E:

  The name for the **E** matrix on output. **E** is calculated by
  `W %*% g_hat_inv`. Default is "E".

## Value

A list or data frame containing `E` matrices.
