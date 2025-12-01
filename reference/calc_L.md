# Calculates total requirements matrices (**L_pxp** and **L_ixp** or **G_pxp** and **G_ixp**)

**L_pxp** tells how much of a product (in a row) is required to make
another product (in a column). **L_ixp** tells how much of an industry's
output (in a row) is required to make another product (in a column).
**G_pxp** and **G_ixp** are the Ghosh (downstream, supply-sided)
equivalents.

## Usage

``` r
calc_L(
  .sutdata = NULL,
  direction = c("upstream", "demand", "Leontief", "downstream", "supply", "Ghosh"),
  method = c("solve", "QR", "SVD"),
  tol = .Machine$double.eps,
  D = "D",
  A = "A",
  D_s = "D_s",
  B = "B",
  L_pxp = "L_pxp",
  L_ixp = "L_ixp",
  G_pxp = "G_pxp",
  G_ixp = "G_ixp"
)

calc_G(
  .sutdata = NULL,
  direction = c("upstream", "demand", "Leontief", "downstream", "supply", "Ghosh"),
  method = c("solve", "QR", "SVD"),
  tol = .Machine$double.eps,
  D = "D",
  A = "A",
  D_s = "D_s",
  B = "B",
  L_pxp = "L_pxp",
  L_ixp = "L_ixp",
  G_pxp = "G_pxp",
  G_ixp = "G_ixp"
)
```

## Arguments

- .sutdata:

  A data frame of supply-use table matrices with matrices arranged in
  columns. Default is `NULL`, meaning that matrices will be taken from
  the `D` and `A` arguments. Set to a list or data frame to pull
  matrices from its store.

- direction:

  A string that identifies the directionality of the IO matrices. See
  details. Default is "upstream".

- method:

  One of "solve", "QR", or "SVD". Default is "solve". See details.

- tol:

  The tolerance for detecting linear dependencies during matrix
  inversion. Default is `.Machine$double.eps`.

- D:

  The **D** matrix or name of the column in `.sutmats` that contains
  same. `D` is required for `direction = "upstream"`. Default is "D".

- A:

  The **A** matrix or name of the column in `.sutmats` that contains
  same. `D` is required for `direction = "upstream"`. Default is "A".

- D_s:

  The **D_s** matrix or name of the column in `.sutmats` that contains
  same. `D_s` is required for `direction = "downstream"`. Default is
  "D_s".

- B:

  The **B** matrix or name of the column in `.sutmats` that contains
  same. `B` is required for `direction = "downstream"`. Default is "B".

- L_pxp:

  The name for the **L_pxp** matrix on output. Default is "L_pxp".
  `L_pxp` is calculated by `inverse(I - A)`.

- L_ixp:

  The name for the **L_ixp** matrix on output. Default is "L_ixp".
  **L_ixp** is calculated by `D * L_pxp`.

- G_pxp:

  The name for the **G_pxp** matrix on output. Default is "G_pxp".
  `G_pxp` is calculated by `inverse(I - A_s)`.

- G_ixp:

  The name for the **G_ixp** matrix on output. Default is "G_ixp".
  **G_ixp** is calculated by `D_s * G_pxp`.

## Value

A list or data frame containing **L_pxp** and **L_ixp** or **G_pxp** and
**G_ixp** matrices.

## Details

Calculating some matrices requires a matrix inversion operation. The
`method` argument specifies which method should be used for calculating
the inverse. See
[`matsbyname::invert_byname()`](https://matthewheun.github.io/matsbyname/reference/invert_byname.html).

Both `tol` and `method` should be single values and apply to all
matrices being inverted.

Input-output matrices can be calculated for either an upstream swim
(demand-sided as Leontief) or a downstream swim (supply-sided as Ghosh).
The `direction` argument defines the direction. Different IO matrices
are calculated based on direction. The default is "upstream", meaning
that an upstream swim is desired. Note that "upstream", "demand", and
"Leontief" are synonyms. "downstream", "supply", and "Ghosh" are
synonyms.

Upstream swim matrices are named after Leontief and are called **L_pxp**
and **L_ixp**. Downstream swim matrices are named after Ghosh and are
called **G_pxp** and **G_ixp**. Which matrices are returned (**L** or
**G**) depends on the value of the `direction` argument. "upstream",
"demand", or "Leontief" generates **L** matrices. "downstream", "supply,
or "Ghosh" generates **G** matrices.

Note that for historical reasons, `calc_L()` and `calc_G()` are
synonyms. Both will calculate **L** matrices or **G** matrices,
depending on the value of the `direction` argument. But it is good
practice to call `calc_L()` when doing an upstream swim and `calc_G()`
when doing a downstream swim. Doing so clearly signals intent.
