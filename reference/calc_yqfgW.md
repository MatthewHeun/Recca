# Calculate **y**, **f**, **g**, **q**, **h**, and **r** vectors and the **W** matrix

Note that a necessary condition for calculating the **f**, **g**, and
**r** vectors is that the **R_bar**, **U_bar**, and **V_bar** matrices
should have only one entry per column and row, respectively, meaning
that all products entering a given industry need to be unit homogeneous
before we can calculate the **f** vector and all products of a given
industry are measured in the same units before we can calculate the
**g** vector. If the unit homogeneity assumptions above are violated, we
will return `NA` for violating industries in the **f** and **g**
vectors. The checks for unit homogeneity are performed only when an
**S_units** matrix is present.

## Usage

``` r
calc_yqfgW(
  .sutdata = NULL,
  method_q_calculation = c("sum_U_Y_rows", "sum_R_V_cols"),
  R = "R",
  U = "U",
  V = "V",
  Y = "Y",
  S_units = "S_units",
  y = "y",
  q = "q",
  f = "f",
  g = "g",
  h = "h",
  r = "r",
  W = "W"
)
```

## Arguments

- .sutdata:

  a data frame of supply-use table matrices with matrices arranged in
  columns.

- method_q_calculation:

  Specifies the method with which the q vector should be calculated. See
  details.

- R:

  The resources (**R**) matrix or name of the column in `.sutmats` that
  contains same. Default is "R".

- U:

  The use (**U**) matrix or name of the column in `.sutmats` that
  contains same. Default is "U".

- V:

  The make (**V**) matrix or name of the column in `.sutmats` that
  contains same. Default is "V".

- Y:

  The final demand (**Y**) matrix or name of the column in \`.sutmats“
  that contains same. Default is "Y".

- S_units:

  The **S_units** matrix or name of the column in `.sutmats` that
  contains same. Default is "S_units".

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

## Value

A list or data frame containing **y**, **q**, **f**, **g**, **h**, and
**r** vectors and the **W** matrix.

## Details

`method_q_calculation` specifies the method with which the q vector
should be calculated. Default is "sum_U_Y_rows", corresponding to a
demand-sided view of **q**. Alternatively, an analyst can choose to use
the "sum_R_V_cols" method, corresponding to a supply-sided view of
**q**. In the case of a balanced ECC, the method does not matter. Both
methods give a column vector as a result.
