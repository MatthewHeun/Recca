# Calculate **Z**, **K**, **C**, **D**, **A**, **B**, and **O** matrices

These matrices define the IO structure of an energy conversion chain.

## Usage

``` r
calc_A(
  .sutdata = NULL,
  direction = c("upstream", "demand", "Leontief", "downstream", "supply", "Ghosh"),
  R = "R",
  U = "U",
  U_feed = "U_feed",
  V = "V",
  Y = "Y",
  q = "q",
  f = "f",
  g = "g",
  r = "r",
  h = "h",
  Z = "Z",
  K = "K",
  C = "C",
  D = "D",
  A = "A",
  O = "O",
  Z_s = "Z_s",
  C_s = "C_s",
  D_s = "D_s",
  D_feed_s = "D_feed_s",
  B = "B",
  O_s = "O_s"
)
```

## Arguments

- .sutdata:

  a data frame of supply-use table matrices with matrices arranged in
  columns.

- direction:

  A string that identifies the directionality of the IO matrices. See
  details. Default is "upstream".

- R:

  resources (**R**) matrix or name of the column in `.sutmats` that
  contains same. Default is "R". `R` is an optional argument. If all of
  **R** is added to **V**, this argument can be left unspecified.

- U:

  Use (**U**) matrix or name of the column in `.sutmats` that contains
  same. Default is "U".

- U_feed:

  Feed portion of the use matrix (**U_feed**) or name of the column in
  `.sutmats` that contains same. Default is "U_feed".

- V:

  Make (**V**) matrix or name of the column in `.sutmats`that contains
  same. Default is "V".

- Y:

  Final demand (**Y**) matrix or name of the column in `.sutmats`that
  contains same. Default is "Y".

- q:

  A **q** vector or name of the column in `.sutmats` that contains same.
  Default is "q".

- f:

  An **f** vector or name of the column in `.sutmats` that contains
  same. Default is "r".

- g:

  A **g** vector or name of the column in `.sutmats` that contains same.
  Default is "g".

- r:

  An **r** vector or name of the column in `.sutmats` that contains
  same. Default is "r".

- h:

  An **h** vector or name of the column in `.sutmats` that contains
  same. Default is "h".

- Z:

  The name for the **Z** matrix on output. Default is "Z". **Z** is
  calculated by `U * g_hat_inv`.

- K:

  The name for the **K** matrix on output. Default is "K". **K** is
  calculated by `U * f_hat_inv`.

- C:

  The name for the **C** matrix on output. Default is "C". **C** is
  calculated by `transpose(V) * g_hat_inv`.

- D:

  The name for the **D** matrix on output. Default is "D". **D** is
  calculated by `V * q_hat_inv`.

- A:

  The name for the **A** matrix on output. Default is "A". **A** is
  calculated by `Z * D`.

- O:

  The name for the **O** matrix on output. Default is "O". **O** is
  calculated by `r_hat_inv * R`.

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

- O_s:

  The name for the **O_s** matrix on output. Default is "O_s". **O** is
  calculated by `q_hat_inv * Y`.

## Value

A list or data frame containing **Z**, **K**, **C**, **D**, **A**, and
**O** matrices or **Z_s**, **C_s**, **D_s**, **D_feed_s**, **B**, and
**O_s** matrices, depending on the value of `direction`.

## Details

Input-output matrices can be calculated for either an upstream swim
(demand-sided as Leontief) or a downstream swim (supply-sided as Ghosh).
The `direction` argument defines the direction. Different IO matrices
are calculated based on direction. The default is "upstream", meaning
that an upstream swim is desired. Note that "upstream", "demand", and
"Leontief" are synonyms. "downstream", "supply", and "Ghosh" are
synonyms.

For `direction = "upstream"`, **Z**, **K**, **C**, **D**, **A**, and
**O** matrices are calculated. For `direction = "downstream"`, **Z_s**,
**C_s**, **D_s**, **D_feed_s**, **B**, and **O_s** matrices are
calculated.
