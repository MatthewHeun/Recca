# Calculate embodied energy matrices

**M** is calculated by `e_hat * G`, but `e_hat` contains lists of
matrices, so the **M** is also contain lists of matrices. In each list,
there is one **M** matrix for each Product in the Energy conversion
chain.

## Usage

``` r
calc_M(
  .YqGHEdata = NULL,
  Y = "Y",
  q = "q",
  G = "G",
  E = "E",
  tol = 1e-04,
  M_p = "M_p",
  M_s = "M_s"
)
```

## Arguments

- .YqGHEdata:

  A data frame containing columns of **q** vectors and **Y**, **G**,
  **H**, and **E** matrices. `.YqGEdata` will likely have been obtained
  from the
  [`calc_GH()`](https://matthewheun.github.io/Recca/reference/calc_GH.md)
  and
  [`calc_E()`](https://matthewheun.github.io/Recca/reference/calc_E.md)
  functions.

- Y:

  A final demand (**Y**) matrix or name of the column in `.YqHGEdata`
  containing same. Default is "Y".

- q:

  A **q** column vector or name of the column in `.YqHGEdata` containing
  same. Default is "q".

- G:

  A **G** matrix or name of the column in `.YqHGEdata` containing same.
  Default is "G".

- E:

  An **E** matrix or name of the column in `.YqHGEdata` containing same.
  Default is "E".

- tol:

  The allowable energy balance error.

- M_p:

  The name for matrices of embodied energy in products on output.
  Default is "M_p". These matrices contain embodied products in rows and
  embodying products in columns.

- M_s:

  The name for matrices of embodied energy consumed by final demand
  sectors. Default is "M_s". These matrices contain embodied products in
  rows and consuming final demand sectors in columns.

## Value

A list or data frame of embodied energy matrices.
