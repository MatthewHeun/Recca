# Assess the effect of changing perfectly substitutable intermediate inputs to an industry

This function calculates the effect of changing perfectly-substitutable
(ps) inputs to an intermediate industry. New versions of `U` and `V`
matrices are returned as `U_prime` and `V_prime`. Changes are made
upstream of the changed industry inputs. The final demand matrix (`Y`)
is unchanged.

## Usage

``` r
new_k_ps(
  .sutmats = NULL,
  k_prime = "k_prime",
  R = "R",
  U = "U",
  V = "V",
  Y = "Y",
  K = "K",
  L_ixp = "L_ixp",
  L_pxp = "L_pxp",
  Z = "Z",
  D = "D",
  f = "f",
  R_prime = "R_prime",
  U_prime = "U_prime",
  V_prime = "V_prime"
)
```

## Arguments

- .sutmats:

  a data frame of supply-use table matrices with matrices arranged in
  columns.

- k_prime:

  a new column vector for the `K` matrix representing new inputs to an
  industry or name of a column in `.sutmats` containing same. Default is
  "k_prime". The name of the single `k_prime` column must match the name
  of one of the columns of matrix `K`.

- R:

  resource (`R`) matrix or name of the column in `.sutmats` that
  contains same. Default is "R".

- U:

  use (`U`) matrix or name of the column in `.sutmats` that contains
  same. Default is "U".

- V:

  make (`V`) matrix or name of the column in `.sutmats`that contains
  same. Default is "V".

- Y:

  final demand (`Y`) matrix or name of the column in `.sutmats` that
  contains same. Default is "Y".

- K:

  a `K` matrix or name of the column in `.sutmats` that contains same.
  Default is "K". `K` consists of columns that sum to 1. Elements of `K`
  indicate the fraction of total input to industries (in columns)
  provided by products (in rows). `K` can be calculated by
  [`calc_io_mats()`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md).

- L_ixp:

  an (`L_ixp`) matrix or name of the column in `.sutmats` that contains
  same. Default is "L_ixp".

- L_pxp:

  an (`L_pxp`) matrix or name of the column in `.sutmats` that contains
  same. Default is "L_pxp".

- Z:

  a `Z` matrix or name of the column in `.sutmats` that contains same.
  Default is "Z".

- D:

  a `D` matrix or name of the column in `.sutmats` that contains same.
  Default is "D".

- f:

  an `f` vector or name of the column in `sutmats` that contains same.
  Default is "f".

- R_prime:

  name for the `R_prime` matrix on output. Default is "R_prime".

- U_prime:

  name for the `U_prime` matrix on output. Default is "U_prime".

- V_prime:

  name for the `V_prime` matrix on output. Default is "V_prime".

## Value

a list or data frame containing `U_prime` and `V_prime` matrices

## Details

Note that inputs `K`, `L_ixp`, `L_pxp`, `Z`, `D`, and `f` can be
conveniently calculated by the function
[`calc_io_mats()`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md).

Internally, this function uses
[`matsindf::matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.html),
and documentation assumes that `.sutmats` is not `NULL` and is a data
frame. If `.sutmats` is present, output is a data frame with columns
named by string values of output arguments, and input arguments should
be character strings that name columns in `.sutmats`. If `.sutmats` is
`NULL` (the default), output is a list with items named by output
strings, and input arguments should be single matrices or vectors.

## Examples

``` r
library(dplyr)
library(matsbyname)
library(tidyr)
# To demonstrate calculating changes to an energy conversion chain due to changes
# in perfectly-substitutable inputs to an intermediate industry,
# we use the PerfectSubmats data frame.
# But we need to calculate several important input-output matrices first.
io_mats <- PerfectSubmats %>%
  tidyr::spread(key = "matrix.name", value = "matrix") %>%
  calc_io_mats()
# Next, find the K matrix that contains the fraction of each type of energy
# that enters each industry
K <- io_mats$K[[1]]
# Develop a new column vector for inputs to the Electric transport sector.
# As provided, the Electric transport sector is dominated by Renewable elec.
# What if the electricity input to the Electric transport sector
# were split 50/50 between Renewable elect and FF elec?
k_prime_vec <- K[, "Electric transport", drop = FALSE]
k_prime_vec["FF elec", "Electric transport"] <- 0.5
k_prime_vec["Ren elec", "Electric transport"] <- 0.5
# Add k_prime_vec to the io_mats data frame.
io_mats <- io_mats %>%
  dplyr::mutate(
    # Set up a new k_prime vector for Electric transport.
    # That vector will be used for the infininte substitution calculation.
    k_prime = matsbyname::select_cols_byname(K,
           retain_pattern = RCLabels::make_or_pattern("Electric transport",
                                                      pattern_type = "exact")),
    k_prime = RCLabels::make_list(k_prime_vec, n = 1)
  )
# Now do the calculation of U_prime and V_prime matrices.
new_UV <- new_k_ps(io_mats)
# There is much more FF extraction now than before.
io_mats$U[[1]]["FF", "FF extraction"]
#> [1] 87
new_UV$U_prime[[1]]["FF", "FF extraction"]
#> [1] 101.8523
```
