# Calculate final-to-useful efficiencies for final demand and EIOU when last stage is final

Final-to-useful efficiencies for energy carriers and sectors in final
demand and energy industry own use can be calculated from allocations
(`C_Y` and `C_eiou`), machine efficiencies (`eta_i`), and (for exergetic
efficiencies) exergy-to-energy ratios (`phi`). This function performs
those calculations. By default, the output contains matrices in the same
structure as **Y** and **U_EIOU** when last stage is final.

## Usage

``` r
calc_eta_fu_Y_eiou(
  .c_mats_eta_phi_vecs = NULL,
  C_Y = Recca::alloc_cols$C_Y,
  C_eiou = Recca::alloc_cols$C_eiou,
  eta_i = Recca::efficiency_cols$eta_i,
  phi = Recca::psut_cols$phi,
  matricize = TRUE,
  energy_type = Recca::energy_types$energy_type,
  eta_fu = Recca::efficiency_cols$eta_fu,
  energy = Recca::energy_types$e,
  exergy = Recca::energy_types$x,
  eta_fu_Y_e = paste0(eta_fu, "_Y_", energy),
  eta_fu_Y_x = paste0(eta_fu, "_Y_", exergy),
  eta_fu_eiou_e = paste0(eta_fu, "_EIOU_", energy),
  eta_fu_eiou_x = paste0(eta_fu, "_EIOU_", exergy),
  notation = RCLabels::arrow_notation
)
```

## Arguments

- .c_mats_eta_phi_vecs:

  A data frame containing allocation matrices (`C_Y` and `C_eiou`),
  vectors of machine efficiencies (`eta_i`), and exergy-to-energy ratio
  vectors (`phi`). Default is `NULL`, in which case individual matrices
  or vectors can be passed to `C_Y`, `C_eiou`, `eta_i`, and `phi`.

- C_Y:

  The name of the column in `.c_mats_eta_phi_vecs` containing allocation
  matrices for final demand (the `Y` in `C_Y`). Or a single **C_Y**
  matrix. Or a list of **C_Y** matrices. Default is
  `Recca::alloc_cols$C_Y`.

- C_eiou:

  The name of the column in `.c_mats_eta_phi_vecs` containing allocation
  matrices for energy industry own use (the `eiou` in `C_eiou`). Or a
  single **C_EIOU** matrix. Or a list of **C_EIOU** matrices. Default is
  `Recca::alloc_cols$C_eiou`.

- eta_i:

  The name of the column in `.c_mats_eta_phi_vecs` containing machine
  efficiencies. Or a single **eta_i** vector. Or a list of **eta_i**
  vectors. Default is `Recca::efficiency_cols$eta_i`.

- phi:

  The name of the column in `.c_mats_eta_phi_vecs` containing
  exergy-to-energy ratios. Or a single **phi** vector. Default is
  `Recca::psut_cols$phi`.

- matricize:

  A boolean that tells whether to return matrices of the same structure
  as **Y** and **U_EIOU** when last stage is final. Default is `TRUE`.
  `FALSE` returns a column vector with rows same as **C_Y** and
  **C_EIOU**.

- energy_type, energy, exergy:

  See
  [`Recca::energy_types`](https://matthewheun.github.io/Recca/reference/energy_types.md).

- eta_fu:

  The base name of the output columns. Default is
  `Recca::efficiency_cols$eta_fu`.

- eta_fu_Y_e:

  The name of the energy efficiency output column for energy flowing
  into final demand (**Y**). Default is `paste0(eta_fu, "_Y_", energy)`.

- eta_fu_Y_x:

  The name of the exergy efficiency output column for energy flowing
  into final demand (**Y**). Default is `paste0(eta_fu, "_Y_", exergy)`.

- eta_fu_eiou_e:

  The name of the energy efficiency output column for energy flowing
  into the energy industry (**U_EIOU**). Default is
  `paste0(eta_fu, "_EIOU_", energy)`.

- eta_fu_eiou_x:

  The name of the exergy efficiency output column for energy flowing
  into the energy industry (**U_EIOU**). Default is
  `paste0(eta_fu, "_EIOU_", energy)`.

- notation:

  The notation for the row and column labels of the matrices and
  vectors. Default is
  [`RCLabels::arrow_notation`](https://matthewheun.github.io/RCLabels/reference/arrow_notation.html).

## Value

A data frame or list containing final-to-useful efficiencies.

## Details

The matrix formula for calculating energy efficiencies is **eta_fu_E**
`=` **C** `*` **eta_i**, where **C** is one of **C_Y** or **C_EIOU**.
The matrix formula for calculating exergy efficiencies from allocations
and machine energy efficiencies is **eta_fu_X** `=` (**phi_u_hat_inv**
`*` (**C** `*` **eta_fu_hat**)) `*` **phi_u**.

The **C_Y** matrix is assumed to have rows named with prefixes of final
energy carriers and suffixes of the final demand sector into which the
final energy carrier flows. The **C_EIOU** matrix is similar, except
that its rows are named with suffixes of the energy industry sector into
which the final energy carrier flows. The columns of the **C_Y** and
**C_EIOU** matrices are named with prefixes of machine names and
suffixes of useful energy product made by the machine. See examples.

The **eta_i** vector of machine efficiencies has rows named with
prefixes same as **C_Y** and **C_EIOU** columns. The name of the
**eta_i** column is not used. See examples.

The **phi** vector of exergy-to-energy ratios has rows named with energy
carriers that correspond to the prefixes of **C_Y** and **C_EIOU** rows
and the suffixes of **C_Y** and **C_EIOU** columns. See examples.

This function uses
[`matsbyname::vec_from_store_byname()`](https://matthewheun.github.io/matsbyname/reference/vec_from_store_byname.html)
to construct the `eta_i` and `phi` vectors before multiplying, thereby
eliminating unnecessary growth of the output matrices.

## Examples

``` r
C_Y <- matrix(c(0.7, 0.3, 0,   0,   0,
                0,   0,   0.2, 0.5, 0.3), byrow = TRUE, nrow = 2, ncol = 5,
              dimnames = list(c("Electricity -> Non-ferrous metals",
                                "PSB -> Residential"),
                              c("Electric arc furnaces -> HTH.600.C",
                                "Electric lights -> L",
                                "Wood stoves -> LTH.20.C",
                                "Wood stoves -> LTH.50.C",
                                "Wood stoves -> MTH.100.C")))
C_Y
#>                                   Electric arc furnaces -> HTH.600.C
#> Electricity -> Non-ferrous metals                                0.7
#> PSB -> Residential                                               0.0
#>                                   Electric lights -> L Wood stoves -> LTH.20.C
#> Electricity -> Non-ferrous metals                  0.3                     0.0
#> PSB -> Residential                                 0.0                     0.2
#>                                   Wood stoves -> LTH.50.C
#> Electricity -> Non-ferrous metals                     0.0
#> PSB -> Residential                                    0.5
#>                                   Wood stoves -> MTH.100.C
#> Electricity -> Non-ferrous metals                      0.0
#> PSB -> Residential                                     0.3
eta_i <- matrix(c(0.9, 0.2, 0.4, 0.4, 0.3), nrow = 5, ncol = 1,
                  dimnames = list(c("Electric arc furnaces -> HTH.600.C",
                                    "Electric lights -> L",
                                    "Wood stoves -> LTH.20.C",
                                    "Wood stoves -> LTH.50.C",
                                    "Wood stoves -> MTH.100.C"),
                                  "eta_i"))
eta_i
#>                                    eta_i
#> Electric arc furnaces -> HTH.600.C   0.9
#> Electric lights -> L                 0.2
#> Wood stoves -> LTH.20.C              0.4
#> Wood stoves -> LTH.50.C              0.4
#> Wood stoves -> MTH.100.C             0.3
phi <- matrix(c(1, 1.1, 1 - 298.15/(600+273.15), 0.95,
                1 - (20 + 273.15)/298.15,
                1 - 298.15/(50+273.15),
                1 - 298.15/(100+273.15)),
              nrow = 7, ncol = 1,
              dimnames = list(c("Electricity", "PSB", "HTH.600.C", "L",
                                "LTH.20.C", "LTH.50.C", "MTH.100.C"),
                              "phi"))
phi
#>                    phi
#> Electricity 1.00000000
#> PSB         1.10000000
#> HTH.600.C   0.65853519
#> L           0.95000000
#> LTH.20.C    0.01677008
#> LTH.50.C    0.07736345
#> MTH.100.C   0.20099156
res <- calc_eta_fu_Y_eiou(C_Y = C_Y, C_eiou = C_Y, eta_i = eta_i, phi = phi)
res$eta_fu_Y_E
#> NULL
res$eta_fu_EIOU_E # Same because C_Y and C_EIOU are same
#> NULL
res$eta_fu_Y_X
#> NULL
res$eta_fu_EIOU_X # Same because C_Y and C_EIOU are same
#> NULL
res2 <- calc_eta_fu_Y_eiou(C_Y = C_Y, C_eiou = C_Y, eta_i = eta_i, phi = phi,
                           matricize = FALSE)
res2$eta_fu_Y_E
#> NULL
res2$eta_fu_Y_X
#> NULL
```
