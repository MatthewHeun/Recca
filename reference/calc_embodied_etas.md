# Embodied energy efficiencies

Embodied energy efficiencies are based on the total upstream primary
energy demand for a product produced by the ECC or for the energy
consumed by a final demand sector of the ECC. This function calculates
both. **eta_s** gives sector-based embodied energy efficiency, and
**eta_p** gives product-based embodied energy efficiency.

## Usage

``` r
calc_embodied_etas(
  .embodiedmats = NULL,
  primary_machine_names,
  Y = "Y",
  G = "G",
  H = "H",
  eta_p = "eta_p",
  eta_s = "eta_s"
)
```

## Arguments

- .embodiedmats:

  A data frame containing columns of **Y**, **G**, and **H** matrices.

- primary_machine_names:

  A list of strings representing names of Industries whose output is
  counted in Total Energy Supply (TPES).

- Y:

  A final demand (**Y**) matrix or name of a column in `.embodiedmats`
  containing same. Default is "Y".

- G:

  A **G** matrix or name of a column in `.embodiedmats` containing same.
  Default is "G".

- H:

  An **H** matrix or name of a column in `.embodiedmats` containing
  same. Default is "H".

- eta_p:

  The name for product-based efficiencies on output. Default is "eta_p".

- eta_s:

  The name for final-demand-sector-based efficiencies on output. Default
  is "eta_s".

## Value

A list or data frame containing embodied energy efficiencies.

## Details

Note that these efficiencies (**eta_s** and **eta_p**) are different
from energy conversion industry efficiencies. To calculate energy
conversion industry efficiencies, use the
[`calc_eta_i()`](https://matthewheun.github.io/Recca/reference/calc_eta_i.md)
function.
