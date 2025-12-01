# Tell whether industry inputs are unit-homogeneous and industry outputs are unit-homogeneous

Returns `TRUE` if each industry's inputs are unit-homogeneous and each
industry's outputs are unit homogeneous. When inputs have different
units from outputs, (but all inputs are unit-homogeneous and all outputs
are unit-homogeneous), `TRUE` is returned.

## Usage

``` r
inputs_outputs_unit_homogeneous(
  .sutmats = NULL,
  U = "U",
  V = "V",
  S_units = "S_units",
  keep_details = FALSE,
  ins_outs_unit_homogeneous = ".inputs_outputs_unit_homogeneous"
)
```

## Arguments

- .sutmats:

  a data frame of supply-use table matrices with matrices arranged in
  columns.

- U:

  a use (`U`) matrix or name of the column in `.sutmats` that contains
  same. Default is "`U`".

- V:

  a make (`V`) matrix or name of the column in `.sutmats` that contains
  same. Default is "`V`".

- S_units:

  an `S_units` matrix or name of a column in `.sutmats` that contains
  same. Default is "`S_units`".

- keep_details:

  if `TRUE`, per-industry results are returned; if `FALSE`, per-ECC
  results are returned.

- ins_outs_unit_homogeneous:

  the name of the output column that tells whether each industry's
  inputs and outputs are unit-homogeneous (though not necessarily in the
  same units). Default is "`.inputs_outputs_unit_homogeneous`".

## Value

a list or data frame containing `TRUE` if inputs from each energy
conversion industry are unit-homogeneous and outputs from each energy
conversion industry are unit-homogeneous, `FALSE` otherwise.

## Details

This function uses both
[inputs_unit_homogeneous](https://matthewheun.github.io/Recca/reference/inputs_unit_homogeneous.md)
and
[outputs_unit_homogeneous](https://matthewheun.github.io/Recca/reference/outputs_unit_homogeneous.md)
internally. This function differs from
[flows_unit_homogeneous](https://matthewheun.github.io/Recca/reference/flows_unit_homogeneous.md),
because
[flows_unit_homogeneous](https://matthewheun.github.io/Recca/reference/flows_unit_homogeneous.md)
requires that all flows are unit-homogeneous before returning `TRUE`.
This function (inputs_outputs_unit_homogeneous) will return true when
all inputs are unit-homogeneous with different units from outputs (which
are also unit-homogeoenous).

## See also

[inputs_unit_homogeneous](https://matthewheun.github.io/Recca/reference/inputs_unit_homogeneous.md),
[outputs_unit_homogeneous](https://matthewheun.github.io/Recca/reference/outputs_unit_homogeneous.md),
[flows_unit_homogeneous](https://matthewheun.github.io/Recca/reference/flows_unit_homogeneous.md)

## Examples

``` r
library(tidyr)
result <- UKEnergy2000mats %>%
  spread(key = "matrix.name", value = "matrix") %>%
  inputs_outputs_unit_homogeneous()
```
