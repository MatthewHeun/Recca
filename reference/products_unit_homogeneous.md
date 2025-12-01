# Tell whether ECC products are unit-homogenous

Returns `TRUE` if products are unit-homogeneous according to the
`S_units` matrix and `FALSE` otherwise.

## Usage

``` r
products_unit_homogeneous(
  .sutmats = NULL,
  S_units = "S_units",
  keep_details = FALSE,
  products_unit_homogeneous = ".products_unit_homogeneous"
)
```

## Arguments

- .sutmats:

  a data frame of supply-use table matrices with matrices arranged in
  columns.

- S_units:

  an `S_units` matrix or name of a column in `.sutmats` that contains
  same. Default is "`S_units`".

- keep_details:

  if `TRUE`, per-product results are returned; if `FALSE`, per-ECC
  results are returned.

- products_unit_homogeneous:

  name for the boolean that tells whether products in `S_units` are
  unit-homogeneous on output. Default is "`.products_unit_homogeneous`".

## Value

a list or data frame containing `TRUE` if products in `S_units` are
unit-homogeneous, `FALSE` otherwise.

importFrom magrittr extract2

## Examples

``` r
library(magrittr)
library(tidyr)
UKEnergy2000mats %>%
  spread(key = "matrix.name", value = "matrix") %>%
  products_unit_homogeneous() %>%
  extract2(".products_unit_homogeneous")
#> [1] TRUE TRUE TRUE TRUE
```
