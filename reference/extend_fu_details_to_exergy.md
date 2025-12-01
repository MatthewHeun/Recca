# Extend the final-to-useful details matrices from energy to exergy

The details matrices contain (in row and column names) details about the
move from the final energy stage to the useful energy stage. Four pieces
of information are provided in row and column names:

- final energy product,

- final demand sector,

- useful energy product, and

- final-to-useful machine.

## Usage

``` r
extend_fu_details_to_exergy(
  .fu_details_mats = NULL,
  Y_fu_details = Recca::psut_cols$Y_fu_details,
  U_eiou_fu_details = Recca::psut_cols$U_eiou_fu_details,
  clean_up_df = TRUE,
  phi = Recca::psut_cols$phi,
  .exergy_suffix = "_exergy",
  mat_piece = "noun",
  phi_piece = "all",
  energy_type = Recca::psut_cols$energy_type,
  mat_col_notation = RCLabels::from_notation,
  mat_colname_preposition =
    RCLabels::prepositions_list[[which(RCLabels::prepositions_list == "from")]],
  Y_fu_details_colname = Recca::psut_cols$Y_fu_details,
  U_eiou_fu_details_colname = Recca::psut_cols$U_eiou_fu_details,
  phi_colname = Recca::psut_cols$phi,
  energy = Recca::energy_types$e,
  exergy = Recca::energy_types$x,
  industry_type = IEATools::row_col_types$industry,
  product_type = IEATools::row_col_types$product
)
```

## Arguments

- .fu_details_mats:

  A data frame containing details matrices.

- Y_fu_details:

  The name of the column of `fu_details_mats` containing details
  matrices or a details matrix.

- U_eiou_fu_details:

  The name of the column of `fu_details_mats` containing details
  matrices or a details matrix.

- clean_up_df:

  When `.fu_details_mats` is a data frame, tells whether to
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  the result, remove no-longer-needed input column `phi`, and fill the
  `energy_type` column with "X" for the exergy versions of the ECC
  matrices. Default is `TRUE`.

- phi:

  The name of the column of `fu_details_mats` containing phi vectors or
  a phi vector.

- .exergy_suffix:

  The string suffix to be appended to exergy versions of ECC matrices.

- mat_piece:

  The piece of details matrix column names which are to be matched
  against names in the `phi` vector. Default is "noun", meaning that the
  part before " \[from XYZ\]" will be matched.

- phi_piece:

  The piece of names in the `phi` vector against which column names for
  the details matrices are to be matched. Default is "all", meaning that
  entire names are to be matched.

- energy_type:

  The name of the EnergyType column in `.fu_details_mats`. Default is
  `Recca::psut_cols$energy_type`.

- mat_col_notation:

  The notation for the column labels of the details matrices. Default is
  [`RCLabels::from_notation`](https://matthewheun.github.io/RCLabels/reference/from_notation.html).

- mat_colname_preposition:

  The prepositions to be used for details matrix column notation.
  Default is
  `RCLabels::prepositions_list[[which(RCLabels::prepositions_list == "from")]]`.

- Y_fu_details_colname:

  The name of the column in `.fu_details_mats` containing `Y_fu_details`
  matrices.

- U_eiou_fu_details_colname:

  The name of the column in `.fu_details_mats` containing
  `U_EIOU_fu_details` matrices.

- phi_colname:

  The name of the column in `.fu_details_mats` containing `phi` vectors.

- energy, exergy:

  String representing energy and exergy in the `energy_type` column.
  Defaults are `Recca::energy_types$e` and `Recca::energy_types$x`,
  respectively.

- industry_type, product_type:

  Industry and product row and column types. Defaults are
  `IEATools::row_col_types$industry` and
  `IEATools::row_col_types$product`, respectively.

## Value

A version of `.fu_details_mats` containing details matrices in exergy
terms.

## Details

Two details matrices are available:

- `Y_fu_details` and

- `U_EIOU_fu_details`.

The two matrices correspond to the two ways in which final energy is
converted into useful energy: in final demand (`Y_fu_details`) and in
energy industry own use (`U_EIOU_fu_details`).

The format for the row and column names for both details matrices is
identical:

- row names

  - [`RCLabels::arrow_notation`](https://matthewheun.github.io/RCLabels/reference/arrow_notation.html)

  - prefix: final energy product

  - suffix: final demand sector

  - example: "Aviation gasoline -\> Domestic aviation"

- column names

  - [`RCLabels::from_notation`](https://matthewheun.github.io/RCLabels/reference/from_notation.html)

  - noun: useful energy product

  - object of from: final-to-useful machine

  - example: "HPL \[from Electric pumps\]"

The row and column types match the row and column names.

- row types

  - [`RCLabels::arrow_notation`](https://matthewheun.github.io/RCLabels/reference/arrow_notation.html)

  - prefix: "Product"

  - suffix: "Industry"

  - specifically: "Product -\> Industry"

- column types

  - [`RCLabels::from_notation`](https://matthewheun.github.io/RCLabels/reference/from_notation.html)

  - noun: "Product"

  - object of from: "Industry"

  - specifically: "Product \[from Industry\]"

The energy stage of the entries in the details matrices are indicated by
the entry in the `EnergyType` column, typically "Useful".

If either of the energy details matrices are `NULL`, the exergy matrix
returned from this function is also `NULL`.

## Examples

``` r
details_mat <- Matrix::sparseMatrix(
  i = c(1, 2, 3),
  j = c(1, 3, 2),
  x = c(10, 20, 100),
  dimnames = list(c("Electricity -> Households",
                    "Electricity -> Industry",
                    "Natural gas -> Households"),
                  c("Light [from Electric lamps]",
                    "MTH.100.C [from Furnaces]",
                    "KE [from Fans]"))) |>
  matsbyname::setrowtype("Product -> Industry") |>
  matsbyname::setcoltype("Product [from Industry]")
phi_vec <- Matrix::sparseMatrix(
  i = c(1, 2, 3, 4),
  j = c(1, 1, 1, 1),
  x = c(1.0, 1-(25+273.15)/(100+273.15), 0.96, 1-(25+273.15)/(1000+273.15)),
  dimnames = list(c("KE", "MTH.100.C", "Light", "HTH.1000.C"),
                  "phi")) |>
  matsbyname::setrowtype("Product") |>
  matsbyname::setcoltype("phi")
extend_fu_details_to_exergy(Y_fu_details = details_mat,
                            U_eiou_fu_details = details_mat,
                            phi = phi_vec)
#> $Y_fu_details_exergy
#> 3 x 3 sparse Matrix of class "dgCMatrix"
#>                           KE [from Fans] Light [from Electric lamps]
#> Electricity -> Households              .                         9.6
#> Electricity -> Industry               20                         .  
#> Natural gas -> Households              .                         .  
#>                           MTH.100.C [from Furnaces]
#> Electricity -> Households                   .      
#> Electricity -> Industry                     .      
#> Natural gas -> Households                  20.09916
#> 
#> $U_EIOU_fu_details_exergy
#> 3 x 3 sparse Matrix of class "dgCMatrix"
#>                           KE [from Fans] Light [from Electric lamps]
#> Electricity -> Households              .                         9.6
#> Electricity -> Industry               20                         .  
#> Natural gas -> Households              .                         .  
#>                           MTH.100.C [from Furnaces]
#> Electricity -> Households                   .      
#> Electricity -> Industry                     .      
#> Natural gas -> Households                  20.09916
#> 
```
