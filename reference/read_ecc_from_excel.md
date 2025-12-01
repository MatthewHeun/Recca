# Read an energy conversion chain from a Excel file

Reads matrices from named regions in an Excel file into `matsindf`
format. The named regions are assumed to be global to the workbook.
Regions are named after their matrices. All region names are assumed to
have worksheet scope to avoid name collisions in the rest of the
workbook, as written by
[`write_ecc_to_excel()`](https://matthewheun.github.io/Recca/reference/write_ecc_to_excel.md).
The exception is the **R** matrix, which has the region name "R\_" by
default due to Excel's (undocumented) prohibition on naming regions "R"
(or "C").

## Usage

``` r
read_ecc_from_excel(
  path,
  worksheets = NULL,
  add_rc_types = TRUE,
  worksheet_names_colname = "WorksheetNames",
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  r_eiou = Recca::psut_cols$r_eiou,
  U_eiou = Recca::psut_cols$U_eiou,
  U_feed = Recca::psut_cols$U_feed,
  S_units = Recca::psut_cols$S_units,
  industry_type = Recca::row_col_types$industry_type,
  product_type = Recca::row_col_types$product_type,
  unit_type = Recca::row_col_types$unit_type,
  alt_R_region_name = "R_"
)
```

## Arguments

- path:

  The path to the Excel file.

- worksheets:

  Names of worksheets from which matrices are to be read. Default is
  `NULL`, meaning that all worksheets are read.

- add_rc_types:

  A boolean that tells whether to add row and column types to the
  outgoing matrices. When `TRUE`, matrix names are determined by
  [`add_row_col_types()`](https://matthewheun.github.io/Recca/reference/add_row_col_types.md).
  Default is `TRUE`.

- worksheet_names_colname:

  The name of a column in the outgoing data frame that contains the
  names of worksheets on which the matrices were found. Default is
  "WorksheetNames".

- R, U, V, Y, r_eiou, U_eiou, U_feed, S_units:

  String names for regions in the file at `path` containing matrices.
  See
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md)
  for defaults.

- industry_type, product_type, unit_type:

  String names of row and column types optionally applied to matrices
  read from the file at `path`. Defaults are taken from
  [`Recca::row_col_types`](https://matthewheun.github.io/Recca/reference/row_col_types.md).

- alt_R_region_name:

  An alternative name for R matrix regions to work around an
  undocumented behaviour of Excel in which the string "R" is rejected
  for region names. Default is "R\_".

## Value

A data frame in `matsindf` format containing the matrices from named
regions in the file at `path`.

## Details

Named regions are assumed to include the rectangle of numerical values,
row names (to the left of the rectangle of numbers), and column names
(above the rectangle of numbers), the same format as written by
[`write_ecc_to_excel()`](https://matthewheun.github.io/Recca/reference/write_ecc_to_excel.md).

This function is an inverse of
[`write_ecc_to_excel()`](https://matthewheun.github.io/Recca/reference/write_ecc_to_excel.md).

## See also

[`write_ecc_to_excel()`](https://matthewheun.github.io/Recca/reference/write_ecc_to_excel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  ecc <- UKEnergy2000mats |>
    tidyr::spread(key = "matrix.name", value = "matrix") |>
    dplyr::mutate(
      WorksheetNames = paste0(EnergyType, "_", LastStage)
    )
  ecc_temp_path <- tempfile(pattern = "write_excel_ecc_test_file",
                            fileext = ".xlsx")
  write_ecc_to_excel(ecc,
                     path = ecc_temp_path,
                     worksheet_names = "WorksheetNames",
                     overwrite = TRUE)
  # Now read the regions
  ecc_temp_path |>
    read_ecc_from_excel()
  if (file.exists(ecc_temp_path)) {
    res <- file.remove(ecc_temp_path)
  }
} # }
```
