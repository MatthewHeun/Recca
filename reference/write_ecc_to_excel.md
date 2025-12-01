# Write energy conversion chain matrices to an Excel file

It is often helpful to see energy conversion chain (ECC) matrices in
Excel format, arranged spatially. This function takes ECC matrices and
writes them to an Excel file.

## Usage

``` r
write_ecc_to_excel(
  .psut_data = NULL,
  path,
  overwrite_file = FALSE,
  worksheet_names = NULL,
  overwrite_worksheets = FALSE,
  pad = 2,
  include_named_regions = TRUE,
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  r_eiou = Recca::psut_cols$r_eiou,
  U_eiou = Recca::psut_cols$U_eiou,
  U_feed = Recca::psut_cols$U_feed,
  S_units = Recca::psut_cols$S_units,
  .wrote_mats_colname = "WroteMats",
  UV_bg_color = openxlsx2::wb_color(hex = "FDF2D0"),
  RY_bg_color = openxlsx2::wb_color(hex = "D3712D"),
  calculated_bg_color = openxlsx2::wb_color(hex = "D9D9D9"),
  alt_R_region_name = "R_"
)
```

## Arguments

- .psut_data:

  A list or data frame of energy conversion chains. Default is `NULL`,
  in which case single matrices can be supplied in the `R`, `U`, `V`,
  `Y`, `r_eiou`, `U_eiou`, `U_feed`, and `S_units` arguments.

- path:

  The path of the Excel file to be created.

- overwrite_file:

  A boolean that tells whether you want to overwrite the file at `path`,
  if it already exists. Default is `FALSE`.

- worksheet_names:

  A string or string vector identifying the names for the worksheets in
  the workbook. Alternatively, when `.psut_data` is a data frame, the
  string name of a column in the data frame containing the names of the
  worksheets. When `NULL`, the default, tabs are numbered sequentially.

- overwrite_worksheets:

  A boolean that tells whether to overwrite existing worksheets of the
  same name when `path` already exists.

- pad:

  The number of rows and columns between adjacent matrices in the Excel
  sheet. Default is `2`.

- include_named_regions:

  A boolean that tells whether to name regions of the Excel tabs
  according to matrices. Default is `TRUE`.

- R, U, U_feed, U_eiou, r_eiou, V, Y, S_units:

  Names of ECC matrices or actual matrices. See
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md)
  for defaults.

- .wrote_mats_colname:

  The name of the outgoing column that tells whether a worksheet was
  written successfully. Default is "WroteMats".

- UV_bg_color:

  The color of cells containing U and V matrices. Default is a creamy
  yellow.

- RY_bg_color:

  The color of cells containing R and Y matrices. Default is a rust
  color.

- calculated_bg_color:

  The color of cells containing calculated matrices. Default is gray.

- alt_R_region_name:

  An alternative name for R matrix regions to work around an
  undocumented behaviour of Excel in which the string "R" is rejected
  for region names. Default is "R\_".

## Value

The wbWorkbook object that was saved (the result of
[`openxlsx2::wb_save()`](https://janmarvin.github.io/openxlsx2/reference/wb_save.html)),
invisibly.

## Details

If `.psut_data` is a PSUT data frame, each row is written to a different
tab in the output file at `path`.

When `worksheet_names` is not `NULL` (the default), be sure that
worksheet names are unique. Also, be aware that worksheet names must
have 31 characters or fewer. Furthermore, the worksheet names may not
contain any of the following characters: `\ / ? * [ ]`.

When `include_named_regions` is `TRUE` (the default), named regions for
matrices are added to Excel sheets. The region names are the same of the
matrix names, and the regions have worksheet-scope. For example, "R" is
the name for the region of the **R** matrix on the sheet named "4". In
Excel, refer to that region with "4!R". The names help to identify
matrices in high-level views of an Excel sheet and can also be used for
later reading matrices from Excel files. (See
\[read_ecc_from_excel()\].) The region names apply to the rectangle of
numbers *and* the row and column names for the matrices, thereby
enabling \[read_ecc_from_excel()\] to easily load row and column names
for the matrices.

Note that region names are more restricted than worksheet names and may
not contain any of the following characters:
`! @ # $ % ^ & * ( ) + - / = { } [ ] | \ : ; " ' < > , . ? space`. Best
to stick with letters, numbers, and underscores.

A warning is given when any worksheet names or region names contain
illegal characters.

When `path` already exists, the worksheets are added to the file when
`overwrite_file` is `TRUE`. The file at `path` may have pre-existing
worksheets with the same names as worksheets to be written.
`overwrite_worksheets` controls whether the pre-existing worksheets will
be deleted before writing the new worksheets.

This function is an inverse of \[read_ecc_from_excel()\].

\[ \]: R:%20%20 \[read_ecc_from_excel()\]: R:read_ecc_from_excel()
\[read_ecc_from_excel()\]: R:read_ecc_from_excel() \[ \]: R:%20
\[read_ecc_from_excel()\]: R:read_ecc_from_excel()

## See also

[`read_ecc_from_excel()`](https://matthewheun.github.io/Recca/reference/read_ecc_from_excel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ecc <- UKEnergy2000mats %>%
  tidyr::pivot_wider(names_from = "matrix.name",
                     values_from = "matrix") |>
dplyr::mutate(
  # Specify worksheet names using metadata guaranteed to be unique.
  worksheet_names = paste(EnergyType, LastStage, sep = "_")
)
ecc_temp_path <- tempfile(pattern = "write_excel_ecc_test_file", fileext = ".xlsx")
write_ecc_to_excel(ecc,
                   path = ecc_temp_path,
                   worksheet_names = "worksheet_names",
                   overwrite = TRUE)
} # }
```
