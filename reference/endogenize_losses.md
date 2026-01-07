# Endogenize losses into PSUT matrices

When a conversion chain does *not* include losses in the **RUVY**
matrices of the PSUT framework, it may be helpful to endogenize the
losses. This function performs the endogenization.

## Usage

``` r
endogenize_losses(
  .sutmats = NULL,
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  intra_industry_balance = Recca::balance_cols$intra_industry_balance_colname,
  losses_alloc = Recca::balance_cols$losses_alloc_colname,
  loss_sector = Recca::balance_cols$losses_sector,
  replace_cols = FALSE,
  clean = FALSE,
  tol = 1e-06,
  V_prime = "V_prime",
  Y_prime = "Y_prime"
)
```

## Arguments

- .sutmats:

  A `matsindf` data frame, wide by matrices, or a list of lists of
  matrices. Default is `NULL`.

- R:

  Resources (**R**) matrix or name of the column in `.sutmats` that
  contains same. Default is "R".

- U:

  Use (**U**) matrix or name of the column in `.sutmats` that contains
  same. Necessary for verifying calculating losses. Default is "U".

- V:

  Make (**V**) matrix or name of the column in `.sutmats` that contains
  same. Default is "V".

- Y:

  Final demand (**Y**) matrix or name of the column in `.sutmats` that
  contains same. Default is "Y".

- intra_industry_balance:

  A vector or the name of the column containing intra-industry balance
  vectors. If missing, losses are calculated internally with
  [`calc_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md)
  before endogenizing. Default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$intra_industry_balance_colname`
  or "SUTIntraIndustryBalance".

- losses_alloc:

  A matrix or the name of the column containing loss allocation
  matrices. See details for structure of this matrix. Default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$losses_alloc_colname`
  or "LossesAlloc".

- loss_sector:

  The string name of the sector that will absorb losses in the **Y**
  matrix. Default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$losses_sector`
  or "Transformation losses".

- replace_cols:

  A boolean that tells whether to (a) replace the `V` and `Y` columns
  with `V_prime` and `Y_prime` columns, respectively and (b) delete the
  `V_prime`, `Y_prime`, `balance_colname`, and `losses_alloc_colname`
  columns after endogenizing the losses when `.sutmats` is a data frame
  or a list. Default is `FALSE`.

- clean:

  A boolean that tells whether the outgoing `V_prime` and `Y_prime`
  matrices should have `0` rows and columns removed. Default is `FALSE`.

- tol:

  The maximum allowable difference from `1` for the rowsums of loss
  allocation matrices. Default is `1e-6`.

- V_prime:

  The name of the **V** matrix with endogenized losses.

- Y_prime:

  The name of the **Y** matrix with endogenized losses.

## Value

A version of the conversion chain with losses endogenized.

## Details

This function endogenizes losses into the **V** and **Y** matrices,
because losses are made (**V**) by industries and gathered by final
demand (**Y**). By default, this function creates new `V_prime` and
`Y_prime` matrices. Setting `replace_cols = TRUE` (default is `FALSE`)
replaces the existing `V` and `Y` matrices with `V_prime` and `Y_prime`,
respectively.

All losses are allocated to the `loss_sector` column in the **Y**
matrix, by default named
[balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$losses_sector or "`r
Recca::balance_cols\$losses_sector\`".

### Endogenizing algorithm

The endogenizing algorithm is this:

- If not present, calculate the balance vector with
  [`calc_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md).

- Hatize the balance vector with
  [`matsbyname::hatize_byname()`](https://matthewheun.github.io/matsbyname/reference/hatize_byname.html).

- Matrix multiply the hatized balance vector and the losses_allocation
  matrix (`losses_alloc`) to obtain a matrix to be added to **V**.

- Transpose the matrix to be added to **V**, calculate rowsums, and set
  the column name to `loss_sector` to obtain a matrix to be added to
  **Y**.

- Add the matrices to **V** and **Y**, respectively.

### Losses allocation matrix

The losses allocation matrix (`losses_alloc`) tells how losses from each
industry should be allocated to products. The losses allocation matrix
should have industries in rows and products (losses) in rows. The values
in the losses allocation matrix are fractions of each industry's losses
(in rows) that are allocated to loss products (in columns). Rows of
losses allocation matrix must sum to `1`. The value of `losses_alloc`
must resolve to a matrix of this form. Options include:

- The default value, namely
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$default_losses_alloc`,
  a 1x1 matrix with a row named "All industries", a column named "Waste
  heat", and a value of `1`. This default matrix ascribes losses from
  all industries in the **V** matrix to a product called "Waste heat".

- A matrix of the same sense (industries in rows, waste products in
  columns, rows sum to 1.0) to be applied to all rows in the
  wide-by-matrices data frame supplied in `.sutmats`. All industries in
  the **V** matrix must be present in the rows of `losses_alloc`. If the
  matrix has a single row (as the default,
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$default_losses_alloc`),
  it is assumed to apply to all industries.

- The string name of a column in `.sutmats` that contains loss
  allocation matrices for every row in `.sutmats`. All industries in the
  **V** matrix must be present in the rows of the matrices in the
  `losses_alloc` column. If any of the matrices in the column names
  `losses_alloc` has a single row, the row is assumed to apply to all
  industries.

### Intra-industry balances

The intra-industry losses to be endogenized are found in
`intra_industry_balance` and can be calculated with
[`calc_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md).
If `intra_industry_balance` is not present, it is calculated internally
via
[`calc_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md).

### Cleaning

When this function operates on a conversion chain with already-balanced
industries, the matrices to be added to **V** and **Y** will be the
**0** matrix to within `tol`. Setting `clean = TRUE` removes those rows
or columns from the output `V_prime` and `Y_prime` matrices.

### Checks

Prior to performing any calculations,
[`verify_inter_industry_balance()`](https://matthewheun.github.io/Recca/reference/verify-balances.md)
is checked. There is no point endogenizing losses for a conversion chain
that is not internally consistent.

After endogenizing the losses, all industries in the conversion chain
should pass
[`verify_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/verify-balances.md),
a condition that is checked before returning.

## Examples

``` r
mats <- UKEnergy2000mats |>
  tidyr::pivot_wider(names_from = matrix.name,
                     values_from = matrix) |>
  dplyr::filter(.data[[IEATools::iea_cols$last_stage]] %in%
                  c(IEATools::last_stages$final, IEATools::last_stages$useful)) |>
  dplyr::mutate(
    # Add a matrix column of loss allocations.
    # This bit of code adds a default loss allocation matrix
    # to every row of the data frame.
    "{Recca::balance_cols$losses_alloc_colname}" :=
      RCLabels::make_list(Recca::balance_cols$default_losses_alloc,
                          n = dplyr::n(),
                          lenx = 1)
    )
dplyr::glimpse(mats)
#> Rows: 2
#> Columns: 13
#> $ Country     <chr> "GBR", "GBR"
#> $ Year        <dbl> 2000, 2000
#> $ EnergyType  <chr> "E", "E"
#> $ LastStage   <chr> "Final", "Useful"
#> $ R           <list> <<matrix[2 x 2]>>, <<matrix[2 x 2]>>
#> $ U           <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ U_EIOU      <list> <<matrix[7 x 8]>>, <<matrix[3 x 8]>>
#> $ U_feed      <list> <<matrix[9 x 9]>>, <<matrix[12 x 13]>>
#> $ V           <list> <<matrix[9 x 10]>>, <<matrix[13 x 14]>>
#> $ Y           <list> <<matrix[4 x 2]>>, <<matrix[4 x 2]>>
#> $ r_EIOU      <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ S_units     <list> <<matrix[12 x 1]>>, <<matrix[16 x 1]>>
#> $ LossesAlloc <list> <matrix[1 x 1]>, <matrix[1 x 1]>
mats |>
  calc_intra_industry_balance() |>
  endogenize_losses() |>
  dplyr::glimpse()
#> Rows: 2
#> Columns: 16
#> $ Country                 <chr> "GBR", "GBR"
#> $ Year                    <dbl> 2000, 2000
#> $ EnergyType              <chr> "E", "E"
#> $ LastStage               <chr> "Final", "Useful"
#> $ R                       <list> <<matrix[2 x 2]>>, <<matrix[2 x 2]>>
#> $ U                       <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ U_EIOU                  <list> <<matrix[7 x 8]>>, <<matrix[3 x 8]>>
#> $ U_feed                  <list> <<matrix[9 x 9]>>, <<matrix[12 x 13]>>
#> $ V                       <list> <<matrix[9 x 10]>>, <<matrix[13 x 14]>>
#> $ Y                       <list> <<matrix[4 x 2]>>, <<matrix[4 x 2]>>
#> $ r_EIOU                  <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ S_units                 <list> <<matrix[12 x 1]>>, <<matrix[16 x 1]>>
#> $ LossesAlloc             <list> <matrix[1 x 1]>, <matrix[1 x 1]>
#> $ SUTIntraIndustryBalance <list> <<matrix[9 x 1]>>, <<matrix[13 x 1]>>
#> $ V_prime                 <list> <<matrix[9 x 11]>>, <<matrix[13 x 15]>>
#> $ Y_prime                 <list> <<matrix[5 x 3]>>, <<matrix[5 x 3]>>
# Replace original matrices with endogenized matrices
mats |>
  calc_intra_industry_balance() |>
  endogenize_losses(replace_cols = TRUE) |>
  # Check the intra-industry balance.
  # Everything should be balanced now.
  calc_intra_industry_balance() |>
  verify_intra_industry_balance() |>
  dplyr::glimpse()
#> Rows: 2
#> Columns: 14
#> $ Country                  <chr> "GBR", "GBR"
#> $ Year                     <dbl> 2000, 2000
#> $ EnergyType               <chr> "E", "E"
#> $ LastStage                <chr> "Final", "Useful"
#> $ R                        <list> <<matrix[2 x 2]>>, <<matrix[2 x 2]>>
#> $ U                        <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ U_EIOU                   <list> <<matrix[7 x 8]>>, <<matrix[3 x 8]>>
#> $ U_feed                   <list> <<matrix[9 x 9]>>, <<matrix[12 x 13]>>
#> $ V                        <list> <<matrix[9 x 11]>>, <<matrix[13 x 15]>>
#> $ Y                        <list> <<matrix[5 x 3]>>, <<matrix[5 x 3]>>
#> $ r_EIOU                   <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ S_units                  <list> <<matrix[12 x 1]>>, <<matrix[16 x 1]>>
#> $ SUTIntraIndustryBalance  <list> <<matrix[9 x 1]>>, <<matrix[13 x 1]>>
#> $ SUTIntraIndustryBalanced <lgl> TRUE, TRUE
```
