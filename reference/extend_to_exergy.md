# Extend an ECC in PSUT format from energy to exergy

A conversion chain can be represented in energy or mass terms (conserved
quantities) or in exergy terms (a non-conserved quantity). This function
moves from an energy or mass quantification to an exergy quantification,
given the matrices for the mass/energy quantification and `phi`
(exergy-to-energy ratio) vectors. Optionally, this function calculates
and endogenizes losses of the conserved quantity and irreversibility
(exergy destruction) using the arguments `losses_alloc`,
`losses_sector`, `irrev_alloc`, and `irrev_sector`.

## Usage

``` r
extend_to_exergy(
  .sutmats = NULL,
  clean_up_df = TRUE,
  endogenize_losses_irrev = FALSE,
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  U_feed = Recca::psut_cols$U_feed,
  U_eiou = Recca::psut_cols$U_eiou,
  r_eiou = Recca::psut_cols$r_eiou,
  losses_alloc = Recca::balance_cols$losses_alloc_colname,
  losses_sector = Recca::balance_cols$losses_sector,
  intra_industry_balance = Recca::balance_cols$intra_industry_balance_colname,
  phi = Recca::psut_cols$phi,
  irrev_alloc = Recca::balance_cols$irrev_alloc_colname,
  irrev_sector = Recca::balance_cols$irrev_sector,
  clean_mats = FALSE,
  mat_piece = "all",
  phi_piece = "all",
  notation = RCLabels::bracket_notation,
  prepositions = RCLabels::prepositions_list,
  energy_type = Recca::psut_cols$energy_type,
  exergy = Recca::energy_types$x,
  tol = 1e-06,
  .exergy_suffix = "_exergy",
  R_exergy = paste0(Recca::psut_cols$R, .exergy_suffix),
  U_exergy = paste0(Recca::psut_cols$U, .exergy_suffix),
  V_exergy = paste0(Recca::psut_cols$V, .exergy_suffix),
  Y_exergy = paste0(Recca::psut_cols$Y, .exergy_suffix),
  U_feed_exergy = paste0(Recca::psut_cols$U_feed, .exergy_suffix),
  U_eiou_exergy = paste0(Recca::psut_cols$U_eiou, .exergy_suffix),
  r_eiou_exergy = paste0(Recca::psut_cols$r_eiou, .exergy_suffix)
)
```

## Arguments

- .sutmats:

  An optional data frame of mass or energy (conserved quantities)
  conversion chains in wide-by-matrix format.

- clean_up_df:

  When `.sutmats` is a data frame, tells whether to
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  the result, remove no-longer-needed input columns `phi`,
  `losses_alloc`, and `irrev_alloc`, and fill the `energy_type` column
  with the value of the `exergy` argument for the exergy versions of the
  ECC matrices. Default is `TRUE`.

- endogenize_losses_irrev:

  A boolean that tells whether to endogenize losses of the conserved
  quantity and calculate irreversibility (exergy destruction) of the
  industries in the exergy version of the conversion chain. Default is
  `FALSE`.

- R:

  Resources (**R**) matrix or name of the column in `.sutmats` that
  contains same. Default is
  [psut_cols](https://matthewheun.github.io/Recca/reference/psut_cols.md)`$R`
  or "R".

- U:

  Use (**U**) matrix or name of the column in `.sutmats` that contains
  same. Necessary for verifying calculating losses. Default is
  [psut_cols](https://matthewheun.github.io/Recca/reference/psut_cols.md)`$U`
  or "U".

- V:

  Make (**V**) matrix or name of the column in `.sutmats` that contains
  same. Default is
  [psut_cols](https://matthewheun.github.io/Recca/reference/psut_cols.md)`$V`
  or "V".

- Y:

  Final demand (**Y**) matrix or name of the column in `.sutmats` that
  contains same. Default is
  [psut_cols](https://matthewheun.github.io/Recca/reference/psut_cols.md)`$Y`
  or "Y".

- U_feed:

  Feedstock use (**U_feed**) matrix or name of the column in `.sutmats`
  that contains same. Default is
  [psut_cols](https://matthewheun.github.io/Recca/reference/psut_cols.md)`$U_feed`
  or "U_feed".

- U_eiou:

  Energy industry own use (**U_EIOU**) matrix or name of the column in
  `.sutmats` that contains same. Default is
  [psut_cols](https://matthewheun.github.io/Recca/reference/psut_cols.md)`$U_eiou`
  or "U_EIOU".

- r_eiou:

  A matrix of the ratio of energy industry own use to total use for the
  use (**U**) matrix or name of the column in `.sutmats` that contains
  same. Default is
  [psut_cols](https://matthewheun.github.io/Recca/reference/psut_cols.md)`$r_eiou`
  or "r_EIOU".

- losses_alloc:

  A matrix or the name of the column containing loss allocation
  matrices. See details for structure of this matrix. Required only if
  `endogenize_losses_irrev` is `TRUE`. Default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$losses_alloc_colname`
  or "LossesAlloc".

- losses_sector:

  The string name of the sector that will absorb losses in the **Y**
  matrix for the conserved quantity. Required only if
  `endogenize_losses_irrev` is `TRUE`. Default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$losses_sector`
  or "Transformation losses".

- intra_industry_balance:

  A vector or the name of the column containing intra-industry balance
  vectors for the conserved quantity. If missing, losses are calculated
  internally with
  [`calc_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md)
  before endogenizing. Required only if `endogenize_losses_irrev` is
  `TRUE`. Default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$intra_industry_balance_colname`
  or "SUTIntraIndustryBalance".

- phi:

  A vector of exergy-to-energy ratios (phi) or the string name of a
  column in `.sutmats` containing same. Default is
  [psut_cols](https://matthewheun.github.io/Recca/reference/psut_cols.md)`$phi`
  or "phi".

- irrev_alloc:

  An irreversibility allocation matrix or the name of a column in
  `.sutmats` containing same. Required only if `endogenize_losses_irrev`
  is `TRUE`. Default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$irrev_alloc_colname`
  or "IrrevAlloc".

- irrev_sector:

  The string name of the sector that will absorb irreversibilities
  (exergy destruction) in the **Y** matrix. Required only if
  `endogenize_losses_irrev` is `TRUE`. Default is
  [balance_cols](https://matthewheun.github.io/Recca/reference/balance_cols.md)`$irrev_sector`
  or "Irreversibilities".

- clean_mats:

  A boolean that tells whether the outgoing **V** and **Y** matrices
  with endogenized losses and irreversibilities should have `0` rows and
  columns removed. Affects results only when `endogenize_irrev_losses`
  is `TRUE`. Default is `FALSE`.

- mat_piece:

  The piece of row and column names for matrices `R`, `U`, `V`, `Y`,
  `U_feed`, and `U_EIOU` against which row names of the `phi` vector is
  to be matched. Default is "all", meaning that entire names are to be
  matched.

- phi_piece:

  The piece of row names in the `phi` vector against which row and
  column names for matrices `R`, `U`, `V`, `Y`, `U_feed`, and `U_EIOU`
  is to be matched. Default is "all", meaning that entire names are to
  be matched.

- notation:

  The nomenclature for the row and column labels. Default is
  [`RCLabels::bracket_notation`](https://matthewheun.github.io/RCLabels/reference/bracket_notation.html).

- prepositions:

  The prepositions to be used row and column notation. Default is
  [`RCLabels::prepositions_list`](https://matthewheun.github.io/RCLabels/reference/prepositions_list.html).

- exergy:

  A string flow type to be given for the results. See
  [energy_types](https://matthewheun.github.io/Recca/reference/energy_types.md).
  Default is
  [energy_types](https://matthewheun.github.io/Recca/reference/energy_types.md)`$x`
  or "X".

- tol:

  The maximum allowable difference from `1` for the rowsums of loss
  allocation matrices. Also the maximum allowable different from `0` for
  the inter-industry balances and intra-industry balances. Default is
  `1e-6`.

- .exergy_suffix:

  The string suffix to be appended to exergy versions of ECC matrices.

- R_exergy, U_exergy, U_feed_exergy, U_eiou_exergy, r_eiou_exergy,
  V_exergy, Y_exergy, energy_type:

  Names of output matrices. Defaults are matrix names with
  `.exergy_suffix` appended.

## Value

A data frame or list of matrices that represents the exergy version of
the ECC.

## Details

Internally, this function uses `matsindf::apply()`, so the PSUT matrices
can be provided as individual matrices, in a named list (passed in
`.sutmats`), or or in a data frame (also passed to `.sutmats`), in which
case the arguments should give the string names in the list or of
columns in the `.sutmats` data frame. The default is strings for most
arguments, thereby assuming `.sutmats` will be supplied with a list or
data frame.

The vector `phi` is considered to be a store of values to be applied to
each type of energy carrier. To determine which entry in the `phi`
vector is matched against which energy carrier, `mat_piece` and
`phi_piece` are consulted. `mat_piece` and `phi_piece` can be any of
"all", "pref", "suff", "noun", or one of many prepositions. Consult the
`RCLabels` package for details.

The input should be a conversion chain that contains conserved
quantities (probably mass or energy). Optionally, the conversion chain
may already have endogenized transformation losses (wastes). The output
contains a new version of the conversion chain in exergy terms with
(optionally) all transformation losses endogenized and exergy
destruction (irreversibility) calculated.

Between input and output, the following steps are taken:

1.  Optionally, if `endogenize_losses_irrev` is `TRUE`, transformation
    losses of the conserved quantity are calculated and endogenized into
    the **V** and **Y** matrices via a call to
    [`endogenize_losses()`](https://matthewheun.github.io/Recca/reference/endogenize_losses.md).
    If `intra_industry_balance` is `NULL`, losses of the conserved
    quantity are calculated within
    [`endogenize_losses()`](https://matthewheun.github.io/Recca/reference/endogenize_losses.md)
    via a call to
    [`calc_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md).
    If the input already has endogenized losses, there will be no change
    to the incoming conversion chain. Set `clean_mats` to `TRUE` to
    eliminate new `0` balance columns. The `losses_alloc` and
    `losses_sector` arguments are passed to
    [`endogenize_losses()`](https://matthewheun.github.io/Recca/reference/endogenize_losses.md).

2.  All matrices of the conversion chain are extended to exergy by pre-
    or post-multiplying the diagonalized `phi` vector into the PSUT
    matrices. Transformation losses calculated previously (in Step 1.)
    become exergy losses, i.e., the exergy of transformation losses.

3.  Optionally, if `endogenize_losses_irrev` is `TRUE`, transformation
    losses of the (non-conserved) exergy flows are endogenized into the
    **V** and **Y** matrices via another call to
    [`endogenize_losses()`](https://matthewheun.github.io/Recca/reference/endogenize_losses.md).
    These transformation losses are interpreted as exergy destruction
    (irreversibility). The `irrev_alloc` and `irrev_sector` arguments
    are passed to
    [`endogenize_losses()`](https://matthewheun.github.io/Recca/reference/endogenize_losses.md).

Throughout the function, inter-industry balances are verified via
[`verify_inter_industry_balance()`](https://matthewheun.github.io/Recca/reference/verify-balances.md).
After Step 1., across-industry balances are verified via
[`verify_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/verify-balances.md).

The new version of the conversion chain is contained in named arguments
(or columns if `.sutmats` is a data frame) named with the suffix
`.exergy_suffix`, by default "\_exergy". If `.sutmats` is a data frame,
a tidy data frame can be output with an `energy_type` column by setting
`clean_up_df = TRUE` (the default). The `energy_type` column is filled
with `exergy`, by default
[energy_types](https://matthewheun.github.io/Recca/reference/energy_types.md)`$x`
or "X".

## Examples

``` r
sutmats <- UKEnergy2000mats |>
  # Put in wide-by-matrix format.
  tidyr::pivot_wider(names_from = matrix.name,
                     values_from = matrix) |>
  # Eliminate services ECCs.
  dplyr::filter(LastStage %in% c("Final", "Useful"))
# Do not calculate losses or irreversibilities (exergy destruction)
sutmats |>
  dplyr::mutate(
    phi = RCLabels::make_list(Recca::phi_vec, n = dplyr::n(), lenx = 1)
  ) |>
  extend_to_exergy()
#> # A tibble: 4 × 12
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 X          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 4 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>
# Repeat, this time calculating losses and irreversibilities
# Create the losses allocation matrix
losses_alloc_mat <- matrix(1,
  dimnames = list("All industries",
                  "MTH.200.C -> Transformation losses")) |>
  matsbyname::setrowtype("Industry") |>
  matsbyname::setcoltype("Product")
losses_alloc_mat
#>                MTH.200.C -> Transformation losses
#> All industries                                  1
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
# Create a phi vector
phi_vec <- matrix(c(1.06, 1.04,    # Crude, NG
                    1.06, 1, 1.06, # Diesel, Elect, Petrol
                    1,             # MD
                    0.143616257,   # LTH (assumed 50 C)
                    0.956,         # Light
                    0.36986157),   # MTH.200.C
                  dimnames = list(c("Crude", "NG",
                                    "Diesel", "Elect", "Petrol",
                                    "MD",
                                    "LTH",
                                    "Light",
                                    "MTH.200.C"),
                                  "phi")) |>
  matsbyname::setrowtype("Product") |>
  matsbyname::setcoltype("phi")
phi_vec
#>                 phi
#> Crude     1.0600000
#> NG        1.0400000
#> Diesel    1.0600000
#> Elect     1.0000000
#> Petrol    1.0600000
#> MD        1.0000000
#> LTH       0.1436163
#> Light     0.9560000
#> MTH.200.C 0.3698616
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "phi"
sutmats |>
  dplyr::mutate(
    "{Recca::balance_cols$losses_alloc_colname}" :=
      RCLabels::make_list(x = losses_alloc_mat, n = 2,lenx = 1),
    "{Recca::psut_cols$phi}" :=
      RCLabels::make_list(x = phi_vec, n = 2, lenx = 1),
    "{Recca::balance_cols$irrev_alloc_colname}" :=
      RCLabels::make_list(x = Recca::balance_cols$default_destruction_alloc_mat,
                          n = 2, lenx = 1)) |>
  extend_to_exergy(endogenize_losses_irrev = TRUE,
                   clean_mats = TRUE,
                   mat_piece = "noun",
                   notation = list(RCLabels::from_notation,
                                   RCLabels::arrow_notation))
#> # A tibble: 4 × 12
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 X          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 4 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>
```
