# Chop the **R** and **Y** matrices and swim downstream/upstream

Chopping the resource (**R**) or final demand (**Y**) matrices involves
isolating products and industries then swimming downstream/upstream to
identify an energy conversion chain (ECC) associated with each resource
or final demand category. These functions perform those calculations.

## Usage

``` r
chop_Y(
  .sut_data = NULL,
  calc_pfd_aggs = TRUE,
  p_industries = NULL,
  fd_sectors = NULL,
  piece = "all",
  notation = RCLabels::notations_list,
  pattern_type = c("exact", "leading", "trailing", "anywhere", "literal"),
  prepositions = RCLabels::prepositions_list,
  unnest = FALSE,
  method = c("solve", "QR", "SVD"),
  tol_invert = .Machine$double.eps,
  tol_chop_sum = 1e-04,
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  U_feed = Recca::psut_cols$U_feed,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  S_units = Recca::psut_cols$S_units,
  chop_df = Recca::aggregate_cols$chop_df,
  product_sector = Recca::aggregate_cols$product_sector,
  aggregate_primary = Recca::aggregate_cols$aggregate_primary,
  net_aggregate_demand = Recca::aggregate_cols$net_aggregate_demand,
  gross_aggregate_demand = Recca::aggregate_cols$gross_aggregate_demand,
  .prime = "_prime",
  R_colname = Recca::psut_cols$R,
  U_colname = Recca::psut_cols$U,
  U_feed_colname = Recca::psut_cols$U_feed,
  U_eiou_colname = Recca::psut_cols$U_eiou,
  r_eiou_colname = Recca::psut_cols$r_eiou,
  V_colname = Recca::psut_cols$V,
  Y_colname = Recca::psut_cols$Y,
  S_units_colname = Recca::psut_cols$S_units,
  R_prime_colname = paste0(R_colname, .prime),
  U_prime_colname = paste0(U_colname, .prime),
  U_feed_prime_colname = paste0(U_feed_colname, .prime),
  U_eiou_prime_colname = paste0(U_eiou_colname, .prime),
  r_eiou_prime_colname = paste0(r_eiou_colname, .prime),
  V_prime_colname = paste0(V_colname, .prime),
  Y_prime_colname = paste0(Y_colname, .prime),
  S_units_prime_colname = paste0(S_units_colname, .prime)
)

chop_R(
  .sut_data = NULL,
  calc_pfd_aggs = TRUE,
  p_industries = NULL,
  fd_sectors = NULL,
  piece = "all",
  notation = RCLabels::notations_list,
  pattern_type = c("exact", "leading", "trailing", "anywhere", "literal"),
  prepositions = RCLabels::prepositions_list,
  unnest = FALSE,
  method = c("solve", "QR", "SVD"),
  tol_invert = .Machine$double.eps,
  tol_chop_sum = 1e-04,
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  U_feed = Recca::psut_cols$U_feed,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  S_units = Recca::psut_cols$S_units,
  chop_df = Recca::aggregate_cols$chop_df,
  product_sector = Recca::aggregate_cols$product_sector,
  aggregate_primary = Recca::aggregate_cols$aggregate_primary,
  net_aggregate_demand = Recca::aggregate_cols$net_aggregate_demand,
  gross_aggregate_demand = Recca::aggregate_cols$gross_aggregate_demand,
  .prime = "_prime",
  R_colname = Recca::psut_cols$R,
  U_colname = Recca::psut_cols$U,
  U_feed_colname = Recca::psut_cols$U_feed,
  U_eiou_colname = Recca::psut_cols$U_eiou,
  r_eiou_colname = Recca::psut_cols$r_eiou,
  V_colname = Recca::psut_cols$V,
  Y_colname = Recca::psut_cols$Y,
  S_units_colname = Recca::psut_cols$S_units,
  R_prime_colname = paste0(R_colname, .prime),
  U_prime_colname = paste0(U_colname, .prime),
  U_feed_prime_colname = paste0(U_feed_colname, .prime),
  U_eiou_prime_colname = paste0(U_eiou_colname, .prime),
  r_eiou_prime_colname = paste0(r_eiou_colname, .prime),
  V_prime_colname = paste0(V_colname, .prime),
  Y_prime_colname = paste0(Y_colname, .prime),
  S_units_prime_colname = paste0(S_units_colname, .prime)
)
```

## Arguments

- .sut_data:

  A data frame or list of physical supply-use table matrices. Default is
  `NULL`.

- calc_pfd_aggs:

  A boolean that tells whether (`TRUE`) or not (`FALSE`) to include
  primary and final demand aggregates to the nested data frame.

- p_industries:

  A vector of names of industries to be aggregated as "primary" and used
  if aggregations are requested. If `.sut_data` is a data frame,
  `p_industries` should be the name of a column in the data frame. If
  `.sut_data` is `NULL`, `p_industries` can be a single vector of
  industry names. These industries in `p_industries` will appear in rows
  of the resource (**R**) and make (**V**) matrices and columns of the
  final demand matrix (**Y**). Entries in **Y_p** will be subtracted
  from entries in **R_p** `+` **V_p** to obtain the total primary energy
  aggregate, where `*_p` is the primary part of those matrices. The
  function
  [`find_p_industry_names()`](https://matthewheun.github.io/Recca/reference/find_p_industry_names.md)
  might be helpful to find primary industry names if they can be
  identified by prefixes. This argument is passed to
  [`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md).
  Default is `NULL`.

- fd_sectors:

  A vector of names of sectors in final demand and used if aggregations
  are requested. Names should include columns in the **Y** and
  **U_EIOU** matrices to cover both net (in **Y**) and gross (in **Y**
  and **U_EIOU**) final demand. This argument is passed to
  [`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md).
  Default is `NULL`.

- piece, notation, pattern_type, prepositions:

  Arguments passed to both
  [`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md)
  and
  [`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md)
  and, ultimately, to
  [`matsbyname::select_rowcol_piece_byname()`](https://matthewheun.github.io/matsbyname/reference/select_rowcol_piece_byname.html)
  for the purpose of selecting rows and columns for primary and final
  demand aggregations. See
  [`matsbyname::select_rowcol_piece_byname()`](https://matthewheun.github.io/matsbyname/reference/select_rowcol_piece_byname.html)
  for details.

- unnest:

  A boolean that tells whether to unnest the outgoing data. When `TRUE`,
  creates a new column called `product_sector` and columns of primary
  and final demand aggregates. Default is `FALSE`.

- method:

  One of "solve", "QR", or "SVD". Default is "solve". See details.

- tol_invert:

  The tolerance for detecting linear dependencies in the columns
  inverted matrices. Default is `.Machine$double.eps`.

- tol_chop_sum:

  The allowable deviation from `0` for the difference between the sum of
  the chopped ECCs and the original ECC. Default is `1e-4`.

- R, U, U_feed, V, Y, S_units:

  Matrices that describe the energy conversion chain (ECC). See
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md)
  for default values.

- chop_df, aggregate_primary, net_aggregate_demand,
  gross_aggregate_demand:

  Names of output columns. See
  [`Recca::aggregate_cols`](https://matthewheun.github.io/Recca/reference/aggregate_cols.md).

- product_sector:

  The name of the output column that contains the product, industry, or
  sector for which footprint aggregates are given. Default is
  `Recca::aggregate_cols$product_sector`.

- .prime:

  A string that denotes new matrices. This string is used as a suffix
  that is appended to many variable names. Default is "\_prime".

- R_colname, U_colname, U_feed_colname, U_eiou_colname, r_eiou_colname,
  V_colname, Y_colname, S_units_colname:

  Names of input matrices in `.sut_data`. See
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md)
  for default values.

- R_prime_colname, U_prime_colname, U_feed_prime_colname,
  U_eiou_prime_colname, r_eiou_prime_colname, V_prime_colname,
  Y_prime_colname, S_units_prime_colname:

  Names of output matrices in the return value. Default values are
  constructed from
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md)
  values suffixed with the value of the `.prime` argument.

## Value

Chopped **R** and **Y** energy conversion chains with optional primary
and final demand aggregates.

## Details

Chopping **R** involves calculating an ECC for each industry row and
each product column in the **R** matrix. This calculation is
accomplished for each description of an energy conversion chain (ECC) by
the following algorithm:

1.  Calculate IO matrices with
    [`calc_io_mats()`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md).
    (Do this step prior to calling this function.)

2.  Identify each industry and each product from rows and columns of the
    **R** matrix.

3.  For each industry and product independently, perform a downstream
    swim with
    [`new_R_ps()`](https://matthewheun.github.io/Recca/reference/new_R_ps.md)
    to obtain the ECC induced by that industry or product only.

4.  Optionally (but included by default with `calc_pfd_aggs = TRUE`),
    calculate primary and final demand aggregates using
    [`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md)
    and
    [`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md).
    Both functions are called with `by = "Total"`, yielding total
    primary and final demand aggregates.

5.  Add the chopped ECCs to the right side of `.sut_data` as a nested
    data frame. If calculated, add the primary and final demand
    aggregates as columns in the nested data frame.

Chopping **Y** involves calculating an ECC for each individual product
row and sector column of final demand in the **Y** matrix. This
calculation is accomplished for each description of an ECC by the
following algorithm:

1.  Calculate io matrices with
    [`calc_io_mats()`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md).
    (Do this step prior to calling this function.)

2.  Identify each product and sector from rows and columns of the **Y**
    matrix.

3.  For each product and sector independently, perform an upstream swim
    with
    [`new_Y()`](https://matthewheun.github.io/Recca/reference/new_Y.md)
    to obtain the ECC requirements to supply that product or sector
    only.

4.  Optionally (but included by default), calculate primary and final
    demand aggregates using
    [`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md)
    and
    [`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md).
    Both functions are called with `by = "Total"`, yielding total
    primary and final demand aggregates.

5.  Add the chopped ECCs to the right side of `.sut_data` as a nested
    data frame. If calculated, add the primary and final demand
    aggregates as columns in the nested data frame.

Use the `unnest` argument to define how the aggregate data are added to
the right side of `.sut_data` when `.sut_data` is a `matsindf` data
frame.

Note that the nested data frame includes columns for the ECC matrices
for each isolated product or sector. Optionally, the nested data frame
includes primary and final demand aggregates for the chopped ECCs. The
names of the columns in the data frame are taken from the
`*_prime_colname` arguments.

`chop_R()` and `chop_Y()` involve downstream and upstream swims
performed by the
[`new_R_ps()`](https://matthewheun.github.io/Recca/reference/new_R_ps.md)
and [`new_Y()`](https://matthewheun.github.io/Recca/reference/new_Y.md)
functions. Both involve matrix inverses. The `method` arguments specify
how the matrix inversion is accomplished. The `tol` argument specifies
the tolerance for detecting linearities in the matrix to be inverted.
See the documentation at
[`matsbyname::invert_byname()`](https://matthewheun.github.io/matsbyname/reference/invert_byname.html)
for details.

Both `tol` and `method` should be a single values and apply to all rows
of `.sut_data`.

Before chopping and swimming are performed, the original **R** or **Y**
matrix is used for an downstream or upstream swim (respectively). An
error will be emitted if we are unable to reproduce the other ECC
matrices (**U**, **U_feed**, **U_EIOU**, **V**, and **Y** in the case of
a downstream swim when chopping **R**; **R**, **U**, **U_feed**,
**U_EIOU**, and **V** in the case of an upstream swim when chopping
**Y**) to within machine precision.

When the **R** and **Y** matrices are chopped by rows or columns, the
sum of the ECCs created from the chopped rows or columns should equal
the original ECC. Internally, these functions check for sum consistency
and emits an error if inconsistencies are found.

## Examples

``` r
p_industries <- c("Resources - Crude", "Resources - NG")
fd_sectors <- c("Residential", "Transport", "Oil fields")
psut_mats <- UKEnergy2000mats %>%
  tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)
psut_mats %>%
  chop_Y(p_industries = p_industries, fd_sectors = fd_sectors)
#> # A tibble: 4 × 13
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 5 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>,
#> #   ChoppedECCs <list>
psut_mats %>%
  chop_Y(p_industries = p_industries, fd_sectors = fd_sectors, unnest = TRUE)
#> # A tibble: 24 × 24
#>    Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>    <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#>  1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#>  2 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#>  3 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#>  4 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#>  5 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#>  6 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#>  7 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#>  8 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#>  9 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 10 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 14 more rows
#> # ℹ 16 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>,
#> #   ProductIndustrySector <chr>, R_prime <named list>, U_prime <named list>,
#> #   U_feed_prime <named list>, U_EIOU_prime <named list>,
#> #   r_EIOU_prime <named list>, V_prime <named list>, Y_prime <named list>,
#> #   S_units_prime <named list>, EXp <dbl>, EXfdgross <dbl>, EXfdnet <dbl>
psut_mats_2 <- psut_mats %>%
  # Slice to avoid the services rows on which NA values are obtained due to unit homogeneity.
  dplyr::filter(LastStage != "Services")
# Calculate aggregates
psut_mats_2 %>%
  chop_R(p_industries = p_industries, fd_sectors = fd_sectors)
#> # A tibble: 2 × 13
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 5 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>,
#> #   ChoppedECCs <list>
psut_mats_2 %>%
  chop_R(p_industries = p_industries, fd_sectors = fd_sectors, unnest = TRUE)
#> # A tibble: 8 × 24
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 5 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 6 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 7 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 8 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 16 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>,
#> #   ProductIndustrySector <chr>, R_prime <named list>, U_prime <named list>,
#> #   U_feed_prime <named list>, U_EIOU_prime <named list>,
#> #   r_EIOU_prime <named list>, V_prime <named list>, Y_prime <named list>,
#> #   S_units_prime <named list>, EXp <dbl>, EXfdgross <dbl>, EXfdnet <dbl>
```
