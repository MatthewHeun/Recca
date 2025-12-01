# Primary energy and exergy aggregates

Calculates aggregate primary energy from a data frame of Supply-Use
matrices.

## Usage

``` r
primary_aggregates(
  .sutdata = NULL,
  p_industries,
  add_net_gross_cols = FALSE,
  piece = "all",
  notation = RCLabels::notations_list,
  pattern_type = c("exact", "leading", "trailing", "anywhere", "literal"),
  prepositions = RCLabels::prepositions_list,
  R = Recca::psut_cols$R,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  by = c("Total", "Product", "Industry", "Flow"),
  aggregate_primary = Recca::aggregate_cols$aggregate_primary,
  net_aggregate_primary = Recca::aggregate_cols$net_aggregate_primary,
  gross_aggregate_primary = Recca::aggregate_cols$gross_aggregate_primary
)
```

## Arguments

- .sutdata:

  A data frame with columns of matrices from a supply-use analysis.

- p_industries:

  A vector of names of industries to be aggregated as "primary." If
  `.sutdata` is a data frame, `p_industries` should be the name of a
  column in the data frame. If `.sutdata` is `NULL`, `p_industries` can
  be a single vector of industry names. These industries in
  `p_industries` will appear in rows of the resource (`R`) and make
  (`V`) matrices and columns of the final demand matrix (`Y`). Entries
  in `Y_p` will be subtracted from entries in `R_p + V_p` to obtain the
  total primary energy aggregate, where `*_p` is the primary part of
  those matrices. The function
  [`find_p_industry_names()`](https://matthewheun.github.io/Recca/reference/find_p_industry_names.md)
  might be helpful to find primary industry names if they can be
  identified by prefixes.

- add_net_gross_cols:

  A boolean that tells whether to add net and gross columns (`TRUE`) or
  not (`FALSE`). Default is `FALSE`.

- piece, notation, pattern_type, prepositions:

  Arguments that control the way row and column matching is accomplished
  when selecting parts of the **R**, **V**, and **Y** matrices for
  primary aggregation. These arguments are passed to
  [`matsbyname::select_rowcol_piece_byname()`](https://matthewheun.github.io/matsbyname/reference/select_rowcol_piece_byname.html)
  and eventually
  [`RCLabels::match_by_pattern()`](https://matthewheun.github.io/RCLabels/reference/regex_funcs.html)
  and
  [`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html).
  Default values are `piece = "all"`,
  `notation = RCLabels::notations_list`, `pattern_type = "exact"`, and
  `prepositions = RCLabels::prepositions_list`.

- R, V, Y:

  See
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md).

- by:

  One of "Total", "Product", "Industry", or "Flow" to indicate the
  desired aggregation:

  - "Total": aggregation over both Product and Flow (the default),

  - "Product": aggregation by energy carrier (Crude oil, Primary solid
    biofuels, etc.), or

  - "Industry" or "Flow": aggregation by Industry (Production, Imports,
    Exports, etc.).

- aggregate_primary, net_aggregate_primary, gross_aggregate_primary:

  The names for aggregates of primary energy on output.

## Value

A list or data frame containing aggregate primary energy.

## Details

By default, this function adds a single column of primary energy
aggregates with the name `aggregate_primary`. If `add_net_gross_cols` is
`TRUE` (default is `FALSE`), two columns are created:
`net_aggregate_primary` and `gross_aggregate_primary`. With net and
gross output (`add_net_gross_cols = TRUE`), the columns contain
identical values. Use `add_net_gross_cols = TRUE` if you later wish to
combine with results from
[`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md),
which provides both net and gross outputs.

## Examples

``` r
library(matsbyname)
p_industries <- c("Resources - Crude", "Resources - NG")
# Calculate primary total aggregates
res <- UKEnergy2000mats %>%
  tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
  Recca::primary_aggregates(p_industries = p_industries, by = "Total")
tibble::as_tibble(res)
#> # A tibble: 4 × 13
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 5 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>,
#> #   EXp <dbl>
res[[Recca::aggregate_cols$aggregate_primary]]
#> [1] 0 0 0 0
# Above, only 1 aggregate column is created, because there is no
# difference between net and gross aggregation for primary energy.
# Below, both net and gross aggregation columns are created,
# for compatibility with the [finaldemand_aggregates()] function.
# Net and gross primary aggregates are identical.
res2 <- UKEnergy2000mats %>%
  tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
  Recca::primary_aggregates(p_industries = p_industries,
                            add_net_gross_cols = TRUE,
                            by = "Total")
tibble::as_tibble(res2)
#> # A tibble: 4 × 14
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 6 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>,
#> #   EXpnet <dbl>, EXpgross <dbl>
res2[[Recca::aggregate_cols$net_aggregate_primary]]
#> [1] 0 0 0 0
res2[[Recca::aggregate_cols$gross_aggregate_primary]]
#> [1] 0 0 0 0
```
