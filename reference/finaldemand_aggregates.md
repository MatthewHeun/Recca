# Final demand energy and exergy aggregates

Calculates aggregate final demand energy from a data frame of Supply-Use
matrices. The calculation counts only `fd_sectors` in final demand
aggregates. If `.sutdata` is `NULL`, `fd_sectors` can be a single vector
of industry names.

## Usage

``` r
finaldemand_aggregates(
  .sutdata = NULL,
  fd_sectors,
  piece = "all",
  notation = RCLabels::notations_list,
  pattern_type = c("exact", "leading", "trailing", "anywhere", "literal"),
  prepositions = RCLabels::prepositions_list,
  U_eiou = Recca::psut_cols$U_eiou,
  Y = Recca::psut_cols$Y,
  by = c("Total", "Product", "Sector", "Industry"),
  net_aggregate_demand = Recca::aggregate_cols$net_aggregate_demand,
  gross_aggregate_demand = Recca::aggregate_cols$gross_aggregate_demand
)
```

## Arguments

- .sutdata:

  A data frame with columns of matrices from a supply-use analysis.

- fd_sectors:

  A vector of names of sectors in final demand. Names should include
  columns in the `Y` and `U_EIOU` matrices to cover both net (in `Y`)
  and gross (in `Y` and `U_EIOU`) final demand.

- piece, notation, pattern_type, prepositions:

  Arguments that control the way row and column matching is accomplished
  when selecting parts of the **U_EIOU** and **Y** matrices for final
  demand aggregation. These arguments are passed to
  [`matsbyname::select_rowcol_piece_byname()`](https://matthewheun.github.io/matsbyname/reference/select_rowcol_piece_byname.html)
  and eventually
  [`RCLabels::match_by_pattern()`](https://matthewheun.github.io/RCLabels/reference/regex_funcs.html)
  and
  [`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html).
  Default values are `piece = "all"`,
  `notation = RCLabels::notations_list`, `pattern_type = "exact"`, and
  `prepositions = RCLabels::prepositions_list`.

- U_eiou, Y:

  Input matrices. See
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md).

- by:

  One of "Product", "Sector", or "Total" to indicate the desired
  aggregation: "Product" for aggregation by energy carrier (Crude oil,
  Primary solid biofuels, etc.), "Sector" for aggregation by final
  demand sector (Agriculture/forestry, Domestic navigation, etc.), or
  "Total" for aggregation over both Product and Sector (the default).

- net_aggregate_demand, gross_aggregate_demand:

  See
  [`Recca::aggregate_cols`](https://matthewheun.github.io/Recca/reference/aggregate_cols.md).
  Defaults are `Recca::aggregate_cols$net_aggregate_demand` and
  `Recca::aggregate_cols$gross_aggregate_demand`.

## Value

A list or data frame containing `net_aggregate_demand` and
`gross_aggregate_demand` columns.

## Details

Net energy demand is calculated by `matsbyname::sum_byname(Y_fd)`, with
sums across rows, columns, or total as needed. Gross energy demand is
calculated by
`matsbyname::sum_byname(Y_fd) + matsbyname::sum_byname(U_EIOU)`, with
sums across rows, columns, or total as needed.

## Examples

``` r
library(matsbyname)
UKEnergy2000mats %>%
  tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
  dplyr::mutate(
    fd_sectors = rep(list(c("Residential", "Transport")), times = nrow(.))
  ) %>%
  dplyr::filter(LastStage %in% c(IEATools::last_stages$final,
                                 IEATools::last_stages$useful)) %>%
  finaldemand_aggregates(fd_sectors = "fd_sectors", by = "Sector")
#> Error in finaldemand_aggregates(., fd_sectors = "fd_sectors", by = "Sector"): unused arguments (fd_sectors = "fd_sectors", by = "Sector")
```
