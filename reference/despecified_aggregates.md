# Despecify and aggregate PSUT matrices

PSUT matrices often have row and column names that been specified to
contain more information than simply the industry or product. Examples
include "Resources \[of Coal\]" and "Automobiles -\> RoP". It is
sometimes helpful to despecify and aggregate these rows and columns so
that all "Resources" are summed together, all "Automobiles" are summed
together, etc. This function performs that aggregation.

## Usage

``` r
despecified_aggregates(
  .sut_data = NULL,
  piece_to_keep = "noun",
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  U_feed = Recca::psut_cols$U_feed,
  U_eiou = Recca::psut_cols$U_eiou,
  r_eiou = Recca::psut_cols$r_eiou,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  S_units = Recca::psut_cols$S_units,
  inf_notation = TRUE,
  notation = list(RCLabels::notations_list),
  margin = list(c(1, 2)),
  choose_most_specific = TRUE,
  prepositions = list(RCLabels::prepositions_list),
  R_aggregated_colname = paste0(Recca::psut_cols$R, aggregated_suffix),
  U_aggregated_colname = paste0(Recca::psut_cols$U, aggregated_suffix),
  U_feed_aggregated_colname = paste0(Recca::psut_cols$U_feed, aggregated_suffix),
  U_eiou_aggregated_colname = paste0(Recca::psut_cols$U_eiou, aggregated_suffix),
  r_eiou_aggregated_colname = paste0(Recca::psut_cols$r_eiou, aggregated_suffix),
  V_aggregated_colname = paste0(Recca::psut_cols$V, aggregated_suffix),
  Y_aggregated_colname = paste0(Recca::psut_cols$Y, aggregated_suffix),
  S_units_aggregated_colname = paste0(Recca::psut_cols$S_units, aggregated_suffix),
  aggregated_suffix = Recca::aggregate_cols$aggregated_suffix
)
```

## Arguments

- .sut_data:

  A data frame of matrices to be despecified and aggregated.

- piece_to_keep:

  The piece of the label to retain before aggregation. Default is
  "noun".

- R, U, U_feed, U_eiou, r_eiou, V, Y, S_units:

  Matrices or names of columns in `.sut_data` to be despecified and
  aggregated. See
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md).

- inf_notation:

  A boolean that tells whether to infer the row and column label
  notation. Default is `TRUE`.

- notation:

  The notation for row and column labels. Default is
  `list(RCLabels::notations_list)`.

- margin:

  The margins over which aggregation is performed. Default is
  `list(c(1, 2))`.

- choose_most_specific:

  A boolean that tells whether to choose the most-specific notation if 2
  or more notations match. Default is `TRUE`.

- prepositions:

  A list of prepositions that could appear in row and column names.
  Default is `list(RCLabels::prepositions_list)`.

- R_aggregated_colname, U_aggregated_colname, U_feed_aggregated_colname,
  U_eiou_aggregated_colname, r_eiou_aggregated_colname,
  V_aggregated_colname, Y_aggregated_colname,
  S_units_aggregated_colname:

  Names of aggregated matrices or columns.

- aggregated_suffix:

  A string suffix used to form the names for aggregated matrices.
  Default is "\_aggregated".

## Value

A modified version of `.sut_data` where rows and columns of matrices
have been aggregated to their despecified parts.

## Details

By default, the aggregation is made to the nouns of row and column names
as defined by the `RCLabels` package. Which piece is to be aggregated is
given in the `piece_to_keep` argument. Internally, this function uses
[`matsbyname::aggregate_pieces_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_pieces_byname.html)
to do the heavy lifting.

## Examples

``` r
UKEnergy2000mats %>%
  tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) %>%
  despecified_aggregates()
#> # A tibble: 4 × 20
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 12 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>,
#> #   Raggregated <list>, Uaggregated <list>, U_feedaggregated <list>,
#> #   U_EIOUaggregated <list>, r_EIOUaggregated <list>, Vaggregated <list>,
#> #   Yaggregated <list>, S_unitsaggregated <list>
```
