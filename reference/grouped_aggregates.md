# Perform grouping aggregations on PSUT matrices

It is often helpful to aggregate data into industry or product
categories, such as "Anthracite" and "Brown coal" to "Coal and coal
products" or "Domestic aviation" and "Domestic navigation" to
"Transport". With the help of an `aggregation_map`, this function
performs such aggregations for a set of PSUT matrices.

## Usage

``` r
grouped_aggregates(
  .sut_data = NULL,
  aggregation_map,
  margin = c(1, 2),
  pattern_type = "exact",
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  U_feed = Recca::psut_cols$U_feed,
  U_eiou = Recca::psut_cols$U_eiou,
  r_eiou = Recca::psut_cols$r_eiou,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  S_units = Recca::psut_cols$S_units,
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

- aggregation_map:

  Aggregation details. See documentation for
  [`matsbyname::aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.html)
  for further information.

- margin:

  `1`, `2`, or `c(1, 2)` for row aggregation, column aggregation, or
  both. Can be a row or column type. Default is `c(1, 2)`.

- pattern_type:

  See
  [`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html).
  Default is "exact".

- R, U, U_feed, U_eiou, r_eiou, V, Y, S_units:

  Matrices or names of columns in `.sut_data` to be despecified and
  aggregated. See
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md).

- R_aggregated_colname, U_aggregated_colname, U_feed_aggregated_colname,
  U_eiou_aggregated_colname, r_eiou_aggregated_colname,
  V_aggregated_colname, Y_aggregated_colname,
  S_units_aggregated_colname:

  Names of aggregated matrices or columns.

- aggregated_suffix:

  A string suffix used to form the names for aggregated matrices.
  Default is "\_aggregated".

## Value

PSUT matrices aggregated according to `aggregation_map`.

## Details

Internally, this function uses
[`matsbyname::aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.html).
See its documentation for details on the format for the
`aggregation_map`.

## Examples

``` r
UKEnergy2000mats %>%
  tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) %>%
  grouped_aggregates(aggregation_map = list(`Oil and oil products` =
                                            c("Crude", "Diesel", "Petrol")),
                     pattern_type = "leading",
                     margin = "Product")
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
