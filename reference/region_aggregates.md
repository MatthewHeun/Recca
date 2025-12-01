# Aggregate PSUT matrices into regions

Aggregates a data frame according to the regions given in a column of
the data frame. The data frame (`.sut_data`) should contain metadata
columns (including `many_colname` and `few_colname`) and be
wide-by-matrices.

## Usage

``` r
region_aggregates(
  .sut_data,
  many_colname = IEATools::iea_cols$country,
  few_colname = Recca::aggregate_cols$region,
  drop_na_few = FALSE,
  year = IEATools::iea_cols$year,
  method = IEATools::iea_cols$method,
  energy_type = IEATools::iea_cols$energy_type,
  last_stage = IEATools::iea_cols$last_stage,
  matrix_cols = c(R = Recca::psut_cols$R, U = Recca::psut_cols$U, U_feed =
    Recca::psut_cols$U_feed, U_eiou = Recca::psut_cols$U_eiou, r_eiou =
    Recca::psut_cols$r_eiou, V = Recca::psut_cols$V, Y = Recca::psut_cols$Y, S_units =
    Recca::psut_cols$S_units, Y_fu_details = Recca::psut_cols$Y_fu_details,
    U_eiou_fu_details = Recca::psut_cols$U_eiou_fu_details),
  matrix_names = Recca::psut_cols$matnames,
  matrix_values = Recca::psut_cols$matvals
)
```

## Arguments

- .sut_data:

  A wide-by-matrices `matsindf`-style data frame of PSUT matrices.

- many_colname:

  The name of the column in `.sut_data` that contains the "many"
  descriptions, for example countries that need to be aggregated to
  continents. Default is `IEATools::iea_cols$country`.

- few_colname:

  The of the column in `.sut_data` that contains the "few" descriptions,
  for example continents into which countries are to be aggregated.
  Default is `Recca::aggregate_cols$region`.

- drop_na_few:

  A boolean that tells whether to ignore (not aggregate) rows with `NA`
  values in `few_colname`. See details. Default is `FALSE`.

- year, method, energy_type, last_stage:

  See
  [`IEATools::iea_cols`](https://matthewheun.github.io/IEATools/reference/iea_cols.html).

- matrix_cols:

  Names of columns in .sut_data containing matrices. Default is a vector
  of names from
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md):
  R, U, U_feed, U_eiou, r_eiou, V, Y, S_units, Y_fu_details, and
  U_eiou_fu_details.

- matrix_names, matrix_values:

  Internal column names. See
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md).

## Value

An aggregated version of `.sut_data` wherein the `many_colname` column
is replaced by `few_colname` as specified by `aggregation_map`.

## Details

The argument `drop_na_few` controls what happens when an item
`many_colname` does not have a corresponding value in `few_colname`.
This condition can occur when, say, "WRLD" is a country. "WRLD" (as a
country in `many_colname`) should not be aggregated to "World" (as a
region in the `few_colname`). In those circumstances, a well-formed
`aggregation_map` will leave `NA` in `few_colname`. Setting
`drop_na_few` to `TRUE` (default is `FALSE`) will eliminate rows with
`NA` in `few_colname` before doing the aggregation so those `NA` rows do
not end up as `NA` in the outgoing data frame.

The default value for `drop_na_few` is `FALSE`, because setting to
`TRUE` will result in data loss. You need to opt in to this behavior
when you know it's what you want.

If all of `few_colname` entries are `NA` and `drop_na_few` is `TRUE`, a
zero-row data frame of the same structure as `.sut_data` is returned.

This function works for both a data frame of PSUT matrices and a data
frame of details matrices.

## Examples

``` r
library(dplyr)
library(matsbyname)
library(tidyr)
mats_GBR <- UKEnergy2000mats %>%
  tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)
# Add other countries, by duplicating and renaming GBR
mats <- dplyr::bind_rows(mats_GBR,
                         mats_GBR %>% dplyr::mutate(Country = "USA"),
                         mats_GBR %>% dplyr::mutate(Country = "FRA"))
# Establish the aggregation map.
agg_df <- list(EUR = c("GBR", "FRA"), AMR = "USA") %>%
  matsbyname::agg_map_to_agg_table(few_colname = "Continent", many_colname = "Country")
# Aggregate into continents
dplyr::left_join(mats, agg_df, by = "Country") %>%
  region_aggregates(many_colname = "Country", few_colname = "Continent")
#> # A tibble: 8 × 12
#>    Year EnergyType LastStage Country R             S_units  U_EIOU   U_feed  
#>   <dbl> <chr>      <chr>     <chr>   <list>        <list>   <list>   <list>  
#> 1  2000 E          Final     AMR     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2  2000 E          Final     EUR     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3  2000 E          Services  AMR     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4  2000 E          Services  EUR     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 5  2000 E          Useful    AMR     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 6  2000 E          Useful    EUR     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 7  2000 X          Services  AMR     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 8  2000 X          Services  EUR     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 4 more variables: V <list>, Y <list>, U <list>, r_EIOU <list>
```
