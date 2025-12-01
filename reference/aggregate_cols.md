# Aggregate data frame column names

A string list containing named names of columns in aggregate data
frames. Items in the list provide default values for column name
function arguments to aggregation functions throughout the `Recca`
package.

## Usage

``` r
aggregate_cols
```

## Format

A string list with 19 entries.

- aggregate_primary:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of primary energy.

- net_aggregate_primary:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of net primary energy. Net and gross aggregates will be
  identical at the primary stage.

- gross_aggregate_primary:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of gross primary energy. Net and gross aggregates will be
  identical at the primary stage.

- aggregate_final:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of final energy.

- net_aggregate_final:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of net final energy, not including energy industry own use.

- gross_aggregate_final:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of gross final energy, including energy industry own use.

- aggregate_useful:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of useful energy.

- net_aggregate_useful:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of net useful energy, not including energy industry own
  use.

- gross_aggregate_useful:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of gross useful energy, including energy industry own use.

- aggregate_services:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of energy services.

- net_aggregate_services:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of net energy services, not including energy industry own
  use.

- gross_aggregate_services:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of gross energy services, including energy industry own
  use.

- aggregate_demand:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of final demand energy (including energy industry own use),
  regardless of whether the last stage is final, useful, or services.

- net_aggregate_demand:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of net final demand energy (excluding energy industry own
  use), regardless of whether the last stage is final, useful, or
  services.

- gross_aggregate_demand:

  The name of a column in a wide-by-matrices data frame containing
  aggregates of gross final demand energy (including energy industry own
  use), regardless of whether the last stage is final, useful, or
  services.

- region:

  The name of a column in a wide-by-matrices data frame containing
  regions.

- aggregated_suffix:

  The suffix for column names containing aggregated matrices.

- product_sector:

  The name of a column containing names of products, industries, or
  sectors. Default is "Product.Industry.Sector".

- chop_df:

  The name of a column containing a nested data frame of chopped energy
  conversion chains. Default is "Chopped.ECCs".

## Examples

``` r
aggregate_cols
#> $aggregate_primary
#> [1] "EXp"
#> 
#> $net_aggregate_primary
#> [1] "EXpnet"
#> 
#> $gross_aggregate_primary
#> [1] "EXpgross"
#> 
#> $aggregate_final
#> [1] "EXf"
#> 
#> $net_aggregate_final
#> [1] "EXfnet"
#> 
#> $gross_aggregate_final
#> [1] "EXfgross"
#> 
#> $aggregate_useful
#> [1] "EXu"
#> 
#> $net_aggregate_useful
#> [1] "EXunet"
#> 
#> $gross_aggregate_useful
#> [1] "EXugross"
#> 
#> $aggregate_services
#> [1] "EXs"
#> 
#> $net_aggregate_services
#> [1] "EXsnet"
#> 
#> $gross_aggregate_services
#> [1] "EXsgross"
#> 
#> $aggregate_demand
#> [1] "EXfd"
#> 
#> $net_aggregate_demand
#> [1] "EXfdnet"
#> 
#> $gross_aggregate_demand
#> [1] "EXfdgross"
#> 
#> $region
#> [1] "Region"
#> 
#> $aggregated_suffix
#> [1] "aggregated"
#> 
#> $product_sector
#> [1] "ProductIndustrySector"
#> 
#> $chop_df
#> [1] "ChoppedECCs"
#> 
```
