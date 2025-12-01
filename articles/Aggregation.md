# Aggregation

## Introduction

`Recca` (`R` Energy Conversion Chain Analysis) contains many functions
useful for aggregating industries and products in energy conversion
chains.

The types of aggregation are shown in the following table.

|                                                                                              Function | Meaning                                                                            |
|------------------------------------------------------------------------------------------------------:|:-----------------------------------------------------------------------------------|
|         [`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md) | Aggregates primary energy                                                          |
| [`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md) | Aggregates final demand energy, the last stage in an energy conversion chain (ECC) |
|           [`region_aggregates()`](https://matthewheun.github.io/Recca/reference/region_aggregates.md) | Aggregates regions                                                                 |
| [`despecified_aggregates()`](https://matthewheun.github.io/Recca/reference/despecified_aggregates.md) | Despecifies industries and products, then aggregates                               |
|         [`grouped_aggregates()`](https://matthewheun.github.io/Recca/reference/grouped_aggregates.md) | Groups industries and products, then aggregates                                    |
|                 [`pfu_aggregates()`](https://matthewheun.github.io/Recca/reference/pfu_aggregates.md) | Aggregates to primary, final, and useful stages of an ECC                          |

This vignette describes those functions and demonstrates their use.

## `primary_aggregates()`

[`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md)
creates a column of primary energy sums coming from the industries
specified in the `p_industries` (primary industries) argument. These
primary industries are often found in the **R**, **V**, and **Y**
matrices of the PSUT framework. Typical `p_industries` are Resources,
Imports, and Exports. In the examples below, primary aggregates are
calculated for the four rows of the `UKEnergy2000mats` data frame.

In the first example, only one aggregate column is created (“EX.p”),
because there is no difference between net and gross aggregation for
primary energy. Net and gross primary aggregates are identical.

``` r
library(matsbyname)
p_industries <- c("Resources [of Crude]", "Resources [of NG]")
# Calculate primary total aggregates
ECCs <- UKEnergy2000mats %>%
  tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix")
ECCs
#> # A tibble: 4 × 12
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 4 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>
res <- ECCs %>%
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
#> [1] 93000 93000 93000 98220
```

In the next example, both net and gross aggregation columns are created
(“EX.p_net” and “EX.p_gross”), for compatibility with the
[`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md)
function discussed below.

``` r
res2 <- ECCs %>%
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
#> [1] 93000 93000 93000 98220
res2[[Recca::aggregate_cols$gross_aggregate_primary]]
#> [1] 93000 93000 93000 98220
```

## `finaldemand_aggregates()`

[`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md)
is similar to
[`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md),
except that it aggregates energy at the final demand stage in the **Y**
matrix. Both net and gross aggregate final demand are calculated. Gross
final demand includes energy industry own use (EIOU). Net final demand
does not.

``` r
fd_sectors <- c("Residential", "Transport")
res <- ECCs %>%
  Recca::finaldemand_aggregates(fd_sectors = fd_sectors, by = "Sector")
tibble::as_tibble(res)
#> # A tibble: 4 × 14
#>   Country  Year EnergyType LastStage R             U        U_EIOU   U_feed  
#>   <chr>   <dbl> <chr>      <chr>     <list>        <list>   <list>   <list>  
#> 1 GBR      2000 E          Final     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2 GBR      2000 E          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 3 GBR      2000 E          Useful    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 4 GBR      2000 X          Services  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 6 more variables: V <list>, Y <list>, r_EIOU <list>, S_units <list>,
#> #   EXfdnet <list>, EXfdgross <list>
res[[Recca::aggregate_cols$net_aggregate_demand]]
#> [[1]]
#>             Product
#> Residential   31000
#> Transport     40750
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
#> [[2]]
#>                  Product
#> Residential 5.000750e+14
#> Transport   6.429166e+11
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
#> [[3]]
#>              Product
#> Residential  4200.40
#> Transport   21714.98
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
#> [[4]]
#>                  Product
#> Residential 5.000750e+14
#> Transport   6.429166e+11
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
res[[Recca::aggregate_cols$gross_aggregate_demand]]
#> [[1]]
#>             Product
#> Residential   31000
#> Transport     40750
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
#> [[2]]
#>                  Product
#> Residential 5.000750e+14
#> Transport   6.429166e+11
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
#> [[3]]
#>              Product
#> Residential  4200.40
#> Transport   21714.98
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
#> [[4]]
#>                  Product
#> Residential 5.000750e+14
#> Transport   6.429166e+11
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
```

## `region_aggregates()`

[`region_aggregates()`](https://matthewheun.github.io/Recca/reference/region_aggregates.md)
sums regions according to `many_colname` and `few_colname`. To
demonstrate this function, we need to modify the The example data frame
slightly. It contains only one country (“GBR”), but it should contain
different countries and a “few” column. Furthermore, the last stage and
energy type columns should be the same where aggregation is required.
The following code produces this modification to pretend that rows of
the data frame apply to different countries.

``` r
ECCs_for_region_agg <- ECCs |> 
  dplyr::mutate(
    Country = c("USA", "GBR", "CAN", "FRA"), 
    Continent = c("NoAmr", "Europe", "NoAmr", "Europe"), 
    EnergyType = "E", 
    LastStage = c("Final", "Services", "Final", "Services")
  )
ECCs_for_region_agg |> 
  dplyr::select(Country, Continent, EnergyType, LastStage)
#> # A tibble: 4 × 4
#>   Country Continent EnergyType LastStage
#>   <chr>   <chr>     <chr>      <chr>    
#> 1 USA     NoAmr     E          Final    
#> 2 GBR     Europe    E          Services 
#> 3 CAN     NoAmr     E          Final    
#> 4 FRA     Europe    E          Services
```

Given this modification, we aggregate to continents with the following
code.

``` r
continent_aggregations <- ECCs_for_region_agg |> 
  Recca::region_aggregates(many_colname = "Country", few_colname = "Continent")
continent_aggregations
#> # A tibble: 2 × 12
#>    Year EnergyType LastStage Country R             S_units  U_EIOU   U_feed  
#>   <dbl> <chr>      <chr>     <chr>   <list>        <list>   <list>   <list>  
#> 1  2000 E          Final     NoAmr   <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2  2000 E          Services  Europe  <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 4 more variables: V <list>, Y <list>, U <list>, r_EIOU <list>
```

By default, `many_colname` is replaced by the common values in
`few_colname`. A simple rename can override this default behaviour.

``` r
continent_aggregations |> 
  dplyr::rename(
    Continent = Country
  )
#> # A tibble: 2 × 12
#>    Year EnergyType LastStage Continent R             S_units  U_EIOU   U_feed  
#>   <dbl> <chr>      <chr>     <chr>     <list>        <list>   <list>   <list>  
#> 1  2000 E          Final     NoAmr     <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> 2  2000 E          Services  Europe    <dbl [2 × 2]> <dbl[…]> <dbl[…]> <dbl[…]>
#> # ℹ 4 more variables: V <list>, Y <list>, U <list>, r_EIOU <list>
```

## `despecified_aggregates()`

When the row and column names of the PSUT matrices are “specified,” they
will look like “Resources \[of Crude\]” or “NG \[from Wells\]”. Many
labels can have a “noun \[preposition object\]” structure, which we call
“specified.”
[`despecified_aggregates()`](https://matthewheun.github.io/Recca/reference/despecified_aggregates.md)
eliminates the specification in the row and/or column names, keeping the
desired `piece` (by default the “noun”), and aggregates (sums) rows or
columns with resulting identical names. See the comments in the examples
below.

``` r
despecified_aggs <- ECCs |> 
  # Accept the default "noun" aggregation
  # and the default "_aggregated" suffix for 
  # aggregated columns.
  despecified_aggregates()

# This is the original R matrix.
# Its row names are specified.
ECCs$R[[1]]
#>                      Crude    NG
#> Resources [of Crude] 50000     0
#> Resources [of NG]        0 43000
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"

# This is the despecified R matrix.
# Note the single Resources row.
despecified_aggs$R_aggregated[[1]]
#> Warning: Unknown or uninitialised column: `R_aggregated`.
#> NULL

# Here is an original use matrix.
# Many rows are specified.
ECCs$U[[2]]
#>                         Car engines   Cars Crude dist. Diesel dist. Elect. grid
#> Crude                             0    0.0           0     0.00e+00           0
#> Crude [from Dist.]                0    0.0           0     0.00e+00           0
#> Crude [from Fields]               0    0.0       47500     0.00e+00           0
#> Diesel                            0    0.0           0     1.55e+04           0
#> Diesel [from Dist.]               0    0.0           0     0.00e+00           0
#> Elect                             0    0.0           0     0.00e+00        6400
#> Elect [from Grid]                 0    0.0          25     0.00e+00           0
#> Freight [tonne-km/year]           0    0.0  1666685185     1.50e+09           0
#> Light                             0    0.0           0     0.00e+00           0
#> LTH                               0    0.0           0     0.00e+00           0
#> MD [from Car engines]             0 3000.4           0     0.00e+00           0
#> MD [from Truck engines]           0    0.0           0     0.00e+00           0
#> NG                                0    0.0           0     0.00e+00           0
#> NG [from Dist.]                   0    0.0           0     0.00e+00           0
#> NG [from Wells]                   0    0.0           0     0.00e+00           0
#> Petrol                            0    0.0           0     0.00e+00           0
#> Petrol [from Dist.]           26000    0.0           0     0.00e+00           0
#>                         Furnaces Gas wells & proc. Homes Light fixtures
#> Crude                          0                 0     0              0
#> Crude [from Dist.]             0                 0     0              0
#> Crude [from Fields]            0                 0     0              0
#> Diesel                         0                 0     0              0
#> Diesel [from Dist.]            0                50     0              0
#> Elect                          0                 0     0              0
#> Elect [from Grid]              0                25     0           6000
#> Freight [tonne-km/year]        0                 0     0              0
#> Light                          0                 0     0              0
#> LTH                            0                 0 20000              0
#> MD [from Car engines]          0                 0     0              0
#> MD [from Truck engines]        0                 0     0              0
#> NG                             0             43000     0              0
#> NG [from Dist.]            25000                 0     0              0
#> NG [from Wells]                0                 0     0              0
#> Petrol                         0                 0     0              0
#> Petrol [from Dist.]            0                 0     0              0
#>                           NG dist. Oil fields Oil refineries Petrol dist.
#> Crude                            0      50000              0     0.00e+00
#> Crude [from Dist.]               0          0          47000     0.00e+00
#> Crude [from Fields]              0          0              0     0.00e+00
#> Diesel                           0          0              0     0.00e+00
#> Diesel [from Dist.]              0         50              0     0.00e+00
#> Elect                            0          0              0     0.00e+00
#> Elect [from Grid]               25         25             75     0.00e+00
#> Freight [tonne-km/year] 1666685185          0              0     2.25e+09
#> Light                            0          0              0     0.00e+00
#> LTH                              0          0              0     0.00e+00
#> MD [from Car engines]            0          0              0     0.00e+00
#> MD [from Truck engines]          0          0              0     0.00e+00
#> NG                               0          0              0     0.00e+00
#> NG [from Dist.]                  0          0              0     0.00e+00
#> NG [from Wells]              41000          0              0     0.00e+00
#> Petrol                           0          0              0     2.65e+04
#> Petrol [from Dist.]              0          0              0     0.00e+00
#>                         Power plants Rooms Truck engines  Trucks
#> Crude                              0     0             0    0.00
#> Crude [from Dist.]                 0     0             0    0.00
#> Crude [from Fields]                0     0             0    0.00
#> Diesel                             0     0             0    0.00
#> Diesel [from Dist.]                0     0         15050    0.00
#> Elect                              0     0             0    0.00
#> Elect [from Grid]                100     0             0    0.00
#> Freight [tonne-km/year]            0     0             0    0.00
#> Light                              0  1200             0    0.00
#> LTH                                0     0             0    0.00
#> MD [from Car engines]              0     0             0    0.00
#> MD [from Truck engines]            0     0             0 1799.98
#> NG                                 0     0             0    0.00
#> NG [from Dist.]                16000     0             0    0.00
#> NG [from Wells]                    0     0             0    0.00
#> Petrol                             0     0             0    0.00
#> Petrol [from Dist.]                0     0             0    0.00
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"

# Here is the corresponding despecified use matrix. 
# None of the rows are specified.
despecified_aggs$U_aggregated[[2]]
#> Warning: Unknown or uninitialised column: `U_aggregated`.
#> NULL

# Here is an original make matrix
# with many specified columns.
ECCs$V[[3]]
#>                   MD [from Car engines] Crude [from Dist.] Diesel [from Dist.]
#> Car engines                      3000.4                  0                   0
#> Crude dist.                         0.0              47000                   0
#> Diesel dist.                        0.0                  0               15150
#> Elect. grid                         0.0                  0                   0
#> Furnaces                            0.0                  0                   0
#> Gas wells & proc.                   0.0                  0                   0
#> Light fixtures                      0.0                  0                   0
#> NG dist.                            0.0                  0                   0
#> Oil fields                          0.0                  0                   0
#> Oil refineries                      0.0                  0                   0
#> Petrol dist.                        0.0                  0                   0
#> Power plants                        0.0                  0                   0
#> Truck engines                       0.0                  0                   0
#>                   Elect [from Grid]   LTH NG [from Wells] Light NG [from Dist.]
#> Car engines                       0     0               0     0               0
#> Crude dist.                       0     0               0     0               0
#> Diesel dist.                      0     0               0     0               0
#> Elect. grid                    6275     0               0     0               0
#> Furnaces                          0 20000               0     0               0
#> Gas wells & proc.                 0     0           41000     0               0
#> Light fixtures                    0     0               0  1200               0
#> NG dist.                          0     0               0     0           41000
#> Oil fields                        0     0               0     0               0
#> Oil refineries                    0     0               0     0               0
#> Petrol dist.                      0     0               0     0               0
#> Power plants                      0     0               0     0               0
#> Truck engines                     0     0               0     0               0
#>                   Crude [from Fields] Diesel Petrol Petrol [from Dist.] Elect
#> Car engines                         0      0      0                   0     0
#> Crude dist.                         0      0      0                   0     0
#> Diesel dist.                        0      0      0                   0     0
#> Elect. grid                         0      0      0                   0     0
#> Furnaces                            0      0      0                   0     0
#> Gas wells & proc.                   0      0      0                   0     0
#> Light fixtures                      0      0      0                   0     0
#> NG dist.                            0      0      0                   0     0
#> Oil fields                      47500      0      0                   0     0
#> Oil refineries                      0  15500  26500                   0     0
#> Petrol dist.                        0      0      0               26000     0
#> Power plants                        0      0      0                   0  6400
#> Truck engines                       0      0      0                   0     0
#>                   MD [from Truck engines]
#> Car engines                          0.00
#> Crude dist.                          0.00
#> Diesel dist.                         0.00
#> Elect. grid                          0.00
#> Furnaces                             0.00
#> Gas wells & proc.                    0.00
#> Light fixtures                       0.00
#> NG dist.                             0.00
#> Oil fields                           0.00
#> Oil refineries                       0.00
#> Petrol dist.                         0.00
#> Power plants                         0.00
#> Truck engines                     1799.98
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"

# None of the columns are specified
# in the despecified version.
despecified_aggs$V_aggregated[[3]]
#> Warning: Unknown or uninitialised column: `V_aggregated`.
#> NULL

# This original final demand matrix
# has specified rownames
# that provide units.
ECCs$Y[[4]]
#>                                Transport Residential
#> Freight [tonne-km/year]     142916629629     0.0e+00
#> Illumination [lumen-hrs/yr]            0     5.0e+14
#> Passenger [passenger-km/yr] 500000000000     0.0e+00
#> Space heating [m3-K]                   0     7.5e+10
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"

# Despecifying this matrix eliminates the units,
# resulting in information loss.
despecified_aggs$Y_aggregated[[4]]
#> Warning: Unknown or uninitialised column: `Y_aggregated`.
#> NULL
```

As the last example (with the final demand matrix, **Y**) shows,
aggregating despecified row and column names can lead to information
loss. Thus,
[`despecified_aggregates()`](https://matthewheun.github.io/Recca/reference/despecified_aggregates.md)
will normally be called only at the last step in a calculation chain.

## `grouped_aggregates()`

Grouping provides the capability to aggregate specific energy products
to classes of energy and groupings of processing stages or final demand
categories. For example, Anthracite and Brown coal can be grouped to
Coal and coal products. An aggregation map is required.

To demonstrate grouped aggregation, we first establish an aggregation
map. An aggregation map is a named list where list members are
aggregated to member names.

``` r
agg_map <- list(`Crude oil` = c("Crude", "Crude [from Dist.]", "Crude [from Fields]"), 
                `Oil and oil products` = c("Diesel", "Diesel [from Dist.]", 
                                           "Petrol", "Petrol [from Dist.]"), 
                NG = c("NG", "NG [from Dist.]", "NG [from Wells]"),
                Electricity = c("Elect", "Elect [from Grid]"))
```

The aggregation map can be used with
[`grouped_aggregates()`](https://matthewheun.github.io/Recca/reference/grouped_aggregates.md)
to do the desired aggregation. Aggregation columns are added to the
right of the `.sut_data` data frame.

``` r
# Here is an original use matrix.
ECCs$U[[1]]
#>                     Crude dist. Diesel dist. Elect. grid Gas wells & proc.
#> Crude                         0            0           0                 0
#> Crude [from Dist.]          500            0           0                 0
#> Crude [from Fields]       47500            0           0                 0
#> Diesel                        0        15500           0                 0
#> Diesel [from Dist.]          25          350           0                50
#> Elect                         0            0        6400                 0
#> Elect [from Grid]            25            0           0                25
#> NG                            0            0           0             43000
#> NG [from Dist.]               0            0           0                 0
#> NG [from Wells]               0            0           0              2000
#> Petrol                        0            0           0                 0
#> Petrol [from Dist.]           0            0           0                 0
#>                     NG dist. Oil fields Oil refineries Petrol dist.
#> Crude                      0      50000              0            0
#> Crude [from Dist.]         0          0          47000            0
#> Crude [from Fields]        0       2500              0            0
#> Diesel                     0          0           5000            0
#> Diesel [from Dist.]       25         50              0          250
#> Elect                      0          0              0            0
#> Elect [from Grid]         25         25             75            0
#> NG                         0          0              0            0
#> NG [from Dist.]            0          0              0            0
#> NG [from Wells]        41000          0              0            0
#> Petrol                     0          0              0        26500
#> Petrol [from Dist.]        0          0              0          500
#>                     Power plants
#> Crude                          0
#> Crude [from Dist.]             0
#> Crude [from Fields]            0
#> Diesel                         0
#> Diesel [from Dist.]            0
#> Elect                          0
#> Elect [from Grid]            100
#> NG                             0
#> NG [from Dist.]            16000
#> NG [from Wells]                0
#> Petrol                         0
#> Petrol [from Dist.]            0
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"

# Aggregate to the desired groups.
ECCs_grouped_aggs <- ECCs |> 
  grouped_aggregates(aggregation_map = agg_map)

# Here is the aggregated use matrix.
# Note that the rows are summed
# and named according to the agg_map.
ECCs_grouped_aggs$U_aggregated[[1]]
#> Warning: Unknown or uninitialised column: `U_aggregated`.
#> NULL
```

## `pfu_aggregates()`

Aggregating to primary, final, and useful stages of the energy
conversion chain (ECC) can be accomplished with
[`pfu_aggregates()`](https://matthewheun.github.io/Recca/reference/pfu_aggregates.md).
Aggregates are added in columns to the right of the `.sutdata` data
frame. Primary industries and final demand sectors are specified as
character vectors in the `p_industries` and `fd_sectors` arguments.

``` r
# Get the defaul separator for column names
sep <- Recca::all_stages$last_stage_sep
# Set primary industry names and final demand sector names
p_industries <- c("Resources [of Crude]", "Resources [of NG]")
fd_sectors <- c("Residential", "Transport", "Oil fields")
# Calculate TOTAL aggregates
pfu_aggs_total <- UKEnergy2000mats |>
  tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
  pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                 by = "Total")
# Look at the column names.
# The naming scheme is 
# EX.stage.gn___lsStage
# EX = energy or exergy aggregate (depending on the value in the EnergyType column)
# stage = the stage at which the aggregation is computed
# gn = whether the aggregate is "gross" or "net"
# ____ls = the separator (Recca::all_stages$last_stage_sep)
# Stage = The last stage in the ECC for this column.
names(pfu_aggs_total)
#>  [1] "Country"               "Year"                  "EnergyType"           
#>  [4] "R___lsFinal"           "U___lsFinal"           "U_feed___lsFinal"     
#>  [7] "U_EIOU___lsFinal"      "r_EIOU___lsFinal"      "V___lsFinal"          
#> [10] "Y___lsFinal"           "S_units___lsFinal"     "R___lsUseful"         
#> [13] "U___lsUseful"          "U_feed___lsUseful"     "U_EIOU___lsUseful"    
#> [16] "r_EIOU___lsUseful"     "V___lsUseful"          "Y___lsUseful"         
#> [19] "S_units___lsUseful"    "R___lsServices"        "U___lsServices"       
#> [22] "U_feed___lsServices"   "U_EIOU___lsServices"   "r_EIOU___lsServices"  
#> [25] "V___lsServices"        "Y___lsServices"        "S_units___lsServices" 
#> [28] "EXpnet___lsFinal"      "EXpgross___lsFinal"    "EXpnet___lsUseful"    
#> [31] "EXpgross___lsUseful"   "EXpnet___lsServices"   "EXpgross___lsServices"
#> [34] "EXfnet___lsFinal"      "EXfgross___lsFinal"    "EXfnet___lsUseful"    
#> [37] "EXfgross___lsUseful"   "EXfnet___lsServices"   "EXfgross___lsServices"
#> [40] "EXunet___lsFinal"      "EXugross___lsFinal"    "EXunet___lsUseful"    
#> [43] "EXugross___lsUseful"   "EXunet___lsServices"   "EXugross___lsServices"
#> [46] "EXsnet___lsFinal"      "EXsgross___lsFinal"    "EXsnet___lsUseful"    
#> [49] "EXsgross___lsUseful"   "EXsnet___lsServices"   "EXsgross___lsServices"
# Check some aggregation values.
# Note that not all aggregations are available, 
# based on the structure of the ECC.

# Net primary energy aggregation when last stage is Final
pfu_aggs_total |>
  dplyr::filter(EnergyType == "E") |>
  magrittr::extract2("EX.p_net___lsFinal") |>
  magrittr::extract2(1)
#> NULL
# Net primary exergy aggregation when last stage is Services
pfu_aggs_total |>
  dplyr::filter(EnergyType == "X") |>
  magrittr::extract2("EX.p_net___lsServices") |>
  magrittr::extract2(1)
#> NULL
# Gross final energy aggregation when last stage is Useful
pfu_aggs_total |>
  dplyr::filter(EnergyType == "E") |>
  magrittr::extract2("EX.f_gross___lsFinal") |>
  magrittr::extract2(1)
#> NULL
# Gross useful energy aggregation when last stage is Useful
pfu_aggs_total |>
  dplyr::filter(EnergyType == "E") |>
  magrittr::extract2("EX.u_gross___lsUseful") |>
  magrittr::extract2(1)
#> NULL
# Net useful energy aggregation when last stage is Useful
pfu_aggs_total |>
  dplyr::filter(EnergyType == "E") |>
  magrittr::extract2("EX.u_net___lsUseful") |>
  magrittr::extract2(1)
#> NULL
# Calculate PRODUCT aggregates
pfu_aggs_product <- UKEnergy2000mats |>
  tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
  pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                 by = "Product")
# Net primary energy aggregation when last stage is Final
# Note that all results are now vectors which 
# show aggregation by product
pfu_aggs_product |>
  dplyr::filter(EnergyType == "E") |>
  magrittr::extract2("EX.p_net___lsFinal") |>
  magrittr::extract2(1)
#> NULL
pfu_aggs_product |>
  dplyr::filter(EnergyType == "E") |>
  magrittr::extract2("EX.u_net___lsUseful") |>
  magrittr::extract2(1)
#> NULL
# Calculate INDUSTRY aggregates
# Note that all results are now vectors which 
# show aggregation by industry
pfu_aggs_industry <- UKEnergy2000mats |>
  tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
  pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                 by = "Industry")
pfu_aggs_industry |>
  dplyr::filter(EnergyType == "E") |>
  magrittr::extract2("EX.p_net___lsFinal") |>
  magrittr::extract2(1)
#> NULL
pfu_aggs_industry |>
  dplyr::filter(EnergyType == "E") |>
  magrittr::extract2("EX.u_net___lsUseful") |>
  magrittr::extract2(1)
#> NULL
```

## Conclusion

Several functions in the `Recca` package assist with aggregations in
energy conversion chains.
[`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md),
[`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md),
[`region_aggregates()`](https://matthewheun.github.io/Recca/reference/region_aggregates.md),
[`despecified_aggregates()`](https://matthewheun.github.io/Recca/reference/despecified_aggregates.md),
[`grouped_aggregates()`](https://matthewheun.github.io/Recca/reference/grouped_aggregates.md),
and
[`pfu_aggregates()`](https://matthewheun.github.io/Recca/reference/pfu_aggregates.md)
all assist with different types of aggregations. Please see help for
these functions for more details.
