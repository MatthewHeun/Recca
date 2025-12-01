# Extract an `S_units` matrix from a tidy data frame

The `.tidydf` should be grouped as needed.

## Usage

``` r
S_units_from_tidy(
  .tidydf,
  Product = IEATools::iea_cols$product,
  Unit = IEATools::iea_cols$unit,
  S_units = Recca::psut_cols$S_units
)
```

## Arguments

- .tidydf:

  the data frame from which an `S_units` matrix is to be formed

- Product:

  the name of the `Product` column in `.tidydf`. Default is "`Product`".

- Unit:

  the name of the `Unit` column in `.tidydf`. Default is "`Unit`".

- S_units:

  the name of the `S_units` column to be added to `.tidydf`. Default is
  "`S_unit`".

## Value

a data frame containing grouping variables and a new `S_unit` column

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(matsindf)
library(Recca)
UKEnergy2000tidy |>
  group_by(Country, Year, EnergyType, LastStage) |>
  S_units_from_tidy()
#>   Country Year EnergyType LastStage
#> 1     GBR 2000          E     Final
#> 2     GBR 2000          E  Services
#> 3     GBR 2000          E    Useful
#> 4     GBR 2000          X  Services
#>                                                                                                                                                                                                                                                                                                      S_units
#> 1                                                                                                                                                                                                                                                                         1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> 2 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
#> 3                                                                                                                                                                                                                                                             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> 4 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
```
