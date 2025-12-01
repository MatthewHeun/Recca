# Confirm that an IEA-style data frame conserves energy.

Energy balances are confirmed (within `tol`) for every combination of
grouping variables in `.ieatidydata`.

## Usage

``` r
verify_IEATable_energy_balance(
  .ieatidydata,
  LedgerSide = IEATools::iea_cols$ledger_side,
  energy = IEATools::iea_cols$e_dot,
  supply = "Supply",
  consumption = "Consumption",
  err = ".err",
  tol = 1e-06
)
```

## Arguments

- .ieatidydata:

  an IEA-style data frame containing grouping columns (typically
  `Country`, `Year`, `Product`, and others), a `LedgerSide` column, and
  an energy column (`E.ktoe`). `.ieatidydata` should be grouped prior to
  sending to this function.

- LedgerSide:

  the name of the column in `.ieatidydata` that contains ledger side
  information (a string). Default is "`LedgerSide`".

- energy:

  the name of the column in `.ieatidydata` that contains energy data (a
  string). Default is "`E.ktoe`".

- supply:

  the identifier for supply data in the `LedgerSide` column (a string).
  Default is "`Supply`".

- consumption:

  the identifier for consumption data in the `LedgerSide` column (a
  string). Default is "`Consumption`".

- err:

  the name of the error column in the output. Default is "`.err`".

- tol:

  the maximum amount by which Supply and Consumption can be out of
  balance

## Value

a data frame containing with grouping variables and an additional column
whose name is the value of `err`. The `err` column should be 0.

## Details

Be sure to group `.ieatidydata` prior to calling this function, as shown
in the example.

If energy is in balance for every group, a data frame with additional
column `err` is returned. If energy balance is not observed for one or
more of the groups, a warning is emitted.

## Examples

``` r
library(dplyr)
UKEnergy2000tidy %>%
  filter(LastStage %in% c("Final", "Useful")) %>%
  group_by(Country, Year, EnergyType, LastStage) %>%
  verify_IEATable_energy_balance(energy = IEATools::iea_cols$e_dot)
#> # A tibble: 2 × 7
#> # Groups:   Country, Year, EnergyType [1]
#>   Country  Year EnergyType LastStage ESupply EConsumption  .err
#>   <chr>   <dbl> <chr>      <chr>       <dbl>        <dbl> <dbl>
#> 1 GBR      2000 E          Final      71750        71750      0
#> 2 GBR      2000 E          Useful     25915.       25915.     0
```
