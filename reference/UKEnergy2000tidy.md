# Energy consumption in the UK in 2000

A dataset containing approximations to some of the energy flows in the
UK in the year 2000. These data first appeared as the example in Figures
3, 4, 6, 7, 10, 11, B.1, and B.2 of M.K. Heun, A. Owen, and P. E.
Brockway. A physical supply-use table framework for energy analysis on
the energy conversion chain. Applied Energy, 226:1134–1162, Sep 2018.

## Usage

``` r
UKEnergy2000tidy
```

## Format

A data frame with 186 rows and 9 variables:

- Country:

  country, GB (Great Britain, only one country)

- Year:

  year, 2000 (only one year)

- LedgerSide:

  Supply or Consumption

- FlowAggregationPoint:

  tells where each row should be aggregated

- EnergyType:

  E.ktoe (for energy) or X.ktoe (for exergy)

- LastStage:

  tells the final stage of the energy conversion chain: final, useful,
  or services

- Flow:

  the Industry or Sector involved in this flow

- Product:

  the energy product involved in this flow

- Edot:

  value of the energy, exergy, or service flow in ktoe

- Unit:

  unit in which quantity is expressed

## Source

[doi:10.1016/j.apenergy.2018.05.109](https://doi.org/10.1016/j.apenergy.2018.05.109)

## Details

`UKEnergy2000tidy` gives each non-zero entry in `UKEnergy2000mats` as a
single column in a data frame. These data are in tidy format.
