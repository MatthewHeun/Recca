# Example energy conversion chain to demonstrate perfect substitution

A dataset containing an example energy conversion chain for the purpose
of demonstrating changes to industry inputs where the inputs are perfect
substitutes.

## Usage

``` r
PerfectSubtidy
```

## Format

A data frame with 20 rows and 10 variables:

- Country:

  country, (Example, only one country)

- Year:

  year, 2000 (only one year as an example)

- LedgerSide:

  Supply or Consumption

- FlowAggregationPoint:

  tells where each row should be aggregated

- EnergyType:

  E (for energy) or X (for exergy)

- LastStage:

  tells the final stage of the energy conversion chain: services is the
  only entry here

- Flow:

  the Industry or Sector involved in this flow

- Product:

  the energy product involved in this flow

- Edot:

  value of the energy, exergy, or service flow in ktoe

- Unit:

  unit in which quantity is expressed

## Details

`PerfectSubtidy` gives each non-zero entry in `PerfectSubmats` as a
single column in a data frame. These data are in tidy format.
