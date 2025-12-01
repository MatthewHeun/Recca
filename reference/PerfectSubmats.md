# Example energy conversion chain to demonstrate perfect substitution

A dataset containing an example energy conversion chain for the purpose
of demonstrating changes to industry inputs where the inputs are perfect
substitutes.

## Usage

``` r
PerfectSubmats
```

## Format

A data frame with 20 rows and 10 variables:

- Country:

  country, (Example, only one country)

- Year:

  year, 2000 (only one year as an example)

- EnergyType:

  E.ktoe (for energy) or X.ktoe (for exergy)

- LastStage:

  tells the final stage of the energy conversion chain: services is the
  only entry here

- matrix.name:

  gives the name of the matrix

- matrix:

  gives use (U), make (V), final demand (Y), r_EIOU, and S_units
  matrices

## Details

`PerfectSubmats` gives the use (`U`), make (`V`), and final demand (`Y`)
matrices as a single column in a data frame. These data are in matsindf
format.
