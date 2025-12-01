# Energy consumption in the UK in 2000

A dataset containing approximations to some of the energy flows in the
UK in the year 2000. These data first appeared as the example in Figures
3, 4, 6, 7, 10, 11, B.1, and B.2 of M.K. Heun, A. Owen, and P. E.
Brockway. A physical supply-use table framework for energy analysis on
the energy conversion chain. Applied Energy, 226:1134–1162, Sep 2018.

## Usage

``` r
UKEnergy2000mats
```

## Format

A data frame with 12 rows and 6 variables:

- Country:

  country, GB (Great Britain, only one country)

- Year:

  year, 2000 (only one year)

- EnergyType:

  E.ktoe (for energy) or X.ktoe (for exergy)

- LastStage:

  tells the final stage of the energy conversion chain: final, useful,
  or services

- matrix.name:

  gives the name of the matrix

- matrix:

  gives use (U), make (V), final demand (Y), r_EIOU, and S_units
  matrices

## Source

[doi:10.1016/j.apenergy.2018.05.109](https://doi.org/10.1016/j.apenergy.2018.05.109)

## Details

`UKEnergy2000mats` gives the use (`U`), make (`V`), and final demand
(`Y`) matrices associated with `UKEnergy2000tidy`. These data are in
matsindf format.
