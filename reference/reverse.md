# Reverse an energy conversion chain

Leontief's original input-output analysis involved swimming "upstream"
to estimate the economy that would be needed if different final demand
were observed. But what if different resources were available? The
analysis is the same if resources become final demand (and vice versa)
and make becomes use (and vice versa). That is, the analysis is the same
if you're dealing with a reversed energy conversion chain (ECC). This
function performs that reversal.

## Usage

``` r
reverse(
  .sutmats = NULL,
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  R_rev = paste0(Recca::psut_cols$R, "_rev"),
  U_rev = paste0(Recca::psut_cols$U, "_rev"),
  V_rev = paste0(Recca::psut_cols$V, "_rev"),
  Y_rev = paste0(Recca::psut_cols$Y, "_rev")
)
```

## Arguments

- .sutmats:

  the input ECC

- R:

  The **R** matrix in the ECC to be reversed. (Default is "R".)

- U:

  The **U** matrix in the ECC to be reversed. (Default is "U".)

- V:

  The **V** matrix in the ECC to be reversed. (Default is "V".)

- Y:

  The **Y** matrix in the ECC to be reversed. (Default is "Y".)

- R_rev:

  The name of the **R** matrix in the reversed ECC. (Default is
  "R_rev".)

- U_rev:

  The name of the **U** matrix in the reversed ECC. (Default is
  "U_rev".)

- V_rev:

  The name of the **V** matrix in the reversed ECC. (Default is
  "V_rev".)

- Y_rev:

  The name of the **Y** matrix in the reversed ECC. (Default is
  "Y_rev".)

## Value

A reversed version of the ECC described by **R**, **U**, **V**, and
**Y**.

## Details

To reverse an ECC, the **R**, **U**, **V**, and **Y** matrices need to
be transposed and swapped: **R** with **Y** and **U** with **V**. This
function performs those operations.

## Examples

``` r
library(dplyr)
library(Recca)
library(tidyr)
mats <- UKEnergy2000mats %>%
  spread(key = "matrix.name", value = "matrix") %>%
  reverse()
mats$R_rev[[1]]
#>             Diesel [from Dist.] Elect [from Grid] NG [from Dist.]
#> Transport                 14750                 0               0
#> Residential                   0              6000           25000
#>             Petrol [from Dist.]
#> Transport                 26000
#> Residential                   0
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
mats$U_rev[[1]]
#>                     Crude dist. Diesel dist. Elect. grid Gas wells & proc.
#> Crude [from Dist.]        47500            0           0                 0
#> Diesel [from Dist.]           0        15500           0                 0
#> Elect [from Grid]             0            0        6275                 0
#> NG [from Wells]               0            0           0             43000
#> NG [from Dist.]               0            0           0                 0
#> Crude [from Fields]           0            0           0                 0
#> Diesel                        0            0           0                 0
#> Petrol                        0            0           0                 0
#> Petrol [from Dist.]           0            0           0                 0
#> Elect                         0            0           0                 0
#>                     NG dist. Oil fields Oil refineries Petrol dist.
#> Crude [from Dist.]         0          0              0            0
#> Diesel [from Dist.]        0          0              0            0
#> Elect [from Grid]          0          0              0            0
#> NG [from Wells]            0          0              0            0
#> NG [from Dist.]        41000          0              0            0
#> Crude [from Fields]        0      50000              0            0
#> Diesel                     0          0          20500            0
#> Petrol                     0          0          26500            0
#> Petrol [from Dist.]        0          0              0        26500
#> Elect                      0          0              0            0
#>                     Power plants
#> Crude [from Dist.]             0
#> Diesel [from Dist.]            0
#> Elect [from Grid]              0
#> NG [from Wells]                0
#> NG [from Dist.]                0
#> Crude [from Fields]            0
#> Diesel                         0
#> Petrol                         0
#> Petrol [from Dist.]            0
#> Elect                       6400
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
mats$V_rev[[1]]
#>                   Crude Crude [from Dist.] Crude [from Fields] Diesel
#> Crude dist.           0                500               47500      0
#> Diesel dist.          0                  0                   0  15500
#> Elect. grid           0                  0                   0      0
#> Gas wells & proc.     0                  0                   0      0
#> NG dist.              0                  0                   0      0
#> Oil fields        50000                  0                2500      0
#> Oil refineries        0              47000                   0   5000
#> Petrol dist.          0                  0                   0      0
#> Power plants          0                  0                   0      0
#>                   Diesel [from Dist.] Elect Elect [from Grid]    NG
#> Crude dist.                        25     0                25     0
#> Diesel dist.                      350     0                 0     0
#> Elect. grid                         0  6400                 0     0
#> Gas wells & proc.                  50     0                25 43000
#> NG dist.                           25     0                25     0
#> Oil fields                         50     0                25     0
#> Oil refineries                      0     0                75     0
#> Petrol dist.                      250     0                 0     0
#> Power plants                        0     0               100     0
#>                   NG [from Dist.] NG [from Wells] Petrol Petrol [from Dist.]
#> Crude dist.                     0               0      0                   0
#> Diesel dist.                    0               0      0                   0
#> Elect. grid                     0               0      0                   0
#> Gas wells & proc.               0            2000      0                   0
#> NG dist.                        0           41000      0                   0
#> Oil fields                      0               0      0                   0
#> Oil refineries                  0               0      0                   0
#> Petrol dist.                    0               0  26500                 500
#> Power plants                16000               0      0                   0
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
mats$Y_rev[[1]]
#>       Resources [of Crude] Resources [of NG]
#> Crude                50000                 0
#> NG                       0             43000
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
```
