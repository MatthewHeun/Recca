# Recca

## Introduction

`Recca` (`R` Energy Conversion Chain Analysis) is an `R` package that
enables energy and exergy analysis of energy conversion chains. `Recca`
makes extensive use of a matrix-based Physical Supply Use Table (PSUT)
analysis technique that first appeared in [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109). This vignette
walks through many of the calculations from [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109), guided by
section numbers from the paper. We begin by discussing the design
philosophy of the `Recca` package, followed by a discussion of the PSUT
matrices that comprise an energy conversion chain (ECC). Thereafter,
calculation of the input-output structure of an ECC is demonstrated.
Finally, advanced calculations are shown, including changes in final
demand, net energy analysis, industry efficiencies, and energy
footprints.

## Design philosophy

The functions in `Recca` are flexibly designed and useful in many
situations. `Recca`’s flexibility stems from its extensive use of the
`matsbyname` and `matsindf` packages under the hood. Inputs to most
`Recca` functions can be any one of:

1.  matrices (with individual matrices as arguments to `Recca`
    functions),
2.  a list as the first argument (with names in the list indicating
    argument names), or
3.  a data frame (with names of data frame columns, as strings, as
    arguments to `Recca` functions).

Outputs are either named matrices in a list (for 1 and 2 above) or named
columns appended to the input data frame (for 3 above).

Argument names for matrices follow a standard nomenclature. It is
recommended that the default matrix names be used whenever possible,
thereby allowing cleaner code.

## PSUT matrices

(Reference [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109), Section 2.2.2.)

For the examples that follow, we’ll use the `UKEnergy2000tidy` data
frame. Each row of `UKEnergy2000tidy` data frame represents another
datum for a portion of the ECC for the UK in 2000. `UKEnergy2000tidy` is
in a format similar to data from the IEA or other organizations.

For data in the format of `UKEnergy2000tidy`, we can create `S_units`
matrices for each grouping using the
[`S_units_from_tidy()`](https://matthewheun.github.io/Recca/reference/S_units_from_tidy.md)
function.

``` r
library(tibble)
S_units <- UKEnergy2000tidy %>%
  dplyr::group_by(Country, Year, EnergyType, LastStage) %>%
  S_units_from_tidy()
tibble::glimpse(S_units)
#> Rows: 4
#> Columns: 5
#> $ Country    <chr> "GBR", "GBR", "GBR", "GBR"
#> $ Year       <dbl> 2000, 2000, 2000, 2000
#> $ EnergyType <chr> "E", "E", "E", "X"
#> $ LastStage  <chr> "Final", "Services", "Useful", "Services"
#> $ S_units    <list> <<matrix[12 x 1]>>, <<matrix[20 x 5]>>, <<matrix[16 x 1]>>,…
```

And we can identify which entries belong in the resource ($\mathbf{R}$),
make ($\mathbf{V}$), use ($\mathbf{U}$), and final demand ($\mathbf{Y}$)
matrices with the
[`IEATools::add_psut_matnames()`](https://matthewheun.github.io/IEATools/reference/add_psut_matnames.html)
and
[`IEATools::add_row_col_meta()`](https://matthewheun.github.io/IEATools/reference/add_row_col_meta.html)
functions.

``` r
WithNames <- UKEnergy2000tidy %>%
  # Add a column indicating the matrix in which this entry belongs (U, V, or Y).
  IEATools::add_psut_matnames() %>%
  # Add metadata columns for row names, column names, row types, and column types.
  IEATools::add_row_col_meta() %>% 
  # Eliminate columns we no longer need
  dplyr::select(-LedgerSide, -FlowAggregationPoint, -Flow, -Product) %>%
  dplyr::mutate(
    # Ensure that all energy values are positive, as required for analysis.
    Edot = abs(Edot)
  )
head(WithNames)
#>   Country Year EnergyType LastStage  Edot Unit matnames             rownames
#> 2     GBR 2000          E     Final 50000 ktoe        R Resources [of Crude]
#> 3     GBR 2000          E     Final 43000 ktoe        R    Resources [of NG]
#> 4     GBR 2000          E     Final 43000 ktoe        V    Gas wells & proc.
#> 5     GBR 2000          E     Final 50000 ktoe        V           Oil fields
#> 6     GBR 2000          E     Final 47500 ktoe        V          Crude dist.
#> 7     GBR 2000          E     Final 41000 ktoe        V             NG dist.
#>              colnames rowtypes coltypes
#> 2               Crude Industry  Product
#> 3                  NG Industry  Product
#> 4     NG [from Wells] Industry  Product
#> 5 Crude [from Fields] Industry  Product
#> 6  Crude [from Dist.] Industry  Product
#> 7     NG [from Dist.] Industry  Product
```

After identifying the matrices, rownames, colnames, rowtypes, and
coltypes, we can collapse all data to matrices and add a unit summation
matrix (`S_units`).

``` r
AsMats <- WithNames %>%
  # Collapse to matrices using functions in the matsindf package
  dplyr::group_by(Country, Year, EnergyType, LastStage, matnames) %>%
  matsindf::collapse_to_matrices(matnames = "matnames", matvals = "Edot",
                       rownames = "rownames", colnames = "colnames",
                       rowtypes = "rowtypes", coltypes = "coltypes") %>%
  dplyr::rename(matrix.name = matnames, matrix = Edot) %>%
  tidyr::spread(key = matrix.name, value = matrix) %>% 
  # Do a little more cleanup
  dplyr::mutate(
    # Create full U matrix
    U = matsbyname::sum_byname(U_feed, U_EIOU),
    # Create r_EIOU, a matrix that identifies the ratio of EIOU to other energy consumed.
    r_EIOU = matsbyname::quotient_byname(U_EIOU, U),
    r_EIOU = matsbyname::replaceNaN_byname(r_EIOU, val = 0)
  ) %>% 
  dplyr::select(-U_EIOU, -U_feed) %>%
  # Add S_units matrices
  dplyr::left_join(S_units, by = c("Country", "Year", "EnergyType", "LastStage")) %>%
  tidyr::gather(key = matrix.name, value = matrix, R, U, V, Y, r_EIOU, S_units)
tibble::glimpse(AsMats)
#> Rows: 24
#> Columns: 6
#> $ Country     <chr> "GBR", "GBR", "GBR", "GBR", "GBR", "GBR", "GBR", "GBR", "G…
#> $ Year        <dbl> 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000…
#> $ EnergyType  <chr> "E", "E", "E", "X", "E", "E", "E", "X", "E", "E", "E", "X"…
#> $ LastStage   <chr> "Final", "Services", "Useful", "Services", "Final", "Servi…
#> $ matrix.name <chr> "R", "R", "R", "R", "U", "U", "U", "U", "V", "V", "V", "V"…
#> $ matrix      <list> <<matrix[2 x 2]>>, <<matrix[2 x 2]>>, <<matrix[2 x 2]>>, …
```

The `AsMats` data frame is essentially the same as the
[`Recca::UKEnergy2000mats`](https://matthewheun.github.io/Recca/reference/UKEnergy2000mats.md)
data frame. The remainder of this vignette uses the `UKEnergy2000mats`
data frame.

## I-O structure

(Reference [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109), Section 2.2.4.)

### With individual matrices

To determine the I-O structure of an ECC, use the
[`calc_io_mats()`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md)
function.

``` r
library(tidyr)
mats <- UKEnergy2000mats %>% 
  tidyr::spread(key = matrix.name, value = matrix) %>% 
  # Put rows in a natural order
  dplyr::mutate(
    LastStage = factor(LastStage, levels = c("Final", "Useful", "Services")),
    EnergyType = factor(EnergyType, levels = c("E", "X"))
  ) %>% 
  dplyr::arrange(LastStage, EnergyType)
# Use the calc_io_mats function with individual matrices, 
# each taken from the first row of the UKEnergy2000mats data frame.
R <- mats$R[[1]]
U <- mats$U[[1]]
U_feed = mats$U_feed[[1]]
V <- mats$V[[1]]
Y <- mats$Y[[1]]
S_units <- mats$S_units[[1]]
IO_list <- calc_io_mats(R = R, U = U, U_feed = U_feed, V = V, Y = Y, S_units = S_units)
```

Most `Recca` functions return a list when called with individual
matrices as arguments. The
[`calc_io_mats()`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md)
function gives several I-O matrices in its returned list.

``` r
class(IO_list)
#> [1] "list"
names(IO_list)
#>  [1] "y"          "q"          "f"          "g"          "h"         
#>  [6] "r"          "W"          "Z"          "K"          "C"         
#> [11] "D"          "A"          "O"          "L_pxp"      "L_ixp"     
#> [16] "Z_feed"     "K_feed"     "A_feed"     "L_pxp_feed" "L_ixp_feed"
IO_list[["y"]]
#>                     Industry
#> Diesel [from Dist.]    14750
#> Elect [from Grid]       6000
#> NG [from Dist.]        25000
#> Petrol [from Dist.]    26000
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
```

The same calculations can be performed by supplying a named list to
`Recca` functions. In this approach, all original data are returned in
the list. So in this case, matrices $\mathbf{U}$, $\mathbf{V}$,
$\mathbf{Y}$, and
$\mathbf{S}_{\mathbf{u}\mathbf{n}\mathbf{i}\mathbf{t}\mathbf{s}}$ are
also returned from the
[`calc_io_mats()`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md)
function. When a list is supplied to a `Recca` function in the
`.sutmats` argument, most other input arguments must be strings that
identify the names of appropriate entries in the `.sutmats` list
containing named vectors or matrices. Helpfully, the default values for
other input arguments conform to a standard nomenclature. When using the
standard nomenclature, most `Recca` functions can use the default
arguments for input and output items in the list.

``` r
IO_from_list <- calc_io_mats(list(R = R, U = U, U_feed = U_feed, V = V, Y = Y, S_units = S_units))
class(IO_from_list)
#> [1] "list"
names(IO_from_list)
#>  [1] "R"          "U"          "U_feed"     "V"          "Y"         
#>  [6] "S_units"    "y"          "q"          "f"          "g"         
#> [11] "h"          "r"          "W"          "Z"          "K"         
#> [16] "C"          "D"          "A"          "O"          "L_pxp"     
#> [21] "L_ixp"      "Z_feed"     "K_feed"     "A_feed"     "L_pxp_feed"
#> [26] "L_ixp_feed"
IO_from_list[["y"]]
#>                     Industry
#> Diesel [from Dist.]    14750
#> Elect [from Grid]       6000
#> NG [from Dist.]        25000
#> Petrol [from Dist.]    26000
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
```

### From a `matsindf`-style data frame

Most `Recca` functions can also operate on a `matsindf`-style data
frame. (A `matsindf`-style data frame has matrices in cells of a data
frame. See the `matsindf` package for additional information.) When a
data frame is supplied to a `Recca` function in the `.sutmats` argument,
most other input arguments must be strings that identify the names of
appropriate columns in `.sutmats` containing named vectors or matrices.
Helpfully, the default values for other input arguments conform to a
standard nomenclature. When using the standard nomenclature, most
`Recca` functions can use the default arguments for input and output
columns. This approach yields very clean piped code, as shown below.

To illustrate the above features of `Recca` functions, we’ll apply the
`calc_io_mats` function to the entire `UKEnergy2000mats` data frame,
calculating appropriate I-O matrices for each row. Used in this way,
`Recca` functions act like specialized
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
functions, with new columns added to the right side of the data frame
supplied to the `.sutmats` argument.

``` r
IO_df <- mats %>% calc_io_mats()
```

By inspecting `IO_df`, we can see, for example, that one $\mathbf{y}$
vector was calculated for each of the four rows of `mats`. The same is
true for all I-O matrices calculated by `calc_io_mats`.

``` r
class(IO_df)
#> [1] "tbl_df"     "tbl"        "data.frame"
names(IO_df)
#>  [1] "Country"    "Year"       "EnergyType" "LastStage"  "R"         
#>  [6] "r_EIOU"     "S_units"    "U"          "U_EIOU"     "U_feed"    
#> [11] "V"          "Y"          "y"          "q"          "f"         
#> [16] "g"          "h"          "r"          "W"          "Z"         
#> [21] "K"          "C"          "D"          "A"          "O"         
#> [26] "L_pxp"      "L_ixp"      "Z_feed"     "K_feed"     "A_feed"    
#> [31] "L_pxp_feed" "L_ixp_feed"
glimpse(IO_df)
#> Rows: 4
#> Columns: 32
#> $ Country    <chr> "GBR", "GBR", "GBR", "GBR"
#> $ Year       <dbl> 2000, 2000, 2000, 2000
#> $ EnergyType <fct> E, E, E, X
#> $ LastStage  <fct> Final, Useful, Services, Services
#> $ R          <list> <<matrix[2 x 2]>>, <<matrix[2 x 2]>>, <<matrix[2 x 2]>>, <<…
#> $ r_EIOU     <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>, <<matrix[17 x 17]>…
#> $ S_units    <list> <<matrix[12 x 1]>>, <<matrix[16 x 1]>>, <<matrix[20 x 5]>>,…
#> $ U          <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>, <<matrix[17 x 17]>…
#> $ U_EIOU     <list> <<matrix[7 x 8]>>, <<matrix[3 x 8]>>, <<matrix[3 x 8]>>, <…
#> $ U_feed     <list> <<matrix[9 x 9]>>, <<matrix[12 x 13]>>, <<matrix[16 x 17]>…
#> $ V          <list> <<matrix[9 x 10]>>, <<matrix[13 x 14]>>, <<matrix[17 x 18]…
#> $ Y          <list> <<matrix[4 x 2]>>, <<matrix[4 x 2]>>, <<matrix[4 x 2]>>, <…
#> $ y          <list> <<matrix[4 x 1]>>, <<matrix[4 x 1]>>, <<matrix[4 x 1]>>, <…
#> $ q          <list> <<matrix[12 x 1]>>, <<matrix[16 x 1]>>, <<matrix[20 x 1]>>…
#> $ f          <list> <<matrix[9 x 1]>>, <<matrix[13 x 1]>>, <<matrix[17 x 1]>>,…
#> $ g          <list> <<matrix[9 x 1]>>, <<matrix[13 x 1]>>, <<matrix[17 x 1]>>,…
#> $ h          <list> <<matrix[2 x 1]>>, <<matrix[2 x 1]>>, <<matrix[2 x 1]>>, <…
#> $ r          <list> <<matrix[2 x 1]>>, <<matrix[2 x 1]>>, <<matrix[2 x 1]>>, <…
#> $ W          <list> <<matrix[12 x 9]>>, <<matrix[16 x 13]>>, <<matrix[20 x 17]…
#> $ Z          <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>, <<matrix[17 x 17]…
#> $ K          <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>, NA, NA
#> $ C          <list> <<matrix[10 x 9]>>, <<matrix[14 x 13]>>, <<matrix[18 x 17]…
#> $ D          <list> <<matrix[9 x 12]>>, <<matrix[13 x 16]>>, <<matrix[17 x 20]…
#> $ A          <list> <<matrix[12 x 12]>>, <<matrix[13 x 16]>>, <<matrix[17 x 20…
#> $ O          <list> <<matrix[2 x 2]>>, <<matrix[2 x 2]>>, <<matrix[2 x 2]>>, <…
#> $ L_pxp      <list> <<matrix[12 x 12]>>, <<matrix[16 x 16]>>, <<matrix[20 x 20…
#> $ L_ixp      <list> <<matrix[9 x 12]>>, <<matrix[13 x 16]>>, <<matrix[17 x 20]…
#> $ Z_feed     <list> <<matrix[9 x 9]>>, <<matrix[12 x 13]>>, <<matrix[16 x 17]>…
#> $ K_feed     <list> <<matrix[9 x 9]>>, <<matrix[12 x 13]>>, NA, NA
#> $ A_feed     <list> <<matrix[9 x 12]>>, <<matrix[12 x 16]>>, <<matrix[16 x 20]…
#> $ L_pxp_feed <list> <<matrix[12 x 12]>>, <<matrix[16 x 16]>>, <<matrix[20 x 20…
#> $ L_ixp_feed <list> <<matrix[9 x 12]>>, <<matrix[13 x 16]>>, <<matrix[17 x 20]…
IO_df[["y"]][[1]]
#>                     Industry
#> Diesel [from Dist.]    14750
#> Elect [from Grid]       6000
#> NG [from Dist.]        25000
#> Petrol [from Dist.]    26000
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
IO_df[["y"]][[4]]
#>                                 Industry
#> Freight [tonne-km/year]     1.429166e+11
#> Illumination [lumen-hrs/yr] 5.000000e+14
#> Passenger [passenger-km/yr] 5.000000e+11
#> Space heating [m3-K]        7.500000e+10
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
```

For the remainder of this vignette, operations will be performed on the
entire `UKEnergy2000mats` data frame. But readers should remember that
functions can be called on named lists or individual matrices, as well.

## Changes in final demand

(Reference [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109), Section 2.2.5.)

One of the first applications of input-output analysis was estimating
changes in industry outputs that would be required to meet new final
demand. `Recca` allows similar calculations on energy conversion chains
with the function
[`new_Y()`](https://matthewheun.github.io/Recca/reference/new_Y.md).
Arguments to
[`new_Y()`](https://matthewheun.github.io/Recca/reference/new_Y.md)
include matrices that describe the input-output structure of the ECC and
`Y_prime`, the new final demand matrix.
[`new_Y()`](https://matthewheun.github.io/Recca/reference/new_Y.md)
calculates `U_prime` and `V_prime` matrices which represent the ECC that
would be required to meet the new final demand represented by `Y_prime`.

``` r
Double_demand <- IO_df %>% 
  dplyr::mutate(
    Y_prime = matsbyname::hadamardproduct_byname(2, Y)
  ) %>% 
  new_Y()
names(Double_demand)
#>  [1] "Country"      "Year"         "EnergyType"   "LastStage"    "R"           
#>  [6] "r_EIOU"       "S_units"      "U"            "U_EIOU"       "U_feed"      
#> [11] "V"            "Y"            "y"            "q"            "f"           
#> [16] "g"            "h"            "r"            "W"            "Z"           
#> [21] "K"            "C"            "D"            "A"            "O"           
#> [26] "L_pxp"        "L_ixp"        "Z_feed"       "K_feed"       "A_feed"      
#> [31] "L_pxp_feed"   "L_ixp_feed"   "Y_prime"      "R_prime"      "U_prime"     
#> [36] "U_feed_prime" "U_EIOU_prime" "r_EIOU_prime" "V_prime"
IO_df[["Y"]][[1]][ , c(1,2)]
#>                     Transport Residential
#> Diesel [from Dist.]     14750           0
#> Elect [from Grid]           0        6000
#> NG [from Dist.]             0       25000
#> Petrol [from Dist.]     26000           0
Double_demand[["Y_prime"]][[1]]
#>                     Residential Transport
#> Diesel [from Dist.]           0     29500
#> Elect [from Grid]         12000         0
#> NG [from Dist.]           50000         0
#> Petrol [from Dist.]           0     52000
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
IO_df[["U"]][[1]][ , c("Crude dist.", "Diesel dist.")]
#>                     Crude dist. Diesel dist.
#> Crude                         0            0
#> Crude [from Dist.]          500            0
#> Crude [from Fields]       47500            0
#> Diesel                        0        15500
#> Diesel [from Dist.]          25          350
#> Elect                         0            0
#> Elect [from Grid]            25            0
#> NG                            0            0
#> NG [from Dist.]               0            0
#> NG [from Wells]               0            0
#> Petrol                        0            0
#> Petrol [from Dist.]           0            0
Double_demand[["U_prime"]][[1]][ , c("Crude dist.", "Diesel dist.")]
#>                     Crude dist. Diesel dist.
#> Crude                         0            0
#> Crude [from Dist.]         1000            0
#> Crude [from Fields]       95000            0
#> Diesel                        0        31000
#> Diesel [from Dist.]          50          700
#> Elect                         0            0
#> Elect [from Grid]            50            0
#> NG                            0            0
#> NG [from Dist.]               0            0
#> NG [from Wells]               0            0
#> Petrol                        0            0
#> Petrol [from Dist.]           0            0
```

See the vignette for the [`new_*()`
functions](https://matthewheun.github.io/Recca/articles/new-functions.md)
for more details.

## Net energy analysis

(Reference [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109), Section 3.1.)

The energy production system itself consumes energy, and important
metrics for energy conversion chain industries are energy return ratios
(ERRs). `Recca` provides a function to calculate three ERRs, a gross
energy ratio (GER), a net energy ratio (NER), and the ratio of NER to
GER. GER is commonly called energy return on investment (EROI). (See
[Brandt & Dale (2011)](https://doi.org/10.3390/en4081211).) These ERRs
can be calculated for a variety of system boundaries. ERRs for the
$\gamma$ system boundary can be calculated readily using the
[`calc_ERRs_gamma()`](https://matthewheun.github.io/Recca/reference/calc_ERRs_gamma.md)
function. All three ERRs are calculated at the same time. The ERRs are
`NA` for industries in which inputs or outputs are unit-inhomogeneous.
The ERRs are `Inf` for industries that make an energy product without
consuming any energy from another processing chain (such as the Elect.
grid). The results below are identical to Fig. 6 in [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109).

``` r
ERRs <- IO_df %>% 
  calc_ERRs_gamma()
ERRs$ger_gamma[[1]]
#>                    ger_gamma
#> Crude dist.        86.363636
#> Diesel dist.       44.285714
#> Elect. grid              Inf
#> Gas wells & proc.  20.722892
#> NG dist.          820.000000
#> Oil fields         19.417476
#> Oil refineries      9.261084
#> Petrol dist.       35.333333
#> Power plants       64.000000
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
ERRs$ner_gamma[[1]]
#>                    ner_gamma
#> Crude dist.        85.363636
#> Diesel dist.       43.285714
#> Elect. grid              Inf
#> Gas wells & proc.  19.722892
#> NG dist.          819.000000
#> Oil fields         18.417476
#> Oil refineries      8.261084
#> Petrol dist.       34.333333
#> Power plants       63.000000
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
ERRs$r_gamma[[1]]
#>                     r_gamma
#> Crude dist.       0.9884211
#> Diesel dist.      0.9774194
#> Elect. grid             NaN
#> Gas wells & proc. 0.9517442
#> NG dist.          0.9987805
#> Oil fields        0.9485000
#> Oil refineries    0.8920213
#> Petrol dist.      0.9716981
#> Power plants      0.9843750
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
```

## Efficiencies

(Reference [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109), Section 3.2.)

The efficiency of every industry in the ECC can be calculated quickly
with the
[`calc_eta_i()`](https://matthewheun.github.io/Recca/reference/calc_eta_i.md)
function, which creates a column named `eta_i` (by default) at the right
of the data frame. If a particular ECC has industries whose inputs or
outputs are unit inhomogeneous, the `eta_i` vector will have `NA` values
in the appropriate places. The results below are identical to Fig. 9 in
[Heun et al. (2018)](https://doi.org/10.1016/j.apenergy.2018.05.109).

``` r
etas <- IO_df %>% 
  calc_eta_i()
names(etas)
#>  [1] "Country"    "Year"       "EnergyType" "LastStage"  "R"         
#>  [6] "r_EIOU"     "S_units"    "U"          "U_EIOU"     "U_feed"    
#> [11] "V"          "Y"          "y"          "q"          "f"         
#> [16] "g"          "h"          "r"          "W"          "Z"         
#> [21] "K"          "C"          "D"          "A"          "O"         
#> [26] "L_pxp"      "L_ixp"      "Z_feed"     "K_feed"     "A_feed"    
#> [31] "L_pxp_feed" "L_ixp_feed" "eta_i"
etas[["eta_i"]][[1]]
#>                       eta_i
#> Crude dist.       0.9885536
#> Diesel dist.      0.9779180
#> Elect. grid       0.9804688
#> Gas wells & proc. 0.9539656
#> NG dist.          0.9987820
#> Oil fields        0.9510223
#> Oil refineries    0.9025444
#> Petrol dist.      0.9724771
#> Power plants      0.3975155
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "eta_i"
etas[["eta_i"]][[3]] # NAs indicate inhomogeneous units on inputs or outputs.
#>                       eta_i
#> Car engines       0.1154000
#> Cars                     NA
#> Crude dist.              NA
#> Diesel dist.             NA
#> Elect. grid       0.9804688
#> Furnaces          0.8000000
#> Gas wells & proc. 0.9518282
#> Homes                    NA
#> Light fixtures    0.2000000
#> NG dist.                 NA
#> Oil fields        0.9485771
#> Oil refineries    0.8921933
#> Petrol dist.             NA
#> Power plants      0.3975155
#> Rooms                    NA
#> Truck engines     0.1196000
#> Trucks                   NA
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "eta_i"
```

## Energy footprints

(Reference [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109), Section 3.3.)

Final demand for energy contains embodied primary energy, the sum of all
primary energy consumed and wasted throughout the ECC in the process of
satisfying that final demand. `Recca` provides two functions to
calculate embodied primary energy and important ratios, namely
[`calc_embodied_mats()`](https://matthewheun.github.io/Recca/reference/calc_embodied_mats.md)
and
[`calc_embodied_etas()`](https://matthewheun.github.io/Recca/reference/calc_embodied_etas.md).

The function
[`calc_embodied_mats()`](https://matthewheun.github.io/Recca/reference/calc_embodied_mats.md)
calculates embodied energy in final demand products ($p$) and final
demand sectors ($s$).

Outputs from
[`calc_embodied_mats()`](https://matthewheun.github.io/Recca/reference/calc_embodied_mats.md)
include the following matrices:

|                                                                                                                                 Matrix | Description (rows$\times$columns)                                                                                                                                          |
|---------------------------------------------------------------------------------------------------------------------------------------:|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|                                                                                         $\mathbf{G} = \mathbf{G}_{R} + \mathbf{G}_{V}$ | Industry and resource stocks output requirements for final demand ($(r + i) \times p$)                                                                                     |
|                         $\mathbf{G}_{R} = \mathbf{R}{\widehat{\mathbf{q}}}^{- 1}\underset{p \times p}{\mathbf{L}}\widehat{\mathbf{y}}$ | Resource stocks output requirements for final demand ($r \times p$)                                                                                                        |
|                                                               $\mathbf{G}_{V} = \underset{i \times p}{\mathbf{L}}\widehat{\mathbf{y}}$ | Industry output requirements for final demand ($i \times p$)                                                                                                               |
|                                                                                         $\mathbf{H} = \mathbf{H}_{R} + \mathbf{H}_{V}$ | Industry and resource stocks output requirements for final demand sectors ($(r + i) \times s$)                                                                             |
|                                   $\mathbf{H}_{R} = \mathbf{R}{\widehat{\mathbf{q}}}^{- 1}\underset{p \times p}{\mathbf{L}}\mathbf{Y}$ | Resource stocks output requirements for final demand sectors ($r \times s$)                                                                                                |
|                                                                         $\mathbf{H}_{V} = \underset{i \times p}{\mathbf{L}}\mathbf{Y}$ | Industry output requirements for final demand sectors ($i \times s$)                                                                                                       |
| $\mathbf{E} = \left\lbrack (\mathbf{R} + \mathbf{V})^{T} - \mathbf{U}_{feed} \right\rbrack{\widehat{(\mathbf{r} + \mathbf{g})}}^{- 1}$ | Energy or services produced ($+$) or consumed ($-$) per unit output by industries (in columns) ($p \times i$)                                                              |
|                                                                                              $\mathbf{r} = \bar{\mathbf{R}}\mathbf{i}$ | Row sums of $\mathbf{R}$ matrix; works only when $\mathbf{R}$ is unit-homogeneous                                                                                          |
|                                                                                              $\mathbf{g} = \bar{\mathbf{V}}\mathbf{i}$ | Row sums of $\mathbf{V}$ matrix; works only when $\mathbf{V}$ is unit-homogeneous                                                                                          |
|                                                                                                                       $\mathbf{e}_{i}$ | Rows of $\mathbf{E}$; subscript $i$ indicates energy products                                                                                                              |
|                                                                                  $\mathbf{Q}_{i} = \widehat{\mathbf{e}_{i}}\mathbf{G}$ | Sources (positive entries) and consumption (negative entries) by industries (in rows) of embodied energy products (subscript $i$) in consumed energy products (in columns) |
|                                                                                                                   $\mathbf{Q}_{i}^{+}$ | $\mathbf{Q}_{i}$ with negative entries set to zero                                                                                                                         |
|                                                                                                         $\mathbf{i}\mathbf{Q}_{i}^{+}$ | Column sums (to form row vectors) of $\mathbf{Q}_{i}^{+}$ (embodied product $\times$ embodying products                                                                    |
|                                                                                                                       $\mathbf{M}_{p}$ | Each row is one $\mathbf{i}\mathbf{Q}_{i}^{+}$ to show embodied energy products in each embodying energy product (embodied products $\times$ embodying products)           |
|                                                                $\mathbf{M}_{s} = \mathbf{M}_{p}{\widehat{\mathbf{q}}}^{- 1}\mathbf{Y}$ | Embodied energy products consumed by final demand sectors (embodied products $\times$ consuming final demand sectors)                                                      |

[`calc_embodied_mats()`](https://matthewheun.github.io/Recca/reference/calc_embodied_mats.md)
also calculates $\mathbf{F}_{footprint}$ and $\mathbf{F}_{ef\! fects}$
matrices, which answer the questions (respectively) “What is the
fractional composition of embodied energy of each final demand energy
type?” and “What is the fractional destination of a given upstream
energy product?” The calculations can be performed for each final demand
product or each final demand sector. The following table describes these
matrices.

|                                                                                              Matrix | Description                                                                                                                                                                         |
|----------------------------------------------------------------------------------------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|  $\mathbf{F}_{footprint,p} = \mathbf{M}_{p}\left( \widehat{\mathbf{i}\mathbf{M}_{p}} \right)^{- 1}$ | Each final demand product (columns) contains embodied energy. On a fractional basis, where does that embodied energy come from (rows)? Columns sum to 1.                            |
| $\mathbf{F}_{ef\! fects,p} = \left( \widehat{\mathbf{M}_{p}}\mathbf{i} \right)^{- 1}\mathbf{M}_{p}$ | Each upstream energy product (rows) becomes embodied somewhere. On a fractional basis, where does that upstream energy become embodied (columns)? Rows sum to 1.                    |
|  $\mathbf{F}_{footprint,s} = \mathbf{M}_{s}\left( \widehat{\mathbf{i}\mathbf{M}_{s}} \right)^{- 1}$ | Each final demand sector (columns) consumes embodied energy. On a fractional basis, where does the consumed embodied energy come from (rows)? Columns sum to 1.                     |
| $\mathbf{F}_{ef\! fects,s} = \left( \widehat{\mathbf{M}_{s}}\mathbf{i} \right)^{- 1}\mathbf{M}_{s}$ | Each embodied upstream energy product (rows) is consumed by a final demand sector. On a fractional basis, where is that embodied upstream energy consumed (columns)? Rows sum to 1. |

Finally, embodied energy efficiencies can be calculated as final demand
energy divided by embodied energy. Again, efficiencies can be calculated
for each final demand product or each final demand sector. The
[`calc_embodied_etas()`](https://matthewheun.github.io/Recca/reference/calc_embodied_etas.md)
function does this computation.

|                                                                                                        Matrix | Description                                                                                               |
|--------------------------------------------------------------------------------------------------------------:|:----------------------------------------------------------------------------------------------------------|
|                            $\eta_{p} = \left( \widehat{\mathbf{G}^{T}\mathbf{s}_{r}} \right)^{- 1}\mathbf{y}$ | Final demand product (rows) divided by embodied primary energy of that final demand product               |
| $\eta_{s} = \left( \widehat{\mathbf{s}_{r}\mathbf{H}} \right)^{- 1}\left( {\mathbf{i}\mathbf{Y}} \right)^{T}$ | Energy consumed by final demand sector (rows) divided by embodied energy input to the final demand sector |

Note: $\mathbf{s}_{r}$ is a selection vector for resource industries.

``` r
primary_machine_names <- c("Resources - Crude", "Resources - NG")

embodied_mats <- IO_df %>% 
  dplyr::mutate(
    U_EIOU = matsbyname::hadamardproduct_byname(r_EIOU, U)
  ) %>%
  calc_embodied_mats() %>%
  calc_embodied_etas(primary_machine_names = primary_machine_names)
names(embodied_mats)
#>  [1] "Country"       "Year"          "EnergyType"    "LastStage"    
#>  [5] "R"             "r_EIOU"        "S_units"       "U"            
#>  [9] "U_EIOU"        "U_feed"        "V"             "Y"            
#> [13] "y"             "q"             "f"             "g"            
#> [17] "h"             "r"             "W"             "Z"            
#> [21] "K"             "C"             "D"             "A"            
#> [25] "O"             "L_pxp"         "L_ixp"         "Z_feed"       
#> [29] "K_feed"        "A_feed"        "L_pxp_feed"    "L_ixp_feed"   
#> [33] "G_V"           "G_R"           "G"             "H_V"          
#> [37] "H_R"           "H"             "E"             "M_p"          
#> [41] "M_s"           "F_footprint_p" "F_effects_p"   "F_footprint_s"
#> [45] "F_effects_s"   "eta_p"         "eta_s"
```

Figure 15 in [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109) shows
primary-to-services energetic efficiencies for the ECC in the 3rd row of
`IO_df`. The following code extracts those results. Rows of the vector
give final demand services and their units. The column gives
efficiencies in units of service per ktoe of energy.

``` r
embodied_mats$eta_p[[3]]
#>                             Industry
#> Freight [tonne-km/year]          Inf
#> Illumination [lumen-hrs/yr]      Inf
#> Passenger [passenger-km/yr]      Inf
#> Space heating [m3-K]             Inf
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
```

## Conclusion

This vignette demonstrated the use of the `Recca` package. `Recca`
provides many useful functions for analyzing energy conversion chains
within the PSUT framework first described in [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109).

## References

Brandt, Adam R., and Michael Dale. 2011. “A General Mathematical
Framework for Calculating Systems-Scale Efficiency of Energy Extraction
and Conversion: Energy Return on Investment (EROI) and Other Energy
Return Ratios.” *Energies* 4: 1211–45.
<https://doi.org/10.3390/en4081211>.

Heun, Matthew Kuperus, Anne Owen, and Paul E. Brockway. 2018. “A
Physical Supply-Use Table Framework for Energy Analysis on the Energy
Conversion Chain.” *Applied Energy* 226 (September): 1134–62.
<https://doi.org/10.1016/j.apenergy.2018.05.109>.
