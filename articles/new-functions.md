# new\_\* functions

## Introduction

`Recca` is an `R` package that builds upon input-output (I-O) analysis,
developed originally to analyze money flow through economies, to analyze
energy conversion chains (ECCs). Historically, input-output (I-O)
analysis has been used to analyze “what-if” scenarios involving changes
to the production structure of an economy. A common scenario asks “how
will the structure of intermediate industries (represented by
$\mathbf{U}$ and $\mathbf{V}$ matrices) change when final demand
(represented by the $\mathbf{Y}$ matrix) changes?” In the economic
arena, Leontief’s early work answered this question via a matrix
inversion that now bears his name, the “Leontief inverse ($\mathbf{L}$)”
(Leontief 1941). Analysts with an I-O background will know the famous
equation $\mathbf{L} = (\mathbf{I} - \mathbf{A})^{- 1}$.

The `Recca` package provides functions that perform several “what if”
analyses for the analyst. Each function has the name `new_*`, where `*`
indicates the changed matrix or portion of the ECC. Some functions have
the suffix `_ps` to indicate the functions assume perfect substitution
(`ps`) of industry inputs throughout the ECC. The `new_*` functions
return the resource ($\mathbf{R}$), use ($\mathbf{U}$), make
($\mathbf{V}$), and final demand ($\mathbf{Y}$) matrices that would be
present under the conditions given by arguments to the `new_*`
functions. Arguments for new or changed versions of vectors or matrices
are given the suffix `_prime` in the `new_*` functions. For example, the
changed version of the use matrix ($\mathbf{U}$) is called `U_prime`.

For the examples that follow, we’ll use one ECC from the
`UKEnergy2000mats` data frame which represents a portion of the primary
and final energy flows through the UK in the year 2000.

``` r
library(dplyr)
library(magrittr)
library(matsbyname)
library(matsindf)
library(Recca)
library(tidyr)
OurECC <- UKEnergy2000mats |> 
  dplyr::filter(LastStage == "Final") |> 
  tidyr::spread(key = "matrix.name", value = "matrix")
dplyr::glimpse(OurECC)
#> Rows: 1
#> Columns: 12
#> $ Country    <chr> "GBR"
#> $ Year       <dbl> 2000
#> $ EnergyType <chr> "E"
#> $ LastStage  <chr> "Final"
#> $ R          <list> <<matrix[2 x 2]>>
#> $ r_EIOU     <list> <<matrix[12 x 9]>>
#> $ S_units    <list> <<matrix[12 x 1]>>
#> $ U          <list> <<matrix[12 x 9]>>
#> $ U_EIOU     <list> <<matrix[7 x 8]>>
#> $ U_feed     <list> <<matrix[9 x 9]>>
#> $ V          <list> <<matrix[9 x 10]>>
#> $ Y          <list> <<matrix[4 x 2]>>
```

After a short discussion of the inputs to the `new_*` functions, each
type of analysis is described in turn.

## Inputs to `new_*` functions

Inputs to the `new_*` functions include matrices that describe the ECC
and matrices that describe the input-output structure of the ECC.

### Matrices that describe the ECC

The `new_*` functions take as inputs resource ($\mathbf{R}$), use
($\mathbf{U}$), make ($\mathbf{V}$), and final demand ($\mathbf{Y}$)
matrices that describe the ECC.

To look at the ECC, we can draw a Sankey diagram. A Sankey diagram for
the first row of the `UKEnergyMatsWithR` data frame is given below.

``` r
make_sankey(OurECC) |> 
  magrittr::extract2("Sankey") |> # Extracts the "Sankey" column
  magrittr::extract2(1) # Extracts the first row of the "Sankey" column
```

### Matrices that describe the input-output structure of the ECC

Each `new_*` function requires basic knowledge about the I-O structure
of the ECCs in the form of named vectors and matrices. Thus, the first
step in any “what-if” analysis is to calculate the input-output
structure of the energy conversion chain. The
[`calc_io_mats()`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md)
function is the simplest way to obtain the necessary I-O structure,
which appears as additional columns appended at the right of the
`OurECC` data frame in our example. (The names of the additional columns
are given by arguments to the
[`calc_io_mats()`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md)
function.)

``` r
IO_mats <- OurECC |> 
  # Obtain the I-O structure of the ECCs
  calc_io_mats()
# Note additional columns appended to the data frame.
dplyr::glimpse(IO_mats)
#> Rows: 1
#> Columns: 32
#> $ Country    <chr> "GBR"
#> $ Year       <dbl> 2000
#> $ EnergyType <chr> "E"
#> $ LastStage  <chr> "Final"
#> $ R          <list> <<matrix[2 x 2]>>
#> $ r_EIOU     <list> <<matrix[12 x 9]>>
#> $ S_units    <list> <<matrix[12 x 1]>>
#> $ U          <list> <<matrix[12 x 9]>>
#> $ U_EIOU     <list> <<matrix[7 x 8]>>
#> $ U_feed     <list> <<matrix[9 x 9]>>
#> $ V          <list> <<matrix[9 x 10]>>
#> $ Y          <list> <<matrix[4 x 2]>>
#> $ y          <list> <<matrix[4 x 1]>>
#> $ q          <list> <<matrix[12 x 1]>>
#> $ f          <list> <<matrix[9 x 1]>>
#> $ g          <list> <<matrix[9 x 1]>>
#> $ h          <list> <<matrix[2 x 1]>>
#> $ r          <list> <<matrix[2 x 1]>>
#> $ W          <list> <<matrix[12 x 9]>>
#> $ Z          <list> <<matrix[12 x 9]>>
#> $ K          <list> <<matrix[12 x 9]>>
#> $ C          <list> <<matrix[10 x 9]>>
#> $ D          <list> <<matrix[9 x 12]>>
#> $ A          <list> <<matrix[12 x 12]>>
#> $ O          <list> <<matrix[2 x 2]>>
#> $ L_pxp      <list> <<matrix[12 x 12]>>
#> $ L_ixp      <list> <<matrix[9 x 12]>>
#> $ Z_feed     <list> <<matrix[9 x 9]>>
#> $ K_feed     <list> <<matrix[9 x 9]>>
#> $ A_feed     <list> <<matrix[9 x 12]>>
#> $ L_pxp_feed <list> <<matrix[12 x 12]>>
#> $ L_ixp_feed <list> <<matrix[9 x 12]>>
```

With the I-O structure of the ECC determined (and represented by the
$\mathbf{q}$, $\mathbf{f}$, $\mathbf{h}$, $\mathbf{r}$ and $\mathbf{g}$
vectors and the $\mathbf{W}$, $\mathbf{Z}$, $\mathbf{K}$, $\mathbf{C}$,
$\mathbf{D}$, , $\mathbf{A}$,
$\mathbf{L}_{\mathbf{p}\mathbf{x}\mathbf{p}}$, and
$\mathbf{L}_{\mathbf{i}\mathbf{x}\mathbf{p}}$ matrices), we can use the
`new_*` functions to ask “what if” questions.

## What if final demand changes? Function new_Y()

The classic I-O question is “what if final demand ($\mathbf{Y}$)
changes?” `Recca` has a function for that:
[`new_Y()`](https://matthewheun.github.io/Recca/reference/new_Y.md). The
[`new_Y()`](https://matthewheun.github.io/Recca/reference/new_Y.md)
function looks upstream in the ECC to determine the effects of a change
in final demand. To demonstrate, we’ll double the `Residential` final
demand for electricity from the grid (`Elect - Grid`) in our ECC.

``` r
double <- function(x) {
  2*x
}

DoubleRes <- IO_mats |> 
  mutate(
    Y_prime = matsbyname::elementapply_byname(double, a = Y, row = "Elect [from Grid]", col = "Residential")
  )
DoubleRes$Y[[1]]
#>                     Transport Residential
#> Diesel [from Dist.]     14750           0
#> Elect [from Grid]           0        6000
#> NG [from Dist.]             0       25000
#> Petrol [from Dist.]     26000           0
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
DoubleRes$Y_prime[[1]]
#>                     Transport Residential
#> Diesel [from Dist.]     14750           0
#> Elect [from Grid]           0       12000
#> NG [from Dist.]             0       25000
#> Petrol [from Dist.]     26000           0
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
```

Now we can use the
[`new_Y()`](https://matthewheun.github.io/Recca/reference/new_Y.md)
function to estimate the ECC with the larger final demand for
residential electricity. The original Sankey diagram is:

``` r
make_sankey(OurECC) |> 
  extract2("Sankey") |> # Extracts the "Sankey" column
  extract2(1)            # Extracts the first row of the "Sankey" column
```

The new Sankey diagram is:

``` r
DoubleRes <- DoubleRes |> 
  new_Y()
make_sankey(DoubleRes, R = "R_prime", U = "U_prime", V = "V_prime", Y = "Y_prime") |> 
  extract2("Sankey") |> 
  extract2(1)
```

Output from the electricity grid is clearly larger now than before.
Also, upstream demand for natural gas is now larger than the upstream
demand for crude, whereas in the original Sankey diagram, demand for
crude was larger than the demand for natural gas.

## What if resources change? Function new_R_ps()

[`new_Y()`](https://matthewheun.github.io/Recca/reference/new_Y.md)
estimates the upstream effects of a change to final demand. But what if
we want to understand the downstream effects of a change in resource
availability? The function
[`new_R_ps()`](https://matthewheun.github.io/Recca/reference/new_R_ps.md)
supplies the answer.
[`new_R_ps()`](https://matthewheun.github.io/Recca/reference/new_R_ps.md)
takes as input the input-output structure of an ECC and the efficiency
of every intermediate industry. After discussing the resource matrix and
resource vectors, the
[`new_R_ps()`](https://matthewheun.github.io/Recca/reference/new_R_ps.md)
function is demonstrated.

### Resource matrices and vectors

The rate of resource extraction is given by the $\mathbf{R}$ matrix or
by the $\mathbf{r}$ vector. The resource matrix ($\mathbf{R}$) is
Industry$\times$Product. Its industries are usually named
`Resources - <Product>`, where `<Product>` is the name of the Product
produced by that industry.
([`clean_byname()`](https://matthewheun.github.io/matsbyname/reference/clean_byname.html)
removes `0` rows and columns for clarity.)

``` r
OurECC$R[[1]] |> 
  clean_byname()
#>                      Crude    NG
#> Resources [of Crude] 50000     0
#> Resources [of NG]        0 43000
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
```

The resource vector ($\mathbf{r}$) is a Product column vector containing
Products that are resources.

``` r
rvecs <- OurECC |> 
  transmute(
    r = clean_byname(R) |> 
      colsums_byname() |> 
      transpose_byname()
  )
rvecs$r[[1]]
#>       Industry
#> Crude    50000
#> NG       43000
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
```

### Industry efficiencies and a new resource matrix

The baseline $\mathbf{R}$ matrix is shown above. Let’s say we want to
estimate the effect of a 50 % reduction in the production rate of crude
oil. So, we cut `Resources - Crude` in half. To execute this reduction,
we’ll use the
[`matsbyname::elementapply_byname()`](https://matthewheun.github.io/matsbyname/reference/elementapply_byname.html)
function.

``` r
half <- function(x){
  x/2
}
HalfCrude <- OurECC |> 
  # Obtain the I-O structure of the ECCs
  calc_io_mats(direction = "downstream") |> 
  mutate(
    R_prime = elementapply_byname(half, a = R, row = "Resources [of Crude]", col = "Crude")
  )
HalfCrude$R_prime[[1]] |> 
  clean_byname()
#>                      Crude    NG
#> Resources [of Crude] 25000     0
#> Resources [of NG]        0 43000
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
```

Now we use the `new_R()` function to estimate changes in the ECC. But
first, we need to calculate the efficiency of all sectors in the ECC
using the
[`calc_eta_i()`](https://matthewheun.github.io/Recca/reference/calc_eta_i.md)
function.

``` r
HalfCrude <- HalfCrude |> 
  calc_eta_i() |> 
  new_R_ps()
glimpse(HalfCrude)
#> Rows: 1
#> Columns: 35
#> $ Country      <chr> "GBR"
#> $ Year         <dbl> 2000
#> $ EnergyType   <chr> "E"
#> $ LastStage    <chr> "Final"
#> $ R            <list> <<matrix[2 x 2]>>
#> $ r_EIOU       <list> <<matrix[12 x 9]>>
#> $ S_units      <list> <<matrix[12 x 1]>>
#> $ U            <list> <<matrix[12 x 9]>>
#> $ U_EIOU       <list> <<matrix[7 x 8]>>
#> $ U_feed       <list> <<matrix[9 x 9]>>
#> $ V            <list> <<matrix[9 x 10]>>
#> $ Y            <list> <<matrix[4 x 2]>>
#> $ y            <list> <<matrix[4 x 1]>>
#> $ q            <list> <<matrix[12 x 1]>>
#> $ f            <list> <<matrix[9 x 1]>>
#> $ g            <list> <<matrix[9 x 1]>>
#> $ h            <list> <<matrix[2 x 1]>>
#> $ r            <list> <<matrix[2 x 1]>>
#> $ W            <list> <<matrix[12 x 9]>>
#> $ Z_s          <list> <<matrix[10 x 9]>>
#> $ C_s          <list> <<matrix[12 x 9]>>
#> $ D_s          <list> <<matrix[9 x 12]>>
#> $ D_feed_s     <list> <<matrix[9 x 12]>>
#> $ O_s          <list> <<matrix[12 x 2]>>
#> $ B            <list> <<matrix[10 x 12]>>
#> $ G_pxp        <list> <<matrix[12 x 12]>>
#> $ G_ixp        <list> <<matrix[9 x 12]>>
#> $ R_prime      <list> <<matrix[2 x 2]>>
#> $ eta_i        <list> <<matrix[9 x 1]>>
#> $ U_prime      <list> <<matrix[12 x 9]>>
#> $ U_feed_prime <list> <<matrix[12 x 9]>>
#> $ U_EIOU_prime <list> <<matrix[12 x 9]>>
#> $ r_EIOU_prime <list> <<matrix[12 x 9]>>
#> $ V_prime      <list> <<matrix[9 x 10]>>
#> $ Y_prime      <list> <<matrix[12 x 2]>>
```

After re-calculating the `HalfCrude` ECC (specifically, after
calculating `U_prime`, `V_prime`, and `Y_prime`), we can visualize the
effect of lower crude oil availability with a modified Sankey diagram.

First, we have the original Sankey diagram.

``` r
make_sankey(OurECC) |> 
  extract2("Sankey") |> # Extracts the "Sankey" column
  extract2(1) # Extracts the first row of the "Sankey" column
```

And here is the modified Sankey diagram showing the much smaller
`Resources - Crude` extraction rate.

``` r
make_sankey(HalfCrude, R = "R_prime", U = "U_prime", V = "V_prime", Y = "Y_prime") |> 
  extract2("Sankey") |> # Extracts the "Sankey" column
  extract2(1) # Extracts the first row of the "Sankey" column
```

The Sankey diagrams clearly show that reducing the availability of crude
oil (`Resources - crude`) means that less `Transport` final demand can
be satisfied.

Note that both ECCs are in complete energy balance.

``` r
OurECC |> 
  verify_SUT_energy_balance_with_units() |> 
  select(Country, Year, EnergyType, LastStage, 
         .SUT_prod_energy_balanced, .SUT_ind_energy_balanced) |> 
  glimpse()
#> Warning: `verify_SUT_energy_balance_with_units()` was deprecated in Recca 0.1.65.
#> ℹ verify_SUT_energy_balance_with_units() seemed like a good idea at the time,
#>   but we never use it.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Rows: 1
#> Columns: 6
#> $ Country                   <chr> "GBR"
#> $ Year                      <dbl> 2000
#> $ EnergyType                <chr> "E"
#> $ LastStage                 <chr> "Final"
#> $ .SUT_prod_energy_balanced <lgl> TRUE
#> $ .SUT_ind_energy_balanced  <lgl> TRUE
```

``` r
HalfCrude |> 
  mutate(
    R_plus_V_prime = sum_byname(R_prime, V_prime)
  ) |> 
  calc_inter_industry_balance() |> 
  verify_inter_industry_balance() |> 
  select(Country, Year, EnergyType, LastStage,
         SUTInterIndustryBalanced) |> 
  glimpse()
#> Rows: 1
#> Columns: 5
#> $ Country                  <chr> "GBR"
#> $ Year                     <dbl> 2000
#> $ EnergyType               <chr> "E"
#> $ LastStage                <chr> "Final"
#> $ SUTInterIndustryBalanced <lgl> TRUE
```

## What if perfectly-substitutable inputs to an intermediate industry change? Function new_k_ps()

[`new_Y()`](https://matthewheun.github.io/Recca/reference/new_Y.md)
estimates the upstream effects of a change in final demand.
[`new_R_ps()`](https://matthewheun.github.io/Recca/reference/new_R_ps.md)
estimates the downstream effects of a change in resource availability.
But what if we want to estimate the effects of a change to the inputs of
an intermediate industry? If those inputs are perfectly substitutable,
the
[`new_k_ps()`](https://matthewheun.github.io/Recca/reference/new_k_ps.md)
function provides an answer.

To demonstrate
[`new_k_ps()`](https://matthewheun.github.io/Recca/reference/new_k_ps.md),
we need an ECC that has at least two reasonably-sized inputs.
Unfortunately, `OurECC` doesn’t have the necessary characteristics, so
we create a new ECC here.

ECC below has two resource industries (R1 and R2), one intermediate
industry (I), and two final demand sectors (Y1 and Y2). The R1 industry
makes product R1p.  
The R2 industry makes product R2p. Industry I makes product Ip. Product
Ip is consumed by final demand sectors Y1 and Y2.

The use ($\mathbf{U}$), make ($\mathbf{V}$), final demand
($\mathbf{Y}$), and unit summation ($\mathbf{S}_{units}$) matrices are
given below.

``` r
U <- matrix(c(0, 0, 10,
              0, 0, 10,
              0, 0,  0),
            byrow = TRUE, nrow = 3, ncol = 3,
            dimnames = list(c("R1p", "R2p", "Ip"), c("R1", "R2", "I"))) |>  
  setrowtype("Products") |> setcoltype("Industries")

# There is no EIOU, so U_feed is the same as U
U_feed <- U

R_plus_V <- matrix(c(10,  0, 0,
               0, 10, 0, 
               0,  0, 4), 
            byrow = TRUE, nrow = 3, ncol = 3, 
            dimnames = list(c("R1", "R2", "I"), c("R1p", "R2p", "Ip"))) |> 
  setrowtype("Industries") |> setcoltype("Products")

Y <- matrix(c(0, 0,
              0, 0, 
              2, 2),
            byrow = TRUE, nrow = 3, ncol = 2,
            dimnames = list(c("R1p", "R2p", "Ip"), c("Y1", "Y2"))) |> 
  setrowtype("Products") |> setcoltype("Industries")

RV <- separate_RV(U = U, R_plus_V = R_plus_V)
R <- RV$R
V <- RV$V

S_units <- matrix(c(1, 
                    1, 
                    1), 
                  byrow = TRUE, nrow = 3, ncol = 1,
                  dimnames = list(c("R1p", "R2p", "Ip"), c("quad"))) |> 
  setrowtype("Products") |> setcoltype("Units")
```

The Sankey diagram for this simple ECC is given below.

``` r
make_sankey(R = R, U = U, V = V, Y = Y) |> 
  extract2("Sankey")
```

We can change the proportion of inputs to industry I using
[`new_k_ps()`](https://matthewheun.github.io/Recca/reference/new_k_ps.md).
To do so, we must first calculate the input-output structure of the ECC
using the
[`calc_io_mats()`](https://matthewheun.github.io/Recca/reference/calc_io_mats.md)
function. Then, we call the
[`new_k_ps()`](https://matthewheun.github.io/Recca/reference/new_k_ps.md)
function with a new k vector that indicates industry I now uses 75/25
mixture of `R1` and `R2` instead of a 50/50 split between `R1` and `R2`.

``` r
simple_iomats <- calc_io_mats(R = R, U = U, U_feed = U_feed, V = V, Y = Y, S_units = S_units)
k_prime <- matrix(c(0.75,
                    0.25), 
                  byrow = TRUE, nrow = 2, ncol = 1,
                  dimnames = list(c("R1p", "R2p"), c("I"))) |> 
  setrowtype("Products") |> setcoltype("Industries")
change_input_proportions <- new_k_ps(c(simple_iomats, 
                                       list(R = R, U = U, V = V, Y = Y, 
                                            S_units = S_units, 
                                            k_prime = k_prime)))
make_sankey(R = change_input_proportions$R_prime, 
            U = change_input_proportions$U_prime, 
            V = change_input_proportions$V_prime, 
            Y = Y) |> 
  extract2("Sankey")
```

In response to the different demands from industry `I`, the rate of
resource extraction from `R1` would need to grow, and the rate of
resource extraction from supply `R2` would need to shrink. If there were
additional upstream industries, they would also change in response to
the new ratio of inputs to industry `I`.

## Conclusion

`Recca` has three functions that allow an analyst to estimate the effect
of changes to an ECC.

- [`new_Y()`](https://matthewheun.github.io/Recca/reference/new_Y.md)
  estimates changes upstream of final demand.
- [`new_R_ps()`](https://matthewheun.github.io/Recca/reference/new_R_ps.md)
  estimates changes downstream from resource extraction.
- [`new_k_ps()`](https://matthewheun.github.io/Recca/reference/new_k_ps.md)
  estimates changes upstream of inputs to an intermediate industry.

The first of these functions
([`new_Y()`](https://matthewheun.github.io/Recca/reference/new_Y.md))
was demonstrated in [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109). The second and
third functions
([`new_R_ps()`](https://matthewheun.github.io/Recca/reference/new_R_ps.md)
and
[`new_k_ps()`](https://matthewheun.github.io/Recca/reference/new_k_ps.md))
are shown here for the first time.

## References

Heun, Matthew Kuperus, Anne Owen, and Paul E. Brockway. 2018. “A
Physical Supply-Use Table Framework for Energy Analysis on the Energy
Conversion Chain.” *Applied Energy* 226 (September): 1134–62.
<https://doi.org/10.1016/j.apenergy.2018.05.109>.

Leontief, Wassily W. 1941. *The Structure of American Economy,
1919-1929: An Empirical Application of Equilibrium Analysis*. Cambridge,
Massachusetts: Harvard University Press.
