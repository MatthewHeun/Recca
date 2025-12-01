# Balances

``` r
library(dplyr)
library(IEATools)
library(Recca)
library(tidyr)
```

## Introduction

When working with material and energy conversion chains in the `Recca`
package with `matsindf` data frames, it is helpful to calculate several
types of balances. This vignette describes those balances and shows when
you might want to use each of them. Balances include

- inter-industry balances,
- intra-industry balances, and
- the $\mathbf{W}$ matrix.

### Terminology

`Recca` assists with matrix-based input-output calculations when the
data are organized into the $\mathbf{R}\mathbf{U}\mathbf{V}\mathbf{Y}$
matrices of the physical supply-use table (PSUT) framework. A generic
processing stage is called an “industry.” The stuff that flows between
industries is generically called a “product.” A “balance” is any
calculation that compares inputs to outputs. Balances can be computed
for all types of products, in particular materials (quantified in mass
or exergy units) and energy (quantified as energy or exergy).

In the PSUT framework, the $\mathbf{R}$ matrix is an industry-by-product
resource matrix. The $\mathbf{U}$ matrix is an product-by-industry use
matrix. The $\mathbf{V}$ matrix is an industry-by-product make matrix.
The $\mathbf{Y}$ matrix is an product-by-industry final demand matrix.

In this vignette, matrices are represented by boldface capital letters,
such as $\mathbf{R}$. Vectors are assumed to be column vectors and
represented by boldface lowercase letters, such as the identity vector
$\mathbf{i}$.

### Data

In the examples that follow, we use the energy conversion chain in
`UKEnergy2000mats` from the `Recca` package.

``` r
ecc <- UKEnergy2000mats |> 
  tidyr::pivot_wider(names_from = matrix.name, 
                     values_from = matrix)
dplyr::glimpse(ecc)
#> Rows: 4
#> Columns: 12
#> $ Country    <chr> "GBR", "GBR", "GBR", "GBR"
#> $ Year       <dbl> 2000, 2000, 2000, 2000
#> $ EnergyType <chr> "E", "E", "E", "X"
#> $ LastStage  <chr> "Final", "Services", "Useful", "Services"
#> $ R          <list> <<matrix[2 x 2]>>, <<matrix[2 x 2]>>, <<matrix[2 x 2]>>, <<…
#> $ U          <list> <<matrix[12 x 9]>>, <<matrix[17 x 17]>>, <<matrix[13 x 13]>…
#> $ U_EIOU     <list> <<matrix[7 x 8]>>, <<matrix[3 x 8]>>, <<matrix[3 x 8]>>, <<…
#> $ U_feed     <list> <<matrix[9 x 9]>>, <<matrix[16 x 17]>>, <<matrix[12 x 13]>>…
#> $ V          <list> <<matrix[9 x 10]>>, <<matrix[17 x 18]>>, <<matrix[13 x 14]…
#> $ Y          <list> <<matrix[4 x 2]>>, <<matrix[4 x 2]>>, <<matrix[4 x 2]>>, <…
#> $ r_EIOU     <list> <<matrix[12 x 9]>>, <<matrix[17 x 17]>>, <<matrix[13 x 13]…
#> $ S_units    <list> <<matrix[12 x 1]>>, <<matrix[20 x 5]>>, <<matrix[16 x 1]>>…
```

## Balances

The following subsections explain each type of balance and how to
calculate it with the `Recca` package.

### Inter-industry (between-industry) balances check for product consistency

In any conversion chain, products that leave one industry must enter
another industry. These between-industry (or inter-industry) balances
should be verified for every conversion chain to ensure consistency.

Inter-industry balances are obtained by the following computation.

$$\left\lbrack (\mathbf{R} + \mathbf{V})^{T} - (\mathbf{U} + \mathbf{Y}) \right\rbrack\,\mathbf{i}$$

If the conversion chain is consistent, the result should be the
$\mathbf{0}$ vector with products in rows.

To compute inter-industry balances within the PSUT framework using the
`Recca` package, use the following code.

``` r
inter_industry_balances <- ecc |> 
  calc_inter_industry_balance()
```

The result is added as a new column, named “SUTInterIndustryBalance” by
default.

``` r
dplyr::glimpse(inter_industry_balances)
#> Rows: 4
#> Columns: 13
#> $ Country                 <chr> "GBR", "GBR", "GBR", "GBR"
#> $ Year                    <dbl> 2000, 2000, 2000, 2000
#> $ EnergyType              <chr> "E", "E", "E", "X"
#> $ LastStage               <chr> "Final", "Services", "Useful", "Services"
#> $ R                       <list> <<matrix[2 x 2]>>, <<matrix[2 x 2]>>, <<matrix…
#> $ U                       <list> <<matrix[12 x 9]>>, <<matrix[17 x 17]>>, <<mat…
#> $ U_EIOU                  <list> <<matrix[7 x 8]>>, <<matrix[3 x 8]>>, <<matrix…
#> $ U_feed                  <list> <<matrix[9 x 9]>>, <<matrix[16 x 17]>>, <<matr…
#> $ V                       <list> <<matrix[9 x 10]>>, <<matrix[17 x 18]>>, <<ma…
#> $ Y                       <list> <<matrix[4 x 2]>>, <<matrix[4 x 2]>>, <<matri…
#> $ r_EIOU                  <list> <<matrix[12 x 9]>>, <<matrix[17 x 17]>>, <<ma…
#> $ S_units                 <list> <<matrix[12 x 1]>>, <<matrix[20 x 5]>>, <<mat…
#> $ SUTInterIndustryBalance <list> <<matrix[12 x 1]>>, <<matrix[20 x 1]>>, <<mat…
```

The vectors in SUTInterIndustryBalance column have products in rows.

``` r
inter_industry_balances$SUTInterIndustryBalance[[1]]
#>                     Industry
#> Crude                      0
#> Crude [from Dist.]         0
#> Crude [from Fields]        0
#> Diesel                     0
#> Diesel [from Dist.]        0
#> Elect                      0
#> Elect [from Grid]          0
#> NG                         0
#> NG [from Dist.]            0
#> NG [from Wells]            0
#> Petrol                     0
#> Petrol [from Dist.]        0
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
inter_industry_balances$SUTInterIndustryBalance[[2]]
#>                                  Industry
#> Crude                        0.000000e+00
#> Crude [from Dist.]           0.000000e+00
#> Crude [from Fields]          0.000000e+00
#> Diesel                       0.000000e+00
#> Diesel [from Dist.]          0.000000e+00
#> Elect                        0.000000e+00
#> Elect [from Grid]            0.000000e+00
#> Freight [tonne-km/year]      3.051758e-05
#> Illumination [lumen-hrs/yr]  0.000000e+00
#> Light                        0.000000e+00
#> LTH                          0.000000e+00
#> MD [from Car engines]       -1.364242e-12
#> MD [from Truck engines]      4.547474e-13
#> NG                           0.000000e+00
#> NG [from Dist.]              0.000000e+00
#> NG [from Wells]              0.000000e+00
#> Passenger [passenger-km/yr]  0.000000e+00
#> Petrol                       0.000000e+00
#> Petrol [from Dist.]          0.000000e+00
#> Space heating [m3-K]         0.000000e+00
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
inter_industry_balances$SUTInterIndustryBalance[[3]]
#>                             Industry
#> Crude                   0.000000e+00
#> Crude [from Dist.]      0.000000e+00
#> Crude [from Fields]     0.000000e+00
#> Diesel                  0.000000e+00
#> Diesel [from Dist.]     0.000000e+00
#> Elect                   0.000000e+00
#> Elect [from Grid]       0.000000e+00
#> Light                   0.000000e+00
#> LTH                     0.000000e+00
#> MD [from Car engines]   0.000000e+00
#> MD [from Truck engines] 2.273737e-13
#> NG                      0.000000e+00
#> NG [from Dist.]         0.000000e+00
#> NG [from Wells]         0.000000e+00
#> Petrol                  0.000000e+00
#> Petrol [from Dist.]     0.000000e+00
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
inter_industry_balances$SUTInterIndustryBalance[[4]]
#>                                 Industry
#> Crude                       0.000000e+00
#> Crude [from Dist.]          0.000000e+00
#> Crude [from Fields]         0.000000e+00
#> Diesel                      0.000000e+00
#> Diesel [from Dist.]         0.000000e+00
#> Elect                       0.000000e+00
#> Elect [from Grid]           0.000000e+00
#> Freight [tonne-km/year]     3.051758e-05
#> Illumination [lumen-hrs/yr] 0.000000e+00
#> Light                       0.000000e+00
#> LTH                         0.000000e+00
#> MD [from Car engines]       0.000000e+00
#> MD [from Truck engines]     0.000000e+00
#> NG                          0.000000e+00
#> NG [from Dist.]             0.000000e+00
#> NG [from Wells]             0.000000e+00
#> Passenger [passenger-km/yr] 0.000000e+00
#> Petrol                      0.000000e+00
#> Petrol [from Dist.]         0.000000e+00
#> Space heating [m3-K]        0.000000e+00
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
```

As expected, these vectors are the $\mathbf{0}$ vector to within
reasonable precision.

If you want to check the inter-industry balances for consistency, use
[`verify_inter_industry_balance()`](https://matthewheun.github.io/Recca/reference/verify-balances.md).
The `tol` argument gives the tolerance for non-zero product balances.

``` r
inter_industry_balances |> 
  verify_inter_industry_balance(tol = 1e-4) |> 
  dplyr::glimpse()
#> Rows: 4
#> Columns: 14
#> $ Country                  <chr> "GBR", "GBR", "GBR", "GBR"
#> $ Year                     <dbl> 2000, 2000, 2000, 2000
#> $ EnergyType               <chr> "E", "E", "E", "X"
#> $ LastStage                <chr> "Final", "Services", "Useful", "Services"
#> $ R                        <list> <<matrix[2 x 2]>>, <<matrix[2 x 2]>>, <<matri…
#> $ U                        <list> <<matrix[12 x 9]>>, <<matrix[17 x 17]>>, <<ma…
#> $ U_EIOU                   <list> <<matrix[7 x 8]>>, <<matrix[3 x 8]>>, <<matri…
#> $ U_feed                   <list> <<matrix[9 x 9]>>, <<matrix[16 x 17]>>, <<mat…
#> $ V                        <list> <<matrix[9 x 10]>>, <<matrix[17 x 18]>>, <<m…
#> $ Y                        <list> <<matrix[4 x 2]>>, <<matrix[4 x 2]>>, <<matr…
#> $ r_EIOU                   <list> <<matrix[12 x 9]>>, <<matrix[17 x 17]>>, <<m…
#> $ S_units                  <list> <<matrix[12 x 1]>>, <<matrix[20 x 5]>>, <<ma…
#> $ SUTInterIndustryBalance  <list> <<matrix[12 x 1]>>, <<matrix[20 x 1]>>, <<ma…
#> $ SUTInterIndustryBalanced <lgl> TRUE, TRUE, TRUE, TRUE
```

If a product is out of balance by more than `tol` (by default `1e-6`), a
warning occurs and `FALSE` appears in the new column,
“SUTInterIndustryBalanced.”

### Intra-industry (across-industry) balances

A second type of balance (the intra-industry balance) computes
differences *across* industries. Intra-industry balances are obtained by
the following computation.

$$\left( \mathbf{U}^{T} - \mathbf{V} \right)\,\mathbf{i}$$

The result is a column vector with industries in rows.

The interpretation of the results depends upon the construction of the
matrices.

In a PSUT description of conserved quantities (such as mass or energy),
when all losses are accounted within the matrices themselves, the
calculation of intra-industry balances should give the $\mathbf{0}$
vector. When losses *are not* accounted in the matrices, the calculation
of intra-industry balances gives losses.

For PSUT matrices describing non-conserved quantities such as exergy
(mass exergy, energy exergy, or both), when losses *are* accounted in
the matrices, the intra-industry balance gives destruction of the
non-conserved quantity. When losses *are not* accounted in the matrices,
the intra-industry balance gives the sum of destruction and losses.

The following table summarizes the above points.

|       Matrices contain        | Matrices include losses (wastes) |             Matrices exclude losses (wastes)              |
|:-----------------------------:|:--------------------------------:|:---------------------------------------------------------:|
|          Mass (MCC)           |           $\mathbf{0}$           |                     $\mathbf{m}_{L}$                      |
|         Energy (ECC)          |           $\mathbf{0}$           |                     $\mathbf{e}_{L}$                      |
|       Mass exergy (BCC)       |         $\mathbf{b}_{D}$         |             $\mathbf{b}_{D} + \mathbf{b}_{L}$             |
|      Energy exergy (XCC)      |         $\mathbf{x}_{D}$         |             $\mathbf{x}_{D} + \mathbf{x}_{L}$             |
| Mass and energy exergy (BXCC) |   ${\mathbf{b}\mathbf{x}}_{D}$   | ${\mathbf{b}\mathbf{x}}_{D} + {\mathbf{b}\mathbf{x}}_{L}$ |

MCC indicates a material conversion chain. ECC indicates an energy
conversion chain. BCC indicates a material exergy conversion chain. XCC
indicates an energy exergy conversion chain. BXCC indicates a combined
material and energy exergy conversion chain. $\mathbf{0}$ indicates the
result is the zero vector; $\mathbf{m}$ indicates the result is a mass
vector; $\mathbf{e}$ indicates the result is an energy vector;
$\mathbf{b}$ indicates the result is a mass exergy vector; $\mathbf{x}$
indicates the result is an energy exergy vector; and
$\mathbf{b}\mathbf{x}$ indicates the result is a the sum of mass and
energy exergy vectors. Subscript $L$ indicates losses (or wastes), and
subscript $D$ indicates exergy destroyed by the industry.

To compute intra-industry (across-industry) balances within the PSUT
framework using the `Recca` package, use the following code.

``` r
intra_industry_balances <- ecc |> 
  dplyr::filter(LastStage %in% c("Final", "Useful")) |> 
  calc_intra_industry_balance()
```

The result is added as a new column, named “SUTIntraIndustryBalance” by
default.

``` r
dplyr::glimpse(intra_industry_balances)
#> Rows: 2
#> Columns: 13
#> $ Country                 <chr> "GBR", "GBR"
#> $ Year                    <dbl> 2000, 2000
#> $ EnergyType              <chr> "E", "E"
#> $ LastStage               <chr> "Final", "Useful"
#> $ R                       <list> <<matrix[2 x 2]>>, <<matrix[2 x 2]>>
#> $ U                       <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ U_EIOU                  <list> <<matrix[7 x 8]>>, <<matrix[3 x 8]>>
#> $ U_feed                  <list> <<matrix[9 x 9]>>, <<matrix[12 x 13]>>
#> $ V                       <list> <<matrix[9 x 10]>>, <<matrix[13 x 14]>>
#> $ Y                       <list> <<matrix[4 x 2]>>, <<matrix[4 x 2]>>
#> $ r_EIOU                  <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ S_units                 <list> <<matrix[12 x 1]>>, <<matrix[16 x 1]>>
#> $ SUTIntraIndustryBalance <list> <<matrix[9 x 1]>>, <<matrix[13 x 1]>>
```

The vectors in the “SUTIntraIndustryBalance” column have industries in
rows.

``` r
intra_industry_balances$SUTIntraIndustryBalance[[1]]
#>                   Product
#> Crude dist.           550
#> Diesel dist.          350
#> Elect. grid           125
#> Gas wells & proc.    2075
#> NG dist.               50
#> Oil fields           2575
#> Oil refineries       5075
#> Petrol dist.          750
#> Power plants         9700
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
intra_industry_balances$SUTIntraIndustryBalance[[2]]
#>                      Product
#> Car engines       22999.6000
#> Crude dist.         545.0000
#> Diesel dist.        367.9998
#> Elect. grid         125.0000
#> Furnaces           5000.0000
#> Gas wells & proc.  2075.0000
#> Light fixtures     4800.0000
#> NG dist.             45.0000
#> Oil fields         2575.0000
#> Oil refineries     5075.0000
#> Petrol dist.        526.9997
#> Power plants       9700.0000
#> Truck engines     13250.0200
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
```

Because the matrices in `ecc` do not contain losses,
[`calc_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md)
calculates energy losses (waste heat) ($\mathbf{e}_{L}$ in the table
above).

### Endogenizing losses

The losses can be endogenized, i.e., added to the
$\mathbf{R}\mathbf{U}\mathbf{V}\mathbf{Y}$ matrices of the PSUT
framework. To do so, use the function
[`endogenize_losses()`](https://matthewheun.github.io/Recca/reference/endogenize_losses.md).

``` r
endogenized <- intra_industry_balances |> 
  endogenize_losses()
```

[`endogenize_losses()`](https://matthewheun.github.io/Recca/reference/endogenize_losses.md)
adds two new columns: by default `V_prime` and `Y_prime`. `V_prime` and
`Y_prime` contain $\mathbf{V}$ and $\mathbf{Y}$ matrices with losses
included directly in the matrices. The $\mathbf{V}$ matrices contain a
new column (named “Waste heat” by default) that shows the heat losses
from each industry.

``` r
endogenized$V_prime[[1]]
#>                   Crude [from Dist.] Crude [from Fields] Diesel
#> Crude dist.                    47500                   0      0
#> Diesel dist.                       0                   0      0
#> Elect. grid                        0                   0      0
#> Gas wells & proc.                  0                   0      0
#> NG dist.                           0                   0      0
#> Oil fields                         0               50000      0
#> Oil refineries                     0                   0  20500
#> Petrol dist.                       0                   0      0
#> Power plants                       0                   0      0
#>                   Diesel [from Dist.] Elect Elect [from Grid] NG [from Dist.]
#> Crude dist.                         0     0                 0               0
#> Diesel dist.                    15500     0                 0               0
#> Elect. grid                         0     0              6275               0
#> Gas wells & proc.                   0     0                 0               0
#> NG dist.                            0     0                 0           41000
#> Oil fields                          0     0                 0               0
#> Oil refineries                      0     0                 0               0
#> Petrol dist.                        0     0                 0               0
#> Power plants                        0  6400                 0               0
#>                   NG [from Wells] Petrol Petrol [from Dist.] Waste heat
#> Crude dist.                     0      0                   0        550
#> Diesel dist.                    0      0                   0        350
#> Elect. grid                     0      0                   0        125
#> Gas wells & proc.           43000      0                   0       2075
#> NG dist.                        0      0                   0         50
#> Oil fields                      0      0                   0       2575
#> Oil refineries                  0  26500                   0       5075
#> Petrol dist.                    0      0               26500        750
#> Power plants                    0      0                   0       9700
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
```

The $\mathbf{Y}$ matrices also have a new column, by default named
“Losses.”

``` r
endogenized$Y_prime[[1]]
#>                     Losses Residential Transport
#> Diesel [from Dist.]      0           0     14750
#> Elect [from Grid]        0        6000         0
#> NG [from Dist.]          0       25000         0
#> Petrol [from Dist.]      0           0     26000
#> Waste heat           21250           0         0
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
```

In addition, the $\mathbf{Y}$ matrices contain a new row (again named
“Waste heat” by default) that shows the total heat loss in a new
“Losses” column.

To accept the endogenized matrices, do the following:

``` r
endogenized |> 
  dplyr::mutate(
    V = V_prime,
    Y = Y_prime, 
    V_prime = NULL, 
    Y_prime = NULL, 
    SUTIntraIndustryBalance = NULL
  ) |> 
  dplyr::glimpse()
#> Rows: 2
#> Columns: 12
#> $ Country    <chr> "GBR", "GBR"
#> $ Year       <dbl> 2000, 2000
#> $ EnergyType <chr> "E", "E"
#> $ LastStage  <chr> "Final", "Useful"
#> $ R          <list> <<matrix[2 x 2]>>, <<matrix[2 x 2]>>
#> $ U          <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ U_EIOU     <list> <<matrix[7 x 8]>>, <<matrix[3 x 8]>>
#> $ U_feed     <list> <<matrix[9 x 9]>>, <<matrix[12 x 13]>>
#> $ V          <list> <<matrix[9 x 11]>>, <<matrix[13 x 15]>>
#> $ Y          <list> <<matrix[5 x 3]>>, <<matrix[5 x 3]>>
#> $ r_EIOU     <list> <<matrix[12 x 9]>>, <<matrix[13 x 13]>>
#> $ S_units    <list> <<matrix[12 x 1]>>, <<matrix[16 x 1]>>
```

### The value added matrix ($\mathbf{W}$)

The value added matrix ($\mathbf{W}$) obtained from
[`calc_yqfgW()`](https://matthewheun.github.io/Recca/reference/calc_yqfgW.md)
is similar to but different from the balances obtained from
[`calc_inter_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md)
and
[`calc_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md).
The following equations show the similarities and differences.

The $\mathbf{W}$ matrix is

$$\mathbf{W} = \mathbf{V}^{T} - \mathbf{U}$$

Inter-industry balances are calculated by

$$\left\lbrack (\mathbf{R} + \mathbf{V})^{T} - (\mathbf{U} + \mathbf{Y}) \right\rbrack\mathbf{i}$$

and the intra-industry balances are calculated by

$$\left( \mathbf{U}^{T} - \mathbf{V} \right)\mathbf{i}\,.$$

The $\mathbf{W}$ matrix is closest to the intra-industry balances, but
with the opposite sense ($\mathbf{W}$ has products in rows and the
intra-industry balances have industries in rows), without the summation
by the unit vector ($\mathbf{i}$), and with the opposite sign. The
following code shows the comparison between $\mathbf{W}$ and the
intra-industry balances.

``` r
compare <- ecc |> 
  calc_yqfgW() |> 
  calc_intra_industry_balance()
compare$W[[1]] |> 
  # Calculate column sums and 
  # transpose to create a column vector 
  # for easier comparison
  matsbyname::colsums_byname() |> 
  matsbyname::transpose_byname()
#>                   Product
#> Crude dist.          -550
#> Diesel dist.         -350
#> Elect. grid          -125
#> Gas wells & proc.   -2075
#> NG dist.              -50
#> Oil fields          -2575
#> Oil refineries      -5075
#> Petrol dist.         -750
#> Power plants        -9700
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
compare$SUTIntraIndustryBalance[[1]]
#>                   Product
#> Crude dist.           550
#> Diesel dist.          350
#> Elect. grid           125
#> Gas wells & proc.    2075
#> NG dist.               50
#> Oil fields           2575
#> Oil refineries       5075
#> Petrol dist.          750
#> Power plants         9700
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
```

### Deprecated functions

Two related functions in the `Recca` package have been deprecated and
will soon be removed.

#### `verify_SUT_energy_balance()`

[`verify_SUT_energy_balance()`](https://matthewheun.github.io/Recca/reference/verify_SUT_energy_balance.md)
has been deprecated for two reasons.

- [`verify_SUT_energy_balance()`](https://matthewheun.github.io/Recca/reference/verify_SUT_energy_balance.md)
  unfortunately combined
  [`calc_inter_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md)
  and
  [`verify_inter_industry_balance()`](https://matthewheun.github.io/Recca/reference/verify-balances.md)
  in a single step. We often need to know the balances independently
  from whether products are balanced. The combination of
  [`calc_inter_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md)
  and
  [`verify_inter_industry_balance()`](https://matthewheun.github.io/Recca/reference/verify-balances.md)
  provides the needed flexibility.
- Using “energy” in the name of the function was an unfortunate choice,
  because we can use the same mathematics to balance masses or exergies.
  The new function names are independent of the type products in the
  $\mathbf{R}\mathbf{U}\mathbf{V}\mathbf{Y}$ matrices and are,
  therefore, an improvement.

For now,
[`verify_SUT_energy_balance()`](https://matthewheun.github.io/Recca/reference/verify_SUT_energy_balance.md)
remains in the `Recca` package but will likely be removed at a later
date.

#### `verify_SUT_energy_balance_with_units()`

[`verify_SUT_energy_balance_with_units()`](https://matthewheun.github.io/Recca/reference/verify_SUT_energy_balance_with_units.md)
seemed like a good idea at the time, but we have never used it. For now,
[`verify_SUT_energy_balance_with_units()`](https://matthewheun.github.io/Recca/reference/verify_SUT_energy_balance_with_units.md)
remains in the `Recca` package but will likely be removed at a later
date.

## Conclusion

There are several types of balances that are of interest to energy and
material conversion chain analysts. The functions
[`calc_inter_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md),
[`verify_inter_industry_balance()`](https://matthewheun.github.io/Recca/reference/verify-balances.md),
[`calc_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/balances.md),
[`verify_intra_industry_balance()`](https://matthewheun.github.io/Recca/reference/verify-balances.md),
and
[`endogenize_losses()`](https://matthewheun.github.io/Recca/reference/endogenize_losses.md)
assist with those analyses.
