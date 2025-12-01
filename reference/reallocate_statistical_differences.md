# Reallocate Statistical differences

The IEA data include "Statistical differences". In some cases, it is
desirable to reallocate Statistical differences in proportion to the
non-zero consumption of each energy carrier in other industries. This
function performs that reallocation and should be called after the PSUT
matrices have been formed, most likely by calling
[`IEATools::prep_psut()`](https://matthewheun.github.io/IEATools/reference/prep_psut.html).

## Usage

``` r
reallocate_statistical_differences(
  .sutmats = NULL,
  stat_diffs = IEATools::tfc_compare_flows$statistical_differences,
  R = IEATools::psut_cols$R,
  U = IEATools::psut_cols$U,
  U_feed = IEATools::psut_cols$U_feed,
  U_eiou = IEATools::psut_cols$U_eiou,
  r_eiou = IEATools::psut_cols$r_eiou,
  V = IEATools::psut_cols$V,
  Y = IEATools::psut_cols$Y,
  R_colname = IEATools::psut_cols$R,
  U_colname = IEATools::psut_cols$U,
  U_feed_colname = IEATools::psut_cols$U_feed,
  U_eiou_colname = IEATools::psut_cols$U_eiou,
  r_eiou_colname = IEATools::psut_cols$r_eiou,
  V_colname = IEATools::psut_cols$V,
  Y_colname = IEATools::psut_cols$Y,
  prime_suffix = "_prime",
  tol = 1e-06,
  country = IEATools::iea_cols$country,
  year = IEATools::iea_cols$year
)
```

## Arguments

- .sutmats:

  A data frame of PSUT matrices, most likely the result of
  [`IEATools::prep_psut()`](https://matthewheun.github.io/IEATools/reference/prep_psut.html).

- stat_diffs:

  The name of the row in **R** and columns in **U_feed** and **Y** for
  Statistical differences. Default is
  `IEATools::tfc_compare_flows$statistical_differences`.

- R, U, U_feed, U_eiou, r_eiou, V, Y:

  Matrices or names of columns of matrices in `.sutmats`. See
  [IEATools::psut_cols](https://matthewheun.github.io/IEATools/reference/psut_cols.html)
  for defaults.

- R_colname, U_colname, U_feed_colname, U_eiou_colname, r_eiou_colname,
  V_colname, Y_colname:

  Names of columns of matrices in `.sutmats`. See
  [IEATools::psut_cols](https://matthewheun.github.io/IEATools/reference/psut_cols.html)
  for defaults.

- prime_suffix:

  The string suffix for new versions of matrices with reallocated
  statistical differences. Default is "\_prime".

- tol:

  The tolerance for energy imbalance. Default is `1e-6`.

- country, year:

  Names of the country and year columns in `.sutmats`, if it exists.
  Used to identify possible country and years where energy imbalances
  may be occurring, but only when those columns exist. See
  [IEATools::iea_cols](https://matthewheun.github.io/IEATools/reference/iea_cols.html)
  for defaults.

## Value

A version of `.sutmats` in which energy consumption by "Statistical
differences" is reallocated to other Industries in proportion to their
energy consumption.

## Details

Statistical differences can be found in either the **R** or **Y**
matrix. Both are reallocated to the **Y** and **U** matrices in
proportion. The steps are:

1.  For those rows with no other consumption in **U** or **Y**, move
    **Y** Statistical differences to **R** by subtraction.

2.  Reallocate negative Statistical differences in **R** to **R** and
    **V** using
    [`matsbyname::reallocate_byname()`](https://matthewheun.github.io/matsbyname/reference/reallocate_byname.html).

3.  Move remaining (positive) Statistical differences found in **R** to
    the **Y** matrix by subtraction.

4.  Reallocate Statistical differences in the **Y** matrix to the **Y**
    and **U** matrices using
    [`matsbyname::reallocate_byname()`](https://matthewheun.github.io/matsbyname/reference/reallocate_byname.html).

Internally, the **R** and **V** matrices are added before calling
[`matsbyname::reallocate_byname()`](https://matthewheun.github.io/matsbyname/reference/reallocate_byname.html).
**R** and **V** are split again prior to returning. Similarly, the **Y**
and **U** matrices are added before calling
[`matsbyname::reallocate_byname()`](https://matthewheun.github.io/matsbyname/reference/reallocate_byname.html).
**Y** and **U** are split again prior to returning.

Energy balance is checked both prior to reallocating statistical
differences and after reallocating statistical differences. Imbalances
greater than `tol` cause an error.

Note that most functions in `IEATools` operate on tabular IEA data.
However, this function assumes IEA data have already been converted to
matrix (PSUT) format, most likely with
[`IEATools::prep_psut()`](https://matthewheun.github.io/IEATools/reference/prep_psut.html).

## Examples

``` r
R <- matrix(c(98, 0,
               0, 50,
               2, 0),
            byrow = TRUE, nrow = 3, ncol = 2,
            dimnames = list(c("Resources [of Coal]",
                              "Resources [of Prod C]",
                              "Statistical differences"),
                            c("Coal [from Resources]", "Prod C"))) |>
  matsbyname::setrowtype("Industry") |> matsbyname::setcoltype("Product")
R
#>                         Coal [from Resources] Prod C
#> Resources [of Coal]                        98      0
#> Resources [of Prod C]                       0     50
#> Statistical differences                     2      0
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
U <- matrix(c(100,
              2),
            byrow = TRUE, nrow = 2, ncol = 1,
            dimnames = list(c("Coal [from Resources]", "Electricity"),
                            c("Mapep"))) |>
  matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry")
U
#>                       Mapep
#> Coal [from Resources]   100
#> Electricity               2
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
V <- matrix(40,
            byrow = TRUE, nrow = 1, ncol = 1,
            dimnames = list(c("Mapep"), c("Electricity"))) |>
  matsbyname::setrowtype("Industry") |> matsbyname::setcoltype("Product")
V
#>       Electricity
#> Mapep          40
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
Y <- matrix(c(20, 10, 8,
               0, 0, 50),
            byrow = TRUE, nrow = 2, ncol = 3,
            dimnames = list(c("Electricity", "Prod C"),
                            c("Industry 1", "Industry 2",
                            "Statistical differences"))) |>
  matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry")
Y
#>             Industry 1 Industry 2 Statistical differences
#> Electricity         20         10                       8
#> Prod C               0          0                      50
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
r_eiou <- matrix(1,
                 byrow = TRUE, nrow = 1, ncol = 1,
                 dimnames = list("Electricity", "Mapep")) |>
  matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry")
r_eiou
#>             Mapep
#> Electricity     1
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
U_EIOU <- matsbyname::hadamardproduct_byname(U, r_eiou)
U_EIOU
#>                       Mapep
#> Coal [from Resources]     0
#> Electricity               2
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
U_feed <- matsbyname::difference_byname(U, U_EIOU)
U_feed
#>                       Mapep
#> Coal [from Resources]   100
#> Electricity               0
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
reallocate_statistical_differences(R = R,
                                   U = U,
                                   U_feed = U_feed,
                                   U_eiou = U_EIOU,
                                   r_eiou = r_eiou,
                                   V = V,
                                   Y = Y)
#> Warning: Statistical differences account for more than half of all consumption.
#> $R_prime
#>                     Coal [from Resources]
#> Resources [of Coal]                    98
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
#> $U_prime
#>                       Mapep
#> Coal [from Resources]  98.0
#> Electricity             2.5
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> $U_feed_prime
#>                       Mapep
#> Coal [from Resources]    98
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> $U_EIOU_prime
#>             Mapep
#> Electricity   2.5
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> $V_prime
#>       Electricity
#> Mapep          40
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
#> $Y_prime
#>             Industry 1 Industry 2
#> Electricity         25       12.5
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
```
