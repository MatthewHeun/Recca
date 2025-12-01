# Aggregate to primary, final, and useful stages

Calculate aggregates at all possible ECC stages (primary, final, and
useful), regardless of whether the last stage is final or useful. See
details for the approach. Note that services aggregations are often
inherently problematic, because energy services are often quantified in
different units. This functions does not perform aggregation to
services. Users are referred to
[`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md)
for aggregation to services, if desired and sensible.

## Usage

``` r
pfu_aggregates(
  .sutdata,
  p_industries,
  fd_sectors,
  by = c("Total", "Product", "Industry", "Flow"),
  add_net_gross_cols = FALSE,
  piece = "all",
  notation = RCLabels::notations_list,
  pattern_type = c("exact", "leading", "trailing", "anywhere", "literal"),
  prepositions = RCLabels::prepositions_list,
  net_aggregate_primary = Recca::aggregate_cols$net_aggregate_primary,
  gross_aggregate_primary = Recca::aggregate_cols$gross_aggregate_primary,
  net_aggregate_final = Recca::aggregate_cols$net_aggregate_final,
  gross_aggregate_final = Recca::aggregate_cols$gross_aggregate_final,
  net_aggregate_useful = Recca::aggregate_cols$net_aggregate_useful,
  gross_aggregate_useful = Recca::aggregate_cols$gross_aggregate_useful,
  net_aggregate_services = Recca::aggregate_cols$net_aggregate_services,
  gross_aggregate_services = Recca::aggregate_cols$gross_aggregate_services,
  last_stage = Recca::psut_cols$last_stage,
  primary = Recca::all_stages$primary,
  final = Recca::all_stages$final,
  useful = Recca::all_stages$useful,
  services = Recca::all_stages$services,
  sep = Recca::all_stages$last_stage_sep,
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  U_feed = Recca::psut_cols$U_feed,
  U_eiou = Recca::psut_cols$U_eiou,
  r_eiou = Recca::psut_cols$r_eiou,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  S_units = Recca::psut_cols$S_units,
  R_lsfinal = paste0(Recca::psut_cols$R, sep, final),
  U_lsfinal = paste0(Recca::psut_cols$U, sep, final),
  U_feed_lsfinal = paste0(Recca::psut_cols$U_feed, sep, final),
  U_eiou_lsfinal = paste0(Recca::psut_cols$U_eiou, sep, final),
  r_eiou_lsfinal = paste0(Recca::psut_cols$r_eiou, sep, final),
  V_lsfinal = paste0(Recca::psut_cols$V, sep, final),
  Y_lsfinal = paste0(Recca::psut_cols$Y, sep, final),
  S_units_lsfinal = paste0(Recca::psut_cols$S_units, sep, final),
  R_lsuseful = paste0(Recca::psut_cols$R, sep, useful),
  U_lsuseful = paste0(Recca::psut_cols$U, sep, useful),
  U_feed_lsuseful = paste0(Recca::psut_cols$U_feed, sep, useful),
  U_eiou_lsuseful = paste0(Recca::psut_cols$U_eiou, sep, useful),
  r_eiou_lsuseful = paste0(Recca::psut_cols$r_eiou, sep, useful),
  V_lsuseful = paste0(Recca::psut_cols$V, sep, useful),
  Y_lsuseful = paste0(Recca::psut_cols$Y, sep, useful),
  S_units_lsuseful = paste0(Recca::psut_cols$S_units, sep, useful),
  R_lsservices = paste0(Recca::psut_cols$R, sep, services),
  U_lsservices = paste0(Recca::psut_cols$U, sep, services),
  U_feed_lsservices = paste0(Recca::psut_cols$U_feed, sep, services),
  U_eiou_lsservices = paste0(Recca::psut_cols$U_eiou, sep, services),
  r_eiou_lsservices = paste0(Recca::psut_cols$r_eiou, sep, services),
  V_lsservices = paste0(Recca::psut_cols$V, sep, services),
  Y_lsservices = paste0(Recca::psut_cols$Y, sep, services),
  S_units_lsservices = paste0(Recca::psut_cols$S_units, sep, services),
  .matnames = Recca::psut_cols$matnames,
  .matvals = Recca::psut_cols$matvals,
  tol = 1e-06
)
```

## Arguments

- .sutdata:

  An optional data frame containing physical supply use table
  descriptions of energy conversion chains.

- p_industries:

  A string vector of primary industries.

- fd_sectors:

  A string vector of final demand sectors.

- by:

  Tells how to aggregate, one of "Total", "Product", "Industry", or
  "Flow". Default is "Total".

- add_net_gross_cols:

  A boolean that tells whether to include net and gross columns for
  primary energy aggregation. Default is `FALSE`.

- piece:

  Tells which piece of row and column labels to use for aggregation
  decision. Default is "all".

- notation:

  Tells which notation is used for row and column labels. Default is
  [`RCLabels::notations_list`](https://matthewheun.github.io/RCLabels/reference/notations_list.html).

- pattern_type:

  Tells how to match row and column names. One of "exact", "leading",
  "trailing", "anywhere", or "literal". Default is "exact".

- prepositions:

  The list of prepositions for row and column labels. Default is
  [`RCLabels::prepositions_list`](https://matthewheun.github.io/RCLabels/reference/prepositions_list.html).

- net_aggregate_primary, gross_aggregate_primary, net_aggregate_final,
  gross_aggregate_final, net_aggregate_useful, gross_aggregate_useful,
  net_aggregate_services, gross_aggregate_services:

  See
  [`Recca::aggregate_cols`](https://matthewheun.github.io/Recca/reference/aggregate_cols.md).

- last_stage:

  Name of the last stage column. Default is
  `Recca::psut_cols$last_stage`.

- primary, final, useful, services:

  String identifiers for ECC stages. See
  [`Recca::all_stages`](https://matthewheun.github.io/Recca/reference/all_stages.md).

- sep:

  The string separator identifying the last stage in the ECC. Default is
  `Recca::all_stages$last_stage_sep`.

- R, U, U_feed, U_eiou, r_eiou, V, Y, S_units:

  Names for columns containing matrices. See
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md).

- R_lsfinal, U_lsfinal, U_feed_lsfinal, U_eiou_lsfinal, r_eiou_lsfinal,
  V_lsfinal, Y_lsfinal, S_units_lsfinal:

  Names for columns when last stage is final energy. Defaults are
  unmodified column names concatenated with `sep` and `final`.

- R_lsuseful, U_lsuseful, U_feed_lsuseful, U_eiou_lsuseful,
  r_eiou_lsuseful, V_lsuseful, Y_lsuseful, S_units_lsuseful:

  Names for columns when last stage is useful energy. Defaults are
  unmodified column names concatenated with `sep` and `useful`.

- R_lsservices, U_lsservices, U_feed_lsservices, U_eiou_lsservices,
  r_eiou_lsservices, V_lsservices, Y_lsservices, S_units_lsservices:

  Names for columns when last stage is energy services. Defaults are
  unmodified column names concatenated with `sep` and `services`.

- .matnames, .matvals:

  Names of columns used internally. Defaults are from
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md).

- tol:

  The allowable energy imbalance in the units of energy flows. Default
  is `1e-6`.

## Value

A data frame of primary, final, and useful aggregates.

## Details

There are several ways to aggregate energy conversion chain (ECC) data
to primary, final, or useful stages of the ECC.
[`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md)
aggregates primary energy using the resource (**R**), make (**V**), and
final demand (**Y**) matrices.
[`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md)
gives upstream (source) aggregations for an ECC.
[`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md)
aggregates to the last stage of an energy conversion chain using **R**
and **Y** matrices, regardless of whether the last stage is final,
useful, or services.
[`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md)
gives downstream (sink) aggregations.

However, applying
[`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md)
to an ECC whose last stage is final cannot produce useful stage
aggregations. Similarly, applying
[`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md)
to an ECC whose last stage is useful cannot provide final stage
aggregations. See the following table.

|                           |                                                                                                       |                                                                                                       |
|---------------------------|-------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------|
| ECC last stage –\>        | Final                                                                                                 | Useful                                                                                                |
| Desired aggregation stage |                                                                                                       |                                                                                                       |
| Primary                   | [`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md) Note A  | [`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md) Note A  |
| Final                     | [`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md) | Note B                                                                                                |
| Useful                    | Note C                                                                                                | [`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md) |

For the off-axis aggregations, special considerations are employed in
this function.

Note A:

The two results from
[`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md)
should be equal to within `tol`. If agreement is not observed, an error
is given.

Note B:

When last stage is useful but we want final stage aggregations and
final-to-useful stage efficiencies, we can again employ
[`calc_eta_fu_Y_eiou()`](https://matthewheun.github.io/Recca/reference/calc_eta_fu_Y_eiou.md)
in an inverse relationship to calculate final stage aggregates when
useful stage information is known. The result is **Y** and **U_EIOU**
matrices of same structure as **Y_Useful** and **U_EIOU_Useful** but
containing final stage data. These final-but-in-same-structure-as-useful
matrices can be used to calculate final aggregations when last stage is
useful.

Note C:

When last stage is final but we want useful energy aggregates and
final-to-useful efficiencies, we can employ
[`calc_eta_fu_Y_eiou()`](https://matthewheun.github.io/Recca/reference/calc_eta_fu_Y_eiou.md)
to calculate useful energy for each piece of final demand or EIOU when
last stage is final, giving **Y** and **U_EIOU** matrices with same
structure as **Y_Final** and **U_EIOU_Final** except containing useful
energy data. These useful-but-in-same-structure-as-final matrices can be
used to calculate useful aggregations when last stage is final.

Whereas
[`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md)
and
[`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md)
work independently of the last stage of an ECC, this function requires
both final and useful last stage ECCs to be present in `.sutdata`. Any
data with last state of services are ignored in this function.

Suffixes to matrix names are assumed to indicate the last stage of the
ECC for which the matrix applies. For example, two versions of the **R**
matrix should be present: `R_final` and `R_useful`.

If `.sutdata` is a wide-by-matrices data frame but contains a
`last_stage` column, `.sutdata` is pivoted wide (as a convenience) to
put data into the correct shape, forming columns for each combination of
ECC matrix and last stage. The `last_stage` and `R` columns make
`R_final` and `R_useful` columns. The `last_stage` and `V` columns make
`V_final` and `V_useful` columns. Etc. If either the last stage final or
the last stage useful ECC representations is missing, an error is
thrown. See examples.

Internally, this function uses
[`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md)
and
[`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md)
to complete its work.

Primary aggregates can be computed when last stage is final, useful, or
services. Ostensibly, the primary aggregates should be the same in all
cases when metadata are the same. An error is thrown if that is not true
to within `tol`.

## Examples

``` r
p_industries <- c("Resources [of Crude]", "Resources [of NG]")
fd_sectors <- c("Residential", "Transport", "Oil fields")
# Primary TOTAL aggregates
UKEnergy2000mats |>
  tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
  # Eliminate the case when last_stage == "Final"
  dplyr::filter(.data[[Recca::psut_cols$last_stage]] != "Final") |>
  pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                by = "Total")
#> # A tibble: 2 × 43
#>   Country  Year EnergyType R___lsUseful  U___lsUseful    U_feed___lsUseful
#>   <chr>   <dbl> <chr>      <list>        <list>          <list>           
#> 1 GBR      2000 E          <dbl [2 × 2]> <dbl [13 × 13]> <dbl [12 × 13]>  
#> 2 GBR      2000 X          <NULL>        <NULL>          <NULL>           
#> # ℹ 37 more variables: U_EIOU___lsUseful <list>, r_EIOU___lsUseful <list>,
#> #   V___lsUseful <list>, Y___lsUseful <list>, S_units___lsUseful <list>,
#> #   R___lsServices <list>, U___lsServices <list>, U_feed___lsServices <list>,
#> #   U_EIOU___lsServices <list>, r_EIOU___lsServices <list>,
#> #   V___lsServices <list>, Y___lsServices <list>, S_units___lsServices <list>,
#> #   EXpnet___lsFinal <list>, EXpgross___lsFinal <list>,
#> #   EXpnet___lsUseful <list>, EXpgross___lsUseful <list>, …
```
