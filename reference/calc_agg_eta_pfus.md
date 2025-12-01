# Calculate aggregations and efficiencies

Given a `matsindf`-style data frame of energy conversion chains (ECCs),
this function calculates primary, final, useful, and services (when
available) aggregates and associated efficiencies. Columns containing
PSUT matrices are not returned in the output.

## Usage

``` r
calc_agg_eta_pfus(
  .psut_df,
  p_industries,
  fd_sectors,
  remove_psut_cols = TRUE,
  piece = "noun",
  notation = list(RCLabels::bracket_notation, RCLabels::arrow_notation),
  pattern_type = "exact",
  prepositions = RCLabels::prepositions_list,
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  U_feed = Recca::psut_cols$U_feed,
  U_eiou = Recca::psut_cols$U_eiou,
  r_eiou = Recca::psut_cols$r_eiou,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  S_units = Recca::psut_cols$S_units,
  gross = Recca::efficiency_cols$gross,
  net = Recca::efficiency_cols$net,
  gross_net = Recca::efficiency_cols$gross_net,
  last_stage = Recca::psut_cols$last_stage,
  primary = Recca::all_stages$primary,
  final = Recca::all_stages$final,
  useful = Recca::all_stages$useful,
  services = Recca::all_stages$services,
  ex_p = Recca::aggregate_cols$aggregate_primary,
  ex_f = Recca::aggregate_cols$aggregate_final,
  ex_u = Recca::aggregate_cols$aggregate_useful,
  ex_s = Recca::aggregate_cols$aggregate_services,
  ex_fd_gross = Recca::aggregate_cols$gross_aggregate_demand,
  ex_fd_net = Recca::aggregate_cols$net_aggregate_demand,
  ex_fd = Recca::aggregate_cols$aggregate_demand,
  eta_pf = Recca::efficiency_cols$eta_pf,
  eta_fu = Recca::efficiency_cols$eta_fu,
  eta_us = Recca::efficiency_cols$eta_us,
  eta_pu = Recca::efficiency_cols$eta_pu,
  eta_ps = Recca::efficiency_cols$eta_ps,
  eta_fs = Recca::efficiency_cols$eta_fs
)
```

## Arguments

- .psut_df:

  A data frame of energy conversion chain data in PSUT format.

- p_industries:

  A string vector of primary industries.

- fd_sectors:

  A string vector of final demand sectors.

- remove_psut_cols:

  A boolean telling whether to delete columns containing PSUT matrices.
  Default is `TRUE`.

- piece:

  The piece of the labels used for matching. Default is "noun".

- notation:

  The notation used for row and column labels. Default is
  `list(RCLabels::bracket_notation, RCLabels::arrow_notation)`.

- pattern_type:

  The pattern type to be used for row and column matching. Default is
  "exact".

- prepositions:

  A list of prepositions for row and column labels. Default is
  [`RCLabels::prepositions_list`](https://matthewheun.github.io/RCLabels/reference/prepositions_list.html).

- R, U, V, Y, r_eiou, U_eiou, U_feed, S_units, last_stage:

  String names of matrix columns in `.psut_df`. See
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md).

- gross, net, gross_net:

  See
  [`Recca::efficiency_cols`](https://matthewheun.github.io/Recca/reference/efficiency_cols.md).

- primary, final, useful, services:

  See
  [`IEATools::all_stages`](https://matthewheun.github.io/IEATools/reference/all_stages.html).

- ex_p, ex_fd_gross, ex_fd_net, ex_fd:

  Names of aggregate columns. See
  [`Recca::aggregate_cols`](https://matthewheun.github.io/Recca/reference/aggregate_cols.md).

- ex_f, ex_u, ex_s:

  See
  [`IEATools::aggregate_cols`](https://matthewheun.github.io/IEATools/reference/aggregate_cols.html).

- eta_pf, eta_fu, eta_us, eta_pu, eta_ps, eta_fs:

  See
  [`Recca::efficiency_cols`](https://matthewheun.github.io/Recca/reference/efficiency_cols.md).

## Value

A data frame of metadata columns; primary, final, useful, and services
aggregations; and efficiencies.

## Details

Final, useful, and services data are assumed to be contained in the
final demand matrix (**Y**) on various rows of `.psut_df`, identified by
the `last_stage` column in `.psut_df`. This function will still work,
even if primary energy is different for each last stage.

Internally, primary aggregates are calculated using
[`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md),
final demand aggregates are calculated using
[`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md).
The meaning of final demand aggregates for each row of `.psut_df` is
determined by the corresponding value in the `last_stage` column.

Note that when an ECC stage is not present, its aggregation and
efficiency columns will be removed from output.

If a services stage is present, its efficiencies will have mixed units
and might be meaningless. Proceed with caution.

## Examples

``` r
p_industries <- "Resources"
fd_sectors <- c("Residential", "Transport", "Oil fields")
UKEnergy2000mats |>
  tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
  calc_agg_eta_pfus(p_industries = p_industries, fd_sectors = fd_sectors)
#> # A tibble: 8 × 15
#>   Country  Year EnergyType LastStage GrossNet   EXp   EXf     EXs    EXu  etapf
#>   <chr>   <dbl> <chr>      <chr>     <chr>    <dbl> <dbl>   <dbl>  <dbl>  <dbl>
#> 1 GBR      2000 E          Final     Gross    93000 74325 5.01e14 25990.  0.799
#> 2 GBR      2000 E          Final     Net      93000 71750 5.01e14 25915.  0.772
#> 3 GBR      2000 E          Services  Gross    93000 74325 5.01e14 25990.  0.799
#> 4 GBR      2000 E          Services  Net      93000 71750 5.01e14 25915.  0.772
#> 5 GBR      2000 E          Useful    Gross    93000 74325 5.01e14 25990.  0.799
#> 6 GBR      2000 E          Useful    Net      93000 71750 5.01e14 25915.  0.772
#> 7 GBR      2000 X          Services  Gross    98220    NA 5.01e14    NA  NA    
#> 8 GBR      2000 X          Services  Net      98220    NA 5.01e14    NA  NA    
#> # ℹ 5 more variables: etafu <dbl>, etapu <dbl>, etaps <dbl>, etafs <dbl>,
#> #   etaus <dbl>
```
