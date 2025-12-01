# Calculate aggregate (total) efficiencies

Calculates aggregate (total) primary-to-final demand (gross and net)
efficiencies across the energy conversion chain. `.aggregate_df` is
probably formed by joining the results from
[`primary_aggregates()`](https://matthewheun.github.io/Recca/reference/primary_aggregates.md)
and
[`finaldemand_aggregates()`](https://matthewheun.github.io/Recca/reference/finaldemand_aggregates.md)
or by calling `footprint_aggregates()`. See examples.

## Usage

``` r
calc_eta_pfd(
  .aggregate_df = NULL,
  efficiency_name_suffix = Recca::efficiency_cols$efficiency_name_suffix,
  aggregate_primary_colname = Recca::aggregate_cols$aggregate_primary,
  gross_aggregate_demand_colname = Recca::aggregate_cols$gross_aggregate_demand,
  net_aggregate_demand_colname = Recca::aggregate_cols$net_aggregate_demand,
  energy_type = Recca::psut_cols$energy_type,
  last_stage = Recca::psut_cols$last_stage,
  eta_pfd_gross = Recca::efficiency_cols$eta_pfd_gross,
  eta_pfd_net = Recca::efficiency_cols$eta_pfd_net,
  eta_pfd_gross_colname = paste0(eta_pfd_gross, efficiency_name_suffix),
  eta_pfd_net_colname = paste0(eta_pfd_net, efficiency_name_suffix)
)
```

## Arguments

- .aggregate_df:

  A data frame or list containing columns `aggregate_primary_colname`,
  `net_aggregate_demand_colname`, `gross_aggregate_demand_colname`,
  probably the result of calling `footprint_aggregates()`.

- efficiency_name_suffix:

  The suffix for efficiency names. Default is
  `Recca::efficiency_cols$efficiency_name_suffix`.

- aggregate_primary_colname:

  The name of the column in `p_aggregates` that contains primary energy
  or exergy aggregates. Default is
  `Recca::aggregate_cols$aggregate_primary`.

- gross_aggregate_demand_colname:

  The name of the column in `finaldemand_aggregates` that contains gross
  final demand aggregates. Default is
  `Recca::aggregate_cols$gross_aggregate_demand`.

- net_aggregate_demand_colname:

  The name of the column in `finaldemand_aggregates` that contains net
  final demand aggregates. Default is
  `Recca::aggregate_cols$net_aggregate_demand`.

- energy_type:

  The name of the energy type column. Default is
  `Recca::psut_cols$energy_type`.

- last_stage:

  The name of the last stage column. Default is
  `Recca::psut_cols$last_stage`.

- eta_pfd_gross:

  The name of the output column containing efficiencies of converting
  primary energy into gross final demand energy. Default is
  `Recca::efficiency_cols$eta_pfd_gross`.

- eta_pfd_net:

  The name of the output column containing efficiencies of converting
  primary energy into net final demand energy. Default is
  `Recca::efficiency_cols$eta_pfd_net`.

- eta_pfd_gross_colname:

  The name of the output column containing names of gross efficiency
  parameters. Default is
  `paste0(eta_pfd_gross, efficiency_name_suffix)`.

- eta_pfd_net_colname:

  The name of the output column containing names of net efficiency
  parameters. Default is `paste0(eta_pfd_net, efficiency_name_suffix)`.

## Value

A data frame of aggregate efficiencies.

## Examples

``` r
wide <- primary_total_aggregates_sut <- UKEnergy2000mats %>%
  tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)
# Define primary industries
p_industries <- c("Resources - Crude", "Resources - NG")
primary_total_aggregates <- wide %>%
  Recca::primary_aggregates(p_industries = p_industries, by = "Total") %>%
  # Don't need the matrices
  dplyr::select(IEATools::iea_cols$country,
                IEATools::iea_cols$year,
                IEATools::iea_cols$energy_type,
                IEATools::iea_cols$last_stage,
                Recca::aggregate_cols$aggregate_primary)
# Define final demand sectors
fd_sectors <- c("Residential", "Transport", "Oil fields")
finaldemand_total_aggregates <- wide %>%
  Recca::finaldemand_aggregates(fd_sectors = fd_sectors, by = "Total") %>%
  # Don't need the matrices
  dplyr::select(IEATools::iea_cols$country,
                IEATools::iea_cols$year,
                IEATools::iea_cols$energy_type,
                IEATools::iea_cols$last_stage,
                Recca::aggregate_cols$gross_aggregate_demand,
                Recca::aggregate_cols$net_aggregate_demand)
dplyr::full_join(primary_total_aggregates,
                 finaldemand_total_aggregates,
                 by = c(IEATools::iea_cols$country,
                        IEATools::iea_cols$year,
                        IEATools::iea_cols$energy_type,
                        IEATools::iea_cols$last_stage)) %>%
  calc_eta_pfd()
#> # A tibble: 4 × 9
#>   Country  Year EnergyType LastStage   EXp EXfdgross EXfdnet etapfdgross
#>   <chr>   <dbl> <chr>      <chr>     <dbl>     <dbl>   <dbl>       <dbl>
#> 1 GBR      2000 E          Final         0   7.43e 4 7.17e 4         Inf
#> 2 GBR      2000 E          Services      0   5.01e14 5.01e14         Inf
#> 3 GBR      2000 E          Useful        0   2.60e 4 2.59e 4         Inf
#> 4 GBR      2000 X          Services      0   5.01e14 5.01e14         Inf
#> # ℹ 1 more variable: etapfdnet <dbl>
```
