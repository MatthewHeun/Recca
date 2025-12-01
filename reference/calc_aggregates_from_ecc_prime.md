# Calculate aggregates from list of reconstructed ECCs

This is a helper function for `footprint_aggregates()` and
`effects_aggregates()`. It calculates the primary and final demand
aggregates for a list of reconstructed energy conversion chains (ECCs)
in `ecc_prime`.

## Usage

``` r
calc_aggregates_from_ecc_prime(
  ecc_prime,
  calc_pfd_aggs,
  p_industries,
  fd_sectors,
  piece = "all",
  notation = RCLabels::notations_list,
  pattern_type = c("exact", "leading", "trailing", "anywhere", "literal"),
  prepositions = RCLabels::prepositions_list,
  product_sector,
  chop_df,
  aggregate_primary,
  gross_aggregate_demand,
  net_aggregate_demand,
  R_prime_colname,
  U_prime_colname,
  U_feed_prime_colname,
  U_eiou_prime_colname,
  r_eiou_prime_colname,
  V_prime_colname,
  Y_prime_colname,
  S_units_prime_colname
)
```

## Arguments

- ecc_prime:

  A list of reconstructed energy conversion chains.

- calc_pfd_aggs:

  Tells whether to calculate and add primary and final demand aggregates
  to the nested data frame.

- p_industries:

  A vector of names of industries to be aggregated as "primary." See
  `footprint_aggregates()` for details.

- fd_sectors:

  A vector of names of sectors in final demand. See
  `footprint_aggregates()` for details.

- piece:

  The piece of labels to be matched. Default is "all". See the
  `RCLabels` package.

- notation:

  The notation type for matching. Default is
  [`RCLabels::notations_list`](https://matthewheun.github.io/RCLabels/reference/notations_list.html).

- pattern_type:

  One of "exact", "leading", "trailing", or "anywhere" which specifies
  how matches are made for `p_industries`. See `footprint_aggregates()`
  for details.

- prepositions:

  Prepositions for notation matching. Default is
  [`RCLabels::prepositions_list`](https://matthewheun.github.io/RCLabels/reference/prepositions_list.html).

- product_sector:

  The name of the output column that contains the product, industry, or
  sector for which footprint aggregates are given.

- chop_df, aggregate_primary, net_aggregate_demand,
  gross_aggregate_demand:

  Names of output columns. See
  [`Recca::aggregate_cols`](https://matthewheun.github.io/Recca/reference/aggregate_cols.md).

- R_prime_colname, U_prime_colname, U_feed_prime_colname,
  U_eiou_prime_colname, r_eiou_prime_colname, V_prime_colname,
  Y_prime_colname, S_units_prime_colname:

  Names of output matrices in the return value. Default values are
  constructed from

## Value

A data frame containing reconstructed (prime) matrices and primary and
final demand aggregates in a list suitable for use in
[`matsindf::matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.html).

## Details

This is not a public function. It is an internal helper function for
`footprint_aggregates()` and `effects_aggregates()`.
