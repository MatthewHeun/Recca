# Balance column names

A string list containing named names of columns in balance data frames.
Items in the list provide default values for column name function
arguments to balance functions throughout the `Recca` package.

## Usage

``` r
balance_cols
```

## Format

A string list with 10 entries.

- inter_industry_balance_colname:

  The name of a column that contains inter-industry balance vectors.

- inter_industry_balanced_colname:

  The name of a column that tells whether inter-industry balance vectors
  are all **0**.

- between_industry_balance_colname:

  The name of a column that contains inter-industry balance vectors.

- between_industry_balanced_colname:

  The name of a column that contains inter-industry balance vectors are
  all **0**.

- intra_industry_balance_colname:

  The name of a column that contains intra-industry balance vectors.

- intra_industry_balanced_colname:

  The name of a column that contains intra-industry balance vectors are
  all **0**.

- across_industry_balance_colname:

  The name of a column that contains intra-industry balance vectors.

- across_industry_balanced_colname:

  The name of a column that contains intra-industry balance vectors are
  all **0**.

- waste_heat:

  The name of a column in **V** and a row in **Y** that contains waste
  heat calculated from intra-industry balances.

- losses_sector:

  The name of a column in **Y** that contains losses calculated from
  intra-industry balances.

## Examples

``` r
balance_cols
#> $inter_industry_balance_colname
#> [1] "SUTInterIndustryBalance"
#> 
#> $inter_industry_balanced_colname
#> [1] "SUTInterIndustryBalanced"
#> 
#> $between_industry_balance_colname
#> [1] "SUTInterIndustryBalance"
#> 
#> $between_industry_balanced_colname
#> [1] "SUTInterIndustryBalanced"
#> 
#> $intra_industry_balance_colname
#> [1] "SUTIntraIndustryBalance"
#> 
#> $intra_industry_balanced_colname
#> [1] "SUTIntraIndustryBalanced"
#> 
#> $across_industry_balance_colname
#> [1] "SUTAcrossIndustryBalance"
#> 
#> $across_industry_balanced_colname
#> [1] "SUTAcrossIndustryBalanced"
#> 
#> $waste_heat
#> [1] "Waste heat"
#> 
#> $losses_sector
#> [1] "Losses"
#> 
```
