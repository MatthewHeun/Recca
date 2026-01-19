# Physical Supply-Use Table (PSUT) data frame column names

A string list containing named names of columns in PSUT data frames.
Items in the list provide default values for column name function
arguments throughout the `Recca` package.

## Usage

``` r
psut_cols
```

## Format

A string list with 28 entries.

- resources,R:

  The name of a column in a wide-by-matrices data frame containing
  resource (`R`) matrices.

- U_feed:

  The name of a column in a wide-by-matrices data frame containing use
  (`U`) matrices that exclude energy industry own use.

- U_eiou:

  The name of a column in a wide-by-matrices data frame containing use
  (`U`) matrices that contain exclusively energy industry own use.

- U:

  The name of a column in a wide data-by-matrices frame containing use
  (`U`) matrices that are the sum of `U_feed` and `U_eiou` matrices.

- r_eiou:

  The name of a column in a wide-by-matrices data frame containing the
  ratio of `U_eiou` and `U` matrices.

- make,V:

  The name of a column in a wide-by-matrices data frame containing make
  (`V`) matrices.

- final_demand,Y:

  The name of a column in a wide-by-matrices data frame containing final
  demand (`Y`) matrices.

- phi:

  The name of a column in a wide-by-matrices data frame containing
  exergy-to-energy-ratio (**phi**) matrices.

- S_units:

  The name of a column in a wide-by-matrices data frame containing unit
  summation (`S_units`) matrices.

- Y_fu_details:

  The name of a column in a wide-by-matrices data frame containing
  details of the final-to-useful extension for final demand.

- U_eiou_fu_details:

  The name of a column in a wide-by-matrices data frame containing
  details of the final-to-useful extension for energy industry own use.

- IncludesNEU:

  The name of a column containing `TRUE` or `FALSE` for whether
  non-energy use is include in the energy conversion chain for the
  associated row.

- matnames:

  The name of a column in a tidy data frame containing matrix names.

- matvals:

  The name of a column in a tidy data frame containing matrices.

- sector:

  The name of a column in a tidy data frame containing sector names.

- country:

  The name of a column in a tidy data frame containing names of
  countries.

- year:

  The name of a column in a tidy data frame containing years.

- method:

  The name of a column in a tidy data frame containing the method for
  accounting for the primary equivalent of renewable electricity.

- energy_type:

  The name of a column in a tidy data frame containing names of energy
  types.

- last_stage:

  The name of a column in a tidy data frame identifying the last stage
  of the energy conversion chain.

- exergy_loss:

  The name of a column in a tidy data frame for vectors of exergy
  losses, i.e., the exergy associated with waste mass or energy.

- irreversibility:

  The name of a column in a tidy data frame for vectors of
  irreversibility, i.e., the exergy destroyed by thermodynamic
  inefficiencies in industries of a conversion chain.

## Details

Note that some of the values are repeated, thereby providing synonyms.
E.g., both `resources` and `R` point to the "R" column name.

## Examples

``` r
psut_cols
#> $resources
#> [1] "R"
#> 
#> $R
#> [1] "R"
#> 
#> $use
#> [1] "U"
#> 
#> $U
#> [1] "U"
#> 
#> $U_feed
#> [1] "U_feed"
#> 
#> $U_eiou
#> [1] "U_EIOU"
#> 
#> $r_eiou
#> [1] "r_EIOU"
#> 
#> $make
#> [1] "V"
#> 
#> $V
#> [1] "V"
#> 
#> $final_demand
#> [1] "Y"
#> 
#> $Y
#> [1] "Y"
#> 
#> $B
#> [1] "B"
#> 
#> $s_units
#> [1] "S_units"
#> 
#> $matvals
#> [1] "matvals"
#> 
#> $Y_fu_details
#> [1] "Y_fu_details"
#> 
#> $U_eiou_fu_details
#> [1] "U_EIOU_fu_details"
#> 
```
