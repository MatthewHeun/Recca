# Efficiency data frame column names

A string list containing named names of columns in efficiency data
frames. Items in the list provide default values for column name
function arguments to efficiency functions throughout the `Recca`
package.

## Usage

``` r
efficiency_cols
```

## Format

A string list with 14 entries.

- eta_i:

  The name of a column in a wide-by-matrices data frame containing
  industry efficiencies.

- eta_pfd_gross:

  The name of a column in a wide-by-matrices data frame containing
  efficiencies between the primary and gross final demand stages.

- eta_pfd_net:

  The name of a column in a wide-by-matrices data frame containing
  efficiencies between the primary and net final demand stages.

- eta_pfd:

  The name of a column in a wide-by-matrices data frame containing
  efficiencies between the primary and final demand stages, regardless
  of net or gross.

- eta_pf:

  The string name for primary-to-final efficiency.

- eta_fu:

  The string name for final-to-useful efficiency.

- eta_pu:

  The string name for primary-to-useful efficiency.

- eta_ps:

  The string name for primary-to-services efficiency.

- eta_fs:

  The string name for final-to-services efficiency.

- eta_us:

  The string name for useful-to-services efficiency.

- efficiency_name_suffix:

  The suffix for names of columns containing efficiency names.

- gross_net:

  The name of a column that contains "Gross" or "Net" for the type of
  efficiency.

- gross:

  The entry in the `gross_net` column that identifies a gross
  efficiency.

- net:

  The entry in the `gross_net` column that identifies a net efficiency.

## Examples

``` r
efficiency_cols
#> $eta_i
#> [1] "eta_i"
#> 
#> $eta_pfd_gross
#> [1] "etapfdgross"
#> 
#> $eta_pfd_net
#> [1] "etapfdnet"
#> 
#> $eta_pfd
#> [1] "etapfd"
#> 
#> $eta_pf
#> [1] "etapf"
#> 
#> $eta_fu
#> [1] "etafu"
#> 
#> $eta_pu
#> [1] "etapu"
#> 
#> $eta_ps
#> [1] "etaps"
#> 
#> $eta_fs
#> [1] "etafs"
#> 
#> $eta_us
#> [1] "etaus"
#> 
#> $efficiency_name_suffix
#> [1] "_name"
#> 
#> $gross_net
#> [1] "GrossNet"
#> 
#> $gross
#> [1] "Gross"
#> 
#> $net
#> [1] "Net"
#> 
```
