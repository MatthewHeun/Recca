# All energy conversion chain stages

A string list containing options for the all stages of energy conversion
chain analysis.

## Usage

``` r
all_stages
```

## Format

A string list with 5

- primary:

  The string identifier for the Primary stage of the energy conversion
  chain.

- final:

  The string identifier for the Final stage of the energy conversion
  chain.

- useful:

  The string identifier for the Useful stage of the energy conversion
  chain.

- services:

  The string identifier for the Services stage of the energy conversion
  chain.

- last_stage_sep:

  A string that separates last-stage identifiers in variable names.

## Examples

``` r
all_stages
#> $primary
#> [1] "Primary"
#> 
#> $final
#> [1] "Final"
#> 
#> $useful
#> [1] "Useful"
#> 
#> $services
#> [1] "Services"
#> 
#> $last_stage_sep
#> [1] "___ls"
#> 
```
