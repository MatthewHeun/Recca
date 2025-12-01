# Scrape primary industry names from R, V, and Y matrices

Primary industry names are needed for aggregation. This function
interrogates the row names of R and V and the column names Y matrices
for names that start with `p_industries`. The assumption is that many of
these row and column names may have compound names of the form
"Resources \[of Oil and gas extraction\]". So this function looks for
leading strings. If "Resources" is in `p_industries`, "Resources \[of
Oil and gas extraction\]" will be among the returned strings.

## Usage

``` r
find_p_industry_names(
  .sutdata = NULL,
  p_industry_prefixes = Recca::industry_cols$p_industry_prefixes,
  R = Recca::psut_cols$R,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  p_industries_complete = Recca::industry_cols$p_industries_complete
)
```

## Arguments

- .sutdata:

  An optional data frame containing columns of PSUT matrices

- p_industry_prefixes:

  The name of a column in `.sutdata` containing vectors of prefixes that
  identify primary industry names, or a vector of prefixes that identify
  primary industry names. Default is
  `Recca::industry_cols$p_industry_prefixes`. Hint:
  [`IEATools::tpes_flows`](https://matthewheun.github.io/IEATools/reference/tpes_flows.html)
  contains a good list of primary industry prefixes.

- R:

  The name of the `R` matrix column in `.sutdata` or an `R` matrix.

- V:

  The name of the `V` matrix column in `.sutdata` or a `V` matrix.

- Y:

  The name of the `Y` matrix column in `.sutdata` or a `Y` matrix.

- p_industries_complete:

  The name of the output column containing complete names of primary
  industries. Default is `Recca::industry_cols$p_industries_complete`.

## Value

If `.sutdata` is a data frame, a data frame with additional column
`p_industries_complete`. If `.sutdata` is a list of named matrices (`R`,
`V`, and `Y`), A vector or vectors of full names of primary industries
in the `R`, `V`, and `Y` matrices, a list of primary industries.

## Details

Note all of `R`, `V`, and `Y` need to be specified.

## Examples

``` r
Rrows <- c("Resources [of Oil and gas extraction]", "Resources [of Coal mines]")
R <- matrix(c(1, 0,
              0, 2), nrow = 2, byrow = TRUE,
            dimnames = list(Rrows, c("Crude oil", "Brown coal")))
Vrows <- c("Imports [of Crude oil]", "Stock changes [of Bituminous coal]")
V <- matrix(c(3, 0,
              0, 4), nrow = 2, byrow = TRUE,
            dimnames = list(Vrows, c("Crude oil", "Bituminous coal")))
Ycols <- c("Exports [of Electricity]", "International marine bunkers [of Gas/diesel oil]")
Y <- matrix(c(5, 0,
              0, 6), nrow = 2, byrow = TRUE,
            dimnames = list(c("Electricity", "Gas/diesel oil"), Ycols))
p_industry_prefixes <- c("Resources", "Imports", "Exports",
                         "Stock changes", "International marine bunkers")
# This function works with individual matrices, so long as they are
# first wrapped in `list()`.
find_p_industry_names(p_industry_prefixes = list(p_industry_prefixes),
                      R = list(R), V = list(V), Y = list(Y))
#> $p_industries_complete
#> [1] "Resources [of Oil and gas extraction]"           
#> [2] "Resources [of Coal mines]"                       
#> [3] "Imports [of Crude oil]"                          
#> [4] "Stock changes [of Bituminous coal]"              
#> [5] "Exports [of Electricity]"                        
#> [6] "International marine bunkers [of Gas/diesel oil]"
#> 
# Also works in the context of a data frame.
# Use a `tibble`, because it handles matrices better
res <- tibble::tibble(R = list(R,R), V = list(V,V), Y = list(Y,Y),
                     p_industries = list(p_industry_prefixes, "Resources")) %>%
 find_p_industry_names(p_industry_prefixes = "p_industries")
res$p_industries_complete[[1]]
#> [1] "Resources [of Oil and gas extraction]"           
#> [2] "Resources [of Coal mines]"                       
#> [3] "Imports [of Crude oil]"                          
#> [4] "Stock changes [of Bituminous coal]"              
#> [5] "Exports [of Electricity]"                        
#> [6] "International marine bunkers [of Gas/diesel oil]"
res$p_industries_complete[[2]]
#> [1] "Resources [of Oil and gas extraction]"
#> [2] "Resources [of Coal mines]"            
```
