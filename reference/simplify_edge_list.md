# Simplify an edge list

A PSUT energy conversion chain edge can be simplified if a product has
only one supplier (i.e., one "from"). Then, every "to" node for that
product can have the product's "from" be its "from". See examples.

## Usage

``` r
simplify_edge_list(
  edge_list,
  from = "From",
  to = "To",
  value = "Value",
  product = "Product"
)
```

## Arguments

- edge_list:

  the edge list to be simplified

- from:

  the name of the from column in the edge list. (Default is "`From`".)

- to:

  the name of the to column in the edge list. (Default is "`To`".)

- value:

  the name of the value column in the edge list. (Default is "`Value`".)

- product:

  the name of the product column in the edge list. (Default is
  "`Product`".)

## Value

a simplified edge list

## Examples

``` r
el <- data.frame(From = c("A", "Oil"), To = c("Oil", "C"),
                 Value = c(42, 42), Product = c("Oil", "Oil"),
                 stringsAsFactors = FALSE)
# Oil flows from A to C through its product node (Oil)
el
#>   From  To Value Product
#> 1    A Oil    42     Oil
#> 2  Oil   C    42     Oil
# Simplify to have Oil flow from A to C without the product node
simplify_edge_list(el)
#>   From To Value Product
#> 1    A  C    42     Oil
```
