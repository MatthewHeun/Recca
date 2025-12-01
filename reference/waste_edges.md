# Create waste energy edges for an edge map

Waste edges are created from the `W` matrix.

## Usage

``` r
waste_edges(
  U_mat,
  V_mat,
  from = "From",
  to = "To",
  value = "Value",
  product = "Product",
  waste = "Waste"
)
```

## Arguments

- U_mat:

  a use matrix.

- V_mat:

  a make matrix.

- from:

  the name of the edge list column containing source nodes. (Default is
  "`From`".)

- to:

  the name of the edge list column containing destination nodes.
  (Default is "`To`".)

- value:

  the name of the edge list column containing magnitudes of the flows.
  (Default is "`Value`".)

- product:

  the name of the edge list column containing the product of the edge
  list flow. (Default is "`Product`".)

- waste:

  the name of the waste product and the destination node for wastes.
  (Default is "`Waste`".)

## Value

waste energy edges computed from the `Umat` and `Vmat` matrices

## Details

The `waste` argument supplies both the name of the waste flow (default
is "`Waste`") and the name of the destination of the waste flows.

## Examples

``` r
library(dplyr)
library(matsbyname)
library(tidyr)
sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
edge_list(sutmats)$`Edge list`[[1]] %>% filter(Product == "Waste")
#>                From    To Value Product edge_id From_node_id To_node_id
#> 1       Crude dist. Waste   550   Waste      31            1         13
#> 2      Diesel dist. Waste   350   Waste      32            4         13
#> 3       Elect. grid Waste   125   Waste      33            6         13
#> 4 Gas wells & proc. Waste  2075   Waste      34            9         13
#> 5          NG dist. Waste    50   Waste      35            8         13
#> 6        Oil fields Waste  2575   Waste      36            2         13
#> 7    Oil refineries Waste  5075   Waste      37            3         13
#> 8      Petrol dist. Waste   750   Waste      38           10         13
#> 9      Power plants Waste  9700   Waste      39            5         13
```
