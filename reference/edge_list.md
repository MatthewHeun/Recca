# Create an edge list

An edge list is a data frame in which each row describes a flow from one
entity to another. Columns in the edge list data frame are `From`, `To`,
`Value`, and `Product`. The edge list can be created from the `U`, `V`,
and `Y` matrices of an energy conversion chain. Edge lists is a typical
data format for visualization software.

## Usage

``` r
edge_list(
  .sutdata = NULL,
  R = "R",
  U = "U",
  V = "V",
  Y = "Y",
  edge_list = "Edge list",
  from = "From",
  to = "To",
  value = "Value",
  product = "Product",
  waste = "Waste",
  rowtypes = "rowtype",
  coltypes = "coltype",
  node_id = "node_id",
  first_node = 0,
  edge_id = "edge_id",
  simplify_edges = TRUE
)
```

## Arguments

- .sutdata:

  Optionally, a data frame containing columns named with the values of
  the `U`, `V`, and `Y` arguments.

- R:

  a resource matrix or the name of a column in `.sutdata` containing
  resource matrices. (Default is "`R`".)

- U:

  a use matrix or the name of a column in `.sutdata` containing use
  matrices. (Default is "`U`".)

- V:

  a make matrix or the name of a column in `.sutdata` containing make
  matrices. (Default is "`V`".)

- Y:

  a final demand matrix or the name of a column in `.sutdata` containing
  final demand matrices. (Default is "`Y`".)

- edge_list:

  the name of the column in the output data frame containing edge lists.
  Or the name of the item in the return list if .sutdata is not
  specified. (Default is "`Edge list`".)

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

  the name of the waste product and the destination node for wastes. Set
  `NULL` to suppress addition of waste edges. (Default is "`Waste`".)

- rowtypes:

  the name of the rowtypes. (Default is "`rowtype`".)

- coltypes:

  the name of the rowtypes. (Default is "`coltype`".)

- node_id:

  the base name of node ID columns. Set `NULL` to suppress addition of
  node ID numbers. (Default is "`node_id`".)

- first_node:

  the first node number. (Default is `0`.)

- edge_id:

  the name of the edge ID column. Set `NULL` to suppress addition of
  edge ID numbers. (Default is "`edge_id`".)

- simplify_edges:

  if `TRUE`, products with only one source will not have a node. The
  source of the product will be connected directly to its consumers. If
  `FALSE`, no simplifications are made. (Default is `TRUE`.)

## Value

an edge list or a column of edge lists

## Details

Optionally, waste streams can be calculated from the `U` and `V`
matrices and added to the edge list. Optionally, edges can be simplified
when a product has a single source. In that event, the node named after
the product is removed, and destinations are connected to the sources.

## Examples

``` r
library(matsbyname)
library(tidyr)
sutmats <- UKEnergy2000mats %>%
  spread(key = matrix.name, value = matrix)
# Don't simplify edges and don't include waste edges
el_basic <- edge_list(sutmats, simplify_edges = FALSE)
head(el_basic$`Edge list`[[1]])
#>                  From           To Value             Product edge_id
#> 1  Crude [from Dist.]  Crude dist.   500  Crude [from Dist.]       1
#> 2 Crude [from Fields]  Crude dist. 47500 Crude [from Fields]       2
#> 3 Diesel [from Dist.]  Crude dist.    25 Diesel [from Dist.]       3
#> 4   Elect [from Grid]  Crude dist.    25   Elect [from Grid]       4
#> 5              Diesel Diesel dist. 15500              Diesel       5
#> 6 Diesel [from Dist.] Diesel dist.   350 Diesel [from Dist.]       6
#>   From_node_id To_node_id
#> 1            0         14
#> 2            1         14
#> 3            2         14
#> 4            3         14
#> 5            4         15
#> 6            2         15
tail(el_basic$`Edge list`[[1]])
#>                 From    To Value Product edge_id From_node_id To_node_id
#> 46 Gas wells & proc. Waste  2075   Waste      46           17         25
#> 47          NG dist. Waste    50   Waste      47           18         25
#> 48        Oil fields Waste  2575   Waste      48           19         25
#> 49    Oil refineries Waste  5075   Waste      49           20         25
#> 50      Petrol dist. Waste   750   Waste      50           21         25
#> 51      Power plants Waste  9700   Waste      51           22         25
# Simplify edges and include waste
el <- edge_list(sutmats)
head(el$`Edge list`[[1]])
#>                   From             To Value             Product edge_id
#> 1 Resources [of Crude]     Oil fields 50000               Crude       1
#> 2          Crude dist.    Crude dist.   500  Crude [from Dist.]       2
#> 3          Crude dist. Oil refineries 47000  Crude [from Dist.]       3
#> 4           Oil fields    Crude dist. 47500 Crude [from Fields]       4
#> 5           Oil fields     Oil fields  2500 Crude [from Fields]       5
#> 6       Oil refineries   Diesel dist. 15500              Diesel       6
#>   From_node_id To_node_id
#> 1            0          2
#> 2            1          1
#> 3            1          3
#> 4            2          1
#> 5            2          2
#> 6            3          4
# Now includes waste edges
tail(el$`Edge list`[[1]])
#>                 From    To Value Product edge_id From_node_id To_node_id
#> 34 Gas wells & proc. Waste  2075   Waste      34            9         13
#> 35          NG dist. Waste    50   Waste      35            8         13
#> 36        Oil fields Waste  2575   Waste      36            2         13
#> 37    Oil refineries Waste  5075   Waste      37            3         13
#> 38      Petrol dist. Waste   750   Waste      38           10         13
#> 39      Power plants Waste  9700   Waste      39            5         13
# Works with single matrices, too.
elmats <- edge_list(R = sutmats$R[[1]], U = sutmats$U[[1]], V = sutmats$V[[1]], Y = sutmats$Y[[1]])
head(elmats[["Edge list"]])
#>                   From             To Value             Product edge_id
#> 1 Resources [of Crude]     Oil fields 50000               Crude       1
#> 2          Crude dist.    Crude dist.   500  Crude [from Dist.]       2
#> 3          Crude dist. Oil refineries 47000  Crude [from Dist.]       3
#> 4           Oil fields    Crude dist. 47500 Crude [from Fields]       4
#> 5           Oil fields     Oil fields  2500 Crude [from Fields]       5
#> 6       Oil refineries   Diesel dist. 15500              Diesel       6
#>   From_node_id To_node_id
#> 1            0          2
#> 2            1          1
#> 3            1          3
#> 4            2          1
#> 5            2          2
#> 6            3          4
tail(elmats[["Edge list"]])
#>                 From    To Value Product edge_id From_node_id To_node_id
#> 34 Gas wells & proc. Waste  2075   Waste      34            9         13
#> 35          NG dist. Waste    50   Waste      35            8         13
#> 36        Oil fields Waste  2575   Waste      36            2         13
#> 37    Oil refineries Waste  5075   Waste      37            3         13
#> 38      Petrol dist. Waste   750   Waste      38           10         13
#> 39      Power plants Waste  9700   Waste      39            5         13
```
