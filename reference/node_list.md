# Create a node list

A node list is a data frame containing node names and associated node ID
numbers (integers). This function creates a node list from an edge list,
as shown in the examples.

## Usage

``` r
node_list(
  edge_list,
  from = "From",
  to = "To",
  node = "Node",
  node_id = "node_id"
)
```

## Arguments

- edge_list:

  the name of the column in `.sutmats` containing edge lists or a single
  edge list data frame. (Default is "`Edge list`".)

- from:

  the name of the `edge_list` column containing names of source nodes.
  (Default is "`From`".)

- to:

  the name of the `edge_list` column containing names of destination
  nodes. (Default is "`To`".)

- node:

  the name of the output column containing node names. (Default is
  "`Node`".)

- node_id:

  the name of the output column containing node ID numbers. (Default is
  "`node_id`".)

## Value

a node list

## Details

See
[`edge_list`](https://matthewheun.github.io/Recca/reference/edge_list.md)
for a function to create edge lists.

## Examples

``` r
library(matsbyname)
library(tidyr)
sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
el <- edge_list(sutmats)$`Edge list`[[1]]
node_list(el)
#>                    Node node_id
#> 1  Resources [of Crude]       0
#> 2           Crude dist.       1
#> 4            Oil fields       2
#> 6        Oil refineries       3
#> 8          Diesel dist.       4
#> 15         Power plants       5
#> 16          Elect. grid       6
#> 23    Resources [of NG]       7
#> 24             NG dist.       8
#> 26    Gas wells & proc.       9
#> 29         Petrol dist.      10
#> 53            Transport      11
#> 61          Residential      12
#> 70                Waste      13
```
