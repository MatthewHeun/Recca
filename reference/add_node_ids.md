# Add node ID numbers to an edge list

Edge lists can contain identification numbers (integers) for each node.
Because each row in the edge list data frame contains a "`From`" node
and a "`To`" node, two columns of node IDs are added, one for "`From`"
and one for "`To`".

## Usage

``` r
add_node_ids(
  edge_list,
  from = "From",
  to = "To",
  node_id = "node_id",
  first_node = 0
)
```

## Arguments

- edge_list:

  the edge list to which node ID numbers are to be added

- from:

  the name of the column containing source nodes. (Default is "`From`".)

- to:

  the name of the column containing destination nodes. (Default is
  "`To`".)

- node_id:

  the root of the column name for node IDs. (Default is "`node_ID`".)
  See details.

- first_node:

  the ID number of the first node. (Default is `0`.)

## Value

`edge_list` with two additional columns containing `From` and `To` node
ID numbers.

## Details

The column names for the "`From`" and "`To`" nodes are created by
`paste`-ing the value of the `from` and `to` arguments with the value of
the `node_id` argument.

## Examples

``` r
library(matsbyname)
library(tidyr)
sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
# Suppress adding node IDs
elDF <- edge_list(sutmats, node_id = NULL)$`Edge list`[[1]]
add_node_ids(elDF)
#>                    From                To Value             Product edge_id
#> 1  Resources [of Crude]        Oil fields 50000               Crude       1
#> 2           Crude dist.       Crude dist.   500  Crude [from Dist.]       2
#> 3           Crude dist.    Oil refineries 47000  Crude [from Dist.]       3
#> 4            Oil fields       Crude dist. 47500 Crude [from Fields]       4
#> 5            Oil fields        Oil fields  2500 Crude [from Fields]       5
#> 6        Oil refineries      Diesel dist. 15500              Diesel       6
#> 7        Oil refineries    Oil refineries  5000              Diesel       7
#> 8          Diesel dist.       Crude dist.    25 Diesel [from Dist.]       8
#> 9          Diesel dist.      Diesel dist.   350 Diesel [from Dist.]       9
#> 10         Diesel dist. Gas wells & proc.    50 Diesel [from Dist.]      10
#> 11         Diesel dist.          NG dist.    25 Diesel [from Dist.]      11
#> 12         Diesel dist.        Oil fields    50 Diesel [from Dist.]      12
#> 13         Diesel dist.      Petrol dist.   250 Diesel [from Dist.]      13
#> 14         Diesel dist.         Transport 14750 Diesel [from Dist.]      14
#> 15         Power plants       Elect. grid  6400               Elect      15
#> 16          Elect. grid       Crude dist.    25   Elect [from Grid]      16
#> 17          Elect. grid Gas wells & proc.    25   Elect [from Grid]      17
#> 18          Elect. grid          NG dist.    25   Elect [from Grid]      18
#> 19          Elect. grid        Oil fields    25   Elect [from Grid]      19
#> 20          Elect. grid    Oil refineries    75   Elect [from Grid]      20
#> 21          Elect. grid      Power plants   100   Elect [from Grid]      21
#> 22          Elect. grid       Residential  6000   Elect [from Grid]      22
#> 23    Resources [of NG] Gas wells & proc. 43000                  NG      23
#> 24             NG dist.      Power plants 16000     NG [from Dist.]      24
#> 25             NG dist.       Residential 25000     NG [from Dist.]      25
#> 26    Gas wells & proc. Gas wells & proc.  2000     NG [from Wells]      26
#> 27    Gas wells & proc.          NG dist. 41000     NG [from Wells]      27
#> 28       Oil refineries      Petrol dist. 26500              Petrol      28
#> 29         Petrol dist.      Petrol dist.   500 Petrol [from Dist.]      29
#> 30         Petrol dist.         Transport 26000 Petrol [from Dist.]      30
#> 31          Crude dist.             Waste   550               Waste      31
#> 32         Diesel dist.             Waste   350               Waste      32
#> 33          Elect. grid             Waste   125               Waste      33
#> 34    Gas wells & proc.             Waste  2075               Waste      34
#> 35             NG dist.             Waste    50               Waste      35
#> 36           Oil fields             Waste  2575               Waste      36
#> 37       Oil refineries             Waste  5075               Waste      37
#> 38         Petrol dist.             Waste   750               Waste      38
#> 39         Power plants             Waste  9700               Waste      39
#>    From_node_id To_node_id
#> 1             0          2
#> 2             1          1
#> 3             1          3
#> 4             2          1
#> 5             2          2
#> 6             3          4
#> 7             3          3
#> 8             4          1
#> 9             4          4
#> 10            4          9
#> 11            4          8
#> 12            4          2
#> 13            4         10
#> 14            4         11
#> 15            5          6
#> 16            6          1
#> 17            6          9
#> 18            6          8
#> 19            6          2
#> 20            6          3
#> 21            6          5
#> 22            6         12
#> 23            7          9
#> 24            8          5
#> 25            8         12
#> 26            9          9
#> 27            9          8
#> 28            3         10
#> 29           10         10
#> 30           10         11
#> 31            1         13
#> 32            4         13
#> 33            6         13
#> 34            9         13
#> 35            8         13
#> 36            2         13
#> 37            3         13
#> 38           10         13
#> 39            5         13
```
