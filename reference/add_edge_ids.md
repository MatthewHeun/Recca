# Add edge ID numbers to an edge list

The edges in an edge list can have ID numbers. This functions adds a
column of edge ID numbers.

## Usage

``` r
add_edge_ids(edge_list, edge_id = "edge_id")
```

## Arguments

- edge_list:

  the edge list to which edge ID numbers are to be added

- edge_id:

  the name of the edge ID column in the outgoing edge list. (Default is
  "`edge_id`".)

## Value

`edge_list` with an added column containing the edge ID numbers.

## Examples

``` r
library(tidyr)
library(matsbyname)
sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
# Suppress adding edge IDs
elDF <- edge_list(sutmats, edge_id = NULL)$`Edge list`[[1]]
add_node_ids(elDF)
#>                    From                To Value             Product
#> 1  Resources [of Crude]        Oil fields 50000               Crude
#> 2           Crude dist.       Crude dist.   500  Crude [from Dist.]
#> 3           Crude dist.    Oil refineries 47000  Crude [from Dist.]
#> 4            Oil fields       Crude dist. 47500 Crude [from Fields]
#> 5            Oil fields        Oil fields  2500 Crude [from Fields]
#> 6        Oil refineries      Diesel dist. 15500              Diesel
#> 7        Oil refineries    Oil refineries  5000              Diesel
#> 8          Diesel dist.       Crude dist.    25 Diesel [from Dist.]
#> 9          Diesel dist.      Diesel dist.   350 Diesel [from Dist.]
#> 10         Diesel dist. Gas wells & proc.    50 Diesel [from Dist.]
#> 11         Diesel dist.          NG dist.    25 Diesel [from Dist.]
#> 12         Diesel dist.        Oil fields    50 Diesel [from Dist.]
#> 13         Diesel dist.      Petrol dist.   250 Diesel [from Dist.]
#> 14         Diesel dist.         Transport 14750 Diesel [from Dist.]
#> 15         Power plants       Elect. grid  6400               Elect
#> 16          Elect. grid       Crude dist.    25   Elect [from Grid]
#> 17          Elect. grid Gas wells & proc.    25   Elect [from Grid]
#> 18          Elect. grid          NG dist.    25   Elect [from Grid]
#> 19          Elect. grid        Oil fields    25   Elect [from Grid]
#> 20          Elect. grid    Oil refineries    75   Elect [from Grid]
#> 21          Elect. grid      Power plants   100   Elect [from Grid]
#> 22          Elect. grid       Residential  6000   Elect [from Grid]
#> 23    Resources [of NG] Gas wells & proc. 43000                  NG
#> 24             NG dist.      Power plants 16000     NG [from Dist.]
#> 25             NG dist.       Residential 25000     NG [from Dist.]
#> 26    Gas wells & proc. Gas wells & proc.  2000     NG [from Wells]
#> 27    Gas wells & proc.          NG dist. 41000     NG [from Wells]
#> 28       Oil refineries      Petrol dist. 26500              Petrol
#> 29         Petrol dist.      Petrol dist.   500 Petrol [from Dist.]
#> 30         Petrol dist.         Transport 26000 Petrol [from Dist.]
#> 31          Crude dist.             Waste   550               Waste
#> 32         Diesel dist.             Waste   350               Waste
#> 33          Elect. grid             Waste   125               Waste
#> 34    Gas wells & proc.             Waste  2075               Waste
#> 35             NG dist.             Waste    50               Waste
#> 36           Oil fields             Waste  2575               Waste
#> 37       Oil refineries             Waste  5075               Waste
#> 38         Petrol dist.             Waste   750               Waste
#> 39         Power plants             Waste  9700               Waste
#>    From_node_id.x To_node_id.x From_node_id.y To_node_id.y
#> 1               0            2              0            2
#> 2               1            1              1            1
#> 3               1            3              1            3
#> 4               2            1              2            1
#> 5               2            2              2            2
#> 6               3            4              3            4
#> 7               3            3              3            3
#> 8               4            1              4            1
#> 9               4            4              4            4
#> 10              4            9              4            9
#> 11              4            8              4            8
#> 12              4            2              4            2
#> 13              4           10              4           10
#> 14              4           11              4           11
#> 15              5            6              5            6
#> 16              6            1              6            1
#> 17              6            9              6            9
#> 18              6            8              6            8
#> 19              6            2              6            2
#> 20              6            3              6            3
#> 21              6            5              6            5
#> 22              6           12              6           12
#> 23              7            9              7            9
#> 24              8            5              8            5
#> 25              8           12              8           12
#> 26              9            9              9            9
#> 27              9            8              9            8
#> 28              3           10              3           10
#> 29             10           10             10           10
#> 30             10           11             10           11
#> 31              1           13              1           13
#> 32              4           13              4           13
#> 33              6           13              6           13
#> 34              9           13              9           13
#> 35              8           13              8           13
#> 36              2           13              2           13
#> 37              3           13              3           13
#> 38             10           13             10           13
#> 39              5           13              5           13
```
