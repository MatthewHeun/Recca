# Make a Sankey diagram

A Sankey diagram is a flow diagram in which the width of the lines is
proportional to the rate of energy flow. Sankey diagrams are a helpful
way to visualize energy flows in an energy conversion chain (ECC). This
function takes a matrix description of an ECC and produces a Sankey
diagram.

## Usage

``` r
make_sankey(
  .sutmats = NULL,
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  simplify_edges = TRUE,
  colour_string = NULL,
  name_col = "name",
  colour_col = "colour",
  sankey = Recca::sankey_cols$sankey,
  ...
)
```

## Arguments

- .sutmats:

  An optional wide-by-matrices data frame

- R, U, V, Y:

  See
  [`Recca::psut_cols`](https://matthewheun.github.io/Recca/reference/psut_cols.md).

- simplify_edges:

  A boolean which tells whether edges should be simplified. Applies to
  every row of `.sutmats` if `.sutmats` is specified.

- colour_string:

  An optional Javascript string that defines colours for the Sankey
  diagram, appropriate for use by
  [`networkD3::sankeyNetwork()`](https://rdrr.io/pkg/networkD3/man/sankeyNetwork.html).
  Default is `NULL`, meaning that the default color palette should be
  used, namely `networkD3::JS("d3.scaleOrdinal(d3.schemeCategory20);")`.
  Can be a data frame with `name_col` and `colour_col` columns in which
  case
  [`create_sankey_colour_string()`](https://matthewheun.github.io/Recca/reference/create_sankey_colour_string.md)
  is called internally with `colour_string`, `name_col`, and
  `colour_col` as the arguments.

- name_col, colour_col:

  The names of columns in `colour_df` for names of nodes and flows
  (`name_col`) and colours of nodes and flows (`colour_col`). Defaults
  are "name" and "colour", respectively.

- sankey:

  See
  [`Recca::sankey_cols`](https://matthewheun.github.io/Recca/reference/sankey_cols.md).

- ...:

  Arguments passed to
  [`networkD3::sankeyNetwork()`](https://rdrr.io/pkg/networkD3/man/sankeyNetwork.html),
  mostly for formatting purposes.

## Value

a Sankey diagram

## Details

At present, this function uses
[`networkD3::sankeyNetwork()`](https://rdrr.io/pkg/networkD3/man/sankeyNetwork.html)
to draw the Sankey diagram.

If any of `R`, `U`, `V`, or `Y` is `NA`, `NA` is returned.

Note that there appears to be a colour bug in
[`networkD3::sankeyNetwork()`](https://rdrr.io/pkg/networkD3/man/sankeyNetwork.html)
when a node name ends in a ".". Colours for those nodes does not render
correctly.

## Examples

``` r
library(dplyr)
library(magrittr)
#> 
#> Attaching package: ‘magrittr’
#> The following object is masked from ‘package:tidyr’:
#> 
#>     extract
library(networkD3)
library(tidyr)
# Default colours are likely to appear nearly random.
UKEnergy2000mats |>
  tidyr::pivot_wider(names_from = "matrix.name",
                     values_from = "matrix") |>
  make_sankey() |>
  extract2("Sankey") |>
  extract2(1)

{"x":{"links":{"source":[0,1,1,2,2,3,3,4,4,4,4,4,4,4,5,6,6,6,6,6,6,6,7,8,8,9,9,3,10,10,1,4,6,9,8,2,3,10,5],"target":[2,1,3,1,2,4,3,1,4,9,8,2,10,11,6,1,9,8,2,3,5,12,9,5,12,9,8,10,10,11,13,13,13,13,13,13,13,13,13],"value":[50000,500,47000,47500,2500,15500,5000,25,350,50,25,50,250,14750,6400,25,25,25,25,75,100,6000,43000,16000,25000,2000,41000,26500,500,26000,550,350,125,2075,50,2575,5075,750,9700],"group":["Crude","Crude [from Dist.]","Crude [from Dist.]","Crude [from Fields]","Crude [from Fields]","Diesel","Diesel","Diesel [from Dist.]","Diesel [from Dist.]","Diesel [from Dist.]","Diesel [from Dist.]","Diesel [from Dist.]","Diesel [from Dist.]","Diesel [from Dist.]","Elect","Elect [from Grid]","Elect [from Grid]","Elect [from Grid]","Elect [from Grid]","Elect [from Grid]","Elect [from Grid]","Elect [from Grid]","NG","NG [from Dist.]","NG [from Dist.]","NG [from Wells]","NG [from Wells]","Petrol","Petrol [from Dist.]","Petrol [from Dist.]","Waste","Waste","Waste","Waste","Waste","Waste","Waste","Waste","Waste"]},"nodes":{"name":["Resources [of Crude]","Crude dist.","Oil fields","Oil refineries","Diesel dist.","Power plants","Elect. grid","Resources [of NG]","NG dist.","Gas wells & proc.","Petrol dist.","Transport","Residential","Waste"],"group":["Resources [of Crude]","Crude dist.","Oil fields","Oil refineries","Diesel dist.","Power plants","Elect. grid","Resources [of NG]","NG dist.","Gas wells & proc.","Petrol dist.","Transport","Residential","Waste"]},"options":{"NodeID":"Node","NodeGroup":"Node","LinkGroup":"Product","colourScale":"d3.scaleOrdinal(d3.schemeCategory20);","fontSize":7,"fontFamily":null,"nodeWidth":15,"nodePadding":10,"units":"","margin":{"top":null,"right":null,"bottom":null,"left":null},"iterations":32,"sinksRight":true}},"evals":[],"jsHooks":[]}# Create your own colour palette.
colour_df <- tibble::tribble(~name, ~colour,
                             "Resources [of Crude]", "gray",
                             "Crude dist.",          "gray",
                             "Oil fields",           "gray",
                             "Oil refineries",       "gray",
                             "Diesel dist.",         "gray",
                             "Power plants",         "gray",
                             "Elect. grid",          "gray",
                             "Resources [of NG]",    "gray",
                             "NG dist.",             "gray",
                             "Gas wells & proc.",    "gray",
                             "Petrol dist.",         "gray",
                             "Transport",            "gray",
                             "Residential",          "gray",
                             "Waste",                "gray",
                             "Crude",                "black",
                             "Crude [from Dist.]",   "black",
                             "Crude [from Fields]",  "black",
                             "Diesel",               "brown",
                             "Diesel [from Dist.]",  "brown",
                             "Diesel [from Fields]", "brown",
                             "Elect",                "yellow",
                             "Elect [from Grid]",    "yellow",
                             "NG",                   "lightblue",
                             "NG [from Dist.]",      "lightblue",
                             "NG [from Wells]",      "lightblue",
                             "Petrol",               "orange",
                             "Petrol [from Dist.]",  "orange")
colour_df
#> # A tibble: 27 × 2
#>    name                 colour
#>    <chr>                <chr> 
#>  1 Resources [of Crude] gray  
#>  2 Crude dist.          gray  
#>  3 Oil fields           gray  
#>  4 Oil refineries       gray  
#>  5 Diesel dist.         gray  
#>  6 Power plants         gray  
#>  7 Elect. grid          gray  
#>  8 Resources [of NG]    gray  
#>  9 NG dist.             gray  
#> 10 Gas wells & proc.    gray  
#> # ℹ 17 more rows
UKEnergy2000mats |>
  tidyr::pivot_wider(names_from = "matrix.name",
                     values_from = "matrix") |>
  make_sankey(colour_string = colour_df,
              # Arguments are passed to networkD3::sankeyNetwork()
              fontSize = 10,
              fontFamily = "Helvetica",
              units = "ktoe",
              width = 400, # pixels
              height = 400 # pixels
  ) |>
  extract2("Sankey") |>
  extract2(1)

{"x":{"links":{"source":[0,1,1,2,2,3,3,4,4,4,4,4,4,4,5,6,6,6,6,6,6,6,7,8,8,9,9,3,10,10,1,4,6,9,8,2,3,10,5],"target":[2,1,3,1,2,4,3,1,4,9,8,2,10,11,6,1,9,8,2,3,5,12,9,5,12,9,8,10,10,11,13,13,13,13,13,13,13,13,13],"value":[50000,500,47000,47500,2500,15500,5000,25,350,50,25,50,250,14750,6400,25,25,25,25,75,100,6000,43000,16000,25000,2000,41000,26500,500,26000,550,350,125,2075,50,2575,5075,750,9700],"group":["Crude","Crude [from Dist.]","Crude [from Dist.]","Crude [from Fields]","Crude [from Fields]","Diesel","Diesel","Diesel [from Dist.]","Diesel [from Dist.]","Diesel [from Dist.]","Diesel [from Dist.]","Diesel [from Dist.]","Diesel [from Dist.]","Diesel [from Dist.]","Elect","Elect [from Grid]","Elect [from Grid]","Elect [from Grid]","Elect [from Grid]","Elect [from Grid]","Elect [from Grid]","Elect [from Grid]","NG","NG [from Dist.]","NG [from Dist.]","NG [from Wells]","NG [from Wells]","Petrol","Petrol [from Dist.]","Petrol [from Dist.]","Waste","Waste","Waste","Waste","Waste","Waste","Waste","Waste","Waste"]},"nodes":{"name":["Resources [of Crude]","Crude dist.","Oil fields","Oil refineries","Diesel dist.","Power plants","Elect. grid","Resources [of NG]","NG dist.","Gas wells & proc.","Petrol dist.","Transport","Residential","Waste"],"group":["Resources [of Crude]","Crude dist.","Oil fields","Oil refineries","Diesel dist.","Power plants","Elect. grid","Resources [of NG]","NG dist.","Gas wells & proc.","Petrol dist.","Transport","Residential","Waste"]},"options":{"NodeID":"Node","NodeGroup":"Node","LinkGroup":"Product","colourScale":"d3.scaleOrdinal() .domain([\"Resources [of Crude]\", \"Crude dist.\", \"Oil fields\", \"Oil refineries\", \"Diesel dist.\", \"Power plants\", \"Elect. grid\", \"Resources [of NG]\", \"NG dist.\", \"Gas wells & proc.\", \"Petrol dist.\", \"Transport\", \"Residential\", \"Waste\", \"Crude\", \"Crude [from Dist.]\", \"Crude [from Fields]\", \"Diesel\", \"Diesel [from Dist.]\", \"Diesel [from Fields]\", \"Elect\", \"Elect [from Grid]\", \"NG\", \"NG [from Dist.]\", \"NG [from Wells]\", \"Petrol\", \"Petrol [from Dist.]\"]) .range([\"gray\", \"gray\", \"gray\", \"gray\", \"gray\", \"gray\", \"gray\", \"gray\", \"gray\", \"gray\", \"gray\", \"gray\", \"gray\", \"gray\", \"black\", \"black\", \"black\", \"brown\", \"brown\", \"brown\", \"yellow\", \"yellow\", \"lightblue\", \"lightblue\", \"lightblue\", \"orange\", \"orange\"])","fontSize":10,"fontFamily":"Helvetica","nodeWidth":15,"nodePadding":10,"units":"ktoe","margin":{"top":null,"right":null,"bottom":null,"left":null},"iterations":32,"sinksRight":true}},"evals":[],"jsHooks":[]}
```
