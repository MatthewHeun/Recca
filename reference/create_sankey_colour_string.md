# Create a Javascript colour string appropriate for networkD3 Sankey diagrams

This package (`Recca`) uses
[`networkD3::sankeyNetwork()`](https://rdrr.io/pkg/networkD3/man/sankeyNetwork.html)
to create Sankey diagrams in
[`make_sankey()`](https://matthewheun.github.io/Recca/reference/make_sankey.md).
This function converts a data frame of colors into a correctly formatted
javascript string for use with
[`networkD3::sankeyNetwork()`](https://rdrr.io/pkg/networkD3/man/sankeyNetwork.html).

## Usage

``` r
create_sankey_colour_string(
  colour_df = NULL,
  name_col = "name",
  colour_col = "colour"
)
```

## Arguments

- colour_df:

  A data frame containing `name_col` and `colour_col`.

- name_col, colour_col:

  Names of columns in `colour_df`.

## Value

A Javascript string formatted for use as the `colourScale` argument to
[`networkD3::sankeyNetwork()`](https://rdrr.io/pkg/networkD3/man/sankeyNetwork.html).

## Examples

``` r
# See https://r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html
# for original examples.
links <- data.frame(
  source = c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"),
  target = c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"),
  value = c(2,3, 2, 3, 1, 3)
)
# From these flows we need to create a node data frame:
# it lists every entity involved in the flow
nodes <- data.frame(
  name = c(as.character(links$source), as.character(links$target)) |>
    unique()
)

# With networkD3, connection must be provided using id,
# not using real name like in the links dataframe.
# So we need to reformat it.
links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1

# Add color to the flows.
links$group <- c("type_a","type_a","type_a","type_b","type_b","type_b")
# Add a 'group' column to each node. Put all nodes in the same group to make them grey.
nodes$group <- c("node_group")

colour_df <- tibble::tribble(~name, ~colour,
                             "type_a", "#69b3a2",
                             "type_b", "steelblue",
                             "node_group", "grey")
colour_string <- create_sankey_colour_string(colour_df)
cat(colour_string)
#> d3.scaleOrdinal() .domain(["type_a", "type_b", "node_group"]) .range(["#69b3a2", "steelblue", "grey"])
networkD3::sankeyNetwork(Links = links,
                         Nodes = nodes,
                         Source = "IDsource",
                         Target = "IDtarget",
                         Value = "value",
                         NodeID = "name",
                         colourScale = colour_string,
                         LinkGroup="group",
                         NodeGroup="group")

{"x":{"links":{"source":[0,0,1,2,2,3],"target":[2,4,3,5,6,7],"value":[2,3,2,3,1,3],"group":["type_a","type_a","type_a","type_b","type_b","type_b"]},"nodes":{"name":["group_A","group_B","group_C","group_E","group_D","group_F","group_G","group_H"],"group":["node_group","node_group","node_group","node_group","node_group","node_group","node_group","node_group"]},"options":{"NodeID":"name","NodeGroup":"group","LinkGroup":"group","colourScale":"d3.scaleOrdinal() .domain([\"type_a\", \"type_b\", \"node_group\"]) .range([\"#69b3a2\", \"steelblue\", \"grey\"])","fontSize":7,"fontFamily":null,"nodeWidth":15,"nodePadding":10,"units":"","margin":{"top":null,"right":null,"bottom":null,"left":null},"iterations":32,"sinksRight":true}},"evals":[],"jsHooks":[]}
```
