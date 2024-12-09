#' Make a Sankey diagram
#'
#' A Sankey diagram is a flow diagram in which the width of the lines is proportional
#' to the rate of energy flow.
#' Sankey diagrams are a helpful way to visualize energy flows in an energy conversion chain (ECC).
#' This function takes a matrix description of an ECC and produces a Sankey diagram.
#'
#' At present, this function uses [networkD3::sankeyNetwork()] to draw the Sankey diagram.
#'
#' If any of `R`, `U`, `V`, or `Y` is `NA`, `NA` is returned.
#'
#' Note that there appears to be a colour bug
#' in [networkD3::sankeyNetwork()]
#' when a node name ends in a ".".
#' Colours for those nodes does not render correctly.
#'
#' @param .sutmats An optional wide-by-matrices data frame
#' @param R,U,V,Y See `Recca::psut_cols`.
#' @param simplify_edges A boolean which tells whether edges should be simplified.
#'                       Applies to every row of `.sutmats` if `.sutmats` is specified.
#' @param colour_string An optional Javascript string that defines colours
#'                      for the Sankey diagram,
#'                      appropriate for use by [networkD3::sankeyNetwork()].
#'                      Default is `NULL`, meaning that the default
#'                      color palette should be used, namely
#'                      `networkD3::JS("d3.scaleOrdinal(d3.schemeCategory20);")`.
#'                      Can be a data frame with `name_col` and `colour_col` columns
#'                      in which case [create_sankey_colour_string()]
#'                      is called internally
#'                      with `colour_string`, `name_col`, and `colour_col`
#'                      as the arguments.
#' @param name_col,colour_col The names of columns in `colour_df` for
#'                            names of nodes and flows (`name_col`) and
#'                            colours of nodes and flows (`colour_col`).
#'                            Defaults are "name" and "colour", respectively.
#' @param sankey See `Recca::sankey_cols`.
#' @param ... Arguments passed to [networkD3::sankeyNetwork()],
#'            mostly for formatting purposes.
#'
#' @return a Sankey diagram
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(magrittr)
#' library(networkD3)
#' library(tidyr)
#' # Default colours are likely to appear nearly random.
#' UKEnergy2000mats |>
#'   tidyr::pivot_wider(names_from = "matrix.name",
#'                      values_from = "matrix") |>
#'   make_sankey() |>
#'   extract2("Sankey") |>
#'   extract2(1)
#' # Create your own colour palette.
#' colour_df <- tibble::tribble(~name, ~colour,
#'                              "Resources [of Crude]", "gray",
#'                              "Crude dist.",          "gray",
#'                              "Oil fields",           "gray",
#'                              "Oil refineries",       "gray",
#'                              "Diesel dist.",         "gray",
#'                              "Power plants",         "gray",
#'                              "Elect. grid",          "gray",
#'                              "Resources [of NG]",    "gray",
#'                              "NG dist.",             "gray",
#'                              "Gas wells & proc.",    "gray",
#'                              "Petrol dist.",         "gray",
#'                              "Transport",            "gray",
#'                              "Residential",          "gray",
#'                              "Waste",                "gray",
#'                              "Crude",                "black",
#'                              "Crude [from Dist.]",   "black",
#'                              "Crude [from Fields]",  "black",
#'                              "Diesel",               "brown",
#'                              "Diesel [from Dist.]",  "brown",
#'                              "Diesel [from Fields]", "brown",
#'                              "Elect",                "yellow",
#'                              "Elect [from Grid]",    "yellow",
#'                              "NG",                   "lightblue",
#'                              "NG [from Dist.]",      "lightblue",
#'                              "NG [from Wells]",      "lightblue",
#'                              "Petrol",               "orange",
#'                              "Petrol [from Dist.]",  "orange")
#' colour_df
#' UKEnergy2000mats |>
#'   tidyr::pivot_wider(names_from = "matrix.name",
#'                      values_from = "matrix") |>
#'   make_sankey(colour_string = colour_df,
#'               # Arguments are passed to networkD3::sankeyNetwork()
#'               fontSize = 10,
#'               fontFamily = "Helvetica",
#'               units = "ktoe",
#'               width = 400, # pixels
#'               height = 400 # pixels
#'   ) |>
#'   extract2("Sankey") |>
#'   extract2(1)
make_sankey <- function(.sutmats = NULL,
                        R = Recca::psut_cols$R,
                        U = Recca::psut_cols$U,
                        V = Recca::psut_cols$V,
                        Y = Recca::psut_cols$Y,
                        simplify_edges = TRUE,
                        colour_string = NULL,
                        name_col = "name",
                        colour_col = "colour",
                        sankey = Recca::sankey_cols$sankey,
                        ...) {
  if (is.null(colour_string)) {
    colour_string <- networkD3::JS("d3.scaleOrdinal(d3.schemeCategory20);")
    link_group <- NULL
  } else if (is.data.frame(colour_string)) {
    colour_string <- colour_string |>
      create_sankey_colour_string(name_col = name_col,
                                  colour_col = colour_col)
  }
  sankey_func <- function(R_mat = NULL, U_mat, V_mat, Y_mat){
    # Check for NA values. If any NA values are found, need to return NA.
    # But we can't check R_mat for NA if it is NULL.
    if (is.null(R_mat)) {
      if (all(is.na(U_mat)) || all(is.na(V_mat)) || all(is.na(Y_mat))) {
        return(NA)
      }
    } else {
      if (all(is.na(R_mat)) || all(is.na(U_mat)) || all(is.na(V_mat)) || all(is.na(Y_mat))) {
        return(NA)
      }
    }
    # When I convert everything to using R matrices, need to change this code.
    if (is.null(R_mat)) {
      # If R is missing, need to extract it from R_plus_V
      res <- separate_RV(U = U_mat, R_plus_V = V_mat)
      R_mat <- res$R
      V_mat <- res$V
    }
    el <- edge_list(R = R_mat, U = U_mat, V = V_mat, Y = Y_mat,
                    simplify_edges = simplify_edges)[["Edge list"]]
    nl <- node_list(el)
    # The strings for Source, Target, Value, NodeID, LinkGroup, and NodeGroup
    # are defaults arguments to edge_list() and node_list().
    s <- networkD3::sankeyNetwork(Links = el,
                                  Nodes = nl,
                                  Source = "From_node_id",
                                  Target = "To_node_id",
                                  Value = "Value",
                                  NodeID = "Node",
                                  colourScale = colour_string,
                                  LinkGroup = "Product",
                                  NodeGroup = "Node",
                                  ...)
    list(s) |>
      magrittr::set_names(sankey)
  }
  matsindf::matsindf_apply(.sutmats, FUN = sankey_func, R_mat = R, U_mat = U, V_mat = V, Y_mat = Y)
}


#' Create a Javascript colour string appropriate for networkD3 Sankey diagrams
#'
#' This package (`Recca`) uses [networkD3::sankeyNetwork()] to create
#' Sankey diagrams in [make_sankey()].
#' This function converts a data frame of colors
#' into a correctly formatted javascript string
#' for use with [networkD3::sankeyNetwork()].
#'
#' @param colour_df A data frame containing `name_col` and `colour_col`.
#' @param name_col,colour_col Names of columns in `colour_df`.
#'
#' @return A Javascript string formatted for use as the `colourScale`
#'         argument to [networkD3::sankeyNetwork()].
#'
#' @export
#'
#' @examples
#' # See https://r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html
#' # for original examples.
#' links <- data.frame(
#'   source = c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"),
#'   target = c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"),
#'   value = c(2,3, 2, 3, 1, 3)
#' )
#' # From these flows we need to create a node data frame:
#' # it lists every entity involved in the flow
#' nodes <- data.frame(
#'   name = c(as.character(links$source), as.character(links$target)) |>
#'     unique()
#' )
#'
#' # With networkD3, connection must be provided using id,
#' # not using real name like in the links dataframe.
#' # So we need to reformat it.
#' links$IDsource <- match(links$source, nodes$name) - 1
#' links$IDtarget <- match(links$target, nodes$name) - 1
#'
#' # Add color to the flows.
#' links$group <- c("type_a","type_a","type_a","type_b","type_b","type_b")
#' # Add a 'group' column to each node. Put all nodes in the same group to make them grey.
#' nodes$group <- c("node_group")
#'
#' colour_df <- tibble::tribble(~name, ~colour,
#'                              "type_a", "#69b3a2",
#'                              "type_b", "steelblue",
#'                              "node_group", "grey")
#' colour_string <- create_sankey_colour_string(colour_df)
#' cat(colour_string)
#' networkD3::sankeyNetwork(Links = links,
#'                          Nodes = nodes,
#'                          Source = "IDsource",
#'                          Target = "IDtarget",
#'                          Value = "value",
#'                          NodeID = "name",
#'                          colourScale = colour_string,
#'                          LinkGroup="group",
#'                          NodeGroup="group")
create_sankey_colour_string <- function(colour_df = NULL,
                                 name_col = "name",
                                 colour_col = "colour") {
  if (is.null(colour_df)) {
    return(NULL)
  }
  name_string <- paste0(paste0('"', colour_df[[name_col]], '"'), collapse = ", ")
  colour_string <- paste0(paste0('"', colour_df[[colour_col]], '"'), collapse = ", ")
  paste0('d3.scaleOrdinal() .domain([', name_string, ']) .range([', colour_string, '])')
}
