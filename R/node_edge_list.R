#
# This file contains functions for constructing node and edge lists.
#

#' Create an edge list
#'
#' An edge list is a data frame in which each row describes a flow from one entity to another.
#' Columns in the edge list data frame are \code{From}, \code{To}, \code{Value}, and \code{Product}.
#' The edge list can be created from the \code{U}, \code{V}, and \code{Y} matrices of an energy conversion chain.
#' Edge lists is a typical data format for visulaization software.
#'
#' Optionally, waste streams can be calculated from the \code{U} and \code{V} matrices and
#' added to the edge list.
#' Optionally, edges can be simplified when a product has a single source.
#' In that event, the node named after the product is removed,
#' and destinations are connected to the sources.
#'
#' @param .sutdata Optionally, a data frame containing columns named with the values of the \code{U}, \code{V}, and \code{Y} arguments.
#' @param R a resource matrix or the name of a column in \code{.sutdata} containing resource matrices. (Default is "\code{R}".)
#' @param U a use matrix or the name of a column in \code{.sutdata} containing use matrices. (Default is "\code{U}".)
#' @param V a make matrix or the name of a column in \code{.sutdata} containing make matrices. (Default is "\code{V}".)
#' @param Y a final demand matrix or the name of a column in \code{.sutdata} containing final demant matrices. (Default is "\code{Y}".)
#' @param edge_list the name of the column in the output data frame containing edge lists.
#'                  Or the name of the item in the return list if .sutdata is not specified.
#'                  (Default is "\code{Edge list}".)
#' @param from the name of the edge list column containing source nodes. (Default is "\code{From}".)
#' @param to the name of the edge list column containing destination nodes. (Default is "\code{To}".)
#' @param value the name of the edge list column containing magnitudes of the flows. (Default is "\code{Value}".)
#' @param product the name of the edge list column containing the product of the edge list flow. (Default is "\code{Product}".)
#' @param waste the name of the waste product and the destination node for wastes.
#'              Set \code{NULL} to suppress addition of waste edges. (Default is "\code{Waste}".)
#' @param rowtypes the name of the rowtypes. (Default is "\code{rowtype}".)
#' @param coltypes the name of the rowtypes. (Default is "\code{coltype}".)
#' @param node_id the base name of node ID columns.
#'                Set \code{NULL} to suppress addition of node ID numbers.
#'                (Default is "\code{node_id}".)
#' @param first_node the first node number. (Default is \code{0}.)
#' @param edge_id the name of the edge ID column.
#'                Set \code{NULL} to suppress addition of edge ID numbers.
#'                (Default is "\code{edge_id}".)
#' @param simplify_edges if \code{TRUE}, products with only one source will not have a node.
#'                       The source of the product will be connected directly to its consumers.
#'                       If \code{FALSE}, no simplifications are made.
#'                       (Default is \code{TRUE}.)
#'
#' @return an edge list or a column of edge lists
#'
#' @export
#'
#' @examples
#' library(matsbyname)
#' library(tidyr)
#' sutmats <- UKEnergy2000mats %>%
#'   spread(key = matrix.name, value = matrix)
#' # Don't simplify edges and don't include waste edges
#' el_basic <- edge_list(sutmats, simplify_edges = FALSE)
#' head(el_basic$`Edge list`[[1]])
#' tail(el_basic$`Edge list`[[1]])
#' # Simplify edges and include waste
#' el <- edge_list(sutmats)
#' head(el$`Edge list`[[1]])
#' # Now includes waste edges
#' tail(el$`Edge list`[[1]])
#' # Works with single matrices, too.
#' elmats <- edge_list(U = sutmats$U[[1]], V = sutmats$V[[1]], Y = sutmats$Y[[1]])
#' head(elmats[["Edge list"]])
#' tail(elmats[["Edge list"]])
edge_list <- function(.sutdata = NULL, R = "R", U = "U", V = "V", Y = "Y",
                      edge_list = "Edge list",
                      from = "From", to = "To", value = "Value", product = "Product", waste = "Waste",
                      rowtypes = "rowtype", coltypes = "coltype",
                      node_id = "node_id", first_node = 0,
                      edge_id = "edge_id", simplify_edges = TRUE){
  el_func <- function(R_mat, U_mat, V_mat, Y_mat){
    # At this point, R_mat, U_mat, V_mat, and Y_mat should be single matrices.
    # But R_mat may be missing, in which case V_mat is actually R_mat + V_mat, and they need to be separated.
    if (missing(R_mat)) {
      res <- separate_RV(U = U_mat, R_plus_V = V_mat)
      R_mat <- res[["R"]]
      V_mat <- res[["V"]]
    }
    # U and Y have the same sense: Products-by-Industries, so expand them together.
    expandedUY <- list(U_mat, Y_mat) %>%
      lapply(FUN = function(m){
        # Convert all to tidy (row, col, value) format
        matsindf::mat_to_rowcolval(m, rownames = from, colnames = to, matvals = value, rowtypes = rowtypes, coltypes = coltypes, drop = 0)
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        !!as.name(product) := !!as.name(from)
      )
    # R and V have the same sense: Industries-by-Products, so expand them together.
    expandedRV <- list(R_mat, V_mat) %>%
      lapply(FUN = function(m){
        matsindf::mat_to_rowcolval(m, rownames = from, colnames = to, matvals = value, rowtypes = rowtypes, coltypes = coltypes, drop = 0)
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        !!as.name(product) := !!as.name(to)
      )
    el <- dplyr::bind_rows(expandedUY, expandedRV) %>%
      # dplyr::select(-rowtype, -coltype)
      dplyr::select(-!!rowtypes, -!!coltypes)
    if (!is.null(waste)) {
      el <- dplyr::bind_rows(el, waste_edges(U_mat = U_mat, V_mat = V_mat,
                                             from = from, to = to,
                                             value = value, product = product,
                                             waste = waste))
    }
    if (simplify_edges) {
      # Figure out which Products have only one source and one destination.
      # These are the flows that can be collapsed in the edge list.
      el <- simplify_edge_list(el, from, to, value, product)
    }
    if (!is.null(edge_id)) {
      el <- add_edge_ids(el)
    }
    if (!is.null(node_id)) {
      el <- add_node_ids(el, from = from, to = to, node_id = node_id, first_node = first_node)
    }
    list(el) %>% magrittr::set_names(edge_list)
  }
  matsindf::matsindf_apply(.sutdata, FUN = el_func, R_mat = R, U_mat = U, V_mat = V, Y_mat = Y)
}


#' Add node ID numbers to an edge list
#'
#' Edge lists can contain identification numbers (integers) for each node.
#' Because each row in the edge list data frame contains a "\code{From}" node
#' and a "\code{To}" node, two columns of node IDs are added, one for "\code{From}"
#' and one for "\code{To}".
#'
#' The column names for the "\code{From}" and "\code{To}" nodes are created by \code{paste}-ing
#' the value of the \code{from} and \code{to} arguments with the value of the \code{node_id} argument.
#'
#' @param edge_list the edge list to which node ID numbers are to be added
#' @param from the name of the column containing source nodes. (Default is "\code{From}".)
#' @param to the name of the column containing destination nodes. (Default is "\code{To}".)
#' @param node_id the root of the column name for node IDs. (Default is "\code{node_ID}".)  See details.
#' @param first_node the ID number of the first node. (Default is \code{0}.)
#'
#' @return \code{edge_list} with two additional columns containing \code{From} and \code{To} node ID numbers.
#'
#' @export
#'
#' @examples
#' library(matsbyname)
#' library(tidyr)
#' sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
#' # Suppress adding node IDs
#' elDF <- edge_list(sutmats, node_id = NULL)$`Edge list`[[1]]
#' add_node_ids(elDF)
add_node_ids <- function(edge_list, from = "From", to = "To", node_id = "node_id", first_node = 0){
  from_id <- paste0(from, "_", node_id)
  to_id <- paste0(to, "_", node_id)
  # Gather a list of all node names
  node_names <- ".node_names"
  # Make a 1-column data frame containing all of the node names.
  NodeNameID <- data.frame(unique(c(edge_list[[from]], edge_list[[to]])), stringsAsFactors = FALSE) %>%
    # Set the name of the only column.
    magrittr::set_names(node_names)
  n_node_names <- nrow(NodeNameID)
  NodeNameID <- NodeNameID %>%
    dplyr::mutate(
      !!as.name(node_id) := seq.int(first_node, first_node + n_node_names - 1)
    )

  # Add node IDs for the from nodes.
  edge_list <- dplyr::left_join(edge_list,
                                NodeNameID %>%
                                  dplyr::rename(
                                    !!as.name(from) := !!as.name(node_names),
                                    !!as.name(from_id) := !!as.name(node_id)
                                  ),
                                by = from)
  edge_list <- dplyr::left_join(edge_list,
                                NodeNameID %>%
                                  dplyr::rename(
                                    !!as.name(to) := !!as.name(node_names),
                                    !!as.name(to_id) := !!as.name(node_id)
                                  ),
                                by = to)
  return(edge_list)
}


#' Add edge ID numbers to an edge list
#'
#' The edges in an edge list can have ID numbers.
#' This functions adds a column of edge ID numbers.
#'
#' @param edge_list the edge list to which edge ID numbers are to be added
#' @param edge_id the name of the edge ID column in the outgoing edge list. (Default is "\code{edge_id}".)
#'
#' @return \code{edge_list} with an added column containing the edge ID numbers.
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' library(matsbyname)
#' sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
#' # Suppress adding edge IDs
#' elDF <- edge_list(sutmats, edge_id = NULL)$`Edge list`[[1]]
#' add_node_ids(elDF)
add_edge_ids <- function(edge_list, edge_id = "edge_id"){
  edge_list %>%
    dplyr::mutate(
      !!as.name(edge_id) := seq.int(nrow(edge_list))
    )
}


#' Simplify an edge list
#'
#' A PSUT energy conversion chain edge can be simplified if
#' a product has only one supplier (i.e., one "from").
#' Then, every "to" node for that product can have the product's "from" be its "from".
#' See examples.
#'
#' @param edge_list the edge list to be simplified
#' @param from the name of the from column in the edge list. (Default is "\code{From}".)
#' @param to the name of the to column in the edge list. (Default is "\code{To}".)
#' @param value the name of the value column in the edge list. (Default is "\code{Value}".)
#' @param product the name of the product column in the edge list. (Default is "\code{Product}".)
#'
#' @export
#'
#' @return a simplified edge list
#'
#' @examples
#' el <- data.frame(From = c("A", "Oil"), To = c("Oil", "C"),
#'                  Value = c(42, 42), Product = c("Oil", "Oil"),
#'                  stringsAsFactors = FALSE)
#' # Oil flows from A to C through its product node (Oil)
#' el
#' # Simplify to have Oil flow from A to C without the product node
#' simplify_edge_list(el)
simplify_edge_list <- function(edge_list, from = "From", to = "To", value = "Value", product = "Product"){
  # First step is to split the edge_list into two data frames.
  # One contains the portion of the edge_list that comes from the use (U) matrix.
  # The other contains the portion of the edge_list that comes from the make (V) matris.
  # Get the entries that would have come from the U matrix.
  U_entries <- edge_list %>% dplyr::filter(!!as.name(from) == !!as.name(product))
  # Get the entries that would have come from the V matrix.
  V_entries <- edge_list %>% dplyr::filter(!!as.name(to) == !!as.name(product))
  # Avoid a NOTE in R CMD check
  num_V_entries <- NULL
  # Figure out which products can be simplified
  # An edge for a product can be simplified if it has only one "From",
  # i.e., the product has only one source.
  # We find this information from the make (V) matrix entries
  products_to_simplify <- V_entries %>%
    dplyr::group_by(!!as.name(product)) %>%
    dplyr::summarise(num_V_entries = length(!!as.name(product))) %>%
    dplyr::filter(num_V_entries == 1) %>%
    dplyr::select(!!as.name(product)) %>%
    unlist() %>%
    magrittr::set_names(NULL)
  # Now simplify the products in the U matrix entries by changing their from value.
  Simplified_U_entries <- lapply(products_to_simplify, FUN = function(p) {
    # Find the row in V_entries that pertain to product p
    V_entries_p <- V_entries %>% dplyr::filter(!!as.name(product) == p)
    # Verify that there is only one row.
    stopifnot(nrow(V_entries_p) == 1)
    # Get the source of product p
    source <- V_entries_p[[from]][[1]]
    # Change the sources of all nodes that receive this product
    # to be the single source of the product instead of p itself.
    U_entries %>%
      dplyr::filter(!!as.name(product) == p) %>%
      dplyr::mutate(
        !!as.name(from) := dplyr::case_when(
          !!as.name(product) == p ~ source,
          TRUE ~ !!as.name(product)
        )
      )
  }) %>%
    # rbind all of these together
    dplyr::bind_rows() %>%
    # Now rbind with rows in U_entries that aren't simplified
    dplyr::bind_rows(U_entries %>% dplyr::filter(!(!!as.name(product) %in% products_to_simplify)))

  # Now remove all of the simplified products from the make (V) matrix rows.
  Reduced_V_entries <- V_entries %>% dplyr::filter(!(!!as.name(product) %in% products_to_simplify))

  # Recombine U_entries and V_entries to make the full edge list and return it.
  dplyr::bind_rows(Simplified_U_entries, Reduced_V_entries)
}

#' Create waste energy edges for an edge map
#'
#' Waste edges are created from the \code{W} matrix.
#'
#' The \code{waste} argument supplies both the name of the waste flow (default is "\code{Waste}")
#' and the name of the destination of the waste flows.
#'
#' @param U_mat a use matrix.
#' @param V_mat a make matrix.
#' @param from the name of the edge list column containing source nodes. (Default is "\code{From}".)
#' @param to the name of the edge list column containing destination nodes. (Default is "\code{To}".)
#' @param value the name of the edge list column containing magnitudes of the flows. (Default is "\code{Value}".)
#' @param product the name of the edge list column containing the product of the edge list flow. (Default is "\code{Product}".)
#' @param waste the name of the waste product and the destination node for wastes. (Default is "\code{Waste}".)
#'
#' @export
#'
#' @return waste energy edges computed from the \code{Umat} and \code{Vmat} matrices
#'
#' @examples
#' library(dplyr)
#' library(matsbyname)
#' library(tidyr)
#' sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
#' edge_list(sutmats)$`Edge list`[[1]] %>% filter(Product == "Waste")
waste_edges <- function(U_mat, V_mat,
                        from = "From", to = "To",
                        value = "Value", product = "Product",
                        waste = "Waste") {
  # Create edges for the waste sectors in a data frame.
  # Start by calculating the W matrix (V^T - U)
  matsbyname::difference_byname(transpose_byname(V_mat), U_mat) %>%
    # The column sums of the W matrix contain positive and negative numbers.
    # We're interested in the negative numbers, because those are industries that are generating waste.
    # Positive numbers arise from industries that extract free gifts from nature.
    matsbyname::colsums_byname() %>%
    # industry names are column names of W. Put those as row names of the matrix by transposing.
    matsbyname::transpose_byname() %>%
    # As a data frame, we can filter and do other useful things.
    # Furthermore, the edge list is a data frame, anyway.
    as.data.frame() %>%
    # The only column in this data frame contains the values of the waste heat flows.
    # So call it by the desired value argument.
    magrittr::set_names(value) %>%
    # The row names are the industry from which waste is generated.
    # Put that in the from column.
    tibble::rownames_to_column(from) %>%
    # Select only those industries that have waste.
    dplyr::filter(!!as.name(value) < 0) %>%
    dplyr::mutate(
      !!as.name(value) := abs(!!as.name(value)),
      !!as.name(to) := waste,
      !!as.name(product) := waste
    ) %>%
    dplyr::select(from, to, value, product)
}


#' Create a node list
#'
#' A node list is a data frame containing node names and associated node ID numbers (integers).
#' This function creates a node list from an edge list, as shown in the examples.
#'
#' See \code{\link{edge_list}} for a function to create edge lists.
#'
#' @param edge_list the name of the column in \code{.sutmats} containing edge lists
#'                  or a single edge list data frame.
#'                  (Default is "\code{Edge list}".)
#' @param from the name of the \code{edge_list} column containing names of source nodes. (Default is "\code{From}".)
#' @param to the name of the \code{edge_list} column containing names of destination nodes. (Default is "\code{To}".)
#' @param node the name of the output column containing node names. (Default is "\code{Node}".)
#' @param node_id the name of the output column containing node ID numbers. (Default is "\code{node_id}".)
#'
#' @return a node list
#'
#' @export
#'
#' @examples
#' library(matsbyname)
#' library(tidyr)
#' sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
#' el <- edge_list(sutmats)$`Edge list`[[1]]
#' node_list(el)
node_list <- function(edge_list, from = "From", to = "To", node = "Node", node_id = "node_id"){
  fromID <- paste0(from, "_", node_id)
  toID <- paste0(to, "_", node_id)
  fromIDs <- edge_list %>%
    dplyr::select(!!as.name(from), !!as.name(fromID)) %>%
    dplyr::rename(
      !!as.name(node) := !!as.name(from),
      !!as.name(node_id) := !!as.name(fromID)
    )
  toIDs <- edge_list %>%
    dplyr::select(!!as.name(to), !!as.name(toID)) %>%
    dplyr::rename(
      !!as.name(node) := !!as.name(to),
      !!as.name(node_id) := !!as.name(toID)
    )
  dplyr::bind_rows(fromIDs, toIDs) %>%
    unique()
}

# node_list <- function(.sutmats = NULL, edge_list = "Edge list",
#                       from = "From", to = "To", node = "Node", node_id = "node_id"){
#   nl_func <- function(el){
#     fromID <- paste0(from, "_", node_id)
#     toID <- paste0(to, "_", node_id)
#     fromIDs <- el %>%
#       dplyr::select(!!as.name(from), !!as.name(fromID)) %>%
#       dplyr::rename(
#         !!as.name(node) := !!as.name(from),
#         !!as.name(node_id) := !!as.name(fromID)
#       )
#     toIDs <- el %>%
#       dplyr::select(!!as.name(to), !!as.name(toID)) %>%
#       dplyr::rename(
#         !!as.name(node) := !!as.name(to),
#         !!as.name(node_id) := !!as.name(toID)
#       )
#     list(dplyr::bind_rows(fromIDs, toIDs) %>% unique()) %>% magrittr::set_names(el)
#   }
#   matsindf_apply(.sutmats, FUN = nl_func, el = edge_list)
# }

