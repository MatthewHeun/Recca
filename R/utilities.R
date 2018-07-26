#
# This file contains utility functions for Recca
#


#' Tell if any of a vector of strings starts with another string
#'
#' This function returns \code{TRUE} if any of the strings in \code{x}
#' starts with the string in \code{target} and \code{FALSE} otherwise.
#'
#' This function is vectorized. If \code{target} is a vector or list of strings,
#' the return value is the same length as \code{target} and
#' contains the result of applying the same test
#' (do any of \code{x} start with \code{target}?)
#' to each item in \code{target}.
#'
#' @param x a vector or list of strings
#' @param target a string (or a vector or list of strings)
#'
#' @return \code{TRUE} if any of \code{x} starts with \code{target}, \code{FALSE} otherwise.
#'         If \code{target} is a vector or list, the return value is the same length as \code{target}
#'         and contains the result of applying the test to each item in \code{target}.
#'
#' @importFrom Hmisc escapeRegex
#'
#' @export
#'
#' @examples
#' # TRUE, because one of the x string ("bd") starts with "b"
#' any_start_with(x = c("ad", "bd", "cd"), target = "b")
#' # TRUE, because two of the x strings starts with "Production"
#' any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Production")
#' # FALSE, because none of the x strings starts with "Offshore"
#' any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Offshore")
#' # TRUE FALSE, because the x strings start with "Production" but not "Offshore"
#' any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"),
#'                target = c("Production", "Offshore"))
any_start_with <- function(x, target){
  sapply(target, FUN = function(t){
    grepl(paste0("^", Hmisc::escapeRegex(t)), x) %>%
      any()
    }) %>%
    as.logical()
}


#' Tell if a string starts with any of a vector of strings
#'
#' This function returns \code{TRUE} if \code{x}
#' starts with any of the strings in \code{target} and \code{FALSE} otherwise.
#'
#' This function is vectorized. If \code{x} is a vector or list of strings,
#' the return value has the same length as \code{x} and contains the result
#' of applying the test (does \code{x} start with any of \code{target})
#' for each item in \code{x}.
#'
#' @param x a string (or vector or list of strings)
#' @param target a vector or list of strings
#'
#' @return \code{TRUE} if \code{x} starts with any of the strings in \code{target},
#'         \code{FALSE} otherwise.
#'         If \code{x} is a vector or list of strings, the return value is the same length as \code{x}
#'         and contains the result of applying the test to each item in \code{x}.
#'
#' @export
#'
#' @examples
#' starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "prefix"))
#' starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "c"))
#' starts_with_any_of(x = "prefix - suffix", target = "suffix")
#' starts_with_any_of(x = c("Production - Crude", "Production - NG",
#'                          "Exports - Oil", "Exports - Crude"),
#'                    target = c("Production", "Imports"))
starts_with_any_of <- function(x, target){
  sapply(x, FUN = function(one_x){
    grepl(paste(paste0("^", Hmisc::escapeRegex(target)), collapse = "|"), one_x)
  }) %>%
    set_names(NULL)
}


#' Primary industries
#'
#' Identify primary industries
#'
#' Primary industries are industries that make a product without using any products.
#' Primary industries are identified by interrogating
#' the use (\code{U}) and make (\code{V}) matrices.
#' Primary industries have all zeroes in their column of the use matrix (\code{U})
#' and at least one non-zero value in their row of the make (\code{V}) matrix.
#'
#' Argument and value descriptions are written assuming that \code{.sutdata} is a data frame.
#' Alternatively, \code{.sutdata} can be unspecified, and \code{U} and \code{V} can be matrices.
#' In that case, the return value is a list with a single item: \code{p_industries}
#' which contains a vector of names of primary industries for the \code{U} and \code{V} matrices.
#'
#' @param .sutdata a list or data frame containing use matrix(ces) and make matrix(ces)
#' @param U identifier for the use matrix (a string). Default is "U".
#' @param V identifier for the make matrix (a string). Default is "Y".
#' @param p_industries name for the primary industries vector (a string). Default is "\code{p_industries}".
#'
#' @return \code{.sutdata} with an additional column (named with the value of the \code{p_industries} argument)
#'         containing the primary industries for each row
#'
#' @importFrom matsbyname sort_rows_cols
#'
#' @export
#'
#' @examples
#' primary_industries(UKEnergy2000mats %>% spread(key = matrix.name, value = matrix))
primary_industries <- function(.sutdata = NULL, U = "U", V = "V", p_industries = "p_industries"){
  p_ind_func <- function(U, V){
    completed_cols_U <- complete_rows_cols(a = U, mat = transpose_byname(V), margin = 2) %>% sort_rows_cols()
    zero_cols_U_inds <- completed_cols_U %>%
      colsums_byname() %>%
      compare_byname("==", 0) %>%
      which()
    list(dimnames(completed_cols_U)[[2]][zero_cols_U_inds]) %>% set_names(p_industries)
  }
  matsindf_apply(.sutdata, FUN = p_ind_func, U = U, V = V)
}


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
#' @param U a use matrix of the name of a column in \code{.sutdata} containing use matrices. (Default is "\code{U}".)
#' @param V a make matrix of the name of a column in \code{.sutdata} containing make matrices. (Default is "\code{V}".)
#' @param Y a final demand matrix of the name of a column in \code{.sutdata} containing final demant matrices. (Default is "\code{Y}".)
#' @param edge_list the name of the column in the output data frame containing edge lists. (Default is "\code{Edge list}".)
#' @param from the name of the edge list column containing source nodes. (Default is "\code{From}".)
#' @param to the name of the edge list column containing destination nodes. (Default is "\code{To}".)
#' @param value the name of the edge list column containing magnitudes of the flows. (Default is "\code{Value}".)
#' @param product the name of the edge list column containing the product of the edge list flow. (Default is "\code{Product}".)
#' @param waste the name of the waste product and the destination node for wastes. (Default is "\code{Waste}".)
#' @param simplify_edges if \code{TRUE}, products with only one source will not have a node.
#' The source of the product will be connected directly to its consumers. If \code{FALSE}, no simplifications are made.
#' (Default is \code{TRUE}.)
#' @param include_waste if \code{TRUE}, edges for waste streams will be added to the edge list.
#' If \code{FALSE}, no waste edges will be added. (Default is \code{TRUE}.)
#'
#' @return an edge list or a column of edge lists
#'
#' @export
#'
#' @examples
#' sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
#' # Don't simplify edges and don't include waste edges
#' el_basic <- edge_list(sutmats, simplify_edges = FALSE, include_waste = FALSE)
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
edge_list <- function(.sutdata = NULL, U = "U", V = "V", Y = "Y",
                      edge_list = "Edge list",
                      from = "From", to = "To", value = "Value", product = "Product",
                      waste = "Waste", simplify_edges = TRUE, include_waste = TRUE){
  # Figure out which Products have only one source and one destination.
  # These are the flows that can be collapsed in the edge list.
  el_func <- function(Umat, Vmat, Ymat){
    # At this point, Umat, Vmat, and Ymat should be single matrices.
    expandedUY <- list(Umat, Ymat) %>%
      lapply(FUN = function(m){
        # Convert all to tidy (row, col, value) format
        mat_to_rowcolval(m, rownames = from, colnames = to, matvals = value, rowtype = "rowtype", coltype = "coltype", drop = 0)
      }) %>%
      bind_rows() %>%
      mutate(
        !!as.name(product) := !!as.name(from)
      )
    expandedV <- mat_to_rowcolval(Vmat, rownames = from, colnames = to, matvals = value, rowtype = "rowtype", coltype = "coltype", drop = 0) %>%
      as.data.frame() %>%
      mutate(
        !!as.name(product) := !!as.name(to)
      )
    el <- bind_rows(expandedUY, expandedV) %>%
      select(-rowtype, -coltype)
    if (include_waste) {
      el <- bind_rows(el, waste_edges(Umat = Umat, Vmat = Vmat,
                                      from = from, to = to,
                                      value = value, product = product,
                                      waste = waste))
    }
    if (simplify_edges) {
      el <- simplify_edge_list(el, from, to, value, product)
    }
    list(el) %>% set_names(edge_list)
  }
  matsindf_apply(.sutdata, FUN = el_func, Umat = U, Vmat = V, Ymat = Y)
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
#' el <- data.frame(From = c("A", "Oil"), To = c("Oil", "C"), Value = c(42, 42), Product = c("Oil", "Oil"), stringsAsFactors = FALSE)
#' # Oil flows from A to C through its product node (Oil)
#' el
#' # Simplify to have Oil flow from A to C without the product node
#' simplify_edge_list(el)
simplify_edge_list <- function(edge_list, from = "From", to = "To", value = "Value", product = "Product"){
  # First step is to split the edge_list into two data frames.
  # One contains the portion of the edge_list that comes from the use (U) matrix.
  # The other contains the portion of the edge_list that comes from the make (V) matris.
  # Get the entries that would have come from the U matrix.
  U_entries <- edge_list %>% filter(!!as.name(from) == !!as.name(product))
  # Get the entries that would have come from the V matrix.
  V_entries <- edge_list %>% filter(!!as.name(to) == !!as.name(product))
  # Figure out which products can be simplified
  # An edge for a product can be simplified if it has only one "From",
  # i.e., the product has only one source.
  # We find this information from the make (V) matrix entries
  products_to_simplify <- V_entries %>%
    group_by(!!as.name(product)) %>%
    summarise(num_V_entries = length(!!as.name(product))) %>%
    filter(num_V_entries == 1) %>%
    select(!!as.name(product)) %>%
    unlist() %>%
    set_names(NULL)
  # Now simplify the products in the U matrix entries by changing their from value.
  Simplified_U_entries <- lapply(products_to_simplify, FUN = function(p) {
    # Find the row in V_entries that pertain to product p
    V_entries_p <- V_entries %>% filter(!!as.name(product) == p)
    # Verify that there is only one row.
    stopifnot(nrow(V_entries_p) == 1)
    # Get the source of product p
    source <- V_entries_p[[from]][[1]]
    # Change the sources of all nodes that receive this product
    # to be the single source of the product instead of p itself.
    U_entries %>%
      filter(!!as.name(product) == p) %>%
      mutate(
        !!as.name(from) := case_when(
          !!as.name(product) == p ~ source,
          TRUE ~ !!as.name(product)
        )
      )
  }) %>%
    # rbind all of these together
    bind_rows() %>%
    # Now rbind with rows in U_entries that aren't simplified
    bind_rows(U_entries %>% filter(!(!!as.name(product) %in% products_to_simplify)))

  # Now remove all of the simplified products from the make (V) matrix rows.
  Reduced_V_entries <- V_entries %>% filter(!(!!as.name(product) %in% products_to_simplify))

  # Recombine U_entries and V_entries to make the full edge list and return it.
  bind_rows(Simplified_U_entries, Reduced_V_entries)
}

#' Create waste energy edges for an edge map
#'
#' Waste edges are created from the \code{W} matrix.
#'
#' The \code{waste} argument supplies both the name of the waste flow (default is "\code{Waste}")
#' and the name of the destination of the waste flows.
#'
#' @param Umat a use matrix.
#' @param Vmat a make matrix.
#' @param from the name of the edge list column containing source nodes. (Default is "\code{From}".)
#' @param to the name of the edge list column containing destination nodes. (Default is "\code{To}".)
#' @param value the name of the edge list column containing magnitudes of the flows. (Default is "\code{Value}".)
#' @param product the name of the edge list column containing the product of the edge list flow. (Default is "\code{Product}".)
#' @param waste the name of the waste product and the destination node for wastes. (Default is "\code{Waste}".)
#'
#' @export
#'
#' @return
#'
#' @examples
#' sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
#' edge_list(sutmats)$`Edge list`[[1]] %>% filter(Product == "Waste")
waste_edges <- function(Umat, Vmat,
                        from = "From", to = "To",
                        value = "Value", product = "Product",
                        waste = "Waste") {
  # Create edges for the waste sectors in a data frame.
  # Start by calculating the W matrix (V^T - U)
  difference_byname(transpose_byname(Vmat), Umat) %>%
    # The column sums of the W matrix contain positive and negative numbers.
    # We're interested in the negative numbers, because those are industries that are generating waste.
    # Positive numbers arise from industries that extract free gifts from nature.
    colsums_byname() %>%
    # industry names are column names of W. Put those as row names of the matrix by transposing.
    transpose_byname() %>%
    # As a data frame, we can filter and do other useful things.
    # Furthermore, the edge list is a data frame, anyway.
    as.data.frame() %>%
    # The only column in this data frame contains the values of the waste heat flows.
    # So call it by the desired value argument.
    set_names(value) %>%
    # The row names are the industry from which waste is generated.
    # Put that in the from column.
    rownames_to_column(from) %>%
    # Select only those industries that have waste.
    filter(!!as.name(value) < 0) %>%
    mutate(
      !!as.name(value) := abs(!!as.name(value)),
      !!as.name(to) := waste,
      !!as.name(product) := waste
    ) %>%
    select(from, to, value, product)
}

#'
#' #' Add UVY, Commodity, Industry, row, and col columns to a tidy data frame
#' #' containing IEA-formatted data.
#' #'
#' #' This function is specific to our data and nomenclature.
#' #' It does not account for sign changes needed when
#' #' switching from the IEA representation to the SUT representation.
#' #' This function merely tags each row of the IEA data to indicate the
#' #' submatix to which it belongs.
#' #' This function assumes that columns with names
#' #' \code{Ledger.side}, \code{Flow}, \code{Flow.aggregation.point},
#' #' \code{EX.ktoe}, and \code{Product} exist.
#' #'
#' #' @param .ieadata a tidy data frame in IEA format containing columns for
#' #' Ledger.side, Flow, and Flow.aggregation.point
#' #' @param Ledger.side the name of the column in \code{.ieadata} containing ledger side information
#' #' @param Flow of the column in \code{.ieadata} containing flow information
#' #' @param Flow.aggregation.point the name of the column in \code{.ieadata} containing flow aggregation point information
#' #' @param Flow the name of the column in \code{.ieadata} containing flow information
#' #' @param E the name of the column in in \code{.ieadata} containing energy information
#' #' @param UVY the name of the column to be added to \code{.ieadata} containing matrix identifiers
#' #' @param U the name of the use matrix (default is "U")
#' #' @param U_EIOU the name of the use matrix (default is "U_EIOU")
#' #' @param V the name of the make matrix (default is "V")
#' #' @param Y the name of the final demand matrix (default is "Y")
#' #'
#' #' @return a modified version of \code{.data} including a UVY column that indicates
#' #' the matrix in which the data point should be placed.
#' #'
#' #' @export
#' #'
#' #' @importFrom dplyr case_when
#' #' @importFrom dplyr mutate
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' library(dplyr)
#' #' library(magrittr)
#' #' addUVY(filter(AllIEAData, Country == "HN"))
#' addUVY <- function(.ieadata,
#'                    # Input columns
#'                    Ledger.side = "Ledger.side",
#'                    Flow.aggregation.point = "Flow.aggregation.point",
#'                    Flow = "Flow",
#'                    E = "E.ktoe",
#'                    # Output columns
#'                    UVY = "UVY",
#'                    U = "U", U_EIOU = "U_EIOU", V = "V", Y = "Y"){
#'   .ieadata %>%
#'     mutate(
#'       UVY = case_when(
#'         # Identify MATRICES for each piece of data in an IEA-table style tidy data frame.
#'         # Note that we proceed from most-specific to most-general,
#'         # because the case_when function applies the rule of the first match.
#'
#'         # Start with the Consumption side of the ledger.
#'         .$Ledger.side == "Consumption" ~                                             "Y",
#'
#'         # Work on the Supply side of the ledger.
#'
#'         # Items that belong in the final demand matrix.
#'
#'         # Exports originated in the IEA data.
#'         # They belong in the primary portion of the final demand matrix.
#'         .$Ledger.side == "Supply" & startsWith(.$Flow, "Exports") ~                    "Y",
#'
#'         # Bunkers and Stock changes both originated in the IEA data.
#'         # They have similar characteristics, namely that
#'         # when these numbers are positive, they belong in the primary portion of the supply (V) matrix, but
#'         # when they are negative, they belong in the primary portion of the final demand matrix (Y) with
#'         # sign changed.
#'         .$Ledger.side == "Supply" &
#'           (startsWith(.$Flow, "International aviation bunkers") |
#'              startsWith(.$Flow, "International marine bunkers") |
#'              startsWith(.$Flow, "Stock changes")) &
#'           .$E.ktoe >= 0 ~                                                           "V",
#'         .$Ledger.side == "Supply" &
#'           (startsWith(.$Flow, "International aviation bunkers") |
#'              startsWith(.$Flow, "International marine bunkers") |
#'              startsWith(.$Flow, "Stock changes")) &
#'           .$E.ktoe < 0 ~                                                            "Y",
#'         # Statistical differences originated in the IEA data, and
#'         # they belong in the balancing portion of the Make (V) or final demand (Y) matrix,
#'         # depending on the sign (+ or -).
#'         .$Ledger.side == "Supply" &
#'           startsWith(.$Flow, "Statistical differences") &
#'           .$E.ktoe >= 0 ~                                                           "V",
#'         .$Ledger.side == "Supply" &
#'           startsWith(.$Flow, "Statistical differences") &
#'           .$E.ktoe < 0 ~                                                            "Y",
#'         # Losses originated in the IEA data, and
#'         # they belong in the balancing submatrix of the final demand (Y) matrix.
#'         .$Ledger.side == "Supply" &
#'           .$Flow == "Losses" ~                                                       "Y",
#'
#'         # Items that belong in the Use (U) and Make (V) transaction matrices.
#'
#'         # Production and Imports originated in the IEA data, and
#'         # they belong in the primary portion of the Make matrix (V_p)
#'         # Note that if we re-classify additional "Production" to an industry
#'         # (as we have done for coal),
#'         # we should add other production machines here (in addition to "Coal mines").
#'         .$Ledger.side == "Supply" &
#'           (startsWith(.$Flow, "Production") |
#'              startsWith(.$Flow, "Imports")) ~                                           "V",
#'         .$Ledger.side == "Supply" &
#'           startsWith(.$Flow, "Coal mines") &
#'           .$Flow.aggregation.point != "Energy industry own use" ~                    "V",
#'         .$Ledger.side == "Supply" &
#'           startsWith(.$Flow, "Oil and gas extraction") &
#'           .$Flow.aggregation.point != "Energy industry own use" ~                    "V",
#'
#'         # Like Production, Transfers originated in the IEA data,
#'         # and they are part of primary-->final transformations.
#'         # Thus, Transfers belong in the _pf portion of the Use (U) and Make (V) matrices
#'         # according to their sign.
#'         .$Ledger.side == "Supply" & startsWith(.$Flow, "Transfers") & .$E.ktoe >= 0 ~         "V",
#'         .$Ledger.side == "Supply" & startsWith(.$Flow, "Transfers") & .$E.ktoe <  0 ~         "U",
#'
#'         # At this point, only Energy industry own use and Transformation processes
#'         # should remain on the Supply side of the ledger.
#'
#'         # Work on Energy industry own use.
#'
#'         # Energy industry own use originates from the IEA data,
#'         # where it typically indicates energy consumed by the energy industry
#'         # when converting primary energy to final energy by, for example,
#'         # Oil refineries.
#'         # Thus, we tag these entries with U_EIOU.
#'         .$Ledger.side == "Supply" &
#'           startsWith(.$Flow.aggregation.point, "Energy industry own use") &
#'           .$E.ktoe < 0 ~                                                            "U_EIOU",
#'
#'         # Work on Transformation processes.
#'
#'         # Transformation process entries with EX.ktoe >= 0 belong in the Make (V) matrix.
#'
#'         # If this entry is a Transformation process that makes energy (EX.ktoe >= 0) and
#'         # the product is a useful energy,
#'         # then this entry belongs in the final-->useful Make submatrix (V_fu).
#'         .$Ledger.side == "Supply" &
#'           .$Flow.aggregation.point == "Transformation processes" &
#'           .$E.ktoe >= 0 ~                                                                "V",
#'
#'         # If this entry is a Transformation process that uses energy (EX.ktoe < 0),
#'         # this entry belongs in the primary-->final Use submatrix (U_pf).
#'         .$Ledger.side == "Supply" &
#'           .$Flow.aggregation.point == "Transformation processes" &
#'           .$E.ktoe < 0 ~                                                                 "U",
#'
#'         # Anything that hasn't been specified above will get an "NA".
#'         # This is almost certainly an error.
#'         # So be sure to check for "NA" values in the UVY column.
#'         TRUE ~ "NA"
#'       )
#'     ) %>%
#'
#'     mutate(
#'       # Set TYPES of rows and columns for the matrices.
#'       # Products are the Commodities, and Flows are the Industries
#'       row = case_when(
#'         startsWith(.$UVY, "U") | startsWith(.$UVY, "Y") ~ .$Product,
#'         startsWith(.$UVY, "V")                          ~ .$Flow,
#'         TRUE ~ "NA"
#'       ),
#'       col = case_when(
#'         startsWith(.$UVY, "U") | startsWith(.$UVY, "Y") ~ .$Flow,
#'         startsWith(.$UVY, "V")                          ~ .$Product,
#'         TRUE ~ "NA"
#'       ),
#'       rowtype = case_when(
#'         startsWith(.$UVY, "U") | startsWith(.$UVY, "Y") ~ "Commodity",
#'         startsWith(.$UVY, "V")                          ~ "Industry",
#'         TRUE ~ "NA"
#'       ),
#'       coltype = case_when(
#'         startsWith(.$UVY, "U") | startsWith(.$UVY, "Y") ~ "Industry",
#'         startsWith(.$UVY, "V")                          ~ "Commodity",
#'         TRUE ~ "NA"
#'       )
#'     )
#' }
