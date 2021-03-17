#' Make a Sankey diagram
#'
#' A Sankey diagram is a flow diagram in which the width of the lines is proportional
#' to the rate of energy flow.
#' Sankey diagrams are a helpful way to visualize energy flows in an energy conversion chain (ECC).
#' This function takes a matrix description of an ECC and produces a Sankey diagram.
#'
#' At present, this function uses the `networkD3` package to draw the Sankey diagram.
#'
#' If any of `R`, `U`, `V`, or `Y` is `NA`, `NA` is returned.
#'
#' @param .sutmats an optional wide-by-matrices data frame
#' @param R,U,V,Y See `Recca::psut_cols`.
#' @param simplify_edges a boolean which tells whether edges should be simplified.
#'        Applies to every row of `.sutmats` if `.sutmats` is specified.
#' @param sankey See `Recca::sankey_cols`.
#' @param ... Arguments passed to `networkD3::sankeyNetwork()`, mostly for formatting purposes.
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
#' UKEnergy2000mats %>%
#'   spread(key = "matrix.name", value = "matrix") %>%
#'   make_sankey() %>%
#'   extract2("Sankey") %>%
#'   extract2(1)
make_sankey <- function(.sutmats = NULL,
                        R = Recca::psut_cols$R,
                        U = Recca::psut_cols$U,
                        V = Recca::psut_cols$V,
                        Y = Recca::psut_cols$Y,
                        simplify_edges = TRUE,
                        sankey = Recca::sankey_cols$sankey,
                        ...){
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
    el <- edge_list(R = R_mat, U = U_mat, V = V_mat, Y = Y_mat, simplify_edges = simplify_edges)[["Edge list"]]
    nl <- node_list(el)
    s <- networkD3::sankeyNetwork(Links = el, Nodes = nl,
                       Source = "From_node_id", Target = "To_node_id", Value = "Value",
                       NodeID = "Node",
                       ...)
    list(s) %>% magrittr::set_names(sankey)
  }
  matsindf::matsindf_apply(.sutmats, FUN = sankey_func, R_mat = R, U_mat = U, V_mat = V, Y_mat = Y)
}
