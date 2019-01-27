#' Make a Sankey diagram
#'
#' A Sankey diagram is a flow diagram in which the width of the lines is proportional
#' to the rate of energy flow.
#' Sankey diagrams are a helpful way to visualize energy flows in an energy conversion chain (ECC).
#' This function takes a matrix description of an ECC and produces a Sankey diagram.
#'
#' At present, this function uses the `networkD3` package to draw the Sankey diagram.
#'
#' If any of \code{R}, \code{U}, \code{V}, or \code{Y} is \code{NA}, \code{NA} is returned.
#'
#' @param .sutmats an optional data frame
#' @param R a resource matrix or the name of the column in \code{.sutmats} containing \code{R} matrices
#' @param U a use matrix or the name of the column in \code{.sutmats} containing \code{U} matrices
#' @param V a make matrix or the name of the column in \code{.sutmats} containing \code{V} matrices
#' @param Y a final demand matrix or the name of the column in \code{.sutmats} containing \code{Y} matrices
#' @param simplify_edges a boolean which tells whether edges should be simplified.
#'        Applies to every row of \code{.sutmats} if \code{.sutmats} is specified.
#' @param sankey the name of the output Sankey diagram or the name of the column in \code{.sutmats} containing Sankey diagrams
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
#'   rename(
#'     R_plus_V = V
#'   ) %>%
#'   separate_RV() %>%
#'   make_sankey() %>%
#'   extract2("Sankey") %>%
#'   extract2(1)
make_sankey <- function(.sutmats = NULL, R = "R", U = "U", V = "V", Y = "Y", simplify_edges = TRUE,
                        sankey = "Sankey"){
  sankey_func <- function(R_mat = NULL, U_mat, V_mat, Y_mat){
    # Check for NA values. If any NA values are found, need to return NA.
    # But we can't check R_mat for NA if it is NULL.
    if (is.null(R_mat)) {
      if (is.na(U_mat) || is.na(V_mat) || is.na(Y_mat)) {
        return(NA)
      }
    } else {
      if (is.na(R_mat) || is.na(U_mat) || is.na(V_mat) || is.na(Y_mat)) {
        return(NA)
      }
    }
    # When I convert everything to using R matrices, need to change this code.
    if (is.null(R_mat)) {
      # If R is missing, need to extract it from V
      res <- separate_RV(U = U_mat, R_plus_V = V_mat)
      R_mat <- res$R
      V_mat <- res$V
    }
    el <- edge_list(R = R_mat, U = U_mat, V = V_mat, Y = Y_mat, simplify_edges = simplify_edges)[["Edge list"]]
    nl <- node_list(el)
    s <- networkD3::sankeyNetwork(Links = el, Nodes = nl,
                       Source = "From_node_id", Target = "To_node_id", Value = "Value",
                       NodeID = "Node", units = "Quads",
                       # LinkGroup = "type",
                       # colourScale = mycolor, fontSize = 20, nodeWidth = 30,
                       iterations = 500, nodePadding = 14, fontSize = 20)
    list(s) %>% magrittr::set_names(sankey)
  }
  matsindf::matsindf_apply(.sutmats, FUN = sankey_func, R_mat = R, U_mat = U, V_mat = V, Y_mat = Y)
}
