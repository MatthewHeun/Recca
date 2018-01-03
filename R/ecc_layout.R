#' Layout for graph representation of an energy conversion chain
#'
#' The layout flows left-to-right, with
#' primary industries and products on the left and
#' services and final demand industries on the right.
#' Left-to-right order is same as arguments
#' from <code>p_industries</code> to <code>fd_industries</code>.
#' <code>scbsd_industries</code> are arranged at the top,
#' in left-to-right order as they appear in the list argument.
#' Each of the <code>_industries</code> and <code>_products</code>
#' arguments may be a named list of character vectors, where
#' names provide groupings for the layout.
#'
#' @param .qgraph          a qgraph object
#' @param p_industries     production industries (named list of string vectors)
#' @param p_products       primary products (named list of string vectors)
#' @param pf_industries    primary-to-final industries (named list of string vectors)
#' @param f_products       final products (named list of string vectors)
#' @param fu_industries    final-to-useful industries (named list of string vectors)
#' @param u_products       useful products (named list of string vectors)
#' @param us_industries    useful-to-services industries (named list of string vectors)
#' @param s_products       services products (named list of string vectors)
#' @param fd_industries    final demand industries (named list of string vectors)
#' @param scbsd_industries stock change, bunker, and statistical differences
#'                         industries (named list of string vectors)
#'
#' @return
#' @export
#'
#' @examples
#' p_ind <- "p_ind_1"
#' p_prod <- "p_prod_1"
#' pf_ind <- "pf_ind_1"
#' f_prod <- "f_prod_1"
#' fd_ind <- "fd_ind_1"
#' names <- c("p_ind_1", "p_prod_1", "pf_ind_1", "f_prod_1", "fd_ind_1")
#' g <- qgraph(matrix(c(0, 10, 0, 0, 0,
#'                      0,  0, 9, 0, 0,
#'                      0,  0, 0, 8, 0,
#'                      0,  0, 0, 0, 7,
#'                      0,  0, 0, 0, 0),
#'               nrow = 5, byrow = TRUE,
#'               dimnames = list(names, names)),
#'             DoNotPlot = TRUE)
#' calc_ecc_layout(g,
#'                 p_industries = p_ind,
#'                 p_products = p_prod,
#'                 pf_industries = pf_ind,
#'                 f_products = f_prod,
#'                 fd_industries = fd_ind)
ecc_layout <- function(.qgraph,
                       p_industries,
                       p_products,
                       pf_industries,
                       f_products,
                       fu_industries,
                       u_products,
                       us_industries,
                       s_products,
                       fd_industries,
                       scbsd_industries){
  # Get the node names (which serve as IDs)
  node_names <- .qgraph$graphAttributes$Nodes$names %>% names
  data.frame(node_names = .qgraph$graphAttributes$Nodes$names %>% names) %>%
    mutate(
      # Set x values for everything except for fd_industries
      x = case_when(
        node_names %in% p_industries ~ 1,
        node_names %in% p_products ~ 2,
        node_names %in% pf_industries ~ 3,
        node_names %in% f_products ~ 4,
        # node_names %in% fu_industries ~ 5,
        # node_names %in% u_products ~ 6,
        # node_names %in% us_industreis ~ 7,
        # node_names %in% s_products ~ 8,
        TRUE ~ NA_real_
      )
    )
}


