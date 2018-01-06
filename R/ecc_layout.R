#' Layout for graph representation of an energy conversion chain
#'
#' Information for the layout is provided by the following arguments:
#' \code{industry_stages},
#' \code{product_stages},
#' \code{fd_stage}, and
#' \code{scbsd_stage},
#' all of which are data frames.
#'
#' @param industry_stages
#' @param product_stages
#' @param fd_stage
#' @param scbsd_stage
#' @param industry_stage_name the name of the column in \code{industry_stages} containing
#'        names of industries (a string).
#'        Default is "Industries".
#' @param industry_stage_number the name of the column in \code{industry_stages} containing
#'        the left-to-right position of each stage (a string).
#' @param industry_group the name of an optional column in \code{industry_stages} containing
#'        names of groups for industries (a string).
#'        The top-to-bottom order of groups at a given stage in the in the graph
#'        is determined by the top-to-bottom
#'        order in which group names appear in the \code{industry_stages} data frame.
#' @param product_stage_name the name of the column in \code{product_stages} containing
#'        names of products (a string).
#'        Default is "Products".
#' @param product_stage_number the name of the column in \code{product_stages} containing
#'        the left-to-right position of each stage (a string).
#' @param product_group
#'
#' @return
#' @export
#'
#' @examples
ecc_layout <- function(Industry_stages,
                       Product_stages,
                       FD_stage,
                       ScBSD_stage,
                       industry_stage_names_colname = "Industries",
                       product_stage_names_colname = "Products",
                       stage_colname = "Stage",
                       group_colname = "Group"){
  # Get the order of industries and products
  industries <- Industry_stages[[industry_stage_names_colname]] %>% unique
  products <- Product_stages[[product_stage_names_colname]] %>% unique

  Industry_stages <- Industry_stages %>%
    mutate(
      stage_number =
    )

}
