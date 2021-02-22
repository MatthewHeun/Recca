#
# This file contains helper functions that calculate aggregates of primary and final demand energy.
# Data frames with both SUT-style matrices and IEA-style tables are supported.
#

#' Primary aggregate energy
#'
#' Calculates aggregate primary energy from a data frame of Supply-Use matrices.
#'
#' @param .sutdata a data frame with columns of matrices from a supply-use analysis.
#' @param p_industries a vector of names of industries to be aggregated as "primary."
#'        If `.sutdata` is a data frame, `p_industries` should be the name of a column in the data frame.
#'        If `.sutdata` is `NULL`, `p_industries` can be a single vector of industry names.
#'        These industries in `p_industries` will appear in rows of the resource (`R`) and make (`V`) matrices and
#'        columns of the final demand matrix (`Y`).
#'        Entries in `Y_p` will be subtracted from entries in `R_p + V_p` to obtain
#'        the total primary energy aggregate,
#'        where `*_p` is the primary part of those matrices.
#'        The function `find_p_industry_names()` might be helpful to find
#'        primary industry names if they can be identified by prefixes.
#' @param R,V,Y See `Recca::psut_cols`.
#' @param by One of "Total", "Product", or "Flow" to indicate the desired aggregation:
#'        \itemize{
#'          \item "Total": aggregation over both Product and Flow (the default)
#'          \item "Product": aggregation by energy carrier (Crude oil, Primary solid biofuels, etc.)
#'          \item "Flow": aggregation by type of flow (Production, Imports, Exports, etc.)
#'        }
#' @param aggregate_primary The name for aggregates of primary energy on output.
#'
#' @return A list or data frame containing aggregate primary energy.
#'
#' @export
#'
#' @examples
#' library(matsbyname)
#' p_industries <- c("Resources - Crude", "Resources - NG")
#' # Calculate primary total aggregates
#' res <- UKEnergy2000mats %>%
#'   tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
#'   dplyr::mutate(
#'     p_industries = rep(list(p_industries), times = nrow(.))
#'   ) %>%
#'   primary_aggregates(p_industries = "p_industries", by = "Total")
#' res[[Recca::aggregate_cols$aggregate_primary]]
primary_aggregates <- function(.sutdata,
                               # Vector of primary industries
                               p_industries,
                               # Input names
                               R = Recca::psut_cols$R,
                               V = Recca::psut_cols$V,
                               Y = Recca::psut_cols$Y,
                               by = c("Total", "Product", "Flow"),
                               # Output names
                               aggregate_primary = Recca::aggregate_cols$aggregate_primary){

  # Ensure that the caller has matsbyname installed and on the package search path.
  assertthat::assert_that(requireNamespace("matsbyname"),
                          msg = "package 'matsbyname' is required but not available.")

  by <- match.arg(by)
  # Figure out which function we need to use.
  aggfuncs <- list(Total = "sumall_byname", Product = "rowsums_byname", Flow = "colsums_byname")
  # agg_func <- match.fun(aggfuncs[[tolower(by)]])
  agg_func <- get(aggfuncs[[by]], envir = as.environment("package:matsbyname"))


  prim_func <- function(p_industries_vec, R_mat = NULL, V_mat, Y_mat){
    # Look for primary industries in each of R, V, and Y matrices
    RT_p <- matsbyname::transpose_byname(R_mat) %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       matsbyname::make_pattern(row_col_names = p_industries_vec, pattern_type = "leading"))
    VT_p <- matsbyname::transpose_byname(V_mat) %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       matsbyname::make_pattern(row_col_names = p_industries_vec, pattern_type = "leading"))
    # Get the primary industries from the Y matrix.
    Y_p <- Y_mat %>% matsbyname::select_cols_byname(retain_pattern =
                                                      matsbyname::make_pattern(row_col_names = p_industries_vec, pattern_type = "leading"))
    # TPES in product x industry matrix format is RT_p + VT_p - Y_p.
    RVT_p_minus_Y_p <- matsbyname::sum_byname(RT_p, VT_p) %>% matsbyname::difference_byname(Y_p)
    agg_primary <- agg_func(RVT_p_minus_Y_p)
    list(agg_primary) %>% magrittr::set_names(aggregate_primary)
  }
  matsindf::matsindf_apply(.sutdata, FUN = prim_func, p_industries_vec = p_industries, R_mat = R, V_mat = V, Y_mat = Y)
}


#' Final demand aggregate energy
#'
#' Calculates aggregate final demand energy from a data frame of Supply-Use matrices.
#' The calculation includes non-energy uses if they are present in the final demand matrix.
#' The calculation does not include balancing items (Losses and Statistical differences).
#' If `.sutdata` is a data frame, `fd_sectors` should be the name of a column in the data frame.
#' If `.sutdata` is `NULL`, `fd_sectors` can be a single vector of industry names.
#'
#' @param .sutdata A data frame with columns of matrices from a supply-use analysis.
#' @param fd_sectors A vector of names of sectors in final demand.
#'                   Names should include columns in the `Y` and `U_EIOU` matrices
#'                   to cover both net (in `Y`) and gross (in `Y` and `U_EIOU`) final demand.
#' @param U,Y,r_EIOU See `Recca::psut_cols`.
#' @param by One of "Product", "Sector", or "Total" to indicate the desired aggregation:
#'           "Product" for aggregation by energy carrier (Crude oil, Primary solid biofuels, etc.),
#'           "Sector" for aggregation by final demand sector (Agriculture/forestry, Domestic navigation, etc.), or
#'           "Total" for aggregation over both Product and Sector (the default).
#' @param net_aggregate_demand,gross_aggregate_demand See `Recca::aggregate_cols`.
#'        Net energy demand is calculated by `sumall(Y_fd)`.
#'        Gross energy demand is calculated by `sumall(Y_fd) + sumall(U_EIOU)`.
#'
#' @return A list or data frame containing `net_aggregate_demand` and `gross_aggregate_demand` columns.
#'
#' @export
#'
#' @examples
#' library(matsbyname)
#' UKEnergy2000mats %>%
#'   tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
#'   dplyr::mutate(
#'     fd_sectors = rep(list(c("Residential", "Transport")), times = nrow(.))
#'   ) %>%
#'   dplyr::filter(Last.stage %in% c(IEATools::last_stages$final,
#'                                   IEATools::last_stages$useful)) %>%
#'   finaldemand_aggregates(fd_sectors = "fd_sectors", by = "Sector")
finaldemand_aggregates <- function(.sutdata,
                                   fd_sectors,
                                   # Input names
                                   U = Recca::psut_cols$U,
                                   Y = Recca::psut_cols$Y,
                                   r_EIOU = Recca::psut_cols$r_eiou,
                                   by = c("Total", "Product", "Sector"),
                                   # Output names
                                   net_aggregate_demand = Recca::aggregate_cols$net_aggregate_demand,
                                   gross_aggregate_demand = Recca::aggregate_cols$gross_aggregate_demand){

  # Ensure that the caller has matsbyname installed and on the package search path.
  assertthat::assert_that(requireNamespace("matsbyname"),
                          msg = "package 'matsbyname' is required but not available.")

  by <- match.arg(by)

  # Decide which aggregation function to use
  aggfuncs <- list(Total = "sumall_byname", Product = "rowsums_byname", Sector = "colsums_byname")
  agg_func <- get(aggfuncs[[by]], envir = as.environment("package:matsbyname"))

  fd_func <- function(fd_sectors_vec, U_mat, Y_mat, r_EIOU_mat){
    U_EIOU <- matsbyname::hadamardproduct_byname(r_EIOU_mat, U_mat)
    net <- Y_mat %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       matsbyname::make_pattern(row_col_names = fd_sectors_vec, pattern_type = "leading")) %>%
      agg_func()
    gross <- U_EIOU %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       matsbyname::make_pattern(row_col_names = fd_sectors_vec, pattern_type = "leading")) %>%
      agg_func() %>%
      matsbyname::sum_byname(net)
    if (by == "Sector") {
      # If "Sector" aggregation is requested, the results will be row vectors.
      # Convert to column vectors.
      net <- matsbyname::transpose_byname(net)
      gross <- matsbyname::transpose_byname(gross)
    }
    list(net, gross) %>% magrittr::set_names(c(net_aggregate_demand, gross_aggregate_demand))
  }
  matsindf::matsindf_apply(.sutdata, FUN = fd_func, fd_sectors_vec = fd_sectors, U_mat = U, Y_mat = Y, r_EIOU_mat = r_EIOU)
}


#' Final demand aggregate energy with units
#'
#' Calculates aggregate final demand energy and services from a data frame of Supply-Use matrices.
#' The calculation includes non-energy uses if they are present in the final demand matrix.
#' The calculation does not include balancing items (Losses and Statistical differences).
#' If `.sutdata` is a data frame, `fd_sectors` should be the name of a column in the data frame.
#' If `.sutdata` is `NULL`, `fd_sectors` can be a single vector of industry names.
#'
#' @param .sutdata A data frame with columns of matrices from a supply-use analysis.
#' @param fd_sectors A vector of names of sectors in final demand.
#'                   Names should include columns in the `Y` and `U_EIOU` matrices
#'                   to cover both net (in `Y`) and gross (in `Y` and `U_EIOU`) final demand.
#' @param U Use (\code{U}) matrix or name of the column in \code{.sutdata} containing same
#' @param Y Final demand (\code{Y}) matrix or name of the column in \code{.sutdata} containing same
#' @param r_EIOU Matrix of ratios of EIOU for the make (\code{U}) matrix or name of the column in \code{.sutdata} containing same
#' @param S_units The name of the column in \code{.sutdata} containing \code{S_units} matrices.
#' @param by One of "Product", "Sector", or "Total" to indicate the desired aggregation:
#'           "Product" for aggregation by energy carrier (Crude oil, Primary solid biofuels, etc.),
#'           "Sector" for aggregation by final demand sector (Agriculture/forestry, Domestic navigation, etc.), or
#'           "Total" for aggregation over both Product and Sector (the default).
#' @param net_aggregate_demand The name of net energy demand (which excludes energy industry own use) on output.
#' @param gross_aggregate_demand The name of gross energy demand (which includes energy industry own use) on output.
#'
#' @return A list or data frame containing net aggregate energy demand
#'         and gross aggregate energy demand
#'
#' @export
finaldemand_aggregates_with_units <- function(.sutdata,
                                              fd_sectors,
                                              # Input names
                                              U = Recca::psut_cols$U,
                                              Y = Recca::psut_cols$Y,
                                              r_EIOU = Recca::psut_cols$r_eiou,
                                              S_units = Recca::psut_cols$s_units,
                                              by = c("Total", "Product", "Sector"),
                                              # Output names
                                              net_aggregate_demand = Recca::aggregate_cols$net_aggregate_demand,
                                              gross_aggregate_demand = Recca::aggregate_cols$gross_aggregate_demand){

  by <- match.arg(by)

  fd_func <- function(fd_sectors_vec, U_mat, Y_mat, r_EIOU_mat, S_units_mat){
    U_EIOU <- matsbyname::hadamardproduct_byname(r_EIOU_mat, U_mat)
    # Filter columns of interest
    U_EIOU <- U_EIOU %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       matsbyname::make_pattern(row_col_names = fd_sectors_vec,
                                                                pattern_type = "leading"))
    Y_mat <- Y_mat %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       matsbyname::make_pattern(row_col_names = fd_sectors_vec,
                                                                pattern_type = "leading"))
    if (by == "Product") {
      net <- matsbyname::rowsums_byname(Y_mat)
      gross <- matsbyname::sum_byname(matsbyname::rowsums_byname(U_EIOU), net)
    } else {
      # by is "Total" or "Sector".
      U_EIOU_bar <- matsbyname::matrixproduct_byname(matsbyname::transpose_byname(S_units_mat), U_EIOU)
      net <- matsbyname::matrixproduct_byname(matsbyname::transpose_byname(S_units_mat), Y_mat)
      gross <- matsbyname::sum_byname(U_EIOU_bar, net)
      net <- matsbyname::transpose_byname(net)
      gross <- matsbyname::transpose_byname(gross)
    }
    if (by == "Total") {
      net <- matsbyname::colsums_byname(net)
      gross <- matsbyname::colsums_byname(gross)
    }
    list(net, gross) %>% magrittr::set_names(c(net_aggregate_demand, gross_aggregate_demand))
  }
  matsindf::matsindf_apply(.sutdata, FUN = fd_func, fd_sectors_vec = fd_sectors, U_mat = U, Y_mat = Y, r_EIOU_mat = r_EIOU, S_units_mat = S_units)
}

