#
# This file contains helper functions that calculate aggregates of primary and final demand energy.
# Data frames with both SUT-style matrices and IEA-style tables are supported.
#

#' Primary aggregate energy
#'
#' Calculates aggregate primary energy from a data frame of Supply-Use matrices.
#'
#' @param .sutdata a data frame with columns of matrices from a supply-use analysis.
#' @param p_industries a vector of names of industries to be aggregated as "primary".
#'        These industries will appear in rows of the make matrix (\code{V}) and
#'        columns of the final demand matrix (\code{Y}).
#'        Entries in \code{Y_p} will be subtracted from entries in \code{V_p} to obtain
#'        the total primary energy aggregate.
#' @param V_colname the name of the column in \code{.sutdata} containing make (\code{V}) matrices.
#' @param Y_colname the name of the column in \code{.sutdata} containing final demand (\code{Y}) matrices.
#' @param by one of \code{Total}, \code{Product}, or \code{Flow} to indicate the desired aggregation:
#' \itemize{
#'   \item{\code{Total}: aggregation over both Product and Flow (the default)}
#'   \item{\code{Product}: aggregation by energy carrier (Crude oil, Primary solid biofuels, etc.)}
#'   \item{\code{Flow}: aggregation by type of flow (Production, Imports, Exports, etc.)}
#' }
#' @param aggregate_primary_colname the name of the output column containing aggregates of primary energy.
#'
#' @return a data frame containing columns of
#' the aggregate primary energy for each row of \code{.sutdata}.
#'
#' @export
primary_aggregates <- function(.sutdata,
                               # Vector of primary industries
                               p_industries,
                               # Input columns
                               V_colname = "V",
                               Y_colname = "Y",
                               by = c("Total", "Product", "Flow"),
                               # Output columns,
                               aggregate_primary_colname){

  by <- match.arg(by)
  aggfuncs <- list(total = "sumall_byname", product = "rowsums_byname", flow = "colsums_byname")
  agg_func <- match.fun(aggfuncs[[tolower(by)]])

  prim_func <- function(V, Y){
    VT_p <- transpose_byname(V) %>% select_cols_byname(retain_pattern = make_pattern(row_col_names = p_industries, pattern_type = "leading"))
    Y_p <- Y %>% select_cols_byname(retain_pattern = make_pattern(row_col_names = p_industries, pattern_type = "leading"))
    # VT_p - Y_p. This is TPES in product x industry matrix format
    VT_p_minus_Y_p <- difference_byname(VT_p, Y_p)
    agg_primary <- agg_func(VT_p_minus_Y_p)
    list(agg_primary) %>% set_names(aggregate_primary_colname)
  }
  matsindf_apply(.sutdata, FUN = prim_func, V = V_colname, Y = Y_colname)
}

#' Final demand aggregate energy
#'
#' Calculates aggregate final demand energy from a data frame of Supply-Use matrices.
#' The calculation includes non-energy uses if they are present in the final demand matrix.
#' The calculation does not include balancing items (Losses and Statistical differences).
#'
#' @param .sutdata a data frame with columns of matrices from a supply-use analysis.
#' @param fd_sectors a vector of names of sectors in final demand.
#' @param U_colname the name of the column in \code{.sutdata} containing Use (\code{U}) matrices.
#' @param V_colname the name of the column in \code{.sutdata} containing Make (\code{V}) matrices.
#' @param Y_colname the name of the column in \code{.sutdata} containing final demand (\code{Y}) matrices.
#' @param r_EIOU_colname the name of the colum that holds ratios of EIOU to total input for each Machine and Product.
#' @param by one of "Product", "Sector", or "Total" to indicate the desired aggregation:
#' "Product" for aggregation by energy carrier (Crude oil, Primary solid biofuels, etc.),
#' "Sector" for aggregation by final demand sector (Agriculture/forestry, Domestic navigation, etc.), or
#' "Total" for aggregation over both Product and Sector (the default).
#' @param net_aggregate_demand_colname the name of the output column containing aggregates of net energy demand.
#' Each entry in this column is \code{sumall(Y_fd)}.
#' @param gross_aggregate_demand_colname the name of the output column containing aggregates of gross energy demand.
#' Each entry in this column is calculated by \code{sumall(Y_fd)} + \code{sumall(U_EIOU)}.
#'
#' @return \code{.sutdata} with columns \code{net_aggregate_demand_colname} and
#' \code{gross_aggregate_demand_colname} added
#'
#' @export
finaldemand_aggregates <- function(.sutdata,
                                   fd_sectors,
                                   # Input columns
                                   U_colname = "U",
                                   Y_colname = "Y",
                                   r_EIOU_colname = "r_EIOU",
                                   by = c("Total", "Product", "Sector"),
                                   # Output columns
                                   net_aggregate_demand_colname,
                                   gross_aggregate_demand_colname){

  by <- match.arg(by)

  # Decide which aggregation function to use
  aggfuncs <- list(Total = "sumall_byname", Product = "rowsums_byname", Sector = "colsums_byname")
  agg_func <- match.fun(aggfuncs[[by]])

  fd_func <- function(U, Y, r_EIOU){
    U_EIOU <- elementproduct_byname(r_EIOU, U)
    net <- Y %>% select_cols_byname(retain_pattern = make_pattern(row_col_names = fd_sectors, pattern_type = "leading")) %>% agg_func()
    gross <- sum_byname(net, agg_func(U_EIOU))
    if (by == "Sector") {
      # If "Sector" aggregation is requested, the results will be row vectors.
      # Convert to column vectors.
      net <- transpose_byname(net)
      gross <- transpose_byname(gross)
    }
    list(net, gross) %>% set_names(net_aggregate_demand_colname, gross_aggregate_demand_colname)
  }
  matsindf_apply(.sutdata, FUN = fd_func, U = U_colname, Y = Y_colname, r_EIOU = r_EIOU_colname)
}

#' Final demand aggregate energy with units
#'
#' Calculates aggregate final demand energy and services from a data frame of Supply-Use matrices.
#' The calculation includes non-energy uses if they are present in the final demand matrix.
#' The calculation does not include balancing items (Losses and Statistical differences).
#'
#' @param .sutdata a data frame with columns of matrices from a supply-use analysis.
#' @param fd_sectors a vector of names of sectors in final demand.
#' @param U the name of the column in \code{.sutdata} containing Use (\code{U}) matrices.
#' @param Y the name of the column in \code{.sutdata} containing final demand (\code{Y}) matrices.
#' @param r_EIOU the name of the column that holds ratios of EIOU to total input for each Machine and Product.
#' @param S_units the name of the column in \code{.sutdata} containing \code{S_units} matrices.
#' @param by one of "Product", "Sector", or "Total" to indicate the desired aggregation:
#' "Product" for aggregation by energy carrier (Crude oil, Primary solid biofuels, etc.),
#' "Sector" for aggregation by final demand sector (Agriculture/forestry, Domestic navigation, etc.), or
#' "Total" for aggregation over both Product and Sector (the default).
#' @param net_aggregate_demand the name of the output column containing aggregates of net energy demand.
#' This column excludes energy industry own use.
#' @param gross_aggregate_demand_colname the name of the output column containing aggregates of gross energy demand.
#' This column includes energy industry own use.
#'
#' @return a data frame containing net aggregate energy demand
#' and gross aggregate energy demand for each row of \code{.sutdata}.
#'
#' @export
finaldemand_aggregates_with_units <- function(.sutdata,
                                              fd_sectors,
                                              # Input columns
                                              U = "U",
                                              Y = "Y",
                                              r_EIOU = "r_EIOU",
                                              S_units = "S_units",
                                              by = c("Total", "Product", "Sector"),
                                              # Output columns
                                              net_aggregate_demand_colname,
                                              gross_aggregate_demand_colname){

  by <- match.arg(by)

  fd_func <- function(U, Y, r_EIOU, S_units){
    U_EIOU <- elementproduct_byname(r_EIOU, U)
    if (by == "Product") {
      net <- rowsums_byname(Y)
      gross <- sum_byname(rowsums_byname(U_EIOU), net)
    } else {
      # by is "Total" or "Sector".
      U_EIOU_bar <- matrixproduct_byname(transpose_byname(S_units), U_EIOU)
      net <- matrixproduct_byname(
        transpose_byname(S_units),
        Y %>% select_cols_byname(retain_pattern = make_pattern(row_col_names = fd_sectors,
                                                               pattern_type = "leading")))
      gross <- sum_byname(U_EIOU_bar, net)
      net <- transpose_byname(net)
      gross <- transpose_byname(gross)
    }
    if (by == "Total") {
      net <- colsums_byname(net)
      gross <- colsums_byname(gross)
    }
    list(net, gross) %>% set_names(net_aggregate_demand_colname, gross_aggregate_demand_colname)
  }
  matsindf_apply(.sutdata, FUN = fd_func, U = U, Y = Y, r_EIOU = r_EIOU, S_units = S_units)
}
