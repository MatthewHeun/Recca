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
#'        These industries will appear in rows of the resource (`R`) and make (`V`) matrices and
#'        columns of the final demand matrix (`Y`).
#'        Entries in `Y_p` will be subtracted from entries in `R_p + V_p` to obtain
#'        the total primary energy aggregate.
#' @param R,V,Y See `Recca::psut_cols`.
#' @param V make (`V`) matrix or the name of the column in `.sutdata` containing same
#' @param Y final demand (`Y`) matrix or the name of the column in `.sutdata` containing same
#' @param by one of `Total`, `Product`, or `Flow` to indicate the desired aggregation:
#' \itemize{
#'   \item `Total`: aggregation over both Product and Flow (the default)
#'   \item `Product`: aggregation by energy carrier (Crude oil, Primary solid biofuels, etc.)
#'   \item `Flow`: aggregation by type of flow (Production, Imports, Exports, etc.)
#' }
#' @param aggregate_primary the name for aggregates of primary energy on output
#'
#' @return a list or data frame containing aggregate primary energy
#'
#' @export
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

  by <- match.arg(by)
  # Figure out which function we need to use.
  aggfuncs <- list(total = "sumall_byname", product = "rowsums_byname", flow = "colsums_byname")
  agg_func <- match.fun(aggfuncs[[tolower(by)]])

  prim_func <- function(R_mat = NULL, V_mat, Y_mat){
    # Look for primary industries in each of R, V, and Y matrices
    RT_p <- matsbyname::transpose_byname(R_mat) %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       matsbyname::make_pattern(row_col_names = p_industries, pattern_type = "leading"))
    VT_p <- matsbyname::transpose_byname(V_mat) %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       matsbyname::make_pattern(row_col_names = p_industries, pattern_type = "leading"))
    # Get the primary industries from the Y matrix.
    Y_p <- Y_mat %>% matsbyname::select_cols_byname(retain_pattern =
                                                      matsbyname::make_pattern(row_col_names = p_industries, pattern_type = "leading"))
    # TPES in product x industry matrix format is RT_p + VT_p - Y_p.
    RVT_p_minus_Y_p <- matsbyname::sum_byname(RT_p, VT_p) %>% matsbyname::difference_byname(Y_p)
    agg_primary <- agg_func(RVT_p_minus_Y_p)
    list(agg_primary) %>% magrittr::set_names(aggregate_primary)
  }
  matsindf::matsindf_apply(.sutdata, FUN = prim_func, R_mat = R, V_mat = V, Y_mat = Y)
}


#' Final demand aggregate energy
#'
#' Calculates aggregate final demand energy from a data frame of Supply-Use matrices.
#' The calculation includes non-energy uses if they are present in the final demand matrix.
#' The calculation does not include balancing items (Losses and Statistical differences).
#'
#' @param .sutdata a data frame with columns of matrices from a supply-use analysis.
#' @param fd_sectors a vector of names of sectors in final demand.
#' @param U use (\code{U}) matrix or name of the column in \code{.sutdata} containing same
#' @param Y final demand (\code{Y}) matrix or name of the column in \code{.sutdata} containing same
#' @param r_EIOU matrix of ratios of EIOU for the make (\code{U}) matrix or name of the column in \code{.sutdata} containing same
#' @param by one of "Product", "Sector", or "Total" to indicate the desired aggregation:
#' "Product" for aggregation by energy carrier (Crude oil, Primary solid biofuels, etc.),
#' "Sector" for aggregation by final demand sector (Agriculture/forestry, Domestic navigation, etc.), or
#' "Total" for aggregation over both Product and Sector (the default).
#' @param net_aggregate_demand the name of net energy demand on output.
#' Each entry is \code{sumall(Y_fd)}.
#' @param gross_aggregate_demand the name of gross energy demand on output.
#' Each entry is calculated by \code{sumall(Y_fd)} + \code{sumall(U_EIOU)}.
#'
#' @return a list or data frame containing \code{net_aggregate_demand} and \code{gross_aggregate_demand}
#'
#' @export
finaldemand_aggregates <- function(.sutdata,
                                   fd_sectors,
                                   # Input names
                                   U = "U",
                                   Y = "Y",
                                   r_EIOU = "r_EIOU",
                                   by = c("Total", "Product", "Sector"),
                                   # Output names
                                   net_aggregate_demand = "EX_fd_net.ktoe",
                                   gross_aggregate_demand = "EX_fd_gross.ktoe"){

  by <- match.arg(by)

  # Decide which aggregation function to use
  aggfuncs <- list(Total = "sumall_byname", Product = "rowsums_byname", Sector = "colsums_byname")
  agg_func <- match.fun(aggfuncs[[by]])

  fd_func <- function(U_mat, Y_mat, r_EIOU_mat){
    U_EIOU <- matsbyname::hadamardproduct_byname(r_EIOU_mat, U_mat)
    net <- Y_mat %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       matsbyname::make_pattern(row_col_names = fd_sectors, pattern_type = "leading")) %>%
      agg_func()
    gross <- matsbyname::sum_byname(net, agg_func(U_EIOU))
    if (by == "Sector") {
      # If "Sector" aggregation is requested, the results will be row vectors.
      # Convert to column vectors.
      net <- matsbyname::transpose_byname(net)
      gross <- matsbyname::transpose_byname(gross)
    }
    list(net, gross) %>% magrittr::set_names(c(net_aggregate_demand, gross_aggregate_demand))
  }
  matsindf::matsindf_apply(.sutdata, FUN = fd_func, U_mat = U, Y_mat = Y, r_EIOU_mat = r_EIOU)
}

#' Final demand aggregate energy with units
#'
#' Calculates aggregate final demand energy and services from a data frame of Supply-Use matrices.
#' The calculation includes non-energy uses if they are present in the final demand matrix.
#' The calculation does not include balancing items (Losses and Statistical differences).
#'
#' @param .sutdata a data frame with columns of matrices from a supply-use analysis.
#' @param fd_sectors a vector of names of sectors in final demand.
#' @param U use (\code{U}) matrix or name of the column in \code{.sutdata} containing same
#' @param Y final demand (\code{Y}) matrix or name of the column in \code{.sutdata} containing same
#' @param r_EIOU matrix of ratios of EIOU for the make (\code{U}) matrix or name of the column in \code{.sutdata} containing same
#' @param S_units the name of the column in \code{.sutdata} containing \code{S_units} matrices.
#' @param by one of "Product", "Sector", or "Total" to indicate the desired aggregation:
#' "Product" for aggregation by energy carrier (Crude oil, Primary solid biofuels, etc.),
#' "Sector" for aggregation by final demand sector (Agriculture/forestry, Domestic navigation, etc.), or
#' "Total" for aggregation over both Product and Sector (the default).
#' @param net_aggregate_demand the name of net energy demand (which excludes energy industry own use) on output.
#' @param gross_aggregate_demand the name of gross energy demand (which includes energy industry own use) on output.
#'
#' @return a list or data frame containing net aggregate energy demand
#' and gross aggregate energy demand
#'
#' @export
finaldemand_aggregates_with_units <- function(.sutdata,
                                              fd_sectors,
                                              # Input names
                                              U = "U",
                                              Y = "Y",
                                              r_EIOU = "r_EIOU",
                                              S_units = "S_units",
                                              by = c("Total", "Product", "Sector"),
                                              # Output names
                                              net_aggregate_demand,
                                              gross_aggregate_demand){

  by <- match.arg(by)

  fd_func <- function(U_mat, Y_mat, r_EIOU_mat, S_units_mat){
    U_EIOU <- matsbyname::hadamardproduct_byname(r_EIOU_mat, U_mat)
    if (by == "Product") {
      net <- matsbyname::rowsums_byname(Y_mat)
      gross <- matsbyname::sum_byname(matsbyname::rowsums_byname(U_EIOU), net)
    } else {
      # by is "Total" or "Sector".
      U_EIOU_bar <- matsbyname::matrixproduct_byname(matsbyname::transpose_byname(S_units_mat), U_EIOU)
      net <- matsbyname::matrixproduct_byname(
        matsbyname::transpose_byname(S_units_mat),
        Y_mat %>%
          matsbyname::select_cols_byname(retain_pattern =
                                           matsbyname::make_pattern(row_col_names = fd_sectors,
                                                                    pattern_type = "leading")))
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
  matsindf::matsindf_apply(.sutdata, FUN = fd_func, U_mat = U, Y_mat = Y, r_EIOU_mat = r_EIOU, S_units_mat = S_units)
}


#' Total primary aggregate energy from IEA tables
#'
#' Calculates total aggregate primary energy from a data frame of IEA data.
#' This function is named with "_IEA", because it is meant to operate on
#' tidy, IEA-style data frames.
#' The function \code{primary_aggregates} does the same thing,
#' but it is meant to operate on tidy SUT-style data frames.
#'
#' This function works similar to \code{\link[dplyr]{summarise}}:
#' it distills \code{.ieadata} to many fewer rows
#' according to the grouping variables.
#' Thus, \code{.ieadata} should be grouped prior to sending into this function.
#' Grouping columns are preserved on output.
#'
#' @param .ieadata the data frame containing an IEA table
#' @param flow the name of the column that contains flow information. Default is "\code{Flow}".
#' @param flow_aggregation_point the name of the column that identifies flow aggregation points.
#'        Default is "\code{Flow.aggregation.point}".
#' @param energy the name of the column that contains energy information. Default is "\code{E.dot}".
#' @param p_industries a vector of names of primary industries. Default is
#'        "\code{c("Coal mines", "Oil and gas extraction",
#'        "Production", "Imports", "Exports",
#'        "International aviation bunkers", "International marine bunkers",
#'        "Stock changes")}"
#' @param eiou the string that identifies energy industry own use in the \code{Flow.aggregation.point} column.
#'        Default is "\code{Energy industry own use}".
#' @param aggregate_primary the name of the aggregate primary energy
#'        column to be created in the output data frame.
#'        Default is "\code{EX_p_IEA.ktoe}".
#'
#' @return a data frame containing the grouping columns of \code{.ieadata}
#'         as well as a column named with the value of \code{aggregate_primary}.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' r_ind <- resource_industries(UKEnergy2000mats %>%
#'   spread(key = matrix.name, value = matrix))[["r_industries"]][[1]]
#' UKEnergy2000tidy %>%
#'   group_by(Country, Year, Energy.type, Last.stage) %>%
#'   primary_aggregates_IEA(p_industries = r_ind)
primary_aggregates_IEA <- function(.ieadata,
                                   # Input names
                                   flow = "Flow",
                                   flow_aggregation_point = "Flow.aggregation.point",
                                   energy = "E.dot",
                                   p_industries = c("Production", "Coal mines", "Oil and gas extraction",
                                                    "Imports", "Exports", "International aviation bunkers",
                                                    "International marine bunkers", "Stock changes"),
                                   eiou = "Energy industry own use",
                                   # Output name
                                   aggregate_primary = "EX_p_IEA.ktoe"){
  flow <- as.name(flow)
  flow_aggregation_point <- as.name(flow_aggregation_point)
  energy <- as.name(energy)
  aggregate_primary <- as.name(aggregate_primary)
  .ieadata %>%
    dplyr::filter(startsWith_any_of(!!flow, p_industries), !startsWith(!!flow_aggregation_point, eiou)) %>%
    dplyr::summarise(!!aggregate_primary := sum(!!energy))
}


#' Final demand aggregate energy from IEA tables
#'
#' Calculates total aggregate final demand energy from a data frame of IEA data
#' on both net and gross bases.
#'
#' This function is named with "_IEA", because it is meant to operate on
#' tidy, IEA-style data frames.
#' The function \link{finaldemand_aggregates} does the same thing,
#' but it is meant to operate on SUT-style data frames.
#'
#' Note that all items in \code{.ieadata} must be in same units.
#'
#' This function works similar to \code{\link[dplyr]{summarise}}:
#' it distills \code{.ieadata} to many fewer rows
#' according to the grouping variables.
#' Thus, \code{.ieadata} should be grouped prior to passing into this function.
#' Grouping columns are preserved on output.
#'
#' @param .ieadata a data frame with columns of IEA data.
#' @param ledger_side the name of the ledger side column in \code{.ieadata}.
#'        Default is "\code{Ledger.side}".
#' @param flow_aggregation_point the name of the flow aggregation point column in \code{.ieadata}.
#'        Default is "\code{Flow.aggregation.point}".
#' @param flow the name of the column that contains flow information.
#'        Default is "\code{Flow}".
#' @param energy the name of the column that contains energy information.
#'        Default is "\code{E.dot}".
#' @param consumption the identifier for consumption in the \code{flow_aggregation_point} column.
#'        Default is "\code{Consumption}".
#' @param eiou the identifier for energy industry own use in the \code{flow_aggregation_point} column.
#'        Default is "\code{Energy industry own use}".
#' @param aggregate_net_finaldemand the name of the output column containing aggregates of net final demand.
#'        Default is "\code{EX_fd_net_IEA.ktoe}".
#' @param aggregate_gross_finaldemand the name of the output column containing aggregates of gross final demand.
#'        Default is "\code{EX_fd_gross_IEA.ktoe}".
#'
#' @export
#'
#' @return a data frame containing grouping columns of \code{.ieadata} and
#'         two additional columns containing net and gross final demand.
#'
#' @examples
#' library(dplyr)
#' # Works only when all entries are in same units.
#' # When Last.stage is services, different units are involved.
#' # Thus, filter to rows in UKEnergy2000tidy where Last.stage is final or useful.
#' print(names(UKEnergy2000tidy))
#' UKEnergy2000tidy %>%
#'   group_by(Country, Year, Energy.type, Last.stage) %>%
#'   filter(Last.stage %in% c("Final", "Useful")) %>%
#'   finaldemand_aggregates_IEA()
finaldemand_aggregates_IEA <- function(.ieadata,
                                       # Input names
                                       ledger_side = "Ledger.side",
                                       flow_aggregation_point = "Flow.aggregation.point",
                                       flow = "Flow",
                                       energy = "E.dot",
                                       consumption = "Consumption",
                                       eiou = "Energy industry own use",
                                       # Output names
                                       aggregate_net_finaldemand = "EX_fd_net_IEA.ktoe",
                                       aggregate_gross_finaldemand = "EX_fd_gross_IEA.ktoe"){
  ledger_side <- as.name(ledger_side)
  flow_aggregation_point <- as.name(flow_aggregation_point)
  flow <- as.name(flow)
  energy <- as.name(energy)
  diff_colname <- as.name(".gross_less_net")

  matsindf::verify_cols_missing(.ieadata, diff_colname)

  # First calculate net energy
  net <- .ieadata %>%
    dplyr::filter(startsWith_any_of(!!ledger_side, consumption)) %>%
    dplyr::summarise(
      !!aggregate_net_finaldemand := sum(!!energy)
    )
  # Now calculate additional energy, gross - net = eiou
  gross_less_net <- .ieadata %>%
    dplyr::filter(startsWith_any_of(!!flow_aggregation_point, eiou)) %>%
    dplyr::mutate(
      !!energy := abs(!!energy)
    ) %>%
    dplyr::summarise(
      !!diff_colname := sum(!!energy)
    )
  # Add net and gross_less_net to obtain gross and return the resulting data frame.
  dplyr::full_join(net, gross_less_net, by = dplyr::group_vars(.ieadata)) %>%
    dplyr::mutate(
      !!as.name(aggregate_gross_finaldemand) := !!as.name(aggregate_net_finaldemand) + !!diff_colname
    ) %>%
    dplyr::select(-(!!diff_colname))
}

