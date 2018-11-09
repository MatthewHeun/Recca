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
#' @importFrom matsbyname select_cols_byname
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
                               aggregate_primary_colname = "EX_p.ktoe"){

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
#' @importFrom matsbyname elementproduct_byname
#' @importFrom matsbyname select_cols_byname
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
                                   net_aggregate_demand_colname = "EX_fd_net.ktoe",
                                   gross_aggregate_demand_colname = "EX_fd_gross.ktoe"){

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
    list(net, gross) %>% set_names(c(net_aggregate_demand_colname, gross_aggregate_demand_colname))
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
#' @param net_aggregate_demand_colname the name of the output column containing aggregates of net energy demand.
#' This column excludes energy industry own use.
#' @param gross_aggregate_demand_colname the name of the output column containing aggregates of gross energy demand.
#' This column includes energy industry own use.
#'
#' @return a data frame containing net aggregate energy demand
#' and gross aggregate energy demand for each row of \code{.sutdata}.
#'
#' @importFrom matsbyname select_cols_byname
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
    list(net, gross) %>% set_names(c(net_aggregate_demand_colname, gross_aggregate_demand_colname))
  }
  matsindf_apply(.sutdata, FUN = fd_func, U = U, Y = Y, r_EIOU = r_EIOU, S_units = S_units)
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
#' @param energy the name of the column that contains energy information. Default is "\code{EX.ktoe}".
#' @param p_industries a vector of names of primary industries. Default is
#'        "\code{c("Coal mines", "Oil and gas extraction",
#'        "Production", "Imports", "Exports",
#'        "International aviation bunkers", "International marine bunkers",
#'        "Stock changes")}"
#' @param eiou the string that identifies energy industry own use in the \code{Flow.aggregation.point} column.
#'        Default is "\code{Energy industry own use}".
#' @param aggregate_primary_colname the name of the aggregate primary energy
#'        column to be created in the output data frame.
#'        Default is "\code{EX_p_IEA.ktoe}".
#'
#' @return a data frame containing the grouping columns of \code{.ieadata}
#'         as well as a column named with the value of \code{aggregate_primary_colname}.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(magrittr)
#' library(tidyr)
#' r_ind <- resource_industries(UKEnergy2000mats %>%
#'   spread(key = matrix.name, value = matrix))[["r_industries"]][[1]]
#' UKEnergy2000tidy %>%
#'   group_by(Country, Year, Energy.type, Last.stage) %>%
#'   primary_aggregates_IEA(p_industries = r_ind)
primary_aggregates_IEA <- function(.ieadata,
                                   # Input information
                                   flow = "Flow",
                                   flow_aggregation_point = "Flow.aggregation.point",
                                   energy = "EX.ktoe",
                                   p_industries = c("Production", "Coal mines", "Oil and gas extraction",
                                                    "Imports", "Exports", "International aviation bunkers",
                                                    "International marine bunkers", "Stock changes"),
                                   eiou = "Energy industry own use",
                                   # Output information
                                   aggregate_primary_colname = "EX_p_IEA.ktoe"){
  flow <- as.name(flow)
  flow_aggregation_point <- as.name(flow_aggregation_point)
  energy <- as.name(energy)
  aggregate_primary_colname <- as.name(aggregate_primary_colname)
  .ieadata %>%
    filter(starts_with_any_of(!!flow, p_industries), !startsWith(!!flow_aggregation_point, eiou)) %>%
    summarise(!!aggregate_primary_colname := sum(!!energy))
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
#'        Default is "\code{EX.ktoe}".
#' @param consumption the identifier for consumption in the \code{flow_aggregation_point} column.
#'        Default is "\code{Consumption}".
#' @param eiou the identifier for energy industry own use in the \code{flow_aggregation_point} column.
#'        Default is "\code{Energy industry own use}".
#' @param aggregate_net_finaldemand_colname the name of the output column containing aggregates of net final demand.
#'        Default is "\code{EX_fd_net_IEA.ktoe}".
#' @param aggregate_gross_finaldemand_colname the name of the output column containing aggregates of gross final demand.
#'        Default is "\code{EX_fd_gross_IEA.ktoe}".
#'
#' @importFrom dplyr full_join
#' @importFrom dplyr group_vars
#' @importFrom dplyr summarise
#' @importFrom matsindf verify_cols_missing
#'
#' @export
#'
#' @return a data frame containing grouping columns of \code{.ieadata} and
#'         two additional columns containing net and gross final demand.
#'
#' @examples
#' library(dplyr)
#' library(magrittr)
#' # Works only when all entries are in same units.
#' # When Last.stage is services, different units are involved.
#' # Thus, filter to rows in UKEnergy2000tidy where Last.stage is final or useful.
#' print(names(UKEnergy2000tidy))
#' UKEnergy2000tidy %>%
#'   group_by(Country, Year, Energy.type, Last.stage) %>%
#'   filter(Last.stage %in% c("final", "useful")) %>%
#'   finaldemand_aggregates_IEA()
finaldemand_aggregates_IEA <- function(.ieadata,
                                       # Input information
                                       ledger_side = "Ledger.side",
                                       flow_aggregation_point = "Flow.aggregation.point",
                                       flow = "Flow",
                                       energy = "EX.ktoe",
                                       consumption = "Consumption",
                                       eiou = "Energy industry own use",
                                       # Output information
                                       aggregate_net_finaldemand_colname = "EX_fd_net_IEA.ktoe",
                                       aggregate_gross_finaldemand_colname = "EX_fd_gross_IEA.ktoe"){
  ledger_side <- as.name(ledger_side)
  flow_aggregation_point <- as.name(flow_aggregation_point)
  flow <- as.name(flow)
  energy <- as.name(energy)
  diff_colname <- as.name(".gross_less_net")

  verify_cols_missing(.ieadata, diff_colname)

  # First calculate net energy
  net <- .ieadata %>%
    filter(starts_with_any_of(!!ledger_side, consumption)) %>%
    summarise(
      !!aggregate_net_finaldemand_colname := sum(!!energy)
    )
  # Now calculate additional energy, gross - net = eiou
  gross_less_net <- .ieadata %>%
    filter(starts_with_any_of(!!flow_aggregation_point, eiou)) %>%
    mutate(
      !!energy := abs(!!energy)
    ) %>%
    summarise(
      !!diff_colname := sum(!!energy)
    )
  # Add net and gross_less_net to obtain gross and return the resulting data frame.
  full_join(net, gross_less_net, by = group_vars(.ieadata)) %>%
    mutate(
      !!as.name(aggregate_gross_finaldemand_colname) := !!as.name(aggregate_net_finaldemand_colname) + !!diff_colname
    ) %>%
    select(-(!!diff_colname))
}

