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
#' @param keep_cols a vector of names of columns of \code{.sutdata} to return with the output
#' @param aggregate_primary_colname the name of the output column containing aggregates of primary energy.
#'
#' @return a data frame containing columns for \code{keep_cols} and
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
                               keep_cols = NULL,
                               aggregate_primary_colname){

  by <- match.arg(by)
  aggfuncs <- list(total = "sumall_byname", product = "rowsums_byname", flow = "colsums_byname")
  agg_func <- match.fun(aggfuncs[[tolower(by)]])

  # Establish names
  V <- as.name(V_colname)
  Y <- as.name(Y_colname)
  # Establish intermediate column names
  VT_p <- as.name(".VT_p")
  Y_p <- as.name(".Y_p")
  VT_p_minus_Y_p <- as.name(".VT_p_minus_Y_p")
  # Establish output column name
  agg_primary <- as.name(aggregate_primary_colname)

  # Ensure that we won't overwrite a column.
  verify_cols_missing(.sutdata, c(VT_p, Y_p, VT_p_minus_Y_p, agg_primary))

  Out <- .sutdata %>%
    # Transpose V so that we can directly add the V and Y matrices.
    # Select only primary columns from VT and Y.
    mutate(
      !!VT_p := transpose_byname(!!V) %>%
        select_cols_byname(retain_pattern = make_pattern(row_col_names = p_industries, pattern_type = "leading")),
      !!Y_p := !!Y %>%
        select_cols_byname(retain_pattern = make_pattern(row_col_names = p_industries, pattern_type = "leading")),
      # VT_p - Y_p. This is TPES in product x industry matrix format
      !!VT_p_minus_Y_p := difference_byname(!!VT_p, !!Y_p),
      !!agg_primary := agg_func(!!VT_p_minus_Y_p)
    )

  # Do some cleanup

  if (by == "Total") {
    # Need to convert aggregate column to numeric,
    # because the aggregate is only a single number when we ask for "Total" aggregation.
    Out <- Out %>%
      mutate(
        !!agg_primary := as.numeric(!!agg_primary)
      )
  } else if (by == "Flow") {
    # If "Flow" aggregation is requested, the results will be a row vector.
    # Convert to a column vector.
    Out <- Out %>%
      mutate(
        !!agg_primary := transpose_byname(!!agg_primary)
      )
  }

  # Eliminate temporary columns
  Out %>%
    select(-(!!VT_p), -(!!Y_p), -(!!VT_p_minus_Y_p))
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
#' @param keep_cols a vector of names of columns of \code{.sutdata} to return with the output
#' @param net_aggregate_demand_colname the name of the output column containing aggregates of net energy demand.
#' Each entry in this column is \code{sumall(Y_fd)}.
#' @param gross_aggregate_demand_colname the name of the output column containing aggregates of gross energy demand.
#' Each entry in this column is calculated by \code{sumall(Y_fd)} + \code{sumall(U_EIOU)}.
#'
#' @return a data frame containing the columns in \code{keep_cols}, gross aggregate energy demand
#' and net aggregate energy demand for each row of \code{.sutdata}.
#'
#' @export
finaldemand_aggregates <- function(.sutdata,
                                   fd_sectors,
                                   # Input columns
                                   U_colname = "U",
                                   V_colname = "V",
                                   Y_colname = "Y",
                                   r_EIOU_colname = "r_EIOU",
                                   by = c("Total", "Product", "Sector"),
                                   # Output columns
                                   keep_cols = NULL,
                                   net_aggregate_demand_colname,
                                   gross_aggregate_demand_colname){

  by <- match.arg(by)

  # Decide which aggregation function to use
  aggfuncs <- list(Total = "sumall_byname", Product = "rowsums_byname", Flow = "colsums_byname")
  agg_func <- match.fun(aggfuncs[[by]])

  # Establish names
  U <- as.name(U_colname)
  V <- as.name(V_colname)
  Y <- as.name(Y_colname)
  r_EIOU <- as.name(r_EIOU_colname)
  # Intermediate column names
  U_EIOU <- as.name(".U_EIOU")
  # Output columns
  net <- as.name(net_aggregate_demand_colname)
  gross <- as.name(gross_aggregate_demand_colname)

  # Ensure that we won't overwrite a column.
  verify_cols_missing(.sutdata, c(net, gross, U_EIOU))

  Out <- .sutdata %>%
    # Select only relevant columns
    # select_(.dots = c(intersect(keep_cols, names(.)), U_colname, V_colname, Y_colname, r_EIOU_colname)) %>%
    mutate(
      # And add EIOU information to the data frame.
      !!U_EIOU := elementproduct_byname(!!r_EIOU, !!U),
      !!net := !!Y %>% select_cols_byname(retain_pattern = make_pattern(row_col_names = fd_sectors, pattern_type = "leading")) %>% agg_func(),
      !!gross := sum_byname(!!net, agg_func(!!U_EIOU))
    )

  # Do some cleanup.

  if (by == "Total") {
    # Need to convert the net and gross columns to numeric,
    # because net and gross are only single numbers when we ask for "Total" aggregation.
    Out <- Out %>%
      mutate(
        !!net := as.numeric(!!net),
        !!gross := as.numeric(!!gross)
      )
  } else if (by == "Sector") {
    # If "Sector" aggregation is requested, the results will be row vectors.
    # Convert to column vectors.
    Out <- Out %>%
      mutate(
        !!net := transpose_byname(!!net),
        !!gross := transpose_byname(!!gross)
      )
  }

  # Eliminate temporary columns
  Out %>%
    select(-(!!U_EIOU))
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

  # Establish names for input columns
  U <- as.name(U)
  Y <- as.name(Y)
  r_EIOU <- as.name(r_EIOU)
  S_units <- as.name(S_units)
  # Establish names for intermediate columns
  U_EIOU <- as.name(".U_EIOU")
  U_EIOU_bar <- as.name(".U_EIOU_bar")
  U_bar <- as.name(".U_bar")
  # Establish names for output columns
  net <- as.name(net_aggregate_demand_colname)
  gross <- as.name(gross_aggregate_demand_colname)

  # Ensure that we won't overwrite a column.
  verify_cols_missing(.sutdata, c(net, gross, U_EIOU, U_EIOU_bar, U_bar))

  if (by == "Product") {
    Out <- .sutdata %>%
      mutate(
        !!U_EIOU := elementproduct_byname(!!r_EIOU, !!U),
        !!net := rowsums_byname(!!Y),
        !!gross := sum_byname(rowsums_byname(!!U_EIOU), !!net)
      ) %>%
      # Eliminate temporary column
      select(-(!!U_EIOU))
  } else {
    # by is "Total" or "Sector".
    Out <- .sutdata %>%
      mutate(
        !!U_EIOU := elementproduct_byname(!!r_EIOU, !!U),
        !!U_EIOU_bar := matrixproduct_byname(transpose_byname(!!S_units), !!U_EIOU),
        !!net := matrixproduct_byname(
          transpose_byname(!!S_units),
          !!Y %>% select_cols_byname(retain_pattern = make_pattern(row_col_names = fd_sectors,
                                                                   pattern_type = "leading"))
        ),
        !!gross := sum_byname(!!U_EIOU_bar, !!net),
        !!net := transpose_byname(!!net),
        !!gross := transpose_byname(!!gross)
      ) %>%
      # Eliminate temporary columns that we just made.
      select(-(!!U_EIOU), -(!!U_EIOU_bar))
  }

  # Do some cleanup.
  if (by == "Total") {
    Out <- Out %>%
      mutate(
        # Sum down columns (Sectors)
        !!net := colsums_byname(!!net),
        !!gross := colsums_byname(!!gross)
      )
  }
  return(Out)
}
