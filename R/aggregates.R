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
  agg_func <- aggfuncs[[tolower(by)]]

  Out <- .sutdata %>%
    select_(.dots = c(intersect(keep_cols, names(.)), V_colname, Y_colname)) %>%
    # Transpose V so that we can directly add the V and Y matrices.
    # Select only primary columns from VT and Y.
    mutate_(
      .dots = list(
        # VT_p
        interp(~ transpose_byname(vcol) %>% select_cols_byname(retain_pattern = make_pattern(row_col_names = p_industries, pattern_type = "leading")),
               vcol = as.name(V_colname)),
        # Y_p
        interp(~ ycol %>% select_cols_byname(retain_pattern = make_pattern(p_industries, pattern_type = "leading")),
               ycol = as.name(Y_colname))
      ) %>%
        setNames(c(".VT_p", ".Y_p"))
    ) %>%
    mutate_(
      .dots = list(
        # VT_p - Y_p. This is TPES in product x industry matrix format
        interp(~ difference_byname(vtpcol, ypcol),
               vtpcol = as.name(".VT_p"),
               ypcol = as.name(".Y_p"))
      ) %>%
        setNames(c(".VT_p_minus_Y_p"))
    ) %>%
    mutate_(
      .dots = list(
        # aggregated TPES = aggfunc(.VT_p_minus_Yp)
        interp(~ af(diffcolname),
               af = as.name(agg_func),
               diffcolname = as.name(".VT_p_minus_Y_p"))
      ) %>%
        setNames(c(aggregate_primary_colname))
    )

  # Do some cleanup

  if (tolower(by) == "total") {
    # Need to convert aggregate column to numeric,
    # because the aggregate is only a single number when we ask for "Total" aggregation.
    Out <- Out %>%
      mutate_(
        .dots = list(
          interp(~ as.numeric(aggcol),
                 aggcol = as.name(aggregate_primary_colname))
        ) %>%
          setNames(c(aggregate_primary_colname))
      )
  } else if (tolower(by) == "flow") {
    # If "Flow" aggregation is requested, the results will be a row vector.
    # Convert to a column vector.
    Out <- Out %>%
      mutate_(
        .dots = list(
          interp(~ transpose_byname(aggcol),
                 aggcol = as.name(aggregate_primary_colname))
        ) %>%
          setNames(c(aggregate_primary_colname))
      )
  }

  # Keep only the desired columns and return the resulting data frame.
  Out %>%
    select_(.dots = c(intersect(keep_cols, names(.)), aggregate_primary_colname))
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
                                   by = "Total",
                                   # Output columns
                                   keep_cols = NULL,
                                   net_aggregate_demand_colname,
                                   gross_aggregate_demand_colname){

  # Decide which aggregation function to use
  aggfuncs <- list(total = "sumall_byname", product = "rowsums_byname", sector = "colsums_byname")
  if (!tolower(by) %in% names(aggfuncs)) {
    stop(paste("Unknown value of argument by:", by, "in primary_aggregates."))
  }
  agg_func <- aggfuncs[[tolower(by)]]

  Out <- .sutdata %>%
    # Select only relevant columns
    select_(.dots = c(intersect(keep_cols, names(.)), U_colname, V_colname, Y_colname, r_EIOU_colname)) %>%
    # And add EIOU information to the data frame.
    mutate_(
      .dots = list(
        # .EIOU = r_EIOU * U
        interp(~ elementproduct_byname(reiou, u),
               reiou = as.name(r_EIOU_colname),
               u = as.name(U_colname))
      ) %>%
        setNames(".EIOU")
    ) %>%
    mutate_(
      .dots = list(
        # net = agg_func(Y_fd)
        interp(~ ycol %>% select_cols_byname(retain_pattern = make_pattern(row_col_names = fd_sectors, pattern_type = "leading")) %>% af(),
               ycol = as.name(Y_colname),
               af = as.name(agg_func))
      ) %>%
        setNames(net_aggregate_demand_colname)
    ) %>%
    mutate_(
      .dots = list(
        # gross = net + agg_func(EIOU)
        interp(~ sum_byname(net, af(eiou)),
               net = as.name(net_aggregate_demand_colname),
               eiou = as.name(".EIOU"),
               af = as.name(agg_func))
      ) %>%
        setNames(gross_aggregate_demand_colname)
    )

  # Do some cleanup.

  if (tolower(by) == "total") {
    # Need to convert the net and gross columns to numeric,
    # because net and gross are only single numbers when we ask for "Total" aggregation.
    Out <- Out %>%
      mutate_(
        .dots = list(
          interp(~ as.numeric(net),
                 net = as.name(net_aggregate_demand_colname)),
          interp(~ as.numeric(gross),
                 gross = as.name(gross_aggregate_demand_colname))
        ) %>%
          setNames(c(net_aggregate_demand_colname, gross_aggregate_demand_colname))
      )
  } else if (tolower(by) == "sector") {
    # If "Sector" aggregation is requested, the results will be row vectors.
    # Convert to column vectors.
    Out <- Out %>%
      mutate_(
        .dots = list(
          interp(~ transpose_byname(net),
                 net = as.name(net_aggregate_demand_colname)),
          interp(~ transpose_byname(gross),
                 gross = as.name(gross_aggregate_demand_colname))
        ) %>%
          setNames(c(net_aggregate_demand_colname, gross_aggregate_demand_colname))
      )
  }

  # Select desired columns only and return.
  Out %>%
    select_(.dots = c(intersect(keep_cols, names(.)), net_aggregate_demand_colname, gross_aggregate_demand_colname))
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
  # Decide which aggregation function to use
  # aggfuncs <- list(total = "sumall_byname", product = "rowsums_byname", sector = "colsums_byname")
  # if (!tolower(by) %in% names(aggfuncs)) {
  #   stop(paste("Unknown value of argument by:", by, "in primary_aggregates."))
  # }
  # agg_func <- as.name(aggfuncs[[tolower(by)]])

  # Establish names for columns
  U_EIOU <- as.name(".U_EIOU")
  U_EIOU_bar <- as.name(".U_EIOU_bar")
  U <- as.name(U)
  U_bar <- as.name(".U_bar")
  Y <- as.name(Y)
  r_EIOU <- as.name(r_EIOU)
  S_units <- as.name(S_units)
  net <- as.name(net_aggregate_demand_colname)
  gross <- as.name(gross_aggregate_demand_colname)

  if (by == "Product") {
    Out <- .sutdata %>%
      mutate(
        !!U_EIOU := elementproduct_byname(!!r_EIOU, !!U),
        !!net := rowsums_byname(!!Y),
        !!gross := sum_byname(rowsums_byname(!!U_EIOU), !!net)
      )
  } else {
    # by is "Total" or "Sector".
    Out <- .sutdata %>%
      mutate(
        !!U_EIOU := elementproduct_byname(!!r_EIOU, !!U),
        !!U_EIOU_bar := matrixproduct_byname(transpose_byname(!!S_units), !!U_EIOU),
        # !!net := !!Y %>%
        #   select_cols_byname(retain_pattern = make_pattern(row_col_names = fd_sectors,
        #                                                    pattern_type = "leading")) %>%
        #   (!!agg_func)(),
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

#' Primary aggregate energy from IEA tables
#'
#' Calculates aggregate primary energy from a data frame of IEA data tables.
#' This function is named with "_IEA", because it is meant to operate on
#' tidy, IEA-style data frames.
#' The function \code{primary_aggregates} does the same thing,
#' but it is meant to operate on tidy SUT-style data frames.
#'
#' Note that \code{.ieadata} should be grouped prior to sending into this function.
#' Grouping columns are preserved on output.
#'
#' @param .ieadata the data frame containing an IEA table
#' @param UVY_colname the name of the column in \code{.ieadata}
#' containing names of submatrices of U, V, and G to which this
#' row of data belongs.
#' @param energy_colname the name of the column in \code{.ieadata}
#' that contains energy data.
#' @param energy_type_colname the name of the column in \code{.ieadata}
#' that contains the energy type for this row in the table
#' @param V_p_varname the name of the primary submatrix (\code{_p})
#' of the Make (\code{V}) matrix.
#' @param Y_p_varname the name of the primary submatrix (\code{_p})
#' of the final demand (\code{G}) matrix.
#' @param aggregate_primary_colname the name of the aggregate primary energy
#' column to be created in the output data frame.
#'
#' @return a data frame containing the grouping columns of \code{.ieadata}
#' as well as a column named \code{aggregate_primary_colname}.
#'
#' @export
primary_aggregates_IEA <- function(.ieadata,
                                   # Input information
                                   UVY_colname = "UVY", energy_colname = "EX.ktoe",
                                   V_p_varname = "V_p", Y_p_varname = "Y_p",
                                   # Output information
                                   aggregate_primary_colname){

  .ieadata %>%
    filter_(interp(~ uvy %in% c(vp, gp),
                   uvy = as.name(UVY_colname),
                   vp = V_p_varname,
                   gp = Y_p_varname)
    ) %>%
    group_by_(.dots = c(UVY_colname), add = TRUE) %>%
    summarise_(.dots = lazyeval::interp(~ sum(EX), EX = as.name(energy_colname)) %>%
                 list %>%
                 setNames(energy_colname)
    ) %>%
    spread_(key_col = UVY_colname, value_col = energy_colname) %>%
    mutate_(
      # Note that the calculation below is a sum, because IEA data are signed;
      # V_p data are positive, and Y_p data are negative, because exports are viewed as negative supply.
      # So, we can simply add these items here.
      .dots = interp(~ vp + gp,
                     vp = as.name(V_p_varname),
                     gp = as.name(Y_p_varname)) %>%
        list %>%
        setNames(aggregate_primary_colname)
    ) %>%
    select_(.dots = c(groups(.), aggregate_primary_colname))
}


#' Final demand aggregate energy from IEA tables
#'
#' Calculates aggregate final demand energy from a data frame of IEA data.
#'
#' @param .ieadata a data frame with columns of IEA data.
#' @param UVY_colname the name of the column in \code{.ieadata}
#' containing names of submatrices of U, V, and G to which this
#' row of data belongs.
#' @param energy_colname the name of the column in \code{.ieadata}
#' that contains energy data.
#' @param Y_fd_varname the name of the variable in \code{.ieadata} containing final demand (\code{Y_fd}) matrices.
#' \code{Y_fd} will typically contain energy consumption by industries, transport, and other.
#' @param U_EIOU_colname the name of the column in \code{.ieadata} containing Energy industry own use
#' (\code{U_EIOU}) matrices.
#' @param keep_cols the names of colums in \code{.sutdata} to retain in the output
#' @param aggregate_net_finaldemand_colname the name of the output column containing aggregates of net energy demand.
#' Each entry in this column is \code{sumall(Y_fd)}.
#' @param aggregate_gross_finaldemand_colname the name of the output column containing aggregates of gross energy demand.
#' Each entry in this column is calculated by \code{sumall(Y_fd)} + \code{sumall(U_EIOU)}.
#'
#' @return a two-column data fram containing the gross aggregate energy demand
#' and net aggregate energy demand for each row of \code{.sutdata}.
#'
#' @export
finaldemand_aggregates_IEA <- function(.ieadata,
                                       # Input information
                                       UVY_colname = "UVY", energy_colname = "EX.ktoe",
                                       Y_fd_varname = "Y_fd", Y_ne_varname = "Y_ne",
                                       U_EIOU_varname = "U_EIOU",
                                       # Output information
                                       aggregate_net_finaldemand_colname,
                                       aggregate_gross_finaldemand_colname){

  # First calculate net energy
  .ieadata %>%
    filter_(.dots = interp(~ uvy %in% c(Y_fd_varname, Y_ne_varname),
                           uvy = as.name(UVY_colname))) %>%
    summarise_(.dots = interp(~ sum(EX), EX = as.name(energy_colname)) %>%
                 list %>%
                 setNames(aggregate_net_finaldemand_colname)
    ) %>%
    full_join(
      # Combine with EIOU energy
      .ieadata %>%
        filter_(lazyeval::interp(~ uvy == ueiou,
                                 uvy = as.name(UVY_colname),
                                 ueiou = U_EIOU_varname)) %>%
        group_by_(.dots = c(UVY_colname), add = TRUE) %>%
        # summarise(EX.ktoe = sum(EX.ktoe)) %>%
        summarise_(.dots = interp(~ sum(EX), EX = as.name(energy_colname)) %>%
                     list %>%
                     setNames(".U_EIOU")
        ) %>%
        select_(.dots = lazyeval::interp(~ - uvy,
                                         uvy = as.name(UVY_colname))
        ),
      by = as.character(groups(.ieadata))
    ) %>%
    mutate_(
      # Gross energy is net - eiou
      # Note that eiou numbers are negative (because EIOU decreases supply in Transformation processes).
      # However, we want to add EIOU to the net energy.
      # So, we need to subtract EIOU from net below.
      .dots = interp(~ net - eiou,
                     net = as.name(aggregate_net_finaldemand_colname),
                     eiou = as.name(".U_EIOU")) %>%
        list %>%
        setNames(aggregate_gross_finaldemand_colname)
    ) %>%
    select_(
      # Get rid of .U_EIOU column
      .dots = interp(~ - eiou,
                     eiou = as.name(".U_EIOU"))
    )
}
