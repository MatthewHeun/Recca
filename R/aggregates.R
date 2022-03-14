#' Primary energy and exergy aggregates
#'
#' Calculates aggregate primary energy from a data frame of Supply-Use matrices.
#'
#' @param .sutdata A data frame with columns of matrices from a supply-use analysis.
#' @param p_industries a vector of names of industries to be aggregated as "primary."
#'                     If `.sutdata` is a data frame, `p_industries` should be the name of a column in the data frame.
#'                     If `.sutdata` is `NULL`, `p_industries` can be a single vector of industry names.
#'                     These industries in `p_industries` will appear in rows of the resource (`R`) and make (`V`) matrices and
#'                     columns of the final demand matrix (`Y`).
#'                     Entries in `Y_p` will be subtracted from entries in `R_p + V_p` to obtain
#'                     the total primary energy aggregate,
#'                     where `*_p` is the primary part of those matrices.
#'                     The function `find_p_industry_names()` might be helpful to find
#'                     primary industry names if they can be identified by prefixes.
#' @param pattern_type One of "exact", "leading", "trailing", or "anywhere" which specifies
#'                     how matches are made for `fd_sectors`.
#'                     If "exact", exact matches specify the sectors to be aggregated.
#'                     If "leading", sectors are aggregated if any entry in `fd_sectors` matches the leading part of a final demand sector's name.
#'                     If "trailing", sectors are aggregated if any entry in `fd_sectors` matches the trailing part of a final demand sector's name.
#'                     If "anywhere", sectors are aggregated if any entry in `fd_sectors` matches any part of a final demand sector's name.
#'                     Default is "leading" to enable primary industries such as "Resources \[of Crude oil\]".
#' @param R,V,Y See `Recca::psut_cols`.
#' @param by One of "Total", "Product", or "Flow" to indicate the desired aggregation:
#'        \itemize{
#'          \item "Total": aggregation over both Product and Flow (the default),
#'          \item "Product": aggregation by energy carrier (Crude oil, Primary solid biofuels, etc.), or
#'          \item "Flow": aggregation by type of flow (Production, Imports, Exports, etc.).
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
                               pattern_type = c("exact", "leading", "trailing", "anywhere"),
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

  pattern_type <- match.arg(pattern_type)
  by <- match.arg(by)

  prim_func <- function(p_industries_vec, R_mat = NULL, V_mat, Y_mat){
    # Look for primary industries in each of R, V, and Y matrices
    RT_p <- matsbyname::transpose_byname(R_mat) %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       RCLabels::make_or_pattern(strings = p_industries_vec, pattern_type = pattern_type))
    VT_p <- matsbyname::transpose_byname(V_mat) %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       RCLabels::make_or_pattern(strings = p_industries_vec, pattern_type = pattern_type))
    # Get the primary industries from the Y matrix.
    Y_p <- Y_mat %>% matsbyname::select_cols_byname(retain_pattern =
                                                      RCLabels::make_or_pattern(strings = p_industries_vec, pattern_type = pattern_type))
    # TPES in product x industry matrix format is RT_p + VT_p - Y_p.
    RVT_p_minus_Y_p <- matsbyname::sum_byname(RT_p, VT_p) %>% matsbyname::difference_byname(Y_p)

    # Use the right function for the requested aggregation
    if (by == "Total") {
      agg_primary <- matsbyname::sumall_byname(RVT_p_minus_Y_p)
    } else if (by == "Product") {
      agg_primary <- matsbyname::rowsums_byname(RVT_p_minus_Y_p)
    } else if (by == "Flow") {
      agg_primary <- matsbyname::colsums_byname(RVT_p_minus_Y_p)
    }
    # No need for a last "else" clause, because match.arg ensures we have only one of
    # "Total", "Product", or "Flow".

    list(agg_primary) %>% magrittr::set_names(aggregate_primary)
  }
  matsindf::matsindf_apply(.sutdata, FUN = prim_func, p_industries_vec = p_industries, R_mat = R, V_mat = V, Y_mat = Y)
}


#' Final demand energy and exergy aggregates
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
#' @param pattern_type One of "exact", "leading", "trailing", or "anywhere" which specifies
#'                     how matches are made for `fd_sectors`.
#'                     If "exact", exact matches specify the sectors to be aggregated.
#'                     If "leading", sectors are aggregated if any entry in `fd_sectors` matches the leading part of a final demand sector's name.
#'                     If "trailing", sectors are aggregated if any entry in `fd_sectors` matches the trailing part of a final demand sector's name.
#'                     If "anywhere", sectors are aggregated if any entry in `fd_sectors` matches any part of a final demand sector's name.
#'                     Default is "exact".
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
                                   pattern_type = c("exact", "leading", "trailing", "anywhere"),
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

  pattern_type <- match.arg(pattern_type)
  by <- match.arg(by)

  fd_func <- function(fd_sectors_vec, U_mat, Y_mat, r_EIOU_mat){
    U_EIOU <- matsbyname::hadamardproduct_byname(r_EIOU_mat, U_mat)

    net_prelim <- Y_mat %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       RCLabels::make_or_pattern(strings = fd_sectors_vec, pattern_type = pattern_type))
    gross_prelim <- U_EIOU %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       RCLabels::make_or_pattern(strings = fd_sectors_vec, pattern_type = pattern_type))

    # Use the right function for the requested aggregation
    if (by == "Total") {
      net <- net_prelim %>%
        matsbyname::sumall_byname()
      gross <- gross_prelim %>%
        matsbyname::sumall_byname() %>%
        matsbyname::sum_byname(net)
    } else if (by == "Product") {
      net <- net_prelim %>%
        matsbyname::rowsums_byname()
      gross <- gross_prelim %>%
        matsbyname::rowsums_byname() %>%
        matsbyname::sum_byname(net)
    } else if (by == "Sector") {
      net <- net_prelim %>%
        matsbyname::colsums_byname()
      gross <- gross_prelim %>%
        matsbyname::colsums_byname() %>%
        matsbyname::sum_byname(net)
    }
    # No need for a last "else" clause, because match.arg ensures we have only one of
    # "Total", "Product", or "Flow".

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


#' Final demand energy and exergy aggregates with units
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
#' @param pattern_type One of "exact", "leading", "trailing", or "anywhere" which specifies
#'                     how matches are made for `fd_sectors`.
#'                     If "exact", exact matches specify the sectors to be aggregated.
#'                     If "leading", sectors are aggregated if any entry in `fd_sectors` matches the leading part of a final demand sector's name.
#'                     If "trailing", sectors are aggregated if any entry in `fd_sectors` matches the trailing part of a final demand sector's name.
#'                     If "anywhere", sectors are aggregated if any entry in `fd_sectors` matches any part of a final demand sector's name.
#'                     Default is "exact".
#' @param U Use (`U`) matrix or name of the column in `.sutdata` containing same.
#' @param Y Final demand (`Y`) matrix or name of the column in `.sutdata` containing same.
#' @param r_EIOU Matrix of ratios of EIOU for the make (`U`) matrix or name of the column in `.sutdata` containing same.
#' @param S_units The name of the column in `.sutdata` containing `S_units` matrices.
#' @param by One of "Product", "Sector", or "Total" to indicate the desired aggregation:
#'           "Product" for aggregation by energy carrier (Crude oil, Primary solid biofuels, etc.),
#'           "Sector" for aggregation by final demand sector (Agriculture/forestry, Domestic navigation, etc.), or
#'           "Total" for aggregation over both Product and Sector (the default).
#' @param net_aggregate_demand The name of net energy demand (which excludes energy industry own use) on output.
#' @param gross_aggregate_demand The name of gross energy demand (which includes energy industry own use) on output.
#'
#' @return A list or data frame containing net aggregate energy demand
#'         and gross aggregate energy demand.
#'
#' @export
finaldemand_aggregates_with_units <- function(.sutdata,
                                              fd_sectors,
                                              pattern_type = c("exact", "leading", "trailing", "anywhere"),
                                              # Input names
                                              U = Recca::psut_cols$U,
                                              Y = Recca::psut_cols$Y,
                                              r_EIOU = Recca::psut_cols$r_eiou,
                                              S_units = Recca::psut_cols$S_units,
                                              by = c("Total", "Product", "Sector"),
                                              # Output names
                                              net_aggregate_demand = Recca::aggregate_cols$net_aggregate_demand,
                                              gross_aggregate_demand = Recca::aggregate_cols$gross_aggregate_demand){

  pattern_type <- match.arg(pattern_type)
  by <- match.arg(by)

  fd_func <- function(fd_sectors_vec, U_mat, Y_mat, r_EIOU_mat, S_units_mat){
    U_EIOU <- matsbyname::hadamardproduct_byname(r_EIOU_mat, U_mat)
    # Filter columns of interest
    U_EIOU <- U_EIOU %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       RCLabels::make_or_pattern(strings = fd_sectors_vec,
                                                                pattern_type = pattern_type))
    Y_mat <- Y_mat %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       RCLabels::make_or_pattern(strings = fd_sectors_vec,
                                                                pattern_type = pattern_type))
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


#' Aggregate into regions
#'
#' Aggregates a data frame according to the regions given in a column of the data frame.
#' The data frame (`.sut_data`) should contain metadata columns (including `many_colname` and `few_colname`)
#' and be wide-by-matrices.
#'
#' @param .sut_data A wide-by-matrices `matsindf`-style data frame of PSUT matrices.
#' @param many_colname The name of the column in `.sut_data` that contains the "many" descriptions,
#'                     for example countries that need to be aggregated to continents.
#'                     Default is `IEATools::iea_cols$country`.
#' @param few_colnme The of the column in `.sut_data` that contains the "few" descriptions,
#'                   for example continents into which countries are to be aggregated.
#'                   Default is `Recca::aggregate_cols$region`.
#' @param year,method,energy_type,last_stage See `IEATools::iea_cols`.
#' @param matrix_cols Names of columns in .sut_data containing matrices.
#'                    Default is a vector of names from `Recca::psut_cols`:
#'                    R, U, U_feed, U_eiou, r_eiou, V, Y, and S_units.
#' @param matrix_names,matrix_values Internal column names. See `Recca::psut_cols`.
#'
#' @return A modified version of `.sut_data` wherein the `country` column is replaced
#'         by region aggregates specified by `aggregation_map`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(matsbyname)
#' library(tidyr)
#' mats_GBR <- UKEnergy2000mats %>%
#'   tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)
#' # Add other countries, by duplicating and renaming GBR
#' mats <- dplyr::bind_rows(mats_GBR,
#'                          mats_GBR %>% dplyr::mutate(Country = "USA"),
#'                          mats_GBR %>% dplyr::mutate(Country = "FRA"))
#' # Establish the aggregation map.
#' agg_df <- list(EUR = c("GBR", "FRA"), AMR = "USA") %>%
#'   matsbyname::aggregation_map_to_df(few_colname = "Continent", many_colname = "Country")
#' # Aggregate into continents
#' dplyr::left_join(mats, agg_df, by = "Country") %>%
#'   region_aggregates(mats, country = "Country", region = "Continent")
region_aggregates <- function(.sut_data,
                              many_colname = IEATools::iea_cols$country,
                              few_colname = Recca::aggregate_cols$region,
                              year = IEATools::iea_cols$year,
                              method = IEATools::iea_cols$method,
                              energy_type = IEATools::iea_cols$energy_type,
                              last_stage = IEATools::iea_cols$last_stage,
                              matrix_cols = c(R = Recca::psut_cols$R,
                                              U = Recca::psut_cols$U,
                                              U_feed = Recca::psut_cols$U_feed,
                                              U_eiou = Recca::psut_cols$U_eiou,
                                              r_eiou = Recca::psut_cols$r_eiou,
                                              V = Recca::psut_cols$V,
                                              Y = Recca::psut_cols$Y,
                                              S_units = Recca::psut_cols$S_units),
                              matrix_names = Recca::psut_cols$matnames,
                              matrix_values = Recca::psut_cols$matvals) {

  # Make the incoming data frame tidy.
  tidy_df <- .sut_data %>%
    tidyr::pivot_longer(cols = unname(matrix_cols), names_to = matrix_names, values_to = matrix_values) %>%
    # We need to re-calculate U ad r_EIOU matrices after aggregation.
    # So get rid of them here.
    dplyr::filter(! .data[[matrix_names]] == matrix_cols[["U"]], ! .data[[matrix_names]] == matrix_cols[["r_eiou"]])
  group_cols <- names(tidy_df) %>%
    setdiff(many_colname) %>%
    setdiff(matrix_values)
  tidy_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)), .add = TRUE) %>%
    # Summarise using the new .summarise argument to sum_byname.
    dplyr::summarise("{matrix_values}" := matsbyname::sum_byname(.data[[matrix_values]], .summarise = TRUE)) %>%
    # Rename few_colname to many_colname
    dplyr::rename(
      "{many_colname}" := .data[[few_colname]]
    ) %>%
    # And pivot wider again to give wide by matrices shape.
    tidyr::pivot_wider(names_from = matrix_names, values_from = matrix_values) %>%
    # Remove the groupings we added.
    dplyr::ungroup() %>%
    # Recalculate U and r_EIOU matrices
    dplyr::mutate(
      "{matrix_cols[['U']]}" := matsbyname::sum_byname(.data[[ matrix_cols[["U_feed"]] ]],
                                                       .data[[ matrix_cols[["U_eiou"]] ]]),
      "{matrix_cols[['r_eiou']]}" := matsbyname::quotient_byname(.data[[ matrix_cols[["U_eiou"]] ]],
                                                                 .data[[ matrix_cols[["U"]] ]]) %>%
        matsbyname::replaceNaN_byname(val = 0),
      # S_units will be summed to give (possibly) non-unity values.
      # Divide by itself and replace NaN by 0 to
      # get back to unity values when non-zero.
      "{matrix_cols[['S_units']]}" := matsbyname::quotient_byname(.data[[ matrix_cols[["S_units"]] ]],
                                                                  .data[[ matrix_cols[["S_units"]] ]])  %>%
        matsbyname::replaceNaN_byname(val = 0)
    )
}
