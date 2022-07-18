#' Primary energy and exergy aggregates
#'
#' Calculates aggregate primary energy from a data frame of Supply-Use matrices.
#'
#' By default, this function adds a single column of primary energy aggregates
#' with the name `aggregate_primary`.
#' If `add_net_gross_cols` is `TRUE` (default is `FALSE`),
#' two columns are created:
#' `net_aggregate_primary` and `gross_aggregate_primary`.
#' With net and gross output (`add_net_gross_cols = TRUE`),
#' the columns contain identical values.
#' Use `add_net_gross_cols = TRUE` if you later wish to combine with
#' results from `finaldemand_aggregates()`,
#' which provides both net and gross outputs.
#'
#' @param .sutdata A data frame with columns of matrices from a supply-use analysis.
#' @param p_industries A vector of names of industries to be aggregated as "primary."
#'                     If `.sutdata` is a data frame, `p_industries` should be the name of a column in the data frame.
#'                     If `.sutdata` is `NULL`, `p_industries` can be a single vector of industry names.
#'                     These industries in `p_industries` will appear in rows of the resource (`R`) and make (`V`) matrices and
#'                     columns of the final demand matrix (`Y`).
#'                     Entries in `Y_p` will be subtracted from entries in `R_p + V_p` to obtain
#'                     the total primary energy aggregate,
#'                     where `*_p` is the primary part of those matrices.
#'                     The function `find_p_industry_names()` might be helpful to find
#'                     primary industry names if they can be identified by prefixes.
#' @param add_net_gross_cols A boolean that tells whether to add net and gross columns (`TRUE`) or not (`FALSE`).
#'                           Default is `FALSE`.
#' @param pattern_type One of "exact", "leading", "trailing", or "anywhere" which specifies
#'                     how matches are made for `p_industries`.
#'                     If "exact", exact matches specify the sectors to be aggregated.
#'                     If "leading", sectors are aggregated if any entry in `p_industries` matches the leading part of a final demand sector's name.
#'                     If "trailing", sectors are aggregated if any entry in `p_industries` matches the trailing part of a final demand sector's name.
#'                     If "anywhere", sectors are aggregated if any entry in `p_industries` matches any part of a final demand sector's name.
#'                     Default is "exact".
#' @param R,V,Y See `Recca::psut_cols`.
#' @param by One of "Total", "Product", or "Flow" to indicate the desired aggregation:
#'        \itemize{
#'          \item "Total": aggregation over both Product and Flow (the default),
#'          \item "Product": aggregation by energy carrier (Crude oil, Primary solid biofuels, etc.), or
#'          \item "Flow": aggregation by type of flow (Production, Imports, Exports, etc.).
#'        }
#' @param aggregate_primary,net_aggregate_primary,gross_aggregate_primary The names for aggregates of primary energy on output.
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
#'   Recca::primary_aggregates(p_industries = "p_industries", by = "Total")
#' tibble::as_tibble(res)
#' res[[Recca::aggregate_cols$aggregate_primary]]
#' # Above, only 1 aggregate column is created, because there is no
#' # difference between net and gross aggregation for primary energy.
#' # Below, both net and gross aggregation columns are created,
#' # for compatibility with the `finaldemand_aggregates()` function.
#' # Net and gross primary aggregates are identical.
#' res2 <- UKEnergy2000mats %>%
#'   tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
#'   dplyr::mutate(
#'     p_industries = rep(list(p_industries), times = nrow(.))
#'   ) %>%
#'   Recca::primary_aggregates(p_industries = "p_industries",
#'                             add_net_gross_cols = TRUE,
#'                             by = "Total")
#' tibble::as_tibble(res2)
#' res2[[Recca::aggregate_cols$net_aggregate_primary]]
#' res2[[Recca::aggregate_cols$gross_aggregate_primary]]
primary_aggregates <- function(.sutdata = NULL,
                               # Vector of primary industries
                               p_industries,
                               add_net_gross_cols = FALSE,
                               pattern_type = c("exact", "leading", "trailing", "anywhere"),
                               # Input names
                               R = Recca::psut_cols$R,
                               V = Recca::psut_cols$V,
                               Y = Recca::psut_cols$Y,
                               by = c("Total", "Product", "Flow"),
                               # Output names
                               aggregate_primary = Recca::aggregate_cols$aggregate_primary,
                               net_aggregate_primary = Recca::aggregate_cols$net_aggregate_primary,
                               gross_aggregate_primary = Recca:: aggregate_cols$gross_aggregate_primary){

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

    # Check if gross and net columns are desired before returning.
    if (add_net_gross_cols) {
      out <- list(agg_primary, agg_primary) %>%
        magrittr::set_names(c(net_aggregate_primary, gross_aggregate_primary))
    } else {
      out <- list(agg_primary) %>%
        magrittr::set_names(aggregate_primary)
    }
  return(out)
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
finaldemand_aggregates <- function(.sutdata = NULL,
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
  # assertthat::assert_that(requireNamespace("matsbyname"),
  #                         msg = "package 'matsbyname' is required but not available.")

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
#' @param few_colname The of the column in `.sut_data` that contains the "few" descriptions,
#'                    for example continents into which countries are to be aggregated.
#'                    Default is `Recca::aggregate_cols$region`.
#' @param year,method,energy_type,last_stage See `IEATools::iea_cols`.
#' @param matrix_cols Names of columns in .sut_data containing matrices.
#'                    Default is a vector of names from `Recca::psut_cols`:
#'                    R, U, U_feed, U_eiou, r_eiou, V, Y, and S_units.
#' @param matrix_names,matrix_values Internal column names. See `Recca::psut_cols`.
#'
#' @return An aggregated version of `.sut_data` wherein the `many_colname` column is replaced
#'         by `few_colname` as specified by `aggregation_map`.
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
#'   matsbyname::agg_map_to_agg_table(few_colname = "Continent", many_colname = "Country")
#' # Aggregate into continents
#' dplyr::left_join(mats, agg_df, by = "Country") %>%
#'   region_aggregates(mats, many_colname = "Country", few_colname = "Continent")
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

  # Handle the case when .sut_data has no rows.
  if (nrow(.sut_data) == 0) {
    # Return .sut_data unmodified.
    return(.sut_data)
  }

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


#' Despecify and aggregate PSUT matrices
#'
#' PSUT matrices often have row and column names that been specified to
#' contain more information than simply the industry or product.
#' Examples include
#' "Resources \[of Coal\]" and
#' "Automobiles -> RoP".
#' It is sometimes helpful to despecify and aggregate these rows and columns
#' so that all "Resources" are summed together,
#' all "Automobiles" are summed together, etc.
#' This function performs that aggregation.
#'
#' By default, the aggregation is made to the nouns of row and column names
#' as defined by the `RCLabels` package.
#' Which piece is to be aggregated is given in the `piece_to_keep` argument.
#' Internally, this function uses `matsbyname::aggregate_pieces_byname()`
#' to do the heavy lifting.
#'
#' @param .sut_data A data frame of matrices to be despecified and aggregated.
#' @param piece_to_keep The piece of the label to retain before aggregation.
#'                      Default is "noun".
#' @param R,U,V,Y,r_EIOU,U_EIOU,U_feed,S_units Matrices or names of columns in `.sut_data` to be despecified and aggregated. See `Recca::psut_cols`.
#' @param inf_notation A boolean that tells whether to infer the row and column label notation.
#'                     Default is `TRUE`.
#' @param notation The notation for row and column labels.
#'                 Default is `list(RCLabels::notations_list)`.
#' @param margin The margins over which aggregation is performed.
#'               Default is `list(c(1, 2))`.
#' @param choose_most_specific A boolean that tells whether to choose the most-specific
#'                             notation if 2 or more notations match.
#'                             Default is `TRUE`.
#' @param prepositions A list of prepositions that could appear in row and column names.
#'                     Default is `list(RCLabels::prepositions_list)`.
#' @param R_aggregated_colname,U_aggregated_colname,V_aggregated_colname,Y_aggregated_colname,r_EIOU_aggregated_colname,U_EIOU_aggregated_colname,U_feed_aggregated_colname,S_units_aggregated_colname Names of
#'                     aggregated matrices or columns.
#' @param aggregated_suffix A string suffix used to form the names for aggregated matrices.
#'                          Default is "_aggregated".
#'
#' @return A modified version of `.sut_data` where rows and columns of matrices
#'         have been aggregated to their despecified parts.
#'
#' @export
#'
#' @examples
#' UKEnergy2000mats %>%
#'   tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) %>%
#'   despecified_aggregates()
despecified_aggregates <- function(.sut_data = NULL,
                                   piece_to_keep = "noun",
                                   # Input matrix names
                                   R = Recca::psut_cols$R,
                                   U = Recca::psut_cols$U,
                                   V = Recca::psut_cols$V,
                                   Y = Recca::psut_cols$Y,
                                   r_eiou = Recca::psut_cols$r_eiou,
                                   U_eiou = Recca::psut_cols$U_eiou,
                                   U_feed = Recca::psut_cols$U_feed,
                                   S_units = Recca::psut_cols$S_units,
                                   # Notation inference
                                   inf_notation = TRUE,
                                   notation = list(RCLabels::notations_list),
                                   margin = list(c(1, 2)),
                                   choose_most_specific = TRUE,
                                   prepositions = list(RCLabels::prepositions_list),
                                   # Names for the aggregated matrices
                                   R_aggregated_colname = paste0(Recca::psut_cols$R, aggregated_suffix),
                                   U_aggregated_colname = paste0(Recca::psut_cols$U, aggregated_suffix),
                                   V_aggregated_colname = paste0(Recca::psut_cols$V, aggregated_suffix),
                                   Y_aggregated_colname = paste0(Recca::psut_cols$Y, aggregated_suffix),
                                   r_eiou_aggregated_colname = paste0(Recca::psut_cols$r_eiou, aggregated_suffix),
                                   U_eiou_aggregated_colname = paste0(Recca::psut_cols$U_eiou, aggregated_suffix),
                                   U_feed_aggregated_colname = paste0(Recca::psut_cols$U_feed, aggregated_suffix),
                                   S_units_aggregated_colname = paste0(Recca::psut_cols$S_units, aggregated_suffix),
                                   # Suffix for aggregated columns
                                   aggregated_suffix = Recca::aggregate_cols$aggregated_suffix) {

  despecify_agg_func <- function(R_mat,
                                 U_mat,
                                 V_mat,
                                 Y_mat,
                                 r_eiou_mat,
                                 U_eiou_mat,
                                 U_feed_mat,
                                 S_units_mat) {
    despecified <- lapply(list(R_mat, U_mat, V_mat, Y_mat, U_feed_mat, S_units_mat), function(m) {
      m %>%
        matsbyname::aggregate_pieces_byname(piece = piece_to_keep,
                                            margin = margin,
                                            inf_notation = inf_notation,
                                            notation = notation,
                                            choose_most_specific = choose_most_specific,
                                            prepositions = prepositions)
    })
    R_out <- despecified[[1]]
    U_out <- despecified[[2]]
    V_out <- despecified[[3]]
    Y_out <- despecified[[4]]
    U_feed_out <- despecified[[5]]
    U_eiou_out <- matsbyname::difference_byname(U_out, U_feed_out)
    r_eiou_out <- matsbyname::quotient_byname(U_eiou_out, U_out) %>%
      matsbyname::replaceNaN_byname(val = 0)
    S_units_out <- matsbyname::quotient_byname(despecified[[6]], despecified[[6]]) %>%
      matsbyname::replaceNaN_byname(val = 0)

    # Make a list and return the matrices
    list(R_out, U_out, V_out, Y_out, U_feed_out, U_eiou_out, r_eiou_out, S_units_out) %>%
      magrittr::set_names(c(R_aggregated_colname, U_aggregated_colname, V_aggregated_colname,
                            Y_aggregated_colname, r_eiou_aggregated_colname, U_eiou_aggregated_colname,
                            U_feed_aggregated_colname, S_units_aggregated_colname))
  }

  matsindf::matsindf_apply(.sut_data,
                           FUN = despecify_agg_func,
                           R_mat = R,
                           U_mat = U,
                           V_mat = V,
                           Y_mat = Y,
                           r_eiou_mat = r_eiou,
                           U_eiou_mat = U_eiou,
                           U_feed_mat = U_feed,
                           S_units_mat = S_units)
}


#' Perform grouping aggregations on PSUT matrices
#'
#' It is often helpful to aggregate data into industry or product categories,
#' such as "Anthracite" and "Brown coal" to "Coal and coal products" or
#' "Domestic aviation" and "Domestic navigation" to "Transport".
#' With the help of an `aggregation_map`, this function
#' performs such aggregations for a set of PSUT matrices.
#'
#' Internally, this function uses `matsbyname::aggregate_byname()`.
#' See its documentation for details on the format for the `aggregation_map`.
#'
#' @param .sut_data A data frame of matrices to be despecified and aggregated.
#' @param aggregation_map Aggregation details. See documentation for `matsbyname::aggregate_byname()` for further information.
#' @param margin `1`, `2`, or `c(1, 2)` for row aggregation, column aggregation, or both.
#'               Can be a row or column type.
#'               Default is `c(1, 2)`.
#' @param pattern_type See `RCLabels::make_or_pattern()`.
#'                     Default is "exact".
#' @param R,U,V,Y,r_EIOU,U_EIOU,U_feed,S_units Matrices or names of columns in `.sut_data` to be despecified and aggregated. See `Recca::psut_cols`.
#' @param R_aggregated_colname,U_aggregated_colname,V_aggregated_colname,Y_aggregated_colname,r_EIOU_aggregated_colname,U_EIOU_aggregated_colname,U_feed_aggregated_colname,S_units_aggregated_colname Names of
#'                     aggregated matrices or columns.
#' @param aggregated_suffix A string suffix used to form the names for aggregated matrices.
#'                          Default is "_aggregated".
#'
#' @return PSUT matrices aggregated according to `aggregation_map`.
#'
#' @export
#'
#' @examples
#' UKEnergy2000mats %>%
#'   tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) %>%
#'   grouped_aggregates(aggregation_map = list(`Oil and oil products` =
#'                                             c("Crude", "Diesel", "Petrol")),
#'                      pattern_type = "leading",
#'                      margin = "Product")
grouped_aggregates <- function(.sut_data = NULL,
                               aggregation_map,
                               margin = c(1, 2),
                               pattern_type = "exact",
                               # Input matrix names
                               R = Recca::psut_cols$R,
                               U = Recca::psut_cols$U,
                               V = Recca::psut_cols$V,
                               Y = Recca::psut_cols$Y,
                               r_eiou = Recca::psut_cols$r_eiou,
                               U_eiou = Recca::psut_cols$U_eiou,
                               U_feed = Recca::psut_cols$U_feed,
                               S_units = Recca::psut_cols$S_units,
                               # Names for the aggregated matrices
                               R_aggregated_colname = paste0(Recca::psut_cols$R, aggregated_suffix),
                               U_aggregated_colname = paste0(Recca::psut_cols$U, aggregated_suffix),
                               V_aggregated_colname = paste0(Recca::psut_cols$V, aggregated_suffix),
                               Y_aggregated_colname = paste0(Recca::psut_cols$Y, aggregated_suffix),
                               r_eiou_aggregated_colname = paste0(Recca::psut_cols$r_eiou, aggregated_suffix),
                               U_eiou_aggregated_colname = paste0(Recca::psut_cols$U_eiou, aggregated_suffix),
                               U_feed_aggregated_colname = paste0(Recca::psut_cols$U_feed, aggregated_suffix),
                               S_units_aggregated_colname = paste0(Recca::psut_cols$S_units, aggregated_suffix),
                               # Suffix for aggregated columns
                               aggregated_suffix = Recca::aggregate_cols$aggregated_suffix) {

  group_agg_func <- function(R_mat,
                             U_mat,
                             V_mat,
                             Y_mat,
                             r_eiou_mat,
                             U_eiou_mat,
                             U_feed_mat,
                             S_units_mat) {
    grouped <- lapply(list(R_mat, U_mat, V_mat, Y_mat, U_feed_mat, S_units_mat), function(m) {
      m %>%
        matsbyname::aggregate_byname(aggregation_map = aggregation_map,
                                     margin = margin,
                                     pattern_type = pattern_type)
    })
    R_out <- grouped[[1]]
    U_out <- grouped[[2]]
    V_out <- grouped[[3]]
    Y_out <- grouped[[4]]
    U_feed_out <- grouped[[5]]
    U_eiou_out <- matsbyname::difference_byname(U_out, U_feed_out)
    r_eiou_out <- matsbyname::quotient_byname(U_eiou_out, U_out) %>%
      matsbyname::replaceNaN_byname(val = 0)
    S_units_out <- matsbyname::quotient_byname(grouped[[6]], grouped[[6]]) %>%
      matsbyname::replaceNaN_byname(val = 0)

    # Make a list and return the matrices
    list(R_out, U_out, V_out, Y_out, U_feed_out, U_eiou_out, r_eiou_out, S_units_out) %>%
      magrittr::set_names(c(R_aggregated_colname, U_aggregated_colname, V_aggregated_colname,
                            Y_aggregated_colname, r_eiou_aggregated_colname, U_eiou_aggregated_colname,
                            U_feed_aggregated_colname, S_units_aggregated_colname))
  }

  matsindf::matsindf_apply(.sut_data,
                           FUN = group_agg_func,
                           R_mat = R,
                           U_mat = U,
                           V_mat = V,
                           Y_mat = Y,
                           r_eiou_mat = r_eiou,
                           U_eiou_mat = U_eiou,
                           U_feed_mat = U_feed,
                           S_units_mat = S_units)
}


#' Calculate footprint aggregates
#'
#' A footprint aggregate are isolated measures of primary and final demand energy
#' required to supply a specific amount of final demand energy.
#' This function calculates footprint aggregates for several categories of final demand.
#'
#' By default, footprint aggregates are calculated for each individual
#' product and sector of final demand in the `Y` matrix.
#' This calculation is accomplished for each description of an energy conversion chain (ECC)
#' by the following algorithm:
#'
#' 1. Calculate io matrices with `calc_io_mats()`.
#' 2. Identify each product and sector from rows and columns of the `Y` matrix.
#' 3. For each product and sector independently,
#'    perform an upstream swim with `new_Y()`
#'    to obtain the ECC requirements to supply that product or sector.
#' 4. Calculate primary and final demand aggregates using `primary_aggregates()` and
#'    `finaldemand_aggregates()`.
#'    Both functions are called with `by = "Total"`,
#'    yielding total primary and final demand aggregates.
#' 5. Add the primary and final demand aggregates as columns at the right side of `.sut_data`.
#'
#' @param .sut_data
#' @param p_industries A vector of names of industries to be aggregated as "primary."
#'                     If `.sut_data` is a data frame, `p_industries` should be the name of a column in the data frame.
#'                     If `.sut_data` is `NULL`, `p_industries` can be a single vector of industry names.
#'                     These industries in `p_industries` will appear in rows of the resource (`R`) and make (`V`) matrices and
#'                     columns of the final demand matrix (`Y`).
#'                     Entries in `Y_p` will be subtracted from entries in `R_p + V_p` to obtain
#'                     the total primary energy aggregate,
#'                     where `*_p` is the primary part of those matrices.
#'                     The function `find_p_industry_names()` might be helpful to find
#'                     primary industry names if they can be identified by prefixes.
#'                     This argument is passed to `primary_aggregates()`.
#' @param fd_sectors A vector of names of sectors in final demand.
#'                   Names should include columns in the `Y` and `U_EIOU` matrices
#'                   to cover both net (in `Y`) and gross (in `Y` and `U_EIOU`) final demand.
#'                   This argument is passed to `finaldemand_aggregates()`.
#' @param pattern_type One of "exact", "leading", "trailing", or "anywhere" which specifies
#'                     how matches are made for `p_industries`.
#'                     If "exact", exact matches specify the sectors to be aggregated.
#'                     If "leading", sectors are aggregated if any entry in `p_industries` matches the leading part of a final demand sector's name.
#'                     If "trailing", sectors are aggregated if any entry in `p_industries` matches the trailing part of a final demand sector's name.
#'                     If "anywhere", sectors are aggregated if any entry in `p_industries` matches any part of a final demand sector's name.
#'                     Default is "exact".
#'                     This argument is passed to both `primary_aggregates()` and `finaldemand_aggregates()`.
#' @param R,U,U_feed,V,Y,S_units Matrices that describe the energy conversion chain (ECC).
#'                       See `Recca::psut_cols` for default values.
#' @param add_primary_net_gross_cols A boolean that tells whether to add primary net and gross columns (`TRUE`) or not (`FALSE`).
#'                                   Default is `FALSE`.
#' @param aggregate_primary,net_aggregate_primary,gross_aggregate_primary,net_aggregate_demand,gross_aggregate_demand Names of
#'                                   output columns.
#'                                   See `Recca::aggregate_cols`.
#'
#' @return
#'
#' @export
#'
#' @examples
footprint_aggregates <- function(.sut_data = NULL,
                                 p_industries,
                                 fd_sectors,
                                 pattern_type = c("exact", "leading", "trailing", "anywhere"),
                                 # Input names
                                 R = Recca::psut_cols$R,
                                 U = Recca::psut_cols$U,
                                 U_feed = Recca::psut_cols$U_feed,
                                 V = Recca::psut_cols$V,
                                 Y = Recca::psut_cols$Y,
                                 S_units = Recca::psut_cols$S_units,
                                 by = c("Total", "Product", "Sector"),
                                 add_primary_net_gross_cols = FALSE,
                                 # Output names
                                 aggregate_primary = Recca::aggregate_cols$aggregate_primary,
                                 net_aggregate_primary = Recca::aggregate_cols$net_aggregate_primary,
                                 gross_aggregate_primary = Recca:: aggregate_cols$gross_aggregate_primary,
                                 net_aggregate_demand = Recca::aggregate_cols$net_aggregate_demand,
                                 gross_aggregate_demand = Recca::aggregate_cols$gross_aggregate_demand) {

  footprint_func <- function(R_mat, U_mat, U_feed_mat, V_mat, Y_mat, S_units_mat) {
    # Calculate the IO matrices
    with_io <- list(R = R_mat, U = U_mat, U_feed = U_feed_mat, V = V_mat, Y = Y_mat, S_units = S_units_mat) %>%
      calc_io_mats()

    # Get the row names in Y. Those are the Products we want to evaluate.
    new_Y_products <- matsbyname::getrownames_byname(Y_mat) %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_product) {
        # For each product (in each row), make a new Y matrix to be used for the calculation.
        Y_mat %>%
          matsbyname::select_rows_byname(Hmisc::escapeRegex(this_product))
      })

    # Get the column names in Y. Those are the Sectors we want to evaluate.
    new_Y_sectors <- matsbyname::getcolnames_byname(Y_mat) %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_sector) {
        # For each sector (in each column), make a new Y matrix to be used for the calculation.
        Y_mat %>%
          matsbyname::select_cols_byname(Hmisc::escapeRegex(this_sector))
      })

    # Create a list with new Y matrices for all products and sectors
    new_Y_list <- c(new_Y_products, new_Y_sectors)

    # For each item in this list, make a new set of matrices
    ecc_prime <- new_Y_list %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_new_Y) {
        with_io %>%
          append(list(this_new_Y)) %>%
          magrittr::set_names(c(names(with_io), "Y_prime")) %>%
          new_Y()

    })

    # Now that we have the new ECCs, calculate primary and final demand aggregates
    p_aggregates <- ecc_prime %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_new_ecc) {
        this_new_ecc %>%
          primary_aggregates(p_industries = list(p_industries),
                             R = "R_prime", V = "V_prime", Y = "Y_prime",
                             pattern_type = pattern_type,
                             by = "Total",
                             aggregate_primary = aggregate_primary)
      })

  }

  matsindf::matsindf_apply(.sut_data,
                           FUN = footprint_func,
                           R_mat = R,
                           U_mat = U,
                           U_feed_mat = U_feed,
                           V_mat = V,
                           Y_mat = Y,
                           S_units_mat = S_units)

  # If .sut_data is a data frame, expand if desired.

}


