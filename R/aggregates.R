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
#'           \itemize{
#'             \item "Total": aggregation over both Product and Flow (the default),
#'             \item "Product": aggregation by energy carrier (Crude oil, Primary solid biofuels, etc.), or
#'             \item "Flow": aggregation by type of flow (Production, Imports, Exports, etc.).
#'           }
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
#'   Recca::primary_aggregates(p_industries = p_industries, by = "Total")
#' tibble::as_tibble(res)
#' res[[Recca::aggregate_cols$aggregate_primary]]
#' # Above, only 1 aggregate column is created, because there is no
#' # difference between net and gross aggregation for primary energy.
#' # Below, both net and gross aggregation columns are created,
#' # for compatibility with the `finaldemand_aggregates()` function.
#' # Net and gross primary aggregates are identical.
#' res2 <- UKEnergy2000mats %>%
#'   tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
#'   Recca::primary_aggregates(p_industries = p_industries,
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

  prim_func <- function(R_mat = NULL, V_mat, Y_mat){
    # Look for primary industries in each of R, V, and Y matrices
    RT_p <- matsbyname::transpose_byname(R_mat) %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       RCLabels::make_or_pattern(strings = p_industries, pattern_type = pattern_type))
    VT_p <- matsbyname::transpose_byname(V_mat) %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       RCLabels::make_or_pattern(strings = p_industries, pattern_type = pattern_type))
    # Get the primary industries from the Y matrix.
    Y_p <- Y_mat %>% matsbyname::select_cols_byname(retain_pattern =
                                                      RCLabels::make_or_pattern(strings = p_industries, pattern_type = pattern_type))
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
  matsindf::matsindf_apply(.sutdata, FUN = prim_func, R_mat = R, V_mat = V, Y_mat = Y)
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
#' @param U,U_feed,Y Input matrices. See `Recca::psut_cols`.
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
                                   U_feed = Recca::psut_cols$U_feed,
                                   Y = Recca::psut_cols$Y,
                                   by = c("Total", "Product", "Sector"),
                                   # Output names
                                   net_aggregate_demand = Recca::aggregate_cols$net_aggregate_demand,
                                   gross_aggregate_demand = Recca::aggregate_cols$gross_aggregate_demand){

  pattern_type <- match.arg(pattern_type)
  by <- match.arg(by)

  fd_func <- function(U_mat, U_feed_mat, Y_mat){
    U_eiou_mat <- matsbyname::difference_byname(U_mat, U_feed_mat)

    Y_mat_cols <- Y_mat %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       RCLabels::make_or_pattern(strings = fd_sectors, pattern_type = pattern_type))
    U_eiou_mat_cols <- U_eiou_mat %>%
      matsbyname::select_cols_byname(retain_pattern =
                                       RCLabels::make_or_pattern(strings = fd_sectors, pattern_type = pattern_type))

    # Use the right function for the requested aggregation
    if (by == "Total") {
      net <- Y_mat_cols %>%
        matsbyname::sumall_byname()
      gross <- U_eiou_mat_cols %>%
        matsbyname::sumall_byname() %>%
        matsbyname::sum_byname(net)
    } else if (by == "Product") {
      net <- Y_mat_cols %>%
        matsbyname::rowsums_byname()
      gross <- U_eiou_mat_cols %>%
        matsbyname::rowsums_byname() %>%
        matsbyname::sum_byname(net)
    } else if (by == "Sector") {
      net <- Y_mat_cols %>%
        matsbyname::colsums_byname()
      gross <- U_eiou_mat_cols %>%
        matsbyname::colsums_byname() %>%
        matsbyname::sum_byname(net)
    }
    # No need for a last "else" clause, because match.arg ensures we have only one of
    # "Total", "Product", or "Flow".

    # When Y_mat_cols and U_mat_cols are NULL (i.e., no columns selected),
    # we get NULL results above for net.
    # That results should really be 0.
    # So check for that condition.
    if (is.null(net)) {
      net <- 0
    }
    # We don't ever get null for gross, because sum_byname(NULL, NULL) is 0.
    # if (is.null(gross)) {
    #   gross <- 0
    # }

    if (by == "Sector") {
      # If "Sector" aggregation is requested, the results will be row vectors.
      # Convert to column vectors.
      net <- matsbyname::transpose_byname(net)
      gross <- matsbyname::transpose_byname(gross)
    }
    list(net, gross) %>% magrittr::set_names(c(net_aggregate_demand, gross_aggregate_demand))
  }
  matsindf::matsindf_apply(.sutdata, FUN = fd_func, U_mat = U, U_feed_mat = U_feed, Y_mat = Y)
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
    # Return .sut_data unmodified,
    # except to eliminate the few_colname and ensure that the many_colname is present
    # Eliminate many_colname.
    out <- .sut_data %>%
      dplyr::mutate(
        "{many_colname}" := NULL
      ) %>%
      dplyr::rename(
        "{many_colname}" := .data[[few_colname]]
      ) %>%
      dplyr::relocate(.data[[many_colname]]) # Relocates to left, where it belongs.
    return(out)
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
#' @param R,U,U_feed,U_eiou,r_eiou,V,Y,S_units Matrices or names of columns in `.sut_data` to be despecified and aggregated. See `Recca::psut_cols`.
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
#' @param R_aggregated_colname,U_aggregated_colname,U_feed_aggregated_colname,U_eiou_aggregated_colname,r_eiou_aggregated_colname,V_aggregated_colname,Y_aggregated_colname,S_units_aggregated_colname Names of
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
                                   U_feed = Recca::psut_cols$U_feed,
                                   U_eiou = Recca::psut_cols$U_eiou,
                                   r_eiou = Recca::psut_cols$r_eiou,
                                   V = Recca::psut_cols$V,
                                   Y = Recca::psut_cols$Y,
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
                                   U_feed_aggregated_colname = paste0(Recca::psut_cols$U_feed, aggregated_suffix),
                                   U_eiou_aggregated_colname = paste0(Recca::psut_cols$U_eiou, aggregated_suffix),
                                   r_eiou_aggregated_colname = paste0(Recca::psut_cols$r_eiou, aggregated_suffix),
                                   V_aggregated_colname = paste0(Recca::psut_cols$V, aggregated_suffix),
                                   Y_aggregated_colname = paste0(Recca::psut_cols$Y, aggregated_suffix),
                                   S_units_aggregated_colname = paste0(Recca::psut_cols$S_units, aggregated_suffix),
                                   # Suffix for aggregated columns
                                   aggregated_suffix = Recca::aggregate_cols$aggregated_suffix) {

  despecify_agg_func <- function(R_mat,
                                 U_mat,
                                 U_feed_mat,
                                 r_eiou_mat,
                                 V_mat,
                                 Y_mat,
                                 U_eiou_mat,
                                 S_units_mat) {
    despecified <- lapply(list(R_mat, U_mat, U_feed_mat, V_mat, Y_mat, S_units_mat), function(m) {
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
    U_feed_out <- despecified[[3]]
    U_eiou_out <- matsbyname::difference_byname(U_out, U_feed_out)
    r_eiou_out <- matsbyname::quotient_byname(U_eiou_out, U_out) %>%
      matsbyname::replaceNaN_byname(val = 0)
    V_out <- despecified[[4]]
    Y_out <- despecified[[5]]
    S_units_out <- matsbyname::quotient_byname(despecified[[6]], despecified[[6]]) %>%
      matsbyname::replaceNaN_byname(val = 0)

    # Make a list and return the matrices
    list(R_out, U_out, U_feed_out, U_eiou_out, r_eiou_out, V_out, Y_out, S_units_out) %>%
      magrittr::set_names(c(R_aggregated_colname,
                            U_aggregated_colname,
                            U_feed_aggregated_colname,
                            U_eiou_aggregated_colname,
                            r_eiou_aggregated_colname,
                            V_aggregated_colname,
                            Y_aggregated_colname,
                            S_units_aggregated_colname))
  }

  matsindf::matsindf_apply(.sut_data,
                           FUN = despecify_agg_func,
                           R_mat = R,
                           U_mat = U,
                           U_feed_mat = U_feed,
                           U_eiou_mat = U_eiou,
                           r_eiou_mat = r_eiou,
                           V_mat = V,
                           Y_mat = Y,
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
#' @param R,U,U_feed,U_eiou,r_eiou,V,Y,S_units Matrices or names of columns in `.sut_data` to be despecified and aggregated. See `Recca::psut_cols`.
#' @param R_aggregated_colname,U_aggregated_colname,U_feed_aggregated_colname,U_eiou_aggregated_colname,r_eiou_aggregated_colname,V_aggregated_colname,Y_aggregated_colname,S_units_aggregated_colname Names of
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
                               U_feed = Recca::psut_cols$U_feed,
                               U_eiou = Recca::psut_cols$U_eiou,
                               r_eiou = Recca::psut_cols$r_eiou,
                               V = Recca::psut_cols$V,
                               Y = Recca::psut_cols$Y,
                               S_units = Recca::psut_cols$S_units,
                               # Names for the aggregated matrices
                               R_aggregated_colname = paste0(Recca::psut_cols$R, aggregated_suffix),
                               U_aggregated_colname = paste0(Recca::psut_cols$U, aggregated_suffix),
                               U_feed_aggregated_colname = paste0(Recca::psut_cols$U_feed, aggregated_suffix),
                               U_eiou_aggregated_colname = paste0(Recca::psut_cols$U_eiou, aggregated_suffix),
                               r_eiou_aggregated_colname = paste0(Recca::psut_cols$r_eiou, aggregated_suffix),
                               V_aggregated_colname = paste0(Recca::psut_cols$V, aggregated_suffix),
                               Y_aggregated_colname = paste0(Recca::psut_cols$Y, aggregated_suffix),
                               S_units_aggregated_colname = paste0(Recca::psut_cols$S_units, aggregated_suffix),
                               # Suffix for aggregated columns
                               aggregated_suffix = Recca::aggregate_cols$aggregated_suffix) {

  group_agg_func <- function(R_mat,
                             U_mat,
                             U_feed_mat,
                             U_eiou_mat,
                             r_eiou_mat,
                             V_mat,
                             Y_mat,
                             S_units_mat) {
    grouped <- lapply(list(R_mat, U_mat, U_feed_mat, V_mat, Y_mat, S_units_mat), function(m) {
      m %>%
        matsbyname::aggregate_byname(aggregation_map = aggregation_map,
                                     margin = margin,
                                     pattern_type = pattern_type)
    })
    R_out <- grouped[[1]]
    U_out <- grouped[[2]]
    U_feed_out <- grouped[[3]]
    V_out <- grouped[[4]]
    Y_out <- grouped[[5]]
    U_eiou_out <- matsbyname::difference_byname(U_out, U_feed_out)
    r_eiou_out <- matsbyname::quotient_byname(U_eiou_out, U_out) %>%
      matsbyname::replaceNaN_byname(val = 0)
    S_units_out <- matsbyname::quotient_byname(grouped[[6]], grouped[[6]]) %>%
      matsbyname::replaceNaN_byname(val = 0)

    # Make a list and return the matrices
    list(R_out, U_out, U_feed_out, U_eiou_out, r_eiou_out, V_out, Y_out, S_units_out) %>%
      magrittr::set_names(c(R_aggregated_colname,
                            U_aggregated_colname,
                            U_feed_aggregated_colname,
                            U_eiou_aggregated_colname,
                            r_eiou_aggregated_colname,
                            V_aggregated_colname,
                            Y_aggregated_colname,
                            S_units_aggregated_colname))
  }

  matsindf::matsindf_apply(.sut_data,
                           FUN = group_agg_func,
                           R_mat = R,
                           U_mat = U,
                           U_feed_mat = U_feed,
                           U_eiou_mat = U_eiou,
                           r_eiou_mat = r_eiou,
                           V_mat = V,
                           Y_mat = Y,
                           S_units_mat = S_units)
}


#' Calculate footprint aggregates
#'
#' Footprint aggregates are isolated measures of primary and final demand energy
#' required to supply a specific amount of final demand energy.
#' This function calculates footprint aggregates for several categories of final demand.
#'
#' By default, footprint aggregates are calculated for each individual
#' product and sector of final demand in the **Y** matrix.
#' This calculation is accomplished for each description of an energy conversion chain (ECC)
#' by the following algorithm:
#'
#' 1. Calculate io matrices with `calc_io_mats()`.
#' 2. Identify each product and sector from rows and columns of the **Y** matrix.
#' 3. For each product and sector independently,
#'    perform an upstream swim with `new_Y()`
#'    to obtain the ECC requirements to supply that product or sector only.
#' 4. Calculate primary and final demand aggregates using `primary_aggregates()` and
#'    `finaldemand_aggregates()`.
#'    Both functions are called with `by = "Total"`,
#'    yielding total primary and final demand aggregates.
#' 5. Add the primary and final demand aggregates as columns at the right side of `.sut_data`
#'    as a nested data frame.
#'
#' Use `unnest` to define how the aggregate data are added to the right side of `.sut_data`.
#'
#' Note that the nested data frame also includes columns for the ECC matrices
#' for each isolated product or sector.
#' The names of the columns in the data frame are taken from the `*_prime_colname` arguments.
#'
#' Footprint aggregation involves "upstream swim" with `new_Y()`.
#' `new_Y()` requires matrix inverse calculations.
#' The `method` argument specifies the method for calculating matrix inverses.
#' The `tol` argument specifies the tolerance for detecting linearities in the matrix.
#' See the documentation at `matsbyname::invert_byname()` for details.
#'
#' Both `tol` and `method` should be a single values and apply to all rows of `.sut_data`.
#'
#' When the **Y** matrix is chopped by rows or columns, the sum of the ECCs
#' created from the chopped rows or columns should equal the original ECC.
#' Internally, this function This function checks internally
#'
#' @param .sut_data A data frame or list of physical supply-use table matrices.
#'                  Default is `NULL`.
#' @param p_industries A vector of names of industries to be aggregated as "primary."
#'                     If `.sut_data` is a data frame, `p_industries` should be the name of a column in the data frame.
#'                     If `.sut_data` is `NULL`, `p_industries` can be a single vector of industry names.
#'                     These industries in `p_industries` will appear in rows of the resource (**R**) and make (**V**) matrices and
#'                     columns of the final demand matrix (**Y**).
#'                     Entries in **Y_p** will be subtracted from entries in **R_p** `+` **V_p** to obtain
#'                     the total primary energy aggregate,
#'                     where `*_p` is the primary part of those matrices.
#'                     The function `find_p_industry_names()` might be helpful to find
#'                     primary industry names if they can be identified by prefixes.
#'                     This argument is passed to `primary_aggregates()`.
#' @param fd_sectors A vector of names of sectors in final demand.
#'                   Names should include columns in the **Y** and **U_EIOU** matrices
#'                   to cover both net (in **Y**) and gross (in **Y** and **U_EIOU**) final demand.
#'                   This argument is passed to `finaldemand_aggregates()`.
#' @param pattern_type One of "exact", "leading", "trailing", or "anywhere" which specifies
#'                     how matches are made for `p_industries`.
#'                     If "exact", exact matches specify the sectors to be aggregated.
#'                     If "leading", sectors are aggregated if any entry in `p_industries` matches the leading part of a final demand sector's name.
#'                     If "trailing", sectors are aggregated if any entry in `p_industries` matches the trailing part of a final demand sector's name.
#'                     If "anywhere", sectors are aggregated if any entry in `p_industries` matches any part of a final demand sector's name.
#'                     Default is "exact".
#'                     This argument is passed to both `primary_aggregates()` and `finaldemand_aggregates()`.
#' @param unnest A boolean that tells whether to unnest the outgoing data.
#'               When `TRUE`, creates a new column called `product_sector` and columns of primary and final demand aggregates.
#'               Default is `FALSE`.
#' @param method One of "solve", "QR", or "SVD". Default is "solve". See details.
#' @param tol_invert The tolerance for detecting linear dependencies in the columns of `a`.
#'                   Default is `.Machine$double.eps`.
#' @param tol_chop_sum The allowable deviation from `0` for the difference between
#'                     the sum of the chopped ECCs and the original ECC.
#'                     Default is `1e-4`.
#' @param R,U,U_feed,V,Y,S_units Matrices that describe the energy conversion chain (ECC).
#'                               See `Recca::psut_cols` for default values.
#' @param footprint_aggregates The name of the output column that contains data frames of footprint aggregates.
#'                             Default is `Recca::psut_cols$footprint_aggregates`.
#' @param product_sector The name of the output column that contains the product, industry, or sector
#'                       for which footprint aggregates are given.
#'                       Default is `Recca::aggregate_cols$product_sector`.
#' @param aggregates_df,aggregate_primary,net_aggregate_demand,gross_aggregate_demand Names of output columns.
#'                                                                                    See `Recca::aggregate_cols`.
#' @param .prime A string that denotes new matrices.
#'               This string is used as a suffix that is appended to
#'               many variable names.
#'               Default is "_prime".
#' @param R_colname,U_colname,U_feed_colname,U_eiou_colname,r_eiou_colname,V_colname,Y_colname Names of input matrices in `.sut_data`. See `Recca::psut_cols` for default values.
#' @param R_prime_colname,U_prime_colname,U_feed_prime_colname,U_eiou_prime_colname,r_eiou_prime_colname,V_prime_colname,Y_prime_colname Names of output matrices in the return value.
#'                                        Default values are constructed from
#'                                        `Recca::psut_cols` values suffixed with
#'                                        the value of the `.prime` argument.
#'
#' @return Primary and final demand (both gross and net) aggregates.
#'
#' @export
#'
#' @examples
#' p_industries <- c("Resources - Crude", "Resources - NG")
#' fd_sectors <- c("Residential", "Transport", "Oil fields")
#' psut_mats <- UKEnergy2000mats %>%
#'   tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)
#' psut_mats %>%
#'   Recca::footprint_aggregates(p_industries = p_industries,
#'                               fd_sectors = fd_sectors)
#' psut_mats %>%
#'   Recca::footprint_aggregates(p_industries = p_industries,
#'                               fd_sectors = fd_sectors,
#'                               unnest = TRUE)
footprint_aggregates <- function(.sut_data = NULL,
                                 p_industries,
                                 fd_sectors,
                                 pattern_type = c("exact", "leading", "trailing", "anywhere"),
                                 unnest = FALSE,
                                 method = c("solve", "QR", "SVD"),
                                 tol_invert = .Machine$double.eps,
                                 tol_chop_sum = 1e-4,
                                 # Input names or matrices
                                 R = Recca::psut_cols$R,
                                 U = Recca::psut_cols$U,
                                 U_feed = Recca::psut_cols$U_feed,
                                 V = Recca::psut_cols$V,
                                 Y = Recca::psut_cols$Y,
                                 S_units = Recca::psut_cols$S_units,
                                 # Output names
                                 aggregates_df = Recca::aggregate_cols$aggregates_df,
                                 product_sector = Recca::aggregate_cols$product_sector,
                                 aggregate_primary = Recca::aggregate_cols$aggregate_primary,
                                 net_aggregate_demand = Recca::aggregate_cols$net_aggregate_demand,
                                 gross_aggregate_demand = Recca::aggregate_cols$gross_aggregate_demand,
                                 # Other internal names
                                 .prime = "_prime",
                                 R_colname = Recca::psut_cols$R,
                                 U_colname = Recca::psut_cols$U,
                                 U_feed_colname = Recca::psut_cols$U_feed,
                                 U_eiou_colname = Recca::psut_cols$U_eiou,
                                 r_eiou_colname = Recca::psut_cols$r_eiou,
                                 V_colname = Recca::psut_cols$V,
                                 Y_colname = Recca::psut_cols$Y,
                                 R_prime_colname = paste0(R_colname, .prime),
                                 U_prime_colname = paste0(U_colname, .prime),
                                 U_feed_prime_colname = paste0(U_feed_colname, .prime),
                                 U_eiou_prime_colname = paste0(U_eiou_colname, .prime),
                                 r_eiou_prime_colname = paste0(r_eiou_colname, .prime),
                                 V_prime_colname = paste0(V_colname, .prime),
                                 Y_prime_colname = paste0(Y_colname, .prime)) {

  pattern_type <- match.arg(pattern_type)
  method <- match.arg(method)

  footprint_func <- function(R_mat, U_mat, U_feed_mat, V_mat, Y_mat, S_units_mat) {
    # At this point, we have single matrices for each of the above variables.
    # Calculate the IO matrices
    with_io <- list(R = R_mat, U = U_mat, U_feed = U_feed_mat, V = V_mat, Y = Y_mat, S_units = S_units_mat) %>%
      # We accept the default vector and matrix names.
      calc_io_mats(method = method, tol = tol_invert)

    # Get the row names in Y. Those are the Products we want to evaluate.
    product_names <- matsbyname::getrownames_byname(Y_mat)
    new_Y_products <- product_names %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_product) {
        # For each product (in each row), make a new Y matrix to be used for the calculation.
        Y_mat %>%
          matsbyname::select_rows_byname(Hmisc::escapeRegex(this_product))
      })

    # Get the column names in Y. Those are the Sectors we want to evaluate.
    sector_names <- matsbyname::getcolnames_byname(Y_mat)
    new_Y_sectors <- sector_names %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_sector) {
        # For each sector (in each column), make a new Y matrix to be used for the calculation.
        Y_mat %>%
          matsbyname::select_cols_byname(Hmisc::escapeRegex(this_sector))
      })

    # Create a list with new Y matrices for all products and sectors
    new_Y_list <- c(new_Y_products, new_Y_sectors)

    # For each item in this list, make a new set of ECC matrices
    ecc_prime <- new_Y_list %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_new_Y) {
        with_io %>%
          append(list(this_new_Y) %>% magrittr::set_names(Y_prime_colname)) %>%
          # Calculate all the new ECC matrices,
          # accepting the default names for intermediate
          # vectors and matrices.
          # We can accept default names for L_ixp, L_pxp, Z, Z_feed, D, and O,
          # because we didn't change those names in the call to calc_io_mats().
          # This gives the new (prime) description of the ECC.
          new_Y(Y_prime = Y_prime_colname,
                R_prime = R_prime_colname,
                U_prime = U_prime_colname,
                U_feed_prime = U_feed_prime_colname,
                U_eiou_prime = U_eiou_prime_colname,
                r_eiou_prime = r_eiou_prime_colname,
                V_prime = V_prime_colname)
    })

    # Verify that energy is balanced.
    # The sum of the ECCs associated with new_Y_products should be equal to the original ECC.
    product_prime_mats <- ecc_prime[product_names] %>%
      purrr::transpose()
    product_prime_balanced <- verify_footprint_effects_aggregate_energy_balance(tol = tol_chop_sum,
                                                                                R_mat = R_mat,
                                                                                U_mat = U_mat,
                                                                                U_feed_mat = U_feed_mat,
                                                                                V_mat = V_mat,
                                                                                Y_mat = Y_mat,
                                                                                R_chop_list = product_prime_mats[[R_prime_colname]],
                                                                                U_chop_list = product_prime_mats[[U_prime_colname]],
                                                                                U_feed_chop_list = product_prime_mats[[U_feed_prime_colname]],
                                                                                V_chop_list = product_prime_mats[[V_prime_colname]],
                                                                                Y_chop_list = product_prime_mats[[Y_prime_colname]])
    assertthat::assert_that(product_prime_balanced, msg = "Products not balanced in footprint_aggregations()")

    # The sum of the ECCs associated with new_Y_sectors should be equal to the original ECC.
    sector_prime_mats <- ecc_prime[sector_names] %>%
      purrr::transpose()
    sector_prime_balanced <- verify_footprint_effects_aggregate_energy_balance(tol = tol_chop_sum,
                                                                               R_mat = R_mat,
                                                                               U_mat = U_mat,
                                                                               U_feed_mat = U_feed_mat,
                                                                               V_mat = V_mat,
                                                                               Y_mat = Y_mat,
                                                                               R_chop_list = sector_prime_mats[[R_prime_colname]],
                                                                               U_chop_list = sector_prime_mats[[U_prime_colname]],
                                                                               U_feed_chop_list = sector_prime_mats[[U_feed_prime_colname]],
                                                                               V_chop_list = sector_prime_mats[[V_prime_colname]],
                                                                               Y_chop_list = sector_prime_mats[[Y_prime_colname]])
    assertthat::assert_that(sector_prime_balanced, msg = "Sectors not balanced in footprint_aggregations()")

    # Calculate primary and final demand aggregates for each of the new ECCs.
    calc_aggregates_from_ecc_prime(ecc_prime,
                                   p_industries = p_industries,
                                   fd_sectors = fd_sectors,
                                   pattern_type = pattern_type,
                                   aggregate_primary = aggregate_primary,
                                   gross_aggregate_demand = gross_aggregate_demand,
                                   net_aggregate_demand = net_aggregate_demand,
                                   aggregates_df = aggregates_df,
                                   product_sector = product_sector,
                                   R_prime_colname = R_prime_colname,
                                   U_prime_colname = U_prime_colname,
                                   U_feed_prime_colname = U_feed_prime_colname,
                                   U_eiou_prime_colname = U_eiou_prime_colname,
                                   r_eiou_prime_colname = r_eiou_prime_colname,
                                   V_prime_colname = V_prime_colname,
                                   Y_prime_colname = Y_prime_colname)
  }

  out <- matsindf::matsindf_apply(.sut_data,
                                  FUN = footprint_func,
                                  R_mat = R,
                                  U_mat = U,
                                  U_feed_mat = U_feed,
                                  V_mat = V,
                                  Y_mat = Y,
                                  S_units_mat = S_units)

  # If .sut_data is a data frame, unnest if desired.
  if (is.data.frame(.sut_data) & unnest) {
    out <- out %>%
      tidyr::unnest(cols = aggregates_df)
  }
  return(out)
}


#' Title
#'
#' @param .sut_data
#' @param p_industries A vector of names of industries to be aggregated as "primary."
#'                     If `.sut_data` is a data frame, `p_industries` should be the name of a column in the data frame.
#'                     If `.sut_data` is `NULL`, `p_industries` can be a single vector of industry names.
#'                     These industries in `p_industries` will appear in rows of the resource (**R**) and make (**V**) matrices and
#'                     columns of the final demand matrix (**Y**).
#'                     Entries in **Y_p** will be subtracted from entries in **R_p** `+` **V_p** to obtain
#'                     the total primary energy aggregate,
#'                     where `*_p` is the primary part of those matrices.
#'                     The function `find_p_industry_names()` might be helpful to find
#'                     primary industry names if they can be identified by prefixes.
#'                     This argument is passed to `primary_aggregates()`.
#' @param fd_sectors A vector of names of sectors in final demand.
#'                   Names should include columns in the **Y** and **U_EIOU** matrices
#'                   to cover both net (in **Y**) and gross (in **Y** and **U_EIOU**) final demand.
#'                   This argument is passed to `finaldemand_aggregates()`.
#' @param pattern_type One of "exact", "leading", "trailing", or "anywhere" which specifies
#'                     how matches are made for `p_industries`.
#'                     If "exact", exact matches specify the sectors to be aggregated.
#'                     If "leading", sectors are aggregated if any entry in `p_industries` matches the leading part of a final demand sector's name.
#'                     If "trailing", sectors are aggregated if any entry in `p_industries` matches the trailing part of a final demand sector's name.
#'                     If "anywhere", sectors are aggregated if any entry in `p_industries` matches any part of a final demand sector's name.
#'                     Default is "exact".
#'                     This argument is passed to both `primary_aggregates()` and `finaldemand_aggregates()`.
#' @param unnest A boolean that tells whether to unnest the outgoing data.
#'               When `TRUE`, creates a new column called `product_sector` and columns of primary and final demand aggregates.
#'               Default is `FALSE`.
#' @param method
#' @param tol_invert
#' @param tol_chop_sum
#' @param R
#' @param U
#' @param U_feed
#' @param V
#' @param Y
#' @param S_units
#' @param footprint_aggregates
#' @param product_sector
#' @param aggregate_primary
#' @param net_aggregate_demand
#' @param gross_aggregate_demand
#' @param .prime
#' @param R_colname
#' @param U_colname
#' @param U_feed_colname
#' @param U_eiou_colname
#' @param r_eiou_colname
#' @param V_colname
#' @param Y_colname
#' @param R_prime_colname
#' @param U_prime_colname
#' @param U_feed_prime_colname
#' @param U_eiou_prime_colname
#' @param r_eiou_prime_colname
#' @param V_prime_colname
#' @param Y_prime_colname
#'
#' @return
#' @export
#'
#' @examples
effects_aggregates <- function(.sut_data = NULL,
                               p_industries,
                               fd_sectors,
                               pattern_type = c("exact", "leading", "trailing", "anywhere"),
                               unnest = FALSE,
                               method = c("solve", "QR", "SVD"),
                               tol_invert = .Machine$double.eps,
                               tol_chop_sum = 1e-4,
                               # Input names or matrices
                               R = Recca::psut_cols$R,
                               U = Recca::psut_cols$U,
                               U_feed = Recca::psut_cols$U_feed,
                               V = Recca::psut_cols$V,
                               Y = Recca::psut_cols$Y,
                               S_units = Recca::psut_cols$S_units,
                               # Output names
                               aggregates_df = Recca::aggregate_cols$aggregates_df,
                               product_sector = Recca::aggregate_cols$product_sector,
                               aggregate_primary = Recca::aggregate_cols$aggregate_primary,
                               net_aggregate_demand = Recca::aggregate_cols$net_aggregate_demand,
                               gross_aggregate_demand = Recca::aggregate_cols$gross_aggregate_demand,
                               # Other internal names
                               .prime = "_prime",
                               R_colname = Recca::psut_cols$R,
                               U_colname = Recca::psut_cols$U,
                               U_feed_colname = Recca::psut_cols$U_feed,
                               U_eiou_colname = Recca::psut_cols$U_eiou,
                               r_eiou_colname = Recca::psut_cols$r_eiou,
                               V_colname = Recca::psut_cols$V,
                               Y_colname = Recca::psut_cols$Y,
                               R_prime_colname = paste0(R_colname, .prime),
                               U_prime_colname = paste0(U_colname, .prime),
                               U_feed_prime_colname = paste0(U_feed_colname, .prime),
                               U_eiou_prime_colname = paste0(U_eiou_colname, .prime),
                               r_eiou_prime_colname = paste0(r_eiou_colname, .prime),
                               V_prime_colname = paste0(V_colname, .prime),
                               Y_prime_colname = paste0(Y_colname, .prime)) {

  effects_func <- function(R_mat, U_mat, U_feed_mat, V_mat, Y_mat, S_units_mat) {

    # Get the column names in R. Those are the Products we want to evaluate.
    product_names <- matsbyname::getcolnames_byname(R_mat)
    new_R_products <- product_names %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_product) {
        # For each product (in each column), make a new Y matrix to be used for the calculation.
        R_mat %>%
          matsbyname::select_cols_byname(Hmisc::escapeRegex(this_product))
      })

    # For each item in this list, make a new set of ECC matrices
    with_qf <- list(R = R_mat, U = U_mat, U_feed = U_feed_mat,
                    V = V_mat, Y = Y_mat, S_units = S_units_mat) %>%
      calc_yqfgW()
    ecc_prime <- new_R_products %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_new_R) {
        with_qf %>%
          append(list(this_new_R) %>% magrittr::set_names(R_prime_colname)) %>%
          # Calculate all the new ECC matrices,
          # giving the new (prime) description of the ECC.
          new_R_ps(R_prime = R_prime_colname,
                   U_prime = U_prime_colname,
                   V_prime = V_prime_colname,
                   Y_prime = Y_prime_colname)
      })

    # Verify that energy is balanced.
    # The sum of the ECCs associated with new_R_products should be equal to the original ECC.
    product_prime_mats <- ecc_prime[product_names] %>%
      purrr::transpose()
    product_prime_balanced <- verify_footprint_effects_aggregate_energy_balance(tol = tol_chop_sum,
                                                                                R_mat = R_mat,
                                                                                U_mat = U_mat,
                                                                                U_feed_mat = U_feed_mat,
                                                                                V_mat = V_mat,
                                                                                Y_mat = Y_mat,
                                                                                R_chop_list = product_prime_mats[[R_prime_colname]],
                                                                                U_chop_list = product_prime_mats[[U_prime_colname]],
                                                                                U_feed_chop_list = product_prime_mats[[U_feed_prime_colname]],
                                                                                V_chop_list = product_prime_mats[[V_prime_colname]],
                                                                                Y_chop_list = product_prime_mats[[Y_prime_colname]])
    assertthat::assert_that(product_prime_balanced, msg = "Products not balanced in footprint_aggregations()")

    # Calculate primary and final demand aggregates for each of the new ECCs.
    calc_aggregates_from_ecc_prime(ecc_prime,
                                   p_industries = p_industries,
                                   fd_sectors = fd_sectors,
                                   pattern_type = pattern_type,
                                   aggregate_primary = aggregate_primary,
                                   gross_aggregate_demand = gross_aggregate_demand,
                                   net_aggregate_demand = net_aggregate_demand,
                                   aggregates_df = aggregates_df,
                                   product_sector = product_sector,
                                   R_prime_colname = R_prime_colname,
                                   U_prime_colname = U_prime_colname,
                                   U_feed_prime_colname = U_feed_prime_colname,
                                   U_eiou_prime_colname = U_eiou_prime_colname,
                                   r_eiou_prime_colname = r_eiou_prime_colname,
                                   V_prime_colname = V_prime_colname,
                                   Y_prime_colname = Y_prime_colname)
  }

  out <- matsindf::matsindf_apply(.sut_data,
                                  FUN = effects_func,
                                  R_mat = R,
                                  U_mat = U,
                                  U_feed_mat = U_feed,
                                  V_mat = V,
                                  Y_mat = Y,
                                  S_units_mat = S_units)

  # If .sut_data is a data frame, unnest if desired.
  if (is.data.frame(.sut_data) & unnest) {
    out <- out %>%
      tidyr::unnest(cols = aggregates_df)
  }
  return(out)
}


#' Calculate aggregates from list of reconstructed ECCs
#'
#' This is a helper function for `footprint_aggregates()` and `effects_aggregates()`.
#' It calculates the primary and final demand aggregates for a list of
#' reconstructed energy conversion chains (ECCs) in `ecc_prime`.
#'
#' @param ecc_prime A list of reconstructed energy conversion chains.
#' @param p_industries A vector of names of industries to be aggregated as "primary."
#'                     See `footprint_aggregates()` for details.
#' @param fd_sectors A vector of names of sectors in final demand.
#'                   See `footprint_aggregates()` for details.
#' @param pattern_type One of "exact", "leading", "trailing", or "anywhere" which specifies
#'                     how matches are made for `p_industries`.
#'                     See `footprint_aggregates()` for details.
#' @param product_sector The name of the output column that contains the product, industry, or sector
#'                       for which footprint aggregates are given.
#' @param aggregates_df,aggregate_primary,net_aggregate_demand,gross_aggregate_demand Names of output columns.
#'                                                                                    See `Recca::aggregate_cols`.
#' @param R_prime_colname,U_prime_colname,U_feed_prime_colname,U_eiou_prime_colname,r_eiou_prime_colname,V_prime_colname,Y_prime_colname Names of output matrices in the return value.
#'                                                                                                                                       Default values are constructed from
#'                                                                                                                                       `Recca::psut_cols` values suffixed with
#'                                                                                                                                       the value of the `.prime` argument.
#'
#' @return A data frame containing reconstructed (prime) matrices and
#'         primary and final demand aggregates in a list suitable for use in `matsindf::matsindf_apply()`.
calc_aggregates_from_ecc_prime <- function(ecc_prime,
                                           p_industries,
                                           fd_sectors,
                                           pattern_type,
                                           product_sector,
                                           aggregates_df,
                                           aggregate_primary,
                                           gross_aggregate_demand,
                                           net_aggregate_demand,
                                           R_prime_colname,
                                           U_prime_colname,
                                           U_feed_prime_colname,
                                           U_eiou_prime_colname,
                                           r_eiou_prime_colname,
                                           V_prime_colname,
                                           Y_prime_colname) {

  # Now that we have the new (prime) ECCs, calculate primary and final demand aggregates
  p_aggregates <- ecc_prime %>%
    sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_new_ecc) {
      this_new_ecc %>%
        primary_aggregates(p_industries = p_industries,
                           R = R_prime_colname,
                           V = V_prime_colname,
                           Y = Y_prime_colname,
                           pattern_type = pattern_type,
                           by = "Total",
                           aggregate_primary = aggregate_primary)
    }) %>%
    # Transpose to pull EX.p to the top level with products and sectors beneath.
    purrr::transpose()
  fd_aggregates <- ecc_prime %>%
    sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_new_ecc) {
      this_new_ecc %>%
        finaldemand_aggregates(fd_sectors = fd_sectors,
                               U = U_prime_colname,
                               U_feed = U_feed_prime_colname,
                               Y = Y_prime_colname,
                               pattern_type = pattern_type,
                               by = "Total",
                               net_aggregate_demand = net_aggregate_demand,
                               gross_aggregate_demand = gross_aggregate_demand)
    }) %>%
    # Transpose to pull EX.fd_net and EX.fd_gross to the top level with products and sectors beneath.
    purrr::transpose()

  # Create data frames that can be later unnested if needed.
  p_aggregates_df <- tibble::tibble(
    "{product_sector}" := p_aggregates[[aggregate_primary]] %>% names(),
    "{aggregate_primary}" := p_aggregates[[aggregate_primary]] %>% unname() %>% unlist()
  )
  net_fd_aggregates_df <- tibble::tibble(
    "{product_sector}" := fd_aggregates[[net_aggregate_demand]] %>% names(),
    "{net_aggregate_demand}" := fd_aggregates[[net_aggregate_demand]] %>% unname() %>% unlist()
  )
  gross_fd_aggregates_df <- tibble::tibble(
    "{product_sector}" := fd_aggregates[[gross_aggregate_demand]] %>% names(),
    "{gross_aggregate_demand}" := fd_aggregates[[gross_aggregate_demand]] %>% unname() %>% unlist()
  )

  # Join the data frames by the product_sector column.
  primary_net_gross <- p_aggregates_df %>%
    dplyr::full_join(gross_fd_aggregates_df, by = product_sector) %>%
    dplyr::full_join(net_fd_aggregates_df, by = product_sector)

  # Add the "prime" ECC matrices to the nested data frame
  ecc_prime_transpose <- purrr::transpose(ecc_prime)
  ecc_primes <- primary_net_gross[product_sector] %>%
    dplyr::mutate(
      "{R_prime_colname}" := ecc_prime_transpose[[R_prime_colname]],
      "{U_prime_colname}" := ecc_prime_transpose[[U_prime_colname]],
      "{U_feed_prime_colname}" := ecc_prime_transpose[[U_feed_prime_colname]],
      "{U_eiou_prime_colname}" := ecc_prime_transpose[[U_eiou_prime_colname]],
      "{r_eiou_prime_colname}" := ecc_prime_transpose[[r_eiou_prime_colname]],
      "{V_prime_colname}" := ecc_prime_transpose[[V_prime_colname]],
      "{Y_prime_colname}" := ecc_prime_transpose[[Y_prime_colname]]
    )

  primary_net_gross <- dplyr::full_join(ecc_primes, primary_net_gross, by = product_sector)

  # Make a list and return it so that the data frame is nested
  # inside the column of the data frame.
  list(primary_net_gross) %>%
    magrittr::set_names(aggregates_df)
}


#' Verify energy balance after footprint calculations
#'
#' Footprint calculations involve
#' isolating rows or columns of the **Y** matrix (chopping),
#' performing upstream swims (with `new_Y()`), and
#' creating the ECC portions that support the creation of the row or column of **Y**.
#' After performing that upstream swim, the sum of the
#' isolated (chopped) ECCs should equal the original ECC.
#' This function performs that energy balance verification.
#'
#' The various `*_chop_list` arguments should be lists of matrices
#' formed by isolating (chopping) different parts of **Y**.
#' The matrices in `R_chop_list`, `U_chop_list`, `U_feed_chop_list`
#' `U_eiou_chop_list`, `V_chop_list`, and `Y_chop_list` should sum to
#' `R`, `U`, `U_feed`, `U_eiou`, `V`, and `Y`, respectively.
#'
#' This is not a public function.
#' It is an internal helper function
#' for `footprint_aggregates()`.
#'
#' @param .sut_data An optional data frame of energy conversion chain matrices.
#' @param tol The tolerance within which energy balance is assumed to be OK. Default is `1e-4`.
#' @param R_mat,U_mat,U_feed_mat,V_mat,Y_mat The matrices of the original ECC.
#' @param R_chop_list,U_chop_list,U_feed_chop_list,V_chop_list,Y_chop_list Lists of matrices from different upstream swims corresponding to different rows or columns of **Y**.
#'
#' @return `TRUE` if energy balance is observed, `FALSE` otherwise.
verify_footprint_effects_aggregate_energy_balance <- function(.sut_data = NULL,
                                                              tol = 1e-4,
                                                              R_mat, U_mat, U_feed_mat, V_mat, Y_mat,
                                                              R_chop_list, U_chop_list, U_feed_chop_list, V_chop_list, Y_chop_list) {

  verify_func <- function(chop_list, mat) {
    mat_sum <- matsbyname::sum_byname(chop_list, .summarise = TRUE)[[1]]
    err <- matsbyname::difference_byname(mat_sum, mat)
    matsbyname::iszero_byname(err, tol = tol)
  }

  # Build lists of matrices
  chop_list <- list(R_chop_list, U_chop_list, U_feed_chop_list, V_chop_list, Y_chop_list)
  mat_list <- list(R_mat, U_mat, U_feed_mat, V_mat, Y_mat)
  # Map across each list to ensure the chop_list sums to the matrix.
  Map(f = verify_func, chop_list, mat_list) %>%
    unlist() %>%
    all()
}
