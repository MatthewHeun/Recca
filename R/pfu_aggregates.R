#' Aggregate to primary, final, and useful stages
#'
#' Calculate aggregates at all possible ECC stages
#' (primary, final, and useful),
#' regardless of whether the last stage is final or useful.
#' See details for the approach.
#' Note that services aggregations are often inherently problematic,
#' because energy services are often quantified in different units.
#' This functions does not perform
#' aggregation to services.
#' Users are referred to [finaldemand_aggregates()]
#' for aggregation to services, if desired and sensible.
#'
#' There are several ways to aggregate energy conversion chain (ECC) data
#' to primary, final, or useful stages of the ECC.
#' [primary_aggregates()] aggregates primary energy using the
#' resource (**R**), make (**V**), and final demand (**Y**) matrices.
#' [primary_aggregates()] gives upstream (source) aggregations for an ECC.
#' [finaldemand_aggregates()] aggregates to the last stage
#' of an energy conversion chain using **R** and **Y** matrices,
#' regardless of whether the last stage is final, useful, or services.
#' [finaldemand_aggregates()] gives downstream (sink) aggregations.
#'
#' However, applying [finaldemand_aggregates()] to an ECC
#' whose last stage is final cannot produce useful stage aggregations.
#' Similarly, applying [finaldemand_aggregates()] to an ECC whose last stage is useful
#' cannot provide final stage aggregations.
#' See the following table.
#'
#' | ECC last stage -->        | Final                         | Useful                        |
#' | :------------------------ | :---------------------------- | :---------------------------- |
#' | Desired aggregation stage |                               |                               |
#' | Primary                   | [primary_aggregates()] Note A | [primary_aggregates()] Note A |
#' | Final                     | [finaldemand_aggregates()]    | Note B                        |
#' | Useful                    | Note C                        | [finaldemand_aggregates()]    |
#'
#' For the off-axis aggregations, special considerations are employed in this function.
#'
#' Note A:
#'
#' The two results from [primary_aggregates()]
#' should be equal to within `tol`.
#' If agreement is not observed, an error is given.
#'
#' Note B:
#'
#' When last stage is useful but we want final stage aggregations
#' and final-to-useful stage efficiencies,
#' we can again employ [calc_eta_fu_Y_eiou()] in an inverse
#' relationship to calculate final stage aggregates when
#' useful stage information is known.
#' The result is **Y** and **U_EIOU** matrices
#' of same structure as **Y_Useful** and **U_EIOU_Useful** but containing
#' final stage data.
#' These final-but-in-same-structure-as-useful matrices
#' can be used to calculate final aggregations when last stage is useful.
#'
#' Note C:
#'
#' When last stage is final but we want useful energy aggregates
#' and final-to-useful efficiencies,
#' we can employ [calc_eta_fu_Y_eiou()] to calculate
#' useful energy for each piece of final demand or EIOU
#' when last stage is final, giving **Y** and **U_EIOU**
#' matrices with same structure as **Y_Final** and **U_EIOU_Final**
#' except containing useful energy data.
#' These useful-but-in-same-structure-as-final matrices
#' can be used to calculate useful aggregations when last stage is final.
#'
#' Whereas [primary_aggregates()] and [finaldemand_aggregates()]
#' work independently of the last stage of an ECC,
#' this function requires both final and useful last stage ECCs
#' to be present in `.sutdata`.
#' Any data with last state of services are ignored in this function.
#'
#' Suffixes to matrix names are assumed to indicate the last stage
#' of the ECC for which the matrix applies.
#' For example, two versions of the **R** matrix should be present:
#' `R_final` and `R_useful`.
#'
#' If `.sutdata` is a wide-by-matrices data frame
#' but contains a `last_stage` column,
#' `.sutdata` is pivoted wide (as a convenience)
#' to put data into the correct shape,
#' forming columns for each combination of ECC matrix and last stage.
#' The `last_stage` and `R` columns make `R_final` and `R_useful` columns.
#' The `last_stage` and `V` columns make `V_final` and `V_useful` columns.
#' Etc.
#' If either the last stage final or the last stage useful ECC representations
#' is missing, an error is thrown.
#' See examples.
#'
#' Internally, this function uses
#' [primary_aggregates()] and [finaldemand_aggregates()], and
#' to complete its work.
#'
#' Primary aggregates can be computed when
#' last stage is final, useful, or services.
#' Ostensibly, the primary aggregates should be the same
#' in all cases when metadata are the same.
#' An error is thrown if that is not true to within `tol`.
#'
#' @param .sutdata An optional data frame containing physical supply use table
#'                 descriptions of energy conversion chains.
#' @param p_industries A string vector of primary industries.
#' @param fd_sectors  A string vector of final demand sectors.
#' @param by Tells how to aggregate, one of "Total", "Product", "Industry", or "Flow".
#'           Default is "Total".
#' @param add_net_gross_cols A boolean that tells whether to include net and gross columns
#'                           for primary energy aggregation.
#'                           Default is `FALSE`.
#' @param piece Tells which piece of row and column labels to use for
#'              aggregation decision.
#'              Default is "all".
#' @param notation Tells which notation is used for row and column labels.
#'                 Default is `RCLabels::notations_list`.
#' @param pattern_type Tells how to match row and column names.
#'                     One of "exact", "leading", "trailing", "anywhere", or "literal".
#'                     Default is "exact".
#' @param prepositions The list of prepositions for row and column labels.
#'                     Default is `RCLabels::prepositions_list`.
#' @param net_aggregate_primary,gross_aggregate_primary,net_aggregate_final,gross_aggregate_final,net_aggregate_useful,gross_aggregate_useful,net_aggregate_services,gross_aggregate_services See `Recca::aggregate_cols`.
#' @param last_stage Name of the last stage column. Default is `Recca::psut_cols$last_stage`.
#' @param primary,final,useful,services String identifiers for ECC stages. See `Recca::all_stages`.
#' @param sep The string separator identifying the last stage in the ECC.
#'            Default is `Recca::all_stages$last_stage_sep`.
#' @param R,U,U_feed,U_eiou,r_eiou,V,Y,S_units Names for columns containing matrices.
#'                                             See `Recca::psut_cols`.
#' @param R_lsfinal,U_lsfinal,U_feed_lsfinal,U_eiou_lsfinal,r_eiou_lsfinal,V_lsfinal,Y_lsfinal,S_units_lsfinal Names for columns when last stage is final energy.
#'                                                                                                             Defaults are unmodified column names concatenated with `sep`
#'                                                                                                             and `final`.
#' @param R_lsuseful,U_lsuseful,U_feed_lsuseful,U_eiou_lsuseful,r_eiou_lsuseful,V_lsuseful,Y_lsuseful,S_units_lsuseful Names for columns when last stage is useful energy.
#'                                                                                                             Defaults are unmodified column names concatenated with `sep`
#'                                                                                                             and `useful`.
#' @param R_lsservices,U_lsservices,U_feed_lsservices,U_eiou_lsservices,r_eiou_lsservices,V_lsservices,Y_lsservices,S_units_lsservices Names for columns when last stage is energy services.
#'                                                                                                             Defaults are unmodified column names concatenated with `sep`
#'                                                                                                             and `services`.
#' @param .matnames,.matvals Names of columns used internally.
#'                           Defaults are from `Recca::psut_cols`.
#' @param tol The allowable energy imbalance in the units of energy flows.
#'            Default is `1e-6`.
#'
#' @return A data frame of primary, final, and useful aggregates.
#'
#' @export
#'
#' @examples
#' p_industries <- c("Resources [of Crude]", "Resources [of NG]")
#' fd_sectors <- c("Residential", "Transport", "Oil fields")
#' # Primary TOTAL aggregates
#' UKEnergy2000mats |>
#'   tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
#'   # Eliminate the case when last_stage == "Final"
#'   dplyr::filter(.data[[Recca::psut_cols$last_stage]] != "Final") |>
#'   pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
#'                 by = "Total")
pfu_aggregates <- function(.sutdata,
                           # Vector of primary industries
                           p_industries,
                           # Vector of final demand sectors
                           fd_sectors,
                           by = c("Total", "Product", "Industry", "Flow"),
                           add_net_gross_cols = FALSE,
                           piece = "all",
                           notation = RCLabels::notations_list,
                           pattern_type = c("exact", "leading", "trailing", "anywhere", "literal"),
                           prepositions = RCLabels::prepositions_list,
                           # Output names
                           net_aggregate_primary = Recca::aggregate_cols$net_aggregate_primary,
                           gross_aggregate_primary = Recca::aggregate_cols$gross_aggregate_primary,

                           net_aggregate_final = Recca::aggregate_cols$net_aggregate_final,
                           gross_aggregate_final = Recca::aggregate_cols$gross_aggregate_final,

                           net_aggregate_useful = Recca::aggregate_cols$net_aggregate_useful,
                           gross_aggregate_useful = Recca::aggregate_cols$gross_aggregate_useful,

                           net_aggregate_services = Recca::aggregate_cols$net_aggregate_services,
                           gross_aggregate_services = Recca::aggregate_cols$gross_aggregate_services,

                           # Stages
                           last_stage = Recca::psut_cols$last_stage,
                           primary = Recca::all_stages$primary,
                           final = Recca::all_stages$final,
                           useful = Recca::all_stages$useful,
                           services = Recca::all_stages$services,
                           sep = Recca::all_stages$last_stage_sep,
                           # Regular matrix names for wide by matrix data frames
                           R = Recca::psut_cols$R,
                           U = Recca::psut_cols$U,
                           U_feed = Recca::psut_cols$U_feed,
                           U_eiou = Recca::psut_cols$U_eiou,
                           r_eiou = Recca::psut_cols$r_eiou,
                           V = Recca::psut_cols$V,
                           Y = Recca::psut_cols$Y,
                           S_units = Recca::psut_cols$S_units,
                           # Last stage final matrix names
                           R_lsfinal = paste0(Recca::psut_cols$R, sep, final),
                           U_lsfinal = paste0(Recca::psut_cols$U, sep, final),
                           U_feed_lsfinal = paste0(Recca::psut_cols$U_feed, sep, final),
                           U_eiou_lsfinal = paste0(Recca::psut_cols$U_eiou, sep, final),
                           r_eiou_lsfinal = paste0(Recca::psut_cols$r_eiou, sep, final),
                           V_lsfinal = paste0(Recca::psut_cols$V, sep, final),
                           Y_lsfinal = paste0(Recca::psut_cols$Y, sep, final),
                           S_units_lsfinal = paste0(Recca::psut_cols$S_units, sep, final),
                           # Last stage useful matrix names
                           R_lsuseful = paste0(Recca::psut_cols$R, sep, useful),
                           U_lsuseful = paste0(Recca::psut_cols$U, sep, useful),
                           U_feed_lsuseful = paste0(Recca::psut_cols$U_feed, sep, useful),
                           U_eiou_lsuseful = paste0(Recca::psut_cols$U_eiou, sep, useful),
                           r_eiou_lsuseful = paste0(Recca::psut_cols$r_eiou, sep, useful),
                           V_lsuseful = paste0(Recca::psut_cols$V, sep, useful),
                           Y_lsuseful = paste0(Recca::psut_cols$Y, sep, useful),
                           S_units_lsuseful = paste0(Recca::psut_cols$S_units, sep, useful),
                           # Last stage services matrix names
                           R_lsservices = paste0(Recca::psut_cols$R, sep, services),
                           U_lsservices = paste0(Recca::psut_cols$U, sep, services),
                           U_feed_lsservices = paste0(Recca::psut_cols$U_feed, sep, services),
                           U_eiou_lsservices = paste0(Recca::psut_cols$U_eiou, sep, services),
                           r_eiou_lsservices = paste0(Recca::psut_cols$r_eiou, sep, services),
                           V_lsservices = paste0(Recca::psut_cols$V, sep, services),
                           Y_lsservices = paste0(Recca::psut_cols$Y, sep, services),
                           S_units_lsservices = paste0(Recca::psut_cols$S_units, sep, services),
                           # Names of internally pivoted columns
                           .matnames = Recca::psut_cols$matnames,
                           .matvals = Recca::psut_cols$matvals,
                           tol = 1e-6) {

  by <- match.arg(by)

  # If .sutdata is a data frame and it contains a last_stage column,
  # pivot wider before calling pfu_agg_func().
  if (is.data.frame(.sutdata) & (last_stage %in% names(.sutdata))) {
    need_to_pivot <- TRUE
    .sutdata <- .sutdata |>
      tidyr::pivot_longer(cols = dplyr::any_of(c(R, U, U_feed, U_eiou, r_eiou, V, Y, S_units)),
                          names_to = .matnames,
                          values_to = .matvals) |>
      dplyr::mutate(
        "{.matnames}" := paste0(.data[[.matnames]], sep, .data[[last_stage]]),
        "{last_stage}" := NULL
      ) |>
      tidyr::pivot_wider(names_from = tidyr::all_of(.matnames),
                         values_from = tidyr::all_of(.matvals)) |>
      # Change order of the columns to make sense
      dplyr::relocate(dplyr::any_of(c(R, U, U_feed, U_eiou, r_eiou, V, Y, S_units,
                                      R_lsfinal, U_lsfinal, U_feed_lsfinal, U_eiou_lsfinal, r_eiou_lsfinal, V_lsfinal, Y_lsfinal, S_units_lsfinal,
                                      R_lsuseful, U_lsuseful, U_feed_lsuseful, U_eiou_lsuseful, r_eiou_lsuseful, V_lsuseful, Y_lsuseful, S_units_lsuseful,
                                      R_lsservices, U_lsservices, U_feed_lsservices, U_eiou_lsservices, r_eiou_lsservices, V_lsservices, Y_lsservices, S_units_lsservices)),
                      .after = dplyr::last_col())
  }

  # Make names for outgoing aggregates
  # There are a lot of names here, because this is a complex function
  # Net and gross primary aggregates for all last stage possibilities
  net_aggregate_primary_lsfinal <- paste0(net_aggregate_primary, sep, final)
  gross_aggregate_primary_lsfinal <- paste0(gross_aggregate_primary, sep, final)
  net_aggregate_primary_lsuseful <- paste0(net_aggregate_primary, sep, useful)
  gross_aggregate_primary_lsuseful <- paste0(gross_aggregate_primary, sep, useful)
  net_aggregate_primary_lsservices <- paste0(net_aggregate_primary, sep, services)
  gross_aggregate_primary_lsservices <- paste0(gross_aggregate_primary, sep, services)
  # Net and gross final aggregates for all last stage possibilities
  net_aggregate_final_lsfinal <- paste0(net_aggregate_final, sep, final)
  gross_aggregate_final_lsfinal <- paste0(gross_aggregate_final, sep, final)
  net_aggregate_final_lsuseful <- paste0(net_aggregate_final, sep, useful)
  gross_aggregate_final_lsuseful <- paste0(gross_aggregate_final, sep, useful)
  net_aggregate_final_lsservices <- paste0(net_aggregate_final, sep, services)
  gross_aggregate_final_lsservices <- paste0(gross_aggregate_final, sep, services)
  # Net and gross useful aggregates for all last stage possibilities
  net_aggregate_useful_lsfinal <- paste0(net_aggregate_useful, sep, final)
  gross_aggregate_useful_lsfinal <- paste0(gross_aggregate_useful, sep, final)
  net_aggregate_useful_lsuseful <- paste0(net_aggregate_useful, sep, useful)
  gross_aggregate_useful_lsuseful <- paste0(gross_aggregate_useful, sep, useful)
  net_aggregate_useful_lsservices <- paste0(net_aggregate_useful, sep, services)
  gross_aggregate_useful_lsservices <- paste0(gross_aggregate_useful, sep, services)
  # Net and gross services aggregates for all last stage possibilities
  net_aggregate_services_lsfinal <- paste0(net_aggregate_services, sep, final)
  gross_aggregate_services_lsfinal <- paste0(gross_aggregate_services, sep, final)
  net_aggregate_services_lsuseful <- paste0(net_aggregate_services, sep, useful)
  gross_aggregate_services_lsuseful <- paste0(gross_aggregate_services, sep, useful)
  net_aggregate_services_lsservices <- paste0(net_aggregate_services, sep, services)
  gross_aggregate_services_lsservices <- paste0(gross_aggregate_services, sep, services)

  pfuagg_func <- function(R_final_mat = NULL, R_useful_mat = NULL, R_services_mat = NULL,
                          U_final_mat = NULL, U_useful_mat = NULL, U_services_mat = NULL,
                          U_feed_final_mat = NULL, U_feed_useful_mat = NULL, U_feed_services_mat = NULL,
                          U_eiou_final_mat = NULL, U_eiou_useful_mat = NULL, U_eiou_services_mat = NULL,
                          r_eiou_final_mat = NULL, r_eiou_useful_mat = NULL, r_eiou_services_mat = NULL,
                          V_final_mat = NULL, V_useful_mat = NULL, V_services_mat = NULL,
                          Y_final_mat = NULL, Y_useful_mat = NULL, Y_services_mat = NULL,
                          S_units_final_mat = NULL, S_units_useful_mat = NULL, S_units_services_mat = NULL) {

    # Calculate primary-stage aggregates
    out_primary <- calc_primary_aggs_helper(R_final_mat = R_final_mat, V_final_mat = V_final_mat, Y_final_mat = Y_final_mat,
                                            R_useful_mat = R_useful_mat, V_useful_mat = V_useful_mat, Y_useful_mat = Y_useful_mat,
                                            R_services_mat = R_services_mat, V_services_mat = V_services_mat, Y_services_mat = Y_services_mat,
                                            p_industries = p_industries, by = by,
                                            add_net_gross_cols = add_net_gross_cols,
                                            piece = piece, notation = notation, pattern_type = pattern_type, prepositions = prepositions,
                                            net_aggregate_primary_lsfinal = net_aggregate_primary_lsfinal,
                                            gross_aggregate_primary_lsfinal = gross_aggregate_primary_lsfinal,
                                            net_aggregate_primary_lsuseful = net_aggregate_primary_lsuseful,
                                            gross_aggregate_primary_lsuseful = gross_aggregate_primary_lsuseful,
                                            net_aggregate_primary_lsservices = net_aggregate_primary_lsservices,
                                            gross_aggregate_primary_lsservices = gross_aggregate_primary_lsservices,
                                            tol = tol)

    # Calculate final-stage aggregates
    out_final <- calc_final_aggs_helper(U_eiou_final_mat = U_eiou_final_mat, Y_final_mat = Y_final_mat,
                                        U_eiou_useful_mat = U_eiou_useful_mat, Y_useful_mat = Y_useful_mat,
                                        U_eiou_services_mat = U_eiou_services_mat, Y_services_mat = Y_services_mat,
                                        fd_sectors = fd_sectors, by = by,
                                        piece = piece, notation = notation, pattern_type = pattern_type, prepositions = prepositions,
                                        net_aggregate_final_lsfinal = net_aggregate_final_lsfinal,
                                        gross_aggregate_final_lsfinal = gross_aggregate_final_lsfinal,
                                        net_aggregate_final_lsuseful = net_aggregate_final_lsuseful,
                                        gross_aggregate_final_lsuseful = gross_aggregate_final_lsuseful,
                                        net_aggregate_final_lsservices = net_aggregate_final_lsservices,
                                        gross_aggregate_final_lsservices = gross_aggregate_final_lsservices)

    # Calculate useful-stage aggregates
    out_useful <- calc_useful_aggs_helper(U_eiou_final_mat, Y_final_mat,
                                          U_eiou_useful_mat, Y_useful_mat,
                                          U_eiou_services_mat, Y_services_mat,
                                          fd_sectors,
                                          piece, notation, pattern_type, prepositions,
                                          by,
                                          net_aggregate_useful_lsfinal = net_aggregate_useful_lsfinal,
                                          gross_aggregate_useful_lsfinal = gross_aggregate_useful_lsfinal,
                                          net_aggregate_useful_lsuseful = net_aggregate_useful_lsuseful,
                                          gross_aggregate_useful_lsuseful = gross_aggregate_useful_lsuseful,
                                          net_aggregate_useful_lsservices = net_aggregate_useful_lsservices,
                                          gross_aggregate_useful_lsservices = gross_aggregate_useful_lsservices)

    # Calculate services-stage aggregates
    out_services <- calc_services_aggs_helper(U_eiou_final_mat, Y_final_mat,
                                              U_eiou_useful_mat, Y_useful_mat,
                                              U_eiou_services_mat, Y_services_mat,
                                              fd_sectors,
                                              piece, notation, pattern_type, prepositions,
                                              by,
                                              net_aggregate_services_lsfinal = net_aggregate_services_lsfinal,
                                              gross_aggregate_services_lsfinal = gross_aggregate_services_lsfinal,
                                              net_aggregate_services_lsuseful = net_aggregate_services_lsuseful,
                                              gross_aggregate_services_lsuseful = gross_aggregate_services_lsuseful,
                                              net_aggregate_services_lsservices = net_aggregate_services_lsservices,
                                              gross_aggregate_services_lsservices = gross_aggregate_services_lsservices)

    c(out_primary, out_final, out_useful, out_services)
  }

  # result <- matsindf::matsindf_apply(.sutdata, FUN = pfuagg_func,
  matsindf::matsindf_apply(.sutdata, FUN = pfuagg_func,
                           R_final_mat = R_lsfinal, R_useful_mat = R_lsuseful, R_services_mat = R_lsservices,
                           U_final_mat = U_lsfinal, U_useful_mat = U_lsuseful, U_services_mat = U_lsservices,
                           U_feed_final_mat = U_feed_lsfinal, U_feed_useful_mat = U_feed_lsuseful, U_feed_services_mat = U_feed_lsservices,
                           U_eiou_final_mat = U_eiou_lsfinal, U_eiou_useful_mat = U_eiou_lsuseful, U_eiou_services_mat = U_eiou_lsservices,
                           r_eiou_final_mat = r_eiou_lsfinal, r_eiou_useful_mat = r_eiou_lsuseful, r_eiou_services_mat = r_eiou_lsservices,
                           V_final_mat = V_lsfinal, V_useful_mat = V_lsuseful, V_services_mat = V_lsservices,
                           Y_final_mat = Y_lsfinal, Y_useful_mat = Y_lsuseful, Y_services_mat = Y_lsservices,
                           S_units_final_mat = S_units_lsfinal, S_units_useful_mat = S_units_lsuseful, S_units_services_mat = S_units_lsservices)

  # If the incoming .sutmats was a wide by matrices data frame,
  # pivot the output.
  # if (need_to_pivot) {
  #   result <- result |>
  #     tidyr::pivot_longer(cols = dplyr::any_of(c(R_final, U_final, U_feed_final, U_eiou_final, r_eiou_final, V_final, Y_final, S_units_final,
  #                                                R_useful, U_useful, U_feed_useful, U_eiou_useful, r_eiou_useful, V_useful, Y_useful, S_units_useful,
  #                                                R_services, U_services, U_feed_services, U_eiou_services, r_eiou_services, V_services, Y_services, S_units_services)),
  #                         names_to = .matnames, values_to = .matvals) |>
  #     # Eliminate rows that do not have matrices
  #     dplyr::filter(!sapply(.data[[.matvals]], is.null)) |>
  #     tidyr::separate(col = dplyr::all_of(.matnames), into = c(.matnames, last_stage), sep = sep, remove = TRUE) |>
  #     tidyr::pivot_wider(names_from = dplyr::all_of(.matnames), values_from = dplyr::all_of(.matvals)) |>
  #     dplyr::relocate(dplyr::any_of(c(net_aggregate_primary_lsfinal, gross_aggregate_primary_lsfinal,
  #                                     net_aggregate_primary_lsuseful, gross_aggregate_primary_lsuseful,
  #                                     net_aggregate_primary_lsservices, gross_aggregate_primary_lsservices,
  #                                     net_aggregate_final_lsfinal, gross_aggregate_final_lsfinal,
  #                                     net_aggregate_final_lsuseful, gross_aggregate_final_lsuseful,
  #                                     net_aggregate_final_lsservices, gross_aggregate_final_lsservices,
  #                                     net_aggregate_useful_lsfinal, gross_aggregate_useful_lsfinal,
  #                                     net_aggregate_useful_lsuseful, gross_aggregate_useful_lsuseful,
  #                                     net_aggregate_useful_lsservices, gross_aggregate_useful_lsservices,
  #                                     net_aggregate_services_lsfinal, gross_aggregate_services_lsfinal,
  #                                     net_aggregate_services_lsuseful, gross_aggregate_services_lsuseful,
  #                                     net_aggregate_services_lsservices, gross_aggregate_services_lsservices)),
  #                     .after = dplyr::last_col())
  # }
  # return(result)
}



calc_primary_aggs_helper <- function(R_final_mat, V_final_mat, Y_final_mat,
                                     R_useful_mat, V_useful_mat, Y_useful_mat,
                                     R_services_mat, V_services_mat, Y_services_mat,
                                     p_industries, by,
                                     add_net_gross_cols,
                                     piece, notation, pattern_type, prepositions,
                                     net_aggregate_primary_lsfinal = net_aggregate_primary_lsfinal,
                                     gross_aggregate_primary_lsfinal = gross_aggregate_primary_lsfinal,
                                     net_aggregate_primary_lsuseful = net_aggregate_primary_lsuseful,
                                     gross_aggregate_primary_lsuseful = gross_aggregate_primary_lsuseful,
                                     net_aggregate_primary_lsservices = net_aggregate_primary_lsservices,
                                     gross_aggregate_primary_lsservices = gross_aggregate_primary_lsservices,
                                     tol) {

  # Calculate primary aggregates from 3 last stages (final, useful, and services) when each is available.
  # When available, calculate primary aggregates when last_stage is final.
  if (!is.null(R_final_mat) & !is.null(V_final_mat) & !is.null(Y_final_mat)) {
    ex_p_lsfinal <- primary_aggregates(R = R_final_mat, V = V_final_mat, Y = Y_final_mat,
                                       p_industries = p_industries,
                                       by = by,
                                       add_net_gross_cols = TRUE,
                                       piece = piece, notation = notation, pattern_type = pattern_type, prepositions = prepositions,
                                       net_aggregate_primary = net_aggregate_primary_lsfinal,
                                       gross_aggregate_primary = gross_aggregate_primary_lsfinal)
  } else {
    ex_p_lsfinal <- list(NULL, NULL) |>
      magrittr::set_names(c(net_aggregate_primary_lsfinal, gross_aggregate_primary_lsfinal))
  }
  # When available, calculate primary aggregates when last_stage is useful.
  if (!is.null(R_useful_mat) & !is.null(V_useful_mat) & !is.null(Y_useful_mat)) {
    ex_p_lsuseful <- primary_aggregates(R = R_useful_mat, V = V_useful_mat, Y = Y_useful_mat,
                                        p_industries = p_industries,
                                        by = by,
                                        add_net_gross_cols = TRUE,
                                        piece = piece, notation = notation, pattern_type = pattern_type, prepositions = prepositions,
                                        net_aggregate_primary = net_aggregate_primary_lsuseful,
                                        gross_aggregate_primary = gross_aggregate_primary_lsuseful)
  } else {
    ex_p_lsuseful <- list(NULL, NULL) |>
      magrittr::set_names(c(net_aggregate_primary_lsuseful, gross_aggregate_primary_lsuseful))
  }
  # When available, calculate primary aggregates when last_stage is services.
  if (!is.null(R_services_mat) & !is.null(V_services_mat) & !is.null(Y_services_mat)) {
    ex_p_lsservices <- primary_aggregates(R = R_services_mat, V = V_services_mat, Y = Y_services_mat,
                                          p_industries = p_industries,
                                          by = by,
                                          add_net_gross_cols = TRUE,
                                          piece = piece, notation = notation, pattern_type = pattern_type, prepositions = prepositions,
                                          net_aggregate_primary = net_aggregate_primary_lsservices,
                                          gross_aggregate_primary = gross_aggregate_primary_lsservices)
  } else {
    ex_p_lsservices <- list(NULL, NULL) |>
      magrittr::set_names(c(net_aggregate_primary_lsservices, gross_aggregate_primary_lsservices))
  }
  # Ensure that final and useful versions of primary aggregates are same.
  # If they are not same, we probably don't have the same ECCs, and an error should be thrown.
  if (!is.null(R_final_mat) & !is.null(V_final_mat) & !is.null(Y_final_mat) &
      !is.null(R_useful_mat) & !is.null(V_useful_mat) & !is.null(Y_useful_mat)) {
    # Test gross aggregate primary
    matsbyname::difference_byname(ex_p_lsfinal[[gross_aggregate_primary_lsfinal]], ex_p_lsuseful[[gross_aggregate_primary_lsuseful]]) |>
      matsbyname::iszero_byname(tol = tol) |>
      assertthat::assert_that(msg = "Gross primary aggregates for last_stage = 'Final' and last_stage = 'Useful' are not same to within `tol` in Recca::pfu_aggregates().")
    # Test net aggregate primary
    matsbyname::difference_byname(ex_p_lsfinal[[net_aggregate_primary_lsfinal]], ex_p_lsuseful[[net_aggregate_primary_lsuseful]]) |>
      matsbyname::iszero_byname(tol = tol) |>
      assertthat::assert_that(msg = "Net primary aggregates for last_stage = 'Final' and last_stage = 'Useful' are not same to within `tol` in Recca::pfu_aggregates().")
  }
  # Ensure that final and services versions of primary aggregates are same.
  # If they are not same, we probably don't have the same ECCs, and an error should be thrown.
  if (!is.null(R_final_mat) & !is.null(V_final_mat) & !is.null(Y_final_mat) &
      !is.null(R_services_mat) & !is.null(V_services_mat) & !is.null(Y_services_mat)) {
    # Test gross aggregate primary
    matsbyname::difference_byname(ex_p_lsfinal[[gross_aggregate_primary_lsfinal]], ex_p_lsservices[[gross_aggregate_primary_lsservices]]) |>
      matsbyname::iszero_byname(tol = tol) |>
      assertthat::assert_that(msg = "Gross primary aggregates for last_stage = 'Final' and last_stage = 'Useful' are not same to within `tol` in Recca::pfu_aggregates().")
    # Test net aggregate primary
    matsbyname::difference_byname(ex_p_lsfinal[[net_aggregate_primary_lsfinal]], ex_p_lsservices[[net_aggregate_primary_lsservices]]) |>
      matsbyname::iszero_byname(tol = tol) |>
      assertthat::assert_that(msg = "Net primary aggregates for last_stage = 'Final' and last_stage = 'Services' are not same to within `tol` in Recca::pfu_aggregates().")
  }
  # Ensure that useful and services versions of primary aggregates are same.
  # If they are not same, we probably don't have the same ECCs, and an error should be thrown.
  if (!is.null(R_useful_mat) & !is.null(V_useful_mat) & !is.null(Y_useful_mat) &
      !is.null(R_services_mat) & !is.null(V_services_mat) & !is.null(Y_services_mat)) {
    # Test gross aggregate primary
    matsbyname::difference_byname(ex_p_lsuseful[[gross_aggregate_primary_lsuseful]], ex_p_lsservices[[gross_aggregate_primary_lsservices]]) |>
      matsbyname::iszero_byname(tol = tol) |>
      assertthat::assert_that(msg = "Gross primary aggregates for last_stage = 'Final' and last_stage = 'Useful' are not same to within `tol` in Recca::pfu_aggregates().")
    # Test net aggregate primary
    matsbyname::difference_byname(ex_p_lsuseful[[net_aggregate_primary_lsuseful]], ex_p_lsservices[[net_aggregate_primary_lsservices]]) |>
      matsbyname::iszero_byname(tol = tol) |>
      assertthat::assert_that(msg = "Net primary aggregates for last_stage = 'Useful' and last_stage = 'Useful' are not same to within `tol` in Recca::pfu_aggregates().")
  }

  c(ex_p_lsfinal, ex_p_lsuseful, ex_p_lsservices)
}



calc_final_aggs_helper <- function(U_eiou_final_mat, Y_final_mat,
                                   U_eiou_useful_mat, Y_useful_mat,
                                   U_eiou_services_mat, Y_services_mat,
                                   fd_sectors,
                                   piece, notation, pattern_type, prepositions,
                                   by,
                                   net_aggregate_final_lsfinal = net_aggregate_final_lsfinal,
                                   gross_aggregate_final_lsfinal = gross_aggregate_final_lsfinal,
                                   net_aggregate_final_lsuseful = net_aggregate_final_lsuseful,
                                   gross_aggregate_final_lsuseful = gross_aggregate_final_lsuseful,
                                   net_aggregate_final_lsservices = net_aggregate_final_lsservices,
                                   gross_aggregate_final_lsservices = gross_aggregate_final_lsservices) {

  # Calculate final stage aggregates when last_stage = "Final"
  # with finaldemand_aggregates()
  if (!is.null(U_eiou_final_mat) & !is.null(Y_final_mat)) {
    ex_f_lsfinal <- finaldemand_aggregates(fd_sectors = fd_sectors,
                                           piece = piece, notation = notation, pattern_type = pattern_type, prepositions = prepositions,
                                           U_eiou = U_eiou_final_mat, Y = Y_final_mat,
                                           by = by,
                                           net_aggregate_demand = net_aggregate_final_lsfinal,
                                           gross_aggregate_demand = gross_aggregate_final_lsfinal)
  } else {
    ex_f_lsfinal <- list(NULL, NULL) |>
      magrittr::set_names(c(net_aggregate_final_lsfinal, gross_aggregate_final_lsfinal))
  }


  # Calculate final stage aggregates when last_stage = "Useful"
  # ********************** Fill this with real code ************************
  ex_f_lsuseful <- list(NULL, NULL) |>
    magrittr::set_names(c(net_aggregate_final_lsuseful, gross_aggregate_final_lsuseful))

  # Ensure that final stage aggregates are same when
  # last_stage = "Final" and last_stage = "Useful.

  # Calculate final stage aggregates when last_stage = "Services"
  ex_f_lsservices <- list(NULL, NULL) |>
    magrittr::set_names(c(net_aggregate_final_lsservices, gross_aggregate_final_lsservices))

  c(ex_f_lsfinal, ex_f_lsuseful, ex_f_lsservices)
}



calc_useful_aggs_helper <- function(U_eiou_final_mat, Y_final_mat,
                                    U_eiou_useful_mat, Y_useful_mat,
                                    U_eiou_services_mat, Y_services_mat,
                                    fd_sectors,
                                    piece, notation, pattern_type, prepositions,
                                    by,
                                    net_aggregate_useful_lsfinal = net_aggregate_useful_lsfinal,
                                    gross_aggregate_useful_lsfinal = gross_aggregate_useful_lsfinal,
                                    net_aggregate_useful_lsuseful = net_aggregate_useful_lsuseful,
                                    gross_aggregate_useful_lsuseful = gross_aggregate_useful_lsuseful,
                                    net_aggregate_useful_lsservices = net_aggregate_useful_lsservices,
                                    gross_aggregate_useful_lsservices = gross_aggregate_useful_lsservices) {

  # Calculate useful stage aggregations when last_stage = "Final"
  # ********************** Fill this with real code ************************
  ex_u_lsfinal <- list(NULL, NULL) |>
    magrittr::set_names(c(net_aggregate_useful_lsfinal, gross_aggregate_useful_lsfinal))

  # Calculate useful stage aggregations when last_stage = "Useful"
  # with finaldemand_aggregates()
  if (!is.null(U_eiou_useful_mat) & !is.null(Y_useful_mat)) {
    ex_u_lsuseful <- finaldemand_aggregates(fd_sectors = fd_sectors,
                                            piece = piece, notation = notation, pattern_type = pattern_type, prepositions = prepositions,
                                            U_eiou = U_eiou_useful_mat, Y = Y_useful_mat,
                                            by = by,
                                            net_aggregate_demand = net_aggregate_useful_lsuseful,
                                            gross_aggregate_demand = gross_aggregate_useful_lsuseful)
  } else {
    ex_u_lsuseful <- list(NULL, NULL) |>
      magrittr::set_names(c(net_aggregate_useful_lsuseful, gross_aggregate_useful_lsuseful))
  }


  # Ensure that useful stage aggregations are same when
  # last_stage = "Final" and last_stage = "Useful".


  # Calculate useful stage aggregations when last_stage = "Services"
  ex_u_lsservices <- list(NULL, NULL) |>
    magrittr::set_names(c(net_aggregate_useful_lsservices, gross_aggregate_useful_lsservices))

  c(ex_u_lsfinal, ex_u_lsuseful, ex_u_lsservices)
}



calc_services_aggs_helper <- function(U_eiou_final_mat, Y_final_mat,
                                      U_eiou_useful_mat, Y_useful_mat,
                                      U_eiou_services_mat, Y_services_mat,
                                      fd_sectors,
                                      piece, notation, pattern_type, prepositions,
                                      by,
                                      net_aggregate_services_lsfinal = net_aggregate_services_lsfinal,
                                      gross_aggregate_services_lsfinal = gross_aggregate_services_lsfinal,
                                      net_aggregate_services_lsuseful = net_aggregate_services_lsuseful,
                                      gross_aggregate_services_lsuseful = gross_aggregate_services_lsuseful,
                                      net_aggregate_services_lsservices = net_aggregate_services_lsservices,
                                      gross_aggregate_services_lsservices = gross_aggregate_services_lsservices) {

  # Calculate services stage aggregations when last_stage = "Final"
  ex_s_lsfinal <- list(NULL, NULL) |>
    magrittr::set_names(c(net_aggregate_services_lsfinal, gross_aggregate_services_lsfinal))

  # Calculate services stage aggregations when last_stage = "Useful"
  ex_s_lsuseful <- list(NULL, NULL) |>
    magrittr::set_names(c(net_aggregate_services_lsuseful, gross_aggregate_services_lsuseful))

  # Calculate services stage aggregations when last_stage = "Services"
  # with finaldemand_aggregates()
  if (!is.null(U_eiou_services_mat) & !is.null(Y_services_mat)) {
    ex_s_lsservices <- finaldemand_aggregates(fd_sectors = fd_sectors,
                                              piece = piece, notation = notation, pattern_type = pattern_type, prepositions = prepositions,
                                              U_eiou = U_eiou_services_mat, Y = Y_services_mat,
                                              by = by,
                                              net_aggregate_demand = net_aggregate_services_lsservices,
                                              gross_aggregate_demand = gross_aggregate_services_lsservices)
  } else {
    ex_s_lsservices <- list(NULL, NULL) |>
      magrittr::set_names(c(net_aggregate_services_lsservices, gross_aggregate_services_lsservices))
  }

  c(ex_s_lsfinal, ex_s_lsuseful, ex_s_lsservices)
}


