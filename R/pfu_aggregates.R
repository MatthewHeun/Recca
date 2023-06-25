#' Aggregate to primary, final, and useful stages
#'
#' There are several ways to aggregate energy conversion chain (ECC) data
#' to primary, final, or useful stages of the ECC.
#' [primary_aggregates()] aggregates primary energy using the
#' resource (**R**), make (**V**), and final demand (**Y**) matrices.
#' [primary_aggregates()] gives upstream (source) aggregations for an ECC.
#' [finaldemand_aggregates()] aggregates to the last stage
#' of an energy conversion chain,
#' regardless of whether the last stage is final or useful.
#' [finaldemand_aggregates()] gives downstream (sink) aggregations.
#' However, applying [finaldemand_aggregates()] to an ECC
#' whose last stage is final cannot produce useful stage aggregattions.
#' Similarly, applying [finaldemand_aggregates()] to an ECC whose last stage is useful
#' cannot provide final stage aggregates.
#' This function calculates aggregates at all ECC stages (primary, final, and useful),
#' regardless of whether the last stage is final or useful.
#' See details for the approach.
#'
#'
#'
#' |                | Desired aggregation stage |
#' | ECC last stage | Final | Useful |
#' | -------------- | ----- | ------ |
#' | Final          | [finaldemand_aggregates()] | Not possible               |
#' | Useful         | Not possible               | [finaldemand_aggregates()] |
#'
#' Whereas [primary_aggregates()] and [finaldemand_aggregates()]
#' work independently of the last stage of an ECC,
#' this function requires both final and useful last stage ECCs
#' to be present in `.sutdata`.
#'
#' Suffixes to matrix names are assumed to indicate the last stage
#' of the ECC for which the matrix applies.
#' For example, two versions of the **R** matrix should be present:
#' `R_final` and `R_useful`.
#'
#' If `.sutdata` is a wide-by-matrices data frame
#' but contains a `last_stage` column,
#' `.sutdata` is pivoted wide to put data into the correct shape,
#' forming columns for each ECC matrix.
#' The `last_stage` and `R` columns make `R_final` and `R_useful` columns.
#' The `last_stage` and `V` columns make `V_final` and `V_useful` columns.
#' Etc.
#' If either the last stage final or the last stage useful ECC representations
#' is missing, an error is thrown.
#' See examples.
#'
#' Internally, this function uses
#' [primary_aggregates()], [finaldemand_aggregates()], and
#' [calc_eta_fu_Y_eiou()] to complete its work.
#'
#' @param .sutdata An optional data frame containing physical supply use table
#'                 descriptions of energy conversion chains.
#'
#' @return
#'
#' @export
#'
#' @examples
pfu_aggregates <- function(.sutdata,
                           # Vector of primary industries
                           p_industries,
                           by = c("Total", "Product", "Industry", "Flow"),
                           add_net_gross_cols = FALSE,
                           piece = "all",
                           notation = RCLabels::notations_list,
                           pattern_type = c("exact", "leading", "trailing", "anywhere", "literal"),
                           prepositions = RCLabels::prepositions_list,
                           # Output names
                           aggregate_primary = Recca::aggregate_cols$aggregate_primary,
                           net_aggregate_primary = Recca::aggregate_cols$net_aggregate_primary,
                           gross_aggregate_primary = Recca::aggregate_cols$gross_aggregate_primary,

                           aggregate_final = Recca::aggregate_cols$aggregate_final,
                           net_aggregate_final = Recca::aggregate_cols$net_aggregate_final,
                           gross_aggregate_final = Recca::aggregate_cols$gross_aggregate_final,

                           aggregate_useful = Recca::aggregate_cols$aggregate_useful,
                           net_aggregate_useful = Recca::aggregate_cols$net_aggregate_useful,
                           gross_aggregate_useful = Recca::aggregate_cols$gross_aggregate_useful,

                           aggregate_services = Recca::aggregate_cols$aggregate_services,
                           net_aggregate_services = Recca::aggregate_cols$net_aggregate_services,
                           gross_aggregate_services = Recca::aggregate_cols$gross_aggregate_services,

                           net_aggregate_finaldemand = Recca::aggregate_cols$net_aggregate_demand,
                           gross_aggregate_finaldemand = Recca::aggregate_cols$gross_aggregate_demand,

                           # Stages
                           last_stage = Recca::psut_cols$last_stage,
                           final = Recca::all_stages$final,
                           useful = Recca::all_stages$useful,
                           services = Recca::all_stages$services,
                           sep = "_",
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
                           R_final = paste0(Recca::psut_cols$R, sep, final),
                           U_final = paste0(Recca::psut_cols$U, sep, final),
                           U_feed_final = paste0(Recca::psut_cols$U_feed, sep, final),
                           U_eiou_final = paste0(Recca::psut_cols$U_eiou, sep, final),
                           r_eiou_final = paste0(Recca::psut_cols$r_eiou, sep, final),
                           V_final = paste0(Recca::psut_cols$V, sep, final),
                           Y_final = paste0(Recca::psut_cols$Y, sep, final),
                           S_units_final = paste0(Recca::psut_cols$S_units, sep, final),
                           # Last stage useful matrix names
                           R_useful = paste0(Recca::psut_cols$R, sep, useful),
                           U_useful = paste0(Recca::psut_cols$U, sep, useful),
                           U_feed_useful = paste0(Recca::psut_cols$U_feed, sep, useful),
                           U_eiou_useful = paste0(Recca::psut_cols$U_eiou, sep, useful),
                           r_eiou_useful = paste0(Recca::psut_cols$r_eiou, sep, useful),
                           V_useful = paste0(Recca::psut_cols$V, sep, useful),
                           Y_useful = paste0(Recca::psut_cols$Y, sep, useful),
                           S_units_useful = paste0(Recca::psut_cols$S_units, sep, useful),
                           # Last stage services matrix names
                           R_services = paste0(Recca::psut_cols$R, sep, services),
                           U_services = paste0(Recca::psut_cols$U, sep, services),
                           U_feed_services = paste0(Recca::psut_cols$U_feed, sep, services),
                           U_eiou_services = paste0(Recca::psut_cols$U_eiou, sep, services),
                           r_eiou_services = paste0(Recca::psut_cols$r_eiou, sep, services),
                           V_services = paste0(Recca::psut_cols$V, sep, services),
                           Y_services = paste0(Recca::psut_cols$Y, sep, services),
                           S_units_services = paste0(Recca::psut_cols$S_units, sep, services),
                           # Names of internally pivoted columns
                           .matnames = Recca::psut_cols$matnames,
                           .matvals = Recca::psut_cols$matvals) {

  # If .sutdata is a data frame and it contains a last_stage column,
  # pivot wider before calling pfu_agg_func().
  if (is.data.frame(.sutdata) & (last_stage %in% names(.sutdata))) {
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
                                      R_final, U_final, U_feed_final, U_eiou_final, r_eiou_final, V_final, Y_final, S_units_final,
                                      R_useful, U_useful, U_feed_useful, U_eiou_useful, r_eiou_useful, V_useful, Y_useful, S_units_useful,
                                      R_services, U_services, U_feed_services, U_eiou_services, r_eiou_services, V_services, Y_services, S_units_services)),
                      .after = dplyr::last_col())
  }


  pfuagg_func <- function(R_final_mat = NULL, R_useful_mat = NULL, R_services_mat = NULL,
                          U_final_mat = NULL, U_useful_mat = NULL, U_services_mat = NULL,
                          U_feed_final_mat = NULL, U_feed_useful_mat = NULL, U_feed_services_mat = NULL,
                          U_eiou_final_mat = NULL, U_eiou_useful_mat = NULL, U_eiou_services_mat = NULL,
                          r_eiou_final_mat = NULL, r_eiou_useful_mat = NULL, r_eiou_services_mat = NULL,
                          V_final_mat = NULL, V_useful_mat = NULL, V_services_mat = NULL,
                          Y_final_mat = NULL, Y_useful_mat = NULL, Y_services_mat = NULL,
                          S_units_final_mat = NULL, S_units_useful_mat = NULL, S_units_services_mat = NULL) {

    # Calculate primary aggregates from all 3 last stages (final, useful, and services)
    ex_p_final <- .sutdata |>
      primary_aggregates(p_industries = p_industries,
                         by = by,
                         add_net_gross_cols = TRUE, piece = piece, notation = notation, pattern_type = pattern_type, prepositions = prepositions,
                         R = R_final_mat, V = V_final_mat, Y = Y_final_mat,
                         aggregate_primary = aggregate_primary, net_aggregate_primary = net_aggregate_primary)


    # Ensure that all 3 primary aggregates agree
    #

  }

  matsindf::matsindf_apply(.sutdata, FUN = pfuagg_func,
                           R_final_mat = R_final, R_useful_mat = R_useful, R_services_mat = R_services,
                           U_final_mat = U_final, U_useful_mat = U_useful, U_services_mat = U_services,
                           U_feed_final_mat = U_feed_final, U_feed_useful_mat = U_feed_useful, U_feed_services_mat = U_feed_services,
                           U_eiou_final_mat = U_eiou_final, U_eiou_useful_mat = U_eiou_useful, U_eiou_services_mat = U_eiou_services,
                           r_eiou_final_mat = r_eiou_final, r_eiou_useful_mat = r_eiou_useful, r_eiou_services_mat = r_eiou_services,
                           V_final_mat = V_final, V_useful_mat = V_useful, V_services_mat = V_services,
                           Y_final_mat = Y_final, Y_useful_mat = Y_useful, Y_services_mat = Y_services,
                           S_units_final_mat = S_units_final, S_units_useful_mat = S_units_useful, S_units_services_mat = S_units_services)

  # If the incoming .sutmats was a wide by matrices data frame,
  # pivot the output.

}
