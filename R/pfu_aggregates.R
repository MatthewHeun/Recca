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
                           last_stage = Recca::psut_cols$last_stage,
                           final = Recca::all_stages$final,
                           useful = Recca::all_stages$useful,
                           services = Recca::all_stages$services,
                           sep = "_",
                           # Regular matrix names
                           R_colname = Recca::psut_cols$R,
                           U_colname = Recca::psut_cols$U,
                           U_feed_colname = Recca::psut_cols$U_feed,
                           U_eiou_colname = Recca::psut_cols$U_eiou,
                           r_eiou_colname = Recca::psut_cols$r_eiou,
                           V_colname = Recca::psut_cols$V,
                           Y_colname = Recca::psut_cols$Y,
                           S_units_colname = Recca::psut_cols$S_units,
                           # Last stage final matrix names
                           R_final_colname = paste0(Recca::psut_cols$R, sep, final),
                           U_final_colname = paste0(Recca::psut_cols$U, sep, final),
                           U_feed_final_colname = paste0(Recca::psut_cols$U_feed, sep, final),
                           U_eiou_final_colname = paste0(Recca::psut_cols$U_eiou, sep, final),
                           r_eiou_final_colname = paste0(Recca::psut_cols$r_eiou, sep, final),
                           V_final_colname = paste0(Recca::psut_cols$V, sep, final),
                           Y_final_colname = paste0(Recca::psut_cols$Y, sep, final),
                           S_units_final_colname = paste0(Recca::psut_cols$S_units, sep, final),
                           # Last stage useful matrix names
                           R_useful_colname = paste0(Recca::psut_cols$R, sep, useful),
                           U_useful_colname = paste0(Recca::psut_cols$U, sep, useful),
                           U_feed_useful_colname = paste0(Recca::psut_cols$U_feed, sep, useful),
                           U_eiou_useful_colname = paste0(Recca::psut_cols$U_eiou, sep, useful),
                           r_eiou_useful_colname = paste0(Recca::psut_cols$r_eiou, sep, useful),
                           V_useful_colname = paste0(Recca::psut_cols$V, sep, useful),
                           Y_useful_colname = paste0(Recca::psut_cols$Y, sep, useful),
                           S_units_useful_colname = paste0(Recca::psut_cols$S_units, sep, useful),
                           # Last stage services matrix names
                           R_services_colname = paste0(Recca::psut_cols$R, sep, services),
                           U_services_colname = paste0(Recca::psut_cols$U, sep, services),
                           U_feed_services_colname = paste0(Recca::psut_cols$U_feed, sep, services),
                           U_eiou_services_colname = paste0(Recca::psut_cols$U_eiou, sep, services),
                           r_eiou_services_colname = paste0(Recca::psut_cols$r_eiou, sep, services),
                           V_services_colname = paste0(Recca::psut_cols$V, sep, services),
                           Y_services_colname = paste0(Recca::psut_cols$Y, sep, services),
                           S_units_services_colname = paste0(Recca::psut_cols$S_units, sep, services),
                           # Names of internally pivoted columns
                           .matnames = ".matnames",
                           .matvals = ".matvals") {


  # If .sutdata is a data frame and it contains a last_stage column,
  # pivot wider before calling pfu_agg_func().
  if (is.data.frame(.sutdata) & (last_stage %in% names(.sutdata))) {
    .sutdata <- .sutdata |>
      tidyr::pivot_longer(cols = dplyr::any_of(c(R_colname, U_colname, U_feed_colname, U_eiou_colname,
                                                 r_eiou_colname, V_colname, Y_colname, S_units_colname)),
                          names_to = .matnames,
                          values_to = .matvals) |>
      dplyr::mutate(
        "{.matnames}" := paste0(.data[[.matnames]], sep, .data[[last_stage]]),
        "{last_stage}" := NULL
      ) |>
      tidyr::pivot_wider(names_from = tidyr::all_of(.matnames),
                         values_from = tidyr::all_of(.matvals))
  }



}
