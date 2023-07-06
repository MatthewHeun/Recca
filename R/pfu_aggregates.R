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
#' Primary aggregates can be computed when
#' last stage is final and last stage is useful.
#' Ostensibly, the primary aggregates should be the same
#' in both cases when metadata are the same.
#' An error is thrown if that is not true to within `tol`.
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
                           # Vector of final demand sectors
                           fd_sectors,
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
                           primary = Recca::all_stages$primary,
                           final = Recca::all_stages$final,
                           useful = Recca::all_stages$useful,
                           services = Recca::all_stages$services,
                           sep = "___ls",
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
                           .matvals = Recca::psut_cols$matvals,
                           tol = 1e-6) {

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
                                      R_final, U_final, U_feed_final, U_eiou_final, r_eiou_final, V_final, Y_final, S_units_final,
                                      R_useful, U_useful, U_feed_useful, U_eiou_useful, r_eiou_useful, V_useful, Y_useful, S_units_useful,
                                      R_services, U_services, U_feed_services, U_eiou_services, r_eiou_services, V_services, Y_services, S_units_services)),
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

    # Calculate primary aggregates from
    # last_stage == "Final", last_stage == "Useful", and last_stage == "Services"
    # when available
    out <- calc_primary_aggs_helper(R_final_mat = R_final_mat, V_final_mat = V_final_mat, Y_final_mat = Y_final_mat,
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

    # Calculate final stage aggregates when last_stage = "Final"
    # with finaldemand_aggregates()
    if (!is.null(U_final_mat) & !is.null(U_feed_final_mat) & !is.null(Y_final_mat)) {
      res <- finaldemand_aggregates(fd_sectors = fd_sectors,
                                    piece = piece, notation = notation, pattern_type = pattern_type, prepositions = prepositions,
                                    U = U_final_mat, U_feed = U_feed_final_mat, Y = Y_final_mat,
                                    by = by,
                                    net_aggregate_demand = net_aggregate_final,
                                    gross_aggregate_demand = gross_aggregate_final)
      out <- c(out, res)
    } else {
      out <- c(out, list(NULL, NULL) |>
                 magrittr::set_names(c(net_aggregate_final, gross_aggregate_final)))
    }


    # Calculate final stage aggregates when last_stage = "Useful"


    # Ensure that final stage aggregates are same when
    # last_stage = "Final" and last_stage = "Useful.





    # Calculate useful stage aggregations when last_stage = "Final"



    # Calculate useful stage aggregations when last_stage = "Useful"
    # with finaldemand_aggregates()
    if (!is.null(U_useful_mat) & !is.null(U_feed_useful_mat) & !is.null(Y_useful_mat)) {
      res <- finaldemand_aggregates(fd_sectors = fd_sectors,
                                    piece = piece, notation = notation, pattern_type = pattern_type, prepositions = prepositions,
                                    U = U_useful_mat, U_feed = U_feed_useful_mat, Y = Y_useful_mat,
                                    by = by,
                                    net_aggregate_demand = net_aggregate_useful,
                                    gross_aggregate_demand = gross_aggregate_useful)
      out <- c(out, res)
    } else {
      out <- c(out, list(NULL, NULL) |>
                 magrittr::set_names(c(net_aggregate_useful, gross_aggregate_useful)))
    }


    # Ensure that useful stage aggregations are same when
    # last_stage = "Final" and last_stage = "Useful".



    # Calculate services stage aggregations when last_stage = "Services"
    # with finaldemand_aggregates()
    if (!is.null(U_services_mat) & !is.null(U_feed_services_mat) & !is.null(Y_services_mat)) {
      res <- finaldemand_aggregates(fd_sectors = fd_sectors,
                                    piece = piece, notation = notation, pattern_type = pattern_type, prepositions = prepositions,
                                    U = U_services_mat, U_feed = U_feed_services_mat, Y = Y_services_mat,
                                    by = by,
                                    net_aggregate_demand = net_aggregate_services,
                                    gross_aggregate_demand = gross_aggregate_services)
      out <- c(out, res)
    } else {
      out <- c(out, list(NULL, NULL) |>
                 magrittr::set_names(c(net_aggregate_services, gross_aggregate_services)))
    }


    return(out)
  }

  # result <- matsindf::matsindf_apply(.sutdata, FUN = pfuagg_func,
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

  out <- list()

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
    out <- c(out, ex_p_lsfinal)
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
    out <- c(out, ex_p_lsuseful)
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
    out <- c(out, ex_p_lsservices)
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

  return(out)
}
