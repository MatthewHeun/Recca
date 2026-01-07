#' Verifies SUT inter-industry energy balances
#'
#' `r lifecycle::badge('deprecated')`
#'
#' [verify_SUT_energy_balance()] is deprecated in favor of the combination of
#' [calc_inter_industry_balance()] and
#' [verify_inter_industry_balance()].
#' Pipe the first into the second.
#'
#' Energy balances are confirmed by Product (within \code{tol}) for every row in \code{.sutmats}.
#'
#' If energy is in balance for every row, \code{.sutmats} is returned with an additional column, and
#' execution returns to the caller.
#' If energy balance is not observed for one or more of the rows,
#' a warning is emitted, and
#' the additional column (\code{SUT_energy_blance})
#' indicates where the problem occurred, with \code{FALSE}
#' showing where energy is not in balance.
#'
#' @param .sutmats an SUT-style data frame with columns of matrices,
#'                 including `U`, `V`, and `Y` columns.
#' @param R resources (`R`) matrix or name of the column in `.sutmats` that contains same. Default is "R".
#' @param U use (`U`) matrix or name of the column in `.sutmats` that contains same. Default is "U".
#' @param V make (`V`) matrix or name of the column in `.sutmats`that contains same. Default is "V".
#' @param Y final demand (`Y`) matrix or name of the column in `.sutmats` that contains same. Default is "Y".
#' @param tol the maximum amount by which Supply and Consumption can be out of balance.
#'            Default is `1e-6`.
#' @param SUT_energy_balance The name of the column in output containing logical
#'                           values representing the balance status of that row.
#'
#' @return a list or data frame saying whether `.sutmats` are in balance.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' # This function is deprecated.
#' # Instead of this:
#' UKEnergy2000mats |>
#'   dplyr::filter(LastStage %in% c("Final", "Useful")) |>
#'   tidyr::spread(key = matrix.name, value = matrix) |>
#'   verify_SUT_energy_balance(tol = 1e-4)
#' # Do this:
#' UKEnergy2000mats |>
#'   dplyr::filter(LastStage %in% c("Final", "Useful")) |>
#'   tidyr::spread(key = matrix.name, value = matrix) |>
#'   calc_inter_industry_balance() |>
#'   verify_inter_industry_balance(tol = 1e-4)
verify_SUT_energy_balance <- function(.sutmats = NULL,
                                      # Input names
                                      R = "R", U = "U", V = "V", Y = "Y",
                                      # Tolerance
                                      tol = 1e-6,
                                      # Output name
                                      SUT_energy_balance = ".SUT_energy_balance"){
  lifecycle::deprecate_soft(when = "0.1.65",
                            what = "verify_SUT_energy_balance()",
                            with = "verify_inter_industry_balance()")

  out <- .sutmats |>
    calc_inter_industry_balance(R = R, U = U, V = V, Y = Y,
                                balance = Recca::balance_cols$inter_industry_balance_colname) |>
    verify_inter_industry_balance(tol = tol,
                                  balances = Recca::balance_cols$inter_industry_balance_colname,
                                  balanced = SUT_energy_balance)
  # Remove the balances item to mimic the behaviour of the deprecated function.
  if (is.data.frame(out)) {
    out <- out |>
      dplyr::mutate(
        "{Recca::balance_cols$inter_industry_balance_colname}" := NULL
      )
  } else {
    # Assume out is a list
    out[Recca::balance_cols$inter_industry_balance_colname] <- NULL
  }
  return(out)
}









#' Confirm that an SUT-style data frame conserves energy.
#'
#' `r lifecycle::badge('deprecated')`
#'
#' If energy is in balance for every row,
#' `.sutmats` is returned with two additional columns, and
#' execution returns to the caller.
#' If energy balance is not observed for one or more rows,
#' a warning is emitted, and
#' columns named `SUT_prod_energy_blance` and `SUT_ind_energy_blance` are added to `.sutmats`.
#' `FALSE` indicates energy is not in balance.
#'
#' This function should be called
#' for its side-effect of testing whether energy is in balance in `.sutmats`.
#'
#' Both product and industry energy balance are verified.
#' Units (as supplied by the `S_units` matrix) are respected.
#'
#' @param .sutmats An SUT-style data frame containing columns
#'                 `R` (optionally), `U`, `V`, `Y`, and `S_units`.
#' @param R Resource (`R`) matrix or name of the column in `.sutmats` that contains same. Default is "R".
#' @param U Use (`U`) matrix or name of the column in `.sutmats` that contains same. Default is "U".
#' @param V Make (`V`) matrix or name of the column in `.sutmats`that contains same. Default is "V".
#' @param Y Final demand (`Y`) matrix or name of the column in `.sutmats` that contains same. Default is "Y".
#' @param S_units `S_units` A matrix or name of the column in `.sutmats` that contains same. Default is "S_units".
#' @param U_feed,U_eiou,r_eiou Optional matrices or columns in `.sutmats`.
#' @param tol The maximum amount by which energy can be out of balance. Default is `1e-6`.
#' @param matnames,matvals,rowtypes,coltypes,rownames,colnames Column names used internally.
#' @param prod_diff,ind_diff,ebal_error,product Column names for product and industry energy balance errors.
#' @param SUT_prod_energy_balanced The name for booleans telling if product energy is in balance. Default is ".SUT_prod_energy_balance".
#' @param SUT_ind_energy_balanced The name for booleans telling if product energy is in balance. Default is ".SUT_inds_energy_balance".
#'
#' @return `.sutmats` with additional columns.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' library(tidyr)
#' verify_SUT_energy_balance_with_units(UKEnergy2000mats %>%
#'                                        tidyr::spread(key = matrix.name, value = matrix),
#'                                        tol = 1e-3)
verify_SUT_energy_balance_with_units <- function(.sutmats = NULL,
                                                 # Input names
                                                 R = "R", U = "U", U_feed = "U_feed", U_eiou = "U_EIOU",
                                                 r_eiou = "r_EIOU", V = "V", Y = "Y", S_units = "S_units",
                                                 # Tolerance
                                                 tol = 1e-6,
                                                 # Column names
                                                 matnames = "matnames",
                                                 matvals = "matvals",
                                                 rowtypes = "rowtypes",
                                                 coltypes = "coltypes",
                                                 rownames = "rownames",
                                                 colnames = "colnames",
                                                 # Output column names
                                                 prod_diff = ".prod_diff",
                                                 ind_diff = ".ind_diff",
                                                 SUT_prod_energy_balanced = ".SUT_prod_energy_balanced",
                                                 SUT_ind_energy_balanced = ".SUT_ind_energy_balanced",
                                                 ebal_error = "ebal_error",
                                                 product = "Product") {

  lifecycle::deprecate_warn(when = "0.1.65",
                            what = "verify_SUT_energy_balance_with_units()",
                            details = "verify_SUT_energy_balance_with_units() seemed like a good idea at the time, but we never use it.")

  verify_func <- function(R = NULL, U, V, Y, S_units){
    y <- matsbyname::rowsums_byname(Y)
    if (is.null(R)) {
      R_plus_V <- V
    } else {
      R_plus_V <- matsbyname::sum_byname(R, V)
    }
    W <- matsbyname::difference_byname(matsbyname::transpose_byname(R_plus_V), U)
    U_bar <- matsbyname::matrixproduct_byname(matsbyname::transpose_byname(S_units), U)
    RV_bar <- matsbyname::matrixproduct_byname(R_plus_V, S_units)
    W_bar <- matsbyname::matrixproduct_byname(matsbyname::transpose_byname(S_units), W)
    prod_diffs <- matsbyname::difference_byname(matsbyname::rowsums_byname(W), y)
    ind_diffs <- matsbyname::difference_byname(RV_bar, matsbyname::transpose_byname(W_bar)) %>%
      matsbyname::difference_byname(matsbyname::transpose_byname(U_bar))
    prodOK <- prod_diffs %>%
      matsbyname::iszero_byname(tol = tol)
    indOK <- ind_diffs %>%
      matsbyname::iszero_byname(tol = tol)
    list(prod_diffs, ind_diffs, prodOK, indOK) %>%
      magrittr::set_names(c(prod_diff, ind_diff,
                            SUT_prod_energy_balanced, SUT_ind_energy_balanced))
  }
  out <- matsindf::matsindf_apply(.sutmats, FUN = verify_func, R = R, U = U, V = V, Y = Y, S_units = S_units)
  if (!all(out[[SUT_prod_energy_balanced]] %>% as.logical())) {
    # Find out which products are out of balance.
    unbalanced <- out |>
      dplyr::filter(!.data[[SUT_prod_energy_balanced]])
    errors_for_msg <- unbalanced |>
      dplyr::mutate(
        # Eliminate all matrix columns, leaving only metadata columns
        "{R}" := NULL,
        "{U}" := NULL,
        "{U_feed}" := NULL,
        "{U_eiou}" := NULL,
        "{r_eiou}" := NULL,
        "{V}" := NULL,
        "{Y}" := NULL,
        "{S_units}" := NULL,
        # Eliminate other unneeded columns
        "{ind_diff}" := NULL,
        "{SUT_prod_energy_balanced}" := NULL,
        "{SUT_ind_energy_balanced}" := NULL
      ) |>
      tidyr::pivot_longer(cols = dplyr::all_of(prod_diff),
                          names_to = matnames,
                          values_to = matvals) |>
      # Expand the error vector to show all unbalances
      matsindf::expand_to_tidy(matnames = matnames,
                               matvals = matvals) |>
      # Filter to only the non-zero error values
      dplyr::filter(.data[[matvals]] != 0) |>
      dplyr::mutate(
        # Get rid of unneeded columns
        "{matnames}" := NULL,
        "{rowtypes}" := NULL,
        "{coltypes}" := NULL,
        "{colnames}" := NULL
      ) |>
      dplyr::rename(
        "{product}" := dplyr::all_of(rownames),
        "{ebal_error}" := dplyr::all_of(matvals)
      ) |>
      matsindf::df_to_msg()
    msg <- paste0("Energy not conserved by product in verify_SUT_energy_balance_with_units().\n",
                  errors_for_msg)
    stop(msg)
  }
  assertthat::assert_that(all(out[[SUT_ind_energy_balanced]] %>% as.logical()),
                          msg = paste("Energy not conserved by industry in verify_SUT_energy_balance_with_units",
                                      "See column",
                                      SUT_ind_energy_balanced))
  # All checks passed.
  # Remove unneeded columns.
  out <- out |>
    dplyr::mutate(
      "{prod_diff}" := NULL,
      "{ind_diff}" := NULL
    )
  return(out)
}
