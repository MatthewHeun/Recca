#
# This file contains functions that verify energy balance
# on ECC representations.
#



#' Calculate inter- and intra-industry balances for PSUT (**RUVY**) matrices2
#'
#' Balances are an important aspect of analyzing mass and energy conversion chains
#' in the PSUT framework with the **RUVY** matrices.
#' Often, balances are calculated on energy or exergy flows.
#' Balances can be calculated for mass flows or monetary flows, too.
#'
#' ## Inter-industry balances
#'
#' In a PSUT description of a mass or energy conversion chain,
#' all of every product leaving one industry must arrive at another industry.
#' `calc_inter_industry_balances()`
#' calculates these between-industry balances via
#' ((**R** + **V**)^T^ - (**U** + **Y**))**i**.
#' Inter-industry balances are calculated for products (not industries).
#'
#' ## Intra-industry balances
#'
#' `calc_intra_industry_balances()` calculates across-industry
#' balances via
#' (**U**^T^ - **V**)**i**.
#' Inter-industry balances are calculated for industries (not products),
#' and the result is a column vector of industry balances.
#'
#' ## Outputs
#'
#' The argument `balance_colname` specifies the name of the
#' result.
#' By default, the output of `calc_inter_industry_balances()` is
#' [Recca::balance_cols]`$inter_industry_balance_colname` or
#' "`r Recca::balance_cols$inter_industry_balance_colname`".
#' By default, the output of `calc_intra_industry_balances()` is
#' [Recca::balance_cols]`$intra_industry_balance_colname` or
#' "`r Recca::balance_cols$intra_industry_balance_colname`".
#'
#' For inter-industry (between-industry) balances,
#' the result is a column vector of product balances.
#' The result should __always__ be the **0** vector.
#'
#' In a PSUT description of a mass or energy conversion chain,
#' the meaning of intra-industry (across-industry) balances
#' depends on the construction of the matrices.
#' When all losses are accounted in the matrices themselves,
#' the calculation of intra-industry balances for
#' mass and energy conversion chains should give the **0** vector
#' with industries in rows,
#' because mass and energy are conserved.
#' When losses are __not__ accounted in the matrices,
#' the calculation of intra-industry balances gives losses.
#'
#' For exergy conversion chains
#' (mass, energy, or both),
#' when losses are accounted in the matrices,
#' the intra-industry balance gives exergy destruction.
#' When losses are __not__ accounted in the matrices,
#' the intra-industry balance gives the sum of
#' exergy destruction and exergy losses.
#'
#' @param .sutmats A named list of matrices or
#'                 an SUT-style, wide-by-matrices data frame
#'                 with columns of matrices,
#'                 including `R`, `U`, `V`, and `Y`.
#' @param R Resources (**R**) matrix or name of the column in `.sutmats`
#'          that contains same.
#'          Default is "R".
#' @param U Use (**U**) matrix or name of the column in `.sutmats`
#'          that contains same.
#'          Default is "U".
#' @param V Make (**V**) matrix or name of the column in `.sutmats`
#'          that contains same.
#'          Default is "V".
#' @param Y Final demand (**Y**) matrix or name of the column in `.sutmats`
#'          that contains same.
#'          Default is "Y".
#' @param balance_colname The name of the column containing energy balance vectors.
#'                        Defaults are
#'                        [Recca::balance_cols]`$inter_industry_balance_colname` or
#'                        "`r Recca::balance_cols$inter_industry_balance_colname`"
#'                        for
#'                        `calc_inter_industry_balances()` and
#'                        [Recca::balance_cols]`$intra_industry_balance_colname` or
#'                        "`r Recca::balance_cols$intra_industry_balance_colname`"
#'                        for
#'                        `calc_intra_industry_balances()`.
#'
#' @returns A list or data frame containing balances.
#'
#' @seealso [verify_inter_industry_balance()] and [verify_intra_industry_balance()]
#'
#' @examples
#' # Inter-industry balances
#' inter <- UKEnergy2000mats |>
#'   dplyr::filter(LastStage %in% c("Final", "Useful")) |>
#'   tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
#'   calc_inter_industry_balance()
#' inter[[Recca::balance_cols$inter_industry_balance_colname]][[1]]
#' inter[[Recca::balance_cols$inter_industry_balance_colname]][[2]]
#' # Intra-industry balances
#' intra <- UKEnergy2000mats |>
#'   dplyr::filter(LastStage %in% c("Final", "Useful")) |>
#'   tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
#'   calc_intra_industry_balance()
#' intra[[Recca::balance_cols$intra_industry_balance_colname]][[1]]
#' intra[[Recca::balance_cols$intra_industry_balance_colname]][[2]]
#' @name balances
NULL



#' Verify inter- and intra-industry balances for PSUT (**RUVY**) matrices
#'
#' Balances can be verified in two ways,
#' inter-industry and intra-industry.
#' Inter-industry balances are between industries and performed by product.
#' Intra-industry balances are across industries and performed per industry.
#'
#' ## Inter-industry balances (by product)
#'
#' In a PSUT description of an economy, all of every product
#' leaving one industry must arrive at another industry.
#' Inter-industry (between-industry) balances are verified by product
#' (within `tol`) for every row in `.sutmats`.
#' Inter-industry balances should be calculated via
#' `calc_inter_industry_balances()` before calling
#' `verify_inter_industry_balances()`.
#' See examples.
#'
#' ## Intra-industry balances (by industry)
#'
#' Intra-industry (across-industry) balances are
#' verified by industry (within `tol`) for every row in `.sutmats`.
#' Intra-industry balances should be calculated via
#' `calc_intra_industry_balances()` before calling
#' `verify_intra_industry_balances()`.
#' See examples.
#'
#' The calculation of inter-industry balances
#' will give non-zero vectors for
#' quantities that are not conserved (such as exergy)
#' and for conversion chains that do not include wastes or losses.
#'
#' ## Outputs
#'
#' If every conversion chain in `.sutmats` is balanced,
#' `.sutmats` is returned with an additional column
#' (called `balanced_colname`), and
#' execution returns to the caller.
#' If balance is not observed for one or more of the rows
#' in `.sutmats`,
#' a warning is emitted, and
#' the additional column (`balanced_colname`)
#' indicates where the problem occurred, with `FALSE`
#' showing where products are not balanced
#' to within `tol`.
#'
#' Typically, one would call `calc_int*_industry_balance()`
#' before calling `verify_int*_industry_balance()`.
#' See the examples.
#'
#' @param .sutmats An SUT-style data frame with a column
#'                 named `balances`.
#' @param balances The name of a column that contains balance vectors.
#'                 For `verify_inter_industry_balances()`,
#'                 the default is [Recca::balance_cols]`$inter_industry_balance_colname`
#'                 or "`r Recca::balance_cols$inter_industry_balance_colname`".
#'                 For `verify_intra_industry_balances()`,
#'                 the default is [Recca::balance_cols]`$intra_industry_balance_colname`
#'                 or "`r Recca::balance_cols$intra_industry_balance_colname`".
#' @param tol The maximum amount by which products can be out of balance
#'            and still be considered balanced.
#'            Default is `1e-6`.
#' @param balanced_colname The name for booleans telling if balance is present.
#'                         For `verify_inter_industry_balances()`,
#'                         the default is [Recca::balance_cols]`$inter_industry_balanced_colname`
#'                         or "`r Recca::balance_cols$inter_industry_balanced_colname`".
#'                         For `verify_intra_industry_balances()`,
#'                         the default is [Recca::balance_cols]`$intra_industry_balanced_colname`
#'                         or "`r Recca::balance_cols$intra_industry_balanced_colname`".
#' @param delete_balance_cols_if_verified A boolean that tells whether to delete
#'                                        the `balances` and `balanced_colname` columns
#'                                        if `.sutmats` is a data frame and
#'                                        if balances are verified.
#'                                        Default is `FALSE`.
#'                                        If individual matrices are specified
#'                                        in the `R`, `U`, `V`, and `Y` arguments,
#'                                        no deletion is performed.
#'
#'
#' @return A list or data frame with an additional value or column
#'         saying whether `.sutmats` are in balance.
#'
#' @seealso [calc_inter_industry_balance()], [calc_intra_industry_balance()]
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' result_inter <- UKEnergy2000mats |>
#'   dplyr::filter(LastStage %in% c("Final", "Useful")) |>
#'   tidyr::pivot_wider(names_from = matrix.name,
#'                      values_from = matrix) |>
#'   calc_inter_industry_balance() |>
#'   verify_inter_industry_balance(tol = 1e-4)
#' result_inter
#' result_inter[[Recca::balance_cols$inter_industry_balanced_colname]]
#' @name verify-balances
NULL



#' @export
#' @rdname balances
calc_inter_industry_balance <- function(.sutmats = NULL,
                                        # Input names
                                        R = "R",
                                        U = "U",
                                        V = "V",
                                        Y = "Y",
                                        # Output name
                                        balance_colname = Recca::balance_cols$inter_industry_balance_colname) {
  calc_bal_func <- function(R_mat = NULL, U_mat, V_mat, Y_mat) {
    V_sums <- matsbyname::colsums_byname(V_mat)
    # Allow for missing R matrix
    if (is.null(R_mat)) {
      RV_sums <- V_sums
    } else {
      R_sums <- matsbyname::colsums_byname(R_mat)
      RV_sums <- matsbyname::sum_byname(R_sums, V_sums)
    }
    RV_sums <- matsbyname::transpose_byname(RV_sums)
    U_sums <- matsbyname::rowsums_byname(U_mat)
    Y_sums <- matsbyname::rowsums_byname(Y_mat)
    UY_sums <- matsbyname::sum_byname(U_sums, Y_sums)
    # (R + V)^T - (U + Y)
    bal <- matsbyname::difference_byname(RV_sums, UY_sums)
    list(bal) |>
      magrittr::set_names(balance_colname)
  }
  matsindf::matsindf_apply(.sutmats, FUN = calc_bal_func, R_mat = R, U_mat = U, V_mat = V, Y_mat = Y)
}



#' @export
#' @rdname verify-balances
verify_inter_industry_balance <- function(.sutmats = NULL,
                                          # Input names
                                          balances = Recca::balance_cols$inter_industry_balance_colname,
                                          # Tolerance
                                          tol = 1e-6,
                                          # Output name
                                          balanced_colname = Recca::balance_cols$inter_industry_balanced_colname,
                                          delete_balance_cols_if_verified = FALSE) {
  verify_func_inter <- function(bal_vector) {
    OK <- bal_vector |>
      matsbyname::iszero_byname(tol) |>
      as.logical()
    if (!OK) {
      return(list(FALSE) |>
               magrittr::set_names(balanced_colname))
    }
    list(TRUE) |>
      magrittr::set_names(balanced_colname)
  }

  out <- matsindf::matsindf_apply(.sutmats, FUN = verify_func_inter, bal_vector = balances)
  if (!all(as.logical(out[[balanced_colname]]))) {
    warning(paste0("Products are not conserved in verify_inter_industry_balance(). See column ",
                   balanced_colname,
                   "."))
  } else {
    if (is.data.frame(.sutmats) & delete_balance_cols_if_verified) {
      out[balances] <- NULL
      out[balanced_colname] <- NULL
    }
  }
  return(out)
}



#' @export
#' @rdname balances
calc_intra_industry_balance <- function(.sutmats = NULL,
                                        # Input names
                                        U = "U",
                                        V = "V",
                                        # Output name
                                        balance_colname = Recca::balance_cols$intra_industry_balance_colname) {
  calc_bal_func <- function(U_mat, V_mat) {
    bal <- matsbyname::difference_byname(matsbyname::transpose_byname(U_mat),
                                         V_mat) |>
      matsbyname::rowsums_byname()
    list(bal) |>
      magrittr::set_names(balance_colname)
  }
  matsindf::matsindf_apply(.sutmats, FUN = calc_bal_func, U_mat = U, V_mat = V)
}



#' @export
#' @rdname verify-balances
verify_intra_industry_balance <- function(.sutmats = NULL,
                                          # Input names
                                          balances = Recca::balance_cols$intra_industry_balance_colname,
                                          # Tolerance
                                          tol = 1e-6,
                                          # Output name
                                          balanced_colname = Recca::balance_cols$intra_industry_balanced_colname,
                                          delete_balance_cols_if_verified = FALSE) {
  verify_func_intra <- function(bal_vector) {
    OK <- bal_vector |>
      matsbyname::iszero_byname(tol) |>
      as.logical()
    if (!OK) {
      return(list(FALSE) |>
               magrittr::set_names(balanced_colname))
    }
    list(TRUE) |>
      magrittr::set_names(balanced_colname)
  }

  out <- matsindf::matsindf_apply(.sutmats, FUN = verify_func_intra, bal_vector = balances)
  if (!all(out[[balanced_colname]] |> as.logical())) {
    warning(paste0("Industries are not balanced in verify_intra_industry_balance(). See column ", balanced_colname, "."))
  } else {
    if (is.data.frame(.sutmats) & delete_balance_cols_if_verified) {
      out[balances] <- NULL
      out[balanced_colname] <- NULL
    }
  }
  return(out)
}










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
                                balance_colname = Recca::balance_cols$inter_industry_balance_colname) |>
    verify_inter_industry_balance(tol = tol,
                                  balances = Recca::balance_cols$inter_industry_balance_colname,
                                  balanced_colname = SUT_energy_balance)
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


#' Confirm that all Industries in an SUT-style data frame produce energy.
#'
#' If a transformation process industry consumes energy (in the `U` matrix)
#' but does not make energy (in the `V` matrix),
#' it is most certainly an error.
#' (In contrast, there can be Industries that make energy but do not consume it,
#' such as Industries involved in Production.
#' And final demand sectors consume energy but do not produce any.)
#' This function emits a warning if an Industry in the `U` matrix
#' is found to consume energy but not make energy.
#' Look at the `industry_production_OK` column of the output to see which rows of
#' `.sutmats` exhibit the problem.
#' Look at the `problem_industries` column of the output to see which industries
#' exhibit this problem.
#'
#' @param .sutmats an SUT-style data frame containing metadata columns
#' (typically `Country`, `Year`, `LedgerSide`, `Product`, etc.)
#' and columns of SUT matrices, including `U` and `V`.
#' @param R resources (**R**) matrix or name of the column in `.sutmats` that contains same. Default is "R".
#' @param U use (**U**) matrix or name of the column in `.sutmats` that contains same. Default is "U".
#' @param V make (**V**) matrix or name of the column in `.sutmats`that contains same. Default is "V".
#' @param industry_production_OK the name of the column in the output that
#'        tells whether all industries produce something. Default is ".industry_production_OK".
#' @param problem_industries the name of the column in the output that
#'        tells which transformation processes consume energy but do not produce anything.
#'
#' @return `.sutmats` with added column named with the value of `industry_production_OK`.
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' verify_SUT_industry_production(UKEnergy2000mats %>%
#'                                  spread(key = matrix.name, value = matrix))
verify_SUT_industry_production <- function(.sutmats = NULL,
                                           # Input column names
                                           R = "R", U = "U", V = "V",
                                           # Output column names
                                           industry_production_OK = ".industry_production_OK",
                                           problem_industries = ".problem_industries"){
  verify_func <- function(R_mat = NULL, U_mat, V_mat) {
    if (is.null(R_mat)) {
      R_plus_V_mat <- V_mat
    } else {
      R_plus_V_mat <- matsbyname::sum_byname(R_mat, V_mat)
    }
    check <- matsbyname::rowsums_byname(R_plus_V_mat) %>%
      matsbyname::complete_rows_cols(mat = matsbyname::transpose_byname(U_mat), margin = 1)
    OK <- !any(check == 0)
    problems <- rownames(check)[which(check == 0)]
    if (length(problems) == 0) {
      # To enable building the outgoing list.
      problems <- NULL
    }
    list(OK, problems) %>% magrittr::set_names(c(industry_production_OK, problem_industries))
  }
  out <- matsindf::matsindf_apply(.sutmats, FUN = verify_func, R_mat = R, U_mat = U, V_mat = V)
  if (!all(out[[industry_production_OK]] %>% as.logical())) {
    warning(paste("There are some industries that consume but do not produce energy. See column", industry_production_OK))
  }
  return(out)
}


#' Confirm that an IEA-style data frame conserves energy.
#'
#' Energy balances are confirmed (within \code{tol}) for every combination of
#' grouping variables in \code{.ieatidydata}.
#'
#' Be sure to group \code{.ieatidydata} prior to calling this function,
#' as shown in the example.
#'
#' If energy is in balance for every group, a data frame with additional column \code{err}
#' is returned.
#' If energy balance is not observed for one or more of the groups,
#' a warning is emitted.
#'
#' @param .ieatidydata an IEA-style data frame containing grouping columns
#'        (typically \code{Country}, \code{Year}, \code{Product}, and others),
#'        a \code{LedgerSide} column, and
#'        an energy column (\code{E.ktoe}).
#'        \code{.ieatidydata} should be grouped prior to sending to this function.
#' @param LedgerSide the name of the column in \code{.ieatidydata}
#'        that contains ledger side information (a string). Default is "\code{LedgerSide}".
#' @param energy the name of the column in \code{.ieatidydata}
#'        that contains energy data (a string). Default is "\code{E.ktoe}".
#' @param supply the identifier for supply data in the \code{LedgerSide} column (a string).
#'        Default is "\code{Supply}".
#' @param consumption the identifier for consumption data in the \code{LedgerSide} column (a string).
#'        Default is "\code{Consumption}".
#' @param err the name of the error column in the output. Default is "\code{.err}".
#' @param tol the maximum amount by which Supply and Consumption can be out of balance
#'
#' @return a data frame containing with grouping variables and
#'         an additional column whose name is the value of \code{err}.
#'         The \code{err} column should be 0.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' UKEnergy2000tidy %>%
#'   filter(LastStage %in% c("Final", "Useful")) %>%
#'   group_by(Country, Year, EnergyType, LastStage) %>%
#'   verify_IEATable_energy_balance(energy = IEATools::iea_cols$e_dot)
verify_IEATable_energy_balance <- function(.ieatidydata,
                                           # Input column names
                                           LedgerSide = IEATools::iea_cols$ledger_side,
                                           energy = IEATools::iea_cols$e_dot,
                                           # ledger side identifiers
                                           supply = "Supply",
                                           consumption = "Consumption",
                                           # Output column names
                                           err = ".err",
                                           # Tolerance
                                           tol = 1e-6){
  LedgerSide <- as.name(LedgerSide)
  energy <- as.name(energy)
  err <- as.name(err)
  esupply <- as.name("ESupply")
  econsumption <- as.name("EConsumption")

  EnergyCheck <- dplyr::full_join(
    .ieatidydata %>%
      dplyr::filter(!!LedgerSide == "Supply") %>%
      dplyr::summarise(!!esupply := sum(!!energy)),
    .ieatidydata %>%
      dplyr::filter(!!LedgerSide == "Consumption") %>%
      dplyr::summarise(!!econsumption := sum(!!energy)),
    by = dplyr::group_vars(.ieatidydata)
  ) %>%
    dplyr::mutate(
      !!err := !!esupply - !!econsumption
    )

  # There are two options here.
  # (a) the Product appears on both Supply and Demand sides
  #     and, therefore, has a value for err
  # (b) the Product appears only on Supply side
  #     (because it is completely transformed
  #     before reaching the Consumption side of the ledger)
  #     and, therefore, has err = NA
  # If (a), then err should be zero (within tol).
  # If (b), then ESupply.ktoe should be zero (within tol).
  # Check that both of these are true.

  # Option (a)
  EnergyCheck_err <- EnergyCheck %>%
    dplyr::filter(!is.na(!!as.name(err)))
  assertthat::assert_that(all(abs(EnergyCheck_err[[err]]) < tol),
                          msg = "Energy not balanced in verify_IEATable_energy_balance.")

  # Option (b)
  EnergyCheck_supply <- EnergyCheck %>%
    dplyr::filter(is.na(!!as.name(err)))
  assertthat::assert_that(all(abs(EnergyCheck_supply[[esupply]]) < tol),
                          msg = paste("Energy not balanced in verify_IEATable_energy_balance."))

  return(EnergyCheck)
}



#' Endogenize losses into PSUT matrices
#'
#' When a conversion chain does _not_ include losses in the
#' **RUVY** matrices of the PSUT framework,
#' it may be helpful to endogenize the losses.
#' This function performs the endogenization.
#'
#' Intra-industry balances need to be calculated
#' (most easily via [calc_intra_industry_balance()])
#' prior to calling this function.
#' See the example.
#'
#' @param .sutmats A `matsindf` data frame, wide by matrices.
#' @param V Make (**V**) matrix or name of the column in `.sutmats`
#'          that contains same.
#'          Default is "V".
#' @param Y Final demand (**Y**) matrix or name
#'          of the column in `.sutmats` that contains same.
#'          Default is "Y".
#' @param loss_product The string name of the loss product.
#'                     Default is [Recca::balance_cols]`$waste_heat` or
#'                     "`r Recca::balance_cols$waste_heat`".
#' @param loss_sector The string name of the sector
#'                    that will absorb losses.
#'                    Default is [Recca::balance_cols]`$losses_sector` or
#'                    "`r Recca::balance_cols$losses_sector`".
#' @param replace_cols A boolean that tells whether to
#'                     (a) replace
#'                         the `V` and `Y` columns with
#'                         `V_prime` and `Y_prime` columns, respectively and
#'                     (b) delete the `V_prime` and `Y_prime`
#'                         columns
#'                     after endogenizing the losses
#'                     when `.sutmats` is a data frame or a list.
#'                     Default is `FALSE`.
#' @param balance_colname The name of the column containing
#'                        energy balance vectors.
#'                        Default is
#'                        [Recca::balance_cols]`$intra_industry_balance_colname` or
#'                        "`r Recca::balance_cols$intra_industry_balance_colname`".
#' @param V_prime The name of the **V** matrix with endogenized losses.
#' @param Y_prime The name of the **Y** matrix with endogenized losses.
#'
#' @returns A version of `.sutmats` with losses endogenized.
#'
#' @export
#'
#' @examples
#' mats <- UKEnergy2000mats |>
#'   tidyr::pivot_wider(names_from = matrix.name,
#'                      values_from = matrix) |>
#'   dplyr::filter(.data[[IEATools::iea_cols$last_stage]] %in%
#'                   c(IEATools::last_stages$final, IEATools::last_stages$useful)) |>
#'   calc_intra_industry_balance()
#' mats |>
#'   endogenize_losses() |>
#'   dplyr::glimpse()
#'   # Replace original matrices with endogenized matrices=
#' mats |>
#'   endogenize_losses(replace_cols = TRUE) |>
#'   dplyr::glimpse()
endogenize_losses <- function(.sutmats = NULL,
                              V = "V",
                              Y = "Y",
                              loss_product = Recca::balance_cols$waste_heat,
                              loss_sector = Recca::balance_cols$losses_sector,
                              replace_cols = FALSE,
                              balance_colname = Recca::balance_cols$intra_industry_balance_colname,
                              # Output columns
                              V_prime = "V_prime",
                              Y_prime = "Y_prime") {

  endogenize_func <- function(V_mat, Y_mat, balance_vec) {
    add_to_V <- balance_vec |>
      matsbyname::setcolnames_byname(loss_product)
    V_prime_mat <- matsbyname::sum_byname(V_mat, add_to_V)
    add_to_Y <- balance_vec |>
      matsbyname::transpose_byname() |>
      matsbyname::sumall_byname() |>
      matsbyname::setrownames_byname(loss_product) |>
      matsbyname::setcolnames_byname(loss_sector)
    Y_prime_mat <- matsbyname::sum_byname(Y_mat, add_to_Y)
    list(V_prime_mat, Y_prime_mat) |>
      magrittr::set_names(c(V_prime, Y_prime))
  }
  out <- matsindf::matsindf_apply(.sutmats,
                                  FUN = endogenize_func,
                                  V_mat = V,
                                  Y_mat = Y,
                                  balance_vec = balance_colname)
  if ((is.data.frame(.sutmats) | is.list(.sutmats)) & replace_cols) {
    out <- out |>
      dplyr::mutate(
        "{V}" := .data[[V_prime]],
        "{Y}" := .data[[Y_prime]],
        "{V_prime}" := NULL,
        "{Y_prime}" := NULL,
        "{balance_colname}" := NULL
      )
  }
  return(out)
}
