#' Calculate inter- and intra-industry balances for PSUT (**RUVY**) matrices2
#'
#' Balances are an important aspect of analyzing material and
#' energy conversion chains
#' in the PSUT framework with the **RUVY** matrices.
#' Often, balances are calculated on energy or exergy flows.
#' Balances can be calculated for mass flows or monetary flows, too.
#'
#' ## Inter-industry (between-industry) balances
#'
#' In a PSUT description of a material or energy conversion chain,
#' all of every product leaving one industry must arrive at another industry.
#' `calc_inter_industry_balances()`
#' calculates these between-industry balances via
#' ((**R** + **V**)^T^ - (**U** + **Y**))**i**.
#' Inter-industry balances are calculated for products (not industries).
#'
#' ## Intra-industry (across-industry) balances
#'
#' `calc_intra_industry_balances()` calculates across-industry
#' balances via
#' (**U**^T^ - **V**)**i**
#' (inputs - outputs).
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
#' The result should _always_ be the **0** vector,
#' regardless of the quantity represented by the matrices.
#'
#' In a PSUT description of a mass or energy conversion chain,
#' the meaning of intra-industry (across-industry) balances
#' depends on the construction of the matrices and the
#' quantity represented by the matrices.
#' When all losses are accounted in the matrices themselves,
#' the calculation of intra-industry balances for
#' conversion chains of conserved quantities
#' (e.g., mass, energy, money)
#' should give the **0** vector
#' with industries in rows.
#' When losses are _not_ accounted in the matrices,
#' the calculation of intra-industry balances gives losses.
#'
#' For conversion chains of unconserved quantities
#' (e.g., entropy and exergy),
#' when losses _are_ accounted in the matrices,
#' the intra-industry balance gives generation or destruction.
#' When losses _are not_ accounted in the matrices,
#' the intra-industry balance gives the sum of
#' generation, destruction and losses.
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
#' @seealso [verify_inter_industry_balance()],
#'          [verify_intra_industry_balance()], and
#'          [calc_yqfgW()] which calculates **W**,
#'          a matrix similar to the inter-industry balance vector.
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
                                        R = Recca::psut_cols$R,
                                        U = Recca::psut_cols$U,
                                        V = Recca::psut_cols$V,
                                        Y = Recca::psut_cols$Y,
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
                                        U = Recca::psut_cols$U,
                                        V = Recca::psut_cols$V,
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
                                           R = Recca::psut_cols$R,
                                           U = Recca::psut_cols$U,
                                           V = Recca::psut_cols$V,
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
