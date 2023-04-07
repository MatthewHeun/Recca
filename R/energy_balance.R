#
# This file contains functions that verify energy balance
# on ECC representations.
#


#' Confirm that an SUT-style data frame conserves energy.
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
#'        including `U`, `V`, and `Y` columns.
#' @param R resources (`R`) matrix or name of the column in `.sutmats` that contains same. Default is "R".
#' @param U use (`U`) matrix or name of the column in `.sutmats` that contains same. Default is "U".
#' @param V make (`V`) matrix or name of the column in `.sutmats`that contains same. Default is "V".
#' @param Y final demand (`Y`) matrix or name of the column in `.sutmats` that contains same. Default is "Y".
#' @param SUT_energy_balance the name for booleans telling if energy is in balance. Default is ".SUT_energy_balance".
#' @param tol the maximum amount by which Supply and Consumption can be out of balance.
#'        Default is `1e-6`.
#'
#' @return a list or data frame saying whether `.sutmats` are in balance.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' verify_SUT_energy_balance(UKEnergy2000mats %>%
#'                             dplyr::filter(Last.stage %in% c("Final", "Useful")) %>%
#'                             tidyr::spread(key = matrix.name, value = matrix),
#'                           tol = 1e-4)
verify_SUT_energy_balance <- function(.sutmats = NULL,
                                      # Input names
                                      R = "R", U = "U", V = "V", Y = "Y",
                                      # Tolerance
                                      tol = 1e-6,
                                      # Output name
                                      SUT_energy_balance = ".SUT_energy_balance"){
  verify_func <- function(R_mat = NULL, U_mat, V_mat, Y_mat){
    if (is.null(R_mat)) {
      # No R matrix, just use the V matrix, assuming that resouces are included there.
      R_plus_V_mat <- V_mat
    } else {
      # An R matrix is present. Sum R and V before proceeding.
      R_plus_V_mat <- matsbyname::sum_byname(R_mat, V_mat)
    }
    RV_sums <- matsbyname::transpose_byname(R_plus_V_mat) %>% matsbyname::rowsums_byname()
    U_sums <- matsbyname::rowsums_byname(U_mat)
    Y_sums <- matsbyname::rowsums_byname(Y_mat)
    # (R + V) - U - Y
    err <- matsbyname::difference_byname(RV_sums, U_sums) %>% matsbyname::difference_byname(Y_sums)
    OK <- err %>%
      matsbyname::iszero_byname(tol) %>%
      as.logical()
    if (!OK) {
      return(list(FALSE) %>% magrittr::set_names(SUT_energy_balance))
    }
    list(TRUE) %>% magrittr::set_names(SUT_energy_balance)
  }

  Out <- matsindf::matsindf_apply(.sutmats, FUN = verify_func, R_mat = R, U_mat = U, V_mat = V, Y_mat = Y)
  if (!all(Out[[SUT_energy_balance]] %>% as.logical())) {
    warning(paste("Energy not conserved in verify_SUT_energy_balance. See", SUT_energy_balance))
  }
  return(Out)
}


#' Confirm that an SUT-style data frame conserves energy.
#'
#' If energy is in balance for every row, `.sutmats` is returned with two additional columns, and
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
#' @param .sutmats an SUT-style data frame containing columns
#' `R` (optionally), `U`, `V`, `Y`, and `S_units`.
#' @param R resource (`R`) matrix or name of the column in `.sutmats` that contains same. Default is "R".
#' @param U use (`U`) matrix or name of the column in `.sutmats` that contains same. Default is "U".
#' @param V make (`V`) matrix or name of the column in `.sutmats`that contains same. Default is "V".
#' @param Y final demand (`Y`) matrix or name of the column in `.sutmats` that contains same. Default is "Y".
#' @param S_units `S_units` matrix or name of the column in `.sutmats` that contains same. Default is "S_units".
#' @param tol the maximum amount by which energy can be out of balance. Default is `1e-6`.
#' @param SUT_prod_energy_balance the name for booleans telling if product energy is in balance. Default is ".SUT_prod_energy_balance".
#' @param SUT_ind_energy_balance the name for booleans telling if product energy is in balance. Default is ".SUT_inds_energy_balance".
#'
#' @return `.sutmats` with additional columns.
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' verify_SUT_energy_balance_with_units(UKEnergy2000mats %>%
#'                                        tidyr::spread(key = matrix.name, value = matrix),
#'                                        tol = 1e-3)
verify_SUT_energy_balance_with_units <- function(.sutmats = NULL,
                                                 # Input names
                                                 R = "R", U = "U", V = "V", Y = "Y", S_units = "S_units",
                                                 # Tolerance
                                                 tol = 1e-6,
                                                 # Output names
                                                 SUT_prod_energy_balance = ".SUT_prod_energy_balance",
                                                 SUT_ind_energy_balance = ".SUT_ind_energy_balance"){
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
    prodOK <- matsbyname::difference_byname(matsbyname::rowsums_byname(W), y) %>% matsbyname::iszero_byname(tol = tol)
    indOK <- matsbyname::difference_byname(RV_bar, matsbyname::transpose_byname(W_bar)) %>%
      matsbyname::difference_byname(matsbyname::transpose_byname(U_bar)) %>% matsbyname::iszero_byname(tol = tol)
    list(prodOK, indOK) %>% magrittr::set_names(c(SUT_prod_energy_balance, SUT_ind_energy_balance))
  }
  Out <- matsindf::matsindf_apply(.sutmats, FUN = verify_func, R = R, U = U, V = V, Y = Y, S_units = S_units)
  assertthat::assert_that(all(Out[[SUT_prod_energy_balance]] %>% as.logical()),
                          msg = paste("Energy not conserved by product in verify_SUT_energy_balance_with_units.",
                                      "See column",
                                      SUT_prod_energy_balance))
  assertthat::assert_that(all(Out[[SUT_ind_energy_balance]] %>% as.logical()),
                          msg = paste("Energy not conserved by industry in verify_SUT_energy_balance_with_units",
                                      "See column",
                                      SUT_ind_energy_balance))
  return(Out)
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
#' (typically `Country`, `Year`, `Ledger.side`, `Product`, etc.)
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
#'        a \code{Ledger.side} column, and
#'        an energy column (\code{E.ktoe}).
#'        \code{.ieatidydata} should be grouped prior to sending to this function.
#' @param ledger.side the name of the column in \code{.ieatidydata}
#'        that contains ledger side information (a string). Default is "\code{Ledger.side}".
#' @param energy the name of the column in \code{.ieatidydata}
#'        that contains energy data (a string). Default is "\code{E.ktoe}".
#' @param supply the identifier for supply data in the \code{ledger.side} column (a string).
#'        Default is "\code{Supply}".
#' @param consumption the identifier for consumption data in the \code{ledger.side} column (a string).
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
#'   filter(Last.stage %in% c("Final", "Useful")) %>%
#'   group_by(Country, Year, Energy.type, Last.stage) %>%
#'   verify_IEATable_energy_balance(energy = "E.dot")
verify_IEATable_energy_balance <- function(.ieatidydata,
                                           # Input column names
                                           ledger.side = "Ledger.side",
                                           energy = "E.dot",
                                           # ledger.side identifiers
                                           supply = "Supply",
                                           consumption = "Consumption",
                                           # Output column names
                                           err = ".err",
                                           # Tolerance
                                           tol = 1e-6){
  ledger.side <- as.name(ledger.side)
  energy <- as.name(energy)
  err <- as.name(err)
  esupply <- as.name("ESupply")
  econsumption <- as.name("EConsumption")

  EnergyCheck <- dplyr::full_join(
    .ieatidydata %>%
      dplyr::filter(!!ledger.side == "Supply") %>%
      dplyr::summarise(!!esupply := sum(!!energy)),
    .ieatidydata %>%
      dplyr::filter(!!ledger.side == "Consumption") %>%
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
