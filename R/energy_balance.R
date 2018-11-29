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
#'        including \code{U}, \code{V}, and \code{Y} columns.
#' @param U use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".
#' @param V make (\code{V}) matrix or name of the column in \code{.sutmats}that contains same. Default is "\code{V}".
#' @param Y final demand (\code{Y}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{Y}".
#' @param SUT_energy_balance the name for booleans telling if energy is in balance. Default is "\code{.SUT_energy_balance}".
#' @param tol the maximum amount by which Supply and Consumption can be out of balance.
#'        Default is \code{1e-6}.
#'
#' @return a list or data frame saying whether \code{.sutmats} are in balance.
#'
#' @importFrom matsbyname complete_rows_cols
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' verify_SUT_energy_balance(UKEnergy2000mats %>%
#'                             filter(Last.stage %in% c("final", "useful")) %>%
#'                             spread(key = matrix.name, value = matrix),
#'                           tol = 1e-4)
verify_SUT_energy_balance <- function(.sutmats = NULL,
                                      # Input names
                                      U = "U", V = "V", Y = "Y",
                                      # Tolerance
                                      tol = 1e-6,
                                      # Output name
                                      SUT_energy_balance = ".SUT_energy_balance"){
  verify_func <- function(U_mat, V_mat, Y_mat){
    U_sums <- rowsums_byname(U_mat)
    V_sums <- transpose_byname(V_mat) %>% rowsums_byname()
    Y_sums <- rowsums_byname(Y_mat)
    err <- difference_byname(V_sums, U_sums) %>% difference_byname(Y_sums)
    OK <- err %>% iszero_byname(tol) %>% as.logical()
    if (!OK) {
      return(list(FALSE) %>% magrittr::set_names(SUT_energy_balance))
    }
    list(TRUE) %>% magrittr::set_names(SUT_energy_balance)
  }
  Out <- matsindf_apply(.sutmats, FUN = verify_func, U_mat = U, V_mat = V, Y_mat = Y)
  if (!all(Out[[SUT_energy_balance]] %>% as.logical())) {
    warning(paste("Energy not conserved in verify_SUT_energy_balance. See", SUT_energy_balance))
  }
  return(Out)
}


#' Confirm that an SUT-style data frame conserves energy.
#'
#' If energy is in balance for every row, \code{.sutmats} is returned with two additional columns, and
#' execution returns to the caller.
#' If energy balance is not observed for one or more rows,
#' a warning is emitted, and
#' columns named \code{SUT_prod_energy_blance} and \code{SUT_ind_energy_blance} are added to \code{.sutmats}.
#' \code{FALSE} indicates energy is not in balance.
#'
#' This function should be called
#' for its side-effect of testing whether energy is in balance in \code{.sutmats}.
#'
#' Both product and industry energy balance are verified.
#' Units (as supplied by the \code{S_units} matrix) are respected.
#'
#' @param .sutmats an SUT-style data frame containing columns
#' \code{U}, \code{V}, \code{Y}, and \code{S_units}.
#' @param U use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".
#' @param V make (\code{V}) matrix or name of the column in \code{.sutmats}that contains same. Default is "\code{V}".
#' @param Y final demand (\code{Y}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{Y}".
#' @param S_units \code{S_units} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{S_units}".
#' @param tol the maximum amount by which energy can be out of balance. Default is \code{1e-6}.
#' @param SUT_prod_energy_balance the name for booleans telling if product energy is in balance. Default is "\code{.SUT_prod_energy_balance}".
#' @param SUT_ind_energy_balance the name for booleans telling if product energy is in balance. Default is "\code{.SUT_inds_energy_balance}".
#'
#' @return \code{.sutmats} with additional columns.
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' verify_SUT_energy_balance_with_units(UKEnergy2000mats %>%
#'                                        spread(key = matrix.name, value = matrix))
verify_SUT_energy_balance_with_units <- function(.sutmats = NULL,
                                                 # Input names
                                                 U = "U", V = "V", Y = "Y", S_units = "S_units",
                                                 # Tolerance
                                                 tol = 1e-6,
                                                 # Output names
                                                 SUT_prod_energy_balance = ".SUT_prod_energy_balance",
                                                 SUT_ind_energy_balance = ".SUT_ind_energy_balance"){
  verify_func <- function(U, V, Y, S_units){
    y <- rowsums_byname(Y)
    W <- difference_byname(transpose_byname(V), U)
    U_bar <- matrixproduct_byname(transpose_byname(S_units), U)
    V_bar <- matrixproduct_byname(V, S_units)
    W_bar <- matrixproduct_byname(transpose_byname(S_units), W)
    prodOK <- difference_byname(rowsums_byname(W), y) %>% iszero_byname(tol = tol)
    indOK <- difference_byname(V_bar, transpose_byname(W_bar)) %>% difference_byname(transpose_byname(U_bar)) %>% iszero_byname(tol = tol)
    list(prodOK, indOK) %>% set_names(c(SUT_prod_energy_balance, SUT_ind_energy_balance))
  }
  Out <- matsindf_apply(.sutmats, FUN = verify_func, U = U, V = V, Y = Y, S_units = S_units)
  if (!all(Out[[SUT_prod_energy_balance]] %>% as.logical())) {
    warning(paste("Energy not conserved by product in verify_SUT_energy_balance_with_units. See column", SUT_prod_energy_balance))
  }
  if (!all(Out[[SUT_ind_energy_balance]] %>% as.logical())) {
    warning(paste("Energy not conserved by industry in verify_SUT_energy_balance_with_units. See column", SUT_ind_energy_balance))
  }
  return(Out)
}


#' Confirm that all Industries in an SUT-style data frame produce energy.
#'
#' If a transformation process industry consumes energy (in the \code{U} matrix)
#' but does not make energy (in the \code{V} matrix),
#' it is most certainly an error.
#' (In contrast, there can be Industries that make energy but do not consume it,
#' such as Industries involved in Production.
#' And final demand sectors consume energy but do not produce any.)
#' This function emits a warning if an Industry in the \code{U} matrix
#' is found to consume energy but not make energy.
#' Look at the \code{industry_production_OK} column of the output to see which rows of
#' \code{.sutmats} exhibit the problem.
#' Look at the \code{problem_industries} column of the output to see which industries
#' exhibit this problem.
#'
#' @param .sutmats an SUT-style data frame containing metadata columns
#' (typically \code{Country}, \code{Year}, \code{Ledger.side}, \code{Product}, etc.)
#' and columns of SUT matrices, including \code{U} and \code{V}.
#' @param U use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".
#' @param V make (\code{V}) matrix or name of the column in \code{.sutmats}that contains same. Default is "\code{V}".
#' @param industry_production_OK the name of the column in the output that
#'        tells whether all industries produce something. Default is "\code{.industry_production_OK}".
#' @param problem_industries the name of the column in the output that
#'        tells which transformation processes consume energy but do not produce anything.
#'
#' @return \code{.sutmats} with added column named with the value of \code{industry_production_OK}.
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' verify_SUT_industry_production(UKEnergy2000mats %>%
#'                                  spread(key = matrix.name, value = matrix))
verify_SUT_industry_production <- function(.sutmats = NULL,
                                           # Input column names
                                           U = "U", V = "V",
                                           # Output column names
                                           industry_production_OK = ".industry_production_OK",
                                           problem_industries = ".problem_industries"){
  verify_func <- function(U_mat, V_mat){
    check <- rowsums_byname(V_mat) %>% complete_rows_cols(mat = transpose_byname(U_mat), margin = 1)
    OK <- !any(check == 0)
    problems <- rownames(check)[which(check == 0)]
    list(OK, problems) %>% set_names(c(industry_production_OK, problem_industries))
  }
  Out <- matsindf_apply(.sutmats, FUN = verify_func, U_mat = U, V_mat = V)
  if (!all(Out[[industry_production_OK]] %>% as.logical())) {
    warning(paste("There are some industries that consume but do not produce energy. See column", industry_production_OK))
  }
  return(Out)
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
#'   filter(Last.stage %in% c("final", "useful")) %>%
#'   group_by(Country, Year, Energy.type, Last.stage) %>%
#'   verify_IEATable_energy_balance(energy = "EX.ktoe")
verify_IEATable_energy_balance <- function(.ieatidydata,
                                           # Input column names
                                           ledger.side = "Ledger.side",
                                           energy = "E.ktoe",
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

  EnergyCheck <- full_join(
    .ieatidydata %>%
      filter(!!ledger.side == "Supply") %>%
      summarise(!!esupply := sum(!!energy)),
    .ieatidydata %>%
      filter(!!ledger.side == "Consumption") %>%
      summarise(!!econsumption := sum(!!energy)),
    by = group_vars(.ieatidydata)
  ) %>%
    mutate(
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
  EnergyCheck_err <- EnergyCheck %>% filter(!is.na(!!as.name(err)))
  if (!all(abs(EnergyCheck_err[[err]]) < tol)) {
    # Emit a warning
    warning(
      paste("Energy not balanced in verify_IEATable_energy_balance.",
            "Check return value for non-zero", err, "column.")
    )
  }

  # Option (b)
  EnergyCheck_supply <- EnergyCheck %>% filter(is.na(!!as.name(err)))
  if (!all(abs(EnergyCheck_supply[[esupply]]) < tol)) {
    # Emit a warning
    warning(
      paste("Energy not balanced in verify_IEATable_energy_balance.",
            "Check return value for non-zero", err, "column.")
    )
  }

  return(EnergyCheck)
}
