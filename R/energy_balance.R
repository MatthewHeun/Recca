#
# This file contains functions that verify energy balance
# on ECC representations.
#


#' Confirm that an SUT-style data frame conserves energy.
#'
#' Energy balances are confirmed by Product (within \code{tol}) for every row in \code{.sutdata}.
#'
#' If energy is in balance for every row, \code{.sutdata} is returned with an additional column, and
#' execution returns to the caller.
#' If energy balance is not observed for one or more of the rows,
#' a warning is emitted, and
#' the additional column (\code{SUT_energy_blance})
#' indicates where the problem occurred, with \code{FALSE}
#' showing where energy is not in balance.
#'
#' This function should be called
#' for its side-effect of testing whether energy is in balance in \code{.sutdata}.
#'
#' @param .sutdata an SUT-style data frame with columns of matrices,
#'        including \code{U}, \code{V}, and \code{Y} columns.
#' @param U_colname the name of the column that contains \code{U} matrices. Default is "\code{U}".
#' @param V_colname the name of the column that contains \code{V} matrices. Default is "\code{V}".
#' @param Y_colname the name of the column that contsins \code{Y} matrices. Default is "\code{Y}".
#' @param SUT_energy_balance the name of the output column. Default is "\code{.SUT_energy_balance}".
#' @param tol the maximum amount by which Supply and Consumption can be out of balance.
#'        Default is \code{1e-6}.
#'
#' @return \code{.sutdata}. If energy balance is not observed,
#'         an additional column is added showing the row on which energy is not balanced.
#'
#' @importFrom matsbyname complete_rows_cols
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' library(magrittr)
#' verify_SUT_energy_balance(UKEnergy2000mats %>%
#'                             filter(Last.stage %in% c("final", "useful")) %>%
#'                             spread(key = matrix.name, value = matrix),
#'                           tol = 1e-4)
verify_SUT_energy_balance <- function(.sutdata = NULL,
                                      # Input column names
                                      U_colname = "U", V_colname = "V", Y_colname = "Y",
                                      # Tolerance
                                      tol = 1e-6,
                                      # Output column name
                                      SUT_energy_balance = ".SUT_energy_balance"){
  verify_func <- function(U, V, Y){
    V_sums <- transpose_byname(V) %>% rowsums_byname()
    U_sums <- rowsums_byname(U)
    Y_sums <- rowsums_byname(Y)
    err <- difference_byname(V_sums, U_sums) %>% difference_byname(Y_sums)
    OK <- err %>% iszero_byname(tol) %>% as.logical()
    if (!OK) {
      return(list(FALSE) %>% set_names(SUT_energy_balance))
    }
    list(TRUE) %>% set_names(SUT_energy_balance)
  }
  Out <- matsindf_apply(.sutdata, FUN = verify_func, U = U_colname, V = V_colname, Y = Y_colname)
  if (!all(Out[[SUT_energy_balance]] %>% as.logical())) {
    warning(paste("Energy not conserved in verify_SUT_energy_balance. See column", SUT_energy_balance))
  }
  return(Out)
}


#' Confirm that an SUT-style data frame conserves energy.
#'
#' If energy is in balance for every row, \code{.sutdata} is returned with two additional columns, and
#' execution returns to the caller.
#' If energy balance is not observed for one or more rows,
#' a warning is emitted, and
#' columns named \code{SUT_prod_energy_blance} and \code{SUT_ind_energy_blance} are added to \code{.sutdata}.
#' \code{FALSE} indicates energy is not in balance.
#'
#' This function should be called
#' for its side-effect of testing whether energy is in balance in \code{.sutdata}.
#'
#' Both product and industry energy balance are verified.
#' Units (as supplied by the \code{S_units} matrix) are respected.
#'
#' @param .sutdata an SUT-style data frame containing columns
#' \code{U}, \code{V}, \code{Y}, and \code{S_units}.
#' @param U the name of the column that contains \code{U} matrices
#' @param V the name of the column that contains \code{V} matrices
#' @param Y the name of the column that contains \code{Y} matrices
#' @param S_units the name of the column that contains \code{S_units} matrices
#' @param tol the maximum amount by which energy can be out of balance
#' @param SUT_prod_energy_balance the name of the output column that tells whether product balance is OK. Default is "\code{.SUT_prod_energy_balance}"
#' @param SUT_ind_energy_balance the name of the output column that tells whether industry balance is OK. Default is "\code{.SUT_ind_energy_balance}"
#'
#' @return \code{.sutdata} with additional columns.
#'
#' @export
#'
#' @examples
#' verify_SUT_energy_balance_with_units(UKEnergy2000mats)
verify_SUT_energy_balance_with_units <- function(.sutdata = NULL,
                                                 # Input column names
                                                 U = "U", V = "V", Y = "Y", S_units = "S_units",
                                                 # Tolerance
                                                 tol = 1e-6,
                                                 # Output column names
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
  Out <- matsindf_apply(.sutdata, FUN = verify_func, U = U, V = V, Y = Y, S_units = S_units)
  if (!all(Out[[SUT_prod_energy_balance]] %>% as.logical())) {
    warning(paste("Energy not conserved in verify_SUT_energy_balance_with_units. See column", SUT_prod_energy_balance))
  }
  if (!all(Out[[SUT_ind_energy_balance]] %>% as.logical())) {
    warning(paste("Energy not conserved in verify_SUT_energy_balance_with_units. See column", SUT_ind_energy_balance))
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
#' \code{.sutdata} exhibit the problem.
#' Look at the \code{problem_industries} column of the output to see which industries
#' exhibit this problem.
#'
#' @param .sutdata an SUT-style data frame containing metadata columns
#' (typically \code{Country}, \code{Year}, \code{Ledger.side}, \code{Product}, etc.)
#' and columns of SUT matrices, including \code{U} and \code{V}.
#' @param U_colname the name of the column of use matrices. Default is "\code{U}".
#' @param V_colname the name of the column of make matrices. Default is "\code{V}".
#' @param industry_production_OK the name of the column in the output that
#'        tells whether all industries produce something. Default is "\code{.industry_production_OK}".
#' @param problem_industries the name of the column in the output that
#'        tells which transformation processes consume energy but do not produce anything.
#'
#' @return \code{.sutdata} with added column named with the value of \code{industry_production_OK}.
#'
#' @export
#'
#' @examples
#' verify_SUT_industry_production(UKEnergy2000mats)
verify_SUT_industry_production <- function(.sutdata = NULL,
                                           # Input column names
                                           U_colname = "U", V_colname = "V",
                                           # Output column names
                                           industry_production_OK = ".industry_production_OK",
                                           problem_industries = ".problem_industries"){
  verify_func <- function(U, V){
    check <- rowsums_byname(V) %>% complete_rows_cols(mat = transpose_byname(U), margin = 1)
    OK <- !any(check == 0)
    problems <- rownames(check)[which(check == 0)]
    list(OK, problems) %>% set_names(c(industry_production_OK, problem_industries))
  }
  Out <- matsindf_apply(.sutdata, FUN = verify_func, U = U_colname, V = V_colname)
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
