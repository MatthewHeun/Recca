#
# This file contains functions that verify energy balance
# on ECC representations.
#


#' Confirm that an SUT-style data frame conserves energy.
#'
#' Energy balances are confirmed by Product (within \code{tol}) for every row in .sutdata.
#'
#' If energy is in balance for every row, \code{sutdata} is returned unmodified, and
#' execution returns to the caller.
#' If energy balance is not observed for any row,
#' a warning is emitted, and
#' a column named \code{SUT_energy_blance} is added to \code{.sutdata}
#' wherein \code{TRUE} indicates energy balance is observed
#' and \code{FALSE} indicates energy is not in balance.
#' This function should be called
#' for its side-effect of testing whether energy is in balance in \code{.sutdata}.
#'
#' @param .sutdata an SUT-style data frame with columns of matrices, including
#' \code{U}, \code{V}, and \code{Y}.
#' @param U_colname the name of the column that contains \code{U} matrices
#' @param V_colname the name of the column that contains \code{V} matrices
#' @param Y_colname the name of the column that contsins \code{Y} matrices
#' @param tol the maximum amount by which Supply and Consumption can be out of balance
#'
#' @return \code{.sutdata}. If energy balance is not observed,
#' an additional column is added showing the row on which energy is not balanced.
#'
#' @export
#'
#' @examples
#' verify_SUT_energy_balance(SUTMatsWne)
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
  Out
}


#' Confirm that an SUT-style data frame conserves energy.
#'
#' Energy balances are confirmed by Product (within \code{tol}) for every row in .sutdata.
#' If energy is in balance for every row, no message is given, and
#' execution returns to the caller.
#' If energy balance is not observed for any combination of Country, Year, and Product, etc.,
#' a message is printed which shows the first few non-balancing Products, and
#' execution halts.
#'
#' Both product and industry energy balance are verified.
#' Units (as supplied by the \code{S_units} matrix) are respected.
#'
#' @param .sutdata an SUT-style data frame containing metadata columns
#' (typically \code{Country}, \code{Year}, etc.)
#' and columns of matrices, including
#' \code{U}, \code{V}, \code{Y}, and \code{S_units}.
#' @param U the name of the column that contains \code{U} matrices
#' @param V the name of the column that contains \code{V} matrices
#' @param Y the name of the column that contains \code{Y} matrices
#' @param S_units the name of the column that contains \code{S_units} matrices
#' @param tol the maximum amount by which energy can be out of balance
#'
#' @return Nothing is returned.
#' This function should be called
#' for its side-effect of testing whether energy is in balance in \code{.sutdata}.
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
    list(prodOK, indOK) %>% set_names(SUT_prod_energy_balance, SUT_ind_energy_balance)
  }
  Out <- matsindf_apply(.sutdata, FUN = verify_func, U = U, V = V, Y = Y, S_units = S_units)
  if (!all(Out[[SUT_prod_energy_balance]] %>% as.logical())) {
    warning(paste("Energy not conserved in verify_SUT_energy_balance_with_units. See column", SUT_prod_energy_balance))
  }
  if (!all(Out[[SUT_ind_energy_balance]] %>% as.logical())) {
    warning(paste("Energy not conserved in verify_SUT_energy_balance_with_units. See column", SUT_ind_energy_balance))
  }
  Out

  # # Input columns
  # U <- as.name(U)
  # V <- as.name(V)
  # Y <- as.name(Y)
  # S_units <- as.name(S_units)
  # # Intermediate columns
  # y <- as.name(".y")
  # W <- as.name(".W")
  # U_bar <- as.name(".U_bar")
  # V_bar <- as.name(".V_bar")
  # W_bar <- as.name(".W_bar")
  # err_prod <- as.name(".err_prod")
  # err_ind <- as.name(".err_ind")
  #
  # verify_cols_missing(.sutdata, c(W, V_bar, U_bar, W_bar, y, err_prod, err_ind))
  #
  # EnergyCheck <- .sutdata %>%
  #   mutate(
  #     !!y := rowsums_byname(!!Y),
  #     !!W := difference_byname(transpose_byname(!!V), !!U),
  #     !!U_bar := matrixproduct_byname(transpose_byname(!!S_units), !!U),
  #     !!V_bar := matrixproduct_byname(!!V, !!S_units),
  #     !!W_bar := matrixproduct_byname(transpose_byname(!!S_units), !!W),
  #     !!err_prod := difference_byname(rowsums_byname(!!W), !!y),
  #     !!err_ind := difference_byname(!!V_bar, transpose_byname(!!W_bar)) %>% difference_byname(transpose_byname(!!U_bar))
  #   )
  #
  # if (!all(EnergyCheck[[err_prod]] %>% iszero_byname(tol) %>% as.logical())) {
  #   # Print an error message containing rows of EnergyCheck that cause the failure
  #   print("Energy balance not obtained in verify_SUT_energy_balance_with_units")
  #   print(paste(err_prod, "should be zero."))
  #   stop()
  # }
  #
  # if (!all(EnergyCheck[[err_ind]] %>% iszero_byname(tol) %>% as.logical())) {
  #   # Print an error message containing rows of EnergyCheck that cause the failure
  #   print("Energy balance not obtained in verify_SUT_energy_balance_with_units")
  #   print(paste(err_prod, "should be zero."))
  #   stop()
  # }
  #
  # # Wrap return value (NULL) with invisible() to prevent printing of the result.
  # invisible(NULL)
}


#' Confirm that all Industries in an SUT-style data frame produce energy.
#'
#' If an Industry consumes energy (in the \strong{U} matrix)
#' but does not make energy (in the \strong{V} matrix),
#' it is most certainly an error.
#' (In contrast, there can be Industries that make energy but do not consume it,
#' such as Industries involved in Production.)
#' This function errors if an Industry is found that consumes energy but does not make energy.
#' But it will also place an object named \code{problems_data_frame_name}
#' into the global environment.
#' \code{problems_data_frame_name} can assist debugging this problem
#' First, look at the \code{industry_production_OK_colname} column of \code{problems_data_frame_name}.
#' It shows which rows of \code{.sutdata} had a problem (FALSE in that column).
#' If you find a row with a problem, look at the \code{check_colname} column.
#' Any \code{0} entries indicate industries that consume energy but do not make energy.
#'
#' @param .sutdata an SUT-style data frame containing metadata columns
#' (typically \code{Country}, \code{Year}, \code{Ledger.side}, \code{Product}, etc.)
#' and columns of SUT matrices, including \code{U} and \code{V}.
#' @param U_colname the name of the column of use matrices
#' @param V_colname the name of the column of make matrices
#' @param check_colname the name of a column of intermediate results
#' that contains \bold{V}*i (rowsums of \bold{V}) completed against \bold{U}^T on rows.
#' @param problems_data_frame_name the name of the data frame created
#' in the event that any problems are discovered
#'
#' @return Nothing is returned.
#' This function should be called
#' for its side-effect of testing whether energy is in balance in \code{.sutdata}.
#'
#' @export
#'
#' @examples
#' verify_SUT_industry_production(UKEnergy2000mats)
verify_SUT_industry_production <- function(.sutdata = NULL,
                                           # Input column names
                                           U_colname = "U", V_colname = "V"){
  # Establish names
  U <- as.name(U_colname)
  V <- as.name(V_colname)
  # Establish intermediate column names
  check <- as.name(".check")
  OK <- as.name(".industry_production_OK")

  verify_cols_missing(.sutdata, c(OK, check))

  IndustryCheck <- .sutdata %>%
    mutate(
      !!check := rowsums_byname(!!V) %>% complete_rows_cols(mat = transpose_byname(!!U), margin = 1),
      !!OK := lapply(!!check, FUN = function(v){
        # v is one of the vectors in the .check column of IndustryCheck
        # If any of the elements in the v vector is zero, we have encountered an error.
        !any(v == 0)
      })
    )

  if (!(all(IndustryCheck[[OK]] %>% as.logical))) {
    # We have a problem.
    stop("There are some industries that consume but do not produce energy.")
  }
  invisible(NULL)
}
