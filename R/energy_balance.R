#
# This file contains functions that verify energy balance
# on ECC representations.
#

#' Confirm that an IEA-style data frame conserves energy.
#'
#' Energy balances are confirmed (within \code{tol}) for every combination of
#' \code{group_vars(.ieatidydata)}.
#' If energy is in balance for every group of \code{.ieatidydata},
#' execution returns to the caller.
#' If energy balance is not observed for any group of \code{.ieatidydata},
#' a message is printed which shows the first few non-balancing Products, and
#' execution halts.
#'
#' Be sure to group \code{.ieatidydata} prior to calling this function,
#' as shown in the example.
#'
#' @param .ieatidydata an IEA-style data frame containing the following columns:
#' \code{Country}, \code{Year}, \code{Ledger.side}, \code{Product}, \code{E.ktoe}.
#' @param tol the maximum amount by which Supply and Consumption can be out of balance
#'
#' @return Nothing is returned.
#' This function should be called
#' for its side-effect of testing whether energy is in balance in \code{.ieatidydata}.
#'
#' @export
#'
#' @examples
#' verify_IEATable_energy_balance(IEAData_orig %>% group_by(Country, Year, Product))
verify_IEATable_energy_balance <- function(.ieatidydata, tol = 1e-6){
  EnergyCheck <- full_join(
    .ieatidydata %>%
      filter(Ledger.side == "Supply") %>%
      summarise(ESupply.ktoe = sum(E.ktoe)),
    .ieatidydata %>%
      filter(Ledger.side == "Consumption") %>%
      summarise(EConsumption.ktoe = sum(E.ktoe)),
    by = group_vars(.ieatidydata)
  ) %>%
    mutate(
      err = ESupply.ktoe - EConsumption.ktoe
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
  EnergyCheck_err <- EnergyCheck %>% filter(!is.na(err))
  if (!all(abs(EnergyCheck_err$err) < tol)) {
    # Print an error message containing rows of EnergyCheck_err that cause the failure
    print("Energy balance not obtained in verify_IEATable_energy_balance")
    print("err should be zero.")
    print("First non-balancing Products are shown below:")
    print(EnergyCheck_err %>% filter(abs(err) >= tol))
    stop()
  }

  # Option (b)
  EnergyCheck_supply <- EnergyCheck %>% filter(is.na(err))
  if (!all(abs(EnergyCheck_supply$ESupply.ktoe) < tol)) {
    # Print an error message containing rows of EnergyCheck_supply that cause the failure
    print("Energy balance not obtained in verify_IEATable_energy_balance")
    print("ESupply.ktoe should be zero.")
    print("First non-balancing Products are shown below:")
    print(EnergyCheck_supply %>% filter(abs(ESupply.ktoe) >= tol))
    stop()
  }

  # Wrap return value (NULL) with invisible() to prevent printing of the result.
  invisible(NULL)
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
#' @param .sutdata an SUT-style data frame containing metadata columns
#' (typically \code{Country}, \code{Year}, \code{Ledger.side}, \code{Product}, etc.)
#' and columns of matrices, including
#' \code{U}, \code{V}, and \code{Y}.
#' @param U_colname the name of the column that contains \strong{U} matrices
#' @param V_colname the name of the column that contains V matrices
#' @param Y_colname the name of the column that contsins Y matrices
#' @param err_colname the name of the column that contains (\strong{V}^T - \strong{U} - \strong{Y})*\strong{i}
#' (which should be zero to within \code{tol})
#' @param tol the maximum amount by which Supply and Consumption can be out of balance
#'
#' @return Nothing is returned.
#' This function should be called
#' for its side-effect of testing whether energy is in balance in \code{.sutdata}.
#'
#' @export
#'
#' @examples
#' verify_SUT_energy_balance(SUTMatsWne)
verify_SUT_energy_balance <- function(.sutdata,
                                      # Input column names
                                      U_colname = "U", V_colname = "V", Y_colname = "Y",
                                      # Tolerance
                                      tol = 1e-6,
                                      # Intermediate results column names
                                      V_sums_colname = ".V_sums",
                                      U_sums_colname = ".U_sums",
                                      Y_sums_colname = ".Y_sums",
                                      err_colname = "err"){
  EnergyCheck <- .sutdata %>%
    mutate_(
      .dots = list(
        # V_sums = rowsums(V^T)
        interp(~ vcol %>% transpose_byname() %>% rowsums_byname(),
               vcol = as.name(V_colname)),
        # U_sums = rowsums(U)
        interp(~ ucol %>% rowsums_byname(),
               ucol = as.name(U_colname)),
        # Y_sums = rowsums(Y)
        interp(~ ycol %>% rowsums_byname(),
               ycol = as.name(Y_colname))
      ) %>%
        setNames(c(V_sums_colname, U_sums_colname, Y_sums_colname))
    ) %>%
    mutate_(
      .dots = list(
        # V^T*i - U*i - Y*i should be the zero vector
        interp(~ vsums %>% difference_byname(usums) %>% difference_byname(ysums),
               vsums = as.name(V_sums_colname),
               usums = as.name(U_sums_colname),
               ysums = as.name(Y_sums_colname)
        )
      ) %>%
        setNames(c(err_colname))
    )

  if (!all(EnergyCheck$err %>% iszero_byname(tol) %>% as.logical())) {
    # Print an error message containing rows of EnergyCheck that cause the failure
    print("Energy balance not obtained in verify_SUT_energy_balance")
    print("err should be zero.")
    print("First non-balancing Products are shown below:")
    EnergyCheck %>%
      select(Country, Year, Last.stage, Energy.type, err) %>%
      mutate(
        matnames = "err"
      ) %>%
      rename(
        matrix.values = err
      ) %>%
      expand_to_tidy(matnames = "matnames", matvals = "matrix.values",
                     rownames = "Product", colnames = "col",
                     rowtypes = "rowtype", coltypes = "coltype", drop = 0) %>%
      select(-matnames, -col, -rowtype, -coltype) %>%
      rename(err.ktoe = matrix.values) %>%
      filter(abs(err.ktoe) >= tol) %>%
      print

    stop()
  }

  # Wrap return value (NULL) with invisible() to prevent printing of the result.
  invisible(NULL)
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
verify_SUT_energy_balance_with_units <- function(.sutdata,
                                      # Input column names
                                      U = "U", V = "V", Y = "Y", S_units = "S_units",
                                      # Tolerance
                                      tol = 1e-6){
  # Input columns
  U <- as.name(U)
  V <- as.name(V)
  Y <- as.name(Y)
  S_units <- as.name(S_units)
  # Intermediate column names
  W <- as.name(".W")
  V_bar <- as.name(".V_bar")
  U_bar <- as.name(".U_bar")
  W_bar <- as.name(".W_bar")
  y <- as.name(".y")
  err_prod <- as.name(".err_prod")
  err_ind <- as.name(".err_ind")

  EnergyCheck <- .sutdata %>%
    mutate(
      !!y := rowsums_byname(!!Y),
      !!W := difference_byname(transpose_byname(!!V), !!U),
      !!U_bar := matrixproduct_byname(transpose_byname(!!S_units), !!U),
      !!V_bar := matrixproduct_byname(!!V, !!S_units),
      !!W_bar := matrixproduct_byname(transpose_byname(!!S_units), !!W),
      !!err_prod := difference_byname(rowsums_byname(!!W), !!y),
      !!err_ind := difference_byname(!!V_bar, transpose_byname(!!W_bar)) %>% difference_byname(transpose_byname(!!U_bar))
    )
  EnergyCheck <- .sutdata %>%
    mutate_(
      .dots = list(
        # V_sums = rowsums(V^T)
        interp(~ vcol %>% transpose_byname() %>% rowsums_byname(),
               vcol = as.name(V_colname)),
        # U_sums = rowsums(U)
        interp(~ ucol %>% rowsums_byname(),
               ucol = as.name(U_colname)),
        # Y_sums = rowsums(Y)
        interp(~ ycol %>% rowsums_byname(),
               ycol = as.name(Y_colname))
      ) %>%
        setNames(c(V_sums_colname, U_sums_colname, Y_sums_colname))
    ) %>%
    mutate_(
      .dots = list(
        # V^T*i - U*i - Y*i should be the zero vector
        interp(~ vsums %>% difference_byname(usums) %>% difference_byname(ysums),
               vsums = as.name(V_sums_colname),
               usums = as.name(U_sums_colname),
               ysums = as.name(Y_sums_colname)
        )
      ) %>%
        setNames(c(err_colname))
    )

  if (!all(EnergyCheck$err %>% iszero_byname(tol) %>% as.logical())) {
    # Print an error message containing rows of EnergyCheck that cause the failure
    print("Energy balance not obtained in verify_SUT_energy_balance")
    print("err should be zero.")
    print("First non-balancing Products are shown below:")
    EnergyCheck %>%
      select(Country, Year, Last.stage, Energy.type, err) %>%
      mutate(
        matnames = "err"
      ) %>%
      rename(
        matrix.values = err
      ) %>%
      expand_to_tidy(matnames = "matnames", matvals = "matrix.values",
                     rownames = "Product", colnames = "col",
                     rowtypes = "rowtype", coltypes = "coltype", drop = 0) %>%
      select(-matnames, -col, -rowtype, -coltype) %>%
      rename(err.ktoe = matrix.values) %>%
      filter(abs(err.ktoe) >= tol) %>%
      print

    stop()
  }

  # Wrap return value (NULL) with invisible() to prevent printing of the result.
  invisible(NULL)
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
#' verify_SUT_industry_production(SUTMatsWne)
verify_SUT_industry_production <- function(.sutdata,
                                           # Input column names
                                           U_colname = "U", V_colname = "V",
                                           # Output column name
                                           industry_production_OK_colname = "industry production OK",
                                           # Intermediate results column names
                                           check_colname = ".check",
                                           problems_data_frame_name = "Problems"){
  IndustryCheck <- .sutdata %>%
    mutate_(
      .dots = list(
        # .check = V * i completed on rows against U^T
        interp(~ vcol %>% rowsums_byname() %>% complete_rows_cols(mat = transpose_byname(ucol), margin = 1),
               vcol = as.name(V_colname),
               ucol = as.name(U_colname))
      ) %>%
        setNames(c(check_colname))
    ) %>%
    mutate_(
      .dots = list(
        interp(~ mclapply(.$checkcol, FUN = function(v){
          # v is one of the vectors in the .check column of IndustryCheck
          # If any of the elements in the v vector is zero, we have encountered an error.
          !any(v == 0)
        }),
        checkcol = as.name(check_colname)
        )
      ) %>%
        setNames(industry_production_OK_colname)
    )
  if (!(all(IndustryCheck[[industry_production_OK_colname]] %>% as.logical))) {
    # We have a problem.
    assign(problems_data_frame_name, IndustryCheck, envir = globalenv())
    stop(paste("There are some industries that consume but do not produce energy.",
               "Check the Problems data frame for details."))
  }
  invisible(NULL)
}
