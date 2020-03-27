#
# This file contains functions that reconstruct an economy given
# total requirements matrices (U and V) and a new final demand matrix (Y)
# or a new inputs matrix (K).
#

#' Reconstruct an economy given a new final demand matrix
#'
#' When the final demand matrix changes from \code{Y} to \code{Y_prime},
#' this function calculates new use (\code{U_prime}) and make (\code{V_prime}) matrices
#' that would be required to meet the new final demand (\code{Y_prime}).
#'
#' Note that inputs \code{L_ixp}, \code{L_pxp},
#' \code{Z}, and \code{D} can be
#' conveniently calculated by the function \code{\link{calc_io_mats}}.
#'
#' Internally, this function uses \code{\link[matsindf]{matsindf_apply}},
#' and documentation assumes that
#' \code{.sutmats} is not \code{NULL} and is a data frame.
#' If \code{.sutmats} is present, output is a data frame with columns named by string values of output arguments, and
#' input arguments should be character strings that name columns in \code{.sutmats}.
#' If \code{.sutmats} is \code{NULL} (the default), output is a list with items named by output strings,
#' and input arguments should be single matrices or vectors.
#'
#' @param .sutmats a data frame of supply-use table matrices with matrices arranged in columns.
#' @param Y_prime a new final demand matrix or name of a column in \code{.sutmats} containing same. Default is "\code{Y_prime}".
#' @param L_ixp an \code{L_ixp} matrix or name of a column in \code{.sutmats} containing same. Default is "\code{L_ixp}".
#' @param L_pxp an \code{L_pxp} matrix or name of a column in \code{.sutmats} containing same. Default is "\code{L_pxp}".
#' @param Z a \code{Z} matrix or name of a column in \code{.sutmats} containing same. Default is "\code{Z}".
#' @param D a \code{D} matrix or name of a column in \code{.sutmats} containing same. Default is "\code{D}".
#' @param U_prime the name for new \code{U} matrices. Default is "\code{U_prime}".
#' @param V_prime the name for new \code{V} matrices. Default is "\code{V_prime}".
#'
#' @return a list or data frame with \code{U_prime} and \code{V_prime} matrices
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(matsbyname)
#' library(tidyr)
#' UKEnergy2000mats %>%
#'   spread(key = matrix.name, value = matrix) %>%
#'   select(Country, Year, Energy.type, Last.stage, U, V, Y, r_EIOU, S_units) %>%
#'   calc_io_mats() %>%
#'   mutate(
#'     # Give new Y matrices that are double the existing Y matrices
#'     Y_prime = hadamardproduct_byname(2, Y)
#'   ) %>%
#'   # Should give U_prime and V_prime matrices that are double the existing U and V matrices
#'   new_Y()
new_Y <- function(.sutmats = NULL,
                  # Input names
                  Y_prime = "Y_prime", L_ixp = "L_ixp", L_pxp = "L_pxp",
                  Z = "Z", D = "D",
                  # Output names
                  U_prime = "U_prime", V_prime = "V_prime"){

  new_Y_func <- function(Y_prime_mat, L_ixp_mat, L_pxp_mat, Z_mat, D_mat){
    y_prime_vec <- matsbyname::rowsums_byname(Y_prime_mat)
    g_prime_vec <- matsbyname::matrixproduct_byname(L_ixp_mat, y_prime_vec)
    q_prime_vec <- matsbyname::matrixproduct_byname(L_pxp_mat, y_prime_vec)
    U_prime_mat <- matsbyname::matrixproduct_byname(Z_mat, matsbyname::hatize_byname(g_prime_vec))
    V_prime_mat <- matsbyname::matrixproduct_byname(D_mat, matsbyname::hatize_byname(q_prime_vec))
    list(U_prime_mat, V_prime_mat) %>% magrittr::set_names(c(U_prime, V_prime))
  }
  matsindf::matsindf_apply(.sutmats, FUN = new_Y_func,
                 Y_prime_mat = Y_prime, L_ixp_mat = L_ixp, L_pxp_mat = L_pxp,
                 Z_mat = Z, D_mat = D)
}


#' Assess the effect of changing perfectly substitutable intermediate inputs to an industry
#'
#' This function calculates the effect of changing perfectly-substitutable (ps) inputs
#' to an intermediate industry.
#' New versions of `U` and `V` matrices are returned
#' as `U_prime` and `V_prime`.
#' Changes are made upstream of the changed industry inputs.
#' The final demand matrix (`Y`) is unchanged.
#'
#' Note that inputs `K`, `L_ixp`, `L_pxp`,
#' `Z`, `D`, and `f` can be
#' conveniently calculated by the function `calc_io_mats()`.
#'
#' Internally, this function uses `matsindf::matsindf_apply()`,
#' and documentation assumes that
#' `.sutmats` is not `NULL` and is a data frame.
#' If `.sutmats` is present, output is a data frame with columns named by string values of output arguments, and
#' input arguments should be character strings that name columns in `.sutmats`.
#' If `.sutmats` is `NULL` (the default), output is a list with items named by output strings,
#' and input arguments should be single matrices or vectors.
#'
#' @param .sutmats a data frame of supply-use table matrices with matrices arranged in columns.
#' @param k_prime a new column vector for the `K` matrix representing new
#'        inputs to an industry or name of a column in `.sutmats` containing same.
#'        Default is "k_prime".
#'        The name of the single `k_prime` column must match the name of one of the columns of matrix `K`.
#' @param R resource (`R`) matrix or name of the column in `.sutmats` that contains same. Default is "R".
#' @param U use (`U`) matrix or name of the column in `.sutmats` that contains same. Default is "U".
#' @param V make (`V`) matrix or name of the column in `.sutmats`that contains same. Default is "V".
#' @param Y final demand (`Y`) matrix or name of the column in `.sutmats` that contains same. Default is "Y".
#' @param K a `K` matrix or name of the column in `.sutmats` that contains same. Default is "K".
#'        `K` consists of columns that sum to 1.
#'        Elements of `K` indicate the fraction of total input to industries (in columns)
#'        provided by products (in rows).
#'        `K` can be calculated by `calc_io_mats()`.
#' @param L_ixp an (`L_ixp`) matrix or name of the column in `.sutmats` that contains same. Default is "L_ixp".
#' @param L_pxp an (`L_pxp`) matrix or name of the column in `.sutmats` that contains same. Default is "L_pxp".
#' @param Z a `Z` matrix or name of the column in `.sutmats` that contains same. Default is "Z".
#' @param D a `D` matrix or name of the column in `.sutmats` that contains same. Default is "D".
#' @param f an `f` vector or name of the column in `sutmats` that contains same. Default is "f".
#' @param R_prime name for the `R_prime` matrix on output. Default is "R_prime".
#' @param U_prime name for the `U_prime` matrix on output. Default is "U_prime".
#' @param V_prime name for the `V_prime` matrix on output. Default is "V_prime".
#'
#' @return a list or data frame containing `U_prime` and `V_prime` matrices
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(matsbyname)
#' library(tidyr)
#' # To demonstrate calculating changes to an energy conversion chain due to changes
#' # in perfectly-substitutable inputs to an intermediate industry,
#' # we use the PerfectSubmats data frame.
#' # But we need to calculate several important input-output matrices first.
#' io_mats <- PerfectSubmats %>%
#'   tidyr::spread(key = "matrix.name", value = "matrix") %>%
#'   calc_io_mats()
#' # Next, find the K matrix that contains the fraction of each type of energy
#' # that enters each industry
#' K <- io_mats$K[[1]]
#' # Develop a new column vector for inputs to the Electric transport sector.
#' # As provided, the Electric transport sector is dominated by Renewable elec.
#' # What if the electricity input to the Electric transport sector
#' # were split 50/50 between Renewable elect and FF elec?
#' k_prime_vec <- K[, "Electric transport", drop = FALSE]
#' k_prime_vec["FF elec", "Electric transport"] <- 0.5
#' k_prime_vec["Ren elec", "Electric transport"] <- 0.5
#' # Add k_prime_vec to the io_mats data frame.
#' io_mats <- io_mats %>%
#'   dplyr::mutate(
#'     # Set up a new k_prime vector for Electric transport.
#'     # That vector will be used for the infininte substitution calculation.
#'     k_prime = matsbyname::select_cols_byname(K, retain_pattern = make_pattern("Electric transport",
#'                                                                   pattern_type = "exact")),
#'     k_prime = matsbyname::make_list(k_prime_vec, n = 1)
#'   )
#' # Now do the calculation of U_prime and V_prime matrices.
#' new_UV <- new_k_ps(io_mats)
#' # There is much more FF extraction now than before.
#' io_mats$U[[1]]["FF", "FF extraction"]
#' new_UV$U_prime[[1]]["FF", "FF extraction"]
new_k_ps <- function(.sutmats = NULL,
                            # Input names
                            k_prime = "k_prime",
                            R = "R", U = "U", V = "V", Y = "Y",
                            K = "K",
                            L_ixp = "L_ixp", L_pxp = "L_pxp",
                            Z = "Z", D = "D", f = "f",
                            # Output names
                            R_prime = "R_prime", U_prime = "U_prime", V_prime = "V_prime"){
  new_k_ps_func <- function(k_prime_2, R_mat = NULL, U_mat, V_mat, Y_mat, K_mat, L_ixp_mat, L_pxp_mat, Z_mat, D_mat, f_vec){

    if (is.null(R_mat)) {
      # No R matrix, just use the V matrix, assuming that resouces are included there.
      R_plus_V_mat <- V_mat
    } else {
      # An R matrix is present. Sum R and V before proceeding.
      R_plus_V_mat <- matsbyname::sum_byname(R_mat, V_mat)
    }

    # In this function, all "1" variables are calculated from the original ECC as supplied by the
    # input matrices and vectors, namely K, Y, L_ixp, Z, and f.
    # All "2" variables are calculated for the "new" ECC as supplied by the k_prime_2 vector.
    # Note that k_prime_colname in the wrapping function is mapped to k_prime_2 inside this function.

    # k_prime_2 is the new vector for the K matrix.
    # Get the name of the industry whose inputs will be changed.
    industry_to_change <- colnames(k_prime_2)
    # Ensure that k_prime_2 is a single-column vector.
    assertthat::assert_that(length(industry_to_change) == 1,
                            msg = paste("k_prime_2 has", ncol(k_prime_2),
                                        "columns in delta_inputs_ps_func. Must have 1 column."))
    # Ensure that the column sum of k_prime_2 is exactly 1.0.
    assertthat::assert_that(matsbyname::colsums_byname(k_prime_2) == 1,
                            msg = paste("k_prime_2 has column sum of",
                                        matsbyname::colsums_byname(y_prime_2),
                                        "but it must be exactly 1.0."))
    # Grab the k_prime_1 (not k_prime_2) column out of the existing K matrix.
    # k_prime_1 is the column from the K matrix with the same name as k_prime_2.
    k_prime_1 <- K_mat[, industry_to_change, drop = FALSE]

    # We need the matrix product of k_prime_1 and f_hat in several places.
    # Calculate it here now.
    k_prime_1_f_hat <- matsbyname::matrixproduct_byname(k_prime_1, matsbyname::hatize_byname(f_vec))
    k_prime_2_f_hat <- matsbyname::matrixproduct_byname(k_prime_2, matsbyname::hatize_byname(f_vec))

    # Get y_prime, g_prime, and q_prime vectors.
    y_prime_1 <- matsbyname::rowsums_byname(k_prime_1_f_hat)
    y_prime_2 <- matsbyname::rowsums_byname(k_prime_2_f_hat)

    g_prime_1 <- matsbyname::matrixproduct_byname(L_ixp_mat, y_prime_1)
    g_prime_2 <- matsbyname::matrixproduct_byname(L_ixp_mat, y_prime_2)

    q_prime_1 <- matsbyname::matrixproduct_byname(L_pxp_mat, y_prime_1)
    q_prime_2 <- matsbyname::matrixproduct_byname(L_pxp_mat, y_prime_2)

    # Calculate U_prime_1 and U_prime_2
    U_prime_1 <- matsbyname::sum_byname(matsbyname::matrixproduct_byname(Z_mat, matsbyname::hatize_byname(g_prime_1)), k_prime_1_f_hat)
    U_prime_2 <- matsbyname::sum_byname(matsbyname::matrixproduct_byname(Z_mat, matsbyname::hatize_byname(g_prime_2)), k_prime_2_f_hat)

    # Calculate V_prime_1 and V_prime_2
    V_prime_1 <- matsbyname::matrixproduct_byname(D_mat, matsbyname::hatize_byname(q_prime_1))
    V_prime_2 <- matsbyname::matrixproduct_byname(D_mat, matsbyname::hatize_byname(q_prime_2))

    # Now subtract the "1" versions and add the "2" versions.
    U_prime_mat <- matsbyname::difference_byname(U_mat, U_prime_1) %>% matsbyname::sum_byname(U_prime_2)
    V_prime_mat <- matsbyname::difference_byname(R_plus_V_mat, V_prime_1) %>% matsbyname::sum_byname(V_prime_2)

    # If we had an R_mat, need to extract R_prime from V_prime and return it.
    if (!is.null(R_mat)) {
      # Extract the R_prime matrix from the V_prime matrix.
      separated <- separate_RV(U = U_prime_mat, R_plus_V = V_prime_mat)
      R_prime_mat <- separated[["R"]]
      V_prime_mat <- separated[["V"]]
      return(list(R_prime_mat, U_prime_mat, V_prime_mat) %>% magrittr::set_names(c(R_prime, U_prime, V_prime)))
    } else {
      # We didn't have an R matrix on input, so don't provide one on output.
      # Furthermore, the V_prime matrix is still fine, so leave it alone.
      # No need to separate R_prime from V_prime.
      return(list(U_prime_mat, V_prime_mat) %>% magrittr::set_names(c(U_prime, V_prime)))
    }

  }
  matsindf::matsindf_apply(.sutmats, FUN = new_k_ps_func,
                 k_prime_2 = k_prime,
                 R_mat = R, U_mat = U, V_mat = V, Y_mat = Y,
                 K_mat = K,
                 L_ixp_mat = L_ixp, L_pxp_mat = L_pxp,
                 Z_mat = Z, D_mat = D, f_vec = f)
}


#' Assess the effect of new levels of resources
#'
#' This function calculates the effect of changing the resources available to an energy conversion chain.
#' New versions of \code{U}, \code{V}, and \code{Y} matrices are returned
#' as \code{U_prime}, \code{V_prime}, and \code{Y_prime}.
#' This function assumes that each industry's inputs are perfectly substitutable (ps).
#'
#' Inputs \code{U}, \code{V}, \code{Y},
#' \code{S_units}, \code{q}, and \code{C}
#' can be
#' conveniently calculated by the function \code{\link{calc_io_mats}};
#' \code{eta_i} can be calculated with \code{\link{calc_eta_i}}.
#'
#' Each industry must be unit-homogeneous on its inputs.
#' If not, \code{NA} is returned as the result for \code{U_prime}, \code{V_prime}, and \code{Y_prime}.
#'
#' @param .sutmats a data frame of supply-use table matrices with matrices arranged in columns.
#' @param R_prime a new resource matrix or name of a column in \code{.sutmats} containing same. Default is "\code{R_prime}".
#' @param U use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".
#' @param V make (\code{V}) matrix or name of the column in \code{.sutmats}that contains same. Default is "\code{V}".
#' @param Y final demand (\code{Y}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{Y}".
#' @param S_units \code{S_units} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{S_units}".
#' @param q \code{q} vector or name of the column in \code{.sutmats} that contains same. Default is "\code{q}".
#' @param C a \code{C} matrix or name of a column in \code{.sutmats} containing same. Default is "\code{C}".
#' @param eta_i an \code{eta_i} vector or name of a column in \code{.sutmats} containing same. Default is "\code{eta_i}".
#' @param maxiter the maximum allowable number of iterations when calculating the effects of a new \code{R} matrix.
#'        Default is \code{100}.
#' @param tol the maximum allowable change in any one entry of the \code{U}, \code{V}, and \code{Y} matrices
#'        from one iteration to the next. Default is 0,
#'        i.e., when two subsequent iterations produce the same values,
#'        the algorithm has converged.
#' @param U_prime name for the \code{U_prime} matrix on output. Default is "\code{U_prime}".
#' @param V_prime name for the \code{V_prime} matrix on output. Default is "\code{V_prime}".
#' @param Y_prime name for the \code{Y_prime} matrix on output. Default is "\code{Y_prime}".
#'
#' @return a list or data frame containing \code{U_prime}, \code{V_prime}, and \code{Y_prime} matrices
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(matsbyname)
#' library(tidyr)
#' doubleR <- UKEnergy2000mats %>%
#'   spread(key = "matrix.name", value = "matrix") %>%
#'   # Calculate the input-output matrices which are inputs to the new_R function.
#'   calc_io_mats() %>%
#'   # Calculate the efficiency of every industry in the ECC.
#'   calc_eta_i() %>%
#'   # Make an R_prime matrix that gives twice the resource inputs to the economy.
#'   mutate(
#'     R_prime = hadamardproduct_byname(2, R)
#'   ) %>%
#'   # Now call the new_R function which will calculate
#'   # updated U, V, and Y matrices (U_prime, V_prime, and Y_prime)
#'   # given R_prime.
#'   # Each of the *_prime matrices should be 2x their originals,
#'   # because R_prime is 2x relative to R.
#'   # Rows with Last.stage == "services" are NA.
#'   new_R_ps()
#' doubleR$U_prime[[1]]
#' doubleR$V_prime[[1]]
#' doubleR$Y_prime[[1]]
new_R_ps <- function(.sutmats = NULL,
                  # Input names
                  R_prime = "R_prime",
                  U = "U", V = "V", Y = "Y", S_units = "S_units",
                  q = "q", C = "C", eta_i = "eta_i",
                  maxiter = 100, tol = 0,
                  # Output names
                  U_prime = "U_prime", V_prime = "V_prime", Y_prime = "Y_prime"){
  new_R_func <- function(R_prime_mat, U_mat, V_mat, Y_mat, S_units_mat, q_vec, C_mat, eta_i_vec){
    iter <- 0

    # Verify that inputs to each industry are unit-homogeneous
    if (!(inputs_unit_homogeneous(U = U_mat, S_units = S_units_mat)[[".inputs_unit_homogeneous"]])) {
      # The method employed here works only when the units on input to all industries are same.
      # If we have a situation where units are not all same, we will return NA
      return(list(NA_real_, NA_real_, NA_real_) %>%
               magrittr::set_names(c(U_prime, V_prime, Y_prime)))
    }

    # Calculate some quantities that we'll use on each iteration.

    # q_hat_inv_times_U
    q_hat_inv_times_U <- matsbyname::matrixproduct_byname(matsbyname::hatinv_byname(q_vec), U_mat)

    # Column sums of the R_prime matrix
    iR_prime <- matsbyname::colsums_byname(R_prime_mat)

    # Set up an initial V_prime, which is a V matrix with all zeroes.
    # The easiest way to make that matrix is to multiply V by 0.
    V_prime_mat <- matsbyname::hadamardproduct_byname(0, V_mat)

    # Values for y and Y_hat_inv * Y will be needed later.
    y_vec <- matsbyname::rowsums_byname(Y_mat)
    y_hat_inv_Y <- matsbyname::matrixproduct_byname(matsbyname::hatinv_byname(y_vec), Y_mat)
    # Set up a value for Y_prime.
    # The easiest way to make Y_prime is to multiply Y by 0.
    Y_prime_mat <- matsbyname::hadamardproduct_byname(0, Y_mat)

    # Set up "previous" matrices for convergence comparison
    U_prime_mat_prev <- matsbyname::hadamardproduct_byname(0, U_mat)
    V_prime_mat_prev <- V_prime_mat
    Y_prime_mat_prev <- Y_prime_mat

    # Step numbers correspond to the file UTEI_Sankey_Simple_ECC_downstream_Swim.xlsx
    # Use a do-while loop structure for this algorithm.
    repeat {
      # Step 0: Calculate q_prime_hat
      # Note that we're making q_prime into a column vector. Purpose: compatibility with the calculation of y_prime later.
      q_prime <- matsbyname::sum_byname(iR_prime, matsbyname::colsums_byname(V_prime_mat)) %>% matsbyname::transpose_byname()
      q_hat_prime <- q_prime %>% matsbyname::hatize_byname()
      # Index the iteration counter
      iter <- iter + 1
      # Step 1: Calculate U_prime
      U_prime_mat <- matsbyname::matrixproduct_byname(q_hat_prime, q_hat_inv_times_U)
      # Step 2: Calculate U_bar_prime
      U_bar_prime <- matsbyname::transpose_byname(S_units_mat) %>% matsbyname::matrixproduct_byname(U_prime_mat)
      # Step 3: Calculate column sums of U_bar_prime
      i_U_bar_prime <- matsbyname::colsums_byname(U_bar_prime)
      # Step 4: Calculate i_U_bar_prime_hat
      i_U_bar_hat_prime <- matsbyname::hatize_byname(i_U_bar_prime)
      # Step 5: Calculate g_prime
      g_prime <- matsbyname::matrixproduct_byname(i_U_bar_hat_prime, eta_i_vec)
      # Step 6: Calculate g_prime_hat
      g_hat_prime <- matsbyname::hatize_byname(g_prime)
      # Step 7: Calculate V_prime
      V_prime_mat <- matsbyname::matrixproduct_byname(C_mat, g_hat_prime) %>% matsbyname::transpose_byname()
      # Step 8: Calculate Y_prime
      y_prime <- matsbyname::difference_byname(q_prime, matsbyname::rowsums_byname(U_prime_mat))
      y_hat_prime <- matsbyname::hatize_byname(y_prime)
      Y_prime_mat <- matsbyname::matrixproduct_byname(y_hat_prime, y_hat_inv_Y)

      # Check convergence condition
      # Calculate only as many differences as necessary.
      if (matsbyname::difference_byname(Y_prime_mat, Y_prime_mat_prev) %>% matsbyname::iszero_byname(tol = tol)) {
        if (matsbyname::difference_byname(V_prime_mat, V_prime_mat_prev) %>% matsbyname::iszero_byname(tol = tol)) {
          if (matsbyname::difference_byname(U_prime_mat, U_prime_mat_prev) %>% matsbyname::iszero_byname(tol = tol)) {
            # If we get here, all of U_prime, V_prime, and Y_prime
            # are same as their respective *_prev values within tol.
            # This is the stopping condition, so break.
            break
          }
        }
      }

      # Check to see if we have exceeded the maximum number of iterations
      if (iter >= maxiter) {
        warning(paste("maxiter =", maxiter, "reached without convergence in new_R"))
        break
      }
      # Prepare for next iteration
      U_prime_mat_prev <- U_prime_mat
      V_prime_mat_prev <- V_prime_mat
      Y_prime_mat_prev <- Y_prime_mat
    }

    # Verify that the ECC is in energy balance.
    verify_SUT_energy_balance_with_units(R = R_prime_mat, U = U_prime_mat, V = V_prime_mat, Y = Y_prime_mat, S_units = S_units_mat)

    # Return the new U, V, and Y matrices.
    list(U_prime_mat, V_prime_mat, Y_prime_mat) %>% magrittr::set_names(c(U_prime, V_prime, Y_prime))
  }

  matsindf::matsindf_apply(.sutmats, FUN = new_R_func, R_prime_mat = R_prime, U_mat = U, V_mat = V, Y_mat = Y, S_units_mat = S_units,
                           q_vec = q, C_mat = C, eta_i_vec = eta_i)
}



