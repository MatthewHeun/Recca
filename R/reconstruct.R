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
#' Note that inputs \code{L_ixp_colname}, \code{L_pxp_colname},
#' \code{Z_colname}, and \code{D_colname} can be
#' conveniently calculated by the function \code{\link{calc_io_mats}}.
#'
#' Internally, this function uses \code{\link[matsindf]{matsindf_apply}},
#' and documentation assumes that
#' \code{.sutdata} is not \code{NULL} and is a data frame.
#' If \code{.sutdata} is present, output is a data frame with columns named by string values of output arguments, and
#' input arguments should be character strings that name columns in \code{.sutdata}.
#' If \code{.sutdata} is \code{NULL} (the default), output is a list with items named by output strings,
#' and input arguments should be single matrices or vectors.
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param Y_prime_colname the name of a column containing new final demand matrices
#'        that will be used to reconstruct the economy. Default is "\code{Y_prime}".
#' @param L_ixp_colname the name of a column containing industry-by-product L matrices.
#'        Default is "\code{L_ixp}".
#' @param L_pxp_colname the name of a column containing product-by-product L matrices.
#'        Default is "\code{L_pxp}".
#' @param Z_colname the name of a column containing \code{Z} matrices.
#'        Default is "\code{Z}".
#' @param D_colname the name of a column containing \code{D} matrices.
#'        Default is "\code{D}".
#' @param U_prime_colname the name of the output column that contains new Use (\code{U}) matrices.
#'        Default is "\code{U_prime}".
#' @param V_prime_colname the name of the output column that contains new Make (\code{V}) matrices.
#'        Default is "\code{V_prime}".
#'
#' @return \code{.sutdata} with additional columns \code{U_prime} and \code{V_prime}
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
#'     Y_prime = elementproduct_byname(2, Y)
#'   ) %>%
#'   # Should give U_prime and V_prime matrices that are double the existing U and V matrices
#'   new_Y()
new_Y <- function(.sutdata = NULL,
                  # Input columns
                  Y_prime_colname = "Y_prime", L_ixp_colname = "L_ixp", L_pxp_colname = "L_pxp",
                  Z_colname = "Z", D_colname = "D",
                  # Output columns
                  U_prime_colname = "U_prime", V_prime_colname = "V_prime"){

  new_Y_func <- function(Y_prime, L_ixp, L_pxp, Z, D){
    y_prime_val <- rowsums_byname(Y_prime)
    g_prime_val <- matrixproduct_byname(L_ixp, y_prime_val)
    q_prime_val <- matrixproduct_byname(L_pxp, y_prime_val)
    U_prime_val <- matrixproduct_byname(Z, hatize_byname(g_prime_val))
    V_prime_val <- matrixproduct_byname(D, hatize_byname(q_prime_val))
    list(U_prime_val, V_prime_val) %>% magrittr::set_names(c(U_prime_colname, V_prime_colname))
  }
  matsindf_apply(.sutdata, FUN = new_Y_func,
                 Y_prime = Y_prime_colname, L_ixp = L_ixp_colname, L_pxp = L_pxp_colname,
                 Z = Z_colname, D = D_colname)
}


#' Assess the effect of changing perfectly substitutable intermediate inputs to an industry
#'
#' This function calculates the effect of changing perfectly-substitutable (ps) inputs
#' to an intermediate industry.
#' New versions of \code{U} and \code{V} matrices are returned
#' as \code{U_prime} and \code{V_prime}.
#' Changes are made upstream of the changed industry inputs.
#' Final demand (\code{Y}) is unchanged.
#'
#' Note that inputs \code{K_colname}, \code{L_ixp_colname}, \code{L_pxp_colname},
#' \code{Z_colname}, \code{D_colname}, and \code{f_colname} can be
#' conveniently calculated by the function \code{\link{calc_io_mats}}.
#'
#' Internally, this function uses \code{\link[matsindf]{matsindf_apply}},
#' and documentation assumes that
#' \code{.sutdata} is not \code{NULL} and is a data frame.
#' If \code{.sutdata} is present, output is a data frame with columns named by string values of output arguments, and
#' input arguments should be character strings that name columns in \code{.sutdata}.
#' If \code{.sutdata} is \code{NULL} (the default), output is a list with items named by output strings,
#' and input arguments should be single matrices or vectors.
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param k_prime_colname the name of a column in \code{.sutdata} containing vectors representing new
#'        columns of the \code{K} matrix in each row of \code{.sutdata}.
#'        Each entry in the \code{k_prime} column of \code{.sutdata} must be a single-column matrix, and
#'        the name of the single column must match the name of one of the columns of matrix \code{K}.
#' @param U_colname the name of a column in \code{.sutdata} containing \code{U} matrices for the base ECC.  Default is "\code{U}".
#' @param V_colname the name of a column in \code{.sutdata} containing \code{V} matrices for the base ECC.  Default is "\code{V}".
#' @param Y_colname the name of a column in \code{.sutdata} containing \code{Y} matrices for the base ECC.  Default is "\code{Y}".
#' @param K_colname the name of a column in \code{.sutdata} containing product-by-industry \code{K} matrices.  Default is "\code{K}".
#'        \code{K} consists of columns that sum to 1.
#'        Elements of \code{K} indicate the fraction of total input to industries (in columns)
#'        provided by products (in rows).
#'        \code{K} can be calculated by \code{\link{calc_io_mats}}.
#' @param L_ixp_colname the name of a column in \code{.sutdata} containing \code{L_ixp} matrices for the base ECC.  Default is "\code{L_ixp}".
#' @param L_pxp_colname the name of a column in \code{.sutdata} containing \code{L_pxp} matrices for the base ECC.  Default is "\code{L_pxp}".
#' @param Z_colname the name of a column in \code{.sutadata} containing \code{Z} matrices for the base ECC.  Default is "\code{Z}".
#' @param D_colname the name of a column in \code{.sutadata} containing \code{D} matrices for the base ECC.  Default is "\code{D}".
#' @param f_colname the name of a column in \code{.sutadata} containing \code{f} vectors for the base ECC.  Default is "\code{f}".
#' @param U_prime_colname the name of the output column that contains new Use (\code{U}) matrices.  Default is "\code{U_prime}".
#' @param V_prime_colname the name of the output column that contains new Make (\code{V}) matrices.  Default is "\code{V_prime}".
#'
#' @return \code{.sutdata} with additional columns \code{U_prime_colname} and \code{V_prime_colname}
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
#'   spread(key = "matrix.name", value = "matrix") %>%
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
#'   mutate(
#'     # Set up a new k_prime vector for Electric transport.
#'     # That vector will be used for the infininte substitution calculation.
#'     k_prime = select_cols_byname(K, retain_pattern = make_pattern("Electric transport",
#'                                                                   pattern_type = "exact")),
#'     k_prime = make_list(k_prime_vec, n = 1)
#'   )
#' # Now do the calculation of U_prime and V_prime matrices.
#' new_UV <- new_k_ps(io_mats)
#' # There is much more FF extraction now than before.
#' io_mats$U[[1]]["FF", "FF extraction"]
#' new_UV$U_prime[[1]]["FF", "FF extraction"]
new_k_ps <- function(.sutdata = NULL,
                            # Input columns
                            k_prime_colname = "k_prime",
                            U_colname = "U", V_colname = "V", Y_colname = "Y",
                            K_colname = "K",
                            L_ixp_colname = "L_ixp", L_pxp_colname = "L_pxp",
                            Z_colname = "Z", D_colname = "D", f_colname = "f",
                            # Output colums
                            U_prime_colname = "U_prime", V_prime_colname = "V_prime"){
  new_k_ps_func <- function(k_prime_2, U, V, Y, K, L_ixp, L_pxp, Z, D, f_vec){
    # In this function, all "1" variables are calculated from the original ECC as supplied by the
    # input matrices and vectors, namely K, Y, L_ixp, Z, and f.
    # All "2" variables are calculated for the "new" ECC as supplied by the k_prime_2 vector.
    # Note that k_prime_colname in the wrapping function is mapped to k_prime_2 inside this function.

    # k_prime_2 is the new vector for the K matrix.
    # Get the name of the industry whose inputs will be changed.
    industry_to_change <- colnames(k_prime_2)
    # Ensure that k_prime_2 is a single-column vector.
    if (length(industry_to_change) != 1) {
      stop(paste("k_prime_2 has", ncol(k_prime_2), "columns in delta_inputs_ps_func. Must be 1."))
    }
    # Ensure that the column sum of k_prime_2 is exactly 1.0.
    if (colsums_byname(k_prime_2) != 1) {
      stop(paste("k_prime_2 has column sum of", colsums_byname(y_prime_2), "but it must be exactly 1.0."))
    }
    # Grab the k_prime_1 (not k_prime_2) column out of the existing K matrix.
    # k_prime_1 is the column from the K matrix with the same name as k_prime_2.
    k_prime_1 <- K[, industry_to_change, drop = FALSE]

    # We need the matrix product of k_prime_1 and f_hat in several places.
    # Calculate it here now.
    k_prime_1_f_hat <- matrixproduct_byname(k_prime_1, hatize_byname(f_vec))
    k_prime_2_f_hat <- matrixproduct_byname(k_prime_2, hatize_byname(f_vec))

    # Get y_prime, g_prime, and q_prime vectors.
    y_prime_1 <- rowsums_byname(k_prime_1_f_hat)
    y_prime_2 <- rowsums_byname(k_prime_2_f_hat)

    g_prime_1 <- matrixproduct_byname(L_ixp, y_prime_1)
    g_prime_2 <- matrixproduct_byname(L_ixp, y_prime_2)

    q_prime_1 <- matrixproduct_byname(L_pxp, y_prime_1)
    q_prime_2 <- matrixproduct_byname(L_pxp, y_prime_2)

    # Calculate U_prime_1 and U_prime_2
    U_prime_1 <- sum_byname(matrixproduct_byname(Z, hatize_byname(g_prime_1)), k_prime_1_f_hat)
    U_prime_2 <- sum_byname(matrixproduct_byname(Z, hatize_byname(g_prime_2)), k_prime_2_f_hat)

    # Calculate V_prime_1 and V_prime_2
    V_prime_1 <- matrixproduct_byname(D, hatize_byname(q_prime_1))
    V_prime_2 <- matrixproduct_byname(D, hatize_byname(q_prime_2))

    # Now subtract the "1" versions and add the "2" versions.
    U_prime <- difference_byname(U, U_prime_1) %>% sum_byname(U_prime_2)
    V_prime <- difference_byname(V, V_prime_1) %>% sum_byname(V_prime_2)

    list(U_prime, V_prime) %>% magrittr::set_names(c(U_prime_colname, V_prime_colname))
  }
  matsindf_apply(.sutdata, FUN = new_k_ps_func,
                 k_prime_2 = k_prime_colname,
                 U = U_colname, V = V_colname, Y = Y_colname,
                 K = K_colname,
                 L_ixp = L_ixp_colname, L_pxp = L_pxp_colname,
                 Z = Z_colname, D = D_colname, f_vec = f_colname)
}


#' Assess the effect of new levels of resources
#'
#' This function calculates the effect of changing the resources available to an energy conversion chain.
#' New versions of \code{U}, \code{V}, and \code{Y} matrices are returned
#' as \code{U_prime}, \code{V_prime}, and \code{Y_prime}.
#' This function assumes that each industry's inputs are perfectly substitutable (ps).
#'
#' Inputs \code{U_colname}, \code{V_colname}, \code{Y_colname},
#' \code{S_units_colname}, \code{q_colname}, and \code{C_colname}
#' can be
#' conveniently calculated by the function \code{\link{calc_io_mats}};
#' \code{eta_i_colname} can be calculated with \code{\link{calc_eta_i}}.
#'
#' Each industry must be unit-homogeneous on its inputs.
#' If not, \code{NA} is returned as the result for \code{U_prime}, \code{V_prime}, and \code{Y_prime}.
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param R_prime_colname the name of an input column in \code{.sutdata} containing a new resource matrix for the ECC.
#' @param U_colname the name of a column in \code{.sutdata} containing \code{U} matrices for the base ECC.  Default is "\code{U}".
#' @param V_colname the name of a column in \code{.sutdata} containing \code{V} matrices for the base ECC.  Default is "\code{V}".
#' @param Y_colname the name of a column in \code{.sutdata} containing \code{Y} matrices for the base ECC.  Default is "\code{Y}".
#' @param S_units_colname the name of a column in \code{.sutdata} containing \code{S_units} matrices for the base ECC.  Default is "\code{S_units}".
#' @param q_colname the name of a column in \code{.sutdata} containing \code{q} matrices for the base ECC.  Default is "\code{q}".
#' @param C_colname the name of a column in \code{.sutdata} containing \code{C} matrices for the base ECC.  Default is "\code{C}".
#' @param eta_i_colname the name of a column in \code{.sutdata} containing \code{eta_i} vectors for the base ECC.  Default is "\code{eta_i}".
#' @param maxiter the maximum allowable number of iterations when calculating the effects of a new \code{R} matrix.
#'        Default is 100.
#' @param tol the maximum allowable change in any one entry of the \code{U}, \code{V}, and \code{Y} matrices
#'        from one iteration to the next. Default is 0.
#'        I.e., when two subsequent iterations must produce the same values,
#'        the algorithm has converged.
#' @param U_prime_colname the name of the output column that contains new Use (\code{U}) matrices.
#'        Default is "\code{U_prime}".
#' @param V_prime_colname the name of the output column that contains new Make (\code{V}) matrices.
#'        Default is "\code{V_prime}".
#' @param Y_Prime_colname the name of the output column that contains new Final Demand (\code{Y}) matrices.
#'        Default is "\code{Y_prime}".
#'
#' @return \code{.sutdata} with additional columns \code{U_prime_colname}, \code{V_prime_colname}, and \code{Y_prime_colname}.
#'
#' @export
#'
#' @importFrom matsbyname abs_byname
#' @importFrom matsbyname equal_byname
#'
#' @examples
#' library(dplyr)
#' library(matsbyname)
#' library(tidyr)
#' doubleR <- UKEnergy2000mats %>%
#'   spread(key = "matrix.name", value = "matrix") %>%
#'   # At present, UKEnergy2000mats has V matrices that are the sum of both V and R.
#'   # Change to use the R matrix.
#'   rename(
#'     R_plus_V = V
#'   ) %>%
#'   separate_RV() %>%
#'   # At this point, the matrices are they way we want them.
#'   # Calculate the input-output matrices which are inputs to the new_R function.
#'   calc_io_mats() %>%
#'   # Calculate the efficiency of every industry in the ECC.
#'   calc_eta_i() %>%
#'   # Make an R_prime matrix that gives twice the resource inputs to the economy.
#'   mutate(
#'     R_prime = elementproduct_byname(2, R)
#'   ) %>%
#'   # Now call the new_R function which will calculate
#'   # updated U, V, and Y matrices (U_prime, V_prime, and Y_prime)
#'   # given R_prime.
#'   # Each of the *_prime matrices should be 2x their originals,
#'   # because R_prime is 2x relative to R.
#'   # Rows with Last.stage == "services" are NA.
#'   new_R_ps()
new_R_ps <- function(.sutdata = NULL,
                  # Input columns
                  R_prime_colname = "R_prime",
                  U_colname = "U", V_colname = "V", Y_colname = "Y", S_units_colname = "S_units",
                  q_colname = "q", C_colname = "C", eta_i_colname = "eta_i",
                  maxiter = 100, tol = 0,
                  # Output columns
                  U_prime_colname = "U_prime", V_prime_colname = "V_prime", Y_Prime_colname = "Y_prime"){
  new_R_func <- function(R_prime, U, V, Y, S_units, q, C, eta_i){
    iter <- 0

    # Verify that inputs to each industry are unit-homogeneous
    if (!(inputs_unit_homogeneous(U_colname = U, S_units_colname = S_units)[["inputs_unit_homogeneous"]])) {
      # The method employed here works only when the units on input to all industries are same.
      # If we have a situation where units are not all same, we will return NA
      return(list(NA_real_, NA_real_, NA_real_) %>%
               magrittr::set_names(c(U_prime_colname, V_prime_colname, Y_Prime_colname)))
    }

    # Calculate some quantities that we'll use on each iteration.

    # q_hat_inv_times_U
    q_hat_inv_times_U <- matrixproduct_byname(hatinv_byname(q), U)

    # Column sums of the R_prime matrix
    iR_prime <- colsums_byname(R_prime)

    # Set up an initial V_prime, which is a V matrix with all zeroes.
    # The easiest way to make that matrix is to multiply V by 0.
    V_prime <- elementproduct_byname(0, V)

    # Values for y and Y_hat_inv * Y will be needed later.
    y <- rowsums_byname(Y)
    y_hat_inv_Y <- matrixproduct_byname(hatinv_byname(y, inf_to_zero = TRUE), Y)
    # Set up a value for Y_prime.
    # The easiest way to make Y_prime is to multiply Y by 0.
    Y_prime <- elementproduct_byname(0, Y)

    # Set up "previous" matrices for convergence comparison
    U_prime_prev <- elementproduct_byname(0, U)
    V_prime_prev <- V_prime
    Y_prime_prev <- Y_prime

    # Step numbers correspond to the file UTEI_Sankey_Simple_ECC_downstream_Swim.xlsx
    # Use a do-while loop structure for this algorithm.
    repeat {
      # Step 0: Calculate q_prime_hat
      # Note that we're making q_prime into a column vector. Purpose: compatibility with the calculation of y_prime later.
      q_prime <- sum_byname(iR_prime, colsums_byname(V_prime)) %>% transpose_byname()
      q_hat_prime <- q_prime %>% hatize_byname()
      # Index the iteration counter
      iter <- iter + 1
      # Step 1: Calculate U_prime
      U_prime <- matrixproduct_byname(q_hat_prime, q_hat_inv_times_U)
      # Step 2: Calculate U_bar_prime
      U_bar_prime <- transpose_byname(S_units) %>% matrixproduct_byname(U_prime)
      # Step 3: Calculate column sums of U_bar_prime
      i_U_bar_prime <- colsums_byname(U_bar_prime)
      # Step 4: Calculate i_U_bar_prime_hat
      i_U_bar_hat_prime <- hatize_byname(i_U_bar_prime)
      # Step 5: Calculate g_prime
      g_prime <- matrixproduct_byname(i_U_bar_hat_prime, eta_i)
      # Step 6: Calculate g_prime_hat
      g_hat_prime <- hatize_byname(g_prime)
      # Step 7: Calculate V_prime
      V_prime <- matrixproduct_byname(C, g_hat_prime) %>% transpose_byname()
      # Step 8: Calculate Y_prime
      y_prime <- difference_byname(q_prime, rowsums_byname(U_prime))
      y_hat_prime <- hatize_byname(y_prime)
      Y_prime <- matrixproduct_byname(y_hat_prime, y_hat_inv_Y)

      # Check convergence condition
      U_OK <- difference_byname(U_prime, U_prime_prev) %>% abs_byname() %>% compare_byname("<=", tol) %>% all()
      V_OK <- difference_byname(V_prime, V_prime_prev) %>% abs_byname() %>% compare_byname("<=", tol) %>% all()
      Y_OK <- difference_byname(Y_prime, Y_prime_prev) %>% abs_byname() %>% compare_byname("<=", tol) %>% all()
      if (U_OK & V_OK & Y_OK) {
        break
      }

      # Check to see if we have exceeded the maximum number of iterations
      if (iter >= maxiter) {
        warning(paste("maxiter =", maxiter, "reached without convergence in new_R"))
        break
      }
      # Prepare for next iteration
      U_prime_prev <- U_prime
      V_prime_prev <- V_prime
      Y_prime_prev <- Y_prime
    }

    # Verify that the ECC is in energy balance.
    # verify_SUT_energy_balance_with_units(U = U, V = V, Y = Y, S_units = S_units)

    # Return the new U, V, and Y matrices.
    list(U_prime, V_prime, Y_prime) %>% magrittr::set_names(c(U_prime_colname, V_prime_colname, Y_Prime_colname))
  }

  matsindf_apply(.sutdata, FUN = new_R_func, U = U_colname, V = V_colname, Y = Y_colname, S_units = S_units_colname,
                 q = q_colname, C = C_colname, eta_i = eta_i_colname)
}



