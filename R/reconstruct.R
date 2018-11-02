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
#' that will be used to reconstruct the economy
#' @param L_ixp_colname the name of a column containing industry-by-product L matrices.
#' @param L_pxp_colname the name of a column containing product-by-product L matrices.
#' @param Z_colname the name of a column containing \code{Z} matrices.
#' @param D_colname the name of a column containing \code{D} matrices.
#' @param U_prime_colname the name of the output column that contains new Use (\code{U}) matrices.
#' @param V_prime_colname the name of the output column that contains new Make (\code{V}) matrices.
#'
#' @return \code{.sutdata} with additional columns \code{U_prime} and \code{V_prime}
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(magrittr)
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
    list(U_prime_val, V_prime_val) %>% purrr::set_names(c(U_prime_colname, V_prime_colname))
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
#' library(magrittr)
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

    list(U_prime, V_prime) %>% purrr::set_names(c(U_prime_colname, V_prime_colname))
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
#'
#' Note that inputs \code{L_ixp_colname}, \code{L_pxp_colname},
#' can be
#' conveniently calculated by the function \code{\link{calc_io_mats}}.
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param R_prime_colname the name of an input column in \code{.sutdata} containing a new resource matrix for the ECC.
#' @param U_colname the name of a column in \code{.sutdata} containing \code{U} matrices for the base ECC.  Default is "\code{U}".
#' @param V_colname the name of a column in \code{.sutdata} containing \code{V} matrices for the base ECC.  Default is "\code{V}".
#' @param Y_colname the name of a column in \code{.sutdata} containing \code{Y} matrices for the base ECC.  Default is "\code{Y}".
#' @param U_prime_colname the name of the output column that contains new Use (\code{U}) matrices.
#'        Default is "\code{U_prime}".
#' @param V_prime_colname the name of the output column that contains new Make (\code{V}) matrices.
#'        Default is "\code{V_prime}".
#' @param Y_Prime_colname
#'
#' @return \code{.sutdata} with additional columns \code{U_prime_colname}, \code{V_prime_colname}, and \code{Y_prime_colname}.
#'
#' @export
#'
#' @examples
new_R <- function(.sutdata = NULL,
                  # Input columns
                  R_prime_colname = "R_prime",
                  U_colname = "U", V_colname = "V", Y_colname = "Y",
                  # Output columns
                  U_prime_colname = "U_prime", V_prime_colname = "V_prime", Y_Prime_colname = "Y_prime"){

}


