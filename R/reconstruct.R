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
#' @param R a \code{R} matrix or name of a column in \code{.sutmats} containing same. Default is "\code{R}".
#' @param Y_prime a new final demand matrix or name of a column in \code{.sutmats} containing same. Default is "\code{Y_prime}".
#' @param L_ixp an \code{L_ixp} matrix or name of a column in \code{.sutmats} containing same. Default is "\code{L_ixp}".
#' @param L_pxp an \code{L_pxp} matrix or name of a column in \code{.sutmats} containing same. Default is "\code{L_pxp}".
#' @param Z a \code{Z} matrix or name of a column in \code{.sutmats} containing same. Default is "\code{Z}".
#' @param D a \code{D} matrix or name of a column in \code{.sutmats} containing same. Default is "\code{D}".
#' @param O a \code{O} matrix or name of a column in \code{.sutmats} containing same. Default is "\code{O}".
#' @param r The name of the `r` vector.
#'          Default is "r".
#' @param h The name of the `h` vector.
#'          Default is "h".
#' @param U_prime the name for new \code{U} matrices. Default is "\code{U_prime}".
#' @param V_prime the name for new \code{V} matrices. Default is "\code{V_prime}".
#' @param W_prime the name for new \code{W} matrices. Default is "\code{W_prime}".
#' @param R_prime the name for new \code{R} matrices. Default is "\code{R_prime}".
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
#'   select(Country, Year, Energy.type, Last.stage, R, U, U_feed, V, Y, r_EIOU, S_units) %>%
#'   calc_io_mats() %>%
#'   mutate(
#'     # Give new Y matrices that are double the existing Y matrices
#'     Y_prime = matsbyname::hadamardproduct_byname(2, Y)
#'   ) %>%
#'   # Should give U_prime and V_prime matrices that are double the existing U and V matrices
#'   new_Y()
new_Y <- function(.sutmats = NULL,
                  # Input names
                  Y_prime = "Y_prime", L_ixp = "L_ixp", L_pxp = "L_pxp",
                  Z = "Z", D = "D", R = "R", r = "r", h = "h", O = "O",
                  # Output names
                  U_prime = "U_prime", V_prime = "V_prime", W_prime = "W_prime", R_prime = "R_prime"){

  new_Y_func <- function(Y_prime_mat, L_ixp_mat, L_pxp_mat, Z_mat, D_mat, O_mat, R_mat, r_vec, h_vec){

    if (is.null(Y_prime_mat)){
      U_prime_mat <- NULL
      V_prime_mat <- NULL
      W_prime_mat <- NULL
      R_prime_mat <- NULL
    } else {
      y_prime_vec <- matsbyname::rowsums_byname(Y_prime_mat)

      g_prime_vec <- matsbyname::matrixproduct_byname(L_ixp_mat, y_prime_vec)

      q_prime_vec <- matsbyname::matrixproduct_byname(L_pxp_mat, y_prime_vec)

      U_prime_mat <- matsbyname::matrixproduct_byname(Z_mat, matsbyname::hatize_byname(g_prime_vec, keep = "rownames"))

      V_prime_mat <- matsbyname::matrixproduct_byname(D_mat, matsbyname::hatize_byname(q_prime_vec, keep = "rownames"))

      W_prime_mat <- matsbyname::difference_byname(
        matsbyname::transpose_byname(V_prime_mat),
        U_prime_mat
      )

      # R_prime_mat <- matsbyname::matrixproduct_byname(
      #   r_vec %>%
      #     matsbyname::hatinv_byname() %>%
      #     matsbyname::matrixproduct_byname(R_mat),
      #   matsbyname::difference_byname(y_prime_vec, matsbyname::rowsums_byname(W_prime_mat)) %>%
      #     matsbyname::hatize_byname()
      #     )

      y_prime_minus_W_i <- y_prime_vec %>%
        matsbyname::difference_byname(
          matsbyname::rowsums_byname(W_prime_mat)
        )

      R_prime_mat <- matsbyname::matrixproduct_byname(
        O_mat,
        matsbyname::transpose_byname(
          matsbyname::hatize_byname(y_prime_minus_W_i, keep = "rownames")
        )
      )
    }

    list(U_prime_mat, V_prime_mat, W_prime_mat, R_prime_mat) %>% magrittr::set_names(c(U_prime, V_prime, W_prime, R_prime))
  }

  matsindf::matsindf_apply(.sutmats, FUN = new_Y_func,
                 Y_prime_mat = Y_prime, L_ixp_mat = L_ixp, L_pxp_mat = L_pxp,
                 Z_mat = Z, D_mat = D, O_mat = O, R_mat = R,
                 r_vec = r, h_vec = h)
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
    k_prime_1_f_hat <- matsbyname::matrixproduct_byname(k_prime_1, matsbyname::hatize_byname(f_vec, keep = "rownames"))
    k_prime_2_f_hat <- matsbyname::matrixproduct_byname(k_prime_2, matsbyname::hatize_byname(f_vec, keep = "rownames"))

    # Get y_prime, g_prime, and q_prime vectors.
    y_prime_1 <- matsbyname::rowsums_byname(k_prime_1_f_hat)
    y_prime_2 <- matsbyname::rowsums_byname(k_prime_2_f_hat)

    g_prime_1 <- matsbyname::matrixproduct_byname(L_ixp_mat, y_prime_1)
    g_prime_2 <- matsbyname::matrixproduct_byname(L_ixp_mat, y_prime_2)

    q_prime_1 <- matsbyname::matrixproduct_byname(L_pxp_mat, y_prime_1)
    q_prime_2 <- matsbyname::matrixproduct_byname(L_pxp_mat, y_prime_2)

    # Calculate U_prime_1 and U_prime_2
    U_prime_1 <- matsbyname::sum_byname(matsbyname::matrixproduct_byname(Z_mat, matsbyname::hatize_byname(g_prime_1, keep = "rownames")), k_prime_1_f_hat)
    U_prime_2 <- matsbyname::sum_byname(matsbyname::matrixproduct_byname(Z_mat, matsbyname::hatize_byname(g_prime_2, keep = "rownames")), k_prime_2_f_hat)

    # Calculate V_prime_1 and V_prime_2
    V_prime_1 <- matsbyname::matrixproduct_byname(D_mat, matsbyname::hatize_byname(q_prime_1, keep = "rownames"))
    V_prime_2 <- matsbyname::matrixproduct_byname(D_mat, matsbyname::hatize_byname(q_prime_2, keep = "rownames"))

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



#' Calculates downstream effects of a new level of extracted resources
#'
#' This function calculates the effect of changing the resources available to an ECC,
#' i.e. of a new resources matrix `R` on the rest of the ECC matrices (`U`, `V`, `W`, and `Ý`).
#' New versions of the `U`, `V`, `W`, and `Ý` matrices are returned,
#' and respectively called `U_prime`, `V_prime`, `W_prime`, and `Ý_prime`.
#' This function assumes that each industry's inputs are perfectly substitutable (ps).
#'
#' Each industry must be unit-homogeneous on its inputs.
#' If not, a matrix populated with \code{NA} is returned as the result for
#' \code{U_prime}, \code{V_prime}, and \code{Y_prime}.
#'
#' @param .sutmats a data frame of supply-use table matrices with matrices arranged in columns.
#' @param R_prime The name of the new R matrix column in the input data frame, for which the new ECC must be assessed.
#'                Default is "R_prime".
#' @param U The name of the U matrix column in the input data frame.
#'          Default is "U".
#' @param V The name of the V matrix column in the input data frame.
#'          Default is "V".
#' @param Y The name of the Y matrix column in the input data frame.
#'          Default is "Y".
#' @param q The name of the q vector column in the input data frame.
#'          Default is "q".
#' @param f The name of the f vector column in the input data frame.
#'          Default is "f".
#' @param U_prime The name of the output column containing the new U matrices.
#'                Default is "U_prime".
#' @param V_prime The name of the output column containing the new V matrices.
#'                Default is "V_prime".
#' @param W_prime The name of the output column containing the new W matrices.
#'                Default is "W_prime".
#' @param Y_prime The name of the output column containing the new Y matrices.
#'                Default is "Y_prime".
#'
#' @return A data frame with added columns representing each of the new `U_prime`, `V_prime`, `W_prime`, and `Ý_prime` matrices.
#' @export
#'
#' @examples
#' UKEnergy2000mats %>%
#' tidyr::spread(key = "matrix.name", value = "matrix") %>%
#' # Calculate the input-output matrices which are inputs to the new_R function.
#'  calc_io_mats() %>%
#' # Make an R_prime matrix that gives twice the resource inputs to the economy.
#'  dplyr::mutate(
#'    R_prime = matsbyname::hadamardproduct_byname(2, R)
#'  ) %>%
#' # Now call the new_R function which will calculate
#' # updated U, V, and Y matrices (U_prime, V_prime, and Y_prime)
#' # given R_prime.
#' # Each of the *_prime matrices should be 2x their originals,
#' # because R_prime is 2x relative to R.
#'  new_R_ps()
new_R_ps <- function(.sutmats = NULL,
                          # Input names
                          R_prime = "R_prime",
                          U = "U", V = "V", Y = "Y",
                          q = "q", f = "f",
                          # Output names
                          U_prime = "U_prime", V_prime = "V_prime", W_prime = "W_prime", Y_prime = "Y_prime"){

  new_R_func <- function(R_prime_mat, U_mat, V_mat, Y_mat, q_vec, f_vec){

    # Calculating all symmetric IO matrices:
    Z_sym_mat <- matsbyname::matrixproduct_byname(
      matsbyname::transpose_byname(V_mat),
      matsbyname::hatinv_byname(f_vec, keep = "rownames")
    )

    C_sym_mat <- matsbyname::matrixproduct_byname(
      U_mat,
      matsbyname::hatinv_byname(f_vec, keep = "rownames")
    )

    D_sym_mat <- matsbyname::matrixproduct_byname(
      matsbyname::transpose_byname(U_mat),
      matsbyname::hatinv_byname(q_vec, keep = "rownames")
    )

    O_sym_mat <- matsbyname::matrixproduct_byname(
      matsbyname::hatinv_byname(q_vec, keep = "rownames"),
      Y_mat
    )

    A_sym_mat <- matsbyname::matrixproduct_byname(
      Z_sym_mat,
      D_sym_mat
    )

    L_pxp_sym_mat <- matsbyname::Iminus_byname(A_sym_mat) %>% matsbyname::invert_byname()

    L_ixp_sym_mat <- matsbyname::matrixproduct_byname(
      D_sym_mat,
      L_pxp_sym_mat
    )


    # Now, calculating the set of prime matrices:
    q_prime_vec <- matsbyname::matrixproduct_byname(
      L_pxp_sym_mat,
      matsbyname::transpose_byname(R_prime_mat) %>% matsbyname::rowsums_byname()
    )

    Y_prime_mat <- matsbyname::matrixproduct_byname(
      matsbyname::hatize_byname(q_prime_vec, keep = "rownames"),
      O_sym_mat
    )

    U_prime_mat <- matsbyname::matrixproduct_byname(
      matsbyname::hatize_byname(q_prime_vec, keep = "rownames"),
      matsbyname::transpose_byname(D_sym_mat)
    )

    # Issue here.
    V_prime_mat <- matsbyname::matrixproduct_byname(
      matsbyname::matrixproduct_byname(
        L_ixp_sym_mat,
        matsbyname::transpose_byname(R_prime_mat) %>% matsbyname::rowsums_byname()
      ) %>%
        matsbyname::hatize_byname(keep = "rownames"),
      matsbyname::transpose_byname(Z_sym_mat)
    )

    W_prime_mat <- matsbyname::difference_byname(
      matsbyname::transpose_byname(V_prime_mat),
      U_prime_mat
    )


    # Return the new U, V, and Y matrices.
    list(U_prime_mat, V_prime_mat, W_prime_mat, Y_prime_mat) %>% magrittr::set_names(c(U_prime, V_prime, W_prime, Y_prime))


  }

  matsindf::matsindf_apply(.sutmats, FUN = new_R_func, R_prime_mat = R_prime, U_mat = U, V_mat = V, Y_mat = Y,
                           q_vec = q, f_vec = f)
}





