
#' Reconstruct an economy given a new final demand matrix
#'
#' When the final demand matrix changes from **Y** to **Y_prime**,
#' this function calculates new
#' resource (**R_prime**),
#' use (**U_prime**),
#' feed (**U_feed**),
#' energy industry own use (**U_eiou**),
#' ratio (**r_eiou**),
#' and make (**V_prime**) matrices
#' that would be required to meet the new final demand (**Y_prime**).
#'
#' Note that inputs **L_ixp**, **L_pxp**,
#' **Z**, and **D** can be
#' conveniently calculated by the function `calc_io_mats()`.
#'
#' Internally, this function uses `matsindf::matsindf_apply()`,
#' and documentation assumes that
#' `.sutmats` is not `NULL` and is a data frame.
#' But `.sutmats` can also be a named list of matrices.
#' Or matrices can be supplied individually to the
#' `Y_prime`, `L_ixp`, `L_pxp`, `Z`, `Z_feed`, `D`, and `O` arguments.
#' If `.sutmats` is present, output is a data frame with columns named by string values of output arguments, and
#' input arguments should be character strings that name columns in `.sutmats`.
#' If `.sutmats` is `NULL` (the default), output is a list with items named by output strings,
#' and input arguments should be single matrices or vectors.
#'
#' @param .sutmats A data frame of supply-use table matrices with matrices arranged in columns.
#' @param Y_prime A new final demand matrix or name of a column in `.sutmats` containing same.
#'                Default is "Y_prime".
#' @param L_ixp,L_pxp,Z,Z_feed,D,O Input matrices that describe the structure of the energy conversion chain.
#'                                 Values can be string names (the default) for columns in a data frame `.sutmats`
#'                                 or names of items in a list `.sutmats`
#' @param R_prime,U_prime,U_feed_prime,U_eiou_prime,r_eiou_prime,V_prime The new names for new matrices.
#'                                                                       Defaults are each argument name as a string.
#'
#' @return A list or data frame containing **R_prime**, **U_prime**, **U_feed**, **U_eiou**, **r_eiou**, and **V_prime** matrices.
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
                  Z = "Z", Z_feed = "Z_feed", D = "D", O = "O",
                  # Output names
                  R_prime = "R_prime",
                  U_prime = "U_prime", U_feed_prime = "U_feed_prime",
                  U_eiou_prime = "U_EIOU_prime", r_eiou_prime = "r_EIOU_prime",
                  V_prime = "V_prime"){

  new_Y_func <- function(Y_prime_mat, L_ixp_mat, L_pxp_mat, Z_mat, Z_feed_mat, D_mat, O_mat){

    if (is.null(Y_prime_mat)){
      R_prime_mat <- NULL
      U_prime_mat <- NULL
      U_feed_prime_mat <- NULL
      U_eiou_prime_mat <- NULL
      r_eiou_prime_mat <- NULL
      V_prime_mat <- NULL
    } else {
      y_prime_vec <- matsbyname::rowsums_byname(Y_prime_mat)

      g_prime_vec <- matsbyname::matrixproduct_byname(L_ixp_mat, y_prime_vec)

      q_prime_vec <- matsbyname::matrixproduct_byname(L_pxp_mat, y_prime_vec)

      g_prime_hat_mat <- matsbyname::hatize_byname(g_prime_vec, keep = "rownames")

      U_prime_mat <- matsbyname::matrixproduct_byname(Z_mat, g_prime_hat_mat)

      U_feed_prime_mat <- matsbyname::matrixproduct_byname(Z_feed_mat, g_prime_hat_mat)

      U_eiou_prime_mat <- matsbyname::difference_byname(U_prime_mat, U_feed_prime_mat)

      r_eiou_prime_mat <- matsbyname::quotient_byname(U_eiou_prime_mat, U_prime_mat) %>%
        matsbyname::replaceNaN_byname(val = 0)

      V_prime_mat <- matsbyname::matrixproduct_byname(D_mat, matsbyname::hatize_byname(q_prime_vec, keep = "rownames"))

      W_prime_mat <- matsbyname::difference_byname(matsbyname::transpose_byname(V_prime_mat), U_prime_mat)

      w_prime_vec <- matsbyname::rowsums_byname(W_prime_mat)

      h_prime_vec <- matsbyname::difference_byname(y_prime_vec, w_prime_vec)

      R_prime_mat <- matsbyname::matrixproduct_byname(O_mat, matsbyname::hatize_byname(h_prime_vec, keep = "rownames"))
    }

    list(R_prime_mat,
         U_prime_mat, U_feed_prime_mat, U_eiou_prime_mat, r_eiou_prime_mat,
         V_prime_mat) %>%
      magrittr::set_names(c(R_prime,
                            U_prime, U_feed_prime, U_eiou_prime, r_eiou_prime,
                            V_prime))
  }

  matsindf::matsindf_apply(.sutmats, FUN = new_Y_func,
                           Y_prime_mat = Y_prime, L_ixp_mat = L_ixp, L_pxp_mat = L_pxp,
                           Z_mat = Z, Z_feed_mat = Z_feed, D_mat = D, O_mat = O)
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
#'     k_prime = matsbyname::select_cols_byname(K,
#'            retain_pattern = RCLabels::make_or_pattern("Electric transport",
#'                                                       pattern_type = "exact")),
#'     k_prime = RCLabels::make_list(k_prime_vec, n = 1)
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
                                        matsbyname::colsums_byname(k_prime_2),
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
#' i.e. of a new resources matrix **R_prime** on the rest of the ECC matrices (**U**, **V**, **W**, and **Y**).
#' New versions of the **U**, **V**, **W**, and **Y** matrices are returned,
#' and respectively called `U_prime`, `V_prime`, `W_prime`, and `Y_prime`.
#' This function assumes that each industry's inputs are perfectly substitutable (ps).
#'
#' Each industry must be unit-homogeneous on its inputs.
#' If not, a matrix populated with `NA` is returned as the result for
#' **U_prime**, **V_prime**, and **Y_prime**.
#'
#' Calculating the new matrices requires
#' a matrix inversion operation.
#' The `method` argument specifies which method should be used for
#' calculating the inverse.
#' "solve" uses `base::solve()` and the value of `tol`.
#' "QR" uses `base::solve.qr()` and the value of `tol`.
#' "SVD" uses `matrixcalc::svd.inverse()`, ignoring the `tol` argument.
#'
#' Both `tol` and `method` should be a single values and apply to all matrices in `a`.
#'
#' @param .sutmats a data frame of supply-use table matrices with matrices arranged in columns.
#' @param method One of "solve", "QR", or "SVD". Default is "solve". See details.
#' @param tol The tolerance for detecting linear dependencies during matrix inversion.
#'            Default is `.Machine$double.eps`.
#' @param R_prime The name of the new **R** matrix column in the input data frame, for which the new ECC must be assessed.
#'                Default is "R_prime".
#' @param U The name of the **U** matrix column in the input data frame.
#'          Default is "U".
#' @param U_feed The name of the **U_feed** matrix column in the input data frame.
#'               Default is "U_feed".
#' @param V The name of the **V** matrix column in the input data frame.
#'          Default is "V".
#' @param Y The name of the **Y** matrix column in the input data frame.
#'          Default is "Y".
#' @param q The name of the **q** vector column in the input data frame.
#'          Default is "q".
#' @param f The name of the **f** vector column in the input data frame.
#'          Default is "f".
#' @param G_pxp The name of the **G_pxp** matrix column in the input data frame.
#'              Default is "G_pxp".
#' @param G_ixp The name of the **G_ixp** matrix column in the input data frame.
#'              Default is "G_ixp".
#' @param O_s The name of the **O_s** matrix column in the input data frame.
#'            Default is "O_s", where "_s" indicates supply-sided.
#' @param D_s The name of the **D_s** matrix column in the input data frame.
#'            Default is "D_s", where "_s" indicates supply-sided.
#' @param D_feed_s The name of the **D_feed_s** matrix column in the input data frame.
#'                 Default is "D_feed_s", where "_s" indicates supply-sided.
#' @param Z_s The name of the **Z_s** matrix column in the input data frame.
#'            Default is "Z_s", where "_s" indicates supply-sided.
#' @param U_prime The name of the output column containing the new **U** matrices.
#'                Default is "U_prime".
#' @param U_feed_prime The name of the output column containing the new **U_feed** matrices.
#'                     Default is "U_feed_prime".
#' @param U_eiou_prime The name of the output column containing the new **U_EIOU** matrices.
#'                     Default is "U_EIOU_prime".
#' @param r_eiou_prime The name of the output column containing the new **r_EIOU** matrices.
#'                     Default is "r_EIOU_prime".
#' @param V_prime The name of the output column containing the new **V** matrices.
#'                Default is "V_prime".
#' @param Y_prime The name of the output column containing the new **Y** matrices.
#'                Default is "Y_prime".
#'
#' @return A data frame with added columns representing each of the new
#'         **U_prime**, **U_feed_prime**, **U_EIOU_prime**, **r_EIOU_prime**,
#'         **V_prime**, and **Y_prime** matrices.
#' @export
#'
#' @examples
#' UKEnergy2000mats %>%
#'   tidyr::spread(key = "matrix.name", value = "matrix") %>%
#'   # Calculate the input-output matrices which are inputs to the new_R function.
#'   calc_io_mats(direction = "downstream") %>%
#'   # Make an R_prime matrix that gives twice the resource inputs to the economy.
#'   dplyr::mutate(
#'     R_prime = matsbyname::hadamardproduct_byname(2, R)
#'   ) %>%
#'   # Now call the new_R function which will calculate
#'   # updated U, V, and Y matrices (U_prime, V_prime, and Y_prime)
#'   # given R_prime.
#'   # Each of the *_prime matrices should be 2x their originals,
#'   # because R_prime is 2x relative to R.
#'   new_R_ps()
new_R_ps <- function(.sutmats = NULL,
                     method = c("solve", "QR", "SVD"),
                     tol = .Machine$double.eps,
                     # Input names
                     R_prime = "R_prime",
                     U = "U", U_feed = "U_feed", V = "V", Y = "Y",
                     q = "q", f = "f",
                     G_pxp = "G_pxp", G_ixp = "G_ixp",
                     O_s = "O_s", D_s = "D_s", D_feed_s = "D_feed_s", Z_s = "Z_s",
                     # Output names
                     U_prime = "U_prime", U_feed_prime = "U_feed_prime", U_eiou_prime = "U_EIOU_prime", r_eiou_prime = "r_EIOU_prime",
                     V_prime = "V_prime", Y_prime = "Y_prime"){

  method <- match.arg(method)

  new_R_func <- function(R_prime_mat, U_mat, U_feed_mat, V_mat, Y_mat,
                         q_vec, f_vec,
                         G_pxp_mat, G_ixp_mat, O_s_mat, D_s_mat, D_feed_s_mat, Z_s_mat){

    # Calculate the set of prime matrices for the downstream swim.

    h_prime_vec <- matsbyname::colsums_byname(R_prime_mat) %>%
      matsbyname::transpose_byname()

    q_prime_vec <- matsbyname::matrixproduct_byname(G_pxp_mat, h_prime_vec)

    q_prim_vec_hat <- matsbyname::hatize_byname(q_prime_vec, keep = "rownames")

    U_prime_mat <- matsbyname::matrixproduct_byname(
      q_prim_vec_hat,
      matsbyname::transpose_byname(D_s_mat)
    )

    f_prime_vec <- matsbyname::colsums_byname(U_prime_mat) %>%
      matsbyname::transpose_byname()

    Y_prime_mat <- matsbyname::matrixproduct_byname(
      q_prim_vec_hat,
      O_s_mat
    )

    U_feed_prime_mat <- matsbyname::matrixproduct_byname(
      q_prim_vec_hat,
      matsbyname::transpose_byname(D_feed_s_mat)
    )

    U_eiou_prime_mat <- matsbyname::difference_byname(U_prime_mat, U_feed_prime_mat)

    r_eiou_prime_mat <- matsbyname::quotient_byname(U_eiou_prime_mat, U_prime_mat) %>%
      matsbyname::replaceNaN_byname(val = 0)

    V_prime_mat <- matsbyname::matrixproduct_byname(
      matsbyname::hatize_byname(f_prime_vec, keep = "rownames"),
      matsbyname::transpose_byname(Z_s_mat)
    )

    # Return the new U, V, and Y matrices.
    list(U_prime_mat, U_feed_prime_mat, U_eiou_prime_mat, r_eiou_prime_mat,
         V_prime_mat, Y_prime_mat) %>%
      magrittr::set_names(c(U_prime, U_feed_prime, U_eiou_prime, r_eiou_prime,
                            V_prime, Y_prime))
  }

  matsindf::matsindf_apply(.sutmats, FUN = new_R_func, R_prime_mat = R_prime,
                           U_mat = U, U_feed_mat = U_feed,
                           V_mat = V, Y_mat = Y,
                           q_vec = q, f_vec = f,
                           G_pxp_mat = G_pxp, G_ixp_mat = G_ixp, O_s_mat = O_s,
                           D_s_mat = D_s, D_feed_s_mat = D_feed_s, Z_s_mat = Z_s)
}





