#' Calculate several input-output matrices
#'
#' This function bundles several others and calculates
#' matrices that describe the structure of an energy conversion chain.
#'
#' Some calculations involve a matrix inversion step.
#' The `method` argument specifies which method should be used for
#' calculating the inverse.
#' See `matsbyname::invert_byname()`.
#'
#' `method_q_calculation` specifies the method with which the q vector should be calculated.
#' Default is "sum_U_Y_rows", corresponding to a demand-sided view of **q**.
#' Alternatively, an analyst can choose to use the "sum_R_V_cols" method,
#' corresponding to a supply-sided view of **q**.
#' In the case of a balanced ECC, the method does not matter.
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param method One of "solve", "QR", or "SVD". Default is "solve". See details.
#' @param tol The tolerance for detecting linear dependencies during matrix inversion.
#'            Default is `.Machine$double.eps`.
#' @param method_q_calculation Specifies the method with which the q vector should be calculated. See details.
#' @param R The resources (**R**) matrix or name of the column in `.sutmats` that contains same. Default is "R".
#' @param U The use (**U**) matrix or name of the column in `.sutmats` that contains same. Default is "U".
#' @param U_feed The feed portion of the use matrix (**U_feed**) or name of the column in `.sutmats` that contains same. Default is "U_feed".
#' @param V The make (**V**) matrix or name of the column in `.sutmats`that contains same. Default is "V".
#' @param Y The final demand (**Y**) matrix or name of the column in `.sutmats` that contains same. Default is "Y".
#' @param S_units The unit summation matrix (**S_units**) or name of the column in `.sutmats` that contains same. Default is "S_units".
#' @param y The name for the **y** vector on output. Default is "y".
#'        **y** is calculated by `rowsums(Y)`.
#' @param q The name for the **q** vector on output. Default is "q".
#'        **q** is calculated by `rowsums(U) + y`.
#' @param f The name for the **f** vector on output. Default is "f".
#'        **f** is calculated by `colsums(U)`.
#' @param h The name for the **h** vector on output. Default is "h".
#'        **h** is calculated by `colsums(transpose(R))`.
#' @param g The name for the **g** vector on output. Default is "g".
#'        **g** is calculated by `rowsums(V)`.
#' @param r The name for the **r** vector on output. Default is "r".
#'        **r** is calculated by `rowsums(R)`.
#' @param W The name for the **W** matrix on output. Default is "W".
#'        **W** is calculated by `transpose(V) - U`.
#' @param K The name for the **K** matrix on output. Default is "K".
#'        **K** is calculated by `U * f_hat_inv`.
#' @param Z The name fort the **Z** matrix on output. Default is "Z".
#'        **Z** is calculated by `U * g_hat_inv`.
#' @param C The name for the **C** matrix on output. Default is "C".
#'        **C** is calculated by `transpose(V) * g_hat_inv`.
#' @param D The name for the **D** matrix on output. Default is "D".
#'        **D** is calculated by `V * q_hat_inv`.
#' @param O name for the **O** matrix on output. Default is "O".
#'        **O** is calculated by `R * h_hat_inv`.
#' @param A The name for the **A** matrix on output. Default is "A".
#'        **A** is calculated by `Z * D`.
#' @param L_ixp The name for the **L_ixp** matrix on output. Default is "L_ixp".
#'        **L_ixp** is calculated by `D * L_pxp`.
#' @param L_pxp The name for the **L_pxp_feed** matrix on output. Default is "L_pxp_feed".
#'        **L_pxp** is calculated by `(I - Z*D)^-1`.
#' @param K_feed The name for the **K_feed** matrix on output. Default is "K_feed".
#'        **K_feed** is calculated by `U_feed * f_hat_inv`.
#' @param Z_feed The name for the **Z_feed** matrix on output. Default is "Z_feed".
#'        **Z_feed** is calculated by `U_feed * g_hat_inv`.
#' @param A_feed The name for the **A_feed** matrix on output. Default is "A_feed".
#'        **A_feed** is calculated by `Z_feed * D_feed`.
#' @param L_ixp_feed The name for the **L_ixp_feed** matrix on output. Default is "L_ixp_feed".
#'        **L_ixp_feed** is calculated by `D_feed * L_pxp_feed`.
#' @param L_pxp_feed The name for the **L_pxp_feed** matrix on output. Default is "L_pxp_feed".
#'        **L_pxp_feed** is calculated by `(I - Z_feed*D)^-1`.
#'
#' @return A list or data frame containing input-output matrices.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' UKEnergy2000mats %>%
#'   spread(key = matrix.name, value = matrix) %>%
#'   select(Country, Year, Energy.type, Last.stage, U, U_feed, V, Y, r_EIOU, S_units) %>%
#'   calc_io_mats()
calc_io_mats <- function(.sutdata = NULL,
                         method = c("solve", "QR", "SVD"),
                         tol = .Machine$double.eps,
                         method_q_calculation = c("sum_U_Y_rows", "sum_R_V_cols"),
                         # Input names
                         R = "R", U = "U", U_feed = "U_feed", V = "V", Y = "Y", S_units = "S_units",
                         # Output names
                         y = "y", q = "q", f = "f", g = "g", h = "h", r = "r", W = "W", K = "K",
                         Z = "Z", C = "C", D = "D", A = "A", L_ixp = "L_ixp", L_pxp = "L_pxp", O = "O",
                         Z_feed = "Z_feed", K_feed = "K_feed", A_feed = "A_feed", L_ixp_feed = "L_ixp_feed", L_pxp_feed = "L_pxp_feed"){

  method <- match.arg(method)
  method_q_calculation <- match.arg(method_q_calculation)

  io_func <- function(R_mat = NULL, U_mat, U_feed_mat, V_mat, Y_mat, S_units_mat = NULL){
    yqfgW <- calc_yqfgW(method_q_calculation = method_q_calculation,
                        R = R_mat, U = U_mat, V = V_mat, Y = Y_mat, S_units = S_units_mat,
                        y = y, q = q, f = f, g = g, h = h, r = r,
                        W = W)

    q_vec <- yqfgW[[q]]
    f_vec <- yqfgW[[f]]
    g_vec <- yqfgW[[g]]
    r_vec <- yqfgW[[r]]
    h_vec <- yqfgW[[h]]

    ZKCDA <- calc_A(R = R_mat, U = U_mat, V = V_mat, q = q_vec, f = f_vec, g = g_vec, r = r_vec, h = h_vec,
                    Z = Z, K = K, C = C, D = D, A = A, O = O)

    D_mat <- ZKCDA[[D]]
    A_mat <- ZKCDA[[A]]

    L_mats <- calc_L(method = method, tol = tol,
                     D = D_mat, A = A_mat,
                     L_ixp = L_ixp, L_pxp = L_pxp)

    # Work on the "_feed" matrices.

    ZKCDA_all_feed <- calc_A(R = R_mat, U = U_feed_mat, V = V_mat, q = q_vec, f = f_vec, g = g_vec, r = r_vec, h = h_vec,
                             Z = Z_feed, K = K_feed, C = C, D = D, A = A_feed, O = O)
    ZKCDA_feed <- list(Z_feed = ZKCDA_all_feed[[Z_feed]],
                       K_feed = ZKCDA_all_feed[[K_feed]],
                       A_feed = ZKCDA_all_feed[[A_feed]])

    A_feed_mat <- ZKCDA_feed[[A_feed]]
    L_feed_mats <- calc_L(method = method, tol = tol,
                          D = D_mat, A = A_feed_mat,
                          L_ixp = L_ixp_feed, L_pxp = L_pxp_feed)

    # Return a list
    c(yqfgW, ZKCDA, L_mats, ZKCDA_feed, L_feed_mats)
  }
  matsindf::matsindf_apply(.sutdata, FUN = io_func, R_mat = R, U_mat = U, U_feed_mat = U_feed, V_mat = V, Y_mat = Y, S_units_mat = S_units)
}


#' Calculate **y**, **f**, **g**, **q**, **h**, and **r** vectors and the **W** matrix
#'
#' Note that a necessary condition for calculating the **f**, **g**, and **r** vectors is that
#' the **R_bar**, **U_bar**, and **V_bar** matrices
#' should have only one entry per column and row, respectively,
#' meaning that all products entering a given industry need to be unit homogeneous
#' before we can calculate the **f** vector and
#' all products of a given industry are measured in the same units
#' before we can calculate the **g** vector.
#' If the unit homogeneity assumptions above are violated, we will return `NA`
#' for violating industries in the **f** and **g** vectors.
#' The checks for unit homogeneity are performed only when an **S_units** matrix is present.
#'
#' `method_q_calculation` specifies the method with which the q vector should be calculated.
#' Default is "sum_U_Y_rows", corresponding to a demand-sided view of **q**.
#' Alternatively, an analyst can choose to use the "sum_R_V_cols" method,
#' corresponding to a supply-sided view of **q**.
#' In the case of a balanced ECC, the method does not matter.
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param method_q_calculation Specifies the method with which the q vector should be calculated. See details.
#' @param R The resources (**R**) matrix or name of the column in `.sutmats` that contains same. Default is "R".
#' @param Y The final demand matrix (**Y**) or name of the column in `.sutmats` that contains same. Default is "Y".
#' @param U The use (**U**) matrix or name of the column in `.sutmats` that contains same. Default is "U".
#' @param V The make (**V**) matrix or name of the column in `.sutmats` that contains same. Default is "V".
#' @param Y The final demand (**Y**) matrix or name of the column in `.sutmats`` that contains same. Default is "Y".
#' @param S_units The **S_units** matrix or name of the column in `.sutmats` that contains same. Default is "S_units".
#' @param y The name for the **y** vector on output. Default is "y".
#'        **y** is calculated by `rowsums(Y)`.
#' @param q The name for the **q** vector on output. Default is "q".
#'        **q** is calculated by `rowsums(U) + y`.
#' @param f The name for the **f** vector on output. Default is "f".
#'        **f** is calculated by `colsums(U)`.
#' @param g The name for the **g** vector on output. Default is "g".
#'        **g** is calculated by `rowsums(V)`.
#' @param h The name for the **h** vector on output. Default is "h".
#'        **h** is calculated by `colsums(transpose(R))`.
#' @param r The name for the **r** vector on output. Default is "r".
#'        **r** is calculated by `rowsums(R)`.
#' @param W The name for the **W** matrix on output. Default is "W".
#'        **W** is calculated by `transpose(V) - U`.
#'
#' @export
#'
#' @return A list or data frame containing **y**, **q**,
#'         **f**, **g**, **h**, and **r** vectors and the **W** matrix.
calc_yqfgW <- function(.sutdata = NULL,
                       method_q_calculation = c("sum_U_Y_rows", "sum_R_V_cols"),
                       # Input names
                       R = "R", U = "U", V = "V", Y = "Y", S_units = "S_units",
                       # Output columns
                       y = "y", q = "q", f = "f", g = "g", h = "h", r = "r",
                       W = "W"){

  method_q_calculation <- match.arg(method_q_calculation)

  yqfgw_func <- function(R_mat = NULL, U_mat, V_mat, Y_mat, S_units_mat = NULL){
    y_vec <- matsbyname::rowsums_byname(Y_mat)

    if (method_q_calculation == "sum_U_Y_rows") {
      q_vec <- matsbyname::sum_byname(matsbyname::rowsums_byname(U_mat), y_vec)
    }
    if (method_q_calculation == "sum_R_V_cols") {
      q_vec <- matsbyname::sum_byname(matsbyname::colsums_byname(R_mat), matsbyname::colsums_byname(V_mat)) %>%
        matsbyname::transpose_byname()
    }

    f_vec <- matsbyname::colsums_byname(U_mat) %>% matsbyname::transpose_byname() # vectors are always column vectors
    if (is.null(R_mat)) {
      # No R matrix, just use the V matrix, assuming that resources are included there.
      R_plus_V_mat <- V_mat
    } else {
      # An R matrix is present. Sum R and V before proceeding.
      R_plus_V_mat <- matsbyname::sum_byname(R_mat, V_mat)
    }
    g_vec <- matsbyname::rowsums_byname(V_mat)
    r_vec <- matsbyname::rowsums_byname(R_mat)
    h_vec <- matsbyname::colsums_byname(R_mat) %>% matsbyname::transpose_byname()
    W_mat <- matsbyname::difference_byname(matsbyname::transpose_byname(V_mat), U_mat)
    # Deal with any unit homogeneity issues for f and g.
    if (!is.null(S_units_mat)) {
      U_bar <- matsbyname::matrixproduct_byname(matsbyname::transpose_byname(S_units_mat), U_mat)
      U_bar_units_OK <- matsbyname::count_vals_incols_byname(U_bar, "!=", 0) %>%
        matsbyname::compare_byname("<=", 1) %>%
        matsbyname::transpose_byname()
      # When we have an Industry whose inputs are not unit-homogeneous,
      # the value for that Industry in the f vector is nonsensical.
      # Replace with NA.
      f_vec[which(!U_bar_units_OK)] <- NA_real_

      RV_bar <- matsbyname::matrixproduct_byname(R_plus_V_mat, S_units_mat)
      RV_bar_units_OK <- matsbyname::count_vals_inrows_byname(RV_bar, "!=", 0) %>%
        matsbyname::compare_byname("<=", 1)
      # When we have an Industry whose outputs are not unit-homogeneous,
      # the value for that Industry in the g vector is nonsensical.
      # Replace with NA.
      g_vec[which(!RV_bar_units_OK)] <- NA_real_
    }
    # Put the values in a list and return the list
    list(y_vec, q_vec, f_vec, g_vec, h_vec, r_vec, W_mat) %>%
      magrittr::set_names(c(y, q, f, g, h, r, W))
  }
  matsindf::matsindf_apply(.sutdata, FUN = yqfgw_func, R_mat = R, U_mat = U, V_mat = V, Y_mat = Y, S_units_mat = S_units)
}


#' Calculate **Z**, **K**, **C**, **D**, **A**, and **O** matrices
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param R resources (**R**) matrix or name of the column in `.sutmats` that contains same. Default is "R".
#'          `R` is an optional argument.
#'          If all of **R** is added to **V**, this argument can be left unspecified.
#' @param U Use (**U**) matrix or name of the column in `.sutmats` that contains same. Default is "U".
#' @param V Make (**V**) matrix or name of the column in `.sutmats`that contains same. Default is "V".
#' @param q A **q** vector or name of the column in `.sutmats` that contains same. Default is "q".
#' @param f An **f** vector or name of the column in `.sutmats` that contains same. Default is "r".
#' @param g A **g** vector or name of the column in `.sutmats` that contains same. Default is "g".
#' @param r An **r** vector or name of the column in `.sutmats` that contains same. Default is "r".
#' @param h An **h** vector or name of the column in `.sutmats` that contains same. Default is "h".
#' @param Z The name for the **Z** matrix on output. Default is "Z".
#'        **Z** is calculated by `U * g_hat_inv`.
#' @param K The name for the **K** matrix on output. Default is "K".
#'        **K** is calculated by `U * f_hat_inv`.
#' @param C The name for the **C** matrix on output. Default is "C".
#'        **C** is calculated by `transpose(V) * g_hat_inv`.
#' @param D The name for the **D** matrix on output. Default is "D".
#'        **D** is calculated by `V * q_hat_inv`.
#' @param A The name for the **A** matrix on output. Default is "A".
#'        **A** is calculated by Z * D`.
#' @param O The name for the **O** matrix on output. Default is "O".
#'        **O** is calculated by `r_hat_inv * R`.
#'
#' @return A list or data frame containing **Z**,
#'         **K**, **C**, **D**, and **A** matrices.
#'
#' @export
calc_A <- function(.sutdata = NULL,
                   # Input names
                   R = "R", U = "U", V = "V", q = "q", f = "f", g = "g", r = "r", h = "h",
                   # Output names
                   Z = "Z", K = "K", C = "C", D = "D", A = "A", O = "O"){
  A_func <- function(R_mat, U_mat, V_mat, q_vec, f_vec, g_vec, r_vec, h_vec){
    if (is.null(R_mat)) {
      # No R matrix, just use the V matrix, assuming that resources are included there.
      R_plus_V_mat <- V_mat
    } else {
      # An R matrix is present. Sum R and V before proceeding.
      R_plus_V_mat <- matsbyname::sum_byname(R_mat, V_mat)
    }
    # If g is a 1-row vector (because V_mat has only 1 row),
    # need to strip off the column name, because hatinv(g) will fail.
    # But actually we don't care about the name of the single column in the g vector,
    # so delete the column name.
    # We use hatinv(g) in several places below, so calculate it once here.
    ghatinv <- g_vec %>%
      matsbyname::hatinv_byname(keep = "rownames")
    # The calculation of C and Z will fail when g contains NA values.
    # NA values can be created when V has any industry whose outputs are unit inhomogeneous.
    # Test here if any entry in g is NA.
    # If so, the value for C will be assigned to NA.
    if (any(is.na(g_vec))) {
      C_mat <- NA_real_ %>%
        # rowtype of C_mat is rowtype(transpose(R_plus_V_mat)), which is same as coltype(R_plus_V_mat))
        matsbyname::setrowtype(matsbyname::coltype(V_mat)) %>%
        matsbyname::setcoltype(matsbyname::coltype(ghatinv))
      Z_mat <- NA_real_ %>%
        matsbyname::setrowtype(matsbyname::rowtype(U_mat)) %>%
        matsbyname::setcoltype(matsbyname::coltype(ghatinv))
    } else {
      C_mat <- matsbyname::matrixproduct_byname(matsbyname::transpose_byname(V_mat), ghatinv)
      Z_mat <- matsbyname::matrixproduct_byname(U_mat, ghatinv)
    }
    # The calculation of K will fail when f contains NA values.
    # NA values can be created when U has any industry whose inputs are inhomogeneous.
    # Test here if any entry in f is NA.
    # If so, the value for K will be assigned NA.
    if (any(is.na(f_vec))) {
      K_mat <- NA_real_ %>%
        matsbyname::setrowtype(U_mat) %>%
        matsbyname::setcoltype(matsbyname::coltype(matsbyname::hatinv_byname(f_vec, keep = "rownames")))
    } else {
      K_mat <- matsbyname::matrixproduct_byname(U_mat, matsbyname::hatinv_byname(f_vec, keep = "rownames"))
    }

    D_mat <- matsbyname::matrixproduct_byname(V_mat, matsbyname::hatinv_byname(q_vec, keep = "rownames"))
    A_mat <- matsbyname::matrixproduct_byname(Z_mat, D_mat)
    O_mat <- matsbyname::matrixproduct_byname(R_mat, matsbyname::hatinv_byname(h_vec, keep = "rownames"))

    # Put all output matrices in a list and return it.
    list(Z_mat, K_mat, C_mat, D_mat, A_mat, O_mat) %>%
      magrittr::set_names(c(Z, K, C, D, A, O))
  }
  matsindf::matsindf_apply(.sutdata, FUN = A_func, R_mat = R, U_mat = U, V_mat = V, q_vec = q, f_vec = f, g_vec = g, r_vec = r, h_vec = h)
}


#' Calculates total requirements matrices (**L_pxp** and **L_ixp**)
#'
#' **L_pxp** tells how much of a product (in a row) is required to make another product (in a column).
#' **L_ixp** tells how much of an industry's output (in a row) is required to make another product (in a column).
#'
#' Calculating some matrices requires
#' a matrix inversion operation.
#' The `method` argument specifies which method should be used for
#' calculating the inverse.
#' See `matsbyname::invert_byname()`.
#'
#' Both `tol` and `method` should be single values and apply to all matrices being inverted.
#'
#' @param .sutdata A data frame of supply-use table matrices with matrices arranged in columns.
#'                 Default is `NULL`, meaning that matrices will be taken from the `D` and `A` arguments.
#'                 Set to a list or data frame to pull matrices from its store.
#' @param method One of "solve", "QR", or "SVD". Default is "solve". See details.
#' @param tol The tolerance for detecting linear dependencies during matrix inversion.
#'            Default is `.Machine$double.eps`.
#' @param D The **D** matrix or name of the column in `.sutmats` that contains same. Default is "D".
#' @param A The **A** matrix or name of the column in `.sutmats` that contains same. Default is "A".
#' @param L_pxp The name for the **L_pxp** matrix on output. Default is "L_pxp".
#'              `L_pxp` is calculated by `inverse(I - A)`.
#' @param L_ixp The name for the **L_ixp** matrix on output. Default is "L_ixp".
#'              **L_ixp** is calculated by `D * L_pxp`.
#'
#' @return A list or data frame containing **L_pxp** and **L_ixp** matrices.
#'
#' @export
calc_L <- function(.sutdata = NULL,
                   method = c("solve", "QR", "SVD"),
                   tol = .Machine$double.eps,
                   # Input names
                   D = "D", A = "A",
                   # Output names
                   L_pxp = "L_pxp", L_ixp = "L_ixp"){
  method <- match.arg(method)
  L_func <- function(D_mat, A_mat){
    L_pxp_mat <- matsbyname::Iminus_byname(A_mat) %>%
      matsbyname::invert_byname(method = method, tol = tol)
    L_ixp_mat <- matsbyname::matrixproduct_byname(D_mat, L_pxp_mat)
    list(L_pxp_mat, L_ixp_mat) %>%
      magrittr::set_names(c(L_pxp, L_ixp))
  }
  matsindf::matsindf_apply(.sutdata, FUN = L_func, D_mat = D, A_mat = A)
}
