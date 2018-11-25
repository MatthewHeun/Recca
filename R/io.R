#
# This file contains functions that calculate matrices
# relevant to input-ouput (PSUT) analyses
#


#' Calculate several input-output matrices
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param U use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".
#' @param V make (\code{V}) matrix or name of the column in \code{.sutmats}that contains same. Default is "\code{V}".
#' @param Y final demand (\code{Y}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{Y}".
#' @param S_units \code{S_units} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{S_units}".
#' @param y final demand (\code{y}) vector or name of the column in \code{.sutmats} that contains same. Default is "\code{y}".
#'        \code{y} is calculated by \code{rowsums(Y)}.
#' @param q \code{q} vector or name of the column in \code{.sutmats} that contains same. Default is "\code{q}".
#'        \code{q} is calculated by \code{rowsums(U) + y}.
#' @param f \code{f} vector or name of the column in \code{.sutmats} that contains same. Default is "\code{r}".
#'        \code{f} is calculated by \code{colsums(U)}.
#' @param g \code{g} vector or name of the column in \code{.sutmats} that contains same. Default is "\code{g}".
#'        \code{g} is calculated by \code{rowsums(V)}.
#' @param W \code{W} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{W}".
#'        \code{W} is calculated by \code{transpose(V) - U}.
#' @param K \code{K} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{K}".
#'        \code{K} is calculated by \code{U * f_hat_inv}.
#' @param Z \code{Z} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{Z}".
#'        \code{W} is calculated by \code{U * g_hat_inv}.
#' @param C \code{C} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{C}".
#'        \code{C} is calculated by \code{transpose(V) * g_hat_inv}.
#' @param D \code{D} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{D}".
#'        \code{D} is calculated by \code{V * q_hat_inv}.
#' @param A \code{A} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{A}".
#'        \code{A} is calculated by \code{Z * D}.
#' @param L_ixp \code{L_ixp} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{L_ixp}".
#'        \code{L_ixp} is calculated by \code{D * L_pxp}.
#' @param L_pxp \code{L_pxp} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{L_pxp}".
#'        \code{L_pxp} is calculated by \code{(I - Z*D)^-1}.
#'
#' @return a list or data frame containing input-output matrices
#'
#' @export
calc_io_mats <- function(.sutdata = NULL,
                         # Input names
                         U = "U", V = "V", Y = "Y", S_units = "S_units",
                         # Output names
                         y = "y", q = "q", f = "f", g = "g", W = "W", K = "K",
                         Z = "Z", C = "C", D = "D", A = "A", L_ixp = "L_ixp", L_pxp = "L_pxp"){
  io_func <- function(U_mat, V_mat, Y_mat, S_units_mat = NULL){
    yqfgW <- calc_yqfgW(U = U_mat, V = V_mat, Y = Y_mat, S_units = S_units_mat,
                        y = y, q = q,
                        f = f, g = g,
                        W = W)
    q_vec <- yqfgW[[q]]
    f_vec <- yqfgW[[f]]
    g_vec <- yqfgW[[g]]
    ZKCDA <- calc_A(U = U_mat, V = V_mat, q = q_vec, f = f_vec, g = g_vec,
                    Z = Z, C = C, D = D, A = A)
    D_mat <- ZKCDA[[D]]
    A_mat <- ZKCDA[[A]]
    L_mats <- calc_L(D_colname = D_mat, A_colname = A_mat,
                     L_ixp_colname = L_ixp, L_pxp_colname = L_pxp)
    # Set names and return
    c(yqfgW, ZKCDA, L_mats) %>% magrittr::set_names(c(names(yqfgW), names(ZKCDA), names(L_mats)))
  }
  matsindf_apply(.sutdata, FUN = io_func, U_mat = U, V_mat = V, Y_mat = Y, S_units_mat = S_units)
}


#' Calculate \code{y}, \code{f}, \code{g}, and \code{q} vectors and the \code{W} matrix
#'
#' Note that a necessary condition for calculating the \code{f} and \code{g} vectors is that
#' the U_bar and V_bar matrices should have only one entry per column and row, respectively,
#' meaning that all products entering a given industry need to be unit homogeneous
#' before we can calculate the \code{f} vector and
#' all products of a given industry are measured in the same units
#' before we can calculate the \code{g} vector.
#' If the unit homogeneity assumptions above are violated, we will return NA
#' for violating industries in the \code{f} and \code{g} vectors.
#' The checks for unit homogenity are performed only when an \code{S_units} matrix is present.
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param U use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".
#' @param V make (\code{V}) matrix or name of the column in \code{.sutmats}that contains same. Default is "\code{V}".
#' @param Y final demand (\code{Y}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{Y}".
#' @param S_units \code{S_units} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{S_units}".
#' @param y final demand (\code{y}) vector or name of the column in \code{.sutmats} that contains same. Default is "\code{y}".
#'        \code{y} is calculated by \code{rowsums(Y)}.
#' @param f \code{f} vector or name of the column in \code{.sutmats} that contains same. Default is "\code{r}".
#'        \code{f} is calculated by \code{colsums(U)}.
#' @param g \code{g} vector or name of the column in \code{.sutmats} that contains same. Default is "\code{g}".
#'        \code{g} is calculated by \code{rowsums(V)}.
#' @param q \code{q} vector or name of the column in \code{.sutmats} that contains same. Default is "\code{q}".
#'        \code{q} is calculated by \code{rowsums(U) + y}.
#' @param W \code{W} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{W}".
#'        \code{W} is calculated by \code{transpose(V) - U}.
#'
#' @importFrom matsbyname count_vals_incols_byname
#' @importFrom matsbyname count_vals_inrows_byname
#' @importFrom matsbyname compare_byname
#' @importFrom matsbyname all_byname
#'
#' @export
#'
#' @return a list or data frame containing \code{y}, \code{q},
#'          \code{f}, \code{g}, and \code{W}.
calc_yqfgW <- function(.sutdata = NULL,
                       # Input names
                       U = "U", V = "V", Y = "Y", S_units = "S_units",
                       # Output columns
                       y = "y", q = "q",
                       f = "f", g = "g",
                       W = "W"){
  yqfgw_func <- function(U_mat, V_mat, Y_mat, S_units_mat = NULL){
    y_vec <- rowsums_byname(Y_mat)
    q_vec <- sum_byname(rowsums_byname(U_mat), y_vec)
    f_vec <- colsums_byname(U_mat) %>% transpose_byname() # vectors are always column vectors
    g_vec <- rowsums_byname(V_mat)
    W_mat <- difference_byname(transpose_byname(V_mat), U_mat)
    # Deal with any unit homogenity issues for f and g.
    if (!is.null(S_units_mat)) {
      U_bar <- matrixproduct_byname(transpose_byname(S_units_mat), U_mat)
      U_bar_units_OK <- count_vals_incols_byname(U_bar, "!=", 0) %>%
        compare_byname("<=", 1) %>%
        transpose_byname()
      # When we have an Industry whose inputs are not unit-homogeneous,
      # the value for that Industry in the f vector is nonsensical.
      # Replace with NA.
      f_vec[which(!U_bar_units_OK)] <- NA_real_

      V_bar <- matrixproduct_byname(V_mat, S_units_mat)
      V_bar_units_OK <- count_vals_inrows_byname(V_bar, "!=", 0) %>%
        compare_byname("<=", 1)
      # When we have an Industry whose outputs are not unit-homogeneous,
      # the value for that Industry in the g vector is nonsensical.
      # Replace with NA.
      g_vec[which(!V_bar_units_OK)] <- NA_real_
    }
    # Put the values in a list and return the list
    list(y_vec, q_vec, f_vec, g_vec, W_mat) %>%
      magrittr::set_names(c(y, q, f, g, W))
  }
  matsindf_apply(.sutdata, FUN = yqfgw_func, U_mat = U, V_mat = V, Y_mat = Y, S_units_mat = S_units)
}

#' Calculate \code{Z}, \code{D}, \code{C}, and \code{A} matrices
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param U use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".
#' @param V make (\code{V}) matrix or name of the column in \code{.sutmats}that contains same. Default is "\code{V}".
#' @param q \code{q} vector or name of the column in \code{.sutmats} that contains same. Default is "\code{q}".
#' @param f \code{f} vector or name of the column in \code{.sutmats} that contains same. Default is "\code{r}".
#' @param g \code{g} vector or name of the column in \code{.sutmats} that contains same. Default is "\code{g}".
#' @param Z \code{Z} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{Z}".
#'        \code{W} is calculated by \code{U * g_hat_inv}.
#' @param K \code{K} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{K}".
#'        \code{K} is calculated by \code{U * f_hat_inv}.
#' @param C \code{C} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{C}".
#'        \code{C} is calculated by \code{transpose(V) * g_hat_inv}.
#' @param D \code{D} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{D}".
#'        \code{D} is calculated by \code{V * q_hat_inv}.
#' @param A \code{A} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{A}".
#'        \code{A} is calculated by \code{Z * D}.
#'
#' @return a list or data frame containing \code{Z},
#' \code{K}, \code{C}, \code{D}, and \code{A} matrices.
#'
#' @importFrom matsbyname hatinv_byname
#' @importFrom matsbyname invert_byname
#' @importFrom matsbyname matrixproduct_byname
#' @importFrom matsbyname transpose_byname
#' @importFrom matsindf matsindf_apply
#'
#' @export
calc_A <- function(.sutdata = NULL,
                   # Input names
                   U = "U", V = "V", q = "q", f = "f", g = "g",
                   # Output names
                   Z = "Z", K = "K", C = "C", D = "D", A = "A"){
  A_func <- function(U_mat, V_mat, q_vec, f_vec, g_vec){
    # The calculation of C and Z will fail when g contains NA values.
    # NA values can be created when V has any industry whose outputs are unit inhomogeneous.
    # Test here if any entry in g is NA.
    # If so, the value for C will be assigned to NA.
    if (any(is.na(g_vec))) {
      C_mat <- NA_real_
      Z_mat <- NA_real_
    } else {
      C_mat <- matrixproduct_byname(transpose_byname(V_mat), hatinv_byname(g_vec))
      Z_mat <- matrixproduct_byname(U_mat, hatinv_byname(g_vec))
    }
    # The calculation of K will fail when f contains NA values.
    # NA values can be created when U has any industry whose inputs are inhomogeneous.
    # Test here if any entry in f is NA.
    # If so, the value for K will be assigned NA.
    if (any(is.na(f_vec))) {
      K_mat <- NA_real_
    } else {
      K_mat <- matrixproduct_byname(U_mat, hatinv_byname(f_vec))
    }
    D_mat <- matrixproduct_byname(V_mat, hatinv_byname(q_vec))
    A_mat <- matrixproduct_byname(Z_mat, D_mat)
    # Put all output matrices in a list and return it.
    list(Z_mat, K_mat, C_mat, D_mat, A_mat) %>%
      magrittr::set_names(c(Z, K, C, D, A))
  }
  matsindf_apply(.sutdata, FUN = A_func, U_mat = U, V_mat = V, q_vec = q, f_vec = f, g_vec = g)
}


#' Calculates total requirements matrices (\code{L_pxp} and \code{L_ixp})
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param D_colname the name of the column in \code{.sutdata} containing the \code{D} matrix.
#' @param A_colname the name of the column in \code{.sutdata} containing the \code{A} matrix.
#' @param L_ixp_colname the name of the output column containing the industry-by-product L matrix.
#' \code{L_ixp} is calculated by \code{D * L_pxp}.
#' @param L_pxp_colname the name of the output column containing the product-by-product L matrix.
#' \code{L_pxp} is calculated by \code{(I - Z*D)^-1}.
#'
#' @return \code{.sutdata} with columns \code{L_ixp_colname} and \code{L_pxp_colname} added
#'
#' @importFrom matsbyname Iminus_byname
#'
#' @export
calc_L <- function(.sutdata = NULL,
                   # Input columns
                   D_colname = "D", A_colname = "A",
                   # Output columns
                   L_pxp_colname = "L_pxp", L_ixp_colname = "L_ixp"){
  L_func <- function(D, A){
    L_pxp_val <- Iminus_byname(A) %>% invert_byname()
    L_ixp_val <- matrixproduct_byname(D, L_pxp_val)
    list(L_pxp_val, L_ixp_val) %>%
      magrittr::set_names(c(L_pxp_colname, L_ixp_colname))
  }
  matsindf_apply(.sutdata, FUN = L_func, D = D_colname, A = A_colname)
}
