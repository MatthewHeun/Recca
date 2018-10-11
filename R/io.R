#
# This file contains functions that calculate matrices
# relevant to input-ouput (PSUT) analyses
#


#' Calculate various input-output matrices
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param U_colname the name of the column in \code{.sutdata} containing Use (\code{U}) matrices.
#' Default is "\code{U}".
#' @param V_colname the name of the column in \code{.sutdata} containing Make (\code{V}) matrices.
#' Default is "\code{V}".
#' @param Y_colname the name of the column in \code{.sutdata} containing final demand (\code{Y}) matrices.
#' Default is "\code{Y}".
#' @param S_units the name of the column in \code{.sutdata} containing unit summation (\code{S_units}) matrices.
#' Default is "\code{S_units}".
#' @param y_colname the name of the output column containing \code{y} vectors.
#' Default is "\code{y}".
#' \code{y} is calculated by \code{rowsums(Y)}.
#' @param q_colname the name of the output column containing \code{q} vectors.
#' Default is "\code{q}".
#' \code{q} is calculated by \code{rowsums(U) + y}.
#' @param f_colname the name of the output column containing \code{f} vectors.
#' Default is "\code{f}".
#' \code{f} is calculated by \code{colsums(U)}.
#' @param g_colname the name of the output column containing \code{g} vectors.
#' Default is "\code{g}".
#' \code{g} is calculated by \code{rowsums(V)}.
#' @param W_colname the name of the output column containing \code{W} matrices.
#' Default is "\code{W}".
#' \code{W} is calculated by \code{transpose(V) - U}.
#' @param B_colname the name of the output column containing \code{B} matrices.
#' Default is "\code{B}".
#' \code{B} is calculated by \code{U * f_hat_inv}.
#' @param Z_colname the name of the output column containing \code{Z} matrices.
#' Default is "\code{Z}".
#' \code{Z} is calculated by \code{U * g_hat_inv}.
#' @param C_colname the name of the output column containing \code{C} matrices.
#' \code{C} is calculated by \code{transpose(V) * g_hat_inv}.
#' Default is "\code{C}".
#' @param D_colname the name of the output column containing \code{D} matrices.
#' Default is "\code{D}".
#' \code{D} is calculated by \code{V * q_hat_inv}.
#' @param A_colname the name of the output column containing \code{A} matrices.
#' Default is "\code{A}".
#' \code{A} is calculated by \code{Z * D}.
#' @param L_ixp_colname the name of the output column containing industry-by-product L matrices.
#' Default is "\code{L_ixp}".
#' \code{L_ixp} is calculated by \code{D * L_pxp}.
#' @param L_pxp_colname the name of the output column containing product-by-product L matrices.
#' Default is "\code{L_pxp}".
#' \code{L_pxp} is calculated by \code{(I - Z*D)^-1}.
#'
#' @return \code{.sutdata} with additional columns:
#' \code{y_colname}, \code{q_colname}, \code{g_colname}, \code{W_colname},
#' \code{Z_colname}, \code{D_colname}, \code{C_colname}, \code{A_colname},
#' \code{L_ixp_colname}, and \code{L_pxp_colname}.
#'
#' @export
calc_io_mats <- function(.sutdata = NULL,
                         # Input columns
                         U_colname = "U", V_colname = "V", Y_colname = "Y", S_units = "S_units",
                         # Output columns
                         y_colname = "y", q_colname = "q",
                         f_colname = "f", g_colname = "g",
                         W_colname = "W", B_colname = "B",
                         Z_colname = "Z", C_colname = "C", D_colname = "D", A_colname = "A",
                         L_ixp_colname = "L_ixp", L_pxp_colname = "L_pxp"){
  io_func <- function(U, V, Y, S_units = NULL){
    # Clean the matrices, thereby avoiding situations where rows or columns of zeroes
    # cause a _hat_inv step to fail due to inverting a matrix with a row or column of zeroes.
    U <- clean_byname(U, margin = c(1,2), clean_value = 0)
    V <- clean_byname(V, margin = c(1,2), clean_value = 0)
    Y <- clean_byname(Y, margin = c(1,2), clean_value = 0)
    yqfgW <- calc_yqfgW(U_colname = U, V_colname = V, Y_colname = Y, S_units = S_units,
                        y_colname = y_colname, q_colname = q_colname,
                        f_colname = f_colname, g_colname = g_colname,
                        W_colname = W_colname)
    q <- yqfgW$q
    g <- yqfgW$g
    ZCDA <- calc_A(U_colname = U, V_colname = V, q_colname = q, g_colname = g,
                   Z_colname = Z_colname, C_colname = C_colname, D_colname = D_colname, A_colname = A_colname)
    D <- ZCDA$D
    A <- ZCDA$A
    L <- calc_L(D_colname = D, A_colname = A,
                L_ixp_colname = L_ixp_colname, L_pxp_colname = L_pxp_colname)
    c(yqfgW, ZCDA, L) %>% set_names(c(names(yqfgW), names(ZCDA), names(L)))
  }
  matsindf_apply(.sutdata, FUN = io_func, U = U_colname, V = V_colname, Y = Y_colname, S_units = S_units)
}


#' Calculate y, f, g, and q vectors and W matrices
#'
#' Note that a necessary condition for calculating the \code{f} and \code{g} vectors is that
#' the U_bar and V_bar matrices should have only one entry per column and row, respectively,
#' meaning that all products input into a given industry need to be unit homogeneous
#' before we can calculate the \code{f} vector and
#' all products of a given industry are measured in the same units
#' before we can calculate the \code{g} vector.
#' If the unit homogeneity assumptions above are violated, we will return NA
#' for violating industries in the \code{f} and \code{g} vectors.
#' The checks for unit homogenity are performed only when an \code{S_units} matrix is present.
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param U_colname the name of the column in \code{.sutdata} containing Use (\code{U}) matrices.
#' @param V_colname the name of the column in \code{.sutdata} containing Make (\code{V}) matrices.
#' @param Y_colname the name of the column in \code{.sutdata} containing final demand (\code{Y}) matrices.
#' @param S_units the name of the column in \code{.sutdata} containing \code{S_units} matrices.
#' @param y_colname the name of the output column containing \code{y} vectors.
#' \code{y} is calculated by \code{rowsums_byname(Y)}.
#' @param f_colname the name of the output column containing \code{f} vectors.
#' \code{f} is calculated by \code{colsums_byname(U)}.
#' @param g_colname the name of the output column containing \code{g} vectors.
#' \code{g} is calculated by \code{rowsums_byname(V)}.
#' @param q_colname the name of the output column containing \code{q} vectors.
#' \code{q} is calculated by \code{rowsums_byname(U) + y}.
#' @param W_colname the name of the output column containing \code{W} matrices.
#' \code{W} is calculated by \code{transpose_byname(V) - U}.
#'
#' @importFrom matsbyname count_vals_incols_byname
#' @importFrom matsbyname count_vals_inrows_byname
#' @importFrom matsbyname compare_byname
#' @importFrom matsbyname all_byname
#'
#' @export
#'
#' @return \code{.sutdata} with columns \code{y_colname}, \code{q_colname},
#'          \code{f_colname}, \code{g_colname}, and \code{W_colname} added
calc_yqfgW <- function(.sutdata = NULL,
                       # Input columns
                       U_colname = "U", V_colname = "V", Y_colname = "Y", S_units = "S_units",
                       # Output columns
                       y_colname = "y", q_colname = "q",
                       f_colname = "f", g_colname = "g",
                       W_colname = "W"){
  yqfgw_func <- function(U, V, Y, S_units = NULL){
    y_val <- rowsums_byname(Y)
    q_val <- sum_byname(rowsums_byname(U), y_val)
    f_val <- colsums_byname(U) %>% transpose_byname() # vectors are always column vectors
    g_val <- rowsums_byname(V)
    W_val <- difference_byname(transpose_byname(V), U)
    # Deal with any unit homogenity issues for f and g.
    if (!is.null(S_units)) {
      U_bar <- matrixproduct_byname(transpose_byname(S_units), U)
      U_bar_units_OK <- count_vals_incols_byname(U_bar, "!=", 0) %>%
        compare_byname("<=", 1) %>%
        transpose_byname()
      # When we have an Industry whose inputs are not unit-homogeneous,
      # the value for that Industry in the f vector is nonsensical.
      # Replace with NA.
      f_val[which(!U_bar_units_OK)] <- NA_real_

      V_bar <- matrixproduct_byname(V, S_units)
      V_bar_units_OK <- count_vals_inrows_byname(V_bar, "!=", 0) %>%
        compare_byname("<=", 1)
      # When we have an Industry whose outputs are not unit-homogeneous,
      # the value for that Industry in the g vector is nonsensical.
      # Replace with NA.
      g_val[which(!V_bar_units_OK)] <- NA_real_
    }
    # Put the values in a list and return the list
    list(y_val, q_val, f_val, g_val, W_val) %>%
      set_names(c(y_colname, q_colname, f_colname, g_colname, W_colname))
  }
  matsindf_apply(.sutdata, FUN = yqfgw_func, U = U_colname, V = V_colname, Y = Y_colname, S_units = S_units)
}

#' Calculate \code{Z}, \code{D}, \code{C}, and \code{A} matrices
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param U_colname the name of the column in \code{.sutdata} containing Use (\code{U}) matrices.
#' @param V_colname the name of the column in \code{.sutdata} containing Make (\code{V}) matrices.
#' @param q_colname the name of the column in \code{.sutdata} containing \code{q} vectors.
#' @param g_colname the name of the column in \code{.sutdata} containing \code{g} vectors.
#' @param Z_colname the name of the output column containing \code{Z} matrices.
#' \code{Z} is calculated by \code{U * g_hat_inv}.
#' @param C_colname the name of the output column containing \code{C} matrices.
#' \code{C} is calculated by \code{transpose(V) * g_hat_inv}.
#' @param D_colname the name of the output column containing \code{D} matrices.
#' \code{D} is calculated by \code{V * q_hat_inv}.
#' @param A_colname the name of the output column containing \code{A} matrices.
#' \code{A} is calculated by \code{Z * D}.
#'
#' @return \code{.sutdata} with columns \code{Z_colname}, \code{D_colname}, and \code{A_colname} added
#'
#' @importFrom matsbyname hatize_byname
#' @importFrom matsbyname invert_byname
#' @importFrom matsbyname matrixproduct_byname
#' @importFrom matsbyname transpose_byname
#' @importFrom matsindf matsindf_apply
#'
#' @export
calc_A <- function(.sutdata = NULL,
                   # Input columns
                   U_colname = "U", V_colname = "V",
                   q_colname = "q", g_colname = "g",
                   # Output columns
                   Z_colname = "Z", C_colname = "C", D_colname = "D", A_colname = "A"){
  A_func <- function(U, V, q, g){
    Z_val <- matrixproduct_byname(U, hatize_byname(g) %>% invert_byname())
    C_val <- matrixproduct_byname(transpose_byname(V), hatize_byname(g) %>% invert_byname())
    D_val <- matrixproduct_byname(V, hatize_byname(q) %>% invert_byname())
    A_val <- matrixproduct_byname(Z_val, D_val)
    list(Z_val, C_val, D_val, A_val) %>% set_names(Z_colname, C_colname, D_colname, A_colname)
  }
  matsindf_apply(.sutdata, FUN = A_func, U = U_colname, V = V_colname, q = q_colname, g = g_colname)
}


#' Calculates total requirements matrices
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
    list(L_pxp_val, L_ixp_val) %>% set_names(L_pxp_colname, L_ixp_colname)
  }
  matsindf_apply(.sutdata, FUN = L_func, D = D_colname, A = A_colname)
}
