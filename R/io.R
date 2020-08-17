#
# This file contains functions that calculate matrices
# relevant to input-ouput (PSUT) analyses
#


#' Calculate several input-output matrices
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param R resources (`R`) matrix or name of the column in `.sutmats` that contains same. Default is "`R`".
#' @param U use (`U`) matrix or name of the column in `.sutmats`` that contains same. Default is "U".
#' @param U_feed use matrix or name of the column in `.sutmats` that contains same. Default is "U_feed".
#' @param V make (`V`) matrix or name of the column in `.sutmats`that contains same. Default is "V".
#' @param Y final demand (`Y`) matrix or name of the column in `.sutmats` that contains same. Default is "Y".
#' @param S_units (`S_units`) matrix or name of the column in `.sutmats` that contains same. Default is "S_units".
#' @param y name for `y` vector on output. Default is "y".
#'        `y` is calculated by `rowsums(Y)`.
#' @param q name for `q` vector on output. Default is "q".
#'        `q` is calculated by `rowsums(U) + y`.
#' @param f name for `f` vector on output. Default is "f".
#'        `f` is calculated by `colsums(U)`.
#' @param g name for `g` vector on output. Default is "g".
#'        `g` is calculated by `rowsums(V)`.
#' @param W name for `W` matrix on output. Default is "W".
#'        `W` is calculated by `transpose(V) - U`.
#' @param K name for `K` matrix on output. Default is "K".
#'        `K` is calculated by `U * f_hat_inv`.
#' @param Z name for `Z` matrix on output. Default is "Z".
#'        `Z` is calculated by `U * g_hat_inv`.
#' @param C name for `C` matrix on output. Default is "C".
#'        `C` is calculated by `transpose(V) * g_hat_inv`.
#' @param D name for `D` matrix on output. Default is "D".
#'        `D` is calculated by `V * q_hat_inv`.
#' @param A name for `A` matrix on output. Default is "A".
#'        `A` is calculated by `Z * D`.
#' @param L_ixp name for `L_ixp` matrix on output. Default is "L_ixp".
#'        `L_ixp` is calculated by `D * L_pxp`.
#' @param L_pxp name for `L_pxp_feed` matrix on output. Default is "L_pxp_feed".
#'        `L_pxp` is calculated by `(I - Z*D)^-1`.
#' @param K_feed name for `K_feed` matrix on output. Default is "K_feed".
#'        `K_feed` is calculated by `U_feed * f_hat_inv`.
#' @param Z_feed name for `Z_feed` matrix on output. Default is "Z_feed".
#'        `Z_feed` is calculated by `U_feed * g_hat_inv`.
#' @param C_feed name for `C_feed` matrix on output. Default is "C_feed".
#'        `C_feed` is calculated by `transpose(V) * g_hat_inv`.
#' @param D_feed name for `D_feed` matrix on output. Default is "D_feed".
#'        `D_feed` is calculated by `V * q_hat_inv`.
#' @param A_feed name for `A_feed` matrix on output. Default is "A_feed".
#'        `A_feed` is calculated by `Z_feed * D_feed`.
#' @param L_ixp_feed name for `L_ixp_feed` matrix on output. Default is "L_ixp_feed".
#'        `L_ixp_feed` is calculated by `D_feed * L_pxp_feed`.
#' @param L_pxp_feed name for `L_pxp_feed` matrix on output. Default is "L_pxp_feed".
#'        `L_pxp_feed` is calculated by `(I - Z_feed*D)^-1`.
#'
#' @return a list or data frame containing input-output matrices
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
                         # Input names
                         R = "R", U = "U", U_feed = "U_feed", V = "V", Y = "Y", S_units = "S_units",
                         # Output names
                         y = "y", q = "q", f = "f", g = "g", W = "W", K = "K",
                         Z = "Z", C = "C", D = "D", A = "A", L_ixp = "L_ixp", L_pxp = "L_pxp",
                         Z_feed = "Z_feed", K_feed = "K_feed", C_feed = "C_feed", D_feed = "D_feed", A_feed = "A_feed", L_ixp_feed = "L_ixp_feed", L_pxp_feed = "L_pxp_feed"){
  io_func <- function(R_mat = NULL, U_mat, U_feed_mat, V_mat, Y_mat, S_units_mat = NULL){
    yqfgW <- calc_yqfgW(R = R_mat, U = U_mat, V = V_mat, Y = Y_mat, S_units = S_units_mat,
                        y = y, q = q,
                        f = f, g = g,
                        W = W)
    q_vec <- yqfgW[[q]]
    f_vec <- yqfgW[[f]]
    g_vec <- yqfgW[[g]]
    ZKCDA <- calc_A(R = R_mat, U = U_mat, V = V_mat, q = q_vec, f = f_vec, g = g_vec,
                    Z = Z, K = K, C = C, D = D, A = A)
    D_mat <- ZKCDA[[D]]
    A_mat <- ZKCDA[[A]]
    L_mats <- calc_L(D = D_mat, A = A_mat,
                     L_ixp = L_ixp, L_pxp = L_pxp)
    # Set names
    # c(yqfgW, ZKCDA, L_mats) %>% magrittr::set_names(c(names(yqfgW), names(ZKCDA), names(L_mats)))

    # Work on the "_feed" matrices.

    ZKCDA_feed <- calc_A(R = R_mat, U = U_feed_mat, V = V_mat, q = q_vec, f = f_vec, g = g_vec,
                    Z = Z_feed, K = K_feed, C = C_feed, D = D_feed, A = A_feed)
    A_feed_mat <- ZKCDA_feed[[A_feed]]
    L_feed_mats <- calc_L(D = D_mat, A = A_feed_mat,
                     L_ixp = L_ixp_feed, L_pxp = L_pxp_feed)

    # Set names
    # c(yqfgW, ZKCDA, ZKCDA_feed, L_mats, L_feed_mats) %>%
    #   magrittr::set_names(c(names(yqfgW), names(ZKCDA), names(ZKCDA_feed), names(L_mats), names(L_feed_mats)))

    # Return a list
    c(yqfgW, ZKCDA, L_mats, ZKCDA_feed, L_feed_mats)
  }
  matsindf::matsindf_apply(.sutdata, FUN = io_func, R_mat = R, U_mat = U, U_feed_mat = U_feed, V_mat = V, Y_mat = Y, S_units_mat = S_units)
}


#' Calculate `y`, `f`, `g`, and `q` vectors and the `W` matrix
#'
#' Note that a necessary condition for calculating the `f` and `g` vectors is that
#' the U_bar and V_bar matrices should have only one entry per column and row, respectively,
#' meaning that all products entering a given industry need to be unit homogeneous
#' before we can calculate the \code{f} vector and
#' all products of a given industry are measured in the same units
#' before we can calculate the \code{g} vector.
#' If the unit homogeneity assumptions above are violated, we will return NA
#' for violating industries in the \code{f} and \code{g} vectors.
#' The checks for unit homogeneity are performed only when an \code{S_units} matrix is present.
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param R resources (`R`) matrix or name of the column in `.sutmats` that contains same. Default is "`R`".
#' @param U use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".
#' @param V make (\code{V}) matrix or name of the column in \code{.sutmats}that contains same. Default is "\code{V}".
#' @param Y final demand (\code{Y}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{Y}".
#' @param S_units \code{S_units} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{S_units}".
#' @param y name for \code{y} vector on output. Default is "\code{y}".
#'        \code{y} is calculated by \code{rowsums(Y)}.
#' @param q name for \code{q} vector on output. Default is "\code{q}".
#'        \code{q} is calculated by \code{rowsums(U) + y}.
#' @param f name for \code{f} vector on output. Default is "\code{f}".
#'        \code{f} is calculated by \code{colsums(U)}.
#' @param g name for \code{g} vector on output. Default is "\code{g}".
#'        \code{g} is calculated by \code{rowsums(V)}.
#' @param W name for \code{W} matrix on output. Default is "\code{W}".
#'        \code{W} is calculated by \code{transpose(V) - U}.
#'
#' @export
#'
#' @return a list or data frame containing \code{y}, \code{q},
#'          \code{f}, \code{g}, and \code{W}.
calc_yqfgW <- function(.sutdata = NULL,
                       # Input names
                       R = "R", U = "U", V = "V", Y = "Y", S_units = "S_units",
                       # Output columns
                       y = "y", q = "q",
                       f = "f", g = "g",
                       W = "W"){
  yqfgw_func <- function(R_mat = NULL, U_mat, V_mat, Y_mat, S_units_mat = NULL){
    y_vec <- matsbyname::rowsums_byname(Y_mat)
    q_vec <- matsbyname::sum_byname(matsbyname::rowsums_byname(U_mat), y_vec)
    f_vec <- matsbyname::colsums_byname(U_mat) %>% matsbyname::transpose_byname() # vectors are always column vectors
    if (is.null(R_mat)) {
      # No R matrix, just use the V matrix, assuming that resouces are included there.
      R_plus_V_mat <- V_mat
    } else {
      # An R matrix is present. Sum R and V before proceeding.
      R_plus_V_mat <- matsbyname::sum_byname(R_mat, V_mat)
    }
    g_vec <- matsbyname::rowsums_byname(R_plus_V_mat)
    W_mat <- matsbyname::difference_byname(matsbyname::transpose_byname(R_plus_V_mat), U_mat)
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
    list(y_vec, q_vec, f_vec, g_vec, W_mat) %>%
      magrittr::set_names(c(y, q, f, g, W))
  }
  matsindf::matsindf_apply(.sutdata, FUN = yqfgw_func, R_mat = R, U_mat = U, V_mat = V, Y_mat = Y, S_units_mat = S_units)
}

#' Calculate \code{Z}, \code{D}, \code{C}, and \code{A} matrices
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param R resources (`R`) matrix or name of the column in `.sutmats` that contains same. Default is "`R`".
#'          `R` is an optional argument.
#'          If all of `R` is added to `V`, this argument can be left unspecified.
#' @param U use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".
#' @param V make (\code{V}) matrix or name of the column in \code{.sutmats}that contains same. Default is "\code{V}".
#' @param q \code{q} vector or name of the column in \code{.sutmats} that contains same. Default is "\code{q}".
#' @param f \code{f} vector or name of the column in \code{.sutmats} that contains same. Default is "\code{r}".
#' @param g \code{g} vector or name of the column in \code{.sutmats} that contains same. Default is "\code{g}".
#' @param Z name for \code{Z} matrix on output. Default is "\code{Z}".
#'        \code{Z} is calculated by \code{U * g_hat_inv}.
#' @param K name for \code{K} matrix on output. Default is "\code{K}".
#'        \code{K} is calculated by \code{U * f_hat_inv}.
#' @param C name for \code{C} matrix on output. Default is "\code{C}".
#'        \code{C} is calculated by \code{transpose(V) * g_hat_inv}.
#' @param D name for \code{D} matrix on output. Default is "\code{D}".
#'        \code{D} is calculated by \code{V * q_hat_inv}.
#' @param A name for \code{A} matrix on output. Default is "\code{A}".
#'        \code{A} is calculated by \code{Z * D}.
#'
#' @return a list or data frame containing \code{Z},
#' \code{K}, \code{C}, \code{D}, and \code{A} matrices
#'
#' @export
calc_A <- function(.sutdata = NULL,
                   # Input names
                   R = "R", U = "U", V = "V", q = "q", f = "f", g = "g",
                   # Output names
                   Z = "Z", K = "K", C = "C", D = "D", A = "A"){
  A_func <- function(R_mat, U_mat, V_mat, q_vec, f_vec, g_vec){
    if (is.null(R_mat)) {
      # No R matrix, just use the V matrix, assuming that resouces are included there.
      R_plus_V_mat <- V_mat
    } else {
      # An R matrix is present. Sum R and V before proceeding.
      R_plus_V_mat <- matsbyname::sum_byname(R_mat, V_mat)
    }
    # The calculation of C and Z will fail when g contains NA values.
    # NA values can be created when V has any industry whose outputs are unit inhomogeneous.
    # Test here if any entry in g is NA.
    # If so, the value for C will be assigned to NA.
    if (any(is.na(g_vec))) {
      C_mat <- NA_real_ %>%
        # rowtype of C_mat is rowtype(transpose(R_plus_V_mat)), which is same as coltype(R_plus_V_mat))
        matsbyname::setrowtype(matsbyname::coltype(R_plus_V_mat)) %>%
        matsbyname::setcoltype(matsbyname::coltype(matsbyname::hatinv_byname(g_vec)))
      Z_mat <- NA_real_ %>%
        matsbyname::setrowtype(matsbyname::rowtype(U_mat)) %>%
        matsbyname::setcoltype(matsbyname::coltype(matsbyname::hatinv_byname(g_vec)))
    } else {
      C_mat <- matsbyname::matrixproduct_byname(matsbyname::transpose_byname(R_plus_V_mat), matsbyname::hatinv_byname(g_vec))
      Z_mat <- matsbyname::matrixproduct_byname(U_mat, matsbyname::hatinv_byname(g_vec))
    }
    # The calculation of K will fail when f contains NA values.
    # NA values can be created when U has any industry whose inputs are inhomogeneous.
    # Test here if any entry in f is NA.
    # If so, the value for K will be assigned NA.
    if (any(is.na(f_vec))) {
      K_mat <- NA_real_ %>%
        matsbyname::setrowtype(U_mat) %>%
        matsbyname::setcoltype(matsbyname::coltype(matsbyname::hatinv_byname(f_vec)))
    } else {
      K_mat <- matsbyname::matrixproduct_byname(U_mat, matsbyname::hatinv_byname(f_vec))
    }
    D_mat <- matsbyname::matrixproduct_byname(R_plus_V_mat, matsbyname::hatinv_byname(q_vec))
    A_mat <- matsbyname::matrixproduct_byname(Z_mat, D_mat)
    # Put all output matrices in a list and return it.
    list(Z_mat, K_mat, C_mat, D_mat, A_mat) %>%
      magrittr::set_names(c(Z, K, C, D, A))
  }
  matsindf::matsindf_apply(.sutdata, FUN = A_func, R_mat = R, U_mat = U, V_mat = V, q_vec = q, f_vec = f, g_vec = g)
}


#' Calculates total requirements matrices (\code{L_pxp} and \code{L_ixp})
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param D \code{D} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{D}".
#' @param A \code{A} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{A}".
#' @param L_ixp name for \code{L_ixp} matrix on output. Default is "\code{L_ixp}".
#'        \code{L_ixp} is calculated by \code{D * L_pxp}.
#' @param L_pxp name for \code{L_pxp} matrix on output. Default is "\code{L_pxp}".
#'        \code{L_pxp} is calculated by \code{(I - Z*D)^-1}.
#'
#' @return a list or data frame containing \code{L_pxp} and \code{L_ixp} matrices
#'
#' @export
calc_L <- function(.sutdata = NULL,
                   # Input names
                   D = "D", A = "A",
                   # Output names
                   L_pxp = "L_pxp", L_ixp = "L_ixp"){
  L_func <- function(D_mat, A_mat){
    L_pxp_mat <- matsbyname::Iminus_byname(A_mat) %>% matsbyname::invert_byname()
    L_ixp_mat <- matsbyname::matrixproduct_byname(D_mat, L_pxp_mat)
    list(L_pxp_mat, L_ixp_mat) %>% magrittr::set_names(c(L_pxp, L_ixp))
  }
  matsindf::matsindf_apply(.sutdata, FUN = L_func, D_mat = D, A_mat = A)
}
