#
# This file contains functions that calculate matrices
# relevant to input-ouput (PSUT) analyses
#


#' Calculate various input-output matrices
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param U_colname the name of the column in \code{.sutdata} containing Use (\code{U}) matrices.
#' @param V_colname the name of the column in \code{.sutdata} containing Make (\code{V}) matrices.
#' @param Y_colname the name of the column in \code{.sutdata} containing final demand (\code{Y}) matrices.
#' @param g_colname the name of the output column containing \code{g} vectors.
#' \code{g} is calculated by \code{rowsums(V)}.
#' @param y_colname the name of the output column containing \code{y} vectors.
#' \code{y} is calculated by \code{rowsums(Y)}.
#' @param q_colname the name of the output column containing \code{q} vectors.
#' \code{q} is calculated by \code{rowsums(U) + y}.
#' @param W_colname the name of the output column containing \code{W} matrices.
#' \code{W} is calculated by \code{transpose(V) - U}.
#' @param Z_colname the name of the output column containing \code{Z} matrices.
#' \code{Z} is calculated by \code{U * g_hat_inv}.
#' @param D_colname the name of the output column containing \code{D} matrices.
#' \code{D} is calculated by \code{V * q_hat_inv}.
#' @param C_colname the name of the output column containing \code{C} matrices.
#' \code{C} is calculated by \code{transpose(V) * g_hat_inv}.
#' @param A_colname the name of the output column containing \code{A} matrices.
#' \code{A} is calculated by \code{Z * D}.
#' @param L_ixp_colname the name of the output column containing industry-by-product L matrices.
#' \code{L_ixp} is calculated by \code{D * L_pxp}.
#' @param L_pxp_colname the name of the output column containing product-by-product L matrices.
#' \code{L_pxp} is calculated by \code{(I - Z*D)^-1}.
#'
#' @return \code{.sutdata} with columns
#' \code{y_colname}, \code{q_colname}, \code{g_colname}, \code{W_colname},
#' \code{Z_colname}, \code{D_colname}, \code{C_colname}, \code{A_colname},
#' \code{L_ixp_colname}, and \code{L_pxp_colname}
#' added
#'
#' @export
calc_io_mats <- function(.sutdata,
                         # Input columns
                         U_colname = "U", V_colname = "V", Y_colname = "Y",
                         # Output columns
                         y_colname = "y", q_colname = "q", g_colname = "g", W_colname = "W",
                         Z_colname = "Z", D_colname = "D", C_colname = "C", A_colname = "A",
                         L_ixp_colname = "L_ixp", L_pxp_colname = "L_pxp"){
  # Establish names
  U <- as.name(U_colname)
  V <- as.name(V_colname)
  Y <- as.name(Y_colname)

  .sutdata %>%
    # Clean the matrices in the columns.
    # This step avoids situations where rows or columns of zeroes
    # cause a _hat_inv step to fail due to inverting a matrix with a row or column of zeroes.
    mutate(
      !!U := clean_byname(!!U, margin = c(1,2), clean_value = 0),
      !!V := clean_byname(!!V, margin = c(1,2), clean_value = 0),
      !!Y := clean_byname(!!Y, margin = c(1,2), clean_value = 0)
    ) %>%
    calc_yqgW(U_colname = U_colname, V_colname = V_colname, Y_colname = Y_colname, W_colname = W_colname,
              y_colname = y_colname, q_colname = q_colname, g_colname = g_colname) %>%
    calc_A(U_colname = U_colname, V_colname = V_colname, q_colname = q_colname, g_colname = g_colname,
           Z_colname = Z_colname, D_colname = D_colname, C_colname = C_colname, A_colname = A_colname) %>%
    calc_L(D_colname = D_colname, A_colname = A_colname,
           L_ixp_colname = L_ixp_colname, L_pxp_colname = L_pxp_colname)
}


#' Calculate y, g, and q vectors and W matrices
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param U_colname the name of the column in \code{.sutdata} containing Use (\code{U}) matrices.
#' @param V_colname the name of the column in \code{.sutdata} containing Make (\code{V}) matrices.
#' @param Y_colname the name of the column in \code{.sutdata} containing final demand (\code{Y}) matrices.
#' @param S_units the name of the column in \code{.sutdata} containing \code{S_units} matrices.
#' @param y_colname the name of the output column containing \code{y} vectors.
#' \code{y} is calculated by \code{rowsums_byname(Y)}.
#' @param g_colname the name of the output column containing \code{g} vectors.
#' \code{g} is calculated by \code{rowsums_byname(V)}.
#' @param q_colname the name of the output column containing \code{q} vectors.
#' \code{q} is calculated by \code{rowsums_byname(U) + y}.
#' @param W_colname the name of the output column containing \code{W} matrices.
#' \code{W} is calculated by \code{transpose_byname(V) - U}.
#'
#' @export
#'
#' @return \code{.sutdata} with columns \code{y_colname}, \code{q_colname}, \code{g_colname}, and \code{W_colname} added
calc_yqgW <- function(.sutdata = NULL,
                      # Input columns
                      U_colname = "U", V_colname = "V", Y_colname = "Y", S_units = "S_units",
                      # Output columns
                      y_colname = "y", q_colname = "q", g_colname = "g", W_colname = "W"){
  yqgw_func <- function(U, V, Y, S_units = NULL){
    # Perform a unit homogeneity check on the incoming V matrix.
    # But only if S_units is present and useable.
    if (!is.null(S_units)) {
      # The V_bar matrix should have only one entry per row,
      # meaning that all products of a given industry are measured in the same units.
      # At the present time, the PSUT framework works only under those conditions
      # (i.e., product unit homogeneity).
      # If product unit homogeneity is violated, the PSUT method cannot be used.
      # To accommodate unit inhomogenity of industry products,
      # further generalizations of the PSUT method will be required.
      V_bar <- matrixproduct_byname(V, S_units)
      V_bar_check <- count_vals_inrows_byname(V_bar, "!=", 0) %>%
        compare_byname("==", 1)
      # Verify that unit homogeneity exists for all products.
      if (!all_byname(V_bar_check)) {
        offenders <- which(!unlist(as.list(V_bar_check) %>% set_names(rownames(V_bar_check))))
        stop(paste("Outputs from each industry not unit homogeneous.",
                   "Offending industries:",
                   paste(names(offenders), collapse = ", ")))
      }
    }
    y_val <- rowsums_byname(Y)
    q_val <- sum_byname(rowsums_byname(U), y_val)
    g_val <- rowsums_byname(V)
    W_val <- difference_byname(transpose_byname(V), U)
    out <- list(y_val, q_val, g_val, W_val) %>% set_names(c(y_colname, q_colname, g_colname, W_colname))
    return(out)
  }
  matsindf_apply(.sutdata, FUN = yqgw_func, U = U_colname, V = V_colname, Y = Y_colname, S_units = S_units)
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
#' @param D_colname the name of the output column containing \code{D} matrices.
#' \code{D} is calculated by \code{V * q_hat_inv}.
#' @param C_colname the name of the output column containing \code{C} matrices.
#' \code{C} is calculated by \code{transpose(V) * g_hat_inv}.
#' @param A_colname the name of the output column containing \code{A} matrices.
#' \code{A} is calculated by \code{Z * D}.
#'
#' @return \code{.sutdata} with columns \code{Z_colname}, \code{D_colname}, and \code{A_colname} added
#'
#' @export
calc_A <- function(.sutdata,
                   # Input columns
                   U_colname = "U", V_colname = "V",
                   q_colname = "q", g_colname = "g",
                   # Output columns
                   Z_colname = "Z", D_colname = "D", C_colname = "C", A_colname = "A"){
  A_func <- function(U, V, q, g){
    Z_val <- matrixproduct_byname(U, hatize_byname(g) %>% invert_byname())
    C_val <- matrixproduct_byname(transpose_byname(V), hatize_byname(g) %>% invert_byname())
    D_val <- matrixproduct_byname(V, hatize_byname(q) %>% invert_byname())
    A_val <- matrixproduct_byname(Z_val, D_val)
    out <- list(Z_val, C_val, D_val, A_val) %>% set_names(Z_colname, C_colname, D_colname, A_colname)
    return(out)
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
#' @export
calc_L <- function(.sutdata,
                   # Input columns
                   D_colname = "D", A_colname = "A",
                   # Output columns
                   L_ixp_colname = "L_ixp", L_pxp_colname = "L_pxp"){
  L_func <- function(D, A){
    L_pxp_val <- Iminus_byname(A) %>% invert_byname()
    L_ixp_val <- matrixproduct_byname(D, L_pxp_val)
    out <- list(L_pxp_val, L_ixp_val) %>% set_names(L_pxp_colname, L_ixp_colname)
    return(out)
  }
  matsindf_apply(.sutdata, FUN = L_func, D = D_colname, A = A_colname)
}
