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
#' @return a data frame containing columns specified in \code{keep_cols},
#' \code{y_colname}, \code{q_colname}, \code{g_colname}, \code{W_colname},
#' \code{Z_colname}, \code{D_colname}, \code{C_colname}, \code{A_colname},
#' \code{L_ixp_colname}, and \code{L_pxp_colname}.
#'
#' @export
calc_io_mats <- function(.sutdata,
                         # Input columns
                         U_colname = "U", V_colname = "V", Y_colname = "Y",
                         # Output columns
                         # keep_cols = NULL,
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
              # keep_cols = c(keep_cols, U_colname, V_colname),
              y_colname = y_colname, q_colname = q_colname, g_colname = g_colname) %>%
    calc_A(U_colname = U_colname, V_colname = V_colname, q_colname = q_colname, g_colname = g_colname,
           # keep_cols = c(keep_cols, y_colname, q_colname, g_colname, W_colname),
           Z_colname = Z_colname, D_colname = D_colname, C_colname = C_colname, A_colname = A_colname) %>%
    calc_L(D_colname = D_colname, A_colname = A_colname,
           # keep_cols = c(keep_cols, y_colname, q_colname, g_colname, W_colname, Z_colname, D_colname, C_colname, A_colname),
           L_ixp_colname = L_ixp_colname, L_pxp_colname = L_pxp_colname)
}



#' Calculate y, g, and q vectors and W matrices
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param U_colname the name of the column in \code{.sutdata} containing Use (\code{U}) matrices.
#' @param V_colname the name of the column in \code{.sutdata} containing Make (\code{V}) matrices.
#' @param Y_colname the name of the column in \code{.sutdata} containing final demand (\code{Y}) matrices.
#' @param S_units the name of the column in \code{.sutdata} containing \code{S_units} matrices.
#' @param keep_cols a vector of names of columns of \code{.sutdata} to return with the output
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
#' @return a data frame containing columns specified in \code{keep_cols},
#' \code{y_colname}, \code{q_colname}, \code{g_colname}, and \code{W_colname}.
calc_yqgW <- function(.sutdata,
                      # Input columns
                      U_colname = "U", V_colname = "V", Y_colname = "Y", S_units = "S_units",
                      # Output columns
                      # keep_cols = NULL,
                      y_colname = "y", q_colname = "q", g_colname = "g", W_colname = "W"){
  # yqgw_func <- function(U, V, Y, S_units){
  #   y_val <- rowsums_byname(Y)
  #   q_val <- sum_byname(rowsums_byname(U), y_val)
  #   g_val <- rowsums_byname(V)
  #   W_val <- difference_byname(transpose_byname(V), U)
  #   out <- list(g_val, y_val, q_val, W_val) %>% set_names(y_colname, q_colname, g_colname, W_colname)
  #   return(out)
  # }
  # matsindf_apply(.sutdata, FUN = yqgw_func, U = U_colname, V = V_colname, Y = Y_colname, S_units = S_units)

  # Establish input column names
  U <- as.name(U_colname)
  V <- as.name(V_colname)
  Y <- as.name(Y_colname)
  S_units <- as.name(S_units)

  # Establish temporary column names
  V_bar_colname <- ".V_bar"
  V_bar <- as.name(V_bar_colname)
  V_bar_check_colname <- ".V_bar_check"
  V_bar_check <- as.name(V_bar_check_colname)

  # Establish output column names
  y <- as.name(y_colname)
  q <- as.name(q_colname)
  g <- as.name(g_colname)
  W <- as.name(W_colname)

  # Ensure that we won't overwrite a column.
  verify_cols_missing(.sutdata, c(V_bar, V_bar_check, y, q, g, W))





  # Perform a check on units.
  # The V_bar matrix should have only one entry per row,
  # meaning that all products of a given industry are measured in the same units.
  # At the present time, this method works only under those conditions
  # (product unit homogeneity).
  # If product unit homogeneity is violated, the methods here cannot be used.
  # To accommodate unit inhomogenity of industry products,
  # further generalizations of this method will be needed.
  # CheckUnits <- .sutdata %>%
  #   mutate(
  #     !!V_bar := matrixproduct_byname(!!V, !!S_units),
  #     !!V_bar_check := count_vals_inrows_byname(!!V_bar, "!=", 0) %>%
  #       compare_byname("==", 1) %>%
  #       all_byname()
  #   )
  # Verify that units are good everywhere.
  # stopifnot(all(as.logical(CheckUnits[[V_bar_check_colname]])))






  # Now that we know the units are fine (i.e., the products of each industry are unit homogeneous),
  # we can proceed with calculating y, q, g, and W.
  .sutdata %>%
    # select_(.dots = c(intersect(keep_cols, names(.)), U_colname, V_colname, Y_colname)) %>%
    mutate(
      !!y := rowsums_byname(!!Y),
      !!q := sum_byname(rowsums_byname(!!U), !!y),
      !!g := rowsums_byname(!!V),
      !!W := difference_byname(transpose_byname(!!V), !!U)
    ) # %>%
    # select_(.dots = c(intersect(keep_cols, names(.)), g_colname, y_colname, q_colname, W_colname))
}

#' Calculate \code{Z}, \code{D}, \code{C}, and \code{A} matrices
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param U_colname the name of the column in \code{.sutdata} containing Use (\code{U}) matrices.
#' @param V_colname the name of the column in \code{.sutdata} containing Make (\code{V}) matrices.
#' @param q_colname the name of the column in \code{.sutdata} containing \code{q} vectors.
#' @param g_colname the name of the column in \code{.sutdata} containing \code{g} vectors.
#' @param keep_cols a vector of names of columns of \code{.sutdata} to return with the output
#' @param Z_colname the name of the output column containing \code{Z} matrices.
#' \code{Z} is calculated by \code{U * g_hat_inv}.
#' @param D_colname the name of the output column containing \code{D} matrices.
#' \code{D} is calculated by \code{V * q_hat_inv}.
#' @param C_colname the name of the output column containing \code{C} matrices.
#' \code{C} is calculated by \code{transpose(V) * g_hat_inv}.
#' @param A_colname the name of the output column containing \code{A} matrices.
#' \code{A} is calculated by \code{Z * D}.
#'
#' @return a data frame containing columns specified in \code{keep_cols},
#' \code{Z_colname}, \code{D_colname}, and \code{A_colname}.
#'
#' @export
calc_A <- function(.sutdata,
                   # Input columns
                   U_colname = "U", V_colname = "V",
                   q_colname = "q", g_colname = "g",
                   # Output columns
                   # keep_cols = NULL,
                   Z_colname = "Z", D_colname = "D", C_colname = "C", A_colname = "A"){
  # A_func <- function(U, V, q, g){
  #   Z_val <- matrixproduct_byname(U, hatize_byname(g) %>% invert_byname())
  #   C_val <- matrixproduct_byname(transpose_byname(V), hatize_byname(g) %>% invert_byname())
  #   D_val <- matrixproduct_byname(V, hatize_byname(q) %>% invert_byname())
  #   A_val <- matrixproduct_byname(Z_val, D_val)
  #   out <- list(Z_val, D_val, C_val, A_val) %>% set_names(Z_colname, D_colname, C_colname, A_colname)
  #   return(out)
  # }
  # matsindf_apply(.sutdata, FUN = A_func, U = U_colname, V = V_colname, q = q_colname, g = g_colname)

  # Establish names of input columns
  U <- as.name(U_colname)
  V <- as.name(V_colname)
  q <- as.name(q_colname)
  g <- as.name(g_colname)
  # Establish names of output columns
  Z <- as.name(Z_colname)
  D <- as.name(D_colname)
  C <- as.name(C_colname)
  A <- as.name(A_colname)
  # Ensure that we won't overwrite a column.
  verify_cols_missing(.sutdata, c(Z, D, C, A))

  .sutdata %>%
    # select_(.dots = c(intersect(keep_cols, names(.)), U_colname, V_colname, q_colname, g_colname)) %>%
    mutate(
      !!Z := matrixproduct_byname(!!U, hatize_byname(!!g) %>% invert_byname()),
      !!C := matrixproduct_byname(transpose_byname(!!V), hatize_byname(!!g) %>% invert_byname()),
      !!D := matrixproduct_byname(!!V, hatize_byname(!!q) %>% invert_byname()),
      !!A := matrixproduct_byname(!!Z, !!D)
    ) # %>%
    # select_(.dots = c(intersect(keep_cols, names(.)), Z_colname, C_colname, D_colname, A_colname))
}


#' Calculates total requirements matrices
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param D_colname the name of the column in \code{.sutdata} containing the \code{D} matrix.
#' @param A_colname the name of the column in \code{.sutdata} containing the \code{A} matrix.
#' @param keep_cols a vector of names of columns of \code{.sutdata} to return with the output
#' @param L_ixp_colname the name of the output column containing the industry-by-product L matrix.
#' \code{L_ixp} is calculated by \code{D * L_pxp}.
#' @param L_pxp_colname the name of the output column containing the product-by-product L matrix.
#' \code{L_pxp} is calculated by \code{(I - Z*D)^-1}.
#'
#' @return a data frame containing columns specified in \code{keep_cols},
#' \code{L_ixp_colname}, and \code{L_pxp_colname}.
#'
#' @export
calc_L <- function(.sutdata,
                   # Input columns
                   D_colname = "D", A_colname = "A",
                   # Output columns
                   # keep_cols = NULL,
                   L_ixp_colname = "L_ixp", L_pxp_colname = "L_pxp"){
  # L_func <- function(D, A){
  #   L_pxp_val := Iminus_byname(A) %>% invert_byname()
  #   L_ixp_val := matrixproduct_byname(D, !!L_pxp_val)
  #   out <- list(L_pxp_val, L_ixp_val) %>% set_names(L_pxp_colname, L_ixp_colname)
  #   return(out)
  # }
  # matsindf_apply(.sutdata, FUN = L_func, D = D_colname, A = A_colname)

  # Establish input column names
  D <- as.name(D_colname)
  A <- as.name(A_colname)
  # Establish output column names
  L_ixp <- as.name(L_ixp_colname)
  L_pxp <- as.name(L_pxp_colname)
  # Ensure that we won't overwrite a column.
  verify_cols_missing(.sutdata, c(L_ixp, L_pxp))

  .sutdata %>%
    # select_(.dots = c(intersect(keep_cols, names(.)), D_colname, A_colname)) %>%
    mutate(
      !!L_pxp := Iminus_byname(!!A) %>% invert_byname(),
      !!L_ixp := matrixproduct_byname(!!D, !!L_pxp)
    ) # %>%
    # select_(.dots = c(intersect(keep_cols, names(.)), L_pxp_colname, L_ixp_colname))
}
