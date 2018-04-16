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
#' @param keep_cols a vector of names of columns of \code{.sutdata} to return with the output
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
                         keep_cols = NULL,
                         y_colname = "y", q_colname = "q", g_colname = "g", W_colname = "W",
                         Z_colname = "Z", D_colname = "D", C_colname = "C", A_colname = "A",
                         L_ixp_colname = "L_ixp", L_pxp_colname = "L_pxp"){
  .sutdata %>%
    # Clean the matrices in the columns.
    # This step avoids situations where rows or columns of zeroes
    # cause a _hat_inv step to fail due to inverting a matrix with a row or column of zeroes.
    mutate_(
      .dots = list(
        # U = clean_byname(U)
        interp(~ clean_byname(ucol, margin = c(1,2), clean_value = 0),
               ucol = as.name(U_colname)),
        # V = clean_byname(V)
        interp(~ clean_byname(vcol, margin = c(1,2), clean_value = 0),
               vcol = as.name(V_colname)),
        # Y = clean_byname(Y)
        interp(~ clean_byname(ycol, margin = c(1,2), clean_value = 0),
               ycol = as.name(Y_colname))
      ) %>%
        setNames(c(U_colname, V_colname, Y_colname))
    ) %>%
    calc_yqgW(U_colname = U_colname, V_colname = V_colname, Y_colname = Y_colname, W_colname = W_colname,
              keep_cols = c(keep_cols, U_colname, V_colname),
              y_colname = y_colname, q_colname = q_colname, g_colname = g_colname) %>%
    calc_A(U_colname = U_colname, V_colname = V_colname, q_colname = q_colname, g_colname = g_colname,
           keep_cols = c(keep_cols, y_colname, q_colname, g_colname, W_colname),
           Z_colname = Z_colname, D_colname = D_colname, C_colname = C_colname, A_colname = A_colname) %>%
    calc_L(D_colname = D_colname, A_colname = A_colname,
           keep_cols = c(keep_cols, y_colname, q_colname, g_colname, W_colname, Z_colname, D_colname, C_colname, A_colname),
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
                      keep_cols = NULL,
                      y_colname = "y", q_colname = "q", g_colname = "g", W_colname = "W"){
  V <- as.name(V_colname)
  V_bar_colname <- ".V_bar"
  V_bar <- as.name(V_bar_colname)
  S_units <- as.name(S_units)
  V_bar_check_colname <- ".V_bar_check"
  V_bar_check <- as.name(V_bar_check_colname)
  # Perform a check on units.
  # The V_bar matrix should have only one entry per row,
  # meaning that all products of a given industry are measured in the same units.
  # At the present time, this method works only under those conditions
  # (product unit homogeneity).
  # If product unit homogeneity is violated, the methods here cannot be used.
  # To accommodate unit inhomogenity of industry products,
  # further generalizations of this method will be needed.
  CheckUnits <- .sutdata %>%
    mutate(
      !!V_bar := matrixproduct_byname(!!V, !!S_units),
      !!V_bar_check := count_vals_inrows_byname(!!V_bar, "!=", 0) %>%
        compare_byname("==", 1) %>%
        all_byname()
    )
  # Verify that units are good everywhere.
  stopifnot(all(as.logical(CheckUnits[[V_bar_check_colname]])))

  # Now that we know the units are fine (i.e., the products of each industry are unit homogeneous),
  # we can proceed with calculating y, q, g, and W.
  .sutdata %>%
    select_(.dots = c(intersect(keep_cols, names(.)), U_colname, V_colname, Y_colname)) %>%
    mutate_(
      .dots = list(
        # .g = rowsums(V)
        interp(~ rowsums_byname(vcol),
               vcol = as.name(V_colname)),
        # .y = rowsums(Y)
        interp(~ rowsums_byname(vcol),
               vcol = as.name(Y_colname))
      ) %>%
        setNames(c(g_colname, y_colname))
    ) %>%
    mutate_(
      .dots = list(
        # .q = rowsums(U) + y
        interp(~ sum_byname(rowsums_byname(ucol), ycol),
               ucol = as.name(U_colname),
               ycol = as.name(y_colname))
      ) %>%
        setNames(c(q_colname))
    ) %>%
    mutate_(
      .dots = list(
        # W = transpose(V) - U
        interp(~ difference_byname(transpose_byname(vcol), ucol),
               ucol = as.name(U_colname),
               vcol = as.name(V_colname))
      ) %>%
        setNames(c(W_colname))
    ) %>%
    select_(.dots = c(intersect(keep_cols, names(.)), g_colname, y_colname, q_colname, W_colname))
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
calc_A <- function(.sutdata,
                   # Input columns
                   U_colname = "U", V_colname = "V",
                   q_colname = "q", g_colname = "g",
                   # Output columns
                   keep_cols = NULL,
                   Z_colname = "Z", D_colname = "D", C_colname = "C", A_colname = "A"){
  .sutdata %>%
    select_(.dots = c(intersect(keep_cols, names(.)), U_colname, V_colname, q_colname, g_colname)) %>%
    mutate_(
      .dots = list(
        # Z = U * g_hat_inv
        interp(~ matrixproduct_byname(ucol, gcol %>% hatize_byname %>% invert_byname()),
               ucol = as.name(U_colname),
               gcol = as.name(g_colname)),
        # D = V * q_hat_inv
        interp(~ matrixproduct_byname(vcol, qcol %>% hatize_byname %>% invert_byname()),
               vcol = as.name(V_colname),
               qcol = as.name(q_colname)),
        # C = transpose(V) * g_hat_inv
        interp(~ matrixproduct_byname(transpose_byname(vcol), gcol %>% hatize_byname %>% invert_byname()),
               vcol = as.name(V_colname),
               gcol = as.name(g_colname))
      ) %>%
        setNames(c(Z_colname, D_colname, C_colname))
    ) %>%
    mutate_(
      .dots = list(
        # A = Z*D
        interp(~ matrixproduct_byname(zcol, dcol),
               zcol = as.name(Z_colname),
               dcol = as.name(D_colname))
      ) %>%
        setNames(A_colname)
    ) %>%
    select_(.dots = c(intersect(keep_cols, names(.)), Z_colname, C_colname, D_colname, A_colname))
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
calc_L <- function(.sutdata,
                   # Input columns
                   D_colname = "D", A_colname = "A",
                   # Output columns
                   keep_cols = NULL,
                   L_ixp_colname = "L_ixp", L_pxp_colname = "L_pxp"){
  .sutdata %>%
    select_(.dots = c(intersect(keep_cols, names(.)), D_colname, A_colname)) %>%
    mutate_(
      .dots = list(
        # L_pxp = (I - A)^-1
        interp(~ Iminus_byname(bdcol) %>% invert_byname(),
               bdcol = as.name(A_colname))
      ) %>%
        setNames(c(L_pxp_colname))
    ) %>%
    mutate_(
      .dots = list(
        # L_ixp = D * L_pxp
        interp(~ matrixproduct_byname(dcol, ppcol),
               dcol = as.name(D_colname),
               ppcol = as.name(L_pxp_colname))
      ) %>%
        setNames(c(L_ixp_colname))
    ) %>%
    select_(.dots = c(intersect(keep_cols, names(.)), L_pxp_colname, L_ixp_colname))
}
