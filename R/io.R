#
# This file contains functions that calculate matrices
# relevant to input-ouput (PSUT) analyses
#

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
