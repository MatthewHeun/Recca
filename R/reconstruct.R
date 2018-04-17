#
# This file contains functions that reconstruct an economy given
# total requirements matrices and a new final demand matrix (Y)
#

#' Reconstruct an economy given a new final demand matrix
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param Y_prime_colname the name of a column containing new final demand matrices
#' that will be used to reconstruct the economy
#' @param L_ixp_colname the name of a column containing industry-by-product L matrices.
#' @param L_pxp_colname the name of a column containing product-by-product L matrices.
#' @param Z_colname the name of a column containing \code{Z} matrices.
#' @param D_colname the name of a column containing \code{D} matrices.
#' @param keep_cols a vector of names of columns of \code{.sutdata} to return with the output
#' @param U_prime_colname the name of the output column that contains new Use (\code{U}) matrices.
#' @param V_prime_colname the name of the output column that contains new Make (\code{V}) matrices.
#' @param y_prime_colname the name of the column containing y_prime vectors
#' @param g_prime_colname the name of the column containing g_prime vectors
#' @param q_prime_colname the name of the column containing q_prime vectors
#'
#' @return a data frame containing columns specified in \code{keep_cols},
#' \code{U_colname} and \code{V_colname}.
#'
#' @export
#'
#' @examples
#'
reconstruct_UV <- function(.sutdata,
                           # Input columns
                           Y_prime_colname = "Y_prime", L_ixp_colname = "L_ixp", L_pxp_colname = "L_pxp",
                           Z_colname = "Z", D_colname = "D",
                           # Output columns
                           keep_cols = NULL,
                           U_prime_colname = "U_prime", V_prime_colname = "V_prime",
                           # Names for columns of intermediate results
                           y_prime_colname = ".yprime", g_prime_colname = ".gprime", q_prime_colname = ".qprime"){
  .sutdata %>%
    select_(.dots = c(intersect(keep_cols, names(.)), Y_prime_colname, L_ixp_colname, L_pxp_colname, Z_colname, D_colname)) %>%
    mutate_(
      .dots = list(
        # .yprime = rowsums(Y_prime)
        interp(~ rowsums_byname(ycol),
               ycol = as.name(Y_prime_colname))
      ) %>%
        setNames(c(y_prime_colname))
    ) %>%
    mutate_(
      .dots = list(
        # .gprime = L_xixp * .yprime
        interp(~ matrixproduct_byname(Lipcol, yprimecol),
               Lipcol = as.name(L_ixp_colname),
               yprimecol = as.name(y_prime_colname)),
        # .qprime = L_pxp * .yprime
        interp(~ matrixproduct_byname(Lppcol, yprimecol),
               Lppcol = as.name(L_pxp_colname),
               yprimecol = as.name(y_prime_colname))
      ) %>%
        setNames(c(g_prime_colname, q_prime_colname))
    ) %>%
    mutate_(
      # Calculate U and V
      .dots = list(
        # U_prime = Z * g_prime_hat
        interp(~ matrixproduct_byname(zcol, g_primecol %>% hatize_byname()),
               zcol = as.name(Z_colname),
               g_primecol = as.name(g_prime_colname)),
        # V_prime = D * q_prime_hat
        interp(~ matrixproduct_byname(dcol, q_primecol %>% hatize_byname()),
               dcol = as.name(D_colname),
               q_primecol = as.name(q_prime_colname))
      ) %>%
        setNames(c(U_prime_colname, V_prime_colname))
    ) %>%
    select_(.dots = c(intersect(keep_cols, names(.)), U_prime_colname, V_prime_colname))
}
