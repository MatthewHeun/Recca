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
#'
#' @return a data frame containing columns specified in \code{keep_cols},
#' \code{U_colname} and \code{V_colname}.
#'
#' @export
#'
#' @examples
#'
reconstruct_UV <- function(.sutdata = NULL,
                           # Input columns
                           Y_prime_colname = "Y_prime", L_ixp_colname = "L_ixp", L_pxp_colname = "L_pxp",
                           Z_colname = "Z", D_colname = "D",
                           # Output columns
                           U_prime_colname = "U_prime", V_prime_colname = "V_prime"){

  reconstruct_func <- function(Y_prime, L_ixp, L_pxp, Z, D){
    y_prime_val <- rowsums_byname(Y_prime)
    g_prime_val <- matrixproduct_byname(L_ixp, y_prime_val)
    q_prime_val <- matrixproduct_byname(L_pxp, y_prime_val)
    U_prime_val <- matrixproduct_byname(Z, hatize_byname(g_prime_val))
    V_prime_val <- matrixproduct_byname(D, hatize_byname(q_prime_val))
    out <- list(U_prime_val, V_prime_val) %>% set_names(U_prime_colname, V_prime_colname)
    return(out)
  }
  return(matsindf_apply(.sutdata, FUN = reconstruct_func,
                        Y_prime = Y_prime_colname, L_ixp = L_ixp_colname, L_pxp = L_pxp_colname,
                        Z = Z_colname, D = D_colname))

  # # Establish names for input columns
  # Y_prime <- as.name(Y_prime_colname)
  # L_ixp <- as.name(L_ixp_colname)
  # L_pxp <- as.name(L_pxp_colname)
  # Z <- as.name(Z_colname)
  # D <- as.name(D_colname)
  # # Establish names for temporary columns
  # y_prime_colname <- ".y_prime"
  # y_prime <- as.name(y_prime_colname)
  # g_prime_colname <- ".g_prime"
  # g_prime <- as.name(g_prime_colname)
  # q_prime_colname <- ".q_prime"
  # q_prime <- as.name(q_prime_colname)
  # # Establish names for output columns
  # U_prime <- as.name(U_prime_colname)
  # V_prime <- as.name(V_prime_colname)
  #
  # # Ensure that we won't overwrite a column.
  # verify_cols_missing(.sutdata, c(y_prime, g_prime, q_prime, U_prime, V_prime))
  #
  # .sutdata %>%
  #   mutate(
  #     !!y_prime := rowsums_byname(!!Y_prime),
  #     !!g_prime := matrixproduct_byname(!!L_ixp, !!y_prime),
  #     !!q_prime := matrixproduct_byname(!!L_pxp, !!y_prime),
  #     !!U_prime := matrixproduct_byname(!!Z, hatize_byname(!!g_prime)),
  #     !!V_prime := matrixproduct_byname(!!D, hatize_byname(!!q_prime))
  #   ) %>%
  #   # Delete temporary columns
  #   select(-(!!y_prime), -(!!g_prime), -(!!q_prime))
}
