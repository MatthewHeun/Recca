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
#' @param U_prime_colname the name of the output column that contains new Use (\code{U}) matrices.
#' @param V_prime_colname the name of the output column that contains new Make (\code{V}) matrices.
#'
#' @return \code{.sutdata} with additional columns \code{U_prime} and \code{V_prime}
#'
#' @export
#'
#' @examples
#' UKEnergy2000mats %>%
#'   spread(key = matrix.name, value = matrix) %>%
#'   select(Country, Year, Energy.type, Last.stage, U, V, Y, r_EIOU, S_units) %>%
#'   calc_io_mats() %>%
#'   mutate(
#'     # Give new Y matrices that are double the existing Y matrices
#'     Y_prime = elementproduct_byname(2, Y)
#'   ) %>%
#'   # Should give U_prime and V_prime matrices that are double the existing U and V matrices
#'   reconstruct_UV()
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
    list(U_prime_val, V_prime_val) %>% purrr::set_names(c(U_prime_colname, V_prime_colname))
  }
  matsindf_apply(.sutdata, FUN = reconstruct_func,
                 Y_prime = Y_prime_colname, L_ixp = L_ixp_colname, L_pxp = L_pxp_colname,
                 Z = Z_colname, D = D_colname)
}


#' Assess the effect of changing perfectly substitutable inputs to an industry
#'
#' Internally, this function uses matsindf_apply, and documentation assumes that
#' \code{.sutdata} is not \code{NULL} and is a data frame.
#' If \code{.sutdata} is present, output is a data frame with columns named by string values of output arguments, and
#' input arguments should be character strings that name columns in \code{.sutdata}.
#' If \code{.sutdata} is \code{NULL} (the default), output is a list with items named by output strings,
#' and input arguments should be single matrices or vectors.
#'
#' @param .sutdata a data frame of supply-use table matrices with matrices arranged in columns.
#' @param k_prime the name of a column in \code{.sutdata} containing vectors representing new
#'        columns of the \code{K} matrix in each row of \code{.sutdata}.
#'        Each entry in the \code{k_prime} column of \code{.sutdata} must be a single-column matrix, and
#'        the name of the single column must match the name of one of the columns of matrix \code{K}.
#' @param K_colname the name of a column in \code{.sutdata} containing product-by-industry \code{K} matrices.
#'        \code{K} consists of columns that sum to 1.
#'        Elements of \code{K} indicate the fraction of total input to industries (in columns)
#'        provided by products (in rows).
#'        \code{K} can be calculated by \code{\link{calc_io_mats}}.
#' @param U_prime_colname the name of the output column that contains new Use (\code{U}) matrices.
#'        Default is "\code{U_prime}".
#' @param V_prime_colname the name of the output column that contains new Make (\code{V}) matrices.
#'        Default is "\code{V_prime}".
#'
#' @return \code{.sutdata} with additional columns \code{U_prime} and \code{V_prime}
#' @export
#'
#' @examples
delta_inputs_ps <- function(.sutdata = NULL,
                            # Input columns
                            k_prime_colname = "k_prime",
                            # Output colums
                            U_prime_colname = "U_prime", V_prime_colname = "V_prime"){
  delta_inputs_ps_func <- function(k_prime){
    # Get the name of the industry whose inputs will be changed.
    ind_to_change <- colnames(k_prime)
    # Ensure that k_prime is a single-column vector.
    if (length(ind_to_change) != 1) {
      stop(paste("k_prime has ", ncol(k_prime), "columns in delta_inputs_ps_func. Must be 1."))
    }


    U_prime_val <- "something"
    V_prime_val <- "something"
    list(U_prime_val, V_prime_val) %>% purrr::set_names(c(U_prime_colname, V_prime_colname))
  }
  matsindf_apply(.sutdata, FUN = delta_inputs_ps_func,
                 k_prime = k_prime_colname)

}




