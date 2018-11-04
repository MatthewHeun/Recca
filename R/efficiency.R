# This file contains functions that calculate direct efficiencies for an ECC.


#' Calculate industry efficiencies
#'
#' Calculates industry efficiencies for all energy conversion industries in the ECC.
#'
#' Note that these efficiencies (\code{eta}) are different from
#' final demand sector and product efficiencies (\code{eta_s} and \code{eta_p}, respectively).
#' Both final demand sector and product efficiencies
#' (\code{eta_s} and \code{eta_p}) are based on embodied energy, whereas
#' industry efficiencies (\code{eta}) is based on direct energy consumed and produced
#' by the energy conversion industry.
#'
#' To calculate energy conversion industry efficiencies, use the
#' \code{\link{calc_embodied_etas}} function.
#'
#' The "\code{g}" column can be calculated by \code{\link{calc_io_mats}}.
#'
#' @param .sutmats a data frame containing columns for \code{g}, \code{U}, and \code{S_units} matrices
#' @param g_colname a string for the name of a column of \code{g} matrices in \code{.sutmats}. (Default is "\code{g}".)
#' @param U_colname a string for the name of a column of \code{U} matrices in \code{.sutmats}. (Default is "\code{U}".)
#' @param S_units_colname a string for the name of a column of \code{S_units} matrices in \code{.sutmats}. (Default is "\code{S_units}".)
#' @param eta_colname the name of the industry efficiency column in output. Default is "\code{eta}".
#'
#' @return \code{.sutmats} with an additional column "\code{eta}"
#'
#' @export
#'
#' @examples
calc_eta_i <- function(.sutdata,
                     # Input columns
                     g_colname = "g", U_colname = "U", S_units_colname = "S_units",
                     # Output columns
                     eta_colname = "eta"){
  eta_func <- function(g, U, S_units){
    # Test whether S_units has the right form.
    stopifnot(products_unit_homogeneous(S_units_colname = S_units)[[1]])
    # Now calculate efficiencies
    U_bar <- matrixproduct_byname(transpose_byname(S_units), U)
    iU_bar_hat <- hatize_byname(colsums_byname(U_bar))
    eta <- g %>% matrixproduct_byname(invert_byname(iU_bar_hat))
    # Return the eta value(s) with the correct name
    list(eta) %>% magrittr::set_names(eta_colname)
  }

  matsindf_apply(.sutdata, FUN = eta_func, g = g_colname, U = U_colname, S_units = S_units_colname)
}
