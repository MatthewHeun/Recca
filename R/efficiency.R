# This file contains functions that calculate direct efficiencies for an ECC.


#' Calculate industry efficiencies
#'
#' Calculates industry efficiencies for all energy conversion industries in the ECC.
#' The efficiency for a given industry is calculated iff the units for inputs and outputs for that industry are unit-homogeneous.
#' If units for inputs and outpus are heterogeneous for an industry, \code{NA} is the result.
#'
#' Note that these efficiencies (\code{eta}) are different from
#' final demand sector and product efficiencies (\code{eta_s} and \code{eta_p}, respectively).
#' Both final demand sector and product efficiencies
#' (\code{eta_s} and \code{eta_p}) are based on embodied energy, whereas
#' industry efficiencies (\code{eta}) is based on direct inputs consumed and outputs produced
#' by the energy conversion industry.
#'
#' To calculate energy conversion final demand sector and product efficiencies, use the
#' \code{\link{calc_embodied_etas}} function.
#'
#' @param .sutmats a data frame containing columns for \code{U}, \code{V}, and \code{S_units} matrices
#' @param U_colname a string for the name of a column of \code{U} matrices in \code{.sutmats}. (Default is "\code{U}".)
#' @param V_colname a string for the name of a column of \code{V} matrices in \code{.sutmats}. (Default is "\code{V}".)
#' @param S_units_colname a string for the name of a column of \code{S_units} matrices in \code{.sutmats}. (Default is "\code{S_units}".)
#' @param eta_i_colname the name of the industry efficiency column in output. Default is "\code{eta_i}".
#'
#' @return \code{.sutmats} with an additional column "\code{eta}"
#'
#' @export
#'
#' @examples
calc_eta_i <- function(.sutdata,
                     # Input columns
                     U_colname = "f", V_colname = "g", S_units_colname = "S_units",
                     # Output columns
                     eta_i_colname = "eta_i"){
  eta_func <- function(U, V, S_units){

    units_OK <- flows_unit_homogeneous(U_colname = U, V_colname = V, S_units_colname = S_units)

    f <- colsums_byname(U) %>% transpose_byname()
    g <- rowsums_byname(V)
    eta <- elementquotient_byname(g, f)
    # Now set eta to NA if the industry is unit-heterogeneous.
    eta[which(!units_OK)] <- NA_real_
    # Return the eta value(s) with the correct name
    list(eta) %>% magrittr::set_names(eta_colname)
  }

  matsindf_apply(.sutdata, FUN = eta_func, U = U_colname, V = V_colname, S_units = S_units_colname)
}
